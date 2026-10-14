"""Command fetcher for the troom FPGA wrapper.

``CommandFetcher`` turns a device-memory command buffer into the core's
``cmd`` stream. It reuses ``roomsoc.peripheral.dma.AXIDMAReader`` for
the AXI read side -- burst clipping, 4 KiB splits, multiple outstanding
bursts, and R response-error propagation -- and assembles little-endian
format-0 descriptors (seventeen 32-bit words, ``troom.abi``) across beat
boundaries: with a 64-bit bus, descriptors start four bytes into a beat,
so alternating descriptors end partway through a beat.

The fetch length is the command count times 68 rounded up to whole
beats; the host must keep that padding inside its allocation. The count
bounds fetching: exactly ``cmd_count`` descriptors are submitted, the
final one validated to be ``END``.

Validation happens before an offending descriptor is submitted:

* every field word must be representable in its RTL field (host words
  carrying bits above a field's width could alias opcodes and
  addresses, so they are rejected instead of truncated);
* ``END`` must appear exactly once, in the final descriptor. An early
  ``END`` or a non-``END`` final descriptor faults the launch with
  ``DECODE_ERROR`` and the descriptor's index. Earlier commands may
  already have executed; validation is not transactional.

A fetch read error (non-OKAY ``RRESP``) faults with ``FETCH_ERROR``.
After a fault or an ``abort`` (the launch controller's error recovery),
outstanding reads are drained and discarded -- AXI beats are never
dropped mid-response -- and no further descriptors are submitted.

Backpressure from the core's command queue propagates naturally: a
descriptor awaiting acceptance blocks further beat consumption, which
stalls the reader's R channel. Error payloads (``err``, ``err_code``,
``err_index``) are sticky levels, so the controller can latch the first
fault without losing simultaneous events.
"""

from amaranth import *

from troom.abi import (
    CMD_DESC_BYTES,
    CMD_DESC_WORDS,
    WrapperError,
    command_field_shapes,
)
from troom.consts import Opcode
from troom.core import Command
from troom.types import HasTRoomParams

from roomsoc.interconnect.axi import AXIInterface
from roomsoc.interconnect.stream import Decoupled
from roomsoc.peripheral.dma import AXIDMAReader


class CommandFetcher(HasTRoomParams, Elaboratable):
    """Device-memory command buffer to ``cmd`` stream converter."""

    def __init__(self, params):
        super().__init__(params)

        if not 32 <= self.data_width <= 512:
            raise ValueError(
                f'data_width {self.data_width} must be between 32 and 512 '
                f'for the descriptor assembler')

        # Fetch descriptors handed to the reader are chunked so that
        # one chunk covers exactly one (beat-padded) command
        # descriptor, and the next chunk is requested only after the
        # previous one is fully consumed. A chunk therefore always
        # fits the reader's data FIFO even under full cmd
        # backpressure, so the read grant is released between
        # descriptors and the wrapper's other read masters (the
        # tensor DMA) can never deadlock against command
        # consumption.
        self.chunk_bytes = (CMD_DESC_BYTES + self.beat_bytes - 1) \
            // self.beat_bytes * self.beat_bytes
        self.chunk_fifo_depth = max(self.dma_fifo_depth,
                                    self.chunk_bytes // self.beat_bytes + 2)

        self.bus = AXIInterface(data_width=self.data_width,
                                addr_width=self.addr_width,
                                id_width=self.id_width)

        self.cmd = Decoupled(Command, self.params)

        # Launch interface: a start pulse captures the configuration
        # inputs for the run.
        self.start = Signal()
        self.cmd_base = Signal(self.addr_width)
        self.cmd_count = Signal(32)

        # Recovery input: once asserted no further descriptors are
        # submitted and outstanding reads are drained and discarded.
        self.abort = Signal()

        self.active = Signal()
        self.done = Signal()
        self.err = Signal()
        self.err_code = Signal(WrapperError)
        self.err_index = Signal(32)

    def elaborate(self, platform):
        m = Module()
        lg = self.lg_beat
        beat_bytes = self.beat_bytes
        bbw = beat_bytes // 4  # 32-bit words per beat

        reader = m.submodules.reader = AXIDMAReader(
            self.bus,
            max_burst_beats=self.dma_max_burst_beats,
            max_outstanding=self.dma_max_outstanding,
            cmd_fifo_depth=self.dma_fifo_depth,
            data_fifo_depth=self.chunk_fifo_depth)

        shapes = command_field_shapes(self)
        op_bits = shapes[0][1]

        running = Signal()
        cursor = Signal(self.addr_width)
        remaining = Signal(40)
        pushed = Signal(40)
        returned = Signal(40)
        taken = Signal(40)
        count_latch = Signal(32)

        # Descriptor assembly: seventeen 32-bit slots, the word count
        # of the descriptor under assembly, and a pending-offer flag
        # for a completed descriptor awaiting cmd acceptance.
        desc = Signal(32 * CMD_DESC_WORDS)
        wc = Signal(5)  # 0..16 words in the current descriptor
        cp = Signal()  # completed descriptor offered on cmd
        desc_idx = Signal(32)
        submitted_all = Signal()
        fault = Signal()
        aborted = Signal()

        # Padded fetch length, computed for the staged count.
        cmd_bytes = Signal(40)
        m.d.comb += cmd_bytes.eq(self.cmd_count * CMD_DESC_BYTES)
        padded = ((cmd_bytes + (beat_bytes - 1)) >> lg) << lg

        drained = pushed == returned
        discard = fault | submitted_all | aborted | self.abort | reader.error
        m.d.comb += [
            self.active.eq(running),
            self.done.eq(submitted_all & ~fault & ~aborted & drained),
            self.err.eq(fault),
        ]

        with m.If(self.start):
            m.d.sync += [
                running.eq(1),
                cursor.eq(self.cmd_base),
                count_latch.eq(self.cmd_count),
                remaining.eq(padded),
                pushed.eq(0),
                returned.eq(0),
                taken.eq(0),
                wc.eq(0),
                cp.eq(0),
                desc_idx.eq(0),
                submitted_all.eq(0),
                fault.eq(0),
                aborted.eq(0),
                self.err_code.eq(WrapperError.NONE),
                self.err_index.eq(0),
            ]
        with m.If(running & discard & drained):
            m.d.sync += running.eq(0)

        with m.If(self.abort):
            m.d.sync += aborted.eq(1)

        # Fetch request generation: one beat-padded descriptor per
        # reader descriptor, requested only once the previous chunk
        # was fully consumed (which also implies it fully returned).
        chunk = Signal(40)
        m.d.comb += chunk.eq(
            Mux(remaining < self.chunk_bytes, remaining, self.chunk_bytes))
        m.d.comb += [
            reader.sink.valid.eq(~discard & running
                                 & (pushed == taken)
                                 & (remaining != 0)),
            reader.sink.bits.addr.eq(cursor),
            reader.sink.bits.len.eq(chunk),
        ]
        with m.If(reader.sink.fire):
            m.d.sync += [
                cursor.eq(cursor + chunk),
                remaining.eq(remaining - chunk),
                pushed.eq(pushed + chunk),
            ]
        with m.If(reader.source.fire):
            m.d.sync += returned.eq(returned + beat_bytes)

        # A read error faults the launch; the descriptor under assembly
        # names the position.
        with m.If(reader.error & ~fault):
            m.d.sync += [
                fault.eq(1),
                self.err_code.eq(WrapperError.FETCH_ERROR),
                self.err_index.eq(desc_idx),
            ]

        #
        # Word assembly. The completed-descriptor view substitutes the
        # incoming beat's words for the slots they fill, so validation
        # and the cmd offer see the would-be descriptor combinationally.
        #
        source = reader.source
        would_take = source.valid & ~cp & ~discard
        new_wc = wc + bbw
        would_complete = would_take & (new_wc >= CMD_DESC_WORDS)

        words = []
        for i in range(CMD_DESC_WORDS):
            w = Signal(32, name=f'desc_word_{i}')
            # Select payload independently of ready/error control. Only
            # cmd.valid and the assembly write enable need that gating;
            # feeding it through this mux makes AXI error detection
            # traverse descriptor decode and back into FIFO enables.
            incoming = ~cp & (wc <= i) & (wc + bbw > i)
            picked = Const(0, 32)
            for j in range(bbw):
                picked = Mux(wc == i - j, source.bits.data[32 * j:32 * j + 32],
                             picked)
            m.d.comb += w.eq(Mux(incoming, picked, desc[32 * i:32 * i + 32]))
            words.append(w)

        fits = Const(1)
        for i, (_, bits) in enumerate(shapes):
            if bits < 32:
                fits = fits & (words[i][bits:] == 0)

        opcode = words[0][:op_bits]
        early_end = (opcode == Opcode.END) & (desc_idx != count_latch - 1)
        missing_end = (desc_idx == count_latch - 1) \
            & (opcode != Opcode.END)
        bad_desc = ~fits | early_end | missing_end

        m.d.comb += self.cmd.valid.eq(~discard
                                      & (cp | (would_complete & ~bad_desc)))
        for i, (name, bits) in enumerate(shapes):
            m.d.comb += getattr(self.cmd.bits, name).eq(words[i][:bits])

        # A straddling beat (one that completes a descriptor and feeds
        # the next) is only accepted when the completed descriptor
        # fires this cycle, or when it is faulty and being dropped.
        fire_or_bad = (self.cmd.fire) | (would_complete & bad_desc)
        take = source.valid & (discard
                               | (~cp & ((new_wc <= CMD_DESC_WORDS)
                                         | fire_or_bad)))
        m.d.comb += source.ready.eq(take)
        with m.If(take):
            m.d.sync += taken.eq(taken + beat_bytes)

        with m.If(take & ~discard):
            for j in range(bbw):
                for k in range(CMD_DESC_WORDS):
                    with m.If((wc + j == k) | (wc + j == k + CMD_DESC_WORDS)):
                        m.d.sync += desc[32 * k:32 * k + 32].eq(
                            source.bits.data[32 * j:32 * j + 32])

            with m.If(would_complete):
                with m.If(bad_desc):
                    m.d.sync += [
                        fault.eq(1),
                        self.err_code.eq(WrapperError.DECODE_ERROR),
                        self.err_index.eq(desc_idx),
                    ]
                with m.Else():
                    with m.If(self.cmd.fire):
                        m.d.sync += [
                            wc.eq(new_wc - CMD_DESC_WORDS),
                            desc_idx.eq(desc_idx + 1),
                        ]
                        with m.If(desc_idx + 1 == count_latch):
                            m.d.sync += submitted_all.eq(1)
                    with m.Else():
                        # Exact completion stalling on cmd backpressure.
                        m.d.sync += cp.eq(1)
            with m.Else():
                m.d.sync += wc.eq(new_wc)

        with m.If(cp & self.cmd.fire):
            m.d.sync += [
                cp.eq(0),
                wc.eq(0),
                desc_idx.eq(desc_idx + 1),
            ]
            with m.If(desc_idx + 1 == count_latch):
                m.d.sync += submitted_all.eq(1)

        return m
