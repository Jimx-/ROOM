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

E3-d-e parameter page: a launch whose ``param_base`` input is nonzero
first prefetches one ``PARAM_PAGE_BYTES`` page (one whole reader
descriptor, one memory row per beat) and only then begins command
fetching, so no marker can be resolved against a half-filled page.
Descriptor words at ``CMD_INDIRECT_WORDS`` may carry ``PARAM_WORD_MARK``
plus a slot index. The marker range lies below deployed DRAM, so ordinary
addresses at 0x80000000 remain immediate. Resolution precedes validation
and command submission. Two descriptor banks preserve values across
straddling beats; a shared synchronous page port resolves their fields.
Marker fills queue at a beat's first presentation even while a completed
descriptor's offer is parked, so a fill is never lost and a stale
same-bank page value is never substituted. Width-limited resolved values
still pass through the normal fits check.
"""

from amaranth import *
from amaranth.utils import log2_int

from troom.abi import (
    CMD_DESC_BYTES,
    CMD_DESC_WORDS,
    CMD_INDIRECT_WORDS,
    PARAM_PAGE_BYTES,
    PARAM_SLOT_BITS,
    PARAM_WORD_MARK,
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
        # consumption. The parameter page deliberately breaks the
        # one-chunk rule once per launch: it is one whole 4 KiB reader
        # descriptor issued before any command chunk, while every
        # engine is idle, and the reader still clips it to legal
        # bursts.
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

        # E3-d-e: parameter-page base for this launch (0 = disabled).
        # Captured by the controller at START and held for the run.
        self.param_base = Signal(self.addr_width)

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

        # E3-d-e parameter page: one memory row per beat, written
        # during the launch-start prefetch and read (one resolver at a
        # time) while substituting marker words.
        lg_bbw = log2_int(bbw)
        page_rows = PARAM_PAGE_BYTES // self.beat_bytes
        page = Memory(width=8 * self.beat_bytes,
                      depth=page_rows,
                      name='param_page')
        page_wp = m.submodules.page_wp = page.write_port()
        page_rp = m.submodules.page_rp = page.read_port(domain='sync',
                                                        transparent=False)
        page_mode = Signal()
        page_ready = Signal()
        page_beats = Signal(range(page_rows + 1))

        # E3-d-e marker resolver state (two banks of indirectible word
        # positions; see the fill logic below the word assembly).
        n_resolvers = 2 * len(CMD_INDIRECT_WORDS)
        res_busy = [Signal(name=f'res_busy_{p}') for p in range(n_resolvers)]
        res_slot = [
            Signal(PARAM_SLOT_BITS, name=f'res_slot_{p}')
            for p in range(n_resolvers)
        ]
        res_val = [Signal(32, name=f'res_val_{p}') for p in range(n_resolvers)]
        fill_rd = Signal()
        fill_p = Signal(range(n_resolvers))
        fill_word = Signal(lg_bbw)
        pending_marks = Signal(4)
        presented_stall = Signal()

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
                cursor.eq(
                    Mux(self.param_base != 0, self.param_base, self.cmd_base)),
                count_latch.eq(self.cmd_count),
                remaining.eq(
                    Mux(self.param_base != 0, PARAM_PAGE_BYTES, padded)),
                pushed.eq(0),
                returned.eq(0),
                taken.eq(0),
                wc.eq(0),
                cp.eq(0),
                desc_idx.eq(0),
                submitted_all.eq(0),
                fault.eq(0),
                aborted.eq(0),
                page_mode.eq(self.param_base != 0),
                # page_ready means the page CONTENTS are valid: only
                # the prefetch completion sets it. A page-less launch
                # never resolves markers (they fault through fits).
                page_ready.eq(0),
                page_beats.eq(0),
                fill_rd.eq(0),
                pending_marks.eq(0),
                presented_stall.eq(0),
                self.err_code.eq(WrapperError.NONE),
                self.err_index.eq(0),
            ]
            for idx in range(n_resolvers):
                m.d.sync += res_busy[idx].eq(0)
        with m.If(running & discard & drained):
            m.d.sync += running.eq(0)

        with m.If(self.abort):
            m.d.sync += aborted.eq(1)

        # Fetch request generation: one beat-padded descriptor per
        # reader descriptor, requested only once the previous chunk
        # was fully consumed (which also implies it fully returned).
        # In page mode the one chunk is the whole parameter page.
        chunk = Signal(40)
        m.d.comb += chunk.eq(
            Mux(page_mode, remaining,
                Mux(remaining < self.chunk_bytes, remaining,
                    self.chunk_bytes)))
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

        source = reader.source

        # Page-mode beat consumption: every returned beat is one page
        # row (the prefetch precedes command fetching, so the assembly
        # state stays idle). The last beat switches the stream to the
        # command buffer.
        take_page = page_mode & source.valid & ~discard
        last_page_beat = take_page & (page_beats == page_rows - 1)
        m.d.comb += [
            page_wp.addr.eq(page_beats),
            page_wp.data.eq(source.bits.data),
            page_wp.en.eq(take_page),
        ]
        with m.If(take_page):
            m.d.sync += page_beats.eq(page_beats + 1)
        with m.If(last_page_beat):
            m.d.sync += [
                page_mode.eq(0),
                page_ready.eq(1),
                cursor.eq(self.cmd_base),
                remaining.eq(padded),
                pushed.eq(0),
                taken.eq(0),
                # returned keeps counting reader beats; reset it here
                # (this block dominates the source.fire increment
                # above) so pushed/returned restart together.
                returned.eq(0),
            ]

        #
        # Word assembly. The completed-descriptor view substitutes the
        # incoming beat's words for the slots they fill, so validation
        # and the cmd offer see the would-be descriptor combinationally.
        #
        would_take = source.valid & ~cp & ~discard & ~page_mode
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

        # E3-d-e marker resolvers: one per indirectible word position,
        # filled from the page memory by a shared priority-serialized
        # two-cycle read. A resolver is queued ONCE per presented beat
        # (its new-presentation cycle) for every clean marker word the
        # beat carries -- this descriptor's or a straddling next
        # one's, and also while a completed descriptor's offer is
        # parked (``cp``): the park's completing beat was consumed, so
        # the presenting beat starts the next descriptor at word 0
        # (a parked offer only ever follows an EXACT completion, and
        # one beat can never span two descriptor boundaries), and its
        # fills must queue then -- ``new_present`` is one-shot, so a
        # gate on ``~cp`` would drop them and the view below would
        # later substitute a stale same-bank page value. The fill
        # machine drains independently of the cmd stream, so those
        # fills hide under the park. The pending count is finite (at
        # most three markers per descriptor, at most two descriptors
        # in flight). The cmd offer waits until every pending fill has
        # latched; the resolved word replaces the marker in the view
        # below, so the field-width validation and the cmd payload see
        # the page value -- never the marker. A marker that never
        # resolves (no page, dirty marker bits, a foreign word
        # position) stays raw in the view and faults the launch
        # through the fits check.
        m.d.sync += presented_stall.eq(source.valid & ~source.ready)
        new_present = source.valid & ~presented_stall

        # Landing positions are computed against the effective
        # assembly offset: while an offer is parked the ``wc``
        # register is stale (it still points into the OFFERED
        # descriptor), but the presenting beat starts the next
        # descriptor exactly at its word 0 -- a parked offer only
        # ever follows an EXACT completion -- so the effective
        # offset is the descriptor boundary itself.
        wc_eff = Mux(cp, CMD_DESC_WORDS, wc)

        # Queue terms and the net pending delta: pending_marks must
        # have exactly ONE scheduled assignment per cycle (a queued
        # marker and a completed fill can coincide), so the per-beat
        # queue hits are summed combinationally first.
        queue_terms = []
        for j in range(bbw):
            wj = source.bits.data[32 * j:32 * j + 32]
            clean = (wj[PARAM_SLOT_BITS:] == (
                PARAM_WORD_MARK >> PARAM_SLOT_BITS))
            for idx, p in enumerate(CMD_INDIRECT_WORDS):
                # A straddling beat can resolve the next descriptor's
                # field before the current one retires. Keep both values.
                for bank in range(2):
                    landing = ((wc_eff + j == p) & (desc_idx[0] == bank)) \
                        | ((wc_eff + j == p + CMD_DESC_WORDS) & (desc_idx[0] != bank))
                    hit = new_present & ~page_mode & ~discard \
                        & page_ready & landing & clean
                    queue_terms.append(
                        (hit, bank * len(CMD_INDIRECT_WORDS) + idx,
                         wj[:PARAM_SLOT_BITS]))

        for hit, idx, slot_bits in queue_terms:
            with m.If(hit):
                m.d.sync += [
                    res_busy[idx].eq(1),
                    res_slot[idx].eq(slot_bits),
                ]

        # Issue side: fixed priority over busy resolvers.
        issue_idx = Const(n_resolvers)
        issue_en = Const(0)
        for idx in range(n_resolvers):
            cond = res_busy[idx] & ~fill_rd
            issue_idx = Mux(cond, idx, issue_idx)
            issue_en = issue_en | cond
        issue_slot = res_slot[0]
        for idx in range(n_resolvers - 1, -1, -1):
            issue_slot = Mux(issue_idx == idx, res_slot[idx], issue_slot)
        m.d.comb += [
            page_rp.en.eq(issue_en),
            page_rp.addr.eq(issue_slot[lg_bbw:]),
        ]

        # Net pending-marks update: exactly one assignment per cycle,
        # summing this cycle's queued markers and the completed fill
        # (they can coincide).
        counts = [hit for hit, _, _ in queue_terms]
        while len(counts) > 1:
            counts = [
                counts[i] + counts[i + 1] if i + 1 < len(counts) else counts[i]
                for i in range(0, len(counts), 2)
            ]
        qcnt = counts[0]
        with m.If(fill_rd | (qcnt != 0)):
            m.d.sync += pending_marks.eq(pending_marks + qcnt - fill_rd)

        # Latch side: the page word the cycle after issue (the data
        # register holds the row; the word index rode along).
        with m.If(fill_rd):
            for idx in range(n_resolvers):
                with m.If(fill_p == idx):
                    m.d.sync += res_val[idx].eq(
                        page_rp.data.word_select(fill_word, 32))
            m.d.sync += fill_rd.eq(0)
        with m.If(issue_en):
            m.d.sync += [
                fill_rd.eq(1),
                fill_p.eq(issue_idx),
                fill_word.eq(issue_slot & (bbw - 1)),
            ]
            for idx in range(n_resolvers):
                with m.If(issue_idx == idx):
                    m.d.sync += res_busy[idx].eq(0)

        # A marker on the current beat must block its command offer
        # immediately, before the registered pending count advances.
        fills_pending = fill_rd | (pending_marks != 0) | (qcnt != 0)
        for idx in range(n_resolvers):
            fills_pending = fills_pending | res_busy[idx]

        # Resolved view: a clean marker (reserved tag and slot bits) at an
        # indirectible position reads the resolver's page word. The
        # offer is gated until the fill lands, so a stale res_val can
        # never be presented; a marker anywhere else -- or a dirty
        # marker, or a marker without a page -- stays raw and fails
        # the fits check below.
        view = []
        for i in range(CMD_DESC_WORDS):
            if i in CMD_INDIRECT_WORDS:
                idx = CMD_INDIRECT_WORDS.index(i)
                raw = words[i]
                marker = (raw[PARAM_SLOT_BITS:] == (
                    PARAM_WORD_MARK >> PARAM_SLOT_BITS))
                v = Signal(32, name=f'desc_view_{i}')
                m.d.comb += v.eq(
                    Mux(
                        page_ready & marker,
                        Mux(desc_idx[0],
                            res_val[idx + len(CMD_INDIRECT_WORDS)],
                            res_val[idx]), raw))
                view.append(v)
            else:
                view.append(words[i])

        fits = Const(1)
        for i, (_, bits) in enumerate(shapes):
            if bits < 32:
                fits = fits & (view[i][bits:] == 0)

        opcode = view[0][:op_bits]
        early_end = (opcode == Opcode.END) & (desc_idx != count_latch - 1)
        missing_end = (desc_idx == count_latch - 1) \
            & (opcode != Opcode.END)
        bad_desc = ~fits | early_end | missing_end

        m.d.comb += self.cmd.valid.eq(~discard & ~fills_pending
                                      & (cp | (would_complete & ~bad_desc)))
        for i, (name, bits) in enumerate(shapes):
            m.d.comb += getattr(self.cmd.bits, name).eq(view[i][:bits])

        # A straddling beat (one that completes a descriptor and feeds
        # the next) is only accepted when the completed descriptor
        # fires this cycle, or when it is faulty and being dropped.
        # A pending marker fill blocks both arms: the offer is gated,
        # and the fault decision needs the resolved word.
        fire_or_bad = (self.cmd.fire) \
            | (would_complete & bad_desc & ~fills_pending)
        # Recovery drains outstanding data in either mode without
        # writing the page or advancing descriptor assembly.
        take = source.valid & (discard
                               |
                               (~page_mode & ~cp
                                &
                                ((new_wc < CMD_DESC_WORDS)
                                 |
                                 ((new_wc == CMD_DESC_WORDS) & ~fills_pending)
                                 | fire_or_bad)))
        m.d.comb += source.ready.eq(take | take_page)
        with m.If(take | take_page):
            # Keep the page-to-command switch's counter reset when
            # consuming the last page beat.
            m.d.sync += taken.eq(Mux(last_page_beat, 0, taken + beat_bytes))

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
