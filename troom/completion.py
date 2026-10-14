"""Completion writer for the troom FPGA wrapper.

``CompletionWriter`` collects the core's ``resp`` records and persists
them to device memory as little-endian eight-byte ``{seq, status}``
records in issue order. It reuses
``roomsoc.peripheral.dma.AXIDMAWriter`` for the AXI write side --
burst clipping, 4 KiB splits, WLAST generation, and B response-error
propagation.

Records are written only after the core produced them; bursts are
bounded by the beats actually in hand, so a burst never waits on
records that might not arrive (an aborted launch cannot hang mid-burst).
Backpressure from memory is allowed and records are never dropped: the
record FIFO decouples the core's ``resp`` drain from write bursts, so a
completion stream longer than the FIFO flows through in order.

``COMPLETED_COUNT`` semantics: the writer counts a record only when the
beat carrying it was acknowledged by a successful AXI B response, in
order. After a write error the log is marked incomplete -- the count
freezes below the expected total, further writes stop, and outstanding
transactions drain. A completion whose status is not ``OK`` is reported
(``CMD_FAILED`` with the record's index) but still collected, matching
the core's continue-to-``END`` contract.

``flush`` terminates a launch early (error recovery): the wrapper
asserts it only once no further records can arrive, and the writer
flushes any partial trailing beat, drains, and reports idle. The
success condition, ``drained``, requires every expected record to have
been committed.
"""

from amaranth import *
from amaranth.hdl.rec import Direction, Record
from amaranth.hdl.xfrm import ResetInserter

from troom.abi import COMP_RECORD_BYTES, ERROR_INDEX_NONE, WrapperError
from troom.consts import Status
from troom.core import Completion
from troom.types import HasTRoomParams

from roomsoc.interconnect.axi import AXIInterface
from roomsoc.interconnect.stream import Decoupled, Queue
from roomsoc.peripheral.dma import AXIDMAWriter


class CompletionWriter(HasTRoomParams, Elaboratable):
    """``resp`` stream to device-memory completion log writer."""

    def __init__(self, params, *, fifo_depth=None):
        super().__init__(params)

        if self.data_width < 64:
            raise ValueError(
                f'data_width {self.data_width} must be at least 64 '
                f'to hold a completion record')

        self.fifo_depth = (fifo_depth if fifo_depth is not None else
                           self.dma_fifo_depth * 2)

        self.bus = AXIInterface(data_width=self.data_width,
                                addr_width=self.addr_width,
                                id_width=self.id_width)

        self.sink = Decoupled(Completion, self.params)

        # Launch interface: a start pulse captures the configuration
        # inputs (completion-buffer base and expected record count).
        self.start = Signal()
        self.comp_base = Signal(self.addr_width)
        self.comp_count = Signal(32)

        # Recovery input: no further records will arrive; flush the
        # partial trailing beat, drain, and go idle.
        self.flush = Signal()

        self.active = Signal()
        self.drained = Signal()
        self.idle = Signal()
        self.completed = Signal(32)
        self.err = Signal()
        self.fatal = Signal()
        self.err_code = Signal(WrapperError)
        self.err_index = Signal(32)

    def elaborate(self, platform):
        m = Module()
        lg = self.lg_beat
        beat_bytes = self.beat_bytes
        full_recs = beat_bytes // COMP_RECORD_BYTES
        rec_bits = max(1, full_recs.bit_length())

        engine = AXIDMAWriter(self.bus,
                              max_burst_beats=self.dma_max_burst_beats,
                              max_outstanding=self.dma_max_outstanding,
                              cmd_fifo_depth=self.dma_fifo_depth)

        rec_q = Queue(self.fifo_depth,
                      Record,
                      [('seq', self.seq_bits, Direction.FANOUT),
                       ('status', Shape.cast(Status), Direction.FANOUT)],
                      flow=False)

        beat_q = Queue(max(2, self.dma_max_burst_beats),
                       Record, [('data', self.data_width, Direction.FANOUT),
                                ('strb', beat_bytes, Direction.FANOUT),
                                ('recs', rec_bits, Direction.FANOUT)],
                       flow=False)

        # Records per issued write descriptor, popped on its final B
        # response to advance the committed-record count.
        desc_recs_bits = max(1, (self.dma_max_burst_beats *
                                 full_recs).bit_length())
        tx_q = Queue(self.dma_fifo_depth + self.dma_max_outstanding, Record,
                     [('recs', desc_recs_bits, Direction.FANOUT)])

        # A new launch is permitted only after all issued AXI work drains.
        # Clear abandoned buffered data and DMA metadata at that boundary.
        for name, submodule in (("engine", engine), ("rec_q", rec_q),
                                ("beat_q", beat_q), ("tx_q", tx_q)):
            m.submodules[name] = ResetInserter(self.start)(submodule)

        running = Signal()
        cursor = Signal(self.addr_width)
        count_latch = Signal(32)
        received = Signal(32)
        beats_made = Signal(32)
        beats_sent = Signal(32)
        issued = Signal(32)
        done_cnt = Signal(32)
        committed = Signal(32)
        stopped = Signal()
        flush_latched = Signal()
        err_latched = Signal()

        # Partial trailing beat bookkeeping.
        beat_buf = Signal(self.data_width)
        filled = Signal(range(beat_bytes + 1))
        partial_in = Signal()
        partial_recs = Signal(rec_bits)

        m.d.comb += [
            self.active.eq(running),
            self.completed.eq(committed),
            self.err.eq(err_latched),
            self.fatal.eq(stopped),
            rec_q.enq.valid.eq(self.sink.valid & running & ~stopped),
            rec_q.enq.bits.eq(self.sink.bits),
            self.sink.ready.eq(stopped | (running & rec_q.enq.ready)),
        ]

        with m.If(self.start):
            m.d.sync += [
                running.eq(1),
                cursor.eq(self.comp_base),
                count_latch.eq(self.comp_count),
                received.eq(0),
                beats_made.eq(0),
                beats_sent.eq(0),
                issued.eq(0),
                done_cnt.eq(0),
                committed.eq(0),
                stopped.eq(0),
                flush_latched.eq(0),
                err_latched.eq(0),
                self.err_code.eq(WrapperError.NONE),
                self.err_index.eq(0),
                filled.eq(0),
                beat_buf.eq(0),
                partial_in.eq(0),
            ]
        with m.If(self.flush):
            m.d.sync += flush_latched.eq(1)

        #
        # Record intake. A non-OK status is reported with the record's
        # index but still collected, so the log stays complete.
        #
        with m.If(self.sink.fire):
            m.d.sync += received.eq(received + 1)
            with m.If((self.sink.bits.status != Status.OK) & ~err_latched):
                m.d.sync += [
                    err_latched.eq(1),
                    self.err_code.eq(WrapperError.CMD_FAILED),
                    self.err_index.eq(received),
                ]

        #
        # Beat assembly. A record completing a beat enqueues it
        # immediately; the trailing partial beat is flushed once no
        # more records can arrive.
        #
        rec = rec_q.deq
        completes = (filled + COMP_RECORD_BYTES) == beat_bytes
        # Word placement is a dynamic shift into the low ``filled``
        # bytes; lanes above ``filled`` are stale but always masked by
        # the beat's strobes.
        word_insert = (rec.bits.seq << (filled * 8)) \
            | (rec.bits.status << ((filled + 4) * 8))
        beat_next = Signal(self.data_width)
        m.d.comb += beat_next.eq(beat_buf | word_insert)
        m.d.comb += rec.ready.eq((filled + COMP_RECORD_BYTES <= beat_bytes)
                                 & (~completes | beat_q.enq.ready))

        finishing = (received == count_latch) | flush_latched
        enq_full = rec.fire & completes
        partial_ready = ~stopped & finishing & ~rec.valid & ~enq_full \
            & (filled != 0)

        part_strb = Signal(beat_bytes)
        for k in range(beat_bytes):
            m.d.comb += part_strb[k].eq(filled > k)

        m.d.comb += [
            beat_q.enq.valid.eq(enq_full | partial_ready),
            beat_q.enq.bits.data.eq(Mux(enq_full, beat_next, beat_buf)),
            beat_q.enq.bits.strb.eq(
                Mux(enq_full, Const((1 << beat_bytes) - 1), part_strb)),
            beat_q.enq.bits.recs.eq(Mux(enq_full, full_recs, filled >> 3)),
        ]
        with m.If(rec.fire):
            with m.If(completes):
                # The completed beat was handed to the FIFO; start the
                # next beat from a clean buffer so lanes never
                # accumulate across beats.
                m.d.sync += [beat_buf.eq(0), filled.eq(0)]
            with m.Else():
                m.d.sync += [
                    beat_buf.eq(beat_next),
                    filled.eq(filled + COMP_RECORD_BYTES)
                ]
        with m.If(partial_ready & beat_q.enq.ready):
            m.d.sync += [
                filled.eq(0),
                partial_in.eq(1),
                partial_recs.eq(filled >> 3),
            ]
        with m.If(beat_q.enq.fire):
            m.d.sync += beats_made.eq(beats_made + 1)

        m.d.comb += [
            engine.data.valid.eq(beat_q.deq.valid),
            engine.data.bits.data.eq(beat_q.deq.bits.data),
            engine.data.bits.strb.eq(beat_q.deq.bits.strb),
            beat_q.deq.ready.eq(engine.data.ready),
        ]
        with m.If(beat_q.deq.fire & (beat_q.deq.bits.recs != full_recs)):
            m.d.sync += partial_in.eq(0)

        #
        # Write descriptor issue. One bounded descriptor at a time,
        # covering only beats already formed; the lone partial beat
        # goes out as its own final descriptor. ``partial_in`` stays
        # set until the partial beat streams on W, so qualify it with
        # availability: Amaranth subtraction widens and signs, and an
        # unqualified ``avail - partial_in`` goes to -1 in the window
        # after the partial beat was issued but before it drains.
        avail_total = beats_made - beats_sent
        partial_pending = partial_in & (avail_total != 0)
        full_avail = avail_total - partial_pending
        n_full = Mux(full_avail < self.dma_max_burst_beats, full_avail,
                     self.dma_max_burst_beats)
        issue_len = Mux(n_full == 0, partial_pending, n_full)
        issue_recs = Mux(n_full == 0, partial_recs, n_full * full_recs)

        m.d.comb += [
            engine.sink.valid.eq(running & ~stopped & (issue_len != 0)
                                 & tx_q.enq.ready),
            engine.sink.bits.addr.eq(cursor),
            engine.sink.bits.len.eq(issue_len << lg),
            tx_q.enq.valid.eq(engine.sink.fire),
            tx_q.enq.bits.recs.eq(issue_recs),
        ]
        with m.If(engine.sink.fire):
            m.d.sync += [
                cursor.eq(cursor + (issue_len << lg)),
                beats_sent.eq(beats_sent + issue_len),
                issued.eq(issued + 1),
            ]

        #
        # Completion accounting. A record counts once its beat's B
        # response succeeded; a failing B response stops further
        # writes, freezes the count, and marks the log incomplete.
        #
        m.d.comb += tx_q.deq.ready.eq(engine.done)
        with m.If(engine.done):
            m.d.sync += done_cnt.eq(done_cnt + 1)
            with m.If(~engine.error & ~stopped):
                m.d.sync += committed.eq(committed + tx_q.deq.bits.recs)
        with m.If(engine.error):
            m.d.sync += stopped.eq(1)
            with m.If(~err_latched):
                m.d.sync += [
                    err_latched.eq(1),
                    self.err_code.eq(WrapperError.COMP_ERROR),
                    self.err_index.eq(ERROR_INDEX_NONE),
                ]

        #
        # Run termination. The writer is idle once the record stream
        # has been consumed (or abandoned after a write error) and
        # every issued descriptor has been acknowledged.
        #
        outstanding = issued - done_cnt
        intake_clear = stopped | (~rec.valid & (filled == 0))
        write_clear = (outstanding == 0) & (stopped
                                            | (beats_made == beats_sent))
        finish = (finishing | stopped) & intake_clear & write_clear

        m.d.comb += [
            self.idle.eq(finish),
            self.drained.eq(finish & (received == count_latch)
                            & ~stopped),
        ]
        with m.If(running & finish):
            m.d.sync += running.eq(0)

        return m
