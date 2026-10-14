"""Tensor DMA for the troom accelerator.

Wraps the linear burst DMA from ``roomsoc.peripheral.dma`` with the 2D
tensor descriptors from the troom plan: a DRAM byte address, a TMEM byte
address, a row byte length, a row count, and per-space row strides.
2D transfers are expanded one row at a time into the linear descriptors
the existing engines accept, so burst splitting, 4 KiB clipping, and
outstanding-transaction management are reused rather than reimplemented.

The initial contract uses aligned, padded device layouts, as the plan
directs: addresses, strides, and row lengths must be non-zero multiples
of the bus beat. Anything else completes with ``BAD_ALIGN`` without
touching the bus or TMEM. Byte-granular row tails are future work.

DRAM addresses are checked against a device memory window
(``mem_base``/``mem_size``, defaulting to the whole address space): a
descriptor whose full 2D extent -- ``dram_addr`` through the end of its
last row -- leaves the window completes with ``BAD_RANGE`` before any
AXI request is issued. The extent is computed with guard bits beyond
the address width, so a stride that would wrap the row cursor around
the address space is caught the same way instead of issuing traffic at
wrapped addresses.

Completion semantics follow the plan:

* a load completes when its final beat is committed into TMEM (there is
  no adapter buffering between the reader and the TMEM write port), and
* a store completes when its final AXI B response arrives.

Bus errors (non-OKAY RRESP/BRESP) and TMEM range overflow are sticky per
command and are reported in the completion status with the precedence
``BAD_ALIGN`` > ``DMA_ERROR`` > ``BAD_RANGE`` > ``OK``. A descriptor that
leaves TMEM range stops issuing rows; rows already in flight still
complete, so TMEM contains exactly the in-range prefix.
"""

from amaranth import *
from amaranth.hdl.rec import Direction, Record

from troom.consts import Status
from troom.types import (
    DMA_DESC_FIELDS,
    DmaDescriptor,
    HasTRoomParams,
    dma_desc_layout,
)

from roomsoc.interconnect.stream import Decoupled, Queue
from roomsoc.peripheral.dma import AXIDMAReader, AXIDMAWriter


def _checked_window(hp, mem_base, mem_size):
    """Validate a device memory window shared by the DMA engines."""
    if mem_size is None:
        mem_size = 1 << hp.addr_width
    if mem_base < 0 or mem_size <= 0:
        raise ValueError('memory window must be non-empty')
    if mem_base + mem_size > 1 << hp.addr_width:
        raise ValueError(
            f'memory window [{mem_base:#x}, {mem_base + mem_size:#x}) '
            f'exceeds the {hp.addr_width}-bit address space')
    return mem_base, mem_size


class DmaCommand(HasTRoomParams, Record):
    """Engine-internal DMA command: a 2D descriptor tagged load or store."""

    def __init__(self, params, name=None, src_loc_at=0):
        HasTRoomParams.__init__(self, params)
        Record.__init__(self, [('store', 1, Direction.FANOUT)] +
                        [(name, width, Direction.FANOUT)
                         for name, width in dma_desc_layout(self)],
                        name=name,
                        src_loc_at=src_loc_at + 1)


def _tmem_stream_ports(p):
    """Decoupled payload layouts shared by the DMA TMEM adapters."""
    return dict(
        write=Decoupled(Record, [('addr', p.tmem_addr_width, Direction.FANOUT),
                                 ('data', p.data_width, Direction.FANOUT)]),
        rreq=Decoupled(Record,
                       [('addr', p.tmem_addr_width, Direction.FANOUT)]),
        rresp=Decoupled(Record, [('data', p.data_width, Direction.FANOUT)]),
    )


class TensorLoader(HasTRoomParams, Elaboratable):
    """LOAD_2D engine: DRAM-to-TMEM fill.

    Expands a 2D descriptor into per-row linear read descriptors and
    steers returned beats into TMEM at
    ``tmem_addr + row * tmem_stride + beat_in_row * beat_bytes``. Because
    the reader completes descriptors in order with a single AXI ID, the
    fill side tracks rows independently of the push side. TMEM addressing
    is checked as rows are pushed: a row that would leave TMEM is never
    requested.
    """

    def __init__(self, params, bus, *, mem_base=0, mem_size=None):
        super().__init__(params)

        self.mem_base, self.mem_size = _checked_window(self, mem_base,
                                                       mem_size)

        self.sink = Decoupled(DmaDescriptor, self.params)
        ports = _tmem_stream_ports(self)
        self.tmem_w = ports['write']

        self.done = Signal()
        self.status = Signal(Status)
        self.active = Signal()
        self.beats = Signal(48)
        self.stall = Signal(48)

        self._bus = bus

    def elaborate(self, platform):
        m = Module()

        reader = m.submodules.reader = AXIDMAReader(
            self._bus,
            max_burst_beats=self.dma_max_burst_beats,
            max_outstanding=self.dma_max_outstanding,
            cmd_fifo_depth=self.dma_fifo_depth,
            data_fifo_depth=self.dma_fifo_depth)

        lg = self.lg_beat
        aw = self.tmem_addr_width

        # Whole-extent DRAM check: the span from the first row to the
        # end of the last row, computed with guard bits beyond the
        # address width so a wrapping stride is detected rather than
        # issued. Every accessed byte lies inside [dram_addr, dram_addr
        # + span), so the bounding extent is exact for unsigned
        # strides.
        ext_bits = self.rows_bits + self.addr_width + 2
        dram_lo = Signal(ext_bits)
        dram_span = Signal(ext_bits)
        m.d.comb += [
            dram_lo.eq(self.sink.bits.dram_addr),
            dram_span.eq((self.sink.bits.rows - 1) *
                         self.sink.bits.dram_stride +
                         self.sink.bits.row_bytes),
        ]
        dram_oob = (dram_lo < self.mem_base) | (
            (dram_lo + dram_span) > self.mem_base + self.mem_size)

        misaligned = ((self.sink.bits.row_bytes == 0) |
                      (self.sink.bits.rows == 0)
                      | self.sink.bits.dram_addr[:lg].any()
                      | self.sink.bits.tmem_addr[:lg].any()
                      | self.sink.bits.row_bytes[:lg].any()
                      | self.sink.bits.dram_stride[:lg].any()
                      | self.sink.bits.tmem_stride[:lg].any())

        dram_cursor = Signal(self.addr_width)
        # TMEM cursors carry two guard bits so a row that would leave the
        # memory is detectable before it is requested.
        push_cursor = Signal(aw + 2)
        fill_cursor = Signal(aw + 2)
        row_bytes = Signal(self.row_bytes_bits)
        beats_per_row = Signal(self.row_bytes_bits)
        rows_total = Signal(self.rows_bits)
        rows_pushed = Signal(self.rows_bits)
        dram_stride = Signal(self.addr_width)
        tmem_stride = Signal(aw)
        fill_beat = Signal(self.row_bytes_bits - lg)
        in_flight = Signal(32)
        push_halt = Signal()
        err_align = Signal()
        err_range = Signal()
        err_bus = Signal()

        with m.FSM() as fsm:
            with m.State('IDLE'):
                m.d.comb += self.sink.ready.eq(1)
                with m.If(self.sink.fire):
                    m.d.sync += [
                        dram_cursor.eq(self.sink.bits.dram_addr),
                        push_cursor.eq(self.sink.bits.tmem_addr),
                        fill_cursor.eq(self.sink.bits.tmem_addr),
                        row_bytes.eq(self.sink.bits.row_bytes),
                        beats_per_row.eq(self.sink.bits.row_bytes >> lg),
                        rows_total.eq(self.sink.bits.rows),
                        rows_pushed.eq(0),
                        dram_stride.eq(self.sink.bits.dram_stride),
                        tmem_stride.eq(self.sink.bits.tmem_stride),
                        fill_beat.eq(0),
                        in_flight.eq(0),
                        push_halt.eq(misaligned | dram_oob),
                        err_align.eq(misaligned),
                        err_range.eq(dram_oob),
                        err_bus.eq(0),
                    ]
                    m.next = 'RUN'

            with m.State('RUN'):
                # Push side: expand one row per accepted descriptor.
                more = (rows_pushed != rows_total) & ~push_halt
                row_end = push_cursor + row_bytes
                range_ok = row_end <= self.tmem_bytes

                m.d.comb += [
                    reader.sink.valid.eq(more & range_ok),
                    reader.sink.bits.addr.eq(dram_cursor),
                    reader.sink.bits.len.eq(row_bytes),
                ]
                with m.If(more & ~range_ok):
                    m.d.sync += [push_halt.eq(1), err_range.eq(1)]
                with m.If(reader.sink.fire):
                    m.d.sync += [
                        dram_cursor.eq(dram_cursor + dram_stride),
                        push_cursor.eq(push_cursor + tmem_stride),
                        rows_pushed.eq(rows_pushed + 1),
                    ]

                # Fill side: steer returned beats into TMEM.
                fill_addr = (fill_cursor + (fill_beat << lg))[:aw]
                m.d.comb += [
                    self.tmem_w.valid.eq(reader.source.valid),
                    self.tmem_w.bits.addr.eq(fill_addr),
                    self.tmem_w.bits.data.eq(reader.source.bits.data),
                    reader.source.ready.eq(self.tmem_w.ready),
                ]
                with m.If(reader.source.fire):
                    with m.If(fill_beat + 1 == beats_per_row):
                        m.d.sync += [
                            fill_beat.eq(0),
                            fill_cursor.eq(fill_cursor + tmem_stride),
                        ]
                    with m.Else():
                        m.d.sync += fill_beat.eq(fill_beat + 1)

                with m.If(reader.error):
                    m.d.sync += err_bus.eq(1)

                # Outstanding-beat accounting. A load retires only when
                # the final beat is committed into TMEM, so track beats
                # requested minus beats consumed.
                with m.If(reader.sink.fire & ~reader.source.fire):
                    m.d.sync += in_flight.eq(in_flight + beats_per_row)
                with m.Elif(~reader.sink.fire & reader.source.fire):
                    m.d.sync += in_flight.eq(in_flight - 1)
                with m.Elif(reader.sink.fire & reader.source.fire):
                    m.d.sync += in_flight.eq(in_flight + beats_per_row - 1)

                push_complete = (rows_pushed == rows_total) | push_halt
                with m.If(push_complete & (in_flight == 0)):
                    m.next = 'DONE'

            with m.State('DONE'):
                m.d.comb += [
                    self.done.eq(1),
                    self.status.eq(
                        Mux(
                            err_align, Status.BAD_ALIGN,
                            Mux(err_bus, Status.DMA_ERROR,
                                Mux(err_range, Status.BAD_RANGE, Status.OK)))),
                ]
                m.next = 'IDLE'

        m.d.comb += self.active.eq(~fsm.ongoing('IDLE'))

        with m.If(fsm.ongoing('RUN') & ~reader.source.fire):
            m.d.sync += self.stall.eq(self.stall + 1)
        with m.If(self.tmem_w.fire):
            m.d.sync += self.beats.eq(self.beats + 1)

        return m


class TensorWriter(HasTRoomParams, Elaboratable):
    """STORE_2D engine: TMEM-to-DRAM drain.

    Expands a 2D descriptor into per-row linear write descriptors,
    streams full-width beats out of TMEM, and completes on the final AXI
    B response. Reads are issued only for rows whose address descriptor
    has been pushed, keeping W beats matched to descriptors in order.
    """

    def __init__(self, params, bus, *, mem_base=0, mem_size=None):
        super().__init__(params)

        self.mem_base, self.mem_size = _checked_window(self, mem_base,
                                                       mem_size)

        self.sink = Decoupled(DmaDescriptor, self.params)
        ports = _tmem_stream_ports(self)
        self.rreq = ports['rreq']
        self.rresp = ports['rresp']

        self.done = Signal()
        self.status = Signal(Status)
        self.active = Signal()
        self.beats = Signal(48)
        self.stall = Signal(48)

        self._bus = bus

    def elaborate(self, platform):
        m = Module()

        axi = m.submodules.writer = AXIDMAWriter(
            self._bus,
            max_burst_beats=self.dma_max_burst_beats,
            max_outstanding=self.dma_max_outstanding,
            cmd_fifo_depth=self.dma_fifo_depth)

        lg = self.lg_beat
        aw = self.tmem_addr_width

        # Whole-extent DRAM check, mirroring the loader: computed with
        # guard bits so a wrapping stride is rejected before any write
        # descriptor is offered.
        ext_bits = self.rows_bits + self.addr_width + 2
        dram_lo = Signal(ext_bits)
        dram_span = Signal(ext_bits)
        m.d.comb += [
            dram_lo.eq(self.sink.bits.dram_addr),
            dram_span.eq((self.sink.bits.rows - 1) *
                         self.sink.bits.dram_stride +
                         self.sink.bits.row_bytes),
        ]
        dram_oob = (dram_lo < self.mem_base) | (
            (dram_lo + dram_span) > self.mem_base + self.mem_size)

        misaligned = ((self.sink.bits.row_bytes == 0) |
                      (self.sink.bits.rows == 0)
                      | self.sink.bits.dram_addr[:lg].any()
                      | self.sink.bits.tmem_addr[:lg].any()
                      | self.sink.bits.row_bytes[:lg].any()
                      | self.sink.bits.dram_stride[:lg].any()
                      | self.sink.bits.tmem_stride[:lg].any())

        dram_cursor = Signal(self.addr_width)
        push_cursor = Signal(aw + 2)
        read_cursor = Signal(aw + 2)
        row_bytes = Signal(self.row_bytes_bits)
        beats_per_row = Signal(self.row_bytes_bits)
        rows_total = Signal(self.rows_bits)
        rows_pushed = Signal(self.rows_bits)
        rows_done = Signal(self.rows_bits)
        dram_stride = Signal(self.addr_width)
        tmem_stride = Signal(aw)
        read_beat = Signal(self.row_bytes_bits - lg)
        issue_row = Signal(self.rows_bits)
        push_halt = Signal()
        err_align = Signal()
        err_range = Signal()
        err_bus = Signal()

        with m.FSM() as fsm:
            with m.State('IDLE'):
                m.d.comb += self.sink.ready.eq(1)
                with m.If(self.sink.fire):
                    m.d.sync += [
                        dram_cursor.eq(self.sink.bits.dram_addr),
                        push_cursor.eq(self.sink.bits.tmem_addr),
                        read_cursor.eq(self.sink.bits.tmem_addr),
                        row_bytes.eq(self.sink.bits.row_bytes),
                        beats_per_row.eq(self.sink.bits.row_bytes >> lg),
                        rows_total.eq(self.sink.bits.rows),
                        rows_pushed.eq(0),
                        rows_done.eq(0),
                        dram_stride.eq(self.sink.bits.dram_stride),
                        tmem_stride.eq(self.sink.bits.tmem_stride),
                        read_beat.eq(0),
                        issue_row.eq(0),
                        push_halt.eq(misaligned | dram_oob),
                        err_align.eq(misaligned),
                        err_range.eq(dram_oob),
                        err_bus.eq(0),
                    ]
                    m.next = 'RUN'

            with m.State('RUN'):
                # Push side: expand one row per accepted descriptor,
                # halting at the first row that would leave TMEM.
                more = (rows_pushed != rows_total) & ~push_halt
                row_end = push_cursor + row_bytes
                range_ok = row_end <= self.tmem_bytes

                m.d.comb += [
                    axi.sink.valid.eq(more & range_ok),
                    axi.sink.bits.addr.eq(dram_cursor),
                    axi.sink.bits.len.eq(row_bytes),
                ]
                with m.If(more & ~range_ok):
                    m.d.sync += [push_halt.eq(1), err_range.eq(1)]
                with m.If(axi.sink.fire):
                    m.d.sync += [
                        dram_cursor.eq(dram_cursor + dram_stride),
                        push_cursor.eq(push_cursor + tmem_stride),
                        rows_pushed.eq(rows_pushed + 1),
                    ]

                # Read side: request beats only for pushed rows, in row
                # order, so the W stream stays matched to descriptors.
                read_addr = (read_cursor + (read_beat << lg))[:aw]
                m.d.comb += [
                    self.rreq.valid.eq(issue_row < rows_pushed),
                    self.rreq.bits.addr.eq(read_addr),
                ]
                with m.If(self.rreq.fire):
                    with m.If(read_beat + 1 == beats_per_row):
                        m.d.sync += [
                            read_beat.eq(0),
                            read_cursor.eq(read_cursor + tmem_stride),
                            issue_row.eq(issue_row + 1),
                        ]
                    with m.Else():
                        m.d.sync += read_beat.eq(read_beat + 1)

                # Data side: forward TMEM beats as full-width W beats.
                m.d.comb += [
                    axi.data.valid.eq(self.rresp.valid),
                    axi.data.bits.data.eq(self.rresp.bits.data),
                    axi.data.bits.strb.eq(Const(1).replicate(self.beat_bytes)),
                    self.rresp.ready.eq(axi.data.ready),
                ]

                with m.If(axi.error):
                    m.d.sync += err_bus.eq(1)
                with m.If(axi.done):
                    m.d.sync += rows_done.eq(rows_done + 1)

                push_complete = (rows_pushed == rows_total) | push_halt
                last_b = axi.done & ((rows_done + 1) == rows_pushed)
                with m.If(push_complete & (last_b | (rows_pushed == 0))):
                    m.next = 'DONE'

            with m.State('DONE'):
                m.d.comb += [
                    self.done.eq(1),
                    self.status.eq(
                        Mux(
                            err_align, Status.BAD_ALIGN,
                            Mux(err_bus, Status.DMA_ERROR,
                                Mux(err_range, Status.BAD_RANGE, Status.OK)))),
                ]
                m.next = 'IDLE'

        m.d.comb += self.active.eq(~fsm.ongoing('IDLE'))

        with m.If(fsm.ongoing('RUN') & ~axi.data.fire):
            m.d.sync += self.stall.eq(self.stall + 1)
        with m.If(axi.data.fire):
            m.d.sync += self.beats.eq(self.beats + 1)

        return m


class DMA(HasTRoomParams, Elaboratable):
    """Tensor DMA front end for the command sequencer.

    ``sink`` accepts ``DmaCommand`` records in order. Loads and stores
    route to independent engines through descriptor queues, so a store
    overlaps a load (subject to the TMEM bank conflict rule) and several
    descriptors can be queued while one streams. ``done`` pulses for one
    cycle when the oldest accepted command finishes its transfer, with
    ``status`` valid alongside; completions therefore preserve
    acceptance order, which the sequencer relies on.

    Counters: ``dram_bytes`` counts beats that crossed the AXI master in
    either direction, and ``stall_cycles`` counts cycles an active
    command moved no beat (bus latency gaps and backpressure alike), as
    the plan's memory-delivery observability requires.
    """

    def __init__(self, params, bus, *, mem_base=0, mem_size=None):
        super().__init__(params)

        self.mem_base, self.mem_size = _checked_window(self, mem_base,
                                                       mem_size)

        self.bus = bus
        self.sink = Decoupled(DmaCommand, self.params)

        ports = _tmem_stream_ports(self)
        self.tmem_w = ports['write']
        self.rreq = ports['rreq']
        self.rresp = ports['rresp']

        self.done = Signal()
        self.status = Signal(Status)
        self.active = Signal()
        self.queued = Signal(48)
        self.dram_bytes = Signal(48)
        self.stall_cycles = Signal(48)

    def elaborate(self, platform):
        m = Module()

        loader = m.submodules.loader = TensorLoader(self.params,
                                                    self.bus,
                                                    mem_base=self.mem_base,
                                                    mem_size=self.mem_size)
        writer = m.submodules.writer = TensorWriter(self.params,
                                                    self.bus,
                                                    mem_base=self.mem_base,
                                                    mem_size=self.mem_size)

        load_q = m.submodules.load_q = Queue(self.dma_queue_depth,
                                             DmaDescriptor, self.params)
        store_q = m.submodules.store_q = Queue(self.dma_queue_depth,
                                               DmaDescriptor, self.params)

        # Retain engine results until their command reaches the head of
        # the acceptance order, including stores that finish before loads.
        order_q = m.submodules.order_q = Queue(
            2 * self.dma_queue_depth + 2, Record,
            [('store', 1, Direction.FANOUT)])

        is_store = self.sink.bits.store

        m.d.comb += [
            load_q.enq.valid.eq(self.sink.valid & ~is_store
                                & order_q.enq.ready),
            store_q.enq.valid.eq(self.sink.valid & is_store
                                 & order_q.enq.ready),
            self.sink.ready.eq(
                Mux(is_store, store_q.enq.ready, load_q.enq.ready)
                & order_q.enq.ready),
            order_q.enq.valid.eq(self.sink.fire),
            order_q.enq.bits.store.eq(is_store),
        ]
        for name in DMA_DESC_FIELDS:
            m.d.comb += [
                getattr(load_q.enq.bits,
                        name).eq(getattr(self.sink.bits, name)),
                getattr(store_q.enq.bits,
                        name).eq(getattr(self.sink.bits, name)),
            ]

        results = []
        for name, commands, engine in [('load', load_q, loader),
                                       ('store', store_q, writer)]:
            result = Queue(2 * self.dma_queue_depth + 2, Record,
                           [('status', Shape.cast(Status), Direction.FANOUT)])
            setattr(m.submodules, name + '_results', result)
            results.append(result)
            # Reserve a result slot before starting an engine command.
            m.d.comb += [
                engine.sink.bits.eq(commands.deq.bits),
                engine.sink.valid.eq(commands.deq.valid & result.enq.ready),
                commands.deq.ready.eq(engine.sink.ready & result.enq.ready),
                result.enq.valid.eq(engine.done),
                result.enq.bits.status.eq(engine.status),
            ]
        load_results, store_results = results

        head_store = order_q.deq.bits.store
        done_sel = order_q.deq.valid & Mux(head_store, store_results.deq.valid,
                                           load_results.deq.valid)

        m.d.comb += [
            self.done.eq(done_sel),
            self.status.eq(
                Mux(head_store, store_results.deq.bits.status,
                    load_results.deq.bits.status)),
            load_results.deq.ready.eq(done_sel & ~head_store),
            store_results.deq.ready.eq(done_sel & head_store),
            order_q.deq.ready.eq(done_sel),
            loader.tmem_w.connect(self.tmem_w),
            writer.rreq.connect(self.rreq),
            self.rresp.connect(writer.rresp),
            self.active.eq(order_q.deq.valid),
            self.queued.eq(load_q.count + store_q.count),
            self.dram_bytes.eq((loader.beats + writer.beats) << self.lg_beat),
            self.stall_cycles.eq(loader.stall + writer.stall),
        ]

        return m
