from amaranth import *
from amaranth.hdl.rec import DIR_FANOUT
from amaranth.lib.fifo import SyncFIFO
from amaranth.utils import log2_int

from .axi_full import AXIInterface
from .. import tilelink

from roomsoc.interconnect.stream import SkidBuffer, Decoupled


class TileLink2AXI(Elaboratable):

    def __init__(self, tl, axi, max_flights=4, prot=0, burst_type='INCR'):
        self.prot = prot
        self.burst_type = burst_type
        self.max_flights = max_flights

        self.tl = tl
        self.axi = axi

        if len(tl.a.bits.source) > len(axi.ar.bits.id):
            raise ValueError(
                "AXI bus has ID width {}, which is smaller than TileLink source ID width {}"
                .format(len(axi.ar.bits.id), len(tl.a.bits.source)))

    @staticmethod
    def get_adapted_interface(tl):
        return AXIInterface(data_width=tl.data_width,
                            addr_width=tl.addr_width,
                            id_width=tl.source_id_width)

    def elaborate(self, platform):
        m = Module()

        tl = self.tl
        axi = self.axi

        ax_layout = list(axi.ar.bits.layout) + [
            ('wen', 1, DIR_FANOUT),
            ('a_size', len(tl.a.bits.size), DIR_FANOUT),
        ]

        a_address = tl.a.bits.address
        a_source = tl.a.bits.source
        a_size = tl.a.bits.size
        a_write = tilelink.Interface.has_data(tl.a.bits)
        _, a_last, _, _ = tilelink.Interface.count(m, tl.a.bits, tl.a.fire)

        ar_ready = Signal()
        aw_ready = Signal()

        out_ax = Decoupled(Record, ax_layout)
        ax_buffer = m.submodules.ax_buffer = SkidBuffer(Record, ax_layout)
        out_w = Decoupled(Record, axi.w.bits.layout)
        w_buffer = m.submodules.w_buffer = SkidBuffer(Record,
                                                      axi.w.bits.layout)
        m.d.comb += [
            out_ax.connect(ax_buffer.enq),
            out_w.connect(w_buffer.enq),
            axi.w.bits.eq(w_buffer.deq.bits),
            axi.w.valid.eq(w_buffer.deq.valid),
            w_buffer.deq.ready.eq(axi.w.ready),
        ]

        m.d.comb += [
            axi.ar.bits.eq(ax_buffer.deq.bits),
            axi.aw.bits.eq(ax_buffer.deq.bits),
            axi.ar.valid.eq(ax_buffer.deq.valid & ~ax_buffer.deq.bits.wen
                            & ar_ready),
            axi.aw.valid.eq(ax_buffer.deq.valid & ax_buffer.deq.bits.wen
                            & aw_ready),
            ax_buffer.deq.ready.eq(
                Mux(ax_buffer.deq.bits.wen, axi.aw.ready & aw_ready,
                    axi.ar.ready & ar_ready)),
        ]

        beat_bytes = axi.data_width // 8
        max_size = log2_int(beat_bytes)
        burst_type = {
            'FIXED': 0b00,
            'INCR': 0b01,
            'WRAP': 0b10
        }[self.burst_type]

        done_aw = Signal()
        with m.If(tl.a.fire):
            m.d.sync += done_aw.eq(~a_last)

        ax = out_ax.bits
        m.d.comb += [
            ax.wen.eq(a_write),
            ax.a_size.eq(a_size),
            ax.id.eq(a_source),
            ax.addr.eq(a_address),
            ax.len.eq(((1 << a_size) >> log2_int(beat_bytes)) - 1),
            ax.size.eq(Mux(a_size >= max_size, max_size, a_size)),
            ax.burst.eq(burst_type),
            ax.prot.eq(self.prot),
            ax.cache.eq(0b0011),
        ]

        m.d.comb += [
            tl.a.ready.eq(
                Mux(a_write, (done_aw | out_ax.ready) & out_w.ready,
                    out_ax.ready)),
            out_ax.valid.eq(tl.a.valid
                            & Mux(a_write, ~done_aw & out_w.ready, 1)),
        ]

        m.d.comb += [
            out_w.valid.eq(tl.a.valid & a_write
                           & (done_aw | out_ax.ready)),
            out_w.bits.data.eq(tl.a.bits.data),
            out_w.bits.strb.eq(tl.a.bits.mask),
            out_w.bits.last.eq(a_last),
        ]

        rqueues = [
            SyncFIFO(depth=self.max_flights, width=len(a_size))
            for _ in range(2**len(a_source))
        ]
        wqueues = [
            SyncFIFO(depth=self.max_flights, width=len(a_size))
            for _ in range(2**len(a_source))
        ]
        m.submodules += rqueues
        m.submodules += wqueues

        with m.Switch(axi.ar.bits.id):
            for i, q in enumerate(rqueues):
                with m.Case(i):
                    m.d.comb += [
                        ar_ready.eq(q.w_rdy),
                        q.w_en.eq(axi.ar.valid & axi.ar.ready),
                        q.w_data.eq(ax_buffer.deq.bits.a_size),
                    ]
        with m.Switch(axi.aw.bits.id):
            for i, q in enumerate(wqueues):
                with m.Case(i):
                    m.d.comb += [
                        aw_ready.eq(q.w_rdy),
                        q.w_en.eq(axi.aw.valid & axi.aw.ready),
                        q.w_data.eq(ax_buffer.deq.bits.a_size),
                    ]

        r_size = Signal.like(tl.a.bits.size)
        b_size = Signal.like(tl.a.bits.size)
        with m.Switch(axi.r.bits.id):
            for i, q in enumerate(rqueues):
                with m.Case(i):
                    m.d.comb += [
                        r_size.eq(q.r_data),
                        q.r_en.eq(axi.r.valid & axi.r.ready & axi.r.bits.last),
                    ]
        with m.Switch(axi.b.bits.id):
            for i, q in enumerate(wqueues):
                with m.Case(i):
                    m.d.comb += [
                        b_size.eq(q.r_data),
                        q.r_en.eq(axi.b.valid & axi.b.ready),
                    ]

        r_holds_d = Signal()
        with m.If(axi.r.valid & axi.r.ready):
            m.d.sync += r_holds_d.eq(~axi.r.bits.last)
        b_delay = Signal(range(8))
        m.d.sync += b_delay.eq(0)
        with m.If(axi.b.valid & ~axi.b.ready):
            m.d.sync += b_delay.eq(b_delay + 1)
        grant_r = (axi.r.valid & (b_delay != 7)) | r_holds_d

        m.d.comb += [
            axi.r.ready.eq(tl.d.ready & grant_r),
            axi.b.ready.eq(tl.d.ready & ~grant_r),
            tl.d.valid.eq(Mux(grant_r, axi.r.valid, axi.b.valid)),
        ]

        r_corrupt = axi.r.bits.resp != 0
        b_denied = axi.b.bits.resp != 0
        with m.If(grant_r):
            m.d.comb += tl.tilelink_access_ack_data(data=0,
                                                    size=r_size,
                                                    source=axi.r.bits.id,
                                                    corrupt=r_corrupt)
        with m.Else():
            m.d.comb += tl.tilelink_access_ack(size=b_size,
                                               source=axi.b.bits.id,
                                               denied=b_denied)
        m.d.comb += tl.d.bits.data.eq(axi.r.bits.data)

        if tl.has_bce:
            m.d.comb += [
                tl.c.ready.eq(1),
                tl.e.ready.eq(1),
            ]

        return m
