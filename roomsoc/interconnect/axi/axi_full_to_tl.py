from amaranth import *
from amaranth.hdl.rec import DIR_FANOUT
from amaranth.lib.fifo import SyncFIFO
from amaranth.utils import log2_int, bits_for

from .axi_full import AXIInterface
from .common import *
from .. import tilelink

from roomsoc.interconnect.stream import SkidBuffer, Decoupled


class TileLink2AXI(Elaboratable):

    def __init__(self, tl, axi, max_flights=4, prot=0, burst_type='INCR'):
        self.prot = prot
        self.burst_type = burst_type
        self.max_flights = max_flights

        self.tl = tl
        self.axi = axi

        if len(tl.a.bits.source) + tl.has_bce > len(axi.ar.bits.id):
            raise ValueError(
                "AXI bus has ID width {}, which is smaller than required TileLink source ID width {}"
                .format(len(axi.ar.bits.id),
                        len(tl.a.bits.source) + tl.has_bce))

    @staticmethod
    def get_adapted_interface(tl):
        return AXIInterface(data_width=tl.data_width,
                            addr_width=tl.addr_width,
                            id_width=tl.source_id_width + tl.has_bce)

    def elaborate(self, platform):
        m = Module()

        tl = self.tl
        axi = self.axi

        if tl.has_bce:
            tl_adapted = tilelink.Interface(
                addr_width=tl.addr_width,
                data_width=tl.data_width,
                size_width=tl.size_width,
                source_id_width=tl.source_id_width + 1,
                sink_id_width=tl.sink_id_width)
            m.submodules.cache_adapter = tilelink.CacheCork(tl, tl_adapted)
            tl = tl_adapted

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
            ax.len.eq(
                Mux(a_size >= max_size,
                    ((1 << a_size) >> log2_int(beat_bytes)) - 1, 0)),
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


class AXI2Tilelink(Elaboratable):

    def __init__(self, axi, tl, max_flights=1):
        self.max_flights = max_flights

        self.axi = axi
        self.tl = tl

        if len(tl.a.bits.source) < len(
                axi.ar.bits.id) + bits_for(max_flights - 1) + 1:
            raise ValueError(
                "TileLink bus has source ID width {}, which is smaller than required width {}"
                .format(len(tl.a.bits.source),
                        len(axi.ar.bits.id) + bits_for(max_flights - 1) + 1))

    @staticmethod
    def get_adapted_interface(axi, max_flights=1, src_loc_at=0):
        return tilelink.Interface(data_width=axi.data_width,
                                  addr_width=axi.addr_width,
                                  size_width=4,
                                  source_id_width=axi.id_width +
                                  bits_for(max_flights - 1) + 1,
                                  src_loc_at=src_loc_at + 1)

    def elaborate(self, platform):
        m = Module()

        axi = self.axi
        tl = self.tl

        max_bytes = 2**len(axi.ar.bits.len) * 2**len(axi.ar.bits.size)
        log_flights = bits_for(self.max_flights - 1)
        txn_count_bits = bits_for(self.max_flights)
        added_bits = log_flights + 1

        def bytesm1(bits):
            max_shift = 1 << len(bits.size)
            tail = Const((1 << max_shift) - 1, max_shift)
            return (Cat(tail, bits.len) << bits.size) >> max_shift

        out_r = Decoupled(tilelink.ChannelA,
                          addr_width=tl.addr_width,
                          data_width=tl.data_width,
                          size_width=tl.size_width,
                          source_id_width=tl.source_id_width)
        r_size_mask = Signal(range(max_bytes))
        r_size = Signal(bits_for(len(r_size_mask)))
        if self.max_flights > 1:
            r_count = Array(
                Signal(txn_count_bits, name=f'r_count{i}')
                for i in range(2**len(axi.ar.bits.id)))
        r_id = Signal.like(out_r.bits.source)

        m.d.comb += r_size_mask.eq(bytesm1(axi.ar.bits))
        for i in range(len(r_size_mask)):
            with m.If(r_size_mask[i]):
                m.d.comb += r_size.eq(i + 1)

        if self.max_flights == 1:
            m.d.comb += r_id.eq(axi.ar.bits.id << 1)
        else:
            m.d.comb += r_id.eq(
                Cat(Const(0, 1), r_count[axi.ar.bits.id][:log_flights],
                    axi.ar.bits.id))

        m.d.comb += [
            axi.ar.ready.eq(out_r.ready),
            out_r.valid.eq(axi.ar.valid),
            out_r.bits.opcode.eq(tilelink.ChannelAOpcode.Get),
            out_r.bits.address.eq(axi.ar.bits.addr),
            out_r.bits.size.eq(r_size),
            out_r.bits.source.eq(r_id),
        ]

        if self.max_flights > 1:
            with m.Switch(axi.ar.bits.id):
                for i, count in enumerate(r_count):
                    with m.Case(i):
                        with m.If(axi.ar.valid & axi.ar.ready):
                            m.d.sync += count.eq(count + 1)

        out_w = Decoupled(tilelink.ChannelA,
                          addr_width=tl.addr_width,
                          data_width=tl.data_width,
                          size_width=tl.size_width,
                          source_id_width=tl.source_id_width)
        w_size_mask = Signal(range(max_bytes))
        w_size = Signal(bits_for(len(w_size_mask)))
        if self.max_flights > 1:
            w_count = Array(
                Signal(txn_count_bits, name=f'w_count{i}')
                for i in range(2**len(axi.aw.bits.id)))
        w_id = Signal.like(out_w.bits.source)

        m.d.comb += w_size_mask.eq(bytesm1(axi.aw.bits))
        for i in range(len(w_size_mask)):
            with m.If(w_size_mask[i]):
                m.d.comb += w_size.eq(i + 1)

        if self.max_flights == 1:
            m.d.comb += w_id.eq((axi.aw.bits.id << 1) | 1)
        else:
            m.d.comb += w_id.eq(
                Cat(Const(1, 1), w_count[axi.aw.bits.id][:log_flights],
                    axi.aw.bits.id))

        m.d.comb += [
            axi.aw.ready.eq(out_w.ready & axi.w.valid & axi.w.bits.last),
            axi.w.ready.eq(out_w.ready & axi.aw.valid),
            out_w.valid.eq(axi.aw.valid & axi.w.valid),
            out_w.bits.opcode.eq(tilelink.ChannelAOpcode.PutPartialData),
            out_w.bits.address.eq(axi.aw.bits.addr),
            out_w.bits.size.eq(w_size),
            out_w.bits.source.eq(w_id),
            out_w.bits.data.eq(axi.w.bits.data),
            out_w.bits.mask.eq(axi.w.bits.strb),
        ]

        if self.max_flights > 1:
            with m.Switch(axi.aw.bits.id):
                for i, count in enumerate(w_count):
                    with m.Case(i):
                        with m.If(axi.aw.valid & axi.aw.ready):
                            m.d.sync += count.eq(count + 1)

        a_arbiter = m.submodules.a_arbiter = tilelink.Arbiter(
            tilelink.ChannelA,
            addr_width=tl.addr_width,
            data_width=tl.data_width,
            size_width=tl.size_width,
            source_id_width=tl.source_id_width)
        a_arbiter.add(out_r)
        a_arbiter.add(out_w)
        m.d.comb += a_arbiter.bus.connect(tl.a)

        ok_b = Decoupled(Record, axi.b.bits.layout)
        ok_r = Decoupled(Record, axi.r.bits.layout)

        d_resp = Mux(tl.d.bits.denied | tl.d.bits.corrupt, AXIResp.SLVERR,
                     AXIResp.OKAY)
        d_has_data = tilelink.Interface.has_data(tl.d.bits)
        _, d_last, _, _ = tilelink.Interface.count(m, tl.d.bits, tl.d.fire)

        m.d.comb += [
            tl.d.ready.eq(Mux(d_has_data, ok_r.ready, ok_b.ready)),
            ok_r.valid.eq(tl.d.valid & d_has_data),
            ok_b.valid.eq(tl.d.valid & ~d_has_data),
        ]

        m.d.comb += [
            ok_r.bits.id.eq(tl.d.bits.source >> added_bits),
            ok_r.bits.data.eq(tl.d.bits.data),
            ok_r.bits.resp.eq(d_resp),
            ok_r.bits.last.eq(d_last),
        ]

        r_buffer = m.submodules.r_buffer = SkidBuffer(Record,
                                                      axi.r.bits.layout)
        m.d.comb += [
            r_buffer.enq.bits.connect(ok_r.bits),
            r_buffer.enq.valid.eq(ok_r.valid),
            ok_r.ready.eq(r_buffer.enq.ready),
            axi.r.bits.connect(r_buffer.deq.bits),
            axi.r.valid.eq(r_buffer.deq.valid),
            r_buffer.deq.ready.eq(axi.r.ready),
        ]

        m.d.comb += [
            ok_b.bits.id.eq(tl.d.bits.source >> added_bits),
            ok_b.bits.resp.eq(d_resp),
        ]

        b_buffer = m.submodules.b_buffer = SkidBuffer(Record,
                                                      axi.b.bits.layout)
        m.d.comb += [
            b_buffer.enq.bits.connect(ok_b.bits),
            b_buffer.enq.valid.eq(ok_b.valid),
            ok_b.ready.eq(b_buffer.enq.ready),
        ]

        b_allow = 1
        if self.max_flights > 1:
            b_count = Array(
                Signal(txn_count_bits, name=f'b_count{i}')
                for i in range(2**len(axi.aw.bits.id)))
            b_allow = b_count[axi.b.bits.id] != w_count[axi.b.bits.id]

            with m.Switch(axi.b.bits.id):
                for i, count in enumerate(b_count):
                    with m.Case(i):
                        with m.If(axi.b.valid & axi.b.ready):
                            m.d.sync += count.eq(count + 1)

        m.d.comb += [
            axi.b.bits.connect(b_buffer.deq.bits),
            axi.b.valid.eq(b_buffer.deq.valid & b_allow),
            b_buffer.deq.ready.eq(axi.b.ready & b_allow),
        ]

        if tl.has_bce:
            m.d.comb += tl.b.readye.eq(1)

        return m
