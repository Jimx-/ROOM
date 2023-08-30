from amaranth import *
from amaranth.hdl.rec import DIR_FANIN, DIR_FANOUT
from amaranth.utils import log2_int
from amaranth_soc.memory import MemoryMap

from .axi_lite import AXILiteInterface, Wishbone2AXILite, AXILite2Wishbone
from .common import *
from roomsoc.interconnect.stream import SkidBuffer, Decoupled


def make_ax_layout(addr_width=32, id_width=1, version='axi4', user_width=0):
    len_width = {'axi3': 4, 'axi4': 8}[version]
    size_width = {'axi3': 4, 'axi4': 3}[version]
    lock_width = {'axi3': 2, 'axi4': 1}[version]

    layout = [
        ('addr', addr_width, DIR_FANOUT),
        ('burst', AXIBurst, DIR_FANOUT),
        ('len', len_width, DIR_FANOUT),
        ('size', size_width, DIR_FANOUT),
        ('lock', lock_width, DIR_FANOUT),
        ('prot', 3, DIR_FANOUT),
        ('cache', 4, DIR_FANOUT),
        ('qos', 4, DIR_FANOUT),
        ('region', 4, DIR_FANOUT),
        ('id', id_width, DIR_FANOUT),
        ('user', user_width, DIR_FANOUT),
    ]

    return layout


def make_axi_layout(data_width=32,
                    addr_width=32,
                    id_width=1,
                    version='axi4',
                    aw_user_width=0,
                    w_user_width=0,
                    b_user_width=0,
                    ar_user_width=0,
                    r_user_width=0):
    wstrb_width = data_width // 8

    aw_layout = make_ax_layout(addr_width, id_width, version, aw_user_width)
    ar_layout = make_ax_layout(addr_width, id_width, version, ar_user_width)

    w_layout = [
        ("data", data_width, DIR_FANOUT),  # write data
        ("strb", wstrb_width, DIR_FANOUT),  # write strobes
        ("user", w_user_width, DIR_FANOUT),
        ("last", 1, DIR_FANOUT),
    ]
    if version == 'axi3':
        w_layout.append(('id', id_width, DIR_FANOUT))

    b_layout = [
        ("resp", AXIResp, DIR_FANIN),  # write response
        ("id", id_width, DIR_FANIN),
        ("user", b_user_width, DIR_FANIN),
    ]

    r_layout = [
        ("data", data_width, DIR_FANIN),  # read data
        ("id", id_width, DIR_FANIN),
        ("user", r_user_width, DIR_FANIN),
        ("last", 1, DIR_FANIN),
        ("resp", AXIResp, DIR_FANIN),  # read response
    ]

    return [
        # write address channel signals
        ("aw", [
            ("bits", aw_layout),
            ("valid", 1, DIR_FANOUT),
            ("ready", 1, DIR_FANIN),
        ]),
        # write data channel signals
        (
            "w",
            [
                ("bits", w_layout),
                ("valid", 1, DIR_FANOUT),  # write valid
                ("ready", 1, DIR_FANIN),  # write ready
            ]),
        # write response channel signals
        (
            "b",
            [
                ("bits", b_layout),
                ("valid", 1, DIR_FANIN),  # write response valid
                ("ready", 1, DIR_FANOUT),  # response ready
            ]),
        # read address channel signals
        ("ar", [
            ("bits", ar_layout),
            ("valid", 1, DIR_FANOUT),
            ("ready", 1, DIR_FANIN),
        ]),
        # read data channel signals
        (
            "r",
            [
                ("bits", r_layout),
                ("valid", 1, DIR_FANIN),  # read valid
                ("ready", 1, DIR_FANOUT),  # read ready
            ]),
    ]


class AXIInterface(Record):

    def __init__(self,
                 data_width=32,
                 addr_width=32,
                 id_width=1,
                 name=None,
                 src_loc_at=1):
        self.addr_width = addr_width
        self.data_width = data_width
        self.id_width = id_width
        self._map = None

        super().__init__(make_axi_layout(data_width=data_width,
                                         addr_width=addr_width,
                                         id_width=id_width),
                         name=name,
                         src_loc_at=src_loc_at)

    @property
    def memory_map(self):
        if self._map is None:
            raise NotImplementedError(
                "Bus interface {!r} does not have a memory map".format(self))
        return self._map

    @memory_map.setter
    def memory_map(self, memory_map):
        if not isinstance(memory_map, MemoryMap):
            raise TypeError(
                "Memory map must be an instance of MemoryMap, not {!r}".format(
                    memory_map))
        if memory_map.addr_width != max(1, self.addr_width):
            raise ValueError(
                "Memory map has address width {}, which is not the same as bus "
                "interface address width {}".format(memory_map.addr_width,
                                                    self.addr_width))
        memory_map.freeze()
        self._map = memory_map


class AXILite2AXI(Elaboratable):

    def __init__(self,
                 axi_lite,
                 axi,
                 write_id=0,
                 read_id=0,
                 prot=0,
                 burst_type='INCR'):

        self.write_id = write_id
        self.read_id = read_id
        self.prot = prot
        self.burst_type = burst_type

        self.axi_lite = axi_lite
        self.axi = axi

    def elaborate(self, platform):
        m = Module()

        axi_lite = self.axi_lite
        axi = self.axi

        burst_size = log2_int(axi.data_width // 8)
        burst_type = {
            'FIXED': 0b00,
            'INCR': 0b01,
            'WRAP': 0b10
        }[self.burst_type]

        m.d.comb += [
            # aw
            axi.aw.valid.eq(axi_lite.aw.valid),
            axi_lite.aw.ready.eq(axi.aw.ready),
            axi.aw.bits.addr.eq(axi_lite.aw.addr),
            axi.aw.bits.burst.eq(burst_type),
            axi.aw.bits.len.eq(0),
            axi.aw.bits.size.eq(burst_size),
            axi.aw.bits.lock.eq(0),
            axi.aw.bits.prot.eq(self.prot),
            axi.aw.bits.cache.eq(0b0011),
            axi.aw.bits.qos.eq(0),
            axi.aw.bits.id.eq(self.write_id),

            # w
            axi.w.valid.eq(axi_lite.w.valid),
            axi_lite.w.ready.eq(axi.w.ready),
            axi.w.bits.data.eq(axi_lite.w.data),
            axi.w.bits.strb.eq(axi_lite.w.strb),
            axi.w.bits.last.eq(1),

            # b
            axi_lite.b.valid.eq(axi.b.valid),
            axi_lite.b.resp.eq(axi.b.bits.resp),
            axi.b.ready.eq(axi_lite.b.ready),

            # ar
            axi.ar.valid.eq(axi_lite.ar.valid),
            axi_lite.ar.ready.eq(axi.ar.ready),
            axi.ar.bits.addr.eq(axi_lite.ar.addr),
            axi.ar.bits.burst.eq(burst_type),
            axi.ar.bits.len.eq(0),
            axi.ar.bits.size.eq(burst_size),
            axi.ar.bits.lock.eq(0),
            axi.ar.bits.prot.eq(self.prot),
            axi.ar.bits.cache.eq(0b0011),
            axi.ar.bits.qos.eq(0),
            axi.ar.bits.id.eq(self.read_id),

            # r
            axi_lite.r.valid.eq(axi.r.valid),
            axi_lite.r.resp.eq(axi.r.bits.resp),
            axi_lite.r.data.eq(axi.r.bits.data),
            axi.r.ready.eq(axi_lite.r.ready),
        ]

        return m


class Wishbone2AXI(Elaboratable):

    def __init__(self, wishbone, axi, base_addr=0x00000000):
        self.base_addr = base_addr

        self.wishbone = wishbone
        self.axi = axi

    def elaborate(self, platform):
        m = Module()

        wb = self.wishbone
        axi = self.axi

        axil = AXILiteInterface(data_width=axi.data_width,
                                addr_width=axi.addr_width)

        m.submodules.wb2axil = Wishbone2AXILite(wb,
                                                axil,
                                                base_addr=self.base_addr)
        m.submodules.axil2axi = AXILite2AXI(axil, axi)

        return m


class AXIFragmenter(Elaboratable):

    def __init__(self, in_bus):
        self.in_bus = in_bus

        ax_layout = make_ax_layout(addr_width=len(in_bus.bits.addr),
                                   id_width=len(in_bus.bits.id))
        self.out_bus = Decoupled(Record, ax_layout + [('last', 1, DIR_FANOUT)])

    def elaborate(self, platform):
        m = Module()

        beat_count = Signal(8)
        beat_size = Signal(8 + 4)
        beat_offset = Signal(8 + 4 + 1)
        beat_wrap = Signal(8 + 4)
        beat_last = beat_count == self.in_bus.bits.len

        m.d.comb += [
            beat_size.eq(1 << self.in_bus.bits.size),
            beat_wrap.eq(self.in_bus.bits.len << self.in_bus.bits.size),
        ]

        m.d.comb += [
            self.out_bus.valid.eq(self.in_bus.valid | (beat_count != 0)),
            self.out_bus.bits.addr.eq(self.in_bus.bits.addr + beat_offset),
            self.out_bus.bits.id.eq(self.in_bus.bits.id),
            self.out_bus.bits.last.eq(beat_last),
        ]

        with m.If(self.out_bus.ready & beat_last):
            m.d.comb += self.in_bus.ready.eq(1)

        with m.If(self.out_bus.fire):
            with m.If(beat_last):
                m.d.sync += [
                    beat_count.eq(0),
                    beat_offset.eq(0),
                ]
            with m.Else():
                m.d.sync += beat_count.eq(beat_count + 1)
                with m.If((self.in_bus.bits.burst == AXIBurst.INCR)
                          | (self.in_bus.bits.burst == AXIBurst.WRAP)):
                    m.d.sync += beat_offset.eq(beat_offset + beat_size)

            with m.If(self.in_bus.bits.burst == AXIBurst.WRAP):
                with m.If((self.out_bus.bits.addr & beat_wrap) == beat_wrap):
                    m.d.sync += beat_offset.eq(beat_offset - beat_wrap)

        return m


class AXI2AXILite(Elaboratable):

    def __init__(self, axi, axi_lite):
        self.axi = axi
        self.axi_lite = axi_lite

    def elaborate(self, platform):
        m = Module()

        axi = self.axi
        axil = self.axi_lite

        ax_layout = make_ax_layout(addr_width=axi.addr_width,
                                   id_width=axi.id_width)
        ax_buffer = m.submodules.ax_buffer = SkidBuffer(Record, ax_layout)
        fragmenter = m.submodules.fragmenter = AXIFragmenter(ax_buffer.deq)

        cmd_done = Signal()
        last_ar = Signal()

        m.d.comb += axil.b.ready.eq(1)

        with m.FSM():
            with m.State('IDLE'):
                m.d.sync += cmd_done.eq(0)

                with m.If(axi.ar.valid & axi.aw.valid):
                    with m.If(last_ar):
                        m.d.comb += [
                            axi.aw.bits.connect(ax_buffer.enq.bits),
                            ax_buffer.enq.valid.eq(axi.aw.valid),
                            axi.aw.ready.eq(ax_buffer.enq.ready),
                        ]
                        m.d.sync += last_ar.eq(0)
                        m.next = 'WRITE'

                    with m.Else():
                        m.d.comb += [
                            axi.ar.bits.connect(ax_buffer.enq.bits),
                            ax_buffer.enq.valid.eq(axi.ar.valid),
                            axi.ar.ready.eq(ax_buffer.enq.ready),
                        ]
                        m.d.sync += last_ar.eq(1)
                        m.next = 'READ'
                with m.Elif(axi.ar.valid):
                    m.d.comb += [
                        axi.ar.bits.connect(ax_buffer.enq.bits),
                        ax_buffer.enq.valid.eq(axi.ar.valid),
                        axi.ar.ready.eq(ax_buffer.enq.ready),
                    ]
                    m.d.sync += last_ar.eq(1)
                    m.next = 'READ'
                with m.Elif(axi.aw.valid):
                    m.d.comb += [
                        axi.aw.bits.connect(ax_buffer.enq.bits),
                        ax_buffer.enq.valid.eq(axi.aw.valid),
                        axi.aw.ready.eq(ax_buffer.enq.ready),
                    ]
                    m.d.sync += last_ar.eq(0)
                    m.next = 'WRITE'

            with m.State('READ'):
                m.d.comb += [
                    axil.ar.valid.eq(fragmenter.out_bus.valid & ~cmd_done),
                    axil.ar.addr.eq(fragmenter.out_bus.bits.addr),
                    fragmenter.out_bus.ready.eq(axil.ar.ready & ~cmd_done),
                ]
                with m.If(fragmenter.out_bus.valid
                          & fragmenter.out_bus.bits.last):
                    with m.If(axil.ar.ready):
                        m.d.comb += fragmenter.out_bus.ready.eq(0)
                        m.d.sync += cmd_done.eq(1)

                m.d.comb += [
                    axi.r.valid.eq(axil.r.valid),
                    axi.r.bits.last.eq(cmd_done),
                    axi.r.bits.resp.eq(0),
                    axi.r.bits.id.eq(fragmenter.out_bus.bits.id),
                    axi.r.bits.data.eq(axil.r.data),
                    axil.r.ready.eq(axi.r.ready),
                ]

                with m.If(axi.r.valid & axi.r.ready & axi.r.bits.last):
                    m.d.comb += fragmenter.out_bus.ready.eq(1)
                    m.next = 'IDLE'

            with m.State('WRITE'):
                m.d.comb += [
                    axil.aw.valid.eq(fragmenter.out_bus.valid & ~cmd_done),
                    axil.aw.addr.eq(fragmenter.out_bus.bits.addr),
                    fragmenter.out_bus.ready.eq(axil.aw.ready & ~cmd_done),
                ]
                with m.If(fragmenter.out_bus.valid
                          & fragmenter.out_bus.bits.last):
                    with m.If(axil.aw.ready):
                        m.d.comb += fragmenter.out_bus.ready.eq(0)
                        m.d.sync += cmd_done.eq(1)

                m.d.comb += [
                    axil.w.valid.eq(axi.w.valid),
                    axil.w.data.eq(axi.w.bits.data),
                    axil.w.strb.eq(axi.w.bits.strb),
                    axi.w.ready.eq(axil.w.ready),
                ]

                with m.If(axi.w.valid & axi.w.ready & axi.w.bits.last):
                    m.next = 'RESP'

            with m.State('RESP'):
                m.d.comb += [
                    axi.b.valid.eq(1),
                    axi.b.bits.resp.eq(0),
                    axi.b.bits.id.eq(fragmenter.out_bus.bits.id),
                ]
                with m.If(axi.b.ready):
                    m.d.comb += fragmenter.out_bus.ready.eq(1)
                    m.next = 'IDLE'

        return m


class AXI2Wishbone(Elaboratable):

    def __init__(self, axi, wishbone, base_addr=0x00000000):
        self.base_addr = base_addr

        self.axi = axi
        self.wishbone = wishbone

    def elaborate(self, platform):
        m = Module()

        wb = self.wishbone
        axi = self.axi

        axil = AXILiteInterface(data_width=axi.data_width,
                                addr_width=axi.addr_width)

        m.submodules.axi2axil = AXI2AXILite(axi, axil)
        m.submodules.axil2wishbone = AXILite2Wishbone(axil,
                                                      wb,
                                                      base_addr=self.base_addr)

        return m


class _RequestCounter(Elaboratable):

    def __init__(self, max_requests=256):
        self.max_requests = max_requests

        self.req = Signal()
        self.resp = Signal()
        self.ready = Signal()
        self.stall = Signal()

    def elaborate(self, platform):
        m = Module()

        counter = Signal(range(self.max_requests))
        full = counter == self.max_requests - 1
        empty = counter == 0
        m.d.comb += [
            self.ready.eq(empty),
            self.stall.eq(self.req & full),
        ]

        with m.If(self.req & self.resp):
            m.d.sync += counter.eq(counter)
        with m.Elif(self.req & ~full):
            m.d.sync += counter.eq(counter + 1)
        with m.Elif(self.resp & ~empty):
            m.d.sync += counter.eq(counter - 1)

        return m


class AXIArbiter(Elaboratable):

    def __init__(self, *, addr_width, data_width, id_width):
        self.bus = AXIInterface(addr_width=addr_width,
                                data_width=data_width,
                                id_width=id_width)
        self._intrs = []

    def add(self, intr_bus):
        if not isinstance(intr_bus, AXIInterface):
            raise TypeError(
                "Initiator bus must be an instance of AXIInterface, not {!r}".
                format(intr_bus))
        if intr_bus.addr_width != self.bus.addr_width:
            raise ValueError(
                "Initiator bus has address width {}, which is not the same as "
                "arbiter address width {}".format(intr_bus.addr_width,
                                                  self.bus.addr_width))
        if intr_bus.data_width != self.bus.data_width:
            raise ValueError(
                "Initiator bus has data width {}, which is not the same as "
                "arbiter data width {}".format(intr_bus.data_width,
                                               self.bus.data_width))

        self._intrs.append(intr_bus)

    def elaborate(self, platform):
        m = Module()

        rd_requests = Signal(len(self._intrs))
        rd_grant = Signal(range(len(self._intrs)))
        rd_early_grant = Signal.like(rd_grant)
        m.d.comb += [
            rd_requests.eq(Cat(intr_bus.ar.valid for intr_bus in self._intrs)),
            rd_early_grant.eq(rd_grant),
        ]
        rd_lock = m.submodules.rd_lock = _RequestCounter()
        m.d.comb += [
            rd_lock.req.eq(self.bus.ar.valid & self.bus.ar.ready),
            rd_lock.resp.eq(self.bus.r.valid & self.bus.r.ready
                            & self.bus.r.bits.last),
        ]

        with m.If(rd_lock.ready):
            with m.Switch(rd_grant):
                for i in range(len(rd_requests)):
                    with m.Case(i):
                        with m.If(rd_requests[i]):
                            m.d.comb += rd_early_grant.eq(i)

                        for pred in reversed(range(i)):
                            with m.If(rd_requests[pred]):
                                m.d.comb += rd_early_grant.eq(pred)

                                with m.If(rd_lock.req):
                                    m.d.sync += rd_grant.eq(pred)

                        for succ in reversed(range(i + 1, len(rd_requests))):
                            with m.If(rd_requests[succ]):
                                m.d.comb += rd_early_grant.eq(succ)

                                with m.If(rd_lock.req):
                                    m.d.sync += rd_grant.eq(succ)

        wr_requests = Signal(len(self._intrs))
        wr_grant = Signal(range(len(self._intrs)))
        wr_early_grant = Signal.like(wr_grant)
        m.d.comb += [
            wr_requests.eq(Cat(intr_bus.aw.valid for intr_bus in self._intrs)),
            wr_early_grant.eq(wr_grant),
        ]
        wr_lock = m.submodules.wr_lock = _RequestCounter()
        m.d.comb += [
            wr_lock.req.eq(self.bus.aw.valid & self.bus.aw.ready),
            wr_lock.resp.eq(self.bus.b.valid & self.bus.b.ready),
        ]

        with m.If(wr_lock.ready):
            with m.Switch(wr_grant):
                for i in range(len(wr_requests)):
                    with m.Case(i):
                        with m.If(wr_requests[i]):
                            m.d.comb += wr_early_grant.eq(i)

                        for pred in reversed(range(i)):
                            with m.If(wr_requests[pred]):
                                m.d.comb += wr_early_grant.eq(pred)

                                with m.If(wr_lock.req):
                                    m.d.sync += wr_grant.eq(pred)

                        for succ in reversed(range(i + 1, len(wr_requests))):
                            with m.If(wr_requests[succ]):
                                m.d.comb += wr_early_grant.eq(succ)

                                with m.If(wr_lock.req):
                                    m.d.sync += wr_grant.eq(succ)

        with m.Switch(rd_early_grant):
            for i, intr_bus in enumerate(self._intrs):
                with m.Case(i):
                    m.d.comb += [
                        intr_bus.ar.connect(self.bus.ar),
                        intr_bus.r.connect(self.bus.r),
                    ]

        with m.Switch(wr_early_grant):
            for i, intr_bus in enumerate(self._intrs):
                with m.Case(i):
                    m.d.comb += [
                        intr_bus.aw.connect(self.bus.aw),
                        intr_bus.w.connect(self.bus.w),
                        intr_bus.b.connect(self.bus.b),
                    ]

        return m


class AXIDecoder(Elaboratable):

    def __init__(self,
                 *,
                 addr_width,
                 data_width,
                 id_width,
                 alignment=0,
                 name=None):
        self.data_width = data_width
        self.id_width = id_width
        self.alignment = alignment

        self._map = MemoryMap(addr_width=max(1, addr_width),
                              data_width=8,
                              alignment=alignment,
                              name=name)
        self._subs = dict()
        self._bus = None

    @property
    def bus(self):
        if self._bus is None:
            self._map.freeze()
            self._bus = AXIInterface(addr_width=self._map.addr_width,
                                     data_width=self.data_width,
                                     id_width=self.id_width)
            self._bus.memory_map = self._map
        return self._bus

    def align_to(self, alignment):
        return self._map.align_to(alignment)

    def add(self, sub_bus, *, addr=None, sparse=False, extend=False):
        if not isinstance(sub_bus, AXIInterface):
            raise TypeError(
                "Subordinate bus must be an instance of AXIInterface, not {!r}"
                .format(sub_bus))
        if not sparse:
            if sub_bus.data_width != self.data_width:
                raise ValueError(
                    "Subordinate bus has data width {}, which is not the same as "
                    "decoder data width {} (required for dense address translation)"
                    .format(sub_bus.data_width, self.data_width))

        self._subs[sub_bus.memory_map] = sub_bus
        return self._map.add_window(sub_bus.memory_map,
                                    addr=addr,
                                    sparse=sparse,
                                    extend=extend)

    def elaborate(self, platform):
        m = Module()

        rd_sel_dec = Signal(len(self._subs))
        rd_sel_reg = Signal(len(self._subs))
        rd_sel = Signal(len(self._subs))

        wr_sel_dec = Signal(len(self._subs))
        wr_sel_reg = Signal(len(self._subs))
        wr_sel = Signal(len(self._subs))

        with m.Switch(self.bus.ar.bits.addr):
            for i, (_, (sub_pat, _)) in enumerate(self._map.window_patterns()):
                with m.Case(sub_pat):
                    m.d.comb += rd_sel_dec[i].eq(1)

        with m.Switch(self.bus.aw.bits.addr):
            for i, (_, (sub_pat, _)) in enumerate(self._map.window_patterns()):
                with m.Case(sub_pat):
                    m.d.comb += wr_sel_dec[i].eq(1)

        rd_lock = m.submodules.rd_lock = _RequestCounter()
        m.d.comb += [
            rd_lock.req.eq(self.bus.ar.valid & self.bus.ar.ready),
            rd_lock.resp.eq(self.bus.r.valid & self.bus.r.ready
                            & self.bus.r.bits.last),
        ]
        wr_lock = m.submodules.wr_lock = _RequestCounter()
        m.d.comb += [
            wr_lock.req.eq(self.bus.aw.valid & self.bus.aw.ready),
            wr_lock.resp.eq(self.bus.b.valid & self.bus.b.ready),
        ]

        with m.If(rd_lock.ready):
            m.d.sync += rd_sel_reg.eq(rd_sel_dec)
        with m.If(wr_lock.ready):
            m.d.sync += wr_sel_reg.eq(wr_sel_dec)

        m.d.comb += [
            rd_sel.eq(Mux(rd_lock.ready, rd_sel_dec, rd_sel_reg)),
            wr_sel.eq(Mux(wr_lock.ready, wr_sel_dec, wr_sel_reg)),
        ]

        for i, (sub_map, (_, _)) in enumerate(self._map.window_patterns()):
            sub_bus = self._subs[sub_map]

            with m.If(rd_sel[i]):
                m.d.comb += [
                    self.bus.ar.connect(sub_bus.ar),
                    self.bus.r.connect(sub_bus.r),
                ]

            with m.If(wr_sel[i]):
                m.d.comb += [
                    self.bus.aw.connect(sub_bus.aw),
                    self.bus.w.connect(sub_bus.w),
                    self.bus.b.connect(sub_bus.b),
                ]

        return m


class AXIInterconnectP2P(Elaboratable):

    def __init__(self, master, slave):
        self.master = master
        self.slave = slave

    def elaborate(self, platform):
        m = Module()

        m.d.comb += self.master.connect(self.slave)

        return m


def _check_parameter(intrs, param_fn):
    param = param_fn(intrs[0])
    if len(intrs) > 1:
        for intr in intrs[1:]:
            param = max(param, param_fn(intr))

    return param


class AXIIDWidthConverter(Elaboratable):

    def __init__(self, in_bus, out_bus):
        self.in_bus = in_bus
        self.out_bus = out_bus

    def elaborate(self, platform):
        m = Module()

        in_id_width = self.in_bus.id_width
        out_id_width = self.out_bus.id_width

        m.d.comb += self.in_bus.connect(self.out_bus)

        if in_id_width > out_id_width:
            rd_lock = m.submodules.rd_lock = _RequestCounter()
            m.d.comb += [
                rd_lock.req.eq(self.in_bus.ar.valid & self.in_bus.ar.ready),
                rd_lock.resp.eq(self.in_bus.r.valid & self.in_bus.r.ready
                                & self.in_bus.r.bits.last),
            ]
            wr_lock = m.submodules.wr_lock = _RequestCounter()
            m.d.comb += [
                wr_lock.req.eq(self.in_bus.aw.valid & self.in_bus.aw.ready),
                wr_lock.resp.eq(self.in_bus.b.valid & self.in_bus.b.ready),
            ]

            rid = Signal(in_id_width)
            wid = Signal(in_id_width)

            with m.If(self.in_bus.ar.valid & self.in_bus.ar.ready):
                m.d.sync += rid.eq(self.in_bus.ar.bits.id)
            with m.If(self.in_bus.aw.valid & self.in_bus.aw.ready):
                m.d.sync += wid.eq(self.in_bus.aw.bits.id)

            m.d.comb += [
                self.in_bus.ar.ready.eq(self.out_bus.ar.ready & rd_lock.ready),
                self.in_bus.r.bits.id.eq(rid),
                self.in_bus.aw.ready.eq(self.out_bus.aw.ready & wr_lock.ready),
                self.in_bus.b.bits.id.eq(wid),
            ]

        return m


class AXIInterconnectShared(Elaboratable):

    def __init__(self,
                 addr_width,
                 data_width,
                 masters,
                 slaves,
                 timeout_cycles=None):
        self.addr_width = addr_width
        self.data_width = data_width
        self.id_width = _check_parameter(intrs=masters,
                                         param_fn=lambda intr: intr.id_width)
        self.masters = masters
        self.slaves = slaves

    def elaborate(self, platform):
        m = Module()

        arbiter = m.submodules.arbiter = AXIArbiter(data_width=self.data_width,
                                                    addr_width=self.addr_width,
                                                    id_width=self.id_width)
        for master in self.masters:
            arbiter.add(master)
        shared = arbiter.bus

        decoder = m.submodules.decoder = AXIDecoder(data_width=self.data_width,
                                                    addr_width=self.addr_width,
                                                    id_width=self.id_width)
        for region, slave in self.slaves:
            if slave.id_width != self.id_width:
                adapted_bus = AXIInterface(addr_width=slave.addr_width,
                                           data_width=slave.data_width,
                                           id_width=self.id_width)
                adapted_bus.memory_map = slave.memory_map

                iw_converter = AXIIDWidthConverter(adapted_bus, slave)
                m.submodules += iw_converter
                slave = adapted_bus

            decoder.add(slave, addr=region.origin)
        m.d.comb += shared.connect(decoder.bus)

        return m
