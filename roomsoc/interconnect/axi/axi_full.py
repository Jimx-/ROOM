from amaranth import *
from amaranth.hdl.rec import DIR_FANIN, DIR_FANOUT
from amaranth.utils import log2_int
from enum import IntEnum

from .axi_lite import AXILiteInterface, Wishbone2AXILite, AXILite2Wishbone
from roomsoc.interconnect.stream import SkidBuffer, Decoupled


class BurstType(IntEnum):
    FIXED = 0b00
    INCR = 0b01
    WRAP = 0b10
    RESERVED = 0b11


def make_ax_layout(addr_width=32, id_width=1, version='axi4', user_width=0):
    len_width = {'axi3': 4, 'axi4': 8}[version]
    size_width = {'axi3': 4, 'axi4': 3}[version]
    lock_width = {'axi3': 2, 'axi4': 1}[version]

    layout = [
        ('addr', addr_width, DIR_FANOUT),
        ('burst', 2, DIR_FANOUT),
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
        ("resp", 2, DIR_FANIN),  # write response
        ("id", id_width, DIR_FANIN),
        ("user", b_user_width, DIR_FANIN),
    ]

    r_layout = [
        ("data", data_width, DIR_FANIN),  # read data
        ("id", id_width, DIR_FANIN),
        ("user", r_user_width, DIR_FANIN),
        ("last", 1, DIR_FANIN),
        ("resp", 2, DIR_FANIN),  # read response
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

        super().__init__(make_axi_layout(data_width=data_width,
                                         addr_width=addr_width,
                                         id_width=id_width),
                         name=name,
                         src_loc_at=src_loc_at)


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
                with m.If((self.in_bus.bits.burst == BurstType.INCR)
                          | (self.in_bus.bits.burst == BurstType.WRAP)):
                    m.d.sync += beat_offset.eq(beat_offset + beat_size)

            with m.If(self.in_bus.bits.burst == BurstType.WRAP):
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


class AXIInterconnectP2P(Elaboratable):

    def __init__(self, master, slave):
        self.master = master
        self.slave = slave

    def elaborate(self, platform):
        m = Module()

        m.d.comb += self.master.connect(self.slave)

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
        self.masters = masters
        self.slaves = slaves

    def elaborate(self, platform):
        m = Module()

        return m
