from amaranth import *
from amaranth.hdl.rec import DIR_FANIN, DIR_FANOUT
from amaranth.utils import log2_int

from .axi_lite import AXILiteInterface, Wishbone2AXILite


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

    def make_ax_layout(user_width):
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
            ('valid', 1, DIR_FANOUT),
            ('ready', 1, DIR_FANIN),
        ]

        return layout

    aw_layout = make_ax_layout(aw_user_width)
    ar_layout = make_ax_layout(ar_user_width)

    w_layout = [
        ("data", data_width, DIR_FANOUT),  # write data
        ("strb", wstrb_width, DIR_FANOUT),  # write strobes
        ("user", w_user_width, DIR_FANOUT),
        ("last", 1, DIR_FANOUT),
        ("valid", 1, DIR_FANOUT),  # write valid
        ("ready", 1, DIR_FANIN),  # write ready
    ]
    if version == 'axi3':
        w_layout.append(('id', id_width, DIR_FANOUT))

    b_layout = [
        ("resp", 2, DIR_FANIN),  # write response
        ("id", id_width, DIR_FANIN),
        ("user", b_user_width, DIR_FANIN),
        ("valid", 1, DIR_FANIN),  # write response valid
        ("ready", 1, DIR_FANOUT),  # response ready
    ]

    r_layout = [
        ("data", data_width, DIR_FANIN),  # read data
        ("id", id_width, DIR_FANIN),
        ("user", b_user_width, DIR_FANIN),
        ("last", 1, DIR_FANIN),
        ("resp", 2, DIR_FANIN),  # read response
        ("valid", 1, DIR_FANIN),  # read valid
        ("ready", 1, DIR_FANOUT),  # read ready
    ]

    return [
        # write address channel signals
        ("aw", aw_layout),
        # write data channel signals
        ("w", w_layout),
        # write response channel signals
        ("b", b_layout),
        # read address channel signals
        ("ar", ar_layout),
        # read data channel signals
        ("r", r_layout),
    ]


class AXIInterface(Record):

    def __init__(self, data_width=32, addr_width=32, name=None, src_loc_at=1):
        self.addr_width = addr_width
        self.data_width = data_width
        super().__init__(make_axi_layout(data_width=data_width,
                                         addr_width=addr_width),
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
            axi.aw.addr.eq(axi_lite.aw.addr),
            axi.aw.burst.eq(burst_type),
            axi.aw.len.eq(0),
            axi.aw.size.eq(burst_size),
            axi.aw.lock.eq(0),
            axi.aw.prot.eq(self.prot),
            axi.aw.cache.eq(0b0011),
            axi.aw.qos.eq(0),
            axi.aw.id.eq(self.write_id),

            # w
            axi.w.valid.eq(axi_lite.w.valid),
            axi_lite.w.ready.eq(axi.w.ready),
            axi.w.data.eq(axi_lite.w.data),
            axi.w.strb.eq(axi_lite.w.strb),
            axi.w.last.eq(1),

            # b
            axi_lite.b.valid.eq(axi.b.valid),
            axi_lite.b.resp.eq(axi.b.resp),
            axi.b.ready.eq(axi_lite.b.ready),

            # ar
            axi.ar.valid.eq(axi_lite.ar.valid),
            axi_lite.ar.ready.eq(axi.ar.ready),
            axi.ar.addr.eq(axi_lite.ar.addr),
            axi.ar.burst.eq(burst_type),
            axi.ar.len.eq(0),
            axi.ar.size.eq(burst_size),
            axi.ar.lock.eq(0),
            axi.ar.prot.eq(self.prot),
            axi.ar.cache.eq(0b0011),
            axi.ar.qos.eq(0),
            axi.ar.id.eq(self.read_id),

            # r
            axi_lite.r.valid.eq(axi.r.valid),
            axi_lite.r.resp.eq(axi.r.resp),
            axi_lite.r.data.eq(axi.r.data),
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
