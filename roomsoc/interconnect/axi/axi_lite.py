from amaranth import *
from amaranth.hdl.rec import DIR_FANIN, DIR_FANOUT
from amaranth.utils import log2_int


def make_axi_lite_layout(data_width=32, addr_width=32):
    wstrb_width = data_width // 8

    return [
        # write address channel signals
        (
            "aw",
            [
                ("addr", addr_width, DIR_FANOUT),  # write address
                ("prot", 3, DIR_FANOUT),  # protection type
                ("valid", 1, DIR_FANOUT),  # write address valid
                ("ready", 1, DIR_FANIN),  # write address ready
            ]),
        # write data channel signals
        (
            "w",
            [
                ("data", data_width, DIR_FANOUT),  # write data
                ("strb", wstrb_width, DIR_FANOUT),  # write strobes
                ("valid", 1, DIR_FANOUT),  # write valid
                ("ready", 1, DIR_FANIN),  # write ready
            ]),
        # write response channel signals
        (
            "b",
            [
                ("resp", 2, DIR_FANIN),  # write response
                ("valid", 1, DIR_FANIN),  # write response valid
                ("ready", 1, DIR_FANOUT),  # response ready
            ]),
        # read address channel signals
        (
            "ar",
            [
                ("addr", addr_width, DIR_FANOUT),  # read address
                ("prot", 3, DIR_FANOUT),  # protection type
                ("valid", 1, DIR_FANOUT),  # read address valid
                ("ready", 1, DIR_FANIN),  # read address ready
            ]),
        # read data channel signals
        (
            "r",
            [
                ("data", data_width, DIR_FANIN),  # read data
                ("resp", 2, DIR_FANIN),  # read response
                ("valid", 1, DIR_FANIN),  # read valid
                ("ready", 1, DIR_FANOUT),  # read ready
            ]),
    ]


class AXILiteInterface(Record):

    def __init__(self, data_width=32, addr_width=32, name=None):
        self.addr_width = addr_width
        self.data_width = data_width
        super().__init__(make_axi_lite_layout(data_width=data_width,
                                              addr_width=addr_width),
                         name=name)

    def write(self, addr, data, strb=None):
        if strb is None:
            strb = 2**len(self.w.strb) - 1
        # aw + w
        yield self.aw.valid.eq(1)
        yield self.aw.addr.eq(addr)
        yield self.w.data.eq(data)
        yield self.w.valid.eq(1)
        yield self.w.strb.eq(strb)
        yield
        while not (yield self.aw.ready):
            yield
        yield self.aw.valid.eq(0)
        yield self.aw.addr.eq(0)
        while not (yield self.w.ready):
            yield
        yield self.w.valid.eq(0)
        yield self.w.strb.eq(0)
        # b
        yield self.b.ready.eq(1)
        while not (yield self.b.valid):
            yield
        resp = (yield self.b.resp)
        yield self.b.ready.eq(0)
        return resp

    def read(self, addr):
        # ar
        yield self.ar.valid.eq(1)
        yield self.ar.addr.eq(addr)
        yield
        while not (yield self.ar.ready):
            yield
        yield self.ar.valid.eq(0)
        # r
        yield self.r.ready.eq(1)
        while not (yield self.r.valid):
            yield
        data = (yield self.r.data)
        resp = (yield self.r.resp)
        yield self.r.ready.eq(0)
        return (data, resp)


class AXILite2Wishbone(Elaboratable):

    def __init__(self, axi_lite, wishbone, base_addr=0x00000000):
        self.base_addr = base_addr

        self.axi_lite = axi_lite
        self.wishbone = wishbone

    def elaborate(self, platform):
        m = Module()

        axil = self.axi_lite
        wb = self.wishbone

        wb_adr_shift = log2_int(axil.data_width // 8)
        r_addr = Signal(axil.addr_width)
        w_addr = Signal(axil.addr_width)
        data = Signal(axil.data_width)
        last_is_r = Signal()

        m.d.comb += [
            r_addr.eq(axil.ar.addr - self.base_addr),
            w_addr.eq(axil.aw.addr - self.base_addr),
        ]

        with m.FSM():
            with m.State('IDLE'):
                with m.If(axil.ar.valid & axil.aw.valid):
                    with m.If(last_is_r):
                        m.d.sync += last_is_r.eq(0)
                        m.next = 'WRITE'
                    with m.Else():
                        m.d.sync += last_is_r.eq(1)
                        m.next = 'READ'
                with m.Elif(axil.ar.valid):
                    m.d.sync += last_is_r.eq(1)
                    m.next = 'READ'
                with m.Elif(axil.aw.valid):
                    m.d.sync += last_is_r.eq(0)
                    m.next = 'WRITE'
            with m.State('READ'):
                m.d.comb += [
                    wb.stb.eq(1),
                    wb.cyc.eq(1),
                    wb.adr.eq(r_addr[wb_adr_shift:]),
                    wb.sel.eq(~0),
                ]

                with m.If(wb.ack):
                    m.d.comb += axil.ar.ready.eq(1)
                    m.d.sync += data.eq(wb.dat_r)
                    m.next = 'READ_DONE'
            with m.State('READ_DONE'):
                m.d.comb += [
                    axil.r.valid.eq(1),
                    axil.r.resp.eq(0),
                    axil.r.data.eq(data),
                ]

                with m.If(axil.r.ready):
                    m.next = 'IDLE'
            with m.State('WRITE'):
                m.d.comb += [
                    wb.stb.eq(axil.w.valid),
                    wb.cyc.eq(axil.w.valid),
                    wb.we.eq(1),
                    wb.adr.eq(w_addr[wb_adr_shift:]),
                    wb.sel.eq(axil.w.strb),
                    wb.dat_w.eq(axil.w.data),
                ]

                with m.If(wb.ack):
                    m.d.comb += [
                        axil.aw.ready.eq(1),
                        axil.w.ready.eq(1),
                    ]
                    m.next = 'WRITE_DONE'
            with m.State('WRITE_DONE'):
                m.d.comb += [
                    axil.b.valid.eq(1),
                    axil.b.resp.eq(0),
                ]

                with m.If(axil.b.ready):
                    m.next = 'IDLE'

        return m
