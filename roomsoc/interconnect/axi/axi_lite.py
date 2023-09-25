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

    def __init__(self, data_width=32, addr_width=32, name=None, src_loc_at=1):
        self.addr_width = addr_width
        self.data_width = data_width
        super().__init__(make_axi_lite_layout(data_width=data_width,
                                              addr_width=addr_width),
                         name=name,
                         src_loc_at=src_loc_at)

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


class Wishbone2AXILite(Elaboratable):

    def __init__(self, wishbone, axi_lite, base_addr=0x00000000):
        self.base_addr = base_addr

        self.wishbone = wishbone
        self.axi_lite = axi_lite

    def elaborate(self, platform):
        m = Module()

        wb = self.wishbone
        axil = self.axi_lite

        wb_adr_shift = log2_int(axil.data_width // 8)

        cmd_done = Signal()
        data_done = Signal()

        addr = Signal(axil.addr_width)
        m.d.comb += addr.eq((wb.adr << wb_adr_shift) - self.base_addr)

        with m.FSM():
            with m.State('IDLE'):
                m.d.sync += [
                    cmd_done.eq(0),
                    data_done.eq(0),
                ]

                with m.If(wb.stb & wb.cyc):
                    with m.If(wb.we):
                        m.next = 'WRITE'
                    with m.Else():
                        m.next = 'READ'

            with m.State('WRITE'):
                m.d.comb += [
                    axil.aw.valid.eq(~cmd_done),
                    axil.aw.addr.eq(addr),
                ]
                with m.If(axil.aw.valid & axil.aw.ready):
                    m.d.sync += cmd_done.eq(1)

                m.d.comb += [
                    axil.w.valid.eq(~data_done),
                    axil.w.data.eq(wb.dat_w),
                    axil.w.strb.eq(wb.sel),
                ]
                with m.If(axil.w.valid & axil.w.ready):
                    m.d.sync += data_done.eq(1)

                m.d.comb += axil.b.ready.eq(cmd_done & data_done)
                with m.If(axil.b.valid & axil.b.ready):
                    with m.If(axil.b.resp == 0):
                        m.d.comb += wb.ack.eq(1)
                        m.next = 'IDLE'
                    with m.Else():
                        m.next = 'ERROR'

            with m.State('READ'):
                m.d.comb += [
                    axil.ar.valid.eq(~cmd_done),
                    axil.ar.addr.eq(addr),
                ]
                with m.If(axil.ar.valid & axil.ar.ready):
                    m.d.sync += cmd_done.eq(1)

                m.d.comb += axil.r.ready.eq(cmd_done)
                with m.If(axil.r.valid & axil.r.ready):
                    with m.If(axil.r.resp == 0):
                        m.d.comb += [
                            wb.dat_r.eq(axil.r.data),
                            wb.ack.eq(1),
                        ]
                        m.next = 'IDLE'
                    with m.Else():
                        m.next = 'ERROR'

            with m.State('ERROR'):
                m.d.comb += wb.ack.eq(1)

                if hasattr(wb, 'err'):
                    m.d.comb += wb.err.eq(1)

                m.next = 'IDLE'

        return m


class AXILiteUpConverter(Elaboratable):

    def __init__(self, master, slave):
        self.master = master
        self.slave = slave

    def elaborate(self, platform):
        m = Module()

        master = self.master
        slave = self.slave

        dw_from = self.master.data_width
        dw_to = self.slave.data_width
        ratio = dw_to // dw_from
        master_align = log2_int(self.master.data_width // 8)
        slave_align = log2_int(self.slave.data_width // 8)

        wr_word = Signal(range(ratio))
        wr_word_r = Signal.like(wr_word)
        rd_word = Signal(range(ratio))
        rd_word_r = Signal.like(rd_word)

        m.d.comb += [
            master.connect(slave),
            slave.aw.addr.eq(0),
            slave.ar.addr.eq(0),
            slave.w.strb.eq(0),
            slave.w.data.eq(0),
        ]

        m.d.comb += [
            slave.aw.addr[slave_align:].eq(master.aw.addr[slave_align:]),
            slave.ar.addr[slave_align:].eq(master.ar.addr[slave_align:]),
        ]

        with m.If(master.aw.valid):
            m.d.sync += wr_word_r.eq(wr_word)
        with m.If(master.ar.valid):
            m.d.sync += rd_word_r.eq(rd_word)

        m.d.comb += [
            wr_word.eq(
                Mux(master.aw.valid, master.aw.addr[master_align:slave_align],
                    wr_word_r)),
            rd_word.eq(
                Mux(master.ar.valid, master.ar.addr[master_align:slave_align],
                    rd_word_r)),
        ]

        with m.Switch(wr_word):
            for i in range(ratio):
                with m.Case(i):
                    m.d.comb += [
                        slave.w.strb[i * dw_from // 8:(i + 1) * dw_from //
                                     8].eq(master.w.strb),
                        slave.w.data[i * dw_from:(i + 1) * dw_from].eq(
                            master.w.data),
                    ]

        with m.Switch(rd_word):
            for i in range(ratio):
                with m.Case(i):
                    m.d.comb += master.r.data.eq(
                        slave.r.data[i * dw_from:(i + 1) * dw_from])

        return m


class AXILiteConverter(Elaboratable):

    def __init__(self, master, slave):
        self.master = master
        self.slave = slave

    def elaborate(self, platform):
        m = Module()

        dw_from = self.master.data_width
        dw_to = self.slave.data_width

        if dw_from > dw_to:
            raise NotImplementedError(
                "AXI-Lite down converter not implemented")
        elif dw_from < dw_to:
            m.submodules.upconverter = AXILiteUpConverter(
                self.master, self.slave)
        else:
            m.d.comb += self.master.connect(self.slave)

        return m
