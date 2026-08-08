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


class AXILiteDownConverter(Elaboratable):
    """Split each wide AXI-Lite access into narrow AXI-Lite accesses."""

    def __init__(self, master, slave):
        if master.addr_width != slave.addr_width:
            raise ValueError("AXI-Lite address widths must match")
        if master.data_width <= slave.data_width:
            raise ValueError("Master must be wider than slave")
        if master.data_width % slave.data_width:
            raise ValueError("AXI-Lite width ratio must be integral")
        ratio = master.data_width // slave.data_width
        if ratio & (ratio - 1):
            raise ValueError("AXI-Lite width ratio must be a power of two")

        self.master = master
        self.slave = slave

    def elaborate(self, platform):
        m = Module()

        master = self.master
        slave = self.slave
        dw_from = master.data_width
        dw_to = slave.data_width
        slave_bytes = dw_to // 8
        ratio = dw_from // dw_to

        # AXI-Lite has no transfer-size signal. As in LiteX, a wide access is
        # defined as `ratio` consecutive narrow accesses starting at the exact
        # address supplied by the wide master. Write lanes whose strobes are
        # zero are skipped; reads always fetch and assemble every lane.

        # Write conversion. Capture AW and W independently, since AXI-Lite does
        # not require the two channels to arrive in the same cycle.
        wr_aw_hold = Signal()
        wr_addr_cur = Signal(master.addr_width)
        wr_prot = Signal(3)
        wr_w_hold = Signal()
        wr_data = Signal(dw_from)
        wr_strb = Signal(dw_from // 8)
        wr_lane = Signal(range(ratio))
        wr_aw_sent = Signal()
        wr_w_sent = Signal()
        wr_resp = Signal(2)

        wr_lane_data = Signal(dw_to)
        wr_lane_strb = Signal(slave_bytes)
        m.d.comb += [
            wr_lane_data.eq(wr_data.word_select(wr_lane, dw_to)),
            wr_lane_strb.eq(wr_strb.word_select(wr_lane, slave_bytes)),
        ]

        with m.FSM(name="axil_down_write"):
            with m.State("IDLE"):
                m.d.comb += [
                    master.aw.ready.eq(~wr_aw_hold),
                    master.w.ready.eq(~wr_w_hold),
                ]
                aw_fire = master.aw.valid & master.aw.ready
                w_fire = master.w.valid & master.w.ready
                with m.If(aw_fire):
                    m.d.sync += [
                        wr_aw_hold.eq(1),
                        wr_addr_cur.eq(master.aw.addr),
                        wr_prot.eq(master.aw.prot),
                    ]
                with m.If(w_fire):
                    m.d.sync += [
                        wr_w_hold.eq(1),
                        wr_data.eq(master.w.data),
                        wr_strb.eq(master.w.strb),
                    ]
                with m.If((wr_aw_hold | aw_fire) & (wr_w_hold | w_fire)):
                    m.d.sync += [
                        wr_lane.eq(0),
                        wr_aw_sent.eq(0),
                        wr_w_sent.eq(0),
                        wr_resp.eq(0),
                    ]
                    m.next = "CONVERT"

            with m.State("CONVERT"):
                with m.If(wr_lane_strb == 0):
                    with m.If(wr_lane == ratio - 1):
                        m.next = "RESPOND"
                    with m.Else():
                        m.d.sync += [
                            wr_lane.eq(wr_lane + 1),
                            wr_addr_cur.eq(wr_addr_cur + slave_bytes),
                        ]
                with m.Else():
                    m.d.comb += [
                        slave.aw.valid.eq(~wr_aw_sent),
                        slave.aw.addr.eq(wr_addr_cur),
                        slave.aw.prot.eq(wr_prot),
                        slave.w.valid.eq(~wr_w_sent),
                        slave.w.data.eq(wr_lane_data),
                        slave.w.strb.eq(wr_lane_strb),
                    ]
                    slave_aw_fire = slave.aw.valid & slave.aw.ready
                    slave_w_fire = slave.w.valid & slave.w.ready
                    with m.If(slave_aw_fire):
                        m.d.sync += wr_aw_sent.eq(1)
                    with m.If(slave_w_fire):
                        m.d.sync += wr_w_sent.eq(1)
                    with m.If((wr_aw_sent | slave_aw_fire)
                              & (wr_w_sent | slave_w_fire)):
                        m.next = "WAIT_RESP"

            with m.State("WAIT_RESP"):
                m.d.comb += slave.b.ready.eq(1)
                with m.If(slave.b.valid):
                    with m.If((wr_resp == 0) & (slave.b.resp != 0)):
                        m.d.sync += wr_resp.eq(slave.b.resp)
                    with m.If(wr_lane == ratio - 1):
                        m.next = "RESPOND"
                    with m.Else():
                        m.d.sync += [
                            wr_lane.eq(wr_lane + 1),
                            wr_addr_cur.eq(wr_addr_cur + slave_bytes),
                            wr_aw_sent.eq(0),
                            wr_w_sent.eq(0),
                        ]
                        m.next = "CONVERT"

            with m.State("RESPOND"):
                m.d.comb += [
                    master.b.valid.eq(1),
                    master.b.resp.eq(wr_resp),
                ]
                with m.If(master.b.ready):
                    m.d.sync += [
                        wr_aw_hold.eq(0),
                        wr_w_hold.eq(0),
                    ]
                    m.next = "IDLE"

        # Read conversion. Each narrow response is placed in its corresponding
        # wide data lane, and the first non-OKAY response is retained.
        rd_addr_cur = Signal(master.addr_width)
        rd_prot = Signal(3)
        rd_lane = Signal(range(ratio))
        rd_data = Signal(dw_from)
        rd_resp = Signal(2)

        with m.FSM(name="axil_down_read"):
            with m.State("IDLE"):
                m.d.comb += master.ar.ready.eq(1)
                with m.If(master.ar.valid):
                    m.d.sync += [
                        rd_addr_cur.eq(master.ar.addr),
                        rd_prot.eq(master.ar.prot),
                        rd_lane.eq(0),
                        rd_data.eq(0),
                        rd_resp.eq(0),
                    ]
                    m.next = "CONVERT"

            with m.State("CONVERT"):
                m.d.comb += [
                    slave.ar.valid.eq(1),
                    slave.ar.addr.eq(rd_addr_cur),
                    slave.ar.prot.eq(rd_prot),
                ]
                with m.If(slave.ar.ready):
                    m.next = "WAIT_RESP"

            with m.State("WAIT_RESP"):
                m.d.comb += slave.r.ready.eq(1)
                with m.If(slave.r.valid):
                    m.d.sync += rd_data.word_select(rd_lane,
                                                    dw_to).eq(slave.r.data)
                    with m.If((rd_resp == 0) & (slave.r.resp != 0)):
                        m.d.sync += rd_resp.eq(slave.r.resp)
                    with m.If(rd_lane == ratio - 1):
                        m.next = "RESPOND"
                    with m.Else():
                        m.d.sync += [
                            rd_lane.eq(rd_lane + 1),
                            rd_addr_cur.eq(rd_addr_cur + slave_bytes),
                        ]
                        m.next = "CONVERT"

            with m.State("RESPOND"):
                m.d.comb += [
                    master.r.valid.eq(1),
                    master.r.data.eq(rd_data),
                    master.r.resp.eq(rd_resp),
                ]
                with m.If(master.r.ready):
                    m.next = "IDLE"

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
            m.submodules.downconverter = AXILiteDownConverter(
                self.master, self.slave)
        elif dw_from < dw_to:
            m.submodules.upconverter = AXILiteUpConverter(
                self.master, self.slave)
        else:
            m.d.comb += self.master.connect(self.slave)

        return m
