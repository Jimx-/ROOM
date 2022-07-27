from amaranth import *

from room.interface import OBI, AXI
from room.core import Core

_iob_layout = [('valid', 1), ('addr', 32), ('wdata', 32), ('wstrb', 4),
               ('rdata', 32), ('ready', 1)]


class OBI2IOB(Elaboratable):

    def __init__(self, obi, iob):
        self.obi = obi
        self.iob = iob

        self.addr = Signal.like(obi.addr)
        self.wdata = Signal.like(obi.wdata)
        self.be = Signal.like(obi.be)
        self.we = Signal.like(obi.be)

    def elaborate(self, platform):
        m = Module()

        with m.FSM():
            with m.State('IDLE'):
                with m.If(self.obi.req == 1):
                    m.d.comb += [
                        self.iob.valid.eq(1),
                        self.iob.addr.eq(self.obi.addr),
                        self.iob.wdata.eq(self.obi.wdata),
                    ]

                    with m.If(self.obi.we):
                        m.d.comb += self.iob.wstrb.eq(self.obi.be)
                    with m.Else():
                        m.d.comb += self.iob.wstrb.eq(0)

                    m.d.sync += [
                        self.addr.eq(self.obi.addr),
                        self.wdata.eq(self.obi.wdata),
                        self.be.eq(self.obi.be),
                        self.we.eq(self.obi.we),
                    ]

                    m.next = 'ACK'
                with m.Else():
                    m.next = 'IDLE'

                m.d.comb += self.obi.gnt.eq(1)

            with m.State('ACK'):
                m.d.comb += [
                    self.iob.valid.eq(1),
                    self.iob.addr.eq(self.addr),
                    self.iob.wdata.eq(self.wdata),
                ]

                with m.If(self.we):
                    m.d.comb += self.iob.wstrb.eq(self.be)
                with m.Else():
                    m.d.comb += self.iob.wstrb.eq(0)

                with m.If(self.iob.ready):
                    m.d.comb += [
                        self.obi.rvalid.eq(1),
                        self.obi.rdata.eq(self.iob.rdata),
                    ]

                    with m.If(self.obi.req == 1):
                        m.d.comb += [
                            self.iob.valid.eq(1),
                            self.iob.addr.eq(self.obi.addr),
                            self.iob.wdata.eq(self.obi.wdata),
                        ]

                        with m.If(self.obi.we):
                            m.d.comb += self.iob.wstrb.eq(self.obi.be)
                        with m.Else():
                            m.d.comb += self.iob.wstrb.eq(0)

                        m.d.sync += [
                            self.addr.eq(self.obi.addr),
                            self.wdata.eq(self.obi.wdata),
                            self.be.eq(self.obi.be),
                            self.we.eq(self.obi.we),
                        ]

                        m.d.comb += self.obi.gnt.eq(1)

                        m.next = 'ACK'
                    with m.Else():
                        m.next = 'IDLE'
                with m.Else():
                    m.next = 'ACK'

        return m


class Wrapper(Elaboratable):

    def __init__(self):
        self.instr_axi = AXI(addr_width=32, data_width=128, id_width=1)
        self.ibus = OBI()

        self.icache_iob = Record(_iob_layout)

        self.icache_params = dict(
            p_WORD_OFF_W=2,
            p_BE_DATA_W=128,
            i_clk=ClockSignal('sync'),
            i_reset=ResetSignal('sync'),

            # Native Bus.
            i_valid=self.icache_iob.valid,
            i_addr=self.icache_iob.addr,
            i_wdata=self.icache_iob.wdata,
            i_wstrb=self.icache_iob.wstrb,
            o_rdata=self.icache_iob.rdata,
            o_ready=self.icache_iob.ready,

            # Slave AXI Bus.
            o_axi_arvalid=self.instr_axi.ar.valid,
            o_axi_araddr=self.instr_axi.ar.addr,
            o_axi_arlen=self.instr_axi.ar.len,
            o_axi_arsize=self.instr_axi.ar.size,
            o_axi_arburst=self.instr_axi.ar.burst,
            o_axi_arlock=self.instr_axi.ar.lock,
            o_axi_arcache=self.instr_axi.ar.cache,
            o_axi_arprot=self.instr_axi.ar.prot,
            o_axi_arqos=self.instr_axi.ar.qos,
            o_axi_arid=self.instr_axi.ar.id,
            i_axi_arready=self.instr_axi.ar.ready,
            i_axi_rvalid=self.instr_axi.r.valid,
            i_axi_rdata=self.instr_axi.r.data,
            i_axi_rresp=self.instr_axi.r.resp,
            i_axi_rlast=self.instr_axi.r.last,
            o_axi_rready=self.instr_axi.r.ready,
            o_axi_awvalid=self.instr_axi.aw.valid,
            o_axi_awaddr=self.instr_axi.aw.addr,
            o_axi_awlen=self.instr_axi.aw.len,
            o_axi_awsize=self.instr_axi.aw.size,
            o_axi_awburst=self.instr_axi.aw.burst,
            o_axi_awlock=self.instr_axi.aw.lock,
            o_axi_awcache=self.instr_axi.aw.cache,
            o_axi_awprot=self.instr_axi.aw.prot,
            o_axi_awqos=self.instr_axi.aw.qos,
            o_axi_awid=self.instr_axi.aw.id,
            i_axi_awready=self.instr_axi.aw.ready,
            o_axi_wvalid=self.instr_axi.w.valid,
            o_axi_wdata=self.instr_axi.w.data,
            o_axi_wstrb=self.instr_axi.w.strb,
            o_axi_wlast=self.instr_axi.w.last,
            i_axi_wready=self.instr_axi.w.ready,
            i_axi_bvalid=self.instr_axi.b.valid,
            i_axi_bresp=self.instr_axi.b.resp,
            o_axi_bready=self.instr_axi.b.ready,
        )

    def elaborate(self, platform):
        m = Module()

        m.submodules.ibus_conv = OBI2IOB(self.ibus, self.icache_iob)
        m.submodules.icache = Instance('iob_cache_axi', **self.icache_params)

        m.submodules.core = Core(self.ibus)

        return m
