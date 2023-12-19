from enum import Enum
from amaranth import *
from amaranth.hdl.rec import DIR_FANIN, DIR_FANOUT
from amaranth.utils import log2_int

from roomsoc.peripheral import Peripheral
from roomsoc.interconnect import wishbone
from roomsoc.interconnect.stream import Decoupled, Valid

import math


class Command(Enum):
    DESELECT = 0b1000
    LOAD_MODE = 0b0000
    AUTO_REFRESH = 0b0001
    PRECHARGE = 0b0010
    ACTIVE = 0b0011
    WRITE = 0b0100
    READ = 0b0101
    STOP = 0b0110
    NOP = 0b0111


class Interface(Record):

    def __init__(self,
                 addr_width,
                 data_width,
                 nr_banks,
                 name=None,
                 src_loc_at=1):

        super().__init__([
            ('a', addr_width, DIR_FANOUT),
            ('ba', range(nr_banks), DIR_FANOUT),
            ('dq_i', data_width, DIR_FANIN),
            ('dq_o', data_width, DIR_FANOUT),
            ('dq_oe', 1, DIR_FANOUT),
            ('cke', 1, DIR_FANOUT),
            ('cs_n', 1, DIR_FANOUT),
            ('ras_n', 1, DIR_FANOUT),
            ('cas_n', 1, DIR_FANOUT),
            ('we_n', 1, DIR_FANOUT),
            ('dqm', 2, DIR_FANOUT),
        ],
                         name=name,
                         src_loc_at=src_loc_at)


class SDRAMPHY(Elaboratable):

    class Request(Record):

        def __init__(self, addr_width, data_width, name=None, src_loc_at=1):
            super().__init__([
                ('addr', addr_width, DIR_FANOUT),
                ('data', data_width, DIR_FANOUT),
                ('we', 1, DIR_FANOUT),
                ('mask', data_width // 8, DIR_FANOUT),
            ],
                             name=name,
                             src_loc_at=src_loc_at)

    def __init__(self,
                 *,
                 clk_freq,
                 addr_width=23,
                 data_width=32,
                 nr_banks=4,
                 sdram_addr_width=13,
                 sdram_data_width=16,
                 sdram_col_width=9,
                 sdram_row_width=13,
                 cas_latency=2,
                 burst_length=2,
                 t_DESL=200000.0,
                 t_MRD=12.0,
                 t_RC=60.0,
                 t_RCD=18.0,
                 t_RP=18.0,
                 t_WR=12.0,
                 t_REFI=7800.0):
        self.clk_freq = clk_freq

        self.addr_width = addr_width
        self.data_width = data_width
        self.nr_banks = nr_banks

        self.sdram_addr_width = sdram_addr_width
        self.sdram_data_width = sdram_data_width
        self.sdram_col_width = sdram_col_width
        self.sdram_row_width = sdram_row_width

        self.cas_latency = cas_latency
        self.burst_length = burst_length

        self.t_DESL = t_DESL
        self.t_MRD = t_MRD
        self.t_RC = t_RC
        self.t_RCD = t_RCD
        self.t_RP = t_RP
        self.t_WR = t_WR
        self.t_REFI = t_REFI

        self.req = Decoupled(SDRAMPHY.Request,
                             addr_width=addr_width,
                             data_width=data_width)

        self.resp = Valid(Signal, data_width)

        self.sdram = Interface(addr_width=sdram_addr_width,
                               data_width=sdram_data_width,
                               nr_banks=nr_banks)

    def elaborate(self, platform):
        m = Module()

        mode_reg = Record([
            ('burst_length', 3),
            ('burst_type', 1),
            ('cas_latency', 3),
            ('op_mode', 2),
            ('wb_mode', 1),
            ('reserved', 3),
        ])
        m.d.comb += [
            mode_reg.burst_length.eq(log2_int(self.burst_length)),
            mode_reg.burst_type.eq(0),
            mode_reg.cas_latency.eq(self.cas_latency),
        ]

        tck = 1.0 / self.clk_freq * 1e9

        init_cycles = math.ceil(self.t_DESL / tck)
        load_mode_cycles = math.ceil(self.t_MRD / tck)
        active_cycles = math.ceil(self.t_RCD / tck)
        refresh_cycles = math.ceil(self.t_RC / tck)
        precharge_cycles = math.ceil(self.t_RC / tck)
        read_cycles = self.cas_latency + self.burst_length
        write_cycles = self.burst_length + math.ceil(
            (self.t_WR + self.t_RP) / tck)

        refresh_interval = math.floor(self.t_REFI / tck) - 10

        wait_counter = Signal(16)
        refresh_counter = Signal(10)
        m.d.sync += [
            wait_counter.eq(wait_counter + 1),
            refresh_counter.eq(refresh_counter + 1),
        ]

        req = SDRAMPHY.Request(addr_width=self.addr_width,
                               data_width=self.data_width)
        with m.If(self.req.fire):
            m.d.sync += [
                req.addr.eq(
                    Cat(
                        Const(
                            0,
                            log2_int(self.data_width //
                                     self.sdram_data_width)),
                        self.req.bits.addr)),
                req.data.eq(self.req.bits.data),
                req.we.eq(self.req.bits.we),
                req.mask.eq(self.req.bits.mask),
            ]

        col = Signal(self.sdram_col_width)
        row = Signal(self.sdram_row_width)
        bank = Signal(range(self.nr_banks))
        m.d.comb += Cat(col, row, bank).eq(req.addr)

        cmd = Signal(Command)
        m.d.sync += cmd.eq(Command.NOP)

        cke = Signal(reset=1)

        with m.FSM():
            with m.State('INIT'):
                m.d.comb += self.sdram.a.eq(0b0010000000000)

                with m.If(wait_counter == 0):
                    m.d.comb += cke.eq(0)
                    m.d.sync += cmd.eq(Command.DESELECT)
                with m.Elif(wait_counter == init_cycles - 1):
                    m.d.sync += cmd.eq(Command.PRECHARGE)
                with m.Elif((wait_counter == init_cycles + precharge_cycles -
                             1) | (wait_counter == init_cycles +
                                   precharge_cycles + refresh_cycles - 1)):
                    m.d.sync += cmd.eq(Command.AUTO_REFRESH)
                with m.Elif(wait_counter == init_cycles + precharge_cycles +
                            2 * refresh_cycles - 1):
                    m.d.sync += [
                        cmd.eq(Command.LOAD_MODE),
                        wait_counter.eq(0),
                    ]
                    m.next = 'MODE'

            with m.State('MODE'):
                m.d.comb += self.sdram.a.eq(mode_reg)

                with m.If(wait_counter == load_mode_cycles - 1):
                    m.next = 'IDLE'

            with m.State('IDLE'):
                with m.If(refresh_counter >= refresh_interval - 1):
                    m.d.sync += [
                        wait_counter.eq(0),
                        cmd.eq(Command.AUTO_REFRESH),
                    ]
                    m.next = 'REFRESH'
                with m.Elif(self.req.valid):
                    m.d.comb += self.req.ready.eq(1)
                    m.d.sync += [
                        cmd.eq(Command.ACTIVE),
                        wait_counter.eq(0),
                    ]
                    m.next = 'ACTIVE'

            with m.State('ACTIVE'):
                m.d.comb += [
                    self.sdram.ba.eq(bank),
                    self.sdram.a.eq(row),
                ]

                with m.If(wait_counter == active_cycles - 1):
                    with m.If(req.we):
                        m.d.sync += [
                            cmd.eq(Command.WRITE),
                            wait_counter.eq(0),
                        ]
                        m.next = 'WRITE'

                    with m.Else():
                        m.d.sync += [
                            cmd.eq(Command.READ),
                            wait_counter.eq(0),
                        ]
                        m.next = 'READ'

            with m.State('READ'):
                m.d.comb += [
                    self.sdram.ba.eq(bank),
                    self.sdram.a.eq(Cat(col, Const(0b0010, 4))),
                ]

                with m.If(wait_counter >= self.cas_latency):
                    with m.Switch(wait_counter - self.cas_latency):
                        for i in range(self.burst_length):
                            with m.Case(i):
                                m.d.sync += self.resp.bits[
                                    i * self.sdram_data_width:(i + 1) *
                                    self.sdram_data_width].eq(self.sdram.dq_i)

                with m.If(wait_counter == read_cycles - 1):
                    m.next = 'READ_RESP'

            with m.State('READ_RESP'):
                m.d.comb += self.resp.valid.eq(1)
                m.next = 'IDLE'

            with m.State('WRITE'):
                m.d.comb += [
                    self.sdram.ba.eq(bank),
                    self.sdram.a.eq(Cat(col, Const(0b0010, 4))),
                ]

                with m.Switch(wait_counter):
                    for i in range(self.burst_length):
                        with m.Case(i):
                            m.d.comb += [
                                self.sdram.dq_oe.eq(1),
                                self.sdram.dq_o.eq(
                                    req.data[i *
                                             self.sdram_data_width:(i + 1) *
                                             self.sdram_data_width]),
                                self.sdram.dqm.eq(
                                    ~req.mask[i *
                                              (self.data_width //
                                               self.sdram_data_width):(i + 1) *
                                              (self.data_width //
                                               self.sdram_data_width)]),
                            ]

                with m.If(wait_counter == write_cycles - 1):
                    m.next = 'IDLE'

            with m.State('REFRESH'):
                with m.If(wait_counter == 0):
                    m.d.sync += refresh_counter.eq(0)
                with m.If(wait_counter == refresh_cycles - 1):
                    m.next = 'IDLE'

        m.d.comb += [
            self.sdram.cke.eq(cke),
            Cat(self.sdram.we_n, self.sdram.cas_n, self.sdram.ras_n,
                self.sdram.cs_n).eq(cmd),
        ]

        return m


class Wishbone2SDRAM(Elaboratable):

    def __init__(self, *, addr_width=23, data_width=32):
        self.addr_width = addr_width
        self.data_width = data_width

        self.wishbone = wishbone.Interface(addr_width=addr_width,
                                           data_width=data_width,
                                           granularity=8)

        self.sdram_req = Decoupled(SDRAMPHY.Request,
                                   addr_width=addr_width,
                                   data_width=data_width)

        self.sdram_resp = Valid(Signal, data_width)

    def elaborate(self, platform):
        m = Module()

        wb = self.wishbone
        req = self.sdram_req
        resp = self.sdram_resp

        with m.FSM():
            with m.State('IDLE'):
                m.d.comb += [
                    req.bits.addr.eq(wb.adr),
                    req.bits.data.eq(wb.dat_w),
                    req.bits.we.eq(wb.we),
                    req.bits.mask.eq(Mux(wb.we, wb.sel, ~0)),
                    req.valid.eq(wb.cyc & wb.stb),
                ]

                with m.If(req.fire):
                    with m.If(wb.we):
                        m.next = 'WRITE'
                    with m.Else():
                        m.next = 'READ'

            with m.State('READ'):
                m.d.comb += [
                    wb.dat_r.eq(resp.bits),
                    wb.ack.eq(resp.valid),
                ]

                with m.If(wb.ack):
                    m.next = 'IDLE'

            with m.State('WRITE'):
                m.d.comb += wb.ack.eq(1)

                m.next = 'IDLE'

        return m


class SDRAMController(Peripheral, Elaboratable):

    def __init__(self,
                 *,
                 clk_freq,
                 addr_width=23,
                 data_width=32,
                 nr_banks=4,
                 sdram_addr_width=13,
                 sdram_data_width=16,
                 name=None):
        super().__init__(name=name)

        self.clk_freq = clk_freq

        self.addr_width = addr_width
        self.data_width = data_width
        self.nr_banks = nr_banks

        self._ram_bus = self.window(addr_width=addr_width,
                                    data_width=data_width,
                                    granularity=8,
                                    addr=0)

        self.sdram = Interface(addr_width=sdram_addr_width,
                               data_width=sdram_data_width,
                               nr_banks=nr_banks)

        self._bridge = self.bridge(data_width=32, granularity=8, alignment=2)
        self.bus = self._bridge.bus

    def elaborate(self, platform):
        m = Module()
        m.submodules.bridge = self._bridge

        phy = m.submodules.phy = SDRAMPHY(clk_freq=self.clk_freq,
                                          addr_width=self.addr_width,
                                          data_width=self.data_width,
                                          nr_banks=self.nr_banks)

        wb2sdram = m.submodules.wb2sdram = Wishbone2SDRAM()
        m.d.comb += [
            self._ram_bus.connect(wb2sdram.wishbone),
            wb2sdram.sdram_req.connect(phy.req),
            wb2sdram.sdram_resp.eq(phy.resp),
            phy.sdram.connect(self.sdram),
        ]

        return m
