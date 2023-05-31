from amaranth import *

from room.consts import *
from room.types import HasCoreParams, MicroOp

from roomsoc.interconnect.stream import Valid


class IssueQueueWakeup(Record):

    def __init__(self, n_warps, name=None, src_loc_at=0):
        super().__init__([
            ('wid', range(n_warps)),
            ('ldst', range(32)),
        ],
                         name=name,
                         src_loc_at=1 + src_loc_at)


class Scoreboard(HasCoreParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.dis_valid = Signal()
        self.dis_wid = Signal(range(self.n_warps))
        self.dis_uop = MicroOp(params)
        self.dis_ready = Signal()

        self.sb_wid = Signal(range(self.n_warps))
        self.sb_uop = MicroOp(params)

        self.wakeup = Valid(IssueQueueWakeup, self.n_warps)

    def elaborate(self, platform):
        m = Module()

        busy_regs = Array(
            Signal(32, name=f'busy_regs{i}') for i in range(self.n_warps))

        with m.If(self.dis_valid & self.dis_ready & self.dis_uop.ldst_valid):
            with m.Switch(self.dis_uop.ldst):
                for i in range(32):
                    with m.Case(i):
                        m.d.sync += busy_regs[self.dis_wid][i].eq(1)

        with m.If(self.wakeup.valid):
            with m.Switch(self.wakeup.bits.ldst):
                for i in range(32):
                    with m.Case(i):
                        m.d.sync += busy_regs[self.wakeup.bits.wid][i].eq(0)

        rd_busy = Signal()
        rs1_busy = Signal()
        rs2_busy = Signal()
        rs3_busy = Signal()

        for i in range(32):
            wb_valid = self.wakeup.valid & (self.wakeup.bits.wid == self.sb_wid
                                            ) & (self.wakeup.bits.ldst == i)

            with m.If((self.sb_uop.ldst == i) & self.sb_uop.ldst_valid):
                m.d.sync += rd_busy.eq(busy_regs[self.sb_wid][i] & ~wb_valid)

            with m.If((self.sb_uop.lrs1 == i)
                      & (self.sb_uop.lrs1_rtype != RegisterType.X)):
                m.d.sync += rs1_busy.eq(busy_regs[self.sb_wid][i] & ~wb_valid)

            with m.If((self.sb_uop.lrs2 == i)
                      & (self.sb_uop.lrs2_rtype != RegisterType.X)):
                m.d.sync += rs2_busy.eq(busy_regs[self.sb_wid][i] & ~wb_valid)

            with m.If((self.sb_uop.lrs3 == i)
                      & (self.sb_uop.iq_type == IssueQueueType.FP)
                      & self.sb_uop.frs3_en):
                m.d.sync += rs3_busy.eq(busy_regs[self.sb_wid][i] & ~wb_valid)

        m.d.comb += self.dis_ready.eq(~(rd_busy | rs1_busy | rs2_busy
                                        | rs3_busy))

        return m
