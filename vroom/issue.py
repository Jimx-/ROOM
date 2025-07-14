from amaranth import *

from vroom.types import HasVectorParams, VMicroOp

from room.consts import RegisterType

from roomsoc.interconnect.stream import Valid


class IssueQueueWakeup(Record):

    def __init__(self, name=None, src_loc_at=0):
        super().__init__([
            ('ldst', range(32)),
        ],
                         name=name,
                         src_loc_at=1 + src_loc_at)


class Scoreboard(HasVectorParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.dis_valid = Signal()
        self.dis_uop = VMicroOp(params)
        self.dis_ready = Signal()

        self.sb_uop = VMicroOp(params)

        self.wakeup = Valid(IssueQueueWakeup)

    def elaborate(self, platform):
        m = Module()

        rtype = RegisterType.VEC

        busy_regs = Signal(32)
        busy_regs_n = Signal(32)
        m.d.comb += busy_regs_n.eq(busy_regs)

        with m.If(self.dis_valid & self.dis_ready & self.dis_uop.ldst_valid
                  & (self.dis_uop.dst_rtype == rtype)):
            with m.Switch(self.dis_uop.ldst):
                for i in range(32):
                    with m.Case(i):
                        m.d.comb += busy_regs_n[i].eq(1)

        with m.If(self.wakeup.valid):
            with m.Switch(self.wakeup.bits.ldst):
                for i in range(32):
                    with m.Case(i):
                        m.d.comb += busy_regs_n[i].eq(0)

        m.d.sync += busy_regs.eq(busy_regs_n)

        rd_busy = Signal()
        rs1_busy = Signal()
        rs2_busy = Signal()
        rs3_busy = Signal()

        m.d.sync += [
            rd_busy.eq(0),
            rs1_busy.eq(0),
            rs2_busy.eq(0),
            rs3_busy.eq(0),
        ]

        for i in range(32):
            with m.If((self.sb_uop.ldst == i) & self.sb_uop.ldst_valid
                      & (self.sb_uop.dst_rtype == rtype)):
                m.d.sync += rd_busy.eq(busy_regs_n[i])

            with m.If((self.sb_uop.lrs1 == i)
                      & (self.sb_uop.lrs1_rtype == rtype)):
                m.d.sync += rs1_busy.eq(busy_regs_n[i])

            with m.If((self.sb_uop.lrs2 == i)
                      & (self.sb_uop.lrs2_rtype == rtype)):
                m.d.sync += rs2_busy.eq(busy_regs_n[i])

            with m.If((self.sb_uop.ldst == i)
                      & (self.sb_uop.lrs2_rtype == rtype)):
                m.d.sync += rs3_busy.eq(busy_regs_n[i])

        m.d.comb += self.dis_ready.eq(~(rd_busy | rs1_busy | rs2_busy
                                        | rs3_busy))

        return m
