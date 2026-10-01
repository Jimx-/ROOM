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

    def __init__(self, is_float, params):
        super().__init__(params)

        self.is_float = is_float

        self.dis_valid = Signal()
        self.dis_wid = Signal(range(self.n_warps))
        self.dis_uop = MicroOp(params)
        self.dis_ready = Signal()

        self.sb_wid = Signal(range(self.n_warps))
        self.sb_uop = MicroOp(params)

        # Per-warp queue heads (the heads that will occupy the queue output
        # buffers after the next edge) and their registered readiness hints.
        self.head_uops = [
            MicroOp(params, name=f'head_uop{i}') for i in range(self.n_warps)
        ]
        self.head_ready = Signal(self.n_warps, reset=(1 << self.n_warps) - 1)

        self.wakeup = Valid(IssueQueueWakeup, self.n_warps)

    def elaborate(self, platform):
        m = Module()

        rtype = RegisterType.FLT if self.is_float else RegisterType.FIX

        busy_regs = Array(
            Signal(32, name=f'busy_regs{i}') for i in range(self.n_warps))
        busy_regs_n = Array(
            Signal(32, name=f'busy_regs_n{i}') for i in range(self.n_warps))
        m.d.comb += Cat(*busy_regs_n).eq(Cat(*busy_regs))

        with m.If(self.dis_valid & self.dis_ready & self.dis_uop.ldst_valid
                  & (self.dis_uop.dst_rtype == rtype)):
            with m.Switch(self.dis_uop.ldst):
                for i in range(32):
                    with m.Case(i):
                        m.d.comb += busy_regs_n[self.dis_wid][i].eq(1)

        with m.If(self.wakeup.valid):
            with m.Switch(self.wakeup.bits.ldst):
                for i in range(32):
                    with m.Case(i):
                        m.d.comb += busy_regs_n[self.wakeup.bits.wid][i].eq(0)

        m.d.sync += Cat(*busy_regs).eq(Cat(*busy_regs_n))

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
                m.d.sync += rd_busy.eq(busy_regs_n[self.sb_wid][i])

            with m.If((self.sb_uop.lrs1 == i)
                      & (self.sb_uop.lrs1_rtype == rtype)):
                m.d.sync += rs1_busy.eq(busy_regs_n[self.sb_wid][i])

            with m.If((self.sb_uop.lrs2 == i)
                      & (self.sb_uop.lrs2_rtype == rtype)):
                m.d.sync += rs2_busy.eq(busy_regs_n[self.sb_wid][i])

            with m.If((self.sb_uop.lrs3 == i)
                      & (self.sb_uop.lrs3_rtype == rtype)):
                m.d.sync += rs3_busy.eq(busy_regs_n[self.sb_wid][i])

        m.d.comb += self.dis_ready.eq(~(rd_busy | rs1_busy | rs2_busy
                                        | rs3_busy))

        # Per-warp readiness hints: registered one cycle ahead from the
        # next-state busy tables. The hint describes the head that will
        # occupy the queue output buffer after the edge, so it is a
        # scheduling hint, not an actual-grant validation.
        for i in range(self.n_warps):
            head = self.head_uops[i]
            head_rd_busy = (head.ldst_valid & (head.dst_rtype == rtype)
                            & busy_regs_n[i].bit_select(head.ldst, 1))
            head_rs1_busy = ((head.lrs1_rtype == rtype)
                             & busy_regs_n[i].bit_select(head.lrs1, 1))
            head_rs2_busy = ((head.lrs2_rtype == rtype)
                             & busy_regs_n[i].bit_select(head.lrs2, 1))
            head_rs3_busy = ((head.lrs3_rtype == rtype)
                             & busy_regs_n[i].bit_select(head.lrs3, 1))
            m.d.sync += self.head_ready[i].eq(~(head_rd_busy | head_rs1_busy
                                                | head_rs2_busy
                                                | head_rs3_busy))

        return m
