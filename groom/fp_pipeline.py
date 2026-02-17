from amaranth import *
import riscvmodel.csrnames as csrnames

from groom.issue import Scoreboard
from groom.regfile import RegisterFile, RegisterRead
from groom.ex_stage import FPUExecUnit
from groom.fu import ExecResp
from groom.csr import CSRAccess, AutoCSR, CSR, BankedCSR, ThreadLocalCSR

from room.consts import *
from room.types import HasCoreParams, MicroOp
from room.utils import Arbiter

from roomsoc.interconnect.stream import Valid, Decoupled


class FPPipeline(HasCoreParams, AutoCSR, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.dis_valid = Signal()
        self.dis_wid = Signal(range(self.n_warps))
        self.dis_uop = MicroOp(params)
        self.dis_ready = Signal()

        self.sb_wid = Signal(range(self.n_warps))
        self.sb_uop = MicroOp(params)
        self.sb_ready = Signal()

        self.int_dis_ready = Signal()
        self.int_sb_ready = Signal()

        self.mem_wb_port = Decoupled(ExecResp, self.flen, params)

        self.from_int = Decoupled(ExecResp, self.flen, params)
        self.to_int = Decoupled(ExecResp, self.xlen, params)
        self.to_lsu = Decoupled(ExecResp, self.xlen, params)

        self.fflags = BankedCSR(ThreadLocalCSR, csrnames.fflags,
                                [('value', 5, CSRAccess.RW)], params)
        self.frm = BankedCSR(CSR, csrnames.frm, [('value', 3, CSRAccess.RW)],
                             params)
        self.fcsr = BankedCSR(ThreadLocalCSR, csrnames.fcsr,
                              [('value', self.xlen, CSRAccess.RW)], params)

    def elaborate(self, platform):
        m = Module()

        #
        # CSR access
        #

        for w in range(self.n_warps):
            for t in range(self.n_threads):
                m.d.comb += self.fcsr.warps[w].r[t].eq(
                    Cat(self.fflags.warps[w].r[t], self.frm.warps[w].r))

                with m.If(self.fflags.warps[w].we[t]):
                    m.d.sync += self.fflags.warps[w].r[t].eq(
                        self.fflags.warps[w].w[t])

                with m.If(self.fcsr.warps[w].we[t]):
                    m.d.sync += Cat(self.fflags.warps[w].r[t],
                                    self.frm.warps[w].r).eq(
                                        self.fcsr.warps[w].w[t])

            with m.If(self.frm.warps[w].we):
                m.d.sync += self.frm.warps[w].r.eq(self.frm.warps[w].w)

        #
        # Issue
        #

        scoreboard = m.submodules.scoreboard = Scoreboard(is_float=True,
                                                          params=self.params)
        m.d.comb += [
            scoreboard.dis_uop.eq(self.dis_uop),
            scoreboard.dis_wid.eq(self.dis_wid),
            scoreboard.sb_uop.eq(self.sb_uop),
            scoreboard.sb_wid.eq(self.sb_wid),
            self.sb_ready.eq(scoreboard.dis_ready),
        ]

        sb_ready = self.int_sb_ready & scoreboard.dis_ready

        #
        # Register read
        #

        fregfile = m.submodules.fregfile = RegisterFile(
            rports=self.n_threads * 3,
            wports=self.n_threads,
            num_regs=32 * self.n_warps,
            data_width=self.flen)

        fregread = m.submodules.fregread = RegisterRead(
            num_rports=self.n_threads * 3,
            rports_array=[3] * self.n_threads,
            reg_width=self.flen,
            params=self.params)

        dis_is_fp = (self.dis_uop.iq_type & IssueQueueType.FP) != 0
        dis_is_int = (self.dis_uop.iq_type &
                      (IssueQueueType.INT | IssueQueueType.MEM)) != 0

        m.d.comb += [
            fregread.dis_valid.eq(self.dis_valid & dis_is_fp
                                  & (~dis_is_int | self.int_dis_ready)
                                  & sb_ready),
            fregread.dis_uop.eq(self.dis_uop),
            fregread.dis_wid.eq(self.dis_wid),
        ]

        dis_ready = (~dis_is_int | self.int_dis_ready) & (~dis_is_fp
                                                          | fregread.dis_ready)

        m.d.comb += [
            scoreboard.dis_valid.eq(self.dis_valid & dis_ready
                                    & self.int_sb_ready),
            self.dis_ready.eq(sb_ready & dis_ready),
        ]

        for irr_rp, rp in zip(fregread.read_ports, fregfile.read_ports):
            m.d.comb += irr_rp.connect(rp)

        #
        # Execute
        #

        exec_unit = m.submodules.exec_unit = FPUExecUnit(self.params)
        m.d.comb += fregread.exec_req.connect(exec_unit.req)

        with m.Switch(exec_unit.req.bits.wid):
            for w in range(self.n_warps):
                with m.Case(w):
                    m.d.comb += exec_unit.frm.eq(self.frm.warps[w].r)

        #
        # Writeback
        #

        wb_arb = m.submodules.wb_arb = Arbiter(3, ExecResp, self.flen,
                                               self.params)
        wb_req = Valid(ExecResp, self.flen, self.params)
        m.d.comb += [
            wb_arb.inp[0].bits.eq(exec_unit.fresp.bits),
            wb_arb.inp[0].valid.eq(exec_unit.fresp.valid),
            self.mem_wb_port.connect(wb_arb.inp[1]),
            self.from_int.connect(wb_arb.inp[2]),
            wb_req.eq(wb_arb.out),
            wb_arb.out.ready.eq(1),
        ]

        m.d.comb += [
            scoreboard.wakeup.valid.eq(
                wb_req.valid & wb_req.bits.uop.rf_wen()
                & (wb_req.bits.uop.dst_rtype == RegisterType.FLT)),
            scoreboard.wakeup.bits.wid.eq(wb_req.bits.wid),
            scoreboard.wakeup.bits.ldst.eq(wb_req.bits.uop.ldst),
        ]

        for i, wp in enumerate(fregfile.write_ports):
            m.d.comb += [
                wp.valid.eq(wb_req.valid & wb_req.bits.uop.rf_wen()
                            & (wb_req.bits.uop.dst_rtype == RegisterType.FLT)
                            & wb_req.bits.uop.tmask[i]),
                wp.bits.addr.eq(Cat(wb_req.bits.uop.ldst, wb_req.bits.wid)),
                wp.bits.data.eq(wb_req.bits.data[i]),
            ]

        fpiu_is_stq = exec_unit.mem_iresp.bits.uop.opcode == UOpCode.STA
        m.d.comb += [
            self.to_int.bits.eq(exec_unit.mem_iresp.bits),
            self.to_int.valid.eq(exec_unit.mem_iresp.valid & ~fpiu_is_stq),
            self.to_lsu.bits.eq(exec_unit.mem_iresp.bits),
            self.to_lsu.valid.eq(exec_unit.mem_iresp.valid & fpiu_is_stq),
            exec_unit.mem_iresp.ready.eq(
                Mux(fpiu_is_stq, self.to_lsu.ready, self.to_int.ready)),
        ]

        # Accrue FP exception flags
        with m.If(exec_unit.fresp.bits.fflags_valid):
            with m.Switch(exec_unit.fresp.bits.wid):
                for w in range(self.n_warps):
                    with m.Case(w):
                        for t in range(self.n_threads):
                            with m.If(exec_unit.fresp.bits.uop.tmask[t]):
                                m.d.sync += self.fflags.warps[w].r[t].eq(
                                    exec_unit.fresp.bits.fflags[t])

        return m
