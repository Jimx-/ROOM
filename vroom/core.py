from amaranth import *

from vroom.types import HasVectorParams
from vroom.if_stage import IFStage
from vroom.id_stage import DecodeStage, VOpExpander
from vroom.dispatch import Dispatcher
from vroom.issue import Scoreboard
from vroom.regfile import RegisterRead, RegisterFile
from vroom.ex_stage import ALUExecUnit

from room.branch import BranchUpdate
from room.fu import ExecReq, ExecResp
from room.csr import CSR, AutoCSR, CSRAccess
from room.utils import Decoupled


def vtype_layout(xlen):
    return [
        ("vlmul_mag", 2, CSRAccess.RO),
        ("vlmul_sign", 1, CSRAccess.RO),
        ("vsew", 3, CSRAccess.RO),
        ("vta", 1, CSRAccess.RO),
        ("vma", 1, CSRAccess.RO),
        ("_rsvd", xlen - 9, CSRAccess.RO),
        ("vill", 1, CSRAccess.RO),
    ]


class VectorUnit(HasVectorParams, AutoCSR, Elaboratable):

    def __init__(self, params, sim_debug=False):
        super().__init__(params)

        self.sim_debug = sim_debug

        self.req = Decoupled(ExecReq, self.xlen, params)
        self.resp = Decoupled(ExecResp, self.xlen, params)

        self.rob_head_idx = Signal(range(self.core_width * self.num_rob_rows))
        self.rob_pnr_idx = Signal(range(self.core_width * self.num_rob_rows))

        self.br_update = BranchUpdate(params)
        self.exception = Signal()

        self.vl = CSR(0xC20, [('value', self.xlen, CSRAccess.RO)])
        self.vtype = CSR(0xC21, vtype_layout(self.xlen))
        self.vlenb = CSR(0xC22, [('value', self.xlen, CSRAccess.RO)])

    def elaborate(self, platform):
        m = Module()

        #
        # Instruction fetch
        #

        if_stage = m.submodules.if_stage = IFStage(self.params)
        m.d.comb += [
            self.req.connect(if_stage.req),
            if_stage.resp.connect(self.resp),
            if_stage.rob_head_idx.eq(self.rob_head_idx),
            if_stage.rob_pnr_idx.eq(self.rob_pnr_idx),
            if_stage.br_update.eq(self.br_update),
            if_stage.exception.eq(self.exception),
        ]

        #
        # CSR
        #

        m.d.comb += [
            self.vtype.r.vlmul_mag.eq(if_stage.vtype.vlmul_mag),
            self.vtype.r.vlmul_sign.eq(if_stage.vtype.vlmul_sign),
            self.vtype.r.vsew.eq(if_stage.vtype.vsew),
            self.vtype.r.vta.eq(if_stage.vtype.vta),
            self.vtype.r.vma.eq(if_stage.vtype.vma),
            self.vtype.r.vill.eq(if_stage.vtype.vill),
            self.vl.r.eq(if_stage.vl),
            self.vlenb.r.eq(self.vlen // 8),
        ]

        #
        # Decoding
        #

        dec_stage = m.submodules.decode_stage = DecodeStage(self.params)
        m.d.comb += [
            dec_stage.fetch_packet.valid.eq(if_stage.fetch_packet.valid),
            dec_stage.fetch_packet.bits.eq(if_stage.fetch_packet.bits),
            if_stage.fetch_packet.ready.eq(dec_stage.fetch_packet.ready),
            dec_stage.vtype.eq(if_stage.vtype),
        ]

        expander = m.submodules.expander = VOpExpander(self.params)
        m.d.comb += [
            expander.dec_valid.eq(dec_stage.valid),
            expander.dec_uop.eq(dec_stage.uop),
            dec_stage.ready.eq(expander.dec_ready),
        ]

        #
        # Dispatch
        #

        dispatcher = m.submodules.dispatcher = Dispatcher(self.params)
        m.d.comb += [
            dispatcher.expd_valid.eq(expander.expd_valid),
            dispatcher.expd_uop.eq(expander.expd_uop),
            expander.expd_ready.eq(dispatcher.expd_ready),
        ]

        m.d.comb += dispatcher.dis_ready.eq(1)

        #
        # Issue
        #

        scoreboard = m.submodules.scoreboard = Scoreboard(params=self.params)
        m.d.comb += [
            scoreboard.dis_uop.eq(dispatcher.dis_uop),
            scoreboard.sb_uop.eq(dispatcher.sb_uop),
        ]

        sb_ready = scoreboard.dis_ready

        #
        # Register read
        #

        vregfile = m.submodules.vregfile = RegisterFile(rports=3,
                                                        wports=1,
                                                        num_regs=32,
                                                        data_width=self.vlen)

        vregread = m.submodules.vregread = RegisterRead(num_rports=3,
                                                        reg_width=self.vlen,
                                                        params=self.params)

        m.d.comb += [
            vregread.dis_valid.eq(dispatcher.dis_valid & sb_ready),
            vregread.dis_uop.eq(dispatcher.dis_uop),
            if_stage.get_rs1.valid.eq(vregread.dis_valid),
            if_stage.get_rs1.bits.eq(dispatcher.dis_uop.ftq_idx),
            if_stage.get_rs2.valid.eq(vregread.dis_valid),
            if_stage.get_rs2.bits.eq(dispatcher.dis_uop.ftq_idx),
        ]

        dis_ready = vregread.dis_ready
        m.d.comb += [
            scoreboard.dis_valid.eq(dispatcher.dis_valid & dis_ready),
            dispatcher.dis_ready.eq(sb_ready & dis_ready),
        ]

        for vrr_rp, rp in zip(vregread.read_ports, vregfile.read_ports):
            m.d.comb += vrr_rp.connect(rp)

        #
        # Execute
        #

        exec_rs1_data = Signal(self.xlen)
        exec_rs2_data = Signal(self.xlen)
        m.d.sync += [
            exec_rs1_data.eq(if_stage.get_rs1_data),
            exec_rs2_data.eq(if_stage.get_rs2_data),
        ]

        exec_unit = m.submodules.exec_unit = ALUExecUnit(self.params)
        m.d.comb += [
            vregread.exec_req.connect(exec_unit.req),
            exec_unit.req.bits.rs1_data.eq(exec_rs1_data),
            exec_unit.req.bits.rs2_data.eq(exec_rs2_data),
        ]

        m.d.comb += exec_unit.lsu_req.ready.eq(1)

        return m
