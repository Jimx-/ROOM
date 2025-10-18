from amaranth import *

from vroom.consts import *
from vroom.types import HasVectorParams, VMicroOp
from vroom.if_stage import IFStage
from vroom.id_stage import VIDDebug, DecodeStage, VOpExpander
from vroom.dispatch import Dispatcher
from vroom.issue import Scoreboard
from vroom.regfile import RegisterRead, RegisterFile
from vroom.ex_stage import VExecDebug, ALUExecUnit, FPUExecUnit
from vroom.lsu import LoadStoreUnit
from vroom.fu import ExecReq as VExecReq, ExecResp as VExecResp

from room.consts import RegisterType
from room.branch import BranchUpdate
from room.fu import ExecReq, ExecResp
from room.csr import CSR, AutoCSR, CSRAccess
from room.mmu import CoreMemRequest, CoreMemResponse
from room.utils import Decoupled, Valid, Arbiter


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


def vxrm_layout(xlen):
    return [
        ("vxrm", 2, CSRAccess.RW),
        ("_rsvd", xlen - 2, CSRAccess.RO),
    ]


class VWritebackDebug(HasVectorParams, Record):

    def __init__(self, params, name=None, src_loc_at=0):
        HasVectorParams.__init__(self, params)

        Record.__init__(self, [
            ('uop_id', VMicroOp.ID_WIDTH),
            ('ldst', range(32)),
            ('data', self.vlen),
        ],
                        name=name,
                        src_loc_at=1 + src_loc_at)


class VectorDebug(HasVectorParams):

    def __init__(self, params):
        super().__init__(params)

        self.id_debug = Valid(VIDDebug, params)
        self.ex_debug = Valid(VExecDebug, params)
        if self.use_fpu:
            self.fp_ex_debug = Valid(VExecDebug, params)
        self.wb_debug = Valid(VWritebackDebug, params)

    def eq(self, rhs):
        return [
            self.id_debug.eq(rhs.id_debug),
            self.ex_debug.eq(rhs.ex_debug),
            self.wb_debug.eq(rhs.wb_debug),
        ] + ([self.fp_ex_debug.eq(rhs.fp_ex_debug)] if self.use_fpu else [])


class VectorUnit(HasVectorParams, AutoCSR, Elaboratable):

    def __init__(self, params, sim_debug=False):
        super().__init__(params)

        self.sim_debug = sim_debug

        self.req = Decoupled(ExecReq, self.xlen, params)
        self.resp = Decoupled(ExecResp, self.xlen, params)
        self.fresp = Decoupled(ExecResp, self.flen, params)

        self.mem_req = Decoupled(CoreMemRequest, params)
        self.mem_nack = Signal()
        self.mem_resp = Valid(CoreMemResponse, params)

        self.rob_head_idx = Signal(range(self.core_width * self.num_rob_rows))
        self.rob_pnr_idx = Signal(range(self.core_width * self.num_rob_rows))

        self.br_update = BranchUpdate(params)
        self.exception = Signal()

        self.vxrm = CSR(0x00A, vxrm_layout(self.xlen))
        self.vl = CSR(0xC20, [('value', self.xlen, CSRAccess.RO)])
        self.vtype = CSR(0xC21, vtype_layout(self.xlen))
        self.vlenb = CSR(0xC22, [('value', self.xlen, CSRAccess.RO)])

        if self.sim_debug:
            self.vec_debug = VectorDebug(params)

    def elaborate(self, platform):
        m = Module()

        #
        # Instruction fetch
        #

        if_stage = m.submodules.if_stage = IFStage(self.params)
        m.d.comb += [
            self.req.connect(if_stage.req),
            if_stage.resp.connect(self.resp),
            if_stage.fresp.connect(self.fresp),
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

        with m.If(self.vxrm.we):
            m.d.sync += self.vxrm.r.eq(self.vxrm.w)

        #
        # Decoding
        #

        dec_stage = m.submodules.decode_stage = DecodeStage(
            self.params, sim_debug=self.sim_debug)
        m.d.comb += [
            dec_stage.fetch_packet.valid.eq(if_stage.fetch_packet.valid),
            dec_stage.fetch_packet.bits.eq(if_stage.fetch_packet.bits),
            if_stage.fetch_packet.ready.eq(dec_stage.fetch_packet.ready),
            dec_stage.vtype.eq(if_stage.vtype),
            dec_stage.vl.eq(if_stage.vl),
            dec_stage.vxrm.eq(self.vxrm.r),
        ]

        if self.sim_debug:
            m.d.comb += self.vec_debug.id_debug.eq(dec_stage.id_debug)

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

        vregfile = m.submodules.vregfile = RegisterFile(rports=4,
                                                        wports=1,
                                                        num_regs=32,
                                                        data_width=self.vlen)
        vregfile_v0 = Signal(self.vlen)

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

        for vrr_rp, rp in zip(vregread.read_ports, vregfile.read_ports[:-1]):
            m.d.comb += vrr_rp.connect(rp)

        #
        # Execute
        #

        exec_rs1_data = Signal(self.xlen)
        exec_rs2_data = Signal(self.xlen)
        with m.If(vregread.dis_valid & vregread.dis_ready):
            m.d.sync += [
                exec_rs1_data.eq(if_stage.get_rs1_data),
                exec_rs2_data.eq(if_stage.get_rs2_data),
            ]

        exec_unit = m.submodules.exec_unit = ALUExecUnit(
            self.params, sim_debug=self.sim_debug)
        m.d.comb += [
            vregread.exec_req.connect(exec_unit.req),
            exec_unit.req.valid.eq(vregread.exec_req.valid
                                   & ~vregread.exec_req.bits.uop.fp_valid),
            exec_unit.req.bits.rs1_data.eq(exec_rs1_data),
            exec_unit.req.bits.rs2_data.eq(exec_rs2_data),
            exec_unit.req.bits.mask.eq(vregfile_v0),
            vregfile.read_ports[-1].addr.eq(exec_unit.perm_rd_port.addr),
        ]
        m.d.sync += exec_unit.perm_rd_port.data.eq(
            vregfile.read_ports[-1].data)

        if self.use_fpu:
            fp_exec_unit = m.submodules.fp_exec_unit = FPUExecUnit(
                self.params, sim_debug=self.sim_debug)
            m.d.comb += [
                fp_exec_unit.req.valid.eq(
                    vregread.exec_req.valid
                    & vregread.exec_req.bits.uop.fp_valid),
                fp_exec_unit.req.bits.eq(vregread.exec_req.bits),
                fp_exec_unit.req.bits.rs1_data.eq(exec_rs1_data),
                fp_exec_unit.req.bits.rs2_data.eq(exec_rs2_data),
                fp_exec_unit.req.bits.mask.eq(vregfile_v0),
            ]

            with m.If(vregread.exec_req.bits.uop.fp_valid):
                m.d.comb += vregread.exec_req.ready.eq(fp_exec_unit.req.ready)

        if self.sim_debug:
            m.d.comb += self.vec_debug.ex_debug.eq(exec_unit.exec_debug)

            if self.use_fpu:
                m.d.comb += self.vec_debug.fp_ex_debug.eq(
                    fp_exec_unit.exec_debug)

        #
        # Load/store unit
        #

        lsu = m.submodules.lsu = LoadStoreUnit(self.params)
        m.d.comb += [
            exec_unit.lsu_req.connect(lsu.exec_req),
            lsu.mem_req.connect(self.mem_req),
            lsu.mem_nack.eq(self.mem_nack),
            lsu.mem_resp.eq(self.mem_resp),
        ]

        #
        # Writeback
        #

        wb_arb = m.submodules.wb_arb = Arbiter(2 + self.use_fpu, VExecResp,
                                               self.params)
        wb_req = Valid(VExecResp, self.params)
        m.d.comb += [
            wb_arb.inp[0].bits.eq(exec_unit.vresp.bits),
            wb_arb.inp[0].valid.eq(exec_unit.vresp.valid),
            lsu.exec_resp.connect(wb_arb.inp[1 + self.use_fpu]),
            wb_req.eq(wb_arb.out),
            wb_arb.out.ready.eq(1),
            if_stage.wb_req.valid.eq(wb_req.valid
                                     & wb_req.bits.uop.expd_end),
            if_stage.wb_req.bits.eq(wb_req.bits),
        ]

        if self.use_fpu:
            m.d.comb += fp_exec_unit.vresp.connect(wb_arb.inp[1])

        m.d.comb += [
            scoreboard.wakeup.valid.eq(
                wb_req.valid
                & (wb_req.bits.uop.dst_rtype == RegisterType.VEC)),
            scoreboard.wakeup.bits.ldst.eq(wb_req.bits.uop.ldst),
            scoreboard.wakeup.bits.is_perm.eq(
                (wb_req.bits.uop.fu_type == VFUType.PERM)
                & wb_req.bits.uop.expd_end),
        ]

        for wp in vregfile.write_ports:
            m.d.comb += [
                wp.valid.eq(wb_req.valid
                            & (wb_req.bits.uop.dst_rtype == RegisterType.VEC)),
                wp.bits.addr.eq(wb_req.bits.uop.ldst),
                wp.bits.data.eq(wb_req.bits.vd_data),
            ]

            with m.If(wp.valid & (wp.bits.addr == 0)):
                m.d.sync += vregfile_v0.eq(wp.bits.data)

        if self.sim_debug:
            m.d.comb += [
                self.vec_debug.wb_debug.valid.eq(
                    wb_req.valid
                    & (wb_req.bits.uop.dst_rtype == RegisterType.VEC)),
                self.vec_debug.wb_debug.bits.uop_id.eq(wb_req.bits.uop.uop_id),
                self.vec_debug.wb_debug.bits.ldst.eq(wb_req.bits.uop.ldst),
                self.vec_debug.wb_debug.bits.data.eq(wb_req.bits.vd_data),
            ]

        return m
