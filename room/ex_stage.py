from amaranth import *

from room.consts import *
from room.fu import ExecReq, ExecResp, ALUUnit, AddrGenUnit, MultiplierUnit, DivUnit, IntToFPUnit, FPUUnit, FDivUnit
from room.branch import GetPCResp, BranchResolution, BranchUpdate, BranchKillableFIFO
from room.types import HasCoreParams, MicroOp
from room.utils import Arbiter, generate_imm
from room.csr import AutoCSR
from room.mmu import CoreMemRequest, CoreMemResponse

from vroom.core import VectorUnit, VectorDebug

from roomsoc.interconnect.stream import Valid, Decoupled


class ExecDebug(HasCoreParams, Record):

    def __init__(self, params, name=None, src_loc_at=0):
        HasCoreParams.__init__(self, params)

        Record.__init__(self, [
            ('uop_id', MicroOp.ID_WIDTH),
            ('opcode', UOpCode),
            ('prs1', range(self.num_pregs)),
            ('rs1_data', self.xlen),
            ('prs2', range(self.num_pregs)),
            ('rs2_data', self.xlen),
        ],
                        name=name,
                        src_loc_at=1 + src_loc_at)


class ExecUnit(HasCoreParams, Elaboratable):

    def __init__(self,
                 data_width,
                 num_bypass_stages,
                 params,
                 irf_read=False,
                 irf_write=False,
                 frf_read=False,
                 frf_write=False,
                 mem_irf_write=False,
                 mem_frf_write=False,
                 can_bypass=False,
                 always_bypass=False,
                 has_jmp_unit=False,
                 has_alu=False,
                 has_mul=False,
                 has_div=False,
                 has_mem=False,
                 has_csr=False,
                 has_fpu=False,
                 has_fdiv=False,
                 has_ifpu=False,
                 has_fpiu=False,
                 has_vec=False,
                 sim_debug=False):
        super().__init__(params)

        self.data_width = data_width
        self.num_bypass_stages = num_bypass_stages
        self.irf_read = irf_read
        self.irf_write = irf_write
        self.frf_read = frf_read
        self.frf_write = frf_write
        self.mem_irf_write = mem_irf_write
        self.mem_frf_write = mem_frf_write
        self.can_bypass = can_bypass
        self.always_bypass = always_bypass
        self.has_jmp_unit = has_jmp_unit
        self.has_alu = has_alu
        self.has_mul = has_mul
        self.has_div = has_div
        self.has_mem = has_mem
        self.has_csr = has_csr
        self.has_fpu = has_fpu
        self.has_fdiv = has_fdiv
        self.has_ifpu = has_ifpu
        self.has_fpiu = has_fpiu
        self.has_fflags = has_fpu or has_fdiv
        self.has_vec = has_vec
        self.sim_debug = sim_debug

        self.fu_types = Signal(FUType)

        self.req = Decoupled(ExecReq, data_width, params)

        if self.irf_write:
            self.iresp = Decoupled(ExecResp, self.data_width, params)

        if self.frf_write:
            self.fresp = Decoupled(ExecResp, self.data_width, params)

        if self.mem_irf_write:
            self.mem_iresp = Decoupled(ExecResp, self.data_width, params)

        if self.mem_frf_write:
            self.mem_fresp = Decoupled(ExecResp, self.data_width, params)

        self.bypass = [
            Valid(ExecResp, self.data_width, self.params, name=f'bypass{i}')
            for i in range(num_bypass_stages)
        ]

        self.br_update = BranchUpdate(params)

        self.br_res = BranchResolution(params,
                                       name='br_res') if has_alu else None

        self.get_pc = None
        if has_jmp_unit:
            self.get_pc = GetPCResp(params, name='get_pc')

        self.lsu_req = None
        if has_mem:
            self.lsu_req = Valid(ExecResp, self.data_width, params)

        if has_vec:
            self.rob_head_idx = Signal(
                range(self.core_width * self.num_rob_rows))
            self.rob_pnr_idx = Signal(
                range(self.core_width * self.num_rob_rows))
            self.exception = Signal()

            self.vec_mem_req = Decoupled(CoreMemRequest, params)
            self.vec_mem_nack = Signal()
            self.vec_mem_resp = Valid(CoreMemResponse, params)

            if self.use_fpu:
                self.fp_req = Decoupled(ExecResp, self.data_width, params)

        if sim_debug:
            self.exec_debug = Valid(ExecDebug, params, name='ex_debug')

    def elaborate(self, platform):
        m = Module()

        if self.sim_debug:
            m.d.comb += [
                self.exec_debug.valid.eq(self.req.valid),
                self.exec_debug.bits.uop_id.eq(self.req.bits.uop.uop_id),
                self.exec_debug.bits.opcode.eq(self.req.bits.uop.opcode),
                self.exec_debug.bits.prs1.eq(self.req.bits.uop.prs1),
                self.exec_debug.bits.rs1_data.eq(self.req.bits.rs1_data),
                self.exec_debug.bits.prs2.eq(self.req.bits.uop.prs2),
                self.exec_debug.bits.rs2_data.eq(self.req.bits.rs2_data),
            ]

        return m


class ALUExecUnit(ExecUnit, AutoCSR):

    def __init__(self,
                 params,
                 has_jmp_unit=False,
                 has_alu=True,
                 has_mul=False,
                 has_div=False,
                 has_mem=False,
                 has_csr=False,
                 has_ifpu=False,
                 has_vec=False,
                 sim_debug=False,
                 name=None):
        super().__init__(params['xlen'],
                         3 if has_alu and has_mul else (1 if has_alu else 0),
                         params,
                         irf_read=True,
                         irf_write=has_alu or has_vec,
                         mem_frf_write=has_ifpu or has_vec,
                         can_bypass=has_alu,
                         has_jmp_unit=has_jmp_unit,
                         has_alu=has_alu,
                         has_mul=has_mul,
                         has_div=has_div,
                         has_mem=has_mem,
                         has_csr=has_csr,
                         has_ifpu=has_ifpu,
                         has_vec=has_vec,
                         sim_debug=sim_debug)
        self.name = name

        if has_vec:
            self._vec_unit = VectorUnit(params, sim_debug=self.sim_debug)

            if sim_debug:
                self.vec_debug = VectorDebug(params)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        div_busy = Signal()
        ifpu_busy = Signal()

        fu_type_div = Signal(FUType)
        fu_type_ifpu = Signal(FUType)

        if self.has_div:
            with m.If(~div_busy):
                m.d.comb += fu_type_div.eq(FUType.DIV)

        if self.has_ifpu:
            with m.If(~ifpu_busy):
                m.d.comb += fu_type_ifpu.eq(FUType.I2F)

        vec_busy = Signal()
        fu_type_vec = Signal(FUType)
        if self.has_vec:
            with m.If(~vec_busy):
                m.d.comb += fu_type_vec.eq(FUType.VEC)

        m.d.comb += self.fu_types.eq((FUType.ALU if self.has_alu else 0)
                                     | (FUType.MUL if self.has_mul else 0)
                                     | fu_type_div
                                     | (FUType.JMP if self.has_jmp_unit else 0)
                                     | (FUType.MEM if self.has_mem else 0)
                                     | (FUType.CSR if self.has_csr else 0)
                                     | fu_type_ifpu
                                     | fu_type_vec)

        iresp_units = []

        if self.has_alu:
            alu = m.submodules.alu = ALUUnit(
                self.data_width,
                self.params,
                is_jmp=self.has_jmp_unit,
                num_stages=3 if self.has_mul else 1)
            iresp_units.append(alu)

            m.d.comb += [
                self.req.connect(alu.req),
                alu.req.valid.eq(
                    self.req.valid
                    & ((self.req.bits.uop.fu_type == FUType.ALU)
                       | (self.req.bits.uop.fu_type == FUType.JMP)
                       | (self.req.bits.uop.fu_type == FUType.CSR))),
                alu.br_update.eq(self.br_update),
                self.br_res.eq(alu.br_res),
            ]

            for byp, alu_byp in zip(self.bypass, alu.bypass):
                m.d.comb += byp.eq(alu_byp)

            if self.has_jmp_unit:
                m.d.comb += alu.get_pc.eq(self.get_pc)

        if self.has_mul:
            imul = m.submodules.imul = MultiplierUnit(self.data_width, 3,
                                                      self.params)
            iresp_units.append(imul)

            m.d.comb += [
                self.req.connect(imul.req),
                imul.req.valid.eq(self.req.valid
                                  & (self.req.bits.uop.fu_type == FUType.MUL)),
                imul.br_update.eq(self.br_update),
            ]

        if self.has_ifpu:
            ifpu = m.submodules.ifpu = IntToFPUnit(self.data_width, 2,
                                                   self.params)
            m.d.comb += [
                self.req.connect(ifpu.req),
                ifpu.req.valid.eq(self.req.valid
                                  &
                                  (self.req.bits.uop.fu_type_has(FUType.I2F))),
                ifpu.br_update.eq(self.br_update),
            ]

            ifpu_q = m.submodules.ifpu_q = BranchKillableFIFO(5,
                                                              self.params,
                                                              ExecResp,
                                                              self.data_width,
                                                              self.params,
                                                              flow=True)

            m.d.comb += [
                ifpu_q.w_data.eq(ifpu.resp.bits),
                ifpu_q.w_br_mask.eq(ifpu.resp.bits.uop.br_mask),
                ifpu_q.w_en.eq(ifpu.resp.valid),
                ifpu_q.br_update.eq(self.br_update),
                ifpu_q.flush.eq(self.req.bits.kill),
            ]

            m.d.comb += [
                self.mem_fresp.bits.eq(ifpu_q.r_data),
                self.mem_fresp.bits.uop.br_mask.eq(ifpu_q.r_br_mask),
                self.mem_fresp.valid.eq(ifpu_q.r_rdy),
                ifpu_q.r_en.eq(self.mem_fresp.ready),
            ]

            m.d.comb += ifpu_busy.eq(~ifpu_q.empty)

        if self.has_div:
            div = m.submodules.div = DivUnit(self.data_width, self.params)

            div_resp_busy = 0
            for iu in iresp_units:
                div_resp_busy |= iu.resp.valid

            m.d.comb += [
                self.req.connect(div.req),
                div.req.valid.eq(self.req.valid
                                 & (self.req.bits.uop.fu_type == FUType.DIV)),
                div.br_update.eq(self.br_update),
                div.resp.ready.eq(~div_resp_busy),
                div_busy.eq(~div.req.ready | div.req.valid),
            ]

            iresp_units.append(div)

        if self.has_mem:
            agu = m.submodules.agu = AddrGenUnit(self.params)

            m.d.comb += [
                self.req.connect(agu.req),
                agu.br_update.eq(self.br_update),
                self.lsu_req.eq(agu.resp),
            ]

        if self.has_vec:
            vec = m.submodules.vec = self._vec_unit

            vec_req_arb = m.submodules.vec_req_arb = Arbiter(
                1 + self.use_fpu, ExecReq, self.data_width, self.params)
            m.d.comb += [
                self.req.connect(vec_req_arb.inp[0]),
                vec_req_arb.inp[0].valid.eq(
                    self.req.valid
                    & (self.req.bits.uop.fu_type_has(FUType.VEC))),
            ]

            if self.use_fpu:
                m.d.comb += [
                    vec_req_arb.inp[1].valid.eq(self.fp_req.valid),
                    vec_req_arb.inp[1].bits.uop.eq(self.fp_req.bits.uop),
                    vec_req_arb.inp[1].bits.rs1_data.eq(self.fp_req.bits.data),
                    self.fp_req.ready.eq(vec_req_arb.inp[1].ready),
                ]

            m.d.comb += [
                vec_req_arb.out.connect(vec.req),
                vec.resp.ready.eq(1),
                vec.fresp.connect(self.mem_fresp),
                vec.mem_req.connect(self.vec_mem_req),
                vec.mem_nack.eq(self.vec_mem_nack),
                vec.mem_resp.eq(self.vec_mem_resp),
                vec.rob_head_idx.eq(self.rob_head_idx),
                vec.rob_pnr_idx.eq(self.rob_pnr_idx),
                vec.br_update.eq(self.br_update),
                vec.exception.eq(self.exception),
                vec_busy.eq(~vec.req.ready | vec.req.valid),
            ]

            iresp_units.append(vec)

            if self.sim_debug:
                m.d.comb += self.vec_debug.eq(vec.vec_debug)

        if self.irf_write:
            for iu in reversed(iresp_units):
                with m.If(iu.resp.valid):
                    m.d.comb += [
                        self.iresp.valid.eq(1),
                        self.iresp.bits.eq(iu.resp.bits),
                    ]

            if self.has_alu:
                m.d.comb += [
                    self.iresp.bits.uop.csr_addr.eq(
                        generate_imm(alu.resp.bits.uop.imm_packed, ImmSel.I)),
                    self.iresp.bits.uop.csr_cmd.eq(alu.resp.bits.uop.csr_cmd),
                ]

        return m


class FPUExecUnit(ExecUnit):

    def __init__(self,
                 params,
                 has_fpu=True,
                 has_fdiv=False,
                 has_fpiu=False,
                 sim_debug=False,
                 name=None):
        super().__init__(params['flen'],
                         0,
                         params,
                         frf_read=True,
                         frf_write=True,
                         mem_irf_write=has_fpiu,
                         has_fpu=has_fpu,
                         has_fdiv=has_fdiv,
                         has_fpiu=has_fpiu,
                         sim_debug=sim_debug)
        self.name = name

    def elaborate(self, platform):
        m = super().elaborate(platform)

        fu_units = []

        fdiv_busy = Signal()
        fpiu_busy = Signal()

        fu_type_fdiv = Signal(FUType)
        fu_type_fpiu = Signal(FUType)

        if self.has_fdiv:
            with m.If(~fdiv_busy):
                m.d.comb += fu_type_fdiv.eq(FUType.FDIV)

        if self.has_fpiu:
            with m.If(~fpiu_busy):
                m.d.comb += fu_type_fpiu.eq(FUType.F2I)

        m.d.comb += self.fu_types.eq((FUType.FPU if self.has_fpu else 0)
                                     | fu_type_fdiv | fu_type_fpiu)

        if self.has_fpu:
            fpu = m.submodules.fpu = FPUUnit(self.data_width, self.params)

            m.d.comb += [
                self.req.connect(fpu.req),
                fpu.req.valid.eq(
                    self.req.valid
                    & (self.req.bits.uop.fu_type_has(FUType.FPU)
                       | self.req.bits.uop.fu_type_has(FUType.F2I))),
                fpu.br_update.eq(self.br_update),
            ]

            fu_units.append(fpu)

        if self.has_fdiv:
            fdiv = m.submodules.fdiv = FDivUnit(self.data_width, self.params)

            fdiv_resp_busy = 0
            for fu in fu_units:
                fdiv_resp_busy |= fu.resp.valid

            m.d.comb += [
                self.req.connect(fdiv.req),
                fdiv.req.valid.eq(
                    self.req.valid
                    & (self.req.bits.uop.fu_type_has(FUType.FDIV))),
                fdiv.br_update.eq(self.br_update),
                fdiv.resp.ready.eq(~fdiv_resp_busy),
                fdiv_busy.eq(~fdiv.req.ready | fdiv.req.valid),
            ]

            fu_units.append(fdiv)

        if self.has_fpiu:
            fpiu_q = m.submodules.fpiu_q = BranchKillableFIFO(6,
                                                              self.params,
                                                              ExecResp,
                                                              self.data_width,
                                                              self.params,
                                                              flow=True)

            m.d.comb += [
                fpiu_q.w_data.eq(fpu.resp.bits),
                fpiu_q.w_br_mask.eq(fpu.resp.bits.uop.br_mask),
                fpiu_q.w_en.eq(fpu.resp.valid
                               & fpu.resp.bits.uop.fu_type_has(FUType.F2I)
                               & (fpu.resp.bits.uop.opcode != UOpCode.STA)),
                fpiu_q.br_update.eq(self.br_update),
                fpiu_q.flush.eq(self.req.bits.kill),
            ]

            fp_stq = m.submodules.fp_stq = BranchKillableFIFO(3,
                                                              self.params,
                                                              ExecResp,
                                                              self.data_width,
                                                              self.params,
                                                              flow=True)

            m.d.comb += [
                fp_stq.w_data.uop.eq(self.req.bits.uop),
                fp_stq.w_data.data.eq(self.req.bits.rs2_data),
                fp_stq.w_br_mask.eq(self.req.bits.uop.br_mask),
                fp_stq.w_en.eq(
                    self.req.valid
                    & (self.req.bits.uop.opcode == UOpCode.STA)
                    & ~self.br_update.uop_killed(self.req.bits.uop)),
                fp_stq.br_update.eq(self.br_update),
                fp_stq.flush.eq(self.req.bits.kill),
            ]

            resp_arb = m.submodules.resp_arb = Arbiter(2, ExecResp,
                                                       self.data_width,
                                                       self.params)

            m.d.comb += [
                resp_arb.inp[0].bits.eq(fpiu_q.r_data),
                resp_arb.inp[0].bits.uop.br_mask.eq(fpiu_q.r_br_mask),
                resp_arb.inp[0].valid.eq(fpiu_q.r_rdy),
                fpiu_q.r_en.eq(resp_arb.inp[0].ready),
                resp_arb.inp[1].bits.eq(fp_stq.r_data),
                resp_arb.inp[1].bits.uop.br_mask.eq(fp_stq.r_br_mask),
                resp_arb.inp[1].valid.eq(fp_stq.r_rdy),
                fp_stq.r_en.eq(resp_arb.inp[1].ready),
                resp_arb.out.connect(self.mem_iresp),
            ]

            m.d.comb += fpiu_busy.eq(~fpiu_q.empty | ~fp_stq.empty)

        fresp_valid = 0
        for fu in reversed(fu_units):
            fresp_valid |= fu.resp.valid

            with m.If(fu.resp.valid):
                m.d.comb += [
                    self.fresp.bits.data.eq(fu.resp.bits.data),
                    self.fresp.bits.uop.eq(fu.resp.bits.uop),
                    self.fresp.bits.fflags.eq(fu.resp.bits.fflags),
                ]

        m.d.comb += self.fresp.valid.eq(
            fresp_valid
            & ~(fpu.resp.valid & fpu.resp.bits.uop.fu_type_has(FUType.F2I)))

        return m


class ExecUnits(HasCoreParams, Elaboratable):

    def __init__(self, is_fpu, params, sim_debug=False):
        super().__init__(params)

        self.exec_units = []
        self.is_fpu = is_fpu

        self.irf_readers = 0
        self.irf_writers = 0
        self.frf_readers = 0
        self.frf_writers = 0

        if sim_debug:
            self.exec_debug = []

        if not is_fpu:
            for i in range(self.mem_width):
                eu = ALUExecUnit(params,
                                 has_alu=False,
                                 has_mem=True,
                                 name=f'mem{i}')
                self.exec_units.append(eu)
                self.irf_readers += eu.irf_read
                self.irf_writers += eu.irf_write

            int_width = self.issue_params[IssueQueueType.INT]['issue_width']
            for i in range(int_width):
                eu = ALUExecUnit(params,
                                 has_jmp_unit=(i == 0),
                                 has_csr=(i == (1 % int_width)),
                                 has_mul=(i == (2 % int_width)),
                                 has_div=(i == (3 % int_width)),
                                 has_ifpu=(i == (4 % int_width)),
                                 sim_debug=sim_debug,
                                 name=f'alu_int{i}')
                self.exec_units.append(eu)
                self.irf_readers += eu.irf_read
                self.irf_writers += eu.irf_write

                if sim_debug:
                    self.exec_debug.append(eu.exec_debug)

            if self.use_vector:
                eu = ALUExecUnit(params,
                                 has_alu=False,
                                 has_vec=True,
                                 sim_debug=sim_debug,
                                 name='vec')
                self.exec_units.append(eu)
                self.irf_readers += eu.irf_read
                self.irf_writers += eu.irf_write

                if sim_debug:
                    self.exec_debug.append(eu.exec_debug)

        else:
            fp_width = self.issue_params[IssueQueueType.FP]['issue_width']
            for i in range(fp_width):
                eu = FPUExecUnit(params,
                                 has_fpu=True,
                                 has_fdiv=(i == (1 % fp_width)),
                                 has_fpiu=(i == (1 % fp_width)),
                                 sim_debug=sim_debug,
                                 name=f'fpu{i}')
                self.exec_units.append(eu)
                self.frf_readers += eu.frf_read
                self.frf_writers += eu.frf_write

                if sim_debug:
                    self.exec_debug.append(eu.exec_debug)

        self.irf_read_ports = self.irf_readers * 2
        self.irf_write_ports = self.irf_writers

        self.frf_read_ports = self.frf_readers * 3
        self.frf_write_ports = self.frf_writers

        self.bypass_ports = 0
        for eu in self.exec_units:
            if eu.can_bypass:
                self.bypass_ports += eu.num_bypass_stages

        self.bypassable_mask = [
            eu.can_bypass for eu in self.exec_units if eu.irf_write
        ]

    def __getitem__(self, key):
        return self.exec_units[key]

    def __iter__(self):
        return iter(self.exec_units)

    def elaborate(self, platform):
        m = Module()

        for eu in self.exec_units:
            setattr(m.submodules, eu.name, eu)

        return m
