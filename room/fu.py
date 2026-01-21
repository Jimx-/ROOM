from amaranth import *
from amaranth import tracer
from amaranth.utils import log2_int

from room.consts import *
from room.types import HasCoreParams, MicroOp
from room.branch import GetPCResp, BranchResolution, BranchUpdate
from room.alu import ALU, Multiplier, IntDiv
from room.fpu import HasFPUParams, FPUOperator, FPFormat, FPUFMA, FPUDivSqrtMulti, FPUCastMulti, FPUComp
from room.exc import Cause
from room.tlb import SFenceReq
from room.utils import sign_extend, generate_imm, generate_imm_type, generate_imm_rm, Pipe

from roomsoc.interconnect.stream import Decoupled, Valid


class ExecReq:

    def __init__(self, data_width, params, name=None, src_loc_at=0):
        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.uop = MicroOp(params, name=f'{name}_uop')

        self.rs1_data = Signal(data_width, name=f'{name}_rs1_data')
        self.rs2_data = Signal(data_width, name=f'{name}_rs2_data')
        self.rs3_data = Signal(data_width, name=f'{name}_rs3_data')

        self.kill = Signal(name=f'{name}_kill')

    def eq(self, rhs):
        attrs = ['uop', 'rs1_data', 'rs2_data', 'rs3_data', 'kill']
        return [getattr(self, a).eq(getattr(rhs, a)) for a in attrs]


class ExecResp(HasCoreParams):

    def __init__(self, data_width, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.uop = MicroOp(params, name=f'{name}_uop')

        self.data = Signal(data_width, name=f'{name}_data')
        self.addr = Signal(self.vaddr_bits + 1, name=f'{name}_addr')

        self.mem_exc = Valid(Signal, Cause, name=f'{name}_mem_exc')
        self.sfence = Valid(SFenceReq, params, name=f'{name}_sfence')

        self.fflags = Valid(Signal, 5, name=f'{name}_fflags')

    def eq(self, rhs):
        attrs = ['uop', 'addr', 'data', 'mem_exc', 'sfence', 'fflags']
        return [getattr(self, a).eq(getattr(rhs, a)) for a in attrs]


class FunctionalUnit(HasCoreParams, Elaboratable):

    def __init__(self,
                 data_width,
                 num_bypass_stages,
                 params,
                 is_jmp=False,
                 is_alu=False):
        super().__init__(params)

        self.num_bypass_stages = num_bypass_stages
        self.is_jmp = is_jmp

        self.req = Decoupled(ExecReq, data_width, params)
        self.resp = Decoupled(ExecResp, data_width, params)

        self.bypass = [
            Valid(ExecResp, data_width, self.params, name=f'bypass{i}')
            for i in range(num_bypass_stages)
        ]

        self.br_update = BranchUpdate(params)

        self.br_res = BranchResolution(params,
                                       name='br_res') if is_alu else None
        self.get_pc = None
        if is_jmp:
            self.get_pc = GetPCResp(params, name='get_pc')


class PipelinedFunctionalUnit(FunctionalUnit):

    def __init__(self,
                 num_stages,
                 num_bypass_stages,
                 data_width,
                 params,
                 is_jmp=False,
                 is_alu=False):
        self.params = params
        self.num_stages = num_stages

        super().__init__(data_width,
                         num_bypass_stages,
                         params,
                         is_jmp=is_jmp,
                         is_alu=is_alu)

    def elaborate(self, platform):
        m = Module()

        m.d.comb += self.req.ready.eq(1)

        if self.num_stages > 0:
            self.valids = Signal(self.num_stages)
            self.uops = [
                MicroOp(self.params, name=f's{i}_uops')
                for i in range(self.num_stages)
            ]

            m.d.sync += [
                self.valids[0].eq(
                    self.req.valid
                    & ~self.br_update.uop_killed(self.req.bits.uop)
                    & ~self.req.bits.kill),
                self.uops[0].eq(self.req.bits.uop),
                self.uops[0].br_mask.eq(
                    self.br_update.get_new_br_mask(self.req.bits.uop.br_mask)),
            ]

            for i in range(1, self.num_stages):
                m.d.sync += [
                    self.valids[i].eq(
                        self.valids[i - 1]
                        & ~self.br_update.uop_killed(self.uops[i - 1])
                        & ~self.req.bits.kill),
                    self.uops[i].eq(self.uops[i - 1]),
                    self.uops[i].br_mask.eq(
                        self.br_update.get_new_br_mask(self.uops[i -
                                                                 1].br_mask)),
                ]

            m.d.comb += [
                self.resp.valid.eq(self.valids[self.num_stages - 1]
                                   & ~self.br_update.uop_killed(self.uops[
                                       self.num_stages - 1])),
                self.resp.bits.uop.eq(self.uops[self.num_stages - 1]),
                self.resp.bits.uop.br_mask.eq(
                    self.br_update.get_new_br_mask(self.uops[self.num_stages -
                                                             1].br_mask)),
            ]

            if self.num_bypass_stages > 0:
                m.d.comb += self.bypass[0].bits.uop.eq(self.req.bits.uop)

                for i in range(1, self.num_bypass_stages):
                    m.d.comb += self.bypass[i].bits.uop.eq(self.uops[i - 1])

        else:
            m.d.comb += [
                self.resp.valid.eq(
                    self.req.valid
                    & ~self.br_update.uop_killed(self.req.bits.uop)
                    & ~self.req.bits.kill),
                self.resp.bits.uop.eq(self.req.bits.uop),
                self.resp.bits.uop.br_mask.eq(
                    self.br_update.get_new_br_mask(self.req.bits.uop.br_mask)),
            ]

        return m


class ALUUnit(PipelinedFunctionalUnit):

    def __init__(self, data_width, params, is_jmp=False, num_stages=1):
        super().__init__(num_stages,
                         num_stages,
                         data_width,
                         params,
                         is_jmp=is_jmp,
                         is_alu=True)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        uop = self.req.bits.uop

        #
        # Operands
        #

        imm = generate_imm(uop.imm_packed, uop.imm_sel)
        if self.xlen == 64:
            imm = Cat(imm, Repl(imm[31], 32))

        opa_data = Signal(self.xlen)
        opb_data = Signal(self.xlen)

        opa_data_shl = Signal(self.xlen)
        if self.use_zba:
            m.d.comb += opa_data_shl.eq(
                Mux(uop.opa_is_uw, self.req.bits.rs1_data[:32],
                    self.req.bits.rs1_data) << uop.opa_shamt)

        with m.Switch(uop.opa_sel):
            with m.Case(OpA.RS1):
                m.d.comb += opa_data.eq(self.req.bits.rs1_data)

            if self.is_jmp:
                uop_pc = (self.get_pc.pc | uop.pc_lsb) - Mux(
                    uop.edge_inst, 2, 0)
                uop_pc_sext = sign_extend(uop_pc[:self.vaddr_bits_extended],
                                          self.xlen)
                with m.Case(OpA.PC):
                    m.d.comb += opa_data.eq(uop_pc_sext)

            with m.Case(OpA.RS1SHL):
                m.d.comb += opa_data.eq(opa_data_shl)

        with m.Switch(uop.opb_sel):
            with m.Case(OpB.IMM):
                m.d.comb += opb_data.eq(imm)
            with m.Case(OpB.IMMC):
                m.d.comb += opb_data.eq(self.req.bits.uop.prs1[:5])
            with m.Case(OpB.RS2):
                m.d.comb += opb_data.eq(self.req.bits.rs2_data)
            with m.Case(OpB.NEXT):
                m.d.comb += opb_data.eq(Mux(uop.is_rvc, 2, 4))

            if self.use_zbs:
                with m.Case(OpB.RS2OH):
                    m.d.comb += opb_data.eq(
                        1 << self.req.bits.rs2_data[:log2_int(self.xlen)])
                with m.Case(OpB.IMMOH):
                    m.d.comb += opb_data.eq(1 << imm[:log2_int(self.xlen)])

        #
        # ALU
        #

        alu = m.submodules.alu = ALU(self.xlen,
                                     use_zbb=self.use_zbb,
                                     use_zbs=self.use_zbs,
                                     use_zicond=self.use_zicond)
        m.d.comb += [
            alu.in1.eq(opa_data),
            alu.in2.eq(opb_data),
            alu.fn.eq(uop.alu_fn),
            alu.dw.eq(uop.alu_dw),
        ]

        #
        # Branch unit
        #

        killed = Signal()
        with m.If(self.req.bits.kill | self.br_update.uop_killed(uop)):
            m.d.comb += killed.eq(1)

        rs1 = self.req.bits.rs1_data
        rs2 = self.req.bits.rs2_data
        br_eq = rs1 == rs2
        br_ltu = (rs1.as_unsigned() < rs2.as_unsigned())
        br_lt = (~(rs1[self.xlen - 1] ^ rs2[self.xlen - 1])
                 & br_ltu) | (rs1[self.xlen - 1] & ~rs2[self.xlen - 1])

        pc_sel = Signal(PCSel)

        with m.Switch(uop.br_type):
            with m.Case(BranchType.NE):
                m.d.comb += pc_sel.eq(Mux(~br_eq, PCSel.BRJMP,
                                          PCSel.PC_PLUS_4))
            with m.Case(BranchType.EQ):
                m.d.comb += pc_sel.eq(Mux(br_eq, PCSel.BRJMP, PCSel.PC_PLUS_4))
            with m.Case(BranchType.GE):
                m.d.comb += pc_sel.eq(Mux(~br_lt, PCSel.BRJMP,
                                          PCSel.PC_PLUS_4))
            with m.Case(BranchType.GEU):
                m.d.comb += pc_sel.eq(
                    Mux(~br_ltu, PCSel.BRJMP, PCSel.PC_PLUS_4))
            with m.Case(BranchType.LT):
                m.d.comb += pc_sel.eq(Mux(br_lt, PCSel.BRJMP, PCSel.PC_PLUS_4))
            with m.Case(BranchType.LTU):
                m.d.comb += pc_sel.eq(Mux(br_ltu, PCSel.BRJMP,
                                          PCSel.PC_PLUS_4))
            with m.Case(BranchType.J):
                m.d.comb += pc_sel.eq(PCSel.BRJMP)
            with m.Case(BranchType.JR):
                m.d.comb += pc_sel.eq(PCSel.JALR)

        br_taken = self.req.valid & ~killed & (uop.is_br | uop.is_jal
                                               | uop.is_jalr) & (
                                                   pc_sel != PCSel.PC_PLUS_4)

        is_br = self.req.valid & ~killed & uop.is_br
        is_jalr = self.req.valid & ~killed & uop.is_jalr

        mispredict = Signal()

        with m.If(is_br | is_jalr):
            m.d.comb += mispredict.eq((uop.taken & (pc_sel == PCSel.PC_PLUS_4))
                                      | (~uop.taken & (pc_sel == PCSel.BRJMP)))

        target_offset = imm[0:22].as_signed()

        m.d.comb += [
            self.br_res.valid.eq(is_br | is_jalr),
            self.br_res.uop.eq(uop),
            self.br_res.mispredict.eq(mispredict),
            self.br_res.cfi_type.eq(
                Mux(is_br, CFIType.BR, Mux(is_jalr, CFIType.JALR, CFIType.X))),
            self.br_res.taken.eq(br_taken),
            self.br_res.pc_sel.eq(pc_sel),
            self.br_res.target_offset.eq(target_offset),
        ]

        if self.is_jmp:
            m.d.comb += self.br_res.jalr_target.eq(self.req.bits.rs1_data +
                                                   target_offset)

            with m.If(pc_sel == PCSel.JALR):
                m.d.comb += mispredict.eq(~self.get_pc.next_valid | (
                    self.get_pc.next_pc != self.br_res.jalr_target))

        #
        # Output
        #

        data = [
            Signal(self.xlen, name=f's{i}_data')
            for i in range(self.num_stages)
        ]

        m.d.sync += [
            data[0].eq(alu.out),
        ]

        for i in range(1, self.num_stages):
            m.d.sync += [
                data[i].eq(data[i - 1]),
            ]

        m.d.comb += [
            self.resp.bits.data.eq(data[self.num_stages - 1]),
        ]

        #
        # Bypass
        #

        m.d.comb += [
            self.bypass[0].valid.eq(self.req.valid),
            self.bypass[0].bits.data.eq(alu.out),
        ]
        for i in range(1, self.num_stages):
            m.d.comb += [
                self.bypass[i].valid.eq(self.valids[i - 1]),
                self.bypass[i].bits.data.eq(data[i - 1]),
            ]

        return m


class AddrGenUnit(PipelinedFunctionalUnit):

    def __init__(self, params):
        super().__init__(0, 0, params['xlen'], params)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        sum = self.req.bits.rs1_data.as_signed(
        ) + self.req.bits.uop.imm_packed[8:20].as_signed()
        ea_sign = Mux(sum[self.vaddr_bits - 1],
                      ~sum[self.vaddr_bits:self.xlen] == 0,
                      sum[self.vaddr_bits:self.xlen] != 0)
        eff_addr = Cat(sum[:self.vaddr_bits], ea_sign).as_unsigned()

        m.d.comb += [
            self.resp.bits.addr.eq(eff_addr),
            self.resp.bits.data.eq(self.req.bits.rs2_data),
        ]

        misaligned = Signal()
        with m.Switch(self.req.bits.uop.mem_size):
            with m.Case(1):
                m.d.comb += misaligned.eq(eff_addr[0])
            with m.Case(2):
                m.d.comb += misaligned.eq(eff_addr[0:2].any())
            with m.Case(3):
                m.d.comb += misaligned.eq(eff_addr[0:3].any())

        ma_ld = self.req.valid & (self.req.bits.uop.opcode
                                  == UOpCode.LD) & misaligned
        ma_st = self.req.valid & (
            (self.req.bits.uop.opcode == UOpCode.STA) |
            (self.req.bits.uop.opcode == UOpCode.AMO_AG)) & misaligned
        m.d.comb += self.resp.bits.mem_exc.valid.eq(ma_ld | ma_st)
        with m.If(ma_ld):
            m.d.comb += self.resp.bits.mem_exc.bits.eq(Cause.LOAD_MISALIGNED)
        with m.Elif(ma_st):
            m.d.comb += self.resp.bits.mem_exc.bits.eq(Cause.STORE_MISALIGNED)

        m.d.comb += [
            self.resp.bits.sfence.valid.eq(self.req.valid & (
                self.req.bits.uop.mem_cmd == MemoryCommand.SFENCE)),
            self.resp.bits.sfence.bits.rs1.eq(self.req.bits.uop.mem_size[0]),
            self.resp.bits.sfence.bits.rs2.eq(self.req.bits.uop.mem_size[1]),
            self.resp.bits.sfence.bits.vaddr.eq(self.req.bits.rs1_data),
        ]

        return m


class MultiplierUnit(PipelinedFunctionalUnit):

    def __init__(self, width, latency, params):
        self.width = width
        self.latency = latency

        super().__init__(latency, 0, width, params)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        mul = m.submodules.mul = Multiplier(self.width, self.latency)
        m.d.comb += [
            mul.req.bits.fn.eq(self.req.bits.uop.alu_fn),
            mul.req.bits.dw.eq(self.req.bits.uop.alu_dw),
            mul.req.bits.in1.eq(self.req.bits.rs1_data),
            mul.req.bits.in2.eq(self.req.bits.rs2_data),
            mul.req.valid.eq(self.req.valid),
        ]

        m.d.comb += self.resp.bits.data.eq(mul.resp_data)

        return m


class IterativeFunctionalUnit(FunctionalUnit):

    def __init__(self, data_width, params):
        self.params = params

        self.do_kill = Signal()

        super().__init__(data_width, 0, params)

    def elaborate(self, platform):
        m = Module()

        uop = MicroOp(self.params)

        with m.If(self.req.valid & self.req.ready):
            m.d.comb += self.do_kill.eq(
                self.req.bits.kill
                | self.br_update.uop_killed(self.req.bits.uop))
            m.d.sync += [
                uop.eq(self.req.bits.uop),
                uop.br_mask.eq(
                    self.br_update.get_new_br_mask(self.req.bits.uop.br_mask))
            ]
        with m.Else():
            m.d.comb += self.do_kill.eq(self.req.bits.kill
                                        | self.br_update.uop_killed(uop))
            m.d.sync += [
                uop.br_mask.eq(self.br_update.get_new_br_mask(uop.br_mask))
            ]

        m.d.comb += self.resp.bits.uop.eq(uop)

        return m


class DivUnit(IterativeFunctionalUnit):

    def __init__(self, width, params):
        self.width = width

        super().__init__(width, params)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        div = m.submodules.div = IntDiv(self.width)

        m.d.comb += [
            div.req.bits.fn.eq(self.req.bits.uop.alu_fn),
            div.req.bits.dw.eq(self.req.bits.uop.alu_dw),
            div.req.bits.in1.eq(self.req.bits.rs1_data),
            div.req.bits.in2.eq(self.req.bits.rs2_data),
            div.req.valid.eq(self.req.valid),
            self.req.ready.eq(div.req.ready),
            self.resp.bits.data.eq(div.resp.bits),
            self.resp.valid.eq(div.resp.valid),
            div.resp.ready.eq(self.resp.ready),
            div.kill.eq(self.do_kill),
        ]

        return m


class IntToFPUnit(PipelinedFunctionalUnit, HasFPUParams):

    def __init__(self, width, latency, params):
        self.width = width
        self.latency = latency

        super().__init__(latency, 0, width, params)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        cast_en = Signal()

        with m.Switch(self.req.bits.uop.opcode):
            with m.Case(UOpCode.FCVT_S_X, UOpCode.FCVT_D_X):
                m.d.comb += cast_en.eq(1)

        in_pipe = m.submodules.in_pipe = Pipe(width=len(
            self.req.bits.rs1_data),
                                              depth=self.latency)
        m.d.comb += [
            in_pipe.in_valid.eq(self.req.valid),
            in_pipe.in_data.eq(self.req.bits.rs1_data),
        ]

        ifpu = m.submodules.ifpu = FPUCastMulti(latency=self.latency)

        typ = generate_imm_type(self.req.bits.uop.imm_packed)

        m.d.comb += [
            ifpu.inp.valid.eq(self.req.valid & cast_en),
            ifpu.inp.bits.fn.eq(FPUOperator.I2F),
            ifpu.inp.bits.fn_mod.eq(typ[0]),
            ifpu.inp.bits.in1.eq(self.req.bits.rs1_data),
            ifpu.inp.bits.dst_fmt.eq(
                Mux(self.req.bits.uop.fp_single, FPFormat.S, FPFormat.D)),
            ifpu.inp.bits.int_fmt.eq(Cat(typ[1], 1)),
        ]

        out_single_pipe = m.submodules.out_single_pipe = Pipe(
            width=1, depth=self.latency)
        m.d.comb += [
            out_single_pipe.in_valid.eq(self.req.valid),
            out_single_pipe.in_data.eq(self.req.bits.uop.fp_single),
        ]

        m.d.comb += [
            self.resp.bits.data.eq(
                self.nan_box(
                    Mux(ifpu.out.valid, ifpu.out.bits.data, in_pipe.out.bits),
                    ~out_single_pipe.out.bits)),
            self.resp.bits.fflags.valid.eq(ifpu.out.valid),
            self.resp.bits.fflags.bits.eq(ifpu.out.bits.status),
        ]

        return m


class FPUUnit(PipelinedFunctionalUnit):

    def __init__(self, width, params):
        self.width = width

        super().__init__(params['fma_latency'], 0, width, params)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        in_pipe = m.submodules.in_pipe = Pipe(width=len(
            self.req.bits.rs1_data),
                                              depth=self.fma_latency)
        m.d.comb += [
            in_pipe.in_valid.eq(self.req.valid),
            in_pipe.in_data.eq(self.req.bits.rs1_data),
        ]

        fma_en = Signal()
        cast_en = Signal()
        cmp_en = Signal()

        fma_op = Signal(FPUOperator)
        fma_op_mod = Signal()

        fmt_in = Signal(FPFormat)
        fmt_out = Signal(FPFormat)
        fmt_int = Signal(2)

        swap32 = Signal()

        fp_rm = Mux(
            generate_imm_rm(self.req.bits.uop.imm_packed) == 7, 0,
            generate_imm_rm(self.req.bits.uop.imm_packed))

        with m.Switch(self.req.bits.uop.opcode):
            with m.Case(UOpCode.FADD_S, UOpCode.FADD_D):
                m.d.comb += [
                    fma_en.eq(1),
                    fma_op.eq(FPUOperator.ADD),
                    fmt_in.eq(
                        Mux(self.req.bits.uop.fp_single, FPFormat.S,
                            FPFormat.D)),
                    fmt_out.eq(
                        Mux(self.req.bits.uop.fp_single, FPFormat.S,
                            FPFormat.D)),
                    swap32.eq(1),
                ]

            with m.Case(UOpCode.FSUB_S, UOpCode.FSUB_D):
                m.d.comb += [
                    fma_en.eq(1),
                    fma_op.eq(FPUOperator.ADD),
                    fma_op_mod.eq(1),
                    fmt_in.eq(
                        Mux(self.req.bits.uop.fp_single, FPFormat.S,
                            FPFormat.D)),
                    fmt_out.eq(
                        Mux(self.req.bits.uop.fp_single, FPFormat.S,
                            FPFormat.D)),
                    swap32.eq(1),
                ]

            with m.Case(UOpCode.FMUL_S, UOpCode.FMUL_D):
                m.d.comb += [
                    fma_en.eq(1),
                    fma_op.eq(FPUOperator.MUL),
                    fmt_in.eq(
                        Mux(self.req.bits.uop.fp_single, FPFormat.S,
                            FPFormat.D)),
                    fmt_out.eq(
                        Mux(self.req.bits.uop.fp_single, FPFormat.S,
                            FPFormat.D)),
                ]

            with m.Case(UOpCode.FMADD_S, UOpCode.FMADD_D):
                m.d.comb += [
                    fma_en.eq(1),
                    fma_op.eq(FPUOperator.FMADD),
                    fmt_in.eq(
                        Mux(self.req.bits.uop.fp_single, FPFormat.S,
                            FPFormat.D)),
                    fmt_out.eq(
                        Mux(self.req.bits.uop.fp_single, FPFormat.S,
                            FPFormat.D)),
                ]

            with m.Case(UOpCode.FMSUB_S, UOpCode.FMSUB_D):
                m.d.comb += [
                    fma_en.eq(1),
                    fma_op.eq(FPUOperator.FMADD),
                    fma_op_mod.eq(1),
                    fmt_in.eq(
                        Mux(self.req.bits.uop.fp_single, FPFormat.S,
                            FPFormat.D)),
                    fmt_out.eq(
                        Mux(self.req.bits.uop.fp_single, FPFormat.S,
                            FPFormat.D)),
                ]

            with m.Case(UOpCode.FNMSUB_S, UOpCode.FNMSUB_D):
                m.d.comb += [
                    fma_en.eq(1),
                    fma_op.eq(FPUOperator.FNMSUB),
                    fmt_in.eq(
                        Mux(self.req.bits.uop.fp_single, FPFormat.S,
                            FPFormat.D)),
                    fmt_out.eq(
                        Mux(self.req.bits.uop.fp_single, FPFormat.S,
                            FPFormat.D)),
                ]

            with m.Case(UOpCode.FNMADD_S, UOpCode.FNMADD_D):
                m.d.comb += [
                    fma_en.eq(1),
                    fma_op.eq(FPUOperator.FNMSUB),
                    fma_op_mod.eq(1),
                    fmt_in.eq(
                        Mux(self.req.bits.uop.fp_single, FPFormat.S,
                            FPFormat.D)),
                    fmt_out.eq(
                        Mux(self.req.bits.uop.fp_single, FPFormat.S,
                            FPFormat.D)),
                ]

            with m.Case(UOpCode.FCVT_X_S, UOpCode.FCVT_X_D):
                typ = generate_imm_type(self.req.bits.uop.imm_packed)

                m.d.comb += [
                    cast_en.eq(1),
                    fma_op.eq(FPUOperator.F2I),
                    fma_op_mod.eq(typ[0]),
                    fmt_in.eq(
                        Mux(self.req.bits.uop.fp_single, FPFormat.S,
                            FPFormat.D)),
                    fmt_int.eq(Cat(typ[1], 1)),
                ]

            with m.Case(UOpCode.FCVT_D_S, UOpCode.FCVT_S_D):
                m.d.comb += [
                    cast_en.eq(1),
                    fma_op.eq(FPUOperator.F2F),
                    fmt_in.eq(
                        Mux(self.req.bits.uop.fp_single, FPFormat.D,
                            FPFormat.S)),
                    fmt_out.eq(
                        Mux(self.req.bits.uop.fp_single, FPFormat.S,
                            FPFormat.D)),
                ]

            with m.Case(UOpCode.FSGNJ_S, UOpCode.FSGNJ_D):
                m.d.comb += [
                    cmp_en.eq(1),
                    fma_op.eq(FPUOperator.SGNJ),
                    fmt_in.eq(
                        Mux(self.req.bits.uop.fp_single, FPFormat.S,
                            FPFormat.D)),
                    fmt_out.eq(
                        Mux(self.req.bits.uop.fp_single, FPFormat.S,
                            FPFormat.D)),
                ]

            with m.Case(UOpCode.FMINMAX_S, UOpCode.FMINMAX_D):
                m.d.comb += [
                    cmp_en.eq(1),
                    fma_op.eq(FPUOperator.MINMAX),
                    fmt_in.eq(
                        Mux(self.req.bits.uop.fp_single, FPFormat.S,
                            FPFormat.D)),
                    fmt_out.eq(
                        Mux(self.req.bits.uop.fp_single, FPFormat.S,
                            FPFormat.D)),
                ]

            with m.Case(UOpCode.CMPR_S, UOpCode.CMPR_D):
                m.d.comb += [
                    cmp_en.eq(1),
                    fma_op.eq(FPUOperator.CMP),
                    fmt_in.eq(
                        Mux(self.req.bits.uop.fp_single, FPFormat.S,
                            FPFormat.D)),
                ]

            with m.Case(UOpCode.FCLASS_S, UOpCode.FCLASS_D):
                m.d.comb += [
                    cmp_en.eq(1),
                    fma_op.eq(FPUOperator.CLASSIFY),
                    fmt_in.eq(
                        Mux(self.req.bits.uop.fp_single, FPFormat.S,
                            FPFormat.D)),
                ]

        def set_fu_input(inp):
            m.d.comb += [
                inp.bits.in1.eq(self.req.bits.rs1_data),
                inp.bits.in2.eq(self.req.bits.rs2_data),
                inp.bits.in3.eq(self.req.bits.rs3_data),
                inp.bits.fn.eq(fma_op),
                inp.bits.fn_mod.eq(fma_op_mod),
                inp.bits.rm.eq(fp_rm),
                inp.bits.src_fmt.eq(fmt_in),
                inp.bits.dst_fmt.eq(fmt_out),
                inp.bits.int_fmt.eq(fmt_int),
            ]

            with m.If(swap32):
                m.d.comb += inp.bits.in3.eq(self.req.bits.rs2_data)

        dfma = m.submodules.dfma = FPUFMA(self.width,
                                          FPFormat.D,
                                          latency=self.fma_latency)
        set_fu_input(dfma.inp)
        m.d.comb += dfma.inp.valid.eq(self.req.valid & fma_en
                                      & (fmt_out == FPFormat.D))

        sfma = m.submodules.sfma = FPUFMA(32,
                                          FPFormat.S,
                                          latency=self.fma_latency)
        set_fu_input(sfma.inp)
        m.d.comb += sfma.inp.valid.eq(self.req.valid & fma_en
                                      & (fmt_out == FPFormat.S))

        fpiu = m.submodules.fpiu = FPUCastMulti(latency=self.fma_latency)
        set_fu_input(fpiu.inp)
        m.d.comb += fpiu.inp.valid.eq(self.req.valid & cast_en)

        dcmp = m.submodules.dcmp = FPUComp(self.width,
                                           FPFormat.D,
                                           latency=self.fma_latency)
        set_fu_input(dcmp.inp)
        m.d.comb += dcmp.inp.valid.eq(self.req.valid & cmp_en
                                      & (fmt_in == FPFormat.D))

        scmp = m.submodules.scmp = FPUComp(32,
                                           FPFormat.S,
                                           latency=self.fma_latency)
        set_fu_input(scmp.inp)
        m.d.comb += scmp.inp.valid.eq(self.req.valid & cmp_en
                                      & (fmt_in == FPFormat.S))

        m.d.comb += [
            self.resp.bits.data.eq(
                Mux(
                    dfma.out.valid, dfma.out.bits.data,
                    Mux(
                        sfma.out.valid, sfma.out.bits.data,
                        Mux(
                            fpiu.out.valid, fpiu.out.bits.data,
                            Mux(
                                dcmp.out.valid, dcmp.out.bits.data,
                                Mux(scmp.out.valid, scmp.out.bits.data,
                                    in_pipe.out.bits)))))),
            self.resp.bits.fflags.valid.eq(self.resp.valid),
            self.resp.bits.fflags.bits.eq(
                Mux(
                    dfma.out.valid, dfma.out.bits.status,
                    Mux(
                        sfma.out.valid, sfma.out.bits.status,
                        Mux(
                            fpiu.out.valid, fpiu.out.bits.status,
                            Mux(dcmp.out.valid, dcmp.out.bits.status,
                                Mux(scmp.out.valid, scmp.out.bits.status,
                                    0)))))),
        ]

        return m


class FDivUnit(IterativeFunctionalUnit):

    def __init__(self, width, params):
        self.width = width

        super().__init__(width, params)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        fdiv = m.submodules.fdiv = FPUDivSqrtMulti()

        m.d.comb += [
            fdiv.a.eq(self.req.bits.rs1_data),
            fdiv.b.eq(self.req.bits.rs2_data),
            fdiv.is_sqrt.eq((self.req.bits.uop.opcode == UOpCode.FSQRT_S)
                            | (self.req.bits.uop.opcode == UOpCode.FSQRT_D)),
            fdiv.fmt.eq(
                Mux(self.req.bits.uop.fp_single, FPFormat.S, FPFormat.D)),
            fdiv.in_valid.eq(self.req.valid),
            self.req.ready.eq(fdiv.in_ready),
            self.resp.bits.data.eq(fdiv.out.bits),
            self.resp.valid.eq(fdiv.out.valid),
            fdiv.out.ready.eq(self.resp.ready),
            fdiv.kill.eq(self.do_kill),
        ]

        return m
