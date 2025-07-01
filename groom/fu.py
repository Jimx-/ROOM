from amaranth import *
from amaranth import tracer
from amaranth.hdl.ast import ValueCastable

from groom.if_stage import BranchResolution, WarpControlReq
from groom.raster import RasterRequest, Fragment
from groom.csr import AutoCSR, ThreadLocalCSR, BankedCSR, CSRAccess
import groom.csrnames as gpucsrnames

from room.consts import *
from room.types import HasCoreParams, MicroOp
from room.alu import ALU, Multiplier, IntDiv
from room.fpu import FPUOperator, FPFormat, FPUFMA, FPUDivSqrtMulti, FPUCastMulti, FPUComp
from room.utils import generate_imm, generate_imm_type, generate_imm_rm, Pipe

from roomsoc.interconnect.stream import Valid, Decoupled


class ExecReq(HasCoreParams, ValueCastable):

    def __init__(self, data_width, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.wid = Signal(range(self.n_warps), name=f'{name}_wid')
        self.uop = MicroOp(params, name=f'{name}_uop')

        self.rs1_data = [
            Signal(data_width, name=f'{name}_rs1_data{t}')
            for t in range(self.n_threads)
        ]
        self.rs2_data = [
            Signal(data_width, name=f'{name}_rs2_data{t}')
            for t in range(self.n_threads)
        ]
        self.rs3_data = [
            Signal(data_width, name=f'{name}_rs3_data{t}')
            for t in range(self.n_threads)
        ]

    @ValueCastable.lowermethod
    def as_value(self):
        return Cat(self.wid, self.uop, *self.rs1_data, *self.rs2_data,
                   *self.rs3_data)

    def shape(self):
        return self.as_value().shape()

    def __len__(self):
        return len(Value.cast(self))

    def eq(self, rhs):
        return Value.cast(self).eq(Value.cast(rhs))


class ExecResp(HasCoreParams, ValueCastable):

    def __init__(self, data_width, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.wid = Signal(range(self.n_warps), name=f'{name}_wid')
        self.uop = MicroOp(params, name=f'{name}_uop')

        self.data = [
            Signal(data_width, name=f'{name}_data{t}')
            for t in range(self.n_threads)
        ]
        self.addr = [
            Signal(32, name=f'{name}_addr{t}') for t in range(self.n_threads)
        ]

    @ValueCastable.lowermethod
    def as_value(self):
        return Cat(self.wid, self.uop, *self.data, *self.addr)

    def shape(self):
        return self.as_value().shape()

    def __len__(self):
        return len(Value.cast(self))

    def eq(self, rhs):
        return Value.cast(self).eq(Value.cast(rhs))


class FunctionalUnit(HasCoreParams, Elaboratable):

    def __init__(self,
                 data_width,
                 params,
                 is_alu=False,
                 is_gpu=False,
                 is_raster=False):
        super().__init__(params)

        self.req = Decoupled(ExecReq, data_width, params)
        self.resp = Decoupled(ExecResp, data_width, params)

        if is_alu:
            self.br_res = Valid(BranchResolution, params)

        if is_gpu:
            self.warp_ctrl = Valid(WarpControlReq, params)

        if is_raster:
            self.raster_req = Decoupled(RasterRequest, params)


class PipelinedFunctionalUnit(FunctionalUnit):

    def __init__(self,
                 num_stages,
                 data_width,
                 params,
                 is_alu=False,
                 is_gpu=False):
        self.params = params
        self.num_stages = num_stages

        super().__init__(data_width, params, is_alu=is_alu, is_gpu=is_gpu)

    def elaborate(self, platform):
        m = Module()

        m.d.comb += self.req.ready.eq(1)

        if self.num_stages > 0:
            self.valids = Signal(self.num_stages)
            self.wids = [
                MicroOp(self.params, name=f's{i}_wid')
                for i in range(self.num_stages)
            ]
            self.uops = [
                MicroOp(self.params, name=f's{i}_uop')
                for i in range(self.num_stages)
            ]

            m.d.sync += [
                self.valids[0].eq(self.req.valid),
                self.wids[0].eq(self.req.bits.wid),
                self.uops[0].eq(self.req.bits.uop),
            ]

            for i in range(1, self.num_stages):
                m.d.sync += [
                    self.valids[i].eq(self.valids[i - 1]),
                    self.wids[i].eq(self.wids[i - 1]),
                    self.uops[i].eq(self.uops[i - 1]),
                ]

            m.d.comb += [
                self.resp.valid.eq(self.valids[self.num_stages - 1]),
                self.resp.bits.wid.eq(self.wids[self.num_stages - 1]),
                self.resp.bits.uop.eq(self.uops[self.num_stages - 1]),
            ]
        else:
            m.d.comb += [
                self.resp.valid.eq(self.req.valid),
                self.resp.bits.wid.eq(self.req.bits.wid),
                self.resp.bits.uop.eq(self.req.bits.uop),
            ]

        return m


class ALUUnit(PipelinedFunctionalUnit):

    def __init__(self, data_width, params, num_stages=1):
        super().__init__(num_stages, data_width, params, is_alu=True)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        uop = self.req.bits.uop

        #
        # Operands
        #

        imm = generate_imm(uop.imm_packed, uop.imm_sel)
        if self.xlen == 64:
            imm = Cat(imm, Repl(imm[31], 32))

        opa_data = [
            Signal(self.xlen, name=f'opa_data{i}')
            for i in range(self.n_threads)
        ]
        opb_data = [
            Signal(self.xlen, name=f'opb_data{i}')
            for i in range(self.n_threads)
        ]

        for i in range(self.n_threads):
            opa_data_shl = Signal(self.xlen, name=f'opa_data_shl{i}')
            if self.use_zba:
                m.d.comb += opa_data_shl.eq(
                    Mux(uop.opa_is_uw, self.req.bits.rs1_data[i][:32],
                        self.req.bits.rs1_data[i]) << uop.opa_shamt)

            m.d.comb += opa_data[i].eq(
                Mux(
                    uop.opa_sel == OpA.RS1, self.req.bits.rs1_data[i],
                    Mux(uop.opa_sel == OpA.PC, uop.pc,
                        Mux(uop.opa_sel == OpA.RS1SHL, opa_data_shl, 0))))

            m.d.comb += opb_data[i].eq(
                Mux(
                    uop.opb_sel == OpB.IMM, imm,
                    Mux(
                        uop.opb_sel == OpB.IMMC, self.req.bits.uop.lrs1,
                        Mux(uop.opb_sel == OpB.RS2, self.req.bits.rs2_data[i],
                            Mux(uop.opb_sel == OpB.NEXT, 4, 0)))))

        #
        # ALU
        #

        alus = []
        for i in range(self.n_threads):
            alu = ALU(self.xlen, use_zicond=self.use_zicond)
            setattr(m.submodules, f'alu{i}', alu)
            m.d.comb += [
                alu.in1.eq(opa_data[i]),
                alu.in2.eq(opb_data[i]),
                alu.fn.eq(uop.alu_fn),
                alu.dw.eq(uop.alu_dw),
            ]
            alus.append(alu)

        #
        # Branch unit
        #

        br_res = Valid(BranchResolution, self.params)

        br_eq = Signal()
        br_ltu = Signal()
        br_lt = Signal()
        jalr_rs1_data = Signal(self.xlen)

        for w in reversed(range(self.n_threads)):
            with m.If(uop.tmask[w]):
                rs1 = self.req.bits.rs1_data[w]
                rs2 = self.req.bits.rs2_data[w]

                m.d.comb += [
                    br_eq.eq(rs1 == rs2),
                    br_ltu.eq(rs1.as_unsigned() < rs2.as_unsigned()),
                    br_lt.eq((~(rs1[self.xlen - 1] ^ rs2[self.xlen - 1])
                              & br_ltu)
                             | (rs1[self.xlen - 1] & ~rs2[self.xlen - 1])),
                    jalr_rs1_data.eq(rs1),
                ]

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

        br_taken = self.req.valid & (uop.is_br | uop.is_jal
                                     | uop.is_jalr) & (pc_sel
                                                       != PCSel.PC_PLUS_4)

        is_br = self.req.valid & uop.is_br
        is_jal = self.req.valid & uop.is_jal
        is_jalr = self.req.valid & uop.is_jalr
        target_offset = imm[0:22].as_signed()

        jalr_target = Signal(32)
        br_target = Signal(32)

        m.d.comb += [
            jalr_target.eq(jalr_rs1_data + target_offset),
            br_target.eq(uop.pc + target_offset),
        ]

        m.d.comb += [
            br_res.valid.eq(is_br | is_jal | is_jalr),
            br_res.bits.wid.eq(self.req.bits.wid),
            br_res.bits.taken.eq(br_taken),
            br_res.bits.target.eq(Mux(is_jalr, jalr_target, br_target))
        ]

        #
        # Output
        #

        data = [[
            Signal(self.xlen, name=f's{i}_data{w}')
            for w in range(self.n_threads)
        ] for i in range(self.num_stages)]

        br_res_stages = [
            Valid(BranchResolution, self.params, name=f's{i}_br_res')
            for i in range(self.num_stages)
        ]

        for w in range(self.n_threads):
            m.d.sync += [
                data[0][w].eq(alus[w].out),
                br_res_stages[0].eq(br_res),
            ]

            for i in range(1, self.num_stages):
                m.d.sync += [
                    data[i][w].eq(data[i - 1][w]),
                    br_res_stages[i].eq(br_res_stages[w]),
                ]

            m.d.comb += [
                self.resp.bits.data[w].eq(data[self.num_stages - 1][w]),
                self.br_res.eq(br_res_stages[self.num_stages - 1]),
            ]

        return m


class MultiplierUnit(PipelinedFunctionalUnit):

    def __init__(self, width, latency, params):
        self.width = width
        self.latency = latency

        super().__init__(latency, width, params)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        for w in range(self.n_threads):
            mul = Multiplier(self.width, self.latency)
            setattr(m.submodules, f'mul{w}', mul)

            m.d.comb += [
                mul.req.bits.fn.eq(self.req.bits.uop.alu_fn),
                mul.req.bits.dw.eq(self.req.bits.uop.alu_dw),
                mul.req.bits.in1.eq(self.req.bits.rs1_data[w]),
                mul.req.bits.in2.eq(self.req.bits.rs2_data[w]),
                mul.req.valid.eq(self.req.valid),
            ]

            m.d.comb += self.resp.bits.data[w].eq(mul.resp_data)

        return m


class AddrGenUnit(PipelinedFunctionalUnit):

    def __init__(self, params):
        super().__init__(0, params['xlen'], params)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        for w in range(self.n_threads):
            sum = self.req.bits.rs1_data[w].as_signed(
            ) + self.req.bits.uop.imm_packed[8:20].as_signed()

            m.d.comb += [
                self.resp.bits.addr[w].eq(sum),
                self.resp.bits.data[w].eq(self.req.bits.rs2_data[w]),
            ]

        return m


class GPUControlUnit(PipelinedFunctionalUnit):

    def __init__(self, params):
        super().__init__(1, params['xlen'], params, is_gpu=True)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        uop = self.req.bits.uop

        taken_mask = Signal(self.n_threads)
        not_taken_mask = Signal(self.n_threads)
        for w in range(self.n_threads):
            taken = self.req.bits.rs2_data[w].any()
            m.d.comb += [
                taken_mask[w].eq(uop.tmask[w] & taken),
                not_taken_mask[w].eq(uop.tmask[w] & ~taken),
            ]

        warp_ctrl = Valid(WarpControlReq, self.params)
        m.d.comb += warp_ctrl.bits.wid.eq(self.req.bits.wid)

        with m.If(self.req.valid):
            with m.Switch(uop.opcode):
                with m.Case(UOpCode.GPU_TMC):
                    m.d.comb += [
                        warp_ctrl.valid.eq(1),
                        warp_ctrl.bits.tmc.valid.eq(1),
                    ]

                    for w in reversed(range(self.n_threads)):
                        with m.If(uop.tmask[w]):
                            m.d.comb += warp_ctrl.bits.tmc.mask.eq(
                                self.req.bits.rs2_data[w])

                with m.Case(UOpCode.GPU_PRED):
                    m.d.comb += [
                        warp_ctrl.valid.eq(1),
                        warp_ctrl.bits.tmc.valid.eq(1),
                        warp_ctrl.bits.tmc.mask.eq(
                            Mux(taken_mask.any(), taken_mask, uop.tmask)),
                    ]

                with m.Case(UOpCode.GPU_WSPAWN):
                    m.d.comb += [
                        warp_ctrl.valid.eq(1),
                        warp_ctrl.bits.wspawn.valid.eq(1),
                    ]

                    for w in reversed(range(self.n_threads)):
                        with m.If(uop.tmask[w]):
                            for i in range(self.n_warps):
                                m.d.comb += warp_ctrl.bits.wspawn.mask[i].eq(
                                    i < self.req.bits.rs2_data[w])

                            m.d.comb += warp_ctrl.bits.wspawn.pc.eq(
                                self.req.bits.rs1_data[w].as_signed() +
                                self.req.bits.uop.imm_packed[8:20].as_signed())

                with m.Case(UOpCode.GPU_SPLIT):
                    m.d.comb += [
                        warp_ctrl.valid.eq(1),
                        warp_ctrl.bits.split.valid.eq(1),
                        warp_ctrl.bits.split.diverged.eq(
                            taken_mask.any() & not_taken_mask.any()),
                        warp_ctrl.bits.split.then_mask.eq(taken_mask),
                        warp_ctrl.bits.split.else_mask.eq(not_taken_mask),
                        warp_ctrl.bits.split.pc.eq(uop.pc + 4),
                    ]

                with m.Case(UOpCode.GPU_BARRIER):
                    m.d.comb += [
                        warp_ctrl.valid.eq(1),
                        warp_ctrl.bits.barrier.valid.eq(1),
                    ]

                    for w in reversed(range(self.n_threads)):
                        with m.If(uop.tmask[w]):
                            m.d.comb += [
                                warp_ctrl.bits.barrier.id.eq(
                                    self.req.bits.rs1_data[w]),
                                warp_ctrl.bits.barrier.count.eq(
                                    self.req.bits.rs2_data[w] - 1),
                            ]

        #
        # Output
        #

        warp_ctrl_pipe = m.submodules.warp_ctrl_pipe = Pipe(
            len(warp_ctrl.bits), depth=self.num_stages)
        m.d.comb += [
            warp_ctrl_pipe.in_data.eq(warp_ctrl.bits),
            warp_ctrl_pipe.in_valid.eq(warp_ctrl.valid),
            self.warp_ctrl.eq(warp_ctrl_pipe.out),
        ]

        return m


class IterativeFunctionalUnit(FunctionalUnit):

    def __init__(self, data_width, params, is_raster=False):
        self.params = params

        super().__init__(data_width, params, is_raster=is_raster)

    def elaborate(self, platform):
        m = Module()

        wid = Signal(range(self.n_warps))
        uop = MicroOp(self.params)

        with m.If(self.req.fire):
            m.d.sync += [
                wid.eq(self.req.bits.wid),
                uop.eq(self.req.bits.uop),
            ]

        m.d.comb += [
            self.resp.bits.wid.eq(wid),
            self.resp.bits.uop.eq(uop),
        ]

        return m


class DivUnit(IterativeFunctionalUnit):

    def __init__(self, width, params):
        self.width = width

        super().__init__(width, params)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        tmask_valid = Signal()
        tmask = Signal(self.n_threads)

        with m.If(self.req.fire):
            m.d.sync += [
                tmask.eq(self.req.bits.uop.tmask),
                tmask_valid.eq(1),
            ]
        with m.Elif(self.resp.fire):
            m.d.sync += [
                tmask.eq(0),
                tmask_valid.eq(0),
            ]
        m.d.comb += self.req.ready.eq(~tmask_valid)

        div_resp_valid = Signal(self.n_threads)
        for w in range(self.n_threads):
            div = IntDiv(self.width)
            setattr(m.submodules, f'div{w}', div)

            m.d.comb += [
                div.req.bits.fn.eq(self.req.bits.uop.alu_fn),
                div.req.bits.dw.eq(self.req.bits.uop.alu_dw),
                div.req.bits.in1.eq(self.req.bits.rs1_data[w]),
                div.req.bits.in2.eq(self.req.bits.rs2_data[w]),
                div.req.valid.eq(self.req.valid & self.req.bits.uop.tmask[w]),
                self.resp.bits.data[w].eq(div.resp.bits),
                div_resp_valid[w].eq(div.resp.valid),
                div.resp.ready.eq(~tmask_valid | self.resp.fire),
            ]

        m.d.comb += self.resp.valid.eq(tmask_valid
                                       & ((tmask & div_resp_valid) == tmask))

        return m


class IntToFPUnit(PipelinedFunctionalUnit):

    def __init__(self, width, latency, params):
        self.width = width
        self.latency = latency

        super().__init__(latency, width, params)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        cast_en = Signal()

        with m.Switch(self.req.bits.uop.opcode):
            with m.Case(UOpCode.FCVT_S_X, UOpCode.FCVT_D_X):
                m.d.comb += cast_en.eq(1)

        typ = generate_imm_type(self.req.bits.uop.imm_packed)

        for w in range(self.n_threads):
            in_pipe = Pipe(width=len(self.req.bits.rs1_data[w]),
                           depth=self.latency)
            setattr(m.submodules, f'in_pipe{w}', in_pipe)
            m.d.comb += [
                in_pipe.in_valid.eq(self.req.valid),
                in_pipe.in_data.eq(self.req.bits.rs1_data[w]),
            ]

            ifpu = FPUCastMulti(latency=self.latency)
            setattr(m.submodules, f'ifpu{w}', ifpu)

            m.d.comb += [
                ifpu.inp.valid.eq(self.req.valid & cast_en),
                ifpu.inp.bits.fn.eq(FPUOperator.I2F),
                ifpu.inp.bits.fn_mod.eq(typ[0]),
                ifpu.inp.bits.in1.eq(self.req.bits.rs1_data[w]),
                ifpu.inp.bits.dst_fmt.eq(
                    Mux(self.req.bits.uop.fp_single, FPFormat.S, FPFormat.D)),
                ifpu.inp.bits.int_fmt.eq(Cat(typ[1], 1)),
            ]

            m.d.comb += self.resp.bits.data[w].eq(
                Mux(ifpu.out.valid, ifpu.out.bits.data, in_pipe.out.bits))

        return m


class FPUUnit(PipelinedFunctionalUnit):

    def __init__(self, width, params):
        self.width = width

        super().__init__(params['fma_latency'], width, params)

    def elaborate(self, platform):
        m = super().elaborate(platform)

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
                    fma_op.eq(1),
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
                    fma_op.eq(1),
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

        for w in range(self.n_threads):
            in_pipe = Pipe(width=len(self.req.bits.rs1_data[w]),
                           depth=self.fma_latency)
            setattr(m.submodules, f'in_pipe{w}', in_pipe)
            m.d.comb += [
                in_pipe.in_valid.eq(self.req.valid),
                in_pipe.in_data.eq(self.req.bits.rs1_data[w]),
            ]

            def set_fu_input(inp):
                m.d.comb += [
                    inp.bits.in1.eq(self.req.bits.rs1_data[w]),
                    inp.bits.in2.eq(self.req.bits.rs2_data[w]),
                    inp.bits.in3.eq(self.req.bits.rs3_data[w]),
                    inp.bits.fn.eq(fma_op),
                    inp.bits.fn_mod.eq(fma_op_mod),
                    inp.bits.rm.eq(fp_rm),
                    inp.bits.src_fmt.eq(fmt_in),
                    inp.bits.dst_fmt.eq(fmt_out),
                    inp.bits.int_fmt.eq(fmt_int),
                ]

                with m.If(swap32):
                    m.d.comb += inp.bits.in3.eq(self.req.bits.rs2_data[w])

            sfma = FPUFMA(32, FPFormat.S, latency=self.fma_latency)
            setattr(m.submodules, f'sfma{w}', sfma)
            set_fu_input(sfma.inp)
            m.d.comb += sfma.inp.valid.eq(self.req.valid & fma_en
                                          & (fmt_out == FPFormat.S))

            fpiu = FPUCastMulti(latency=self.fma_latency)
            setattr(m.submodules, f'fpiu{w}', fpiu)
            set_fu_input(fpiu.inp)
            m.d.comb += fpiu.inp.valid.eq(self.req.valid & cast_en)

            scmp = FPUComp(32, FPFormat.S, latency=self.fma_latency)
            setattr(m.submodules, f'scmp{w}', scmp)
            set_fu_input(scmp.inp)
            m.d.comb += scmp.inp.valid.eq(self.req.valid & cmp_en
                                          & (fmt_in == FPFormat.S))

            m.d.comb += self.resp.bits.data[w].eq(
                Mux(
                    sfma.out.valid, sfma.out.bits.data,
                    Mux(
                        fpiu.out.valid, fpiu.out.bits.data,
                        Mux(scmp.out.valid, scmp.out.bits.data,
                            in_pipe.out.bits))))

        return m


class FDivUnit(IterativeFunctionalUnit):

    def __init__(self, width, params):
        self.width = width

        super().__init__(width, params)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        tmask_valid = Signal()
        tmask = Signal(self.n_threads)

        with m.If(self.req.fire):
            m.d.sync += [
                tmask.eq(self.req.bits.uop.tmask),
                tmask_valid.eq(1),
            ]
        with m.Elif(self.resp.fire):
            m.d.sync += [
                tmask.eq(0),
                tmask_valid.eq(0),
            ]
        m.d.comb += self.req.ready.eq(~tmask_valid)

        div_resp_valid = Signal(self.n_threads)
        for w in range(self.n_threads):
            fdiv = FPUDivSqrtMulti()
            setattr(m.submodules, f'fdiv{w}', fdiv)

            m.d.comb += [
                fdiv.a.eq(self.req.bits.rs1_data[w]),
                fdiv.b.eq(self.req.bits.rs2_data[w]),
                fdiv.is_sqrt.eq((self.req.bits.uop.opcode == UOpCode.FSQRT_S)
                                |
                                (self.req.bits.uop.opcode == UOpCode.FSQRT_D)),
                fdiv.fmt.eq(
                    Mux(self.req.bits.uop.fp_single, FPFormat.S, FPFormat.D)),
                fdiv.in_valid.eq(self.req.fire & self.req.bits.uop.tmask[w]),
                self.resp.bits.data[w].eq(fdiv.out.bits),
                div_resp_valid[w].eq(fdiv.out.valid),
                fdiv.out.ready.eq(~tmask_valid | self.resp.fire),
            ]

        m.d.comb += self.resp.valid.eq(tmask_valid
                                       & ((tmask & div_resp_valid) == tmask))

        return m


class RasterUnit(IterativeFunctionalUnit, AutoCSR):

    def __init__(self, width, params):
        super().__init__(width, params, is_raster=True)

        self.rastpos = BankedCSR(ThreadLocalCSR, gpucsrnames.rastpos,
                                 [('value', 32, CSRAccess.RO)], params)
        self.rastpid = BankedCSR(ThreadLocalCSR, gpucsrnames.rastpid,
                                 [('value', 32, CSRAccess.RO)], params)
        self.rastbca = BankedCSR(ThreadLocalCSR, gpucsrnames.rastbca,
                                 [('value', 32, CSRAccess.RO)], params)
        self.rastbcb = BankedCSR(ThreadLocalCSR, gpucsrnames.rastbcb,
                                 [('value', 32, CSRAccess.RO)], params)
        self.rastbcc = BankedCSR(ThreadLocalCSR, gpucsrnames.rastbcc,
                                 [('value', 32, CSRAccess.RO)], params)
        self.rastmask = BankedCSR(ThreadLocalCSR, gpucsrnames.rastmask,
                                  [('value', 4, CSRAccess.RO)], params)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        wid = Signal(range(self.n_warps))
        tmask = Signal(self.n_threads)
        fragments = [[
            Fragment(name=f'fragment{w}_{t}') for t in range(self.n_threads)
        ] for w in range(self.n_warps)]

        for w in range(self.n_warps):
            for t in range(self.n_threads):
                m.d.comb += [
                    self.rastpos.warps[w].r[t][:16].eq(fragments[w][t].x),
                    self.rastpos.warps[w].r[t][16:].eq(fragments[w][t].y),
                    self.rastpid.warps[w].r[t].eq(fragments[w][t].pid),
                    self.rastbca.warps[w].r[t].eq(
                        fragments[w][t].barycentric.x),
                    self.rastbcb.warps[w].r[t].eq(
                        fragments[w][t].barycentric.y),
                    self.rastbcc.warps[w].r[t].eq(
                        fragments[w][t].barycentric.z),
                    self.rastmask.warps[w].r[t].eq(fragments[w][t].mask),
                ]

        with m.FSM():
            with m.State('IDLE'):
                m.d.comb += self.req.ready.eq(1)

                with m.If(self.req.fire):
                    m.d.sync += [
                        wid.eq(self.req.bits.wid),
                        tmask.eq(self.req.bits.uop.tmask),
                    ]
                    m.next = 'ACT'

            with m.State('ACT'):
                m.d.comb += [
                    self.raster_req.valid.eq(1),
                    self.raster_req.bits.tmask.eq(tmask),
                ]

                with m.If(self.raster_req.ready):
                    with m.Switch(wid):
                        for w in range(self.n_warps):
                            with m.Case(w):
                                for frag, resp in zip(
                                        fragments[w],
                                        self.raster_req.bits.resp):
                                    m.d.sync += frag.eq(resp.bits)

                    for t in range(self.n_threads):
                        m.d.sync += self.resp.bits.data[t].eq(
                            self.raster_req.bits.resp[t].valid),

                    m.next = 'ACK'

            with m.State('ACK'):
                m.d.comb += self.resp.valid.eq(1)

                with m.If(self.resp.ready):
                    m.next = 'IDLE'

        return m
