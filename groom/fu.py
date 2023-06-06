from amaranth import *
from amaranth import tracer
from amaranth.hdl.ast import ValueCastable

from groom.if_stage import BranchResolution, WarpControlReq

from room.consts import *
from room.types import HasCoreParams, MicroOp
from room.alu import ALU, Multiplier, IntDiv
from room.utils import generate_imm, Pipe

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
            Signal(self.vaddr_bits + 1, name=f'{name}_addr{t}')
            for t in range(self.n_threads)
        ]

    @ValueCastable.lowermethod
    def as_value(self):
        return Cat(self.wid, self.uop, *self.data, *self.addr)

    def __len__(self):
        return len(Value.cast(self))

    def eq(self, rhs):
        return Value.cast(self).eq(Value.cast(rhs))


class FunctionalUnit(HasCoreParams, Elaboratable):

    def __init__(self, data_width, params, is_alu=False, is_gpu=False):
        super().__init__(params)

        self.req = Decoupled(ExecReq, data_width, params)
        self.resp = Decoupled(ExecResp, data_width, params)

        if is_alu:
            self.br_res = Valid(BranchResolution, params)

        if is_gpu:
            self.warp_ctrl = Valid(WarpControlReq, params)


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
            m.d.comb += opa_data[i].eq(
                Mux(uop.opa_sel == OpA.RS1, self.req.bits.rs1_data[i],
                    Mux(uop.opa_sel == OpA.PC, uop.pc, 0)))

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
            alu = ALU(self.xlen)
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
            ea_sign = Mux(sum[self.vaddr_bits - 1],
                          ~sum[self.vaddr_bits:self.xlen] == 0,
                          sum[self.vaddr_bits:self.xlen] != 0)
            eff_addr = Cat(sum[:self.vaddr_bits], ea_sign).as_unsigned()

            m.d.comb += [
                self.resp.bits.addr[w].eq(eff_addr),
                self.resp.bits.data[w].eq(self.req.bits.rs2_data[w]),
            ]

        return m


class GPUControlUnit(PipelinedFunctionalUnit):

    def __init__(self, params):
        super().__init__(1, params['xlen'], params, is_gpu=True)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        uop = self.req.bits.uop

        warp_ctrl = Valid(WarpControlReq, self.params)
        m.d.comb += warp_ctrl.bits.wid.eq(self.req.bits.wid)

        with m.If(self.req.valid):
            with m.Switch(uop.opcode):
                with m.Case(UOpCode.GPU_TMC):
                    m.d.comb += [
                        warp_ctrl.valid.eq(1),
                        warp_ctrl.bits.tmc_valid.eq(1),
                    ]

                    for w in reversed(range(self.n_threads)):
                        with m.If(uop.tmask[w]):
                            m.d.comb += warp_ctrl.bits.tmc_mask.eq(
                                self.req.bits.rs2_data[w])

                with m.Case(UOpCode.GPU_WSPAWN):
                    m.d.comb += [
                        warp_ctrl.valid.eq(1),
                        warp_ctrl.bits.wspawn_valid.eq(1),
                    ]

                    for w in reversed(range(self.n_threads)):
                        with m.If(uop.tmask[w]):
                            for i in range(self.n_warps):
                                m.d.comb += warp_ctrl.bits.wspawn_mask[i].eq(
                                    i < self.req.bits.rs2_data[w])

                            m.d.comb += warp_ctrl.bits.wspawn_pc.eq(
                                self.req.bits.rs1_data[w].as_signed() +
                                self.req.bits.uop.imm_packed[8:20].as_signed())

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

    def __init__(self, data_width, params):
        self.params = params

        super().__init__(data_width, params)

    def elaborate(self, platform):
        m = Module()

        uop = MicroOp(self.params)

        with m.If(self.req.fire):
            m.d.sync += uop.eq(self.req.bits.uop)

        m.d.comb += self.resp.bits.uop.eq(uop)

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
