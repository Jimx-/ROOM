from amaranth import *
from amaranth import tracer
from enum import IntEnum

from room.consts import *
from room.types import MicroOp
from room.if_stage import GetPCResp
from room.branch import BranchResolution, BranchUpdate
from room.fpu import FPUOperator, FPFormat, FPUFMA, FPUDivSqrtMulti
from room.utils import generate_imm


class ExecReq:

    def __init__(self, data_width, params, name=None, src_loc_at=0):
        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.uop = MicroOp(params, name=f'{name}_uop')
        self.valid = Signal(name=f'{name}_valid')

        self.rs1_data = Signal(data_width, name=f'{name}_rs1_data')
        self.rs2_data = Signal(data_width, name=f'{name}_rs2_data')
        self.rs3_data = Signal(data_width, name=f'{name}_rs3_data')

        self.kill = Signal(name=f'{name}_kill')

        self.ready = Signal(name=f'{name}_ready')

    def eq(self, rhs):
        attrs = ['uop', 'valid', 'rs1_data', 'rs2_data', 'rs3_data', 'kill']
        return [getattr(self, a).eq(getattr(rhs, a)) for a in attrs]


class ExecResp:

    def __init__(self, data_width, params, name=None, src_loc_at=0):
        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.vaddr_bits = params['vaddr_bits']

        self.uop = MicroOp(params, name=f'{name}_uop')
        self.valid = Signal(name=f'{name}_valid')

        self.data = Signal(data_width, name=f'{name}_data')
        self.addr = Signal(self.vaddr_bits + 1, name=f'{name}_addr')

        self.ready = Signal(name=f'{name}_ready')

    def eq(self, rhs):
        attrs = ['uop', 'valid', 'addr', 'data']
        return [getattr(self, a).eq(getattr(rhs, a)) for a in attrs]

    def connect(self, subord):
        return subord.eq(self) + [
            subord.valid.eq(self.valid),
            self.ready.eq(subord.ready),
        ]


class FunctionalUnit(Elaboratable):

    def __init__(self, data_width, params, is_jmp=False, is_alu=False):
        self.is_jmp = is_jmp

        self.req = ExecReq(data_width, params, name='req')
        self.resp = ExecResp(data_width, params, name='resp')

        self.br_update = BranchUpdate(params)

        self.br_res = is_alu and BranchResolution(params,
                                                  name='br_res') or None
        self.get_pc = None
        if is_jmp:
            self.get_pc = GetPCResp(name='get_pc')


class PipelinedFunctionalUnit(FunctionalUnit):

    def __init__(self,
                 num_stages,
                 data_width,
                 params,
                 is_jmp=False,
                 is_alu=False):
        self.params = params
        self.num_stages = num_stages

        super().__init__(data_width, params, is_jmp=is_jmp, is_alu=is_alu)

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
                self.valids[0].eq(self.req.valid
                                  & ~self.br_update.uop_killed(self.req.uop)
                                  & ~self.req.kill),
                self.uops[0].eq(self.req.uop),
                self.uops[0].br_mask.eq(
                    self.br_update.get_new_br_mask(self.req.uop.br_mask)),
            ]

            for i in range(1, self.num_stages):
                m.d.sync += [
                    self.valids[i].eq(
                        self.valids[i - 1]
                        & ~self.br_update.uop_killed(self.uops[i - 1])
                        & ~self.req.kill),
                    self.uops[i].eq(self.uops[i - 1]),
                    self.uops[i].br_mask.eq(
                        self.br_update.get_new_br_mask(self.uops[i -
                                                                 1].br_mask)),
                ]

            m.d.comb += [
                self.resp.valid.eq(self.valids[self.num_stages - 1]
                                   & ~self.br_update.uop_killed(self.uops[
                                       self.num_stages - 1])),
                self.resp.uop.eq(self.uops[self.num_stages - 1]),
                self.resp.uop.br_mask.eq(
                    self.br_update.get_new_br_mask(self.uops[self.num_stages -
                                                             1].br_mask)),
            ]
        else:
            m.d.comb += [
                self.resp.valid.eq(self.req.valid
                                   & ~self.br_update.uop_killed(self.req.uop)),
                self.resp.uop.eq(self.req.uop),
                self.resp.uop.br_mask.eq(
                    self.br_update.get_new_br_mask(self.req.uop.br_mask)),
            ]

        return m


class ALU(Elaboratable):

    def __init__(self, width):
        self.width = width

        self.fn = Signal(ALUOperator)
        self.dw = Signal(ALUWidth)
        self.in1 = Signal(width)
        self.in2 = Signal(width)
        self.out = Signal(width)

    def elaborate(self, platform):
        m = Module()

        is_sub = self.fn[3]
        is_cmp = self.fn >= ALUOperator.SLT
        is_cmp_unsigned = self.fn[1]

        #
        # ADD, SUB
        #

        adder_out = Signal(self.width)
        in2_inv = Mux(is_sub == 1, ~self.in2, self.in2)
        inv = Signal(self.width)
        m.d.comb += inv.eq(in2_inv)
        m.d.comb += adder_out.eq(self.in1 + in2_inv + is_sub)

        #
        # SLT, SLTU
        #

        in1_xor_in2 = self.in1 ^ in2_inv
        slt = Mux(
            self.in1[self.width - 1] == self.in2[self.width - 1],
            adder_out[self.width - 1],
            Mux(is_cmp_unsigned, self.in2[self.width - 1],
                self.in1[self.width - 1]))

        #
        # SLL, SRL, SRA
        #

        shin_r = Signal(self.width)

        if self.width == 32:
            shamt = self.in2[:5]
            m.d.comb += shin_r.eq(self.in1)
        else:
            shin_hi_32 = Repl(is_sub & self.in1[31], 32)
            shin_hi = Mux(self.dw == ALUWidth.DW_XLEN, self.in1[32:64],
                          shin_hi_32)
            shamt = Cat(self.in2[:5],
                        self.in2[5] & (self.dw == ALUWidth.DW_XLEN))
            m.d.comb += shin_r.eq(Cat(self.in1[:32], shin_hi))

        shin_l = Signal(self.width)
        for a, b in zip(shin_l, reversed(shin_r)):
            m.d.comb += a.eq(b)

        shin = Mux((self.fn == ALUOperator.SR) | (self.fn == ALUOperator.SRA),
                   shin_r, shin_l)

        shout_r = Signal(self.width)
        shout_l = Signal(self.width)
        m.d.comb += shout_r.eq(
            Cat(shin, is_sub & shin[self.width - 1]).as_signed() >> shamt)
        for a, b in zip(shout_l, reversed(shout_r)):
            m.d.comb += a.eq(b)
        shout = Mux((self.fn == ALUOperator.SR) | (self.fn == ALUOperator.SRA),
                    shout_r, 0) | Mux(self.fn == ALUOperator.SL, shout_l, 0)

        #
        # AND, OR, XOR
        #

        logic = Mux((self.fn == ALUOperator.XOR) |
                    (self.fn == ALUOperator.OR), in1_xor_in2, 0) | Mux(
                        (self.fn == ALUOperator.AND) |
                        (self.fn == ALUOperator.OR), self.in1 & self.in2, 0)

        shift_logic = (is_cmp & slt) | logic | shout

        out = Signal.like(self.out)
        m.d.comb += out.eq(
            Mux((self.fn == ALUOperator.ADD) | (self.fn == ALUOperator.SUB),
                adder_out, shift_logic))

        m.d.comb += self.out.eq(out)
        if self.width > 32:
            with m.If(self.dw == ALUWidth.DW_32):
                m.d.comb += self.out.eq(Cat(out[:32], Repl(out[31], 32)))

        return m


class ALUUnit(PipelinedFunctionalUnit):

    def __init__(self, data_width, params, is_jmp=False, num_stages=1):
        self.xlen = params['xlen']

        super().__init__(num_stages,
                         data_width,
                         params,
                         is_jmp=is_jmp,
                         is_alu=True)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        uop = self.req.uop

        #
        # Operands
        #

        imm = generate_imm(uop.imm_packed, uop.imm_sel)
        if self.xlen == 64:
            imm = Cat(imm, Repl(imm[31], 32))

        opa_data = Signal(self.xlen)
        opb_data = Signal(self.xlen)

        if self.is_jmp:
            uop_pc = self.get_pc.pc | uop.pc_lsb

            m.d.comb += opa_data.eq(
                Mux(uop.opa_sel == OpA.RS1, self.req.rs1_data,
                    Mux(uop.opa_sel == OpA.PC, uop_pc, 0)))
        else:
            m.d.comb += opa_data.eq(
                Mux(uop.opa_sel == OpA.RS1, self.req.rs1_data, 0))

        m.d.comb += opb_data.eq(
            Mux(
                uop.opb_sel == OpB.IMM, imm,
                Mux(
                    uop.opb_sel == OpB.IMMC, self.req.uop.prs1[:5],
                    Mux(uop.opb_sel == OpB.RS2, self.req.rs2_data,
                        Mux(uop.opb_sel == OpB.NEXT, Mux(uop.is_rvc, 2, 4),
                            0)))))

        #
        # ALU
        #

        alu = m.submodules.alu = ALU(self.xlen)
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
        with m.If(self.req.kill | self.br_update.uop_killed(uop)):
            m.d.comb += killed.eq(1)

        rs1 = self.req.rs1_data
        rs2 = self.req.rs2_data
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
            m.d.comb += self.br_res.jalr_target.eq(self.req.rs1_data +
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
            self.resp.data.eq(data[self.num_stages - 1]),
        ]

        return m


class AddrGenUnit(PipelinedFunctionalUnit):

    def __init__(self, params):
        super().__init__(0, params['xlen'], params)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        m.d.comb += [
            self.resp.addr.eq(self.req.rs1_data +
                              self.req.uop.imm_packed[8:20].as_signed()),
            self.resp.data.eq(self.req.rs2_data),
        ]

        return m


class MulDivReq(Record):

    def __init__(self, width, name=None, src_loc_at=0):
        super().__init__([
            ('fn', Shape.cast(ALUOperator).width),
            ('dw', Shape.cast(ALUWidth).width),
            ('in1', width),
            ('in2', width),
        ],
                         name=name,
                         src_loc_at=1 + src_loc_at)


class Multiplier(Elaboratable):

    def __init__(self, width, latency):
        self.width = width
        self.latency = latency

        self.req = MulDivReq(width)
        self.req_valid = Signal()

        self.resp_data = Signal(width)

    def elaborate(self, platform):
        m = Module()

        in_req = MulDivReq(self.width, name='in')
        in_valid = Signal()
        m.d.sync += [
            in_req.eq(self.req),
            in_valid.eq(self.req_valid),
        ]

        fn = in_req.fn
        h = (fn == ALUOperator.MULH) | (fn == ALUOperator.MULHU) | (
            fn == ALUOperator.MULHSU)
        half = (self.width > 32) & (self.req.dw == ALUWidth.DW_32)
        lhs_signed = (fn == ALUOperator.MULH) | (fn == ALUOperator.MULHSU)
        rhs_signed = (fn == ALUOperator.MULH)

        lhs = Cat(in_req.in1, (lhs_signed & in_req.in1[-1])).as_signed()
        rhs = Cat(in_req.in2, (rhs_signed & in_req.in2[-1])).as_signed()
        prod = lhs * rhs
        half_sext = Cat(prod[:self.width // 2],
                        Repl(prod[self.width // 2 - 1], self.width // 2))
        muxed = Mux(h, prod[self.width:self.width * 2],
                    Mux(half, half_sext, prod[:self.width]))

        valid = in_valid
        data = muxed

        for _ in range(self.latency - 1):
            next_valid = Signal()
            next_data = Signal.like(data)

            m.d.sync += next_valid.eq(valid)

            with m.If(valid):
                m.d.sync += next_data.eq(data)

            valid = next_valid
            data = next_data

        m.d.comb += self.resp_data.eq(data)

        return m


class MultiplierUnit(PipelinedFunctionalUnit):

    def __init__(self, width, latency, params):
        self.width = width
        self.latency = latency

        super().__init__(latency, width, params)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        mul = m.submodules.mul = Multiplier(self.width, self.latency)
        m.d.comb += [
            mul.req.fn.eq(self.req.uop.alu_fn),
            mul.req.dw.eq(self.req.uop.alu_dw),
            mul.req.in1.eq(self.req.rs1_data),
            mul.req.in2.eq(self.req.rs2_data),
            mul.req_valid.eq(self.req.valid),
        ]

        m.d.comb += self.resp.data.eq(mul.resp_data)

        return m


class IterativeFunctionalUnit(FunctionalUnit):

    def __init__(self, data_width, params):
        self.params = params

        self.do_kill = Signal()

        super().__init__(data_width, params)

    def elaborate(self, platform):
        m = Module()

        uop = MicroOp(self.params)

        with m.If(self.req.valid & self.req.ready):
            m.d.comb += self.do_kill.eq(
                self.req.kill | self.br_update.uop_killed(self.req.uop))
            m.d.sync += [
                uop.eq(self.req.uop),
                uop.br_mask.eq(
                    self.br_update.get_new_br_mask(self.req.uop.br_mask))
            ]
        with m.Else():
            m.d.comb += self.do_kill.eq(self.req.kill
                                        | self.br_update.uop_killed(uop))
            m.d.sync += [
                uop.br_mask.eq(self.br_update.get_new_br_mask(uop.br_mask))
            ]

        m.d.comb += self.resp.uop.eq(uop)

        return m


class IntDiv(Elaboratable):

    def __init__(self, width):
        self.width = width

        self.req = MulDivReq(width)
        self.req_valid = Signal()
        self.req_ready = Signal()

        self.resp_data = Signal(width)
        self.resp_valid = Signal()
        self.resp_ready = Signal()

        self.kill = Signal()

    def elaborate(self, platform):
        m = Module()

        req = MulDivReq(self.width, name='in')
        count = Signal(range(self.width + 1))
        neg_out = Signal()
        divisor = Signal(self.width)
        remainder = Signal(2 * self.width + 1)

        fn = self.req.fn
        h = (fn == ALUOperator.REM) | (fn == ALUOperator.REMU)
        half_width = (self.width > 32) & (self.req.dw == ALUWidth.DW_32)
        lhs_signed = (fn == ALUOperator.DIV) | (fn == ALUOperator.REM)
        rhs_signed = lhs_signed

        def sext(x, signed):
            sign = Mux(half_width, x[self.width // 2 - 1], x[-1]) & signed
            hi = Mux(half_width, Repl(sign, self.width // 2),
                     x[self.width // 2:])
            return Cat(x[:self.width // 2], hi), sign

        lhs_in, lhs_sign = sext(self.req.in1, lhs_signed)
        rhs_in, rhs_sign = sext(self.req.in2, rhs_signed)

        is_hi = Signal()
        res_hi = Signal()
        result = Mux(res_hi, remainder[self.width + 1:],
                     remainder[:self.width])

        with m.FSM():
            with m.State('IDLE'):
                m.d.comb += self.req_ready.eq(1)

                with m.If(self.req_valid & self.req_ready & ~self.kill):
                    m.d.sync += [
                        req.eq(self.req),
                        neg_out.eq(Mux(h, lhs_sign, lhs_sign ^ rhs_sign)),
                        count.eq(0),
                        divisor.eq(Cat(rhs_in, rhs_sign)),
                        remainder.eq(lhs_in),
                        is_hi.eq(h),
                        res_hi.eq(0),
                    ]

                    with m.If(lhs_sign | rhs_sign):
                        m.next = 'NEG_IN'
                    with m.Else():
                        m.next = 'DIV'

            with m.State('NEG_IN'):
                with m.If(self.kill):
                    m.next = 'IDLE'
                with m.Else():
                    with m.If(remainder[self.width - 1]):
                        m.d.sync += remainder[:self.width].eq(
                            -remainder[:self.width])
                    with m.If(divisor[self.width - 1]):
                        m.d.sync += divisor.eq(-divisor)
                    m.next = 'DIV'

            with m.State('DIV'):
                with m.If(self.kill):
                    m.next = 'IDLE'
                with m.Else():
                    d = remainder[self.width:2 * self.width +
                                  1] - divisor[:self.width]
                    less = d[self.width]
                    m.d.sync += [
                        remainder.eq(
                            Cat(
                                ~less, remainder[:self.width],
                                Mux(less, remainder[self.width:2 * self.width],
                                    d[:self.width]))),
                        count.eq(count + 1),
                    ]

                    with m.If(count == self.width):
                        m.d.sync += res_hi.eq(is_hi)

                        with m.If(neg_out):
                            m.next = 'NEG_OUT'
                        with m.Else():
                            m.next = 'DIV_DONE'

                    with m.If((count == 0) & (divisor == 0) & ~is_hi):
                        m.d.sync += neg_out.eq(0)

            with m.State('NEG_OUT'):
                with m.If(self.kill):
                    m.next = 'IDLE'
                with m.Else():
                    m.d.sync += [
                        remainder.eq(-result),
                        res_hi.eq(0),
                    ]

                    m.next = 'DIV_DONE'

            with m.State('DIV_DONE'):
                m.d.comb += self.resp_valid.eq(~self.kill)

                with m.If(self.kill | (self.resp_valid & self.resp_ready)):
                    m.next = 'IDLE'

        result_lo = result[:self.width // 2]
        result_hi = Mux(half_width,
                        Repl(result[self.width // 2 - 1], self.width // 2),
                        result[self.width // 2:])
        m.d.comb += self.resp_data.eq(Cat(result_lo, result_hi))

        return m


class DivUnit(IterativeFunctionalUnit):

    def __init__(self, width, params):
        self.width = width

        super().__init__(width, params)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        div = m.submodules.div = IntDiv(self.width)

        m.d.comb += [
            div.req.fn.eq(self.req.uop.alu_fn),
            div.req.dw.eq(self.req.uop.alu_dw),
            div.req.in1.eq(self.req.rs1_data),
            div.req.in2.eq(self.req.rs2_data),
            div.req_valid.eq(self.req.valid),
            self.req.ready.eq(div.req_ready),
            self.resp.data.eq(div.resp_data),
            self.resp.valid.eq(div.resp_valid),
            div.resp_ready.eq(self.resp.ready),
            div.kill.eq(self.do_kill),
        ]

        return m


class FPUUnit(PipelinedFunctionalUnit):

    def __init__(self, width, params):
        self.width = width
        self.fma_latency = params['fma_latency']

        super().__init__(self.fma_latency, width, params)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        fma_en = Signal()
        fma_op = Signal(FPUOperator)
        fma_op_mod = Signal()

        fmt_in = Signal(FPFormat)
        fmt_out = Signal(FPFormat)

        swap32 = Signal()

        with m.Switch(self.req.uop.opcode):
            with m.Case(UOpCode.FADD_S, UOpCode.FADD_D):
                m.d.comb += [
                    fma_en.eq(1),
                    fma_op.eq(FPUOperator.ADD),
                    fmt_in.eq(
                        Mux(self.req.uop.fp_single, FPFormat.S, FPFormat.D)),
                    fmt_out.eq(
                        Mux(self.req.uop.fp_single, FPFormat.S, FPFormat.D)),
                    swap32.eq(1),
                ]

            with m.Case(UOpCode.FSUB_S, UOpCode.FSUB_D):
                m.d.comb += [
                    fma_en.eq(1),
                    fma_op.eq(FPUOperator.ADD),
                    fma_op_mod.eq(1),
                    fmt_in.eq(
                        Mux(self.req.uop.fp_single, FPFormat.S, FPFormat.D)),
                    fmt_out.eq(
                        Mux(self.req.uop.fp_single, FPFormat.S, FPFormat.D)),
                    swap32.eq(1),
                ]

            with m.Case(UOpCode.FMUL_S, UOpCode.FMUL_D):
                m.d.comb += [
                    fma_en.eq(1),
                    fma_op.eq(FPUOperator.MUL),
                    fmt_in.eq(
                        Mux(self.req.uop.fp_single, FPFormat.S, FPFormat.D)),
                    fmt_out.eq(
                        Mux(self.req.uop.fp_single, FPFormat.S, FPFormat.D)),
                ]

            with m.Case(UOpCode.FMADD_S, UOpCode.FMADD_D):
                m.d.comb += [
                    fma_en.eq(1),
                    fma_op.eq(FPUOperator.FMADD),
                    fmt_in.eq(
                        Mux(self.req.uop.fp_single, FPFormat.S, FPFormat.D)),
                    fmt_out.eq(
                        Mux(self.req.uop.fp_single, FPFormat.S, FPFormat.D)),
                ]

            with m.Case(UOpCode.FMSUB_S, UOpCode.FMSUB_D):
                m.d.comb += [
                    fma_en.eq(1),
                    fma_op.eq(FPUOperator.FMADD),
                    fma_op.eq(1),
                    fmt_in.eq(
                        Mux(self.req.uop.fp_single, FPFormat.S, FPFormat.D)),
                    fmt_out.eq(
                        Mux(self.req.uop.fp_single, FPFormat.S, FPFormat.D)),
                ]

            with m.Case(UOpCode.FNMSUB_S, UOpCode.FNMSUB_D):
                m.d.comb += [
                    fma_en.eq(1),
                    fma_op.eq(FPUOperator.FNMSUB),
                    fmt_in.eq(
                        Mux(self.req.uop.fp_single, FPFormat.S, FPFormat.D)),
                    fmt_out.eq(
                        Mux(self.req.uop.fp_single, FPFormat.S, FPFormat.D)),
                ]

            with m.Case(UOpCode.FNMADD_S, UOpCode.FNMADD_D):
                m.d.comb += [
                    fma_en.eq(1),
                    fma_op.eq(FPUOperator.FNMSUB),
                    fma_op.eq(1),
                    fmt_in.eq(
                        Mux(self.req.uop.fp_single, FPFormat.S, FPFormat.D)),
                    fmt_out.eq(
                        Mux(self.req.uop.fp_single, FPFormat.S, FPFormat.D)),
                ]

        def set_fu_input(inp):
            m.d.comb += [
                inp.in1.eq(self.req.rs1_data),
                inp.in2.eq(self.req.rs2_data),
                inp.in3.eq(self.req.rs3_data),
                inp.fn.eq(fma_op),
                inp.fn_mod.eq(fma_op_mod),
            ]

            with m.If(swap32):
                m.d.comb += inp.in3.eq(self.req.rs2_data)

        dfma = m.submodules.dfma = FPUFMA(self.width,
                                          FPFormat.D,
                                          latency=self.fma_latency)
        set_fu_input(dfma.inp)
        m.d.comb += dfma.inp_valid.eq(self.req.valid & fma_en
                                      & (fmt_out == FPFormat.D))

        sfma = m.submodules.sfma = FPUFMA(32,
                                          FPFormat.S,
                                          latency=self.fma_latency)
        set_fu_input(sfma.inp)
        m.d.comb += sfma.inp_valid.eq(self.req.valid & fma_en
                                      & (fmt_out == FPFormat.S))

        m.d.comb += self.resp.data.eq(
            Mux(dfma.out_valid, dfma.out.data,
                Mux(sfma.out_valid, sfma.out.data, 0)))

        return m


class FDivUnit(IterativeFunctionalUnit):

    def __init__(self, width, params):
        self.width = width

        super().__init__(width, params)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        fdiv = m.submodules.fdiv = FPUDivSqrtMulti()

        m.d.comb += [
            fdiv.a.eq(self.req.rs1_data),
            fdiv.b.eq(self.req.rs2_data),
            fdiv.is_sqrt.eq((self.req.uop.opcode == UOpCode.FSQRT_S)
                            | (self.req.uop.opcode == UOpCode.FSQRT_D)),
            fdiv.fmt.eq(Mux(self.req.uop.fp_single, FPFormat.S, FPFormat.D)),
            fdiv.in_valid.eq(self.req.valid),
            self.req.ready.eq(fdiv.in_ready),
            self.resp.data.eq(fdiv.out),
            self.resp.valid.eq(fdiv.out_valid),
            fdiv.out_ready.eq(self.resp.ready),
            fdiv.kill.eq(self.do_kill),
        ]

        return m
