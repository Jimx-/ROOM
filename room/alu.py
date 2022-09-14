from amaranth import *
from enum import IntEnum

from room.consts import *
from room.types import MicroOp
from room.if_stage import GetPCResp
from room.branch import BranchResolution, BranchUpdate


class ExecReq:

    def __init__(self, params, name=None):
        name = (name is not None) and f'{name}_' or ''
        self.xlen = params['xlen']

        self.uop = MicroOp(params, name=f'{name}uop')
        self.valid = Signal(name=f'{name}valid')

        self.rs1_data = Signal(self.xlen, name=f'{name}rs1_data')
        self.rs2_data = Signal(self.xlen, name=f'{name}rs2_data')

        self.kill = Signal(name=f'{name}kill')

        self.ready = Signal(name=f'{name}ready')

    def eq(self, rhs):
        attrs = ['uop', 'valid', 'rs1_data', 'rs2_data', 'kill']
        return [getattr(self, a).eq(getattr(rhs, a)) for a in attrs]


class ExecResp:

    def __init__(self, params, name=None):
        name = (name is not None) and f'{name}_' or ''
        self.xlen = params['xlen']
        self.vaddr_bits = params['vaddr_bits']

        self.uop = MicroOp(params, name=f'{name}uop')
        self.valid = Signal(name=f'{name}valid')

        self.data = Signal(self.xlen, name=f'{name}data')
        self.addr = Signal(self.vaddr_bits + 1, name=f'{name}addr')

        self.ready = Signal(name=f'{name}ready')

    def eq(self, rhs):
        attrs = ['uop', 'valid', 'addr', 'data']
        return [getattr(self, a).eq(getattr(rhs, a)) for a in attrs]


class FunctionalUnit(Elaboratable):

    def __init__(self, params, is_jmp=False, is_alu=False):
        self.is_jmp = is_jmp

        self.req = ExecReq(params, name='req')
        self.resp = ExecResp(params, name='resp')

        self.br_update = BranchUpdate(params)

        self.br_res = is_alu and BranchResolution(params,
                                                  name='br_res') or None
        self.get_pc = None
        if is_jmp:
            self.get_pc = GetPCResp(name='get_pc')


class PipelinedFunctionalUnit(FunctionalUnit):

    def __init__(self, num_stages, params, is_jmp=False, is_alu=False):
        self.params = params
        self.num_stages = num_stages

        super().__init__(params, is_jmp=is_jmp, is_alu=is_alu)

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


def generate_imm(ip, sel):
    sign = ip[-1]
    i20_30 = Mux(sel == ImmSel.U, ip[8:19], Repl(sign, 11))
    i12_19 = Mux((sel == ImmSel.U) | (sel == ImmSel.J), ip[0:8], Repl(sign, 8))
    i11 = Mux(sel == ImmSel.U, 0,
              Mux((sel == ImmSel.J) | (sel == ImmSel.B), ip[8], sign))
    i5_10 = Mux(sel == ImmSel.U, 0, ip[14:19])
    i1_4 = Mux(sel == ImmSel.U, 0, ip[9:14])
    i0 = Mux((sel == ImmSel.S) | (sel == ImmSel.I), ip[8], 0)
    return Cat(i0, i1_4, i5_10, i11, i12_19, i20_30, sign)


class ALU(Elaboratable):

    def __init__(self, width):
        self.width = width

        self.fn = Signal(ALUOperator)
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
        slt = Mux(self.in1[-1] == self.in2[-1], adder_out[-1],
                  Mux(is_cmp_unsigned, self.in2[-1], self.in1[-1]))

        #
        # SLL, SRL, SRA
        #

        if self.width == 32:
            shamt = self.in2[:5]
        else:
            shamt = self.in2[:6]

        shin_r = Signal(self.width)
        shin_l = Signal(self.width)
        m.d.comb += shin_r.eq(self.in1)
        for a, b in zip(shin_l, reversed(shin_r)):
            m.d.comb += a.eq(b)

        shin = Mux((self.fn == ALUOperator.SR) | (self.fn == ALUOperator.SRA),
                   shin_r, shin_l)

        shout_r = Signal(self.width)
        shout_l = Signal(self.width)
        m.d.comb += shout_r.eq(
            Cat(shin, is_sub & shin[-1]).as_signed() >> shamt)
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

        m.d.comb += self.out.eq(
            Mux((self.fn == ALUOperator.ADD) | (self.fn == ALUOperator.SUB),
                adder_out, shift_logic))

        return m


class ALUUnit(PipelinedFunctionalUnit):

    def __init__(self, params, is_jmp=False, num_stages=1):
        self.xlen = params['xlen']

        super().__init__(num_stages, params, is_jmp=is_jmp, is_alu=True)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        uop = self.req.uop

        #
        # Operands
        #

        imm = generate_imm(uop.imm_packed, uop.imm_sel)

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
        br_lt = (~(rs1[-1] ^ rs2[-1]) & br_ltu) | (rs1[-1] & ~rs2[-1])

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
        super().__init__(0, params)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        m.d.comb += [
            self.resp.addr.eq(self.req.rs1_data +
                              self.req.uop.imm_packed[8:20].as_signed()),
            self.resp.data.eq(self.req.rs2_data),
        ]

        return m


class MultiplierUnit(PipelinedFunctionalUnit):

    def __init__(self, width, latency, params):
        self.width = width
        self.latency = latency

        super().__init__(latency, params)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        in_req = ExecReq(self.params, name='in')
        m.d.sync += in_req.eq(self.req)

        fn = in_req.uop.alu_fn
        h = (fn == ALUOperator.MULH) | (fn == ALUOperator.MULHU) | (
            fn == ALUOperator.MULHSU)
        lhs_signed = (fn == ALUOperator.MULH) | (fn == ALUOperator.MULHSU)
        rhs_signed = (fn == ALUOperator.MULH)

        lhs = Cat(in_req.rs1_data,
                  (lhs_signed & in_req.rs1_data[-1])).as_signed()
        rhs = Cat(in_req.rs2_data,
                  (rhs_signed & in_req.rs2_data[-1])).as_signed()
        prod = lhs * rhs
        muxed = Mux(h, prod[self.width:self.width * 2], prod[:self.width])

        valid = in_req.valid
        data = muxed

        for _ in range(self.latency - 1):
            next_valid = Signal()
            next_data = Signal.like(data)

            m.d.sync += next_valid.eq(valid)

            with m.If(valid):
                m.d.sync += next_data.eq(data)

            valid = next_valid
            data = next_data

        m.d.comb += self.resp.data.eq(data)

        return m


class IterativeFunctionalUnit(FunctionalUnit):

    def __init__(self, params):
        self.params = params

        self.do_kill = Signal()

        super().__init__(params)

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


class DivReq(Record):

    def __init__(self, width, name=None, src_loc_at=0):
        super().__init__([
            ('fn', Shape.cast(ALUOperator).width),
            ('in1', width),
            ('in2', width),
        ],
                         name=name,
                         src_loc_at=1 + src_loc_at)


class IntDiv(Elaboratable):

    def __init__(self, width):
        self.width = width

        self.req = DivReq(width)
        self.req_valid = Signal()
        self.req_ready = Signal()

        self.resp_data = Signal(width)
        self.resp_valid = Signal()
        self.resp_ready = Signal()

        self.kill = Signal()

    def elaborate(self, platform):
        m = Module()

        req = DivReq(self.width, name='in')
        count = Signal(range(self.width + 1))
        neg_out = Signal()
        divisor = Signal(self.width)
        remainder = Signal(2 * self.width + 1)

        fn = self.req.fn
        h = (fn == ALUOperator.REM) | (fn == ALUOperator.REMU)
        lhs_signed = (fn == ALUOperator.DIV) | (fn == ALUOperator.REM)
        rhs_signed = lhs_signed

        lhs_sign = self.req.in1[-1] & lhs_signed
        rhs_sign = self.req.in2[-1] & rhs_signed

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
                        divisor.eq(Cat(self.req.in2, rhs_sign)),
                        remainder.eq(self.req.in1),
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

        m.d.comb += self.resp_data.eq(result)

        return m


class DivUnit(IterativeFunctionalUnit):

    def __init__(self, width, params):
        self.width = width

        super().__init__(params)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        div = m.submodules.div = IntDiv(self.width)

        m.d.comb += [
            div.req.fn.eq(self.req.uop.alu_fn),
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
