from amaranth import *
from enum import IntEnum

from room.consts import *
from room.types import MicroOp
from room.if_stage import GetPCResp
from room.branch import BranchResolution, BranchUpdate


class ExecReq:

    def __init__(self, params, name=None):
        name = (name is not None) and f'{name}_' or ''

        self.uop = MicroOp(params, name=f'{name}uop')
        self.valid = Signal(name=f'{name}valid')

        self.rs1_data = Signal(32, name=f'{name}rs1_data')
        self.rs2_data = Signal(32, name=f'{name}rs2_data')

        self.kill = Signal(name=f'{name}kill')

    def eq(self, rhs):
        return [
            self.uop.eq(rhs.uop),
            self.valid.eq(rhs.valid),
            self.rs1_data.eq(rhs.rs1_data),
            self.rs2_data.eq(rhs.rs2_data),
            self.kill.eq(rhs.kill),
        ]


class ExecResp:

    def __init__(self, params, name=None):
        name = (name is not None) and f'{name}_' or ''

        self.uop = MicroOp(params, name=f'{name}uop')
        self.valid = Signal(name=f'{name}valid')

        self.data = Signal(32, name=f'{name}data')

    def eq(self, rhs):
        return [
            self.uop.eq(rhs.uop),
            self.valid.eq(rhs.valid),
            self.data.eq(rhs.data),
        ]


class FunctionalUnit(Elaboratable):

    def __init__(self, params, is_jmp=False, is_alu=False):
        self.is_jmp = is_jmp

        self.req = ExecReq(params)
        self.resp = ExecResp(params)

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
            ]

            for i in range(1, self.num_stages):
                m.d.sync += [
                    self.valids[i].eq(
                        self.valids[i - 1]
                        & ~self.br_update.uop_killed(self.uops[i - 1])
                        & ~self.req.kill),
                    self.uops[i].eq(self.uops[i - 1]),
                ]

            m.d.comb += [
                self.resp.valid.eq(self.valids[self.num_stages - 1]
                                   & ~self.br_update.uop_killed(self.uops[
                                       self.num_stages - 1])),
                self.resp.uop.eq(self.uops[self.num_stages - 1]),
            ]
        else:
            m.d.comb += [
                self.resp.valid.eq(self.req.valid
                                   & ~self.br_update.uop_killed(self.req.uop)),
                self.resp.uop.eq(self.req.uop),
            ]

        return m


def generate_imm(ip, sel):
    sign = ip[-1]
    i20_30 = Mux(sel == ImmSel.U, ip[8:19], Repl(sign, 10))
    i12_19 = Mux((sel == ImmSel.U) | (sel == ImmSel.J), ip[0:8], Repl(sign, 8))
    i11 = Mux(sel == ImmSel.U, 0,
              Mux((sel == ImmSel.J) | (sel == ImmSel.B), ip[8], sign))
    i5_10 = Mux(sel == ImmSel.U, 0, ip[14:19])
    i1_4 = Mux(sel == ImmSel.U, 0, ip[9:14])
    i0 = Mux((sel == ImmSel.S) | (sel == ImmSel.I), ip[8], 0)
    return Cat(i0, i1_4, i5_10, i11, i12_19, i20_30, sign)


class ALU(PipelinedFunctionalUnit):

    def __init__(self, params, is_jmp=False):
        super().__init__(1, params, is_jmp=is_jmp, is_alu=True)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        uop = self.req.uop

        imm = generate_imm(uop.imm_packed, uop.imm_sel)

        opa_data = Signal(32)
        opb_data = Signal(32)

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
                Mux(uop.opb_sel == OpB.RS2, self.req.rs2_data,
                    Mux(uop.opb_sel == OpB.NEXT, Mux(uop.is_rvc, 2, 4), 0))))

        is_sub = uop.alu_fn[3]

        adder_out = Signal(32)
        opb_inv = Mux(is_sub == 1, ~opb_data, opb_data)
        m.d.comb += adder_out.eq(opa_data + opb_inv + is_sub)

        alu_out = Signal(32)
        m.d.comb += alu_out.eq(
            Mux((uop.alu_fn == ALUOperator.ADD) |
                (uop.alu_fn == ALUOperator.SUB), adder_out, 0))

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
        data = [Signal(32, name=f's{i}_data') for i in range(self.num_stages)]

        m.d.sync += [
            data[0].eq(alu_out),
        ]

        for i in range(1, self.num_stages):
            m.d.sync += [
                data[i].eq(data[i - 1]),
            ]

        m.d.comb += [
            self.resp.data.eq(data[self.num_stages - 1]),
        ]

        return m
