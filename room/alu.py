from amaranth import *
from enum import IntEnum

from room.consts import *
from room.types import MicroOp


class ExecReq:

    def __init__(self, params, name=None):
        self.uop = MicroOp(params, name=name and f'{name}_uop' or None)
        self.valid = Signal(name=name and f'{name}_valid' or None)

        self.rs1_data = Signal(32, name=name and f'{name}_rs1_data' or None)
        self.rs2_data = Signal(32, name=name and f'{name}_rs2_data' or None)

        self.kill = Signal(name=name and f'{name}_kill' or None)

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
        self.uop = MicroOp(params, name=name and f'{name}_uop' or None)
        self.valid = Signal(name=name and f'{name}_valid' or None)

        self.data = Signal(32, name=name and f'{name}_data' or None)

    def eq(self, rhs):
        return [
            self.uop.eq(rhs.uop),
            self.valid.eq(rhs.valid),
            self.data.eq(rhs.data),
        ]


class PipelinedFunctionalUnit(Elaboratable):

    def __init__(self, num_stages, params):
        self.params = params
        self.num_stages = num_stages

        self.req = ExecReq(params)
        self.resp = ExecResp(params)

    def elaborate(self, platform):
        m = Module()

        if self.num_stages > 0:
            self.valids = Signal(self.num_stages)
            self.uops = [
                MicroOp(self.params, name=f's{i}_uops')
                for i in range(self.num_stages)
            ]

            m.d.sync += [
                self.valids[0].eq(self.req.valid & ~self.req.kill),
                self.uops[0].eq(self.req.uop),
            ]

            for i in range(1, self.num_stages):
                m.d.sync += [
                    self.valids[i].eq(self.valids[i - 1] & ~self.req.kill),
                    self.uops[i].eq(self.uops[i - 1]),
                ]

            m.d.comb += [
                self.resp.valid.eq(self.valids[self.num_stages - 1]),
                self.resp.uop.eq(self.uops[self.num_stages - 1]),
            ]
        else:
            m.d.comb += [
                self.resp.valid.eq(self.req.valid),
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

    def __init__(self, params):
        super().__init__(1, params)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        uop = self.req.uop

        imm = generate_imm(uop.imm_packed, uop.imm_sel)

        opa_data = Signal(32)
        opb_data = Signal(32)

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
