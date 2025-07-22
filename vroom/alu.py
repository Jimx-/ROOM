from amaranth import *

from vroom.consts import *


class VALU(Elaboratable):

    def __init__(self, width):
        self.width = width

        self.fn = Signal(VALUOperator)
        self.sew = Signal(2)
        self.eew_vd = Signal(2)
        self.in1 = Signal(width)
        self.in2 = Signal(width)
        self.in3 = Signal(width)
        self.out = Signal(width)

    def elaborate(self, platform):
        m = Module()

        lane_bytes = self.width // 8

        is_sub = VALUOperator.is_sub(self.fn)
        is_rsub = self.fn == VALUOperator.VRSUB

        in1_inv = Mux(is_sub, ~self.in1, self.in1)
        in2_inv = Mux(is_rsub, ~self.in2, self.in2)

        in1_xor_in2 = in1_inv ^ self.in2
        in1_and_in2 = in1_inv & self.in2

        #
        # VADD, VSUB
        #

        carry_in = Signal(lane_bytes)
        cin = Signal(lane_bytes)
        cout = Signal(lane_bytes)
        adder_out = Signal(self.width)

        eew = self.sew
        for w in range(lane_bytes):
            m.d.comb += carry_in[w].eq(is_sub | is_rsub)

            if w == 0:
                m.d.comb += cin[w].eq(carry_in[w])
            elif w % 4 == 0:
                m.d.comb += cin[w].eq(Mux(eew == 3, cout[w - 1], carry_in[w]))
            elif w % 2 == 0:
                m.d.comb += cin[w].eq(
                    Mux((eew == 3) | (eew == 2), cout[w - 1], carry_in[w]))
            else:
                m.d.comb += cin[w].eq(Mux(eew == 0, carry_in[w], cout[w - 1]))

            s = Cat(cin[w], in1_inv[w * 8:(w + 1) * 8], Const(0, 1)) + Cat(
                cin[w], in2_inv[w * 8:(w + 1) * 8], Const(0, 1))
            m.d.comb += Cat(adder_out[w * 8:(w + 1) * 8], cout[w]).eq(s[1:])

        #
        # AND, OR, XOR
        #

        logic = Mux(
            (self.fn == VALUOperator.VXOR) | (self.fn == VALUOperator.VOR) |
            (self.fn == VALUOperator.VORN) |
            (self.fn == VALUOperator.VXNOR), in1_xor_in2, 0) | Mux(
                (self.fn == VALUOperator.VAND) |
                (self.fn == VALUOperator.VOR) |
                (self.fn == VALUOperator.VORN) |
                (self.fn == VALUOperator.VANDN), in1_and_in2, 0)

        shift_logic = logic

        with m.Switch(self.fn):
            with m.Case(VALUOperator.VADD, VALUOperator.VSUB):
                m.d.comb += self.out.eq(adder_out)
            with m.Default():
                m.d.comb += self.out.eq(shift_logic)

        return m
