from amaranth import *
from amaranth.utils import log2_int

from vroom.consts import *

from room.utils import sign_extend


class VALU(Elaboratable):

    def __init__(self, width):
        self.width = width

        self.fn = Signal(VALUOperator)
        self.sew = Signal(2)
        self.eew_vd = Signal(2)
        self.uop_idx = Signal(3)
        self.in1 = Signal(width)
        self.in2 = Signal(width)
        self.in3 = Signal(width)
        self.widen = Signal()
        self.widen2 = Signal()
        self.out = Signal(width)

    def elaborate(self, platform):
        m = Module()

        lane_bytes = self.width // 8

        is_sub = VALUOperator.is_sub(self.fn)
        is_rsub = self.fn == VALUOperator.VRSUB
        signed = self.fn[0]

        in1_w = Signal(32)
        in2_w = Signal(32)
        with m.Switch(self.uop_idx[:log2_int(self.width // 32)]):
            for i in range(self.width // 32):
                with m.Case(i):
                    m.d.comb += [
                        in1_w.eq(self.in1[i * 32:(i + 1) * 32]),
                        in2_w.eq(self.in2[i * 32:(i + 1) * 32]),
                    ]

        in1_widen = Signal(self.width)
        in2_widen = Signal(self.width)
        with m.Switch(self.sew):
            for i in range(3):
                with m.Case(i):
                    n = 1 << (3 + i)
                    for j in range(self.width // (2 * n)):
                        m.d.comb += [
                            in1_widen[j * 2 * n:(j + 1) * 2 * n].eq(
                                Mux(
                                    signed,
                                    sign_extend(in1_w[j * n:(j + 1) * n],
                                                2 * n),
                                    in1_w[j * n:(j + 1) * n])),
                            in2_widen[j * 2 * n:(j + 1) * 2 * n].eq(
                                Mux(
                                    signed,
                                    sign_extend(in2_w[j * n:(j + 1) * n],
                                                2 * n),
                                    in2_w[j * n:(j + 1) * n])),
                        ]

        in1_adjust = Mux(self.widen | self.widen2, in1_widen, self.in1)
        in2_adjust = Mux(self.widen, in2_widen, self.in2)

        in1_inv = Mux(is_sub, ~in1_adjust, in1_adjust)
        in2_inv = Mux(is_rsub, ~in2_adjust, in2_adjust)

        in1_xor_in2 = in1_inv ^ self.in2
        in1_and_in2 = in1_inv & self.in2

        #
        # VADD, VSUB, VRSUB
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
        # VMSEQ, VMSNE, VMSLT, VMSLTU, VMSLE, VMSLEU, VMSGT, VMSGTU
        #

        lt_vec = Signal(lane_bytes)
        eq_vec = Signal(lane_bytes)
        for i in range(lane_bytes):
            m.d.comb += [
                lt_vec[i].eq(
                    Mux(signed,
                        self.in2[i * 8 + 7] ^ in1_inv[i * 8 + 7] ^ cout[i],
                        ~cout[i])),
                eq_vec[i].eq(self.in1[i * 8:(i + 1) *
                                      8] == self.in2[i * 8:(i + 1) * 8]),
            ]

        cmp_eq = Signal(lane_bytes)
        with m.Switch(self.sew):
            for i in range(4):
                with m.Case(i):
                    sew = 1 << i
                    m.d.comb += cmp_eq.eq(
                        Cat(eq_vec[w * sew:(w + 1) * sew].all().replicate(sew)
                            for w in range(0, lane_bytes, sew)))

        #
        # VMIN, VMAX
        #

        minmax = Signal(self.width)
        select_vs = lt_vec ^ (
            (self.fn == VALUOperator.VMIN) |
            (self.fn == VALUOperator.VMINU)).replicate(self.width)

        for w in range(lane_bytes):
            sel = Signal()
            with m.Switch(self.sew):
                for i in range(4):
                    with m.Case(i):
                        sew = 1 << i
                        m.d.comb += sel.eq(select_vs[(w // sew) * sew +
                                                     (sew - 1)])

            m.d.comb += minmax[w * 8:(w + 1) * 8].eq(
                Mux(sel, self.in1[w * 8:(w + 1) * 8],
                    self.in2[w * 8:(w + 1) * 8]))

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
            with m.Case(VALUOperator.VADD, VALUOperator.VSUB,
                        VALUOperator.VRSUB):
                m.d.comb += self.out.eq(adder_out)
            with m.Case(VALUOperator.VMAXU, VALUOperator.VMAX,
                        VALUOperator.VMINU, VALUOperator.VMIN):
                m.d.comb += self.out.eq(minmax)
            with m.Default():
                m.d.comb += self.out.eq(shift_logic)

        return m
