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
        self.vmask = Signal(width // 8)
        self.vm = Signal()
        self.ma = Signal()
        self.widen = Signal()
        self.widen2 = Signal()
        self.narrow_to_1 = Signal()
        self.out = Signal(width)
        self.cmp_out = Signal(width // 8)

    def elaborate(self, platform):
        m = Module()

        lane_bytes = self.width // 8

        is_sub = VALUOperator.is_sub(self.fn)
        is_rsub = self.fn == VALUOperator.VRSUB
        sub = is_sub | is_rsub
        signed = self.fn[0]

        def get_element(elem, data, uop_idx, elem_width):
            with m.Switch(uop_idx[:log2_int(self.width // elem_width)]):
                for i in range(self.width // elem_width):
                    with m.Case(i):
                        m.d.comb += elem.eq(data[i * elem_width:(i + 1) *
                                                 elem_width])

        in1_w = Signal(32)
        in2_w = Signal(32)
        get_element(in1_w, self.in1, self.uop_idx, 32)
        get_element(in2_w, self.in2, self.uop_idx, 32)

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

        vmask_adjust = Signal(lane_bytes)
        with m.Switch(self.sew):
            for w in (1, 2):  # vsew=16/32
                with m.Case(w):
                    n = 1 << w
                    for i in range(lane_bytes // n):
                        m.d.comb += vmask_adjust[i * n:(i + 1) * n].eq(
                            self.vmask[i])

        in1_inv = Mux(is_sub, ~in1_adjust, in1_adjust)
        in2_inv = Mux(is_rsub, ~in2_adjust, in2_adjust)

        in1_xor_in2 = in1_inv ^ self.in2
        in1_and_in2 = in1_inv & self.in2

        #
        # VADD, VSUB, VRSUB
        #

        add_with_carry = (self.fn == VALUOperator.VADC) | (
            self.fn == VALUOperator.VMADC) | (self.fn == VALUOperator.VSBC) | (
                self.fn == VALUOperator.VMSBC)
        carry_in = Signal(lane_bytes)
        cin = Signal(lane_bytes)
        cout = Signal(lane_bytes)
        adder_out = Signal(self.width)

        eew = Mux(self.narrow_to_1, self.sew, self.eew_vd)
        for w in range(lane_bytes):
            m.d.comb += carry_in[w].eq(
                Mux(add_with_carry & ~self.vm, vmask_adjust[w] ^ sub, sub))

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

        cmp_result = Signal(lane_bytes)
        with m.Switch(self.fn):
            with m.Case(VALUOperator.VMSEQ):
                m.d.comb += cmp_result.eq(cmp_eq)
            with m.Case(VALUOperator.VMSNE):
                m.d.comb += cmp_result.eq(~cmp_eq)
            with m.Case(VALUOperator.VMSLT, VALUOperator.VMSLTU):
                m.d.comb += cmp_result.eq(lt_vec)
            with m.Case(VALUOperator.VMSLE, VALUOperator.VMSLEU):
                m.d.comb += cmp_result.eq(lt_vec | cmp_eq)
            with m.Case(VALUOperator.VMSGT, VALUOperator.VMSGTU):
                m.d.comb += cmp_result.eq(~(lt_vec | cmp_eq))

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

        #
        # VZEXT, VSEXT
        #

        in2_h = Signal(16)
        in2_b = Signal(16)
        get_element(in2_h, self.in2, self.uop_idx, 16)
        get_element(in2_b, self.in2, self.uop_idx, 8)

        def extend_vector(ext_data, data, elem_width, signed):
            n_elems = len(data) // elem_width
            ext_width = len(ext_data) // n_elems

            for i in range(n_elems):
                m.d.comb += ext_data[i * ext_width:(i + 1) * ext_width].eq(
                    Mux(
                        signed,
                        sign_extend(data[i * elem_width:(i + 1) * elem_width],
                                    ext_width),
                        data[i * elem_width:(i + 1) * elem_width]))

        ext_out = Signal(self.width)
        with m.Switch(self.in1[1:3]):
            with m.Case(0b01):  # vf8
                extend_vector(ext_out, in2_b, 8, signed)

            with m.Case(0b10):  # vf4
                with m.Switch(self.sew):
                    for i in range(2):
                        with m.Case(i):
                            extend_vector(ext_out, in2_h, 1 << (3 + i), signed)

            with m.Case(0b11):  # vf2
                with m.Switch(self.sew):
                    for i in range(3):
                        with m.Case(i):
                            extend_vector(ext_out, in2_w, 1 << (3 + i), signed)

        with m.Switch(self.fn):
            with m.Case(VALUOperator.VADD, VALUOperator.VSUB,
                        VALUOperator.VRSUB, VALUOperator.VADC,
                        VALUOperator.VSBC):
                m.d.comb += self.out.eq(adder_out)
            with m.Case(VALUOperator.VMAXU, VALUOperator.VMAX,
                        VALUOperator.VMINU, VALUOperator.VMIN):
                m.d.comb += self.out.eq(minmax)
            with m.Case(VALUOperator.VZEXT, VALUOperator.VSEXT):
                m.d.comb += self.out.eq(ext_out)
            with m.Default():
                m.d.comb += self.out.eq(shift_logic)

        cmp_out = Mux(add_with_carry, Mux(is_sub, ~cout, cout), cmp_result)
        cmp_out_adjust = Signal(lane_bytes)
        with m.Switch(self.sew):
            for w in range(4):
                with m.Case(w):
                    n = 1 << w
                    m.d.comb += cmp_out_adjust.eq(
                        Cat(cmp_out[i + n - 1]
                            for i in range(0, lane_bytes, n)))

        for i in range(lane_bytes):
            with m.If(add_with_carry):
                m.d.comb += self.cmp_out[i].eq(cmp_out_adjust[i])
            with m.Elif(~self.vm & ~self.vmask[i]):
                m.d.comb += self.cmp_out[i].eq(Mux(self.ma, 1, self.in3[i]))
            with m.Else():
                m.d.comb += self.cmp_out[i].eq(cmp_out_adjust[i])

        return m
