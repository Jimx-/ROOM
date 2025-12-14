from amaranth import *
from amaranth.utils import log2_int
import functools
import operator

from vroom.consts import *
from vroom.utils import get_round_inc

from room.consts import ALUOperator
from room.alu import IntDiv
from room.utils import Pipe, sign_extend, PopCount, FindFirstSet, SetBeforeFirst

from roomsoc.interconnect.stream import Decoupled, Valid


class RoundAdder(Elaboratable):

    def __init__(self, width):
        self.width = width

        self.din = Signal(width)
        self.incr = Signal(width // 8)
        self.is_nclip = Signal()
        self.sew = Signal(2)

        self.dout = Signal(width)
        self.cout = Signal(width // 8)

    def elaborate(self, platform):
        m = Module()

        sew = Signal(2)
        m.d.comb += sew.eq(Mux(self.is_nclip, self.sew + 1, self.sew))

        cin = Signal(self.width // 8)
        for w in range(self.width // 8):
            if w == 0:
                m.d.comb += cin[w].eq(self.incr[w])
            elif w % 4 == 0:
                m.d.comb += cin[w].eq(
                    Mux((sew == 3), self.cout[w - 1], self.incr[w]))
            elif w % 2 == 0:
                m.d.comb += cin[w].eq(
                    Mux((sew == 3) | (sew == 2), self.cout[w - 1],
                        self.incr[w]))
            else:
                m.d.comb += cin[w].eq(
                    Mux(~sew.any(), self.incr[w], self.cout[w - 1]))

            s = self.din.word_select(w, 8) + cin[w]
            m.d.comb += Cat(self.dout.word_select(w, 8), self.cout[w]).eq(s)

        return m


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
        self.vi = Signal()
        self.widen = Signal()
        self.widen2 = Signal()
        self.narrow = Signal()
        self.narrow_to_1 = Signal()

        self.out = Signal(width)
        self.narrow_out = Signal(width // 2)
        self.cmp_out = Signal(width // 8)

        # To fix-point
        self.adder_out = Signal(width)
        self.adder_cout = Signal(width // 8)
        self.in1h = Signal(width // 8)
        self.in2h = Signal(width // 8)
        self.shift_out = Signal(width)
        self.round_high = Signal(width // 8)
        self.round_tail = Signal(width // 8)

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
                        m.d.comb += elem.eq(data.word_select(i, elem_width))

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
                            in1_widen.word_select(j, 2 * n).eq(
                                Mux(
                                    signed,
                                    sign_extend(in1_w.word_select(j,
                                                                  n), 2 * n),
                                    in1_w.word_select(j, n))),
                            in2_widen.word_select(j, 2 * n).eq(
                                Mux(
                                    signed,
                                    sign_extend(in2_w.word_select(j,
                                                                  n), 2 * n),
                                    in2_w.word_select(j, n))),
                        ]

        in1_adjust = Mux(self.widen | self.widen2, in1_widen, self.in1)
        in2_adjust = Mux(self.widen, in2_widen, self.in2)

        vmask_adjust = Signal(lane_bytes)
        with m.Switch(self.sew):
            for w in (1, 2):  # vsew=16/32
                with m.Case(w):
                    n = 1 << w
                    for i in range(lane_bytes // n):
                        m.d.comb += vmask_adjust.word_select(i, n).eq(
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

            s = Cat(cin[w], in1_inv.word_select(w, 8), Const(0, 1)) + Cat(
                cin[w], in2_inv.word_select(w, 8), Const(0, 1))
            m.d.comb += Cat(adder_out.word_select(w, 8), cout[w]).eq(s[1:])

        m.d.comb += [
            self.adder_out.eq(adder_out),
            self.adder_cout.eq(cout),
        ]
        for i in range(lane_bytes):
            m.d.comb += [
                self.in1h[i].eq(self.in1[i * 8 + 7]),
                self.in2h[i].eq(self.in2[i * 8 + 7]),
            ]

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
                eq_vec[i].eq(
                    self.in1.word_select(i, 8) == self.in2.word_select(i, 8)),
            ]

        cmp_eq = Signal(lane_bytes)
        with m.Switch(self.sew):
            for i in range(4):
                with m.Case(i):
                    sew = 1 << i
                    m.d.comb += cmp_eq.eq(
                        Cat(
                            eq_vec.bit_select(w, sew).all().replicate(sew)
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
        # VSLL, VSRL, VSRA
        #

        shift_l = self.fn == VALUOperator.VSL
        shin_r = self.in2
        shin_l = self.in2[::-1]
        shin = Mux(shift_l, shin_l, shin_r)

        in1_rev = Signal(self.width)
        m.d.comb += in1_rev.eq(self.in1)
        with m.If(shift_l):
            with m.Switch(self.sew):
                for w in range(4):
                    with m.Case(w):
                        n = 1 << (3 + w)
                        m.d.comb += in1_rev.eq(
                            Cat([
                                self.in1.bit_select(i, n)
                                for i in range(0, self.width, n)
                            ][::-1]))

        shamts = Signal(self.width)
        m.d.comb += shamts.eq(in1_rev)
        with m.If(self.narrow):
            for w in range(self.width // 16):
                with m.If(self.uop_idx[0]):
                    m.d.comb += shamts.word_select(w, 16).eq(
                        in1_rev.word_select(w + self.width // 16, 8))
                with m.Else():
                    m.d.comb += shamts.word_select(w, 16).eq(
                        in1_rev.word_select(w, 8))

        shout_r = Signal(self.width)
        m.d.comb += self.shift_out.eq(shout_r)
        with m.Switch(self.sew + self.narrow):
            for w in range(4):
                with m.Case(w):
                    nb = 1 << w
                    n = nb << 3
                    for i in range(self.width // n):
                        if w == 3:
                            shamt = Cat(
                                shamts.bit_select(i * n, 2 + w),
                                Mux(self.vi, 0, shamts[i * n + (2 + w)]))
                        else:
                            shamt = shamts.bit_select(i * n, 3 + w)
                        m.d.comb += shout_r.word_select(i, n).eq(
                            Cat(shin.word_select(i, n), is_sub
                                & shin[(i + 1) * n - 1]).as_signed() >> shamt)

                        with m.Switch(shamt):
                            with m.Case(0):
                                pass
                            with m.Case(1):
                                m.d.comb += self.round_high.word_select(
                                    i, nb).eq(shin[i * n].replicate(nb))
                            for j in range(2, n):
                                with m.Case(j):
                                    m.d.comb += [
                                        self.round_high.word_select(i, nb).eq(
                                            shin[i * n + j - 1].replicate(nb)),
                                        self.round_tail[i * nb:(
                                            i + 1) * nb].eq((shin.bit_select(
                                                i * n,
                                                j - 1).any().replicate(nb))),
                                    ]
                            with m.Default():
                                m.d.comb += self.round_tail.word_select(
                                    i, nb).eq(
                                        shin.word_select(
                                            i, n).any().replicate(nb))

        shout_l = shout_r[::-1]
        shout = Mux(
            (self.fn == VALUOperator.VSR) | (self.fn == VALUOperator.VSRA),
            shout_r, 0) | Mux(self.fn == VALUOperator.VSL, shout_l, 0)

        with m.Switch(self.sew):
            for w in range(4):
                with m.Case(w):
                    n = 1 << (3 + w)
                    m.d.comb += self.narrow_out.eq(
                        Cat(
                            shout.word_select(i * 2, n)
                            for i in range(self.width // (2 * n))))

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

            m.d.comb += minmax.word_select(w, 8).eq(
                Mux(sel, self.in1.word_select(w, 8),
                    self.in2.word_select(w, 8)))

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

        shift_logic = logic | shout

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
                m.d.comb += ext_data.word_select(i, ext_width).eq(
                    Mux(
                        signed,
                        sign_extend(data.word_select(i,
                                                     elem_width), ext_width),
                        data.word_select(i, elem_width)))

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

        #
        # VMERGE/VMV
        #

        merge_mask = Signal(self.width)
        with m.Switch(self.sew):
            for i in range(4):
                with m.Case(i):
                    m.d.comb += merge_mask.eq(
                        Cat(x.replicate(1 << i) for x in self.vmask))

        merge_result = Signal(self.width)
        for w in range(lane_bytes):
            m.d.comb += merge_result.word_select(w, 8).eq(
                Mux(merge_mask[w], self.in1.word_select(w, 8),
                    self.in2.word_select(w, 8)))

        merge_out = Mux(self.vm, self.in1, merge_result)

        with m.Switch(self.fn):
            with m.Case(VALUOperator.VADDU, VALUOperator.VADD,
                        VALUOperator.VSUB, VALUOperator.VSUBU,
                        VALUOperator.VRSUB, VALUOperator.VADC,
                        VALUOperator.VSBC):
                m.d.comb += self.out.eq(adder_out)
            with m.Case(VALUOperator.VMAXU, VALUOperator.VMAX,
                        VALUOperator.VMINU, VALUOperator.VMIN):
                m.d.comb += self.out.eq(minmax)
            with m.Case(VALUOperator.VZEXT, VALUOperator.VSEXT):
                m.d.comb += self.out.eq(ext_out)
            with m.Case(VALUOperator.VMVSX):
                m.d.comb += self.out.eq(self.in1)
            with m.Case(VALUOperator.VMVXS):
                with m.Switch(self.sew):
                    for w in range(4):
                        with m.Case(w):
                            m.d.comb += self.out.eq(
                                sign_extend(self.in2[:1 << (3 + w)],
                                            self.width))
            with m.Case(VALUOperator.VMVNRV):
                m.d.comb += self.out.eq(self.in2)
            with m.Case(VALUOperator.VMERGE):
                m.d.comb += self.out.eq(merge_out)
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


class VFixPointALU(Elaboratable):

    def __init__(self, width):
        self.width = width

        self.fn = Signal(VALUOperator)
        self.sew = Signal(2)
        self.vxrm = Signal(VXRoundingMode)
        self.alu_out = Signal(width)
        self.alu_cout = Signal(width // 8)
        self.in1h = Signal(width // 8)
        self.in2h = Signal(width // 8)
        self.alu_shift_out = Signal(width)
        self.round_high = Signal(width // 8)
        self.round_tail = Signal(width // 8)

        self.out = Signal(width)
        self.narrow_out = Signal(width // 2)
        self.vxsat = Signal(width // 8)

    def elaborate(self, platform):
        m = Module()

        is_sub = VALUOperator.is_sub(self.fn)
        is_sat_add = (self.fn == VALUOperator.VSADDU) | (
            self.fn == VALUOperator.VSADD
        ) | (self.fn == VALUOperator.VSSUBU) | (self.fn == VALUOperator.VSSUB)
        is_avg_add = (self.fn == VALUOperator.VAADDU) | (
            self.fn == VALUOperator.VAADD
        ) | (self.fn == VALUOperator.VASUBU) | (self.fn == VALUOperator.VASUB)
        signed = self.fn[0]
        is_nclip = (self.fn == VALUOperator.VNCLIPU) | (self.fn
                                                        == VALUOperator.VNCLIP)

        #
        # VSADD, VSSUB
        #

        sat = Signal(self.width // 8)
        for i in range(self.width // 8):
            with m.If(signed):
                m.d.comb += sat[i].eq((is_sub ^ (self.in1h[i] ^ self.in2h[i]))
                                      & (self.in2h[i]
                                         ^ self.alu_out[i * 8 + 7]))
            with m.Else():
                m.d.comb += sat[i].eq(self.alu_cout[i] ^ is_sub)

        sat_bytes = Signal(self.width // 8)
        with m.Switch(self.sew):
            for w in range(4):
                with m.Case(w):
                    n = 1 << w
                    for i in range(len(sat_bytes) // n):
                        m.d.comb += sat_bytes.word_select(i, n).eq(
                            sat[(i + 1) * n - 1].replicate(n))

        sat_result = Signal(self.width)
        with m.Switch(self.sew):
            for w in range(4):
                with m.Case(w):
                    n = 1 << w
                    for i in range(self.width // 8):
                        overflow = self.alu_cout[(i // n + 1) * n - 1]
                        underflow = self.in2h[(i // n + 1) * n - 1]
                        is_msb = (i & (n - 1)) == (n - 1)
                        with m.If(sat_bytes[i]):
                            with m.If(signed):
                                m.d.comb += sat_result.word_select(i, 8).eq(
                                    Mux(underflow, Cat(Const(0, 7), is_msb),
                                        Cat(Const(~0, 7), not is_msb)))
                            with m.Else():
                                m.d.comb += sat_result.word_select(i, 8).eq(
                                    overflow.replicate(8))

                        with m.Else():
                            m.d.comb += sat_result.word_select(i, 8).eq(
                                self.alu_out.word_select(i, 8))

        #
        # VAADD, VASUB
        #

        avg_high_bit = Signal(self.width // 8)
        with m.Switch(self.sew):
            for w in range(4):
                with m.Case(w):
                    n = 1 << w
                    for i in range(self.width // 8):
                        if i % n == (n - 1):
                            m.d.comb += avg_high_bit[i].eq(
                                self.alu_cout[i] ^ is_sub
                                ^ (signed & (self.in1h[i] ^ self.in2h[i])))
                        else:
                            m.d.comb += avg_high_bit[i].eq(
                                self.alu_out[(i + 1) * 8])

        avg_pre_round = Signal(self.width)
        avg_round_inc = Signal(self.width // 8)
        for i in range(self.width // 8):
            m.d.comb += [
                avg_pre_round.word_select(i, 8).eq(
                    Cat(self.alu_out.word_select(i, 8), avg_high_bit[i])),
                avg_round_inc[i].eq(
                    get_round_inc(self.vxrm, self.alu_out.word_select(i, 8),
                                  1)),
            ]

        #
        # VSSRL, VSSRA
        #

        shift_pre_round = self.alu_shift_out
        shift_round_inc = Signal(self.width // 8)
        for i in range(self.width // 8):
            m.d.comb += shift_round_inc[i].eq(
                get_round_inc(
                    self.vxrm,
                    Cat(self.round_tail[i], self.round_high[i],
                        self.alu_shift_out[i * 8]), 2))

        rnd_adder = m.submodules.rnd_adder = RoundAdder(self.width)
        m.d.comb += [
            rnd_adder.din.eq(Mux(is_avg_add, avg_pre_round, shift_pre_round)),
            rnd_adder.incr.eq(Mux(is_avg_add, avg_round_inc, shift_round_inc)),
            rnd_adder.sew.eq(self.sew),
            rnd_adder.is_nclip.eq(is_nclip),
        ]

        #
        # VNCLIPU, VNCLIP
        #

        all_1s_mask = Signal(self.width // 8)
        all_0s_mask = Signal(self.width // 8)
        for i in range(self.width // 8):
            m.d.comb += [
                all_1s_mask[i].eq(rnd_adder.din.word_select(i, 8).all()),
                all_0s_mask[i].eq(~rnd_adder.din.word_select(i, 8).any()),
            ]

        nclip_sat = Signal(self.width // 16)
        with m.Switch(self.sew):
            for w in range(3):
                with m.Case(w):
                    nb = 1 << w
                    n = nb << 3
                    nw = n << 1

                    for i in range(self.width // nw):
                        data = rnd_adder.dout.word_select(i, nw)
                        all_1s = all_1s_mask[i * nb + nb // 2:(i + 1) *
                                             nb].all()
                        all_0s = all_0s_mask[i * nb + nb // 2:(i + 1) *
                                             nb].all()
                        carry = rnd_adder.cout[i * nb + nb // 2 - 1]

                        with m.If(signed):
                            with m.If(all_0s & ~carry & ~data[n - 1] | all_1s
                                      & (carry | ~carry & data[n - 1])):
                                m.d.comb += self.narrow_out.word_select(
                                    i, n).eq(data[:n])
                            with m.Else():
                                m.d.comb += [
                                    self.narrow_out.word_select(i, n).eq(
                                        Cat(~data[n - 1].replicate(n - 1),
                                            data[n - 1])),
                                    nclip_sat.word_select(i, nb).eq(~0),
                                ]
                        with m.Else():
                            with m.If(all_0s & ~carry):
                                m.d.comb += self.narrow_out.word_select(
                                    i, n).eq(data[:n])
                            with m.Else():
                                m.d.comb += [
                                    self.narrow_out.word_select(i, n).eq(~0),
                                    nclip_sat.word_select(i, nb).eq(~0),
                                ]

        m.d.comb += [
            self.out.eq(Mux(is_sat_add, sat_result, rnd_adder.dout)),
            self.vxsat.eq(
                Mux(is_sat_add, sat_bytes, Mux(is_nclip, nclip_sat, 0))),
        ]

        return m


class CSA3to2(Elaboratable):

    def __init__(self, width):
        self.width = width

        self.a = Signal(width)
        self.b = Signal(width)
        self.c = Signal(width)

        self.sum = Signal(width)
        self.cout = Signal(width)

    def elaborate(self, platform):
        m = Module()

        m.d.comb += [
            self.sum.eq(self.a ^ self.b ^ self.c),
            self.cout[1:].eq((self.a & self.b) | (self.b & self.c)
                             | (self.a & self.c)),
        ]

        return m


class CSA4to2(Elaboratable):

    def __init__(self, width):
        self.width = width

        self.a = Signal(width)
        self.b = Signal(width)
        self.c = Signal(width)
        self.d = Signal(width)

        self.sum = Signal(width)
        self.cout = Signal(width)

    def elaborate(self, platform):
        m = Module()

        csa1 = CSA3to2(self.width)
        m.submodules += csa1
        m.d.comb += [
            csa1.a.eq(self.a),
            csa1.b.eq(self.b),
            csa1.c.eq(self.c),
        ]

        csa2 = CSA3to2(self.width)
        m.submodules += csa2
        m.d.comb += [
            csa2.a.eq(csa1.sum),
            csa2.b.eq(csa1.cout),
            csa2.c.eq(self.d),
        ]

        m.d.comb += [
            self.sum.eq(csa2.sum),
            self.cout.eq(csa2.cout),
        ]

        return m


class VMultiplier(Elaboratable):

    CSA_BREAK = 3

    class BoothEncoder(Elaboratable):

        def __init__(self):
            self.d = Signal(3)

            self.pos = Signal()
            self.neg = Signal()
            self.one = Signal()
            self.double = Signal()

        def elaborate(self, platform):
            m = Module()

            m.d.comb += [
                self.pos.eq(~self.d[2] & self.d[:2].any()),
                self.neg.eq(self.d[2] & ~self.d[:2].all()),
                self.one.eq(self.d[1] ^ self.d[0]),
                self.double.eq((self.d[2] ^ self.d[1])
                               & ~(self.d[1] ^ self.d[0])),
            ]

            return m

    def __init__(self, width, latency):
        self.width = width
        self.latency = latency

        self.valid = Signal()
        self.fn = Signal(VALUOperator)
        self.sew = Signal(2)
        self.uop_idx = Signal(3)
        self.vxrm = Signal(VXRoundingMode)
        self.in1 = Signal(width)
        self.in2 = Signal(width)
        self.in3 = Signal(width)
        self.widen = Signal()

        self.resp_data = Signal(width)
        self.vxsat = Signal(width // 8)

    def elaborate(self, platform):
        m = Module()

        lane_bytes = self.width // 8

        h = (self.fn == VALUOperator.VMULH) | (
            self.fn == VALUOperator.VMULHU) | (self.fn == VALUOperator.VMULHSU)
        lhs_signed = (self.fn == VALUOperator.VMULH) | (
            self.fn == VALUOperator.VMULHSU) | (
                (self.fn == VALUOperator.VMACC)
                & self.widen) | (self.fn == VALUOperator.VMACCUS)
        rhs_signed = (self.fn == VALUOperator.VMULH) | (
            (self.fn == VALUOperator.VMACC)
            & self.widen) | (self.fn == VALUOperator.VMACCSU)

        is_sub = (self.fn == VALUOperator.VNMSAC) | (self.fn
                                                     == VALUOperator.VNMSUB)
        is_fixp = VALUOperator.is_fixp(self.fn)

        #
        # Booth encoding
        #

        in1_booth = [
            VMultiplier.BoothEncoder() for _ in range(self.width // 2)
        ]
        for w, b in enumerate(in1_booth):
            setattr(m.submodules, f'booth{w}', b)

            if w == 0:
                m.d.comb += b.d.eq(Cat(Const(0, 1), self.in1[:2]))
            else:
                b1 = self.in1[2 * w - 1:2 * w + 2]
                b2 = Cat(Const(0, 1), self.in1[2 * w:2 * w + 2])

                if w % 4 != 0:
                    m.d.comb += b.d.eq(b1)
                elif w == 16:
                    m.d.comb += b.d.eq(Mux(self.sew == 3, b1, b2))
                elif w % 8 == 0:
                    m.d.comb += b.d.eq(
                        Mux((self.sew == 3) | (self.sew == 2), b1, b2))
                else:
                    m.d.comb += b.d.eq(Mux(self.sew != 0, b1, b2))

        in2_blocks = []
        for i in range(4):
            in2_blocks.append([])
            sew = 1 << (3 + i)
            for bidx in range(self.width // sew):
                in2_block = self.in2.word_select(bidx, sew)
                in2_elem = Signal(2 * sew, name=f'in2_elem{bidx}_e{sew}')
                m.d.comb += in2_elem.eq(
                    Mux(lhs_signed, sign_extend(in2_block, 2 * sew),
                        in2_block))
                in2_blocks[-1].append(in2_elem)

        part_prod = [
            Signal(self.width * 2, name=f'part_prod{i}')
            for i in range(self.width // 2 + 2)
        ]
        for i, (booth, prod) in enumerate(zip(in1_booth, part_prod)):
            with m.Switch(self.sew):
                for w, blocks in enumerate(in2_blocks):
                    with m.Case(w):
                        sew = 1 << (3 + w)
                        bidx = 2 * i // sew
                        in2_elem = blocks[bidx]
                        booth_double = Signal(2 * sew)
                        m.d.comb += booth_double.eq(
                            Mux(booth.one, in2_elem,
                                Mux(booth.double, in2_elem << 1, 0)))

                        booth_result = Mux(booth.pos, booth_double,
                                           Mux(booth.neg, ~booth_double, 0))
                        shamt = 2 * i - sew * bidx
                        shifted = Signal(2 * sew)
                        m.d.comb += shifted.eq(booth_result << shamt)

                        if sew == 64 or bidx == 0:
                            m.d.comb += prod.eq(shifted)
                        else:
                            m.d.comb += prod.eq(
                                Cat(Const(0, bidx * sew * 2), shifted))
        with m.Switch(self.sew):
            for w in range(4):
                with m.Case(w):
                    sew = 1 << (w + 3)

                    comps = []
                    for i in range(self.width // sew):
                        hi = Mux(self.in1[(i + 1) * sew - 1] & ~rhs_signed,
                                 self.in2.word_select(i, sew), 0)
                        lo = Cat(
                            Cat(b.neg, Const(0, 1))
                            for b in in1_booth[i * sew // 2:(i + 1) * sew //
                                               2])
                        comps.append(Cat((lo[:2] + is_sub)[:2], lo[2:], hi))

                    m.d.comb += part_prod[self.width // 2].eq(Cat(comps))

        in3_adjust = Signal(self.width * 2)
        with m.Switch(self.sew):
            for w in range(4):
                with m.Case(w):
                    sew = 1 << (w + 3)
                    for i in range(self.width // sew):
                        m.d.comb += in3_adjust.word_select(i, 2 * sew).eq(
                            self.in3.word_select(i, sew))

        with m.If(VALUOperator.is_macc(self.fn)):
            with m.If(self.widen):
                m.d.comb += part_prod[-1].eq(Cat(self.in3, self.in3))
            with m.Else():
                m.d.comb += part_prod[-1].eq(
                    Mux(is_sub, ~in3_adjust, in3_adjust))

        #
        # 3-to-2 CSA
        #

        n = len(part_prod)
        addends_seq = []
        while n >= 3:
            addends_seq.append(n)
            n = n // 3 * 2 + n % 3

        def get_cout_mask(mask, sew):
            with m.Switch(sew):
                for w in range(4):
                    with m.Case(w):
                        n = 1 << (3 + w)
                        m.d.comb += mask.eq(
                            Cat(
                                Const(i % (2 * n) != 2 * n - 1, 1)
                                for i in range(self.width * 2)))

        def compress_3_to_2(addens, cout_mask):
            groups = [addens[i:i + 3] for i in range(0, len(addens), 3)]
            rem = []
            if len(groups[-1]) < 3:
                groups, rem = groups[:-1], groups[-1]

            cout = [Signal(self.width * 2) for _ in range(len(groups))]
            sum = [Signal(self.width * 2) for _ in range(len(groups))]

            for group, co, s in zip(groups, cout, sum):
                a, b, c = group
                csa = CSA3to2(self.width * 2)
                m.submodules += csa

                m.d.comb += [
                    csa.a.eq(a),
                    csa.b.eq(b),
                    csa.c.eq(c),
                    co.eq(csa.cout & (cout_mask << 1)),
                    s.eq(csa.sum),
                ]

            return sum + cout + rem

        cout_mask = Signal(self.width * 2)
        get_cout_mask(cout_mask, self.sew)

        addens = part_prod
        for n_addens in addends_seq[:-VMultiplier.CSA_BREAK]:
            assert len(addens) == n_addens
            addens = compress_3_to_2(addens, cout_mask)

        s1_valid = Signal()
        s1_sew = Signal.like(self.sew)
        s1_uop_idx = Signal.like(self.uop_idx)
        s1_widen = Signal()
        s1_h = Signal()
        s1_is_sub = Signal()
        s1_is_fixp = Signal()
        s1_vxrm = Signal(VXRoundingMode)
        s1_addens = [
            Signal(2 * self.width, name=f's1_addens{i}')
            for i in range(len(addens))
        ]
        m.d.sync += s1_valid.eq(self.valid)
        with m.If(self.valid):
            m.d.sync += [
                s1_sew.eq(self.sew),
                s1_uop_idx.eq(self.uop_idx),
                s1_widen.eq(self.widen),
                s1_h.eq(h),
                s1_is_sub.eq(is_sub),
                s1_is_fixp.eq(is_fixp),
                s1_vxrm.eq(self.vxrm),
            ]
            m.d.sync += [a.eq(b) for a, b in zip(s1_addens, addens)]

        s1_cout_mask = Signal(self.width * 2)
        get_cout_mask(s1_cout_mask, s1_sew)

        addens = s1_addens
        for n_addens in addends_seq[-VMultiplier.CSA_BREAK:]:
            assert len(addens) == n_addens
            addens = compress_3_to_2(addens, s1_cout_mask)

        assert len(addens) == 2

        cin = Signal(lane_bytes)
        cout = Signal(lane_bytes)
        adder_out = Signal(self.width * 2)
        for w in range(lane_bytes):
            if w == 0:
                m.d.comb += cin[w].eq(0)
            elif w % 4 == 0:
                m.d.comb += cin[w].eq((s1_sew == 3) & cout[w - 1])
            elif w % 2 == 0:
                m.d.comb += cin[w].eq(((s1_sew == 3) | (s1_sew == 2))
                                      & cout[w - 1])
            else:
                m.d.comb += cin[w].eq(s1_sew.any() & cout[w - 1])

            s = Cat(cin[w], addens[0].word_select(w, 16), Const(0, 1)) + Cat(
                cin[w], addens[1].word_select(w, 16), Const(0, 1))
            m.d.comb += Cat(adder_out.word_select(w, 16), cout[w]).eq(s[1:])

        wal_out = Signal(self.width * 2)
        m.d.sync += wal_out.eq(adder_out)

        s2_valid = Signal()
        s2_sew = Signal.like(s1_sew)
        s2_uop_idx = Signal.like(s1_uop_idx)
        s2_widen = Signal()
        s2_h = Signal()
        s2_is_sub = Signal()
        s2_is_fixp = Signal()
        s2_vxrm = Signal(VXRoundingMode)
        m.d.sync += s2_valid.eq(s1_valid)
        with m.If(s1_valid):
            m.d.sync += [
                s2_sew.eq(s1_sew),
                s2_widen.eq(s1_widen),
                s2_uop_idx.eq(s1_uop_idx),
                s2_h.eq(s1_h),
                s2_is_sub.eq(s1_is_sub),
                s2_is_fixp.eq(s1_is_fixp),
                s2_vxrm.eq(s1_vxrm),
            ]

        mul_out = Signal(self.width)
        with m.Switch(s2_sew):
            for w in range(4):
                with m.Case(w):
                    n = 1 << (3 + w)
                    for i in range(self.width // n):
                        m.d.comb += mul_out.word_select(i, n).eq(
                            Mux(s2_h, wal_out.word_select(2 * i + 1, n),
                                wal_out.word_select(2 * i, n)))

        with m.If(s2_widen):
            m.d.comb += mul_out.eq(
                Mux(s2_uop_idx[0], wal_out[self.width:], wal_out[:self.width]))

        vxsat = Signal(lane_bytes)
        with m.Switch(s2_sew):
            for w in range(4):
                with m.Case(w):
                    n = 1 << (3 + w)
                    for i in range(self.width // n):
                        m.d.comb += vxsat.word_select(i, n // 8).eq(
                            (wal_out[(i + 1) * n * 2 - 2:(i + 1) * n *
                                     2] == 1).replicate(n // 8))

        wal_out_rnd = Signal(self.width)
        wal_rnd_inc = Signal(lane_bytes)
        with m.Switch(s2_sew):
            for w in range(4):
                with m.Case(w):
                    n = 1 << (3 + w)
                    for i in range(self.width // n):
                        m.d.comb += [
                            wal_out_rnd.word_select(i, n).eq(
                                wal_out[i * n * 2 + n - 1:(i + 1) * n * 2 -
                                        1]),
                            wal_rnd_inc[i * n // 8].eq(
                                get_round_inc(s2_vxrm,
                                              wal_out.bit_select(i * n * 2, n),
                                              n - 1)),
                        ]

        rnd_adder = m.submodules.rnd_adder = RoundAdder(self.width)
        m.d.comb += [
            rnd_adder.din.eq(Mux(s2_is_sub, ~mul_out, wal_out_rnd)),
            rnd_adder.incr.eq(
                Mux(s2_is_sub, Const(~0, lane_bytes), wal_rnd_inc)),
            rnd_adder.sew.eq(s2_sew),
        ]

        fixp_out = Signal(self.width)
        m.d.comb += fixp_out.eq(rnd_adder.dout)
        with m.Switch(s2_sew):
            for w in range(4):
                with m.Case(w):
                    n = 1 << (3 + w)
                    for i in range(self.width // n):
                        with m.If(vxsat[i * n // 8]):
                            m.d.comb += fixp_out.word_select(i, n).eq(
                                Const(~0, n - 1))

        out = Signal(self.width)
        with m.If(s2_is_fixp):
            m.d.comb += out.eq(fixp_out)
        with m.Elif(s2_is_sub):
            m.d.comb += out.eq(rnd_adder.dout)
        with m.Else():
            m.d.comb += out.eq(mul_out)

        out_pipe_in = Cat(out, vxsat)
        out_pipe = m.submodules.out_pipe = Pipe(width=len(out_pipe_in),
                                                depth=self.latency - 2)
        m.d.comb += [
            out_pipe.in_valid.eq(s2_valid),
            out_pipe.in_data.eq(out_pipe_in),
            Cat(self.resp_data, self.vxsat).eq(out_pipe.out.bits),
        ]

        return m


class VIntDiv(Elaboratable):

    def __init__(self, width):
        self.width = width

        self.valid = Signal()
        self.ready = Signal()
        self.fn = Signal(VALUOperator)
        self.sew = Signal(2)
        self.in1 = Signal(width)
        self.in2 = Signal(width)

        self.resp = Decoupled(Signal, width)

    def elaborate(self, platform):
        m = Module()

        dividers = []
        for w in range(4):
            sew = 1 << (3 + w)
            ms = []

            for i in range(self.width // sew):
                div = IntDiv(sew)
                setattr(m.submodules, f'div{i}_e{sew}', div)

                with m.Switch(self.fn):
                    for valu_fn, alu_fn in (
                        (VALUOperator.VDIVU, ALUOperator.DIVU),
                        (VALUOperator.VDIV, ALUOperator.DIV),
                        (VALUOperator.VREMU, ALUOperator.REMU),
                        (VALUOperator.VREM, ALUOperator.REM),
                    ):
                        with m.Case(valu_fn):
                            m.d.comb += div.req.bits.fn.eq(alu_fn)

                m.d.comb += [
                    div.req.bits.in1.eq(self.in2.word_select(i, sew)),
                    div.req.bits.in2.eq(self.in1.word_select(i, sew)),
                ]

                ms.append(div)

            for i, div in enumerate(ms):
                m.d.comb += div.req.valid.eq((self.sew == w) & Cat(
                    self.valid & d.req.ready
                    for j, d in enumerate(ms) if i != j).all())

            dividers.append(ms)

        with m.Switch(self.sew):
            for w in range(4):
                with m.Case(w):
                    m.d.comb += self.ready.eq(
                        Cat(d.req.ready for d in dividers[w]).all())

        resp_sew = Signal.like(self.sew)
        with m.If(self.valid & self.ready):
            m.d.sync += resp_sew.eq(self.sew)

        with m.Switch(resp_sew):
            for w in range(4):
                with m.Case(w):
                    m.d.comb += [
                        self.resp.valid.eq(
                            Cat(d.resp.valid for d in dividers[w]).all()),
                        self.resp.bits.eq(Cat(d.resp.bits
                                              for d in dividers[w])),
                    ]

                    for i, div in enumerate(dividers[w]):
                        m.d.comb += div.resp.ready.eq(
                            Cat(self.resp.ready & d.resp.valid
                                for j, d in enumerate(dividers[w])
                                if i != j).all())

        return m


class Compare2to1(Elaboratable):

    def __init__(self, width):
        self.width = width

        self.a = Signal(width)
        self.b = Signal(width)
        self.max = Signal()
        self.unsigned = Signal()

        self.out = Signal(width)

    def elaborate(self, platform):
        m = Module()

        b_inv = ~self.b
        sum = Cat(Const(1, 1), self.a) + Cat(Const(1, 1), b_inv)
        cout = sum[self.width + 1]
        less = Mux(self.unsigned, ~cout, self.a[-1] ^ b_inv[-1] ^ cout)
        m.d.comb += self.out.eq(Mux(self.max ^ less, self.a, self.b))

        return m


class Compare3to1(Elaboratable):

    def __init__(self, width):
        self.width = width

        self.a = Signal(width)
        self.b = Signal(width)
        self.c = Signal(width)
        self.max = Signal()
        self.unsigned = Signal()

        self.out = Signal(width)

    def elaborate(self, platform):
        m = Module()

        lhs = [self.a, self.a, self.b]
        rhs = [self.b, self.c, self.c]

        cout = Signal(3)
        less = Signal(3)
        for i, (l, r) in enumerate(zip(lhs, rhs)):
            r_inv = ~r
            sum = Cat(Const(1, 1), l) + Cat(Const(1, 1), r_inv)
            m.d.comb += [
                cout[i].eq(sum[self.width + 1]),
                less[i].eq(
                    Mux(self.unsigned, ~cout[i],
                        l[self.width - 1] ^ r_inv[self.width - 1] ^ cout[i])),
            ]

        with m.If((less[0] & less[1] & ~self.max)
                  | (~less[0] & ~less[1] & self.max)):
            m.d.comb += self.out.eq(self.a)
        with m.Elif((~less[0] & less[2] & ~self.max)
                    | (less[0] & ~less[2] & self.max)):
            m.d.comb += self.out.eq(self.b)
        with m.Elif((~less[1] & ~less[2] & ~self.max)
                    | (less[1] & less[2] & self.max)):
            m.d.comb += self.out.eq(self.c)

        return m


class ReductionSlice(Elaboratable):

    def __init__(self, width):
        self.width = width

        self.valid = Signal()
        self.sew = Signal(2)
        self.widen = Signal()
        self.opcode = Signal(VOpCode)
        self.in_data = Signal(width * 2)

        self.resp_data = Signal(width)

    def elaborate(self, platform):
        m = Module()

        is_max = (self.opcode == VOpCode.VREDMAXU) | (self.opcode
                                                      == VOpCode.VREDMAX)
        is_unsigned = (self.opcode == VOpCode.VREDMAXU) | (self.opcode
                                                           == VOpCode.VREDMINU)

        logic_out = [Signal(64, name=f'logic_out{w}') for w in range(4)]
        in_data_64b = [
            self.in_data.word_select(i, 64) for i in range(self.width // 64)
        ]
        with m.Switch(self.opcode):
            with m.Case(VOpCode.VREDAND):
                m.d.comb += logic_out[3].eq(
                    functools.reduce(operator.and_, in_data_64b))
            with m.Case(VOpCode.VREDOR):
                m.d.comb += logic_out[3].eq(
                    functools.reduce(operator.or_, in_data_64b))
            with m.Case(VOpCode.VREDXOR):
                m.d.comb += logic_out[3].eq(
                    functools.reduce(operator.xor, in_data_64b))

        for w in reversed(range(3)):
            n = 1 << (w + 3)
            with m.Switch(self.opcode):
                with m.Case(VOpCode.VREDAND):
                    m.d.comb += logic_out[w].eq(logic_out[w + 1][0:n]
                                                & logic_out[w + 1][n:2 * n])
                with m.Case(VOpCode.VREDOR):
                    m.d.comb += logic_out[w].eq(logic_out[w + 1][0:n]
                                                | logic_out[w + 1][n:2 * n])
                with m.Case(VOpCode.VREDXOR):
                    m.d.comb += logic_out[w].eq(logic_out[w + 1][0:n]
                                                ^ logic_out[w + 1][n:2 * n])

        adder_out = [
            Signal(self.width, name=f'adder_out{w}') for w in range(4)
        ]
        for w in range(4):
            n = 1 << (3 + w)
            addens = [
                self.in_data.word_select(i, n)
                for i in range(len(self.in_data) // n)
            ]

            while len(addens) > 2:
                groups = [addens[i:i + 4] for i in range(0, len(addens), 4)]
                rem = []
                if len(groups[-1]) < 4:
                    groups, rem = groups[:-1], groups[-1]

                cout = [Signal(n) for _ in range(len(groups))]
                sum = [Signal(n) for _ in range(len(groups))]
                for group, co, s in zip(groups, cout, sum):
                    a, b, c, d = group
                    csa = CSA4to2(n)
                    m.submodules += csa

                    m.d.comb += [
                        csa.a.eq(a),
                        csa.b.eq(b),
                        csa.c.eq(c),
                        csa.d.eq(d),
                        co.eq(csa.cout),
                        s.eq(csa.sum),
                    ]

                addens = sum + cout + rem

            assert len(addens) == 2
            m.d.comb += adder_out[w].eq(Cat(addens[0], addens[1]))

        minmax = [Signal(self.width, name=f'minmax{w}') for w in range(4)]
        for w in range(4):
            n = 1 << (3 + w)
            comparands = [
                self.in_data.word_select(i, n)
                for i in range(len(self.in_data) // n)
            ]

            while len(comparands) > 1:
                new_comparands = []

                if len(comparands) == 2:
                    comp2 = Compare2to1(width=n)
                    m.submodules += comp2
                    m.d.comb += [
                        comp2.a.eq(comparands[0]),
                        comp2.b.eq(comparands[1]),
                        comp2.max.eq(is_max),
                        comp2.unsigned.eq(is_unsigned),
                    ]

                    new_comparands.append(comp2.out)

                else:
                    groups = [
                        comparands[i:i + 3]
                        for i in range(0, len(comparands), 3)
                    ]
                    rem = []
                    if len(groups[-1]) < 3:
                        groups, rem = groups[:-1], groups[-1]

                    for a, b, c in groups:
                        comp3 = Compare3to1(width=n)
                        m.submodules += comp3
                        m.d.comb += [
                            comp3.a.eq(a),
                            comp3.b.eq(b),
                            comp3.c.eq(c),
                            comp3.max.eq(is_max),
                            comp3.unsigned.eq(is_unsigned),
                        ]
                        new_comparands.append(comp3.out)

                    new_comparands.extend(rem)

                comparands = new_comparands

            assert len(comparands) == 1
            m.d.comb += minmax[w].eq(comparands[0])

        with m.If(self.valid):
            with m.Switch(self.opcode):
                with m.Case(VOpCode.VREDAND, VOpCode.VREDOR, VOpCode.VREDXOR):
                    with m.Switch(self.sew):
                        for w in range(4):
                            with m.Case(w):
                                m.d.sync += self.resp_data.eq(logic_out[w])

                with m.Case(VOpCode.VREDMINU, VOpCode.VREDMIN,
                            VOpCode.VREDMAXU, VOpCode.VREDMAX):
                    with m.Switch(self.sew):
                        for w in range(4):
                            with m.Case(w):
                                m.d.sync += self.resp_data.eq(minmax[w])

                with m.Default():
                    with m.Switch(self.sew + self.widen):
                        for w in range(4):
                            with m.Case(w):
                                m.d.sync += self.resp_data.eq(adder_out[w])

        return m


class VMask(Elaboratable):

    def __init__(self, width):
        self.width = width

        self.valid = Signal()
        self.sew = Signal(2)
        self.uop_idx = Signal(3)
        self.cpop_done = Signal()
        self.vm = Signal()
        self.opcode = Signal(VOpCode)
        self.in1 = Signal(width)
        self.in2 = Signal(width)
        self.tail = Signal(width)
        self.vmask = Signal(width)

        self.resp_data = Valid(Signal, width)

    def elaborate(self, platform):
        m = Module()

        is_vcpop = self.opcode == VOpCode.VCPOP
        is_vid = self.opcode == VOpCode.VID

        vs2_masked = Signal(self.width)
        for i in range(self.width):
            m.d.comb += vs2_masked[i].eq(self.in2[i]
                                         & (self.vm | self.vmask[i])
                                         & ~self.tail[i])

        #
        # Logical
        #

        in1_inv = Mux((self.opcode == VOpCode.VMANDNOT) |
                      (self.opcode == VOpCode.VMORNOT), ~self.in1, self.in1)
        in1_xor_in2 = in1_inv ^ self.in2
        in1_and_in2 = in1_inv & self.in2

        logic = Mux(
            (self.opcode == VOpCode.VMXOR) | (self.opcode == VOpCode.VMOR) |
            (self.opcode == VOpCode.VMORNOT) |
            (self.opcode == VOpCode.VMXNOR), in1_xor_in2, 0) | Mux(
                (self.opcode == VOpCode.VMAND) |
                (self.opcode == VOpCode.VMOR) |
                (self.opcode == VOpCode.VMORNOT) |
                (self.opcode == VOpCode.VMANDNOT), in1_and_in2, 0)

        #
        # VFIRST
        #

        vfirst = Signal(self.width)
        ffs = m.submodules.ffs = FindFirstSet(self.width)
        m.d.comb += ffs.inp.eq(vs2_masked)

        with m.If(~vs2_masked.any()):
            m.d.comb += vfirst.eq(~0)
        with m.Else():
            m.d.comb += vfirst.eq(ffs.out)

        #
        # VMSBF, VMSIF, VMSOF
        #

        vmsbf = Signal(self.width)
        vmsif = Signal(self.width)
        vmsof = Signal(self.width)
        sbf = m.submodules.sbf = SetBeforeFirst(self.width)
        m.d.comb += [
            sbf.inp.eq(vs2_masked),
            vmsbf.eq(sbf.out),
            vmsif.eq(Cat(Const(1, 1), vmsbf[:-1])),
            vmsof.eq(~vmsbf & vmsif),
        ]

        #
        # VCPOP, VIOTA, VID
        #

        vs2_masked_uop = Signal(self.width // 8)
        vs2_masked_vid = Signal(self.width // 8)
        m.d.comb += vs2_masked_vid.eq(Mux(is_vid, ~0, vs2_masked_uop))
        with m.Switch(self.sew):
            for w in range(4):
                with m.Case(w):
                    n = 1 << (3 + w)
                    stride = self.width // n

                    m.d.comb += vs2_masked_uop.eq(
                        vs2_masked.word_select(self.uop_idx, stride))

        one_count = Signal(self.width + 8)
        one_count_acc = Signal(self.width + 8)
        one_sum = Signal(range(self.width + 1))
        for i in range(self.width // 8):
            popcount = PopCount(i + 1)
            m.submodules += popcount
            m.d.comb += [
                popcount.inp.eq(vs2_masked_vid[:i + 1]),
                one_count.word_select(i + 1, 8).eq(popcount.out),
            ]

        for i in range(self.width // 8 + 1):
            m.d.comb += one_count_acc.word_select(i, 8).eq(
                Mux(self.uop_idx.any(),
                    one_count.word_select(i, 8) + one_sum,
                    one_count.word_select(i, 8)))

        cur_count = Signal(range(self.width + 1))
        with m.Switch(self.sew):
            for w in range(4):
                with m.Case(w):
                    n = 1 << (3 + w)
                    stride = self.width // n
                    m.d.comb += cur_count.eq(
                        one_count_acc.word_select(stride, 8))
        m.d.sync += one_sum.eq(cur_count)

        s1_sew = Signal.like(self.sew)
        s1_opcode = Signal(VOpCode)
        with m.If(self.valid):
            m.d.sync += [
                s1_sew.eq(self.sew),
                s1_opcode.eq(self.opcode),
            ]

        vd_reg = Signal(self.width)

        with m.If(self.valid):
            with m.Switch(self.opcode):
                with m.Case(VOpCode.VFIRST):
                    m.d.sync += vd_reg.eq(vfirst)
                with m.Case(VOpCode.VMSBF):
                    m.d.sync += vd_reg.eq(vmsbf)
                with m.Case(VOpCode.VMSIF):
                    m.d.sync += vd_reg.eq(vmsif)
                with m.Case(VOpCode.VMSOF):
                    m.d.sync += vd_reg.eq(vmsof)
                with m.Case(VOpCode.VIOTA, VOpCode.VID):
                    m.d.sync += vd_reg.eq(one_count_acc[:self.width])
                with m.Case(VOpCode.VCPOP):
                    m.d.sync += vd_reg.eq(cur_count)

                with m.Default():
                    m.d.sync += vd_reg.eq(logic)

        vd_out = Signal(self.width)
        with m.Switch(s1_opcode):
            with m.Case(VOpCode.VIOTA, VOpCode.VID):
                with m.Switch(s1_sew):
                    for w in range(4):
                        with m.Case(w):
                            n = 1 << (3 + w)
                            for i in range(self.width // n):
                                m.d.comb += vd_out.bit_select(i * n, 8).eq(
                                    vd_reg.word_select(i, 8))

            with m.Default():
                m.d.comb += vd_out.eq(vd_reg)

        out_valid = Signal()
        m.d.sync += out_valid.eq(0)
        with m.If(self.valid & (~is_vcpop | self.cpop_done)):
            m.d.sync += out_valid.eq(1)

        m.d.comb += [
            self.resp_data.valid.eq(out_valid),
            self.resp_data.bits.eq(vd_out),
        ]

        return m
