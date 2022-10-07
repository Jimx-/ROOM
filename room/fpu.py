from amaranth import *
from enum import IntEnum

from room.consts import *


class FPUOperator(IntEnum):
    FMADD = 0
    FNMSUB = 1
    ADD = 2
    MUL = 3


class FPFormat(IntEnum):
    S = 0
    D = 1


class FType:

    INFO_LAYOUT = [('is_normal', 1), ('is_subnormal', 1), ('is_zero', 1),
                   ('is_inf', 1), ('is_nan', 1), ('is_snan', 1),
                   ('is_qnan', 1)]

    def __init__(self, exp, man):
        self.exp = exp
        self.man = man

    def bias(self):
        return (2**(self.exp - 1)) - 1

    def half_bias(self):
        return (2**(self.exp - 2)) - 1

    def record_layout(self):
        return [('man', self.man), ('exp', self.exp), ('sign', 1)]

    def classify(self, x):
        man = x[:self.man]
        exp = x[self.man:self.man + self.exp]

        is_normal = (exp != 0) & (exp != Repl(1, len(exp)))
        is_zero = (exp == 0) & (man == 0)
        is_subnormal = (exp == 0) & (man != 0)
        is_inf = (exp == Repl(1, len(exp))) & (man == 0)
        is_nan = (exp == Repl(1, len(exp))) & (man != 0)
        is_signalling = is_nan & (man[-1] == 0)
        is_quiet = is_nan & ~is_signalling

        return Cat(is_normal, is_subnormal, is_zero, is_inf, is_nan,
                   is_signalling, is_quiet)


FType.FP32 = FType(8, 23)
FType.FP64 = FType(11, 52)

_fmt_ftypes = {
    FPFormat.S: FType.FP32,
    FPFormat.D: FType.FP64,
}


class FPUInput(Record):

    def __init__(self, width, name=None, src_loc_at=0):
        super().__init__([
            ('fn', Shape.cast(FPUOperator).width),
            ('fn_mod', 1),
            ('rm', Shape.cast(RoundingMode).width),
            ('in1', width),
            ('in2', width),
            ('in3', width),
        ],
                         name=name,
                         src_loc_at=1 + src_loc_at)


class FPUResult(Record):

    def __init__(self, width, name=None, src_loc_at=0):
        super().__init__([
            ('data', width),
        ],
                         name=name,
                         src_loc_at=1 + src_loc_at)


class FPURounding(Elaboratable):

    def __init__(self, abs_width):
        self.abs_width = abs_width

        self.in_abs = Signal(abs_width)
        self.in_sign = Signal()

        self.round_sticky_bits = Signal(2)
        self.round_mode = Signal(RoundingMode)
        self.eff_subtraction = Signal()

        self.rounded_abs = Signal(abs_width)
        self.out_sign = Signal()

    def elaborate(self, platform):
        m = Module()

        round_up = Signal()

        with m.Switch(self.round_mode):
            with m.Case(RoundingMode.RNE):
                with m.Switch(self.round_sticky_bits):
                    with m.Case(0b10):
                        m.d.comb += round_up.eq(self.in_abs[0])
                    with m.Case(0b11):
                        m.d.comb += round_up.eq(1)

            with m.Case(RoundingMode.RDN):
                m.d.comb += round_up.eq((self.round_sticky_bits != 0)
                                        & self.in_sign)

            with m.Case(RoundingMode.RUP):
                m.d.comb += round_up.eq((self.round_sticky_bits != 0)
                                        & ~self.in_sign)

            with m.Case(RoundingMode.RMM):
                m.d.comb += round_up.eq(self.round_sticky_bits[1])

        m.d.comb += [
            self.rounded_abs.eq(self.in_abs + round_up),
            self.out_sign.eq(self.in_sign),
        ]

        return m


class FPUFMA(Elaboratable):

    def __init__(self, width, format, latency=3):
        self.width = width
        self.ftyp = _fmt_ftypes[format]
        self.latency = latency

        self.inp = FPUInput(self.width)
        self.inp_valid = Signal()

        self.out = FPUResult(self.width)
        self.out_valid = Signal()

    def elaborate(self, platform):
        m = Module()

        prec_bits = self.ftyp.man + 1
        exp_width = max(self.ftyp.exp + 2,
                        Shape.cast(range(2 * prec_bits + 3)).width)

        valid = Signal()
        inp = FPUInput(self.width)

        m.d.sync += valid.eq(self.inp_valid)
        with m.If(self.inp_valid):
            m.d.sync += inp.eq(self.inp)

        in1 = Record(self.ftyp.record_layout())
        in2 = Record(self.ftyp.record_layout())
        in3 = Record(self.ftyp.record_layout())

        info1 = Record(self.ftyp.INFO_LAYOUT)
        info2 = Record(self.ftyp.INFO_LAYOUT)
        info3 = Record(self.ftyp.INFO_LAYOUT)

        m.d.comb += [
            in1.eq(inp.in1),
            in2.eq(inp.in2),
            in3.eq(inp.in3),
            in3.sign.eq(inp.in3[-1] ^ inp.fn_mod),
            info1.eq(self.ftyp.classify(in1)),
            info2.eq(self.ftyp.classify(in2)),
            info3.eq(self.ftyp.classify(in3)),
        ]

        with m.Switch(inp.fn):
            with m.Case(FPUOperator.FNMSUB):
                m.d.comb += in1.sign.eq(~inp.in1[-1])

            with m.Case(FPUOperator.ADD):
                m.d.comb += [
                    in2.sign.eq(0),
                    in2.exp.eq(self.ftyp.bias()),
                    in2.man.eq(0),
                    info2.is_normal.eq(1),
                ]

            with m.Case(FPUOperator.MUL):
                m.d.comb += [
                    in3.sign.eq(1),
                    in3.exp.eq(0),
                    in3.man.eq(0),
                    info3.is_zero.eq(1),
                ]

        eff_subtraction = in1.sign ^ in2.sign ^ in3.sign
        tentative_sign = in1.sign ^ in2.sign

        exp_addend = Signal(signed(exp_width))
        exp_product = Signal(signed(exp_width))
        exp_diff = Signal(signed(exp_width))
        tentative_exp = Signal(signed(exp_width))

        addend_shamt = Signal(range(3 * prec_bits + 5))
        with m.If(exp_diff <= -2 * prec_bits - 1):
            m.d.comb += addend_shamt.eq(3 * prec_bits + 4)
        with m.Elif(exp_diff <= prec_bits + 2):
            m.d.comb += addend_shamt.eq(prec_bits + 3 - exp_diff)
        with m.Else():
            m.d.comb += addend_shamt.eq(0)

        exp1 = Signal(signed(exp_width))
        exp2 = Signal(signed(exp_width))
        exp3 = Signal(signed(exp_width))

        m.d.comb += [
            exp1.eq(Cat(in1.exp, 0)),
            exp2.eq(Cat(in2.exp, 0)),
            exp3.eq(Cat(in3.exp, 0)),
            exp_addend.eq(exp3 + Cat(~info3.is_normal, 0).as_signed()),
            exp_product.eq(
                Mux(
                    info1.is_zero | info2.is_zero, 2 - self.ftyp.bias(),
                    exp1 + info1.is_subnormal + exp2 + info2.is_subnormal -
                    self.ftyp.bias())),
            exp_diff.eq(exp_addend - exp_product),
            tentative_exp.eq(Mux(exp_diff > 0, exp_addend, exp_product)),
        ]

        man1 = Signal(prec_bits)
        man2 = Signal(prec_bits)
        man3 = Signal(prec_bits)
        product = Signal(2 * prec_bits)

        m.d.comb += [
            man1.eq(Cat(in1.man, info1.is_normal)),
            man2.eq(Cat(in2.man, info2.is_normal)),
            man3.eq(Cat(in3.man, info3.is_normal)),
            product.eq(man1 * man2),
        ]

        addend_shifted = Signal(3 * prec_bits + 4)
        addend_sticky_bits = Signal(prec_bits)
        sticky_before_add = (addend_sticky_bits != 0)

        m.d.comb += Cat(
            addend_sticky_bits,
            addend_shifted).eq((man3 << (3 * prec_bits + 4)) >> addend_shamt)

        sum_raw = Signal(3 * prec_bits + 5)
        sum = Signal(3 * prec_bits + 4)

        m.d.comb += [
            sum_raw.eq((product << 2) +
                       Mux(eff_subtraction, ~addend_shifted, addend_shifted) +
                       (eff_subtraction & (addend_sticky_bits != 0))),
            sum.eq(Mux(eff_subtraction & ~sum_raw[-1], -sum_raw, sum_raw)),
        ]

        final_sign = Mux(eff_subtraction & (sum_raw[-1] == tentative_sign), 1,
                         Mux(eff_subtraction, 0, tentative_sign))

        eff_subtraction_q = Signal.like(eff_subtraction)
        exp_product_q = Signal.like(exp_product)
        exp_diff_q = Signal.like(exp_diff)
        tentative_exp_q = Signal.like(tentative_exp)
        addend_shamt_q = Signal.like(addend_shamt)
        sticky_before_add_q = Signal.like(sticky_before_add)
        sum_q = Signal.like(sum)
        final_sign_q = Signal.like(final_sign)
        round_mode_q = Signal.like(inp.rm)
        valid_q1 = Signal()

        if self.latency > 1:
            m.d.sync += [
                eff_subtraction_q.eq(eff_subtraction),
                exp_product_q.eq(exp_product),
                exp_diff_q.eq(exp_diff),
                tentative_exp_q.eq(tentative_exp),
                addend_shamt_q.eq(addend_shamt),
                sticky_before_add_q.eq(sticky_before_add),
                sum_q.eq(sum),
                final_sign_q.eq(final_sign),
                round_mode_q.eq(inp.rm),
                valid_q1.eq(valid),
            ]
        else:
            m.d.comb += [
                eff_subtraction_q.eq(eff_subtraction),
                exp_product_q.eq(exp_product),
                exp_diff_q.eq(exp_diff),
                tentative_exp_q.eq(tentative_exp),
                addend_shamt_q.eq(addend_shamt),
                sticky_before_add_q.eq(sticky_before_add),
                sum_q.eq(sum),
                final_sign_q.eq(final_sign),
                round_mode_q.eq(inp.rm),
                valid_q1.eq(valid),
            ]

        clz = Signal(range(2 * prec_bits + 4))
        clz_empty = Signal()

        m.d.comb += [
            clz.eq(2 * prec_bits + 3),
            clz_empty.eq(1),
        ]

        for i in range(2 * prec_bits + 3):
            with m.If(sum_q[i]):
                m.d.comb += [
                    clz.eq(2 * prec_bits + 2 - i),
                    clz_empty.eq(0),
                ]

        norm_shamt = Signal.like(addend_shamt)
        exp_normalized = Signal(self.ftyp.exp)

        with m.If((exp_diff_q <= 0) | (eff_subtraction_q & (exp_diff_q <= 2))):
            with m.If((exp_product_q - Cat(clz, 0).as_signed() + 1 >= 0)
                      & ~clz_empty):
                m.d.comb += [
                    norm_shamt.eq(prec_bits + 2 + clz),
                    exp_normalized.eq(exp_product_q - Cat(clz, 0).as_signed() +
                                      1),
                ]
            with m.Else():
                m.d.comb += [
                    norm_shamt.eq(prec_bits + 2 + exp_product_q),
                    exp_normalized.eq(0),
                ]
        with m.Else():
            m.d.comb += [
                norm_shamt.eq(addend_shamt_q),
                exp_normalized.eq(tentative_exp_q),
            ]

        sum_shifted = Signal.like(sum_raw)
        m.d.comb += sum_shifted.eq(sum_q << norm_shamt)

        final_mantissa = Signal(prec_bits + 1)
        final_exponent = Signal(self.ftyp.exp)
        sum_sticky_bits = Signal(2 * prec_bits + 3)

        m.d.comb += [
            Cat(sum_sticky_bits, final_mantissa).eq(sum_shifted),
            final_exponent.eq(exp_normalized),
        ]

        with m.If(sum_shifted[-1]):
            m.d.comb += [
                Cat(sum_sticky_bits, final_mantissa).eq(sum_shifted >> 1),
                final_exponent.eq(exp_normalized + 1),
            ]
        with m.Elif(sum_shifted[-2]):
            pass
        with m.Elif(exp_normalized > 1):
            m.d.comb += [
                Cat(sum_sticky_bits, final_mantissa).eq(sum_shifted << 1),
                final_exponent.eq(exp_normalized - 1),
            ]

        sticky_after_norm = (sum_sticky_bits != 0) | sticky_before_add_q

        of_before_round = final_exponent >= (2**self.ftyp.exp - 1)
        uf_before_round = final_exponent == 0

        pre_round_sign = final_sign
        pre_round_exponent = Mux(of_before_round, 2**self.ftyp.exp - 2,
                                 final_exponent[:self.ftyp.exp])
        pre_round_mantissa = Mux(of_before_round, Repl(1, self.ftyp.man),
                                 final_mantissa[1:self.ftyp.man + 1])

        round_sticky_bits = Mux(of_before_round, 0b11,
                                Cat(sticky_after_norm, final_mantissa[0]))

        rounding = m.submodules.rounding = FPURounding(self.ftyp.exp +
                                                       self.ftyp.man)
        m.d.comb += [
            rounding.in_sign.eq(pre_round_sign),
            rounding.in_abs.eq(Cat(pre_round_mantissa, pre_round_exponent)),
            rounding.round_mode.eq(round_mode_q),
            rounding.round_sticky_bits.eq(round_sticky_bits),
            rounding.eff_subtraction.eq(eff_subtraction_q),
        ]

        uf_after_round = rounding.rounded_abs[self.ftyp.man:self.ftyp.man +
                                              self.ftyp.exp] == 0
        of_after_round = rounding.rounded_abs[self.ftyp.man:self.ftyp.man +
                                              self.ftyp.exp] == Repl(
                                                  1, self.ftyp.exp)

        regular_result = Cat(rounding.rounded_abs, rounding.out_sign)

        result = regular_result

        if self.latency > 2:
            result_q = [
                Signal.like(result, name=f's{2+i}_result')
                for i in range(self.latency - 2)
            ]
            valid_q2 = [
                Signal(name=f's{2+i}_valid') for i in range(self.latency - 2)
            ]

            m.d.sync += [
                result_q[0].eq(result),
                valid_q2[0].eq(valid_q1),
            ]

            for i in range(1, self.latency - 2):
                m.d.sync += [
                    result_q[i].eq(result_q[i - 1]),
                    valid_q2[i].eq(valid_q2[i - 1]),
                ]

            m.d.comb += [
                self.out.data.eq(result_q[-1]),
                self.out_valid.eq(valid_q2[-1]),
            ]

        else:
            m.d.comb += [
                self.out.data.eq(result),
                self.out_valid.eq(valid_q1),
            ]

        return m


class FPUDivSqrtMulti(Elaboratable):

    def __init__(self):
        self.ftyp = FType.FP64

        self.a = Signal(64)
        self.b = Signal(64)
        self.is_sqrt = Signal()
        self.fmt = Signal(FPFormat)
        self.rm = Signal(RoundingMode)
        self.in_valid = Signal()
        self.in_ready = Signal()

        self.kill = Signal()

        self.out = Signal(64)
        self.out_valid = Signal()
        self.out_ready = Signal()

    def elaborate(self, platform):
        m = Module()

        ROUND_BITS = 6
        PREC_BITS = self.ftyp.man + ROUND_BITS

        N_ITERS = {
            FPFormat.S: PREC_BITS // 4,
            FPFormat.D: PREC_BITS // 2 - 1,
        }

        in_a = Record(self.ftyp.record_layout())
        in_b = Record(self.ftyp.record_layout())

        in_info_a = Record(self.ftyp.INFO_LAYOUT)
        in_info_b = Record(self.ftyp.INFO_LAYOUT)

        m.d.comb += [
            in_a.eq(self.a),
            in_b.eq(self.b),
            in_info_a.eq(self.ftyp.classify(self.a)),
            in_info_b.eq(self.ftyp.classify(self.b)),
        ]

        with m.Switch(self.fmt):
            for fmt in [FPFormat.S, FPFormat.D]:
                with m.Case(fmt):
                    ftyp = _fmt_ftypes[fmt]

                    m.d.comb += [
                        in_info_a.eq(ftyp.classify(self.a)),
                        in_info_b.eq(ftyp.classify(self.b)),
                    ]

        #
        # Preprocessing
        #

        exp_a = Signal(self.ftyp.exp + 1)
        exp_b = Signal(self.ftyp.exp + 1)

        man_a = Signal(self.ftyp.man + 2)  # With implied bit & sign bit
        man_b = Signal(self.ftyp.man + 2)
        man_b_inv = Signal(self.ftyp.man + 2)
        man_sqrt = Signal(self.ftyp.man + 2)

        info_a = Record(self.ftyp.INFO_LAYOUT)
        info_b = Record(self.ftyp.INFO_LAYOUT)

        start = Signal()
        do_sqrt = Signal()
        fmt_sel = Signal()
        round_mode = Signal.like(self.rm)
        final_sign = Signal()

        m.d.sync += start.eq(self.in_valid & self.in_ready),

        with m.If(self.in_valid & self.in_ready):
            m.d.sync += [
                info_a.eq(in_info_a),
                info_b.eq(in_info_b),
                do_sqrt.eq(self.is_sqrt),
                fmt_sel.eq(self.fmt),
                round_mode.eq(self.rm),
            ]

            with m.Switch(self.fmt):
                for fmt in [FPFormat.S, FPFormat.D]:
                    with m.Case(fmt):
                        ftyp = _fmt_ftypes[fmt]

                        sign_a = in_a[ftyp.man + ftyp.exp]
                        sign_b = in_b[ftyp.man + ftyp.exp]

                        m.d.sync += [
                            exp_a.eq(in_a[ftyp.man:ftyp.man + ftyp.exp]),
                            exp_b.eq(in_b[ftyp.man:ftyp.man + ftyp.exp]),
                            man_a.eq(
                                Cat(Repl(0, self.ftyp.man - ftyp.man),
                                    in_a[:ftyp.man], in_info_a.is_normal, 0)),
                            man_b.eq(
                                Cat(Repl(0, self.ftyp.man - ftyp.man),
                                    in_b[:ftyp.man], in_info_b.is_normal, 0)),
                        ]

                        with m.If(self.is_sqrt):
                            m.d.sync += final_sign.eq(sign_a)
                        with m.Else():
                            m.d.sync += final_sign.eq(sign_a ^ sign_b)

        m.d.comb += man_sqrt.eq(
            Mux(exp_a[0], Cat(man_a[:-1], 0), Cat(0, man_a[:-1])))

        with m.Switch(self.fmt):
            for fmt in [FPFormat.S, FPFormat.D]:
                with m.Case(fmt):
                    ftyp = _fmt_ftypes[fmt]

                    m.d.comb += man_b_inv.eq(
                        Cat(Repl(0, self.ftyp.man - ftyp.man),
                            ~man_b[self.ftyp.man - ftyp.man:]))

        #
        # FSM control
        #

        count = Signal(range(64))
        max_iters = Signal.like(count)

        with m.Switch(fmt_sel):
            for fmt in [FPFormat.S, FPFormat.D]:
                with m.Case(fmt):
                    m.d.comb += max_iters.eq(N_ITERS[fmt])

        with m.If(self.kill | (count == max_iters)):
            m.d.sync += count.eq(0)
        with m.Elif(start | (count > 0)):
            m.d.sync += count.eq(count + 1)
        with m.Else():
            m.d.sync += count.eq(0)

        m.d.comb += self.in_ready.eq(~start & (count == 0) & ~self.out_valid)

        with m.If(self.kill):
            m.d.sync += self.out_valid.eq(0)
        with m.Elif(count == max_iters):
            m.d.sync += self.out_valid.eq(1)
        with m.Elif(self.out_valid & self.out_ready):
            m.d.sync += self.out_valid.eq(0)

        quotient = Signal(PREC_BITS + 1)  # 59 bits
        remainder = Signal(PREC_BITS + 2)  # 60 bits

        adder_in_div_a = [Signal.like(remainder) for _ in range(2)]
        adder_in_div_b = [Signal.like(remainder) for _ in range(2)]

        adder_in_a = [
            Signal.like(remainder, name=f'adder_in_a{i}') for i in range(2)
        ]
        adder_in_b = [
            Signal.like(remainder, name=f'adder_in_b{i}') for i in range(2)
        ]

        adder_out = [
            Signal.like(remainder, name=f'adder_out{i}') for i in range(2)
        ]
        adder_carry = [Signal(name=f'adder_carry{i}') for i in range(2)]

        sqrt_R = [Signal.like(remainder, name=f'sqrt_R{i}') for i in range(2)]
        sqrt_Q = [Signal.like(remainder, name=f'sqrt_Q{i}') for i in range(2)]

        sqrt_Di = [Signal(2, name=f'sqrt_Di{i}') for i in range(2)]
        sqrt_Do = [Signal(2, name=f'sqrt_Do{i}') for i in range(2)]

        with m.Switch(count):
            for s in range(N_ITERS[FPFormat.D] + 1):
                with m.Case(s):
                    if 4 * s <= self.ftyp.man:
                        m.d.comb += sqrt_Di[0].eq(
                            man_sqrt[self.ftyp.man - 4 * s:self.ftyp.man -
                                     4 * s + 2])

                    Q0 = Cat(quotient[:s * 2], Repl(0, len(sqrt_Q[0]) - s * 2))
                    m.d.comb += sqrt_Q[0].eq(
                        Mux((count == 0) | quotient[0], ~Q0, Q0))

                    if 4 * s + 2 <= self.ftyp.man:
                        m.d.comb += sqrt_Di[1].eq(
                            man_sqrt[self.ftyp.man - 4 * s - 2:self.ftyp.man -
                                     4 * s])

                    Q1 = Cat(adder_carry[0], quotient[:s * 2],
                             Repl(0,
                                  len(sqrt_Q[1]) - s * 2 - 1))
                    m.d.comb += sqrt_Q[1].eq(Mux(adder_carry[0], ~Q1, Q1))

        m.d.comb += [
            sqrt_R[0].eq(Mux(start, 0, remainder)),
            sqrt_R[1].eq(
                Cat(sqrt_Do[0], adder_out[0][:self.ftyp.man + ROUND_BITS - 1],
                    adder_out[0][-1])),
        ]

        def recode_vec(x, carry):
            return Cat(
                Const(0, ROUND_BITS - 1),
                (fmt_sel == FPFormat.D) & carry,
                x[:self.ftyp.man - FType.FP32.man - 1],
                Mux(fmt_sel == FPFormat.S, carry,
                    x[self.ftyp.man - FType.FP32.man - 1]),
                x[self.ftyp.man - FType.FP32.man:],
            )

        m.d.comb += [
            adder_in_div_a[0].eq(
                Mux(start, recode_vec(man_a, 1),
                    recode_vec(remainder[ROUND_BITS - 1:], quotient[0]))),
            adder_in_div_b[0].eq(
                Mux(start | quotient[0], recode_vec(man_b_inv, 1),
                    Cat(Const(0, ROUND_BITS), man_b))),
            adder_in_div_a[1].eq(
                recode_vec(adder_out[0][ROUND_BITS - 1:], ~adder_out[0][-1])),
            adder_in_div_b[1].eq(
                Mux(~adder_out[0][-1], recode_vec(man_b_inv, 1),
                    Cat(Const(0, ROUND_BITS), man_b))),
        ]

        m.d.comb += [
            adder_in_a[0].eq(Mux(do_sqrt, sqrt_R[0], adder_in_div_a[0])),
            adder_in_b[0].eq(Mux(do_sqrt, sqrt_Q[0], adder_in_div_b[0])),
            adder_in_a[1].eq(Mux(do_sqrt, sqrt_R[1], adder_in_div_a[1])),
            adder_in_b[1].eq(Mux(do_sqrt, sqrt_Q[1], adder_in_div_b[1])),
        ]

        for i in range(2):
            cin = Mux(do_sqrt, sqrt_Di[i][0] | sqrt_Di[i][1], 0)

            m.d.comb += [
                sqrt_Do[i][0].eq(~sqrt_Di[i][0]),
                sqrt_Do[i][1].eq(~(sqrt_Di[i][0] ^ sqrt_Di[i][1])),
                Cat(adder_out[i],
                    adder_carry[i]).eq(adder_in_a[i] + adder_in_b[i] + cin),
            ]

        with m.If(start | (count > 0)):
            sqrt_R = Cat(sqrt_Do[1],
                         adder_out[1][:self.ftyp.man + ROUND_BITS - 1],
                         adder_out[1][-1])

            m.d.sync += [
                remainder.eq(Mux(do_sqrt, sqrt_R, adder_out[-1])),
                quotient.eq(Cat(*reversed(adder_carry), quotient[:-2])),
            ]

        final_exponent = Signal(self.ftyp.exp + 2)
        final_mantissa = Signal.like(quotient)

        exp_add_a = Mux(do_sqrt, Cat(exp_a[1:], Repl(exp_a[-1], 3)),
                        Cat(exp_a, Repl(exp_a[-1], 2)))
        exp_add_b = Mux(do_sqrt, Cat(exp_a[0], Repl(0, self.ftyp.exp + 1)),
                        ~Cat(exp_b, Repl(exp_b[-1], 2)))

        exp_add_c = Signal.like(final_exponent)
        with m.Switch(fmt_sel):
            for fmt in [FPFormat.S, FPFormat.D]:
                with m.Case(fmt):
                    ftyp = _fmt_ftypes[fmt]

                    m.d.comb += exp_add_c.eq(
                        Mux(do_sqrt, ftyp.half_bias(),
                            ftyp.bias() + 1))

        with m.If(fmt_sel == FPFormat.S):
            m.d.comb += final_mantissa.eq(quotient)
        with m.Else():
            m.d.comb += final_mantissa.eq(Cat(Const(0, 1), quotient[:-1]))

        with m.If(start):
            m.d.sync += final_exponent.eq(exp_add_a + exp_add_b + exp_add_c)

        #
        # Normalization
        #

        pre_round_sign = Signal()
        pre_round_exponent = Signal(self.ftyp.exp)
        pre_round_mantissa = Signal(self.ftyp.man)
        pre_round_abs = Signal(64)
        round_sticky_bits = Signal(2)

        with m.Switch(fmt_sel):
            for fmt in [FPFormat.S, FPFormat.D]:
                with m.Case(fmt):
                    ftyp = _fmt_ftypes[fmt]

                    with m.If(final_mantissa[ftyp.man + ROUND_BITS]):
                        m.d.comb += [
                            pre_round_sign.eq(final_sign),
                            pre_round_exponent.eq(final_exponent),
                            pre_round_mantissa.eq(
                                final_mantissa[ROUND_BITS:ftyp.man +
                                               ROUND_BITS]),
                            round_sticky_bits.eq(
                                Cat(final_mantissa[:ROUND_BITS - 1] != 0,
                                    final_mantissa[ROUND_BITS - 1])),
                        ]

                    with m.Else():
                        m.d.comb += [
                            pre_round_sign.eq(final_sign),
                            pre_round_exponent.eq(final_exponent - 1),
                            pre_round_mantissa.eq(
                                final_mantissa[ROUND_BITS - 1:ftyp.man +
                                               ROUND_BITS - 1]),
                            round_sticky_bits.eq(
                                Cat(final_mantissa[:ROUND_BITS - 2] != 0,
                                    final_mantissa[ROUND_BITS - 2])),
                        ]

                    m.d.comb += pre_round_abs.eq(
                        Cat(pre_round_mantissa[:ftyp.man],
                            pre_round_exponent[:ftyp.exp]))

        #
        # Rounding
        #

        rounding = m.submodules.rounding = FPURounding(self.ftyp.exp +
                                                       self.ftyp.man)

        m.d.comb += [
            rounding.in_sign.eq(pre_round_sign),
            rounding.in_abs.eq(pre_round_abs),
            rounding.round_mode.eq(round_mode),
            rounding.round_sticky_bits.eq(round_sticky_bits),
        ]

        result = Signal(64)

        with m.Switch(fmt_sel):
            for fmt in [FPFormat.S, FPFormat.D]:
                with m.Case(fmt):
                    ftyp = _fmt_ftypes[fmt]

                    m.d.comb += result.eq(
                        Cat(rounding.rounded_abs[:ftyp.man + ftyp.exp],
                            rounding.out_sign))

        m.d.comb += self.out.eq(result)

        return m
