from amaranth import *
from enum import IntEnum

from room.consts import *
from room.utils import Pipe

from roomsoc.interconnect.stream import Valid, Decoupled


class FPUOperator(IntEnum):
    FMADD = 0
    FNMSUB = 1
    ADD = 2
    MUL = 3
    SGNJ = 4
    MINMAX = 5
    CMP = 6
    CLASSIFY = 7
    F2F = 8
    F2I = 9
    I2F = 10


class FPFormat(IntEnum):
    S = 0
    D = 1


class IntFormat(IntEnum):
    INT8 = 0
    INT16 = 1
    INT32 = 2
    INT64 = 3


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


class ClassMask(IntEnum):
    NEG_INF = (1 << 0)
    NEG_NORM = (1 << 1)
    NEG_SUBNORM = (1 << 2)
    NEG_ZERO = (1 << 3)
    POS_ZERO = (1 << 4)
    POS_SUBNORM = (1 << 5)
    POS_NORM = (1 << 6)
    POS_INF = (1 << 7)
    SNAN = (1 << 8)
    QNAN = (1 << 9)


class FPUInput(Record):

    def __init__(self, width, name=None, src_loc_at=0):
        super().__init__([
            ('fn', Shape.cast(FPUOperator).width),
            ('fn_mod', 1),
            ('rm', Shape.cast(RoundingMode).width),
            ('in1', width),
            ('in2', width),
            ('in3', width),
            ('src_fmt', Shape.cast(FPFormat).width),
            ('dst_fmt', Shape.cast(FPFormat).width),
            ('int_fmt', Shape.cast(IntFormat).width),
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

        self.inp = Valid(FPUInput, self.width)
        self.out = Valid(FPUResult, self.width)

    def elaborate(self, platform):
        m = Module()

        prec_bits = self.ftyp.man + 1
        exp_width = max(self.ftyp.exp + 2,
                        Shape.cast(range(2 * prec_bits + 3)).width)

        #
        # S1 - Input
        #

        s1_valid = Signal()
        s1_inp = FPUInput(self.width)

        inp_pipe = m.submodules.inp_pipe = Pipe(width=len(self.inp.bits),
                                                depth=self.latency - 2)
        m.d.comb += [
            inp_pipe.in_valid.eq(self.inp.valid),
            inp_pipe.in_data.eq(self.inp.bits),
            s1_valid.eq(inp_pipe.out.valid),
            s1_inp.eq(inp_pipe.out.bits),
        ]

        in1 = Record(self.ftyp.record_layout())
        in2 = Record(self.ftyp.record_layout())
        in3 = Record(self.ftyp.record_layout())

        info1 = Record(self.ftyp.INFO_LAYOUT)
        info2 = Record(self.ftyp.INFO_LAYOUT)
        info3 = Record(self.ftyp.INFO_LAYOUT)

        m.d.comb += [
            in1.eq(s1_inp.in1),
            in2.eq(s1_inp.in2),
            in3.eq(s1_inp.in3),
            in3.sign.eq(s1_inp.in3[-1] ^ s1_inp.fn_mod),
            info1.eq(self.ftyp.classify(in1)),
            info2.eq(self.ftyp.classify(in2)),
            info3.eq(self.ftyp.classify(in3)),
        ]

        with m.Switch(s1_inp.fn):
            with m.Case(FPUOperator.FNMSUB):
                m.d.comb += in1.sign.eq(~s1_inp.in1[-1])

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

        s1_eff_subtraction = in1.sign ^ in2.sign ^ in3.sign
        tentative_sign = in1.sign ^ in2.sign

        exp_addend = Signal(signed(exp_width))
        s1_exp_product = Signal(signed(exp_width))
        s1_exp_diff = Signal(signed(exp_width))
        s1_tentative_exp = Signal(signed(exp_width))

        s1_addend_shamt = Signal(range(3 * prec_bits + 5))
        with m.If(s1_exp_diff <= -2 * prec_bits - 1):
            m.d.comb += s1_addend_shamt.eq(3 * prec_bits + 4)
        with m.Elif(s1_exp_diff <= prec_bits + 2):
            m.d.comb += s1_addend_shamt.eq(prec_bits + 3 - s1_exp_diff)
        with m.Else():
            m.d.comb += s1_addend_shamt.eq(0)

        exp1 = Signal(signed(exp_width))
        exp2 = Signal(signed(exp_width))
        exp3 = Signal(signed(exp_width))

        m.d.comb += [
            exp1.eq(Cat(in1.exp, 0)),
            exp2.eq(Cat(in2.exp, 0)),
            exp3.eq(Cat(in3.exp, 0)),
            exp_addend.eq(exp3 + Cat(~info3.is_normal, 0).as_signed()),
            s1_exp_product.eq(
                Mux(
                    info1.is_zero | info2.is_zero, 2 - self.ftyp.bias(),
                    exp1 + info1.is_subnormal + exp2 + info2.is_subnormal -
                    self.ftyp.bias())),
            s1_exp_diff.eq(exp_addend - s1_exp_product),
            s1_tentative_exp.eq(
                Mux(s1_exp_diff > 0, exp_addend, s1_exp_product)),
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
        s1_sticky_before_add = (addend_sticky_bits != 0)

        m.d.comb += Cat(addend_sticky_bits, addend_shifted).eq(
            (man3 << (3 * prec_bits + 4)) >> s1_addend_shamt)

        sum_raw = Signal(3 * prec_bits + 5)
        s1_sum = Signal(3 * prec_bits + 4)

        m.d.comb += [
            sum_raw.eq(
                (product << 2) +
                Mux(s1_eff_subtraction, ~addend_shifted, addend_shifted) +
                (s1_eff_subtraction & (addend_sticky_bits != 0))),
            s1_sum.eq(Mux(s1_eff_subtraction & ~sum_raw[-1], -sum_raw,
                          sum_raw)),
        ]

        s1_final_sign = Mux(
            s1_eff_subtraction & (sum_raw[-1] == tentative_sign), 1,
            Mux(s1_eff_subtraction, 0, tentative_sign))

        #
        # S2 - Normalization & Rounding
        #

        s2_eff_subtraction = Signal.like(s1_eff_subtraction)
        s2_exp_product = Signal.like(s1_exp_product)
        s2_exp_diff = Signal.like(s1_exp_diff)
        s2_tentative_exp = Signal.like(s1_tentative_exp)
        s2_addend_shamt = Signal.like(s1_addend_shamt)
        s2_sticky_before_add = Signal.like(s1_sticky_before_add)
        s2_sum = Signal.like(s1_sum)
        s2_final_sign = Signal.like(s1_final_sign)
        s2_round_mode = Signal.like(s1_inp.rm)
        s2_valid = Signal()

        s2_pipe_in = Cat(s1_eff_subtraction, s1_exp_product, s1_exp_diff,
                         s1_tentative_exp, s1_addend_shamt,
                         s1_sticky_before_add, s1_sum, s1_final_sign,
                         s1_inp.rm)
        s2_pipe_out = Cat(s2_eff_subtraction, s2_exp_product, s2_exp_diff,
                          s2_tentative_exp, s2_addend_shamt,
                          s2_sticky_before_add, s2_sum, s2_final_sign,
                          s2_round_mode)

        s2_pipe = m.submodules.s2_pipe = Pipe(
            width=len(s2_pipe_in), depth=1 if self.latency > 1 else 0)

        m.d.comb += [
            s2_pipe.in_valid.eq(s1_valid),
            s2_pipe.in_data.eq(s2_pipe_in),
            s2_valid.eq(s2_pipe.out.valid),
            s2_pipe_out.eq(s2_pipe.out.bits),
        ]

        clz = Signal(range(2 * prec_bits + 4))
        clz_empty = Signal()

        m.d.comb += [
            clz.eq(2 * prec_bits + 3),
            clz_empty.eq(1),
        ]

        for i in range(2 * prec_bits + 3):
            with m.If(s2_sum[i]):
                m.d.comb += [
                    clz.eq(2 * prec_bits + 2 - i),
                    clz_empty.eq(0),
                ]

        norm_shamt = Signal.like(s2_addend_shamt)
        exp_normalized = Signal(self.ftyp.exp)

        with m.If((s2_exp_diff <= 0)
                  | (s2_eff_subtraction & (s2_exp_diff <= 2))):
            with m.If((s2_exp_product - Cat(clz, 0).as_signed() + 1 >= 0)
                      & ~clz_empty):
                m.d.comb += [
                    norm_shamt.eq(prec_bits + 2 + clz),
                    exp_normalized.eq(s2_exp_product -
                                      Cat(clz, 0).as_signed() + 1),
                ]
            with m.Else():
                m.d.comb += [
                    norm_shamt.eq(prec_bits + 2 + s2_exp_product),
                    exp_normalized.eq(0),
                ]
        with m.Else():
            m.d.comb += [
                norm_shamt.eq(s2_addend_shamt),
                exp_normalized.eq(s2_tentative_exp),
            ]

        sum_shifted = Signal.like(sum_raw)
        m.d.comb += sum_shifted.eq(s2_sum << norm_shamt)

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

        sticky_after_norm = (sum_sticky_bits != 0) | s2_sticky_before_add

        of_before_round = final_exponent >= (2**self.ftyp.exp - 1)
        uf_before_round = final_exponent == 0

        pre_round_sign = s2_final_sign
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
            rounding.round_mode.eq(s2_round_mode),
            rounding.round_sticky_bits.eq(round_sticky_bits),
            rounding.eff_subtraction.eq(s2_eff_subtraction),
        ]

        uf_after_round = rounding.rounded_abs[self.ftyp.man:self.ftyp.man +
                                              self.ftyp.exp] == 0
        of_after_round = rounding.rounded_abs[self.ftyp.man:self.ftyp.man +
                                              self.ftyp.exp] == Repl(
                                                  1, self.ftyp.exp)

        regular_result = Cat(rounding.rounded_abs, rounding.out_sign)

        result = regular_result

        #
        # S3 - Output
        #

        out_pipe = m.submodules.out_pipe = Pipe(width=len(result))
        m.d.comb += [
            out_pipe.in_valid.eq(s2_valid),
            out_pipe.in_data.eq(result),
            self.out.valid.eq(out_pipe.out.valid),
            self.out.bits.data.eq(out_pipe.out.bits),
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

        self.out = Decoupled(Signal, 64)

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
            for fmt in FPFormat:
                with m.Case(fmt.value):
                    ftyp = _fmt_ftypes[fmt.value]

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
                for fmt in FPFormat:
                    with m.Case(fmt.value):
                        ftyp = _fmt_ftypes[fmt.value]

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
            for fmt in FPFormat:
                with m.Case(fmt.value):
                    ftyp = _fmt_ftypes[fmt.value]

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

        m.d.comb += self.in_ready.eq(~start & (count == 0) & ~self.out.valid)

        with m.If(self.kill):
            m.d.sync += self.out.valid.eq(0)
        with m.Elif(count == max_iters):
            m.d.sync += self.out.valid.eq(1)
        with m.Elif(self.out.fire):
            m.d.sync += self.out.valid.eq(0)

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
            for fmt in FPFormat:
                with m.Case(fmt.value):
                    ftyp = _fmt_ftypes[fmt.value]

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
            for fmt in FPFormat:
                with m.Case(fmt.value):
                    ftyp = _fmt_ftypes[fmt.value]

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
            for fmt in FPFormat:
                with m.Case(fmt.value):
                    ftyp = _fmt_ftypes[fmt.value]

                    m.d.comb += result.eq(
                        Cat(rounding.rounded_abs[:ftyp.man + ftyp.exp],
                            rounding.out_sign))

        m.d.comb += self.out.bits.eq(result)

        return m


class FPUCastMulti(Elaboratable):

    def __init__(self, latency=3):
        self.width = 64
        self.ftyp = FType.FP64
        self.latency = latency

        self.inp = Valid(FPUInput, self.width)
        self.out = Valid(FPUResult, self.width)

    def elaborate(self, platform):
        m = Module()

        MAN_WIDTH = max(self.ftyp.man + 1, self.width)
        EXP_WIDTH = max(
            Shape.cast(range(self.width)).width,
            max(self.ftyp.exp,
                Shape.cast(range(self.ftyp.bias() + self.ftyp.man)).width)) + 1

        #
        # S1 - Input
        #

        s1_valid = Signal()
        s1_inp = FPUInput(self.width)

        inp_pipe = m.submodules.inp_pipe = Pipe(width=len(self.inp.bits))
        m.d.comb += [
            inp_pipe.in_valid.eq(self.inp.valid),
            inp_pipe.in_data.eq(self.inp.bits),
            s1_valid.eq(inp_pipe.out.valid),
            s1_inp.eq(inp_pipe.out.bits),
        ]

        src_is_int = s1_inp.fn == FPUOperator.I2F
        dst_is_int = s1_inp.fn == FPUOperator.F2I

        fmt_sign = Signal()
        fmt_exponent = Signal(signed(EXP_WIDTH))
        fmt_mantissa = Signal(MAN_WIDTH)
        fmt_offset = Signal(signed(EXP_WIDTH))
        fmt_bias = Signal(signed(EXP_WIDTH))
        fmt_subnormal = Signal(signed(EXP_WIDTH))
        fmt_info = Record(self.ftyp.INFO_LAYOUT)

        with m.Switch(s1_inp.src_fmt):
            for fmt in FPFormat:
                with m.Case(fmt.value):
                    ftyp = _fmt_ftypes[fmt.value]

                    m.d.comb += [
                        fmt_info.eq(ftyp.classify(s1_inp.in1)),
                        fmt_sign.eq(s1_inp.in1[ftyp.exp +
                                               ftyp.man].as_signed()),
                        fmt_exponent.eq(
                            Cat(s1_inp.in1[ftyp.man:ftyp.man + ftyp.exp], 0)),
                        fmt_mantissa.eq(
                            Cat(s1_inp.in1[:ftyp.man], fmt_info.is_normal)),
                        fmt_offset.eq(MAN_WIDTH - 1 - ftyp.man),
                        fmt_bias.eq(ftyp.bias()),
                        fmt_subnormal.eq(Cat(fmt_info.is_subnormal, 0)),
                    ]

        int_sign = Signal()
        int_value = Signal(MAN_WIDTH)
        int_mantissa = Signal(MAN_WIDTH)

        with m.Switch(s1_inp.int_fmt):
            for fmt in IntFormat:
                with m.Case(fmt.value):
                    int_width = 1 << (fmt.value + 3)

                    m.d.comb += [
                        int_value.eq(
                            Repl(s1_inp.in1[int_width - 1] & ~s1_inp.fn_mod,
                                 len(int_value))),
                        int_value[:int_width].eq(s1_inp.in1[:int_width]),
                    ]

        m.d.comb += [
            int_sign.eq(int_value[-1] & ~s1_inp.fn_mod),
            int_mantissa.eq(
                Mux(int_sign, (-int_value).as_unsigned(), int_value)),
        ]

        encoded_mant = Mux(src_is_int, int_mantissa, fmt_mantissa)

        renorm_shamt = Signal(range(MAN_WIDTH + 1))
        mant_is_zero = Signal()

        m.d.comb += [
            renorm_shamt.eq(MAN_WIDTH),
            mant_is_zero.eq(1),
        ]

        for i in range(MAN_WIDTH):
            with m.If(encoded_mant[i]):
                m.d.comb += [
                    renorm_shamt.eq(MAN_WIDTH - 1 - i),
                    mant_is_zero.eq(0),
                ]

        renorm_shamt_se = Signal(signed(len(renorm_shamt) + 1))
        m.d.comb += renorm_shamt_se.eq(Cat(renorm_shamt, 0))

        input_sign = Mux(src_is_int, int_sign, fmt_sign)
        input_mant = (encoded_mant << renorm_shamt)[:MAN_WIDTH]

        fp_input_exp = Signal(signed(EXP_WIDTH))
        int_input_exp = Signal(signed(EXP_WIDTH))
        m.d.comb += [
            fp_input_exp.eq(fmt_exponent + fmt_subnormal - fmt_bias -
                            renorm_shamt_se + fmt_offset),
            int_input_exp.eq(MAN_WIDTH - 1 - renorm_shamt_se),
        ]

        input_exp = Signal(signed(EXP_WIDTH))
        m.d.comb += input_exp.eq(Mux(src_is_int, int_input_exp, fp_input_exp))

        dst_exp = Signal(signed(EXP_WIDTH))
        with m.Switch(s1_inp.dst_fmt):
            for fmt in FPFormat:
                with m.Case(fmt.value):
                    ftyp = _fmt_ftypes[fmt.value]
                    m.d.comb += dst_exp.eq(input_exp + ftyp.bias())

        s2_input_sign = Signal.like(input_sign)
        s2_input_exp = Signal.like(input_exp)
        s2_input_mant = Signal.like(input_mant)
        s2_src_info = Record(self.ftyp.INFO_LAYOUT)
        s2_dst_exp = Signal.like(dst_exp)
        s2_inp = FPUInput(self.width)
        s2_src_is_int = Signal()
        s2_dst_is_int = Signal()
        s2_mant_is_zero = Signal()
        s2_valid = Signal()

        s2_pipe_in = Cat(input_sign, input_exp, input_mant, fmt_info, dst_exp,
                         s1_inp, src_is_int, dst_is_int, mant_is_zero)
        s2_pipe_out = Cat(s2_input_sign, s2_input_exp, s2_input_mant,
                          s2_src_info, s2_dst_exp, s2_inp, s2_src_is_int,
                          s2_dst_is_int, s2_mant_is_zero)

        s2_pipe = m.submodules.s2_pipe = Pipe(
            width=len(s2_pipe_in), depth=1 if self.latency > 1 else 0)

        m.d.comb += [
            s2_pipe.in_valid.eq(s1_valid),
            s2_pipe.in_data.eq(s2_pipe_in),
            s2_valid.eq(s2_pipe.out.valid),
            s2_pipe_out.eq(s2_pipe.out.bits),
        ]

        dst_exp_bits = Signal(range(self.width))
        dst_man_bits = Signal(range(self.width))

        with m.Switch(s2_inp.dst_fmt):
            for fmt in FPFormat:
                with m.Case(fmt.value):
                    ftyp = _fmt_ftypes[fmt.value]

                    m.d.comb += [
                        dst_exp_bits.eq(ftyp.exp),
                        dst_man_bits.eq(ftyp.man),
                    ]

        final_exp = Signal(EXP_WIDTH)
        preshift_mant = Signal(2 * MAN_WIDTH + 1)
        dst_mant = Signal(2 * MAN_WIDTH + 1)
        final_mant = Signal(self.ftyp.man)
        final_int = Signal(self.width)

        fp_round_sticky_bits = Signal(2)
        int_round_sticky_bits = Signal(2)

        of_before_round = Signal()
        uf_before_round = Signal()

        denorm_shamt = Signal(range(MAN_WIDTH + 1))

        m.d.comb += [
            final_exp.eq(s2_dst_exp),
            denorm_shamt.eq(self.ftyp.man - dst_man_bits),
            preshift_mant.eq(s2_input_mant << (MAN_WIDTH + 1)),
        ]

        with m.If(s2_dst_is_int):
            m.d.comb += denorm_shamt.eq(self.width - 1 - s2_input_exp)

            with m.If(s2_input_exp >= (1 << (s2_inp.int_fmt + 3)) - 1 +
                      s2_inp.fn_mod):
                m.d.comb += [
                    denorm_shamt.eq(0),
                    of_before_round.eq(1),
                ]
            with m.Elif(s2_input_exp < -1):
                m.d.comb += [
                    denorm_shamt.eq(self.width + 1),
                    uf_before_round.eq(1),
                ]

        m.d.comb += [
            dst_mant.eq(preshift_mant >> denorm_shamt),
            Cat(fp_round_sticky_bits[1],
                final_mant).eq(dst_mant[-self.ftyp.man - 2:-1]),
            Cat(int_round_sticky_bits[1],
                final_int).eq(dst_mant[-self.width - 1:]),
            fp_round_sticky_bits[0].eq(dst_mant[:-self.ftyp.man - 2] != 0),
            int_round_sticky_bits[0].eq(dst_mant[:-self.width - 1] != 0),
        ]

        round_sticky_bits = Mux(s2_dst_is_int, int_round_sticky_bits,
                                fp_round_sticky_bits)

        fmt_pre_round_abs = Signal(self.width)
        ifmt_pre_round_abs = Signal(self.width)

        with m.Switch(s2_inp.dst_fmt):
            for fmt in FPFormat:
                with m.Case(fmt.value):
                    ftyp = _fmt_ftypes[fmt.value]

                    m.d.comb += fmt_pre_round_abs.eq(
                        Cat(final_mant[:ftyp.man], final_exp[:ftyp.exp]))

        with m.Switch(s2_inp.int_fmt):
            for fmt in IntFormat:
                with m.Case(fmt.value):
                    width = 1 << (fmt.value + 3)

                    m.d.comb += [
                        ifmt_pre_round_abs.eq(
                            Repl(final_int[width - 1],
                                 len(ifmt_pre_round_abs))),
                        ifmt_pre_round_abs[:width].eq(final_int[:width]),
                    ]

        pre_round_abs = Mux(s2_dst_is_int, ifmt_pre_round_abs,
                            fmt_pre_round_abs)

        #
        # Rounding
        #

        rounding = m.submodules.rounding = FPURounding(self.ftyp.exp +
                                                       self.ftyp.man + 1)

        m.d.comb += [
            rounding.in_sign.eq(s2_input_sign),
            rounding.in_abs.eq(pre_round_abs),
            rounding.round_mode.eq(s2_inp.rm),
            rounding.round_sticky_bits.eq(round_sticky_bits),
        ]

        fmt_result = Signal(self.width)
        m.d.comb += fmt_result.eq(Repl(1, len(fmt_result)))

        with m.Switch(s2_inp.dst_fmt):
            for fmt in FPFormat:
                with m.Case(fmt.value):
                    ftyp = _fmt_ftypes[fmt.value]

                    m.d.comb += fmt_result[:ftyp.man + ftyp.exp + 1].eq(
                        Mux(
                            s2_src_is_int & s2_mant_is_zero, 0,
                            Cat(rounding.rounded_abs[:ftyp.man + ftyp.exp],
                                rounding.out_sign)))

        rounded_int_res = Mux(rounding.out_sign,
                              (-rounding.rounded_abs).as_unsigned(),
                              rounding.rounded_abs)

        ifmt_special_result = Signal(self.width)
        with m.Switch(s2_inp.int_fmt):
            for fmt in IntFormat:
                with m.Case(fmt.value):
                    width = 1 << (fmt.value + 3)

                    special_result = Signal(width)

                    m.d.comb += [
                        special_result[:width - 1].eq(Repl(1, width - 1)),
                        special_result[width - 1].eq(s2_inp.fn_mod),
                    ]

                    neg_result = s2_input_sign & ~s2_src_info.is_nan

                    m.d.comb += [
                        ifmt_special_result.eq(
                            Repl(neg_result ^ special_result[-1],
                                 len(ifmt_special_result))),
                        ifmt_special_result[:width].eq(
                            Mux(neg_result, ~special_result, special_result)),
                    ]

        int_result_is_special = of_before_round | (s2_input_sign
                                                   & s2_inp.fn_mod &
                                                   (rounded_int_res != 0))

        fp_result = fmt_result
        int_result = Mux(int_result_is_special, ifmt_special_result,
                         rounded_int_res)

        result = Mux(s2_dst_is_int, int_result, fp_result)

        out_pipe = m.submodules.out_pipe = Pipe(width=len(result),
                                                depth=self.latency - 2)
        m.d.comb += [
            out_pipe.in_valid.eq(s2_valid),
            out_pipe.in_data.eq(result),
            self.out.valid.eq(out_pipe.out.valid),
            self.out.bits.data.eq(out_pipe.out.bits),
        ]

        return m


class FPUComp(Elaboratable):

    def __init__(self, width, format, latency=3):
        self.width = width
        self.ftyp = _fmt_ftypes[format]
        self.latency = latency

        self.inp = Valid(FPUInput, self.width)
        self.out = Valid(FPUResult, self.width)

    def elaborate(self, platform):
        m = Module()

        #
        # S1 - Input
        #

        s1_valid = Signal()
        s1_inp = FPUInput(self.width)

        inp_pipe = m.submodules.inp_pipe = Pipe(width=len(self.inp.bits))
        m.d.comb += [
            inp_pipe.in_valid.eq(self.inp.valid),
            inp_pipe.in_data.eq(self.inp.bits),
            s1_valid.eq(inp_pipe.out.valid),
            s1_inp.eq(inp_pipe.out.bits),
        ]

        in1 = Record(self.ftyp.record_layout())
        in2 = Record(self.ftyp.record_layout())

        info1 = Record(self.ftyp.INFO_LAYOUT)
        info2 = Record(self.ftyp.INFO_LAYOUT)

        m.d.comb += [
            in1.eq(s1_inp.in1),
            in2.eq(s1_inp.in2),
            info1.eq(self.ftyp.classify(in1)),
            info2.eq(self.ftyp.classify(in2)),
        ]

        in_equal = (in1 == in2) | (info1.is_zero & info2.is_zero)
        in1_smaller = (in1 < in2) ^ (in1.sign | in2.sign)

        #
        # Sign injection
        #

        sgnj_result = Record(self.ftyp.record_layout())

        m.d.comb += sgnj_result.eq(in1)
        with m.Switch(s1_inp.rm):
            with m.Case(RoundingMode.RNE):
                m.d.comb += sgnj_result.sign.eq(in2.sign)

            with m.Case(RoundingMode.RTZ):
                m.d.comb += sgnj_result.sign.eq(~in2.sign)

            with m.Case(RoundingMode.RDN):
                m.d.comb += sgnj_result.sign.eq(in1.sign ^ in2.sign)

            with m.Case(RoundingMode.RUP):
                m.d.comb += sgnj_result.sign.eq(in1.sign)

        #
        # Min/Max
        #

        minmax_result = Record(self.ftyp.record_layout())

        with m.If(info1.is_nan & info2.is_nan):
            m.d.comb += [
                minmax_result.sign.eq(0),
                minmax_result.exp.eq(~0),
                minmax_result.man.eq(2**(self.ftyp.man - 1)),
            ]

        with m.Elif(info1.is_nan):
            m.d.comb += minmax_result.eq(in1)

        with m.Elif(info2.is_nan):
            m.d.comb += minmax_result.eq(in2)

        with m.Else():
            with m.Switch(s1_inp.rm):
                with m.Case(RoundingMode.RNE):
                    m.d.comb += minmax_result.eq(Mux(in1_smaller, in1, in2))

                with m.Case(RoundingMode.RTZ):
                    m.d.comb += minmax_result.eq(Mux(in1_smaller, in2, in1))

        #
        # Comparison
        #

        cmp_result = Record(self.ftyp.record_layout())

        with m.Switch(s1_inp.rm):
            with m.Case(RoundingMode.RNE):
                m.d.comb += cmp_result.eq((in1_smaller | in_equal)
                                          ^ s1_inp.fn_mod)

            with m.Case(RoundingMode.RTZ):
                m.d.comb += cmp_result.eq((in1_smaller & ~in_equal)
                                          ^ s1_inp.fn_mod)

            with m.Case(RoundingMode.RDN):
                m.d.comb += cmp_result.eq(in_equal ^ s1_inp.fn_mod)

        #
        # Classification
        #

        class_result = Record(self.ftyp.record_layout())

        with m.If(info1.is_normal):
            m.d.comb += class_result.eq(
                Mux(in1.sign, ClassMask.NEG_NORM, ClassMask.POS_NORM))

        with m.Elif(info1.is_subnormal):
            m.d.comb += class_result.eq(
                Mux(in1.sign, ClassMask.NEG_SUBNORM, ClassMask.POS_SUBNORM))

        with m.Elif(info1.is_zero):
            m.d.comb += class_result.eq(
                Mux(in1.sign, ClassMask.NEG_ZERO, ClassMask.POS_ZERO))

        with m.Elif(info1.is_inf):
            m.d.comb += class_result.eq(
                Mux(in1.sign, ClassMask.NEG_INF, ClassMask.POS_INF))

        with m.Elif(info1.is_nan):
            m.d.comb += class_result.eq(
                Mux(info1.is_snan, ClassMask.SNAN, ClassMask.QNAN))

        result = Record(self.ftyp.record_layout())

        with m.Switch(s1_inp.fn):
            with m.Case(FPUOperator.SGNJ):
                m.d.comb += result.eq(sgnj_result)

            with m.Case(FPUOperator.MINMAX):
                m.d.comb += result.eq(minmax_result)

            with m.Case(FPUOperator.CMP):
                m.d.comb += result.eq(cmp_result)

            with m.Case(FPUOperator.CLASSIFY):
                m.d.comb += result.eq(class_result)

        out_pipe = m.submodules.out_pipe = Pipe(width=len(result),
                                                depth=self.latency - 1)
        m.d.comb += [
            out_pipe.in_valid.eq(s1_valid),
            out_pipe.in_data.eq(result),
            self.out.valid.eq(out_pipe.out.valid),
            self.out.bits.data.eq(out_pipe.out.bits),
        ]

        return m
