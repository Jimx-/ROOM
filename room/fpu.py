from amaranth import *
from amaranth.lib.coding import PriorityEncoder
from amaranth.hdl.rec import Direction
from enum import IntEnum

from room.consts import *
from room.types import HasCoreParams
from room.utils import Pipe, sign_extend

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
    REC = 11


class FPFormat(IntEnum):
    S = 0
    D = 1
    H = 2


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

    @property
    def width(self):
        return 1 + self.exp + self.man

    def bias(self):
        return (2**(self.exp - 1)) - 1

    def half_bias(self):
        return (2**(self.exp - 2)) - 1

    def default_nan(self):
        return Cat(Const(0, self.man - 1),
                   Const(1, 1).replicate(self.exp + 1), Const(0, 1))

    def inf(self, sign):
        return Cat(Const(0, self.man), Const(1, 1).replicate(self.exp), sign)

    def zero(self, sign):
        return Cat(Const(0, self.man + self.exp), sign)

    def greatest_finite(self, sign):
        return Cat(
            Const(1, 1).replicate(self.man), Const(0, 1),
            Const(1, 1).replicate(self.exp - 1), sign)

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


FType.FP16 = FType(5, 11)
FType.FP32 = FType(8, 23)
FType.FP64 = FType(11, 52)

_fmt_ftypes = {
    FPFormat.H: FType.FP16,
    FPFormat.S: FType.FP32,
    FPFormat.D: FType.FP64,
}


class HasFPUParams(HasCoreParams):

    def __init__(self, params, *args, **kwargs):
        super().__init__(params)

        self.min_flen = 32
        self.float_types = Array(
            (fmt, typ) for fmt, typ in _fmt_ftypes.items()
            if typ.width >= self.min_flen and typ.width <= self.flen)
        self.max_type = self.float_types[-1][1]

        self.type_tag = IntEnum(
            'type_tag', {
                'H': self._type_to_tag(FType.FP16),
                'S': self._type_to_tag(FType.FP32),
                'D': self._type_to_tag(FType.FP64),
                'I': self._type_to_tag(self.max_type),
            })

    def _type_to_tag(self, typ):
        for i, (_, ftyp) in enumerate(self.float_types):
            if typ is ftyp:
                return i
        return len(self.float_types) - 1

    def tag_to_format(self, tag):
        return self.float_types[tag][0]

    def _nan_box(self, x, from_typ, to_typ):
        return x | ((1 << to_typ.width) - (1 << from_typ.width))

    def nan_box(self, x, tag):
        return Mux(tag == len(self.float_types) - 1, x,
                   self._nan_box(x, self.float_types[tag][1], self.max_type))


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


class FPException(Record):

    def __init__(self, name=None, src_loc_at=0):
        super().__init__([
            ('nx', 1),
            ('uf', 1),
            ('of', 1),
            ('dz', 1),
            ('nv', 1),
        ],
                         name=name,
                         src_loc_at=1 + src_loc_at)


class FPUInput(Record):

    def __init__(self, width, name=None, src_loc_at=0):
        super().__init__([
            ('fn', FPUOperator),
            ('fn_mod', 1),
            ('rm', RoundingMode),
            ('in1', width),
            ('in2', width),
            ('in3', width),
            ('src_fmt', FPFormat),
            ('dst_fmt', FPFormat),
            ('int_fmt', IntFormat),
        ],
                         name=name,
                         src_loc_at=1 + src_loc_at)


class FPUResult(Record):

    def __init__(self, width, name=None, src_loc_at=0):
        super().__init__([
            ('data', width, Direction.FANOUT),
            ('status', 5, Direction.FANOUT),
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
            self.out_sign.eq(
                Mux(
                    self.eff_subtraction & ~self.in_abs.any()
                    & ~self.round_sticky_bits.any(),
                    self.round_mode == RoundingMode.RDN, self.in_sign)),
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
                    in3.sign.eq(s1_inp.rm != RoundingMode.RDN),
                    in3.exp.eq(0),
                    in3.man.eq(0),
                    info3.is_zero.eq(1),
                ]

        s1_eff_subtraction = in1.sign ^ in2.sign ^ in3.sign
        tentative_sign = in1.sign ^ in2.sign

        #
        # Special case
        #

        s1_special_result = Record(self.ftyp.record_layout())
        s1_special_status = FPException()
        s1_result_is_special = Signal()
        m.d.comb += s1_special_result.eq(self.ftyp.default_nan())

        with m.If((info1.is_inf & info2.is_zero)
                  | (info1.is_zero & info2.is_inf)):
            m.d.comb += [
                s1_result_is_special.eq(1),
                s1_special_status.nv.eq(1),
            ]
        with m.Elif(info1.is_nan | info2.is_nan | info3.is_nan):
            m.d.comb += [
                s1_result_is_special.eq(1),
                s1_special_status.nv.eq(info1.is_snan | info2.is_snan
                                        | info3.is_snan),
            ]
        with m.Elif(info1.is_inf | info2.is_inf | info3.is_inf):
            m.d.comb += s1_result_is_special.eq(1)
            with m.If((info1.is_inf | info2.is_inf) & info3.is_inf
                      & s1_eff_subtraction):
                m.d.comb += s1_special_status.nv.eq(1)
            with m.Elif(info1.is_inf | info2.is_inf):
                m.d.comb += s1_special_result.eq(
                    self.ftyp.inf(in1.sign ^ in2.sign))
            with m.Elif(info3.is_inf):
                m.d.comb += s1_special_result.eq(self.ftyp.inf(in3.sign))

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
        s1_sticky_before_add = addend_sticky_bits.any()

        m.d.comb += Cat(addend_sticky_bits, addend_shifted).eq(
            (man3 << (3 * prec_bits + 4)) >> s1_addend_shamt)

        sum_raw = Signal(3 * prec_bits + 5)
        s1_sum = Signal(3 * prec_bits + 4)

        m.d.comb += [
            sum_raw.eq(
                (product << 2) +
                Mux(s1_eff_subtraction, ~addend_shifted, addend_shifted) +
                (s1_eff_subtraction & ~s1_sticky_before_add)),
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
        s2_special_result = Signal.like(s1_special_result)
        s2_special_status = Signal.like(s1_special_status)
        s2_result_is_special = Signal()
        s2_valid = Signal()

        s2_pipe_in = Cat(s1_eff_subtraction, s1_exp_product, s1_exp_diff,
                         s1_tentative_exp, s1_addend_shamt,
                         s1_sticky_before_add, s1_sum, s1_final_sign,
                         s1_inp.rm, s1_special_result, s1_special_status,
                         s1_result_is_special)
        s2_pipe_out = Cat(s2_eff_subtraction, s2_exp_product, s2_exp_diff,
                          s2_tentative_exp, s2_addend_shamt,
                          s2_sticky_before_add, s2_sum, s2_final_sign,
                          s2_round_mode, s2_special_result, s2_special_status,
                          s2_result_is_special)

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
        exp_normalized = Signal(signed(exp_width))

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
        final_exponent = Signal(signed(exp_width))
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
        with m.Else():
            m.d.comb += final_exponent.eq(0)

        sticky_after_norm = (sum_sticky_bits != 0) | s2_sticky_before_add

        of_before_round = final_exponent >= (2**self.ftyp.exp - 1)

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
        regular_status = FPException()
        m.d.comb += [
            regular_status.of.eq(of_before_round | of_after_round),
            regular_status.uf.eq(uf_after_round & regular_status.nx),
            regular_status.nx.eq(round_sticky_bits.any() | of_before_round
                                 | of_after_round),
        ]

        result = Mux(s2_result_is_special, s2_special_result, regular_result)
        status = Mux(s2_result_is_special, s2_special_status, regular_status)

        #
        # S3 - Output
        #

        out_pipe = m.submodules.out_pipe = Pipe(width=len(result) +
                                                len(status))
        m.d.comb += [
            out_pipe.in_valid.eq(s2_valid),
            out_pipe.in_data.eq(Cat(result, status)),
            self.out.valid.eq(out_pipe.out.valid),
            Cat(self.out.bits.data,
                self.out.bits.status).eq(out_pipe.out.bits),
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

        self.out = Decoupled(FPUResult, 64)

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
            in_b.eq(Mux(self.is_sqrt, 0, self.b)),
            in_info_a.eq(self.ftyp.classify(self.a)),
            in_info_b.eq(Mux(self.is_sqrt, 0, self.ftyp.classify(self.b))),
        ]

        with m.Switch(self.fmt):
            for fmt in FPFormat:
                with m.Case(fmt.value):
                    ftyp = _fmt_ftypes[fmt.value]

                    m.d.comb += [
                        in_info_a.eq(ftyp.classify(self.a)),
                        in_info_b.eq(
                            Mux(self.is_sqrt, 0, ftyp.classify(self.b))),
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
        fmt_sel = Signal(FPFormat)
        round_mode = Signal.like(self.rm)
        final_sign = Signal()

        quotient = Signal(PREC_BITS + 1)  # 59 bits
        remainder = Signal(PREC_BITS + 2)  # 60 bits

        m.d.sync += start.eq(self.in_valid & self.in_ready)

        #
        # Pre-normalization
        #

        man_a_pre_norm = Signal(self.ftyp.man + 1)  # With implied bit
        man_b_pre_norm = Signal(self.ftyp.man + 1)

        clz_a = m.submodules.clz_a = PriorityEncoder(self.ftyp.man + 1)
        m.d.comb += clz_a.i.eq(man_a_pre_norm[::-1])

        clz_b = m.submodules.clz_b = PriorityEncoder(self.ftyp.man + 1)
        m.d.comb += clz_b.i.eq(man_b_pre_norm[::-1])

        with m.If(self.in_valid & self.in_ready):
            m.d.sync += [
                info_a.eq(in_info_a),
                info_b.eq(in_info_b),
                do_sqrt.eq(self.is_sqrt),
                fmt_sel.eq(self.fmt),
                round_mode.eq(self.rm),
                quotient.eq(0),
                remainder.eq(0),
            ]

            with m.Switch(self.fmt):
                for fmt in FPFormat:
                    with m.Case(fmt.value):
                        ftyp = _fmt_ftypes[fmt.value]

                        sign_a = in_a[ftyp.man + ftyp.exp]
                        sign_b = in_b[ftyp.man + ftyp.exp]

                        m.d.comb += [
                            man_a_pre_norm.eq(
                                Cat(Const(0, self.ftyp.man - ftyp.man),
                                    in_a[:ftyp.man], in_info_a.is_normal)),
                            man_b_pre_norm.eq(
                                Cat(Const(0, self.ftyp.man - ftyp.man),
                                    in_b[:ftyp.man], in_info_b.is_normal)),
                        ]

                        exp_a_fmt = in_a[ftyp.man:ftyp.man + ftyp.exp]
                        exp_b_fmt = in_b[ftyp.man:ftyp.man + ftyp.exp]

                        m.d.sync += [
                            exp_a.eq(exp_a_fmt - clz_a.o + clz_a.o.any()),
                            exp_b.eq(exp_b_fmt - clz_b.o + clz_b.o.any()),
                            man_a.eq(man_a_pre_norm << clz_a.o),
                            man_b.eq(man_b_pre_norm << clz_b.o),
                        ]

                        with m.If(self.is_sqrt):
                            m.d.sync += final_sign.eq(sign_a)
                        with m.Else():
                            m.d.sync += final_sign.eq(sign_a ^ sign_b)

        m.d.comb += man_sqrt.eq(
            Mux(exp_a[0], Cat(man_a[:-1], 0), Cat(0, man_a[:-1])))

        with m.Switch(fmt_sel):
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
        do_adjust = count == max_iters

        with m.Switch(fmt_sel):
            for fmt in [FPFormat.S, FPFormat.D]:
                with m.Case(fmt):
                    m.d.comb += max_iters.eq(
                        N_ITERS[fmt] + 1
                    )  # +1 cycle to adjust remainder for detecting inexact

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

        with m.If(do_adjust):
            # FSQRT adjust: if R is negative, R = R + ((Q << 1) | 1)
            m.d.comb += sqrt_Q[0].eq(Cat(Const(1, 1), quotient))

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

        # Normal: R = 2 * R - D
        # Adjust: If R is negative, R = R + D
        m.d.comb += [
            adder_in_div_a[0].eq(
                Mux(
                    start, recode_vec(man_a, 1),
                    recode_vec(
                        Mux(do_adjust, remainder[ROUND_BITS:],
                            remainder[ROUND_BITS - 1:]),
                        quotient[0] & ~do_adjust))),
            adder_in_div_b[0].eq(
                Mux((start | quotient[0]) & ~do_adjust,
                    recode_vec(man_b_inv, 1), Cat(Const(0, ROUND_BITS),
                                                  man_b))),
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

            with m.If(do_adjust):
                m.d.sync += remainder.eq(
                    Mux(remainder[-1], adder_out[0], remainder))
            with m.Else():
                m.d.sync += [
                    quotient.eq(Cat(*reversed(adder_carry), quotient[:-2])),
                    remainder.eq(Mux(do_sqrt, sqrt_R, adder_out[-1])),
                ]

        tentative_exponent = Signal(self.ftyp.exp + 2)

        exp_add_a = Mux(do_sqrt, Cat(exp_a[1:], Repl(exp_a[-1], 3)),
                        Cat(exp_a, Repl(exp_a[-1], 2)))
        exp_add_b = Mux(do_sqrt, Cat(exp_a[0], Repl(0, self.ftyp.exp + 1)),
                        ~Cat(exp_b, Repl(exp_b[-1], 2)))

        exp_add_c = Signal.like(tentative_exponent)
        with m.Switch(fmt_sel):
            for fmt in FPFormat:
                with m.Case(fmt.value):
                    ftyp = _fmt_ftypes[fmt.value]

                    m.d.comb += exp_add_c.eq(
                        Mux(do_sqrt, ftyp.half_bias(),
                            ftyp.bias() + 1))

        with m.If(start):
            m.d.sync += tentative_exponent.eq(exp_add_a + exp_add_b +
                                              exp_add_c)

        final_quotient = Signal.like(quotient)
        sticky_before_norm = Signal()
        with m.Switch(fmt_sel):
            for fmt in FPFormat:
                with m.Case(fmt.value):
                    ftyp = _fmt_ftypes[fmt.value]

                    m.d.comb += sticky_before_norm.eq(
                        Mux(do_sqrt, remainder[:ftyp.man + 1].any(),
                            remainder[-(ftyp.man + 1):].any()))

                    if fmt.value == FPFormat.S:
                        m.d.comb += final_quotient.eq(quotient[:ftyp.man +
                                                               ROUND_BITS + 1])
                    else:
                        m.d.comb += final_quotient.eq(
                            Cat(Const(0, 1), quotient[:-1]))

        #
        # Normalization
        #

        norm_shamt = Signal(self.ftyp.exp + 2)
        exp_normalized = Signal(self.ftyp.exp + 2)
        with m.If(tentative_exponent[-1] | ~tentative_exponent.any()):
            m.d.comb += [
                norm_shamt.eq(~tentative_exponent + 2),
                exp_normalized.eq(0),
            ]
        with m.Else():
            m.d.comb += exp_normalized.eq(tentative_exponent)

        quotient_shifted = Signal(PREC_BITS + self.ftyp.man + 1)
        m.d.comb += quotient_shifted.eq(
            Cat(Const(0, self.ftyp.man), final_quotient) >> norm_shamt)

        final_mantissa = Signal(PREC_BITS + 1)
        final_exponent = Signal(self.ftyp.exp + 2)
        quotient_sticky_bits = Signal(PREC_BITS - 1)

        m.d.comb += [
            Cat(quotient_sticky_bits, final_mantissa).eq(quotient_shifted),
            final_exponent.eq(exp_normalized),
        ]

        with m.Switch(fmt_sel):
            for fmt in FPFormat:
                with m.Case(fmt.value):
                    ftyp = _fmt_ftypes[fmt.value]

                    with m.If(quotient_shifted[ftyp.man + PREC_BITS]):
                        pass
                    with m.Elif(exp_normalized > 1):
                        m.d.comb += [
                            Cat(quotient_sticky_bits,
                                final_mantissa).eq(quotient_shifted << 1),
                            final_exponent.eq(exp_normalized - 1),
                        ]
                    with m.Else():
                        m.d.comb += final_exponent.eq(0)

        sticky_after_norm = quotient_sticky_bits.any() | sticky_before_norm

        of_before_round = Signal()
        of_after_round = Signal()

        pre_round_sign = Signal()
        pre_round_exponent = Signal(self.ftyp.exp)
        pre_round_mantissa = Signal(self.ftyp.man)
        pre_round_abs = Signal(64)
        round_sticky_bits = Signal(2)

        m.d.comb += [
            pre_round_sign.eq(final_sign),
            round_sticky_bits.eq(
                Mux(of_before_round, 0b11,
                    Cat(sticky_after_norm, final_mantissa[0]))),
        ]

        with m.Switch(fmt_sel):
            for fmt in FPFormat:
                with m.Case(fmt.value):
                    ftyp = _fmt_ftypes[fmt.value]

                    m.d.comb += of_before_round.eq(
                        final_exponent >= (2**ftyp.exp - 1))

                    m.d.comb += [
                        pre_round_exponent.eq(
                            Mux(of_before_round, 2**ftyp.exp - 2,
                                final_exponent[:ftyp.exp])),
                        pre_round_mantissa.eq(
                            Mux(of_before_round,
                                Const(1, 1).replicate(ftyp.man),
                                final_mantissa[1:ftyp.man + 1])),
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

        uf_after_round = Signal()
        with m.Switch(fmt_sel):
            for fmt in FPFormat:
                with m.Case(fmt.value):
                    ftyp = _fmt_ftypes[fmt.value]

                    m.d.comb += [
                        uf_after_round.eq(
                            rounding.rounded_abs[ftyp.man:ftyp.man +
                                                 ftyp.exp] == 0),
                        of_after_round.eq(
                            rounding.rounded_abs[ftyp.man:ftyp.man + ftyp.exp]
                            == Const(1, 1).replicate(ftyp.exp)),
                    ]

        #
        # Special case
        #

        special_result = Signal(64)
        special_status = FPException()
        result_is_special = Signal()

        with m.Switch(fmt_sel):
            for fmt in FPFormat:
                with m.Case(fmt.value):
                    ftyp = _fmt_ftypes[fmt.value]

                    m.d.comb += special_result.eq(ftyp.default_nan())

                    with m.If(info_a.is_nan | info_b.is_nan):
                        m.d.comb += [
                            result_is_special.eq(1),
                            special_status.nv.eq(info_a.is_snan
                                                 | info_b.is_snan),
                        ]

                    with m.Elif(info_a.is_inf):
                        m.d.comb += result_is_special.eq(1)

                        with m.If(~do_sqrt & info_b.is_inf):
                            m.d.comb += special_status.nv.eq(1)
                        with m.Elif(do_sqrt & final_sign):  # sqrt(-inf)
                            m.d.comb += special_status.nv.eq(1)
                        with m.Else():
                            m.d.comb += special_result.eq(ftyp.inf(final_sign))

                    with m.Elif(info_b.is_inf):
                        m.d.comb += [
                            result_is_special.eq(1),
                            special_result.eq(ftyp.zero(final_sign)),
                        ]

                    with m.Elif(~do_sqrt & info_b.is_zero):  # Divide by zero
                        m.d.comb += result_is_special.eq(1)

                        with m.If(info_a.is_zero):
                            m.d.comb += special_status.nv.eq(1)
                        with m.Else():
                            m.d.comb += [
                                special_result.eq(ftyp.inf(final_sign)),
                                special_status.dz.eq(1),
                            ]

                    with m.Elif(info_a.is_zero):  # Zero result
                        m.d.comb += [
                            result_is_special.eq(1),
                            special_result.eq(ftyp.zero(final_sign)),
                        ]

                    with m.Elif(do_sqrt & final_sign):  # sqrt(-a)
                        m.d.comb += [
                            result_is_special.eq(1),
                            special_status.nv.eq(1),
                        ]

        regular_result = Signal(64)
        regular_status = FPException()

        with m.Switch(fmt_sel):
            for fmt in FPFormat:
                with m.Case(fmt.value):
                    ftyp = _fmt_ftypes[fmt.value]

                    m.d.comb += [
                        regular_result.eq(
                            Cat(rounding.rounded_abs[:ftyp.man + ftyp.exp],
                                rounding.out_sign)),
                        regular_status.of.eq(of_before_round | of_after_round),
                        regular_status.uf.eq(uf_after_round
                                             & regular_status.nx),
                        regular_status.nx.eq(round_sticky_bits.any()
                                             | of_before_round
                                             | of_after_round),
                    ]

        m.d.comb += [
            self.out.bits.data.eq(
                Mux(result_is_special, special_result, regular_result)),
            self.out.bits.status.eq(
                Mux(result_is_special, special_status, regular_status)),
        ]

        return m


class FPUCastMulti(Elaboratable):

    def __init__(self, latency=3):
        if latency < 1:
            raise ValueError('Latency should be at least 1 cycle')

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

        inp_pipe = m.submodules.inp_pipe = Pipe(width=len(self.inp.bits),
                                                depth=int(self.latency > 2))
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
        fmt_is_boxed = Signal()

        m.d.comb += fmt_is_boxed.eq(1)
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

                    if ftyp.width < self.width:
                        m.d.comb += fmt_is_boxed.eq(
                            s1_inp.in1[ftyp.width:self.width].all())

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
        s2_is_boxed = Signal()
        s2_valid = Signal()

        s2_pipe_in = Cat(input_sign, input_exp, input_mant, fmt_info, dst_exp,
                         s1_inp, src_is_int, dst_is_int, mant_is_zero,
                         fmt_is_boxed)
        s2_pipe_out = Cat(s2_input_sign, s2_input_exp, s2_input_mant,
                          s2_src_info, s2_dst_exp, s2_inp, s2_src_is_int,
                          s2_dst_is_int, s2_mant_is_zero, s2_is_boxed)

        s2_pipe = m.submodules.s2_pipe = Pipe(width=len(s2_pipe_in),
                                              depth=int(self.latency > 1))

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

            with m.If((s2_input_exp >= (1 << (s2_inp.int_fmt + 3)) - 1 +
                       s2_inp.fn_mod)
                      & ~(~s2_inp.fn_mod & s2_input_sign & (
                          (s2_input_exp == (1 << (s2_inp.int_fmt + 3)) - 1))
                          & s2_input_mant[-1] & ~s2_input_mant[:-1].any())):
                m.d.comb += [
                    denorm_shamt.eq(0),
                    of_before_round.eq(1),
                ]
            with m.Elif(s2_input_exp < -1):
                m.d.comb += [
                    denorm_shamt.eq(self.width + 1),
                    uf_before_round.eq(1),
                ]

        with m.Else():
            with m.Switch(s2_inp.dst_fmt):
                for fmt in FPFormat:
                    with m.Case(fmt.value):
                        ftyp = _fmt_ftypes[fmt.value]

                        with m.If(~s2_src_is_int & s2_src_info.is_inf):
                            m.d.comb += [
                                final_exp.eq(Const(1, 1).replicate(ftyp.exp)),
                                preshift_mant.eq(0),
                            ]

                        with m.Elif(s2_dst_exp >= 2**ftyp.exp - 1):
                            m.d.comb += [
                                final_exp.eq(2**ftyp.exp - 2),
                                preshift_mant.eq(~0),
                                of_before_round.eq(1),
                            ]

                        with m.Elif((s2_dst_exp < 1)
                                    & (s2_dst_exp >= -ftyp.man)):
                            m.d.comb += [
                                final_exp.eq(0),
                                denorm_shamt.eq(self.ftyp.man - dst_man_bits +
                                                1 - s2_dst_exp),
                                uf_before_round.eq(1),
                            ]

                        with m.Elif(s2_dst_exp < -ftyp.man):
                            m.d.comb += [
                                final_exp.eq(0),
                                denorm_shamt.eq(self.ftyp.man - dst_man_bits +
                                                2 + ftyp.man),
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
        fmt_special_result = Signal(self.width)
        fmt_of_after_round = Signal()
        fmt_uf_after_round = Signal()
        m.d.comb += [
            fmt_result.eq(~0),
            fmt_special_result.eq(~0),
        ]
        with m.Switch(s2_inp.dst_fmt):
            for fmt in FPFormat:
                with m.Case(fmt.value):
                    ftyp = _fmt_ftypes[fmt.value]

                    m.d.comb += [
                        fmt_result[:ftyp.width].eq(
                            Mux(
                                s2_src_is_int & s2_mant_is_zero, 0,
                                Cat(rounding.rounded_abs[:ftyp.man + ftyp.exp],
                                    rounding.out_sign))),
                        fmt_special_result[:ftyp.width].eq(
                            Mux(s2_src_info.is_zero & s2_is_boxed,
                                ftyp.zero(s2_input_sign), ftyp.default_nan())),
                        fmt_of_after_round.eq(
                            rounding.rounded_abs[ftyp.man:ftyp.man +
                                                 ftyp.exp].all()),
                        fmt_uf_after_round.eq(
                            ~rounding.rounded_abs[ftyp.man:ftyp.man +
                                                  ftyp.exp].any()),
                    ]

        rounded_uint_res = Mux(rounding.out_sign,
                               (-rounding.rounded_abs).as_unsigned(),
                               rounding.rounded_abs)[:rounding.abs_width]

        rounded_int_res = Signal.like(rounded_uint_res)
        ifmt_special_result = Signal(self.width)
        ifmt_of_after_round = Signal()
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
                        rounded_int_res.eq(
                            sign_extend(rounded_uint_res[:width],
                                        len(rounded_int_res))),
                        ifmt_special_result.eq(
                            Repl(neg_result ^ special_result[-1],
                                 len(ifmt_special_result))),
                        ifmt_special_result[:width].eq(
                            Mux(neg_result, ~special_result, special_result)),
                    ]

                    with m.If(~rounding.out_sign
                              & (s2_input_exp == width - 2 + s2_inp.fn_mod)):
                        m.d.comb += ifmt_of_after_round.eq(
                            ~rounded_int_res.bit_select(
                                width - 2 + s2_inp.fn_mod, 1))

        uf_after_round = fmt_uf_after_round
        of_after_round = Mux(s2_dst_is_int, ifmt_of_after_round,
                             fmt_of_after_round)

        fp_result_is_special = ~s2_src_is_int & (s2_src_info.is_zero
                                                 | s2_src_info.is_nan
                                                 | ~s2_is_boxed)
        int_result_is_special = s2_src_info.is_nan | s2_src_info.is_inf | of_before_round | (
            s2_input_sign
            & s2_inp.fn_mod & (rounded_int_res != 0))

        fp_result = Mux(fp_result_is_special, fmt_special_result, fmt_result)
        int_result = Mux(int_result_is_special, ifmt_special_result,
                         rounded_int_res)

        fp_regular_status = FPException()
        fp_special_status = FPException()
        fp_status = Mux(fp_result_is_special, fp_special_status,
                        fp_regular_status)
        m.d.comb += [
            fp_regular_status.of.eq((s2_src_is_int | ~s2_src_info.is_inf)
                                    & (of_before_round | of_after_round)),
            fp_regular_status.uf.eq(uf_after_round & fp_regular_status.nx),
            fp_regular_status.nx.eq(fp_round_sticky_bits.any()
                                    | (s2_src_is_int | ~s2_src_info.is_inf)
                                    & (of_before_round | of_after_round)),
            fp_special_status.nv.eq(s2_src_info.is_snan),
        ]

        int_status = FPException()
        m.d.comb += [
            int_status.nx.eq(~int_result_is_special
                             & int_round_sticky_bits.any()),
            int_status.nv.eq(int_result_is_special),
        ]

        result = Mux(s2_dst_is_int, int_result, fp_result)
        status = Mux(s2_dst_is_int, int_status, fp_status)

        out_pipe = m.submodules.out_pipe = Pipe(width=len(result) +
                                                len(status),
                                                depth=max(self.latency - 2, 1))
        m.d.comb += [
            out_pipe.in_valid.eq(s2_valid),
            out_pipe.in_data.eq(Cat(result, status)),
            self.out.valid.eq(out_pipe.out.valid),
            Cat(self.out.bits.data,
                self.out.bits.status).eq(out_pipe.out.bits),
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
        minmax_status = FPException()
        m.d.comb += minmax_status.nv.eq(info1.is_snan | info2.is_snan)

        with m.If(info1.is_nan & info2.is_nan):
            m.d.comb += [
                minmax_result.sign.eq(0),
                minmax_result.exp.eq(~0),
                minmax_result.man.eq(2**(self.ftyp.man - 1)),
            ]

        with m.Elif(info1.is_nan):
            m.d.comb += minmax_result.eq(in2)

        with m.Elif(info2.is_nan):
            m.d.comb += minmax_result.eq(in1)

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
        cmp_status = FPException()

        with m.If(info1.is_snan | info2.is_snan):
            m.d.comb += [
                cmp_status.nv.eq(1),
                cmp_result.eq((s1_inp.rm == RoundingMode.RDN) & s1_inp.fn_mod),
            ]

        with m.Else():
            with m.Switch(s1_inp.rm):
                with m.Case(RoundingMode.RNE):
                    with m.If(info1.is_nan | info2.is_nan):
                        m.d.comb += cmp_status.nv.eq(1)
                    with m.Else():
                        m.d.comb += cmp_result.eq((in1_smaller | in_equal)
                                                  ^ s1_inp.fn_mod)

                with m.Case(RoundingMode.RTZ):
                    with m.If(info1.is_nan | info2.is_nan):
                        m.d.comb += cmp_status.nv.eq(1)
                    with m.Else():
                        m.d.comb += cmp_result.eq((in1_smaller & ~in_equal)
                                                  ^ s1_inp.fn_mod)

                with m.Case(RoundingMode.RDN):
                    with m.If(info1.is_nan | info2.is_nan):
                        m.d.comb += cmp_result.eq(s1_inp.fn_mod)
                    with m.Else():
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
        status = FPException()

        with m.Switch(s1_inp.fn):
            with m.Case(FPUOperator.SGNJ):
                m.d.comb += result.eq(sgnj_result)

            with m.Case(FPUOperator.MINMAX):
                m.d.comb += [
                    result.eq(minmax_result),
                    status.eq(minmax_status),
                ]

            with m.Case(FPUOperator.CMP):
                m.d.comb += [
                    result.eq(cmp_result),
                    status.eq(cmp_status),
                ]

            with m.Case(FPUOperator.CLASSIFY):
                m.d.comb += result.eq(class_result)

        out_pipe = m.submodules.out_pipe = Pipe(width=len(result) +
                                                len(status),
                                                depth=self.latency - 1)
        m.d.comb += [
            out_pipe.in_valid.eq(s1_valid),
            out_pipe.in_data.eq(Cat(result, status)),
            self.out.valid.eq(out_pipe.out.valid),
            Cat(self.out.bits.data,
                self.out.bits.status).eq(out_pipe.out.bits),
        ]

        return m


class FPURec(Elaboratable):
    _RSQRT_TABLE = [
        # exp[0] = 0
        52,  # man[-6:] = 0
        51,  # man[-6:] = 1
        50,  # man[-6:] = 2
        48,  # man[-6:] = 3
        47,  # man[-6:] = 4
        46,  # man[-6:] = 5
        44,  # man[-6:] = 6
        43,  # man[-6:] = 7
        42,  # man[-6:] = 8
        41,  # man[-6:] = 9
        40,  # man[-6:] = 10
        39,  # man[-6:] = 11
        38,  # man[-6:] = 12
        36,  # man[-6:] = 13
        35,  # man[-6:] = 14
        34,  # man[-6:] = 15
        33,  # man[-6:] = 16
        32,  # man[-6:] = 17
        31,  # man[-6:] = 18
        30,  # man[-6:] = 19
        30,  # man[-6:] = 20
        29,  # man[-6:] = 21
        28,  # man[-6:] = 22
        27,  # man[-6:] = 23
        26,  # man[-6:] = 24
        25,  # man[-6:] = 25
        24,  # man[-6:] = 26
        23,  # man[-6:] = 27
        23,  # man[-6:] = 28
        22,  # man[-6:] = 29
        21,  # man[-6:] = 30
        20,  # man[-6:] = 31
        19,  # man[-6:] = 32
        19,  # man[-6:] = 33
        18,  # man[-6:] = 34
        17,  # man[-6:] = 35
        16,  # man[-6:] = 36
        16,  # man[-6:] = 37
        15,  # man[-6:] = 38
        14,  # man[-6:] = 39
        14,  # man[-6:] = 40
        13,  # man[-6:] = 41
        12,  # man[-6:] = 42
        12,  # man[-6:] = 43
        11,  # man[-6:] = 44
        10,  # man[-6:] = 45
        10,  # man[-6:] = 46
        9,  # man[-6:] = 47
        9,  # man[-6:] = 48
        8,  # man[-6:] = 49
        7,  # man[-6:] = 50
        7,  # man[-6:] = 51
        6,  # man[-6:] = 52
        6,  # man[-6:] = 53
        5,  # man[-6:] = 54
        4,  # man[-6:] = 55
        4,  # man[-6:] = 56
        3,  # man[-6:] = 57
        3,  # man[-6:] = 58
        2,  # man[-6:] = 59
        2,  # man[-6:] = 60
        1,  # man[-6:] = 61
        1,  # man[-6:] = 62
        0,  # man[-6:] = 63
        # exp[0] = 1
        127,  # man[-6:] = 0
        125,  # man[-6:] = 1
        123,  # man[-6:] = 2
        121,  # man[-6:] = 3
        119,  # man[-6:] = 4
        118,  # man[-6:] = 5
        116,  # man[-6:] = 6
        114,  # man[-6:] = 7
        113,  # man[-6:] = 8
        111,  # man[-6:] = 9
        109,  # man[-6:] = 10
        108,  # man[-6:] = 11
        106,  # man[-6:] = 12
        105,  # man[-6:] = 13
        103,  # man[-6:] = 14
        102,  # man[-6:] = 15
        100,  # man[-6:] = 16
        99,  # man[-6:] = 17
        97,  # man[-6:] = 18
        96,  # man[-6:] = 19
        95,  # man[-6:] = 20
        93,  # man[-6:] = 21
        92,  # man[-6:] = 22
        91,  # man[-6:] = 23
        90,  # man[-6:] = 24
        88,  # man[-6:] = 25
        87,  # man[-6:] = 26
        86,  # man[-6:] = 27
        85,  # man[-6:] = 28
        84,  # man[-6:] = 29
        83,  # man[-6:] = 30
        82,  # man[-6:] = 31
        80,  # man[-6:] = 32
        79,  # man[-6:] = 33
        78,  # man[-6:] = 34
        77,  # man[-6:] = 35
        76,  # man[-6:] = 36
        75,  # man[-6:] = 37
        74,  # man[-6:] = 38
        73,  # man[-6:] = 39
        72,  # man[-6:] = 40
        71,  # man[-6:] = 41
        70,  # man[-6:] = 42
        70,  # man[-6:] = 43
        69,  # man[-6:] = 44
        68,  # man[-6:] = 45
        67,  # man[-6:] = 46
        66,  # man[-6:] = 47
        65,  # man[-6:] = 48
        64,  # man[-6:] = 49
        63,  # man[-6:] = 50
        63,  # man[-6:] = 51
        62,  # man[-6:] = 52
        61,  # man[-6:] = 53
        60,  # man[-6:] = 54
        59,  # man[-6:] = 55
        59,  # man[-6:] = 56
        58,  # man[-6:] = 57
        57,  # man[-6:] = 58
        56,  # man[-6:] = 59
        56,  # man[-6:] = 60
        55,  # man[-6:] = 61
        54,  # man[-6:] = 62
        53,  # man[-6:] = 63
    ]

    _REC_TABLE = [
        127,  # man[-7:] = 0
        125,  # man[-7:] = 1
        123,  # man[-7:] = 2
        121,  # man[-7:] = 3
        119,  # man[-7:] = 4
        117,  # man[-7:] = 5
        116,  # man[-7:] = 6
        114,  # man[-7:] = 7
        112,  # man[-7:] = 8
        110,  # man[-7:] = 9
        109,  # man[-7:] = 10
        107,  # man[-7:] = 11
        105,  # man[-7:] = 12
        104,  # man[-7:] = 13
        102,  # man[-7:] = 14
        100,  # man[-7:] = 15
        99,  # man[-7:] = 16
        97,  # man[-7:] = 17
        96,  # man[-7:] = 18
        94,  # man[-7:] = 19
        93,  # man[-7:] = 20
        91,  # man[-7:] = 21
        90,  # man[-7:] = 22
        88,  # man[-7:] = 23
        87,  # man[-7:] = 24
        85,  # man[-7:] = 25
        84,  # man[-7:] = 26
        83,  # man[-7:] = 27
        81,  # man[-7:] = 28
        80,  # man[-7:] = 29
        79,  # man[-7:] = 30
        77,  # man[-7:] = 31
        76,  # man[-7:] = 32
        75,  # man[-7:] = 33
        74,  # man[-7:] = 34
        72,  # man[-7:] = 35
        71,  # man[-7:] = 36
        70,  # man[-7:] = 37
        69,  # man[-7:] = 38
        68,  # man[-7:] = 39
        66,  # man[-7:] = 40
        65,  # man[-7:] = 41
        64,  # man[-7:] = 42
        63,  # man[-7:] = 43
        62,  # man[-7:] = 44
        61,  # man[-7:] = 45
        60,  # man[-7:] = 46
        59,  # man[-7:] = 47
        58,  # man[-7:] = 48
        57,  # man[-7:] = 49
        56,  # man[-7:] = 50
        55,  # man[-7:] = 51
        54,  # man[-7:] = 52
        53,  # man[-7:] = 53
        52,  # man[-7:] = 54
        51,  # man[-7:] = 55
        50,  # man[-7:] = 56
        49,  # man[-7:] = 57
        48,  # man[-7:] = 58
        47,  # man[-7:] = 59
        46,  # man[-7:] = 60
        45,  # man[-7:] = 61
        44,  # man[-7:] = 62
        43,  # man[-7:] = 63
        42,  # man[-7:] = 64
        41,  # man[-7:] = 65
        40,  # man[-7:] = 66
        40,  # man[-7:] = 67
        39,  # man[-7:] = 68
        38,  # man[-7:] = 69
        37,  # man[-7:] = 70
        36,  # man[-7:] = 71
        35,  # man[-7:] = 72
        35,  # man[-7:] = 73
        34,  # man[-7:] = 74
        33,  # man[-7:] = 75
        32,  # man[-7:] = 76
        31,  # man[-7:] = 77
        31,  # man[-7:] = 78
        30,  # man[-7:] = 79
        29,  # man[-7:] = 80
        28,  # man[-7:] = 81
        28,  # man[-7:] = 82
        27,  # man[-7:] = 83
        26,  # man[-7:] = 84
        25,  # man[-7:] = 85
        25,  # man[-7:] = 86
        24,  # man[-7:] = 87
        23,  # man[-7:] = 88
        23,  # man[-7:] = 89
        22,  # man[-7:] = 90
        21,  # man[-7:] = 91
        21,  # man[-7:] = 92
        20,  # man[-7:] = 93
        19,  # man[-7:] = 94
        19,  # man[-7:] = 95
        18,  # man[-7:] = 96
        17,  # man[-7:] = 97
        17,  # man[-7:] = 98
        16,  # man[-7:] = 99
        15,  # man[-7:] = 100
        15,  # man[-7:] = 101
        14,  # man[-7:] = 102
        14,  # man[-7:] = 103
        13,  # man[-7:] = 104
        12,  # man[-7:] = 105
        12,  # man[-7:] = 106
        11,  # man[-7:] = 107
        11,  # man[-7:] = 108
        10,  # man[-7:] = 109
        9,  # man[-7:] = 110
        9,  # man[-7:] = 111
        8,  # man[-7:] = 112
        8,  # man[-7:] = 113
        7,  # man[-7:] = 114
        7,  # man[-7:] = 115
        6,  # man[-7:] = 116
        5,  # man[-7:] = 117
        5,  # man[-7:] = 118
        4,  # man[-7:] = 119
        4,  # man[-7:] = 120
        3,  # man[-7:] = 121
        3,  # man[-7:] = 122
        2,  # man[-7:] = 123
        2,  # man[-7:] = 124
        1,  # man[-7:] = 125
        1,  # man[-7:] = 126
        0,  # man[-7:] = 127
    ]

    def __init__(self, width, format, latency=3):
        if latency < 1:
            raise ValueError('Latency should be at least 1 cycle')

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

        inp_pipe = m.submodules.inp_pipe = Pipe(width=len(self.inp.bits),
                                                depth=int(self.latency > 2))
        m.d.comb += [
            inp_pipe.in_valid.eq(self.inp.valid),
            inp_pipe.in_data.eq(self.inp.bits),
            s1_valid.eq(inp_pipe.out.valid),
            s1_inp.eq(inp_pipe.out.bits),
        ]

        s1_in1 = Record(self.ftyp.record_layout())
        s1_info1 = Record(self.ftyp.INFO_LAYOUT)
        m.d.comb += [
            s1_in1.eq(s1_inp.in1),
            s1_info1.eq(self.ftyp.classify(s1_in1)),
        ]

        s1_rm = s1_inp.rm
        s1_is_rsqrt = s1_inp.fn_mod

        #
        # Normalization
        #

        clz_pe = m.submodules.clz_pe = PriorityEncoder(self.ftyp.man)
        m.d.comb += clz_pe.i.eq(s1_in1.man)

        norm_exponent = Signal(self.ftyp.exp + 1)
        norm_mantissa = Signal(self.ftyp.man)
        with m.If(s1_info1.is_subnormal):
            m.d.comb += [
                norm_exponent.eq(-clz_pe.o),
                norm_mantissa.eq(s1_in1.man << (1 + clz_pe.o)),
            ]
        with m.Else():
            m.d.comb += [
                norm_exponent.eq(s1_in1.exp),
                norm_mantissa.eq(s1_in1.man),
            ]

        rsqrt_table = Memory(width=7,
                             depth=len(FPURec._RSQRT_TABLE),
                             init=FPURec._RSQRT_TABLE)
        rsqrt_rd = m.submodules.rsqrt_rd = rsqrt_table.read_port()
        m.d.comb += rsqrt_rd.addr.eq(Cat(norm_mantissa[-6:], norm_exponent[0]))

        rec_table = Memory(width=7,
                           depth=len(FPURec._REC_TABLE),
                           init=FPURec._REC_TABLE)
        rec_rd = m.submodules.rec_rd = rec_table.read_port()
        m.d.comb += rec_rd.addr.eq(Cat(norm_mantissa[-7:]))

        #
        # S2
        #

        s2_valid = Signal()
        s2_in1 = Record(self.ftyp.record_layout())
        s2_rm = Signal(RoundingMode)
        s2_is_rsqrt = Signal()
        s2_info1 = Record(self.ftyp.INFO_LAYOUT)
        s2_norm_exponent = Signal(self.ftyp.exp + 1)
        s2_norm_mantissa = Signal(self.ftyp.man)
        m.d.sync += [
            s2_valid.eq(s1_valid),
            s2_in1.eq(s1_in1),
            s2_rm.eq(s1_rm),
            s2_is_rsqrt.eq(s1_is_rsqrt),
            s2_info1.eq(s1_info1),
            s2_norm_exponent.eq(norm_exponent),
            s2_norm_mantissa.eq(norm_mantissa),
        ]

        s2_exponent = Signal(self.ftyp.exp)
        s2_norm_out = Record(self.ftyp.record_layout())
        with m.If(s2_is_rsqrt):
            m.d.comb += [
                s2_exponent.eq((3 * self.ftyp.bias() - 1 -
                                s2_norm_exponent) >> 1),
                s2_norm_out.sign.eq(s2_in1.sign),
                s2_norm_out.exp.eq(s2_exponent),
                s2_norm_out.man[-7:].eq(rsqrt_rd.data),
            ]

        with m.Else():
            m.d.comb += [
                s2_exponent.eq(2 * self.ftyp.bias() - 1 - s2_norm_exponent),
                s2_norm_out.sign.eq(s2_in1.sign),
                s2_norm_out.exp.eq(s2_exponent),
                s2_norm_out.man[-7:].eq(rec_rd.data),
            ]

        result = Record(self.ftyp.record_layout())
        with m.If(s2_is_rsqrt):
            with m.If(s2_in1.sign & (
                (~s2_info1.is_nan & ~s2_info1.is_zero) | s2_info1.is_inf)):
                # -inf <= x < -0.0, canonical NaN, NV
                m.d.comb += result.eq(self.ftyp.default_nan())
            with m.Elif(s2_in1.sign & s2_info1.is_zero):
                # -0.0, -inf, DZ
                m.d.comb += result.eq(self.ftyp.inf(1))
            with m.Elif(~s2_in1.sign & s2_info1.is_zero):
                # +0.0, +inf, DZ
                m.d.comb += result.eq(self.ftyp.inf(0))
            with m.Elif(~s2_in1.sign & (
                (~s2_info1.is_nan & ~s2_info1.is_zero) | s2_info1.is_inf)):
                # +0.0 < x < +inf, estimate
                m.d.comb += result.eq(s2_norm_out)
            with m.Elif(~s2_in1.sign & s2_info1.is_inf):
                # +inf, +0.0
                m.d.comb += result.eq(self.ftyp.zero(0))
            with m.Elif(s2_info1.is_qnan):
                # qNaN, canonical NaN
                m.d.comb += result.eq(self.ftyp.default_nan())
            with m.Elif(s2_info1.is_snan):
                # sNaN, canonical NaN, NV
                m.d.comb += result.eq(self.ftyp.default_nan())

        with m.Else():
            m.d.comb += result.eq(s2_norm_out)
            with m.If(s2_info1.is_inf):
                # +-inf, +-0.0
                m.d.comb += result.eq(self.ftyp.zero(s2_in1.sign))
            with m.Elif(s2_in1.exp[2:].all() & s2_in1.exp[1] & ~s2_in1.exp[0]):
                # 2^B <= x < 2^(B+1) (normal), 2^-B > y >= 2^-(B+1) (subnormal, sig=01...)
                m.d.comb += result.eq(
                    Cat(s2_norm_out.man >> 2, Const(1, 2), s2_norm_out.exp,
                        s2_norm_out.sign))
            with m.Elif(s2_in1.exp[2:].all() & ~s2_in1.exp[1] & s2_in1.exp[0]):
                # 2^(B-1) <= x < 2^B (normal), 2^-(B-1) > y >= 2^-B (subnormal, sig=1...)
                m.d.comb += result.eq(
                    Cat(s2_norm_out.man >> 1, Const(1, 1), s2_norm_out.exp,
                        s2_norm_out.sign))
            with m.If(s2_in1.sign & s2_info1.is_subnormal
                      & ~s2_in1.man[-2:].any()
                      & ((s2_rm == RoundingMode.RTZ)
                         | (s2_rm == RoundingMode.RUP))):
                # -2^-(B+1) < x < -0.0 (subnormal, sig=00...), {RUP, RTZ}, greatest-mag. negative finite value, {NX, OF}
                m.d.comb += result.eq(self.ftyp.greatest_finite(1))
            with m.If(s2_in1.sign & s2_info1.is_subnormal
                      & ~s2_in1.man[-2:].any()
                      & ((s2_rm == RoundingMode.RDN)
                         | (s2_rm == RoundingMode.RNE)
                         | (s2_rm == RoundingMode.RMM))):
                # -2^-(B+1) < x < -0.0 (subnormal, sig=00...), {RDN, RNE, RMM}, -inf, {NX, OF}
                m.d.comb += result.eq(self.ftyp.inf(1))
            with m.If(~s2_in1.sign & s2_info1.is_subnormal
                      & ~s2_in1.man[-2:].any()
                      & ((s2_rm == RoundingMode.RTZ)
                         | (s2_rm == RoundingMode.RUP))):
                # +0.0 < x < 2^-(B+1) (subnormal, sig=00...), {RUP, RTZ}, greatest finite value, {NX, OF}
                m.d.comb += result.eq(self.ftyp.greatest_finite(0))
            with m.If(~s2_in1.sign & s2_info1.is_subnormal
                      & ~s2_in1.man[-2:].any()
                      & ((s2_rm == RoundingMode.RDN)
                         | (s2_rm == RoundingMode.RNE)
                         | (s2_rm == RoundingMode.RMM))):
                # +0.0 < x < 2^-(B+1) (subnormal, sig=00...), {RDN, RNE, RMM}, +inf, {NX, OF}
                m.d.comb += result.eq(self.ftyp.inf(0))
            with m.If(s2_info1.is_zero):
                # +-0.0, +-inf, DZ
                m.d.comb += result.eq(self.ftyp.inf(s2_in1.sign))
            with m.If(s2_info1.is_qnan):
                # qNaN, canonical NaN
                m.d.comb += result.eq(self.ftyp.default_nan())
            with m.If(s2_info1.is_snan):
                # sNaN, canonical NaN, NV
                m.d.comb += result.eq(self.ftyp.default_nan())

        #
        # S3 - Output
        #

        out_pipe = m.submodules.out_pipe = Pipe(
            width=len(result),
            depth=0 if self.latency == 1 else max(self.latency - 2, 1))
        m.d.comb += [
            out_pipe.in_valid.eq(s2_valid),
            out_pipe.in_data.eq(result),
            self.out.valid.eq(out_pipe.out.valid),
            self.out.bits.data.eq(out_pipe.out.bits),
        ]

        return m
