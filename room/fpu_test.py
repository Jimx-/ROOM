import struct

from room.consts import RoundingMode
from room.fpu import FPUOperator, FPFormat, IntFormat, FPUFMA, FPUDivSqrtMulti, FPUCastMulti
from room.test import run_test


def convert_float(f, typ, flen):
    return int(struct.pack('>' + typ, f).hex(), base=16) & ((1 << flen) - 1)


def mask_int(data, fmt):
    return data & ((1 << (2**(fmt + 3))) - 1)


def fma_unittest(fma, a, b, c, fn, fn_mod, expected):

    def proc():
        yield fma.inp.bits.in1.eq(a)
        yield fma.inp.bits.in2.eq(b)
        yield fma.inp.bits.in3.eq(c)
        yield fma.inp.bits.fn.eq(fn)
        yield fma.inp.bits.fn_mod.eq(fn_mod)
        yield fma.inp.valid.eq(1)
        yield
        yield fma.inp.valid.eq(0)

        for _ in range(3):
            yield

        assert (yield fma.out.valid)
        out = yield fma.out.bits.data
        assert out == expected

    return proc


def test_fma_add_d():
    dut = FPUFMA(64, FPFormat.D)

    cases = [
        # fadd
        (2.5, 0.0, 1.0, 3.5, FPUOperator.ADD, 0),
        (-1235.1, 0.0, 1.1, -1234, FPUOperator.ADD, 0),
        (3.14159265, 0.0, 0.00000001, 3.14159266, FPUOperator.ADD, 0),
        # fsub
        (2.5, 0.0, 1.0, 1.5, FPUOperator.ADD, 1),
        (-1235.1, 0.0, -1.1, -1234, FPUOperator.ADD, 1),
        (3.14159265, 0.0, 0.00000001, 3.1415926400000001, FPUOperator.ADD, 1),
        # fmul
        (2.5, 1.0, 0.0, 2.5, FPUOperator.MUL, 0),
        (-1235.1, -1.1, 0.0, 1358.61, FPUOperator.MUL, 0),
        (3.14159265, 0.00000001, 0.0, 3.14159265e-8, FPUOperator.MUL, 0),
    ]

    for a, b, c, expected, fn, fn_mod in cases:
        a = convert_float(a, 'd', 64)
        b = convert_float(b, 'd', 64)
        c = convert_float(c, 'd', 64)
        expected = convert_float(expected, 'd', 64)

        run_test(dut,
                 fma_unittest(dut, a, b, c, fn, fn_mod, expected),
                 sync=True)


def fdiv_unittest(fdiv, a, b, is_sqrt, fmt, expected):

    def proc():
        yield fdiv.a.eq(a)
        yield fdiv.b.eq(b)
        yield fdiv.is_sqrt.eq(is_sqrt)
        yield fdiv.fmt.eq(fmt)
        yield fdiv.in_valid.eq(1)
        yield
        yield fdiv.in_valid.eq(0)

        while not (yield fdiv.out.valid):
            yield

        out = yield fdiv.out.bits
        assert out == expected

    return proc


def test_fdiv_d():
    dut = FPUDivSqrtMulti()

    cases = [
        # fdiv
        (3.14159265, 2.71828182, 1.1557273520668288, False),
        (-1234, 1235.1, -0.9991093838555584, False),
        (3.14159265, 1.0, 3.14159265, False),
        # fsqrt
        (3.14159265, 0.0, 1.7724538498928541, True),
        (10000, 0.0, 100, True),
        (171.0, 0.0, 13.076696830622021, True),
        (1.60795e-7, 0, 0.00040099251863345283320230749702, True),
    ]

    for a, b, expected, is_sqrt in cases:
        a = convert_float(a, 'd', 64)
        b = convert_float(b, 'd', 64)
        expected = convert_float(expected, 'd', 64)

        run_test(dut,
                 fdiv_unittest(dut, a, b, is_sqrt, FPFormat.D, expected),
                 sync=True)


def test_fdiv_s():
    dut = FPUDivSqrtMulti()

    cases = [
        # fdiv
        (3.14159265, 2.71828182, 1.1557273520668288, False),
        (-1234, 1235.1, -0.9991093838555584, False),
        (3.14159265, 1.0, 3.14159265, False),
        # fsqrt
        (3.14159265, 0.0, 1.7724538498928541, True),
        (10000, 0.0, 100, True),
        (171.0, 0.0, 13.076696, True),
    ]

    for a, b, expected, is_sqrt in cases:
        a = convert_float(a, 'f', 32)
        b = convert_float(b, 'f', 32)
        expected = convert_float(expected, 'f', 32)

        run_test(dut,
                 fdiv_unittest(dut, a, b, is_sqrt, FPFormat.S, expected),
                 sync=True)


def fp_cast_unittest(dut,
                     a,
                     fn,
                     fn_mod,
                     src_fmt,
                     dst_fmt,
                     int_fmt,
                     expected,
                     rm=RoundingMode.RNE):

    def proc():
        yield dut.inp.bits.in1.eq(a)
        yield dut.inp.bits.fn.eq(fn)
        yield dut.inp.bits.fn_mod.eq(fn_mod)
        yield dut.inp.bits.rm.eq(rm)
        yield dut.inp.bits.src_fmt.eq(src_fmt)
        yield dut.inp.bits.dst_fmt.eq(dst_fmt)
        yield dut.inp.bits.int_fmt.eq(int_fmt)
        yield dut.inp.valid.eq(1)
        yield
        yield dut.inp.valid.eq(0)

        for _ in range(3):
            yield

        assert (yield dut.out.valid)
        out = yield dut.out.bits.data

        if fn == FPUOperator.I2F and dst_fmt == FPFormat.S:
            out = out & ((1 << 32) - 1)

        if fn == FPUOperator.F2I and int_fmt == IntFormat.INT32:
            out = out & ((1 << 32) - 1)

        assert out == expected

    return proc


def test_fcvt_s():
    dut = FPUCastMulti()

    cases = [
        # fcvt.s.w
        (2, 2.0, IntFormat.INT32, 0),
        (-2, -2.0, IntFormat.INT32, 0),
        # fcvt.s.wu
        (2, 2.0, IntFormat.INT32, 1),
        (-2, 4.2949673e9, IntFormat.INT32, 1),
        # fcvt.s.l
        (2, 2.0, IntFormat.INT64, 0),
        (-2, -2.0, IntFormat.INT64, 0),
        # fcvt.s.lu
        (2, 2.0, IntFormat.INT64, 1),
        (-2, 1.8446744e19, IntFormat.INT64, 1),
    ]

    for a, expected, int_fmt, fn_mod in cases:
        a = a & ((1 << 64) - 1)
        expected = convert_float(expected, 'f', 32)

        run_test(dut,
                 fp_cast_unittest(dut, a, FPUOperator.I2F, fn_mod, FPFormat.S,
                                  FPFormat.S, int_fmt, expected),
                 sync=True)


def test_fcvt_d():
    dut = FPUCastMulti()

    cases = [
        # fcvt.d.w
        (2, 2.0, IntFormat.INT32, 0),
        (-2, -2.0, IntFormat.INT32, 0),
        # fcvt.d.wu
        (2, 2.0, IntFormat.INT32, 1),
        (-2, 4294967294, IntFormat.INT32, 1),
        # fcvt.d.l
        (2, 2.0, IntFormat.INT64, 0),
        (-2, -2.0, IntFormat.INT64, 0),
        # fcvt.d.lu
        (2, 2.0, IntFormat.INT64, 1),
        (-2, 1.8446744073709552e19, IntFormat.INT64, 1),
    ]

    for a, expected, int_fmt, fn_mod in cases:
        a = a & ((1 << 64) - 1)
        expected = convert_float(expected, 'd', 64)

        run_test(dut,
                 fp_cast_unittest(dut, a, FPUOperator.I2F, fn_mod, FPFormat.D,
                                  FPFormat.D, int_fmt, expected),
                 sync=True)


def test_fcvt_w_s():
    dut = FPUCastMulti()

    cases = [
        # fcvt.w.s
        (-1.1, -1, IntFormat.INT32, 0),
        (-1.0, -1, IntFormat.INT32, 0),
        (-0.9, 0, IntFormat.INT32, 0),
        (0.9, 0, IntFormat.INT32, 0),
        (1.0, 1, IntFormat.INT32, 0),
        (1.1, 1, IntFormat.INT32, 0),
        (-3e9, -1 << 31, IntFormat.INT32, 0),
        (3e9, (1 << 31) - 1, IntFormat.INT32, 0),
        # fcvt.wu.s
        (-3.0, 0, IntFormat.INT32, 1),
        (-1.0, 0, IntFormat.INT32, 1),
        (-0.9, 0, IntFormat.INT32, 1),
        (0.9, 0, IntFormat.INT32, 1),
        (1.0, 1, IntFormat.INT32, 1),
        (1.1, 1, IntFormat.INT32, 1),
        (-3e9, 0, IntFormat.INT32, 1),
        (3e9, 3000000000, IntFormat.INT32, 1),
        # fcvt.l.s
        (-1.1, -1, IntFormat.INT64, 0),
        (-1.0, -1, IntFormat.INT64, 0),
        (-0.9, 0, IntFormat.INT64, 0),
        (0.9, 0, IntFormat.INT64, 0),
        (1.0, 1, IntFormat.INT64, 0),
        (1.1, 1, IntFormat.INT64, 0),
        # fcvt.lu.s
        (-3.0, 0, IntFormat.INT64, 1),
        (-1.0, 0, IntFormat.INT64, 1),
        (-0.9, 0, IntFormat.INT64, 1),
        (0.9, 0, IntFormat.INT64, 1),
        (1.0, 1, IntFormat.INT64, 1),
        (1.1, 1, IntFormat.INT64, 1),
        (-3e9, 0, IntFormat.INT64, 1),
    ]

    for a, expected, int_fmt, fn_mod in cases:
        a = convert_float(a, 'f', 32)
        expected = mask_int(expected, int_fmt)

        run_test(dut,
                 fp_cast_unittest(dut,
                                  a,
                                  FPUOperator.F2I,
                                  fn_mod,
                                  FPFormat.S,
                                  FPFormat.S,
                                  int_fmt,
                                  expected,
                                  rm=RoundingMode.RTZ),
                 sync=True)


def test_fcvt_w_d():
    dut = FPUCastMulti()

    cases = [
        # fcvt.w.d
        (-1.1, -1, IntFormat.INT32, 0),
        (-1.0, -1, IntFormat.INT32, 0),
        (-0.9, 0, IntFormat.INT32, 0),
        (0.9, 0, IntFormat.INT32, 0),
        (1.0, 1, IntFormat.INT32, 0),
        (1.1, 1, IntFormat.INT32, 0),
        (-3e9, -1 << 31, IntFormat.INT32, 0),
        (3e9, (1 << 31) - 1, IntFormat.INT32, 0),
        # fcvt.wu.d
        (-3.0, 0, IntFormat.INT32, 1),
        (-1.0, 0, IntFormat.INT32, 1),
        (-0.9, 0, IntFormat.INT32, 1),
        (0.9, 0, IntFormat.INT32, 1),
        (1.0, 1, IntFormat.INT32, 1),
        (1.1, 1, IntFormat.INT32, 1),
        (-3e9, 0, IntFormat.INT32, 1),
        (3e9, 0xffffffffb2d05e00, IntFormat.INT32, 1),
        # fcvt.l.d
        (-1.1, -1, IntFormat.INT64, 0),
        (-1.0, -1, IntFormat.INT64, 0),
        (-0.9, 0, IntFormat.INT64, 0),
        (0.9, 0, IntFormat.INT64, 0),
        (1.0, 1, IntFormat.INT64, 0),
        (1.1, 1, IntFormat.INT64, 0),
        (-3e9, -3000000000, IntFormat.INT64, 0),
        (3e9, 3000000000, IntFormat.INT64, 0),
        (-3e19, -1 << 63, IntFormat.INT64, 0),
        (3e19, (1 << 63) - 1, IntFormat.INT64, 0),
        # fcvt.lu.d
        (-3.0, 0, IntFormat.INT64, 1),
        (-1.0, 0, IntFormat.INT64, 1),
        (-0.9, 0, IntFormat.INT64, 1),
        (0.9, 0, IntFormat.INT64, 1),
        (1.0, 1, IntFormat.INT64, 1),
        (1.1, 1, IntFormat.INT64, 1),
        (-3e9, 0, IntFormat.INT64, 1),
        (3e9, 3000000000, IntFormat.INT64, 1),
    ]

    for a, expected, int_fmt, fn_mod in cases:
        a = convert_float(a, 'd', 64)
        expected = mask_int(expected, int_fmt)

        run_test(dut,
                 fp_cast_unittest(dut,
                                  a,
                                  FPUOperator.F2I,
                                  fn_mod,
                                  FPFormat.D,
                                  FPFormat.D,
                                  int_fmt,
                                  expected,
                                  rm=RoundingMode.RTZ),
                 sync=True)
