import struct

from room.fpu import FPUOperator, FType, FPUFMA, FPUDivSqrt64
from room.test import run_test


def convert_float(f, typ, flen):
    return int(struct.pack('>' + typ, f).hex(), base=16) & ((1 << flen) - 1)


def fma_unittest(fma, a, b, c, fn, fn_mod, expected):

    def proc():
        yield fma.inp.in1.eq(a)
        yield fma.inp.in2.eq(b)
        yield fma.inp.in3.eq(c)
        yield fma.inp.fn.eq(fn)
        yield fma.inp.fn_mod.eq(fn_mod)
        yield fma.inp_valid.eq(1)
        yield
        yield fma.inp_valid.eq(0)

        for _ in range(3):
            yield

        assert (yield fma.out_valid)
        out = yield fma.out.data
        assert out == expected

    return proc


def test_fma_add_d():
    dut = FPUFMA(64, FType.FP64)

    cases = [
        # fadd
        (0.0, 2.5, 1.0, 3.5, FPUOperator.ADD, 0),
        (0.0, -1235.1, 1.1, -1234, FPUOperator.ADD, 0),
        (0.0, 3.14159265, 0.00000001, 3.14159266, FPUOperator.ADD, 0),
        # fsub
        (0.0, 2.5, 1.0, 1.5, FPUOperator.ADD, 1),
        (0.0, -1235.1, -1.1, -1234, FPUOperator.ADD, 1),
        (0.0, 3.14159265, 0.00000001, 3.1415926400000001, FPUOperator.ADD, 1),
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


def fdiv_unittest(fdiv, a, b, is_sqrt, expected):

    def proc():
        yield fdiv.a.eq(a)
        yield fdiv.b.eq(b)
        yield fdiv.is_sqrt.eq(is_sqrt)
        yield fdiv.in_valid.eq(1)
        yield
        yield fdiv.in_valid.eq(0)

        while not (yield fdiv.out_valid):
            yield

        out = yield fdiv.out
        assert out == expected

    return proc


def test_fdiv():
    dut = FPUDivSqrt64()

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

        run_test(dut, fdiv_unittest(dut, a, b, is_sqrt, expected), sync=True)
