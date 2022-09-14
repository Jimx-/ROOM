from amaranth.sim import Settle
import pytest

from room.alu import ALU, ALUOperator, IntDiv
from room.test import run_test


def mask_xlen(data, xlen):
    return data & ((1 << xlen) - 1)


def alu_unittest(alu, a, b, fn, expected):

    def proc():
        yield alu.in1.eq(a)
        yield alu.in2.eq(b)
        yield alu.fn.eq(fn)
        yield Settle()
        out = yield alu.out
        assert out == expected

    return proc


@pytest.mark.parametrize("xlen", [32, 64])
def test_alu_add(xlen):
    dut = ALU(xlen)

    cases = [
        (0x00000000, 0x00000000, 0x00000000),
        (0x00000001, 0x00000001, 0x00000002),
        (0x00000003, 0x00000007, 0x0000000a),
        (0x0000000000000000, 0xffffffffffff8000, 0xffffffffffff8000),
        (0xffffffff80000000, 0x00000000, 0xffffffff80000000),
        (0xffffffff80000000, 0xffffffffffff8000, 0xffffffff7fff8000),
        (0x0000000000000000, 0x0000000000007fff, 0x0000000000007fff),
        (0x000000007fffffff, 0x0000000000000000, 0x000000007fffffff),
        (0x000000007fffffff, 0x0000000000007fff, 0x0000000080007ffe),
        (0xffffffff80000000, 0x0000000000007fff, 0xffffffff80007fff),
        (0x000000007fffffff, 0xffffffffffff8000, 0x000000007fff7fff),
        (0x0000000000000000, 0xffffffffffffffff, 0xffffffffffffffff),
        (0xffffffffffffffff, 0x0000000000000001, 0x0000000000000000),
        (0xffffffffffffffff, 0xffffffffffffffff, 0xfffffffffffffffe),
        (0x0000000000000001, 0x000000007fffffff, 0x0000000080000000),
    ]

    for a, b, expected in cases:
        a = mask_xlen(a, xlen)
        b = mask_xlen(b, xlen)
        expected = mask_xlen(expected, xlen)

        run_test(dut, alu_unittest(dut, a, b, ALUOperator.ADD, expected))


@pytest.mark.parametrize("xlen", [32, 64])
def test_alu_and(xlen):
    dut = ALU(xlen)

    cases = [
        (0xff00ff00, 0x0f0f0f0f, 0x0f000f00),
        (0x0ff00ff0, 0xf0f0f0f0, 0x00f000f0),
        (0x00ff00ff, 0x0f0f0f0f, 0x000f000f),
        (0xf00ff00f, 0xf0f0f0f0, 0xf000f000),
        (0xff00ff00, 0x0f0f0f0f, 0x0f000f00),
        (0x0ff00ff0, 0xf0f0f0f0, 0x00f000f0),
    ]

    for a, b, expected in cases:
        a = mask_xlen(a, xlen)
        b = mask_xlen(b, xlen)
        expected = mask_xlen(expected, xlen)

        run_test(dut, alu_unittest(dut, a, b, ALUOperator.AND, expected))


@pytest.mark.parametrize("xlen", [32, 64])
def test_alu_or(xlen):
    dut = ALU(xlen)

    cases = [
        (0xff00ff00, 0x0f0f0f0f, 0xff0fff0f),
        (0x0ff00ff0, 0xf0f0f0f0, 0xfff0fff0),
        (0x00ff00ff, 0x0f0f0f0f, 0x0fff0fff),
        (0xf00ff00f, 0xf0f0f0f0, 0xf0fff0ff),
    ]

    for a, b, expected in cases:
        a = mask_xlen(a, xlen)
        b = mask_xlen(b, xlen)
        expected = mask_xlen(expected, xlen)

        run_test(dut, alu_unittest(dut, a, b, ALUOperator.OR, expected))


@pytest.mark.parametrize("xlen", [32, 64])
def test_alu_sll(xlen):
    dut = ALU(xlen)

    cases = [
        (0x0000000000000001, 0, 0x0000000000000001),
        (0x0000000000000001, 1, 0x0000000000000002),
        (0x0000000000000001, 7, 0x0000000000000080),
        (0x0000000000000001, 14, 0x0000000000004000),
        (0x0000000000000001, 31, 0x0000000080000000),
        (0xffffffffffffffff, 0, 0xffffffffffffffff),
        (0xffffffffffffffff, 1, 0xfffffffffffffffe),
        (0xffffffffffffffff, 7, 0xffffffffffffff80),
        (0xffffffffffffffff, 14, 0xffffffffffffc000),
        (0xffffffffffffffff, 31, 0xffffffff80000000),
        (0x0000000021212121, 0, 0x0000000021212121),
        (0x0000000021212121, 1, 0x0000000042424242),
        (0x0000000021212121, 7, 0x0000001090909080),
        (0x0000000021212121, 14, 0x0000084848484000),
        (0x0000000021212121, 31, 0x1090909080000000),
        (0x0000000021212121, 0xffffffffffffffc0, 0x0000000021212121),
        (0x0000000021212121, 0xffffffffffffffc1, 0x0000000042424242),
        (0x0000000021212121, 0xffffffffffffffc7, 0x0000001090909080),
        (0x0000000021212121, 0xffffffffffffffce, 0x0000084848484000),
    ]

    if xlen == 64:
        cases += [
            (0x0000000021212121, 0xffffffffffffffff, 0x8000000000000000),
            (0x0000000000000001, 63, 0x8000000000000000),
            (0xffffffffffffffff, 39, 0xffffff8000000000),
            (0x0000000021212121, 43, 0x0909080000000000),
        ]

    for a, b, expected in cases:
        a = mask_xlen(a, xlen)
        b = mask_xlen(b, xlen)
        expected = mask_xlen(expected, xlen)

        run_test(dut, alu_unittest(dut, a, b, ALUOperator.SL, expected))


@pytest.mark.parametrize("xlen", [32, 64])
def test_alu_slt(xlen):
    dut = ALU(xlen)

    cases = [
        (0x0000000000000000, 0x0000000000000000, 0),
        (0x0000000000000001, 0x0000000000000001, 0),
        (0x0000000000000003, 0x0000000000000007, 1),
        (0x0000000000000007, 0x0000000000000003, 0),
        (0x0000000000000000, 0xffffffffffff8000, 0),
        (0xffffffff80000000, 0x0000000000000000, 1),
        (0xffffffff80000000, 0xffffffffffff8000, 1),
        (0x0000000000000000, 0x0000000000007fff, 1),
        (0x000000007fffffff, 0x0000000000000000, 0),
        (0x000000007fffffff, 0x0000000000007fff, 0),
        (0xffffffff80000000, 0x0000000000007fff, 1),
        (0x000000007fffffff, 0xffffffffffff8000, 0),
        (0x0000000000000000, 0xffffffffffffffff, 0),
        (0xffffffffffffffff, 0x0000000000000001, 1),
        (0xffffffffffffffff, 0xffffffffffffffff, 0),
    ]

    for a, b, expected in cases:
        a = mask_xlen(a, xlen)
        b = mask_xlen(b, xlen)
        expected = mask_xlen(expected, xlen)

        run_test(dut, alu_unittest(dut, a, b, ALUOperator.SLT, expected))


@pytest.mark.parametrize("xlen", [32, 64])
def test_alu_sltu(xlen):
    dut = ALU(xlen)

    cases = [
        (0x00000000, 0x00000000, 0),
        (0x00000001, 0x00000001, 0),
        (0x00000003, 0x00000007, 1),
        (0x00000007, 0x00000003, 0),
        (0x00000000, 0xffff8000, 1),
        (0x80000000, 0x00000000, 0),
        (0x80000000, 0xffff8000, 1),
        (0x00000000, 0x00007fff, 1),
        (0x7fffffff, 0x00000000, 0),
        (0x7fffffff, 0x00007fff, 0),
        (0x80000000, 0x00007fff, 0),
        (0x7fffffff, 0xffff8000, 1),
        (0x00000000, 0xffffffff, 1),
        (0xffffffff, 0x00000001, 0),
        (0xffffffff, 0xffffffff, 0),
    ]

    for a, b, expected in cases:
        a = mask_xlen(a, xlen)
        b = mask_xlen(b, xlen)
        expected = mask_xlen(expected, xlen)

        run_test(dut, alu_unittest(dut, a, b, ALUOperator.SLTU, expected))


@pytest.mark.parametrize("xlen", [32, 64])
def test_alu_sra(xlen):
    dut = ALU(xlen)

    cases = [
        (0xffffffff80000000, 0, 0xffffffff80000000),
        (0xffffffff80000000, 1, 0xffffffffc0000000),
        (0xffffffff80000000, 7, 0xffffffffff000000),
        (0xffffffff80000000, 14, 0xfffffffffffe0000),
        (0xffffffff80000001, 31, 0xffffffffffffffff),
        (0x000000007fffffff, 0, 0x000000007fffffff),
        (0x000000007fffffff, 1, 0x000000003fffffff),
        (0x000000007fffffff, 7, 0x0000000000ffffff),
        (0x000000007fffffff, 14, 0x000000000001ffff),
        (0x000000007fffffff, 31, 0x0000000000000000),
        (0xffffffff81818181, 0, 0xffffffff81818181),
        (0xffffffff81818181, 1, 0xffffffffc0c0c0c0),
        (0xffffffff81818181, 7, 0xffffffffff030303),
        (0xffffffff81818181, 14, 0xfffffffffffe0606),
        (0xffffffff81818181, 31, 0xffffffffffffffff),
        (0xffffffff81818181, 0xffffffffffffffc0, 0xffffffff81818181),
        (0xffffffff81818181, 0xffffffffffffffc1, 0xffffffffc0c0c0c0),
        (0xffffffff81818181, 0xffffffffffffffc7, 0xffffffffff030303),
        (0xffffffff81818181, 0xffffffffffffffce, 0xfffffffffffe0606),
        (0xffffffff81818181, 0xffffffffffffffff, 0xffffffffffffffff),
    ]

    for a, b, expected in cases:
        a = mask_xlen(a, xlen)
        b = mask_xlen(b, xlen)
        expected = mask_xlen(expected, xlen)

        run_test(dut, alu_unittest(dut, a, b, ALUOperator.SRA, expected))


@pytest.mark.parametrize("xlen", [32, 64])
def test_alu_srl(xlen):
    dut = ALU(xlen)

    cases = [
        (0xffffffff80000000, 0),
        (0xffffffff80000000, 1),
        (0xffffffff80000000, 7),
        (0xffffffff80000000, 14),
        (0xffffffff80000001, 31),
        (0xffffffffffffffff, 0),
        (0xffffffffffffffff, 1),
        (0xffffffffffffffff, 7),
        (0xffffffffffffffff, 14),
        (0xffffffffffffffff, 31),
        (0x0000000021212121, 0),
        (0x0000000021212121, 1),
        (0x0000000021212121, 7),
        (0x0000000021212121, 14),
        (0x0000000021212121, 31),
        (0x0000000021212121, 0xffffffffffffffc0, 0x0000000021212121),
        (0x0000000021212121, 0xffffffffffffffc1, 0x0000000010909090),
        (0x0000000021212121, 0xffffffffffffffc7, 0x0000000000424242),
        (0x0000000021212121, 0xffffffffffffffce, 0x0000000000008484),
        (0x0000000021212121, 0xffffffffffffffff, 0x0000000000000000),
    ]

    for c in cases:
        if len(c) == 2:
            a, b = c
            expected = mask_xlen(a, xlen) >> b
        else:
            a, b, expected = c

        a = mask_xlen(a, xlen)
        expected = mask_xlen(expected, xlen)

        run_test(dut, alu_unittest(dut, a, b, ALUOperator.SR, expected))


@pytest.mark.parametrize("xlen", [32, 64])
def test_alu_sub(xlen):
    dut = ALU(xlen)

    cases = [
        (0x0000000000000000, 0x0000000000000000, 0x0000000000000000),
        (0x0000000000000001, 0x0000000000000001, 0x0000000000000000),
        (0x0000000000000003, 0x0000000000000007, 0xfffffffffffffffc),
        (0x0000000000000000, 0xffffffffffff8000, 0x0000000000008000),
        (0xffffffff80000000, 0x0000000000000000, 0xffffffff80000000),
        (0xffffffff80000000, 0xffffffffffff8000, 0xffffffff80008000),
        (0x0000000000000000, 0x0000000000007fff, 0xffffffffffff8001),
        (0x000000007fffffff, 0x0000000000000000, 0x000000007fffffff),
        (0x000000007fffffff, 0x0000000000007fff, 0x000000007fff8000),
        (0xffffffff80000000, 0x0000000000007fff, 0xffffffff7fff8001),
        (0x000000007fffffff, 0xffffffffffff8000, 0x0000000080007fff),
        (0x0000000000000000, 0xffffffffffffffff, 0x0000000000000001),
        (0xffffffffffffffff, 0x0000000000000001, 0xfffffffffffffffe),
        (0xffffffffffffffff, 0xffffffffffffffff, 0x0000000000000000),
    ]

    for a, b, expected in cases:
        a = mask_xlen(a, xlen)
        b = mask_xlen(b, xlen)
        expected = mask_xlen(expected, xlen)

        run_test(dut, alu_unittest(dut, a, b, ALUOperator.SUB, expected))


@pytest.mark.parametrize("xlen", [32, 64])
def test_alu_sub(xlen):
    dut = ALU(xlen)

    cases = [
        (0x0000000000000000, 0x0000000000000000, 0x0000000000000000),
        (0x0000000000000001, 0x0000000000000001, 0x0000000000000000),
        (0x0000000000000003, 0x0000000000000007, 0xfffffffffffffffc),
        (0x0000000000000000, 0xffffffffffff8000, 0x0000000000008000),
        (0xffffffff80000000, 0x0000000000000000, 0xffffffff80000000),
        (0xffffffff80000000, 0xffffffffffff8000, 0xffffffff80008000),
        (0x0000000000000000, 0x0000000000007fff, 0xffffffffffff8001),
        (0x000000007fffffff, 0x0000000000000000, 0x000000007fffffff),
        (0x000000007fffffff, 0x0000000000007fff, 0x000000007fff8000),
        (0xffffffff80000000, 0x0000000000007fff, 0xffffffff7fff8001),
        (0x000000007fffffff, 0xffffffffffff8000, 0x0000000080007fff),
        (0x0000000000000000, 0xffffffffffffffff, 0x0000000000000001),
        (0xffffffffffffffff, 0x0000000000000001, 0xfffffffffffffffe),
        (0xffffffffffffffff, 0xffffffffffffffff, 0x0000000000000000),
    ]

    for a, b, expected in cases:
        a = mask_xlen(a, xlen)
        b = mask_xlen(b, xlen)
        expected = mask_xlen(expected, xlen)

        run_test(dut, alu_unittest(dut, a, b, ALUOperator.SUB, expected))


@pytest.mark.parametrize("xlen", [32, 64])
def test_alu_xor(xlen):
    dut = ALU(xlen)

    cases = [
        (0xff00ff00, 0x0f0f0f0f, 0xf00ff00f),
        (0x0ff00ff0, 0xf0f0f0f0, 0xff00ff00),
        (0x00ff00ff, 0x0f0f0f0f, 0x0ff00ff0),
        (0xf00ff00f, 0xf0f0f0f0, 0x00ff00ff),
    ]

    for a, b, expected in cases:
        a = mask_xlen(a, xlen)
        b = mask_xlen(b, xlen)
        expected = mask_xlen(expected, xlen)

        run_test(dut, alu_unittest(dut, a, b, ALUOperator.XOR, expected))


def div_unittest(div, a, b, fn, expected):

    def proc():
        yield div.req.in1.eq(a)
        yield div.req.in2.eq(b)
        yield div.req.fn.eq(fn)
        yield div.req_valid.eq(1)
        yield

        while not (yield div.resp_valid):
            yield

        out = yield div.resp_data
        yield div.resp_ready.eq(1)
        yield
        if out != expected:
            print(hex(a), hex(b), hex(expected), hex(out))
        assert out == expected

    return proc


@pytest.mark.parametrize("xlen", [32, 64])
def test_div_div(xlen):
    dut = IntDiv(xlen)

    cases = [
        (20, 6, 3),
        (-20, 6, -3),
        (20, -6, -3),
        (-20, -6, 3),
        (-1 << (xlen - 1), 1, -1 << (xlen - 1)),
        (-1 << (xlen - 1), -1, -1 << (xlen - 1)),
        (-1 << (xlen - 1), 0, -1),
        (1, 0, -1),
        (0, 0, -1),
    ]

    for a, b, expected in cases:
        a = mask_xlen(a, xlen)
        b = mask_xlen(b, xlen)
        expected = mask_xlen(expected, xlen)

        run_test(dut,
                 div_unittest(dut, a, b, ALUOperator.DIV, expected),
                 sync=True)


@pytest.mark.parametrize("xlen", [32, 64])
def test_div_divu(xlen):
    dut = IntDiv(xlen)

    cases = [
        (20, 6, 3),
        (-20, 6, 3074457345618258599 if xlen == 64 else 715827879),
        (20, -6, 0),
        (-20, -6, 0),
        (-1 << (xlen - 1), 1, -1 << (xlen - 1)),
        (-1 << (xlen - 1), -1, 0),
        (-1 << (xlen - 1), 0, -1),
        (1, 0, -1),
        (0, 0, -1),
    ]

    for a, b, expected in cases:
        a = mask_xlen(a, xlen)
        b = mask_xlen(b, xlen)
        expected = mask_xlen(expected, xlen)

        run_test(dut,
                 div_unittest(dut, a, b, ALUOperator.DIVU, expected),
                 sync=True)


@pytest.mark.parametrize("xlen", [32, 64])
def test_div_rem(xlen):
    dut = IntDiv(xlen)

    cases = [
        (20, 6, 2),
        (-20, 6, -2),
        (20, -6, 2),
        (-20, -6, -2),
        (-1 << (xlen - 1), 1, 0),
        (-1 << (xlen - 1), -1, 0),
        (-1 << (xlen - 1), 0, -1 << (xlen - 1)),
        (1, 0, 1),
        (0, 0, 0),
    ]

    for a, b, expected in cases:
        a = mask_xlen(a, xlen)
        b = mask_xlen(b, xlen)
        expected = mask_xlen(expected, xlen)

        run_test(dut,
                 div_unittest(dut, a, b, ALUOperator.REM, expected),
                 sync=True)


@pytest.mark.parametrize("xlen", [32, 64])
def test_div_remu(xlen):
    dut = IntDiv(xlen)

    cases = [
        (20, 6, 2),
        (-20, 6, 2),
        (20, -6, 20),
        (-20, -6, -20),
        (-1 << (xlen - 1), 1, 0),
        (-1 << (xlen - 1), -1, -1 << (xlen - 1)),
        (-1 << (xlen - 1), 0, -1 << (xlen - 1)),
        (1, 0, 1),
        (0, 0, 0),
    ]

    for a, b, expected in cases:
        a = mask_xlen(a, xlen)
        b = mask_xlen(b, xlen)
        expected = mask_xlen(expected, xlen)

        run_test(dut,
                 div_unittest(dut, a, b, ALUOperator.REMU, expected),
                 sync=True)
