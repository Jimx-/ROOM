import struct

from room.consts import RoundingMode
from room.fpu import (FPUOperator, FPFormat, FPUComp, FPUCastMulti,
                      IntFormat)
from room.test import run_test

FLI_TABLES = FPUCastMulti._FLI_TABLES


def fcvtmod_ref(a):
    """Reference model of Spike's fcvtmod_w_d.h."""
    sign = (a >> 63) & 1
    exp = (a >> 52) & 0x7ff
    frac = a & ((1 << 52) - 1)

    inexact = invalid = False

    if exp == 0:
        inexact = frac != 0
        frac = 0
    elif exp == 0x7ff:  # inf or NaN
        invalid = True
        frac = 0
    else:
        true_exp = exp - 1023
        shift = true_exp - 52
        frac |= 1 << 52

        if shift >= 64:
            frac = 0
        elif 0 <= shift < 64:
            frac <<= shift
        elif -64 < shift < 0:
            inexact = ((frac << (64 + shift)) & ((1 << 64) - 1)) != 0
            frac >>= -shift
        else:
            frac = 0
            inexact = True

        if true_exp > 31 or frac > (0x80000000 if sign else 0x7fffffff):
            invalid = True
            inexact = False  # invalid takes precedence

        if sign:
            frac = -frac

    result = frac & 0xFFFFFFFF
    if result & 0x80000000:
        result -= 1 << 32

    return result, invalid, inexact


def round_unittest(dut, a, fn_mod, rm, fmt, typ, expected, expected_nx,
                   expected_nv=False):

    def proc():
        yield dut.inp.bits.in1.eq(a)
        yield dut.inp.bits.fn.eq(FPUOperator.ROUND)
        yield dut.inp.bits.fn_mod.eq(fn_mod)
        yield dut.inp.bits.rm.eq(rm)
        yield dut.inp.bits.src_fmt.eq(fmt)
        yield dut.inp.bits.dst_fmt.eq(fmt)
        yield dut.inp.bits.int_fmt.eq(3)
        yield dut.inp.valid.eq(1)
        yield
        yield dut.inp.valid.eq(0)

        for _ in range(3):
            yield

        assert (yield dut.out.valid)
        out = yield dut.out.bits.data
        status = yield dut.out.bits.status

        exp = expected
        if typ == 'f':
            out &= (1 << 32) - 1
            exp &= (1 << 32) - 1

        assert out == exp, f'{a:#x}: {out:#x} != {exp:#x}'
        assert status & 1 == expected_nx, \
            f'{a:#x}: nx {status & 1} != {expected_nx}'
        assert (status >> 4) & 1 == expected_nv, \
            f'{a:#x}: nv {(status >> 4) & 1} != {expected_nv}'
        # fround never raises of/uf/dz
        assert status & 0xe == 0, f'{a:#x}: unexpected flags {status:#x}'

    return proc


def test_fround_s():
    dut = FPUCastMulti(use_zfa=True)

    f = lambda v: convert_float(v, 'f') | 0xffffffff00000000  # nan-boxed
    cases = [
        # (input, rm, expected, nx-for-roundnx)
        (2.5, 0, 2.0, True),  # rne ties to even
        (2.5, 1, 2.0, True),  # rtz
        (2.5, 2, 2.0, True),  # rdn
        (2.5, 3, 3.0, True),  # rup
        (2.5, 4, 3.0, True),  # rmm
        (3.5, 0, 4.0, True),  # rne ties to even (up)
        (1.5, 0, 2.0, True),
        (-2.5, 0, -2.0, True),
        (-2.5, 2, -3.0, True),
        (-2.5, 3, -2.0, True),
        (2.0, 0, 2.0, False),  # integral passthrough
        (0.5, 0, 0.0, True),  # rne to even 0
        (0.5, 3, 1.0, True),
        (-0.5, 0, -0.0, True),  # rne -> -0
        (-0.5, 2, -1.0, True),
        (0.49999997, 0, 0.0, True),  # largest f < 0.5
        (-1.0, 0, -1.0, False),
        # 0.99999994 -> carry into next binade
        (0.99999994, 3, 1.0, True),
        (0.99999994, 1, 0.0, True),
        # largest f, exponent 127, integral
        (3.4028235e38, 0, 3.4028235e38, False),
        # 2^23 and 2^24 are integral for f
        (8388608.0, 0, 8388608.0, False),
        # 2^22 + 0.5: integral boundary
        (4194304.5, 1, 4194304.0, True),
        # min subnormal -> 0 (nx) / 1 (rup)
        (1e-45, 0, 0.0, True),
        (1e-45, 3, 1.0, True),
        (0.0, 0, 0.0, False),
    ]

    for v, rm, ev, nx in cases:
        for fn_mod in (0, 1):
            run_test(dut,
                     round_unittest(dut, f(v), fn_mod, rm, FPFormat.S, 'f',
                                    f(ev), nx if fn_mod else False),
                     sync=True)

    # specials: (input, expected, nv)
    specials = [
        (0x7f800000, 0x7f800000, 0),  # +inf
        (0xff800000, 0xff800000, 0),  # -inf
        (0x7fc00000, 0x7fc00000, 0),  # qNaN -> canonical NaN
        (0x7fa00000, 0x7fc00000, 1),  # sNaN -> canonical NaN + NV
        (0x80000000, 0x80000000, 0),  # -0
    ]

    for a, ev, nv in specials:
        a |= 0xffffffff00000000

        for fn_mod in (0, 1):
            run_test(dut,
                     round_unittest(dut, a, fn_mod, 0, FPFormat.S, 'f', ev,
                                    False, nv),
                     sync=True)

    # badly NaN-boxed operands are canonical qNaNs: canonical NaN
    # result and no flags, even when the raw low bits look like an
    # sNaN (ACT4 ZfaD-fround.s cp_fs1_badNB_D_S)
    for a in (0xfeffffff7f800001, 0xdeadbeef7fc00000,
              0xaaaaaaaa80000000, 0x000000007f800001):
        for fn_mod in (0, 1):
            run_test(dut,
                     round_unittest(dut, a, fn_mod, 0, FPFormat.S, 'f',
                                    0x7fc00000, False, False),
                     sync=True)


def test_fround_d():
    dut = FPUCastMulti(use_zfa=True)

    f = lambda v: convert_float(v, 'd')
    cases = [
        (1.5, 0, 2.0, True),
        (1.5, 2, 1.0, True),
        (2.5, 0, 2.0, True),
        (0.5, 0, 0.0, True),
        (-0.5, 0, -0.0, True),
        (4503599627370496.0, 0, 4503599627370496.0, False),  # 2^52 integral
        (2251799813685248.5, 1, 2251799813685248.0, True),  # 2^51 + 0.5
        (0.9999999999999999, 3, 1.0, True),  # carry into next binade
        (5e-324, 0, 0.0, True),  # min subnormal
        (5e-324, 3, 1.0, True),
        (2.0, 0, 2.0, False),
        (1e300, 0, 1e300, False),  # huge integral passthrough
    ]

    for v, rm, ev, nx in cases:
        for fn_mod in (0, 1):
            run_test(dut,
                     round_unittest(dut, f(v), fn_mod, rm, FPFormat.D, 'd',
                                    f(ev), nx if fn_mod else False),
                     sync=True)

    for a, ev, nv in [(0x7ff0000000000000, 0x7ff0000000000000, 0),
                      (0x7ff8000000000000, 0x7ff8000000000000, 0),
                      (0x7ff0000000000001, 0x7ff8000000000000, 1)]:
        for fn_mod in (0, 1):
            run_test(dut,
                     round_unittest(dut, a, fn_mod, 0, FPFormat.D, 'd', ev,
                                    False, nv),
                     sync=True)


def fcvtmod_unittest(dut, a, expected):

    def proc():
        yield dut.inp.bits.in1.eq(a)
        yield dut.inp.bits.fn.eq(FPUOperator.MODF2I)
        yield dut.inp.bits.fn_mod.eq(0)
        yield dut.inp.bits.rm.eq(RoundingMode.RTZ)
        yield dut.inp.bits.src_fmt.eq(FPFormat.D)
        yield dut.inp.bits.dst_fmt.eq(FPFormat.D)
        yield dut.inp.bits.int_fmt.eq(IntFormat.INT32)
        yield dut.inp.valid.eq(1)
        yield
        yield dut.inp.valid.eq(0)

        for _ in range(3):
            yield

        assert (yield dut.out.valid)
        out = yield dut.out.bits.data
        status = yield dut.out.bits.status

        assert out == (expected[0] & ((1 << 64) - 1)), \
            f'{a:#x}: {out:#x} != {expected[0]:#x}'
        assert (status >> 4) & 1 == expected[1], \
            f'{a:#x}: nv {(status >> 4) & 1} != {expected[1]}'
        assert status & 1 == expected[2], \
            f'{a:#x}: nx {status & 1} != {expected[2]}'

    return proc


def test_fcvtmod_w_d():
    dut = FPUCastMulti(use_zfa=True)

    cases = [
        0x0000000000000000,  # 0.0
        0x8000000000000000,  # -0.0
        0x7ff0000000000000,  # +inf
        0xfff0000000000000,  # -inf
        0x7ff8000000000000,  # qNaN
        0x7ff0000000000001,  # sNaN
        0x0000000000000001,  # min subnormal
        0x41f0000000500000,  # 2^32 + 5
        0xc1f0000000500000,  # -(2^32 + 5)
        0xbff8000000000000,  # -1.5
        0x3ff8000000000000,  # 1.5
        0x4006000000000000,  # 2.75
        0xc006000000000000,  # -2.75
        0xc004000000000000,  # -2.5
        0xc002000000000000,  # -2.25
        0xc000000000000000,  # -2.0
        0x41dffffffc0000000,  # 2^31 - 1
        0x41e0000000000000,  # 2^31
        0xc1e0000000000000,  # -2^31 (exact, valid)
        0xc1e0000000000001,  # -(2^31 + 1)
        0xc1dffffffe0000000,  # -(2^31 + 0.5) -> RTZ -2^31, valid + NX
        0x41e3b2d05e000000,  # ~3e9
        0xc1e3b2d05e000000,  # ~-3e9
        0x43e0000000000000,  # 2^63
        0x43f0000000000000,  # 2^64 -> 0 modulo 2^32
        0x43f0000000000001,  # 2^64 + 4096 -> 4096
        0xc3f0000000000001,  # -(2^64 + 4096) -> -4096
        0x4400000000000001,  # 2^65 + 8192 -> 8192
        0x4530000000000001,  # exponent 84: low 32 bits forced to zero
        0x4330000000000000,  # 2^52
        0x4330000000000001,  # 2^52 + 1
        0x43cb72eb13dc494a,  # ~3.96e18 (RISCOF vector)
        0xc3d967a4ae26514c,  # ~-7.32e18 (RISCOF vector)
        0xbfdb008d57e19f88,  # -0.42
        0xbf80000000000000,  # -0.0078125
        0x3fe0000000000000,  # 0.5 -> 0 + NX
        0x9b2ce50433164ad4,  # ~-2^-589 (ACT4 vector): denorm_shamt wrap
        0x0130000000000001,  # ~2^-1004: denorm_shamt wrap, far negative
    ]

    for a in cases:
        run_test(dut,
                 fcvtmod_unittest(dut, a, fcvtmod_ref(a)),
                 sync=True)


def test_fli_table_s():
    values = [
        -1.0, None, 2.0**-16, 2.0**-15, 2.0**-8, 2.0**-7, 2.0**-4, 2.0**-3,
        0.25, 0.3125, 0.375, 0.4375, 0.5, 0.625, 0.75, 0.875,
        1.0, 1.25, 1.5, 1.75, 2.0, 2.5, 3.0, 4.0,
        8.0, 16.0, 2.0**7, 2.0**8, 2.0**15, 2.0**16, float('inf'), None,
    ]
    table = FLI_TABLES[FPFormat.S]

    assert len(table) == 32
    for imm, v in enumerate(values):
        if imm == 0x1:  # min positive normal
            assert table[imm] == 0x00800000
        elif imm == 0x1f:  # canonical NaN
            assert table[imm] == 0x7fc00000
        else:
            assert table[imm] == convert_float(v, 'f'), hex(imm)


def test_fli_table_d():
    values = [
        -1.0, None, 2.0**-16, 2.0**-15, 2.0**-8, 2.0**-7, 2.0**-4, 2.0**-3,
        0.25, 0.3125, 0.375, 0.4375, 0.5, 0.625, 0.75, 0.875,
        1.0, 1.25, 1.5, 1.75, 2.0, 2.5, 3.0, 4.0,
        8.0, 16.0, 2.0**7, 2.0**8, 2.0**15, 2.0**16, float('inf'), None,
    ]
    table = FLI_TABLES[FPFormat.D]

    assert len(table) == 32
    for imm, v in enumerate(values):
        if imm == 0x1:  # min positive normal
            assert table[imm] == 0x0010000000000000
        elif imm == 0x1f:  # canonical NaN
            assert table[imm] == 0x7ff8000000000000
        else:
            assert table[imm] == convert_float(v, 'd'), hex(imm)


def test_fli_table_h():
    table = FLI_TABLES[FPFormat.H]
    assert len(table) == 32
    # Entries whose half encoding differs from the naive value mapping.
    expected = {
        0x0: 0xbc00,  # -1.0
        0x1: 0x0400,  # min positive normal
        0x2: 0x0100,  # 2^-16 (subnormal)
        0x3: 0x0200,  # 2^-15 (subnormal)
        0x1d: 0x7c00,  # 2^16 -> +inf
        0x1e: 0x7c00,  # +inf
        0x1f: 0x7e00,  # canonical NaN
    }
    for imm, bits in expected.items():
        assert table[imm] == bits, hex(imm)


def fli_unittest(dut, imm, dst_fmt, expected):

    def proc():
        yield dut.inp.bits.in1.eq(imm)
        yield dut.inp.bits.fn.eq(FPUOperator.FLI)
        yield dut.inp.bits.fn_mod.eq(0)
        yield dut.inp.bits.rm.eq(RoundingMode.RNE)
        yield dut.inp.bits.src_fmt.eq(dst_fmt)
        yield dut.inp.bits.dst_fmt.eq(dst_fmt)
        yield dut.inp.valid.eq(1)
        yield
        yield dut.inp.valid.eq(0)

        for _ in range(3):
            yield

        assert (yield dut.out.valid)
        assert (yield dut.out.bits.data) == expected
        assert (yield dut.out.bits.status) == 0  # fli never raises flags

    return proc


def test_fli_s():
    dut = FPUCastMulti(use_zfa=True)
    for imm in range(32):
        run_test(dut,
                 fli_unittest(dut, imm, FPFormat.S,
                              FLI_TABLES[FPFormat.S][imm]),
                 sync=True)


def test_fli_d():
    dut = FPUCastMulti(use_zfa=True)
    for imm in range(32):
        run_test(dut,
                 fli_unittest(dut, imm, FPFormat.D,
                              FLI_TABLES[FPFormat.D][imm]),
                 sync=True)


def convert_float(f, typ):
    return int(struct.pack('>' + typ, f).hex(), base=16)


def nan_bits(typ, signaling=False):
    if typ == 'f':
        bits = 0x7f800000
        man = 1 << 22
    else:
        bits = 0x7ff0000000000000
        man = 1 << 51

    return bits | (man - 1 if signaling else man)


def comp_unittest(dut, a, b, fn, rm, expected, expected_nv):

    def proc():
        yield dut.inp.bits.in1.eq(a)
        yield dut.inp.bits.in2.eq(b)
        yield dut.inp.bits.fn.eq(fn)
        yield dut.inp.bits.fn_mod.eq(0)
        yield dut.inp.bits.rm.eq(rm)
        yield dut.inp.valid.eq(1)
        yield
        yield dut.inp.valid.eq(0)

        for _ in range(3):
            yield

        assert (yield dut.out.valid)
        out = yield dut.out.bits.data
        nv = yield dut.out.bits.status[4]
        assert out == expected, f'data {out:#x} != {expected:#x}'
        assert nv == expected_nv, f'nv {nv} != {expected_nv}'

    return proc


def comp_cases(dut, fmt, typ, width, nan, snan):
    fli_nan = nan

    minmax = [
        # fminm
        (1.0, 0.5, RoundingMode.RDN, convert_float(0.5, typ), 0),
        (0.5, 1.0, RoundingMode.RDN, convert_float(0.5, typ), 0),
        (-0.0, 0.0, RoundingMode.RDN, (1 << (width - 1)), 0),  # -0
        # fmaxm
        (1.0, 0.5, RoundingMode.RUP, convert_float(1.0, typ), 0),
        (0.5, 1.0, RoundingMode.RUP, convert_float(1.0, typ), 0),
        (0.0, -0.0, RoundingMode.RUP, 0, 0),  # +0
        # qNaN operand -> canonical NaN for both fminm/fmaxm
        (1.0, nan, RoundingMode.RDN, fli_nan, 0),
        (nan, 1.0, RoundingMode.RUP, fli_nan, 0),
        # sNaN operand -> canonical NaN + NV
        (1.0, snan, RoundingMode.RDN, fli_nan, 1),
        (snan, 1.0, RoundingMode.RUP, fli_nan, 1),
        (nan, nan, RoundingMode.RDN, fli_nan, 0),
    ]

    for a, b, rm, expected, nv in minmax:
        a = convert_float(a, typ) if not isinstance(a, int) else a
        b = convert_float(b, typ) if not isinstance(b, int) else b

        run_test(dut,
                 comp_unittest(dut, a, b, FPUOperator.MINMAX, rm, expected,
                               nv),
                 sync=True)

    cmp = [
        # fleq (rm=4)
        (1.0, 1.0, 4, 1, 0),
        (1.0, 0.5, 4, 0, 0),
        (0.5, 1.0, 4, 1, 0),
        # fltq (rm=5)
        (1.0, 1.0, 5, 0, 0),
        (1.0, 0.5, 5, 0, 0),
        (0.5, 1.0, 5, 1, 0),
        # qNaN -> false, no NV
        (1.0, nan, 4, 0, 0),
        (nan, 1.0, 5, 0, 0),
        # sNaN -> false + NV
        (1.0, snan, 4, 0, 1),
        (snan, 1.0, 5, 0, 1),
    ]

    for a, b, rm, expected, nv in cmp:
        a = convert_float(a, typ) if not isinstance(a, int) else a
        b = convert_float(b, typ) if not isinstance(b, int) else b

        run_test(dut,
                 comp_unittest(dut, a, b, FPUOperator.CMP, rm, expected, nv),
                 sync=True)


def canonical_nan(fmt):
    if fmt == FPFormat.S:
        return 0x7fc00000
    if fmt == FPFormat.D:
        return 0x7ff8000000000000
    return 0x7e00


def test_fminmax_zfa_s():
    dut = FPUComp(32, FPFormat.S, use_zfa=True)
    comp_cases(dut, FPFormat.S, 'f', 32, canonical_nan(FPFormat.S),
               nan_bits('f', signaling=True))


def test_fminmax_zfa_d():
    dut = FPUComp(64, FPFormat.D, use_zfa=True)
    comp_cases(dut, FPFormat.D, 'd', 64, canonical_nan(FPFormat.D),
               nan_bits('d', signaling=True))
