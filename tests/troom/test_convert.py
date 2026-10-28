"""Dedicated conversion units vs the shared FPUCastMulti (M3c).

``Fp16Widen`` is compared against FPUCastMulti F2F H->S over ALL
65536 FP16 bit patterns; ``IntToFP32`` against I2F INT32->S over
boundary and random vectors including every exponent transition and
bothINT32 extremes (exhaustive over 2^32 is infeasible; the RNE path
only engages above 2^23, and every rounding configuration -- exact,
round-down, round-up-tie-even, round-up, carry -- is covered).
"""

import random

from amaranth import *
from amaranth.sim import Passive, Simulator

from room.consts import RoundingMode
from room.fpu import FPUCastMulti, FPFormat, FPUOperator, IntFormat

from troom.convert import Fp16Widen, Fp32To16, IntToFP32


def run_cast_vs_ref(cases, drive):
    """Drive one reference FPUCastMulti op over ``cases``; return the
    list of (input, ref_bits) pairs, one per case, in order."""

    class DUT(Elaboratable):

        def __init__(self):
            self.ref = FPUCastMulti(32, FPFormat.S, latency=3)

        def elaborate(self, platform):
            m = Module()
            m.submodules.ref = self.ref
            return m

    dut = DUT()
    got = [None] * len(cases)

    def tx():
        yield dut.ref.inp.bits.rm.eq(RoundingMode.RNE)
        yield dut.ref.inp.bits.fn_mod.eq(0)
        yield from drive(dut)  # static bits (fn, formats)
        # Prime the pipeline, then stream one op per cycle.
        yield
        for i, c in enumerate(cases):
            yield from drive(dut, c)
            yield dut.ref.inp.valid.eq(1)
            yield
        yield dut.ref.inp.valid.eq(0)
        for _ in range(8):
            yield

    def rx():
        yield Passive()
        i = 0
        seen = 0
        while seen < len(cases):
            yield
            if (yield dut.ref.out.valid):
                got[i] = (yield dut.ref.out.bits.data)
                i += 1
                seen += 1

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(tx)
    sim.add_sync_process(rx)
    sim.run()
    return got


def run_widen(cases):

    class DUT(Elaboratable):

        def __init__(self):
            self.u = Fp16Widen()

        def elaborate(self, platform):
            m = Module()
            m.submodules.u = self.u
            return m

    dut = DUT()
    got = [None] * len(cases)

    def tx():
        yield
        for i, c in enumerate(cases):
            yield dut.u.inp_valid.eq(1)
            yield dut.u.inp.eq(c)
            yield
        yield dut.u.inp_valid.eq(0)
        for _ in range(4):
            yield

    def rx():
        yield Passive()
        i = 0
        while i < len(cases):
            yield
            if (yield dut.u.out_valid):
                got[i] = (yield dut.u.out)
                i += 1

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(tx)
    sim.add_sync_process(rx)
    sim.run()
    return got


def run_i2f(cases):

    class DUT(Elaboratable):

        def __init__(self):
            self.u = IntToFP32()

        def elaborate(self, platform):
            m = Module()
            m.submodules.u = self.u
            return m

    dut = DUT()
    got = [None] * len(cases)

    def tx():
        yield
        for c in cases:
            yield dut.u.inp_valid.eq(1)
            yield dut.u.inp.eq(c)
            yield
        yield dut.u.inp_valid.eq(0)
        for _ in range(4):
            yield

    def rx():
        yield Passive()
        i = 0
        while i < len(cases):
            yield
            if (yield dut.u.out_valid):
                got[i] = (yield dut.u.out)
                i += 1

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(tx)
    sim.add_sync_process(rx)
    sim.run()
    return got


def ref_f2f(cases):

    def drive(dut, c=None):
        yield dut.ref.inp.bits.fn.eq(FPUOperator.F2F)
        yield dut.ref.inp.bits.src_fmt.eq(FPFormat.H)
        yield dut.ref.inp.bits.dst_fmt.eq(FPFormat.S)
        if c is not None:
            yield dut.ref.inp.bits.in1.eq(c | 0xFFFF0000)

    return run_cast_vs_ref(cases, drive)


def ref_i2f(cases):

    def drive(dut, c=None):
        yield dut.ref.inp.bits.fn.eq(FPUOperator.I2F)
        yield dut.ref.inp.bits.int_fmt.eq(IntFormat.INT32)
        yield dut.ref.inp.bits.dst_fmt.eq(FPFormat.S)
        if c is not None:
            yield dut.ref.inp.bits.in1.eq(c & 0xFFFFFFFF)

    return run_cast_vs_ref(cases, drive)


def test_fp16_widen_exhaustive():
    cases = list(range(1 << 16))
    ref = ref_f2f(cases)
    got = run_widen(cases)
    bad = [(c, hex(g), hex(r)) for c, g, r in zip(cases, got, ref) if g != r]
    assert not bad, f'{len(bad)} mismatches, first 8: {bad[:8]}'


def test_int_to_fp32_boundaries():
    rng = random.Random(7)
    cases = [0, 1, -1, 0x7FFFFFFF, -0x80000000]
    for k in range(0, 32):
        for d in (-2, -1, 0, 1, 2):
            for b in (1 << k, -(1 << k)):
                v = b + d
                if -0x80000000 <= v <= 0x7FFFFFFF:
                    cases.append(v)
    for k in range(0, 32):
        base = 1 << k
        for v in (base + (1 << max(k - 26, 0)),
                  -(base + (1 << max(k - 26, 0)))):
            if -0x80000000 <= v <= 0x7FFFFFFF:
                cases.append(v)
    cases += [rng.randint(-0x80000000, 0x7FFFFFFF) for _ in range(4096)]
    cases = sorted(set(cases))
    ref = ref_i2f(cases)
    got = run_i2f(cases)
    bad = [(c, hex(g), hex(r)) for c, g, r in zip(cases, got, ref) if g != r]
    assert not bad, f'{len(bad)} mismatches, first 8: {bad[:8]}'


def run_narrow(cases):
    """Fp32To16 over ``cases`` (32-bit words) -> 16-bit outputs."""

    class DUT(Elaboratable):

        def __init__(self):
            self.u = Fp32To16()

        def elaborate(self, platform):
            m = Module()
            m.submodules.u = self.u
            return m

    dut = DUT()
    got = [None] * len(cases)

    def tx():
        yield
        for c in cases:
            yield dut.u.inp_valid.eq(1)
            yield dut.u.inp.eq(c)
            yield
        yield dut.u.inp_valid.eq(0)
        for _ in range(4):
            yield

    def rx():
        yield Passive()
        i = 0
        while i < len(cases):
            yield
            if (yield dut.u.out_valid):
                got[i] = (yield dut.u.out)
                i += 1

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(tx)
    sim.add_sync_process(rx)
    sim.run()
    return got


def test_fp32_to_fp16_subnormal_carry():
    # The exact midpoint rounds up to the even smallest normal; check
    # both signs and the FP32 values immediately around the transition.
    cases = [0x387fdfff, 0x387fe000, 0x387fe001, 0x387fffff, 0x38800000]
    cases += [c | 0x80000000 for c in cases]
    assert run_narrow(cases) == [
        0x03ff,
        0x0400,
        0x0400,
        0x0400,
        0x0400,
        0x83ff,
        0x8400,
        0x8400,
        0x8400,
        0x8400,
    ]


def test_fp32_to_fp16_rne():
    """Fp32To16 vs the frozen model helper over round-trips, every
    rounding boundary, specials, and random vectors."""
    from tests.troom.model import fp32_to_fp16_rne_bits

    rng = random.Random(11)
    cases = []

    # Every FP16 pattern widens and narrows back to itself.
    for h in range(1 << 16):
        from tests.troom.model import fp16_to_f32, f32_bits
        cases.append(f32_bits(fp16_to_f32(h)))

    # Rounding boundaries in each FP32 exponent region: the guard
    # bit, exact ties (with both parities of the kept LSB), and one
    # off each side, across normals and the subnormal crossover.
    for e in range(1, 160):
        for low in (0x0ffe, 0x0fff, 0x1000, 0x1001, 0x1002, 0x1ffe, 0x1fff,
                    0x2000, 0x2001, 0x2ffe):
            cases.append((e << 23) | low)
            cases.append((1 << 31) | (e << 23) | low)

    # Specials and the extreme exponents.
    cases += [
        0x00000000, 0x80000000, 0x007fffff, 0x807fffff, 0x7f800000, 0xff800000,
        0x7fc00000, 0xffc00001, 0x7fffffff, 0xffffffff
    ]
    for _ in range(8192):
        cases.append(rng.randint(0, 0xffffffff))

    cases = sorted(set(cases))
    ref = [fp32_to_fp16_rne_bits(c) for c in cases]
    got = run_narrow(cases)
    bad = [(hex(c), hex(g), hex(r)) for c, g, r in zip(cases, got, ref)
           if g != r]
    assert not bad, f'{len(bad)} mismatches, first 8: {bad[:8]}'

    # The rounding behavior for finite values inside the FP16 range
    # also agrees with CPython's own RNE float16 conversion.
    import struct
    checked = 0
    for c, r in zip(cases, ref):
        f = struct.unpack('<f', struct.pack('<I', c))[0]
        if not (f == f and abs(f) != float('inf')) or abs(f) >= 65520:
            continue
        want = struct.unpack('<H', struct.pack('<e', f))[0]
        assert r == want, (hex(c), hex(r), hex(want))
        checked += 1
    assert checked > 10000
