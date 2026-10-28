"""Dedicated exact conversion pipelines for the FP32 epilogue (M3c).

The shared ``FPUCastMulti`` is a general unit: one op per cycle across
every format pair and integer width, with a renormalizer sized for the
widest case. The epilogue needs exactly two of its conversions, and
needs them on independent pipelines so a record's casts stop sharing
one issue slot:

* signed int32 -> FP32 (the segment scale ``S`` and Q4_K's
  ``mi * sum_a`` correction product), and
* FP16 -> FP32 widening (the per-lane ``d``/``dmin`` scales).

Both conversions are exact for every representable input the epilogue
produces, and the units match ``FPUCastMulti``'s results bit for bit
over the *entire* input space (verified exhaustively for FP16 and
over boundary/random int32 vectors in ``tests/troom/test_convert.py``):
int32 -> FP32 rounds (RNE) whenever |v| >= 2^24, and FP16 -> FP32
renormalizes subnormals and canonicalizes NaNs exactly as the shared
unit does. No bound on the record arithmetic is required for
correctness -- the units are full-range exact by construction.

Latency 2 (issue, compute, register), one input per cycle; the
epilogue's destination FIFO reservations cover the flight.
"""

from amaranth import *


class IntToFP32(Elaboratable):
    """Signed int32 -> FP32, round to nearest even; latency 2."""

    def __init__(self):
        self.inp_valid = Signal()
        self.inp = Signal(signed(32))

        self.out_valid = Signal()
        self.out = Signal(32)

    def elaborate(self, platform):
        m = Module()

        # S1: sign/magnitude, leading-one position, aligned 24-bit
        # significand with RNE round/sticky on the bits it drops.
        s1_valid = Signal()
        s1_inp = Signal(signed(32))
        m.d.sync += [
            s1_valid.eq(self.inp_valid),
            s1_inp.eq(self.inp),
        ]
        sign = Signal()
        mag = Signal(32)
        m.d.comb += [
            sign.eq(s1_inp[31]),
            mag.eq(Mux(sign, -s1_inp.as_unsigned(), s1_inp.as_unsigned())),
        ]

        msb = Signal(5)
        with m.If(mag == 0):
            m.d.comb += msb.eq(0)
        for i in range(1, 32):
            with m.If(mag[i]):
                m.d.comb += msb.eq(i)

        # Left shift (|v| < 2^23) or right shift with rounding
        # (|v| >= 2^24); msb == 23 shifts by zero either way.
        sh_l = Signal(5)
        sh_r = Signal(5)
        m.d.comb += [
            sh_l.eq(Mux(msb < 23, 23 - msb, 0)),
            sh_r.eq(Mux(msb > 23, msb - 23, 0)),
        ]

        g = Signal(24)  # leading one at bit 23 before rounding
        m.d.comb += g.eq(Mux(msb > 23, (mag >> sh_r)[:24], (mag << sh_l)[:24]))
        shr_m1 = Signal(5)
        m.d.comb += shr_m1.eq(sh_r - 1)
        round_bit = Signal()
        sticky = Signal()
        m.d.comb += [
            round_bit.eq((sh_r > 0) & mag.bit_select(shr_m1, 1)),
            sticky.eq((sh_r > 1)
                      & (mag & ((1 << shr_m1) - 1)).any()),
        ]
        carry_in = round_bit & (sticky | g[0])
        frac = Signal(25)
        m.d.comb += frac.eq(g + carry_in)

        # A rounding carry out of the 24-bit significand (only the
        # all-ones pattern can produce it) clears the fraction and
        # bumps the exponent by one.
        exp = Signal(8)
        m.d.comb += exp.eq((msb + 127)[:8] + frac[24])

        bits = Signal(32)
        with m.If(mag == 0):
            m.d.comb += bits.eq(0)  # int zero converts to +0
        with m.Else():
            m.d.comb += bits.eq(Cat(frac[:23], exp, sign))

        m.d.sync += [
            self.out_valid.eq(s1_valid),
            self.out.eq(bits),
        ]
        return m


class Fp16Widen(Elaboratable):
    """FP16 -> FP32 widening, exact for every bit pattern; latency 2.

    Matches ``FPUCastMulti`` F2F H->S on NaN-boxed inputs: normals
    shift the fraction, subnormals renormalize (exactly -- FP32's
    wider exponent swallows every FP16 subnormal), zeros/infinities
    pass through, and NaNs canonicalize exactly as the shared unit
    does (see test_convert.py for the observed bits).
    """

    def __init__(self):
        self.inp_valid = Signal()
        self.inp = Signal(16)

        self.out_valid = Signal()
        self.out = Signal(32)

    def elaborate(self, platform):
        m = Module()

        sign = Signal()
        e = Signal(5)
        frac = Signal(10)
        s1_valid = Signal()
        s1_inp = Signal(16)
        m.d.sync += [
            s1_valid.eq(self.inp_valid),
            s1_inp.eq(self.inp),
        ]
        m.d.comb += [
            sign.eq(s1_inp[15]),
            e.eq(s1_inp[10:15]),
            frac.eq(s1_inp[:10])
        ]

        # Subnormal renormalization: the leading one lands at bit 9.
        sh = Signal(4)
        with m.If(frac == 0):
            m.d.comb += sh.eq(0)
        for i in range(10):
            with m.If(frac[i]):
                m.d.comb += sh.eq(9 - i)
        norm = Signal(10)
        m.d.comb += norm.eq(frac << sh)

        # A subnormal's value is frac * 2^-24; after normalization its
        # significand is 1.norm[:9] at exponent -(15 + sh), so the
        # biased FP32 exponent is 112 - sh. Normals are 1.f at
        # exponent e - 15, i.e. e + 112 biased.
        exp = Signal(8)
        m.d.comb += exp.eq(Mux(e == 0, 112 - sh, Mux(e == 31, 255, e + 112)))

        bits = Signal(32)
        with m.If((frac == 0) & ((e == 0) | (e == 31))):
            # +-0 and +-Inf share the all-zero fraction path.
            m.d.comb += bits.eq(
                Cat(Const(0, 23), Mux(e == 31, Const(0xff, 8), Const(0, 8)),
                    sign))
        with m.Elif(e == 31):
            # NaN: canonical quiet NaN, sign dropped (the shared unit
            # canonicalizes rather than shifting the payload).
            m.d.comb += bits.eq(0x7fc00000)
        with m.Else():
            full = Signal(23)
            m.d.comb += full.eq(Mux(e == 0, norm[:9] << 14, frac << 13))
            m.d.comb += bits.eq(Cat(full, exp, sign))

        m.d.sync += [
            self.out_valid.eq(s1_valid),
            self.out.eq(bits),
        ]
        return m


class Fp32To16(Elaboratable):
    """FP32 -> FP16 narrowing, RNE for every bit pattern; latency 2.

    Mirrors the reference helper in ``tests/troom/model.py``
    (``fp32_to_fp16_rne_bits``) bit for bit: normals round their
    24-bit significand to 11 bits (one guard bit plus sticky, round to
    nearest even), values below the normal range round into the FP16
    subnormal grid 2^-24 (a carry out of it becomes the smallest
    normal), magnitudes at or past the half-way point above the
    largest finite become infinity, zeros and infinities pass through,
    and NaNs canonicalize to a quiet NaN with the sign kept (the
    payload drops, the quiet bit is forced).
    """

    def __init__(self):
        self.inp_valid = Signal()
        self.inp = Signal(32)

        self.out_valid = Signal()
        self.out = Signal(16)

    def elaborate(self, platform):
        m = Module()

        s1_valid = Signal()
        s1_inp = Signal(32)
        m.d.sync += [
            s1_valid.eq(self.inp_valid),
            s1_inp.eq(self.inp),
        ]

        sign = s1_inp[31]
        e = Signal(8)
        man = Signal(23)
        m.d.comb += [e.eq(s1_inp[23:31]), man.eq(s1_inp[:23])]
        sig = Signal(24)
        m.d.comb += sig.eq(Cat(man, Const(1, 1)))

        # Normal FP16 (FP32 exponents -14 .. 15): keep the top 11
        # significand bits, guard bit 12, sticky the 12 below it. The
        # rounded value may carry to 0x800, bumping the exponent and
        # zeroing the fraction (at the top exponent this overflows to
        # infinity, exactly the >= 65520 half-way rule).
        n_keep = sig[13:24]
        n_guard = sig[12]
        n_sticky = sig[:12].any()
        n_rne = Signal(12)
        m.d.comb += n_rne.eq(
            n_keep +
            Mux(n_guard & (n_sticky | n_keep[0]), Const(1, 12), Const(0, 12)))
        n_exp = (e - 127 + 15 + n_rne[11])[:5]
        n_frac = Mux(n_rne[11], Const(0, 10), n_rne[:10])

        # Subnormal FP16 (FP32 exponents below -14, but normal FP32):
        # one FP16 subnormal unit is 2^-24 and the value is
        # sig * 2^(e-150), so the scaled significand is sig shifted
        # right by 126 - e. The guard is the top dropped bit and the
        # sticky everything below it; rounding may carry to 1024,
        # which is the smallest normal.
        sh = (126 - e)[:7].as_unsigned()  # 126 - e for e <= 112
        sh_m1 = (sh - 1).as_unsigned()
        s_full = Signal(12)
        m.d.comb += s_full.eq((sig >> sh)[:12])
        s_guard = (sig >> sh_m1)[0]
        # Sticky: any bit of sig strictly below sh-1. For sh >= 25
        # every bit of sig is below it; otherwise shift the low bits
        # up so they stay inside 24.
        s_sticky = Mux(sh >= 25, sig.any(),
                       (sig << (25 - sh)[:7].as_unsigned())[:24].any())
        s_rne = Signal(13)
        m.d.comb += s_rne.eq(
            s_full +
            Mux(s_guard & (s_sticky | s_full[0]), Const(1, 13), Const(0, 13)))

        bits = Signal(16)
        with m.If(e == 255):
            # NaN -> quiet NaN (sign kept, quiet bit = F[9]); infinity
            # passes through.
            m.d.comb += bits.eq(
                Cat(Const(0, 9), man.any(), Const(0x1f, 5), sign))
        with m.Elif(e == 0):
            # FP32 zeros and subnormals round to signed zero: the
            # largest FP32 subnormal is far below the FP16 grid.
            m.d.comb += bits.eq(Cat(Const(0, 15), sign))
        with m.Elif(e >= 127 + 16):
            # At or past 2^16 the value cannot round below the
            # half-way point above the largest finite FP16.
            m.d.comb += bits.eq(Cat(Const(0, 10), Const(0x1f, 5), sign))
        with m.Elif(e >= 127 - 14):
            m.d.comb += bits.eq(Cat(n_frac, n_exp, sign))
        with m.Else():
            # A carry to 1024 lands on the smallest normal (E=1, F=0);
            # otherwise the subnormal fraction is the rounded value.
            m.d.comb += bits.eq(
                Cat(Mux(s_rne[10], Const(0, 10), s_rne[:10]), s_rne[10],
                    Const(0, 4), sign))

        m.d.sync += [
            self.out_valid.eq(s1_valid),
            self.out.eq(bits),
        ]
        return m
