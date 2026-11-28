"""Dedicated FP32 multiply and add units for TROOM's fixed-role FP sites.

``FPMul32`` and ``FPAdd32`` stand in for an ``FPUFMA`` whose operator is
tied to ``MUL`` or ``ADD`` under round-to-nearest-even, which is how the
matrix epilogue, VBUILD and the RoPE builder use the generic block. Each
keeps the ``FPUFMA`` Valid interface and three-cycle latency: the inputs
are registered on the first edge, the result on the third, no stage
stalls, and ``out.bits.status`` is never read.

Bit contract
------------
Both units reproduce ``FPUFMA`` exactly: round-to-nearest-even in IEEE
754 order of operations, and a canonical NaN (``0x7fc00000``, sign
cleared) for every NaN result rather than a propagated payload.
``FPUFMA`` itself matched the host's IEEE 754 hardware on every vector
it was compared with, so there is no quirk to copy.

Structure
---------
Each unit is three register stages with two combinational spans between
them, as in ``FPUFMA``:

    R0  raw operands
    R1  significand product (multiplier) / aligned sum (adder)
    R2  packed result

The last span is the one that limits ``FPUFMA``: leading-zero count,
normalisation shift and rounding share one cycle. Here it is a
three-level priority tree, one barrel shift that yields only the 25 bits
rounding needs (a round bit over the significand) and the rounding
increment. The shift is taken once, out of the raw R1 magnitude, so the
round bit is ``x[s]`` and the sticky is the OR of ``x`` below ``s`` and
nothing is dropped: the first prototypes shifted a normalised value a
second time and lost the product's low bits.

In the adder the larger magnitude is the minuend and the smaller
operand's shifted-out bits enter the subtraction as a borrow-in, so the
R1 sum is already a non-negative magnitude and R2 has no negation.
"""

from amaranth import *

from room.fpu import FPUInput, FPUResult
from roomsoc.interconnect.stream import Valid

NAN32 = 0x7fc00000
INF32 = 0x7f800000


def _flags(x):
    """Class of an FP32 word by its zero and all-ones exponent fields."""
    man, exp = x[:23], x[23:31]
    return dict(
        zero=(exp == 0) & (man == 0),
        e0=exp == 0,
        inf=(exp == 255) & (man == 0),
        nan=(exp == 255) & (man != 0),
    )


def _msb(m, x, group=6):
    """Index of the most significant set bit of ``x``; 0 when ``x`` is zero.

    A three-level priority tree. Each group of ``group`` bits reports
    whether it holds a one and encodes the index of its leading one; the
    highest nonzero group's code is selected.
    """
    n = len(x)
    width = max(1, (n - 1).bit_length())
    nz = []
    for lo in range(0, n, group):
        has = Signal()
        m.d.comb += has.eq(x[lo:lo + group].any())
        nz.append(has)
    bits = [Const(0, 1)] * width
    for g, lo in enumerate(range(0, n, group)):
        hi = min(n, lo + group)
        hit = Signal()
        m.d.comb += hit.eq(nz[g] & ~Cat(*nz[g + 1:]).any())
        lead = []
        for i in range(lo, hi):
            one = Signal()
            m.d.comb += one.eq(x[i] & ~x[i + 1:hi].any())
            lead.append(one)
        for b in range(width):
            sel = [lead[i - lo] for i in range(lo, hi) if (i >> b) & 1]
            if sel:
                code = Signal()
                m.d.comb += code.eq(Cat(*sel).any())
                bits[b] = bits[b] | (hit & code)
    msb = Signal(width)
    m.d.comb += msb.eq(Cat(*bits))
    return msb


def _or_tree(m, x):
    """OR of every aligned power-of-two block of ``x``, smallest blocks first.

    ``levels[k][j]`` is the OR of ``x[j * 2**k : (j + 1) * 2**k]``. One
    tree serves every count asked of the same ``x``.
    """
    levels = [[x[i] for i in range(len(x))]]
    while len(levels[-1]) > 1:
        prev = levels[-1]
        nxt = []
        for j in range((len(prev) + 1) // 2):
            bit = Signal()
            m.d.comb += bit.eq(prev[2 * j] | (prev[2 * j + 1] if 2 * j +
                                              1 < len(prev) else Const(0)))
            nxt.append(bit)
        levels.append(nxt)
    return levels


def _low_or(m, x, t, levels=None):
    """OR of the low ``t`` bits of ``x``; ``t`` is unsigned.

    ``t`` at or above ``len(x)`` selects all of ``x``. The count is taken
    apart in binary from its top: for each set bit ``k`` of ``t`` the
    block of ``2**k`` bits starting at the next multiple of ``2**(k+1)``
    belongs to the range. Only those aligned blocks are selected, so the
    cost is the shared tree plus one narrow mux per bit of ``t``.
    """
    if levels is None:
        levels = _or_tree(m, x)
    terms = []
    for k in range(len(t)):
        q = levels[k]
        entries = []
        for j in range(1 << (len(t) - k - 1)):
            entries.append(q[2 * j] if 2 * j < len(q) else Const(0))
        term = Signal()
        m.d.comb += term.eq(Mux(t[k], Array(entries)[t[k + 1:]], Const(0)))
        terms.append(term)
    out = Signal()
    m.d.comb += out.eq(Cat(*terms).any())
    return out


def _window(m, x, s, pad, width):
    """Bits ``Cat(0 * pad, x)[s + j]`` for ``j`` below ``width``.

    ``x`` sits ``pad`` places up, so ``s`` below ``pad`` reaches zero
    fill and ``s - pad`` indexes ``x``. One right shift by ``s`` that
    produces only the ``width`` bits asked for: a coarse shift by whole
    eights, then a fine shift by the low three bits.
    """
    u = Cat(Const(0, pad), x)
    # Every stage is a signal: an expression reused inside the next
    # Mux would be elaborated again for each use.
    span = width + 7
    coarse = Signal(span)
    for bit in range(span):
        entries = [
            u[bit + 8 * k] if bit + 8 * k < len(u) else Const(0)
            for k in range(1 << len(s[3:]))
        ]
        m.d.comb += coarse[bit].eq(Array(entries)[s[3:]])
    w = Signal(span + 7)
    m.d.comb += w.eq(coarse)
    for bit in range(3):
        nxt = Signal(span + 7)
        m.d.comb += nxt.eq(
            Mux(s[bit], Cat(w[1 << bit:], Const(0, 1 << bit)), w))
        w = nxt
    return w[:width]


def _rounded(m, normal, e_base, win, sticky, exp_add, of_a, of_b, sign):
    """Round a window to nearest even and pack it with ``sign``.

    ``win`` is a round bit over a 24-bit significand, already in the
    result's scale, and ``sticky`` everything below the round bit.

    One adder rounds the significand and the exponent together. Its low
    24 bits hold the significand, bit 24 is a one, and bits 33 to 25
    hold the exponent before rounding: ``e_base`` for a normal result
    and zero for a subnormal one. ``exp_add`` adds one more to a normal
    exponent, in bit 25. The rounding increment goes into bit 0. A
    normal significand has its leading one at bit 23; its carry out of
    bit 23 turns the one at bit 24 into a zero and ripples into the
    exponent. So the exponent's unit is that carry, and bit 24 reads
    zero exactly when the significand carried. A subnormal result is an
    integer count of ``2**-149`` below ``2**23`` with bit 23 zero; its
    carry out of bit 22 stops at bit 23, which is then the exponent's
    least significant bit and makes the smallest normal number. The
    fraction is zero after either carry.

    ``of_a`` and ``of_b`` predict the overflow test from the exponent
    before rounding, gated by ``normal``: ``of_a`` when it is already
    255 or more, ``of_b`` when it is 254, in which case the rounding
    carry decides. That is IEEE 754's overflow test.
    """
    rnd = win[0]
    keep = win[1:25]
    inc = rnd & (sticky | keep[0])
    total = Signal(34)
    m.d.comb += total.eq(
        Cat(keep, Const(1, 1), Mux(normal, e_base[:9], 0)) +
        Cat(inc, Const(0, 24), normal & exp_add))
    exp_lsb = Mux(normal, total[25], total[23])
    return (Cat(total[:23], exp_lsb, total[26:33],
                sign), of_a | (of_b & ~total[24]))


class _Unit(Elaboratable):

    def __init__(self):
        self.inp = Valid(FPUInput, 32)
        self.out = Valid(FPUResult, 32)


class FPMul32(_Unit):
    """``RNE(in1 * in2)`` as FP32; three cycles."""

    def elaborate(self, platform):
        m = Module()
        op_a = Signal(32)
        op_b = Signal(32)
        v1 = Signal()
        m.d.sync += [
            op_a.eq(self.inp.bits.in1),
            op_b.eq(self.inp.bits.in2),
            v1.eq(self.inp.valid)
        ]

        # R1: the 48-bit significand product, and everything the last
        # span needs that depends on the operands alone. A subnormal
        # operand is one binade below what its zero exponent field says,
        # so ``em + msb(prod)`` is the biased exponent before rounding.
        fa, fb = _flags(op_a), _flags(op_b)
        sa = Cat(op_a[:23], ~fa['e0'])
        sb = Cat(op_b[:23], ~fb['e0'])
        # A product of significands with leading ones at ``ma`` and
        # ``mb`` has its leading one at ``ma + mb`` or one above, so the
        # last span needs one product bit, not a 48-bit priority tree.
        base = _msb(m, sa) + _msb(m, sb)
        em = Signal(signed(10))
        eb1 = Signal(signed(11))
        m.d.comb += [
            em.eq(op_a[23:31] + op_b[23:31] - 173 + fa['e0'] + fb['e0']),
            eb1.eq(em + base),
        ]
        prod = Signal(48)
        base_r = Signal(6)
        eb1_r = Signal(signed(11))
        # Sticky counts under the round bit for either leading-one
        # position of a normal result.
        tn0 = Signal(5)
        tn1 = Signal(5)
        # A subnormal result keeps the operands' scale: the window
        # starts 47 places below the product's unit and anything past
        # the product rounds to zero.
        s_sub = Signal(7)
        t_sub = Signal(6)
        # Exponent facts of a normal result, before its last bit.
        n_ge1 = Signal()
        n_eq0 = Signal()
        ov255 = Signal()
        ov254 = Signal()
        ov253 = Signal()
        sign = Signal()
        is_nan = Signal()
        is_inf = Signal()
        is_zero = Signal()
        v2 = Signal()
        m.d.sync += [
            prod.eq(sa * sb),
            base_r.eq(base),
            eb1_r.eq(eb1),
            tn0.eq(Mux(base >= 24, base - 24, 0)),
            tn1.eq(Mux(base >= 23, base - 23, 0)),
            s_sub.eq(Mux(em >= -71, 1 - em, 72)),
            t_sub.eq(Mux(em >= -23, 0, Mux(em > -86, -23 - em, 63))),
            n_ge1.eq(eb1 >= 1),
            n_eq0.eq(eb1 == 0),
            ov255.eq(eb1 >= 255),
            ov254.eq(eb1 == 254),
            ov253.eq(eb1 == 253),
            sign.eq(op_a[31] ^ op_b[31]),
            is_nan.eq(fa['nan'] | fb['nan'] | (fa['inf'] & fb['zero'])
                      | (fb['inf'] & fa['zero'])),
            is_inf.eq(fa['inf'] | fb['inf']),
            is_zero.eq(fa['zero'] | fb['zero']),
            v2.eq(v1),
        ]

        # R2: normalise, round and pack. ``up`` is the product bit that
        # says whether the leading one is at ``base_r`` or one above.
        up = _window(m, prod, base_r + 1, 0, 1)
        normal = n_ge1 | (n_eq0 & up)
        # The shift is taken in two steps so that its main part starts
        # from a registered amount. With ``eb1_r`` at least zero the
        # window is first shifted by ``base_r``, then one place down
        # when ``up`` says the leading one is above ``base_r``. When
        # ``eb1_r`` is zero that one place applies either way: with
        # ``up`` set the result is normal at ``base_r + 1``, without it
        # subnormal on the same window. With ``eb1_r`` negative the
        # result is subnormal whatever ``up`` says, at the operands'
        # scale, and needs no second step.
        first = Signal(7)
        m.d.comb += first.eq(Mux(n_ge1 | n_eq0, base_r, s_sub))
        shifted = _window(m, prod, first, 24, 26)
        again = Mux(n_eq0, 1, Mux(n_ge1, up, 0))
        win = Signal(25)
        m.d.comb += win.eq(Mux(again, shifted[1:26], shifted[0:25]))

        # Sticky: every product bit below the round bit.
        tree = _or_tree(m, prod)
        sticky = Mux(
            normal,
            Mux(up, _low_or(m, prod, tn1, tree), _low_or(m, prod, tn0, tree)),
            _low_or(m, prod, t_sub, tree))
        of_a = normal & (ov255 | (ov254 & up))
        of_b = normal & ((ov254 & ~up) | (ov253 & up))
        result, overflow = _rounded(m, normal, eb1_r, win, sticky, up, of_a,
                                    of_b, sign)

        # The flag cases are packed beside the arithmetic: only the
        # regular result waits on ``overflow``.
        inf = Cat(Const(INF32, 31), sign)
        special = Mux(is_nan, NAN32, Mux(is_inf, inf, Cat(Const(0, 31), sign)))
        data = Mux(is_nan | is_inf | is_zero, special,
                   Mux(overflow, inf, result))
        m.d.sync += [
            self.out.valid.eq(v2),
            self.out.bits.data.eq(data),
            self.out.bits.status.eq(0),
        ]
        return m


class FPAdd32(_Unit):
    """``RNE(in1 + in3)``, with ``fn_mod`` negating ``in3``; three cycles."""

    def elaborate(self, platform):
        m = Module()
        op_a = Signal(32)
        op_b = Signal(32)
        v1 = Signal()
        m.d.sync += [
            op_a.eq(self.inp.bits.in1),
            op_b.eq(
                Cat(self.inp.bits.in3[:31],
                    self.inp.bits.in3[31] ^ self.inp.bits.fn_mod)),
            v1.eq(self.inp.valid),
        ]

        # R1: the aligned sum on a 27-bit significand with three guard
        # digits (28 bits). The larger magnitude is the minuend, so an
        # effective subtraction never borrows: the bits shifted out of
        # the smaller operand are OR-reduced into a sticky that the
        # subtraction takes as its borrow-in.
        fa, fb = _flags(op_a), _flags(op_b)
        sub = op_a[31] ^ op_b[31]
        swap = op_b[:31].as_unsigned() > op_a[:31].as_unsigned()
        big = Signal(32)
        small = Signal(32)
        m.d.comb += [
            big.eq(Mux(swap, op_b, op_a)),
            small.eq(Mux(swap, op_a, op_b))
        ]
        big_e, small_e = big[23:31], small[23:31]
        eb = Signal(8)
        es = Signal(8)
        m.d.comb += [
            eb.eq(Mux(big_e == 0, 1, big_e)),
            es.eq(Mux(small_e == 0, 1, small_e)),
        ]
        bm = Cat(big[:23], big_e != 0)
        sm = Cat(small[:23], small_e != 0)

        # Alignment: both significands at the working point, one eighth
        # of the larger operand's LSB, and the smaller one shifted right
        # by at most 27 places; beyond that it is all sticky.
        d = Signal(9)
        m.d.comb += d.eq(eb - es)
        dc = Signal(5)
        m.d.comb += dc.eq(Mux(d >= 28, 27, d[:5]))
        small_w = Signal(27)
        aligned = Signal(27)
        m.d.comb += [
            small_w.eq(sm << 3),
            aligned.eq(_window(m, small_w, dc, 0, 27)),
        ]
        sticky_in = _low_or(m, small_w, dc)

        big_w = bm << 3
        work_r = Signal(28)
        is_nan = Signal()
        inf_a = Signal()
        inf_b = Signal()
        both_zero = Signal()
        zero_sign = Signal()
        big_sign = Signal()
        sign_a = Signal()
        sign_b = Signal()
        sticky = Signal()
        eb_r = Signal(8)
        # Window start of a subnormal result: the larger operand's scale.
        s_sub = Signal(5)
        v2 = Signal()
        # Decode the low exponent bits directly. A full-width subtraction
        # lets Vivado pack the exponent mux and offset into a feedback carry.
        subnormal_offset = Cat(
            *(Const(sum(((max(0, 27 - i) >> bit) & 1) << i
                        for i in range(32)), 32).bit_select(eb[:5], 1)
              for bit in range(5)))
        m.d.sync += [
            work_r.eq(Mux(sub, big_w - aligned - sticky_in, big_w + aligned)),
            s_sub.eq(Mux(eb[5:].any(), 0, subnormal_offset)),
            is_nan.eq(fa['nan'] | fb['nan'] | (fa['inf'] & fb['inf'] & sub)),
            inf_a.eq(fa['inf']),
            inf_b.eq(fb['inf']),
            both_zero.eq(fa['zero'] & fb['zero']),
            zero_sign.eq(op_a[31] & op_b[31]),
            big_sign.eq(big[31]),
            sign_a.eq(op_a[31]),
            sign_b.eq(op_b[31]),
            sticky.eq(sticky_in),
            eb_r.eq(eb),
            v2.eq(v1),
        ]

        # R2: normalise, round and pack. A normal result is normalised,
        # so the window starts at the sum's leading one and ``eb_r - 26 +
        # idx`` is the biased exponent before rounding. A subnormal
        # result keeps the larger operand's scale, a registered amount:
        # its significand is the sum shifted by ``eb_r - 4``.
        idx = _msb(m, work_r)
        e1 = Signal(signed(9))
        m.d.comb += e1.eq(eb_r - 26 + idx)
        normal = e1 >= 1
        s = Signal(5)
        m.d.comb += s.eq(Mux(normal, idx, s_sub))
        win = Signal(25)
        m.d.comb += win.eq(_window(m, work_r, s, 24, 25))

        # Only a window that starts below the working point can leave
        # bits of the sum under the round bit: ``s`` of 24 to 27.
        below = (s[4] & s[3]) & _low_or(m, work_r[:3], s[:2])

        of_a = normal & (e1 >= 255)
        of_b = normal & (e1 == 254)
        result, overflow = _rounded(m, normal, e1, win, sticky | below, 0,
                                    of_a, of_b, big_sign)

        # A zero sum is +0 in RNE when it is exact (an effective
        # subtraction of equals); two zeros keep the common sign, and an
        # inexact zero the sign of the larger operand. The flag cases and
        # the zero sum are packed beside the arithmetic: only the regular
        # result waits on ``overflow``.
        is_zero = Signal()
        m.d.comb += is_zero.eq(work_r == 0)
        inf = Cat(Const(INF32, 31), big_sign)
        special = Mux(
            is_nan, NAN32,
            Mux(
                inf_a, Cat(Const(INF32, 31), sign_a),
                Mux(
                    inf_b, Cat(Const(INF32, 31), sign_b),
                    Mux(
                        both_zero, Cat(Const(0, 31), zero_sign),
                        Mux(is_zero & ~sticky, Cat(Const(0, 31), 0),
                            Cat(Const(0, 31), big_sign))))))
        data = Mux(is_nan | inf_a | inf_b | both_zero | is_zero, special,
                   Mux(overflow, inf, result))
        m.d.sync += [
            self.out.valid.eq(v2),
            self.out.bits.data.eq(data),
            self.out.bits.status.eq(0),
        ]
        return m
