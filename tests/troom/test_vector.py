"""Tests for the troom vector/reduction unit and its command contract.

Three layers, following the suite's conventions:

* the numerical contract in ``tests/troom/model.py`` checked against
  Python float semantics (ordinary values) and against hand-derived
  policies for NaN, infinities, signed zeros, overflow, and the
  ``isum_4`` placement (including a case where it provably differs
  from a sequential sum),
* the RTL unit against that model bit for bit: every elementwise op
  over a pool of special FP32 values (all ordered pairs), reductions
  under write backpressure, final-beat zero padding, and descriptor
  rejections,
* the full command path: ``LOAD_2D`` regions, ``VECTOR``/``REDUCE``
  through ``Core``, and ``STORE_2D`` of the results, compared against
  the model in DRAM -- plus a vector op consuming a matmul's result
  region, a concurrent store sharing the TMEM read port, back-to-back
  vector commands, bad-class and overlap rejection, and the
  deployment beat width.
"""

from amaranth import *
from amaranth.sim import Passive, Simulator

import pytest

from troom.consts import Format, Opcode, Status, VecOp
from troom.tmem import TMEM
from troom.types import HasTRoomParams
from troom.vector import VectorUnit

from tests.troom.model import (
    FP32_NAN_BITS,
    TRoomModel,
    bits_to_f32,
    f32,
    f32_bits,
    fp32_add_bits,
    fp32_max_bits,
    fp32_mul_bits,
    mm_row_results,
    mm_segments,
    pack_row,
    rdc_result,
    vec_word,
    vec_recip_bits,
    vec_rsqrt_bits,
    vec_exp2_bits,
)
from tests.troom.test_full_path import (
    DRAM_SIZE,
    PARAMS,
    R_BASE,
    Harness,
    build_program_image,
    matmul_program,
    run_program,
    send_cmd,
)
from tests.troom.test_matrix import gen_wsegs
from tests.troom.model import build_act

# Hand-picked FP32 bit patterns covering the policy corners: signed
# zeros, subnormals, the exponent extremes, infinities, quiet and
# signalling NaN payloads, and the tie values 2^-24 / 2^-23.
SPECIALS = [
    0x00000000,  # +0
    0x80000000,  # -0
    0x3f800000,  # 1.0
    0xbf800000,  # -1.0
    0x00000001,  # smallest subnormal
    0x007fffff,  # largest subnormal
    0x00800000,  # smallest normal
    0x33800000,  # 2^-24 (tie bit of 1.0)
    0x34000000,  # 2^-23 (ulp of 1.0)
    0x7f7fffff,  # largest finite
    0xff7fffff,  # largest finite, negative
    0x7f800000,  # +inf
    0xff800000,  # -inf
    0x7fc00000,  # canonical qNaN
    0x7f800001,  # sNaN
    0x7fc00001,  # qNaN with payload
]


def words_to_bytes(words):
    return b''.join(w.to_bytes(4, 'little') for w in words)


def words_from_bytes(data):
    return [
        int.from_bytes(data[4 * i:4 * i + 4], 'little')
        for i in range(len(data) // 4)
    ]


# ---------------------------------------------------------------------------
# Model-level numerical contract
# ---------------------------------------------------------------------------


def test_fp32_primitives_match_python_rounding():
    import random

    rng = random.Random(1)
    maxf = f32(3.4e38)
    for _ in range(2000):
        a = f32_bits(f32(rng.uniform(-1e30, 1e30)))
        b = f32_bits(f32(rng.uniform(-1e30, 1e30)))
        s = bits_to_f32(a) + bits_to_f32(b)
        p = bits_to_f32(a) * bits_to_f32(b)
        # Values that stay finite in binary64 and binary64->binary32
        # rounding is exact for these (53 >= 2*24 + 2 bits).
        if abs(s) <= maxf:
            assert fp32_add_bits(a, b) == f32_bits(s)
        if abs(p) <= maxf and p != 0:
            assert fp32_mul_bits(a, b) == f32_bits(p)


def test_fp32_special_value_policies():
    qnan = 0x7fc00000
    inf = 0x7f800000
    ninf = 0xff800000

    # NaN propagates canonically whatever the payload.
    for x in (0x7fc00001, 0x7f800001):
        assert fp32_add_bits(x, 0x3f800000) == qnan
        assert fp32_add_bits(0x3f800000, x) == qnan
        assert fp32_mul_bits(x, 0x3f800000) == qnan
        assert fp32_mul_bits(0x3f800000, x) == qnan
        assert fp32_max_bits(x, 0x3f800000) == 0x3f800000
        assert fp32_max_bits(0x3f800000, x) == 0x3f800000
    assert fp32_max_bits(0x7f800001, 0x7fc00001) == qnan

    # Invalid operations.
    assert fp32_add_bits(inf, ninf) == qnan
    assert fp32_mul_bits(0x00000000, inf) == qnan
    assert fp32_mul_bits(inf, 0x80000000) == qnan

    # Infinities and overflow.
    assert fp32_add_bits(inf, 0x3f800000) == inf
    assert fp32_add_bits(0xff7fffff, 0xff7fffff) == ninf
    assert fp32_add_bits(0x7f7fffff, 0x7f7fffff) == inf
    assert fp32_mul_bits(0x7f7fffff, 0x40000000) == inf
    assert fp32_mul_bits(0xff7fffff, 0x40000000) == ninf

    # Signed zeros: an exact cancellation under RNE yields +0, and
    # the maximum prefers +0; a zero product keeps the product sign.
    assert fp32_mul_bits(0x80000000, 0x3f800000) == 0x80000000
    assert fp32_mul_bits(0x80000000, 0x80000000) == 0x00000000
    assert fp32_mul_bits(0x3f800000, 0x80000000) == 0x80000000
    assert fp32_add_bits(0x80000000, 0x3f800000) == 0x3f800000
    assert fp32_add_bits(0x80000000, 0x80000000) == 0x80000000
    assert fp32_max_bits(0x80000000, 0x00000000) == 0x00000000
    assert fp32_max_bits(0x00000000, 0x80000000) == 0x00000000
    assert fp32_max_bits(0xff800000, 0xff7fffff) == 0xff7fffff

    # Subnormals round and stay exact where representable.
    assert fp32_add_bits(0x00000001, 0x00000001) == 0x00000002
    assert fp32_add_bits(0x00000001, 0x00000000) == 0x00000001


def test_rdc_isum4_placement_differs_from_sequential():
    """One plus four tie bits: isum_4 keeps the exact 2^-23 that a
    sequential sum rounds away."""
    words = [0x3f800000] + [0x33800000] * 4  # 1.0 + 4 * 2^-24
    seq = 0x3f800000
    for w in words[1:]:
        seq = fp32_add_bits(seq, w)
    assert seq == 0x3f800000
    assert rdc_result(VecOp.RDC_SUM, words) == 0x3f800001  # 1 + 2^-23


def test_rdc_sumsq_and_max_contracts():
    words = [0x3f800000, 0xbf800000, 0x40000000, 0x00000000]
    # Squares: 1, 1, 4, 0 -> partials [1, 1, 4, 0]; fold 5 + 1 = 6.
    assert rdc_result(VecOp.RDC_SUMSQ, words) == f32_bits(6.0)
    assert rdc_result(VecOp.RDC_MAX, words) == 0x40000000
    assert rdc_result(VecOp.RDC_MAX, [0x7f800001] * 3) == FP32_NAN_BITS
    # A single element is every reduction's identity.
    assert rdc_result(VecOp.RDC_SUM, [0xbf800000]) == 0xbf800000
    assert rdc_result(VecOp.RDC_SUMSQ, [0xbf800000]) == 0x3f800000
    assert rdc_result(VecOp.RDC_MAX, [0x80000000]) == 0x80000000


# ---------------------------------------------------------------------------
# Unary transcendental contract (model level)
# ---------------------------------------------------------------------------

QNAN = FP32_NAN_BITS
PINF = 0x7f800000
NINF = 0xff800000


def fp32_ulp_diff(a, b):
    """Representable steps between two finite same-sign FP32 bits."""
    if a == b:
        return 0
    lo, hi = min(a, b), max(a, b)
    n = 0
    while lo < hi:
        e_lo, e_hi = (lo >> 23) & 0xff, (hi >> 23) & 0xff
        if e_lo == e_hi:
            return n + hi - lo
        step = (1 << 23) - (lo & 0x7fffff)
        lo += step
        n += step
    return n


def test_vec_unary_special_value_table():
    """The frozen special-value table, hand-derived per class."""
    # NaNs canonicalize whatever the payload and the op.
    for x in (0x7fc00000, 0x7f800001, 0x7fc00001, 0xffc00001):
        assert vec_recip_bits(x) == QNAN
        assert vec_rsqrt_bits(x) == QNAN
        assert vec_exp2_bits(x) == QNAN

    # recip: zeros/subnormals -> signed inf; >= 2^126 (incl. inf) ->
    # signed zero.
    assert vec_recip_bits(0x00000000) == PINF
    assert vec_recip_bits(0x80000000) == NINF
    assert vec_recip_bits(0x00000001) == PINF
    assert vec_recip_bits(0x007fffff) == PINF
    assert vec_recip_bits(0x80000001) == NINF
    assert vec_recip_bits(0x7f800000) == 0x00000000
    assert vec_recip_bits(0xff800000) == 0x80000000
    assert vec_recip_bits(0x7e800000) == 0x00000000  # 2^126
    assert vec_recip_bits(0xfe800000) == 0x80000000  # -2^126
    # 2^126 * (1 - 2^-23) is the largest sequenced magnitude.
    assert vec_recip_bits(0x7e7fffff) == f32_bits(f32(2.0**-126))

    # rsqrt: IEEE sqrt-class policy on the negatives and zeros,
    # +inf -> +0, positive subnormal -> +inf clamp.
    assert vec_rsqrt_bits(0xff800000) == QNAN
    assert vec_rsqrt_bits(0xbf800000) == QNAN
    assert vec_rsqrt_bits(0x80000001) == QNAN
    assert vec_rsqrt_bits(0x80000000) == NINF
    assert vec_rsqrt_bits(0x00000000) == PINF
    assert vec_rsqrt_bits(0x00000001) == PINF
    assert vec_rsqrt_bits(0x007fffff) == PINF
    assert vec_rsqrt_bits(0x7f800000) == 0x00000000

    # exp2: x >= 128 -> +inf (includes +inf), x < -126 -> +0
    # (includes -inf); boundaries exact.
    assert vec_exp2_bits(PINF) == PINF
    assert vec_exp2_bits(NINF) == 0x00000000
    assert vec_exp2_bits(f32_bits(128.0)) == PINF
    assert vec_exp2_bits(f32_bits(127.5)) < PINF  # finite: < 2^128
    assert vec_exp2_bits(f32_bits(-126.0)) == f32_bits(2.0**-126)
    assert vec_exp2_bits(f32_bits(-126.00001)) == 0x00000000
    assert vec_exp2_bits(0x00000000) == 0x3f800000
    assert vec_exp2_bits(0x80000000) == 0x3f800000
    # Tiny inputs: positive rounds through the polynomial to exactly
    # 1.0 (f = x, p = R(C7*x + ... + 1.0)); negative takes i = -1,
    # f = R(1 + x) = 1.0, so the Taylor tail shows (the plan's
    # documented near-integer split) -- within the measured bound.
    assert vec_exp2_bits(0x00000001) == 0x3f800000
    assert fp32_ulp_diff(vec_exp2_bits(0x80000001), 0x3f800000) \
        <= ACCURACY_ULP['exp2']


def test_vec_unary_integer_arguments_exact():
    """exp2 of every in-domain integer is exactly 2**i (f = +0
    collapses the Horner evaluation to C0)."""
    for i in range(-126, 128):
        assert vec_exp2_bits(f32_bits(float(i))) == f32_bits(2.0**i)


def _unary_samples(rng, per_exp=12):
    """Every exponent with dense mantissa samples and table-index
    boundaries, per the plan's accuracy-measurement domain."""
    out = []
    for e in range(0, 256):
        base = e << 23
        out += [
            base, base | 0x000001, base | 0x1ffff, base | 0x20000,
            base | 0x3ffff, base | 0x40000, base | 0x7fffe, base | 0x7fffff
        ]
        out += [
            base | (rng.randrange(1 << 23) & ~0x1f) for _ in range(per_exp)
        ]
    return out


ACCURACY_ULP = {
    'recip': 4,  # measured 2 over the tested domain
    'rsqrt': 4,  # measured 2
    'exp2': 32,  # measured 13
}


def _check_unary_accuracy(fn, true_fn, samples, bound, skip):
    import math

    worst = 0
    worst_x = None
    checked = 0
    for x in samples:
        if (x >> 23) == 255 and (x & 0x7fffff):
            continue  # NaNs: covered by the special-value table
        if skip(x):
            continue
        got = fn(x)
        t = true_fn(bits_to_f32(x))
        if math.isnan(t) or math.isinf(t) or t == 0 or abs(t) > 3e38:
            continue
        ref = f32_bits(t)
        # Intentional input clamps are excluded by skip(), never by
        # the implementation's output. A finite normal reference must
        # produce a finite normal result with the same sign.
        if (ref & 0x7fffffff) < (1 << 23):
            continue
        assert 0 < ((got >> 23) & 0xff) < 255, (hex(x), hex(got))
        assert (got >> 31) == (ref >> 31), (hex(x), hex(got), hex(ref))
        checked += 1
        error = fp32_ulp_diff(got, ref)
        if error > worst:
            worst, worst_x = error, x
    assert checked, 'accuracy check evaluated no finite normal references'
    assert worst <= bound, (hex(worst_x), worst, bound)


@pytest.mark.parametrize('result', [0, 0x80000000, PINF, NINF, QNAN])
def test_unary_accuracy_rejects_exceptional_results(result):
    with pytest.raises(AssertionError):
        _check_unary_accuracy(lambda x: result, lambda x: 1.0 / x,
                              [f32_bits(2.0)], 4, lambda x: False)


def test_vec_recip_accuracy():
    import random

    rng = random.Random(101)

    def skip(x):
        e = (x >> 23) & 0xff
        return e == 0 or e >= 253

    _check_unary_accuracy(vec_recip_bits, lambda v: 1.0 / v,
                          _unary_samples(rng), ACCURACY_ULP['recip'], skip)


def test_vec_rsqrt_accuracy():
    import math
    import random

    rng = random.Random(102)

    def skip(x):
        e = (x >> 23) & 0xff
        return (x >> 31) or e == 0

    _check_unary_accuracy(vec_rsqrt_bits, lambda v: 1.0 / math.sqrt(v),
                          _unary_samples(rng), ACCURACY_ULP['rsqrt'], skip)


def test_vec_exp2_accuracy():
    import math
    import random

    rng = random.Random(103)
    grid = [f32_bits(f32(-130 + i * 2.0**-7)) for i in range(int(260 * 128))]
    rand = [f32_bits(f32(rng.uniform(-130, 130))) for _ in range(4000)]

    def skip(x):
        xf = bits_to_f32(x)
        return xf >= 128.0 or xf < -126.0

    _check_unary_accuracy(vec_exp2_bits, lambda v: 2.0**v, grid + rand,
                          ACCURACY_ULP['exp2'], skip)


def test_vec_unary_compositions():
    """SiLU and RMSNorm compositions against binary64 references.

    The measured relative errors (silu ~1.4e-6, dominated by exp2's
    Taylor tail) bound what the ggml mapping can expect.
    """
    import math
    import random

    rng = random.Random(104)

    def silu_dev(x):
        # sigmoid(x) = recip(exp2(-x * log2(e)) + 1), per the reference
        # composition recipes.
        neg = fp32_mul_bits(f32_bits(-math.log2(math.e)), x)
        s = fp32_add_bits(vec_exp2_bits(neg), 0x3f800000)
        return fp32_mul_bits(x, vec_recip_bits(s))

    worst_s = 0.0
    for _ in range(2000):
        v = f32(rng.uniform(-30, 30))
        got = bits_to_f32(silu_dev(f32_bits(v)))
        want = v / (1.0 + math.exp(-v))
        worst_s = max(worst_s, abs(got - want) / max(abs(want), 2.0**-126))
    assert worst_s <= 2**-18, worst_s  # measured ~2.2e-6 (exp2 tail)

    def rmsnorm_dev(xs, eps=1e-6):
        words = [f32_bits(v) for v in xs]
        ss = rdc_result(VecOp.RDC_SUMSQ, words)
        mean = fp32_mul_bits(ss, f32_bits(1.0 / len(xs)))
        inv = vec_rsqrt_bits(fp32_add_bits(mean, f32_bits(eps)))
        return [fp32_mul_bits(w, inv) for w in words]

    xs = [rng.uniform(-3, 3) for _ in range(64)]
    got = [bits_to_f32(w) for w in rmsnorm_dev(xs)]
    scale = 1.0 / math.sqrt(sum(v * v for v in xs) / len(xs) + 1e-6)
    worst_r = max(
        abs(g - scale * x) / max(abs(scale * x), 1e-9)
        for g, x in zip(got, xs))
    assert worst_r <= 2**-20, worst_r


def make_model(**over):
    params = dict(PARAMS)
    params.update(over)
    hp = HasTRoomParams(params)
    return hp, TRoomModel(params, bytearray(DRAM_SIZE),
                          bytearray(hp.tmem_bytes))


def vec_cmd(opcode, seq, op, n, a_base, b_base, o_base, evt=1):
    return dict(opcode=opcode,
                seq=seq,
                evt=evt,
                vec_op=int(op),
                n=n,
                a_base=a_base,
                b_base=b_base,
                o_base=o_base)


def test_model_vector_command_regions_and_validation():
    hp, model = make_model()
    a = SPECIALS[:10]
    b = SPECIALS[2:12]
    model.tmem[0x100:0x100 + 40] = words_to_bytes(a)
    model.tmem[0x200:0x200 + 40] = words_to_bytes(b)

    cmds = [
        vec_cmd(Opcode.VECTOR, 1, VecOp.VEC_ADD, 10, 0x100, 0x200, 0x400),
        dict(opcode=Opcode.END, seq=2, evt=0),
    ]
    assert model.run(cmds) == [(1, Status.OK), (2, Status.OK)]
    blob = words_to_bytes(
        [vec_word(VecOp.VEC_ADD, x, y) for x, y in zip(a, b)])
    blob += bytes((-len(blob)) % hp.beat_bytes)
    assert model.tmem[0x400:0x400 + len(blob)] == blob

    # Precedence: shape > align > range, with the same formulas as the
    # RTL (overlap against padded regions, b unused by reductions).
    cases = [
        # Output overlaps the A region.
        (vec_cmd(Opcode.VECTOR, 3, VecOp.VEC_MUL, 4, 0x100, 0x200,
                 0x104), Status.BAD_SHAPE),
        # Element count zero.
        (vec_cmd(Opcode.VECTOR, 3, VecOp.VEC_MUL, 0, 0x100, 0x200,
                 0x400), Status.BAD_SHAPE),
        # Sub-operation from the other opcode class.
        (vec_cmd(Opcode.REDUCE, 3, VecOp.VEC_MUL, 4, 0x100, 0x200,
                 0x400), Status.BAD_SHAPE),
        (vec_cmd(Opcode.VECTOR, 3, VecOp.RDC_SUM, 4, 0x100, 0x200,
                 0x400), Status.BAD_SHAPE),
        # Misaligned output base.
        (vec_cmd(Opcode.VECTOR, 3, VecOp.VEC_MUL, 4, 0x100, 0x200,
                 0x401), Status.BAD_ALIGN),
        # B misalignment is not checked for reductions.
        (vec_cmd(Opcode.REDUCE, 3, VecOp.RDC_SUM, 4, 0x100, 0x201,
                 0x400), Status.OK),
        # Nor is a B alias with the output: reductions never read B.
        (vec_cmd(Opcode.REDUCE, 3, VecOp.RDC_SUM, 8, 0x100, 0x1f8,
                 0x200), Status.OK),
        # Region beyond TMEM.
        (vec_cmd(Opcode.VECTOR, 3, VecOp.VEC_MUL, 4, hp.tmem_bytes - 8, 0x200,
                 0x400), Status.BAD_RANGE),
    ]
    for cmd, status in cases:
        assert model._vector(cmd) is status, cmd


# ---------------------------------------------------------------------------
# RTL unit against the model
# ---------------------------------------------------------------------------

A_BASE = 0x100
# Regions must not overlap: the widest unit test streams 256 words
# (1 KiB) per region, so A, B, and O are spaced a full KiB apart.
B_BASE = 0x800
O_BASE = 0x1000


class UnitHarness(Elaboratable):
    """VectorUnit reading from a banked TMEM; writes are captured."""

    def __init__(self, params):
        self.params = params
        self.tmem = TMEM(params)
        self.vec = VectorUnit(params)

    def elaborate(self, platform):
        m = Module()
        m.submodules.tmem = self.tmem
        m.submodules.vec = self.vec
        m.d.comb += [
            self.vec.tmem_rreq.connect(self.tmem.rreq),
            self.tmem.rresp.connect(self.vec.tmem_rresp),
        ]
        return m


def tmem_write(write, addr, word):
    yield write.bits.addr.eq(addr)
    yield write.bits.data.eq(word)
    yield write.valid.eq(1)
    yield
    while not (yield write.fire):
        yield
    yield write.valid.eq(0)


def run_unit(params,
             descs,
             a_bytes,
             b_bytes=b'',
             *,
             stall_wr=False,
             hold_writes=0,
             hold_after_beats=0,
             deadline=60000):
    """Run descriptors sequentially; return writes and per-command data.

    ``descs`` is one descriptor dict or a list. Every command's
    completion samples the observation counters, so repeated-command
    deltas are comparable; ``writes`` accumulates TMEM writes in order.
    ``hold_after_beats`` selects how many writes commit before the
    ``hold_writes``-cycle stall on the next pending beat.
    """
    import random

    if isinstance(descs, dict):
        descs = [descs]

    hp = HasTRoomParams(params)
    bb = hp.beat_bytes
    dut = UnitHarness(params)
    writes = []
    results = []
    shared = {'blocked': 0, 'cycles': 0}
    reads = []

    def tx():
        for off in range(0, len(a_bytes), bb):
            word = int.from_bytes(a_bytes[off:off + bb], 'little')
            yield from tmem_write(dut.tmem.write, A_BASE + off, word)
        for off in range(0, len(b_bytes), bb):
            word = int.from_bytes(b_bytes[off:off + bb], 'little')
            yield from tmem_write(dut.tmem.write, B_BASE + off, word)

        sink = dut.vec.sink
        for desc in descs:
            yield sink.bits.op.eq(desc['op'])
            yield sink.bits.n.eq(desc['n'])
            yield sink.bits.a_base.eq(desc.get('a_base', A_BASE))
            yield sink.bits.b_base.eq(desc.get('b_base', B_BASE))
            yield sink.bits.o_base.eq(desc.get('o_base', O_BASE))
            yield sink.valid.eq(1)
            yield
            while not (yield sink.fire):
                yield
            # A rejected descriptor pulses done combinationally with
            # the accept; an accepted one pulses it at true completion.
            start_cycle = shared['cycles']
            read_start = len(reads)
            rejected = (yield dut.vec.done)
            yield sink.valid.eq(0)
            yield
            if not rejected:
                while not (yield dut.vec.done):
                    yield
            results.append(
                dict(
                    status=Status((yield dut.vec.status)),
                    elements=(yield dut.vec.elements),
                    write_beats=(yield dut.vec.write_beats),
                    stall_cycles=(yield dut.vec.stall_cycles),
                    blocked=shared['blocked'],
                    cycles=shared['cycles'],
                    execution_cycles=shared['cycles'] - start_cycle,
                    reads=reads[read_start:],
                ))

    def wrdrive():
        # Write readiness is independent of the stimulus process, so a
        # command can complete while its result beats are backpressed.
        yield Passive()
        rng = random.Random(7)
        if hold_after_beats:
            yield dut.vec.tmem_w.ready.eq(1)
            yield
            accepted = 0
            while accepted < hold_after_beats:
                if (yield dut.vec.tmem_w.fire):
                    accepted += 1
                yield
        # The deliberate hold must reach execution and must catch the
        # selected beat: the port stays not-ready from before its
        # result-beat request (a ready-high wait would let it fire the
        # cycle valid rises) through hold_writes cycles of a pending
        # request.
        yield dut.vec.tmem_w.ready.eq(0)
        yield
        while not (yield dut.vec.tmem_w.valid):
            yield dut.vec.tmem_w.ready.eq(0)
            yield
        for _ in range(hold_writes):
            yield dut.vec.tmem_w.ready.eq(0)
            yield
        while True:
            yield dut.vec.tmem_w.ready.eq(rng.random() <
                                          0.5 if stall_wr else 1)
            yield

    def watch():
        yield Passive()
        while True:
            shared['cycles'] += 1
            if (yield dut.vec.tmem_rreq.fire):
                reads.append((yield dut.vec.tmem_rreq.bits.addr))
            if (yield dut.vec.tmem_w.fire):
                writes.append(
                    ((yield
                      dut.vec.tmem_w.bits.addr), (yield
                                                  dut.vec.tmem_w.bits.data)))
            elif (yield dut.vec.tmem_w.valid):
                shared['blocked'] += 1
            yield

    def finish():
        # The one active process past the stimulus: its return (or
        # timeout) ends the simulation.
        for _ in range(deadline):
            yield
            if len(results) == len(descs):
                return
        raise TimeoutError('vector unit did not finish')

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(tx)
    sim.add_sync_process(wrdrive)
    sim.add_sync_process(watch)
    sim.add_sync_process(finish)
    sim.run()
    if len(descs) == 1:
        return writes, results[0]
    return writes, results


def expected_beats(params, words):
    hp = HasTRoomParams(params)
    bb = hp.beat_bytes
    blob = words_to_bytes(words)
    blob += bytes((-len(blob)) % bb)
    return [
        int.from_bytes(blob[off:off + bb], 'little')
        for off in range(0, len(blob), bb)
    ]


@pytest.mark.parametrize('op', [
    VecOp.VEC_ADD,
    VecOp.VEC_SUB,
    VecOp.VEC_MUL,
])
def test_unit_elementwise_specials(op):
    """All ordered pairs of the special pool, bit for bit."""
    params = dict(PARAMS)
    n = len(SPECIALS)
    a = [SPECIALS[i % n] for i in range(n * n)]
    b = [SPECIALS[(i // n) % n] for i in range(n * n)]
    writes, res = run_unit(params, dict(op=int(op), n=len(a)),
                           words_to_bytes(a), words_to_bytes(b))
    assert res['status'] is Status.OK
    assert res['elements'] == len(a)
    exp = expected_beats(params, [vec_word(op, x, y) for x, y in zip(a, b)])
    assert res['write_beats'] == len(exp)
    assert writes == [(O_BASE + 8 * i, d) for i, d in enumerate(exp)]


def test_unit_elementwise_scale():
    params = dict(PARAMS)
    a = SPECIALS[:14]
    scalar = 0xbf490fdb  # -pi
    writes, res = run_unit(params, dict(op=int(VecOp.VEC_SCALE), n=len(a)),
                           words_to_bytes(a), words_to_bytes([scalar]))
    assert res['status'] is Status.OK
    exp = expected_beats(params,
                         [vec_word(VecOp.VEC_SCALE, x, scalar) for x in a])
    assert writes == [(O_BASE + 8 * i, d) for i, d in enumerate(exp)]


def test_unit_elementwise_write_backpressure():
    """Random write readiness must not lose or duplicate a result.

    The deliberate hold now starts at the first result-beat request and
    outlasts the result FIFO plus the FPU pipeline, so the unit must
    stall issue through credit accounting alone.
    """
    import random

    rng = random.Random(4)
    params = dict(PARAMS, vec_result_q=2)
    n = 37  # final partial beat exercises zero padding
    a = [f32_bits(f32(rng.uniform(-100, 100))) for _ in range(n)]
    b = [f32_bits(f32(rng.uniform(-100, 100))) for _ in range(n)]
    writes, res = run_unit(params,
                           dict(op=int(VecOp.VEC_ADD), n=n),
                           words_to_bytes(a),
                           words_to_bytes(b),
                           stall_wr=True,
                           hold_writes=40)
    assert res['status'] is Status.OK
    assert res['elements'] == n
    assert res['stall_cycles'] > 0
    # The deliberate hold caught the first request and outlasted the
    # result FIFO plus the FPU pipeline.
    assert res['blocked'] >= 40, res['blocked']
    exp = expected_beats(params,
                         [vec_word(VecOp.VEC_ADD, x, y) for x, y in zip(a, b)])
    assert writes == [(O_BASE + 8 * i, d) for i, d in enumerate(exp)]


@pytest.mark.parametrize(
    'op,words',
    [
        (VecOp.RDC_SUM, [0x3f800000] + [0x33800000] * 4),  # isum_4 case
        (VecOp.RDC_SUM, SPECIALS),
        (VecOp.RDC_SUMSQ, SPECIALS[:13]),
        (VecOp.RDC_SUMSQ, [0x3f800000, 0xbf800000, 0x40000000]),
        (VecOp.RDC_MAX, SPECIALS),
        (VecOp.RDC_MAX, [0x7f800001, 0x7fc00000, 0xff800000]),
        (VecOp.RDC_SUM, [0x42f6e979]),  # single element
        (VecOp.RDC_MAX, [0x80000000]),  # single -0
    ])
def test_unit_reductions(op, words):
    params = dict(PARAMS)
    writes, res = run_unit(params, dict(op=int(op), n=len(words)),
                           words_to_bytes(words))
    assert res['status'] is Status.OK
    assert res['elements'] == len(words)
    assert res['write_beats'] == 1
    assert writes == [(O_BASE, rdc_result(op, words))]


def test_unit_reductions_write_backpressure():
    params = dict(PARAMS)
    words = SPECIALS
    writes, res = run_unit(params,
                           dict(op=int(VecOp.RDC_SUMSQ), n=len(words)),
                           words_to_bytes(words),
                           stall_wr=True,
                           hold_writes=20)
    assert res['status'] is Status.OK
    # The single result beat was actually held, not merely delayed
    # past an idle port.
    assert res['blocked'] >= 20, res['blocked']
    assert writes == [(O_BASE, rdc_result(VecOp.RDC_SUMSQ, words))]


def test_unit_wide_beat():
    """One 512-bit beat holds sixteen words; windows span beats."""
    import random

    rng = random.Random(6)
    params = dict(PARAMS, data_width=512, n_tmem_banks=8, act_max_k=2048)
    bb = 64
    n = 100  # six full beats plus a partial
    a = [f32_bits(f32(rng.uniform(-1e6, 1e6))) for _ in range(n)]
    b = [f32_bits(f32(rng.uniform(-1e6, 1e6))) for _ in range(n)]
    writes, res = run_unit(params, dict(op=int(VecOp.VEC_MUL), n=n),
                           words_to_bytes(a), words_to_bytes(b))
    assert res['status'] is Status.OK
    exp = expected_beats(params,
                         [vec_word(VecOp.VEC_MUL, x, y) for x, y in zip(a, b)])
    assert writes == [(O_BASE + bb * i, d) for i, d in enumerate(exp)]

    rwords = a[:80]
    writes, res = run_unit(params, dict(op=int(VecOp.RDC_SUM), n=80),
                           words_to_bytes(rwords))
    assert writes == [(O_BASE, rdc_result(VecOp.RDC_SUM, rwords))]


@pytest.mark.parametrize('desc,status,why', [
    (dict(op=int(VecOp.VEC_ADD), n=0), Status.BAD_SHAPE, 'zero elements'),
    (dict(op=int(VecOp.VEC_ADD), n=8,
          o_base=A_BASE + 4), Status.BAD_SHAPE, 'output inside A'),
    (dict(op=int(VecOp.VEC_ADD), n=8,
          o_base=B_BASE + 28), Status.BAD_SHAPE, 'output overlaps B tail'),
    (dict(op=int(VecOp.VEC_SCALE), n=8, o_base=B_BASE + 4), Status.BAD_SHAPE,
     'scale output inside the scalar beat'),
    (dict(op=int(VecOp.RDC_SUM), n=8,
          o_base=A_BASE + 28), Status.BAD_SHAPE, 'reduce output in A'),
    (dict(op=int(VecOp.VEC_ADD), n=8,
          o_base=O_BASE + 1), Status.BAD_ALIGN, 'output misaligned'),
    (dict(op=int(VecOp.VEC_ADD), n=8,
          a_base=A_BASE + 2), Status.BAD_ALIGN, 'A misaligned'),
    (dict(op=int(VecOp.VEC_ADD), n=8,
          b_base=B_BASE + 4), Status.BAD_ALIGN, 'B misaligned'),
    (dict(op=int(VecOp.VEC_ADD), n=8,
          a_base=0x3ff0), Status.BAD_RANGE, 'regions beyond TMEM'),
    (dict(op=11, n=8), Status.BAD_SHAPE, 'reserved sub-op encoding'),
])
def test_unit_rejects_bad_descriptors(desc, status, why):
    """Rejection leaves the unit inert: no latched state executes.

    The bad offer stays valid for many cycles after the reject pulse
    (the sink re-fires combinationally, as with the activation
    loader), so any execution gated on latched state -- reads,
    arithmetic launches, result writes -- would show up as traffic.
    """
    params = dict(PARAMS)
    dut = VectorUnit(params)

    def tx():
        sink = dut.sink
        yield sink.bits.op.eq(desc['op'])
        yield sink.bits.n.eq(desc['n'])
        yield sink.bits.a_base.eq(desc.get('a_base', A_BASE))
        yield sink.bits.b_base.eq(desc.get('b_base', B_BASE))
        yield sink.bits.o_base.eq(desc.get('o_base', O_BASE))
        yield sink.valid.eq(1)
        yield
        assert (yield dut.done)
        assert (yield dut.status) == status
        for _ in range(60):
            assert not (yield dut.tmem_rreq.valid), \
                f'{why}: read after rejection'
            assert not (yield dut.tmem_w.valid), \
                f'{why}: write after rejection'
            assert not (yield dut.tmem_w.fire), \
                f'{why}: write beat after rejection'
            assert not (yield dut.active), f'{why}: active after reject'
            yield
        yield sink.valid.eq(0)
        yield
        assert not (yield dut.tmem_rreq.valid)
        assert not (yield dut.tmem_w.valid)
        assert not (yield dut.active)

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(tx)
    sim.run()


def test_unit_reject_then_valid_recovers():
    """A rejected command must not disturb the next valid one."""
    import random

    rng = random.Random(31)
    params = dict(PARAMS)
    n = 9
    a = [f32_bits(f32(rng.uniform(-30, 30))) for _ in range(n)]
    b = [f32_bits(f32(rng.uniform(-30, 30))) for _ in range(n)]
    bad = dict(op=int(VecOp.VEC_ADD), n=n, o_base=A_BASE + 4)
    good = dict(op=int(VecOp.VEC_ADD), n=n)
    writes, res = run_unit(params, [bad, good], words_to_bytes(a),
                           words_to_bytes(b))
    assert res[0]['status'] is Status.BAD_SHAPE
    assert res[0]['write_beats'] == 0, 'rejected command wrote beats'
    assert res[0]['elements'] == 0, 'rejected command consumed elements'
    assert res[0]['stall_cycles'] == 0
    assert res[1]['status'] is Status.OK
    assert res[1]['elements'] == res[0]['elements'] + n
    assert res[1]['write_beats'] == res[0]['write_beats'] + 5
    exp = expected_beats(params,
                         [vec_word(VecOp.VEC_ADD, x, y) for x, y in zip(a, b)])
    assert writes == [(O_BASE + 8 * i, d) for i, d in enumerate(exp)]


def test_unit_stall_deltas_are_command_local():
    """Repeated identical commands accrue identical stall counts."""
    words = [0x42f6e979]
    descs = [dict(op=int(VecOp.RDC_SUM), n=1) for _ in range(3)]
    writes, res = run_unit(dict(PARAMS), descs, words_to_bytes(words))
    assert all(r['status'] is Status.OK for r in res)
    deltas = [
        res[i]['stall_cycles'] - res[i - 1]['stall_cycles']
        for i in range(1, 3)
    ]
    elem_deltas = [
        res[i]['elements'] - res[i - 1]['elements'] for i in range(1, 3)
    ]
    assert elem_deltas == [1, 1]
    assert deltas[0] == deltas[1]
    assert writes == [(O_BASE, words[0])] * 3


@pytest.mark.parametrize(
    'op,b_base,o_base',
    [
        (VecOp.RDC_SUM, O_BASE - 24, O_BASE),  # B would overlap the output
        (VecOp.RDC_MAX, O_BASE, O_BASE),  # B aliases the output
    ])
def test_unit_reduce_ignores_b_region(op, b_base, o_base):
    """Reductions never read B, so a B alias with the output is fine
    (an elementwise op with the same regions is rejected)."""
    params = dict(PARAMS)
    words = SPECIALS[:12]
    writes, res = run_unit(
        params, dict(op=int(op), n=len(words), b_base=b_base, o_base=o_base),
        words_to_bytes(words))
    assert res['status'] is Status.OK
    assert res['elements'] == len(words)
    assert writes == [(o_base, rdc_result(op, words))]


# ---------------------------------------------------------------------------
# Full command path through Core
# ---------------------------------------------------------------------------

VA_TMEM = 0x0000
VB_TMEM = 0x1000
VO_TMEM = 0x2000
VA_DRAM = 0x4000
# A's DRAM image can reach 0x4900 (576 words), so B starts a full
# region later; O stays beyond both.
VB_DRAM = 0x5000
VO_DRAM = 0x6000


def vector_image(a_words, b_words, params):
    hp = HasTRoomParams(params)
    bb = hp.beat_bytes
    image = bytearray(DRAM_SIZE)

    def place(base, words):
        blob = words_to_bytes(words) + bytes((-4 * len(words)) % bb)
        image[base:base + len(blob)] = blob
        return len(blob)

    la = place(VA_DRAM, a_words)
    lb = place(VB_DRAM, b_words)
    return image, la, lb


def vector_program(opcode, op, n, la, lb, *, base_seq=1, evt=4, lo=None):
    """load A, load B, wait, vector/reduce, wait, store, wait, end."""
    program = [
        dict(opcode=Opcode.LOAD_2D,
             seq=base_seq,
             evt=1,
             dram_addr=VA_DRAM,
             tmem_addr=VA_TMEM,
             row_bytes=la,
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.LOAD_2D,
             seq=base_seq + 1,
             evt=2,
             dram_addr=VB_DRAM,
             tmem_addr=VB_TMEM,
             row_bytes=lb,
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.WAIT, seq=base_seq + 2, evt=1),
        dict(opcode=Opcode.WAIT, seq=base_seq + 3, evt=2),
        dict(opcode=opcode,
             seq=base_seq + 4,
             evt=evt,
             vec_op=int(op),
             k=n,
             a_base=VA_TMEM,
             m_base=VB_TMEM,
             r_base=VO_TMEM),
        dict(opcode=Opcode.WAIT, seq=base_seq + 5, evt=evt),
    ]
    o_bytes = lo or (4 * n + 7) // 8 * 8
    program += [
        dict(opcode=Opcode.STORE_2D,
             seq=base_seq + 6,
             evt=5,
             dram_addr=VO_DRAM,
             tmem_addr=VO_TMEM,
             row_bytes=o_bytes,
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.WAIT, seq=base_seq + 7, evt=5),
        dict(opcode=Opcode.END, seq=base_seq + 8),
    ]
    return program


def check_dram_vector(image, words):
    blob = words_to_bytes(words)
    got = image[VO_DRAM:VO_DRAM + len(blob)]
    assert got == blob, (got.hex(), blob.hex())


@pytest.mark.parametrize('opcode,op', [
    (Opcode.VECTOR, VecOp.VEC_ADD),
    (Opcode.VECTOR, VecOp.VEC_SUB),
    (Opcode.VECTOR, VecOp.VEC_MUL),
    (Opcode.VECTOR, VecOp.VEC_SCALE),
])
def test_core_vector_full_path(opcode, op):
    import random

    rng = random.Random(21)
    params = dict(PARAMS)
    n = 24
    a = [f32_bits(f32(rng.uniform(-50, 50))) for _ in range(n)]
    a[3] = 0x7fc00000
    a[7] = 0x7f800000
    a[11] = 0x80000000
    b = ([f32_bits(f32(rng.uniform(-50, 50)))] if op is VecOp.VEC_SCALE else
         [f32_bits(f32(rng.uniform(-50, 50))) for _ in range(n)])
    b[0] = 0x40490fdb
    image, la, lb = vector_image(a, b, params)
    program = vector_program(opcode, op, n, la, lb)
    results, image, _ = run_program(params, program, image)
    assert all(s is Status.OK for s in results.values()), results
    check_dram_vector(image, [vec_word(op, x, y) for x, y in zip(a, b)])


@pytest.mark.parametrize('op', [VecOp.RDC_SUM, VecOp.RDC_SUMSQ, VecOp.RDC_MAX])
def test_core_reduce_full_path(op):
    import random

    rng = random.Random(22)
    params = dict(PARAMS)
    n = 24
    a = [f32_bits(f32(rng.uniform(-10, 10))) for _ in range(n)]
    a[5] = 0x7f800001
    a[9] = 0xff800000
    image, la, lb = vector_image(a, a[:2], params)
    program = vector_program(Opcode.REDUCE, op, n, la, lb, lo=8)
    results, image, _ = run_program(params, program, image)
    assert all(s is Status.OK for s in results.values()), results
    check_dram_vector(image, [rdc_result(op, a)])


def test_core_vector_deployment_width():
    import random

    rng = random.Random(23)
    params = dict(PARAMS,
                  data_width=512,
                  id_width=6,
                  n_tmem_banks=8,
                  act_max_k=2048)
    n = 576  # a hidden-dim vector
    a = [f32_bits(f32(rng.uniform(-3, 3))) for _ in range(n)]
    b = [f32_bits(f32(rng.uniform(-3, 3))) for _ in range(n)]
    image, la, lb = vector_image(a, b, params)
    program = vector_program(Opcode.VECTOR, VecOp.VEC_MUL, n, la, lb)
    results, image, _ = run_program(params, program, image, deadline=30000)
    assert all(s is Status.OK for s in results.values()), results
    check_dram_vector(image,
                      [vec_word(VecOp.VEC_MUL, x, y) for x, y in zip(a, b)])


def test_core_vector_back_to_back_and_queued():
    """Two vector commands without an intervening WAIT; the second
    consumes the first command's live result region."""
    import random

    rng = random.Random(24)
    params = dict(PARAMS)
    n = 12
    a = [f32_bits(f32(rng.uniform(-9, 9))) for _ in range(n)]
    b = [f32_bits(f32(rng.uniform(-9, 9))) for _ in range(n)]
    image, la, lb = vector_image(a, b, params)
    program = [
        dict(opcode=Opcode.LOAD_2D,
             seq=1,
             evt=1,
             dram_addr=VA_DRAM,
             tmem_addr=VA_TMEM,
             row_bytes=la,
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.LOAD_2D,
             seq=2,
             evt=2,
             dram_addr=VB_DRAM,
             tmem_addr=VB_TMEM,
             row_bytes=lb,
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.WAIT, seq=3, evt=1),
        dict(opcode=Opcode.WAIT, seq=4, evt=2),
        dict(opcode=Opcode.VECTOR,
             seq=5,
             evt=4,
             vec_op=int(VecOp.VEC_ADD),
             k=n,
             a_base=VA_TMEM,
             m_base=VB_TMEM,
             r_base=VO_TMEM),
        dict(opcode=Opcode.VECTOR,
             seq=6,
             evt=5,
             vec_op=int(VecOp.VEC_SCALE),
             k=n,
             a_base=VO_TMEM,
             m_base=VB_TMEM,
             r_base=VO_TMEM + 0x100),
        dict(opcode=Opcode.WAIT, seq=7, evt=5),
        dict(opcode=Opcode.STORE_2D,
             seq=8,
             evt=6,
             dram_addr=VO_DRAM + 0x100,
             tmem_addr=VO_TMEM + 0x100,
             row_bytes=48,
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.WAIT, seq=9, evt=6),
        dict(opcode=Opcode.END, seq=10),
    ]
    results, image, _ = run_program(params, program, image, stall_resp=True)
    assert all(s is Status.OK for s in results.values()), results
    add = [vec_word(VecOp.VEC_ADD, x, y) for x, y in zip(a, b)]
    scaled = [vec_word(VecOp.VEC_SCALE, w, b[0]) for w in add]
    got = words_from_bytes(image[VO_DRAM + 0x100:VO_DRAM + 0x100 + 48])
    assert got == scaled


def test_core_vector_consumes_matmul_result():
    """A residual VECTOR ADD over a MATMUL's FP32 result region."""
    import random

    fmt = Format.Q8_0
    k, rows = 64, 3
    rng = random.Random(25)
    params = dict(PARAMS, mm_formats=(fmt, ))
    wsegs = gen_wsegs(rng, fmt, rows * (k // 32))
    act = build_act(fmt, [rng.uniform(-1, 1) for _ in range(k)])

    image, stride, rstride, qlen, mlen = build_program_image(fmt,
                                                             k,
                                                             rows,
                                                             wsegs,
                                                             act,
                                                             rng,
                                                             params=params)

    # Truncate the plain matmul program after its result WAIT and
    # continue with the vector tail.
    base = matmul_program(fmt,
                          k,
                          rows,
                          stride,
                          rstride,
                          qlen,
                          mlen,
                          params=params)
    cut = next(i for i, e in enumerate(base) if e['opcode'] is Opcode.STORE_2D)
    program = base[:cut]
    resid = [f32_bits(f32(rng.uniform(-2, 2))) for _ in range(rows)]
    rblob = words_to_bytes(resid) + bytes((-4 * rows) % 8)
    image[VB_DRAM:VB_DRAM + len(rblob)] = rblob
    program += [
        dict(opcode=Opcode.LOAD_2D,
             seq=30,
             evt=6,
             dram_addr=VB_DRAM,
             tmem_addr=VB_TMEM,
             row_bytes=len(rblob),
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.WAIT, seq=31, evt=6),
        dict(opcode=Opcode.VECTOR,
             seq=32,
             evt=7,
             vec_op=int(VecOp.VEC_ADD),
             k=rows,
             a_base=R_BASE,
             m_base=VB_TMEM,
             r_base=VO_TMEM),
        dict(opcode=Opcode.WAIT, seq=33, evt=7),
        dict(opcode=Opcode.STORE_2D,
             seq=34,
             evt=8,
             dram_addr=VO_DRAM,
             tmem_addr=VO_TMEM,
             row_bytes=rstride,
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.WAIT, seq=35, evt=8),
        dict(opcode=Opcode.END, seq=36),
    ]
    results, image, _ = run_program(params, program, image, deadline=30000)
    assert all(s is Status.OK for s in results.values()), results

    recs = mm_segments(fmt, k, rows, [
        pack_row(fmt, wsegs[r * (k // 32):(r + 1) * (k // 32)])
        for r in range(rows)
    ], act)
    exp_rows = mm_row_results(recs)
    got = words_from_bytes(image[VO_DRAM:VO_DRAM + 4 * rows])
    assert got == [
        fp32_add_bits(f32_bits(exp_rows[r]), resid[r]) for r in range(rows)
    ]


def test_core_store_overlaps_vector():
    """Store an independent live tile while the vector unit streams."""
    import random

    rng = random.Random(26)
    params = dict(PARAMS)
    n = 40
    a = [f32_bits(f32(rng.uniform(-5, 5))) for _ in range(n)]
    b = [f32_bits(f32(rng.uniform(-5, 5))) for _ in range(n)]
    image, la, lb = vector_image(a, b, params)
    expected_copy = bytes(image[VA_DRAM:VA_DRAM + la])
    program = vector_program(Opcode.VECTOR, VecOp.VEC_ADD, n, la, lb)
    idx = next(i for i, e in enumerate(program)
               if e['opcode'] is Opcode.VECTOR)
    program.insert(
        idx + 1,
        dict(opcode=Opcode.STORE_2D,
             seq=40,
             evt=6,
             dram_addr=0x7000,
             tmem_addr=VA_TMEM,
             row_bytes=la,
             rows=1,
             dram_stride=0,
             tmem_stride=0))
    program.insert(-1, dict(opcode=Opcode.WAIT, seq=41, evt=6))
    results, image, _ = run_program(params, program, image)
    assert all(s is Status.OK for s in results.values()), results
    assert image[0x7000:0x7000 + la] == expected_copy
    check_dram_vector(image,
                      [vec_word(VecOp.VEC_ADD, x, y) for x, y in zip(a, b)])


@pytest.mark.parametrize('opcode,op,o_base,why', [
    (Opcode.VECTOR, VecOp.VEC_ADD, VA_TMEM + 8, 'output inside A'),
    (Opcode.REDUCE, VecOp.RDC_SUM, VA_TMEM + 4 * 23, 'reduce output in A'),
])
def test_core_vector_overlap_rejected(opcode, op, o_base, why):
    params = dict(PARAMS)
    program = [
        dict(opcode=opcode,
             seq=1,
             evt=1,
             vec_op=int(op),
             k=24,
             a_base=VA_TMEM,
             m_base=VB_TMEM,
             r_base=o_base),
        dict(opcode=Opcode.WAIT, seq=2, evt=1),
        dict(opcode=Opcode.END, seq=3),
    ]
    image = bytearray(DRAM_SIZE)
    results, _, _ = run_program(params, program, image, deadline=2000)
    assert results == {1: Status.BAD_SHAPE, 2: Status.OK, 3: Status.OK}, \
        results


@pytest.mark.parametrize('opcode,op', [
    (Opcode.REDUCE, VecOp.VEC_ADD),
    (Opcode.VECTOR, VecOp.RDC_SUM),
    (Opcode.VECTOR, 11),
    (Opcode.REDUCE, VecOp.VEC_EXP2),
])
def test_core_vector_bad_op_class(opcode, op):
    params = dict(PARAMS)
    program = [
        dict(opcode=opcode,
             seq=1,
             evt=1,
             vec_op=int(op),
             k=8,
             a_base=VA_TMEM,
             m_base=VB_TMEM,
             r_base=VO_TMEM),
        dict(opcode=Opcode.WAIT, seq=2, evt=1),
        dict(opcode=Opcode.END, seq=3),
    ]
    image = bytearray(DRAM_SIZE)
    results, _, _ = run_program(params, program, image, deadline=2000)
    assert results == {1: Status.BAD_SHAPE, 2: Status.OK, 3: Status.OK}, \
        results


def test_core_vector_after_end_backpressured():
    """END halts issue: a later vector command never completes."""
    params = dict(PARAMS)
    dut = Harness(params, bytearray(DRAM_SIZE))
    seen = []

    def tx():
        for _ in range(4):
            yield
        yield from send_cmd(dut.core.cmd, Opcode.SIGNAL, 1, evt=1)
        yield from send_cmd(dut.core.cmd, Opcode.END, 2)
        yield from send_cmd(dut.core.cmd,
                            Opcode.VECTOR,
                            3,
                            evt=2,
                            vec_op=int(VecOp.VEC_ADD),
                            k=2,
                            a_base=VA_TMEM,
                            m_base=VB_TMEM,
                            r_base=VO_TMEM)
        for _ in range(50):
            yield

    def rx():
        yield dut.core.resp.ready.eq(1)
        for _ in range(80):
            if (yield dut.core.resp.fire):
                seen.append((yield dut.core.resp.bits.seq))
            yield
        assert seen == [1, 2]
        assert (yield dut.core.done)

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(tx)
    sim.add_sync_process(rx)
    sim.run()
