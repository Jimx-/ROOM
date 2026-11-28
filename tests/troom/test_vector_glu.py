"""RTL tests for the fused VEC_SILU_GLU op (VecOp 18, CAPS bit 29).

o[i] = R(R(a[i]*sigmoid(a[i])) * b[i]) -- the fused VEC_SILU op's
every rounding point plus the gate*up product's one rounded multiply,
issued inside the same unary contexts; B (the up row) is a checked
input read once in element order and latched per context at accept.

Parked with the session tree
(results/troom-qwen3-silu-glu-fusion-20261122); installed into
room/tests/troom unchanged at adoption. During candidacy run with
PYTHONPATH=<session>/rtl:<repo>/room:<repo>.

Layers, following ``test_vector_unary.py``:
* the unit against ``tests/troom/model.py``'s composition bit for
  bit: specials, dense pools over both bypass tables against
  arbitrary B, wide beats, backpressure with a partial final beat;
* the identity gate against the standing two-command emission
  (VEC_SILU then VEC_MUL) on one TMEM image -- at the 64-bit default
  geometry, where the commit-readback pattern is sound (it corrupts
  at 512-bit/8-bank pysim on the standing tree too; see the session
  README's artifact note);
* B's checked-input contract: overlap/alignment/range rejections and
  B beats interleaving A in the read stream;
* issue accounting (11 add + 14 mul + 1 seed), context-count
  equivalence, and the full command path through ``Core``.
"""

from amaranth import *
from amaranth.sim import Simulator

import pytest

from troom.consts import Opcode, Status, VecOp

from tests.troom.model import (
    bits_to_f32,
    f32,
    f32_bits,
    fp32_mul_bits,
    vec_silu_bits,
)
from tests.troom.test_full_path import PARAMS, run_program
from tests.troom.test_vector import (
    A_BASE,
    B_BASE,
    O_BASE,
    SPECIALS,
    VO_DRAM,
    expected_beats,
    run_unit,
    words_to_bytes,
    vector_image,
    vector_program,
)

GLU = VecOp.VEC_SILU_GLU


def glu_word(a, b):
    """The frozen composition: silu, then the chain's trailing mul."""
    return fp32_mul_bits(vec_silu_bits(a), b)


def glu_pool(rng):
    """Exponent extremes, table boundaries, both signs, specials."""
    pool = []
    for e in (0, 1, 2, 62, 63, 64, 126, 127, 128, 129, 130, 133, 134, 150, 200,
              252, 253, 254, 255):
        for man in (0, 1, 0x1fffd, 0x1ffff, 0x20000, 0x20001, 0x3ffff, 0x40000,
                    0x7fffe, 0x7fffff):
            pool += [(e << 23) | man, ((e << 23) | man) | 0x80000000]
    pool += SPECIALS
    pool += [f32_bits(f32(rng.uniform(-130, 130))) for _ in range(30)]
    return pool


def b_pool(rng):
    """Everything a checked FP32 input can carry."""
    pool = list(SPECIALS)
    pool += [
        0x00000001, 0x80000001,
        f32_bits(1e-40),
        f32_bits(-1e-40),
        f32_bits(3e38),
        f32_bits(-3e38),
        f32_bits(0.0),
        f32_bits(-0.0)
    ]
    pool += [f32_bits(rng.uniform(-8, 8)) for _ in range(32)]
    pool += [f32_bits(rng.uniform(0.5, 1.5)) for _ in range(32)]
    return pool


# ---------------------------------------------------------------------------
# Unit level, bit for bit against the model
# ---------------------------------------------------------------------------


def test_unit_glu_specials():
    rng = _rng(500)
    words = glu_pool(rng)[:len(SPECIALS) * 2]
    bs = b_pool(rng)
    b = [bs[i % len(bs)] for i in range(len(words))]
    writes, res = run_unit(dict(PARAMS),
                           dict(op=int(GLU), n=len(words)),
                           words_to_bytes(words),
                           b_bytes=words_to_bytes(b),
                           deadline=300000)
    assert res['status'] is Status.OK
    assert res['elements'] == len(words)
    exp = expected_beats(dict(PARAMS),
                         [glu_word(x, y) for x, y in zip(words, b)])
    assert writes == [(O_BASE + 8 * i, d) for i, d in enumerate(exp)]


def test_unit_glu_dense():
    """Every exponent band, table boundaries, clamp neighbors, with
    a mixed B pool -- both bypass tables compose with any up word."""
    rng = _rng(501)
    words = glu_pool(rng)
    bs = b_pool(rng)
    b = [bs[i % len(bs)] for i in range(len(words))]
    writes, res = run_unit(dict(PARAMS),
                           dict(op=int(GLU), n=len(words)),
                           words_to_bytes(words),
                           b_bytes=words_to_bytes(b),
                           deadline=400000)
    assert res['status'] is Status.OK
    exp = expected_beats(dict(PARAMS),
                         [glu_word(x, y) for x, y in zip(words, b)])
    assert writes == [(O_BASE + 8 * i, d) for i, d in enumerate(exp)]


def test_unit_glu_wide_beat():
    """512-bit beats: sixteen words per beat, full plus partial."""
    rng = _rng(502)
    params = dict(PARAMS, data_width=512, n_tmem_banks=8, act_max_k=2048)
    n = 100
    a = [f32_bits(f32(rng.uniform(-40, 40))) for _ in range(n)]
    b = [f32_bits(f32(rng.uniform(0.5, 1.5))) for _ in range(n)]
    writes, res = run_unit(params,
                           dict(op=int(GLU), n=n),
                           words_to_bytes(a),
                           b_bytes=words_to_bytes(b),
                           deadline=300000)
    assert res['status'] is Status.OK
    assert res['elements'] == n
    exp = expected_beats(params, [glu_word(x, y) for x, y in zip(a, b)])
    assert writes == [(O_BASE + 64 * i, d) for i, d in enumerate(exp)]


def test_unit_glu_partial_beat_and_backpressure():
    """37 elements exercise the zero-padded final beat while random
    write readiness stresses the credit accounting."""
    rng = _rng(503)
    params = dict(PARAMS, vec_result_q=2)
    n = 37
    a = glu_pool(rng)[:n]
    b = [f32_bits(f32(rng.uniform(0.5, 1.5))) for _ in range(n)]
    writes, res = run_unit(params,
                           dict(op=int(GLU), n=n),
                           words_to_bytes(a),
                           b_bytes=words_to_bytes(b),
                           stall_wr=True,
                           hold_writes=40,
                           deadline=300000)
    assert res['status'] is Status.OK
    assert res['elements'] == n
    assert res['blocked'] >= 40, res['blocked']
    exp = expected_beats(params, [glu_word(x, y) for x, y in zip(a, b)])
    assert writes == [(O_BASE + 8 * i, d) for i, d in enumerate(exp)]


def test_unit_glu_identity_chain():
    """The fused op is bit-identical to the standing two-command
    emission (VEC_SILU then VEC_MUL) on the same TMEM image: same
    operands, both bypass tables exercised. The 64-bit default
    geometry (the test_unit_silu_equals_chain technique)."""
    rng = _rng(504)
    words = glu_pool(rng)[:40]
    words += [
        0x7f800000, 0xff800000, 0x00000000, 0x7fc00000,
        f32_bits(-89.5),
        f32_bits(89.5),
        f32_bits(-2000.0),
        f32_bits(2000.0),
        f32_bits(-0.5)
    ]
    words += [f32_bits(rng.uniform(-70.0, 70.0)) for _ in range(32)]
    bs = b_pool(rng)
    b = [bs[i % len(bs)] for i in range(len(words))]
    n = len(words)
    params = dict(PARAMS)
    bb = params['data_width'] // 8

    writes, res = run_unit(params,
                           dict(op=int(GLU), n=n),
                           words_to_bytes(words),
                           b_bytes=words_to_bytes(b),
                           deadline=400000)
    assert res['status'] is Status.OK
    fused = expected_beats(params, [glu_word(x, y) for x, y in zip(words, b)])
    assert writes == [(O_BASE + bb * i, d) for i, d in enumerate(fused)]

    r1 = O_BASE + 0x400
    cwrites, cres = run_unit(params, [
        dict(op=int(VecOp.VEC_SILU), n=n, a_base=A_BASE, o_base=r1),
        dict(op=int(VecOp.VEC_MUL),
             n=n,
             a_base=r1,
             b_base=B_BASE,
             o_base=O_BASE)
    ],
                             words_to_bytes(words),
                             b_bytes=words_to_bytes(b),
                             commit_writes=True,
                             deadline=600000)
    assert all(r['status'] is Status.OK for r in cres)
    by_addr = {a: d for a, d in cwrites}
    chain = [by_addr.get(O_BASE + bb * i) for i in range(len(fused))]
    assert chain == fused, 'fused VEC_SILU_GLU differs from the chain'


def test_unit_glu_issue_accounting():
    """The op's whole-element issue demand is 11 add + 14 mul + 1
    seed -- the chain's 26th issue folded into the fused op."""
    n = 96
    a = [0x3fa00000] * n
    b = [f32_bits(1.25)] * n
    params = dict(PARAMS, vec_unary_ctx=32, vec_add_units=4, vec_mul_units=4)
    writes, res = run_unit(params,
                           dict(op=int(GLU), n=n),
                           words_to_bytes(a),
                           b_bytes=words_to_bytes(b))
    assert res['status'] is Status.OK
    assert res['add_issues'] == 11 * n, res
    assert res['mul_issues'] == 14 * n, res
    assert res['rec_issues'] == n, res


def test_unit_glu_reads_b_stream():
    """B is a real input: its beats interleave A's in the read
    stream, in region order."""
    words = [f32_bits(1.25 + i / 8) for i in range(12)]
    b = [f32_bits(0.75 + i / 8) for i in range(12)]
    _, res = run_unit(dict(PARAMS),
                      dict(op=int(GLU), n=len(words)),
                      words_to_bytes(words),
                      b_bytes=words_to_bytes(b))
    assert res['status'] is Status.OK
    a_beats = [A_BASE + off for off in range(0, 4 * len(words), 8)]
    b_beats = [B_BASE + off for off in range(0, 4 * len(words), 8)]
    assert res['reads'][0] == A_BASE
    assert sorted(res['reads']) == sorted(a_beats + b_beats)


@pytest.mark.parametrize('desc,status,why', [
    (dict(op=int(GLU), n=8,
          o_base=B_BASE), Status.BAD_SHAPE, 'output inside B'),
    (dict(op=int(GLU), n=8,
          b_base=B_BASE + 4), Status.BAD_ALIGN, 'B misaligned'),
    (dict(op=int(GLU), n=8, b_base=0x3ff8), Status.BAD_RANGE, 'B beyond TMEM'),
    (dict(op=int(GLU), n=8,
          o_base=A_BASE + 4), Status.BAD_SHAPE, 'output inside A'),
    (dict(op=int(GLU), n=0), Status.BAD_SHAPE, 'zero elements'),
])
def test_unit_glu_rejects(desc, status, why):
    from troom.vector import VectorUnit

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
            yield
        yield sink.valid.eq(0)

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(tx)
    sim.run()


@pytest.mark.parametrize('ctx', [1, 2, 4, 8, 17])
def test_unit_glu_context_equivalence(ctx):
    """The interleaved sequencer is result-identical across context
    counts: the per-element sequence is untouched, so every count
    produces the same beats over the dense pool."""
    rng = _rng(505 + ctx)
    words = glu_pool(rng)[:96] + SPECIALS
    bs = b_pool(rng)
    b = [bs[i % len(bs)] for i in range(len(words))]
    params = dict(PARAMS, vec_unary_ctx=ctx)
    writes, res = run_unit(params,
                           dict(op=int(GLU), n=len(words)),
                           words_to_bytes(words),
                           b_bytes=words_to_bytes(b),
                           deadline=400000)
    assert res['status'] is Status.OK
    exp = expected_beats(params, [glu_word(x, y) for x, y in zip(words, b)])
    assert writes == [(O_BASE + 8 * i, d) for i, d in enumerate(exp)]


# ---------------------------------------------------------------------------
# Full command path through Core
# ---------------------------------------------------------------------------


def test_core_glu_full_path():
    rng = _rng(510)
    params = dict(PARAMS)
    n = 24
    a = [f32_bits(f32(rng.uniform(-30, 30))) for _ in range(n)]
    a[3] = 0x7fc00000
    a[7] = 0x7f800000
    a[11] = 0x80000000
    a[15] = f32_bits(-127.0)
    a[19] = 0x00000001
    b = [f32_bits(f32(rng.uniform(0.5, 1.5))) for _ in range(n)]
    b[5] = 0x7fc00000
    b[9] = f32_bits(3e38)
    image, la, lb = vector_image(a, b, params)
    program = vector_program(Opcode.VECTOR, GLU, n, la, lb)
    results, image, _ = run_program(params, program, image, deadline=40000)
    assert all(s is Status.OK for s in results.values()), results
    blob = words_to_bytes([glu_word(x, y) for x, y in zip(a, b)])
    assert image[VO_DRAM:VO_DRAM + len(blob)] == blob


def _rng(seed):
    import random

    return random.Random(seed)
