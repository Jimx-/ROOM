"""RTL tests for the troom unary transcendental ops (VEC_EXP2,
VEC_RECIP, VEC_RSQRT).

Layers, following ``test_vector.py``:

* the ``FPURec`` seed block against the model's bit-exact seed
  functions over the positive normal domain the sequencer feeds it,
* the vector unit against ``tests/troom/model.py`` bit for bit: the
  full special-value pool, a dense exponent/table-boundary pool per
  op, final-beat padding under write backpressure, the wide
  (512-bit) beat configuration, back-to-back commands sharing the
  arithmetic ports with the binary ops, ignored B regions, and
  descriptor rejection,
* the full command path through ``Core`` per op, plus an RMSNorm-
  shaped reduction -> scale -> add -> rsqrt -> multiply chain with
  ``k = 1`` commands,
* a per-element cycle measurement documenting the serialized
  sequencer's cost (the plan's pre-interleaving number).
"""

from amaranth import *
from amaranth.sim import Simulator

import pytest

from room.consts import RoundingMode
from room.fpu import FPFormat, FPURec, FPUOperator

from troom.consts import Opcode, Status, VecOp

from tests.troom.model import (
    bits_to_f32,
    f32,
    f32_bits,
    fp32_add_bits,
    fp32_mul_bits,
    fp32_rec_seed_bits,
    fp32_rsqrt_seed_bits,
    rdc_result,
    vec_recip_bits,
    vec_rsqrt_bits,
    vec_word,
)
from tests.troom.test_full_path import PARAMS, run_program
from tests.troom.test_vector import (
    A_BASE,
    B_BASE,
    O_BASE,
    SPECIALS,
    expected_beats,
    run_unit,
    words_to_bytes,
    VA_DRAM,
    VB_DRAM,
    VO_DRAM,
    VA_TMEM,
    VB_TMEM,
    VO_TMEM,
    vector_image,
    vector_program,
)

UNARY_OPS = (VecOp.VEC_EXP2, VecOp.VEC_RECIP, VecOp.VEC_RSQRT)


def un_word(op, x):
    return vec_word(op, x, 0)


def unary_pool(rng):
    """Exponent extremes, table-index boundaries, both signs, the
    special pool, and random in-domain arguments."""
    pool = []
    for e in (0, 1, 2, 62, 63, 64, 126, 127, 128, 129, 130, 133, 134, 150, 200,
              252, 253, 254, 255):
        for man in (0, 1, 0x1fffd, 0x1ffff, 0x20000, 0x20001, 0x3ffff, 0x40000,
                    0x7fffe, 0x7fffff):
            pool += [(e << 23) | man, ((e << 23) | man) | 0x80000000]
    pool += SPECIALS
    pool += [f32_bits(f32(rng.uniform(-130, 130))) for _ in range(30)]
    return pool


# ---------------------------------------------------------------------------
# FPURec seed block vs the model seeds
# ---------------------------------------------------------------------------


def frec_run(inputs, fn_mod):
    """Drive FPURec one input at a time; collect out.data."""
    dut = FPURec(32, FPFormat.S, latency=3)
    out = []

    def tx():
        for x in inputs:
            yield dut.inp.bits.in1.eq(x)
            yield dut.inp.bits.fn.eq(FPUOperator.REC)
            yield dut.inp.bits.fn_mod.eq(fn_mod)
            yield dut.inp.bits.rm.eq(RoundingMode.RNE)
            yield dut.inp.valid.eq(1)
            yield
            yield dut.inp.valid.eq(0)
            responses = []
            for _ in range(8):
                if (yield dut.out.valid):
                    responses.append((yield dut.out.bits.data))
                yield
            assert len(responses) == 1, (hex(x), responses)
            out.extend(responses)

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(tx)
    sim.run()
    return out


@pytest.mark.parametrize('fn_mod,model', [
    (0, fp32_rec_seed_bits),
    (1, fp32_rsqrt_seed_bits),
])
def test_frec_seed_matches_model(fn_mod, model):
    import random

    rng = random.Random(200 + fn_mod)
    top = 252 if fn_mod == 0 else 254
    xs = []
    for e in range(1, top + 1):
        for man in (0, 1, 0x1ffff, 0x20000, 0x3ffff, 0x7ffff,
                    rng.randrange(1 << 23)):
            xs.append((e << 23) | man)
    got = frec_run(xs, fn_mod)
    for x, y in zip(xs, got):
        assert y == model(x), (hex(x), hex(y), hex(model(x)))


# ---------------------------------------------------------------------------
# Unit level, bit for bit against the model
# ---------------------------------------------------------------------------


@pytest.mark.parametrize('op', UNARY_OPS)
def test_unit_unary_specials(op):
    words = SPECIALS
    writes, res = run_unit(dict(PARAMS), dict(op=int(op), n=len(words)),
                           words_to_bytes(words))
    assert res['status'] is Status.OK
    assert res['elements'] == len(words)
    exp = expected_beats(dict(PARAMS), [un_word(op, w) for w in words])
    assert res['write_beats'] == len(exp)
    assert writes == [(O_BASE + 8 * i, d) for i, d in enumerate(exp)]


@pytest.mark.parametrize('op', UNARY_OPS)
def test_unit_unary_dense(op):
    """Every exponent band, table boundaries, clamp neighbors."""
    import random

    rng = random.Random(300 + int(op))
    words = unary_pool(rng)
    writes, res = run_unit(dict(PARAMS),
                           dict(op=int(op), n=len(words)),
                           words_to_bytes(words),
                           deadline=300000)
    assert res['status'] is Status.OK
    assert res['elements'] == len(words)
    exp = expected_beats(dict(PARAMS), [un_word(op, w) for w in words])
    assert res['write_beats'] == len(exp)
    assert writes == [(O_BASE + 8 * i, d) for i, d in enumerate(exp)]


def test_unit_unary_partial_beat_and_backpressure():
    """37 elements exercise the zero-padded final beat while random
    write readiness stresses the credit accounting."""
    import random

    rng = random.Random(31)
    params = dict(PARAMS, vec_result_q=2)
    n = 37
    a = unary_pool(rng)[:n]
    writes, res = run_unit(params,
                           dict(op=int(VecOp.VEC_RSQRT), n=n),
                           words_to_bytes(a),
                           stall_wr=True,
                           hold_writes=40,
                           deadline=300000)
    assert res['status'] is Status.OK
    assert res['elements'] == n
    assert res['blocked'] >= 40, res['blocked']
    assert res['stall_cycles'] > 0
    exp = expected_beats(params, [un_word(VecOp.VEC_RSQRT, w) for w in a])
    assert writes == [(O_BASE + 8 * i, d) for i, d in enumerate(exp)]


def test_unit_unary_wide_beat():
    """512-bit beats: sixteen words per beat, six full plus partial."""
    import random

    rng = random.Random(32)
    params = dict(PARAMS, data_width=512, n_tmem_banks=8, act_max_k=2048)
    n = 100
    a = [f32_bits(f32(rng.uniform(-40, 40))) for _ in range(n)]
    for op in UNARY_OPS:
        writes, res = run_unit(params,
                               dict(op=int(op), n=n),
                               words_to_bytes(a),
                               deadline=300000)
        assert res['status'] is Status.OK
        assert res['elements'] == n
        exp = expected_beats(params, [un_word(op, w) for w in a])
        assert writes == [(O_BASE + 64 * i, d) for i, d in enumerate(exp)]


@pytest.mark.parametrize('op', UNARY_OPS)
@pytest.mark.parametrize('b_base', [O_BASE, O_BASE + 1, 0x3ff8])
def test_unit_unary_ignores_b_region(op, b_base):
    """Ignore B even when it aliases O, is misaligned, or its vector
    region would extend beyond TMEM. Only A read requests may fire."""
    params = dict(PARAMS)
    words = [f32_bits(1.25 + i / 8) for i in range(12)]
    writes, res = run_unit(params, dict(op=int(op),
                                        n=len(words),
                                        b_base=b_base), words_to_bytes(words))
    assert res['status'] is Status.OK
    assert res['elements'] == len(words)
    assert res['reads'] == [
        A_BASE + off for off in range(0, 4 * len(words), 8)
    ]
    exp = expected_beats(params, [un_word(op, w) for w in words])
    assert writes == [(O_BASE + 8 * i, d) for i, d in enumerate(exp)]


def test_unit_unary_back_to_back_and_ports_shared():
    """Unary then binary then reduction commands share the add/mul
    ports and the credit accounting across command boundaries."""
    import random

    rng = random.Random(34)
    params = dict(PARAMS)
    n = 9
    a = [f32_bits(f32(rng.uniform(-20, 20))) for _ in range(n)]
    b = [f32_bits(f32(rng.uniform(-20, 20))) for _ in range(n)]
    descs = [
        dict(op=int(VecOp.VEC_EXP2), n=n, o_base=O_BASE),
        dict(op=int(VecOp.VEC_ADD), n=n, o_base=O_BASE + 0x100),
        dict(op=int(VecOp.VEC_RECIP), n=n, o_base=O_BASE + 0x200),
        dict(op=int(VecOp.RDC_SUMSQ), n=n, o_base=O_BASE + 0x300),
        dict(op=int(VecOp.VEC_MUL), n=n, o_base=O_BASE + 0x400),
        dict(op=int(VecOp.VEC_RSQRT), n=n, o_base=O_BASE + 0x500),
    ]
    writes, res = run_unit(params,
                           descs,
                           words_to_bytes(a),
                           words_to_bytes(b),
                           deadline=300000)
    assert all(r['status'] is Status.OK for r in res), res

    seqs = [
        [un_word(VecOp.VEC_EXP2, w) for w in a],
        [vec_word(VecOp.VEC_ADD, x, y) for x, y in zip(a, b)],
        [un_word(VecOp.VEC_RECIP, w) for w in a],
        [rdc_result(VecOp.RDC_SUMSQ, a)],
        [vec_word(VecOp.VEC_MUL, x, y) for x, y in zip(a, b)],
        [un_word(VecOp.VEC_RSQRT, w) for w in a],
    ]
    pos = 0
    for desc, seq in zip(descs, seqs):
        exp = expected_beats(params, seq)
        chunk = writes[pos:pos + len(exp)]
        pos += len(exp)
        assert chunk == [(desc['o_base'] + 8 * i, d)
                         for i, d in enumerate(exp)]
    assert pos == len(writes)
    # Elements accrue per command; the elementwise commands write
    # ceil(9*4/8) = 5 beats each, the reduction one.
    assert [r['elements'] for r in res] == [9 * (i + 1) for i in range(6)]
    assert [r['write_beats'] for r in res] == [5, 10, 15, 16, 21, 26]


@pytest.mark.parametrize('desc,status,why', [
    (dict(op=int(VecOp.VEC_EXP2), n=0), Status.BAD_SHAPE, 'zero elements'),
    (dict(op=11, n=8), Status.BAD_SHAPE, 'reserved sub-op encoding'),
    (dict(op=int(VecOp.VEC_RECIP), n=8,
          o_base=A_BASE + 4), Status.BAD_SHAPE, 'output inside A'),
    (dict(op=int(VecOp.VEC_RSQRT), n=8,
          a_base=0x3ff0), Status.BAD_RANGE, 'A beyond TMEM'),
])
def test_unit_unary_rejects_bad_descriptors(desc, status, why):
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
            assert not (yield dut.active), f'{why}: active after reject'
            yield
        yield sink.valid.eq(0)

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(tx)
    sim.run()


def test_unit_unary_cycles_per_element():
    """Document the serialized sequencer's cost: every step is one
    issue cycle plus the three-cycle FPU latency, so recip (seed +
    6 ops) < 40 cycles, rsqrt (seed + 9) < 55, exp2 (15 ops) < 80,
    bypasses < 6. These are upper bounds for regression alarms, not
    the tight per-op numbers."""
    params = dict(PARAMS)
    # Every input executes the complete arithmetic sequence for all ops.
    seq = [f32_bits(1.25)] * 40
    fast = [0x7fc00000, 0x00000000, 0x7f800000, 0x00000001] * 10
    for op, bound in ((VecOp.VEC_RECIP, 40), (VecOp.VEC_RSQRT, 55),
                      (VecOp.VEC_EXP2, 80)):
        _, res0 = run_unit(params,
                           dict(op=int(op), n=2),
                           words_to_bytes(seq[:2]),
                           deadline=300000)
        _, res1 = run_unit(params,
                           dict(op=int(op), n=len(seq)),
                           words_to_bytes(seq),
                           deadline=300000)
        per = (res1['execution_cycles'] - res0['execution_cycles']) \
            / (len(seq) - 2)
        assert per <= bound, (op, per)
    _, resb = run_unit(params,
                       dict(op=int(VecOp.VEC_RECIP), n=len(fast)),
                       words_to_bytes(fast),
                       deadline=300000)
    per = resb['execution_cycles'] / len(fast)
    assert per <= 6, per


# ---------------------------------------------------------------------------
# Full command path through Core
# ---------------------------------------------------------------------------


def check_dram_vector(image, words):
    blob = words_to_bytes(words)
    got = image[VO_DRAM:VO_DRAM + len(blob)]
    assert got == blob, (got.hex(), blob.hex())


@pytest.mark.parametrize('op', UNARY_OPS)
def test_core_unary_full_path(op):
    import random

    rng = random.Random(400 + int(op))
    params = dict(PARAMS)
    n = 24
    a = [f32_bits(f32(rng.uniform(-30, 30))) for _ in range(n)]
    a[3] = 0x7fc00000
    a[7] = 0x7f800000
    a[11] = 0x80000000
    a[15] = f32_bits(-127.0)
    a[19] = 0x00000001
    b = [f32_bits(1.0)]  # never read; loaded to prove it stays inert
    image, la, lb = vector_image(a, b, params)
    program = vector_program(Opcode.VECTOR, op, n, la, lb)
    results, image, _ = run_program(params, program, image, deadline=40000)
    assert all(s is Status.OK for s in results.values()), results
    check_dram_vector(image, [un_word(op, w) for w in a])


def test_core_unary_rmsnorm_chain():
    """The RMSNorm composition: RDC_SUMSQ, scale by 1/H (k = 1), add
    eps, VEC_RSQRT (k = 1), then VEC_SCALE by the result."""
    import random

    rng = random.Random(401)
    params = dict(PARAMS)
    n = 16
    xs = [rng.uniform(-3, 3) for _ in range(n)]
    a = [f32_bits(v) for v in xs]
    inv_h = f32_bits(1.0 / n)
    eps = f32_bits(1e-6)
    ones = [f32_bits(1.0)] * n

    image, la, lb = vector_image(a, ones, params)
    # Constants after B's region: the 1/H scalar, then the eps vector.
    image[VB_DRAM + 0x100:VB_DRAM + 0x104] = inv_h.to_bytes(4, 'little')
    image[VB_DRAM + 0x200:VB_DRAM + 0x200 + 4 * n] = \
        eps.to_bytes(4, 'little') * n

    def vec(seq, op, k, a_base, m_base, r_base, evt):
        return dict(opcode=Opcode.VECTOR,
                    seq=seq,
                    evt=evt,
                    vec_op=int(op),
                    k=k,
                    a_base=a_base,
                    m_base=m_base,
                    r_base=r_base)

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
        dict(opcode=Opcode.LOAD_2D,
             seq=3,
             evt=3,
             dram_addr=VB_DRAM + 0x100,
             tmem_addr=0x3800,
             row_bytes=8,
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.LOAD_2D,
             seq=4,
             evt=4,
             dram_addr=VB_DRAM + 0x200,
             tmem_addr=0x3900,
             row_bytes=la,
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.WAIT, seq=5, evt=1),
        dict(opcode=Opcode.WAIT, seq=6, evt=2),
        dict(opcode=Opcode.WAIT, seq=7, evt=3),
        dict(opcode=Opcode.WAIT, seq=8, evt=4),
        dict(opcode=Opcode.REDUCE,
             seq=9,
             evt=5,
             vec_op=int(VecOp.RDC_SUMSQ),
             k=n,
             a_base=VA_TMEM,
             m_base=VB_TMEM,
             r_base=VO_TMEM),
        dict(opcode=Opcode.WAIT, seq=10, evt=5),
        vec(11, VecOp.VEC_SCALE, 1, VO_TMEM, 0x3800, VO_TMEM + 0x10, 5),
        dict(opcode=Opcode.WAIT, seq=12, evt=5),
        vec(13, VecOp.VEC_ADD, 1, VO_TMEM + 0x10, 0x3900, VO_TMEM + 0x20, 5),
        dict(opcode=Opcode.WAIT, seq=14, evt=5),
        vec(15, VecOp.VEC_RSQRT, 1, VO_TMEM + 0x20, VB_TMEM, 0x3a00, 5),
        dict(opcode=Opcode.WAIT, seq=16, evt=5),
        vec(17, VecOp.VEC_SCALE, n, VA_TMEM, 0x3a00, VO_TMEM, 6),
        dict(opcode=Opcode.WAIT, seq=18, evt=6),
        dict(opcode=Opcode.STORE_2D,
             seq=19,
             evt=7,
             dram_addr=VO_DRAM,
             tmem_addr=VO_TMEM,
             row_bytes=la,
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.WAIT, seq=20, evt=7),
        dict(opcode=Opcode.END, seq=21),
    ]
    results, image, _ = run_program(params, program, image, deadline=40000)
    assert all(s is Status.OK for s in results.values()), results

    ss = rdc_result(VecOp.RDC_SUMSQ, a)
    mean = fp32_mul_bits(ss, inv_h)
    inv = vec_rsqrt_bits(fp32_add_bits(mean, eps))
    exp = [fp32_mul_bits(w, inv) for w in a]
    check_dram_vector(image, exp)
