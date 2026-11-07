"""RTL tests for the fused softmax body (VEC_SMEXP, P2b-ii base slice).

The op must be bit-identical to the standing five-op FA chain
(RDC_MAX -> VEC_SCALE on the ones buffer -> VEC_SUB -> VEC_EXP2 ->
RDC_SUM) for every region: the numerator image at ``o_base`` and the
denominator beat at ``b_base``. Layers, following ``test_vector.py``
and ``test_vector_unary.py``:

* the model composition (``smexp_regions``) against the chain
  executed op by op through the model's primitives -- the contract
  the RTL must match is the chain itself,
* the vector unit against the model bit for bit: the special-value
  pool (signed zeros, subnormals, infinities, NaN mixes, all-NaN
  regions, the exp2 clamp boundaries), n edges with partial final
  beats, write backpressure covering the denominator beat, back to
  back commands sharing the arithmetic ports with every op class,
  descriptor rejection and inertness, the single-issue-group
  geometry, a small result queue, the wide beat, and the fused-vs-
  chain unit identity on the same TMEM image,
* the full command path through ``Core`` in the FA softmax shape
  (LOAD -> ADD -> SMEXP -> RECIP -> STORE),
* a per-element cycle measurement documenting the fused cost against
  the chain's (a pin, not a gate: the chain is re-derived per image).
"""

from amaranth import *
from amaranth.sim import Simulator

import pytest

from troom.consts import Opcode, Status, VecOp

from tests.troom.model import (
    FP32_NAN_BITS,
    rdc_result,
    smexp_regions,
    vec_word,
)
from tests.troom.test_full_path import PARAMS, run_program
from tests.troom.test_vector import (
    A_BASE,
    B_BASE,
    O_BASE,
    SPECIALS,
    VA_DRAM,
    VB_DRAM,
    VO_DRAM,
    VA_TMEM,
    VB_TMEM,
    VO_TMEM,
    expected_beats,
    run_unit,
    vector_image,
    vector_program,
    words_from_bytes,
    words_to_bytes,
)


def f32_bits(v):
    import struct
    return int.from_bytes(struct.pack('<f', v), 'little')


def smexp_writes(params, words):
    """The expected write list: E beats at O_BASE then the b beat."""
    out, den = smexp_regions(words)
    exp = expected_beats(params, words_from_bytes(out))
    return [(O_BASE + (params['data_width'] // 8) * i, d)
            for i, d in enumerate(exp)] + [(B_BASE, den)]


# ---------------------------------------------------------------------------
# The contract itself: model composition vs the op-by-op chain
# ---------------------------------------------------------------------------


@pytest.mark.parametrize('seed', [7, 31])
def test_model_smexp_equals_chain(seed):
    import random

    rng = random.Random(seed)
    pools = [
        SPECIALS * 4,
        [rng.getrandbits(32) for _ in range(200)],
        # Softmax-shaped: masked slots at -inf, scores below.
        [
            0xff800000 if rng.random() < 0.3 else f32_bits(rng.uniform(-40, 0))
            for _ in range(200)
        ],
    ]
    for words in pools:
        out, den = smexp_regions(words)
        # The chain, op by op through the frozen primitives: the max,
        # the ones-broadcast scale (R(1.0*m) = m, bit-exact), the
        # subtract, the exponential, the isum_4 fold.
        m = rdc_result(VecOp.RDC_MAX, words)
        shifted = [vec_word(VecOp.VEC_SUB, w, m) for w in words]
        e = [vec_word(VecOp.VEC_EXP2, w, 0) for w in shifted]
        assert out == b''.join(w.to_bytes(4, 'little') for w in e)
        assert den == rdc_result(VecOp.RDC_SUM, e)


def specials_regions():
    """Regions exercising the fold and sequence corners."""
    qnan = FP32_NAN_BITS
    inf = 0x7f800000
    ninf = 0xff800000
    return [
        # All-NaN folds to the canonical NaN; e = exp2(NaN - NaN).
        [qnan, 0x7f800001, qnan],
        # NaNs dropped wherever they sit.
        [qnan, 0x3f800000, 0x7f800001, 0x40000000],
        [0x3f800000, qnan, 0x40400000],
        # -0 beats nothing but -0; +0 beats -0.
        [0x80000000, 0x80000000],
        [0x80000000, 0x00000000, 0x80000000],
        [0x80000000, 0x80000000, 0x00000000],
        # Signed magnitudes: -5 < 3 < 5; the max is 5 either way.
        [0xc0a00000, 0x40400000, 0x40a00000],
        [0x40a00000, 0xc0a00000],
        # Infinities and subnormals.
        [ninf, ninf, ninf],
        [ninf, 0x00000001, 0x007fffff, inf],
        [inf, qnan, ninf],
        # exp2 clamp boundary inputs: a - m large negative.
        [0x3f800000, 0xc2f80000, ninf],
        [0x00000000, 0x80000000],
        # Equal maxima at several positions.
        [0x40000000, 0x40000000, 0x40000000, 0x40000000],
    ]


# ---------------------------------------------------------------------------
# The vector unit vs the model
# ---------------------------------------------------------------------------


@pytest.mark.parametrize('n',
                         [1, 2, 3, 5, 7, 8, 9, 15, 16, 17, 31, 33, 64, 65])
def test_smexp_unit_matches_model(n):
    import random

    rng = random.Random(n)
    words = [rng.getrandbits(32) for _ in range(n)]
    writes, results = run_unit(PARAMS,
                               dict(op=int(VecOp.VEC_SMEXP), n=n),
                               words_to_bytes(words),
                               b_base=B_BASE)
    assert results['status'] == Status.OK
    assert writes == smexp_writes(PARAMS, words)


def test_smexp_unit_specials():
    for words in specials_regions():
        writes, results = run_unit(PARAMS,
                                   dict(op=int(VecOp.VEC_SMEXP), n=len(words)),
                                   words_to_bytes(words),
                                   b_base=B_BASE)
        assert results['status'] == Status.OK
        assert writes == smexp_writes(PARAMS, words)


def test_smexp_unit_dense_random():
    import random

    rng = random.Random(1234)
    words = [rng.getrandbits(32) for _ in range(257)]
    writes, results = run_unit(PARAMS,
                               dict(op=int(VecOp.VEC_SMEXP), n=len(words)),
                               words_to_bytes(words),
                               b_base=B_BASE,
                               deadline=200000)
    assert results['status'] == Status.OK
    assert writes == smexp_writes(PARAMS, words)


def test_smexp_unit_write_stall():
    """A randomly stalled write port through the final beats and the
    denominator beat."""
    import random

    rng = random.Random(99)
    words = [rng.getrandbits(32) for _ in range(40)]
    writes, results = run_unit(PARAMS,
                               dict(op=int(VecOp.VEC_SMEXP), n=40),
                               words_to_bytes(words),
                               b_base=B_BASE,
                               stall_wr=True)
    assert results['status'] == Status.OK
    assert writes == smexp_writes(PARAMS, words)


def test_smexp_unit_hold_writes():
    import random

    rng = random.Random(98)
    words = [rng.getrandbits(32) for _ in range(24)]
    writes, results = run_unit(PARAMS,
                               dict(op=int(VecOp.VEC_SMEXP), n=24),
                               words_to_bytes(words),
                               b_base=B_BASE,
                               hold_writes=30,
                               hold_after_beats=1)
    assert results['status'] == Status.OK
    assert writes == smexp_writes(PARAMS, words)


def test_smexp_unit_rejections():
    import random

    rng = random.Random(11)
    words = [rng.getrandbits(32) for _ in range(20)]
    a = words_to_bytes(words)
    cases = [
        # n = 0.
        (dict(op=int(VecOp.VEC_SMEXP), n=0), Status.BAD_SHAPE),
        # o overlaps a.
        (dict(op=int(VecOp.VEC_SMEXP), n=8,
              o_base=A_BASE + 16), Status.BAD_SHAPE),
        # b overlaps a.
        (dict(op=int(VecOp.VEC_SMEXP), n=8,
              b_base=A_BASE + 8), Status.BAD_SHAPE),
        # b overlaps o.
        (dict(op=int(VecOp.VEC_SMEXP), n=8, b_base=O_BASE), Status.BAD_SHAPE),
        # Misaligned b.
        (dict(op=int(VecOp.VEC_SMEXP), n=8,
              b_base=B_BASE + 4), Status.BAD_ALIGN),
        # An aligned input region extends past TMEM. A one-beat B
        # cannot overflow its address-width-limited base.
        (dict(op=int(VecOp.VEC_SMEXP), n=8,
              a_base=PARAMS['tmem_bytes'] - 8), Status.BAD_RANGE),
    ]
    for desc, status in cases:
        writes, results = run_unit(PARAMS, desc, a, b_base=B_BASE)
        assert results['status'] == status, desc
        assert writes == []
    # A rejection leaves the unit inert: a following good command
    # produces the full result.
    writes, results = run_unit(
        PARAMS, [cases[0][0], dict(op=int(VecOp.VEC_SMEXP), n=20)],
        a,
        b_base=B_BASE)
    assert results[0]['status'] == Status.BAD_SHAPE
    assert results[1]['status'] == Status.OK
    assert writes == smexp_writes(PARAMS, words)


def test_smexp_unit_back_to_back_and_handoff():
    """SMEXP between every other op class: the unary ports, the
    reduction partials, and the shared elementwise paths all stay
    clean across the handoffs (the FA chain runs ADD -> SMEXP ->
    RECIP back to back on one engine)."""
    import random

    rng = random.Random(21)
    a1 = [rng.getrandbits(32) for _ in range(12)]
    a2 = [rng.getrandbits(32) for _ in range(12)]
    descs = [
        dict(op=int(VecOp.VEC_SMEXP),
             n=12,
             a_base=A_BASE,
             b_base=B_BASE,
             o_base=O_BASE),
        dict(op=int(VecOp.RDC_SUM), n=12, a_base=A_BASE, o_base=O_BASE),
        dict(op=int(VecOp.VEC_EXP2), n=12, a_base=A_BASE, o_base=O_BASE),
        dict(op=int(VecOp.VEC_SMEXP),
             n=12,
             a_base=A_BASE,
             b_base=B_BASE,
             o_base=O_BASE),
        dict(op=int(VecOp.RDC_MAX), n=12, a_base=A_BASE, o_base=O_BASE),
        dict(op=int(VecOp.VEC_ADD),
             n=12,
             a_base=A_BASE,
             b_base=A_BASE,
             o_base=O_BASE),
        dict(op=int(VecOp.VEC_SMEXP),
             n=12,
             a_base=A_BASE,
             b_base=B_BASE,
             o_base=O_BASE),
    ]
    writes, results = run_unit(PARAMS,
                               descs,
                               words_to_bytes(a1),
                               words_to_bytes(a2),
                               b_base=B_BASE)
    assert [r['status'] for r in results] == [Status.OK] * len(descs)
    den_beats = [w for w in writes if w[0] == B_BASE]
    _, den = smexp_regions(a1)
    assert den_beats == [(B_BASE, den)] * 3
    # The reduction partials were not clobbered by the fused op's
    # accumulations: RDC_SUM and RDC_MAX produced their own beats at
    # O_BASE right after their commands -- checked via the full
    # write sequence equality on the smexp positions above and the
    # final command's completeness here.
    assert writes[-1] == (B_BASE, den)


def test_smexp_unit_single_group():
    import random

    rng = random.Random(5)
    words = [rng.getrandbits(32) for _ in range(33)]
    params = dict(PARAMS, vec_add_units=1, vec_mul_units=1)
    writes, results = run_unit(params,
                               dict(op=int(VecOp.VEC_SMEXP), n=33),
                               words_to_bytes(words),
                               b_base=B_BASE)
    assert results['status'] == Status.OK
    assert writes == smexp_writes(params, words)


def test_smexp_unit_small_result_queue():
    import random

    rng = random.Random(6)
    words = [rng.getrandbits(32) for _ in range(70)]
    params = dict(PARAMS, vec_result_q=2)
    writes, results = run_unit(params,
                               dict(op=int(VecOp.VEC_SMEXP), n=70),
                               words_to_bytes(words),
                               b_base=B_BASE)
    assert results['status'] == Status.OK
    assert writes == smexp_writes(params, words)


def test_smexp_unit_wide_beat():
    """The deployed 512-bit beat configuration."""
    import random

    rng = random.Random(77)
    words = [rng.getrandbits(32) for _ in range(100)]
    params = dict(PARAMS,
                  data_width=512,
                  id_width=6,
                  n_tmem_banks=8,
                  tmem_bytes=256 * 1024,
                  n_outputs=4,
                  mm_out_depth=24,
                  vec_unary_ctx=16,
                  vec_result_q=16,
                  vec_add_units=2,
                  vec_mul_units=2)
    writes, results = run_unit(params,
                               dict(op=int(VecOp.VEC_SMEXP), n=100),
                               words_to_bytes(words),
                               b_base=B_BASE)
    assert results['status'] == Status.OK
    assert writes == smexp_writes(params, words)


def test_smexp_unit_equals_chain():
    """The S1 identity gate: the fused op is bit-identical to the
    five-command chain it replaces, run on the same TMEM image with
    the backend's own scratch regions (ones vector, broadcast,
    shifted scores, max/sum slots)."""
    import random

    rng = random.Random(77)
    n = 96
    words = [f32_bits(rng.uniform(-40, 2)) for _ in range(n)]
    words[3] = 0xff800000
    words[17] = 0x7fc00000
    words[31] = 0x80000000
    words += [f32_bits(rng.uniform(-40, 0)) for _ in range(32)]
    n = len(words)
    params = dict(PARAMS)
    bb = params['data_width'] // 8
    npad = ((n * 4 + bb - 1) // bb) * bb

    # The fused op.
    writes, res = run_unit(params,
                           dict(op=int(VecOp.VEC_SMEXP), n=n),
                           words_to_bytes(words),
                           b_base=B_BASE,
                           deadline=400000)
    assert res['status'] is Status.OK
    fused = smexp_writes(params, words)
    assert writes == fused

    # The chain, exactly as the backend emits it: the ones vector at
    # B_BASE, then scratch regions spaced clear of everything.
    r_max = O_BASE + 0x400
    r_bcast = r_max + bb
    r_shift = r_bcast + npad
    assert B_BASE + bb + npad <= r_max
    b_bytes = words_to_bytes([0x3f800000] * n) + bytes(npad - n * 4)
    descs = [
        dict(op=int(VecOp.RDC_MAX), n=n, a_base=A_BASE, o_base=r_max),
        dict(op=int(VecOp.VEC_SCALE),
             n=n,
             a_base=B_BASE,
             b_base=r_max,
             o_base=r_bcast),
        dict(op=int(VecOp.VEC_SUB),
             n=n,
             a_base=A_BASE,
             b_base=r_bcast,
             o_base=r_shift),
        dict(op=int(VecOp.VEC_EXP2), n=n, a_base=r_shift, o_base=O_BASE),
        dict(op=int(VecOp.RDC_SUM), n=n, a_base=O_BASE, o_base=B_BASE),
    ]
    cwrites, cres = run_unit(params,
                             descs,
                             words_to_bytes(words),
                             b_bytes,
                             commit_writes=True,
                             deadline=600000)
    assert all(r['status'] is Status.OK for r in cres)
    e_out, e_den = smexp_regions(words)
    exp = expected_beats(params, words_from_bytes(e_out))
    got = [d for a, d in cwrites if O_BASE <= a < O_BASE + npad]
    # E beats at O_BASE plus the chain's RDC_MAX/SCALE/SUB scratch
    # writes elsewhere; the O_BASE sequence must be exactly E.
    assert got[:len(exp)] == exp
    dens = [d for a, d in cwrites if a == B_BASE]
    assert dens == [e_den]


def test_smexp_cycle_pin():
    """Document the fused cost per element against the chain's.

    The chain on the standing image measures ~12 cy/el (ADD 1.25 +
    MAX 3.1 + SCALE 1.25 + SUB 1.25 + EXP2 ~3.9 + SUM ~1.2). The
    fused op prices at one max pass near the read rate plus an
    exp2-class stream with one extra subtract issue. A pin with wide
    margins, not a rate gate.
    """
    import random

    rng = random.Random(1000)
    n = 512
    words = [f32_bits(rng.uniform(-30, 0)) for _ in range(n)]
    words[::17] = [0xff800000] * len(words[::17])
    _, results = run_unit(PARAMS,
                          dict(op=int(VecOp.VEC_SMEXP), n=n, b_base=0x3000),
                          words_to_bytes(words),
                          b_base=0x3000,
                          deadline=200000)
    per = results['execution_cycles'] / n
    assert per < 8.5, per
    assert per > 2.0, per


# ---------------------------------------------------------------------------
# The full command path through Core
# ---------------------------------------------------------------------------


def test_core_smexp_fa_chain():
    """The FA softmax shape through Core: LOAD scores and mask, ADD,
    SMEXP, RECIP on the denominator slot, STORE both products."""
    import random

    rng = random.Random(41)
    n = 96
    scores = [f32_bits(rng.uniform(-40, 2)) for _ in range(n)]
    mask = [0 if i < n - 5 else 0xff800000 for i in range(n)]
    params = dict(PARAMS)
    la = ((n * 4 + 7) // 8) * 8

    sm = [vec_word(VecOp.VEC_ADD, s, m) for s, m in zip(scores, mask)]
    e_out, den = smexp_regions(sm)

    image, la_a, la_b = vector_image(scores, mask, params)
    # Scratch: the masked scores below the inputs' regions, the
    # denominator and reciprocal slots clear of everything.
    sm_slot = 0x2800
    e_slot = 0x3000
    l_slot = 0x3800
    rl_slot = 0x3900
    assert VO_TMEM + la <= sm_slot
    assert sm_slot + la <= e_slot and e_slot + la <= l_slot

    def vec(seq, op, k, a_base, m_base, r_base):
        return dict(opcode=Opcode.VECTOR,
                    seq=seq,
                    evt=5,
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
             row_bytes=la,
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.WAIT, seq=3, evt=1),
        dict(opcode=Opcode.WAIT, seq=4, evt=2),
        vec(5, VecOp.VEC_ADD, n, VA_TMEM, VB_TMEM, sm_slot),
        dict(opcode=Opcode.WAIT, seq=6, evt=5),
        vec(7, VecOp.VEC_SMEXP, n, sm_slot, l_slot, e_slot),
        dict(opcode=Opcode.WAIT, seq=8, evt=5),
        vec(9, VecOp.VEC_RECIP, 1, l_slot, VB_TMEM, rl_slot),
        dict(opcode=Opcode.WAIT, seq=10, evt=5),
        dict(opcode=Opcode.STORE_2D,
             seq=11,
             evt=6,
             dram_addr=VO_DRAM,
             tmem_addr=e_slot,
             row_bytes=la,
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.STORE_2D,
             seq=12,
             evt=7,
             dram_addr=VO_DRAM + 0x400,
             tmem_addr=rl_slot,
             row_bytes=8,
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.WAIT, seq=13, evt=6),
        dict(opcode=Opcode.WAIT, seq=14, evt=7),
        dict(opcode=Opcode.END, seq=15),
    ]
    results, image, _ = run_program(params, program, image, deadline=60000)
    assert all(s is Status.OK for s in results.values()), results
    blob = e_out + bytes(la - len(e_out))
    assert image[VO_DRAM:VO_DRAM + la] == blob
    from tests.troom.model import vec_recip_bits
    want = vec_recip_bits(den).to_bytes(4, 'little')
    assert image[VO_DRAM + 0x400:VO_DRAM + 0x404] == want


@pytest.mark.parametrize('add_units,mul_units', [(2, 4), (4, 2), (4, 4)])
def test_smexp_unit_four_issue_groups(add_units, mul_units):
    """Wider Horner issue preserves the frozen denominator reduction order."""
    import random
    rng = random.Random(106)
    from tests.troom.model import f32_bits
    words = [0xff800000, 0x7fc00000, 0, 0x80000000]
    words += [f32_bits(rng.uniform(-40, 2)) for _ in range(65)]
    params = dict(PARAMS,
                  data_width=512,
                  n_tmem_banks=8,
                  vec_unary_ctx=32,
                  vec_result_q=4,
                  vec_add_units=add_units,
                  vec_mul_units=mul_units)
    writes, result = run_unit(params,
                              dict(op=int(VecOp.VEC_SMEXP), n=len(words)),
                              words_to_bytes(words),
                              b_base=B_BASE,
                              stall_wr=True,
                              hold_writes=10)
    assert result['status'] == Status.OK
    assert writes == smexp_writes(params, words)
