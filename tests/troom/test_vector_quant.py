"""RTL tests for VEC_QUANT (device-side activation quantization).

Layers, following ``test_vector_unary.py``:

* the model's quantization contract against the host-side Q8_0
  quantizer's dequant accuracy (the scale pair is a two-ulp
  approximation, so the image differs from the host's but must
  reconstruct the input within the INT8 grid),
* the vector unit against ``tests/troom/model.py`` bit for bit:
  single and multiple segments, a zero-padded tail segment, the
  special-value pool, the frozen clamps (zero/subnormal amax, huge
  amax), write backpressure with a shallow result FIFO, the wide
  (512-bit) beat configuration, back-to-back commands sharing the
  arithmetic ports, and descriptor rejection,
* the full command path through ``Core``: VEC_QUANT stores its two
  regions to DRAM, and its image feeds the ``QUANTIZE`` copy and a
  real ``MATMUL`` whose FP32 results match the reference chain fed
  the device-produced activation image.
"""

import random

import pytest

from troom.consts import Format, Opcode, Status, VecOp
from troom.types import HasTRoomParams

from tests.troom.model import (
    INV127_BITS,
    bits_to_f32,
    f32,
    f32_bits,
    fp32_mul_bits,
    mm_row_results,
    mm_segments,
    pack_row,
    quantize_act_q8_0,
    row_bytes,
    vec_quant_regions,
    vec_quant_segments,
)
from tests.troom.test_full_path import (
    DRAM_SIZE,
    PARAMS,
    R_DRAM,
    W_BASE,
    W_DRAM,
    run_program,
)
from tests.troom.test_matrix import gen_wsegs
from tests.troom.test_vector import (
    A_BASE,
    O_BASE,
    SPECIALS,
    run_unit,
    words_to_bytes,
)

# The meta region is an output; park it beyond the O region. The
# binary-ops scratch sits above that for the shared-ports test.
M_BASE = 0x1800
BIN_BASE = 0x1800
BIG_M_BASE = 0x1a00

# Wide-region placement for the maximum-count case: A spans 4 KiB
# from A_BASE, so O moves above it.
BIG_O_BASE = 0x1200


def beats_of(blob, params):
    bb = HasTRoomParams(params).beat_bytes
    return [
        int.from_bytes(blob[off:off + bb], 'little')
        for off in range(0, len(blob), bb)
    ]


def region_writes(base, blob, params):
    return [(base + HasTRoomParams(params).beat_bytes * i, d)
            for i, d in enumerate(beats_of(blob, params))]


def expected_quant_writes(params, words, o_base, m_base):
    qblob, mblob = vec_quant_regions(words, HasTRoomParams(params).beat_bytes)
    return (region_writes(o_base, qblob, params) +
            region_writes(m_base, mblob, params))


def run_quant(params, words, *, o_base=O_BASE, m_base=M_BASE, **kw):
    return run_unit(
        params,
        dict(op=int(VecOp.VEC_QUANT),
             n=len(words),
             o_base=o_base,
             b_base=m_base), words_to_bytes(words), **kw)


def check_quant(params, words, res, writes, *, o_base=O_BASE, m_base=M_BASE):
    assert res['status'] is Status.OK
    assert res['elements'] == len(words)
    assert writes == expected_quant_writes(params, words, o_base, m_base)


# ---------------------------------------------------------------------------
# Model contract sanity
# ---------------------------------------------------------------------------


def dequant_err(seg, i, words):
    d = bits_to_f32(seg['s_act'])
    q = seg['quants'][i]
    q = q - 256 if q > 127 else q
    return abs(q * d - bits_to_f32(words[i]))


def test_model_matches_host_within_grid():
    """The device image reconstructs the input as well as the host
    Q8_0-style quantizer does: both are bounded by the same half-
    quantum grid term, the device adds only the scale pair's ~2 ulp
    relative error."""
    rng = random.Random(3)
    for _ in range(50):
        words = [f32_bits(f32(rng.uniform(-20, 20))) for _ in range(64)]
        seg = vec_quant_segments(words)
        host = [
            quantize_act_q8_0(
                [bits_to_f32(w) for w in words[32 * j:32 * j + 32]])
            for j in range(2)
        ]
        for j in range(2):
            seg_max = max(
                abs(bits_to_f32(w)) for w in words[32 * j:32 * j + 32])
            # Half a quantum of rounding, plus one more quantum when
            # the two-ulp multiplier flips a boundary case, plus the
            # scale pair's relative error.
            bound = 1.5 * seg_max / 127.0 + seg_max * 2**-20
            for i in range(32):
                assert dequant_err(seg[j], i,
                                   words[32 * j:32 * j + 32]) <= bound, \
                    (j, i)
            for i, h in enumerate(host[j]['quants']):
                assert abs(h * bits_to_f32(host[j]['s_act']) -
                           bits_to_f32(words[32 * j + i])) <= bound


def test_model_clamps():
    from tests.troom.model import q8_rne_bits

    # Zero and subnormal amax: the frozen zero segment.
    for words in ([0] * 32, [0x00000001] * 32,
                  [0x007fffff, 0x80000001] + [0] * 30):
        seg = vec_quant_segments(words)[0]
        assert seg['s_act'] == 0
        assert seg['sum_a'] == 0
        assert all(q == 0 for q in seg['quants'])
    # amax >= 2^126: inv is forced to +0, quants all zero, sc keeps
    # the correctly rounded product.
    words = [0x7f000000, 0xbf000000] + [f32_bits(1.0)] * 30
    seg = vec_quant_segments(words)[0]
    assert seg['s_act'] == fp32_mul_bits(0x7f000000, INV127_BITS)
    assert all(q == 0 for q in seg['quants'])
    assert seg['sum_a'] == 0
    # NaN never wins the fold; an all-NaN segment is the zero clamp.
    seg = vec_quant_segments([0x7fc00000] * 32)[0]
    assert seg['s_act'] == 0
    # q8 RNE reference: halves to even, saturation, tiny to zero.
    assert q8_rne_bits(f32_bits(0.4)) == 0
    assert q8_rne_bits(f32_bits(0.5)) == 0
    assert q8_rne_bits(f32_bits(1.5)) == 2
    assert q8_rne_bits(f32_bits(2.5)) == 2
    assert q8_rne_bits(f32_bits(127.4)) == 127
    assert q8_rne_bits(f32_bits(127.5)) == 127
    assert q8_rne_bits(f32_bits(128.0)) == 0x7f
    assert q8_rne_bits(f32_bits(-128.0)) == 0x80
    assert q8_rne_bits(0x80000000) == 0
    assert q8_rne_bits(0x7f800000) == 0x7f
    assert q8_rne_bits(0xff800000) == 0x80


# ---------------------------------------------------------------------------
# Vector unit vs the model, bit for bit
# ---------------------------------------------------------------------------


@pytest.mark.parametrize('n', [1, 7, 32, 33, 64, 100, 256])
def test_unit_quant_matches_model(n):
    rng = random.Random(100 + n)
    params = dict(PARAMS)
    words = [f32_bits(f32(rng.uniform(-5, 5))) for _ in range(n)]
    writes, res = run_quant(params, words)
    check_quant(params, words, res, writes)


def test_unit_quant_max_k():
    rng = random.Random(7)
    params = dict(PARAMS)
    n = params['act_max_k']
    words = [f32_bits(f32(rng.uniform(-3, 3))) for _ in range(n)]
    writes, res = run_quant(params,
                            words,
                            o_base=BIG_O_BASE,
                            m_base=BIG_M_BASE)
    check_quant(params,
                words,
                res,
                writes,
                o_base=BIG_O_BASE,
                m_base=BIG_M_BASE)


def test_unit_quant_specials():
    params = dict(PARAMS)
    words = (SPECIALS * ((2 * len(SPECIALS) + 31) // 32))[:96]
    writes, res = run_quant(params, words)
    check_quant(params, words, res, writes)


def test_unit_quant_clamps():
    params = dict(PARAMS)
    cases = [
        [0] * 64 + SPECIALS[:8],  # zero tail
        [0x00000001] * 32 + [f32_bits(1.0)] * 32,  # subnormal seg
        [0x7f000000, 0xbf000000] + [f32_bits(1.0)] * 62,
        [0x7fc00000] * 32 + [0x7f800000, 0xff800000] + [0] * 30,
        # The smallest normal amax: 127*recip overflows, so the pad
        # bytes must still be 0 and the element saturates 127.
        [0x00800000],
        [0x00800000, 0x80400000] + [0] * 30,
        # All-tiny segments quantize on the unscaled recip path.
        [0x80400000 if i % 3 == 0 else 0x00800000 for i in range(32)],
    ]
    for words in cases:
        writes, res = run_quant(params, words)
        check_quant(params, words, res, writes)
    # The single-tiny-word tail keeps its pads zero.
    seg = vec_quant_segments([0x00800000])[0]
    assert seg['quants'][0] == 127
    assert all(q == 0 for q in seg['quants'][1:])
    assert seg['sum_a'] == 127


def test_unit_quant_write_backpressure():
    """Random write readiness with the shallowest result FIFO must
    not lose or duplicate a byte; the hold outlasts the FIFO and the
    shared multiplier pipeline."""
    rng = random.Random(9)
    params = dict(PARAMS, vec_result_q=2)
    n = 100
    words = [f32_bits(f32(rng.uniform(-100, 100))) for _ in range(n)]
    writes, res = run_quant(params, words, stall_wr=True, hold_writes=60)
    check_quant(params, words, res, writes)


def test_unit_quant_drain_backpressure():
    """A hold landing on the final quant beat must not let DRAIN
    leave on the index alone: the last meta word still enqueues
    (its sum is 32 * 127 = 4064, not 0)."""
    params = dict(PARAMS, vec_result_q=3)
    words = [f32_bits(1.0)] * 64
    # Eight 64-bit quant beats: accept seven, then hold the final
    # beat while the three-entry FIFO fills with metadata.
    writes, res = run_quant(params,
                            words,
                            hold_after_beats=7,
                            hold_writes=150,
                            deadline=2000)
    assert res['blocked'] >= 150
    check_quant(params, words, res, writes)


def test_unit_quant_back_to_back():
    """Two and three consecutive quant commands: the sequencer must
    park after draining instead of re-reading the exhausted window
    while the packer still holds the command."""
    rng = random.Random(15)
    params = dict(PARAMS)
    chunks = ([f32_bits(1.0)] * 32, [f32_bits(-1.0)] * 32,
              [f32_bits(f32(rng.uniform(-3, 3))) for _ in range(64)])
    words = [w for c in chunks for w in c]
    descs, exp = [], []
    for i, chunk in enumerate(chunks):
        descs.append(
            dict(op=int(VecOp.VEC_QUANT),
                 n=len(chunk),
                 a_base=A_BASE + 128 * i,
                 o_base=O_BASE + 0x100 * i,
                 b_base=M_BASE))
        qblob, mblob = vec_quant_regions(chunk,
                                         HasTRoomParams(params).beat_bytes)
        exp += region_writes(O_BASE + 0x100 * i, qblob, params)
        exp += region_writes(M_BASE, mblob, params)
    writes, results = run_unit(params, descs, words_to_bytes(words))
    assert all(r['status'] is Status.OK for r in results), \
        [r['status'] for r in results]
    assert writes == exp


def test_unit_quant_shared_ports():
    """A quant command between two binary commands shares the
    arithmetic ports without interference."""
    rng = random.Random(11)
    params = dict(PARAMS)
    from tests.troom.model import vec_word

    n = 64
    a = [f32_bits(f32(rng.uniform(-2, 2))) for _ in range(n)]
    b = [f32_bits(f32(rng.uniform(-2, 2))) for _ in range(n)]
    descs = [
        dict(op=int(VecOp.VEC_ADD), n=n, o_base=BIN_BASE),
        dict(op=int(VecOp.VEC_QUANT), n=n, o_base=O_BASE, b_base=BIG_M_BASE),
        dict(op=int(VecOp.VEC_MUL), n=n, o_base=BIN_BASE),
    ]
    writes, results = run_unit(params, descs, words_to_bytes(a),
                               words_to_bytes(b))
    assert all(r['status'] is Status.OK for r in results)

    # The quant command reads the same A region the binary ops do.
    add_words = [vec_word(VecOp.VEC_ADD, x, y) for x, y in zip(a, b)]
    mul_words = [vec_word(VecOp.VEC_MUL, x, y) for x, y in zip(a, b)]
    qblob, mblob = vec_quant_regions(a, HasTRoomParams(params).beat_bytes)
    assert writes == (
        region_writes(BIN_BASE, words_to_bytes(add_words), params) +
        region_writes(O_BASE, qblob, params) +
        region_writes(BIG_M_BASE, mblob, params) +
        region_writes(BIN_BASE, words_to_bytes(mul_words), params))


def test_unit_quant_wide_beat():
    rng = random.Random(13)
    params = dict(PARAMS,
                  data_width=512,
                  n_tmem_banks=8,
                  act_max_k=2048,
                  cmd_queue_depth=8)
    words = [f32_bits(f32(rng.uniform(-8, 8))) for _ in range(100)]
    writes, res = run_quant(params, words)
    check_quant(params, words, res, writes)


@pytest.mark.parametrize('desc, status, why', [
    (dict(op=int(VecOp.VEC_QUANT), n=0), Status.BAD_SHAPE, 'zero count'),
    (dict(op=int(VecOp.VEC_QUANT), n=8,
          o_base=A_BASE), Status.BAD_SHAPE, 'output inside A'),
    (dict(op=int(VecOp.VEC_QUANT), n=8,
          b_base=A_BASE), Status.BAD_SHAPE, 'meta inside A'),
    (dict(op=int(VecOp.VEC_QUANT), n=8,
          b_base=O_BASE), Status.BAD_SHAPE, 'meta inside output'),
    (dict(op=int(VecOp.VEC_QUANT), n=8,
          b_base=M_BASE + 4), Status.BAD_ALIGN, 'unaligned meta base'),
    (dict(op=int(VecOp.VEC_QUANT), n=64,
          b_base=0x3ff8), Status.BAD_RANGE, 'meta past TMEM'),
    (dict(op=11, n=8), Status.BAD_SHAPE, 'reserved sub-op encoding'),
])
def test_unit_quant_rejected(desc, status, why):
    params = dict(PARAMS)
    d = dict(desc)
    d.setdefault('o_base', O_BASE)
    d.setdefault('b_base', M_BASE)
    writes, res = run_unit(params, d, words_to_bytes([f32_bits(1.0)] * 8))
    assert res['status'] is status, why
    assert writes == []


# ---------------------------------------------------------------------------
# Full command path through Core
# ---------------------------------------------------------------------------

VA_TMEM = 0x2000  # FP32 source
VQ_TMEM = 0x2200  # quants image (VEC_QUANT r_base / QUANTIZE a_base)
VM_TMEM = 0x2300  # meta image (VEC_QUANT m_base / QUANTIZE m_base)
RT_BASE = 0x3000  # matmul results
VFP_DRAM = 0x4800  # FP32 source image in DRAM
VQ_DRAM = 0x5000  # stored quants image
VM_DRAM = 0x5800  # stored meta image


def test_core_quant_image_stores():
    """VEC_QUANT's two regions round-trip to DRAM byte for byte."""
    rng = random.Random(17)
    k = 96
    params = dict(PARAMS)
    bb = HasTRoomParams(params).beat_bytes
    words = [f32_bits(f32(rng.uniform(-3, 3))) for _ in range(k)]
    qblob, mblob = vec_quant_regions(words, bb)
    image = bytearray(DRAM_SIZE)
    image[VFP_DRAM:VFP_DRAM + 4 * k] = words_to_bytes(words)
    program = [
        dict(opcode=Opcode.LOAD_2D,
             seq=1,
             evt=1,
             dram_addr=VFP_DRAM,
             tmem_addr=VA_TMEM,
             row_bytes=-(-4 * k // bb) * bb,
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.WAIT, seq=2, evt=1),
        dict(opcode=Opcode.VECTOR,
             seq=3,
             evt=6,
             vec_op=int(VecOp.VEC_QUANT),
             k=k,
             a_base=VA_TMEM,
             r_base=VQ_TMEM,
             m_base=VM_TMEM),
        dict(opcode=Opcode.WAIT, seq=4, evt=6),
        dict(opcode=Opcode.STORE_2D,
             seq=5,
             evt=7,
             dram_addr=VQ_DRAM,
             tmem_addr=VQ_TMEM,
             row_bytes=len(qblob),
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.WAIT, seq=6, evt=7),
        dict(opcode=Opcode.STORE_2D,
             seq=7,
             evt=7,
             dram_addr=VM_DRAM,
             tmem_addr=VM_TMEM,
             row_bytes=len(mblob),
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.WAIT, seq=8, evt=7),
        dict(opcode=Opcode.END, seq=9),
    ]
    results, image, _ = run_program(params, program, image)
    assert all(s is Status.OK for s in results.values()), results
    assert bytes(image[VQ_DRAM:VQ_DRAM + len(qblob)]) == qblob
    assert bytes(image[VM_DRAM:VM_DRAM + len(mblob)]) == mblob


@pytest.mark.parametrize('k,rows', [(64, 4), (96, 3)])
def test_core_quant_feeds_matmul(k, rows):
    """The device-produced image drives the QUANTIZE copy and a real
    MATMUL; the FP32 results match the reference chain fed the same
    per-segment image."""
    rng = random.Random(19 + k)
    fmt = Format.Q8_0
    params = dict(PARAMS)
    bb = HasTRoomParams(params).beat_bytes
    words = [f32_bits(f32(rng.uniform(-1, 1))) for _ in range(k)]
    act = vec_quant_segments(words)

    wsegs = gen_wsegs(rng, fmt, rows * (k // 32))
    stride = -(-row_bytes(fmt, k) // bb) * bb
    rstride = -(-(rows * 4) // bb) * bb

    image = bytearray(DRAM_SIZE)
    for r in range(rows):
        row = pack_row(fmt, wsegs[r * (k // 32):(r + 1) * (k // 32)])
        row += bytes((-len(row)) % bb)
        image[W_DRAM + r * stride:W_DRAM + r * stride + len(row)] = row
    image[VFP_DRAM:VFP_DRAM + 4 * k] = words_to_bytes(words)

    program = [
        dict(opcode=Opcode.LOAD_2D,
             seq=1,
             evt=1,
             dram_addr=W_DRAM,
             tmem_addr=W_BASE,
             row_bytes=stride,
             rows=rows,
             dram_stride=stride,
             tmem_stride=stride),
        dict(opcode=Opcode.LOAD_2D,
             seq=2,
             evt=2,
             dram_addr=VFP_DRAM,
             tmem_addr=VA_TMEM,
             row_bytes=-(-4 * k // bb) * bb,
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.WAIT, seq=3, evt=1),
        dict(opcode=Opcode.WAIT, seq=4, evt=2),
        dict(opcode=Opcode.VECTOR,
             seq=5,
             evt=6,
             vec_op=int(VecOp.VEC_QUANT),
             k=k,
             a_base=VA_TMEM,
             r_base=VQ_TMEM,
             m_base=VM_TMEM),
        dict(opcode=Opcode.WAIT, seq=6, evt=6),
        dict(opcode=Opcode.QUANTIZE,
             seq=7,
             evt=4,
             a_base=VQ_TMEM,
             m_base=VM_TMEM,
             k=k),
        dict(opcode=Opcode.WAIT, seq=8, evt=4),
        dict(opcode=Opcode.MATMUL,
             seq=9,
             evt=5,
             fmt=int(fmt),
             k=k,
             rows=rows,
             w_base=W_BASE,
             w_stride=stride,
             r_base=RT_BASE),
        dict(opcode=Opcode.WAIT, seq=10, evt=5),
        dict(opcode=Opcode.STORE_2D,
             seq=11,
             evt=7,
             dram_addr=R_DRAM,
             tmem_addr=RT_BASE,
             row_bytes=rstride,
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.WAIT, seq=12, evt=7),
        dict(opcode=Opcode.END, seq=13),
    ]
    results, image, counters = run_program(params,
                                           program,
                                           image,
                                           deadline=40000)
    assert all(s is Status.OK for s in results.values()), results

    records = mm_segments(fmt, k, rows, [
        pack_row(fmt, wsegs[r * (k // 32):(r + 1) * (k // 32)])
        for r in range(rows)
    ], act)
    exp = mm_row_results(records)
    for r in range(rows):
        off = R_DRAM + 4 * r
        got = int.from_bytes(image[off:off + 4], 'little')
        assert got == f32_bits(exp[r]), (r, hex(got), exp[r])
    assert counters['useful_macs'] == rows * k
