"""Common matrix stimulus, packed-format goldens and arithmetic model checks.

Hardware scheduling and backpressure are exercised by the streaming suites.
"""

from amaranth import *

import pytest

from troom.consts import Format, Status

from tests.troom.model import (
    BLOCK_BYTES,
    SEGMENT_BOUND,
    build_act,
    f32,
    f32_bits,
    f32_to_fp16_bits,
    mm_row_results,
    mm_segments,
    pack_row,
    row_bytes,
    unpack_row,
)

PARAMS = dict(
    addr_width=32,
    data_width=64,
    id_width=4,
    seg_len=32,
    n_outputs=2,
    n_tokens=1,
    tmem_bytes=16 * 1024,
    n_tmem_banks=4,
    act_max_k=1024,
    mm_seg_fifo=8,
    mm_act_fifo=4,
    mm_out_depth=16,
)

BEAT = 8
W_BASE = 0x400


def params_for(fmt, mm_formats=None, **over):
    """Test params elaborating decoders for just the used formats."""
    p = dict(PARAMS)
    p['mm_formats'] = tuple(mm_formats) if mm_formats else (fmt, )
    p.update(over)
    return p


RECORD_FIELDS = (
    ['fmt', 'row_mask', 's_act', 'sum_a', 'seg_k', 'row_last'] +
    [f'{f}{p}' for p in range(2) for f in ('S', 'row', 'd', 'dmin', 'mi')])


def act_write(unit, seg, entry):
    wr = unit.act_wr
    word = 0
    for i, q in enumerate(entry['quants']):
        word |= (q & 0xFF) << (8 * i)
    yield wr.bits.seg.eq(seg)
    yield wr.bits.quants.eq(word)
    yield wr.bits.s_act.eq(entry['s_act'])
    yield wr.bits.sum_a.eq(entry['sum_a'] & 0xFFFF)
    yield wr.valid.eq(1)
    yield
    while not (yield wr.fire):
        yield
    yield wr.valid.eq(0)
    yield


def rand_fp16_bits(rng):
    return f32_to_fp16_bits(
        f32(2.0**rng.randint(-8, 2) * rng.uniform(1.0, 1.9)))


def rand_s_act(rng):
    return f32_bits(f32(2.0**rng.randint(-6, 1) * rng.uniform(1.0, 1.99)))


def gen_wsegs(rng, fmt, nsegs):
    """Random per-segment logical weight fields, ``nsegs`` total."""
    segs = []
    if fmt is Format.Q5_0:
        for _ in range(nsegs):
            segs.append(
                dict(w=[rng.randint(-16, 15) for _ in range(32)],
                     d=rand_fp16_bits(rng),
                     dmin=0,
                     mi=0,
                     sc0=0,
                     sc1=0))
    elif fmt is Format.Q8_0:
        for _ in range(nsegs):
            segs.append(
                dict(w=[rng.randint(-128, 127) for _ in range(32)],
                     d=rand_fp16_bits(rng),
                     dmin=0,
                     mi=0,
                     sc0=0,
                     sc1=0))
    elif fmt is Format.Q4_K:
        for _ in range(nsegs // 8):
            d = rand_fp16_bits(rng)
            dmin = rand_fp16_bits(rng)
            for _ in range(8):
                segs.append(
                    dict(w=[rng.randint(0, 15) for _ in range(32)],
                         d=d,
                         dmin=dmin,
                         mi=rng.randint(0, 63),
                         sc0=rng.randint(0, 63),
                         sc1=0))
    else:
        for _ in range(nsegs // 8):
            d = rand_fp16_bits(rng)
            for _ in range(8):
                segs.append(
                    dict(w=[rng.randint(-32, 31) for _ in range(32)],
                         d=d,
                         dmin=0,
                         mi=0,
                         sc0=rng.randint(-128, 127),
                         sc1=rng.randint(-128, 127)))
    return segs


def gen_act(rng, nsegs):
    return [
        dict(quants=[rng.randint(-128, 127) for _ in range(32)],
             s_act=rand_s_act(rng),
             sum_a=0) for _ in range(nsegs)
    ]


def check_records(got, exp, n_outputs=2):
    assert len(got) == len(exp), f'{len(got)} records, {len(exp)} expected'
    for i, (g, e) in enumerate(zip(got, exp)):
        for f in ('fmt', 'row_mask', 's_act', 'sum_a', 'seg_k', 'row_last'):
            assert g[f] == e[f], (i, f, g[f], e[f])
        for p in range(n_outputs):
            if (e['row_mask'] >> p) & 1:
                for f in ('S', 'row', 'd', 'dmin', 'mi'):
                    assert g[f'{f}{p}'] == e[f'{f}{p}'], \
                        (i, f'{f}{p}', g[f'{f}{p}'], e[f'{f}{p}'])
            else:
                # Masked lanes launch zero operands and zero scales.
                for f in ('S', 'd', 'dmin', 'mi'):
                    assert g[f'{f}{p}'] == 0, (i, f'{f}{p}')


# ---------------------------------------------------------------------------
# Model-side checks: pack/unpack agreement and golden layout sentinels.
# ---------------------------------------------------------------------------


@pytest.mark.parametrize('fmt,k', [
    (Format.Q5_0, 96),
    (Format.Q8_0, 64),
    (Format.Q4_K, 512),
    (Format.Q6_K, 256),
])
def test_model_pack_unpack_roundtrip(fmt, k):
    import random

    rng = random.Random(k)
    wsegs = gen_wsegs(rng, fmt, k // 32)
    data = pack_row(fmt, wsegs)
    assert len(data) == row_bytes(fmt, k)
    assert len(data) % BLOCK_BYTES[fmt] == 0
    back = unpack_row(fmt, data)
    assert len(back) == len(wsegs)
    for a, b in zip(wsegs, back):
        for f in ('w', 'd', 'dmin', 'mi', 'sc0', 'sc1'):
            assert a[f] == b[f], (f, a[f], b[f])


def test_model_golden_blocks():
    """Hand-encoded sentinel bytes anchoring each format's layout.

    Each element carries a distinct code so a nibble swap, bit-plane
    slip, or scale-index error shows up directly.
    """
    # Q5_0: element 0 = code 5, element 1 = code 17 (qh bit 1),
    # element 16 = code 16 (qh bit 16), element 17 = code 31.
    d = 0x3C00  # 1.0
    qs = bytearray(16)
    qs[0] = 5 | (0 << 4)  # e0 low, e16 high
    qs[1] = 1 | (15 << 4)  # e1 low, e17 high
    qh = (1 << 1) | (1 << 16) | (1 << 17)
    blk = (d.to_bytes(2, 'little') + qh.to_bytes(4, 'little') + bytes(qs))
    seg = unpack_row(Format.Q5_0, blk)[0]
    assert seg['w'][0] == 5 - 16
    assert seg['w'][1] == 17 - 16
    assert seg['w'][16] == 16 - 16
    assert seg['w'][17] == 31 - 16
    assert seg['d'] == d

    # Q8_0: signed bytes in order.
    qs8 = bytearray(32)
    qs8[0:4] = bytes([0x80, 0x7F, 0xFF, 0x01])
    blk = d.to_bytes(2, 'little') + bytes(qs8)
    seg = unpack_row(Format.Q8_0, blk)[0]
    assert seg['w'][:4] == [-128, 127, -1, 1]

    # Q4_K: segment 0 rides the low nibbles of the first 32 qs bytes,
    # segment 1 the high nibbles; scale pair j=0 is sc[0], mi[0].
    scales = _golden_k4_scales()
    qs = bytearray(128)
    for l in range(32):
        qs[l] = (l & 0xF) | (15 << 4)
    blk = ((d.to_bytes(2, 'little') + (0x4000).to_bytes(2, 'little') + scales +
            bytes(qs)))
    segs = unpack_row(Format.Q4_K, blk)
    assert segs[0]['w'] == [l & 0xF for l in range(32)]
    assert segs[1]['w'] == [15] * 32
    sc, mi = [63, 1, 2, 3, 4, 5, 6, 7], [0, 33, 34, 35, 36, 37, 38, 39]
    assert [s['sc0'] for s in segs] == sc
    assert [s['mi'] for s in segs] == mi
    assert segs[0]['dmin'] == 0x4000

    # Q6_K: segment 0 elements are ql[l] low / qh[l] bits 0-1;
    # segment 5 (h=1, q=1) elements are ql[96+l] low / qh[32+l] bits
    # 2-3; scales are 8h + 2q and 8h + 2q + 1.
    ql = bytearray(128)
    qh = bytearray(64)
    for l in range(32):
        ql[l] |= l & 0xF
        qh[l] |= (l >> 4) & 3
        ql[96 + l] |= 21 & 0xF
        qh[32 + l] |= ((21 >> 4) & 3) << 2
    sc8 = bytearray(16)
    sc8[0] = -3 & 0xFF
    sc8[1] = 12
    sc8[10] = -128 & 0xFF
    sc8[11] = 127
    blk = bytes(ql) + bytes(qh) + bytes(sc8) + d.to_bytes(2, 'little')
    segs = unpack_row(Format.Q6_K, blk)
    assert segs[0]['w'] == [(l & 0x3F) - 32 for l in range(32)]
    assert segs[0]['sc0'] == -3 and segs[0]['sc1'] == 12
    assert segs[5]['w'] == [21 - 32] * 32
    assert segs[5]['sc0'] == -128 and segs[5]['sc1'] == 127


def _golden_k4_scales():
    from tests.troom.model import _pack_k4_scales

    return _pack_k4_scales([63, 1, 2, 3, 4, 5, 6, 7],
                           [0, 33, 34, 35, 36, 37, 38, 39])


def test_segment_bounds_match_plan():
    """The integer exactness bounds from the plan, restated."""
    assert SEGMENT_BOUND[Format.Q8_0] == 524288
    assert SEGMENT_BOUND[Format.Q4_K] == 3870720
    assert SEGMENT_BOUND[Format.Q6_K] == 16777216
    assert SEGMENT_BOUND[Format.Q5_0] == 65536
    for bound in SEGMENT_BOUND.values():
        assert bound <= 2**24


# ---------------------------------------------------------------------------
# RTL versus model, per format and under stalls.
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Epilogue contract smoke (RTL epilogue lands in a later milestone).
# ---------------------------------------------------------------------------


def test_model_epilogue_ordering():
    """The ordered FP32 epilogue on hand-checkable values."""
    d1 = 0x3C00  # fp16 1.0
    d2 = 0x4000  # fp16 2.0
    one = f32_bits(1.0)

    recs = [
        dict(fmt=Format.Q8_0,
             row_mask=1,
             s_act=one,
             sum_a=0,
             seg_k=0,
             row_last=True,
             S0=2,
             row0=0,
             d0=d1)
    ]
    assert mm_row_results(recs) == {0: 2.0}

    # Q4_K minimum-term correction: u = 2, v = R(R(1*3 * 2.0) * 1) = 6.
    recs = [
        dict(fmt=Format.Q4_K,
             row_mask=1,
             s_act=one,
             sum_a=3,
             seg_k=0,
             row_last=True,
             S0=2,
             row0=0,
             d0=d1,
             dmin0=d2,
             mi0=1)
    ]
    assert mm_row_results(recs) == {0: f32(2.0 - 6.0)}

    # isum_4: k mod 4 placement then the paired final fold.
    recs = []
    for k in range(4):
        recs.append(
            dict(fmt=Format.Q8_0,
                 row_mask=1,
                 s_act=one,
                 sum_a=0,
                 seg_k=k,
                 row_last=(k == 3),
                 S0=2**k,
                 row0=0,
                 d0=d1))
    # acc = [1+... ]: k=0->p0=1, k=1->p1=2, k=2->p2=4, k=3->p3=8
    # result = R(R(1+4) + R(2+8)) = 15
    assert mm_row_results(recs) == {0: 15.0}


def test_matrix_extremes_hit_bounds():
    """Extremes of every operand range, checked against the bounds."""
    import random
    from tests.troom.test_stream import run_tiles, params_for as stream_params

    def stream_case(fmt, k, rows, wsegs, act, seed):
        nsegs = k // 32
        packed = [
            pack_row(fmt, wsegs[r * nsegs:(r + 1) * nsegs])
            for r in range(rows)
        ]
        tile = dict(fmt=fmt,
                    k=k,
                    rows=rows,
                    wsegs=wsegs,
                    act=act,
                    exp=mm_segments(fmt, k, rows, packed, act))
        out = run_tiles([tile], params=stream_params(fmt), seed=seed)
        out['status'] = out['results'][0]['status']
        return out

    rng = random.Random(15)
    # All -128 activations against maximal weights reach the exact
    # integer bounds from the plan.
    act = [
        dict(quants=[-128] * 32, s_act=rand_s_act(rng), sum_a=0)
        for _ in range(8)
    ]
    wsegs = []
    for _ in range(16):
        wsegs.append(
            dict(w=[15] * 32,
                 d=rand_fp16_bits(rng),
                 dmin=rand_fp16_bits(rng),
                 mi=63,
                 sc0=63,
                 sc1=0))
    out = stream_case(Format.Q4_K, 256, 2, wsegs, act, seed=15)
    assert out['status'] is Status.OK
    check_records(out['records'], out['expected'])
    for rec in out['records']:
        assert rec['S0'] == -SEGMENT_BOUND[Format.Q4_K]

    wsegs = [
        dict(w=[-32] * 32, d=0x3C00, dmin=0, mi=0, sc0=-128, sc1=-128)
        for _ in range(16)
    ]
    out = stream_case(Format.Q6_K, 256, 2, wsegs, act, seed=16)
    assert out['status'] is Status.OK
    check_records(out['records'], out['expected'])
    for rec in out['records']:
        assert rec['S0'] == -SEGMENT_BOUND[Format.Q6_K]
