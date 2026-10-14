"""Focused pysim tests for the troom FP32 epilogue.

The reference side is ``tests/troom/model.py``'s ordered epilogue
(``mm_row_results``): every rounding point, the ``isum_4`` placement,
and the paired final fold. These tests drive ``MmSegment`` records
straight into the epilogue and compare the packed TMEM beats bit for
bit, per format, under write backpressure, with hand-built
rounding-sensitive and cancellation cases (where the ordered contract
provably differs from a naive sequential sum), short-K fold stress,
and final-beat zero padding.
"""

from amaranth.sim import Passive, Simulator

import pytest

from troom.consts import Format
from troom.epilogue import Epilogue
from troom.types import HasTRoomParams

from tests.troom.model import (
    build_act,
    f32,
    f32_bits,
    mm_row_results,
    mm_segments,
    pack_row,
)
from tests.troom.test_matrix import gen_act, gen_wsegs

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

R_BASE = 0x200


def params_for(**over):
    p = dict(PARAMS)
    p.update(over)
    return p


def push_record(dut, rec, n_outputs=2):
    sink = dut.sink
    yield sink.bits.fmt.eq(int(rec['fmt']))
    yield sink.bits.row_mask.eq(rec['row_mask'])
    yield sink.bits.s_act.eq(rec['s_act'])
    yield sink.bits.sum_a.eq(rec['sum_a'] & 0xFFFF)
    yield sink.bits.seg_k.eq(rec['seg_k'])
    yield sink.bits.row_last.eq(int(rec['row_last']))
    for p in range(n_outputs):
        yield getattr(sink.bits, f'S{p}').eq(rec.get(f'S{p}', 0) & 0xFFFFFFFF)
        yield getattr(sink.bits, f'row{p}').eq(rec.get(f'row{p}', 0))
        yield getattr(sink.bits, f'd{p}').eq(rec.get(f'd{p}', 0))
        yield getattr(sink.bits, f'dmin{p}').eq(rec.get(f'dmin{p}', 0))
        yield getattr(sink.bits, f'mi{p}').eq(rec.get(f'mi{p}', 0))
    yield sink.valid.eq(1)
    yield
    while not (yield sink.fire):
        yield
    yield sink.valid.eq(0)
    yield


def run_epilogue(params,
                 records,
                 rows,
                 *,
                 r_base=R_BASE,
                 stall_wr=False,
                 seed=3,
                 deadline=10000,
                 hold_writes=0):
    """Drive records through the epilogue; return collected beats."""
    import random

    rng = random.Random(seed)
    dut = Epilogue(params)
    writes = []
    result = {}

    def tx():
        for _ in range(3):
            yield
        yield dut.rows.eq(rows)
        yield dut.r_base.eq(r_base)
        yield dut.start.eq(1)
        yield
        yield dut.start.eq(0)
        yield
        for rec in records:
            yield from push_record(dut, rec, n_outputs=params['n_outputs'])
            if rng.random() < 0.3:
                for _ in range(rng.randint(0, 3)):
                    yield
        for _ in range(deadline):
            yield
            if (yield dut.done):
                yield
                result['done'] = True
                result['records'] = (yield dut.records)
                result['folds'] = (yield dut.folds)
                result['write_beats'] = (yield dut.write_beats)
                break
        else:
            raise TimeoutError('epilogue did not finish')

    def wr():
        yield Passive()
        for _ in range(hold_writes):
            yield dut.tmem_w.ready.eq(0)
            yield
        while True:
            yield dut.tmem_w.ready.eq(rng.random() < 0.6 if stall_wr else 1)
            if (yield dut.tmem_w.fire):
                writes.append(((yield
                                dut.tmem_w.bits.addr), (yield
                                                        dut.tmem_w.bits.data)))
            yield

    def watchdog():
        yield Passive()
        for _ in range(deadline):
            yield
        raise TimeoutError('epilogue stalled during input or result drain')

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(tx)
    sim.add_sync_process(wr)
    sim.add_sync_process(watchdog)
    sim.run()

    assert result['done']
    return writes, result


def unpack(writes, params, rows, r_base=R_BASE):
    hp = HasTRoomParams(params)
    bw = hp.beat_bytes // 4
    beats = dict(writes)
    words = {}
    for r in range(rows):
        addr = r_base + (r // bw) * hp.beat_bytes
        assert addr in beats, f'missing beat {addr:#x}'
        words[r] = (beats[addr] >> (32 * (r % bw))) & 0xFFFFFFFF
    total = (rows + bw - 1) // bw
    for b in range(total):
        addr = r_base + b * hp.beat_bytes
        assert addr in beats, f'missing beat {addr:#x}'
    # The final beat's unused lanes must be zero.
    for w in range(rows, total * bw):
        addr = r_base + (w // bw) * hp.beat_bytes
        pad = (beats[addr] >> (32 * (w % bw))) & 0xFFFFFFFF
        assert pad == 0, f'lane {w} not zeroed: {pad:#x}'
    addrs = sorted(a for a, _ in writes)
    assert addrs == sorted(set(addrs)), 'duplicate result beats'
    return words


def check(writes, params, records, rows, **kw):
    words = unpack(writes, params, rows, **kw)
    exp = mm_row_results(records)
    assert sorted(exp) == list(range(rows))
    for r in range(rows):
        assert words[r] == f32_bits(exp[r]), \
            (r, hex(words[r]), hex(f32_bits(exp[r])), exp[r])
    return words


# ---------------------------------------------------------------------------
# Model-driven cases, per format.
# ---------------------------------------------------------------------------


def format_case(fmt, k, rows, seed, **kw):
    import random

    rng = random.Random(seed)
    wsegs = gen_wsegs(rng, fmt, rows * (k // 32))
    act = gen_act(rng, k // 32)
    act = [dict(e, sum_a=sum(e['quants'])) for e in act]
    records = mm_segments(fmt, k, rows, [
        pack_row(fmt, wsegs[r * (k // 32):(r + 1) * (k // 32)])
        for r in range(rows)
    ], act)
    writes, res = run_epilogue(params_for(), records, rows, seed=seed, **kw)
    assert res['records'] == len(records)
    assert res['folds'] == rows
    hp = HasTRoomParams(PARAMS)
    assert res['write_beats'] == (rows + 1) // 2
    check(writes, params_for(), records, rows)


def test_epilogue_q5_0_matches_model():
    format_case(Format.Q5_0, 96, 3, 11)


def test_epilogue_q8_0_matches_model():
    format_case(Format.Q8_0, 64, 2, 12)


def test_epilogue_q4_k_matches_model():
    # The minimum-term correction path runs end to end.
    format_case(Format.Q4_K, 512, 5, 13)


def test_epilogue_q6_k_matches_model():
    format_case(Format.Q6_K, 256, 3, 14)


def test_epilogue_write_backpressure():
    format_case(Format.Q6_K, 512, 5, 21, stall_wr=True)


@pytest.mark.parametrize('fmt,k', [(Format.Q8_0, 32), (Format.Q4_K, 256)])
@pytest.mark.parametrize('n_outputs', [1, 2, 4])
def test_epilogue_long_write_stall(fmt, k, n_outputs):
    """Results and fold contexts survive a stall beyond all queue capacity."""
    import random

    rows = 33
    rng = random.Random(22)
    nsegs = k // 32
    weights = gen_wsegs(rng, fmt, rows * nsegs)
    act = gen_act(rng, nsegs)
    records = mm_segments(fmt,
                          k,
                          rows, [
                              pack_row(fmt, weights[r * nsegs:(r + 1) * nsegs])
                              for r in range(rows)
                          ],
                          act,
                          n_outputs=n_outputs)
    params = params_for(mm_result_q=1, n_outputs=n_outputs)
    writes, _ = run_epilogue(params, records, rows, hold_writes=1500)
    check(writes, params, records, rows)


def test_epilogue_short_k_fold_stress():
    # One segment per row: every record is a row fold; the snapshot
    # must always retire ahead of the next one.
    format_case(Format.Q8_0, 32, 17, 31)


def test_epilogue_q8_0_real_k():
    # A real projection width with host-style activation blocks.
    import random

    fmt, k, rows = Format.Q8_0, 576, 3
    rng = random.Random(41)
    wsegs = gen_wsegs(rng, fmt, rows * (k // 32))
    vec = [rng.uniform(-1, 1) for _ in range(k)]
    act = build_act(fmt, vec)
    records = mm_segments(fmt, k, rows, [
        pack_row(fmt, wsegs[r * (k // 32):(r + 1) * (k // 32)])
        for r in range(rows)
    ], act)
    writes, _ = run_epilogue(params_for(), records, rows, seed=41)
    check(writes, params_for(), records, rows)


# ---------------------------------------------------------------------------
# Hand-built records: rounding-sensitive ordering and cancellation.
# ---------------------------------------------------------------------------

ONE16 = 0x3C00  # fp16 1.0
TWO16 = 0x4000  # fp16 2.0
F32_ONE = f32_bits(1.0)
F32_2M24 = f32_bits(2.0**-24)


def srec(S,
         seg_k,
         row_last,
         fmt=Format.Q8_0,
         d=ONE16,
         s_act=F32_ONE,
         sum_a=0,
         dmin=0,
         mi=0):
    return dict(fmt=fmt,
                row_mask=1,
                s_act=s_act,
                sum_a=sum_a,
                seg_k=seg_k,
                row_last=row_last,
                S0=S,
                row0=0,
                d0=d,
                dmin0=dmin,
                mi0=mi)


def test_epilogue_isum4_order_is_bit_defining():
    """A case where the ordered contract differs from a naive sum.

    With s_act = 2^-24, integer partials become u = S * 2^-24. The
    four segments carry +1.0, +2^-24, +2^-24, -1.0: the paired fold
    keeps the 2^-24 contributions alive, a left-to-right sum cancels
    them to zero.
    """
    recs = [
        srec(1 << 24, 0, False, s_act=F32_2M24),
        srec(1, 1, False, s_act=F32_2M24),
        srec(1, 2, False, s_act=F32_2M24),
        srec(-(1 << 24), 3, True, s_act=F32_2M24),
    ]
    exp = mm_row_results(recs)[0]
    naive = f32(f32(f32(f32(1.0 + 2.0**-24) + 2.0**-24) - 1.0))
    assert exp != naive and exp == 2.0**-24

    writes, _ = run_epilogue(params_for(), recs, 1)
    words = check(writes, params_for(), recs, 1)
    assert words[0] == F32_2M24


def test_epilogue_rounding_tie_sweep():
    """Randomized tie-heavy values; the RTL must round twice per
    product chain exactly as the model does, never fusing."""
    import random

    rng = random.Random(99)
    d16s = [0x3C00, 0x3E00, 0x3C01, 0x3801, 0x4380]
    s_acts = [
        f32_bits(x) for x in (1.0, 1.5, 2.0, 4096.0, 2.0**-12, 3.0, 2.0**20)
    ]
    recs = []
    for j in range(24):
        recs.append(
            srec(rng.randint(-(1 << 24), 1 << 24),
                 j,
                 j == 23,
                 d=rng.choice(d16s),
                 s_act=rng.choice(s_acts)))
    writes, _ = run_epilogue(params_for(), recs, 1, seed=99)
    check(writes, params_for(), recs, 1)


def test_epilogue_q4_k_cancellation():
    """u - v exactly zero and near-zero corrections."""
    recs = [
        # u = 100 * 1 * 1, v = 1 * 100 * 1 -> t = 0
        srec(100,
             0,
             False,
             fmt=Format.Q4_K,
             d=ONE16,
             sum_a=100,
             dmin=ONE16,
             mi=1),
        # u = 100, v = 1 * 99 * 1 -> t = 1
        srec(100,
             1,
             False,
             fmt=Format.Q4_K,
             d=ONE16,
             sum_a=99,
             dmin=ONE16,
             mi=1),
        # u = -50 via a negative-scale dmin path: v = 1 * 25 * 2 = 50
        srec(0, 2, False, fmt=Format.Q4_K, d=ONE16, sum_a=25, dmin=TWO16,
             mi=1),
        srec(7, 3, True, fmt=Format.Q4_K, d=ONE16, sum_a=0, dmin=ONE16, mi=63),
    ]
    writes, _ = run_epilogue(params_for(), recs, 1)
    check(writes, params_for(), recs, 1)
    words = unpack(writes, params_for(), 1)
    # t = [0, 1, -50, 7]: p0 = 0, p1 = 1, p2 = -50, p3 = 7;
    # fold = R(R(0 + -50) + R(1 + 7)) = -42.
    assert words[0] == f32_bits(-42.0)


def test_epilogue_zero_rows_of_quants():
    """All-zero segments produce +0 results with no NaN seeding."""
    recs = [srec(0, j, j == 3) for j in range(4)]
    writes, _ = run_epilogue(params_for(), recs, 1)
    words = check(writes, params_for(), recs, 1)
    assert words[0] == 0


def test_epilogue_lanes_disagree():
    """Both lanes fold different rows from shared records."""
    recs = []
    for j in range(2):
        recs.append(
            dict(fmt=Format.Q8_0,
                 row_mask=3,
                 s_act=F32_ONE,
                 sum_a=0,
                 seg_k=j,
                 row_last=j == 1,
                 S0=2 + j,
                 row0=0,
                 d0=ONE16,
                 S1=-(4 + j),
                 row1=1,
                 d1=ONE16))
    writes, _ = run_epilogue(params_for(), recs, 2)
    words = check(writes, params_for(), recs, 2)
    assert words[0] == f32_bits(5.0)  # 2 + 3
    assert words[1] == f32_bits(-9.0)  # -(4 + 5)


def test_epilogue_result_region_offset():
    """A non-zero r_base lands beats at the requested addresses."""
    recs = [srec(5, 0, True)]
    base = 0x600
    writes, _ = run_epilogue(params_for(), recs, 1, r_base=base)
    words = check(writes, params_for(), recs, 1, r_base=base)
    assert words[0] == f32_bits(5.0)
