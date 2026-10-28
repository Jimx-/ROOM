"""Focused pysim tests for the per-lane streaming epilogue (D2).

The correctness contract is identical to ``tests/troom/test_epilogue.py``
-- every case here is that file's case re-run against
``Epilogue``, which must stay bit-exact against ``mm_row_results``
per format, under write backpressure, across ``n_outputs`` 1/2/4, with
one-segment rows and hand-built rounding/cancellation records.  The
added ``test_lane_*_rate`` cases measure what the de-share exists for:
with an always-valid sink, acceptance must sustain one record per cycle
for every canonical format (the shared bank's deterministic floor was
~2.3 cycles/record mix, 4.0 on Q4_K).
"""

import random

import pytest

from amaranth.sim import Passive, Simulator

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
from tests.troom.test_epilogue import (
    PARAMS,
    R_BASE,
    WIDE,
    check,
    params_for,
    stream_records,
    srec,
    unpack,
    ONE16,
    TWO16,
    F32_2M24,
)


def run_lane(params,
             records,
             rows,
             *,
             r_base=R_BASE,
             stall_wr=False,
             seed=3,
             deadline=40000,
             hold_writes=0,
             stream=False):
    """Drive records through ``Epilogue``; return beats, results
    and the sink fire-cycle list (acceptance-rate evidence)."""
    rng = random.Random(seed)
    dut = Epilogue(params)
    writes = []
    fires = []
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
        if stream:
            yield from stream_records(dut,
                                      records,
                                      n_outputs=params['n_outputs'])
        else:
            from tests.troom.test_epilogue import push_record
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
                for name in ('records', 'folds', 'write_beats',
                             'active_cycles', 'stall_cycles', 'cast_ops',
                             'mul_ops', 'add_ops'):
                    result[name] = (yield getattr(dut, name))
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

    def mon():
        yield Passive()
        cyc = 0
        while True:
            if (yield dut.sink.valid) and (yield dut.sink.ready):
                fires.append(cyc)
            yield
            cyc += 1

    def watchdog():
        yield Passive()
        for _ in range(deadline):
            yield
        raise TimeoutError('epilogue stalled during input or drain')

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(tx)
    sim.add_sync_process(wr)
    sim.add_sync_process(mon)
    sim.add_sync_process(watchdog)
    sim.run()

    assert result['done']
    return writes, result, fires


def lane_case(fmt, k, rows, seed, **kw):
    rng = random.Random(seed)
    wsegs = gen_wsegs(rng, fmt, rows * (k // 32))
    act = gen_act(rng, k // 32)
    act = [dict(e, sum_a=sum(e['quants'])) for e in act]
    records = mm_segments(fmt, k, rows, [
        pack_row(fmt, wsegs[r * (k // 32):(r + 1) * (k // 32)])
        for r in range(rows)
    ], act)
    params = kw.pop('params', params_for())
    writes, res, _ = run_lane(params, records, rows, seed=seed, **kw)
    assert res['records'] == len(records)
    assert res['folds'] == rows
    hp = HasTRoomParams(params)
    assert res['write_beats'] == (rows + hp.beat_bytes // 4 - 1) \
        // (hp.beat_bytes // 4)
    check(writes, params, records, rows)


# ---------------------------------------------------------------------------
# Model-driven correctness, per format.
# ---------------------------------------------------------------------------


@pytest.mark.parametrize('fmt', [Format.Q8_0, Format.Q4_K])
@pytest.mark.parametrize('n_outputs', [1, 2, 4])
def test_lane_operation_counters(fmt, n_outputs):
    rng = random.Random(13)
    k, rows = 256, 4 * n_outputs
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
    params = params_for(n_outputs=n_outputs)
    writes, res, _ = run_lane(params, records, rows, stream=True)
    check(writes, params, records, rows)
    segments = rows * nsegs
    chains = 2 if fmt is Format.Q4_K else 1
    assert res['records'] == len(records)
    assert res['folds'] == rows
    assert res['cast_ops'] == 2 * chains * segments
    assert res['mul_ops'] == 2 * chains * segments
    assert res['add_ops'] == chains * segments + 3 * rows


def test_lane_q5_0_matches_model():
    lane_case(Format.Q5_0, 96, 3, 11)


def test_lane_q8_0_matches_model():
    lane_case(Format.Q8_0, 64, 2, 12)


def test_lane_q4_k_matches_model():
    lane_case(Format.Q4_K, 512, 5, 13)


def test_lane_q6_k_matches_model():
    lane_case(Format.Q6_K, 256, 3, 14)


def test_lane_write_backpressure():
    lane_case(Format.Q6_K, 512, 5, 21, stall_wr=True)


def test_lane_inter_record_gaps():
    # The default (non-stream) harness inserts random input gaps; the
    # Q4_K correction path must survive bubbles in every lane pipe.
    lane_case(Format.Q4_K, 256, 4, 17)


@pytest.mark.parametrize('fmt,k', [(Format.Q8_0, 32), (Format.Q4_K, 256)])
@pytest.mark.parametrize('n_outputs', [1, 2, 4])
def test_lane_long_write_stall(fmt, k, n_outputs):
    """Results and fold contexts survive a stall beyond all queue
    capacity, with one-segment rows exercising admission while a
    previous capture is still pending."""
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
    writes, _, _ = run_lane(params, records, rows, hold_writes=1500)
    check(writes, params, records, rows)


def test_lane_short_k_fold_stress():
    lane_case(Format.Q8_0, 32, 17, 31)


def test_lane_q8_0_real_k():
    fmt, k, rows = Format.Q8_0, 576, 3
    rng = random.Random(41)
    wsegs = gen_wsegs(rng, fmt, rows * (k // 32))
    vec = [rng.uniform(-1, 1) for _ in range(k)]
    act = build_act(fmt, vec)
    records = mm_segments(fmt, k, rows, [
        pack_row(fmt, wsegs[r * (k // 32):(r + 1) * (k // 32)])
        for r in range(rows)
    ], act)
    writes, _, _ = run_lane(params_for(), records, rows, seed=41)
    check(writes, params_for(), records, rows)


# ---------------------------------------------------------------------------
# Hand-built records: ordering, cancellation, zeros, offsets.
# ---------------------------------------------------------------------------


def test_lane_isum4_order_is_bit_defining():
    recs = [
        srec(1 << 24, 0, False, s_act=F32_2M24),
        srec(1, 1, False, s_act=F32_2M24),
        srec(1, 2, False, s_act=F32_2M24),
        srec(-(1 << 24), 3, True, s_act=F32_2M24),
    ]
    assert mm_row_results(recs)[0] == 2.0**-24
    writes, _, _ = run_lane(params_for(), recs, 1)
    words = check(writes, params_for(), recs, 1)
    assert words[0] == F32_2M24


def test_lane_rounding_tie_sweep():
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
    writes, _, _ = run_lane(params_for(), recs, 1, seed=99)
    check(writes, params_for(), recs, 1)


def test_lane_q4_k_cancellation():
    recs = [
        srec(100,
             0,
             False,
             fmt=Format.Q4_K,
             d=ONE16,
             sum_a=100,
             dmin=ONE16,
             mi=1),
        srec(100,
             1,
             False,
             fmt=Format.Q4_K,
             d=ONE16,
             sum_a=99,
             dmin=ONE16,
             mi=1),
        srec(0, 2, False, fmt=Format.Q4_K, d=ONE16, sum_a=25, dmin=TWO16,
             mi=1),
        srec(7, 3, True, fmt=Format.Q4_K, d=ONE16, sum_a=0, dmin=ONE16, mi=63),
    ]
    writes, _, _ = run_lane(params_for(), recs, 1)
    words = check(writes, params_for(), recs, 1)
    assert words[0] == f32_bits(-42.0)


def test_lane_zero_rows_of_quants():
    recs = [srec(0, j, j == 3) for j in range(4)]
    writes, _, _ = run_lane(params_for(), recs, 1)
    words = check(writes, params_for(), recs, 1)
    assert words[0] == 0


def test_lane_lanes_disagree():
    recs = []
    for j in range(2):
        recs.append(
            dict(fmt=Format.Q8_0,
                 row_mask=3,
                 s_act=f32_bits(1.0),
                 sum_a=0,
                 seg_k=j,
                 row_last=j == 1,
                 S0=2 + j,
                 row0=0,
                 d0=ONE16,
                 S1=-(4 + j),
                 row1=1,
                 d1=ONE16))
    writes, _, _ = run_lane(params_for(), recs, 2)
    words = check(writes, params_for(), recs, 2)
    assert words[0] == f32_bits(5.0)
    assert words[1] == f32_bits(-9.0)


def test_lane_result_region_offset():
    recs = [srec(5, 0, True)]
    base = 0x600
    writes, _, _ = run_lane(params_for(), recs, 1, r_base=base)
    words = check(writes, params_for(), recs, 1, r_base=base)
    assert words[0] == f32_bits(5.0)


# ---------------------------------------------------------------------------
# Acceptance rate at deployment width: the de-share's deliverable.
# The shared epilogue's deterministic floors here were 2 cyc/record
# (Q8_0 family) and 4 (Q4_K); the per-lane banks must sustain 1.
# ---------------------------------------------------------------------------


def _rate_case(fmt, k, rows, seed, limit):
    rng = random.Random(seed)
    wsegs = gen_wsegs(rng, fmt, rows * (k // 32))
    act = [dict(e, sum_a=sum(e['quants'])) for e in gen_act(rng, k // 32)]
    records = mm_segments(fmt, k, rows, [
        pack_row(fmt, wsegs[r * (k // 32):(r + 1) * (k // 32)])
        for r in range(rows)
    ], act)
    writes, res, fires = run_lane(dict(WIDE),
                                  records,
                                  rows,
                                  stream=True,
                                  seed=seed)
    n = len(records)
    assert res['records'] == n
    assert res['folds'] == rows
    assert len(fires) == n, 'every record must fire exactly once'
    steady = (fires[-1] - fires[0] + 1) / n
    whole = res['active_cycles'] / n
    # Exact per-record issue plan at n_outputs=2.
    if fmt is Format.Q4_K:
        assert res['cast_ops'] == 8 * n
        assert res['mul_ops'] == 8 * n
        assert res['add_ops'] == 4 * n + 3 * rows
    else:
        assert res['cast_ops'] == 4 * n
        assert res['mul_ops'] == 4 * n
        assert res['add_ops'] == 2 * n + 3 * rows
    # Row turnover is covered by snapshot capacity, not serialization:
    # steady acceptance must be one record per cycle (a bubble budget
    # of one cycle per row covers the snapshot-retire race).
    assert steady <= limit, (fmt, steady, whole)
    check(writes, dict(WIDE), records, rows)
    return steady, whole


def test_lane_q8_0_rate():
    steady, whole = _rate_case(Format.Q8_0, 576, 64, 84, 1.02)
    assert whole < 1.25


def test_lane_q5_0_rate():
    steady, whole = _rate_case(Format.Q5_0, 576, 64, 85, 1.02)
    assert whole < 1.25


def test_lane_q6_k_rate():
    steady, whole = _rate_case(Format.Q6_K, 1536, 32, 86, 1.02)
    assert whole < 1.25


def test_lane_q4_k_rate():
    # The worst per-record demand: two full FP chains per lane.
    steady, whole = _rate_case(Format.Q4_K, 1536, 32, 87, 1.02)
    assert whole < 1.25


def test_lane_q4_k_short_rows_rate():
    # 8-segment rows turn over six times as often as Q4_K's canonical
    # 48: a row holds "occupied + pending" admission credit from its
    # last record's fire through capture (+14, Q4_K's landing depth)
    # and fold-half retire (+3), so two snapshot buffers cover rows of
    # >= ~17 segments (the canonical 18 and 48 sustain 1.0) but cap
    # 8-segment rows at ~1.5.  A third snapshot buffer is the sized
    # follow-up if sub-16-segment rows ever stream; this test pins the
    # two-buffer behavior.
    steady, whole = _rate_case(Format.Q4_K, 256, 64, 88, 1.70)
    assert whole < 2.0
