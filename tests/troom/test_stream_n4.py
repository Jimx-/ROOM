"""Focused pysim tests for the D2 wider join (n_outputs=4).

D2 item 2 of the streaming redesign: the SG1 image and ``StreamSupply``
interleave whole output-row *groups* whose width is the unit's
``n_outputs``. At n_outputs=4 (the quad image) the decoder emits up to
four windows per cycle into four lane queues, the launch join consumes
one four-lane slot per cycle against the 128-MAC array, and the D2
per-lane epilogue (``epilogue.Epilogue``) drains it -- the
reference contract is unchanged ``tests/troom/model.py`` at
``n_outputs=4``: chain-end records field for field in launch order, and
one integrated case per format through the per-lane epilogue compared
against ``mm_row_results`` bit for bit.

The rate cases are the D2 item-2 exit: at the 512-bit deployment width
the n=2 join was the binding stage for every format except Q8_0 (two
windows per cycle against Q5_0's 3.2 windows per beat, Q6_K's 2.67 and
Q4_K's 4); at n=4 the join's four windows per cycle cover every
format's beat demand, so the unit must sustain close to one beat per
cycle under an ideal memory -- the memory becomes the binding stage,
which is the design contract.

Quad packing notes exercised here: a short final group contributes only
its real rows' windows (rows 1..3 mod 4 tails; one-group tiles), the
bonded K-quant group is the cell's t lanes (a tail group's cells bond
fewer chunks), and consecutive tail-cell windows that share a lane
serialize exactly as the pair image's odd tails did.
"""

from amaranth import *
from amaranth.sim import Passive, Simulator

import pytest
import random

from troom.consts import Format, Status
from troom.epilogue import Epilogue
from troom.stream import (
    StreamGemv,
    pack_stream_kquant,
    pack_stream_q5_0,
    pack_stream_q8_0,
    stream_groups,
    unpack_stream_kquant,
    unpack_stream_q5_0,
    unpack_stream_q8_0,
)
from troom.types import HasTRoomParams

from tests.troom.model import f32_bits, mm_row_results
from tests.troom.test_matrix import gen_act, gen_wsegs, check_records
from tests.troom.test_stream import (
    M_BASE,
    P_BASE,
    Harness,
    make_tile,
    mem_model,
    params_for,
    place_tiles,
    push_desc,
    run_tiles,
)
from tests.troom.test_matrix import act_write


def n4_params(*fmts, **over):
    p = params_for(*fmts, **over)
    p['n_outputs'] = 4
    return p


def run_n4(k, rows, fmt=Format.Q8_0, **kw):
    kw.setdefault('params', n4_params(fmt))
    kw['params']['n_outputs'] = 4
    return run_tiles([dict(k=k, rows=rows, fmt=fmt)], **kw)


# ---------------------------------------------------------------------------
# Quad image contract (pure Python).
# ---------------------------------------------------------------------------

N4_PACKERS = {
    Format.Q8_0: pack_stream_q8_0,
    Format.Q5_0: pack_stream_q5_0,
    Format.Q6_K:
    lambda w, k, r, n: pack_stream_kquant(Format.Q6_K, w, k, r, n),
    Format.Q4_K:
    lambda w, k, r, n: pack_stream_kquant(Format.Q4_K, w, k, r, n),
}

N4_UNPACKERS = {
    Format.Q8_0:
    unpack_stream_q8_0,
    Format.Q5_0:
    unpack_stream_q5_0,
    Format.Q6_K:
    lambda p, m, k, r, n: unpack_stream_kquant(Format.Q6_K, p, m, k, r, n),
    Format.Q4_K:
    lambda p, m, k, r, n: unpack_stream_kquant(Format.Q4_K, p, m, k, r, n),
}


@pytest.mark.parametrize('fmt,k,rows', [
    (Format.Q8_0, 576, 5),
    (Format.Q8_0, 32, 1),
    (Format.Q8_0, 96, 7),
    (Format.Q5_0, 64, 4),
    (Format.Q5_0, 576, 6),
    (Format.Q6_K, 512, 5),
    (Format.Q6_K, 256, 6),
    (Format.Q4_K, 512, 7),
    (Format.Q4_K, 256, 2),
])
def test_n4_image_roundtrip(fmt, k, rows):
    rng = random.Random(31)
    nsegs = k // 32
    wsegs = gen_wsegs(rng, fmt, rows * nsegs)
    payload, meta = N4_PACKERS[fmt](wsegs, k, rows, 4)
    segs = N4_UNPACKERS[fmt](payload, meta, k, rows, 4)
    assert segs == wsegs
    # The group count is the quad count; extents match the pair image
    # (only the byte order differs).
    assert stream_groups(rows, 4) == (rows + 3) // 4


# ---------------------------------------------------------------------------
# Model-driven correctness per format, through the MAC array.
# ---------------------------------------------------------------------------


@pytest.mark.parametrize('fmt,k,rows', [
    (Format.Q8_0, 128, 8),
    (Format.Q8_0, 128, 5),
    (Format.Q8_0, 576, 7),
    (Format.Q5_0, 64, 6),
    (Format.Q5_0, 128, 5),
    (Format.Q6_K, 512, 6),
    (Format.Q6_K, 512, 9),
    (Format.Q4_K, 512, 7),
    (Format.Q4_K, 256, 5),
])
def test_n4_records(fmt, k, rows):
    out = run_n4(k, rows, fmt, seed=41)
    res = out['results'][-1]
    assert res['status'] is Status.OK
    check_records(out['records'], out['expected'], n_outputs=4)
    nsegs = k // 32
    assert res['slots'] == stream_groups(rows, 4) * nsegs


@pytest.mark.parametrize('fmt,rows', [
    (Format.Q8_0, 5),
    (Format.Q5_0, 6),
    (Format.Q6_K, 7),
    (Format.Q4_K, 5),
])
def test_n4_odd_rows_backpressure(fmt, rows):
    k = 128 if fmt in (Format.Q8_0, Format.Q5_0) else 512
    out = run_n4(k, rows, fmt, seed=51, stall_sink=True)
    assert out['results'][-1]['status'] is Status.OK
    check_records(out['records'], out['expected'], n_outputs=4)


def test_n4_one_block_rows():
    # One K block per row: the shortest possible rows (flat one window,
    # K-quant one chunk), including lone tail rows. (Q5_0 k=32 rows=1
    # packs 20 payload bytes -- not beat-divisible -- and belongs to
    # the rejects contract, not here.)
    for fmt, k, rows in ((Format.Q8_0, 32, 4), (Format.Q8_0, 32, 5),
                         (Format.Q8_0, 32, 1), (Format.Q5_0, 32, 4),
                         (Format.Q5_0, 32, 8), (Format.Q6_K, 256, 4),
                         (Format.Q6_K, 256, 5), (Format.Q4_K, 256,
                                                 4), (Format.Q4_K, 256, 1)):
        out = run_n4(k, rows, fmt, seed=61)
        assert out['results'][-1]['status'] is Status.OK
        check_records(out['records'], out['expected'], n_outputs=4)


def test_n4_port_request_stalls():
    out = run_n4(128, 6, Format.Q5_0, seed=71, latency=5, req_prob=0.35)
    assert out['results'][-1]['status'] is Status.OK
    check_records(out['records'], out['expected'], n_outputs=4)


@pytest.mark.parametrize('pay_depth,meta_depth,credits,burst', [
    (1, 1, 1, 1),
    (3, 2, 1, 256),
    (5, 3, 3, 3),
])
def test_n4_tiny_fifos(pay_depth, meta_depth, credits, burst):
    params = n4_params(Format.Q5_0,
                       Format.Q4_K,
                       sg_pay_fifo=pay_depth,
                       sg_meta_fifo=meta_depth,
                       sg_credits=credits,
                       sg_burst_beats=burst)
    out = run_tiles([
        dict(k=64, rows=6, fmt=Format.Q5_0),
        dict(k=256, rows=5, fmt=Format.Q4_K)
    ],
                    params=params,
                    latency=5,
                    req_prob=0.35,
                    stall_sink=True,
                    seed=81)
    assert all(r['status'] is Status.OK for r in out['results'])
    check_records(out['records'], out['expected'], n_outputs=4)


def test_n4_back_to_back_format_change():
    params = n4_params(Format.Q6_K, Format.Q5_0, Format.Q8_0)
    out = run_tiles([
        dict(k=512, rows=6, fmt=Format.Q6_K),
        dict(k=128, rows=5, fmt=Format.Q5_0),
        dict(k=128, rows=8, fmt=Format.Q8_0),
    ],
                    params=params,
                    seed=91)
    assert all(r['status'] is Status.OK for r in out['results'])
    check_records(out['records'], out['expected'], n_outputs=4)


# ---------------------------------------------------------------------------
# Rejection contract is the group-width-independent extent contract.
# ---------------------------------------------------------------------------


def test_n4_rejects():
    # Q5_0 registered so the extent check itself is exercised: 3 rows
    # of one 20-byte window is not a whole 8-byte beat.
    params = n4_params(Format.Q8_0, Format.Q5_0)
    rng = random.Random(31)
    k, rows = 64, 4
    tile = make_tile(rng, k, rows, n_outputs=4)
    image = place_tiles(params, [tile])

    cases = [
        (Format.Q5_0, 32, 3, P_BASE, M_BASE, Status.BAD_SHAPE),
        (Format.Q8_0, 33, 4, P_BASE, M_BASE, Status.BAD_SHAPE),
        (Format.Q8_0, 64, 0, P_BASE, M_BASE, Status.BAD_SHAPE),
        (Format.Q8_0, 64, 1, P_BASE + 1, M_BASE, Status.BAD_ALIGN),
        (Format.Q8_0, 64, 1, P_BASE, M_BASE + 4, Status.BAD_ALIGN),
    ]

    for fmt, ck, crows, pb, mb, want in cases:
        dut = Harness(params)
        seen = {}

        def setup():
            yield Passive()
            while 'status' not in seen:
                if (yield dut.gemv.done):
                    seen['status'] = Status((yield dut.gemv.status))
                yield

        def drive():
            for j, entry in enumerate(tile['act']):
                yield from act_write(dut.gemv, j, entry)
            yield from push_desc(dut.gemv, fmt, ck, crows, pb, mb)
            for _ in range(200):
                yield
            assert 'status' in seen, 'no completion for rejected descriptor'

        sim = Simulator(dut)
        sim.add_clock(1e-6)
        sim.add_sync_process(drive)
        sim.add_sync_process(setup)
        sim.add_sync_process(mem_model(dut, image, params))
        sim.run()
        assert seen['status'] is want, (fmt, ck, crows, seen['status'])


# ---------------------------------------------------------------------------
# Deployment width: the D2 rate contract at one beat per cycle.
# ---------------------------------------------------------------------------


@pytest.mark.parametrize(
    'fmt,k,rows,wpc',
    [
        # windows per 64-B beat: Q5_0 3.2, Q6_K 2.67, Q4_K 4, Q8_0 2 --
        # every format at or under the n=4 join's 4 windows/cycle. Tiles
        # large enough that fill/drain amortize (the measured reference:
        # 0.989/0.983/0.938/0.935 beats/cycle for these shapes).
        (Format.Q5_0, 576, 64, 3.2),
        (Format.Q6_K, 512, 32, 8 / 3),
        (Format.Q4_K, 512, 64, 4.0),
        (Format.Q8_0, 576, 64, 2.0),
    ])
def test_n4_rate_contract(fmt, k, rows, wpc):
    # mm_out_depth=24 isolates the structural rate from the out_q
    # credit artifact (depth 16 vs the 18-cycle MAC chain), matching
    # the phase-B rate discipline.
    params = n4_params(fmt, data_width=512, mm_out_depth=24)
    out = run_tiles([dict(k=k, rows=rows, fmt=fmt)],
                    params=params,
                    seed=101,
                    deadline=4000000)
    res = out['results'][-1]
    assert res['status'] is Status.OK
    check_records(out['records'], out['expected'], n_outputs=4)

    nsegs = k // 32
    total = res['pay_beats'] + res['meta_beats']
    assert res['slots'] == stream_groups(rows, 4) * nsegs
    assert res['credit'] == 0, res
    # The D2 contract: the supply/join keeps up with the port, so RUN
    # is bounded by the beats that must arrive (one per cycle) plus
    # fill/drain slack -- not by the join's window rate.
    assert res['run_cycles'] <= total + 24, res
    beats_per_cyc = total / res['run_cycles']
    assert beats_per_cyc > 0.93, (fmt, beats_per_cyc, res)
    # Useful-weight rate: the n=2 join capped every format at two
    # windows per cycle (<= 64 useful weights); the wider join must
    # exceed that on the formats the beat rate allows (Q5_0 3.2, Q6_K
    # 2.67, Q4_K 4 windows per cycle at one beat per cycle).
    if wpc > 2.0:
        assert beats_per_cyc * wpc * 32 > 64, (fmt, beats_per_cyc)


def test_n4_rate_short_tail():
    # A two-lane tail group serializes its cells to two windows per
    # cycle (distinct-lane discipline); a 64-row tile amortizes the
    # tail, and the records must stay exact.
    params = n4_params(Format.Q8_0, data_width=512, mm_out_depth=24)
    out = run_tiles([dict(k=576, rows=66, fmt=Format.Q8_0)],
                    params=params,
                    seed=101,
                    deadline=4000)
    res = out['results'][-1]
    assert res['status'] is Status.OK
    check_records(out['records'], out['expected'], n_outputs=4)
    total = res['pay_beats'] + res['meta_beats']
    # The two-lane tail adds nsegs serialized cells at the end.
    assert res['run_cycles'] <= total + 24 + 18, res


@pytest.mark.parametrize('fmt,k', [
    (Format.Q8_0, 32),
    (Format.Q5_0, 64),
    (Format.Q6_K, 256),
    (Format.Q4_K, 256),
])
@pytest.mark.parametrize('tail', [1, 2, 3])
def test_n4_large_group_count_short_tail(fmt, k, tail):
    # More than seven groups must not saturate the persistent group
    # counter. Only the combinational look-ahead may saturate: the
    # final short group must stay unreachable until all full groups
    # have retired. Exercise each tail and bonded/flat format.
    out = run_n4(k, 40 + tail, fmt, seed=107, deadline=4000)
    assert out['results'][-1]['status'] is Status.OK
    check_records(out['records'], out['expected'], n_outputs=4)


def test_n4_deployment_depth_counters():
    # The deployed out_q depth (16) against the 18-cycle chain: the
    # credit artifact is bounded, not a structural rate loss.
    params = n4_params(Format.Q5_0, data_width=512)
    out = run_tiles([dict(k=576, rows=64, fmt=Format.Q5_0)],
                    params=params,
                    seed=101,
                    deadline=4000000)
    res = out['results'][-1]
    assert res['status'] is Status.OK
    check_records(out['records'], out['expected'], n_outputs=4)
    total = res['pay_beats'] + res['meta_beats']
    assert res['run_cycles'] <= total + 96, res


@pytest.mark.parametrize('pay_depth,meta_depth,credits', [
    (32, 8, 64),
    (64, 8, 64),
    (64, 32, 64),
    (128, 32, 128),
    (3, 1, 2),
    (8, 3, 4),
])
@pytest.mark.parametrize('batch,meta_batch', [(1, 1), (16, 8)])
def test_n4_batched_refill_tails_and_stalls(pay_depth, meta_depth, credits,
                                            batch, meta_batch):
    # Batch thresholds must clip to FIFO/credit limits and the region's
    # final short burst. Change formats with short row groups while both
    # the request port and result sink stall; the memory model checks
    # held requests, reservations and ready on every returned beat.
    params = n4_params(Format.Q8_0,
                       Format.Q5_0,
                       Format.Q6_K,
                       Format.Q4_K,
                       data_width=512,
                       mm_out_depth=24,
                       sg_min_burst_beats=batch,
                       sg_pay_fifo=pay_depth,
                       sg_meta_min_burst_beats=meta_batch,
                       sg_meta_fifo=meta_depth,
                       sg_credits=credits)
    tiles = [
        dict(k=k, rows=rows, fmt=fmt) for fmt, k, rows in (
            (Format.Q8_0, 64, 43),
            (Format.Q5_0, 512, 41),
            (Format.Q6_K, 256, 42),
            (Format.Q4_K, 256, 43),
        )
    ]
    out = run_tiles(tiles,
                    params=params,
                    latency=17,
                    req_prob=0.45,
                    stall_sink=True,
                    seed=131,
                    deadline=4000)
    assert all(r['status'] is Status.OK for r in out['results'])
    check_records(out['records'], out['expected'], n_outputs=4)


# ---------------------------------------------------------------------------
# Full replacement chain through the D2 per-lane epilogue.
# ---------------------------------------------------------------------------


class LaneEpilogueHarness(Elaboratable):
    """StreamGemv at n=4 draining through the per-lane epilogue."""

    def __init__(self, params):
        self.params = params
        self.gemv = StreamGemv(params)
        self.epi = Epilogue(params)

    def elaborate(self, platform):
        m = Module()
        m.submodules.gemv = self.gemv
        m.submodules.epi = self.epi
        m.d.comb += self.gemv.segs.connect(self.epi.sink)
        return m


@pytest.mark.parametrize('fmt,k', [
    (Format.Q8_0, 128),
    (Format.Q5_0, 128),
    (Format.Q6_K, 256),
    (Format.Q4_K, 256),
])
def test_n4_with_lane_epilogue_bit_exact(fmt, k):
    rows = 5  # a two-lane tail group through the packer
    rng = random.Random(43)
    params = n4_params(fmt)
    tile_out = run_tiles([dict(k=k, rows=rows, fmt=fmt)],
                         params=params,
                         seed=43)
    tile = tile_out['tiles'][0]
    want = mm_row_results(tile['exp'])
    image = place_tiles(params, [tile])

    r_base = 0x80
    dut = LaneEpilogueHarness(params)
    writes = []
    done = {}

    def setup():
        for j, entry in enumerate(tile['act']):
            yield from act_write(dut.gemv, j, entry)
        for _ in range(3):
            yield
        while not (yield dut.epi.ready):
            yield
        yield dut.epi.rows.eq(rows)
        yield dut.epi.r_base.eq(r_base)
        yield dut.epi.start.eq(1)
        yield
        yield dut.epi.start.eq(0)
        yield
        yield from push_desc(dut.gemv, fmt, k, rows, tile['p_base'],
                             tile['m_base'])
        for _ in range(40000):
            yield
            if (yield dut.epi.done):
                done['ok'] = True
                break
        else:
            raise TimeoutError('epilogue did not finish')

    def wr():
        yield Passive()
        w = dut.epi.tmem_w
        while True:
            yield w.ready.eq(1)
            if (yield w.fire):
                writes.append(((yield w.bits.addr), (yield w.bits.data)))
            yield

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(setup)
    sim.add_sync_process(mem_model(dut, image, params))
    sim.add_sync_process(wr)
    sim.run()

    assert done.get('ok')
    hp = HasTRoomParams(params)
    bb = hp.beat_bytes
    mem = {}
    for addr, data in writes:
        mem[addr] = data.to_bytes(bb, 'little')
    for r in range(rows):
        addr = r_base + 4 * r
        beat = mem[addr & ~(bb - 1)]
        off = addr & (bb - 1)
        got = int.from_bytes(beat[off:off + 4], 'little')
        assert got == f32_bits(want[r]), (r, hex(got), hex(f32_bits(want[r])))
