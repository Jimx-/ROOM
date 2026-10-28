"""Focused pysim tests for the troom direct-stream GEMV prototype.

Phase B of the streaming architecture redesign (see
``plans/2026-10-20-troom-streaming-architecture-redesign.md``): the
``troom.stream`` SG1 image, reader/metadata queues, and decoder feed
the proven MAC array, so the reference side is the same
``tests/troom/model.py`` MATMUL contract the staged path is checked
against -- chain-end records field for field in launch order, and one
integrated case through the unchanged FP epilogue compared against
``mm_row_results`` bit for bit.

Coverage: the SG1 pack/unpack contract against the GGUF model packer;
random tiles even/odd rows, short K, one-row tiles; output
backpressure and port request stalls; back-to-back descriptors across
the meta tail pad; capability/shape rejection; tiny-FIFO credit
stress; the rate contract under an ideal always-ready memory (the
launch join must keep up with one beat per cycle up to fill latency);
and the 512-bit deployment width, where one beat is exactly one launch
slot (including the odd-tail hold path).
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
    stream_pairs,
    unpack_stream_kquant,
    unpack_stream_q5_0,
    unpack_stream_q8_0,
)
from troom.types import HasTRoomParams

from tests.troom.model import (
    _sgn8,
    f32_bits,
    mm_row_results,
    mm_segments,
    pack_row,
    unpack_row,
)
from tests.troom.test_matrix import (
    act_write,
    check_records,
    gen_act,
    gen_wsegs,
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

P_BASE = 0x4000
M_BASE = 0x6000


def record_fields(n_outputs):
    """MmSegment field names at a given join width."""
    return (['fmt', 'row_mask', 's_act', 'sum_a', 'seg_k', 'row_last'] + [
        f'{f}{p}' for p in range(n_outputs)
        for f in ('S', 'row', 'd', 'dmin', 'mi')
    ])


RECORD_FIELDS = record_fields(2)


def params_for(*fmts, **over):
    p = dict(PARAMS)
    p['mm_formats'] = fmts if fmts else (Format.Q8_0, )
    p.update(over)
    return p


def make_tile(rng, k, rows, fmt=Format.Q8_0, n_outputs=2):
    """Random tile with its model reference records."""
    nsegs = k // 32
    wsegs = gen_wsegs(rng, fmt, rows * nsegs)
    act = [dict(e, sum_a=sum(e['quants'])) for e in gen_act(rng, nsegs)]
    packed = [
        pack_row(fmt, wsegs[r * nsegs:(r + 1) * nsegs]) for r in range(rows)
    ]
    return dict(k=k,
                rows=rows,
                fmt=fmt,
                wsegs=wsegs,
                act=act,
                exp=mm_segments(fmt, k, rows, packed, act,
                                n_outputs=n_outputs))


STREAM_PACKERS = {
    Format.Q8_0:
    pack_stream_q8_0,
    Format.Q5_0:
    pack_stream_q5_0,
    Format.Q6_K:
    lambda w, k, r, n_outputs=2: pack_stream_kquant(Format.Q6_K, w, k, r,
                                                    n_outputs),
    Format.Q4_K:
    lambda w, k, r, n_outputs=2: pack_stream_kquant(Format.Q4_K, w, k, r,
                                                    n_outputs),
}


def place_tiles(params, tiles, p_start=P_BASE, m_start=None):
    """Lay tiles out in one DRAM image; set each tile's region bases.

    Returns the image. Payloads pack densely in tile order from
    ``p_start``; the meta region starts beat-aligned after the *whole*
    payload region (or at ``m_start`` when forced), with metas dense
    in tile order, so the two regions are disjoint by construction
    from the actual packed sizes. (The old fixed ``M_BASE`` gap left 8
    KiB between them: the 16 KiB payload of the 512-bit Q4_K 512x64
    rate tile overwrote the middle of its meta region, and the
    corruption survived because that script checked only rates, not
    records.)
    """
    hp = HasTRoomParams(params)
    bb = hp.beat_bytes
    n_out = hp.n_outputs
    packed = []
    p_cur = p_start
    for t in tiles:
        payload, meta = STREAM_PACKERS[t['fmt']](t['wsegs'],
                                                 t['k'],
                                                 t['rows'],
                                                 n_outputs=n_out)
        meta = meta + bytes((-len(meta)) % bb)
        packed.append((t, payload, meta))
        t['p_base'] = p_cur
        p_cur += len(payload)
    m_cur = m_start if m_start is not None else -(-p_cur // bb) * bb
    image = bytearray(m_cur)
    for t, payload, meta in packed:
        assert t['p_base'] + len(payload) <= len(image)
        if m_cur + len(meta) > len(image):
            image.extend(bytes(m_cur + len(meta) - len(image)))
        image[t['p_base']:t['p_base'] + len(payload)] = payload
        image[m_cur:m_cur + len(meta)] = meta
        t['m_base'] = m_cur
        m_cur += len(meta)
    return image


class Harness(Elaboratable):
    """StreamGemv alone: the testbench models the memory port."""

    def __init__(self, params):
        self.params = params
        self.gemv = StreamGemv(params)

    def elaborate(self, platform):
        m = Module()
        m.submodules.gemv = self.gemv
        return m


class EpilogueHarness(Elaboratable):
    """StreamGemv draining through the unchanged FP epilogue."""

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


def mem_model(dut, image, params, *, latency=2, req_prob=1.0, seed=5):
    """Ideal burst memory: in-order responses at one beat per cycle.

    Requests are accepted every cycle (optionably gated by ``req_prob``
    to model port contention); each accepted burst returns its beats
    back to back starting ``latency`` cycles later. The DUT's FIFO
    space crediting guarantees every outstanding beat has queue space,
    so responses are never held by the DUT's ready.
    """

    def proc():
        yield Passive()
        rreq = dut.gemv.mem_rreq
        rresp = dut.gemv.mem_rresp
        hp = HasTRoomParams(params)
        bb = hp.beat_bytes
        rng = random.Random(seed)
        pend = []
        cyc = 0
        held_request = None

        yield rresp.valid.eq(0)
        yield rreq.ready.eq(1)
        while True:
            # The external request boundary must hold its complete
            # descriptor while AXI refuses it, even as responses land.
            request = ((yield
                        rreq.bits.addr), (yield
                                          rreq.bits.beats), (yield
                                                             rreq.bits.meta))
            if held_request is not None:
                assert (yield rreq.valid)
                assert request == held_request
            held_request = request if ((yield rreq.valid)
                                       and not (yield rreq.ready)) else None

            supply = dut.gemv.supply
            if (yield supply.run) and not (yield supply.start):
                # Includes requests waiting in the registered request
                # queue, not only those accepted by this memory model.
                for used, outstanding, fifo, depth in ((supply.p_used,
                                                        supply.p_out,
                                                        supply.pay_q,
                                                        hp.sg_pay_fifo),
                                                       (supply.m_used,
                                                        supply.m_out,
                                                        supply.meta_q,
                                                        hp.sg_meta_fifo)):
                    booked = yield used
                    pending = yield outstanding
                    stored = yield fifo.count
                    assert booked == pending + stored
                    assert 0 <= booked <= depth
                    assert 0 <= pending <= min(depth, hp.sg_credits)
            if (yield rresp.valid):
                assert (yield rresp.ready), 'reserved response has no space'

            # Fires of the cycle now in progress (outputs driven at the
            # end of the previous iteration are stable this cycle).
            if (yield rreq.valid) & (yield rreq.ready):
                pend.append(
                    dict(addr=(yield rreq.bits.addr),
                         left=(yield rreq.bits.beats),
                         tag=(yield rreq.bits.meta),
                         due=cyc + latency))
            if pend and (yield rresp.valid) and (yield rresp.ready):
                e = pend[0]
                e['addr'] += bb
                e['left'] -= 1
                if e['left'] == 0:
                    pend.pop(0)
            cyc += 1

            # Drive the next cycle.
            gate = rng.random() < req_prob
            yield rreq.ready.eq(gate)
            if pend and pend[0]['due'] <= cyc:
                e = pend[0]
                yield rresp.valid.eq(1)
                yield rresp.bits.data.eq(
                    int.from_bytes(image[e['addr']:e['addr'] + bb], 'little'))
                yield rresp.bits.meta.eq(e['tag'])
            else:
                yield rresp.valid.eq(0)
            yield

    return proc


def push_desc(unit, fmt, k, rows, p_base=P_BASE, m_base=M_BASE):
    sink = unit.sink
    yield sink.bits.fmt.eq(int(fmt))
    yield sink.bits.k.eq(k)
    yield sink.bits.rows.eq(rows)
    yield sink.bits.p_base.eq(p_base)
    yield sink.bits.m_base.eq(m_base)
    yield sink.valid.eq(1)
    yield
    while not (yield sink.fire):
        yield
    yield sink.valid.eq(0)
    yield


def run_tiles(tiles,
              *,
              seed=7,
              params=None,
              latency=2,
              req_prob=1.0,
              stall_sink=False,
              deadline=40000,
              p_start=P_BASE,
              m_start=None):
    """Run tiles back to back; return records, per-done counters, etc."""
    rng = random.Random(seed)
    params = params if params is not None else params_for()
    n_out = HasTRoomParams(params).n_outputs
    if 'wsegs' not in tiles[0]:
        for t in tiles:
            t.update(
                make_tile(rng,
                          t['k'],
                          t['rows'],
                          t.get('fmt', Format.Q8_0),
                          n_outputs=n_out))
    image = place_tiles(params, tiles, p_start=p_start, m_start=m_start)

    dut = Harness(params)
    records = []
    results = []
    expected = []

    def setup():
        for t in tiles:
            for j, entry in enumerate(t['act']):
                yield from act_write(dut.gemv, j, entry)
            yield from push_desc(dut.gemv, t.get('fmt', Format.Q8_0), t['k'],
                                 t['rows'], t['p_base'], t['m_base'])
            expected.extend(t['exp'])
            for _ in range(deadline):
                yield
                if (yield dut.gemv.done):
                    results.append(
                        dict(
                            status=Status((yield dut.gemv.status)),
                            slots=(yield dut.gemv.slots),
                            bubbles=(yield dut.gemv.bubbles),
                            starve=(yield dut.gemv.starve_cycles),
                            credit=(yield dut.gemv.credit_stalls),
                            run_cycles=(yield dut.gemv.run_cycles),
                            drain_cycles=(yield dut.gemv.drain_cycles),
                            pay_beats=(yield dut.gemv.pay_beats),
                            meta_beats=(yield dut.gemv.meta_beats),
                            meta_pad=(yield dut.gemv.meta_pad_bytes),
                            bursts=(yield dut.gemv.bursts),
                            port_idle=(yield dut.gemv.port_idle),
                        ))
                    break
            else:
                s = dut.gemv.supply
                observed = {}
                for name in ('pair', 'j', 'lane', 'grp_rem', 'mhave', 'p_rem',
                             'm_rem', 'p_used', 'm_used'):
                    observed[name] = (yield getattr(s, name))
                for name in ('ucnt', 'head', 'ph'):
                    observed[name] = (yield getattr(s.pay_asm, name))
                for name in ('join_pair', 'join_seg', 'join_act_cnt',
                             'join_rd_idx'):
                    observed[name] = (yield getattr(dut.gemv, name))
                observed['slots'] = (yield dut.gemv.slots)
                observed['out_valid'] = (yield dut.gemv.segs.valid)
                observed['records'] = len(records)
                observed['drain_cycles'] = (yield dut.gemv.drain_cycles)
                raise TimeoutError(f'stream gemv did not finish: {observed}')

    def sink():
        yield Passive()
        deq = dut.gemv.segs
        fields = record_fields(n_out)
        while True:
            yield deq.ready.eq(rng.random() < 0.45 if stall_sink else 1)
            if (yield deq.valid) & (yield deq.ready):
                rec = {}
                for f in fields:
                    rec[f] = (yield getattr(deq.bits, f))
                records.append(rec)
            yield

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(setup)
    sim.add_sync_process(
        mem_model(dut,
                  image,
                  params,
                  latency=latency,
                  req_prob=req_prob,
                  seed=seed))
    sim.add_sync_process(sink)
    sim.run()

    return dict(records=records,
                expected=expected,
                results=results,
                tiles=tiles,
                params=params)


def run_stream(k, rows, fmt=Format.Q8_0, **kw):
    return run_tiles([dict(k=k, rows=rows, fmt=fmt)], **kw)


# ---------------------------------------------------------------------------
# SG1 image contract (pure Python).
# ---------------------------------------------------------------------------


@pytest.mark.parametrize('k,rows', [
    (576, 5),
    (64, 4),
    (32, 1),
    (96, 7),
])
def test_stream_image_roundtrip(k, rows):
    rng = random.Random(11)
    nsegs = k // 32
    wsegs = gen_wsegs(rng, Format.Q8_0, rows * nsegs)
    payload, meta = pack_stream_q8_0(wsegs, k, rows)

    assert len(payload) == rows * k
    assert len(meta) == rows * nsegs * 2
    assert stream_pairs(rows) == (rows + 1) // 2

    back = unpack_stream_q8_0(payload, meta, k, rows)
    for r in range(rows):
        for j in range(nsegs):
            got, exp = back[r * nsegs + j], wsegs[r * nsegs + j]
            assert got['w'] == exp['w']
            assert got['d'] == exp['d']

    # The same weights in GGUF device order decode to the same segments.
    for r in range(rows):
        gguf = pack_row(Format.Q8_0, wsegs[r * nsegs:(r + 1) * nsegs])
        ref = unpack_row(Format.Q8_0, gguf)
        for j in range(nsegs):
            assert back[r * nsegs + j]['w'] == ref[j]['w']
            assert back[r * nsegs + j]['d'] == ref[j]['d']


@pytest.mark.parametrize('k,rows', [
    (576, 5),
    (64, 4),
    (32, 1),
    (160, 3),
])
def test_stream_image_roundtrip_q5_0(k, rows):
    rng = random.Random(12)
    nsegs = k // 32
    wsegs = gen_wsegs(rng, Format.Q5_0, rows * nsegs)
    payload, meta = pack_stream_q5_0(wsegs, k, rows)

    assert len(payload) == rows * nsegs * 20
    assert len(meta) == rows * nsegs * 2

    back = unpack_stream_q5_0(payload, meta, k, rows)
    for r in range(rows):
        for j in range(nsegs):
            got, exp = back[r * nsegs + j], wsegs[r * nsegs + j]
            assert got['w'] == exp['w']
            assert got['d'] == exp['d']

    # The payload's 20-byte groups are the GGUF block bytes with d
    # split to meta: re-assembling d + qh + qs must reproduce the
    # reference packer's decode exactly.
    for r in range(rows):
        gguf = pack_row(Format.Q5_0, wsegs[r * nsegs:(r + 1) * nsegs])
        ref = unpack_row(Format.Q5_0, gguf)
        for j in range(nsegs):
            assert back[r * nsegs + j]['w'] == ref[j]['w']
            assert back[r * nsegs + j]['d'] == ref[j]['d']


@pytest.mark.parametrize('fmt', [Format.Q6_K, Format.Q4_K])
@pytest.mark.parametrize('k,rows', [
    (256, 4),
    (512, 5),
    (1024, 2),
    (256, 1),
])
def test_stream_image_roundtrip_kquant(fmt, k, rows):
    rng = random.Random(14)
    nsegs = k // 32
    chunks = nsegs // 8
    wsegs = gen_wsegs(rng, fmt, rows * nsegs)
    payload, meta = pack_stream_kquant(fmt, wsegs, k, rows)

    chunk_b = 192 if fmt is Format.Q6_K else 128
    entry_b = 18 if fmt is Format.Q6_K else 16
    assert len(payload) == rows * chunks * chunk_b
    assert len(meta) == rows * chunks * entry_b

    back = unpack_stream_kquant(fmt, payload, meta, k, rows)
    for r in range(rows):
        for j in range(nsegs):
            got, exp = back[r * nsegs + j], wsegs[r * nsegs + j]
            assert got['w'] == exp['w']
            assert got['d'] == exp['d']
            assert got['dmin'] == exp['dmin']
            assert got['mi'] == exp['mi']
            assert got['sc0'] == exp['sc0']
            assert got['sc1'] == exp['sc1']


# ---------------------------------------------------------------------------
# Record-level bit-exactness against the model.
# ---------------------------------------------------------------------------


@pytest.mark.parametrize(
    'k,rows',
    [
        (64, 4),
        (576, 4),
        (32, 2),
        (32, 1),
        (96, 5),
        (160, 3),
        (1024, 2),  # nsegs == 2**lg_act_segs: the wrap compare's edge
    ])
def test_stream_q8_0_records(k, rows):
    out = run_stream(k, rows)
    assert out['results'][-1]['status'] is Status.OK
    check_records(out['records'], out['expected'])

    hp = HasTRoomParams(out['params'])
    nsegs = k // 32
    res = out['results'][-1]
    assert res['slots'] == stream_pairs(rows) * nsegs
    assert res['pay_beats'] == rows * k // hp.beat_bytes
    assert res['meta_beats'] == -(-(rows * nsegs * 2) // hp.beat_bytes)
    assert res['meta_pad'] == (res['meta_beats'] * hp.beat_bytes -
                               rows * nsegs * 2)
    assert res['bubbles'] == res['starve'] + res['credit']


def test_stream_q8_0_odd_rows_backpressure():
    out = run_stream(128, 5, stall_sink=True, seed=13)
    assert out['results'][-1]['status'] is Status.OK
    check_records(out['records'], out['expected'])


@pytest.mark.parametrize('k,rows', [
    (64, 4),
    (576, 4),
    (32, 2),
    (64, 5),
    (96, 4),
])
def test_stream_q5_0_records(k, rows):
    params = params_for(Format.Q8_0, Format.Q5_0)
    out = run_stream(k, rows, params=params, fmt=Format.Q5_0)
    assert out['results'][-1]['status'] is Status.OK
    check_records(out['records'], out['expected'])

    hp = HasTRoomParams(params)
    nsegs = k // 32
    res = out['results'][-1]
    assert res['slots'] == stream_pairs(rows) * nsegs
    assert res['pay_beats'] == rows * nsegs * 20 // hp.beat_bytes
    assert res['meta_beats'] == -(-(rows * nsegs * 2) // hp.beat_bytes)
    assert res['meta_pad'] == (res['meta_beats'] * hp.beat_bytes -
                               rows * nsegs * 2)
    assert res['bubbles'] == res['starve'] + res['credit']


def test_stream_q5_0_odd_rows_backpressure():
    params = params_for(Format.Q8_0, Format.Q5_0)
    out = run_stream(128,
                     5,
                     params=params,
                     fmt=Format.Q5_0,
                     stall_sink=True,
                     seed=13)
    assert out['results'][-1]['status'] is Status.OK
    check_records(out['records'], out['expected'])


def test_stream_q5_0_port_request_stalls():
    params = params_for(Format.Q8_0, Format.Q5_0)
    out = run_stream(96,
                     4,
                     params=params,
                     fmt=Format.Q5_0,
                     req_prob=0.6,
                     latency=5,
                     seed=17)
    assert out['results'][-1]['status'] is Status.OK
    check_records(out['records'], out['expected'])


def test_stream_q5_0_tiny_fifos():
    params = params_for(Format.Q8_0,
                        Format.Q5_0,
                        sg_burst_beats=3,
                        sg_pay_fifo=4,
                        sg_meta_fifo=2,
                        sg_credits=8)
    out = run_stream(64,
                     4,
                     params=params,
                     fmt=Format.Q5_0,
                     latency=3,
                     req_prob=0.7,
                     seed=23)
    assert out['results'][-1]['status'] is Status.OK
    check_records(out['records'], out['expected'])


KQ_CHUNK = {Format.Q6_K: 192, Format.Q4_K: 128}
KQ_ENTRY = {Format.Q6_K: 18, Format.Q4_K: 16}


def check_kq_counters(out, fmt, k, rows):
    hp = HasTRoomParams(out['params'])
    nsegs = k // 32
    chunks = nsegs // 8
    res = out['results'][-1]
    assert res['slots'] == stream_pairs(rows) * nsegs
    assert res['pay_beats'] == rows * chunks * KQ_CHUNK[fmt] \
        // hp.beat_bytes
    assert res['meta_beats'] == -(-(rows * chunks * KQ_ENTRY[fmt]) //
                                  hp.beat_bytes)
    assert res['meta_pad'] == (res['meta_beats'] * hp.beat_bytes -
                               rows * chunks * KQ_ENTRY[fmt])
    assert res['bubbles'] == res['starve'] + res['credit']


@pytest.mark.parametrize('fmt', [Format.Q6_K, Format.Q4_K])
@pytest.mark.parametrize('k,rows', [
    (256, 4),
    (512, 5),
    (1024, 4),
    (256, 3),
])
def test_stream_kq_records(fmt, k, rows):
    params = params_for(Format.Q8_0, Format.Q5_0, fmt)
    out = run_stream(k, rows, params=params, fmt=fmt)
    assert out['results'][-1]['status'] is Status.OK
    check_records(out['records'], out['expected'])
    check_kq_counters(out, fmt, k, rows)


def test_stream_q6_k_odd_rows_backpressure():
    params = params_for(Format.Q8_0, Format.Q5_0, Format.Q6_K)
    out = run_stream(512,
                     5,
                     params=params,
                     fmt=Format.Q6_K,
                     stall_sink=True,
                     seed=13)
    assert out['results'][-1]['status'] is Status.OK
    check_records(out['records'], out['expected'])


def test_stream_q4_k_backpressure_and_port_stalls():
    params = params_for(Format.Q4_K)
    out = run_stream(256,
                     4,
                     params=params,
                     fmt=Format.Q4_K,
                     stall_sink=True,
                     req_prob=0.6,
                     latency=5,
                     seed=17)
    assert out['results'][-1]['status'] is Status.OK
    check_records(out['records'], out['expected'])


@pytest.mark.parametrize('fmt', [Format.Q6_K, Format.Q4_K])
def test_stream_kq_chunk_boundary_stall(fmt):
    # Directed at the chunk release boundary: backpressure landing on
    # a chunk's 8th window must hold the chunk and its meta entry
    # until the emission actually fires. The pre-fix decoder released
    # both whenever j == 7 regardless of fire0, so the stalled mate
    # discarded the 8th window's payload live -- reproduced by Q6_K
    # k=1024 at the deployment width (record 55 returned S0=0 against
    # -303698). The stalled sink backs the lane queues up onto the
    # bonded pair exactly across whole super-block releases.
    params = params_for(fmt, data_width=512)
    out = run_stream(1024, 4, params=params, fmt=fmt, stall_sink=True, seed=7)
    assert out['results'][-1]['status'] is Status.OK
    check_records(out['records'], out['expected'])
    check_kq_counters(out, fmt, 1024, 4)


def test_stream_kq_tiny_fifos():
    params = params_for(Format.Q6_K,
                        Format.Q4_K,
                        sg_burst_beats=3,
                        sg_pay_fifo=4,
                        sg_meta_fifo=2,
                        sg_credits=8)
    out = run_stream(256,
                     4,
                     params=params,
                     fmt=Format.Q4_K,
                     latency=3,
                     req_prob=0.7,
                     seed=23)
    assert out['results'][-1]['status'] is Status.OK
    check_records(out['records'], out['expected'])


def test_stream_port_request_stalls():
    out = run_stream(96, 4, req_prob=0.6, latency=5, seed=17)
    assert out['results'][-1]['status'] is Status.OK
    check_records(out['records'], out['expected'])
    assert out['results'][-1]['port_idle'] > 0


def test_stream_tiny_fifos():
    params = params_for(sg_burst_beats=3,
                        sg_pay_fifo=4,
                        sg_meta_fifo=2,
                        sg_credits=8)
    out = run_stream(64, 5, params=params, latency=3, req_prob=0.7, seed=23)
    assert out['results'][-1]['status'] is Status.OK
    check_records(out['records'], out['expected'])


def test_stream_back_to_back_tiles():
    # The first tile's meta tail pad must be flushed, not consumed by
    # the second tile's decoder.
    out = run_tiles([dict(k=64, rows=4), dict(k=96, rows=3)], seed=29)
    assert len(out['results']) == 2
    assert all(r['status'] is Status.OK for r in out['results'])

    a, b = out['results']
    assert b['pay_beats'] - a['pay_beats'] == 3 * 96 // 8
    assert b['meta_beats'] - a['meta_beats'] == -(-(3 * 3 * 2) // 8)
    assert b['slots'] - a['slots'] == 2 * 3
    check_records(out['records'], out['expected'])


def test_stream_mixed_format_back_to_back():
    # Format switches between tiles: each tile's window size, decode,
    # and meta consumption must reset cleanly, and the first tile's
    # meta tail pad must not leak into the Q5_0 decoder.
    params = params_for(Format.Q8_0, Format.Q5_0)
    out = run_tiles([
        dict(k=64, rows=4, fmt=Format.Q8_0),
        dict(k=96, rows=4, fmt=Format.Q5_0),
        dict(k=64, rows=2, fmt=Format.Q8_0),
    ],
                    params=params,
                    seed=29)
    assert len(out['results']) == 3
    assert all(r['status'] is Status.OK for r in out['results'])

    a, b, c = out['results']
    assert b['pay_beats'] - a['pay_beats'] == 4 * 3 * 20 // 8
    assert b['meta_beats'] - a['meta_beats'] == -(-(4 * 3 * 2) // 8)
    assert b['slots'] - a['slots'] == 2 * 3
    assert c['slots'] - b['slots'] == 1 * 2
    check_records(out['records'], out['expected'])


def test_stream_all_format_back_to_back():
    # All four formats in one chain: window size, chunk residency,
    # meta pitch, and decode must all reset cleanly per tile.
    params = params_for(*tuple(Format))
    out = run_tiles([
        dict(k=64, rows=4, fmt=Format.Q8_0),
        dict(k=96, rows=4, fmt=Format.Q5_0),
        dict(k=256, rows=4, fmt=Format.Q6_K),
        dict(k=256, rows=3, fmt=Format.Q4_K),
    ],
                    params=params,
                    seed=29)
    assert len(out['results']) == 4
    assert all(r['status'] is Status.OK for r in out['results'])
    check_records(out['records'], out['expected'])

    a, b, c, d = out['results']
    assert b['pay_beats'] - a['pay_beats'] == 4 * 3 * 20 // 8
    assert c['pay_beats'] - b['pay_beats'] == 4 * 1 * 192 // 8
    assert d['pay_beats'] - c['pay_beats'] == 3 * 1 * 128 // 8
    assert d['slots'] - c['slots'] == 2 * 8


def test_stream_rejects():
    params = params_for()
    rng = random.Random(31)
    k, rows = 64, 4
    tile = make_tile(rng, k, rows)
    image = place_tiles(params, [tile])

    cases = [
        (Format.Q5_0, 64, 4, P_BASE, M_BASE, Status.BAD_SHAPE),
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


def test_stream_q5_0_rejects():
    # Enabled but beat-indivisible payload (3 * 5 * 20 = 300 bytes)
    # and a misaligned meta base must both reject without traffic.
    params = params_for(Format.Q8_0, Format.Q5_0)
    rng = random.Random(37)
    tile = make_tile(rng, 64, 4, Format.Q8_0)
    image = place_tiles(params, [tile])

    cases = [
        (Format.Q5_0, 160, 3, P_BASE, M_BASE, Status.BAD_SHAPE),
        (Format.Q5_0, 160, 4, P_BASE, M_BASE + 4, Status.BAD_ALIGN),
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
        assert seen['status'] is want, (ck, crows, mb, seen['status'])


def test_stream_kq_rejects():
    # k must be a multiple of 256 for the super-block formats, a
    # disabled format is BAD_SHAPE, and bases must stay beat-aligned.
    rng = random.Random(47)
    all_params = params_for(*tuple(Format))
    q6_only = params_for(Format.Q6_K)
    tile = make_tile(rng, 256, 4, Format.Q6_K)
    image = place_tiles(all_params, [tile])

    cases = [
        (all_params, Format.Q6_K, 576, 4, P_BASE, M_BASE, Status.BAD_SHAPE),
        (q6_only, Format.Q4_K, 256, 4, P_BASE, M_BASE, Status.BAD_SHAPE),
        (all_params, Format.Q4_K, 256, 4, P_BASE + 4, M_BASE,
         Status.BAD_ALIGN),
    ]

    for params, fmt, ck, crows, pb, mb, want in cases:
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
        assert seen['status'] is want, (fmt.name, ck, crows, pb,
                                        seen['status'])


# ---------------------------------------------------------------------------
# Rate contract under an ideal memory.
# ---------------------------------------------------------------------------


def test_stream_rate_contract():
    k, rows = 64, 6
    out = run_stream(k, rows, latency=2)
    res = out['results'][-1]
    total = res['pay_beats'] + res['meta_beats']
    # The join must accept one beat per cycle up to fill latency and
    # queue turnaround; anything beyond that is an unexplained stall.
    assert res['run_cycles'] <= total + 24, res
    assert res['port_idle'] <= 12, res
    check_records(out['records'], out['expected'])


def test_stream_deployment_width():
    params = params_for(data_width=512)
    for k, rows in [(128, 6), (128, 5), (192, 2), (64, 2)]:
        out = run_stream(k, rows, params=params, seed=41)
        res = out['results'][-1]
        assert res['status'] is Status.OK
        check_records(out['records'], out['expected'])
        assert res['pay_beats'] == k * rows // 64
        assert res['slots'] == stream_pairs(rows) * (k // 32)
        # One beat is one slot pair: near the port rate.
        total = res['pay_beats'] + res['meta_beats']
        assert res['run_cycles'] <= total + 16, res


@pytest.mark.parametrize('k,rows', [
    (576, 8),
    (1024, 5),
    (128, 4),
])
def test_stream_q5_0_deployment_width(k, rows):
    # At 512 bits a Q5_0 beat carries 3.2 windows against the join's
    # two per cycle: the unit is join-bound at 0.625 beats/cycle (the
    # plan's lane-count limit), and the port must not be the limiter.
    # mm_out_depth=24 avoids the step-1 credit artifact (depth 16 vs
    # the 18-cycle MAC chain), isolating the structural rate.
    params = params_for(Format.Q8_0,
                        Format.Q5_0,
                        data_width=512,
                        mm_out_depth=24)
    out = run_stream(k, rows, params=params, fmt=Format.Q5_0, seed=41)
    res = out['results'][-1]
    assert res['status'] is Status.OK
    check_records(out['records'], out['expected'])

    nsegs = k // 32
    slots = stream_pairs(rows) * nsegs
    total = res['pay_beats'] + res['meta_beats']
    assert res['pay_beats'] == rows * nsegs * 20 // 64
    assert res['slots'] == slots
    assert res['credit'] == 0, res
    assert total < slots
    # The join's one-slot-per-cycle floor with fill/drain slack.
    assert slots <= res['run_cycles'] <= slots + 24, res


@pytest.mark.parametrize('fmt,k,rows', [
    (Format.Q6_K, 512, 8),
    (Format.Q6_K, 1024, 5),
    (Format.Q4_K, 512, 8),
    (Format.Q4_K, 1024, 5),
])
def test_stream_kq_deployment_width(fmt, k, rows):
    # Structural join bounds at 512 bits: a beat carries 2.67 Q6_K
    # (4 Q4_K) windows against the join's two per cycle, so the
    # ceilings are 0.75 (Q6_K) and 0.5 (Q4_K) beats/cycle -- the same
    # lane-count limit Q5_0 exposed, now from the chunked formats.
    # KQ payloads are always beat-divisible at this width (192 and
    # 128 are multiples of 64). mm_out_depth=24 isolates the
    # structural rate from the step-1 credit artifact.
    params = params_for(fmt, data_width=512, mm_out_depth=24)
    out = run_stream(k, rows, params=params, fmt=fmt, seed=41)
    res = out['results'][-1]
    assert res['status'] is Status.OK
    check_records(out['records'], out['expected'])
    check_kq_counters(out, fmt, k, rows)

    slots = res['slots']
    total = res['pay_beats'] + res['meta_beats']
    assert res['credit'] == 0, res
    assert total < slots
    assert slots <= res['run_cycles'] <= slots + 24, res


def test_stream_q4_k_large_tile_disjoint_regions():
    # The recorded 512-bit Q4_K 512x64 rate tile packs 16 KiB of
    # payload -- twice the old fixed P_BASE..M_BASE gap -- so its meta
    # region was laid over the payload's middle and record 256 decoded
    # zeros. place_tiles must size the regions from the actual tiles,
    # and an oversized tile's records must be checked, not just its
    # rates (this case ran for a day with no record verification).
    params = params_for(Format.Q4_K, data_width=512, mm_out_depth=24)
    out = run_stream(512, 64, params=params, fmt=Format.Q4_K, seed=101)
    assert out['results'][-1]['status'] is Status.OK
    check_records(out['records'], out['expected'])
    check_kq_counters(out, Format.Q4_K, 512, 64)


# ---------------------------------------------------------------------------
# UnitAssembler contract: golden-model phase coverage (both beat widths).
# ---------------------------------------------------------------------------

ASM_UB = 192  # unit frame covers every pitch
ASM_DEPTH = 8  # production depth at n_outputs=4 (group + margin)
ASM_SLOTS = 4  # production emission width at n_outputs=4
ASM_PITCHES = (20, 32, 128, 192)


def asm_reachable_phases(pitch, bb):
    """Byte phases reachable from 0 under continuous beat arrival."""
    seen = set()
    ph = 0
    while ph not in seen:
        seen.add(ph)
        ph = (ph + bb) % pitch
    return seen


def asm_golden_step(st, beat, beat_in, pitch, consume, bb, ub, depth, slots=2):
    """One cycle of the specified UnitAssembler semantics.

    ``st`` is (uf list, ucnt, part, ph) as ints. Returns (avail,
    can_take, slots tuple, next_st). Mirrors the module contract:
    whole-unit file + one partial unit + byte phase; the offered beat
    lands at ``ph``; completions bypass to the slots the same cycle;
    ``can_take`` counts the offered beat's completions ungated.
    """
    uf, ucnt, part, ph = st
    # The module's view is ungated: ``stream`` ORs the offered beat in
    # whether or not it is accepted, so the slots preview would-be
    # completions; only avail and the state transitions gate on
    # ``beat_in``. ``can_take`` counts the offered beat's completions
    # ungated (the parent checks it before asserting acceptance).
    stream = part | (beat << (8 * ph))
    tot = ph + bb
    cu_full = tot // pitch
    cu = cu_full if beat_in else 0
    avail = ucnt + cu
    can_take = (ucnt + cu_full) <= depth

    def comp(k):
        lo, hi = 8 * pitch * k, 8 * pitch * (k + 1)
        bits = (ub + bb) * 8
        if hi > bits:
            return 0
        return (stream >> lo) & ((1 << (hi - lo)) - 1)

    def visible(j):
        if j >= depth:
            return 0
        if ucnt > j:
            return uf[j]
        return comp(j - ucnt)

    slots_out = tuple(visible(j) for j in range(slots))
    part_next = part
    if beat_in:
        lo = 8 * pitch * cu
        bits = (ub + bb) * 8
        v = stream >> lo
        keep = min(max(0, bits - lo), ub * 8)
        part_next = v & ((1 << keep) - 1)
    uf2 = [visible(i + consume) for i in range(depth)]
    ucnt2 = ucnt + cu - consume
    ph2 = (tot if beat_in else ph) - cu * pitch
    return (avail, can_take, slots_out, (uf2, ucnt2, part_next, ph2))


def asm_bench(dw, pitch, seed, cycles=400, banked=False):
    """Drive one UnitAssembler against its golden model.

    pysim clock discipline: drive inputs, naked-yield (they become this
    cycle's inputs; state still holds the last transition's result),
    then read the settled combinational outputs and compare with the
    model evaluated on the same (state, inputs). No internal poking:
    phases are reached by feeding beats, so only reachable states are
    ever checked.
    """
    from troom.stream import UnitAssembler

    bb = dw // 8
    ub, depth, slots = ASM_UB, ASM_DEPTH, ASM_SLOTS
    dut = UnitAssembler(beat_bytes=bb,
                        unit_bytes=ub,
                        depth=depth,
                        pitches=ASM_PITCHES,
                        slots=slots,
                        banked_slots=banked)
    rng = random.Random(seed)
    init = ([0] * depth, 0, 0, 0)
    state = {'st': init, 'ph_seen': set()}

    def run():
        yield dut.pitch.eq(pitch)
        yield dut.beat.eq(0)
        yield dut.beat_in.eq(0)
        yield dut.consume.eq(0)
        yield dut.clr.eq(1)
        yield
        yield dut.clr.eq(0)
        yield
        st = init
        for _ in range(cycles):
            offer = rng.getrandbits(dw)
            probe = asm_golden_step(st, offer, True, pitch, 0, bb, ub, depth)
            take = probe[1] if rng.random() < 0.9 else False
            base = asm_golden_step(st, offer, take, pitch, 0, bb, ub, depth)
            consume = rng.randint(0, min(base[0], slots))
            e = asm_golden_step(st, offer, take, pitch, consume, bb, ub, depth,
                                slots)
            yield dut.beat.eq(offer)
            yield dut.beat_in.eq(1 if take else 0)
            yield dut.consume.eq(consume)
            yield  # inputs now active for this cycle; state == st
            slots_got = []
            for v in range(slots):
                slots_got.append((yield dut.slot[v]))
            got = ((yield dut.avail), (yield dut.can_take), tuple(slots_got))
            expected = (e[0], e[1], e[2])
            if banked:
                # Bank order rotates only after the decoder in the
                # production path. Compare the visible logical units
                # against the unconstrained golden file, including
                # arbitrary heads/consumes and same-cycle bypasses.
                rotation = (yield dut.rotation)
                valid = min(got[0], slots)
                got = (got[0], got[1],
                       tuple(slots_got[(rotation + v) % slots]
                             for v in range(valid)))
                expected = (e[0], e[1], e[2][:valid])
            assert got == expected, ('mismatch', dw, pitch, st[1], st[3], take,
                                     consume, got, (e[0], e[1], e[2]))
            st = e[3]
            state['st'] = st
            state['ph_seen'].add(st[3])
            assert 0 <= st[1] <= depth
            assert 0 <= st[3] < pitch, ('phase escape', dw, pitch, st)

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(run)
    sim.run()
    return state


@pytest.mark.parametrize('pitch', ASM_PITCHES)
@pytest.mark.parametrize('dw', [64, 512])
@pytest.mark.parametrize('banked', [False, True])
def test_unit_assembler_random_soak(dw, pitch, banked):
    # Random beats gated by can_take and random consumes at every
    # width/pitch pair; the golden model checks avail, can_take, both
    # slots (same-cycle completion bypass) and the phase invariant
    # every cycle, and every phase reachable by continuous arrival
    # must actually be visited (the step-4 division removal must
    # preserve each one; Q5_0 at the wide beat reaches four phases,
    # including the 4-completions corner at ph 16 -> 80 = 4 x 20).
    bb = dw // 8
    state = asm_bench(dw,
                      pitch,
                      seed=(dw * 131 + pitch) & 0xFFFF,
                      banked=banked)
    want = asm_reachable_phases(pitch, bb)
    assert state['ph_seen'] >= want, (dw, pitch,
                                      sorted(want - state['ph_seen']))


@pytest.mark.parametrize('dw', [64, 512])
def test_unit_assembler_four_completions(dw):
    # Directed at the wide-beat Q5_0 corner that the 2-bit truncation
    # bug once zeroed: land on ph 16 and take one 64-byte beat --
    # 80 = 4 x 20 completes four units in a single cycle, so avail
    # must report 4 and slot0/slot1 only the first two.
    pitch, bb = 20, dw // 8
    state = asm_bench(dw, pitch, seed=91, cycles=120)
    assert state['ph_seen'] >= asm_reachable_phases(pitch, bb)


# ---------------------------------------------------------------------------
# Full replacement chain through the unchanged epilogue.
# ---------------------------------------------------------------------------


@pytest.mark.parametrize('fmt,k', [
    (Format.Q8_0, 128),
    (Format.Q5_0, 128),
    (Format.Q6_K, 256),
    (Format.Q4_K, 256),
])
def test_stream_with_epilogue_bit_exact(fmt, k):
    rows = 5
    rng = random.Random(43)
    params = params_for(fmt)
    tile = make_tile(rng, k, rows, fmt)
    want = mm_row_results(tile['exp'])
    image = place_tiles(params, [tile])

    r_base = 0x80
    dut = EpilogueHarness(params)
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


@pytest.mark.parametrize('pay_depth,meta_depth,credits,burst', [
    (1, 1, 1, 1),
    (3, 2, 1, 256),
    (3, 3, 5, 256),
    (5, 3, 3, 3),
])
def test_stream_registered_reservations(pay_depth, meta_depth, credits, burst):
    # Exercise tails, non-power-of-two capacities, queued requests stalled
    # while old responses land, and meta-padding flush across a format change.
    params = params_for(Format.Q5_0,
                        Format.Q4_K,
                        sg_pay_fifo=pay_depth,
                        sg_meta_fifo=meta_depth,
                        sg_credits=credits,
                        sg_burst_beats=burst)
    out = run_tiles([
        dict(k=64, rows=4, fmt=Format.Q5_0),
        dict(k=256, rows=3, fmt=Format.Q4_K)
    ],
                    params=params,
                    latency=5,
                    req_prob=0.35,
                    stall_sink=True,
                    seed=72)
    assert all(r['status'] is Status.OK for r in out['results'])
    check_records(out['records'], out['expected'])
