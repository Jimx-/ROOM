"""Focused pysim tests for the troom matrix unit.

The reference side is ``tests/troom/model.py``'s MATMUL contract: the
model packs weight rows and activations, computes the expected
chain-end records (integer combines with the plan's bounds), and these
tests run the RTL matrix unit against them field for field, in launch
order. Coverage per the plan's verification obligations: every format's
unpack algebra, skew (distinct per-element sentinels), bubbles within
rows and back-to-back row pairs from randomized TMEM-request gating,
output backpressure longer than queue plus pipeline capacity, odd
output counts with a masked lane that never reads beyond its tile,
short-K many-rows-in-flight, ACT pinning across back-to-back matmuls,
and capability rejection of unsupported shapes.
"""

from amaranth import *
from amaranth.sim import Passive, Simulator

import pytest

from troom.consts import Format, Status
from troom.matrix import MatrixUnit
from troom.tmem import TMEM

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


class MatrixHarness(Elaboratable):
    """Matrix unit attached to a banked TMEM with a gate on requests.

    The gate lets a test process stall the weight feeder's read
    requests cycle by cycle, forcing starvation bubbles independently
    of TMEM behavior.
    """

    def __init__(self, params):
        self.params = params
        self.tmem = TMEM(params)
        self.matrix = MatrixUnit(params)
        self.allow = Signal(reset=1)

    def elaborate(self, platform):
        m = Module()
        m.submodules.tmem = self.tmem
        m.submodules.matrix = self.matrix

        rreq = self.matrix.tmem_rreq
        m.d.comb += [
            self.tmem.rreq.valid.eq(rreq.valid & self.allow),
            self.tmem.rreq.bits.addr.eq(rreq.bits.addr),
            rreq.ready.eq(self.tmem.rreq.ready & self.allow),
            self.tmem.rresp.connect(self.matrix.tmem_rresp),
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
    yield


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


def push_desc(unit, fmt, k, rows, stride):
    sink = unit.sink
    yield sink.bits.fmt.eq(int(fmt))
    yield sink.bits.k.eq(k)
    yield sink.bits.rows.eq(rows)
    yield sink.bits.w_base.eq(W_BASE)
    yield sink.bits.w_stride.eq(stride)
    yield sink.valid.eq(1)
    yield
    while not (yield sink.fire):
        yield
    yield sink.valid.eq(0)
    yield


def row_beats(data, base, beat=BEAT):
    """(address, beat word) pairs for beat-padded row bytes."""
    pad = (-len(data)) % beat
    data = data + bytes(pad)
    for off in range(0, len(data), beat):
        yield base + off, int.from_bytes(data[off:off + beat], 'little')


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


def build_tile(fmt, k, rows, wsegs, beat=BEAT):
    """Beat image and stride for a row-major tile at W_BASE."""
    nsegs = k // 32
    stride = -(-row_bytes(fmt, k) // beat) * beat
    beats = {}
    for r in range(rows):
        data = pack_row(fmt, wsegs[r * nsegs:(r + 1) * nsegs])
        for addr, word in row_beats(data, W_BASE + r * stride, beat):
            beats[addr] = word
    return stride, beats


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


def run_matmul(fmt,
               k,
               rows,
               wsegs,
               act,
               *,
               seed=7,
               stall_rreq=False,
               stall_sink=False,
               deadline=300000,
               params=None):
    """Run one matmul in the harness; return (records, status, extras)."""
    import random

    rng = random.Random(seed)
    params = params if params is not None else params_for(fmt)
    stride, beats = build_tile(fmt,
                               k,
                               rows,
                               wsegs,
                               beat=params['data_width'] // 8)
    act = [dict(entry, sum_a=sum(entry['quants'])) for entry in act]
    expected = mm_segments(fmt, k, rows, [
        pack_row(fmt, wsegs[r * (k // 32):(r + 1) * (k // 32)])
        for r in range(rows)
    ], act)

    dut = MatrixHarness(params)
    records = []
    req_addrs = []
    result = {}

    def setup():
        for addr in sorted(beats):
            yield from tmem_write(dut.tmem.write, addr, beats[addr])
        for j, entry in enumerate(act):
            yield from act_write(dut.matrix, j, entry)
        yield from push_desc(dut.matrix, fmt, k, rows, stride)
        for _ in range(deadline):
            yield
            if (yield dut.matrix.done):
                result['status'] = Status((yield dut.matrix.status))
                result['slots'] = (yield dut.matrix.slots)
                result['bubbles'] = (yield dut.matrix.bubbles)
                result['starve'] = (yield dut.matrix.starve_cycles)
                result['credit'] = (yield dut.matrix.credit_stalls)
                result['macs'] = (yield dut.matrix.useful_macs)
                break
        else:
            raise TimeoutError('matrix unit did not finish')

    def gate():
        yield Passive()
        while True:
            if stall_rreq:
                yield dut.allow.eq(rng.random() < 0.55)
            else:
                yield dut.allow.eq(1)
            if (yield dut.matrix.tmem_rreq.fire):
                req_addrs.append((yield dut.matrix.tmem_rreq.bits.addr))
            yield

    def sink():
        yield Passive()
        deq = dut.matrix.segs
        while True:
            if stall_sink:
                yield deq.ready.eq(rng.random() < 0.45)
            else:
                yield deq.ready.eq(1)
            if (yield deq.valid) & (yield deq.ready):
                rec = {}
                for f in RECORD_FIELDS:
                    rec[f] = (yield getattr(deq.bits, f))
                records.append(rec)
            yield

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(setup)
    sim.add_sync_process(gate)
    sim.add_sync_process(sink)
    sim.run()

    return dict(records=records,
                expected=expected,
                status=result['status'],
                stride=stride,
                req_addrs=req_addrs,
                slots=result.get('slots'),
                bubbles=result.get('bubbles'),
                starve=result.get('starve'),
                credit=result.get('credit'),
                macs=result.get('macs'))


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


def run_format_case(fmt, k, rows, seed):
    import random

    rng = random.Random(seed)
    wsegs = gen_wsegs(rng, fmt, rows * (k // 32))
    act = gen_act(rng, k // 32)
    out = run_matmul(fmt, k, rows, wsegs, act, seed=seed)
    assert out['status'] is Status.OK
    assert out['macs'] == rows * k
    assert out['slots'] == ((rows + 1) // 2) * (k // 32)
    check_records(out['records'], out['expected'])


def test_matrix_q5_0_records():
    # 3 blocks per row (66 bytes: beats straddle block boundaries) and
    # an odd row count exercising the masked final lane.
    run_format_case(Format.Q5_0, 96, 3, 11)


def test_matrix_q8_0_records():
    run_format_case(Format.Q8_0, 64, 2, 12)


def test_matrix_q4_k_records():
    # Two super-blocks per row: block-to-block transitions through the
    # ping/pong buffers, five rows with the last lane masked.
    run_format_case(Format.Q4_K, 512, 5, 13)


def test_matrix_q6_k_records():
    run_format_case(Format.Q6_K, 256, 3, 14)


@pytest.mark.parametrize('fmt,k', [
    (Format.Q5_0, 576),
    (Format.Q8_0, 576),
    (Format.Q4_K, 1536),
    (Format.Q6_K, 1536),
])
@pytest.mark.parametrize('stalls', [False, True])
def test_matrix_wide_block_tails(fmt, k, stalls):
    """Wide beats span blocks; tails respect ping/pong ownership and padding."""
    import random

    rng = random.Random(84)
    rows = 5  # Multiple row transitions and a masked final output lane.
    wsegs = gen_wsegs(rng, fmt, rows * (k // 32))
    act = gen_act(rng, k // 32)
    params = params_for(fmt,
                        mm_formats=tuple(Format),
                        data_width=512,
                        id_width=6,
                        n_tmem_banks=8,
                        tmem_bytes=256 * 1024,
                        act_max_k=2048,
                        mm_out_depth=1 if stalls else 16)
    out = run_matmul(fmt,
                     k,
                     rows,
                     wsegs,
                     act,
                     params=params,
                     stall_rreq=stalls,
                     stall_sink=stalls)
    assert out['status'] is Status.OK
    check_records(out['records'], out['expected'])
    assert out['macs'] == rows * k
    expected_reads = [
        W_BASE + r * out['stride'] + off for r in range(rows)
        for off in range(0, out['stride'], 64)
    ]
    assert sorted(out['req_addrs']) == expected_reads
    if stalls:
        assert out['credit'] > 0  # Force decode/fill to wait for buffer reuse.


def test_matrix_wide_q8_supply_throughput():
    """Prevent a return to byte-at-a-time assembly (3,476 starved cycles)."""
    import random

    rng = random.Random(84)
    fmt, k, rows = Format.Q8_0, 576, 16
    wsegs = gen_wsegs(rng, fmt, rows * (k // 32))
    act = gen_act(rng, k // 32)
    params = params_for(fmt,
                        data_width=512,
                        id_width=6,
                        n_tmem_banks=8,
                        tmem_bytes=256 * 1024,
                        act_max_k=2048)
    out = run_matmul(fmt, k, rows, wsegs, act, params=params)
    assert out['status'] is Status.OK
    check_records(out['records'], out['expected'])
    assert out['slots'] == 144
    assert out['macs'] == 9216
    # Leave margin for control changes while requiring a substantial gain.
    assert out['slots'] + out['bubbles'] < 1000


def test_matrix_extremes_hit_bounds():
    """Extremes of every operand range, checked against the bounds."""
    import random

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
    out = run_matmul(Format.Q4_K, 256, 2, wsegs, act, seed=15)
    assert out['status'] is Status.OK
    check_records(out['records'], out['expected'])
    for rec in out['records']:
        assert rec['S0'] == -SEGMENT_BOUND[Format.Q4_K]

    wsegs = [
        dict(w=[-32] * 32, d=0x3C00, dmin=0, mi=0, sc0=-128, sc1=-128)
        for _ in range(16)
    ]
    out = run_matmul(Format.Q6_K, 256, 2, wsegs, act, seed=16)
    assert out['status'] is Status.OK
    check_records(out['records'], out['expected'])
    for rec in out['records']:
        assert rec['S0'] == -SEGMENT_BOUND[Format.Q6_K]


def test_matrix_stalls_bubbles_and_backpressure():
    """Randomized request gating and sink backpressure.

    The gated feeder must insert invalid slots without losing, duping,
    or reordering records, and every TMEM request must stay inside the
    tile (the masked lane never reads beyond it). The counters must
    account every record as a valid slot of useful MACs and every other
    run cycle as a bubble.
    """
    import random

    fmt, k, rows = Format.Q6_K, 512, 5
    rng = random.Random(21)
    wsegs = gen_wsegs(rng, fmt, rows * (k // 32))
    act = gen_act(rng, k // 32)
    out = run_matmul(fmt,
                     k,
                     rows,
                     wsegs,
                     act,
                     seed=21,
                     stall_rreq=True,
                     stall_sink=True)
    assert out['status'] is Status.OK
    check_records(out['records'], out['expected'])
    end = W_BASE + rows * out['stride']
    for addr in out['req_addrs']:
        assert W_BASE <= addr < end, hex(addr)

    # Slot and MAC accounting: one valid slot per record, 32 useful
    # element products per live pipeline, and the stall run must have
    # needed starvation bubbles (credit stalls are exercised by the
    # long-sink-stall test).
    macs = sum(32 * bin(rec['row_mask']).count('1') for rec in out['records'])
    assert out['slots'] == len(out['records'])
    assert out['macs'] == macs
    assert out['bubbles'] > 0
    assert out['starve'] + out['credit'] == out['bubbles']
    assert out['starve'] > 0


def test_matrix_long_sink_stall():
    """A sink stalled far longer than queue plus pipeline capacity.

    Output credit must throttle launch (not overflow or drop records)
    and done must still mean the final record was dequeued.
    """
    import random

    fmt, k, rows = Format.Q4_K, 256, 5
    rng = random.Random(77)
    wsegs = gen_wsegs(rng, fmt, rows * (k // 32))
    act = gen_act(rng, k // 32)
    act = [dict(e, sum_a=sum(e['quants'])) for e in act]
    stride, beats = build_tile(fmt, k, rows, wsegs)
    expected = mm_segments(fmt, k, rows, [
        pack_row(fmt, wsegs[r * (k // 32):(r + 1) * (k // 32)])
        for r in range(rows)
    ], act)

    dut = MatrixHarness(params_for(fmt))
    records = []
    result = {}

    def setup():
        for addr in sorted(beats):
            yield from tmem_write(dut.tmem.write, addr, beats[addr])
        for j, entry in enumerate(act):
            yield from act_write(dut.matrix, j, entry)
        yield from push_desc(dut.matrix, fmt, k, rows, stride)
        for _ in range(300000):
            yield
            if (yield dut.matrix.done):
                result['status'] = Status((yield dut.matrix.status))
                result['bubbles'] = (yield dut.matrix.bubbles)
                result['credit'] = (yield dut.matrix.credit_stalls)
                break
        else:
            raise TimeoutError('matrix unit did not finish')

    def sink():
        yield Passive()
        deq = dut.matrix.segs
        # Hold ready low far longer than the 16-entry output queue plus
        # the 18-cycle record pipeline, then drain everything.
        for _ in range(400):
            yield deq.ready.eq(0)
            yield
        while True:
            yield deq.ready.eq(1)
            if (yield deq.valid) & (yield deq.ready):
                rec = {}
                for f in RECORD_FIELDS:
                    rec[f] = (yield getattr(deq.bits, f))
                records.append(rec)
            yield

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(setup)
    sim.add_sync_process(sink)
    sim.run()

    assert result['status'] is Status.OK
    assert result['credit'] > 0
    check_records(records, expected)


def test_matrix_short_k_many_rows():
    """Seventeen rows of one segment each: many rows in flight."""
    import random

    fmt, k, rows = Format.Q8_0, 32, 17
    rng = random.Random(31)
    wsegs = gen_wsegs(rng, fmt, rows * (k // 32))
    act = gen_act(rng, 1)
    out = run_matmul(fmt, k, rows, wsegs, act, seed=31)
    assert out['status'] is Status.OK
    check_records(out['records'], out['expected'])


def test_matrix_q8_0_long_k():
    """A real projection width: K = 576."""
    import random

    fmt, k, rows = Format.Q8_0, 576, 3
    rng = random.Random(41)
    wsegs = gen_wsegs(rng, fmt, rows * (k // 32))
    vec = [rng.uniform(-1, 1) for _ in range(k)]
    act = build_act(fmt, vec)
    out = run_matmul(fmt, k, rows, wsegs, act, seed=41)
    assert out['status'] is Status.OK
    check_records(out['records'], out['expected'])
    results = mm_row_results(out['records'])
    assert len(results) == rows
    assert all(r == r and abs(r) < 1e38 for r in results.values())


def test_matrix_back_to_back_and_act_pin():
    """Two matmuls with an ACT rewrite between; writes pin during run."""
    import random

    fmt, k, rows = Format.Q5_0, 64, 2
    rng = random.Random(51)
    wsegs = gen_wsegs(rng, fmt, rows * (k // 32))
    wsegs2 = gen_wsegs(rng, fmt, rows * (k // 32))
    act = gen_act(rng, k // 32)
    act2 = gen_act(rng, k // 32)

    stride, beats = build_tile(fmt, k, rows, wsegs)
    for r in range(rows):
        data = pack_row(fmt, wsegs2[r * (k // 32):(r + 1) * (k // 32)])
        for addr, word in row_beats(data, W_BASE + rows * stride + r * stride):
            beats[addr] = word

    dut = MatrixHarness(params_for(fmt))
    act = [dict(e, sum_a=sum(e['quants'])) for e in act]
    act2 = [dict(e, sum_a=sum(e['quants'])) for e in act2]
    records = [[], []]
    expected = [
        mm_segments(
            fmt, k, rows,
            [pack_row(fmt, wsegs[r * 2:(r + 1) * 2])
             for r in range(rows)], act),
        mm_segments(
            fmt, k, rows,
            [pack_row(fmt, wsegs2[r * 2:(r + 1) * 2])
             for r in range(rows)], act2),
    ]
    result = {}

    def setup():
        for addr in sorted(beats):
            yield from tmem_write(dut.tmem.write, addr, beats[addr])
        for j, entry in enumerate(act):
            yield from act_write(dut.matrix, j, entry)
        yield from push_desc(dut.matrix, fmt, k, rows, stride)
        for _ in range(300000):
            yield
            if (yield dut.matrix.done):
                break
        else:
            raise TimeoutError('first matmul did not finish')

        # The unit must be idle again: ACT writes unblocked, a second
        # descriptor accepted against a different tile and activations.
        yield dut.matrix.act_wr.bits.seg.eq(0)
        yield dut.matrix.act_wr.valid.eq(1)
        yield
        assert (yield dut.matrix.act_wr.ready)
        yield dut.matrix.act_wr.valid.eq(0)
        yield
        for j, entry in enumerate(act2):
            yield from act_write(dut.matrix, j, entry)
        yield from push_desc2(fmt, k, rows, stride, W_BASE + rows * stride)
        for _ in range(300000):
            yield
            if (yield dut.matrix.done):
                result['status'] = Status((yield dut.matrix.status))
                break
        else:
            raise TimeoutError('second matmul did not finish')

    def push_desc2(fmt_, k_, rows_, stride_, base):
        sink = dut.matrix.sink
        yield sink.bits.fmt.eq(int(fmt_))
        yield sink.bits.k.eq(k_)
        yield sink.bits.rows.eq(rows_)
        yield sink.bits.w_base.eq(base)
        yield sink.bits.w_stride.eq(stride_)
        yield sink.valid.eq(1)
        yield
        while not (yield sink.fire):
            yield
        yield sink.valid.eq(0)
        yield

    def sink():
        yield Passive()
        deq = dut.matrix.segs
        phase = 0
        while True:
            yield deq.ready.eq(1)
            if (yield deq.valid) & (yield deq.ready):
                rec = {}
                for f in RECORD_FIELDS:
                    rec[f] = (yield getattr(deq.bits, f))
                records[phase].append(rec)
            yield
            if (yield dut.matrix.done) and phase == 0:
                phase = 1

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(setup)
    sim.add_sync_process(sink)
    sim.run()

    assert result['status'] is Status.OK
    check_records(records[0], expected[0])
    check_records(records[1], expected[1])


def test_matrix_act_pinned_during_run():
    """ACT writes are back-pressured from accept to drain."""
    import random

    fmt, k, rows = Format.Q6_K, 256, 2
    rng = random.Random(61)
    wsegs = gen_wsegs(rng, fmt, rows * (k // 32))
    act = gen_act(rng, k // 32)
    act = [dict(e, sum_a=sum(e['quants'])) for e in act]
    stride, beats = build_tile(fmt, k, rows, wsegs)

    dut = MatrixHarness(params_for(fmt))
    seen = {}

    def setup():
        for addr in sorted(beats):
            yield from tmem_write(dut.tmem.write, addr, beats[addr])
        for j, entry in enumerate(act):
            yield from act_write(dut.matrix, j, entry)
        yield from push_desc(dut.matrix, fmt, k, rows, stride)
        for _ in range(64):
            yield
            if (yield dut.matrix.active):
                break
        else:
            raise AssertionError('unit never became active')
        wr = dut.matrix.act_wr
        yield wr.bits.seg.eq(0)
        yield wr.valid.eq(1)
        for _ in range(32):
            yield
            assert (yield dut.matrix.active)
            assert not (yield wr.ready)
        yield wr.valid.eq(0)
        seen['pinned'] = True
        for _ in range(300000):
            yield
            if (yield dut.matrix.done):
                break
        else:
            raise TimeoutError('matmul did not finish')
        # Idle again: the write goes through now.
        yield wr.valid.eq(1)
        yield
        assert (yield wr.ready)
        yield wr.valid.eq(0)

    def sink():
        yield Passive()
        deq = dut.matrix.segs
        while True:
            yield deq.ready.eq(1)
            yield

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(setup)
    sim.add_sync_process(sink)
    sim.run()
    assert seen['pinned']


@pytest.mark.parametrize('fmt,k,rows,stride,why', [
    (Format.Q5_0, 33, 2, 64, 'k not a multiple of 32'),
    (Format.Q5_0, 576, 0, 528, 'no rows'),
    (Format.Q5_0, 2048, 2, 64, 'k beyond act_max_k'),
    (Format.Q4_K, 32, 2, 64, 'k not a multiple of 256 for q4_k'),
    (Format.Q5_0, 64, 2, 63, 'stride not beat aligned'),
    (Format.Q5_0, 64, 2, 40, 'stride smaller than the packed row'),
    (Format.Q6_K, 256, 2, 224, 'format not configured'),
])
def test_matrix_rejects_bad_shapes(fmt, k, rows, stride, why):
    # The unconfigured-format case runs with only Q5_0 decoders.
    mm_formats = (Format.Q5_0, ) if why == 'format not configured' else None

    dut = MatrixHarness(params_for(fmt, mm_formats=mm_formats))
    result = {}

    def setup():
        yield from push_desc(dut.matrix, fmt, k, rows, stride)
        for _ in range(64):
            yield
            if 'status' in result:
                return
        raise TimeoutError(f'no completion for {why}')

    def sink():
        yield Passive()
        deq = dut.matrix.segs
        while True:
            yield deq.ready.eq(1)
            assert not (yield deq.valid), 'rejected shape produced records'
            # A rejected descriptor neither starts the feeder nor may
            # consume shared TMEM read service with stale cursors.
            assert not (yield dut.matrix.tmem_rreq.valid), \
                f'{why}: weight read after rejection'
            assert not (yield dut.matrix.active), \
                f'{why}: active after rejection'
            if (yield dut.matrix.done):
                result['status'] = Status((yield dut.matrix.status))
            yield

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(setup)
    sim.add_sync_process(sink)
    sim.run()
    assert result['status'] is Status.BAD_SHAPE, why


def test_matrix_max_rows_then_fresh_tile():
    """A maximum-row-count tile must not wrap the fill cursor.

    With ``rows_bits`` narrowed to 8, a full 255-row tile walks
    pipeline 0's row cursor through row 254 to 256; wrapping there
    would re-read row 0 and queue its segment for the next command,
    whose first record then carries a foreign partial. The follow-up
    tile must complete with only its own records queued.
    """
    import random

    fmt = Format.Q8_0
    k = 32
    row_bits = 8
    rng = random.Random(321)
    act = [dict(e, sum_a=sum(e['quants'])) for e in gen_act(rng, k // 32)]

    tiles = []
    expected = []
    for rows in ((1 << row_bits) - 1, 2):
        wsegs = gen_wsegs(rng, fmt, rows)
        stride, beats = build_tile(fmt, k, rows, wsegs)
        tiles.append((rows, stride, beats))
        expected.append(
            mm_segments(fmt, k, rows,
                        [pack_row(fmt, wsegs[r:r + 1]) for r in range(rows)],
                        act))

    dut = MatrixHarness(params_for(fmt, rows_bits=row_bits))
    records = []
    result = {}

    def setup():
        for i, entry in enumerate(act):
            yield from act_write(dut.matrix, i, entry)
        for rows, stride, beats in tiles:
            for addr in sorted(beats):
                yield from tmem_write(dut.tmem.write, addr, beats[addr])
            yield from push_desc(dut.matrix, fmt, k, rows, stride)
            for _ in range(20000):
                yield
                if (yield dut.matrix.done):
                    assert (yield dut.matrix.status) == Status.OK
                    # No segment may linger for the next command.
                    for p in range(2):
                        assert (yield dut.matrix.supply.seg_q[p].count) \
                            == 0, f'{rows} rows: pipeline {p} queued'
                    break
            else:
                raise TimeoutError(f'{rows}-row matmul stalled')
            result.setdefault('done', []).append(rows)
            yield

    def sink():
        yield Passive()
        deq = dut.matrix.segs
        while True:
            yield deq.ready.eq(1)
            if (yield deq.fire):
                rec = {}
                for f in RECORD_FIELDS:
                    rec[f] = (yield getattr(deq.bits, f))
                records.append(rec)
            yield

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(setup)
    sim.add_sync_process(sink)
    sim.run()
    assert result['done'] == [255, 2]
    n0 = len(expected[0])
    check_records(records[:n0], expected[0])
    check_records(records[n0:], expected[1])
    assert len(records) == n0 + len(expected[1])


def test_matrix_reject_then_valid_recovers():
    """A rejected descriptor leaves no configuration or traffic behind.

    The bad shape (K not a multiple of 32) is followed by forty idle
    cycles with no weight reads, then the valid tile must complete
    against the reference records with untouched counters.
    """
    import random

    fmt = Format.Q8_0
    k, rows = 64, 2
    rng = random.Random(91)
    wsegs = gen_wsegs(rng, fmt, rows * (k // 32))
    act = [dict(e, sum_a=sum(e['quants'])) for e in gen_act(rng, k // 32)]
    stride, beats = build_tile(fmt, k, rows, wsegs)
    expected = mm_segments(fmt, k, rows, [
        pack_row(fmt, wsegs[r * (k // 32):(r + 1) * (k // 32)])
        for r in range(rows)
    ], act)

    dut = MatrixHarness(params_for(fmt))
    records = []
    result = {}

    def setup():
        for addr in sorted(beats):
            yield from tmem_write(dut.tmem.write, addr, beats[addr])
        for j, entry in enumerate(act):
            yield from act_write(dut.matrix, j, entry)

        yield from push_desc(dut.matrix, fmt, 33, rows, stride)
        for _ in range(40):
            yield
            assert not (yield dut.matrix.tmem_rreq.valid), \
                'weight read after rejection'
            assert not (yield dut.matrix.active)

        yield from push_desc(dut.matrix, fmt, k, rows, stride)
        for _ in range(20000):
            yield
            if (yield dut.matrix.done):
                result['status'] = Status((yield dut.matrix.status))
                result['macs'] = (yield dut.matrix.useful_macs)
                return
        raise TimeoutError('valid matmul after rejection did not finish')

    def sink():
        yield Passive()
        deq = dut.matrix.segs
        while True:
            yield deq.ready.eq(1)
            if (yield deq.fire):
                rec = {}
                for f in RECORD_FIELDS:
                    rec[f] = (yield getattr(deq.bits, f))
                records.append(rec)
            yield

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(setup)
    sim.add_sync_process(sink)
    sim.run()
    assert result['status'] is Status.OK
    check_records(records, expected)
    assert result['macs'] == rows * k


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
