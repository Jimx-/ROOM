"""P2a split-TMEM-cluster tests (2026-11-03).

The resource-contract slice 2 of the pipelined-batch-decode plan:
TMEM halves into two independent clusters (top word bit selects),
each with its own write port and read channel, so a dataflow class
placed in the upper half (the KV maintenance staging) runs off the
lower half's ports entirely. These tests pin:

* the unit-level dual-cluster contract: independent port pairs,
  per-cluster conflict backpressure, boundary-straddling coherence,
  summed counters, and the ``tmem_clusters=1`` historical pin,
* the core level: a low-half streaming chain with a top-half chain's
  loads, staged SQUANTIZE and stores interleaved under it -- the
  maintenance-splice concurrency shape -- exact against the model on
  both halves, and a DMA region straddling the cluster boundary
  round-tripping byte-exact (the per-master read-order guard).
"""

import random

import pytest

from amaranth import *
from amaranth.sim import Simulator

from troom.consts import Format, Opcode, Status
from troom.tmem import TMEM
from troom.types import HasTRoomParams

from tests.troom.model import build_act, f32_bits, mm_row_results, \
    mm_segments, pack_row, row_bytes
from tests.troom.test_full_path import (
    DRAM_SIZE,
    M_DRAM,
    PARAMS,
    R_DRAM,
    A_DRAM,
    W_DRAM,
    WM_DRAM,
    run_program,
)
from tests.troom.test_matrix import gen_wsegs
from troom.stream import pack_stream_q8_0

HP = HasTRoomParams(PARAMS)
HALF = HP.tmem_bytes // 2


def run_streams(dut, *procs):
    sim = Simulator(dut)
    sim.add_clock(1e-6)
    for proc in procs:
        sim.add_sync_process(proc)
    sim.run()


def beat_value(addr):
    return ((addr // HP.beat_bytes) * 0x0001000100010001) & \
        ((1 << HP.data_width) - 1)


def tmem_write(write, addr, data):
    yield write.bits.addr.eq(addr)
    yield write.bits.data.eq(data)
    yield write.valid.eq(1)
    yield
    while not (yield write.fire):
        yield
    yield write.valid.eq(0)
    yield


def tmem_read(rreq, rresp, addr):
    yield rreq.bits.addr.eq(addr)
    yield rreq.valid.eq(1)
    yield
    while not (yield rreq.fire):
        yield
    yield rreq.valid.eq(0)
    yield
    while not (yield rresp.valid):
        yield
    data = (yield rresp.bits.data)
    yield rresp.ready.eq(1)
    yield
    yield rresp.ready.eq(0)
    yield
    return data


#
# Unit level: the dual-cluster port contract.
#


def test_split_roundtrip_both_clusters():
    dut = TMEM(PARAMS)
    # The partition is a routing contract (as in the core): a beat
    # goes through the port group its cluster names.
    low = [0, 8, HALF - 8]
    high = [HALF, HALF + 8, HP.tmem_bytes - 8]

    def proc():
        for addr in low:
            yield from tmem_write(dut.write, addr, beat_value(addr))
        for addr in high:
            yield from tmem_write(dut.write_b, addr, beat_value(addr))
        assert (yield dut.write_beats) == 6
        for addr in low:
            data = yield from tmem_read(dut.rreq, dut.rresp, addr)
            assert data == beat_value(addr), hex(addr)
        for addr in high:
            data = yield from tmem_read(dut.rreq_b, dut.rresp_b, addr)
            assert data == beat_value(addr), hex(addr)
        assert (yield dut.read_beats) == 6
        assert (yield dut.conflicts) == 0
        # The clusters split the beats evenly at these addresses.
        assert (yield dut.write_beats_a) == 3
        assert (yield dut.write_beats_b) == 3

    run_streams(dut, proc)


def test_split_ports_concurrent():
    """Both write ports commit a beat in the same cycle, and both read
    channels carry a request/response pair concurrently -- the port
    pair independence the slice exists for. One simulator run: pysim
    memories do not persist across Simulator instances."""
    dut = TMEM(PARAMS)

    def pa():
        yield dut.write.bits.addr.eq(0x40)
        yield dut.write.bits.data.eq(0xA1A1)
        yield dut.write.valid.eq(1)
        yield dut.write_b.bits.addr.eq(HALF + 0x40)
        yield dut.write_b.bits.data.eq(0xB2B2)
        yield dut.write_b.valid.eq(1)
        yield  # both ports fire in this cycle
        assert (yield dut.write.fire)
        assert (yield dut.write_b.fire)
        yield dut.write.valid.eq(0)
        yield dut.write_b.valid.eq(0)
        yield
        # Both reads present in the same cycle (the writes retired
        # last cycle; each cluster's slot is free).
        yield dut.rreq.bits.addr.eq(0x40)
        yield dut.rreq.valid.eq(1)
        yield dut.rreq_b.bits.addr.eq(HALF + 0x40)
        yield dut.rreq_b.valid.eq(1)
        yield
        assert (yield dut.rreq.fire)
        assert (yield dut.rreq_b.fire)
        yield dut.rreq.valid.eq(0)
        yield dut.rreq_b.valid.eq(0)
        yield
        while not (yield dut.rresp.valid):
            yield
        assert (yield dut.rresp.bits.data) == 0xA1A1
        while not (yield dut.rresp_b.valid):
            yield
        assert (yield dut.rresp_b.bits.data) == 0xB2B2
        yield dut.rresp.ready.eq(1)
        yield dut.rresp_b.ready.eq(1)
        yield

    run_streams(dut, pa)


def test_split_conflict_isolated_per_cluster():
    """A cluster-A same-bank read/write conflict back-pressures only
    cluster A's read; cluster B's read is accepted in that same cycle.

    Addresses 0 and 32 share bank 0 in cluster A; HALF+0 shares bank 0
    in cluster B.
    """
    dut = TMEM(PARAMS)

    def proc():
        yield from tmem_write(dut.write, 32, beat_value(32))
        for _ in range(2):
            yield
        # Present the bank-0 write and both reads together; the next
        # cycle is the conflict cycle.
        yield dut.write.bits.addr.eq(0)
        yield dut.write.bits.data.eq(0xDEADBEEF)
        yield dut.write.valid.eq(1)
        yield dut.rreq.bits.addr.eq(0)
        yield dut.rreq.valid.eq(1)
        yield dut.rreq_b.bits.addr.eq(HALF)
        yield dut.rreq_b.valid.eq(1)
        yield  # the write commits; A's read must stall, B's must go
        assert (yield dut.write.fire)
        assert not (yield dut.rreq.ready), 'A read accepted into a write'
        assert (yield dut.rreq_b.ready), 'B read held by an A conflict'
        yield dut.write.valid.eq(0)
        # ``fire`` is a one-cycle pulse: B is conflict-free and can
        # fire in the conflict cycle itself, so its wait comes first
        # (a fire waited for after the fact never comes). A fires
        # once the write retires; the responses are level-held.
        while not (yield dut.rreq_b.fire):
            yield
        yield dut.rreq_b.valid.eq(0)
        while not (yield dut.rreq.fire):
            yield
        yield dut.rreq.valid.eq(0)
        while not (yield dut.rresp.valid):
            yield
        assert (yield dut.rresp.bits.data) == 0xDEADBEEF
        while not (yield dut.rresp_b.valid):
            yield
        yield dut.rresp.ready.eq(1)
        yield dut.rresp_b.ready.eq(1)
        yield
        assert (yield dut.conflicts) == 1
        assert (yield dut.conflicts_a) == 1
        assert (yield dut.conflicts_b) == 0

    run_streams(dut, proc)


def test_split_boundary_straddle_unit():
    """A sequential beat run across the boundary lands in both clusters
    and reads back in order (each beat through the port its cluster
    names, the routing the core performs)."""
    dut = TMEM(PARAMS)
    start = HALF - 3 * HP.beat_bytes
    n = 6

    def proc():
        for i in range(n):
            addr = start + i * HP.beat_bytes
            port = dut.write if addr < HALF else dut.write_b
            yield from tmem_write(port, addr, beat_value(addr))
        for i in range(n):
            addr = start + i * HP.beat_bytes
            req, resp = ((dut.rreq, dut.rresp) if addr < HALF else
                         (dut.rreq_b, dut.rresp_b))
            data = yield from tmem_read(req, resp, addr)
            assert data == beat_value(addr), i
        assert (yield dut.write_beats_a) == 3
        assert (yield dut.write_beats_b) == 3

    run_streams(dut, proc)


def test_single_cluster_pin():
    """``tmem_clusters=1`` keeps the historical single-port-pair
    module: no second port group, full-depth banks, one counter set."""
    params = dict(PARAMS, tmem_clusters=1)
    dut = TMEM(params)
    assert not hasattr(dut, 'write_b')

    def proc():
        addrs = [0, 8, HALF, HALF + 8, params['tmem_bytes'] - 8]
        for addr in addrs:
            yield from tmem_write(dut.write, addr, beat_value(addr))
        for addr in addrs:
            data = yield from tmem_read(dut.rreq, dut.rresp, addr)
            assert data == beat_value(addr), hex(addr)
        assert (yield dut.write_beats) == len(addrs)

    run_streams(dut, proc)


def test_split_params_validation():
    with pytest.raises(ValueError):
        TMEM(dict(PARAMS, tmem_clusters=3))
    # bank_words == 1 cannot halve.
    with pytest.raises(ValueError):
        TMEM(dict(PARAMS, tmem_bytes=32, n_tmem_banks=4))


#
# Core level: the concurrency the splice needs, exact on both halves.
#


def tile_image(seed, k, rows, dram_off):
    """One Q8_0 tile's DRAM footprint at a payload/meta/activation
    offset, plus its model inputs."""
    rng = random.Random(seed)
    wsegs = gen_wsegs(rng, Format.Q8_0, rows * (k // 32))
    act = build_act(Format.Q8_0, [rng.uniform(-1, 1) for _ in range(k)])
    payload, meta = pack_stream_q8_0(wsegs, k, rows, HP.n_outputs)
    image = bytearray(DRAM_SIZE)
    image[W_DRAM + dram_off:W_DRAM + dram_off + len(payload)] = payload
    image[WM_DRAM + dram_off:WM_DRAM + dram_off + len(meta)] = meta
    qimg = bytes(q & 255 for seg in act for q in seg['quants'])
    mimg = b''.join(seg['s_act'].to_bytes(4, 'little') +
                    (seg['sum_a'] & 65535).to_bytes(2, 'little') + bytes(2)
                    for seg in act)
    qimg += bytes(-len(qimg) % HP.beat_bytes)
    mimg += bytes(-len(mimg) % HP.beat_bytes)
    image[A_DRAM + dram_off:A_DRAM + dram_off + len(qimg)] = qimg
    image[M_DRAM + dram_off:M_DRAM + dram_off + len(mimg)] = mimg
    rstride = -(-(rows * 4) // HP.beat_bytes) * HP.beat_bytes
    return image, wsegs, act, len(qimg), len(mimg), rstride


def check_tile(image, fmt, k, rows, wsegs, act, dram_out):
    exp_records = mm_segments(fmt, k, rows, [
        pack_row(fmt, wsegs[r * (k // 32):(r + 1) * (k // 32)])
        for r in range(rows)
    ], act)
    exp = mm_row_results(exp_records)
    for r in range(rows):
        off = dram_out + 4 * r
        got = int.from_bytes(image[off:off + 4], 'little')
        assert got == f32_bits(exp[r]), (r, hex(got), hex(f32_bits(exp[r])))


def test_cross_cluster_concurrent_chains():
    """A low-half chain with a top-half chain's loads, staged
    SQUANTIZE and stores interleaved under it -- the maintenance
    splice's concurrency shape (the top-half fills and store reads run
    on cluster B's ports while the low-half chain's stream, epilogue
    drain and store occupy cluster A). Both chains exact vs the
    model."""
    fmt = Format.Q8_0
    k, rows = 256, 4
    params = dict(PARAMS, mm_formats=(fmt, ))
    img_l, wsegs_l, act_l, ql, ml, rstride = tile_image(11, k, rows, 0x0)
    img_h, wsegs_h, act_h, qh, mh, _ = tile_image(23, k, rows, 0x1000)
    image = bytearray(img_l)
    for i, b in enumerate(img_h):
        if b:
            image[i] = b

    # TMEM bases: chain L in cluster A, chain H in cluster B.
    la, lm, lr = 0x0000, 0x0400, 0x0800
    ha, hm, hr = HALF + 0x0000, HALF + 0x0400, HALF + 0x0800

    program = [
        dict(opcode=Opcode.LOAD_2D,
             seq=1,
             evt=2,
             dram_addr=A_DRAM,
             tmem_addr=la,
             row_bytes=ql,
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.LOAD_2D,
             seq=2,
             evt=3,
             dram_addr=M_DRAM,
             tmem_addr=lm,
             row_bytes=ml,
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.WAIT, seq=3, evt=2),
        dict(opcode=Opcode.WAIT, seq=4, evt=3),
        dict(opcode=Opcode.SQUANTIZE, seq=5, evt=4, a_base=la, m_base=lm, k=k),
        dict(opcode=Opcode.WAIT, seq=6, evt=4),
        # SMATMUL L issued without waiting: the top-half activation
        # fills and their staged SQUANTIZE ride under the stream run
        # and the epilogue drain (cluster B fills under cluster A's
        # drain; the banked ACT covers the second activation).
        dict(opcode=Opcode.SMATMUL,
             seq=7,
             evt=5,
             fmt=int(fmt),
             k=k,
             rows=rows,
             dram_addr=W_DRAM,
             dram_stride=WM_DRAM,
             r_base=lr),
        dict(opcode=Opcode.LOAD_2D,
             seq=8,
             evt=6,
             dram_addr=A_DRAM + 0x1000,
             tmem_addr=ha,
             row_bytes=qh,
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.LOAD_2D,
             seq=9,
             evt=7,
             dram_addr=M_DRAM + 0x1000,
             tmem_addr=hm,
             row_bytes=mh,
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.WAIT, seq=10, evt=6),
        dict(opcode=Opcode.WAIT, seq=11, evt=7),
        dict(opcode=Opcode.SQUANTIZE, seq=12, evt=8, a_base=ha, m_base=hm,
             k=k),
        dict(opcode=Opcode.WAIT, seq=13, evt=5),  # L completion
        dict(opcode=Opcode.WAIT, seq=14, evt=8),  # H load drained
        # L's store reads cluster A while H's epilogue drains cluster
        # B.
        dict(opcode=Opcode.STORE_2D,
             seq=15,
             evt=9,
             dram_addr=R_DRAM,
             tmem_addr=lr,
             row_bytes=rstride,
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.SMATMUL,
             seq=16,
             evt=10,
             fmt=int(fmt),
             k=k,
             rows=rows,
             dram_addr=W_DRAM + 0x1000,
             dram_stride=WM_DRAM + 0x1000,
             r_base=hr),
        dict(opcode=Opcode.WAIT, seq=17, evt=9),
        dict(opcode=Opcode.WAIT, seq=18, evt=10),
        dict(opcode=Opcode.STORE_2D,
             seq=19,
             evt=11,
             dram_addr=R_DRAM + 0x1000,
             tmem_addr=hr,
             row_bytes=rstride,
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.WAIT, seq=20, evt=11),
        dict(opcode=Opcode.END, seq=21),
    ]
    results, image, _ = run_program(params, program, image, deadline=60000)
    assert all(s is Status.OK for s in results.values()), results
    check_tile(image, fmt, k, rows, wsegs_l, act_l, R_DRAM)
    check_tile(image, fmt, k, rows, wsegs_h, act_h, R_DRAM + 0x1000)


@pytest.mark.parametrize('start', [HALF - 16, HALF - 8, HALF])
def test_straddling_region_dma_roundtrip(start):
    """A LOAD/STORE pair whose region straddles (or abuts) the cluster
    boundary round-trips byte-exact -- every beat self-routes by its
    own address and the store's read order is pinned by the
    per-master in-flight guard."""
    row_bytes = 8
    rows = 4
    src = R_DRAM + 0x2000
    dst = R_DRAM + 0x2800
    image = bytearray(DRAM_SIZE)
    pattern = bytes((i * 37 + 11) & 0xFF for i in range(row_bytes * rows))
    image[src:src + len(pattern)] = pattern

    program = [
        dict(opcode=Opcode.LOAD_2D,
             seq=1,
             evt=1,
             dram_addr=src,
             tmem_addr=start,
             row_bytes=row_bytes,
             rows=rows,
             dram_stride=row_bytes,
             tmem_stride=row_bytes),
        dict(opcode=Opcode.WAIT, seq=2, evt=1),
        dict(opcode=Opcode.STORE_2D,
             seq=3,
             evt=2,
             dram_addr=dst,
             tmem_addr=start,
             row_bytes=row_bytes,
             rows=rows,
             dram_stride=row_bytes,
             tmem_stride=row_bytes),
        dict(opcode=Opcode.WAIT, seq=4, evt=2),
        dict(opcode=Opcode.END, seq=5),
    ]
    results, image, _ = run_program(PARAMS, program, image)
    assert all(s is Status.OK for s in results.values()), results
    assert bytes(image[dst:dst + len(pattern)]) == pattern
