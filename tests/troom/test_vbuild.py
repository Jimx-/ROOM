"""Focused pysim tests for the troom V-window tile builder.

The builder turns one 64-slot FP16 pool window (slot-major TMEM
image) into the transposed two-block Q8_0 tile the FA MATMUL
consumes. These tests drive real banked TMEM contents at several
beat widths and live counts and compare the streamed tile beats
byte for byte against the frozen model reference
(``model.vbuild_tile``), including the special-value table (zeros,
subnormals, infinities, NaN mixes) and the descriptor rejections.

The SG1 drain cases (nonzero ``m_base``) compare the staged payload
and meta images byte for byte against ``model.vbuild_sg1`` -- the
same frozen quant bytes and scales in the streaming engine's
consumption order -- at the deployed 64-byte beat only.

The K-orientation cases (``rows == 16``) compare the staged group
images (1024/64 bytes) byte for byte against ``model.vbuild_sg1_k``
at live counts 0..16, over the same special-value, NaN and corpus
gates, plus the K-specific descriptor rejections.

The P2d-a partial cases (``rows`` 1/2/17) compose the retained
host-loaded extents with the rebuilt ones against the model's masks.

The P2d-b append cases (``rows`` 18/19, the open block's per-token
contract) compare the patched payload beats and the scale-row beat
against ``model.vbuild_sg1_append`` -- the frozen single-scale row
sequence of ``model.vbuild_quant_row_token`` composed over the prior
appends' bytes -- plus the token-sequence, fold and seal-overwrite
model gates.
"""

from amaranth import *
from amaranth.hdl.rec import Direction, Record
from amaranth.sim import Passive, Simulator
from amaranth.utils import log2_int

import pytest
import random
import struct

from troom.abi import VBUILD_ROWS_HD128
from troom.consts import Status
from troom.tmem import TMEM
from troom.types import HasTRoomParams
from troom.vbuild import VB_ROWS_K_APP, VBuildUnit

from roomsoc.interconnect.stream import Decoupled

from tests.troom.model import (
    VB_OPEN_META_BITS, f32_bits, fp16_to_f32, vbuild_pfold_bits, vbuild_sg1,
    vbuild_sg1_append, vbuild_sg1_append_mask, vbuild_sg1_block_mask,
    vbuild_sg1_k, vbuild_sg1_k_append, vbuild_sg1_k_append_mask,
    vbuild_sg1_k_mask, vbuild_quant_row, vbuild_quant_row_token, vbuild_tile,
    vbuild_sg1_wide, vbuild_sg1_k4_append, vbuild_sg1_k4_state)

PARAMS = dict(
    addr_width=32,
    data_width=64,
    id_width=4,
    seg_len=32,
    n_outputs=2,
    n_tokens=1,
    tmem_bytes=64 * 1024,
    n_tmem_banks=4,
    act_max_k=1024,
)

A_BASE = 0x100
R_BASE = 0x2000

# Interesting FP16 patterns: specials plus spread magnitudes.
SPECIAL_16 = [
    0x0000,
    0x8000,  # +-0
    0x0001,
    0x8001,  # smallest subnormals
    0x03ff,
    0x8400,  # subnormal / smallest normal boundaries
    0x0400,
    0x3c00,  # 1.0-ish normals
    0x7bff,
    0xfbff,  # largest finite +-65504
    0x7c00,
    0xfc00,  # +-inf
    0x7c01,
    0xfd00,  # NaNs (positive patterns)
    0x7e00,
    0xfe00,  # more NaNs (negative patterns)
]


def gen_window(rng, live, specials=True):
    """64 x 64 FP16 patterns; slots >= live are zero (the builder
    gathers +0 for them, and the model bakes the same zeros)."""
    win = []
    for j in range(64):
        for d in range(64):
            if j >= live:
                win.append(0x0000)
            elif specials and rng.random() < 0.05:
                win.append(rng.choice(SPECIAL_16))
            else:
                # A spread of finite magnitudes, normals and
                # subnormals alike, both signs.
                e = rng.randint(1, 30)
                m = rng.randint(0, 0x3ff)
                win.append((rng.randint(0, 1) << 15) | (e << 10) | m)
    return win


def gen_group(rng, live, specials=True):
    """16 x 64 FP16 patterns (one K group, slot-major); slots >= live
    are zero, exactly as ``gen_window`` bakes the V dead tail."""
    return gen_window(rng, live, specials)[:16 * 64]


class Harness(Elaboratable):
    """VBuildUnit reading from banked TMEM; tile beats captured.

    With split TMEM (the default) the unit's ports route to the
    cluster each address names, exactly as core.py routes the shared
    ports -- the plain cluster-A-only wiring aliases any address at
    or above the half boundary into the wrong cluster's rows (the
    top word bit is dropped in the row decode), silently corrupting
    images that live across the boundary.
    """

    def __init__(self, params):
        self.params = params
        self.tmem = TMEM(params)
        self.dut = VBuildUnit(params)
        self.write_ready = Signal(reset=1)
        self.write_hold_enable = Signal()
        self.write_hold_base = Signal.like(self.dut.tmem_w.bits.addr)

    def elaborate(self, platform):
        m = Module()
        m.submodules.tmem = self.tmem
        m.submodules.dut = self.dut
        write_ready = Signal()
        wr = self.dut.tmem_w
        m.d.comb += write_ready.eq(
            self.write_ready
            & ~(self.write_hold_enable
                & (wr.bits.addr >= self.write_hold_base)
                & (wr.bits.addr < self.write_hold_base + 1024)))
        if self.tmem.tmem_clusters == 2:
            half = log2_int(self.tmem.tmem_bytes // 2)
            rq = self.dut.tmem_rreq
            rhi = rq.bits.addr[half]
            b_owner = Signal()
            with m.If(rq.fire):
                m.d.sync += b_owner.eq(rhi)
            m.d.comb += [
                self.tmem.rreq.valid.eq(rq.valid & ~rhi),
                self.tmem.rreq_b.valid.eq(rq.valid & rhi),
                self.tmem.rreq.bits.addr.eq(rq.bits.addr),
                self.tmem.rreq_b.bits.addr.eq(rq.bits.addr),
                rq.ready.eq(
                    Mux(rhi, self.tmem.rreq_b.ready, self.tmem.rreq.ready)),
                self.dut.tmem_rresp.valid.eq(
                    Mux(b_owner, self.tmem.rresp_b.valid,
                        self.tmem.rresp.valid)),
                self.dut.tmem_rresp.bits.data.eq(
                    Mux(b_owner, self.tmem.rresp_b.bits.data,
                        self.tmem.rresp.bits.data)),
                self.tmem.rresp.ready.eq(~b_owner
                                         & self.dut.tmem_rresp.ready),
                self.tmem.rresp_b.ready.eq(b_owner
                                           & self.dut.tmem_rresp.ready),
            ]
            whi = self.dut.tmem_w.bits.addr[half]
            m.d.comb += [
                self.tmem.write.valid.eq(self.dut.tmem_w.valid & ~whi
                                         & write_ready),
                self.tmem.write_b.valid.eq(self.dut.tmem_w.valid & whi
                                           & write_ready),
                self.tmem.write.bits.addr.eq(self.dut.tmem_w.bits.addr),
                self.tmem.write_b.bits.addr.eq(self.dut.tmem_w.bits.addr),
                self.tmem.write.bits.data.eq(self.dut.tmem_w.bits.data),
                self.tmem.write_b.bits.data.eq(self.dut.tmem_w.bits.data),
                self.dut.tmem_w.ready.eq(write_ready),
            ]
        else:
            m.d.comb += [
                self.dut.tmem_rreq.connect(self.tmem.rreq),
                self.tmem.rresp.connect(self.dut.tmem_rresp),
                self.dut.tmem_w.ready.eq(write_ready),
            ]
        return m


def run_build(params,
              window,
              live,
              a_base=A_BASE,
              r_base=R_BASE,
              m_base=0,
              rows=0,
              retain=False,
              retain_images=None,
              deadline=400000,
              write_stalls=False):
    hp = HasTRoomParams(params)
    dut = Harness(params)
    beats = []
    result = {'status': None, 'counters': None}
    stall_injected = []

    def tx():
        # Fill TMEM with the window image (the LOAD_2D rows).
        img = b''.join(p.to_bytes(2, 'little') for p in window)
        img += bytes((-len(img)) % hp.beat_bytes)
        for off in range(0, len(img), hp.beat_bytes):
            word = int.from_bytes(img[off:off + hp.beat_bytes], 'little')
            yield from tmem_write_routed(dut.tmem, a_base + off, word)
        if retain_images is not None:
            # Explicit prior staging images (e.g. a previous append's
            # output): the load-modify-store half of the contract.
            for base, blob in retain_images:
                for off in range(0, len(blob), hp.beat_bytes):
                    word = int.from_bytes(blob[off:off + hp.beat_bytes],
                                          'little')
                    yield from tmem_write_routed(dut.tmem, base + off, word)
        elif retain:
            # The host's meta/payload LOAD ahead of a partial run: the
            # staging extents the run does not rebuild keep these
            # bytes (the previous build's output, in device terms).
            for base, n in ((r_base, 64 * 64), (m_base, 64 * 4)):
                pat = bytes((0xC0 | (i % 0x30)) & 0xff for i in range(n))
                for off in range(0, n, hp.beat_bytes):
                    word = int.from_bytes(pat[off:off + hp.beat_bytes],
                                          'little')
                    yield from tmem_write_routed(dut.tmem, base + off, word)

        sink = dut.dut.sink
        yield sink.bits.a_base.eq(a_base)
        yield sink.bits.r_base.eq(r_base)
        yield sink.bits.m_base.eq(m_base)
        yield sink.bits.k.eq(live)
        yield sink.bits.rows.eq(rows)
        yield sink.valid.eq(1)
        yield
        while not (yield sink.fire):
            yield
        # A rejected descriptor pulses done combinationally on the
        # accept cycle; capture it before the valid drop.
        if (yield dut.dut.done):
            result['status'] = Status((yield dut.dut.status))
        yield sink.valid.eq(0)
        yield

        if result['status'] is None:
            for _ in range(deadline):
                yield
                if (yield dut.dut.done):
                    result['status'] = Status((yield dut.dut.status))
                    # One more edge before the snapshot: the DONE state
                    # increments `tiles` (and nothing else) on its exit
                    # edge, so reading in the done cycle reads it low.
                    yield
                    result['counters'] = {
                        'run': (yield dut.dut.run_cycles),
                        'fill': (yield dut.dut.fill_cycles),
                        'gather': (yield dut.dut.gather_cycles),
                        'scale': (yield dut.dut.scale_cycles),
                        'quant': (yield dut.dut.quant_cycles),
                        'drain': (yield dut.dut.drain_cycles),
                        'dead_blocks': (yield dut.dut.dead_blocks),
                        'zero_blocks': (yield dut.dut.zero_blocks),
                        # D1 phase-split riders (register window at
                        # REG_VB_*): dispatch, the TMEM wait subsets
                        # and the gather/quant prefetch overlap, with
                        # the occupancy total.
                        'active': (yield dut.dut.active_cycles),
                        'dispatch': (yield dut.dut.dispatch_cycles),
                        'fill_wait': (yield dut.dut.fill_wait_cycles),
                        'drain_wait': (yield dut.dut.drain_wait_cycles),
                        'prefetch': (yield dut.dut.prefetch_cycles),
                        'tiles': (yield dut.dut.tiles),
                        'rows': (yield dut.dut.rows),
                        'beats': (yield dut.dut.beats),
                    }
                    break
            else:
                progress = {
                    'run': (yield dut.dut.run_cycles),
                    'rows': (yield dut.dut.rows),
                    'beats': (yield dut.dut.beats),
                    'scale': (yield dut.dut.scale_cycles),
                    'quant': (yield dut.dut.quant_cycles),
                }
                raise TimeoutError(f'vbuild did not finish: {progress}')

    def watch():
        yield Passive()
        wr = dut.dut.tmem_w
        held = None
        while True:
            if held is not None:
                assert (yield
                        wr.valid), 'write valid dropped under backpressure'
                assert ((yield wr.bits.addr), (yield wr.bits.data)) == held
            held = (((yield wr.bits.addr), (yield wr.bits.data)) if
                    (yield wr.valid) and not (yield wr.ready) else None)
            if (yield wr.fire):
                addr = (yield wr.bits.addr)
                data = (yield wr.bits.data)
                beats.append((addr, data))
            yield

    def stall_writes():
        yield Passive()
        # The tx coroutine owns TMEM write signals while loading the
        # input images. Start backpressure only after descriptor accept.
        while not (yield dut.dut.active):
            yield
        if write_stalls == 'state':
            state_base = m_base + 512 + (int((live & 0x7f) > 32) * 1024)
            yield dut.write_hold_base.eq(state_base)
            yield dut.write_hold_enable.eq(1)
            while True:
                wr = dut.dut.tmem_w
                addr = (yield wr.bits.addr)
                if (yield wr.valid) and state_base <= addr < state_base + 1024:
                    stall_injected.append(True)
                    assert not (yield wr.ready)
                    for _ in range(47):
                        yield
                    yield dut.write_hold_enable.eq(0)
                    yield
                    return
                yield
        cycle = 0
        while True:
            yield dut.write_ready.eq(cycle % 53 >= 47)
            cycle += 1
            yield

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(tx)
    sim.add_sync_process(watch)
    if write_stalls:
        sim.add_sync_process(stall_writes)
    sim.run()
    if write_stalls == 'state':
        assert stall_injected, 'targeted state-write stall was not exercised'
    return beats, result


def tmem_write(write, addr, word):
    yield write.bits.addr.eq(addr)
    yield write.bits.data.eq(word)
    yield write.valid.eq(1)
    yield
    while not (yield write.fire):
        yield
    yield write.valid.eq(0)
    yield


def tmem_write_routed(tmem, addr, word):
    """tmem_write against the cluster the address names (the plain
    harness's TMEM exposes both write ports; the address half picks
    the one core.py would route to)."""
    port = tmem.write
    if getattr(tmem, 'tmem_clusters', 1) == 2 \
            and (addr >> (tmem.tmem_addr_width - 1)) & 1:
        port = tmem.write_b
    yield from tmem_write(port, addr, word)


def first_diff(got, ref):
    for i, (g, r) in enumerate(zip(got, ref)):
        if g != r:
            return f'first diff at byte {i}: got {g:#x} want {r:#x}'
    return f'lengths {len(got)} vs {len(ref)}'


def expected_skips(window, live):
    """(dead, zero) block-skip counts the early-out must take: a
    block with no live slots is dead; one whose live slots are all
    finite zeros (no NaN -- NaN bytes saturate, not zero) skips on
    amax == 0."""
    dead = zero = 0
    for d in range(64):
        for b in range(2):
            seg = [
                window[j * 64 + d]
                for j in range(32 * b, min(live, 32 * b + 32))
            ]
            if not seg:
                dead += 1
                continue
            mags = [h & 0x7fff for h in seg]
            if any(m >= 0x7c01 for m in mags):
                continue
            if max(mags, default=0) == 0:
                zero += 1
    return dead, zero


def rebased(beats, hp):
    """The tile image with addresses rebased to 0..64*row_bytes."""
    row_bytes = -(-68 // hp.beat_bytes) * hp.beat_bytes
    img = bytearray(64 * row_bytes)
    for addr, data in beats:
        off = addr - R_BASE
        assert 0 <= off < len(img), hex(addr)
        for i in range(hp.data_width // 8):
            img[off + i] = (data >> (8 * i)) & 0xff
    return bytes(img)


M_BASE = 0x4000


def sg1_images(beats, hp):
    """The SG1 payload (rebased to r_base) and meta (rebased to
    m_base) staging images from captured beats."""
    payload = bytearray(64 * 64)
    meta = bytearray(64 * 4)
    for addr, data in beats:
        for i in range(hp.data_width // 8):
            if R_BASE <= addr < R_BASE + len(payload):
                payload[addr - R_BASE + i] = (data >> (8 * i)) & 0xff
            elif M_BASE <= addr < M_BASE + len(meta):
                meta[addr - M_BASE + i] = (data >> (8 * i)) & 0xff
            else:
                raise AssertionError(hex(addr))
    return bytes(payload), bytes(meta)


def sg1_k_images(beats, hp):
    """The K group's payload (1024 B at r_base) and meta (64 B at
    m_base) staging images from captured beats."""
    payload = bytearray(16 * 64)
    meta = bytearray(16 * 4)
    for addr, data in beats:
        for i in range(hp.data_width // 8):
            if R_BASE <= addr < R_BASE + len(payload):
                payload[addr - R_BASE + i] = (data >> (8 * i)) & 0xff
            elif M_BASE <= addr < M_BASE + len(meta):
                meta[addr - M_BASE + i] = (data >> (8 * i)) & 0xff
            else:
                raise AssertionError(hex(addr))
    return bytes(payload), bytes(meta)


def expected_skips_k(group, live):
    """(dead, zero) block-skip counts the K orientation must take: a
    slot row at or above the live count is dead (both blocks); a live
    row whose block is all finite zeros skips on amax == 0."""
    dead = zero = 0
    for u in range(16):
        for b in range(2):
            if u >= live:
                dead += 1
                continue
            seg = group[u * 64 + 32 * b:u * 64 + 32 * b + 32]
            mags = [h & 0x7fff for h in seg]
            if any(m >= 0x7c01 for m in mags):
                continue
            if max(mags, default=0) == 0:
                zero += 1
    return dead, zero


@pytest.mark.parametrize('data_width', [64, 512])
@pytest.mark.parametrize('live', [64, 39, 33, 32, 20, 8, 1])
def test_vbuild_tile_matches_model(data_width, live):
    rng = random.Random(1000 + data_width + live)
    window = gen_window(rng, live)
    params = dict(PARAMS, data_width=data_width)
    beats, res = run_build(params, window, live)
    assert res['status'] is Status.OK
    hp = HasTRoomParams(params)
    got = rebased(beats, hp)
    ref = vbuild_tile(window, hp.beat_bytes)
    assert len(got) == len(ref)
    assert got == ref, first_diff(got, ref)
    row_bytes = -(-68 // hp.beat_bytes) * hp.beat_bytes
    assert len(beats) == 64 * row_bytes // hp.beat_bytes
    if data_width == 512:
        # Cycle-budget guard for the dead-tail early-out: the dead
        # prefix must not stream.  Live 8 gathers/quantizes 8 of 32
        # block-0 elements per row (baseline before the early-out:
        # 7969); live 64 is all-live and must stay near the old cost
        # (11.9k) -- the early-out may not slow the full tile.
        if live == 8:
            assert res['counters']['run'] < 5700
        else:
            assert res['counters']['run'] < 13500
    assert res['counters']['dead_blocks'] == (64 if live <= 32 else 0)
    edead, ezero = expected_skips(window, live)
    assert res['counters']['dead_blocks'] == edead
    assert res['counters']['zero_blocks'] == ezero


def test_vbuild_all_zero_window():
    window = [0x0000] * 64 * 64
    beats, res = run_build(dict(PARAMS), window, 64)
    assert res['status'] is Status.OK
    hp = HasTRoomParams(PARAMS)
    assert rebased(beats, hp) == vbuild_tile(window, hp.beat_bytes)
    # Every block classifies amax == 0 with live slots present: the
    # zero-amax early-out skips both quant streams of every row.
    assert res['counters']['zero_blocks'] == 128
    assert res['counters']['dead_blocks'] == 0


def test_vbuild_zero_amax_rows_mixed():
    """Live data whose some rows are identically zero: those rows'
    blocks take the zero-amax skip, their neighbors do not, and the
    tile still matches the model byte for byte."""
    rng = random.Random(77)
    window = gen_window(rng, 40)
    for d in (9, 10, 17):  # rows with all-zero live data
        for j in range(64):
            window[j * 64 + d] = 0x0000
    params = dict(PARAMS, data_width=512)
    beats, res = run_build(params, window, 40)
    assert res['status'] is Status.OK
    hp = HasTRoomParams(params)
    got = rebased(beats, hp)
    ref = vbuild_tile(window, hp.beat_bytes)
    assert got == ref, first_diff(got, ref)
    assert res['counters']['zero_blocks'] == 3 * 2
    assert res['counters']['dead_blocks'] == 0


def test_vbuild_nan_in_zero_amax_block():
    """A block whose live elements are all NaN has amax 0 (the fold
    excludes NaNs) but must NOT take the zero-amax skip: NaN * 0 is
    NaN and the frozen q8 helper saturates it to 0x7f, matching the
    model. Exercises the gather-fold NaN latch."""
    rng = random.Random(7)
    window = gen_window(rng, 1)
    for d in (5,
              40):  # block-0-only and both-block NaNs (live=1: block 1 dead)
        window[0 * 64 + d] = 0x7e00
    params = dict(PARAMS, data_width=512)
    beats, res = run_build(params, window, 1)
    assert res['status'] is Status.OK
    hp = HasTRoomParams(params)
    got = rebased(beats, hp)
    ref = vbuild_tile(window, hp.beat_bytes)
    assert got == ref, first_diff(got, ref)
    # row 5's block 0 ran its quant stream (NaN byte); row 40's
    # block 0 likewise. Rows with NaNs never skip; only genuine
    # all-zero rows do.
    edead, ezero = expected_skips(window, 1)
    assert res['counters']['zero_blocks'] == ezero
    assert res['counters']['dead_blocks'] == edead == 64


def test_vbuild_special_heavy_window():
    rng = random.Random(42)
    window = [rng.choice(SPECIAL_16) for _ in range(64 * 64)]
    # Zero the dead slots like the callers (live = 64 keeps all; the
    # exercise is the specials across every block).
    window = [p if (j // 64) < 64 else 0 for j, p in enumerate(window)]
    params = dict(PARAMS, data_width=512)
    beats, res = run_build(params, window, 64)
    assert res['status'] is Status.OK
    hp = HasTRoomParams(params)
    got = rebased(beats, hp)
    ref = vbuild_tile(window, hp.beat_bytes)
    assert got == ref, first_diff(got, ref)


@pytest.mark.parametrize('live', [64, 33, 32, 20, 1])
def test_vbuild_sg1_matches_model(live):
    """The SG1 drain stages the payload/meta images the frozen model
    packs, at every live count: same quant bytes and scales as the
    legacy tile, in stream consumption order."""
    rng = random.Random(5000 + live)
    window = gen_window(rng, live)
    params = dict(PARAMS, data_width=512)
    beats, res = run_build(params, window, live, m_base=M_BASE)
    assert res['status'] is Status.OK
    hp = HasTRoomParams(params)
    pay, meta = sg1_images(beats, hp)
    ref_pay, ref_meta = vbuild_sg1(window)
    assert pay == ref_pay, first_diff(pay, ref_pay)
    assert meta == ref_meta, first_diff(meta, ref_meta)
    assert len(beats) == (64 * 64 + 64 * 4) // hp.beat_bytes
    edead, ezero = expected_skips(window, live)
    assert res['counters']['dead_blocks'] == edead
    assert res['counters']['zero_blocks'] == ezero


def test_vbuild_sg1_all_zero_window():
    """A fully-dead (or all-zero) tile still stages every byte: the
    skips only avoid compute, never the zero contributions."""
    window = [0x0000] * 64 * 64
    params = dict(PARAMS, data_width=512)
    beats, res = run_build(params, window, 64, m_base=M_BASE)
    assert res['status'] is Status.OK
    hp = HasTRoomParams(params)
    pay, meta = sg1_images(beats, hp)
    ref_pay, ref_meta = vbuild_sg1(window)
    assert pay == ref_pay, first_diff(pay, ref_pay)
    assert meta == ref_meta, first_diff(meta, ref_meta)
    assert not any(pay) and not any(meta)


def test_vbuild_sg1_special_heavy_window():
    rng = random.Random(43)
    window = [rng.choice(SPECIAL_16) for _ in range(64 * 64)]
    params = dict(PARAMS, data_width=512)
    beats, res = run_build(params, window, 64, m_base=M_BASE)
    assert res['status'] is Status.OK
    hp = HasTRoomParams(params)
    pay, meta = sg1_images(beats, hp)
    ref_pay, ref_meta = vbuild_sg1(window)
    assert pay == ref_pay, first_diff(pay, ref_pay)
    assert meta == ref_meta, first_diff(meta, ref_meta)


def test_vbuild_sg1_nan_rows():
    """NaN rows exercise the no-skip path (NaN bytes saturate) inside
    the SG1 placement: bytes must equal the model's, not zeros."""
    rng = random.Random(7)
    window = gen_window(rng, 1)
    for d in (5, 40):
        window[0 * 64 + d] = 0x7e00
    params = dict(PARAMS, data_width=512)
    beats, res = run_build(params, window, 1, m_base=M_BASE)
    assert res['status'] is Status.OK
    hp = HasTRoomParams(params)
    pay, meta = sg1_images(beats, hp)
    ref_pay, ref_meta = vbuild_sg1(window)
    assert pay == ref_pay, first_diff(pay, ref_pay)
    assert meta == ref_meta, first_diff(meta, ref_meta)


def test_vbuild_sg1_corpus():
    """The E1 gate's corpus (the 2056-row set of
    check-vbuild-quant.py: special values, every FP16 bit pattern in
    sequence, and random mixtures) as 32 whole windows, byte-compared
    against the frozen model's SG1 packing."""
    rng = random.Random(20261027)
    rows = [[0] * 64, [0x8000] * 64, [0x3c00] * 64, [0x7bff] * 64, [1] * 64,
            [0x7c00] * 64, [0xfc00] * 64, [0x7e00] * 64]
    rows += [list(range(i, i + 64)) for i in range(0, 65536, 64)]
    rows += [[rng.randrange(65536) for _ in range(64)] for _ in range(1024)]
    assert len(rows) == 2056
    params = dict(PARAMS, data_width=512)
    hp = HasTRoomParams(params)
    for w in range(32):
        window = rows[w * 64:(w + 1) * 64]
        window = [p for slot in window for p in slot]
        beats, res = run_build(params, window, 64, m_base=M_BASE)
        assert res['status'] is Status.OK, f'corpus window {w}'
        pay, meta = sg1_images(beats, hp)
        ref_pay, ref_meta = vbuild_sg1(window)
        assert pay == ref_pay, f'corpus window {w} payload: {first_diff(pay, ref_pay)}'
        assert meta == ref_meta, f'corpus window {w} meta: {first_diff(meta, ref_meta)}'


@pytest.mark.parametrize('live', [16, 13, 8, 2, 1, 0])
def test_vbuild_k_matches_model(live):
    """The K orientation stages the group's payload/meta images the
    frozen model packs, at every live count including the fully-dead
    group (live 0: no pool read, all-zero rows)."""
    rng = random.Random(7000 + live)
    group = gen_group(rng, live)
    params = dict(PARAMS, data_width=512)
    beats, res = run_build(params, group, live, m_base=M_BASE, rows=16)
    assert res['status'] is Status.OK
    hp = HasTRoomParams(params)
    pay, meta = sg1_k_images(beats, hp)
    ref_pay, ref_meta = vbuild_sg1_k(group)
    assert pay == ref_pay, first_diff(pay, ref_pay)
    assert meta == ref_meta, first_diff(meta, ref_meta)
    # 16 payload beats (8 lane pairs x 2 blocks) + 1 meta beat.
    assert len(beats) == (16 * 64 + 16 * 4) // hp.beat_bytes
    edead, ezero = expected_skips_k(group, live)
    assert res['counters']['dead_blocks'] == edead
    assert res['counters']['zero_blocks'] == ezero


def test_vbuild_k_special_heavy_group():
    rng = random.Random(44)
    group = [rng.choice(SPECIAL_16) for _ in range(16 * 64)]
    params = dict(PARAMS, data_width=512)
    beats, res = run_build(params, group, 16, m_base=M_BASE, rows=16)
    assert res['status'] is Status.OK
    hp = HasTRoomParams(params)
    pay, meta = sg1_k_images(beats, hp)
    ref_pay, ref_meta = vbuild_sg1_k(group)
    assert pay == ref_pay, first_diff(pay, ref_pay)
    assert meta == ref_meta, first_diff(meta, ref_meta)


def test_vbuild_k_nan_rows():
    """A NaN dim inside an otherwise-zero live row must not take the
    zero-amax skip: its byte saturates, matching the model, in the K
    placement as in the V one. Dead rows stay zero regardless of their
    buffer content (the gather mask), so the NaNs live in live rows."""
    rng = random.Random(8)
    group = gen_group(rng, 12)
    for u in (2, 11):  # a block-0-only and a both-block NaN row
        group[u * 64 + 5] = 0x7e00
    group[11 * 64 + 40] = 0x7e00
    params = dict(PARAMS, data_width=512)
    beats, res = run_build(params, group, 12, m_base=M_BASE, rows=16)
    assert res['status'] is Status.OK
    hp = HasTRoomParams(params)
    pay, meta = sg1_k_images(beats, hp)
    ref_pay, ref_meta = vbuild_sg1_k(group)
    assert pay == ref_pay, first_diff(pay, ref_pay)
    assert meta == ref_meta, first_diff(meta, ref_meta)
    edead, ezero = expected_skips_k(group, 12)
    assert res['counters']['dead_blocks'] == edead
    assert res['counters']['zero_blocks'] == ezero


def test_vbuild_k_corpus():
    """The E1-K gate's corpus: the frozen 2056-row set arranged as 128
    whole groups plus one live-8 partial group, byte-compared against
    the frozen model's K packing."""
    rng = random.Random(20261027)
    rows = [[0] * 64, [0x8000] * 64, [0x3c00] * 64, [0x7bff] * 64, [1] * 64,
            [0x7c00] * 64, [0xfc00] * 64, [0x7e00] * 64]
    rows += [list(range(i, i + 64)) for i in range(0, 65536, 64)]
    rows += [[rng.randrange(65536) for _ in range(64)] for _ in range(1024)]
    assert len(rows) == 2056
    params = dict(PARAMS, data_width=512)
    hp = HasTRoomParams(params)
    for g in range(129):
        slots = rows[g * 16:(g + 1) * 16]
        live = 16 if g < 128 else len(slots)
        slots = slots + [[0] * 64] * (16 - len(slots))
        group = [p for slot in slots for p in slot]
        beats, res = run_build(params, group, live, m_base=M_BASE, rows=16)
        assert res['status'] is Status.OK, f'corpus group {g}'
        pay, meta = sg1_k_images(beats, hp)
        ref_pay, ref_meta = vbuild_sg1_k(group)
        assert pay == ref_pay, (
            f'corpus group {g} payload: {first_diff(pay, ref_pay)}')
        assert meta == ref_meta, (
            f'corpus group {g} meta: {first_diff(meta, ref_meta)}')


# ---------------------------------------------------------------------
# P2d-a partial-extent drain: the staging bytes a partial run writes
# are the full build's bytes on the rebuilt extents; everything else
# keeps the host-loaded previous build's pattern (retain=True fills
# staging with a distinctive byte pattern before the run).
# ---------------------------------------------------------------------


def retain_pattern(n):
    return bytes((0xC0 | (i % 0x30)) & 0xff for i in range(n))


def images_over(beats, hp, pay0, met0):
    """Staging images reconstructed over the retained pattern."""
    pay = bytearray(pay0)
    met = bytearray(met0)
    for addr, data in beats:
        for i in range(hp.data_width // 8):
            if R_BASE <= addr < R_BASE + len(pay):
                pay[addr - R_BASE + i] = (data >> (8 * i)) & 0xff
            elif M_BASE <= addr < M_BASE + len(met):
                met[addr - M_BASE + i] = (data >> (8 * i)) & 0xff
            else:
                raise AssertionError(hex(addr))
    return bytes(pay), bytes(met)


def compose_retained(prefill, mask, ref):
    """The expected partial image: retained pattern everywhere except
    the masked extents, which carry the full build's bytes."""
    out = bytearray(prefill)
    for i, m in enumerate(mask):
        if m:
            out[i] = ref[i]
    return bytes(out)


# The partial-extent runs read the staging regions back (the meta
# pre-read), so their window image needs a base disjoint from both
# staging extents -- A_BASE's 8 KiB window would overlap R_BASE.
P_BASE = 0x8000


@pytest.mark.parametrize('live', [64, 48, 33])
def test_vbuild_sg1_partial_block1_matches_model(live):
    """rows == 2 (the block-1 extent): the clean block 0 is neither
    gathered nor quantized nor written; the written bytes are exactly
    the full build's block-1 bytes, and the run is materially cheaper
    than the full build at the same live count."""
    rng = random.Random(3100 + live)
    window = gen_window(rng, live)
    params = dict(PARAMS, data_width=512)
    beats, res = run_build(params,
                           window,
                           live,
                           a_base=P_BASE,
                           m_base=M_BASE,
                           rows=2,
                           retain=True)
    assert res['status'] is Status.OK
    hp = HasTRoomParams(params)
    pay, met = images_over(beats, hp, retain_pattern(64 * 64),
                           retain_pattern(64 * 4))
    ref_pay, ref_met = vbuild_sg1(window)
    mpay, mmet = vbuild_sg1_block_mask(1)
    assert pay == compose_retained(retain_pattern(64 * 64), mpay, ref_pay), \
        first_diff(pay, compose_retained(retain_pattern(64 * 64), mpay, ref_pay))
    assert met == compose_retained(retain_pattern(64 * 4), mmet, ref_met), \
        first_diff(met, compose_retained(retain_pattern(64 * 4), mmet, ref_met))
    # 32 payload beats (block 1's lane pairs) + 4 meta beats.
    assert len(beats) == (32 * 64 + 64 * 4) // hp.beat_bytes
    _, full = run_build(params,
                        window,
                        live,
                        a_base=P_BASE,
                        m_base=M_BASE,
                        rows=0)
    assert res['counters']['run'] < full['counters']['run'] * 3 // 4


@pytest.mark.parametrize('live', [32, 20, 1])
def test_vbuild_sg1_partial_block0_matches_model(live):
    """rows == 1 (the block-0 extent, the opening-phase shape): same
    contract on block 0's extents; block 1 (dead at these live counts
    in a full build's zeros) stays loaded instead."""
    rng = random.Random(3200 + live)
    window = gen_window(rng, live)
    params = dict(PARAMS, data_width=512)
    beats, res = run_build(params,
                           window,
                           live,
                           a_base=P_BASE,
                           m_base=M_BASE,
                           rows=1,
                           retain=True)
    assert res['status'] is Status.OK
    hp = HasTRoomParams(params)
    pay, met = images_over(beats, hp, retain_pattern(64 * 64),
                           retain_pattern(64 * 4))
    ref_pay, ref_met = vbuild_sg1(window)
    mpay, mmet = vbuild_sg1_block_mask(0)
    assert pay == compose_retained(retain_pattern(64 * 64), mpay, ref_pay)
    assert met == compose_retained(retain_pattern(64 * 4), mmet, ref_met)
    assert len(beats) == (32 * 64 + 64 * 4) // hp.beat_bytes


def test_vbuild_sg1_partial_zero_amax_block1():
    """A block-1 extent whose live data is all zero: the zero-amax
    early-out still lands the (zero) d16 cells and zero chunks on
    exactly the block-1 extents."""
    rng = random.Random(33)
    window = gen_window(rng, 40)
    for j in range(32, 40):
        for d in range(64):
            window[j * 64 + d] = 0x0000
    params = dict(PARAMS, data_width=512)
    beats, res = run_build(params,
                           window,
                           40,
                           a_base=P_BASE,
                           m_base=M_BASE,
                           rows=2,
                           retain=True)
    assert res['status'] is Status.OK
    hp = HasTRoomParams(params)
    pay, met = images_over(beats, hp, retain_pattern(64 * 64),
                           retain_pattern(64 * 4))
    ref_pay, ref_met = vbuild_sg1(window)
    mpay, mmet = vbuild_sg1_block_mask(1)
    assert pay == compose_retained(retain_pattern(64 * 64), mpay, ref_pay)
    assert met == compose_retained(retain_pattern(64 * 4), mmet, ref_met)


def test_vbuild_sg1_partial_nan_block1():
    """NaN patterns inside the rebuilt extent saturate exactly as the
    full build's bytes; the retained cells never see them."""
    rng = random.Random(34)
    window = gen_window(rng, 45)
    for j in (36, 40):
        for d in (3, 33):
            window[j * 64 + d] = 0x7e00
    params = dict(PARAMS, data_width=512)
    beats, res = run_build(params,
                           window,
                           45,
                           a_base=P_BASE,
                           m_base=M_BASE,
                           rows=2,
                           retain=True)
    assert res['status'] is Status.OK
    hp = HasTRoomParams(params)
    pay, met = images_over(beats, hp, retain_pattern(64 * 64),
                           retain_pattern(64 * 4))
    ref_pay, ref_met = vbuild_sg1(window)
    mpay, mmet = vbuild_sg1_block_mask(1)
    assert pay == compose_retained(retain_pattern(64 * 64), mpay, ref_pay)
    assert met == compose_retained(retain_pattern(64 * 4), mmet, ref_met)


@pytest.mark.parametrize('r', range(16))
def test_vbuild_sg1_k_partial_pair_matches_model(r):
    """rows == 17 (the K row extent): the dirty lane pair re-based at
    buffer rows 0/1. k retains the original live prefix r+1: the input
    length is derived from parity, while the metadata must land at the
    original pair's cells. Payload remains rebased for the sliced STORE.
    Check every pair and both parities against the full group model."""
    rng = random.Random(4100 + r)
    group = gen_group(rng, r + 1)
    lo = r & ~1
    if r & 1:
        k = 2
        pair = group[lo * 64:(lo + 2) * 64]
    else:
        k = 1
        pair = group[lo * 64:(lo + 1) * 64] + [0] * 64
    params = dict(PARAMS, data_width=512)
    beats, res = run_build(params,
                           pair,
                           r + 1,
                           a_base=P_BASE,
                           m_base=M_BASE,
                           rows=17,
                           retain=True)
    assert res['status'] is Status.OK
    hp = HasTRoomParams(params)
    pay, met = images_over(beats, hp, retain_pattern(16 * 64),
                           retain_pattern(16 * 4))
    # The model over the zero-padded group: the masked extents carry
    # only rows < k, so the padding never reaches the comparison.
    ref_pay, ref_met = vbuild_sg1_k(pair + [0] * (14 * 64))
    mpay, mmet = vbuild_sg1_k_mask(k)
    # The payload STORE relocates whole pair beats; the metadata STORE
    # writes the entire group unchanged, so RTL must relocate its cells.
    _, ref_met = vbuild_sg1_k(group)
    mmet = bytearray(64)
    for lane in range(lo, r + 1):
        for block in range(2):
            off = (lane >> 2) * 16 + block * 8 + (lane & 3) * 2
            mmet[off:off + 2] = b'\x01\x01'
    assert pay == compose_retained(retain_pattern(16 * 64), mpay, ref_pay), \
        first_diff(pay, compose_retained(retain_pattern(16 * 64), mpay, ref_pay))
    assert met == compose_retained(retain_pattern(16 * 4), mmet, ref_met), \
        first_diff(met, compose_retained(retain_pattern(16 * 4), mmet, ref_met))
    # One lane pair: 2 payload beats + 1 meta beat.
    assert len(beats) == 3
    # Rate guard against the real alternative -- the full chain at the
    # group's actual live count (r+1 rows re-quantized) -- for every
    # shape the backend's r >= 2 rule takes (r == 1 saves no quant
    # work: the pair patch would re-quantize both rows anyway).
    full_window = pair + [0] * (14 * 64)
    _, full = run_build(params, full_window, r + 1, m_base=M_BASE, rows=16)
    if r >= 2:
        assert res['counters']['run'] < full['counters']['run']
    else:
        assert res['counters']['run'] < full['counters']['run'] + 100


def test_vbuild_sg1_k_partial_nan_pair():
    """A NaN dim in the rebuilt pair saturates to the model's byte on
    the pair's extents; the retained cells are untouched."""
    rng = random.Random(41)
    group = gen_group(rng, 6)
    group[5 * 64 + 5] = 0x7e00  # r = 5 (odd): both rows 4,5 rebuilt
    group[4 * 64 + 40] = 0x7e00
    pair = group[4 * 64:6 * 64]
    params = dict(PARAMS, data_width=512)
    beats, res = run_build(params,
                           pair,
                           2,
                           m_base=M_BASE,
                           rows=17,
                           retain=True)
    assert res['status'] is Status.OK
    hp = HasTRoomParams(params)
    pay, met = images_over(beats, hp, retain_pattern(16 * 64),
                           retain_pattern(16 * 4))
    # The model over the zero-padded group: the masked extents carry
    # only rows < k, so the padding never reaches the comparison.
    ref_pay, ref_met = vbuild_sg1_k(pair + [0] * (14 * 64))
    mpay, mmet = vbuild_sg1_k_mask(2)
    assert pay == compose_retained(retain_pattern(16 * 64), mpay, ref_pay)
    assert met == compose_retained(retain_pattern(16 * 4), mmet, ref_met)


# ---------------------------------------------------------------------
# P2d-b append mode (rows 18/19): the open block's per-token contract.
# The model gates run against the frozen reference alone (the host
# twin); the RTL cases drive the descriptor and compare the patched
# payload beats and the scale-row beat against the same reference.
# ---------------------------------------------------------------------


def gen_slot(rng, specials=True):
    """One appended slot's 64 FP16 dim patterns."""
    return [
        rng.choice(SPECIAL_16) if specials and rng.random() < 0.05 else
        (rng.randint(0, 1) << 15) | (rng.randint(1, 30) << 10)
        | rng.randint(0, 0x3ff) for _ in range(64)
    ]


def widen16(h):
    """FP16 bits -> Python float (struct 'e' widens exactly)."""
    return struct.unpack('<e', h.to_bytes(2, 'little'))[0]


def test_vbuild_token_row_matches_independent():
    """The token row's bytes against an independent reconstruction:
    quants are the RNE int8 of 127 * x / amax (the frozen r is a
    two-Newton recip of a, so |127*x*r - 127*x/a| is ulp-class), the
    d16 slots are equal, and every byte lands in its K-layout chunk."""
    rng = random.Random(5100)
    for t in range(40):
        if t < 38:
            slot = gen_slot(rng, specials=False)
        elif t == 38:
            slot = [0x0000] * 64  # the all-zero slot
        else:
            slot = [0x8000] * 3 + [0x0000] * 30 + [0x03ff] * 31
        row = vbuild_quant_row_token(slot)
        assert row[0:2] == row[34:36]
        vals = [widen16(h) for h in slot]
        amax = max((abs(v) for v in vals), default=0.0)
        for i, v in enumerate(vals):
            got = row[34 * (i >> 5) + 2 + (i & 31)]
            got = got - 256 if got > 127 else got
            want = 127.0 * v / amax if amax else 0.0
            assert abs(got - want) <= 1.0 + 1e-3, (t, i, got, want)


def test_vbuild_token_row_equals_block_row_on_equal_amax_halves():
    """A slot whose dim halves share the widened amax quantizes
    identically under both frozen sequences: the per-block row and the
    per-token row agree byte for byte (same a, same sc/d16, same r)."""
    rng = random.Random(5101)
    for _ in range(20):
        half = gen_slot(rng, specials=False)[:32]
        slot = half + [half[i] ^ 0x8000 for i in range(32)]
        assert vbuild_quant_row_token(slot) == vbuild_quant_row(slot)
        slot = half + list(half)
        assert vbuild_quant_row_token(slot) == vbuild_quant_row(slot)
    # The all-zero slot: d16 0, all quants 0, both sequences.
    zero = [0x0000] * 64
    assert vbuild_quant_row_token(zero) == vbuild_quant_row(zero) \
        == bytes(68)


def test_vbuild_token_row_nan_and_special_slots():
    """NaN patterns never win the fold but saturate their own bytes;
    an all-zero-plus-NaN slot still emits the zero-amax d16."""
    slot = [0x0000] * 64
    slot[7] = 0x7e00
    slot[40] = 0xfd00
    row = vbuild_quant_row_token(slot)
    assert row[0:2] == (0).to_bytes(2, 'little')
    assert row[2 + 7] == 0x7f and row[36 + 8] == 0x7f
    assert sum(row[2:34]) + sum(row[36:68]) == 0x7f + 0x7f
    # A largest-finite slot rounds its max byte to 127.
    slot = [0x7bff] * 64
    row = vbuild_quant_row_token(slot)
    assert row[2 + 0] == 0x7f and row[36 + 31] == 0x7f


def test_vbuild_pfold_bits():
    """The fold is one rounded FP32 multiply by the widened d16: the
    identity at the open-meta constant 1.0, zero at 0, and bit-exact
    against struct's single-rounded product on a spread sample."""
    assert vbuild_pfold_bits(0x3f800000, VB_OPEN_META_BITS) == 0x3f800000
    assert vbuild_pfold_bits(0xbf6abcde, VB_OPEN_META_BITS) == 0xbf6abcde
    assert vbuild_pfold_bits(0x7f7fffff, 0x0000) == 0x00000000
    rng = random.Random(5102)
    for _ in range(200):
        p = rng.getrandbits(32)
        if (p & 0x7f800000) == 0x7f800000 and (p & 0x7fffff):
            continue  # NaN operands ride the canonical-NaN policy
        d = rng.getrandbits(16) & 0x7bff  # finite d16 patterns
        pf = widen16(d)
        p32 = struct.unpack('<f', struct.pack('<I', p))[0]
        try:
            want = struct.unpack('<I', struct.pack('<f', p32 * pf))[0]
        except OverflowError:
            want = 0xff800000 if (p >> 31) else 0x7f800000
        assert vbuild_pfold_bits(p, d) == want, (hex(p), hex(d))


def test_vbuild_sg1_append_compose_and_mask():
    """The append composes only its masked bytes over the prior image,
    and a fully-appended block's chunks equal the per-slot token
    columns placed independently -- the placement math an incremental
    open tail must reproduce."""
    rng = random.Random(5103)
    pay0 = retain_pattern(64 * 64)
    srow0 = retain_pattern(64 * 4)
    slots = [gen_slot(rng) for _ in range(32)]
    blk, p = 1, 17
    pay, srow = vbuild_sg1_append(slots[p], blk, p, pay0, srow0)
    mpay, msrow = vbuild_sg1_append_mask(blk, p)
    # Exactly the masked bytes changed, and each carries the token row.
    row = vbuild_quant_row_token(slots[p])
    diff = [i for i in range(64 * 64) if pay[i] != pay0[i]]
    mdiff = [i for i in range(64 * 4) if srow[i] != srow0[i]]
    assert diff == [i for i in range(64 * 64) if mpay[i]]
    assert mdiff == [i for i in range(64 * 4) if msrow[i]]
    for d in range(64):
        off = ((d >> 2) * 8 + blk * 4 + (d & 3)) * 32 + p
        assert pay[off] == row[34 * (d >> 5) + 2 + (d & 31)]
    d16 = int.from_bytes(row[0:2], 'little')
    wo = 4 * (blk * 32 + p)
    assert srow[wo:wo + 4] == f32_bits(fp16_to_f32(d16)).to_bytes(4, 'little')
    # Appending every position in order builds the whole block from
    # per-slot token columns: chunk(d, blk)[p] is slot p's dim-d byte.
    pay = bytes(64 * 64)
    srow = bytes(64 * 4)
    for p in range(32):
        pay, srow = vbuild_sg1_append(slots[p], blk, p, pay, srow)
    for p in range(32):
        row = vbuild_quant_row_token(slots[p])
        for d in range(64):
            off = ((d >> 2) * 8 + blk * 4 + (d & 3)) * 32 + p
            assert pay[off] == row[34 * (d >> 5) + 2 + (d & 31)]
        d16 = int.from_bytes(row[0:2], 'little')
        wo = 4 * (blk * 32 + p)
        assert srow[wo:wo + 4] == f32_bits(fp16_to_f32(d16)).to_bytes(
            4, 'little')


def test_vbuild_seal_overwrites_append_bytes():
    """The seal (a rows-1/2 partial over the full block) rewrites every
    payload byte any append wrote inside its block -- the property that
    makes truncation into a sealed block a pure re-quantize."""
    for blk in (0, 1):
        seal_pay, _ = vbuild_sg1_block_mask(blk)
        for p in range(32):
            apay, _ = vbuild_sg1_append_mask(blk, p)
            for i in range(64 * 64):
                assert not (apay[i] and not seal_pay[i]), (blk, p, i)


@pytest.mark.parametrize('rows,blk', [(18, 0), (19, 1)])
@pytest.mark.parametrize('p', [0, 1, 15, 16, 31])
def test_vbuild_sg1_append_matches_model(rows, blk, p):
    """rows == 18/19 (the V append): one 128-B slot row re-based at the
    buffer, ONE per-token scale over its 64 dims, the drain patching
    block blk's 32 payload beats (two bytes each, pre-read composed)
    plus the widened-d16 word of the four-beat F32 scale row at
    m_base. Everything else keeps the host-loaded previous appends'
    bytes."""
    rng = random.Random(5200 + rows * 40 + p)
    slot = gen_slot(rng)
    params = dict(PARAMS, data_width=512)
    beats, res = run_build(params,
                           slot,
                           p + 1 + 32 * blk,
                           a_base=P_BASE,
                           m_base=M_BASE,
                           rows=rows,
                           retain=True)
    assert res['status'] is Status.OK
    hp = HasTRoomParams(params)
    pay, srow = images_over(beats, hp, retain_pattern(64 * 64),
                            retain_pattern(64 * 4))
    ref_pay, ref_srow = vbuild_sg1_append(slot, blk, p,
                                          retain_pattern(64 * 64),
                                          retain_pattern(64 * 4))
    assert pay == ref_pay, first_diff(pay, ref_pay)
    assert srow == ref_srow, first_diff(srow, ref_srow)
    # 32 patched payload beats + the four scale-row beats.
    assert len(beats) == 36
    # Rate guard: the append is the K-partial class, not a rebuild.
    assert res['counters']['run'] < 2500, res['counters']
    assert res['counters']['quant'] <= 96


def test_vbuild_sg1_append_zero_slot():
    """A fully zero appended slot: the widened d16 0 lands in the scale
    row's word and 64 zero bytes in the patch positions; retained
    bytes untouched."""
    rng = random.Random(5250)
    slot = [0x0000] * 64
    params = dict(PARAMS, data_width=512)
    beats, res = run_build(params,
                           slot,
                           20,
                           a_base=P_BASE,
                           m_base=M_BASE,
                           rows=18,
                           retain=True)
    assert res['status'] is Status.OK
    hp = HasTRoomParams(params)
    pay, srow = images_over(beats, hp, retain_pattern(64 * 64),
                            retain_pattern(64 * 4))
    ref_pay, ref_srow = vbuild_sg1_append(slot, 0, 19, retain_pattern(64 * 64),
                                          retain_pattern(64 * 4))
    assert pay == ref_pay
    assert srow == ref_srow
    # The zero-amax skip: no quant stream ran.
    assert res['counters']['quant'] < 8


def test_vbuild_sg1_append_nan_slot():
    """NaN dims saturate to the model's bytes on the patched positions;
    the retained cells never see them."""
    rng = random.Random(5251)
    slot = gen_slot(rng, specials=False)
    slot[3] = 0x7e00
    slot[33] = 0xfd00
    params = dict(PARAMS, data_width=512)
    beats, res = run_build(params,
                           slot,
                           40,
                           a_base=P_BASE,
                           m_base=M_BASE,
                           rows=19,
                           retain=True)
    assert res['status'] is Status.OK
    hp = HasTRoomParams(params)
    pay, srow = images_over(beats, hp, retain_pattern(64 * 64),
                            retain_pattern(64 * 4))
    ref_pay, ref_srow = vbuild_sg1_append(slot, 1, 7, retain_pattern(64 * 64),
                                          retain_pattern(64 * 4))
    assert pay == ref_pay, first_diff(pay, ref_pay)
    assert srow == ref_srow, first_diff(srow, ref_srow)


def test_vbuild_sg1_append_then_append():
    """The deployment shape: a second append composes over the first's
    output images (loaded back into staging), exactly the incremental
    open tail the emission slice will drive."""
    rng = random.Random(5252)
    params = dict(PARAMS, data_width=512)
    hp = HasTRoomParams(params)
    pay0, srow0 = retain_pattern(64 * 64), retain_pattern(64 * 4)
    for p in (5, 6):
        slot = gen_slot(rng)
        beats, res = run_build(params,
                               slot,
                               p + 1,
                               a_base=P_BASE,
                               r_base=R_BASE,
                               m_base=M_BASE,
                               rows=18,
                               retain_images=((R_BASE, pay0), (M_BASE, srow0)))
        assert res['status'] is Status.OK
        pay, srow = images_over(beats, hp, pay0, srow0)
        ref_pay, ref_srow = vbuild_sg1_append(slot, 0, p, pay0, srow0)
        assert pay == ref_pay, (p, first_diff(pay, ref_pay))
        assert srow == ref_srow, (p, first_diff(srow, ref_srow))
        pay0, srow0 = ref_pay, ref_srow


@pytest.mark.parametrize(
    'rows,k,m_base,want',
    [
        # The partial codes are SG1-only with block-scoped k ranges.
        (1, 33, M_BASE, Status.BAD_SHAPE),  # block-0 extent, k > 32
        (1, 0, M_BASE, Status.BAD_SHAPE),
        (1, 32, 0, Status.BAD_SHAPE),  # partial requires the SG1 drain
        (2, 32, M_BASE, Status.BAD_SHAPE),  # block-1 extent, k <= 32
        (2, 65, M_BASE, Status.BAD_SHAPE),
        (2, 33, 0, Status.BAD_SHAPE),
        (17, 0, M_BASE, Status.BAD_SHAPE),  # K row extent: 1 <= k <= 16
        (17, 17, M_BASE, Status.BAD_SHAPE),
        (17, 2, 0, Status.BAD_SHAPE),
        (3, 33, M_BASE, Status.BAD_SHAPE),  # unknown rows code
        # The append codes are SG1-only with block-scoped k ranges.
        (18, 0, M_BASE, Status.BAD_SHAPE),  # append block 0: 1 <= k <= 32
        (18, 33, M_BASE, Status.BAD_SHAPE),
        (18, 5, 0, Status.BAD_SHAPE),
        (19, 32, M_BASE, Status.BAD_SHAPE),  # append block 1: 33 <= k <= 64
        (19, 65, M_BASE, Status.BAD_SHAPE),
        (19, 40, 0, Status.BAD_SHAPE),
        # The scale-row extent is four 64 B beats, so a beat-aligned
        # m_base cannot overflow the 64 KiB TMEM (as with the meta); the
        # m gate shows as the alignment case below.
        (19, 40, M_BASE + 4, Status.BAD_ALIGN),
    ])
def test_vbuild_partial_rejects(rows, k, m_base, want):
    window = [0x0000] * 64 * 64
    params = dict(PARAMS, data_width=512)
    beats, res = run_build(params,
                           window,
                           k,
                           m_base=m_base,
                           rows=rows,
                           deadline=100)
    assert res['status'] is want
    assert beats == []


@pytest.mark.parametrize(
    'live,rows,k,a_base,r_base,m_base,want',
    [
        # rows must be 0 or 16 for VBUILD; anything else is a shape error.
        (16, 5, 16, A_BASE, R_BASE, M_BASE, Status.BAD_SHAPE),
        (16, 64, 16, A_BASE, R_BASE, M_BASE, Status.BAD_SHAPE),
        # K mode requires the SG1 drain and k <= 16.
        (16, 16, 16, A_BASE, R_BASE, 0, Status.BAD_SHAPE),
        (16, 16, 17, A_BASE, R_BASE, M_BASE, Status.BAD_SHAPE),
        # Group staging extents: payload 1024 B (a beat-aligned meta base
        # cannot overflow the 64 KiB TMEM by 64 B, so the meta gate shows
        # as the alignment case below).
        (16, 16, 16, A_BASE, 0xff00, M_BASE, Status.BAD_RANGE),
        (16, 16, 16, A_BASE + 4, R_BASE, M_BASE, Status.BAD_ALIGN),
        (16, 16, 16, A_BASE, R_BASE, M_BASE + 4, Status.BAD_ALIGN),
    ])
def test_vbuild_k_rejects(live, rows, k, a_base, r_base, m_base, want):
    group = [0x0000] * 16 * 64
    params = dict(PARAMS, data_width=512)
    beats, res = run_build(params,
                           group,
                           k,
                           a_base=a_base,
                           r_base=r_base,
                           m_base=m_base,
                           rows=rows,
                           deadline=100)
    assert res['status'] is want
    assert beats == []


@pytest.mark.parametrize(
    'live,k,a_base,r_base,m_base,want',
    [
        (64, 64, A_BASE, R_BASE, M_BASE + 4, Status.BAD_ALIGN),
        (64, 64, A_BASE, R_BASE, 0xff80, Status.BAD_RANGE),  # meta extent
        (64, 64, A_BASE, 0xf800, M_BASE, Status.BAD_RANGE),  # payload extent
        (64, 0, A_BASE, R_BASE, M_BASE, Status.BAD_SHAPE),
    ])
def test_vbuild_sg1_rejects(live, k, a_base, r_base, m_base, want):
    window = [0x0000] * 64 * 64
    params = dict(PARAMS, data_width=512)
    beats, res = run_build(params,
                           window,
                           k,
                           a_base=a_base,
                           r_base=r_base,
                           m_base=m_base,
                           deadline=100)
    assert res['status'] is want
    assert beats == []


@pytest.mark.parametrize('k,a_base,r_base,want', [
    (0, A_BASE, R_BASE, Status.BAD_SHAPE),
    (65, A_BASE, R_BASE, Status.BAD_SHAPE),
    (64, A_BASE + 4, R_BASE, Status.BAD_ALIGN),
    (64, A_BASE, R_BASE + 4, Status.BAD_ALIGN),
    (64, 0xff000, R_BASE, Status.BAD_RANGE),
    (64, A_BASE, 0xff000, Status.BAD_RANGE),
])
def test_vbuild_rejects(k, a_base, r_base, want):
    window = [0x0000] * 64 * 64
    beats, res = run_build(dict(PARAMS),
                           window,
                           k,
                           a_base=a_base,
                           r_base=r_base,
                           deadline=100)
    assert res['status'] is want
    assert beats == []


# ---------------------------------------------------------------------
# Q3c direct-append gates (2026-11-10,
# plans/2026-11-09-troom-qwen3-hill-climb.md): the wide per-token V
# append (candidate A of the frozen contract: one FP16 scale per token
# over all 128 dims, quantized once at append) and the K append (rows
# 20), against the generalized model twins. The RTL byte contracts are
# results/troom-qwen3-append-q3c-20261110/DESIGN.md.
# ---------------------------------------------------------------------

WIDE_PARAMS = dict(PARAMS,
                   data_width=512,
                   act_max_k=4096,
                   vbuild_max_head_dim=128)


def gen_slot128(rng, specials=True):
    """One appended slot's 128 FP16 dim patterns."""
    return [
        rng.choice(SPECIAL_16) if specials and rng.random() < 0.05 else
        (rng.randint(0, 1) << 15) | (rng.randint(1, 30) << 10)
        | rng.randint(0, 0x3ff) for _ in range(128)
    ]


def images_over_bases(beats, hp, pay0, met0, r_base, m_base, skip=()):
    """images_over with explicit staging bases (placement variants).

    ``skip`` names address ranges handled by the caller (e.g. the K4
    state staging at m_base + 512) -- their beats are tolerated here.
    """
    pay = bytearray(pay0)
    met = bytearray(met0)
    for addr, data in beats:
        for i in range(hp.data_width // 8):
            if r_base <= addr < r_base + len(pay):
                pay[addr - r_base + i] = (data >> (8 * i)) & 0xff
            elif m_base <= addr < m_base + len(met):
                met[addr - m_base + i] = (data >> (8 * i)) & 0xff
            elif any(lo <= addr < hi for lo, hi in skip):
                continue
            else:
                raise AssertionError(hex(addr))
    return bytes(pay), bytes(met)


class SplitHarness(Elaboratable):
    """VBuildUnit against split TMEM with the core's cluster routing.

    Each beat -- the unit's and the test setup's -- goes to the
    cluster its address names, exactly as core.py routes the shared
    ports, so staging images may live in either TMEM half (the
    backend's TROOM_TMEM_SPLIT placement choice).
    """

    def __init__(self, params):
        self.params = params
        self.tmem = TMEM(params)
        self.dut = VBuildUnit(params)
        hp = self.dut
        self.write = Decoupled(Record,
                               [('addr', hp.tmem_addr_width, Direction.FANOUT),
                                ('data', hp.data_width, Direction.FANOUT)])

    def elaborate(self, platform):
        m = Module()
        m.submodules.tmem = self.tmem
        m.submodules.dut = self.dut
        hp = HasTRoomParams(self.params)
        half = log2_int(hp.tmem_bytes // 2)

        # One merged write source: the test setup's beats strictly
        # precede the run (the descriptor is offered only after the
        # images land), so setup wins any overlap by construction.
        # TMEM's write port is always ready.
        sw = self.write
        dw = self.dut.tmem_w
        wsel = Signal()
        m.d.comb += wsel.eq(sw.valid)
        w_valid = Signal()
        w_addr = Signal.like(sw.bits.addr)
        w_data = Signal.like(sw.bits.data)
        m.d.comb += [
            w_valid.eq(sw.valid | dw.valid),
            w_addr.eq(Mux(wsel, sw.bits.addr, dw.bits.addr)),
            w_data.eq(Mux(wsel, sw.bits.data, dw.bits.data)),
            sw.ready.eq(wsel),
            dw.ready.eq(~wsel),
        ]
        whi = w_addr[half]
        m.d.comb += [
            self.tmem.write.valid.eq(w_valid & ~whi),
            self.tmem.write_b.valid.eq(w_valid & whi),
            self.tmem.write.bits.addr.eq(w_addr),
            self.tmem.write_b.bits.addr.eq(w_addr),
            self.tmem.write.bits.data.eq(w_data),
            self.tmem.write_b.bits.data.eq(w_data),
        ]

        rq = self.dut.tmem_rreq
        rhi = rq.bits.addr[half]
        b_owner = Signal()
        with m.If(rq.fire):
            m.d.sync += b_owner.eq(rhi)
        m.d.comb += [
            self.tmem.rreq.valid.eq(rq.valid & ~rhi),
            self.tmem.rreq_b.valid.eq(rq.valid & rhi),
            self.tmem.rreq.bits.addr.eq(rq.bits.addr),
            self.tmem.rreq_b.bits.addr.eq(rq.bits.addr),
            rq.ready.eq(Mux(rhi, self.tmem.rreq_b.ready,
                            self.tmem.rreq.ready)),
            self.dut.tmem_rresp.valid.eq(
                Mux(b_owner, self.tmem.rresp_b.valid, self.tmem.rresp.valid)),
            self.dut.tmem_rresp.bits.data.eq(
                Mux(b_owner, self.tmem.rresp_b.bits.data,
                    self.tmem.rresp.bits.data)),
            self.tmem.rresp.ready.eq(~b_owner
                                     & self.dut.tmem_rresp.ready),
            self.tmem.rresp_b.ready.eq(b_owner
                                       & self.dut.tmem_rresp.ready),
        ]
        return m


def run_split_build(params,
                    window,
                    live,
                    a_base,
                    r_base,
                    m_base,
                    rows,
                    retain_images,
                    deadline=400000):
    """run_build over the cluster-routing harness (either half)."""
    dut = SplitHarness(params)
    beats = []
    result = {'status': None, 'counters': None}

    def tx():
        img = b''.join(p.to_bytes(2, 'little') for p in window)
        img += bytes((-len(img)) % dut.dut.beat_bytes)
        for off in range(0, len(img), dut.dut.beat_bytes):
            word = int.from_bytes(img[off:off + dut.dut.beat_bytes], 'little')
            yield from tmem_write(dut.write, a_base + off, word)
        for base, blob in retain_images:
            for off in range(0, len(blob), dut.dut.beat_bytes):
                word = int.from_bytes(blob[off:off + dut.dut.beat_bytes],
                                      'little')
                yield from tmem_write(dut.write, base + off, word)
        sink = dut.dut.sink
        yield sink.bits.a_base.eq(a_base)
        yield sink.bits.r_base.eq(r_base)
        yield sink.bits.m_base.eq(m_base)
        yield sink.bits.k.eq(live)
        yield sink.bits.rows.eq(rows)
        yield sink.valid.eq(1)
        yield
        while not (yield sink.fire):
            yield
        if (yield dut.dut.done):
            result['status'] = Status((yield dut.dut.status))
        yield sink.valid.eq(0)
        yield
        if result['status'] is None:
            for _ in range(deadline):
                yield
                if (yield dut.dut.done):
                    result['status'] = Status((yield dut.dut.status))
                    result['counters'] = {
                        'run': (yield dut.dut.run_cycles),
                    }
                    break
            else:
                raise TimeoutError('vbuild did not finish')

    def watch():
        yield Passive()
        wr = dut.dut.tmem_w
        while True:
            if (yield wr.fire):
                beats.append(((yield wr.bits.addr), (yield wr.bits.data)))
            yield

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(tx)
    sim.add_sync_process(watch)
    sim.run()
    return beats, result


def test_vbuild_wide_token_row_matches_independent():
    """The 128-dim token row against an independent reconstruction:
    one scale over all four blocks, every d16 slot equal, quants the
    RNE int8 of 127 * x / amax, and equal-amax quads agree with the
    per-block sequence."""
    rng = random.Random(6100)
    for t in range(40):
        if t < 38:
            slot = gen_slot128(rng, specials=False)
        elif t == 38:
            slot = [0x0000] * 128  # the all-zero slot
        else:
            slot = [0x8000] * 3 + [0x0000] * 61 + [0x03ff] * 64
        row = vbuild_quant_row_token(slot)
        assert len(row) == 136
        for j in range(4):
            assert row[34 * j:34 * j + 2] == row[0:2]
        vals = [widen16(h) for h in slot]
        amax = max((abs(v) for v in vals), default=0.0)
        for i, v in enumerate(vals):
            got = row[34 * (i >> 5) + 2 + (i & 31)]
            got = got - 256 if got > 127 else got
            want = 127.0 * v / amax if amax else 0.0
            assert abs(got - want) <= 1.0 + 1e-3, (t, i, got, want)
    # Equal-amax quads: the per-token and per-block sequences agree.
    for _ in range(15):
        quad = gen_slot128(rng, specials=False)[:32]
        slot = quad + [q ^ 0x8000 for q in quad] + list(quad) \
            + [q ^ 0x8000 for q in quad]
        assert vbuild_quant_row_token(slot) == vbuild_quant_row(slot)
    zero = [0x0000] * 128
    assert vbuild_quant_row_token(zero) == vbuild_quant_row(zero) \
        == bytes(136)


def test_vbuild_wide_token_row_nan_and_special_slots():
    """NaN patterns never win the fold but saturate their own bytes;
    an all-zero-plus-NaN slot still emits the zero-amax d16."""
    slot = [0x0000] * 128
    slot[7] = 0x7e00
    slot[100] = 0xfd00
    row = vbuild_quant_row_token(slot)
    assert row[0:2] == (0).to_bytes(2, 'little')
    assert row[2 + 7] == 0x7f and row[34 * 3 + 2 + 4] == 0x7f
    assert sum(sum(row[34 * j + 2:34 * j + 34]) for j in range(4)) \
        == 0x7f + 0x7f
    # A largest-finite slot rounds its max bytes to 127.
    slot = [0x7bff] * 128
    row = vbuild_quant_row_token(slot)
    assert row[2 + 0] == 0x7f and row[34 * 3 + 2 + 31] == 0x7f


def test_vbuild_wide_sg1_append_compose_and_mask():
    """The wide append composes only its masked bytes over the prior
    image, and appending every position in order builds the whole
    block from per-slot token columns."""
    rng = random.Random(6101)
    pay0 = retain_pattern(128 * 64)
    srow0 = retain_pattern(64 * 4)
    slots = [gen_slot128(rng) for _ in range(32)]
    blk, p = 1, 17
    pay, srow = vbuild_sg1_append(slots[p], blk, p, pay0, srow0)
    mpay, msrow = vbuild_sg1_append_mask(blk, p, dims=128)
    diff = [i for i in range(128 * 64) if pay[i] != pay0[i]]
    mdiff = [i for i in range(64 * 4) if srow[i] != srow0[i]]
    assert diff == [i for i in range(128 * 64) if mpay[i]]
    assert mdiff == [i for i in range(64 * 4) if msrow[i]]
    row = vbuild_quant_row_token(slots[p])
    for d in range(128):
        off = ((d >> 2) * 8 + blk * 4 + (d & 3)) * 32 + p
        assert pay[off] == row[34 * (d >> 5) + 2 + (d & 31)]
    d16 = int.from_bytes(row[0:2], 'little')
    wo = 4 * (blk * 32 + p)
    assert srow[wo:wo + 4] == f32_bits(fp16_to_f32(d16)).to_bytes(4, 'little')
    # Appending every position in order builds the whole block.
    pay, srow = bytes(128 * 64), bytes(64 * 4)
    for p in range(32):
        pay, srow = vbuild_sg1_append(slots[p], blk, p, pay, srow)
    for p in range(32):
        row = vbuild_quant_row_token(slots[p])
        for d in range(128):
            off = ((d >> 2) * 8 + blk * 4 + (d & 3)) * 32 + p
            assert pay[off] == row[34 * (d >> 5) + 2 + (d & 31)]
        d16 = int.from_bytes(row[0:2], 'little')
        wo = 4 * (blk * 32 + p)
        assert srow[wo:wo + 4] == f32_bits(fp16_to_f32(d16)).to_bytes(
            4, 'little')


@pytest.mark.parametrize('dims', [64, 128])
def test_vbuild_k_append_compose_and_mask(dims):
    """The K append patches exactly its token's chunks and cells over
    the prior group image, and appending all sixteen tokens in order
    builds the image a direct per-token placement produces."""
    rng = random.Random(6102 + dims)
    blocks = dims // 32
    pay0 = retain_pattern(16 * dims)
    met0 = retain_pattern(16 * blocks * 2)
    slot = gen_slot128(rng) if dims == 128 else gen_slot(rng)
    for k in (1, 2, 5, 9, 16):
        pay, met = vbuild_sg1_k_append(slot, k, pay0, met0)
        mpay, mmet = vbuild_sg1_k_append_mask(k, dims=dims)
        assert [i for i in range(16 * dims) if pay[i] != pay0[i]] == \
            [i for i in range(16 * dims) if mpay[i]]
        assert [i for i in range(16 * blocks * 2) if met[i] != met0[i]] == \
            [i for i in range(16 * blocks * 2) if mmet[i]]
        row = vbuild_quant_row(slot)
        r = k - 1
        for b in range(blocks):
            off = ((r >> 2) * (blocks * 4) + b * 4 + (r & 3)) * 32
            assert pay[off:off + 32] == row[34 * b + 2:34 * b + 34]
            moff = (r >> 2) * (blocks * 8) + b * 8 + (r & 3) * 2
            assert met[moff:moff + 2] == row[34 * b:34 * b + 2]
    # Appending every token in order equals direct placement.
    slots = [
        gen_slot128(rng) if dims == 128 else gen_slot(rng) for _ in range(16)
    ]
    pay, met = bytes(16 * dims), bytes(16 * blocks * 2)
    for k in range(1, 17):
        pay, met = vbuild_sg1_k_append(slots[k - 1], k, pay, met)
    for r in range(16):
        row = vbuild_quant_row(slots[r])
        for b in range(blocks):
            off = ((r >> 2) * (blocks * 4) + b * 4 + (r & 3)) * 32
            assert pay[off:off + 32] == row[34 * b + 2:34 * b + 34]
            moff = (r >> 2) * (blocks * 8) + b * 8 + (r & 3) * 2
            assert met[moff:moff + 2] == row[34 * b:34 * b + 2]


@pytest.mark.parametrize('rows,blk', [(18, 0), (19, 1)])
@pytest.mark.parametrize('p', [0, 1, 15, 16, 31])
def test_vbuild_wide_sg1_append_matches_model(rows, blk, p):
    """rows == 18/19 with bit 15 (the wide V append): one 256-B slot
    row, ONE per-token scale over all 128 dims, the drain patching
    block blk's 64 payload beats (two bytes each, pre-read composed)
    plus the four scale-row beats. No meta staging write anywhere."""
    rng = random.Random(6200 + rows * 40 + p)
    slot = gen_slot128(rng)
    params = dict(WIDE_PARAMS)
    beats, res = run_build(params,
                           slot,
                           p + 1 + 32 * blk,
                           a_base=P_BASE,
                           r_base=R_BASE,
                           m_base=M_BASE,
                           rows=VBUILD_ROWS_HD128 | rows,
                           retain_images=((R_BASE, retain_pattern(128 * 64)),
                                          (M_BASE, retain_pattern(64 * 4))))
    assert res['status'] is Status.OK
    hp = HasTRoomParams(params)
    pay, srow = images_over(beats, hp, retain_pattern(128 * 64),
                            retain_pattern(64 * 4))
    ref_pay, ref_srow = vbuild_sg1_append(slot, blk, p,
                                          retain_pattern(128 * 64),
                                          retain_pattern(64 * 4))
    assert pay == ref_pay, first_diff(pay, ref_pay)
    assert srow == ref_srow, first_diff(srow, ref_srow)
    # 64 patched payload beats + the four scale-row beats.
    assert len(beats) == 68
    # Rate guard: the append stays the patch class, not a rebuild.
    assert res['counters']['run'] < 5000, res['counters']
    assert res['counters']['quant'] <= 200


def test_vbuild_wide_sg1_append_zero_slot():
    """A fully zero appended slot: the widened d16 0 lands in the scale
    row's word and 128 zero bytes in the patch positions; the retained
    bytes are untouched and the quant stream is skipped."""
    slot = [0x0000] * 128
    params = dict(WIDE_PARAMS)
    beats, res = run_build(params,
                           slot,
                           20,
                           a_base=P_BASE,
                           r_base=R_BASE,
                           m_base=M_BASE,
                           rows=VBUILD_ROWS_HD128 | 18,
                           retain_images=((R_BASE, retain_pattern(128 * 64)),
                                          (M_BASE, retain_pattern(64 * 4))))
    assert res['status'] is Status.OK
    hp = HasTRoomParams(params)
    pay, srow = images_over(beats, hp, retain_pattern(128 * 64),
                            retain_pattern(64 * 4))
    ref_pay, ref_srow = vbuild_sg1_append(slot, 0, 19,
                                          retain_pattern(128 * 64),
                                          retain_pattern(64 * 4))
    assert pay == ref_pay
    assert srow == ref_srow
    assert res['counters']['quant'] < 8


def test_vbuild_wide_sg1_append_nan_slot():
    """NaN dims saturate to the model's bytes on the patched
    positions; the retained cells never see them."""
    rng = random.Random(6251)
    slot = gen_slot128(rng, specials=False)
    slot[3] = 0x7e00
    slot[70] = 0xfd00
    params = dict(WIDE_PARAMS)
    beats, res = run_build(params,
                           slot,
                           40,
                           a_base=P_BASE,
                           r_base=R_BASE,
                           m_base=M_BASE,
                           rows=VBUILD_ROWS_HD128 | 19,
                           retain_images=((R_BASE, retain_pattern(128 * 64)),
                                          (M_BASE, retain_pattern(64 * 4))))
    assert res['status'] is Status.OK
    hp = HasTRoomParams(params)
    pay, srow = images_over(beats, hp, retain_pattern(128 * 64),
                            retain_pattern(64 * 4))
    ref_pay, ref_srow = vbuild_sg1_append(slot, 1, 7, retain_pattern(128 * 64),
                                          retain_pattern(64 * 4))
    assert pay == ref_pay, first_diff(pay, ref_pay)
    assert srow == ref_srow, first_diff(srow, ref_srow)


def test_vbuild_wide_sg1_append_then_append():
    """The deployment shape at 128 dims: consecutive appends compose
    over the previous output images, across the block-0/1 boundary
    (live 32 -> 33) and within a block."""
    rng = random.Random(6252)
    params = dict(WIDE_PARAMS)
    hp = HasTRoomParams(params)
    pay0, srow0 = retain_pattern(128 * 64), retain_pattern(64 * 4)
    for rows, p in ((VBUILD_ROWS_HD128 | 18, 31), (VBUILD_ROWS_HD128 | 19, 0),
                    (VBUILD_ROWS_HD128 | 19, 1)):
        slot = gen_slot128(rng)
        beats, res = run_build(params,
                               slot,
                               p + 1 + 32 * (rows & 1),
                               a_base=P_BASE,
                               r_base=R_BASE,
                               m_base=M_BASE,
                               rows=rows,
                               retain_images=((R_BASE, pay0), (M_BASE, srow0)))
        assert res['status'] is Status.OK
        pay, srow = images_over(beats, hp, pay0, srow0)
        ref_pay, ref_srow = vbuild_sg1_append(slot, rows & 1, p, pay0, srow0)
        assert pay == ref_pay, (p, first_diff(pay, ref_pay))
        assert srow == ref_srow, (p, first_diff(srow, ref_srow))
        pay0, srow0 = ref_pay, ref_srow


@pytest.mark.parametrize('wide', [False, True])
@pytest.mark.parametrize('k', [1, 2, 5, 8, 15, 16])
def test_vbuild_k_append_matches_model(wide, k):
    """rows == 20 (the K append): the token's row quantizes with the
    frozen per-block K sequence and patches its chunks and meta cells
    into the group image beat-composed; every neighboring byte keeps
    the host-loaded value."""
    dims = 128 if wide else 64
    rng = random.Random(6300 + k + dims)
    slot = gen_slot128(rng) if wide else gen_slot(rng)
    params = dict(PARAMS, data_width=512)
    if wide:
        params.update(vbuild_max_head_dim=128, act_max_k=4096)
    pay0 = retain_pattern(16 * dims)
    met0 = retain_pattern(16 * (dims // 32) * 2)
    beats, res = run_build(params,
                           slot,
                           k,
                           a_base=P_BASE,
                           r_base=R_BASE,
                           m_base=M_BASE,
                           rows=VB_ROWS_K_APP
                           | (VBUILD_ROWS_HD128 if wide else 0),
                           retain_images=((R_BASE, pay0), (M_BASE, met0)))
    assert res['status'] is Status.OK
    hp = HasTRoomParams(params)
    pay, met = images_over(beats, hp, pay0, met0)
    ref_pay, ref_met = vbuild_sg1_k_append(slot, k, pay0, met0)
    assert pay == ref_pay, first_diff(pay, ref_pay)
    assert met == ref_met, first_diff(met, ref_met)
    # One beat per 32-dim block plus the meta beat.
    assert len(beats) == dims // 32 + 1
    # Rate guard: quantize one row, patch a handful of beats.
    assert res['counters']['run'] < 1600, res['counters']


def test_vbuild_k_append_then_append_and_overwrite():
    """Consecutive K appends compose (each token's extents disjoint),
    and re-appending the same k re-quantizes exactly that token's
    bytes while every other cell is byte-identical -- the lifecycle's
    overwrite primitive."""
    rng = random.Random(6350)
    params = dict(WIDE_PARAMS)
    hp = HasTRoomParams(params)
    pay0, met0 = retain_pattern(16 * 128), retain_pattern(16 * 4 * 2)
    for k in (3, 4, 12):
        pay, met = pay0, met0
        for _rep in range(2):
            slot = gen_slot128(rng)
            beats, res = run_build(params,
                                   slot,
                                   k,
                                   a_base=P_BASE,
                                   r_base=R_BASE,
                                   m_base=M_BASE,
                                   rows=VB_ROWS_K_APP | VBUILD_ROWS_HD128,
                                   retain_images=((R_BASE, pay), (M_BASE,
                                                                  met)))
            assert res['status'] is Status.OK
            ref_pay, ref_met = vbuild_sg1_k_append(slot, k, pay, met)
            got_pay, got_met = images_over(beats, hp, pay, met)
            assert got_pay == ref_pay, (k, first_diff(got_pay, ref_pay))
            assert got_met == ref_met, (k, first_diff(got_met, ref_met))
            pay, met = ref_pay, ref_met
        # Only the token's extents ever moved from the retained image.
        mpay, mmet = vbuild_sg1_k_append_mask(k, dims=128)
        for i in range(len(pay0)):
            assert bool(mpay[i]) or pay[i] == pay0[i], (k, i)
        for i in range(len(met0)):
            assert bool(mmet[i]) or met[i] == met0[i], (k, i)


@pytest.mark.parametrize('placement', ['lower', 'upper'])
def test_vbuild_append_both_tmem_placements(placement):
    """The wide V append and the K append compose identically with
    their staging in either TMEM half -- the backend's two staging
    placements (TROOM_TMEM_SPLIT 0/1) at the unit level."""
    upper = placement == 'upper'
    a_base = 0x8100 if upper else P_BASE
    r_base = 0x8200 if upper else R_BASE
    m_base = 0xA400 if upper else M_BASE
    params = dict(WIDE_PARAMS)
    hp = HasTRoomParams(params)
    rng = random.Random(6400 if upper else 6401)

    slot = gen_slot128(rng)
    pay0, srow0 = retain_pattern(128 * 64), retain_pattern(64 * 4)
    beats, res = run_split_build(params, slot, 18, a_base, r_base, m_base,
                                 VBUILD_ROWS_HD128 | 18,
                                 ((r_base, pay0), (m_base, srow0)))
    assert res['status'] is Status.OK
    pay, srow = images_over_bases(beats, hp, pay0, srow0, r_base, m_base)
    ref_pay, ref_srow = vbuild_sg1_append(slot, 0, 17, pay0, srow0)
    assert pay == ref_pay, first_diff(pay, ref_pay)
    assert srow == ref_srow, first_diff(srow, ref_srow)

    kslot = gen_slot128(rng)
    kpay0, kmet0 = retain_pattern(16 * 128), retain_pattern(16 * 4 * 2)
    beats, res = run_split_build(params, kslot, 6, a_base, r_base, m_base,
                                 VB_ROWS_K_APP | VBUILD_ROWS_HD128,
                                 ((r_base, kpay0), (m_base, kmet0)))
    assert res['status'] is Status.OK
    kpay, kmet = images_over_bases(beats, hp, kpay0, kmet0, r_base, m_base)
    ref_kpay, ref_kmet = vbuild_sg1_k_append(kslot, 6, kpay0, kmet0)
    assert kpay == ref_kpay, first_diff(kpay, ref_kpay)
    assert kmet == ref_kmet, first_diff(kmet, ref_kmet)


@pytest.mark.parametrize(
    'rows,k,m_base,params_over,want',
    [
        # The K append code is SG1-only with the group's k range.
        (VB_ROWS_K_APP, 0, M_BASE, {}, Status.BAD_SHAPE),
        (VB_ROWS_K_APP, 17, M_BASE, {}, Status.BAD_SHAPE),
        (VB_ROWS_K_APP, 5, 0, {}, Status.BAD_SHAPE),
        (VB_ROWS_K_APP, 5, M_BASE + 4, {}, Status.BAD_ALIGN),
        # Append codes are rejected on production builds with native
        # append disabled, and the wide V append needs the wide build.
        (VB_ROWS_K_APP, 5, M_BASE, dict(vbuild_native_append=False),
         Status.BAD_SHAPE),
        (VBUILD_ROWS_HD128 | 18, 5, M_BASE,
         dict(vbuild_native_append=False,
              vbuild_max_head_dim=128,
              act_max_k=4096), Status.BAD_SHAPE),
        (VBUILD_ROWS_HD128 | 18, 5, M_BASE, {}, Status.BAD_SHAPE),
    ])
def test_vbuild_q3c_append_rejects(rows, k, m_base, params_over, want):
    window = [0x0000] * 128
    params = dict(PARAMS, data_width=512, **params_over)
    beats, res = run_build(params,
                           window,
                           k,
                           a_base=P_BASE,
                           r_base=R_BASE,
                           m_base=m_base,
                           rows=rows,
                           deadline=100)
    assert res['status'] is want
    assert beats == []


# ---------------------------------------------------------------------
# D2 K4 selective same-contract requant (2026-11-12,
# plans/2026-11-09-troom-qwen3-hill-climb.md; VBUILD rows 3|WIDE):
# one slot appended to the open block with only risen-max rows
# re-quantized; byte-identical to the temporal rebuild at every live
# prefix. results/troom-qwen3-kv-k4-20261112/DESIGN.md.
# ---------------------------------------------------------------------

K4_PARAMS = dict(WIDE_PARAMS, vbuild_k4_selective=True)
K4_ROWS = 3 | VBUILD_ROWS_HD128
K4_HEAL_BIT = 1 << 7
ST_BASE = M_BASE + 512  # the state staging follows the wide meta image


def k4_images_over(beats, hp, pay0, met0, st0, blk):
    """The three composed staging images of a K4 run: the full
    payload tile (the open block's beats at g*256 + blk*128), the meta
    image, and the open block's state at M_BASE + 512 + blk*1024."""
    pay = bytearray(pay0)
    met = bytearray(met0)
    st = bytearray(st0)
    st_base = ST_BASE + blk * 1024
    for addr, data in beats:
        for i in range(hp.data_width // 8):
            if R_BASE <= addr < R_BASE + len(pay):
                pay[addr - R_BASE + i] = (data >> (8 * i)) & 0xff
            elif M_BASE <= addr < M_BASE + len(met):
                met[addr - M_BASE + i] = (data >> (8 * i)) & 0xff
            elif st_base <= addr < st_base + len(st):
                st[addr - st_base + i] = (data >> (8 * i)) & 0xff
            else:
                raise AssertionError(hex(addr))
    return bytes(pay), bytes(met), bytes(st)


def k4_chain(rng, kmax, specials=True):
    """Run a full K4 chain k = 1..kmax from zero images; yield each
    (k, beats, result) with the model's expected images."""
    import tests.troom.model as M
    from troom.vbuild import VB_ROWS_V_K4
    assert VB_ROWS_V_K4 == 3
    win = []
    for j in range(64):
        row = gen_slot128(rng, specials) if j < kmax else [0] * 128
        win.extend(row)
    pay, met = bytes(128 * 64), bytes(128 * 4)
    sts = [bytes(128 * 8), bytes(128 * 8)]
    for k in range(1, kmax + 1):
        blk = 1 if k > 32 else 0
        beats, res = run_build(dict(K4_PARAMS),
                               win[:k * 128],
                               k,
                               a_base=P_BASE,
                               r_base=R_BASE,
                               m_base=M_BASE,
                               rows=K4_ROWS,
                               retain_images=((R_BASE, pay), (M_BASE, met),
                                              (ST_BASE + blk * 1024,
                                               sts[blk])))
        exp_pay, exp_met, exp_st = vbuild_sg1_k4_append(
            win, k, pay, met, sts[blk])
        yield k, beats, res, (exp_pay, exp_met, exp_st)
        pay, met = exp_pay, exp_met
        sts[blk] = exp_st


@pytest.mark.parametrize('k', [1, 2, 5, 17, 32, 33, 40, 64])
def test_vbuild_k4_append_matches_model(k):
    """One K4 append over random prior images equals the model's
    composed images, writes exactly 64+8+16 beats, and the counters
    hold the D1 partition identity."""
    rng = random.Random(7100 + k)
    win = []
    for j in range(64):
        win.extend(gen_slot128(rng) if j < k else [0] * 128)
    blk = 1 if k > 32 else 0
    pay0 = retain_pattern(128 * 64)
    met0 = retain_pattern(128 * 4)
    # a prior state from the temporal extraction (the honest case);
    # at a block boundary (k == 33) the new block's state is zero
    st0 = vbuild_sg1_k4_state(win, k - 1) \
        if k > 1 and k != 33 else bytes(128 * 8)
    beats, res = run_build(dict(K4_PARAMS),
                           win[:k * 128],
                           k,
                           a_base=P_BASE,
                           r_base=R_BASE,
                           m_base=M_BASE,
                           rows=K4_ROWS,
                           retain_images=((R_BASE, pay0), (M_BASE, met0),
                                          (ST_BASE + blk * 1024, st0)))
    assert res['status'] is Status.OK
    hp = HasTRoomParams(K4_PARAMS)
    pay, met, st = k4_images_over(beats, hp, pay0, met0, st0, blk)
    exp = vbuild_sg1_k4_append(win, k, pay0, met0, st0)
    assert pay == exp[0], first_diff(pay, exp[0])
    assert met == exp[1], first_diff(met, exp[1])
    assert st == exp[2], first_diff(st, exp[2])
    assert len(beats) == 64 + 8 + 16
    c = res['counters']
    assert c['active'] == c['fill'] + (c['gather'] - c['prefetch']) \
        + c['scale'] + c['quant'] + c['drain'] + c['dispatch'] \
        + c['tiles'], c
    assert c['rows'] == 128 and c['tiles'] == 1


def k4_run(params, win, k, pay, met, st, heal=False):
    """One K4 run over the given prior full images; the payload
    staging carries the open block's slice. Returns (beats, res)."""
    blk = 1 if k > 32 else 0
    # The heal flag rides k bit 7 (VB_K4_HEAL), not rows.
    return run_build(params,
                     win[:k * 128],
                     k | (128 if heal else 0),
                     a_base=P_BASE,
                     r_base=R_BASE,
                     m_base=M_BASE,
                     rows=K4_ROWS,
                     retain_images=((R_BASE, pay), (M_BASE, met),
                                    (ST_BASE + blk * 1024, st)))


def k4_temporal(win, k):
    """The temporal full rebuild's images at live prefix k."""
    w = list(win[:k * 128]) + [0] * ((64 - k) * 128)
    return vbuild_sg1_wide(w)


@pytest.mark.parametrize('k,pattern', [
    (2, 'all'),
    (2, 'quick'),
    (2, 'alternating'),
    (17, 'boundaries'),
    (33, 'all'),
    (34, 'alternating'),
])
def test_vbuild_k4_interleaved_chains_progress(k, pattern):
    """Exhaust/reuse chain contexts and wrap the dispatch FIFO with
    deterministic quick/risen mixes, including state/meta bank edges.
    A bounded launch catches repeated grants to an in-flight Newton op.
    """
    win = [0x3800 + d * 8 for _ in range(64) for d in range(128)]
    for d in range(128):
        risen = (pattern == 'all' or (pattern == 'alternating' and d % 2 == 0)
                 or (pattern == 'boundaries' and d % 16 in (0, 7, 8, 15)))
        win[(k - 1) * 128 + d] = (0x4000 + d * 8 if risen else 0x3400 + d * 4)
    # Distinct row arithmetic also exercises the shared unit grants;
    # infinity takes the reciprocal short circuit and NaN never rises.
    if pattern != 'quick':
        win[(k - 1) * 128 + 3] = 0x7c00
        win[(k - 1) * 128 + 9] = 0x7e00
    pay, met = bytes(128 * 64), bytes(128 * 4)
    states = [bytes(128 * 8), bytes(128 * 8)]
    for step in range(1, k):
        blk = int(step > 32)
        pay, met, states[blk] = vbuild_sg1_k4_append(win, step, pay, met,
                                                     states[blk])
    blk = int(k > 32)
    st = states[blk]
    beats, res = run_build(dict(K4_PARAMS),
                           win[:k * 128],
                           k,
                           a_base=P_BASE,
                           r_base=R_BASE,
                           m_base=M_BASE,
                           rows=K4_ROWS,
                           retain_images=((R_BASE, pay), (M_BASE, met),
                                          (ST_BASE + blk * 1024, st)),
                           deadline=12000,
                           write_stalls=pattern != 'all')
    assert res['status'] is Status.OK
    expected = vbuild_sg1_k4_append(win, k, pay, met, st)
    got = k4_images_over(beats, HasTRoomParams(K4_PARAMS), pay, met, st, blk)
    for actual, reference in zip(got, expected):
        assert actual == reference, first_diff(actual, reference)
    assert len(beats) == 64 + 8 + 16
    c = res['counters']
    assert c['active'] == (c['fill'] + c['gather'] - c['prefetch'] +
                           c['scale'] + c['quant'] + c['drain'] +
                           c['dispatch'] + c['tiles']), c


@pytest.mark.parametrize('k', [1, 5, 32, 33, 64])
def test_vbuild_k4_temporal_identity_at_k(k):
    """A K4-maintained window equals the temporal full rebuild at
    live prefix k (the chain from zero images, including the birth
    append and the block-1 boundary), and the state equals the
    temporal extraction."""
    rng = random.Random(7200 + k)
    win = []
    for j in range(64):
        win.extend(gen_slot128(rng) if j < k else [0] * 128)
    pay, met = bytes(128 * 64), bytes(128 * 4)
    sts = [bytes(128 * 8), bytes(128 * 8)]
    hp = HasTRoomParams(K4_PARAMS)
    for step in range(1, k + 1):
        blk = 1 if step > 32 else 0
        beats, res = k4_run(K4_PARAMS, win, step, pay, met, sts[blk])
        assert res['status'] is Status.OK, step
        pay, met, st_new = vbuild_sg1_k4_append(win, step, pay, met, sts[blk])
        got = k4_images_over(beats, hp, pay, met, st_new, blk)
        assert got == (pay, met, st_new), step
        sts[blk] = st_new
    ref_pay, ref_met = k4_temporal(win, k)
    assert pay == ref_pay, (k, first_diff(pay, ref_pay))
    assert met == ref_met, (k, first_diff(met, ref_met))
    blk = 1 if k > 32 else 0
    assert sts[blk] == vbuild_sg1_k4_state(win, k)


@pytest.mark.parametrize('heal_k', [8, 40])
def test_vbuild_k4_heal_after_corrupt_state(heal_k):
    """A stale state (a fallback rebuild happened) is re-derived by
    the heal flag: every row requantizes and the images equal the
    temporal rebuild regardless of the prior state bytes."""
    rng = random.Random(7300 + heal_k)
    win = []
    for j in range(64):
        win.extend(gen_slot128(rng) if j < heal_k else [0] * 128)
    # an honest chain up to heal_k-1, then a corrupted state
    pay, met = bytes(128 * 64), bytes(128 * 4)
    sts = [bytes(128 * 8), bytes(128 * 8)]
    hp = HasTRoomParams(K4_PARAMS)
    for step in range(1, heal_k):
        blk = 1 if step > 32 else 0
        beats, res = k4_run(K4_PARAMS, win, step, pay, met, sts[blk])
        pay, met, sts[blk] = vbuild_sg1_k4_append(win, step, pay, met,
                                                  sts[blk])
    blk = 1 if heal_k > 32 else 0
    stale = retain_pattern(128 * 8)
    beats, res = k4_run(K4_PARAMS, win, heal_k, pay, met, stale, heal=True)
    assert res['status'] is Status.OK
    got = k4_images_over(beats, hp, pay, met, stale, blk)
    ref_pay, ref_met = k4_temporal(win, heal_k)
    # The state is beat-RMW: heal re-derives every row's pattern and r
    # (the temporal extraction's values) while the reserved halfword
    # keeps the staged image's bytes, exactly like the append model.
    ref_st = vbuild_sg1_k4_append(win, heal_k, pay, met, stale, heal=True)[2]
    assert got[0] == ref_pay, first_diff(got[0], ref_pay)
    assert got[1] == ref_met, first_diff(got[1], ref_met)
    assert got[2] == ref_st, first_diff(got[2], ref_st)
    for d in range(128):
        row = got[2][d * 8:d * 8 + 8]
        exp_row = vbuild_sg1_k4_state(win, heal_k)[d * 8:d * 8 + 8]
        assert row[:2] == exp_row[:2], d
        assert row[4:] == exp_row[4:], d


@pytest.mark.parametrize('k,write_stalls', [
    (1, False),
    (2, True),
    (16, False),
    (32, False),
    (33, False),
    (40, True),
    (64, True),
    (2, 'state'),
])
def test_vbuild_k4_heal_prefetch_banks(k, write_stalls):
    """Heal ignores stale maxima/r, preserves reserved state, and folds
    every live element before publishing a bank to the scale engine.
    Adjacent rows differ, including zero-amax/NaN and a last-slot winner.
    """
    rng = random.Random(7900 + k)
    win = [0] * (64 * 128)
    blk = int(k > 32)
    first = blk * 32
    live = k - first
    for j in range(first, k):
        for d in range(128):
            bits = 0x3400 + d * 8 + (j - first) * 4
            kind = d % 8
            if kind == 0:
                bits = 0x8000 if j % 2 else 0  # signed-zero amax
            elif kind == 1:
                bits = 0x7e00  # all-NaN: amax 0, quant still sees NaNs
            elif kind == 2:
                bits = 0x7c00 if j == first else 0x3c00
            elif kind == 3:
                bits = 1 + (j - first)  # subnormal reciprocal chain
            elif kind == 4:
                bits = 0x4000 + d * 8 if j == k - 1 else 0x3000
            elif kind == 5:
                bits |= 0x8000
            elif kind == 6:
                bits = 0x7e00 if j == first else bits
            win[j * 128 + d] = bits
    pay = bytes(rng.randrange(256) for _ in range(128 * 64))
    met = bytes(rng.randrange(256) for _ in range(128 * 4))
    st = bytes(rng.randrange(256) for _ in range(128 * 8))
    placement = 0x8000 if k >= 40 else 0
    params = dict(K4_PARAMS, tmem_bytes=128 * 1024)
    base_p, base_r, base_m = (P_BASE + placement, R_BASE + placement,
                              M_BASE + placement)
    beats, res = run_build(params,
                           win[:k * 128],
                           k | K4_HEAL_BIT,
                           a_base=base_p,
                           r_base=base_r,
                           m_base=base_m,
                           rows=K4_ROWS,
                           retain_images=((base_r, pay), (base_m, met),
                                          (base_m + 512 + blk * 1024, st)),
                           deadline=16000,
                           write_stalls=write_stalls)
    assert res['status'] is Status.OK
    normalized = [(addr - placement, data) for addr, data in beats]
    got = k4_images_over(normalized, HasTRoomParams(params), pay, met, st, blk)
    expected = vbuild_sg1_k4_append(win, k, pay, met, st, heal=True)
    for actual, reference in zip(got, expected):
        assert actual == reference, first_diff(actual, reference)
    assert len(beats) == 88
    c = res['counters']
    assert c['active'] == (c['fill'] + c['gather'] - c['prefetch'] +
                           c['scale'] + c['quant'] + c['drain'] +
                           c['dispatch'] + c['tiles']), c
    assert c['rows'] == 128 and c['tiles'] == 1
    # Require useful overlap, relative to the serial chain alone. This
    # includes bank backpressure at live=1 and the longest live=32 rows.
    assert c['run'] < 128 * (39 + live + 9), c


def test_vbuild_k4_heal_and_append_relaunch():
    """Switch prongs and blocks on one DUT, including live-count shrink.
    Prefetch bank ownership, contexts and landing tags must reset per run.
    """
    hp = HasTRoomParams(K4_PARAMS)
    dut = Harness(K4_PARAMS)
    beats = []
    rng = random.Random(7980)
    win = [p for _ in range(64) for p in gen_slot128(rng)]

    def tx():
        pay, met = bytes(128 * 64), bytes(128 * 4)
        states = [bytes(128 * 8), bytes(128 * 8)]
        for launch, (k, heal) in enumerate(
            ((2, True), (3, False), (33, True), (34, False), (1, True),
             (2, False)),
                start=1):
            blk = int(k > 32)
            st = states[blk]
            image = b''.join(p.to_bytes(2, 'little') for p in win[:k * 128])
            for base, blob in ((P_BASE, image), (R_BASE, pay), (M_BASE, met),
                               (ST_BASE + blk * 1024, st)):
                for off in range(0, len(blob), hp.beat_bytes):
                    word = int.from_bytes(blob[off:off + hp.beat_bytes],
                                          'little')
                    yield from tmem_write_routed(dut.tmem, base + off, word)
            begin = len(beats)
            sink = dut.dut.sink
            yield sink.bits.a_base.eq(P_BASE)
            yield sink.bits.r_base.eq(R_BASE)
            yield sink.bits.m_base.eq(M_BASE)
            yield sink.bits.k.eq(k | (K4_HEAL_BIT if heal else 0))
            yield sink.bits.rows.eq(K4_ROWS)
            yield sink.valid.eq(1)
            yield
            for _ in range(100):
                if (yield sink.fire):
                    break
                yield
            else:
                raise TimeoutError('K4 relaunch was not accepted')
            yield sink.valid.eq(0)
            yield
            for _ in range(16000):
                if (yield dut.dut.done):
                    assert Status((yield dut.dut.status)) is Status.OK
                    yield
                    break
                yield
            else:
                raise TimeoutError(f'K4 relaunch did not finish: {k}, {heal}')
            emitted = beats[begin:]
            assert len(emitted) == 88, (k, heal, len(emitted))
            expected = vbuild_sg1_k4_append(win, k, pay, met, st, heal=heal)
            got = k4_images_over(emitted, hp, pay, met, st, blk)
            for actual, reference in zip(got, expected):
                assert actual == reference, (k, heal,
                                             first_diff(actual, reference))
            assert (yield dut.dut.rows) == 128 * launch
            assert (yield dut.dut.tiles) == launch
            pay, met, states[blk] = expected
            yield

    def watch():
        yield Passive()
        while True:
            if (yield dut.dut.tmem_w.fire):
                beats.append(
                    ((yield
                      dut.dut.tmem_w.bits.addr), (yield
                                                  dut.dut.tmem_w.bits.data)))
            yield

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(tx)
    sim.add_sync_process(watch)
    sim.run()


def test_vbuild_k4_special_pool():
    """NaN/inf/zero-heavy pools: NaN never rises but its column byte
    saturates under the retained r; inf wins and is sticky; zeros
    never rise; subnormal rises work. Identity against the temporal
    rebuild at every prefix of a 6-slot block-0 chain."""
    rng = random.Random(7400)
    win = []
    for j in range(64):
        if j < 6:
            row = gen_slot128(rng, specials=False)
            if j == 0:
                row[3] = 0x7e00  # NaN (never wins)
                row[9] = 0x7c00  # +inf (wins, sticky)
                row[80] = 0x0001  # smallest subnormal
            if j == 2:
                row[9] = 0x7bff  # below inf: must NOT re-rise row 9
                row[3] = 0x7e00
                row[1] = 0x0000  # zero column under a live state
            win.extend(row)
        else:
            win.extend([0] * 128)
    pay, met = bytes(128 * 64), bytes(128 * 4)
    st = bytes(128 * 8)
    hp = HasTRoomParams(K4_PARAMS)
    for k in range(1, 7):
        beats, res = k4_run(K4_PARAMS, win, k, pay, met, st)
        assert res['status'] is Status.OK, k
        pay, met, st = vbuild_sg1_k4_append(win, k, pay, met, st)
        got = k4_images_over(beats, hp, pay, met, st, 0)
        assert got == (pay, met, st), k
        ref_pay, ref_met = k4_temporal(win, k)
        assert pay == ref_pay, (k, first_diff(pay, ref_pay))
        assert met == ref_met, (k, first_diff(met, ref_met))


def test_vbuild_k4_rate_beats_partial():
    """The unit-level win: at the same live count, a steady-state K4
    append (a real prior state, ~10% rows rising) runs materially
    fewer cycles than the block-1 partial rebuild of the same block,
    and the partition identity holds."""
    rng = random.Random(7500)
    k = 45  # block 1, e = 13 (past the early-rise surge)
    win = []
    for j in range(64):
        win.extend(gen_slot128(rng) if j < k else [0] * 128)
    blk = 1
    pay, met = bytes(128 * 64), bytes(128 * 4)
    st = vbuild_sg1_k4_state(win, k - 1)
    beats, res = k4_run(K4_PARAMS, win, k, pay, met, st)
    assert res['status'] is Status.OK
    c_k4 = res['counters']
    # the partial rebuild of the same block over the same window
    beats, res = run_build(dict(WIDE_PARAMS),
                           win[:k * 128],
                           k,
                           a_base=P_BASE,
                           r_base=R_BASE,
                           m_base=M_BASE,
                           rows=2 | VBUILD_ROWS_HD128,
                           retain=True)
    assert res['status'] is Status.OK
    c_part = res['counters']
    assert c_k4['run'] < 0.6 * c_part['run'], (c_k4, c_part)
    assert c_k4['scale'] < 0.2 * c_part['scale'], (c_k4, scale_of(c_part))


def scale_of(c):
    return c['scale']


@pytest.mark.parametrize(
    'rows,k,m_base,params_over,want',
    [
        (K4_ROWS, 0, M_BASE, {}, Status.BAD_SHAPE),
        (K4_ROWS, 65, M_BASE, {}, Status.BAD_SHAPE),
        (K4_ROWS, 300, M_BASE, {}, Status.BAD_SHAPE),  # bits above heal
        (K4_ROWS, 5, 0, {}, Status.BAD_SHAPE),
        (K4_ROWS, 5, M_BASE + 4, {}, Status.BAD_ALIGN),
        (K4_ROWS & ~VBUILD_ROWS_HD128, 5, M_BASE, {}, Status.BAD_SHAPE),
        (K4_ROWS, 5, M_BASE, dict(vbuild_k4_selective=False),
         Status.BAD_SHAPE),
        (K4_ROWS, 5, M_BASE, {}, Status.BAD_RANGE),
    ])
def test_vbuild_k4_rejects(rows, k, m_base, params_over, want):
    """Shape/align/range/param rejections complete at accept."""
    params = dict(K4_PARAMS, tmem_bytes=32 * 1024, **params_over)
    a_base, r_base = P_BASE, R_BASE
    if want is Status.BAD_RANGE:
        params = dict(K4_PARAMS)  # r/m extents past a small TMEM
        params.update(tmem_bytes=8 * 1024)
        # Bases that FIT the small TMEM's address width: the stale
        # P_BASE/R_BASE/M_BASE values truncate to zero (and a zero
        # m_base rejects BAD_SHAPE before the range check runs).
        a_base, r_base, m_base = 0x100, 0x200, 0x400
    rng = random.Random(7600 + k)
    slot = gen_slot128(rng)
    beats, res = run_build(params,
                           slot,
                           k,
                           a_base=a_base,
                           r_base=r_base,
                           m_base=m_base,
                           rows=rows,
                           retain_images=((r_base, bytes(128 * 64)), ))
    assert res['status'] is want, res['status']
    assert beats == []


@pytest.mark.parametrize('placement', [0, 1])
def test_vbuild_k4_both_tmem_placements(placement):
    """The K4 chain runs identically with the staging images in
    either TMEM half (the split-TMEM placement choice)."""
    rng = random.Random(7700 + placement)
    k = 20
    win = []
    for j in range(64):
        win.extend(gen_slot128(rng) if j < k else [0] * 128)
    pay, met = bytes(128 * 64), bytes(128 * 4)
    st = bytes(128 * 8)
    hp = HasTRoomParams(K4_PARAMS)
    for step in range(1, k + 1):
        exp_pay, exp_met, exp_st = vbuild_sg1_k4_append(
            win, step, pay, met, st)
        blk = 0
        base_r = R_BASE + placement * 0x8000
        base_m = M_BASE + placement * 0x8000
        st_base = base_m + 512
        beats, res = run_split_build(dict(K4_PARAMS, tmem_bytes=128 * 1024),
                                     win, step, P_BASE + placement * 0x8000,
                                     base_r, base_m, K4_ROWS, [(base_r, pay),
                                                               (base_m, met),
                                                               (st_base, st)])
        assert res['status'] is Status.OK, step
        pay_got, met_got = images_over_bases(beats,
                                             hp,
                                             pay,
                                             met,
                                             base_r,
                                             base_m,
                                             skip=[
                                                 (st_base + b * 1024,
                                                  st_base + b * 1024 + 128 * 8)
                                                 for b in range(2)
                                             ])
        st_got = bytearray(st)
        for addr, data in beats:
            for i in range(hp.data_width // 8):
                if st_base <= addr < st_base + len(st_got):
                    st_got[addr - st_base + i] = (data >> (8 * i)) & 0xff
                elif not (base_r <= addr < base_r + len(pay_got)
                          or base_m <= addr < base_m + len(met_got)):
                    raise AssertionError(hex(addr))
        assert bytes(pay_got) == exp_pay, step
        assert bytes(met_got) == exp_met, step
        assert bytes(st_got) == exp_st, step
        pay, met, st = exp_pay, exp_met, exp_st


@pytest.mark.parametrize('overrides', [dict(rows_bits=8), dict(act_max_k=64)])
def test_vbuild_narrow_descriptor_elaborates(overrides):
    from amaranth.hdl import Fragment

    Fragment.get(VBuildUnit(dict(PARAMS, **overrides)), None)


@pytest.mark.parametrize('overrides, message', [
    (dict(vbuild_max_head_dim=128, rows_bits=8), 'rows_bits'),
    (dict(vbuild_k4_selective=True, act_max_k=64), 'act_max_k'),
])
def test_vbuild_feature_descriptor_width_validation(overrides, message):
    with pytest.raises(ValueError, match=message):
        HasTRoomParams(dict(PARAMS, **overrides))


@pytest.mark.parametrize('rows', [0x10000, 0x10001, 0x80000000])
def test_vbuild_reserved_rows_rejected_without_writes(rows):
    beats, result = run_build(dict(PARAMS, rows_bits=32), [], 1, rows=rows)
    assert result['status'] == Status.BAD_SHAPE
    assert beats == []
