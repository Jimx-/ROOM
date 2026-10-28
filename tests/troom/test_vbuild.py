"""Focused pysim tests for the troom V-window tile builder.

The builder turns one 64-slot FP16 pool window (slot-major TMEM
image) into the transposed two-block Q8_0 tile the FA MATMUL
consumes. These tests drive real banked TMEM contents at several
beat widths and live counts and compare the streamed tile beats
byte for byte against the frozen model reference
(``model.vbuild_tile``), including the special-value table (zeros,
subnormals, infinities, NaN mixes) and the descriptor rejections.
"""

from amaranth import *
from amaranth.sim import Passive, Simulator

import pytest
import random

from troom.consts import Status
from troom.tmem import TMEM
from troom.types import HasTRoomParams
from troom.vbuild import VBuildUnit

from tests.troom.model import vbuild_tile

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


class Harness(Elaboratable):
    """VBuildUnit reading from banked TMEM; tile beats captured."""

    def __init__(self, params):
        self.params = params
        self.tmem = TMEM(params)
        self.dut = VBuildUnit(params)

    def elaborate(self, platform):
        m = Module()
        m.submodules.tmem = self.tmem
        m.submodules.dut = self.dut
        m.d.comb += [
            self.dut.tmem_rreq.connect(self.tmem.rreq),
            self.tmem.rresp.connect(self.dut.tmem_rresp),
            self.dut.tmem_w.ready.eq(1),
        ]
        return m


def run_build(params,
              window,
              live,
              a_base=A_BASE,
              r_base=R_BASE,
              deadline=400000):
    hp = HasTRoomParams(params)
    dut = Harness(params)
    beats = []
    result = {'status': None, 'counters': None}

    def tx():
        # Fill TMEM with the window image (the LOAD_2D rows).
        img = b''.join(p.to_bytes(2, 'little') for p in window)
        img += bytes((-len(img)) % hp.beat_bytes)
        for off in range(0, len(img), hp.beat_bytes):
            word = int.from_bytes(img[off:off + hp.beat_bytes], 'little')
            yield from tmem_write(dut.tmem.write, a_base + off, word)

        sink = dut.dut.sink
        yield sink.bits.a_base.eq(a_base)
        yield sink.bits.r_base.eq(r_base)
        yield sink.bits.k.eq(live)
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
                    result['counters'] = {
                        'run': (yield dut.dut.run_cycles),
                        'fill': (yield dut.dut.fill_cycles),
                        'gather': (yield dut.dut.gather_cycles),
                        'scale': (yield dut.dut.scale_cycles),
                        'quant': (yield dut.dut.quant_cycles),
                        'drain': (yield dut.dut.drain_cycles),
                        'dead_blocks': (yield dut.dut.dead_blocks),
                        'zero_blocks': (yield dut.dut.zero_blocks),
                    }
                    break
            else:
                raise TimeoutError('vbuild did not finish')

    def watch():
        yield Passive()
        wr = dut.dut.tmem_w
        while True:
            if (yield wr.fire):
                addr = (yield wr.bits.addr)
                data = (yield wr.bits.data)
                beats.append((addr, data))
            yield

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(tx)
    sim.add_sync_process(watch)
    sim.run()
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
