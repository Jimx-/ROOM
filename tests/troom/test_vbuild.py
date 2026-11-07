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
from amaranth.sim import Passive, Simulator

import pytest
import random
import struct

from troom.consts import Status
from troom.tmem import TMEM
from troom.types import HasTRoomParams
from troom.vbuild import VBuildUnit

from tests.troom.model import (VB_OPEN_META_BITS, f32_bits, fp16_to_f32,
                               vbuild_pfold_bits, vbuild_sg1,
                               vbuild_sg1_append, vbuild_sg1_append_mask,
                               vbuild_sg1_block_mask, vbuild_sg1_k,
                               vbuild_sg1_k_mask, vbuild_quant_row,
                               vbuild_quant_row_token, vbuild_tile)

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
              m_base=0,
              rows=0,
              retain=False,
              retain_images=None,
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
        if retain_images is not None:
            # Explicit prior staging images (e.g. a previous append's
            # output): the load-modify-store half of the contract.
            for base, blob in retain_images:
                for off in range(0, len(blob), hp.beat_bytes):
                    word = int.from_bytes(blob[off:off + hp.beat_bytes],
                                          'little')
                    yield from tmem_write(dut.tmem.write, base + off, word)
        elif retain:
            # The host's meta/payload LOAD ahead of a partial run: the
            # staging extents the run does not rebuild keep these
            # bytes (the previous build's output, in device terms).
            for base, n in ((r_base, 64 * 64), (m_base, 64 * 4)):
                pat = bytes((0xC0 | (i % 0x30)) & 0xff for i in range(n))
                for off in range(0, n, hp.beat_bytes):
                    word = int.from_bytes(pat[off:off + hp.beat_bytes],
                                          'little')
                    yield from tmem_write(dut.tmem.write, base + off, word)

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
