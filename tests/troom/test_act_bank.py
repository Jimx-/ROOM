"""P2a banked-ACT-RAM tests (2026-11-03).

The resource-contract slice of the pipelined-batch-decode plan:
``ActRAM`` grows a second bank whose ownership flips at every stream
accept, so a staged ``SQUANTIZE`` runs under a live ``SMATMUL``
(the corrected S3/S4b 0x8 class). These tests pin:

* the bank commit/flip mechanics at the unit level (and the
  single-bank ``pin`` backpressure staying bit-for-bit),
* the core-level staged list -- activation loads plus ``SQUANTIZE``
  issued between the producer ``SMATMUL`` and its ``WAIT`` -- exact
  against the reference model, in both the one-deep and two-deep
  hoist forms,
* the old exclusion on ``act_banks=1`` builds: the same staged list
  must stall, not corrupt (the accept gate keeps the loader out of
  the running stream's bank).
"""

import random

import pytest

from amaranth import *
from amaranth.sim import Simulator

from troom.consts import Format, Opcode, Status
from troom.matrix import ActRAM
from troom.types import HasTRoomParams

from tests.troom.model import f32_bits
from tests.troom.test_stream_core import (
    A_BASE,
    A_DRAM,
    M_BASE,
    M_DRAM,
    R_BASE,
    R_DRAM,
    SGP_DRAM,
    SGM_DRAM,
    STREAM_PACKERS,
    act_regions,
    check_stream_results,
    core_params,
    make_tile,
    place_stream_tile,
    run_program,
)


def bank_params(banks, **over):
    return core_params(act_banks=banks, **over)


def act_word(quants, s_act, sum_a):
    return (int.from_bytes(bytes(q & 0xFF for q in quants),
                           'little'), s_act, sum_a)


#
# Unit level: the ActRAM bank machinery itself.
#


def test_actram_banked_commit_and_flip():
    """Writes go to wr_bank; commit latches it for reads and rotates.

    Generation 0 fills the bank, commit makes it readable while
    generation 1 fills the other bank; reads keep returning
    generation 0 until the second commit.
    """
    params = bank_params(2)
    bench = Module()
    dut = ActRAM(params)
    bench.submodules.dut = dut
    bench.domains.sync = ClockDomain()

    def proc():

        def wr(word):
            yield dut.wr.bits.seg.eq(word[3])
            yield dut.wr.bits.quants.eq(word[0])
            yield dut.wr.bits.s_act.eq(word[1])
            yield dut.wr.bits.sum_a.eq(word[2])
            yield dut.wr.valid.eq(1)
            while not (yield dut.wr.fire):
                yield
            yield dut.wr.valid.eq(0)
            yield

        def rd(seg):
            yield dut.rd_seg.eq(seg)
            yield
            yield
            return ((yield dut.rd_quants), (yield
                                            dut.rd_s_act), (yield
                                                            dut.rd_sum_a))

        def commit():
            yield dut.commit.eq(1)
            yield
            yield dut.commit.eq(0)
            yield

        g0 = act_word([0xA0] * 32, f32_bits(1.0), 0xA0) + (1, )
        g1 = act_word([0xA1] * 32, f32_bits(1.25), 0xA1) + (3, )
        yield from wr(g0)
        yield from wr(g1)
        yield from commit()
        q, s, sa = yield from rd(1)
        assert q == g0[0], hex(q)
        assert s == f32_bits(1.0)
        assert sa == 0xA0
        # The loader fills the OTHER bank while reads continue; the
        # committed bank must not tear.
        yield from wr(act_word([0xB0] * 32, f32_bits(2.0), 0xB0) + (1, ))
        yield from wr(act_word([0xB1] * 32, f32_bits(3.0), 0xB1) + (3, ))
        q, s, sa = yield from rd(1)
        assert q == g0[0], 'write to the idle bank tore the committed bank'
        q, s, sa = yield from rd(3)
        assert s == f32_bits(1.25)
        yield from commit()
        q, s, sa = yield from rd(1)
        assert q == int.from_bytes(bytes([0xB0] * 32), 'little'), hex(q)
        assert s == f32_bits(2.0)
        q, s, sa = yield from rd(3)
        assert s == f32_bits(3.0)
        # Persistence: a commit with no fill since the last one is a
        # no-op (back-to-back stream commands read the same bank --
        # the follower discipline).
        yield from commit()
        q, s, sa = yield from rd(1)
        assert q == int.from_bytes(bytes([0xB0] * 32), 'little'), \
            'unloaded commit rotated the read bank'
        assert s == f32_bits(2.0)

    sim = Simulator(bench)
    sim.add_clock(1e-6)
    sim.add_sync_process(proc)
    sim.run()


def test_actram_single_bank_pin_backpressure():
    """act_banks=1 keeps the historical contract: pin back-pressures
    writes and commit is a no-op."""
    params = bank_params(1)
    bench = Module()
    dut = ActRAM(params)
    bench.submodules.dut = dut
    bench.domains.sync = ClockDomain()

    def proc():
        yield dut.pin.eq(1)
        yield dut.wr.bits.seg.eq(0)
        yield dut.wr.bits.quants.eq(0x5A)
        yield dut.wr.bits.s_act.eq(f32_bits(4.0))
        yield dut.wr.bits.sum_a.eq(7)
        yield dut.wr.valid.eq(1)
        yield
        assert not (yield dut.wr.fire), 'write accepted while pinned'
        yield dut.pin.eq(0)
        while not (yield dut.wr.fire):
            yield
        yield dut.wr.valid.eq(0)
        yield dut.commit.eq(1)
        yield
        yield dut.commit.eq(0)
        yield
        yield dut.rd_seg.eq(0)
        yield
        yield
        assert (yield dut.rd_s_act) == f32_bits(4.0)

    sim = Simulator(bench)
    sim.add_clock(1e-6)
    sim.add_sync_process(proc)
    sim.run()


#
# Core level: the staged lists the slice exists for.
#


def staged_image(rng, fmt, k, rows, params, tiles=2):
    """Distinct tiles/images at non-overlapping offsets.

    Tile i's streamed weights land at SGP_DRAM + 0x400*i and its
    activation image pair at A_DRAM/M_DRAM + 0x200*i (the quant and
    meta extents are <= 0x200 bytes at these shapes, and the
    A_DRAM+0x800 of the standing overlap test would collide with
    M_DRAM). Every program loads image i into the SAME TMEM region
    (A_BASE/M_BASE) in list order, so each SQUANTIZE consumes the
    bytes its own loads put there (the emitter discipline the staged
    form keeps).
    """
    hp = HasTRoomParams(params)
    out = [
        make_tile(random.Random(rng + i * 101), k, rows, fmt)
        for i in range(tiles)
    ]
    image = bytearray(R_DRAM + 0x8000)
    place_stream_tile(image, out[0])
    qlen, mlen = act_regions(image, out[0]['act'], params)
    packer = STREAM_PACKERS[fmt]
    for i, t in enumerate(out[1:], start=1):
        payload, meta = packer(t['wsegs'], t['k'], t['rows'])
        image[SGP_DRAM + 0x400 * i:SGP_DRAM + 0x400 * i + len(payload)] = \
            payload
        image[SGM_DRAM + 0x400 * i:SGM_DRAM + 0x400 * i + len(meta)] = meta
        q, m = bytearray(), bytearray()
        for s in t['act']:
            q += bytes(x & 0xFF for x in s['quants'])
            m += s['s_act'].to_bytes(4, 'little')
            m += (s['sum_a'] & 0xFFFF).to_bytes(2, 'little')
            m += b'\x00\x00'
        q += bytes((-len(q)) % hp.beat_bytes)
        m += bytes((-len(m)) % hp.beat_bytes)
        image[A_DRAM + 0x200 * i:A_DRAM + 0x200 * i + len(q)] = q
        image[M_DRAM + 0x200 * i:M_DRAM + 0x200 * i + len(m)] = m
    return image, out, (qlen, mlen)


def staged_preamble(seq0, k, i, qlen, mlen):
    """Tile i's activation preamble: two loads, their waits, and the
    SQUANTIZE (evt C2) -- issued between a producer SMATMUL and its
    WAIT, exactly the P2a staging shape."""
    return [
        dict(opcode=Opcode.LOAD_2D,
             seq=seq0,
             evt=1,
             dram_addr=A_DRAM + 0x200 * i,
             tmem_addr=A_BASE,
             row_bytes=qlen,
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.LOAD_2D,
             seq=seq0 + 1,
             evt=3,
             dram_addr=M_DRAM + 0x200 * i,
             tmem_addr=M_BASE,
             row_bytes=mlen,
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.WAIT, seq=seq0 + 2, evt=1),
        dict(opcode=Opcode.WAIT, seq=seq0 + 3, evt=3),
        dict(opcode=Opcode.SQUANTIZE,
             seq=seq0 + 4,
             evt=6,
             a_base=A_BASE,
             m_base=M_BASE,
             k=k),
    ]


def tile_store(seq0, rstride, dram_off):
    return [
        dict(opcode=Opcode.STORE_2D,
             seq=seq0,
             evt=5,
             dram_addr=R_DRAM + dram_off,
             tmem_addr=R_BASE,
             row_bytes=rstride,
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.WAIT, seq=seq0 + 1, evt=5),
    ]


@pytest.mark.parametrize('fmt', [Format.Q8_0, Format.Q6_K])
def test_staged_squantize_under_matmul(fmt):
    """Tile 1's SQUANTIZE runs under tile 0's SMATMUL: exact results.

    Both stores are checked against the reference model bit for bit:
    the staged load fills the bank the second accept commits while
    the first stream reads the other one.
    """
    k, rows = 256, 4
    params = bank_params(2)
    hp = HasTRoomParams(params)
    image, tiles, (qlen, mlen) = staged_image(7, fmt, k, rows, params, tiles=2)
    rstride = -(-(rows * 4) // hp.beat_bytes) * hp.beat_bytes
    program = [
        dict(opcode=Opcode.LOAD_2D,
             seq=1,
             evt=1,
             dram_addr=A_DRAM,
             tmem_addr=A_BASE,
             row_bytes=qlen,
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.LOAD_2D,
             seq=2,
             evt=3,
             dram_addr=M_DRAM,
             tmem_addr=M_BASE,
             row_bytes=mlen,
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.WAIT, seq=3, evt=1),
        dict(opcode=Opcode.WAIT, seq=4, evt=3),
        dict(opcode=Opcode.SQUANTIZE,
             seq=5,
             evt=2,
             a_base=A_BASE,
             m_base=M_BASE,
             k=k),
        dict(opcode=Opcode.WAIT, seq=6, evt=2),
        dict(opcode=Opcode.SMATMUL,
             seq=7,
             evt=4,
             fmt=int(fmt),
             k=k,
             rows=rows,
             dram_addr=SGP_DRAM,
             dram_stride=SGM_DRAM,
             r_base=R_BASE),
    ]
    program += staged_preamble(10, k, 1, qlen, mlen)
    program += [
        dict(opcode=Opcode.WAIT, seq=15, evt=4),  # tile 0 completion
    ] + tile_store(16, rstride, 0) + [
        dict(opcode=Opcode.WAIT, seq=18, evt=6),  # staged load drained
        dict(opcode=Opcode.SMATMUL,
             seq=19,
             evt=7,
             fmt=int(fmt),
             k=k,
             rows=rows,
             dram_addr=SGP_DRAM + 0x400,
             dram_stride=SGM_DRAM + 0x400,
             r_base=R_BASE),
        dict(opcode=Opcode.WAIT, seq=20, evt=7),
    ] + tile_store(21, rstride, 0x1000) + [
        dict(opcode=Opcode.END, seq=23),
    ]
    results, image, _ = run_program(params, program, image, deadline=60000)
    assert all(s is Status.OK for s in results.values()), results
    t0, t1 = tiles
    saved = bytes(image[R_DRAM:R_DRAM + rstride])
    check_stream_results(image, fmt, k, rows, t0['wsegs'], t0['act'], rstride)
    image[R_DRAM:R_DRAM + rstride] = image[R_DRAM + 0x1000:R_DRAM + 0x1000 +
                                           rstride]
    check_stream_results(image, fmt, k, rows, t1['wsegs'], t1['act'], rstride)
    image[R_DRAM:R_DRAM + rstride] = saved


def test_staged_squantize_two_deep():
    """Two successive stagings: tile 2's preamble goes under tile 1's
    SMATMUL, one bank-commit later -- the fill must land in the bank
    tile 0 read (free again once the single-context stream moved on).
    Three distinct activations keep the banks honest."""
    fmt = Format.Q8_0
    k, rows = 128, 4
    params = bank_params(2)
    hp = HasTRoomParams(params)
    image, tiles, (qlen, mlen) = staged_image(11,
                                              fmt,
                                              k,
                                              rows,
                                              params,
                                              tiles=3)
    rstride = -(-(rows * 4) // hp.beat_bytes) * hp.beat_bytes
    program = [
        dict(opcode=Opcode.LOAD_2D,
             seq=1,
             evt=1,
             dram_addr=A_DRAM,
             tmem_addr=A_BASE,
             row_bytes=qlen,
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.LOAD_2D,
             seq=2,
             evt=3,
             dram_addr=M_DRAM,
             tmem_addr=M_BASE,
             row_bytes=mlen,
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.WAIT, seq=3, evt=1),
        dict(opcode=Opcode.WAIT, seq=4, evt=3),
        dict(opcode=Opcode.SQUANTIZE,
             seq=5,
             evt=2,
             a_base=A_BASE,
             m_base=M_BASE,
             k=k),
        dict(opcode=Opcode.WAIT, seq=6, evt=2),
        dict(opcode=Opcode.SMATMUL,
             seq=7,
             evt=4,
             fmt=int(fmt),
             k=k,
             rows=rows,
             dram_addr=SGP_DRAM,
             dram_stride=SGM_DRAM,
             r_base=R_BASE),
    ]
    program += staged_preamble(10, k, 1, qlen, mlen)
    program += [
        dict(opcode=Opcode.WAIT, seq=15, evt=4),
    ] + tile_store(16, rstride, 0) + [
        dict(opcode=Opcode.WAIT, seq=18, evt=6),
        dict(opcode=Opcode.SMATMUL,
             seq=19,
             evt=7,
             fmt=int(fmt),
             k=k,
             rows=rows,
             dram_addr=SGP_DRAM + 0x400,
             dram_stride=SGM_DRAM + 0x400,
             r_base=R_BASE),
    ]
    program += staged_preamble(30, k, 2, qlen, mlen)
    program += [
        dict(opcode=Opcode.WAIT, seq=35, evt=7),
    ] + tile_store(36, rstride, 0x1000) + [
        dict(opcode=Opcode.WAIT, seq=38, evt=6),
        dict(opcode=Opcode.SMATMUL,
             seq=39,
             evt=4,
             fmt=int(fmt),
             k=k,
             rows=rows,
             dram_addr=SGP_DRAM + 0x800,
             dram_stride=SGM_DRAM + 0x800,
             r_base=R_BASE),
        dict(opcode=Opcode.WAIT, seq=40, evt=4),
    ] + tile_store(41, rstride, 0x2000) + [
        dict(opcode=Opcode.END, seq=43),
    ]
    results, image, _ = run_program(params, program, image, deadline=60000)
    assert all(s is Status.OK for s in results.values()), results
    saved0 = bytes(image[R_DRAM:R_DRAM + rstride])
    check_stream_results(image, fmt, k, rows, tiles[0]['wsegs'],
                         tiles[0]['act'], rstride)
    image[R_DRAM:R_DRAM + rstride] = image[R_DRAM + 0x1000:R_DRAM + 0x1000 +
                                           rstride]
    check_stream_results(image, fmt, k, rows, tiles[1]['wsegs'],
                         tiles[1]['act'], rstride)
    image[R_DRAM:R_DRAM + rstride] = image[R_DRAM + 0x2000:R_DRAM + 0x2000 +
                                           rstride]
    check_stream_results(image, fmt, k, rows, tiles[2]['wsegs'],
                         tiles[2]['act'], rstride)
    image[R_DRAM:R_DRAM + rstride] = saved0


def test_staged_squantize_stalls_on_single_bank():
    """The same staged list on an act_banks=1 build must not run: the
    loader's offer is excluded while the stream is active, so the
    list stalls at the staged SQUANTIZE (the standing exclusion the
    slice relaxes, kept switchable -- a stall, never a corruption).
    A long-running tile guarantees the stream is still active when
    issue reaches the staged load."""
    fmt = Format.Q8_0
    k, rows = 1024, 64
    params = bank_params(1)
    hp = HasTRoomParams(params)
    image, tiles, (qlen, mlen) = staged_image(7, fmt, k, rows, params, tiles=2)
    rstride = -(-(rows * 4) // hp.beat_bytes) * hp.beat_bytes
    program = [
        dict(opcode=Opcode.LOAD_2D,
             seq=1,
             evt=1,
             dram_addr=A_DRAM,
             tmem_addr=A_BASE,
             row_bytes=qlen,
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.LOAD_2D,
             seq=2,
             evt=3,
             dram_addr=M_DRAM,
             tmem_addr=M_BASE,
             row_bytes=mlen,
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.WAIT, seq=3, evt=1),
        dict(opcode=Opcode.WAIT, seq=4, evt=3),
        dict(opcode=Opcode.SQUANTIZE,
             seq=5,
             evt=2,
             a_base=A_BASE,
             m_base=M_BASE,
             k=k),
        dict(opcode=Opcode.WAIT, seq=6, evt=2),
        dict(opcode=Opcode.SMATMUL,
             seq=7,
             evt=4,
             fmt=int(fmt),
             k=k,
             rows=rows,
             dram_addr=SGP_DRAM,
             dram_stride=SGM_DRAM,
             r_base=R_BASE),
    ]
    program += staged_preamble(10, k, 1, qlen, mlen)
    program += [
        dict(opcode=Opcode.WAIT, seq=15, evt=4),
        dict(opcode=Opcode.END, seq=16),
    ]
    with pytest.raises(TimeoutError):
        run_program(params, program, image, deadline=4000)
