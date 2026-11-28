"""RTL tests for VEC_QUANT_ACT (G3 direct-activation quantization).

Layers, following ``test_vector_quant.py``:

* the vector unit against ``tests/troom/model.py`` bit for bit: the
  per-segment ``act_wr`` records (segment order, quants, ``s_act``,
  ``sum_a``) equal ``vec_quant_segments`` at the multiples of 32, the
  special-value pool, the frozen clamps (zero/subnormal amax, huge
  amax, all-NaN), with no TMEM traffic at all in direct mode,
* writer-level byte identity (the DESIGN's first gate): the direct
  records equal the records rebuilt from a staged ``VEC_QUANT``
  run's actual TMEM image -- the bytes the ActLoader copies --
* act_wr backpressure: a held record must stay stable and land
  exactly once,
* descriptor rejection (coverage, alignment, range) and the
  don't-care contract (o/b bases never checked),
* the full command path through ``Core``: the nine-command direct
  GEMV head chain (no SQUANTIZE, no staging regions) feeds SMATMUL
  with byte-exact results at the seven Qwen3 GEMV shapes (production
  k 1024/2048/3072 plus minimum and small multiples of 32), the
  paired staged/direct chains produce byte-identical results (the
  DESIGN's chain-level gate), an illegal list (the fill's WAIT
  removed) stalls the accept instead of tearing the bank, the
  shared-x follower and the direct-fill latch behave, the
  single-bank configuration runs the serialized chain, and the two
  ACT writers are never valid simultaneously under serialized
  emission (the priority mux's exclusion watch).
"""

import random

import pytest

from amaranth import Elaboratable, Module
from amaranth.sim import Passive, Simulator

from troom.abi import PV_EARLY_WEIGHTS_FLAG
from troom.consts import Format, Opcode, Status, VecOp
from troom.stream import pack_stream_q8_0
from troom.tmem import TMEM
from troom.types import HasTRoomParams
from troom.vector import VectorUnit

from tests.troom.model import (
    f32,
    f32_bits,
    fp32_mul_bits,
    mm_row_results,
    mm_segments,
    pack_row,
    row_bytes,
    vec_quant_segments,
)
from tests.troom.test_full_path import (
    DRAM_SIZE,
    PARAMS,
    R_DRAM,
    W_DRAM,
    WM_DRAM,
    run_program,
)
from tests.troom.test_matrix import gen_wsegs
from tests.troom.test_vector import (
    A_BASE,
    B_BASE,
    O_BASE,
    SPECIALS,
    run_unit,
    tmem_write,
    words_to_bytes,
)

# Staged-run meta placement (an output for VEC_QUANT); beyond O_BASE.
M_BASE = 0x1800

# The seven Qwen3 GEMV head shapes: the production k values (qkv and
# gate/up 1024, o 2048, down 3072) plus the minimum and small
# multiples of 32.
GEMV_SHAPES = [(32, 4), (64, 2), (96, 3), (128, 4), (1024, 2), (2048, 2),
               (3072, 2)]

# ---------------------------------------------------------------------------
# Unit harness: VectorUnit + TMEM, act_wr records captured
# ---------------------------------------------------------------------------


class QActHarness(Elaboratable):
    """VectorUnit reading from a banked TMEM; the act_wr records and
    any TMEM writes are captured by the simulation processes."""

    def __init__(self, params):
        self.params = params
        self.tmem = TMEM(params)
        self.vec = VectorUnit(params)

    def elaborate(self, platform):
        m = Module()
        m.submodules.tmem = self.tmem
        m.submodules.vec = self.vec
        m.d.comb += [
            self.vec.tmem_rreq.connect(self.tmem.rreq),
            self.tmem.rresp.connect(self.vec.tmem_rresp),
        ]
        return m


def run_q_act(params,
              descs,
              a_bytes,
              *,
              b_bytes=None,
              b_addr=B_BASE,
              hold_act=0,
              deadline=60000):
    """Run descriptors sequentially; return (records, writes, results,
    held).

    ``records`` collects each act_wr fire in order as ``(seg,
    quants_bytes, s_act, sum_a)``. ``hold_act`` keeps the act_wr port
    not-ready from before its first request through that many cycles
    of the pending record -- the record must hold stable, and nothing
    may be lost or duplicated. ``held`` samples the offered record on
    every blocked cycle. ``writes`` collects any TMEM write (direct
    mode must produce none).
    """
    if isinstance(descs, dict):
        descs = [descs]

    hp = HasTRoomParams(params)
    dut = QActHarness(params)
    records = []
    held = []
    writes = []
    results = []
    shared = {'cycles': 0, 'act_blocked': 0}

    def sample():
        quants = (yield dut.vec.act_wr.bits.quants)
        return ((yield dut.vec.act_wr.bits.seg),
                quants.to_bytes(hp.seg_len, 'little'),
                (yield dut.vec.act_wr.bits.s_act), (yield
                                                    dut.vec.act_wr.bits.sum_a))

    def tx():
        for off in range(0, len(a_bytes), hp.beat_bytes):
            word = int.from_bytes(a_bytes[off:off + hp.beat_bytes], 'little')
            yield from tmem_write(dut.tmem.write, A_BASE + off, word)

        if b_bytes is not None:
            for off in range(0, len(b_bytes), hp.beat_bytes):
                word = int.from_bytes(b_bytes[off:off + hp.beat_bytes],
                                      'little')
                yield from tmem_write(dut.tmem.write, b_addr + off, word)

        sink = dut.vec.sink
        for desc in descs:
            yield sink.bits.op.eq(desc['op'])
            yield sink.bits.n.eq(desc['n'])
            yield sink.bits.a_base.eq(desc.get('a_base', A_BASE))
            yield sink.bits.b_base.eq(desc.get('b_base', B_BASE))
            yield sink.bits.o_base.eq(desc.get('o_base', O_BASE))
            yield sink.valid.eq(1)
            yield
            while not (yield sink.fire):
                yield
            start_cycle = shared['cycles']
            # A rejected descriptor pulses done combinationally with
            # the accept; an accepted one pulses it at completion.
            rejected = (yield dut.vec.done)
            yield sink.valid.eq(0)
            yield
            if not rejected:
                while not (yield dut.vec.done):
                    yield
            results.append(
                dict(
                    status=Status((yield dut.vec.status)),
                    elements=(yield dut.vec.elements),
                    write_beats=(yield dut.vec.write_beats),
                    execution_cycles=shared['cycles'] - start_cycle,
                ))

    def actd():
        # Ready is independent of the stimulus: hold from before the
        # first record request through ``hold_act`` cycles of the
        # pending record, then accept freely.
        yield Passive()
        yield dut.vec.act_wr.ready.eq(0)
        yield
        while not (yield dut.vec.act_wr.valid):
            yield dut.vec.act_wr.ready.eq(0)
            yield
        for _ in range(hold_act):
            yield dut.vec.act_wr.ready.eq(0)
            yield
        while True:
            yield dut.vec.act_wr.ready.eq(1)
            yield

    def watch():
        yield Passive()
        while True:
            shared['cycles'] += 1
            if (yield dut.vec.act_wr.fire):
                records.append((yield from sample()))
            elif (yield dut.vec.act_wr.valid):
                shared['act_blocked'] += 1
                held.append((yield from sample()))
            if (yield dut.vec.tmem_w.fire):
                writes.append(
                    ((yield
                      dut.vec.tmem_w.bits.addr), (yield
                                                  dut.vec.tmem_w.bits.data)))
            yield

    def finish():
        for _ in range(deadline):
            yield
            if len(results) == len(descs):
                return
        raise TimeoutError(
            f'vector unit did not finish descriptor {len(results)}: '
            f'{descs[len(results)]}')

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(tx)
    sim.add_sync_process(actd)
    sim.add_sync_process(watch)
    sim.add_sync_process(finish)
    sim.run()
    if len(descs) == 1:
        return records, writes, results[0], held
    return records, writes, results, held


def model_records(words):
    """The ActLoader-shaped records the frozen model implies, in
    segment order (quants as raw bytes, ``sum_a`` signed)."""
    return [(i, bytes(q & 0xff
                      for q in seg['quants']), seg['s_act'], seg['sum_a'])
            for i, seg in enumerate(vec_quant_segments(words))]


def staged_records(params, words, *, o_base=O_BASE, m_base=M_BASE):
    """Rebuild the same records from a staged VEC_QUANT run's actual
    TMEM writes -- the image the ActLoader copies -- closing the loop
    through the staged RTL, not the model."""
    hp = HasTRoomParams(params)
    nsegs = (len(words) + 31) >> 5
    qblob = bytearray(nsegs * 32)
    mblob = bytearray(nsegs * 8)
    writes, res = run_unit(
        params,
        dict(op=int(VecOp.VEC_QUANT),
             n=len(words),
             o_base=o_base,
             b_base=m_base), words_to_bytes(words))
    assert res['status'] is Status.OK
    for addr, data in writes:
        beat = data.to_bytes(hp.beat_bytes, 'little')
        if o_base <= addr < o_base + len(qblob):
            off = addr - o_base
            qblob[off:off + len(beat)] = beat
        else:
            off = addr - m_base
            assert 0 <= off < len(mblob), hex(addr)
            mblob[off:off + len(beat)] = beat
    return [(i, bytes(qblob[32 * i:32 * i + 32]),
             int.from_bytes(mblob[8 * i:8 * i + 4], 'little'),
             int.from_bytes(mblob[8 * i + 4:8 * i + 6], 'little', signed=True))
            for i in range(nsegs)]


# ---------------------------------------------------------------------------
# Vector unit vs the model, bit for bit
# ---------------------------------------------------------------------------


@pytest.mark.parametrize('n', [32, 64, 96, 128, 256, 1024])
def test_unit_q_act_matches_model(n):
    rng = random.Random(200 + n)
    params = dict(PARAMS)
    words = [f32_bits(f32(rng.uniform(-5, 5))) for _ in range(n)]
    records, writes, res, _ = run_q_act(params,
                                        dict(op=int(VecOp.VEC_QUANT_ACT), n=n),
                                        words_to_bytes(words))
    assert res['status'] is Status.OK
    assert res['elements'] == n
    assert res['write_beats'] == 0
    # Direct mode writes no TMEM beat at all.
    assert writes == []
    assert records == model_records(words)


def test_unit_norm_quant_act_matches_rounded_chain():
    rng = random.Random(429)
    n = 128
    params = dict(PARAMS, vec_mul_units=4, vec_quant_pipe=True)
    values, gains = [[f32_bits(rng.uniform(-5, 5)) for _ in range(n)]
                     for _ in range(2)]
    scalar = f32_bits(0.71)
    gain_image = words_to_bytes(gains).ljust(O_BASE - B_BASE, b'\0')
    gain_image += words_to_bytes([scalar])
    normalized = [
        fp32_mul_bits(fp32_mul_bits(value, scalar), gain)
        for value, gain in zip(values, gains)
    ]

    records, writes, result, _ = run_q_act(params,
                                           dict(op=VecOp.VEC_NORM_QUANT_ACT,
                                                n=n),
                                           words_to_bytes(values),
                                           b_bytes=gain_image)

    assert result['status'] == Status.OK
    assert result['elements'] == n
    assert writes == []
    assert records == model_records(normalized)


@pytest.mark.parametrize('n', [96, 256])
def test_unit_q_act_writer_identity(n):
    """The DESIGN's writer-level gate: the direct records equal the
    records the staged chain produces from the same words."""
    rng = random.Random(300 + n)
    params = dict(PARAMS)
    words = [f32_bits(f32(rng.uniform(-3, 3))) for _ in range(n)]
    records, writes, res, _ = run_q_act(params,
                                        dict(op=int(VecOp.VEC_QUANT_ACT), n=n),
                                        words_to_bytes(words))
    assert res['status'] is Status.OK
    assert writes == []
    assert records == staged_records(params, words)
    assert records == model_records(words)


def test_unit_q_act_specials():
    params = dict(PARAMS)
    reps = -(-128 // len(SPECIALS))
    words = (SPECIALS * reps)[:128]
    records, writes, res, _ = run_q_act(
        params, dict(op=int(VecOp.VEC_QUANT_ACT), n=len(words)),
        words_to_bytes(words))
    assert res['status'] is Status.OK
    assert writes == []
    assert records == model_records(words)


def test_unit_q_act_clamps():
    """The frozen clamp branches, one segment each: zero and
    subnormal amax (the zero segment), amax >= 2^126 (sc kept, inv
    forced +0), all-NaN, and the smallest normal amax."""
    params = dict(PARAMS)
    cases = [
        [0] * 32,
        [0x00000001] * 32,
        [0x007fffff, 0x80000001] + [0] * 30,
        [0x7f000000, 0xbf000000] + [f32_bits(1.0)] * 30,
        [0x7fc00000] * 32,
        [0x00800000, 0x80400000] + [0] * 30,
    ]
    for words in cases:
        records, writes, res, _ = run_q_act(
            params, dict(op=int(VecOp.VEC_QUANT_ACT), n=32),
            words_to_bytes(words))
        assert res['status'] is Status.OK
        assert writes == []
        assert records == model_records(words)


def test_unit_q_act_write_backpressure():
    """A held record must stay stable for the whole hold and land
    exactly once; nothing before or after it may change."""
    rng = random.Random(9)
    params = dict(PARAMS)
    words = [f32_bits(f32(rng.uniform(-100, 100))) for _ in range(128)]
    records, writes, res, held = run_q_act(params,
                                           dict(op=int(VecOp.VEC_QUANT_ACT),
                                                n=128),
                                           words_to_bytes(words),
                                           hold_act=300)
    assert res['status'] is Status.OK
    assert len(held) >= 300
    assert all(h == records[0] for h in held)
    assert records == model_records(words)
    assert writes == []


def test_unit_q_act_back_to_back():
    """Three consecutive direct commands: the sequencer parks in QIDLE
    and re-arms; segment indices restart per command."""
    rng = random.Random(15)
    params = dict(PARAMS)
    words = ([f32_bits(1.0)] * 32 + [f32_bits(-1.0)] * 32 +
             [f32_bits(f32(rng.uniform(-3, 3))) for _ in range(64)])
    descs = [
        dict(op=int(VecOp.VEC_QUANT_ACT), n=32, a_base=A_BASE),
        dict(op=int(VecOp.VEC_QUANT_ACT), n=32, a_base=A_BASE + 128),
        dict(op=int(VecOp.VEC_QUANT_ACT), n=64, a_base=A_BASE + 256),
    ]
    records, writes, results, _ = run_q_act(params, descs,
                                            words_to_bytes(words))
    assert all(r['status'] is Status.OK for r in results)
    assert writes == []
    assert records == (model_records(words[:32]) +
                       model_records(words[32:64]) +
                       model_records(words[64:128]))


@pytest.mark.parametrize('desc, status, why', [
    (dict(n=0), Status.BAD_SHAPE, 'zero count'),
    (dict(n=8), Status.BAD_SHAPE, 'not a multiple of 32'),
    (dict(n=33), Status.BAD_SHAPE, 'not a multiple of 32'),
    (dict(n=1056), Status.BAD_SHAPE, 'beyond act_max_k'),
    (dict(n=32, a_base=A_BASE + 1), Status.BAD_ALIGN, 'unaligned a'),
    (dict(n=1024, a_base=0x3f00), Status.BAD_RANGE, 'a past TMEM'),
])
def test_unit_q_act_rejected(desc, status, why):
    params = dict(PARAMS)
    d = dict(desc, op=int(VecOp.VEC_QUANT_ACT))
    records, writes, res, _ = run_q_act(params, d,
                                        words_to_bytes([f32_bits(1.0)] * 32))
    assert res['status'] is status, why
    assert records == []
    assert writes == []


def test_unit_q_act_output_bases_dont_care():
    """o_base inside A and an unaligned b_base are accepted: the
    direct op writes neither TMEM region, so none of the staged
    checks may reject them."""
    rng = random.Random(21)
    params = dict(PARAMS)
    words = [f32_bits(f32(rng.uniform(-2, 2))) for _ in range(64)]
    records, writes, res, _ = run_q_act(
        params,
        dict(op=int(VecOp.VEC_QUANT_ACT),
             n=64,
             o_base=A_BASE,
             b_base=A_BASE + 1), words_to_bytes(words))
    assert res['status'] is Status.OK
    assert writes == []
    assert records == model_records(words)


# ---------------------------------------------------------------------------
# Full command path through Core
# ---------------------------------------------------------------------------

X_DRAM = 0x8000  # FP32 activation source image
VA = 0x2000  # FP32 activation region in TMEM


def conflict_watch(dut):
    """Assert the two ACT writers are never valid simultaneously
    under the serialized emission; the priority mux is there as a
    backstop, not as an assumption."""

    def proc():
        yield Passive()
        while True:
            both = (yield dut.core.act_wr_conflict)
            assert not both, 'both ACT RAM writers valid at once'
            yield

    return proc


def pad_to(bb, b):
    return -(-b // bb) * bb


def build_image(fmt, k, rows, wsegs, words, params):
    hp = HasTRoomParams(params)
    payload, meta = pack_stream_q8_0(wsegs, k, rows, hp.n_outputs)
    image = bytearray(DRAM_SIZE)
    image[W_DRAM:W_DRAM + len(payload)] = payload
    image[WM_DRAM:WM_DRAM + len(meta)] = meta
    image[X_DRAM:X_DRAM + 4 * k] = words_to_bytes(words)
    return image


def q_act_chain(params, words, wsegs, fmt, k, rows, *, wait=True, seq0=2):
    """The G3 GEMV head chain: LOAD x, VEC_QUANT_ACT (no staging
    regions, no SQUANTIZE), SMATMUL, STORE -- nine commands with the
    WAITs, eight without the fill's WAIT (the illegal-list case)."""
    hp = HasTRoomParams(params)
    bb = hp.beat_bytes
    rt = VA + pad_to(bb, 4 * k)
    assert rt + pad_to(bb, rows * 4) <= hp.tmem_bytes, \
        'fixture exceeds TMEM capacity'
    rstride = pad_to(bb, rows * 4)
    image = build_image(fmt, k, rows, wsegs, words, params)
    program = [
        dict(opcode=Opcode.LOAD_2D,
             seq=seq0,
             evt=2,
             dram_addr=X_DRAM,
             tmem_addr=VA,
             row_bytes=pad_to(bb, 4 * k),
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.WAIT, seq=seq0 + 1, evt=2),
        # r_base/m_base are don't-care for the direct op: zero proves
        # they are never consulted.
        dict(opcode=Opcode.VECTOR,
             seq=seq0 + 2,
             evt=6,
             vec_op=int(VecOp.VEC_QUANT_ACT),
             k=k,
             a_base=VA,
             r_base=0,
             m_base=0),
    ]
    if wait:
        program.append(dict(opcode=Opcode.WAIT, seq=seq0 + 3, evt=6))
    program += [
        dict(opcode=Opcode.SMATMUL,
             seq=seq0 + 4,
             evt=5,
             fmt=int(fmt),
             k=k,
             rows=rows,
             dram_addr=W_DRAM,
             dram_stride=WM_DRAM,
             r_base=rt),
        dict(opcode=Opcode.WAIT, seq=seq0 + 5, evt=5),
        dict(opcode=Opcode.STORE_2D,
             seq=seq0 + 6,
             evt=7,
             dram_addr=R_DRAM,
             tmem_addr=rt,
             row_bytes=rstride,
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.WAIT, seq=seq0 + 7, evt=7),
        dict(opcode=Opcode.END, seq=seq0 + 8),
    ]
    return program, image, rt, rstride


def staged_chain(params, words, wsegs, fmt, k, rows, *, seq0=2):
    """The standing 11-command chain at the same geometry: VEC_QUANT
    into the staging regions, SQUANTIZE copy, SMATMUL."""
    hp = HasTRoomParams(params)
    bb = hp.beat_bytes
    vq = VA + pad_to(bb, 4 * k)
    vm = vq + pad_to(bb, k)
    rt = vm + pad_to(bb, k // 8)
    assert rt + pad_to(bb, rows * 4) <= hp.tmem_bytes, \
        'fixture exceeds TMEM capacity'
    rstride = pad_to(bb, rows * 4)
    image = build_image(fmt, k, rows, wsegs, words, params)
    program = [
        dict(opcode=Opcode.LOAD_2D,
             seq=seq0,
             evt=2,
             dram_addr=X_DRAM,
             tmem_addr=VA,
             row_bytes=pad_to(bb, 4 * k),
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.WAIT, seq=seq0 + 1, evt=2),
        dict(opcode=Opcode.VECTOR,
             seq=seq0 + 2,
             evt=6,
             vec_op=int(VecOp.VEC_QUANT),
             k=k,
             a_base=VA,
             r_base=vq,
             m_base=vm),
        dict(opcode=Opcode.WAIT, seq=seq0 + 3, evt=6),
        dict(opcode=Opcode.SQUANTIZE,
             seq=seq0 + 4,
             evt=4,
             a_base=vq,
             m_base=vm,
             k=k),
        dict(opcode=Opcode.WAIT, seq=seq0 + 5, evt=4),
        dict(opcode=Opcode.SMATMUL,
             seq=seq0 + 6,
             evt=5,
             fmt=int(fmt),
             k=k,
             rows=rows,
             dram_addr=W_DRAM,
             dram_stride=WM_DRAM,
             r_base=rt),
        dict(opcode=Opcode.WAIT, seq=seq0 + 7, evt=5),
        dict(opcode=Opcode.STORE_2D,
             seq=seq0 + 8,
             evt=7,
             dram_addr=R_DRAM,
             tmem_addr=rt,
             row_bytes=rstride,
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.WAIT, seq=seq0 + 9, evt=7),
        dict(opcode=Opcode.END, seq=seq0 + 10),
    ]
    return program, image


def chain_reference(fmt, k, rows, wsegs, words):
    act = vec_quant_segments(words)
    records = mm_segments(fmt, k, rows, [
        pack_row(fmt, wsegs[r * (k // 32):(r + 1) * (k // 32)])
        for r in range(rows)
    ], act)
    res = mm_row_results(records)
    # mm_row_results returns {row index: FP32 value}.
    return [f32_bits(res[r]) for r in range(rows)]


def check_chain_results(image, exp, k, rows):
    for r in range(rows):
        got = int.from_bytes(image[R_DRAM + 4 * r:R_DRAM + 4 * r + 4],
                             'little')
        assert got == exp[r], (k, r, hex(got), hex(exp[r]))


@pytest.mark.parametrize('k,rows', GEMV_SHAPES)
def test_core_q_act_gemm_shapes(k, rows):
    """The nine-command direct chain at the seven Qwen3 GEMV shapes:
    byte-exact results against the model reference chain, with the
    writer-exclusion watch over the whole run."""
    rng = random.Random(400 + k)
    fmt = Format.Q8_0
    params = dict(PARAMS, act_max_k=4096, tmem_bytes=64 * 1024)
    words = [f32_bits(f32(rng.uniform(-1, 1))) for _ in range(k)]
    wsegs = gen_wsegs(rng, fmt, rows * (k // 32))
    program, image, _, _ = q_act_chain(params, words, wsegs, fmt, k, rows)
    results, image, counters = run_program(params,
                                           program,
                                           image,
                                           deadline=200000,
                                           watch=conflict_watch)
    assert all(s is Status.OK for s in results.values()), results
    check_chain_results(image, chain_reference(fmt, k, rows, wsegs, words), k,
                        rows)
    assert counters['useful_macs'] == rows * k


@pytest.mark.parametrize('k,rows', [(96, 3), (1024, 2)])
def test_core_q_act_paired_staged_identity(k, rows):
    """The DESIGN's chain-level gate: the staged and direct chains on
    the same words produce byte-identical FP32 results end to end."""
    rng = random.Random(500 + k)
    fmt = Format.Q8_0
    params = dict(PARAMS, act_max_k=4096, tmem_bytes=64 * 1024)
    words = [f32_bits(f32(rng.uniform(-1, 1))) for _ in range(k)]
    wsegs = gen_wsegs(rng, fmt, rows * (k // 32))

    dprog, dimage, _, _ = q_act_chain(params, words, wsegs, fmt, k, rows)
    dres, dimage, _ = run_program(params,
                                  dprog,
                                  dimage,
                                  deadline=200000,
                                  watch=conflict_watch)
    assert all(s is Status.OK for s in dres.values()), dres

    sprog, simage = staged_chain(params, words, wsegs, fmt, k, rows)
    sres, simage, _ = run_program(params,
                                  sprog,
                                  simage,
                                  deadline=200000,
                                  watch=conflict_watch)
    assert all(s is Status.OK for s in sres.values()), sres

    rows_bytes = 4 * rows
    assert bytes(dimage[R_DRAM:R_DRAM + rows_bytes]) == \
        bytes(simage[R_DRAM:R_DRAM + rows_bytes])
    check_chain_results(dimage, chain_reference(fmt, k, rows, wsegs, words), k,
                        rows)


def test_core_q_act_illegal_list_stalls():
    """The bank-commit invariant: with the fill's WAIT removed (an
    illegal list), the SMATMUL accept must stall on the in-flight
    direct fill instead of committing a half-written bank -- the
    results stay byte-exact."""
    k, rows = 1024, 2
    rng = random.Random(600 + k)
    fmt = Format.Q8_0
    params = dict(PARAMS, act_max_k=4096, tmem_bytes=64 * 1024)
    words = [f32_bits(f32(rng.uniform(-1, 1))) for _ in range(k)]
    wsegs = gen_wsegs(rng, fmt, rows * (k // 32))
    program, image, _, _ = q_act_chain(params,
                                       words,
                                       wsegs,
                                       fmt,
                                       k,
                                       rows,
                                       wait=False)
    results, image, _ = run_program(params,
                                    program,
                                    image,
                                    deadline=200000,
                                    watch=conflict_watch)
    assert all(s is Status.OK for s in results.values()), results
    check_chain_results(image, chain_reference(fmt, k, rows, wsegs, words), k,
                        rows)


def test_core_q_act_follower_and_relatch():
    """The shared-x follower discipline and the direct-fill latch: a
    second SMATMUL with no refill reads the same committed bank, and
    the ordinary vector command between them clears the latch -- the
    second accept must not be wedged by a stale vec_act_direct."""
    k, rows = 256, 2
    rng = random.Random(700 + k)
    fmt = Format.Q8_0
    params = dict(PARAMS, act_max_k=4096, tmem_bytes=64 * 1024)
    hp = HasTRoomParams(params)
    bb = hp.beat_bytes
    words = [f32_bits(f32(rng.uniform(-1, 1))) for _ in range(k)]
    wsegs = gen_wsegs(rng, fmt, rows * (k // 32))
    program, image, rt, rstride = q_act_chain(params, words, wsegs, fmt, k,
                                              rows)
    scratch = rt + rstride
    assert scratch + pad_to(bb, 4 * k) <= hp.tmem_bytes, \
        'follower fixture exceeds TMEM capacity'
    follow = [
        # An ordinary vector command re-latches the core's
        # vec_act_direct to 0.
        dict(opcode=Opcode.VECTOR,
             seq=30,
             evt=4,
             vec_op=int(VecOp.VEC_MUL),
             k=k,
             a_base=VA,
             m_base=VA,
             r_base=scratch),
        dict(opcode=Opcode.WAIT, seq=31, evt=4),
        dict(opcode=Opcode.SMATMUL,
             seq=32,
             evt=5,
             fmt=int(fmt),
             k=k,
             rows=rows,
             dram_addr=W_DRAM,
             dram_stride=WM_DRAM,
             r_base=rt),
        dict(opcode=Opcode.WAIT, seq=33, evt=5),
        dict(opcode=Opcode.STORE_2D,
             seq=34,
             evt=7,
             dram_addr=R_DRAM + 64,
             tmem_addr=rt,
             row_bytes=rstride,
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.WAIT, seq=35, evt=7),
    ]
    program = program[:-1] + follow + [program[-1]]
    results, image, _ = run_program(params,
                                    program,
                                    image,
                                    deadline=200000,
                                    watch=conflict_watch)
    assert all(s is Status.OK for s in results.values()), results
    assert image[R_DRAM + 64:R_DRAM + 64 + 4 * rows] == \
        image[R_DRAM:R_DRAM + 4 * rows]
    check_chain_results(image, chain_reference(fmt, k, rows, wsegs, words), k,
                        rows)


def test_core_q_act_single_bank():
    """The single-bank configuration runs the serialized chain
    unchanged: the fill completes between matmuls (the WAIT
    discipline), and the accept keeps the same invariant."""
    k, rows = 64, 2
    rng = random.Random(800 + k)
    fmt = Format.Q8_0
    params = dict(PARAMS, act_max_k=4096, tmem_bytes=64 * 1024, act_banks=1)
    words = [f32_bits(f32(rng.uniform(-1, 1))) for _ in range(k)]
    wsegs = gen_wsegs(rng, fmt, rows * (k // 32))
    program, image, _, _ = q_act_chain(params, words, wsegs, fmt, k, rows)
    results, image, _ = run_program(params,
                                    program,
                                    image,
                                    deadline=200000,
                                    watch=conflict_watch)
    assert all(s is Status.OK for s in results.values()), results
    check_chain_results(image, chain_reference(fmt, k, rows, wsegs, words), k,
                        rows)


def test_core_q_act_reduce_class_rejected():
    """REDUCE with the elementwise-only direct op completes at issue
    with BAD_SHAPE, like every class-mismatched encoding."""
    params = dict(PARAMS, act_max_k=4096, tmem_bytes=64 * 1024)
    image = bytearray(DRAM_SIZE)
    program = [
        dict(opcode=Opcode.REDUCE,
             seq=2,
             evt=3,
             vec_op=int(VecOp.VEC_QUANT_ACT),
             k=64,
             a_base=VA,
             r_base=0x3000,
             m_base=0),
        dict(opcode=Opcode.END, seq=3),
    ]
    results, _, _ = run_program(params, program, image)
    assert results[2] is Status.BAD_SHAPE


def test_core_q_act_mixed_fill_priority():
    """An out-of-contract mixed list -- the ActLoader still filling
    while the direct fill starts -- must not hang or corrupt: the
    priority mux grants the ActLoader, the direct records wait, and
    with identical x the bank ends byte-identical either way."""
    k, rows = 1024, 2
    rng = random.Random(900 + k)
    fmt = Format.Q8_0
    params = dict(PARAMS, act_max_k=4096, tmem_bytes=64 * 1024)
    hp = HasTRoomParams(params)
    bb = hp.beat_bytes
    words = [f32_bits(f32(rng.uniform(-1, 1))) for _ in range(k)]
    wsegs = gen_wsegs(rng, fmt, rows * (k // 32))
    vq = VA + pad_to(bb, 4 * k)
    vm = vq + pad_to(bb, k)
    rt = vm + pad_to(bb, k // 8)
    rstride = pad_to(bb, rows * 4)
    assert rt + rstride <= hp.tmem_bytes, 'fixture exceeds TMEM capacity'
    image = build_image(fmt, k, rows, wsegs, words, params)
    program = [
        dict(opcode=Opcode.LOAD_2D,
             seq=2,
             evt=2,
             dram_addr=X_DRAM,
             tmem_addr=VA,
             row_bytes=pad_to(bb, 4 * k),
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.WAIT, seq=3, evt=2),
        dict(opcode=Opcode.VECTOR,
             seq=4,
             evt=6,
             vec_op=int(VecOp.VEC_QUANT),
             k=k,
             a_base=VA,
             r_base=vq,
             m_base=vm),
        dict(opcode=Opcode.WAIT, seq=5, evt=6),
        dict(opcode=Opcode.SQUANTIZE, seq=6, evt=4, a_base=vq, m_base=vm, k=k),
        # No WAIT: the direct fill starts while the ActLoader streams.
        dict(opcode=Opcode.VECTOR,
             seq=7,
             evt=6,
             vec_op=int(VecOp.VEC_QUANT_ACT),
             k=k,
             a_base=VA,
             r_base=0,
             m_base=0),
        dict(opcode=Opcode.SMATMUL,
             seq=8,
             evt=5,
             fmt=int(fmt),
             k=k,
             rows=rows,
             dram_addr=W_DRAM,
             dram_stride=WM_DRAM,
             r_base=rt),
        dict(opcode=Opcode.WAIT, seq=9, evt=5),
        dict(opcode=Opcode.STORE_2D,
             seq=10,
             evt=7,
             dram_addr=R_DRAM,
             tmem_addr=rt,
             row_bytes=rstride,
             rows=1,
             dram_stride=0,
             tmem_stride=0),
        dict(opcode=Opcode.WAIT, seq=11, evt=7),
        dict(opcode=Opcode.END, seq=12),
    ]
    results, image, _ = run_program(params, program, image, deadline=200000)
    assert all(s is Status.OK for s in results.values()), results
    check_chain_results(image, chain_reference(fmt, k, rows, wsegs, words), k,
                        rows)


@pytest.mark.parametrize('width,lanes,banks', [(64, 2, 2), (512, 4, 2),
                                               (64, 2, 1)])
def test_pv_early_weights_keeps_activation_commit_order(width, lanes, banks):
    """Fetch PV weights while direct P quantization fills the next ACT bank."""
    rng = random.Random(822)
    k, rows = 64, 128
    fmt = Format.Q8_0
    words = [f32_bits(f32(rng.uniform(-1, 1))) for _ in range(k)]
    wsegs = gen_wsegs(rng, fmt, rows * (k // 32))
    params = dict(PARAMS,
                  data_width=width,
                  n_outputs=lanes,
                  act_banks=banks,
                  n_tmem_banks=8 if width == 512 else 4,
                  act_max_k=4096,
                  tmem_bytes=64 * 1024)
    timing = []
    for early in (False, True):
        program, image, _, _ = q_act_chain(params, words, wsegs, fmt, k, rows)
        if early:
            program[3], program[4] = program[4], program[3]
            program[3].update(seq=5, w_stride=PV_EARLY_WEIGHTS_FLAG)
            program[4]['seq'] = 6
        overlap = []

        def watch(dut):

            def proc():
                yield Passive()
                stream = dut.core.stream
                while True:
                    assert not (yield dut.core.act_wr_conflict)
                    if (yield stream.act_fill_busy):
                        assert not (yield stream.actram.commit)
                        if (yield stream.mem_rreq.fire):
                            overlap.append(1)
                    yield

            return proc

        results, image, counters = run_program(params,
                                               program,
                                               image,
                                               deadline=40000,
                                               watch=watch)
        assert bool(overlap) == (early and banks == 2)
        assert all(s is Status.OK for s in results.values()), results
        check_chain_results(image, chain_reference(fmt, k, rows, wsegs, words),
                            k, rows)
        timing.append(counters['cycles'])
    print(
        f'PV early weights cycles ({width}-bit, {banks} ACT banks): {timing}')
    if banks == 2:
        assert timing[1] < timing[0], timing
