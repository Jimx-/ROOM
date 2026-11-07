"""S3 FA quant-staging repro: the staged QK chain vs the serial list.

The historical board bisect recorded late-head corruption/watchdogs
when staging QIMG quantization under QK. The original reproducer also
reused EVT_W before draining the preceding quantizer completion, which
the faster pipelined quantizer exposed as a lost-event timeout. The
corrected lists drain each completion before consuming QIMG and reusing
its event tag. At board-matched 512-bit beats, compare their stored
outputs against the serial list bit for bit.
"""

import random

import pytest

from troom.consts import Format, Opcode, Status, VecOp
from troom.types import HasTRoomParams
from troom.stream import pack_stream_q8_0

from tests.troom.model import f32, f32_bits
from tests.troom.test_matrix import gen_wsegs
from tests.troom.test_full_path import run_program

# Board-matched shape: 9 query heads over 3 KV heads, head dim 64, the
# two-window n_eff class (the failing "resync to 96" shape).
DATA_WIDTH = 512
N_HEAD = 9
N_KV = 3
GQA = N_HEAD // N_KV
HEAD_DIM = 64
N_EFF = 128
NSEG = HEAD_DIM // 32

# TMEM layout (the FA emitter's ordering, scaled to 64 KiB).
Q2 = 0x1000
MASKR = 0x2000
S = 0x2400
SM = 0x2800
QIMG = 0x2c00

# DRAM layout.
# Keep the simulated RAM compact; high addresses add no coverage here
# and make pysim's AXI memory unnecessarily expensive to elaborate.
Q_DRAM = 0x1000
K_PAY = 0x10000
K_META = 0x18000
DRAM_SIZE = 0x38000

EVT_A, EVT_C, EVT_OP, EVT_MM, EVT_ST, EVT_W = 0, 2, 4, 5, 6, 3

PARAMS = dict(
    addr_width=32,
    data_width=DATA_WIDTH,
    id_width=4,
    seg_len=32,
    n_outputs=2,
    n_tokens=1,
    tmem_bytes=64 * 1024,
    n_tmem_banks=8,
    act_max_k=1024,
    cmd_queue_depth=8,
    n_events=8,
    seq_bits=8,
    mm_formats=(Format.Q8_0, ),
)


def build_image(hp, rng):
    """Q rows, a zero mask row, and the streamed K payload/meta."""
    image = bytearray(DRAM_SIZE)
    blob = b''.join(
        f32_bits(f32(rng.uniform(-2, 2))).to_bytes(4, 'little')
        for _ in range(N_HEAD * HEAD_DIM))
    image[Q_DRAM:Q_DRAM + len(blob)] = blob
    image[MASKR:MASKR + N_EFF * 4] = bytes(N_EFF * 4)  # zero mask
    wsegs = gen_wsegs(rng, Format.Q8_0, N_EFF * N_KV * NSEG)
    payload, meta = pack_stream_q8_0(wsegs, HEAD_DIM, N_EFF * N_KV,
                                     hp.n_outputs)
    image[K_PAY:K_PAY + len(payload)] = payload
    image[K_META:K_META + len(meta)] = meta
    return image, wsegs


def qk_program(staged):
    """The FA QK section per head: qq, qa, mm, softmax stand-in, store."""
    prog = []
    seq = 0

    def cmd(op, evt, **f):
        nonlocal seq
        seq += 1
        e = {'opcode': int(op), 'seq': seq, 'evt': evt}
        e.update(f)
        prog.append(e)

    def wait(evt):
        cmd(Opcode.WAIT, evt)

    # header: load the q rows (the q-fold output stands in DRAM)
    cmd(Opcode.LOAD_2D,
        EVT_A,
        dram_addr=Q_DRAM,
        tmem_addr=Q2,
        row_bytes=N_HEAD * HEAD_DIM * 4,
        rows=1)
    wait(EVT_A)

    for h in range(N_HEAD):
        kv = h // GQA
        if not (staged and h > 0):
            cmd(Opcode.VECTOR,
                EVT_OP,
                vec_op=int(VecOp.VEC_QUANT),
                k=HEAD_DIM,
                a_base=Q2 + 256 * h,
                r_base=QIMG,
                m_base=QIMG + 0x80)
            wait(EVT_OP)
        else:
            # Events hold one level, not a completion count. Drain qq(h)
            # before consuming QIMG or issuing qq(h+1) on the same tag.
            wait(EVT_W)
        cmd(Opcode.SQUANTIZE,
            EVT_OP,
            k=HEAD_DIM,
            a_base=QIMG,
            m_base=QIMG + 0x80)
        wait(EVT_OP)
        if staged and h + 1 < N_HEAD:
            cmd(Opcode.VECTOR,
                EVT_W,
                vec_op=int(VecOp.VEC_QUANT),
                k=HEAD_DIM,
                a_base=Q2 + 256 * (h + 1),
                r_base=QIMG,
                m_base=QIMG + 0x80)
        cmd(Opcode.SMATMUL,
            EVT_MM,
            fmt=int(Format.Q8_0),
            k=HEAD_DIM,
            rows=N_EFF,
            r_base=S,
            dram_addr=K_PAY,
            dram_stride=K_META)
        wait(EVT_MM)
        # softmax stand-in: S + mask -> SM (reads every S beat)
        cmd(Opcode.VECTOR,
            EVT_OP,
            vec_op=int(VecOp.VEC_ADD),
            k=N_EFF,
            a_base=S,
            m_base=MASKR,
            r_base=SM)
        wait(EVT_OP)
        cmd(Opcode.STORE_2D,
            EVT_ST,
            dram_addr=WRAP_OUT + 0x1000 * h,
            tmem_addr=SM,
            row_bytes=N_EFF * 4,
            rows=1)
        wait(EVT_ST)
    cmd(Opcode.END, 0)
    return prog


def run_arm(staged, seed=20261101):
    hp = HasTRoomParams(PARAMS)
    rng = random.Random(seed)
    image, _ = build_image(hp, rng)
    results, out, counters = run_program(dict(PARAMS),
                                         qk_program(staged),
                                         image,
                                         deadline=150000)
    assert all(s == Status.OK for s in results.values()), results
    # S4b: read the region qk_program actually stores to (WRAP_OUT).
    # The original read used OUT (0x300000) -- a region nothing writes
    # -- so every head compared zeros and the staged-vs-serial pin was
    # vacuous (it could not fail for ANY ordering; this is why S3's
    # "the twin passes the identical lists" claim could not catch the
    # 0x8 corruption).
    outs = {
        h: bytes(out[WRAP_OUT + 0x1000 * h:WRAP_OUT + 0x1000 * h + N_EFF * 4])
        for h in range(N_HEAD)
    }
    return outs, counters


def test_qk_staged_matches_serial():
    """The staged list must store bit-identical S rows to the serial one."""
    ser, cser = run_arm(False)
    stg, cstg = run_arm(True)
    for h in range(N_HEAD):
        assert stg[h] == ser[h], \
            f'head {h}: staged S row differs from serial'
    print('cycles serial', cser.get('cycles'), 'staged', cstg.get('cycles'))


# ---------------------------------------------------------------------------
# S4b: the 0x8 (ACT-load staging) shape. The S3 emission staged head h+1's
# SQUANTIZE at head h's tail while the VEC_QUANT writing QIMG for head h+1
# stayed at head h+1's top -- AFTER the staging -- so the ACT load read
# head h's QIMG and every QK(h+1) computed with the previous head's query
# (the deterministic 17-check fa-suite failure; closed as an emitter
# dataflow bug 2026-11-02, results/troom-s4b-q8fix-20261102). No model ever
# executed this list shape before -- this pin feeds the twin the exact
# broken and fixed shapes so the coverage gap cannot reopen silently.
# ---------------------------------------------------------------------------


def qa_program(variant):
    """The FA head chain with the SQUANTIZE staged under the tail.

    serial: qq(h), qa(h) inline.  broken: the S3 0x8 emission -- qa(h+1)
    staged behind the QK(h) wait-mm (reading whatever QIMG holds, i.e.
    head h's image) with qq(h+1) only at head h+1's top.  fixed: qq(h+1)
    staged under the QK(h) (vector under stream) and drained behind
    wait-mm, THEN qa(h+1) staged under the scale/store tail.
    """
    assert variant in ('serial', 'broken', 'fixed')
    prog = []
    seq = 0

    def cmd(op, evt, **f):
        nonlocal seq
        seq += 1
        e = {'opcode': int(op), 'seq': seq, 'evt': evt}
        e.update(f)
        prog.append(e)

    def wait(evt):
        cmd(Opcode.WAIT, evt)

    cmd(Opcode.LOAD_2D,
        EVT_A,
        dram_addr=Q_DRAM,
        tmem_addr=Q2,
        row_bytes=N_HEAD * HEAD_DIM * 4,
        rows=1)
    wait(EVT_A)

    staged = variant != 'serial'
    for h in range(N_HEAD):
        h_next = h + 1 < N_HEAD
        if not (staged and h > 0):
            cmd(Opcode.VECTOR,
                EVT_OP,
                vec_op=int(VecOp.VEC_QUANT),
                k=HEAD_DIM,
                a_base=Q2 + 256 * h,
                r_base=QIMG,
                m_base=QIMG + 0x80)
            wait(EVT_OP)
        if not (staged and h > 0):
            cmd(Opcode.SQUANTIZE,
                EVT_OP,
                k=HEAD_DIM,
                a_base=QIMG,
                m_base=QIMG + 0x80)
            wait(EVT_OP)
        if variant == 'fixed' and h_next:
            cmd(Opcode.VECTOR,
                EVT_W,
                vec_op=int(VecOp.VEC_QUANT),
                k=HEAD_DIM,
                a_base=Q2 + 256 * (h + 1),
                r_base=QIMG,
                m_base=QIMG + 0x80)
        cmd(Opcode.SMATMUL,
            EVT_MM,
            fmt=int(Format.Q8_0),
            k=HEAD_DIM,
            rows=N_EFF,
            r_base=S,
            dram_addr=K_PAY,
            dram_stride=K_META)
        wait(EVT_MM)
        if variant == 'fixed' and h_next:
            wait(EVT_W)
            cmd(Opcode.SQUANTIZE,
                EVT_W,
                k=HEAD_DIM,
                a_base=QIMG,
                m_base=QIMG + 0x80)
        if variant == 'broken' and h_next:
            cmd(Opcode.SQUANTIZE,
                EVT_W,
                k=HEAD_DIM,
                a_base=QIMG,
                m_base=QIMG + 0x80)
        # softmax stand-in + normalize/store tail (the staging's cover)
        cmd(Opcode.VECTOR,
            EVT_OP,
            vec_op=int(VecOp.VEC_ADD),
            k=N_EFF,
            a_base=S,
            m_base=MASKR,
            r_base=SM)
        wait(EVT_OP)
        cmd(Opcode.STORE_2D,
            EVT_ST,
            dram_addr=WRAP_OUT + 0x1000 * h,
            tmem_addr=SM,
            row_bytes=N_EFF * 4,
            rows=1)
        wait(EVT_ST)
        if staged and h_next:
            wait(EVT_W)
    cmd(Opcode.END, 0)
    return prog


def run_qa_arm(variant, seed=20261102):
    hp = HasTRoomParams(PARAMS)
    rng = random.Random(seed)
    image, _ = build_image(hp, rng)
    results, out, counters = run_program(dict(PARAMS),
                                         qa_program(variant),
                                         image,
                                         deadline=150000)
    assert all(s == Status.OK for s in results.values()), \
        (variant, results)
    # S4b: read the region qa_program stores to (WRAP_OUT) -- see the
    # run_arm note about the original OUT/WRAP_OUT read-back bug.
    outs = {
        h: bytes(out[WRAP_OUT + 0x1000 * h:WRAP_OUT + 0x1000 * h + N_EFF * 4])
        for h in range(N_HEAD)
    }
    return outs, counters


def test_qa_staged_broken_and_fixed():
    """The broken 0x8 shape corrupts in the twin too (it was never
    silicon-only -- it had simply never been modeled); the fixed shape
    is bit-identical to serial."""
    ser, _ = run_qa_arm('serial')
    brk, _ = run_qa_arm('broken')
    fix, _ = run_qa_arm('fixed')
    assert brk[0] == ser[0], 'head 0 must be correct in every variant'
    wrong = [h for h in range(1, N_HEAD) if brk[h] != ser[h]]
    assert wrong, 'broken 0x8 shape must corrupt the staged heads in the twin'
    for h in range(N_HEAD):
        assert fix[h] == ser[h], f'head {h}: fixed 0x8 differs from serial'


# ---------------------------------------------------------------------------
# Wrapper-level repro: the same lists through the fetcher + wrapper.
# (The core-level test above drives core.cmd directly; the board adds the
# command fetcher, whose beat/offer pacing differs.)
# ---------------------------------------------------------------------------

from troom.abi import (
    REG_CMD_BASE_HI,
    REG_CMD_BASE_LO,
    REG_CMD_COUNT,
    REG_CMD_FORMAT,
    REG_COMP_BASE_HI,
    REG_COMP_BASE_LO,
    REG_COMP_CAPACITY,
    REG_ERROR_CODE,
    REG_ERROR_INDEX,
    REG_MAX_CYCLES,
    REG_PARAM_BASE,
    REG_STATUS,
    REG_SUBMIT_ID,
    STATUS_BUSY,
    command_field_shapes,
    pack_buffer,
)
from troom.wrapper import TroomWrapper

from tests.troom.test_wrapper import (
    WrapperTop,
    collect_writes,
    mmio_read,
    mmio_write,
    run_wrap,
    wrap_cmd,
)

WRAP_BASE = dict(PARAMS)
WRAP_BASE.update(cmd_queue_depth=8)
WHP = HasTRoomParams(WRAP_BASE)
WSHAPES = command_field_shapes(WHP)

WRAP_OUT = 0x20000
CMD_BUF = 0x30000
COMP_BUF = 0x34000
WRAP_IMAGE = DRAM_SIZE
MAXC = 3_000_000


def wrap_image(rng):
    """A compact wrapper-space image: Q rows, mask, K payload/meta."""
    hp = WHP
    image = bytearray(WRAP_IMAGE)
    blob = b''.join(
        f32_bits(f32(rng.uniform(-2, 2))).to_bytes(4, 'little')
        for _ in range(N_HEAD * HEAD_DIM))
    image[Q_DRAM:Q_DRAM + len(blob)] = blob
    image[MASKR:MASKR + N_EFF * 4] = bytes(N_EFF * 4)
    wsegs = gen_wsegs(rng, Format.Q8_0, N_EFF * N_KV * NSEG)
    payload, meta = pack_stream_q8_0(wsegs, HEAD_DIM, N_EFF * N_KV,
                                     hp.n_outputs)
    image[K_PAY:K_PAY + len(payload)] = payload
    image[K_META:K_META + len(meta)] = meta
    return image


def wrap_program(staged):
    prog = []
    seq = 0

    def cmd(op, evt, **f):
        nonlocal seq
        seq += 1
        prog.append(wrap_cmd(op, seq, evt=evt, **f))

    def wait(evt):
        cmd(Opcode.WAIT, evt)

    cmd(Opcode.LOAD_2D,
        EVT_A,
        dram_addr=Q_DRAM,
        tmem_addr=Q2,
        row_bytes=N_HEAD * HEAD_DIM * 4,
        rows=1)
    wait(EVT_A)
    for h in range(N_HEAD):
        if not (staged and h > 0):
            cmd(Opcode.VECTOR,
                EVT_OP,
                vec_op=int(VecOp.VEC_QUANT),
                k=HEAD_DIM,
                a_base=Q2 + 256 * h,
                r_base=QIMG,
                m_base=QIMG + 0x80)
            wait(EVT_OP)
        else:
            # Match qk_program's single-outstanding EVT_W contract.
            wait(EVT_W)
        cmd(Opcode.SQUANTIZE,
            EVT_OP,
            k=HEAD_DIM,
            a_base=QIMG,
            m_base=QIMG + 0x80)
        wait(EVT_OP)
        if staged and h + 1 < N_HEAD:
            cmd(Opcode.VECTOR,
                EVT_W,
                vec_op=int(VecOp.VEC_QUANT),
                k=HEAD_DIM,
                a_base=Q2 + 256 * (h + 1),
                r_base=QIMG,
                m_base=QIMG + 0x80)
        cmd(Opcode.SMATMUL,
            EVT_MM,
            fmt=int(Format.Q8_0),
            k=HEAD_DIM,
            rows=N_EFF,
            r_base=S,
            dram_addr=K_PAY,
            dram_stride=K_META)
        wait(EVT_MM)
        cmd(Opcode.VECTOR,
            EVT_OP,
            vec_op=int(VecOp.VEC_ADD),
            k=N_EFF,
            a_base=S,
            m_base=MASKR,
            r_base=SM)
        wait(EVT_OP)
        cmd(Opcode.STORE_2D,
            EVT_ST,
            dram_addr=WRAP_OUT + 0x1000 * h,
            tmem_addr=SM,
            row_bytes=N_EFF * 4,
            rows=1)
        wait(EVT_ST)
    cmd(Opcode.END, 0)
    return prog


def run_wrap_arm(staged, rng_seed=20261101, read_latency=2, b_latency=2):
    from tests.troom.test_wrapper import launch
    src_img = wrap_image(random.Random(rng_seed))
    prog = wrap_program(staged)
    buf = pack_buffer(prog, WSHAPES)
    src_img[CMD_BUF:CMD_BUF + len(buf)] = buf
    top = WrapperTop(WRAP_BASE,
                     bytes(src_img),
                     read_latency=read_latency,
                     b_latency=b_latency)
    img = bytearray(src_img)

    info = {}

    def main():
        res = yield from launch(top,
                                img,
                                submit_id=1,
                                cmd_base=CMD_BUF,
                                count=len(prog),
                                comp_base=COMP_BUF,
                                capacity=len(prog) + 1,
                                max_cycles=MAXC)
        info.update(res)
        bad = [r for r in res['records'] if r[1] != 0]
        assert not bad, f'non-OK completions: {bad[:4]}'

    run_wrap(top, main, collect_writes(top, img))
    print(f'staged={staged} status={info["status"]:#x} code={info["code"]} '
          f'index={info["index"]} completed={info["completed"]}')
    outs = {
        h: bytes(img[WRAP_OUT + 0x1000 * h:WRAP_OUT + 0x1000 * h + N_EFF * 4])
        for h in range(N_HEAD)
    }
    return outs


@pytest.mark.parametrize('read_latency', [2, 6])
def test_qk_wrapper_staged_vs_serial(read_latency):
    ser = run_wrap_arm(False, read_latency=read_latency)
    stg = run_wrap_arm(True, read_latency=read_latency)
    for h in range(N_HEAD):
        assert stg[h] == ser[
            h], f'head {h}: staged S row differs (lat={read_latency})'


# ---------------------------------------------------------------------------
# P2b-i: the qs_qks (0x20) shape -- the next head's QK staged under this
# head's softmax cover (results/troom-p2b-qks-20261104). op 1 (the S+mask
# add) is the last S reader, so QK(h+1) may rewrite S while ops 2-3 (which
# read SM, never S) and the qa(h+1) ACT fill run beside it on the banked
# image; every completion drains inside the cover. This pin feeds the twin
# the exact serial and staged head-boundary structures; the read-back
# (per-head SM rows) is sensitive to the staged classes: a QK(h+1) that
# lands before op 1 corrupts every later head's S reads, and a wrong ACT
# bank turn makes QK(h+1) stream the previous head's query image (the S4b
# 0x8 class).
# ---------------------------------------------------------------------------

SM2 = 0x3800


def qks_program(qks):
    prog = []
    seq = 0

    def cmd(op, evt, **f):
        nonlocal seq
        seq += 1
        e = {'opcode': int(op), 'seq': seq, 'evt': evt}
        e.update(f)
        prog.append(e)

    def wait(evt):
        cmd(Opcode.WAIT, evt)

    def qq(hh, evt):
        cmd(Opcode.VECTOR,
            evt,
            vec_op=int(VecOp.VEC_QUANT),
            k=HEAD_DIM,
            a_base=Q2 + 256 * hh,
            r_base=QIMG,
            m_base=QIMG + 0x80)

    def qa(evt):
        cmd(Opcode.SQUANTIZE, evt, k=HEAD_DIM, a_base=QIMG, m_base=QIMG + 0x80)

    def qk(hh):
        kvh = hh // GQA
        cmd(Opcode.SMATMUL,
            EVT_MM,
            fmt=int(Format.Q8_0),
            k=HEAD_DIM,
            rows=N_EFF,
            r_base=S,
            dram_addr=K_PAY + kvh * 0x2000,
            dram_stride=K_META + kvh * 0x200)

    cmd(Opcode.LOAD_2D,
        EVT_A,
        dram_addr=Q_DRAM,
        tmem_addr=Q2,
        row_bytes=N_HEAD * HEAD_DIM * 4,
        rows=1)
    wait(EVT_A)

    for h in range(N_HEAD):
        h_next = h + 1 < N_HEAD
        if not qks:
            qq(h, EVT_OP)
            wait(EVT_OP)
            qa(EVT_OP)
            wait(EVT_OP)
            qk(h)
            wait(EVT_MM)
        else:
            if h == 0:
                qq(0, EVT_OP)
                wait(EVT_OP)
                qa(EVT_OP)
                wait(EVT_OP)
                if h_next:
                    qq(1, EVT_W)  # rides the QK(0) wall
                qk(0)
                wait(EVT_MM)
                if h_next:
                    wait(EVT_W)  # drain qq(1)
                    qa(EVT_W)  # fills beside op 1+
            else:
                if h_next:
                    qa(EVT_W)  # qa(h+1) at the head top
        # op 1: the last S reader
        cmd(Opcode.VECTOR,
            EVT_OP,
            vec_op=int(VecOp.VEC_ADD),
            k=N_EFF,
            a_base=S,
            m_base=MASKR,
            r_base=SM)
        wait(EVT_OP)
        if qks and h_next:
            wait(EVT_W)  # drain qa(h+1)
            qk(h + 1)  # streams under ops 2-3
            if h + 2 < N_HEAD:
                qq(h + 2, EVT_W)  # rides the QK(h+1) wall
        # ops 2-3: SM traffic concurrent with the staged S rewrites
        cmd(Opcode.VECTOR,
            EVT_OP,
            vec_op=int(VecOp.VEC_SUB),
            k=N_EFF,
            a_base=SM,
            m_base=MASKR,
            r_base=SM2)
        wait(EVT_OP)
        cmd(Opcode.VECTOR,
            EVT_OP,
            vec_op=int(VecOp.VEC_ADD),
            k=N_EFF,
            a_base=SM2,
            m_base=MASKR,
            r_base=SM)
        wait(EVT_OP)
        if qks and h_next:
            wait(EVT_MM)  # drain QK(h+1)
            if h + 2 < N_HEAD:
                wait(EVT_W)  # drain qq(h+2)
        cmd(Opcode.STORE_2D,
            EVT_ST,
            dram_addr=WRAP_OUT + 0x1000 * h,
            tmem_addr=SM,
            row_bytes=N_EFF * 4,
            rows=1)
        wait(EVT_ST)
    cmd(Opcode.END, 0)
    return prog


def run_qks_arm(qks, seed=20261104):
    hp = HasTRoomParams(PARAMS)
    rng = random.Random(seed)
    image, _ = build_image(hp, rng)
    results, out, counters = run_program(dict(PARAMS),
                                         qks_program(qks),
                                         image,
                                         deadline=150000)
    assert all(s == Status.OK for s in results.values()), results
    outs = {
        h: bytes(out[WRAP_OUT + 0x1000 * h:WRAP_OUT + 0x1000 * h + N_EFF * 4])
        for h in range(N_HEAD)
    }
    return outs, counters


def test_qks_staged_matches_serial():
    """The P2b-i head-boundary move is bit-identical to the serial list,
    with a sensitivity floor: the serial heads are pairwise distinct,
    so a staged arm that matched a wrong head (or zeros) cannot pass."""
    ser, cser = run_qks_arm(False)
    stg, cstg = run_qks_arm(True)
    vals = list(ser.values())
    assert all(v != vals[0] for v in vals[1:]), \
        'serial heads must be distinct for the pin to be sensitive'
    for h in range(N_HEAD):
        assert stg[h] == ser[h], \
            f'head {h}: qs_qks SM row differs from serial'
    print('cycles serial', cser.get('cycles'), 'qks', cstg.get('cycles'))


def wrap_qks_arm(qks, rng_seed=20261104, read_latency=2, b_latency=2):
    from tests.troom.test_wrapper import launch
    src_img = wrap_image(random.Random(rng_seed))
    prog = []
    seq = 0

    def cmd(op, evt, **f):
        nonlocal seq
        seq += 1
        prog.append(wrap_cmd(op, seq, evt=evt, **f))

    def wait(evt):
        cmd(Opcode.WAIT, evt)

    def qq(hh, evt):
        cmd(Opcode.VECTOR,
            evt,
            vec_op=int(VecOp.VEC_QUANT),
            k=HEAD_DIM,
            a_base=Q2 + 256 * hh,
            r_base=QIMG,
            m_base=QIMG + 0x80)

    def qa(evt):
        cmd(Opcode.SQUANTIZE, evt, k=HEAD_DIM, a_base=QIMG, m_base=QIMG + 0x80)

    def qk(hh):
        kvh = hh // GQA
        cmd(Opcode.SMATMUL,
            EVT_MM,
            fmt=int(Format.Q8_0),
            k=HEAD_DIM,
            rows=N_EFF,
            r_base=S,
            dram_addr=K_PAY + kvh * 0x2000,
            dram_stride=K_META + kvh * 0x200)

    cmd(Opcode.LOAD_2D,
        EVT_A,
        dram_addr=Q_DRAM,
        tmem_addr=Q2,
        row_bytes=N_HEAD * HEAD_DIM * 4,
        rows=1)
    wait(EVT_A)
    for h in range(N_HEAD):
        h_next = h + 1 < N_HEAD
        if not qks:
            qq(h, EVT_OP)
            wait(EVT_OP)
            qa(EVT_OP)
            wait(EVT_OP)
            qk(h)
            wait(EVT_MM)
        else:
            if h == 0:
                qq(0, EVT_OP)
                wait(EVT_OP)
                qa(EVT_OP)
                wait(EVT_OP)
                if h_next:
                    qq(1, EVT_W)
                qk(0)
                wait(EVT_MM)
                if h_next:
                    wait(EVT_W)
                    qa(EVT_W)
            else:
                if h_next:
                    qa(EVT_W)
        cmd(Opcode.VECTOR,
            EVT_OP,
            vec_op=int(VecOp.VEC_ADD),
            k=N_EFF,
            a_base=S,
            m_base=MASKR,
            r_base=SM)
        wait(EVT_OP)
        if qks and h_next:
            wait(EVT_W)
            qk(h + 1)
            if h + 2 < N_HEAD:
                qq(h + 2, EVT_W)
        cmd(Opcode.VECTOR,
            EVT_OP,
            vec_op=int(VecOp.VEC_SUB),
            k=N_EFF,
            a_base=SM,
            m_base=MASKR,
            r_base=SM2)
        wait(EVT_OP)
        cmd(Opcode.VECTOR,
            EVT_OP,
            vec_op=int(VecOp.VEC_ADD),
            k=N_EFF,
            a_base=SM2,
            m_base=MASKR,
            r_base=SM)
        wait(EVT_OP)
        if qks and h_next:
            wait(EVT_MM)
            if h + 2 < N_HEAD:
                wait(EVT_W)
        cmd(Opcode.STORE_2D,
            EVT_ST,
            dram_addr=WRAP_OUT + 0x1000 * h,
            tmem_addr=SM,
            row_bytes=N_EFF * 4,
            rows=1)
        wait(EVT_ST)
    cmd(Opcode.END, 0)

    buf = pack_buffer(prog, WSHAPES)
    src_img[CMD_BUF:CMD_BUF + len(buf)] = buf
    top = WrapperTop(WRAP_BASE,
                     bytes(src_img),
                     read_latency=read_latency,
                     b_latency=b_latency)
    img = bytearray(src_img)
    info = {}

    def main():
        res = yield from launch(top,
                                img,
                                submit_id=1,
                                cmd_base=CMD_BUF,
                                count=len(prog),
                                comp_base=COMP_BUF,
                                capacity=len(prog) + 1,
                                max_cycles=MAXC)
        info.update(res)
        bad = [r for r in res['records'] if r[1] != 0]
        assert not bad, f'non-OK completions: {bad[:4]}'

    run_wrap(top, main, collect_writes(top, img))
    print(f"qks={qks} status={info['status']:#x} code={info['code']} "
          f"index={info['index']} completed={info['completed']}")
    return {
        h: bytes(img[WRAP_OUT + 0x1000 * h:WRAP_OUT + 0x1000 * h + N_EFF * 4])
        for h in range(N_HEAD)
    }


@pytest.mark.parametrize('read_latency', [2, 6])
def test_qks_wrapper_staged_vs_serial(read_latency):
    ser = wrap_qks_arm(False, read_latency=read_latency)
    stg = wrap_qks_arm(True, read_latency=read_latency)
    for h in range(N_HEAD):
        assert stg[h] == ser[h], \
            f'head {h}: qs_qks SM row differs (lat={read_latency})'
