"""Broadcast receiver-mode tests for the asynchronous copy engine.

* direct engine tests with a scripted TileLink responder: broadcast to
  two and four cores through a common destination offset — every core's
  shared memory receives identical bytes, each GMEM line is fetched once
  regardless of receiver count, and the single done event follows the
  last receiver's commit;
* asymmetric receiver backpressure: per-core ready gating holds the
  replay cursor on a stalled receiver without losing or duplicating
  fragments, and completion still follows the final commit;
* multi-port commit accounting: per-core acknowledgment delays mirroring
  the serial acceptance spacing force several receivers to commit the
  same fragment in one cycle, and a withheld receiver acknowledgment
  keeps `done` low until released;
* pitched 2D broadcast, zero-row broadcast, and destination-span
  rejection that leaves every core untouched;
* frontend tests: the broadcast meta bit reaches the command, and a
  launch selecting the unimplemented barrier completion target or
  nonzero reserved meta bits is rejected locally without offering a
  command and releases its parked warp;
* an end-to-end bare-core program broadcasting through the real engine,
  shared-memory endpoint, and token wait path, then loading the
  committed bytes.

All processes honour the amaranth ``pysim`` clock model documented in
AGENTS.md: only a naked ``yield`` advances the clock, and signal reads and
writes between naked yields are coherent within one cycle.
"""

import pytest
from amaranth import *

from room.consts import UOpCode

from groom.async_copy import AsyncCopyEngine

from tests.sim import run_test
from tests.groom.encoding import addi, gcopy, gcopywait, gpu_tmc, lui, lw, \
    wgather
from tests.groom.test_async_copy import LINE_BYTES, SparseRAM, _params, \
    _proc, _run, _watchdog, dma_sink, recv_done, tl_copy_responder
from tests.groom.test_gcopy import BUF_A, COPY_BASE, PROG_BASE, SMEM_BASE, \
    E2E_PARAMS, GcopyCoreTop, _FrontendTop, _fu_op, _fu_resp


class _BcastTop(Elaboratable):

    def __init__(self, params, n_cores=2):
        self.engine = AsyncCopyEngine(n_cores, params, block_bytes=LINE_BYTES)

    def elaborate(self, platform):
        m = Module()
        m.submodules.engine = self.engine
        return m


def send_bcast(cmd,
               *,
               id,
               core=0,
               bcast=1,
               mode=0,
               src=0,
               base=0,
               rcnt=0,
               rbytes=0,
               gs=0,
               ss=0,
               dst=0,
               nbytes=0):
    """Drive one command port (1D or 2D, unicast or broadcast)."""
    yield cmd.bits.id.eq(id)
    yield cmd.bits.core.eq(core)
    yield cmd.bits.bcast.eq(bcast)
    yield cmd.bits.src_addr.eq(src)
    yield cmd.bits.nbytes.eq(nbytes)
    yield cmd.bits.dst_offset.eq(dst)
    yield cmd.bits.mode.eq(mode)
    yield cmd.bits.src_base.eq(base)
    yield cmd.bits.row_count.eq(rcnt)
    yield cmd.bits.row_bytes.eq(rbytes)
    yield cmd.bits.g_stride.eq(gs)
    yield cmd.bits.s_stride.eq(ss)
    yield cmd.valid.eq(1)
    yield
    while not (yield cmd.ready):
        yield
    yield cmd.valid.eq(0)
    yield


def dma_sink_delayed(engine, core, smem, commits, done, *, delay=2, hold=None):
    """Behavioural DMA endpoint with an independent acknowledgment delay.

    The engine's contract is exactly one commit event per accepted
    fragment with no assumption about its distance — the two-cycle
    grant-to-commit distance is a property of the real ``SharedMemory``
    endpoint, covered by the LSU tests. Delays that mirror the serial
    acceptance spacing make several receivers' acknowledgments land in
    the same cycle, and ``hold`` (a one-element list) withholds a
    receiver's commits — including its final one — while the engine
    waits.
    """
    port = engine.dma_req[core]
    commit = engine.dma_commit[core]
    pending = []
    cycle = 0
    while not (done[0] and not pending):
        yield port.ready.eq(1)
        yield commit.valid.eq(0)
        if (yield port.valid):
            fid = yield port.bits.id
            offset = yield port.bits.offset
            data = yield port.bits.data
            mask = yield port.bits.byte_enable
            for k in range(8):
                if (mask >> k) & 1:
                    assert offset + k < len(smem), \
                        'DMA write out of bounds'
                    smem[offset + k] = (data >> (8 * k)) & 0xff
            pending.append((cycle + delay, fid, bin(mask).count('1')))
        if pending and pending[0][0] <= cycle + 1 \
                and not (hold is not None and hold[0]):
            _, fid, nbytes = pending.pop(0)
            yield commit.valid.eq(1)
            yield commit.bits.id.eq(fid)
            yield commit.bits.nbytes.eq(nbytes)
            commits.append((fid, nbytes))
        yield
        cycle += 1


def commit_monitor(engine, done, stats):
    """Count commit-port fires, tracking how many coincide per cycle."""
    while not done[0]:
        n = 0
        for port in engine.dma_commit:
            if (yield port.fire):
                n += 1
        stats['max'] = max(stats['max'], n)
        stats['total'] += n
        yield


# ---------------------------------------------------------------------------
# Engine: broadcast delivery
# ---------------------------------------------------------------------------


def test_engine_broadcast_1d_two_cores():
    """Both cores receive identical bytes; one done follows the last commit."""
    params = _params()
    top = _BcastTop(params, n_cores=2)
    engine = top.engine
    model = SparseRAM()
    smems = [bytearray(params['smem_params']['size']) for _ in range(2)]
    commits = [[], []]
    done = [False]
    a_log = []

    def driver():
        yield from send_bcast(engine.cmd, id=1, src=0x40, nbytes=40, dst=0x80)
        res = yield from recv_done(engine.done)
        assert res == (1, 0)
        for c in range(2):
            assert bytes(smems[c][0x80:0x80 + 40]) == model.peek(0x40, 40), \
                f'core {c} mismatch'
            assert sum(n for _, n in commits[c]) == 40, \
                f'core {c} must commit every byte exactly once'
        assert [addr for _, addr in a_log] == [0x40], \
            '40 bytes from 0x40 stay inside one line, fetched once'
        done[0] = True

    _run(top, _proc(dma_sink, engine, 0, smems[0], commits[0], done),
         _proc(dma_sink, engine, 1, smems[1], commits[1], done),
         _proc(tl_copy_responder, engine.mem_bus, model, done, a_log=a_log),
         driver, _proc(_watchdog, done))


def test_engine_broadcast_gmem_lines_fetched_once():
    """GMEM read traffic is independent of the receiver count.

    The same geometry runs unicast and broadcast on separate engines; the
    issued Get sequence (and therefore the bus traffic) must be identical.
    """
    logs = []
    for bcast in (0, 1):
        params = _params()
        top = _BcastTop(params, n_cores=4)
        engine = top.engine
        model = SparseRAM()
        smems = [bytearray(params['smem_params']['size']) for _ in range(4)]
        commits = [[] for _ in range(4)]
        done = [False]
        a_log = []

        def driver():
            yield from send_bcast(engine.cmd,
                                  id=2,
                                  bcast=bcast,
                                  src=0x100,
                                  nbytes=0x80,
                                  dst=0x40)
            res = yield from recv_done(engine.done)
            assert res == (2, 0)
            done[0] = True

        _run(
            top, *[
                _proc(dma_sink, engine, c, smems[c], commits[c], done)
                for c in range(4)
            ],
            _proc(tl_copy_responder, engine.mem_bus, model, done, a_log=a_log),
            driver, _proc(_watchdog, done))
        logs.append(a_log)
        if bcast:
            for c in range(4):
                assert bytes(smems[c][0x40:0x40 + 0x80]) == \
                    model.peek(0x100, 0x80), f'core {c} mismatch'
                assert sum(n for _, n in commits[c]) == 0x80
        else:
            assert bytes(smems[0][0x40:0x40 + 0x80]) == \
                model.peek(0x100, 0x80)
            assert all(not c for c in commits[1:]), \
                'a unicast command writes only its own core'

    assert logs[0] == logs[1], \
        'broadcast must not multiply GMEM requests'


def test_engine_broadcast_four_cores_asymmetric_backpressure():
    """A stalled receiver holds the replay cursor without data loss."""
    params = _params()
    top = _BcastTop(params, n_cores=4)
    engine = top.engine
    model = SparseRAM()
    smems = [bytearray(params['smem_params']['size']) for _ in range(4)]
    commits = [[] for _ in range(4)]
    done = [False]

    ready_fns = [
        lambda c: 1,
        lambda c: c % 3 != 0,
        lambda c: c > 60,
        lambda c: c % 2 == 0,
    ]

    def driver():
        yield from send_bcast(engine.cmd,
                              id=3,
                              src=0x2000,
                              nbytes=96,
                              dst=0x100)
        res = yield from recv_done(engine.done)
        assert res == (3, 0)
        for c in range(4):
            assert bytes(smems[c][0x100:0x100 + 96]) == \
                model.peek(0x2000, 96), f'core {c} mismatch'
            assert sum(n for _, n in commits[c]) == 96
        done[0] = True

    _run(
        top, *[
            _proc(dma_sink,
                  engine,
                  c,
                  smems[c],
                  commits[c],
                  done,
                  ready_fn=ready_fns[c]) for c in range(4)
        ], _proc(tl_copy_responder, engine.mem_bus, model, done), driver,
        _proc(_watchdog, done))


def test_engine_broadcast_simultaneous_commits_counted_once():
    """All four receivers commit the same fragment in the same cycle.

    Serial replay accepts one receiver per cycle, so per-core delays of
    4/3/2/1 mirror the acceptance spacing and land every receiver's
    acknowledgment for a fragment together. The outstanding-fragment
    count must fall by the number of firing ports: an any() decrement
    would leave it positive and hang completion behind the watchdog.
    """
    params = _params()
    top = _BcastTop(params, n_cores=4)
    engine = top.engine
    model = SparseRAM()
    smems = [bytearray(params['smem_params']['size']) for _ in range(4)]
    commits = [[] for _ in range(4)]
    done = [False]
    stats = {'max': 0, 'total': 0}

    def driver():
        yield from send_bcast(engine.cmd, id=7, src=0x40, nbytes=32, dst=0x80)
        res = yield from recv_done(engine.done)
        assert res == (7, 0)
        assert stats['max'] == 4, \
            'the delayed sinks must force four coincident commits'
        assert stats['total'] == 16, \
            'one commit per accepted fragment per receiver'
        for c in range(4):
            assert bytes(smems[c][0x80:0x80 + 32]) == \
                model.peek(0x40, 32), f'core {c} mismatch'
            assert sum(n for _, n in commits[c]) == 32
        done[0] = True

    _run(
        top, *[
            _proc(dma_sink_delayed,
                  engine,
                  c,
                  smems[c],
                  commits[c],
                  done,
                  delay=4 - c) for c in range(4)
        ], _proc(commit_monitor, engine, done, stats),
        _proc(tl_copy_responder, engine.mem_bus, model, done), driver,
        _proc(_watchdog, done))


def test_engine_broadcast_done_waits_for_withheld_receiver_commit():
    """A withheld receiver acknowledgment keeps the transfer open.

    Core 0 fully commits while core 1's acknowledgments are held back:
    `done` must stay low and the engine busy; releasing the withheld
    final acknowledgment is what completes the transfer.
    """
    params = _params()
    top = _BcastTop(params, n_cores=2)
    engine = top.engine
    model = SparseRAM()
    smems = [bytearray(params['smem_params']['size']) for _ in range(2)]
    commits = [[], []]
    done = [False]
    hold = [True]

    def driver():
        yield from send_bcast(engine.cmd, id=8, src=0x40, nbytes=16, dst=0x80)
        for _ in range(500):
            if sum(n for _, n in commits[0]) == 16:
                break
            yield
        assert sum(n for _, n in commits[0]) == 16, \
            'core 0 must fully commit while core 1 withholds'
        for _ in range(32):
            yield
        assert not (yield engine.done.valid), \
            'done must not fire while a receiver commit is withheld'
        assert (yield engine.busy)
        hold[0] = False
        res = yield from recv_done(engine.done)
        assert res == (8, 0)
        for c in range(2):
            assert bytes(smems[c][0x80:0x80 + 16]) == \
                model.peek(0x40, 16), f'core {c} mismatch'
            assert sum(n for _, n in commits[c]) == 16
        done[0] = True

    _run(
        top, _proc(dma_sink_delayed, engine, 0, smems[0], commits[0], done),
        _proc(dma_sink_delayed,
              engine,
              1,
              smems[1],
              commits[1],
              done,
              hold=hold), _proc(tl_copy_responder, engine.mem_bus, model,
                                done), driver, _proc(_watchdog, done))


def test_engine_broadcast_2d_pitched():
    """Pitched rows broadcast with per-row destination strides."""
    params = _params()
    top = _BcastTop(params, n_cores=2)
    engine = top.engine
    model = SparseRAM()
    smems = [bytearray(params['smem_params']['size']) for _ in range(2)]
    commits = [[], []]
    done = [False]

    def driver():
        yield from send_bcast(engine.cmd,
                              id=4,
                              mode=1,
                              src=0x10,
                              base=0x3000,
                              rcnt=3,
                              rbytes=16,
                              gs=64,
                              ss=32,
                              dst=0x40)
        res = yield from recv_done(engine.done)
        assert res == (4, 0)
        for c in range(2):
            for r in range(3):
                src_row = model.peek(0x3010 + 64 * r, 16)
                dst_row = bytes(smems[c][0x40 + 32 * r:0x40 + 32 * r + 16])
                assert dst_row == src_row, f'core {c} row {r} mismatch'
            assert sum(n for _, n in commits[c]) == 48
        done[0] = True

    _run(top, _proc(dma_sink, engine, 0, smems[0], commits[0], done),
         _proc(dma_sink, engine, 1, smems[1], commits[1], done),
         _proc(tl_copy_responder, engine.mem_bus, model, done), driver,
         _proc(_watchdog, done))


def test_engine_broadcast_zero_bytes_completes_without_traffic():
    params = _params()
    top = _BcastTop(params, n_cores=2)
    engine = top.engine
    done = [False]
    a_log = []
    smems = [bytearray(params['smem_params']['size']) for _ in range(2)]

    def driver():
        yield from send_bcast(engine.cmd, id=5, src=0, nbytes=0, dst=0)
        res = yield from recv_done(engine.done)
        assert res == (5, 0)
        assert a_log == []
        assert not any(any(s) for s in smems)
        done[0] = True

    _run(
        top, _proc(dma_sink, engine, 0, smems[0], [], done),
        _proc(dma_sink, engine, 1, smems[1], [], done),
        _proc(tl_copy_responder,
              engine.mem_bus,
              SparseRAM(),
              done,
              a_log=a_log), driver, _proc(_watchdog, done))


def test_engine_broadcast_rejects_destination_span():
    """An out-of-aperture broadcast is rejected before any core is written."""
    params = _params()
    top = _BcastTop(params, n_cores=2)
    engine = top.engine
    done = [False]
    a_log = []
    smems = [bytearray(params['smem_params']['size']) for _ in range(2)]

    def driver():
        yield from send_bcast(engine.cmd, id=6, src=0x40, nbytes=8, dst=0x8000)
        res = yield from recv_done(engine.done)
        assert res == (6, 1)
        assert a_log == []
        assert not any(any(s) for s in smems)
        done[0] = True

    _run(
        top, _proc(dma_sink, engine, 0, smems[0], [], done),
        _proc(dma_sink, engine, 1, smems[1], [], done),
        _proc(tl_copy_responder,
              engine.mem_bus,
              SparseRAM(),
              done,
              a_log=a_log), driver, _proc(_watchdog, done))


# ---------------------------------------------------------------------------
# Frontend: launch meta handling
# ---------------------------------------------------------------------------


def test_frontend_broadcast_meta_reaches_command():
    """meta[7] selects broadcast on the offered command."""
    top = _FrontendTop(_params())
    u = top.unit

    def driver():
        yield top.launch_ready.eq(0)
        yield from _fu_op(top,
                          opcode=UOpCode.GPU_COPY_ISSUE,
                          wid=1,
                          rs1=(0x80, 0x21 | (1 << 7), 0x40, 0),
                          rs2=(0x2000, (1 << 16) | 16, 0, 0))
        while not (yield u.copy_launch.valid):
            yield
        assert (yield u.copy_launch.bits.cmd.bcast) == 1
        assert (yield u.copy_launch.bits.cmd.id) == 1
        assert (yield u.copy_launch.bits.cmd.mode) == 1
        assert (yield u.copy_launch.bits.cmd.dst_offset) == 0x80
        yield
        assert (yield u.copy_launch.bits.cmd.bcast) == 1, \
            'a stalled offer must keep the receiver mode stable'

        yield top.launch_ready.eq(1)
        assert (yield from _fu_resp(top)) == 0

    run_test(top, driver, sync=True)


@pytest.mark.parametrize('meta_extra', [1 << 6, 1 << 12, 1 << 31])
def test_frontend_unsupported_meta_rejected_locally(meta_extra):
    """A barrier completion target or reserved meta bits never offer a
    command; the result is rejection and the warp's park is released."""
    top = _FrontendTop(_params())
    u = top.unit

    def driver():
        yield from _fu_op(top,
                          opcode=UOpCode.GPU_COPY_ISSUE,
                          wid=2,
                          rs1=(0x80, 0x21 | meta_extra, 0x40, 0),
                          rs2=(0x2000, (1 << 16) | 16, 0, 0))
        released = False
        for _ in range(8):
            assert not (yield u.copy_launch.valid), \
                'unsupported meta must not offer a command'
            if (yield u.warp_wake[2]):
                released = True
            yield
            if (yield u.resp.fire):
                break
        assert released, 'the parked warp must be released without an ack'
        assert (yield from _fu_resp(top)) == 1

    run_test(top, driver, sync=True)


# ---------------------------------------------------------------------------
# End-to-end core program
# ---------------------------------------------------------------------------


def _bcast_program():
    """Broadcast a 32-byte 1D copy (token 1), wait on the token, and load."""
    meta = 1 | (1 << 7)  # token 1, generation 0, mode 1D, broadcast
    return [
        addi(1, 0, 0xF),
        gpu_tmc(rs2=1),
        lui(7, COPY_BASE >> 12),
        addi(7, 7, 0x40),  # source offset inside the copy base
        addi(5, 0, BUF_A),  # destination offset
        addi(6, 0, meta),
        addi(8, 0, 32),  # nbytes
        wgather(5, 6, 7, 8, 0),  # launch vector in rs1
        gcopy(24, 5, 0),  # 1D launch with rs2 = x0: accepted, 0
        addi(30, 0, 0x55),  # computation while the copy may be in flight
        addi(6, 0, 1),  # token word {id 1, gen 0}
        gcopywait(6),
        lui(21, SMEM_BASE >> 12),
        addi(21, 21, BUF_A),
        lw(22, 21, 0),
        addi(31, 0, 0xAA),  # end marker
    ]


def test_core_broadcast_program_end_to_end():
    """A program broadcasts through the real engine and shared-memory
    endpoint, waits on the issuing token, and observes the committed bytes
    through a shared lane load."""
    prog = _bcast_program()
    image = bytearray()
    for word in prog:
        image += word.to_bytes(4, 'little')

    def pattern(a):
        off = a - PROG_BASE
        if 0 <= off < len(image):
            return image[off]
        return (a * 73 + 29) & 0xff

    model = SparseRAM(pattern=pattern)
    image_words = [0] * (PROG_BASE // 4) + [
        int.from_bytes(image[4 * i:4 * i + 4], 'little')
        for i in range(len(image) // 4)
    ]
    top = GcopyCoreTop(dict(E2E_PARAMS), image_words)
    done = [False]
    events = []

    def word_at(off):
        return int.from_bytes(model.peek(COPY_BASE + off, 4), 'little')

    def driver():
        while not any(e['ldst'] == 31 and e['data'][0] == 0xAA
                      for e in events):
            yield
        yield

        got = {}
        for e in events:
            if e['ldst'] in (24, 30, 22, 31) and e['ldst'] not in got:
                got[e['ldst']] = e['data'][0]
        assert got == {
            24: 0,  # launch accepted
            30: 0x55,  # computation retired
            22: word_at(0x40),  # broadcast data visible after the wait
            31: 0xAA,
        }

        for _ in range(64):
            if not (yield top.engine.busy) and not (yield top.completion.busy):
                break
            yield
        assert not (yield top.engine.busy)
        assert not (yield top.completion.busy)
        done[0] = True

    def monitor():
        from amaranth.sim import Passive
        yield Passive()
        wb = top.core.core_debug.wb_debug
        while True:
            if (yield wb.valid):
                data = [(yield wb.bits.data[0]), (yield wb.bits.data[1]),
                        (yield wb.bits.data[2]), (yield wb.bits.data[3])]
                events.append({
                    'wid': (yield wb.bits.wid),
                    'ldst': (yield wb.bits.ldst),
                    'tmask': (yield wb.bits.tmask),
                    'data': data,
                })
            yield

    _run(top, _proc(tl_copy_responder, top.engine.mem_bus, model, done),
         driver, monitor, _proc(_watchdog, done))
