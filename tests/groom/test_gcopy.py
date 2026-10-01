"""Tests for asynchronous-copy launch instructions and completion tokens.

* direct engine tests for pitched 2D commands: row walk, per-row masks and
  destination strides, launch-time rejection of destination/source span
  violations (including destinations too wide for the aperture), and
  zero-row commands;
* direct ``AsyncCopyCompletion`` tests: launch acceptance and generation
  guards, parking waits, done-event wakeup with and without waiters,
  status queries, the done mirror, retained results, same-cycle
  wait/completion races, generation rollover, and reset isolation;
* frontend (``AsyncCopyUnit``) tests: empty-mask no-ops, per-warp parked
  waits with reversed completion order, immediate-answer wake release,
  launch rejection results, and full-width destination operands;
* decode tests for ``gcopy``/``gcopywait``/``gcopystat`` including illegal
  encodings and the option gate;
* a wrapper wiring test asserting the done mirror stays drained;
* an end-to-end core test running a real program on one bare core with
  the cluster's engine and completion wiring attached: each warp packs
  launch and geometry vectors with wgather, launches a 2D copy, computes
  while the copy is deliberately incomplete, parks at the wait, reads
  the launch-rejection and retained-error results, double buffers
  through token reuse, and observes the copied bytes through real
  shared-memory lane loads.

All processes honour the amaranth ``pysim`` clock model documented in
AGENTS.md: only a naked ``yield`` advances the clock, and signal reads and
writes between naked yields are coherent within one cycle.
"""

import pytest
from amaranth import *
from amaranth.sim import Settle

from room.consts import FUType, IssueQueueType, RegisterType, UOpCode
from room.exc import Cause

from groom.async_copy import AsyncCopyCompletion, AsyncCopyEngine
from groom.core import Core
from groom.id_stage import DecodeUnit

from tests.sim import run_test
from tests.groom.encoding import addi, gcopy, gcopywait, gcopystat, \
    gpu_tmc, lui, lw, wgather
from tests.groom.sim import TLFetchROM
from tests.groom.test_async_copy import LINE_BYTES, SparseRAM, _params, \
    _proc, _run, _watchdog, dma_sink, recv_done, tl_copy_responder, \
    _cluster_params


def send_cmd2d(cmd,
               *,
               id,
               core,
               mode=1,
               src=0,
               base=0,
               rcnt=0,
               rbytes=0,
               gs=0,
               ss=0,
               dst=0,
               nbytes=0):
    """Drive one command port (1D or 2D) and wait for acceptance."""
    yield cmd.bits.id.eq(id)
    yield cmd.bits.core.eq(core)
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


# ---------------------------------------------------------------------------
# Engine: pitched 2D commands
# ---------------------------------------------------------------------------


def test_engine_2d_pitched_copy():
    """Three 16-byte rows with 64-byte source and 32-byte shared strides."""
    params = _params()
    top_engine = _EngineTop(params)
    engine = top_engine.engine
    model = SparseRAM()
    smem = bytearray(params['smem_params']['size'])
    commits = []
    done = [False]
    a_log = []

    def driver():
        yield from send_cmd2d(engine.cmd,
                              id=1,
                              core=0,
                              mode=1,
                              src=0x10,
                              base=0x2000,
                              rcnt=3,
                              rbytes=16,
                              gs=64,
                              ss=32,
                              dst=0x40)
        res = yield from recv_done(engine.done)
        assert res == (1, 0)
        assert a_log == [(0, 0x2000), (0, 0x2040), (0, 0x2080)], \
            'rows are serialized: each row drains before the next issues'
        for r in range(3):
            src_row = model.peek(0x2010 + 64 * r, 16)
            dst_row = bytes(smem[0x40 + 32 * r:0x40 + 32 * r + 16])
            assert dst_row == src_row, f'row {r} mismatch'
        assert sum(n for _, n in commits) == 48
        done[0] = True

    _run(top_engine, _proc(dma_sink, engine, 0, smem, commits, done),
         _proc(tl_copy_responder, engine.mem_bus, model, done, a_log=a_log),
         driver, _proc(_watchdog, done))


def test_engine_2d_unaligned_rows_share_lines():
    """Rows spanning line boundaries trim masks per row."""
    params = _params()
    top_engine = _EngineTop(params)
    engine = top_engine.engine
    model = SparseRAM()
    smem = bytearray(params['smem_params']['size'])
    commits = []
    done = [False]
    a_log = []

    def driver():
        # Row 0 = [0x2038, 0x2058) spans lines 0x2000 and 0x2040; row 1
        # starts exactly at row 0's end. Destination rows stay disjoint.
        yield from send_cmd2d(engine.cmd,
                              id=2,
                              core=1,
                              mode=1,
                              src=0x38,
                              base=0x2000,
                              rcnt=2,
                              rbytes=32,
                              gs=32,
                              ss=32,
                              dst=0x100)
        res = yield from recv_done(engine.done)
        assert res == (2, 0)
        # Row 0 spans lines 0x2000 and 0x2040 (two slots in flight); row 1
        # (0x2058..0x2078) lives inside line 0x2040, refetched on slot 0
        # after row 0 drained.
        assert a_log == [(0, 0x2000), (1, 0x2040), (0, 0x2040)]
        for r in range(2):
            src_row = model.peek(0x2038 + 32 * r, 32)
            dst_row = bytes(smem[0x100 + 32 * r:0x100 + 32 * r + 32])
            assert dst_row == src_row, f'row {r} mismatch'
        assert sum(n for _, n in commits) == 64
        done[0] = True

    _run(top_engine, _proc(dma_sink, engine, 1, smem, commits, done),
         _proc(tl_copy_responder, engine.mem_bus, model, done, a_log=a_log),
         driver, _proc(_watchdog, done))


def test_engine_2d_zero_rows_completes_without_traffic():
    params = _params()
    top_engine = _EngineTop(params)
    engine = top_engine.engine
    done = [False]
    a_log = []

    def driver():
        yield from send_cmd2d(engine.cmd,
                              id=3,
                              core=0,
                              mode=1,
                              src=0,
                              base=0x2000,
                              rcnt=0,
                              rbytes=64,
                              gs=64,
                              ss=32,
                              dst=0)
        res = yield from recv_done(engine.done)
        assert res == (3, 0)
        assert a_log == []
        done[0] = True

    _run(
        top_engine,
        _proc(tl_copy_responder,
              engine.mem_bus,
              SparseRAM(),
              done,
              a_log=a_log), driver, _proc(_watchdog, done))


def test_engine_2d_rejects_destination_span():
    """The shared-side stride walk must fit the aperture before traffic."""
    params = _params()
    top_engine = _EngineTop(params)
    engine = top_engine.engine
    done = [False]
    a_log = []
    smem = bytearray(params['smem_params']['size'])

    def driver():
        # Last row ends at 0x80 + 15 * 0x800 + 0x100, far past 16 KiB.
        yield from send_cmd2d(engine.cmd,
                              id=4,
                              core=0,
                              mode=1,
                              src=0,
                              base=0x2000,
                              rcnt=16,
                              rbytes=0x100,
                              gs=0x80,
                              ss=0x800,
                              dst=0x80)
        res = yield from recv_done(engine.done)
        assert res == (4, 1)
        assert a_log == []
        assert not any(smem)
        done[0] = True

    _run(
        top_engine,
        _proc(tl_copy_responder,
              engine.mem_bus,
              SparseRAM(),
              done,
              a_log=a_log), driver, _proc(_watchdog, done))


def test_engine_2d_rejects_source_span_wraparound():
    params = _params()
    top_engine = _EngineTop(params)
    engine = top_engine.engine
    done = [False]
    a_log = []

    def driver():
        # (rows - 1) * g_stride + row_bytes pushes the final row past
        # the 32-bit address space.
        yield from send_cmd2d(engine.cmd,
                              id=5,
                              core=0,
                              mode=1,
                              src=0xfffff000,
                              base=0,
                              rcnt=4,
                              rbytes=16,
                              gs=0x8000,
                              ss=64,
                              dst=0)
        res = yield from recv_done(engine.done)
        assert res == (5, 1)
        assert a_log == []
        done[0] = True

    _run(
        top_engine,
        _proc(tl_copy_responder,
              engine.mem_bus,
              SparseRAM(),
              done,
              a_log=a_log), driver, _proc(_watchdog, done))


def test_engine_rejects_destination_beyond_aperture_width():
    """A destination too wide for the aperture must be rejected, not wrapped.

    With 16 KiB of shared memory, dst 0x8000 no longer fits an
    aperture-width field: it must reach the bounds check intact (where it
    fails) instead of truncating to a valid unintended destination.
    """
    params = _params()
    top_engine = _EngineTop(params)
    engine = top_engine.engine
    done = [False]
    a_log = []
    smem = bytearray(params['smem_params']['size'])

    def driver():
        yield from send_cmd2d(engine.cmd,
                              id=6,
                              core=0,
                              mode=0,
                              src=0x40,
                              nbytes=8,
                              dst=0x8000)
        res = yield from recv_done(engine.done)
        assert res == (6, 1)
        assert a_log == []
        assert not any(smem)
        done[0] = True

    _run(
        top_engine,
        _proc(tl_copy_responder,
              engine.mem_bus,
              SparseRAM(),
              done,
              a_log=a_log), driver, _proc(_watchdog, done))


class _EngineTop(Elaboratable):

    def __init__(self, params, n_cores=2):
        self.engine = AsyncCopyEngine(n_cores, params, block_bytes=LINE_BYTES)

    def elaborate(self, platform):
        m = Module()
        m.submodules.engine = self.engine
        return m


# ---------------------------------------------------------------------------
# Completion tokens
# ---------------------------------------------------------------------------


class _CompTop(Elaboratable):

    def __init__(self, params, n_cores=2):
        self.comp = AsyncCopyCompletion(n_cores, params, core_id_width=1)

    def elaborate(self, platform):
        m = Module()
        m.submodules.comp = self.comp
        return m


def _send_launch(launch,
                 comp=None,
                 core=None,
                 *,
                 wid,
                 tid,
                 gen,
                 src=0x800,
                 nbytes=8,
                 dst=0x40):
    yield launch.bits.cmd.id.eq(tid)
    yield launch.bits.cmd.src_addr.eq(src)
    yield launch.bits.cmd.nbytes.eq(nbytes)
    yield launch.bits.cmd.dst_offset.eq(dst)
    yield launch.bits.gen.eq(gen)
    yield launch.bits.wid.eq(wid)
    yield launch.valid.eq(1)
    yield
    while not (yield launch.fire):
        yield
    res = None
    if comp is not None:
        res = ((yield comp.ack_valid[core]), (yield comp.ack_wid[core]),
               (yield comp.cmd.valid), (yield comp.cmd.bits.core))
    yield launch.valid.eq(0)
    yield
    return res


def _send_wait(wait, comp=None, core=None, *, wid, tid, gen, query):
    """Serve one wait/stat and capture the answer produced on the fire
    cycle (immediate answers pulse exactly then; a park holds it low)."""
    yield wait.bits.wid.eq(wid)
    yield wait.bits.id.eq(tid)
    yield wait.bits.gen.eq(gen)
    yield wait.bits.query.eq(query)
    yield wait.valid.eq(1)
    yield
    while not (yield wait.fire):
        yield
    res = None
    if comp is not None:
        res = ((yield comp.wake_valid[core]), (yield comp.wake_wid[core]),
               (yield comp.wake_stale[core]), (yield comp.wake_error[core]),
               (yield comp.wake_done[core]))
    yield wait.valid.eq(0)
    yield
    return res


def _send_done(done, comp=None, core=None, *, tid, error):
    """Deliver one engine done event, capturing any waiter wake that fires
    with its intake."""
    yield done.bits.id.eq(tid)
    yield done.bits.error.eq(error)
    yield done.valid.eq(1)
    yield
    while not (yield done.fire):
        yield
    res = None
    if comp is not None:
        res = ((yield comp.wake_valid[core]), (yield comp.wake_wid[core]),
               (yield comp.wake_stale[core]), (yield comp.wake_error[core]),
               (yield comp.wake_done[core]))
    yield done.valid.eq(0)
    yield
    return res


def test_completion_launch_done_wait_lifecycle():
    """A token with no waiter stays queryable until a matching wait."""
    comp = _CompTop(_params()).comp

    def driver():
        yield comp.cmd.ready.eq(1)

        # Launch token 3 (gen 0) from core 1, warp 2: forwarded to the
        # engine with the core filled in, and acknowledged in the same
        # cycle.
        res = yield from _send_launch(comp.launch[1],
                                      comp,
                                      1,
                                      wid=2,
                                      tid=3,
                                      gen=0)
        assert res == (1, 2, 1, 1)

        # Completion without a waiter: no wake, token queryable.
        res = yield from _send_done(comp.done, comp, 1, tid=3, error=0)
        assert res[0] == 0

        # A status query sees done=1 without consuming.
        res = yield from _send_wait(comp.wait[1],
                                    comp,
                                    1,
                                    wid=1,
                                    tid=3,
                                    gen=0,
                                    query=1)
        assert res == (1, 1, 0, 0, 1)

        # The matching wait consumes it and wakes immediately.
        res = yield from _send_wait(comp.wait[1],
                                    comp,
                                    1,
                                    wid=1,
                                    tid=3,
                                    gen=0,
                                    query=0)
        assert res == (1, 1, 0, 0, 1)

        # The consumed result is retained: a stat with the spent
        # generation still reports the transfer's outcome instead of a
        # bare stale, so the normal launch/compute/wait sequence can
        # read completion errors after the wait.
        res = yield from _send_wait(comp.wait[1],
                                    comp,
                                    1,
                                    wid=1,
                                    tid=3,
                                    gen=0,
                                    query=1)
        assert res == (1, 1, 0, 0, 1)

        # An id that matches no token and no retained result is stale.
        res = yield from _send_wait(comp.wait[1],
                                    comp,
                                    1,
                                    wid=1,
                                    tid=12,
                                    gen=0,
                                    query=1)
        assert res == (1, 1, 1, 0, 0)

        # The generation toggled: gen 1 may relaunch token 3, gen 0 may
        # not.
        res = yield from _send_launch(comp.launch[1],
                                      comp,
                                      1,
                                      wid=2,
                                      tid=3,
                                      gen=0)
        assert res[:2] == (1, 2) and res[2] == 0, \
            'wrong-generation launch must be dropped without a command'

        res = yield from _send_launch(comp.launch[1],
                                      comp,
                                      1,
                                      wid=2,
                                      tid=3,
                                      gen=1)
        assert res == (1, 2, 1, 1)

    run_test(comp, driver, sync=True)


def test_completion_wait_parks_until_done():
    comp = _CompTop(_params()).comp

    def driver():
        yield comp.cmd.ready.eq(1)
        yield from _send_launch(comp.launch[0], comp, 0, wid=3, tid=5, gen=0)

        # Park a waiter: no immediate wake.
        res = yield from _send_wait(comp.wait[0],
                                    comp,
                                    0,
                                    wid=3,
                                    tid=5,
                                    gen=0,
                                    query=0)
        assert res[0] == 0

        # Completion wakes the parked waiter and consumes the token.
        res = yield from _send_done(comp.done, comp, 0, tid=5, error=0)
        assert res == (1, 3, 0, 0, 1)

        # The spent generation reports the retained result, not stale.
        res = yield from _send_wait(comp.wait[0],
                                    comp,
                                    0,
                                    wid=0,
                                    tid=5,
                                    gen=0,
                                    query=1)
        assert res == (1, 0, 0, 0, 1)

    run_test(comp, driver, sync=True)


def test_completion_error_done_wakes_waiter():
    comp = _CompTop(_params()).comp

    def driver():
        yield comp.cmd.ready.eq(1)
        yield from _send_launch(comp.launch[0], comp, 0, wid=1, tid=6, gen=0)
        yield from _send_wait(comp.wait[0],
                              comp,
                              0,
                              wid=1,
                              tid=6,
                              gen=0,
                              query=0)
        res = yield from _send_done(comp.done, comp, 0, tid=6, error=1)
        assert res == (1, 1, 0, 1, 1)

    run_test(comp, driver, sync=True)


def test_completion_stale_launch_is_dropped():
    """Launching a busy or wrong-generation token never reaches the engine."""
    comp = _CompTop(_params()).comp

    def driver():
        yield comp.cmd.ready.eq(1)
        yield from _send_launch(comp.launch[0], comp, 0, wid=0, tid=2, gen=0)

        # Same token, same generation while in flight: dropped and
        # acknowledged without a second engine command.
        res = yield from _send_launch(comp.launch[0],
                                      comp,
                                      0,
                                      wid=1,
                                      tid=2,
                                      gen=0)
        assert res[:2] == (1, 1) and res[2] == 0

        # Consume the completed token, then try the spent generation.
        yield from _send_done(comp.done, comp, 0, tid=2, error=0)
        res = yield from _send_wait(comp.wait[0],
                                    comp,
                                    0,
                                    wid=0,
                                    tid=2,
                                    gen=0,
                                    query=0)
        assert res == (1, 0, 0, 0, 1)

        res = yield from _send_launch(comp.launch[0],
                                      comp,
                                      0,
                                      wid=2,
                                      tid=2,
                                      gen=0)
        assert res[:2] == (1, 2) and res[2] == 0

    run_test(comp, driver, sync=True)


def test_completion_second_waiter_is_stale():
    comp = _CompTop(_params()).comp

    def driver():
        yield comp.cmd.ready.eq(1)
        yield from _send_launch(comp.launch[0], comp, 0, wid=1, tid=7, gen=0)

        res = yield from _send_wait(comp.wait[0],
                                    comp,
                                    0,
                                    wid=1,
                                    tid=7,
                                    gen=0,
                                    query=0)
        assert res[0] == 0

        # A second waiter on the same token gets an immediate stale wake.
        res = yield from _send_wait(comp.wait[0],
                                    comp,
                                    0,
                                    wid=2,
                                    tid=7,
                                    gen=0,
                                    query=0)
        assert res == (1, 2, 1, 0, 0)

        # The original waiter still owns the completion.
        res = yield from _send_done(comp.done, comp, 0, tid=7, error=0)
        assert res == (1, 1, 0, 0, 1)

    run_test(comp, driver, sync=True)


def test_completion_done_mirror_flow_through():
    """Unknown-id done events pass through to done_out untouched.

    The flow-through queue fires done_out in the intake cycle itself,
    and more than two events drain without blocking while the mirror
    output stays ready.
    """
    comp = _CompTop(_params()).comp

    def driver():
        yield comp.cmd.ready.eq(1)

        yield comp.done_out.ready.eq(1)
        for i in range(4):
            yield comp.done.bits.id.eq(8 + i)
            yield comp.done.bits.error.eq(0)
            yield comp.done.valid.eq(1)
            yield
            while not (yield comp.done.fire):
                yield
            assert (yield comp.done_out.valid)
            assert (yield comp.done_out.bits.id) == 8 + i
            assert (yield comp.done_out.fire)
            yield comp.done.valid.eq(0)
            yield
        yield comp.done_out.ready.eq(0)
        yield

    run_test(comp, driver, sync=True)


def test_completion_wait_on_free_token_answers_stale():
    """Only a FLIGHT token may park: a never-launched token is stale.

    A wait on a FREE token with the reset generation previously
    registered a waiter that nothing could ever wake, parking the warp
    forever.
    """
    comp = _CompTop(_params()).comp

    def driver():
        yield comp.cmd.ready.eq(1)

        res = yield from _send_wait(comp.wait[0],
                                    comp,
                                    0,
                                    wid=1,
                                    tid=4,
                                    gen=0,
                                    query=0)
        assert res == (1, 1, 1, 0, 0), 'FREE wait must answer stale'

        res = yield from _send_wait(comp.wait[0],
                                    comp,
                                    0,
                                    wid=1,
                                    tid=4,
                                    gen=0,
                                    query=1)
        assert res == (1, 1, 1, 0, 0), 'FREE query must answer stale'

        # No waiter was registered and the token is still FREE: a launch
        # with the same generation is accepted.
        res = yield from _send_launch(comp.launch[0],
                                      comp,
                                      0,
                                      wid=2,
                                      tid=4,
                                      gen=0)
        assert res == (1, 2, 1, 0)

    run_test(comp, driver, sync=True)


def test_completion_wait_and_done_race_in_one_cycle():
    """A wait whose fire cycle carries the done event consumes it."""
    comp = _CompTop(_params()).comp

    def driver():
        yield comp.cmd.ready.eq(1)
        yield from _send_launch(comp.launch[0], comp, 0, wid=3, tid=6, gen=0)

        yield comp.wait[0].bits.wid.eq(3)
        yield comp.wait[0].bits.id.eq(6)
        yield comp.wait[0].bits.gen.eq(0)
        yield comp.wait[0].bits.query.eq(0)
        yield comp.wait[0].valid.eq(1)
        yield comp.done.bits.id.eq(6)
        yield comp.done.bits.error.eq(0)
        yield comp.done.valid.eq(1)
        yield comp.done_out.ready.eq(1)
        yield
        while not (yield comp.wait[0].fire):
            assert (yield comp.done.valid), 'done must stay offered'
            yield
        assert (yield comp.done.fire)
        res = ((yield comp.wake_valid[0]), (yield comp.wake_wid[0]),
               (yield
                comp.wake_stale[0]), (yield
                                      comp.wake_error[0]), (yield
                                                            comp.wake_done[0]))
        assert res == (1, 3, 0, 0, 1), \
            'same-cycle completion must answer done, not park'
        yield comp.wait[0].valid.eq(0)
        yield comp.done.valid.eq(0)
        yield

        # The token was consumed by the racing wait: the next generation
        # launches, and the spent generation reads the retained result.
        res = yield from _send_launch(comp.launch[0],
                                      comp,
                                      0,
                                      wid=1,
                                      tid=6,
                                      gen=1)
        assert res == (1, 1, 1, 0)
        res = yield from _send_wait(comp.wait[0],
                                    comp,
                                    0,
                                    wid=1,
                                    tid=6,
                                    gen=0,
                                    query=1)
        assert res == (1, 1, 0, 0, 1)

    run_test(comp, driver, sync=True)


def test_completion_error_result_survives_wait_consumption():
    """The retained result reports the error after the wait consumed it."""
    comp = _CompTop(_params()).comp

    def driver():
        yield comp.cmd.ready.eq(1)
        yield from _send_launch(comp.launch[0], comp, 0, wid=1, tid=2, gen=0)
        yield from _send_wait(comp.wait[0],
                              comp,
                              0,
                              wid=1,
                              tid=2,
                              gen=0,
                              query=0)
        res = yield from _send_done(comp.done, comp, 0, tid=2, error=1)
        assert res == (1, 1, 0, 1, 1)

        # The wait consumed the token, but the failed transfer's status
        # must remain observable through the spent handle.
        res = yield from _send_wait(comp.wait[0],
                                    comp,
                                    0,
                                    wid=0,
                                    tid=2,
                                    gen=0,
                                    query=1)
        assert res == (1, 0, 0, 1, 1)

        # Repeating the wait with the spent handle is idempotent: it
        # answers the retained result without parking or reconsuming.
        res = yield from _send_wait(comp.wait[0],
                                    comp,
                                    0,
                                    wid=0,
                                    tid=2,
                                    gen=0,
                                    query=0)
        assert res == (1, 0, 0, 1, 1)

    run_test(comp, driver, sync=True)


def test_completion_retained_results_are_per_token():
    """Another token's consumption cannot overwrite a token's result.

    Token A fails and wakes its waiter; token B completes before A's
    software reaches its status instruction. A's spent handle must still
    report A's error, and B's handle B's outcome.
    """
    comp = _CompTop(_params()).comp

    def driver():
        yield comp.cmd.ready.eq(1)
        # Three done events flow through this test; the mirror queue
        # holds two, so keep the mirror drained.
        yield comp.done_out.ready.eq(1)

        # Two in-flight tokens with registered waiters on distinct cores.
        yield from _send_launch(comp.launch[0], comp, 0, wid=1, tid=2, gen=0)
        yield from _send_wait(comp.wait[0],
                              comp,
                              0,
                              wid=1,
                              tid=2,
                              gen=0,
                              query=0)
        yield from _send_launch(comp.launch[1], comp, 1, wid=3, tid=5, gen=0)
        yield from _send_wait(comp.wait[1],
                              comp,
                              1,
                              wid=3,
                              tid=5,
                              gen=0,
                              query=0)

        # A fails first, then B completes — both consumed by their wakes.
        res = yield from _send_done(comp.done, comp, 0, tid=2, error=1)
        assert res == (1, 1, 0, 1, 1)
        res = yield from _send_done(comp.done, comp, 1, tid=5, error=0)
        assert res == (1, 3, 0, 0, 1)

        # A's status instruction arrives only after B's completion.
        res = yield from _send_wait(comp.wait[0],
                                    comp,
                                    0,
                                    wid=0,
                                    tid=2,
                                    gen=0,
                                    query=1)
        assert res == (1, 0, 0, 1, 1), \
            "token A's error must survive token B's consumption"

        res = yield from _send_wait(comp.wait[1],
                                    comp,
                                    1,
                                    wid=0,
                                    tid=5,
                                    gen=0,
                                    query=1)
        assert res == (1, 0, 0, 0, 1)

        # A's result survives further consumptions of *other* tokens,
        # including another failure, and only A's own next consumption
        # (a relaunch through the toggled generation) replaces it.
        yield from _send_launch(comp.launch[1], comp, 1, wid=3, tid=5, gen=1)
        yield from _send_wait(comp.wait[1],
                              comp,
                              1,
                              wid=3,
                              tid=5,
                              gen=1,
                              query=0)
        yield from _send_done(comp.done, comp, 1, tid=5, error=1)

        res = yield from _send_wait(comp.wait[0],
                                    comp,
                                    0,
                                    wid=0,
                                    tid=2,
                                    gen=0,
                                    query=1)
        assert res == (1, 0, 0, 1, 1)

    run_test(comp, driver, sync=True)


def test_completion_query_during_waiter_completion():
    """A cross-core status query in the done's own cycle sees the result.

    The completing done consumes the waiter's token and toggles its
    generation in the fire cycle; the retained-result registers only
    update at the edge. A query from another core landing in that same
    cycle must have the completing transfer's result forwarded to it,
    not stale — same-core queries are deferred by the wake blocking and
    never observe this window.
    """
    comp = _CompTop(_params()).comp

    def driver():
        yield comp.cmd.ready.eq(1)
        yield comp.done_out.ready.eq(1)

        # Token 2 launched from core 0 with a parked waiter (warp 1).
        yield from _send_launch(comp.launch[0], comp, 0, wid=1, tid=2, gen=0)
        res = yield from _send_wait(comp.wait[0],
                                    comp,
                                    0,
                                    wid=1,
                                    tid=2,
                                    gen=0,
                                    query=0)
        assert res[0] == 0, 'waiter parked'

        # Core 1 queries the same handle in the very cycle the failed
        # transfer completes core 0's waiter.
        yield comp.wait[1].bits.wid.eq(0)
        yield comp.wait[1].bits.id.eq(2)
        yield comp.wait[1].bits.gen.eq(0)
        yield comp.wait[1].bits.query.eq(1)
        yield comp.wait[1].valid.eq(1)
        yield comp.done.bits.id.eq(2)
        yield comp.done.bits.error.eq(1)
        yield comp.done.valid.eq(1)
        yield
        while not (yield comp.wait[1].fire):
            assert (yield comp.done.valid), 'done must stay offered'
            yield
        assert (yield comp.done.fire)

        # Core 1's query is answered with the forwarded result; core 0's
        # waiter wakes with the same error in the same cycle.
        assert ((yield comp.wake_valid[1]), (yield comp.wake_stale[1]),
                (yield comp.wake_error[1]),
                (yield comp.wake_done[1])) == (1, 0, 1, 1), \
            'query racing a completion must report {done, error}'
        assert ((yield comp.wake_valid[0]), (yield comp.wake_wid[0]),
                (yield
                 comp.wake_error[0]), (yield
                                       comp.wake_done[0])) == (1, 1, 1, 1)

        yield comp.wait[1].valid.eq(0)
        yield comp.done.valid.eq(0)
        yield

        # After the edge, the registered retained result serves the same
        # handle identically.
        res = yield from _send_wait(comp.wait[1],
                                    comp,
                                    1,
                                    wid=0,
                                    tid=2,
                                    gen=0,
                                    query=1)
        assert res == (1, 0, 0, 1, 1)

    run_test(comp, driver, sync=True)


def test_completion_generation_rollover():
    """Two consumptions return the generation to its first value."""
    comp = _CompTop(_params()).comp

    def driver():
        yield comp.cmd.ready.eq(1)

        # First transfer: gen 0, completes cleanly.
        yield from _send_launch(comp.launch[0], comp, 0, wid=0, tid=1, gen=0)
        yield from _send_done(comp.done, comp, 0, tid=1, error=0)
        yield from _send_wait(comp.wait[0],
                              comp,
                              0,
                              wid=0,
                              tid=1,
                              gen=0,
                              query=0)

        # Second transfer: gen 1, fails.
        yield from _send_launch(comp.launch[0], comp, 0, wid=0, tid=1, gen=1)
        yield from _send_done(comp.done, comp, 0, tid=1, error=1)
        yield from _send_wait(comp.wait[0],
                              comp,
                              0,
                              wid=0,
                              tid=1,
                              gen=1,
                              query=0)

        # The generation rolled back to 0 and the token relaunches.
        res = yield from _send_launch(comp.launch[0],
                                      comp,
                                      0,
                                      wid=0,
                                      tid=1,
                                      gen=0)
        assert res == (1, 0, 1, 0)

        # The retained result is the most recently consumed transfer.
        res = yield from _send_wait(comp.wait[0],
                                    comp,
                                    0,
                                    wid=0,
                                    tid=1,
                                    gen=1,
                                    query=1)
        assert res == (1, 0, 0, 1, 1)

        # The relaunched generation is in flight, not done.
        res = yield from _send_wait(comp.wait[0],
                                    comp,
                                    0,
                                    wid=0,
                                    tid=1,
                                    gen=0,
                                    query=1)
        assert res == (1, 0, 0, 0, 0)

    run_test(comp, driver, sync=True)


def test_completion_reset_clears_tokens_waiters_and_retained():
    """Coordinated reset leaves no live tokens, waiters, or results."""

    class _RstTop(Elaboratable):

        def __init__(self, params):
            self.rst = Signal()
            self.comp = AsyncCopyCompletion(2, params, core_id_width=1)

        def elaborate(self, platform):
            m = Module()
            m.submodules.comp = ResetInserter(self.rst)(self.comp)
            return m

    top = _RstTop(_params())
    comp = top.comp

    def driver():
        yield comp.cmd.ready.eq(1)
        yield from _send_launch(comp.launch[0], comp, 0, wid=1, tid=0, gen=0)
        res = yield from _send_wait(comp.wait[0],
                                    comp,
                                    0,
                                    wid=1,
                                    tid=0,
                                    gen=0,
                                    query=0)
        assert res[0] == 0, 'waiter parked'

        # Coordinated reset: an explicit abort.
        yield top.rst.eq(1)
        yield
        yield
        yield top.rst.eq(0)
        yield

        # The token is FREE again: the same generation launches, and a
        # wait with that generation answers stale instead of parking on
        # state that no longer exists.
        res = yield from _send_wait(comp.wait[0],
                                    comp,
                                    0,
                                    wid=1,
                                    tid=0,
                                    gen=0,
                                    query=0)
        assert res == (1, 1, 1, 0, 0)

        res = yield from _send_launch(comp.launch[0],
                                      comp,
                                      0,
                                      wid=1,
                                      tid=0,
                                      gen=0)
        assert res == (1, 1, 1, 0)

    run_test(top, driver, sync=True)


# ---------------------------------------------------------------------------
# Frontend unit
# ---------------------------------------------------------------------------


class _FrontendTop(Elaboratable):
    """``AsyncCopyUnit`` with scripted acknowledgement and wake sources.

    The acknowledgement mirrors the completion unit: it pulses
    combinationally in the launch's fire cycle, carrying the programmed
    reject status. Waits are answered either immediately in their fire
    cycle (``imm_answer``, like stale/retained/done-at-fire answers) or
    later through a one-cycle ``wk_*`` pulse (like a registered waiter's
    asynchronous completion).
    """

    def __init__(self, params):
        from groom.fu import AsyncCopyUnit

        n_warps = params['n_warps']
        self.unit = AsyncCopyUnit(params)
        self.launch_ready = Signal(reset=1)
        self.wait_ready = Signal(reset=1)
        self.ack_reject = Signal()
        self.imm_answer = Signal()
        self.imm_stale = Signal()
        self.imm_error = Signal()
        self.imm_done = Signal()
        self.wk_valid = Signal()
        self.wk_wid = Signal(range(n_warps))
        self.wk_stale = Signal()
        self.wk_error = Signal()
        self.wk_done = Signal()

    def elaborate(self, platform):
        m = Module()
        u = self.unit
        m.submodules.unit = u
        m.d.comb += [
            u.copy_launch.ready.eq(self.launch_ready),
            u.copy_ack_valid.eq(u.copy_launch.fire),
            u.copy_ack_wid.eq(u.copy_launch.bits.wid),
            u.copy_ack_reject.eq(self.ack_reject),
            u.copy_wait.ready.eq(self.wait_ready),
            u.copy_wake_valid.eq((u.copy_wait.fire & self.imm_answer)
                                 | self.wk_valid),
            u.copy_wake_wid.eq(
                Mux(self.wk_valid, self.wk_wid, u.copy_wait.bits.wid)),
            u.copy_wake_stale.eq(
                Mux(self.wk_valid, self.wk_stale, self.imm_stale)),
            u.copy_wake_error.eq(
                Mux(self.wk_valid, self.wk_error, self.imm_error)),
            u.copy_wake_done.eq(Mux(self.wk_valid, self.wk_done,
                                    self.imm_done)),
            u.resp.ready.eq(1),
        ]
        return m


def _fu_op(top,
           *,
           opcode,
           wid,
           tmask=0b1111,
           rs1=(0, 0, 0, 0),
           rs2=(0, 0, 0, 0)):
    """Present one execution request and wait for its acceptance."""
    u = top.unit
    yield u.req.bits.wid.eq(wid)
    yield u.req.bits.uop.opcode.eq(opcode)
    yield u.req.bits.uop.tmask.eq(tmask)
    for lane in range(4):
        yield u.req.bits.rs1_data[lane].eq(rs1[lane] & 0xffffffff)
        yield u.req.bits.rs2_data[lane].eq(rs2[lane] & 0xffffffff)
    yield u.req.valid.eq(1)
    yield
    while not (yield u.req.fire):
        yield
    yield u.req.valid.eq(0)
    yield


def _fu_resp(top):
    """Wait for the unit's response and return lane 0's data."""
    u = top.unit
    while not (yield u.resp.fire):
        yield
    data = (yield u.resp.bits.data[0])
    yield
    return data


def test_frontend_empty_mask_offers_nothing():
    """Empty-mask launch/wait/status never assert the cluster ports."""
    top = _FrontendTop(_params())
    u = top.unit

    def driver():
        for opcode in (UOpCode.GPU_COPY_ISSUE, UOpCode.GPU_COPY_WAIT,
                       UOpCode.GPU_COPY_STAT):
            rs1 = (0x8000, 1, 0x100,
                   8) if (opcode == UOpCode.GPU_COPY_ISSUE) else (5, 0, 0, 0)
            yield from _fu_op(top, opcode=opcode, wid=0, tmask=0, rs1=rs1)
            # The cluster ports must stay silent while the no-op drains;
            # the response fires within a few cycles.
            fired = False
            for _ in range(8):
                assert not (yield u.copy_launch.valid), \
                    'empty-mask launch offered a command'
                assert not (yield u.copy_wait.valid), \
                    'empty-mask wait offered a request'
                if (yield u.resp.fire):
                    fired = True
                yield
                if fired:
                    break
            assert fired, 'empty-mask op never responded'

    run_test(top, driver, sync=True)


def test_frontend_launch_result_reports_rejection():
    """The launch acknowledgement's reject status becomes rd."""
    top = _FrontendTop(_params())
    u = top.unit

    def driver():
        geometry = (0x2000, (3 << 16) | 16, (64 << 16) | 32, 0)

        yield top.ack_reject.eq(0)
        yield from _fu_op(top,
                          opcode=UOpCode.GPU_COPY_ISSUE,
                          wid=2,
                          rs1=(0x80, 0x21, 0x40, 0),
                          rs2=geometry)
        assert (yield from _fu_resp(top)) == 0, 'accepted launch reports 0'

        # Same token in flight again: the drop acknowledgement carries
        # the reject status and the instruction writes 1.
        yield top.ack_reject.eq(1)
        yield from _fu_op(top,
                          opcode=UOpCode.GPU_COPY_ISSUE,
                          wid=2,
                          rs1=(0x80, 0x21, 0x40, 0),
                          rs2=geometry)
        assert (yield from _fu_resp(top)) == 1, 'rejected launch reports 1'

    run_test(top, driver, sync=True)


def test_frontend_preserves_full_width_destination():
    """The destination operand reaches the command untruncated.

    With 16 KiB of shared memory, 0x8000 used to truncate to zero before
    the engine could bounds-check it.
    """
    top = _FrontendTop(_params())
    u = top.unit

    def driver():
        yield top.launch_ready.eq(0)
        yield from _fu_op(top,
                          opcode=UOpCode.GPU_COPY_ISSUE,
                          wid=1,
                          rs1=(0x8000, 0x21, 0x40, 0),
                          rs2=(0x2000, (1 << 16) | 16, 0, 0))
        while not (yield u.copy_launch.valid):
            yield
        # The offer must present the full 32-bit destination and the
        # token identity until it is accepted.
        assert (yield u.copy_launch.bits.cmd.dst_offset) == 0x8000
        assert (yield u.copy_launch.bits.cmd.id) == 1
        assert (yield u.copy_launch.bits.gen) == 0
        assert (yield u.copy_launch.bits.cmd.mode) == 1
        yield
        assert (yield u.copy_launch.bits.cmd.dst_offset) == 0x8000, \
            'stalled offer must keep the full destination'

        yield top.launch_ready.eq(1)
        data = yield from _fu_resp(top)
        assert data == 0
        assert not (yield u.copy_launch.valid)

    run_test(top, driver, sync=True)


def test_frontend_immediate_answer_releases_park_in_fire_cycle():
    """A stale/immediate wait answer wakes its warp in the fire cycle."""
    top = _FrontendTop(_params())
    u = top.unit

    def driver():
        yield top.imm_answer.eq(1)
        yield top.imm_stale.eq(1)
        yield from _fu_op(top,
                          opcode=UOpCode.GPU_COPY_WAIT,
                          wid=2,
                          rs1=(5, 0, 0, 0))
        # The scheduler park rides a one-cycle pipe, so the release must
        # pulse in the wait's own fire cycle.
        while True:
            if (yield u.copy_wait.fire):
                assert (yield u.warp_wake[2]) == 1, \
                    'immediate answer must wake in the fire cycle'
                assert (yield u.warp_wake[0]) == 0
                assert (yield u.warp_wake[1]) == 0
                break
            yield
        assert (yield from _fu_resp(top)) == 0

        # A query with the same immediate answer writes the status word.
        yield top.imm_stale.eq(0)
        yield top.imm_done.eq(1)
        yield from _fu_op(top,
                          opcode=UOpCode.GPU_COPY_STAT,
                          wid=3,
                          rs1=(5, 0, 0, 0))
        while True:
            if (yield u.copy_wait.fire):
                assert (yield u.warp_wake[3]) == 0, \
                    'queries do not park, so nothing must wake'
                break
            yield
        assert (yield from _fu_resp(top)) == 1  # {done}

    run_test(top, driver, sync=True)


def test_frontend_two_parked_waits_wake_independently():
    """Completions for several parked waiters cannot be lost.

    Two warps wait on distinct tokens; the second-registered waiter
    completes first and must not suppress the first waiter's later wake.
    """
    top = _FrontendTop(_params())
    u = top.unit

    def driver():
        # Neither wait is answered at fire: both park.
        yield from _fu_op(top,
                          opcode=UOpCode.GPU_COPY_WAIT,
                          wid=0,
                          rs1=(1, 0, 0, 0))
        yield from _fu_op(top,
                          opcode=UOpCode.GPU_COPY_WAIT,
                          wid=1,
                          rs1=(2, 0, 0, 0))

        # Completion order is reversed relative to wait registration.
        yield top.wk_valid.eq(1)
        yield top.wk_wid.eq(1)
        yield top.wk_done.eq(1)
        yield
        assert (yield u.warp_wake[1]) == 1
        assert (yield u.warp_wake[0]) == 0, \
            'the other waiter must stay parked'
        yield top.wk_valid.eq(0)
        yield top.wk_done.eq(0)
        yield

        # The first waiter's completion still releases it.
        yield top.wk_valid.eq(1)
        yield top.wk_wid.eq(0)
        yield top.wk_done.eq(1)
        yield
        assert (yield u.warp_wake[0]) == 1
        assert (yield u.warp_wake[1]) == 0, \
            'an already-woken waiter must not re-wake'
        yield top.wk_valid.eq(0)
        yield top.wk_done.eq(0)
        yield

    run_test(top, driver, sync=True)


# ---------------------------------------------------------------------------
# Decode
# ---------------------------------------------------------------------------

DECODE_PARAMS = {
    'is_groom': True,
    'xlen': 32,
    'flen': 32,
    'use_fpu': False,
    'vaddr_bits': 32,
    'io_regions': {},
    'fma_latency': 2,
    'n_cores': 1,
    'n_warps': 4,
    'n_threads': 4,
    'n_barriers': 4,
    'use_raster': False,
    'issue_params': {
        'queue_depth': 4,
    },
    'smem_params': dict(base=0x20000, size=0x4000, n_banks=4),
}


def test_decode_gcopy():
    dut = DecodeUnit(dict(DECODE_PARAMS, use_async_copy=True))

    def proc():
        yield dut.in_uop.inst.eq(gcopy(24, 3, 4))
        yield Settle()

        assert (yield dut.out_uop.opcode) == UOpCode.GPU_COPY_ISSUE
        assert (yield dut.out_uop.iq_type) == IssueQueueType.INT
        assert (yield dut.out_uop.fu_type) == FUType.COPY
        assert (yield dut.out_uop.lrs1) == 3
        assert (yield dut.out_uop.lrs2) == 4
        assert (yield dut.out_uop.lrs1_rtype) == RegisterType.FIX
        assert (yield dut.out_uop.lrs2_rtype) == RegisterType.FIX
        # The launch result (0 accepted, 1 rejected) lands in rd.
        assert (yield dut.out_uop.ldst) == 24
        assert (yield dut.out_uop.ldst_valid)
        assert (yield dut.out_uop.dst_rtype) == RegisterType.FIX
        assert (yield dut.out_uop.stall_warp)

    run_test(dut, proc)


def test_decode_gcopywait():
    dut = DecodeUnit(dict(DECODE_PARAMS, use_async_copy=True))

    def proc():
        yield dut.in_uop.inst.eq(gcopywait(6))
        yield Settle()

        assert (yield dut.out_uop.opcode) == UOpCode.GPU_COPY_WAIT
        assert (yield dut.out_uop.fu_type) == FUType.COPY
        assert (yield dut.out_uop.lrs1) == 6
        assert (yield dut.out_uop.lrs1_rtype) == RegisterType.FIX
        assert (yield dut.out_uop.stall_warp)
        assert not (yield dut.out_uop.ldst_valid)

    run_test(dut, proc)


def test_decode_gcopystat():
    dut = DecodeUnit(dict(DECODE_PARAMS, use_async_copy=True))

    def proc():
        yield dut.in_uop.inst.eq(gcopystat(3, 6))
        yield Settle()

        assert (yield dut.out_uop.opcode) == UOpCode.GPU_COPY_STAT
        assert (yield dut.out_uop.fu_type) == FUType.COPY
        assert (yield dut.out_uop.ldst) == 3
        assert (yield dut.out_uop.ldst_valid)
        assert (yield dut.out_uop.dst_rtype) == RegisterType.FIX
        assert not (yield dut.out_uop.stall_warp)

    run_test(dut, proc)


@pytest.mark.parametrize('funct3', [0, 1, 2, 3, 4, 5, 6, 7])
def test_decode_gcopy_gated_by_option(funct3):
    inst = (((funct3 & 0x7) << 12) | (4 << 7) | 0b1111011)
    dut = DecodeUnit(dict(DECODE_PARAMS, use_async_copy=False))

    def proc():
        yield dut.in_uop.inst.eq(inst)
        yield Settle()

        assert (yield dut.out_uop.exception)
        assert (yield dut.out_uop.exc_cause) == Cause.ILLEGAL_INSTRUCTION

    run_test(dut, proc)


@pytest.mark.parametrize('funct3', [3, 4, 5, 6, 7])
def test_decode_gcopy_illegal_funct3(funct3):
    inst = (((funct3 & 0x7) << 12) | (4 << 7) | 0b1111011)
    dut = DecodeUnit(dict(DECODE_PARAMS, use_async_copy=True))

    def proc():
        yield dut.in_uop.inst.eq(inst)
        yield Settle()

        assert (yield dut.out_uop.exception)
        assert (yield dut.out_uop.exc_cause) == Cause.ILLEGAL_INSTRUCTION

    run_test(dut, proc)


@pytest.mark.parametrize('opcode_name, tmask, stall, copy',
                         [('gcopy', 0b1111, 1, 1), ('gcopy', 0, 0, 0),
                          ('gcopywait', 0b1111, 1, 1), ('gcopywait', 0, 0, 0),
                          ('gcopystat', 0b1111, 0, 0)])
def test_decode_empty_mask_copy_never_stalls(opcode_name, tmask, stall, copy):
    """An empty-mask launch/wait neither parks nor stalls the warp.

    STALL-based parking has no wake path for a no-op, so an empty-mask
    gcopy would otherwise hang its warp at decode.
    """
    from groom.id_stage import DecodeStage

    if opcode_name == 'gcopy':
        inst = gcopy(2, 3, 4)
    elif opcode_name == 'gcopywait':
        inst = gcopywait(3)
    else:
        inst = gcopystat(2, 3)

    dut = DecodeStage(dict(DECODE_PARAMS, use_async_copy=True))

    def proc():
        yield dut.fetch_packet.bits.wid.eq(0)
        yield dut.fetch_packet.bits.uop.inst.eq(inst)
        yield dut.fetch_packet.bits.uop.tmask.eq(tmask)
        yield dut.fetch_packet.valid.eq(1)
        yield dut.ready.eq(1)
        yield Settle()

        assert (yield dut.stall_req.valid)
        assert (yield dut.stall_req.bits.stall) == stall
        assert (yield dut.stall_req.bits.copy) == copy

    run_test(dut, proc)


# ---------------------------------------------------------------------------
# Cluster wiring
# ---------------------------------------------------------------------------


def test_completion_busy_covers_unconsumed_tokens():
    """The completion unit stays busy until results are consumed.

    ``copy_busy`` ORs this into ``Cluster.busy``, so software waiting for
    a clean disable observes both engine activity and unconsumed tokens.
    """
    comp = _CompTop(_params()).comp

    def driver():
        yield comp.cmd.ready.eq(1)
        yield comp.done_out.ready.eq(1)
        assert not (yield comp.busy)

        yield from _send_launch(comp.launch[0], comp, 0, wid=0, tid=1, gen=0)
        assert (yield comp.busy), 'in-flight token must count as busy'

        yield from _send_done(comp.done, comp, 0, tid=1, error=0)
        assert (yield comp.busy), \
            'completed-but-unconsumed token must keep busy'

        res = yield from _send_wait(comp.wait[0],
                                    comp,
                                    0,
                                    wid=0,
                                    tid=1,
                                    gen=0,
                                    query=0)
        assert res[0] == 1
        for _ in range(2):
            yield
        assert not (yield comp.busy), 'consumed token must clear busy'

    run_test(comp, driver, sync=True)


def test_groom_wrapper_drains_cluster_copy_done():
    """The wrapper drains the optional done mirror.

    The wrapper attaches no consumer to ``cluster.copy_done``; leaving it
    undriven would let the two-entry mirror block the third completion,
    so the wrapper must drain the output itself.
    """
    from groom.wrapper import GroomWrapper

    class _WrapperTop(Elaboratable):

        def __init__(self, params):
            self.wrapper = GroomWrapper(params)

        def elaborate(self, platform):
            m = Module()
            m.submodules.wrapper = self.wrapper
            return m

    top = _WrapperTop(_cluster_params())

    def driver():
        cluster = top.wrapper.clusters[0]
        for _ in range(8):
            yield
            assert (yield cluster.copy_done.ready) == 1, \
                'wrapper must keep the cluster copy_done mirror drained'

    run_test(top, driver, sync=True)


# ---------------------------------------------------------------------------
# End-to-end core program
# ---------------------------------------------------------------------------

SMEM_BASE = 0x20000
COPY_BASE = 0x2000
ROWS, RBYTES, GS, SS = 3, 16, 64, 32
BUF_A, BUF_B = 0x80, 0x180
# Distinct source rows for every vector so each buffer's content is
# attributable to exactly one launch.
SRC_VEC1, SRC_VEC2 = 0x40, 0x140
SRC_VEC3, SRC_VEC4 = 0x240, 0x340
META_TEMPLATE = (1 << 5)  # mode 2D; token id and generation filled in
PROG_BASE = 0x1000


def _meta(token, gen):
    return token | (gen << 4) | META_TEMPLATE


def _program():
    """Launch/compute/wait with two in-flight tokens and token reuse.

    Round one launches both buffers while their transfers are held
    incomplete, so the computation after the launches provably retires
    before either completion; a duplicate launch reports rejection, the
    waits park, and the loads only observe committed bytes afterwards.
    Round two relaunches both tokens through the toggled generations.
    The tail launch reuses token 2 after its generation rolled back to
    zero, aiming past the aperture: the engine must reject it, the wait
    must still complete, and the retained status must report the error.
    """
    return [
        addi(1, 0, 0xF),
        gpu_tmc(rs2=1),  # activate the full warp
        # geometry (shared by every 2D launch): 3 rows x 16 bytes,
        # 64-byte global stride, 32-byte shared stride
        lui(10, COPY_BASE >> 12),  # 0x2000
        lui(11, (ROWS << 16) >> 12),  # row_count in the high half
        addi(11, 11, RBYTES),
        lui(12, (GS << 16) >> 12),  # g_stride in the high half
        addi(12, 12, SS),
        wgather(10, 11, 12, 0, 0),  # geometry vector in rs2
        # round one: two tokens in flight together (gen 0)
        addi(5, 0, BUF_A),
        addi(6, 0, _meta(1, 0)),
        addi(7, 0, SRC_VEC1),
        wgather(5, 6, 7, 0, 0),  # launch vector for token 1
        addi(9, 0, BUF_B),
        addi(6, 0, _meta(2, 0)),
        addi(7, 0, SRC_VEC2),
        wgather(9, 6, 7, 0, 0),  # launch vector for token 2
        gcopy(24, 5, 10),  # accepted: 0
        gcopy(25, 9, 10),  # accepted: 0
        gcopy(23, 5, 10),  # duplicate token 1 while in flight: rejected 1
        addi(30, 0, 0x55),  # computation while both copies are incomplete
        addi(6, 0, 1),
        gcopywait(6),  # parks until token 1 completes
        addi(6, 0, 2),
        gcopywait(6),  # parks until token 2 completes
        addi(6, 0, 2),
        gcopystat(20, 6),  # retained token 2 result: {done} = 1
        lui(21, SMEM_BASE >> 12),  # 0x20000
        addi(21, 21, BUF_A),
        lw(22, 21, 0),
        addi(21, 21, BUF_B - BUF_A),
        lw(18, 21, 0),
        # round two: both tokens relaunch through the toggled generations
        addi(5, 0, BUF_A),
        addi(6, 0, _meta(1, 1)),
        addi(7, 0, SRC_VEC3),
        wgather(5, 6, 7, 0, 0),
        addi(9, 0, BUF_B),
        addi(6, 0, _meta(2, 1)),
        addi(7, 0, SRC_VEC4),
        wgather(9, 6, 7, 0, 0),
        gcopy(26, 5, 10),  # accepted through the rolled generation: 0
        gcopy(27, 9, 10),  # accepted: 0
        addi(6, 0, 0x11),  # token word {id 1, gen 1}
        gcopywait(6),
        addi(6, 0, 0x12),  # token word {id 2, gen 1}
        gcopywait(6),
        lui(21, SMEM_BASE >> 12),
        addi(21, 21, BUF_A),
        lw(16, 21, 0),
        addi(21, 21, BUF_B - BUF_A),
        lw(17, 21, 0),
        # tail: token 2's generation rolled back to zero; a destination
        # past the aperture must be rejected by the engine, wake the
        # wait anyway, and leave the error queryable
        lui(5, 0x8000 >> 12),  # 0x8000: far past the 16 KiB aperture
        addi(6, 0, 2),  # token word {id 2, gen 0}, mode 1D
        lui(7, COPY_BASE >> 12),
        addi(7, 7, SRC_VEC1),
        addi(8, 0, 16),  # nbytes
        wgather(5, 6, 7, 8, 0),
        gcopy(28, 5, 10),  # launch accepted: 0
        gcopywait(6),  # parks, wakes on the failed transfer
        gcopystat(29, 6),  # retained {done, error} = 3
        addi(31, 0, 0xAA),  # end marker
    ]


def _word(model, addr):
    return int.from_bytes(model.peek(addr, 4), 'little')


E2E_PARAMS = {
    'is_groom': True,
    'xlen': 32,
    'flen': 32,
    'use_fpu': False,
    'vaddr_bits': 32,
    'io_regions': {},
    'fma_latency': 2,
    'n_cores': 1,
    'n_warps': 4,
    'n_threads': 4,
    'n_barriers': 4,
    'use_raster': False,
    'issue_params': {
        'queue_depth': 4,
    },
    'icache_params': dict(n_sets=8, n_ways=2, block_bytes=64),
    'smem_params': dict(base=SMEM_BASE, size=0x4000, n_banks=4),
    'use_async_copy': True,
    'pma_regions': [(0, 0x40000000, 'rw', True)],
}


class GcopyCoreTop(Elaboratable):
    """One core with the cluster's copy wiring attached directly.

    The core, engine, and completion unit all live in the simulator's
    sync domain (the wrapper puts cores in per-core domains, which pysim
    does not clock), so this top exercises decode, the frontend's
    park/ack path, token bookkeeping, the 2D engine, and the real shared
    memory endpoint end to end.
    """

    def __init__(self, params, image):
        self.params = params
        self.image = list(image)
        self.core = Core(params, sim_debug=True)
        self.engine = AsyncCopyEngine(1, params, block_bytes=LINE_BYTES)
        self.completion = AsyncCopyCompletion(1, params, core_id_width=1)

    def elaborate(self, platform):
        m = Module()
        core = m.submodules.core = self.core
        engine = m.submodules.engine = self.engine
        completion = m.submodules.completion = self.completion
        m.submodules.rom = TLFetchROM(core.ibus, self.image)

        m.d.comb += [
            core.reset_vector.eq(PROG_BASE),
            core.copy_launch.connect(completion.launch[0]),
            core.copy_wait.connect(completion.wait[0]),
            core.copy_ack_valid.eq(completion.ack_valid[0]),
            core.copy_ack_wid.eq(completion.ack_wid[0]),
            core.copy_ack_reject.eq(completion.ack_reject[0]),
            core.copy_wake_valid.eq(completion.wake_valid[0]),
            core.copy_wake_wid.eq(completion.wake_wid[0]),
            core.copy_wake_stale.eq(completion.wake_stale[0]),
            core.copy_wake_error.eq(completion.wake_error[0]),
            core.copy_wake_done.eq(completion.wake_done[0]),
            completion.cmd.connect(engine.cmd),
            engine.done.connect(completion.done),
            # The mirror is optional observation; drain it so it can
            # never back-pressure completions.
            completion.done_out.ready.eq(1),
            engine.dma_req[0].connect(core.dma_req),
            core.dma_commit.connect(engine.dma_commit[0]),
        ]

        return m


def test_core_gcopy_program_end_to_end():
    """A warp double-buffers launches, computes during the copy, waits,
    and reads.

    Both round-one launches are accepted while their transfers are held
    incomplete (the subordinate withholds its responses), the duplicate
    launch reports rejection in its result register, and the independent
    computation retires before any completion; the parked waits then
    release only after committed data is visible to the lane loads. The
    second round relaunches both tokens through toggled generations, and
    the tail's out-of-aperture launch reports its engine rejection
    through the wait-then-stat sequence.
    """
    prog = _program()
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
    core = top.core
    done = [False]
    events = []
    release = [False]

    def word_at(off):
        return _word(model, COPY_BASE + off)

    def driver():

        def seen(reg, val=None):
            for e in events:
                if e['ldst'] == reg:
                    return val is None or e['data'][0] == val
            return False

        # Phase one: computation must retire while the copies are
        # incomplete. Both launches were accepted (24 and 25 read zero),
        # the duplicate was rejected (23 reads one), and 0x55 landed —
        # all before any completion existed.
        for _ in range(5000):
            if seen(24, 0) and seen(25, 0) and seen(23, 1) and seen(30, 0x55):
                break
            yield
        assert seen(24, 0) and seen(25, 0), 'launches must be accepted'
        assert seen(23, 1), 'duplicate launch must report rejection'
        assert seen(30, 0x55), 'computation must retire during the copy'
        assert (yield top.engine.busy), \
            'the transfer must still be incomplete at the checkpoint'
        assert not any(seen(r) for r in (22, 18, 16, 17)), \
            'loads cannot retire before the parked waits release'

        # Release the subordinate: completions arrive, the waits wake,
        # and the program drains to its end marker.
        release[0] = True
        while not any(e['ldst'] == 31 and e['data'][0] == 0xAA
                      for e in events):
            yield
        yield

        expected = {
            23: 1,  # duplicate launch rejected
            24: 0,
            25: 0,
            26: 0,
            27: 0,
            28: 0,
            20: 1,  # retained token 2 result: {done}
            29: 3,  # retained failed transfer: {done, error}
            30: 0x55,
            22: word_at(SRC_VEC1),
            18: word_at(SRC_VEC2),
            16: word_at(SRC_VEC3),
            17: word_at(SRC_VEC4),
            31: 0xAA,
        }
        got = {}
        for e in events:
            if e['ldst'] in expected and e['ldst'] not in got:
                got[e['ldst']] = e['data'][0]
        assert got == expected, f'{got} != {expected}'

        # The engine and completion unit must fully drain.
        for _ in range(64):
            if not (yield top.engine.busy) and not (yield top.completion.busy):
                break
            yield
        assert not (yield top.engine.busy)
        assert not (yield top.completion.busy)
        # The tail's out-of-aperture launch set the engine's sticky
        # error status, which software observes through the retained
        # token result (register 29 above).
        assert (yield top.engine.error)
        done[0] = True

    def monitor():
        from amaranth.sim import Passive
        yield Passive()
        wb = core.core_debug.wb_debug
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

    _run(
        top,
        _proc(tl_copy_responder,
              top.engine.mem_bus,
              model,
              done,
              release=release), driver, monitor,
        _proc(_watchdog, done, limit=30000))
