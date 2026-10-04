"""Tests for the asynchronous global-to-shared copy engine.

* direct engine tests with a scripted TileLink responder: aligned/unaligned
  copies, byte masks, multi-beat reassembly, per-slot source IDs, reordered
  and interleaved responses, delayed beats, denied/corrupt responses,
  PMA/MMIO/overflow and destination launch rejection, zero-length commands,
  full-line overfetch validation, stalled Get/fragment stability, destination
  backpressure with read-slot exhaustion, completion backpressure, command
  queue fullness, per-core fragment routing, and the busy/error status;
* engine + real ``L2Cache`` coherence tests: clean miss and hit, and dirty
  local/remote owners surrendering data on probe (the non-client source ID
  must force the L2 to probe every dirty owner);
* real ``DCache`` dirty stores copied through L2 into real ``SharedMemory``,
  checking local/remote source ranges, stalled writes, and lane-load
  visibility after completion;
* a ``Cluster`` wrapper test proving the copy source-ID construction and
  D-channel demultiplexing with the cores and DCache held in reset;
* a launch-port width-crossing test reproducing the wrapper's core-to-
  completion connection under multi-cluster geometry, where the command
  core field is two bits wide on the core side and one bit wide on the
  cluster side.

All processes honour the amaranth ``pysim`` clock model documented in
AGENTS.md: only a naked ``yield`` advances the clock, and signal reads/writes
between naked yields are coherent within one cycle.
"""

import json
from pathlib import Path

import room  # noqa: F401  (import order resolves package circular imports)
import pytest
from amaranth import *
from amaranth.sim import Simulator

from groom.async_copy import AsyncCopyCompletion, AsyncCopyEngine, AsyncCopyLaunch
from groom.lsu import SharedMemory
from room.consts import MemoryCommand
from room.dcache import DCache
from roomsoc.interconnect import tilelink as tl
from roomsoc.interconnect.stream import Decoupled

LINE_BYTES = 64
BEATS_PER_LINE = LINE_BYTES // 8
QUEUE_DEPTH = 8
N_SLOTS = 8


def _params(**overrides):
    with open(
            Path(__file__).resolve().parents[2] /
            'config/groom/default.json') as f:
        params = json.load(f)
    params['io_regions'] = {
        int(base): size
        for base, size in params['io_regions'].items()
    }
    params['use_async_copy'] = True
    params['pma_regions'] = [(0, 0x40000000, 'rw', True),
                             (0x80000000, 0x80000000, 'rw', True)]
    params.update(overrides)
    return params


class SparseRAM:
    """Byte-addressable golden memory with an address-derived pattern."""

    def __init__(self, pattern=lambda a: (a * 73 + 29) & 0xff):
        self.pattern = pattern

    def peek(self, addr, nbytes):
        return bytes(self.pattern(a) for a in range(addr, addr + nbytes))


class CopyTop(Elaboratable):
    """Bare ``AsyncCopyEngine`` wrapper."""

    def __init__(self, params, n_cores=2, block_bytes=LINE_BYTES):
        self.engine = AsyncCopyEngine(n_cores, params, block_bytes=block_bytes)

    def elaborate(self, platform):
        m = Module()
        m.submodules.engine = self.engine
        return m


def _proc(genfunc, *args, **kwargs):

    def wrapper():
        yield from genfunc(*args, **kwargs)

    return wrapper


def _watchdog(done, limit=30000):
    for _ in range(limit):
        if done[0]:
            return
        yield
    raise AssertionError(
        f'copy engine simulation deadlocked within {limit} cycles')


def _run(top, *procs):
    sim = Simulator(top)
    sim.add_clock(1e-6)
    for proc in procs:
        sim.add_sync_process(proc)
    sim.run()


def send_cmd(cmd, *, id, core, src, nbytes, dst):
    """Drive one command port and wait for its acceptance."""
    yield cmd.bits.id.eq(id)
    yield cmd.bits.core.eq(core)
    yield cmd.bits.src_addr.eq(src)
    yield cmd.bits.nbytes.eq(nbytes)
    yield cmd.bits.dst_offset.eq(dst)
    yield cmd.valid.eq(1)
    yield
    while not (yield cmd.ready):
        yield
    yield cmd.valid.eq(0)
    yield


def recv_done(done):
    """Wait for ``done``, accept it, and return ``(id, error)``.

    The ready pulse fires the event at the next edge, so ``done.valid``
    stays high during the fire cycle itself. Waiting for ``valid`` to fall
    before returning prevents a subsequent ``recv_done`` from re-consuming
    the same event.
    """
    while not (yield done.valid):
        yield
    res = ((yield done.bits.id), (yield done.bits.error))
    yield done.ready.eq(1)
    yield
    yield done.ready.eq(0)
    while (yield done.valid):
        yield
    return res


def dma_sink(engine, core, smem, commits, done, *, ready_fn=lambda c: 1):
    """Behavioural shared-memory DMA endpoint for one core.

    Accepts 64-bit byte-masked fragments, applies them to ``smem`` and
    acknowledges each accepted fragment with one commit event two cycles
    later, mirroring the two-cycle grant-to-commit distance of the real
    ``SharedMemory`` endpoint. While ``ready_fn`` withholds ``ready``, the
    stalled offer payload must stay stable.
    """
    port = engine.dma_req[core]
    commit = engine.dma_commit[core]
    pending = []
    stalled = None
    cycle = 0
    while not (done[0] and not pending):
        yield port.ready.eq(ready_fn(cycle))
        yield commit.valid.eq(0)
        valid = (yield port.valid)
        if valid:
            payload = ((yield port.bits.id), (yield port.bits.offset),
                       (yield port.bits.data), (yield port.bits.byte_enable))
            if stalled is not None:
                assert payload == stalled, \
                    'stalled DMA offer changed payload between cycles'
        if valid and (yield port.ready):
            fid, offset, data, mask = payload
            for k in range(8):
                if (mask >> k) & 1:
                    assert offset + k < len(smem), 'DMA write out of bounds'
                    smem[offset + k] = (data >> (8 * k)) & 0xff
            pending.append((cycle + 2, fid, bin(mask).count('1')))
            stalled = None
        else:
            stalled = payload if valid else None
        if pending and pending[0][0] == cycle + 1:
            _, fid, nbytes = pending.pop(0)
            yield commit.valid.eq(1)
            yield commit.bits.id.eq(fid)
            yield commit.bits.nbytes.eq(nbytes)
            commits.append((fid, nbytes))
        yield
        cycle += 1


def _respond_line(bus,
                  model,
                  source,
                  addr,
                  *,
                  denied=False,
                  corrupt=False,
                  beat_gap=0):
    """Answer one full-line Get with ``BEATS_PER_LINE`` AccessAckData beats."""
    line = model.peek(addr, LINE_BYTES)
    for i in range(BEATS_PER_LINE):
        data = int.from_bytes(line[8 * i:8 * i + 8], 'little')
        yield bus.d.bits.opcode.eq(tl.ChannelDOpcode.AccessAckData)
        yield bus.d.bits.param.eq(0)
        yield bus.d.bits.size.eq(6)
        yield bus.d.bits.source.eq(source)
        yield bus.d.bits.sink.eq(0)
        yield bus.d.bits.denied.eq(int(denied))
        yield bus.d.bits.corrupt.eq(int(corrupt))
        yield bus.d.bits.data.eq(0 if denied else data)
        yield bus.d.valid.eq(1)
        yield
        while not (yield bus.d.ready):
            yield
        if beat_gap and i != BEATS_PER_LINE - 1:
            yield bus.d.valid.eq(0)
            for _ in range(beat_gap):
                yield
    yield bus.d.valid.eq(0)
    yield


def tl_copy_responder(bus,
                      model,
                      done,
                      *,
                      a_log=None,
                      order='fifo',
                      beat_gap=0,
                      first_delay=0,
                      denied=(),
                      corrupt=(),
                      release=None,
                      interleave=False):
    """Scripted TileLink subordinate for the copy engine's Get stream.

    Collects fired A beats, then answers each full-line Get from ``model``.
    ``order`` picks FIFO or LIFO response order across pending Gets.
    ``release`` is a one-element list gating when responses may start, which
    lets tests first fill every read slot.  ``interleave`` rotates one beat
    at a time across pending responses, which is legal TileLink traffic
    since they carry distinct sources.  ``denied``/``corrupt`` are sets of
    line addresses to fail.

    Acceptance uses the one-shot arm discipline from AGENTS.md: the request
    payload is captured and ``a.ready`` is dropped within the same cycle,
    so the DUT cannot fire a second, uncaptured request before the drop
    lands.  While ``release`` gates responses, ``a.ready`` stays high and
    one request per cycle is captured instead.
    """
    a_log = a_log if a_log is not None else []
    pending = []
    yield bus.a.ready.eq(1)
    yield bus.d.valid.eq(0)
    yield
    while not done[0]:
        # Sample the bus once per cycle. When capturing an ungated
        # request, ready is dropped within the same evaluation so at most
        # the captured request fires; while gated, ready stays high and
        # every fired request is captured each cycle.
        gated = release is not None and not release[0]
        valid = (yield bus.a.valid)
        source = addr = None
        if valid:
            source = (yield bus.a.bits.source)
            addr = (yield bus.a.bits.address)
            opcode = (yield bus.a.bits.opcode)
            size = (yield bus.a.bits.size)
            assert opcode == tl.ChannelAOpcode.Get.value
            assert size == 6, 'engine must request full cache lines'
            assert addr % LINE_BYTES == 0, 'engine Get not line aligned'
            if not gated:
                yield bus.a.ready.eq(0)
        yield
        if valid:
            a_log.append((source, addr))
            pending.append((source, addr))
        if gated or not pending:
            continue
        if first_delay:
            for _ in range(first_delay):
                yield
            first_delay = 0
        # Hold off further requests while responding. After a gated
        # collection phase a.ready is still high, and a slot freed by a
        # completed drain would let the DUT fire an uncaptured Get here.
        yield bus.a.ready.eq(0)
        yield
        if interleave and len(pending) > 1:
            active = [[s, a, 0] for s, a in pending]
            pending.clear()
            while active:
                for resp in active[:]:
                    s, a, beat = resp
                    yield from _respond_one_beat(bus, model, s, a, beat)
                    resp[2] += 1
                    if resp[2] == BEATS_PER_LINE:
                        active.remove(resp)
                if beat_gap:
                    yield bus.d.valid.eq(0)
                    for _ in range(beat_gap):
                        yield
            yield bus.d.valid.eq(0)
        else:
            while pending:
                if order == 'fifo':
                    source, addr = pending.pop(0)
                else:
                    source, addr = pending.pop()
                yield from _respond_line(bus,
                                         model,
                                         source,
                                         addr,
                                         denied=addr in denied,
                                         corrupt=addr in corrupt,
                                         beat_gap=beat_gap)
        yield bus.a.ready.eq(1)
        yield
    yield bus.d.valid.eq(0)


def _respond_one_beat(bus, model, source, addr, beat):
    line = model.peek(addr, LINE_BYTES)
    data = int.from_bytes(line[8 * beat:8 * beat + 8], 'little')
    yield bus.d.bits.opcode.eq(tl.ChannelDOpcode.AccessAckData)
    yield bus.d.bits.param.eq(0)
    yield bus.d.bits.size.eq(6)
    yield bus.d.bits.source.eq(source)
    yield bus.d.bits.sink.eq(0)
    yield bus.d.bits.denied.eq(0)
    yield bus.d.bits.corrupt.eq(0)
    yield bus.d.bits.data.eq(data)
    yield bus.d.valid.eq(1)
    yield
    while not (yield bus.d.ready):
        yield


# ---------------------------------------------------------------------------
# Basic 1D copies
# ---------------------------------------------------------------------------
def test_engine_aligned_copy_bytes_and_beats():
    params = _params()
    top = CopyTop(params)
    engine = top.engine
    model = SparseRAM()
    smem = bytearray(params['smem_params']['size'])
    commits = []
    done = [False]
    a_log = []

    def driver():
        yield from send_cmd(engine.cmd,
                            id=1,
                            core=0,
                            src=0x2000,
                            nbytes=128,
                            dst=0)
        assert (yield engine.busy)
        res = yield from recv_done(engine.done)
        assert res == (1, 0)
        assert a_log == [(0, 0x2000), (1, 0x2040)], \
            'expected two sequential line Gets with distinct slot sources'
        expected = model.peek(0x2000, 128)
        assert bytes(smem[:128]) == expected
        # Two full lines at 8 bytes per fragment.
        assert len(commits) == 16
        assert sum(n for _, n in commits) == 128
        assert {fid for fid, _ in commits} == {1}
        # Everything has drained: busy must fall once done was consumed.
        for _ in range(4):
            if not (yield engine.busy):
                break
            yield
        assert not (yield engine.busy)
        assert not (yield engine.error)
        done[0] = True

    _run(top, _proc(dma_sink, engine, 0, smem, commits, done),
         _proc(tl_copy_responder, engine.mem_bus, model, done, a_log=a_log),
         driver, _proc(_watchdog, done))


def test_engine_unaligned_copy_masks_fragments():
    params = _params()
    top = CopyTop(params)
    engine = top.engine
    model = SparseRAM()
    smem = bytearray(params['smem_params']['size'])
    commits = []
    done = [False]
    a_log = []

    def driver():
        # 100 bytes starting 5 bytes into a line: the copy spans lines
        # 0x1000 and 0x1040, with partial first and last fragments.
        yield from send_cmd(engine.cmd,
                            id=2,
                            core=0,
                            src=0x1005,
                            nbytes=100,
                            dst=0x80)
        res = yield from recv_done(engine.done)
        assert res == (2, 0)
        assert [addr for _, addr in a_log] == [0x1000, 0x1040]
        assert bytes(smem[0x80:0x80 + 100]) == model.peek(0x1005, 100)
        # Bytes outside the range must stay untouched.
        assert smem[:0x80] == bytearray(0x80)
        assert smem[0x80 + 100:0x80 + 112] == bytearray(12)
        # 1 (3 B) + 7 (8 B) + 5 (8 B) + 1 (1 B) fragments.
        assert len(commits) == 14
        assert sum(n for _, n in commits) == 100
        done[0] = True

    _run(top, _proc(dma_sink, engine, 0, smem, commits, done),
         _proc(tl_copy_responder, engine.mem_bus, model, done, a_log=a_log),
         driver, _proc(_watchdog, done))


def test_engine_zero_length_completes_without_traffic():
    params = _params()
    top = CopyTop(params)
    engine = top.engine
    model = SparseRAM()
    smem = bytearray(params['smem_params']['size'])
    commits = []
    done = [False]
    a_log = []

    def driver():
        yield from send_cmd(engine.cmd,
                            id=7,
                            core=0,
                            src=0x2000,
                            nbytes=0,
                            dst=0x40)
        res = yield from recv_done(engine.done)
        assert res == (7, 0)
        assert a_log == []
        assert commits == []
        assert smem == bytearray(len(smem))
        done[0] = True

    _run(top, _proc(dma_sink, engine, 0, smem, commits, done),
         _proc(tl_copy_responder, engine.mem_bus, model, done, a_log=a_log),
         driver, _proc(_watchdog, done))


def test_engine_routes_fragments_to_command_core():
    params = _params()
    top = CopyTop(params)
    engine = top.engine
    model = SparseRAM()
    smem0 = bytearray(params['smem_params']['size'])
    smem1 = bytearray(params['smem_params']['size'])
    commits0 = []
    commits1 = []
    done = [False]

    def core0_guard():
        # The unselected core's DMA request port must stay silent.
        while not done[0]:
            assert not (yield engine.dma_req[0].valid)
            yield

    def driver():
        yield from send_cmd(engine.cmd,
                            id=4,
                            core=1,
                            src=0x3000,
                            nbytes=64,
                            dst=0x20)
        res = yield from recv_done(engine.done)
        assert res == (4, 0)
        assert bytes(smem1[0x20:0x60]) == model.peek(0x3000, 64)
        assert smem0 == bytearray(len(smem0))
        assert commits0 == []
        assert {fid for fid, _ in commits1} == {4}
        done[0] = True

    _run(top, _proc(dma_sink, engine, 0, smem0, commits0, done),
         _proc(dma_sink, engine, 1, smem1, commits1, done),
         _proc(tl_copy_responder, engine.mem_bus, model, done), driver,
         core0_guard, _proc(_watchdog, done))


# ---------------------------------------------------------------------------
# Response ordering, interleaving, and pacing
# ---------------------------------------------------------------------------
def test_engine_out_of_order_slot_responses():
    params = _params()
    top = CopyTop(params)
    engine = top.engine
    model = SparseRAM()
    smem = bytearray(params['smem_params']['size'])
    commits = []
    done = [False]
    a_log = []
    release = [False]

    def driver():
        # 512 bytes = eight lines = all read slots. The responder collects
        # every Get before answering in reverse issue order.
        yield from send_cmd(engine.cmd,
                            id=3,
                            core=0,
                            src=0x4000,
                            nbytes=512,
                            dst=0)
        for _ in range(16):
            yield
        assert len(a_log) == N_SLOTS
        release[0] = True
        res = yield from recv_done(engine.done)
        assert res == (3, 0)
        assert bytes(smem[:512]) == model.peek(0x4000, 512)
        assert sum(n for _, n in commits) == 512
        done[0] = True

    _run(
        top, _proc(dma_sink, engine, 0, smem, commits, done),
        _proc(tl_copy_responder,
              engine.mem_bus,
              model,
              done,
              a_log=a_log,
              order='lifo',
              release=release), driver, _proc(_watchdog, done))


def test_engine_interleaved_beats_across_slots():
    params = _params()
    top = CopyTop(params)
    engine = top.engine
    model = SparseRAM()
    smem = bytearray(params['smem_params']['size'])
    commits = []
    done = [False]
    release = [False]

    def driver():
        yield from send_cmd(engine.cmd,
                            id=5,
                            core=0,
                            src=0x5000,
                            nbytes=256,
                            dst=0x100)
        for _ in range(12):
            yield
        release[0] = True
        res = yield from recv_done(engine.done)
        assert res == (5, 0)
        assert bytes(smem[0x100:0x200]) == model.peek(0x5000, 256)
        assert sum(n for _, n in commits) == 256
        done[0] = True

    _run(
        top, _proc(dma_sink, engine, 0, smem, commits, done),
        _proc(tl_copy_responder,
              engine.mem_bus,
              model,
              done,
              interleave=True,
              release=release), driver, _proc(_watchdog, done))


def test_engine_delayed_and_gapped_beats():
    params = _params()
    top = CopyTop(params)
    engine = top.engine
    model = SparseRAM()
    smem = bytearray(params['smem_params']['size'])
    commits = []
    done = [False]

    def driver():
        yield from send_cmd(engine.cmd,
                            id=6,
                            core=0,
                            src=0x6000,
                            nbytes=128,
                            dst=0x200)
        res = yield from recv_done(engine.done)
        assert res == (6, 0)
        assert bytes(smem[0x200:0x280]) == model.peek(0x6000, 128)
        done[0] = True

    _run(
        top, _proc(dma_sink, engine, 0, smem, commits, done),
        _proc(tl_copy_responder,
              engine.mem_bus,
              model,
              done,
              first_delay=9,
              beat_gap=3), driver, _proc(_watchdog, done))


# ---------------------------------------------------------------------------
# Error handling
# ---------------------------------------------------------------------------
def test_engine_denied_response_fails_but_drains_good_lines():
    params = _params()
    top = CopyTop(params)
    engine = top.engine
    model = SparseRAM()
    smem = bytearray(params['smem_params']['size'])
    commits = []
    done = [False]

    def driver():
        yield from send_cmd(engine.cmd,
                            id=8,
                            core=0,
                            src=0x2000,
                            nbytes=128,
                            dst=0)
        res = yield from recv_done(engine.done)
        assert res == (8, 1)
        assert (yield engine.error), 'error status must be sticky'
        # The good line was still committed; the denied line never landed.
        assert bytes(smem[:64]) == model.peek(0x2000, 64)
        assert smem[64:128] == bytearray(64)
        # The engine stays usable: a clean follow-up command completes.
        yield from send_cmd(engine.cmd,
                            id=9,
                            core=0,
                            src=0x7000,
                            nbytes=64,
                            dst=0x400)
        res = yield from recv_done(engine.done)
        assert res == (9, 0)
        assert bytes(smem[0x400:0x440]) == model.peek(0x7000, 64)
        assert (yield engine.error), 'sticky error survives later success'
        done[0] = True

    _run(
        top, _proc(dma_sink, engine, 0, smem, commits, done),
        _proc(tl_copy_responder, engine.mem_bus, model, done, denied={0x2040}),
        driver, _proc(_watchdog, done))


def test_engine_corrupt_response_fails_transfer():
    params = _params()
    top = CopyTop(params)
    engine = top.engine
    model = SparseRAM()
    smem = bytearray(params['smem_params']['size'])
    commits = []
    done = [False]

    def driver():
        yield from send_cmd(engine.cmd,
                            id=10,
                            core=0,
                            src=0x2800,
                            nbytes=128,
                            dst=0)
        res = yield from recv_done(engine.done)
        assert res == (10, 1)
        assert (yield engine.error)
        assert bytes(smem[:64]) == model.peek(0x2800, 64)
        assert smem[64:128] == bytearray(64)
        done[0] = True

    _run(
        top, _proc(dma_sink, engine, 0, smem, commits, done),
        _proc(tl_copy_responder, engine.mem_bus, model, done,
              corrupt={0x2840}), driver, _proc(_watchdog, done))


def test_engine_failure_mid_address_generation_still_completes():
    params = _params()
    top = CopyTop(params)
    engine = top.engine
    model = SparseRAM()
    smem = bytearray(params['smem_params']['size'])
    commits = []
    done = [False]
    a_log = []
    release = [False]

    def driver():
        # Sixteen lines but only eight read slots: when line 2 comes back
        # denied, address generation is still unfinished. The engine must
        # stop offering new Gets, finish any already-stalled offer, drain
        # outstanding traffic (the good lines may still land), and report
        # completion with the error status instead of hanging on addr_done.
        yield from send_cmd(engine.cmd,
                            id=11,
                            core=0,
                            src=0x8000,
                            nbytes=16 * LINE_BYTES,
                            dst=0)
        for _ in range(20):
            yield
        release[0] = True
        res = yield from recv_done(engine.done)
        assert res == (11, 1)
        assert N_SLOTS <= len(a_log) <= N_SLOTS + 1, \
            'only an already-stalled Get may issue after failure'
        # Good lines drained; the denied line never landed.
        for line in range(len(a_log)):
            if line == 2:
                assert smem[2 * LINE_BYTES:3 * LINE_BYTES] == \
                    bytearray(LINE_BYTES)
                continue
            got = bytes(smem[line * LINE_BYTES:(line + 1) * LINE_BYTES])
            assert got == model.peek(0x8000 + line * LINE_BYTES,
                                     LINE_BYTES), f'line {line}'
        assert (yield engine.error)
        done[0] = True

    _run(
        top, _proc(dma_sink, engine, 0, smem, commits, done),
        _proc(tl_copy_responder,
              engine.mem_bus,
              model,
              done,
              a_log=a_log,
              denied={0x8080},
              release=release), driver, _proc(_watchdog, done))


def test_engine_rejects_mmio_sources():
    params = _params()
    io_base = next(iter(params['io_regions']))
    top = CopyTop(params)
    engine = top.engine
    model = SparseRAM()
    smem = bytearray(params['smem_params']['size'])
    commits = []
    done = [False]
    a_log = []

    def driver():
        # Fully inside an MMIO region.
        yield from send_cmd(engine.cmd,
                            id=1,
                            core=0,
                            src=io_base + 0x10,
                            nbytes=0x40,
                            dst=0)
        assert (yield from recv_done(engine.done)) == (1, 1)
        # Straddling the region boundary is equally rejected.
        yield from send_cmd(engine.cmd,
                            id=2,
                            core=0,
                            src=io_base - 0x40,
                            nbytes=0x80,
                            dst=0)
        assert (yield from recv_done(engine.done)) == (2, 1)
        assert (yield engine.error)
        # Ending exactly at the boundary stays in normal memory.
        yield from send_cmd(engine.cmd,
                            id=3,
                            core=0,
                            src=io_base - 0x80,
                            nbytes=0x80,
                            dst=0x10)
        assert (yield from recv_done(engine.done)) == (3, 0)
        assert a_log == [(0, io_base - 0x80), (1, io_base - 0x40)], \
            'only the accepted command may fetch'
        assert bytes(smem[0x10:0x90]) == model.peek(io_base - 0x80, 0x80)
        done[0] = True

    _run(top, _proc(dma_sink, engine, 0, smem, commits, done),
         _proc(tl_copy_responder, engine.mem_bus, model, done, a_log=a_log),
         driver, _proc(_watchdog, done))


def test_engine_rejects_source_address_wraparound():
    params = _params()
    top = CopyTop(params)
    engine = top.engine
    model = SparseRAM()
    smem = bytearray(params['smem_params']['size'])
    commits = []
    done = [False]
    a_log = []

    def driver():
        yield from send_cmd(engine.cmd,
                            id=4,
                            core=0,
                            src=0xFFFFFF00,
                            nbytes=0x200,
                            dst=0)
        assert (yield from recv_done(engine.done)) == (4, 1)
        assert (yield engine.error)
        assert a_log == []
        assert commits == []
        done[0] = True

    _run(top, _proc(dma_sink, engine, 0, smem, commits, done),
         _proc(tl_copy_responder, engine.mem_bus, model, done, a_log=a_log),
         driver, _proc(_watchdog, done))


@pytest.mark.parametrize('dst,nbytes,core', [
    (0x3ff8, 16, 0),
    (0x4000, 8, 0),
    (0x7ff8, 16, 0),
    (0, 0xffff, 0),
    (0, 8, 3),
])
def test_engine_rejects_invalid_destination_before_traffic(dst, nbytes, core):
    top = CopyTop(_params(), n_cores=3)
    engine = top.engine
    done = [False]

    def driver():
        yield from send_cmd(engine.cmd,
                            id=7,
                            core=core,
                            src=0x2000,
                            nbytes=nbytes,
                            dst=dst)
        assert (yield from recv_done(engine.done)) == (7, 1)
        done[0] = True

    def guard():
        while not done[0]:
            assert not (yield engine.mem_bus.a.valid)
            for port in engine.dma_req:
                assert not (yield port.valid)
            yield

    _run(top, driver, guard, _proc(_watchdog, done, limit=100))


@pytest.mark.parametrize('regions,io_regions,src', [
    (None, {}, 0x2000),
    ([], {}, 0x2000),
    ([(0x2000, 64, 'w', True)], {}, 0x2000),
    ([(0x2000, 64, 'rw', False)], {}, 0x2000),
    ([(0x2000, 32, 'rw', True)], {}, 0x2000),
    ([(0x2008, 56, 'rw', True)], {}, 0x2008),
    ([(0x2000, 64, 'rw', True)], {
        0x2020: 16
    }, 0x2000),
    ([(0x2000, 64, 'rw', True)], {
        0x2000: 8
    }, 0x2010),
    ([(0x3000, 64, 'rw', True)], {}, 0x2000),
])
def test_engine_rejects_unsafe_full_line_overfetch(regions, io_regions, src):
    top = CopyTop(_params(pma_regions=regions, io_regions=io_regions))
    engine = top.engine
    done = [False]

    def driver():
        yield from send_cmd(engine.cmd, id=8, core=0, src=src, nbytes=8, dst=0)
        assert (yield from recv_done(engine.done)) == (8, 1)
        done[0] = True

    def guard():
        while not done[0]:
            assert not (yield engine.mem_bus.a.valid)
            assert not (yield engine.dma_req[0].valid)
            yield

    _run(top, driver, guard, _proc(_watchdog, done, limit=100))


def test_engine_copy_can_end_at_address_space_and_smem_boundaries():
    params = _params()
    top = CopyTop(params)
    engine = top.engine
    model = SparseRAM()
    smem = bytearray(params['smem_params']['size'])
    done = [False]
    commits = []

    def driver():
        yield from send_cmd(engine.cmd,
                            id=9,
                            core=0,
                            src=0xfffffff8,
                            nbytes=8,
                            dst=len(smem) - 8)
        assert (yield from recv_done(engine.done)) == (9, 0)
        assert smem[-8:] == model.peek(0xfffffff8, 8)
        assert commits == [(9, 8)]
        done[0] = True

    _run(top, driver, _proc(dma_sink, engine, 0, smem, commits, done),
         _proc(tl_copy_responder, engine.mem_bus, model, done),
         _proc(_watchdog, done, limit=1000))


def test_engine_stalled_fragment_survives_new_line_response():
    params = _params()
    top = CopyTop(params)
    engine = top.engine
    model = SparseRAM()
    smem = bytearray(params['smem_params']['size'])
    done = [False]
    commits = []
    offers = []

    def driver():
        yield from send_cmd(engine.cmd,
                            id=10,
                            core=0,
                            src=0x2000,
                            nbytes=128,
                            dst=0)
        assert (yield from recv_done(engine.done)) == (10, 0)
        assert smem[:128] == model.peek(0x2000, 128)
        assert offers and set(offers) == {0}
        done[0] = True

    def monitor():
        while not done[0]:
            port = engine.dma_req[0]
            if (yield port.valid) and not (yield port.ready):
                offers.append((yield port.bits.offset))
            yield

    _run(
        top, driver, monitor,
        _proc(dma_sink,
              engine,
              0,
              smem,
              commits,
              done,
              ready_fn=lambda cycle: cycle >= 100),
        _proc(tl_copy_responder, engine.mem_bus, model, done),
        _proc(_watchdog, done, limit=1000))


def test_engine_stalled_get_survives_failure_of_outstanding_read():
    params = _params()
    top = CopyTop(params)
    engine = top.engine
    bus = engine.mem_bus
    model = SparseRAM()
    smem = bytearray(params['smem_params']['size'])
    done = [False]
    commits = []

    def driver():
        yield bus.a.ready.eq(0)
        yield from send_cmd(engine.cmd,
                            id=11,
                            core=0,
                            src=0x2000,
                            nbytes=192,
                            dst=0)
        while not (yield bus.a.valid):
            yield
        first = (yield bus.a.bits.source)
        yield bus.a.ready.eq(1)
        yield
        yield bus.a.ready.eq(0)
        yield
        assert (yield bus.a.valid)
        held = (yield bus.a.bits.as_value())
        second = (yield bus.a.bits.source)
        assert (yield bus.a.bits.address) == 0x2040
        yield from _respond_line(bus, model, first, 0x2000, denied=True)
        for _ in range(8):
            assert (yield bus.a.valid)
            assert (yield bus.a.bits.as_value()) == held
            assert not (yield engine.done.valid)
            yield
        yield bus.a.ready.eq(1)
        yield
        yield bus.a.ready.eq(0)
        yield
        assert not (yield bus.a.valid), 'failed transfer issued another Get'
        yield from _respond_line(bus, model, second, 0x2040)
        assert (yield from recv_done(engine.done)) == (11, 1)
        assert smem[64:128] == model.peek(0x2040, 64)
        done[0] = True

    _run(top, driver, _proc(dma_sink, engine, 0, smem, commits, done),
         _proc(_watchdog, done, limit=1000))


# ---------------------------------------------------------------------------
# Flow control
# ---------------------------------------------------------------------------
def test_engine_destination_backpressure_and_slot_exhaustion():
    params = _params()
    top = CopyTop(params)
    engine = top.engine
    model = SparseRAM()
    smem = bytearray(params['smem_params']['size'])
    commits = []
    done = [False]
    a_log = []
    release = [False]

    def stalled_then_open(cycle):
        return 0 if cycle < 60 else 1

    def driver():
        yield from send_cmd(engine.cmd,
                            id=12,
                            core=0,
                            src=0x9000,
                            nbytes=512,
                            dst=0)
        # All eight read slots fill; no ninth Get may fire before one
        # drains, and the stalled destination must not lose data.
        for _ in range(24):
            yield
        assert len(a_log) == N_SLOTS
        n_before = len(a_log)
        for _ in range(30):
            yield
        assert len(a_log) == n_before, \
            'engine issued a ninth Get with every read slot occupied'
        release[0] = True
        res = yield from recv_done(engine.done)
        assert res == (12, 0)
        assert bytes(smem[:512]) == model.peek(0x9000, 512)
        assert sum(n for _, n in commits) == 512
        done[0] = True

    _run(
        top,
        _proc(dma_sink,
              engine,
              0,
              smem,
              commits,
              done,
              ready_fn=stalled_then_open),
        _proc(tl_copy_responder,
              engine.mem_bus,
              model,
              done,
              a_log=a_log,
              order='lifo',
              release=release), driver, _proc(_watchdog, done))


def test_engine_done_backpressure_gates_next_command():
    params = _params()
    top = CopyTop(params)
    engine = top.engine
    model = SparseRAM()
    smem = bytearray(params['smem_params']['size'])
    commits = []
    done = [False]
    a_log = []

    def driver():
        yield from send_cmd(engine.cmd,
                            id=5,
                            core=0,
                            src=0xA000,
                            nbytes=64,
                            dst=0)
        # Wait for completion but do not accept it.
        while not (yield engine.done.valid):
            yield
        assert ((yield
                 engine.done.bits.id), (yield
                                        engine.done.bits.error)) == (5, 0)
        yield engine.done.ready.eq(0)
        yield from send_cmd(engine.cmd,
                            id=6,
                            core=0,
                            src=0xA100,
                            nbytes=64,
                            dst=0x100)
        assert (yield engine.busy)
        n_before = len(a_log)
        for _ in range(20):
            yield
        assert len(a_log) == n_before, \
            'next transfer started before the prior done was consumed'
        # Consume the held completion; the second copy must now proceed.
        res_id, res_err = (yield engine.done.bits.id), (yield
                                                        engine.done.bits.error)
        assert (res_id, res_err) == (5, 0)
        yield engine.done.ready.eq(1)
        yield
        yield engine.done.ready.eq(0)
        while (yield engine.done.valid):
            yield
        assert (yield from recv_done(engine.done)) == (6, 0)
        assert bytes(smem[:64]) == model.peek(0xA000, 64)
        assert bytes(smem[0x100:0x140]) == model.peek(0xA100, 64)
        done[0] = True

    _run(top, _proc(dma_sink, engine, 0, smem, commits, done),
         _proc(tl_copy_responder, engine.mem_bus, model, done, a_log=a_log),
         driver, _proc(_watchdog, done))


def test_engine_command_queue_fills_and_drains():
    params = _params()
    top = CopyTop(params)
    engine = top.engine
    model = SparseRAM()
    smem = bytearray(params['smem_params']['size'])
    commits = []
    done = [False]
    release = [False]
    n_cmds = QUEUE_DEPTH + 1

    def driver():
        # One active transfer plus a full command queue: the next command
        # must backpressure at cmd.ready and nothing may be lost.
        for i in range(n_cmds):
            yield from send_cmd(engine.cmd,
                                id=1 + i,
                                core=0,
                                src=0x10000 + i * LINE_BYTES,
                                nbytes=LINE_BYTES,
                                dst=i * LINE_BYTES)
        # The active transfer occupies the engine; the queue holds the rest.
        yield engine.cmd.bits.id.eq(10)
        yield engine.cmd.bits.core.eq(0)
        yield engine.cmd.bits.src_addr.eq(0)
        yield engine.cmd.bits.nbytes.eq(LINE_BYTES)
        yield engine.cmd.bits.dst_offset.eq(0)
        yield engine.cmd.valid.eq(1)
        for _ in range(10):
            yield
            assert not (yield engine.cmd.fire), \
                'command accepted with a full queue'
        release[0] = True
        yield engine.cmd.valid.eq(0)
        yield

        for i in range(n_cmds):
            assert (yield from recv_done(engine.done)) == (1 + i, 0), \
                'commands must complete in issue order'
        for i in range(n_cmds):
            expect = model.peek(0x10000 + i * LINE_BYTES, LINE_BYTES)
            got = bytes(smem[i * LINE_BYTES:(i + 1) * LINE_BYTES])
            assert got == expect, f'command {i} data mismatch'
        assert not (yield engine.busy)
        done[0] = True

    _run(
        top, _proc(dma_sink, engine, 0, smem, commits, done),
        _proc(tl_copy_responder, engine.mem_bus, model, done, release=release),
        driver, _proc(_watchdog, done))


# ---------------------------------------------------------------------------
# Engine + coherent L2
# ---------------------------------------------------------------------------
from roomsoc.peripheral.l2cache import L2Cache
from tests.roomsoc.interconnect.tl_helpers import (TLRamModel, tl_acquire,
                                                   tl_c_responder, tl_grantack)

# Copy sources occupy 32..39: outside both coherent client ranges below,
# mirroring the wrapper's placement of copy IDs in the instruction-side
# source half. An L2 Get from such a source has no client bit and must
# therefore probe every dirty owner.
COPY_SOURCE_BASE = 32


def _l2_params(**overrides):
    params = dict(
        capacity_kb=1,
        n_ways=2,
        block_bytes=64,
        inner_beat_bytes=8,
        outer_beat_bytes=8,
        n_mshrs=4,
        in_bus=dict(source_id_width=8, sink_id_width=2, size_width=3),
        out_bus=dict(source_id_width=2, sink_id_width=1, size_width=3),
        client_source_map={
            0: (0, 15),
            1: (64, 79)
        },
    )
    params.update(overrides)
    return params


class EngineL2Top(Elaboratable):
    """``AsyncCopyEngine`` attached to a coherent L2 beside one client port.

    The engine's three-bit slot sources are widened into the L2 source
    space at ``COPY_SOURCE_BASE`` (outside every client range), exactly as
    ``Cluster`` does. Channel A is arbitrated and channel D demultiplexed
    by source: copy sources flow to the engine, everything else to
    ``client_bus``. B/C/E belong to the client side alone.
    """

    def __init__(self, params, l2_params, n_cores=1):
        self.engine = AsyncCopyEngine(n_cores,
                                      params,
                                      block_bytes=l2_params['block_bytes'])
        self.l2 = L2Cache(l2_params)
        self.out_bus = self.l2.out_bus
        self.client_bus = tl.Interface(data_width=64,
                                       addr_width=32,
                                       size_width=3,
                                       source_id_width=8,
                                       sink_id_width=2,
                                       has_bce=True,
                                       name='client_bus')

    def elaborate(self, platform):
        m = Module()
        m.submodules.engine = self.engine
        m.submodules.l2 = self.l2

        a_arbiter = m.submodules.a_arbiter = tl.Arbiter(tl.ChannelA,
                                                        data_width=64,
                                                        addr_width=32,
                                                        size_width=3,
                                                        source_id_width=8)
        copy_a = Decoupled(tl.ChannelA,
                           data_width=64,
                           addr_width=32,
                           size_width=3,
                           source_id_width=8)
        m.d.comb += [
            self.engine.mem_bus.a.connect(copy_a),
            copy_a.bits.source.eq(self.engine.mem_bus.a.bits.source +
                                  COPY_SOURCE_BASE),
        ]
        a_arbiter.add(copy_a)
        a_arbiter.add(self.client_bus.a)
        m.d.comb += a_arbiter.bus.connect(self.l2.in_bus.a)

        d = self.l2.in_bus.d
        d_is_copy = ((d.bits.source >= COPY_SOURCE_BASE)
                     & (d.bits.source < COPY_SOURCE_BASE + N_SLOTS))
        m.d.comb += [
            self.engine.mem_bus.d.valid.eq(d.valid & d_is_copy),
            self.engine.mem_bus.d.bits.opcode.eq(d.bits.opcode),
            self.engine.mem_bus.d.bits.param.eq(d.bits.param),
            self.engine.mem_bus.d.bits.size.eq(d.bits.size),
            self.engine.mem_bus.d.bits.source.eq(d.bits.source[:3]),
            self.engine.mem_bus.d.bits.sink.eq(0),
            self.engine.mem_bus.d.bits.denied.eq(d.bits.denied),
            self.engine.mem_bus.d.bits.corrupt.eq(d.bits.corrupt),
            self.engine.mem_bus.d.bits.data.eq(d.bits.data),
            d.ready.eq(
                Mux(d_is_copy, self.engine.mem_bus.d.ready,
                    self.client_bus.d.ready)),
            self.client_bus.d.valid.eq(d.valid & ~d_is_copy),
            self.client_bus.d.bits.eq(d.bits),
            self.l2.in_bus.b.connect(self.client_bus.b),
            self.client_bus.c.connect(self.l2.in_bus.c),
            self.client_bus.e.connect(self.l2.in_bus.e),
        ]
        return m


def _mpeek(model, addr, nbytes):
    return bytes(model.mem[addr:addr + nbytes]) if hasattr(
        model, 'mem') else model.peek(addr, nbytes)


def _outer_model(depth=4096):
    return TLRamModel(data_width=64,
                      depth=depth,
                      init=[0xC000_0000_0000 + i for i in range(depth)])


def _monitor_outer_fetches(top, done, fetches):
    yield
    while not done[0]:
        if (yield top.out_bus.a.fire):
            fetches.append((yield top.out_bus.a.bits.address))
        yield


def _acquire(client_bus, address, *, source, grow_param):
    res = yield from tl_acquire(client_bus,
                                address,
                                size=6,
                                source=source,
                                grow_param=grow_param)
    _op, _param, _src, d_sink, _data, _denied, _corrupt = res
    yield from tl_grantack(client_bus, sink=d_sink)
    return res


def _client_probe_responder(client_bus, clients, probes, done):
    """Serve L2 probes for modeled coherent clients.

    ``clients`` is keyed by client source and holds ``cap``, ``data``
    (64-byte ``bytes``) and ``dirty``. Dirty owners answer ProbeAckData
    with their bytes; clean owners answer ProbeAck.
    """
    beat_bytes = client_bus.data_width // 8
    yield client_bus.b.ready.eq(1)
    yield client_bus.c.valid.eq(0)
    yield
    while not done[0]:
        if not (yield client_bus.b.valid):
            yield
            continue

        opcode = (yield client_bus.b.bits.opcode)
        target = (yield client_bus.b.bits.param)
        size = (yield client_bus.b.bits.size)
        source = (yield client_bus.b.bits.source)
        address = (yield client_bus.b.bits.address)
        assert opcode == tl.ChannelBOpcode.Probe.value
        assert source in clients, f'probe targeted unknown client {source}'
        state = clients[source]
        if state['cap'] == tl.CapParam.toT:
            report = (tl.ShrinkReportParam.TtoN if target
                      == tl.CapParam.toN.value else tl.ShrinkReportParam.TtoB)
        else:
            report = (tl.ShrinkReportParam.BtoN if target
                      == tl.CapParam.toN.value else tl.ShrinkReportParam.BtoB)
        has_data = state['dirty']
        probes.append((source, address, target))
        yield  # accept the probe
        yield client_bus.b.ready.eq(0)

        data = int.from_bytes(state['data'], 'little')
        yield client_bus.c.bits.opcode.eq(
            tl.ChannelCOpcode.ProbeAckData if has_data else tl.ChannelCOpcode.
            ProbeAck)
        yield client_bus.c.bits.param.eq(report)
        yield client_bus.c.bits.size.eq(size)
        yield client_bus.c.bits.source.eq(source)
        yield client_bus.c.bits.address.eq(address)
        yield client_bus.c.bits.corrupt.eq(0)
        yield client_bus.c.valid.eq(1)

        beats = max(1, (1 << size) // beat_bytes) if has_data else 1
        for beat in range(beats):
            yield client_bus.c.bits.data.eq(
                (data >> (beat * client_bus.data_width))
                & ((1 << client_bus.data_width) - 1))
            yield
            while not (yield client_bus.c.ready):
                yield

        yield client_bus.c.valid.eq(0)
        if target == tl.CapParam.toN.value:
            clients.pop(source)
        else:
            state['cap'] = tl.CapParam.toB
            state['dirty'] = False
        yield client_bus.b.ready.eq(1)
        yield
    yield client_bus.c.valid.eq(0)


def test_engine_l2_miss_copies_outer_data():
    params = _params()
    top = EngineL2Top(params, _l2_params())
    engine = top.engine
    model = _outer_model()
    smem = bytearray(params['smem_params']['size'])
    commits = []
    done = [False]
    fetches = []

    def driver():
        yield from send_cmd(engine.cmd,
                            id=1,
                            core=0,
                            src=0x800,
                            nbytes=128,
                            dst=0)
        assert (yield from recv_done(engine.done)) == (1, 0)
        assert bytes(smem[:128]) == _mpeek(model, 0x800, 128)
        assert sorted(fetches) == [0x800, 0x840], \
            'both lines must be fetched from outer memory'
        done[0] = True

    _run(top, _proc(dma_sink, engine, 0, smem, commits, done),
         _proc(tl_c_responder, top.out_bus, model=model, done=done),
         _proc(_monitor_outer_fetches, top, done, fetches), driver,
         _proc(_watchdog, done))


def test_engine_l2_clean_hit_avoids_refetch():
    params = _params()
    top = EngineL2Top(params, _l2_params())
    engine = top.engine
    model = _outer_model()
    smem = bytearray(params['smem_params']['size'])
    commits = []
    done = [False]
    fetches = []

    def driver():
        for dst in (0, 0x100):
            yield from send_cmd(engine.cmd,
                                id=2,
                                core=0,
                                src=0x800,
                                nbytes=64,
                                dst=dst)
            assert (yield from recv_done(engine.done)) == (2, 0)
        assert bytes(smem[:64]) == _mpeek(model, 0x800, 64)
        assert bytes(smem[0x100:0x140]) == _mpeek(model, 0x800, 64)
        assert fetches == [0x800], \
            'the second copy of a resident line must hit in the L2'
        done[0] = True

    _run(top, _proc(dma_sink, engine, 0, smem, commits, done),
         _proc(tl_c_responder, top.out_bus, model=model, done=done),
         _proc(_monitor_outer_fetches, top, done, fetches), driver,
         _proc(_watchdog, done))


@pytest.mark.parametrize('client_source', [0, 64])
def test_engine_l2_dirty_owner_surrenders_data_on_probe(client_source):
    """The phase 2 coherence gate: a non-client Get must observe dirty data.

    A coherent client (either cluster's DCache class) acquires the line
    trunk and then writes it privately, so the only up-to-date copy is in
    the client. The copy engine's Get uses a source outside every client
    range, which must force the L2 to probe that client and forward the
    dirty bytes to the engine.
    """
    params = _params()
    top = EngineL2Top(params, _l2_params())
    engine = top.engine
    model = _outer_model()
    smem = bytearray(params['smem_params']['size'])
    commits = []
    done = [False]
    fetches = []
    probes = []
    clients = {}
    dirty = bytes((0xB0 + i) & 0xff for i in range(64))

    def driver():
        res = yield from _acquire(top.client_bus,
                                  0x800,
                                  source=client_source,
                                  grow_param=tl.GrowParam.NtoT)
        assert res[1] == tl.CapParam.toT.value
        # The client's private store is invisible to the L2 and the outer
        # memory: only the probe can carry these bytes back.
        clients[client_source] = dict(cap=tl.CapParam.toT,
                                      data=dirty,
                                      dirty=True)
        n_fetches = len(fetches)

        yield from send_cmd(engine.cmd,
                            id=3,
                            core=0,
                            src=0x800,
                            nbytes=64,
                            dst=0)
        assert (yield from recv_done(engine.done)) == (3, 0)
        assert bytes(smem[:64]) == dirty, \
            'engine must receive the client-dirty data, not outer data'
        assert bytes(smem[:64]) != _mpeek(model, 0x800, 64)
        assert probes and probes[0][:2] == (client_source, 0x800), \
            'L2 must probe the dirty owner for a non-client Get'
        assert len(fetches) == n_fetches, \
            'dirty data must come from the owner, not an outer fetch'
        done[0] = True

    _run(top, _proc(dma_sink, engine, 0, smem, commits, done),
         _proc(tl_c_responder, top.out_bus, model=model, done=done),
         _proc(_monitor_outer_fetches, top, done, fetches),
         _proc(_client_probe_responder, top.client_bus, clients, probes, done),
         driver, _proc(_watchdog, done))


class EngineDCacheL2Top(EngineL2Top):
    """Real dirty owner and real shared endpoint around the engine/L2 path."""

    def __init__(self, params, client_source):
        super().__init__(params, _l2_params())
        self.client_source = client_source
        self.dcache_bus = tl.Interface(data_width=64,
                                       addr_width=32,
                                       size_width=3,
                                       source_id_width=8,
                                       sink_id_width=2,
                                       has_bce=True)
        self.mmio_bus = tl.Interface(data_width=64,
                                     addr_width=32,
                                     size_width=3,
                                     source_id_width=8)
        self.dcache = DCache(self.dcache_bus, self.mmio_bus, params)
        self.smem = SharedMemory(params)
        self.dma_enable = Signal(reset=1)

    def elaborate(self, platform):
        m = super().elaborate(platform)
        m.submodules.dcache = self.dcache
        m.submodules.smem = self.smem
        local = self.dcache_bus
        client = self.client_bus
        m.d.comb += [
            local.a.connect(client.a),
            client.a.bits.source.eq(local.a.bits.source + self.client_source),
            local.c.connect(client.c),
            client.c.bits.source.eq(local.c.bits.source + self.client_source),
            local.e.connect(client.e),
            client.b.connect(local.b),
            local.b.bits.source.eq(client.b.bits.source - self.client_source),
            client.d.connect(local.d),
            local.d.bits.source.eq(client.d.bits.source - self.client_source),
            self.mmio_bus.a.ready.eq(1),
            self.engine.dma_req[0].connect(self.smem.dma_req),
            self.smem.dma_req.valid.eq(self.engine.dma_req[0].valid
                                       & self.dma_enable),
            self.engine.dma_req[0].ready.eq(self.smem.dma_req.ready
                                            & self.dma_enable),
            self.smem.dma_commit.connect(self.engine.dma_commit[0]),
        ]
        return m


@pytest.mark.parametrize('client_source', [0, 64])
def test_engine_real_dcache_dirty_surrender_to_shared_memory(client_source):
    from tests.room.test_dcache import load_process, store_process

    params = _params()
    params['dcache_params'].update(n_sets=16,
                                   n_ways=2,
                                   n_mshrs=2,
                                   n_iomshrs=2,
                                   n_banks=2)
    top = EngineDCacheL2Top(params, client_source)
    engine = top.engine
    done = [False]
    responses, nacks, probes, probe_data, fetches = [], [], [], [], []
    model = _outer_model()
    dirty = bytes((0xb0 + i) & 0xff for i in range(64))

    def monitor():
        while not done[0]:
            for lane in range(top.dcache.mem_width):
                if (yield top.dcache.resp[lane].valid):
                    responses.append(
                        (lane, (yield top.dcache.resp[lane].bits.uop.uop_id),
                         (yield top.dcache.resp[lane].bits.data)))
                if (yield top.dcache.nack[lane].valid):
                    nacks.append(
                        (lane, (yield top.dcache.nack[lane].bits.uop.uop_id)))
            if (yield top.client_bus.b.fire):
                probes.append(((yield top.client_bus.b.bits.source),
                               (yield top.client_bus.b.bits.address)))
            if ((yield top.client_bus.c.fire)
                    and (yield top.client_bus.c.bits.opcode)
                    == tl.ChannelCOpcode.ProbeAckData.value):
                probe_data.append((yield top.client_bus.c.bits.data))
            yield

    def driver():
        yield from load_process(top, 0, 0x800, 0x20, responses, nacks)
        yield top.dcache.req[0].bits.uop.uses_ldq.eq(0)
        for word in range(16):
            value = int.from_bytes(dirty[word * 4:word * 4 + 4], 'little')
            yield from store_process(top, 0, 0x800 + word * 4, value,
                                     0x30 + word, responses, nacks)
        # Wait for the store pipeline to commit before the copy is launched.
        for _ in range(8):
            yield
        before = len(fetches)
        yield top.dma_enable.eq(0)
        yield from send_cmd(engine.cmd,
                            id=3,
                            core=0,
                            src=0x800,
                            nbytes=64,
                            dst=0)
        for _ in range(1000):
            if len(probe_data) == 8 and (yield engine.dma_req[0].valid):
                break
            yield
        else:
            pytest.fail(
                f'real DCache failed to surrender dirty data: '
                f'probes={probes}, data={probe_data}, '
                f'fetches={fetches}, done={(yield engine.done.valid)}, '
                f'error={(yield engine.error)}, '
                f'dma={(yield engine.dma_req[0].valid)}')
        assert probes == [(client_source, 0x800)]
        assert b''.join(word.to_bytes(8, 'little')
                        for word in probe_data) == dirty
        assert not (yield engine.done.valid)
        assert (yield engine.busy)
        yield top.dma_enable.eq(1)
        assert (yield from recv_done(engine.done)) == (3, 0)
        assert len(fetches) == before
        assert _mpeek(model, 0x800, 64) != dirty

        # Completion must already make every byte visible through lane loads.
        for word in range(16):
            req = top.smem.req[0]
            yield req.valid.eq(1)
            yield req.bits.uop.mem_cmd.eq(MemoryCommand.READ)
            yield req.bits.uop.mem_size.eq(2)
            yield req.bits.addr.eq(params['smem_params']['base'] + word * 4)
            yield
            yield req.valid.eq(0)
            observed = []
            for _ in range(5):
                if (yield top.smem.resp[0].valid):
                    observed.append((yield top.smem.resp[0].bits.data))
                yield
            assert observed == [
                int.from_bytes(dirty[word * 4:word * 4 + 4], 'little')
            ]
        done[0] = True

    _run(top, driver, monitor,
         _proc(tl_c_responder, top.out_bus, model=model, done=done),
         _proc(_monitor_outer_fetches, top, done, fetches),
         _proc(_watchdog, done, limit=5000))


# ---------------------------------------------------------------------------
# Cluster wrapper integration
# ---------------------------------------------------------------------------
from groom.wrapper import Cluster


def _cluster_params():
    """Reduced-geometry cluster mirroring ``groom_main``'s parameter shape.

    The raster is absent and the FPU is off to keep the netlist small; the
    cores and DCache are held in reset by the test, so the copy engine is
    the only cluster bus master.
    """
    core_params = dict(
        is_groom=True,
        xlen=32,
        vaddr_bits=32,
        n_warps=4,
        n_threads=4,
        n_barriers=4,
        issue_params=dict(queue_depth=4),
        icache_params=dict(n_sets=16, n_ways=2, block_bytes=64),
        dcache_params=dict(n_sets=16,
                           n_ways=2,
                           block_bytes=64,
                           row_bits=64,
                           n_mshrs=2,
                           n_iomshrs=2,
                           sdq_size=8,
                           rpq_size=8,
                           n_banks=2),
        use_fpu=False,
        flen=32,
        fma_latency=2,
        smem_params=dict(base=0x20000, size=0x4000, n_banks=4),
        use_raster=False,
        io_regions={0x40000000: 0x40000000},
        use_async_copy=True,
    )
    core_params['n_cores'] = 2
    return dict(
        n_cores_per_cluster=2,
        n_clusters=1,
        core_params=core_params,
        l2cache_params=dict(
            capacity_kb=1,
            n_ways=2,
            block_bytes=64,
            inner_beat_bytes=8,
            outer_beat_bytes=8,
            n_mshrs=4,
            in_bus=dict(source_id_width=8, sink_id_width=4, size_width=3),
            out_bus=dict(source_id_width=2, sink_id_width=1),
        ),
        io_regions={0x40000000: 0x40000000},
        raster_params=None,
        pma_regions=[(0, 0x40000000, 'rw', True)],
    )


class ClusterTop(Elaboratable):

    def __init__(self, params):
        self.cluster = Cluster(0, params)

    def elaborate(self, platform):
        m = Module()
        m.submodules.cluster = self.cluster
        return m


def test_cluster_copy_d_response_demux():
    """Copy-class D responses route to the engine; other classes do not.

    The debug command ingress is gone and the per-core launch ports
    belong to the cores (which cannot run under pysim), so cluster-level
    request traffic is exercised by the bare-core end-to-end test in
    ``test_gcopy.py`` instead. This test drives the cluster's dbus D
    channel the way the L2 would and checks the response half of the
    copy wiring: a source carrying the copy marker connects through to
    the engine's read slot, while dbus-class and instruction-class
    sources never reach it. In this geometry, source 0x50 is copy slot 0
    aimed at core 1 (marker bit set above the slot id), 0x51 is slot 1,
    0x01 is the DCache's dbus class, and 0x40 is core 1's
    instruction-cache class.
    """
    params = _cluster_params()
    top = ClusterTop(params)
    cluster = top.cluster
    engine_d = cluster.copy_engine.mem_bus.d

    def driver():
        yield cluster.core_enable.eq(0)
        yield cluster.cache_enable.eq(0)
        yield cluster.raster_enable.eq(0)
        yield

        def offer(source, slot, expect_engine):
            yield cluster.dbus.d.bits.opcode.eq(
                tl.ChannelDOpcode.AccessAckData)
            yield cluster.dbus.d.bits.source.eq(source)
            yield cluster.dbus.d.valid.eq(1)
            yield
            assert (yield engine_d.valid) == expect_engine, \
                f'source {source:#x} misrouted by the copy demultiplexer'
            if expect_engine:
                assert (yield engine_d.bits.source) == slot
                assert not (yield engine_d.fire), \
                    'the engine only accepts into a receiving slot'
            yield cluster.dbus.d.valid.eq(0)
            yield

        offer(0x50, 0, True)  # copy class: slot 0, core 1
        offer(0x51, 1, True)  # copy class: slot 1
        offer(0x01, 0, False)  # dbus class: the DCache's response
        offer(0x40, 0, False)  # instruction class, core 1: no marker

    _run(top, driver)


class LaunchPortCrossingTop(Elaboratable):
    """The wrapper's per-core launch wiring reproduced under pysim.

    ``Cluster`` connects each core's ``copy_launch`` port — whose command
    core field ``groom/core.py`` sizes for system-wide core IDs — to the
    completion unit's ``launch[i]``, which ``groom/wrapper.py`` sizes for
    cluster-local IDs. The cores cannot run under pysim, so this top
    performs that exact connection under the production multi-cluster
    geometry and the testbench drives the core side.
    """

    def __init__(self, params, n_clusters, n_cores_per_cluster):
        core_bits = max(
            1,
            Shape.cast(range(n_clusters * n_cores_per_cluster)).width)
        self.core_launch = [
            Decoupled(AsyncCopyLaunch,
                      params,
                      core_id_width=core_bits,
                      name=f'tb_core_launch{i}')
            for i in range(n_cores_per_cluster)
        ]
        cluster_bits = max(1, Shape.cast(range(n_cores_per_cluster)).width)
        self.completion = AsyncCopyCompletion(n_cores_per_cluster,
                                              params,
                                              core_id_width=cluster_bits)

    def elaborate(self, platform):
        m = Module()
        completion = m.submodules.completion = self.completion
        m.d.comb += completion.done_out.ready.eq(1)
        for i, port in enumerate(self.core_launch):
            m.d.comb += port.connect(completion.launch[i])
        return m


def test_cluster_launch_port_core_id_width_crossing():
    """A launch crosses the core/cluster core-ID width boundary intact.

    With two clusters of two cores the core-side launch port carries a
    two-bit system-wide core field while the cluster-side completion port
    carries one bit. The packed ``AsyncCopyCmd`` assignment such a
    connection used to perform shifted every field after the core ID by
    the width difference, so an instruction-driven ``src=0x80000000,
    nbytes=256`` launch reached the engine as a different address with an
    odd length; only instruction-driven Verilator runs through the real
    ``Cluster`` exposed it, because every pysim harness either drove the
    engine/completion units directly or used single-cluster widths. The
    completion unit rewrites the core field from the port index, so the
    surviving fields are the observable contract: every one of them must
    arrive exactly as launched.
    """
    params = _params()
    top = LaunchPortCrossingTop(params, n_clusters=2, n_cores_per_cluster=2)
    completion = top.completion

    # The geometry really is the hazardous one: the two port flavors of
    # the same command carry different core-field widths.
    assert len(top.core_launch[0].bits.cmd.core) == 2
    assert len(completion.launch[0].bits.cmd.core) == 1

    sent = dict(id=5,
                src_addr=0x80000001,
                nbytes=0x8001,
                dst_offset=0x40000002,
                mode=1,
                src_base=0x7FFFFFF0,
                row_count=0x1234,
                row_bytes=0x5678,
                g_stride=0x9ABC,
                s_stride=0xDEF0,
                bcast=1)

    def driver():
        launch = top.core_launch[0]
        cmd = launch.bits.cmd
        yield cmd.id.eq(sent['id'])
        yield cmd.core.eq(0b10)  # system-wide pattern; unused downstream
        for field in ('src_addr', 'nbytes', 'dst_offset', 'mode', 'src_base',
                      'row_count', 'row_bytes', 'g_stride', 's_stride',
                      'bcast'):
            yield getattr(cmd, field).eq(sent[field])
        yield launch.bits.gen.eq(0)
        yield launch.bits.wid.eq(2)
        yield completion.cmd.ready.eq(1)
        yield launch.valid.eq(1)
        yield

        for _ in range(8):
            if (yield completion.cmd.valid):
                break
            assert not (yield completion.ack_valid[0]), \
                'launch was rejected: the command identity corrupted ' \
                'while crossing the width mismatch'
            yield
        else:
            raise AssertionError('launch was never serviced')

        assert (yield launch.ready)
        assert (yield completion.ack_valid[0])
        assert (yield completion.ack_wid[0]) == 2
        assert not (yield completion.ack_reject[0])

        got = {'core': (yield completion.cmd.bits.core)}
        for field in sent:
            got[field] = (yield getattr(completion.cmd.bits, field))
        assert got['core'] == 0, \
            'the completion unit must rewrite core as the port index'
        for field, value in sent.items():
            assert got[field] == value, \
                f'{field} crossed the port as {got[field]:#x}, ' \
                f'expected {value:#x}'

        yield  # fire the command; the absent engine keeps cmd.ready high
        yield launch.valid.eq(0)
        yield
        assert (yield completion.busy), \
            'the accepted launch must hold its token in flight'

    _run(top, driver)
