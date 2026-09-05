"""Functional tests for the coherent L2 cache (``roomsoc.peripheral.l2cache``).

The L2 is a non-blocking TileLink-C cache sitting between coherent clients
(``in_bus``, has_bce=True) and a coherent outer memory (``out_bus``,
has_bce=True). On the inner side it accepts Acquire/Get/Put/Release from L1
clients and answers with Grant/GrantData/AccessAck/ReleaseAck; it may Probe
clients on channel B. On the outer side it is itself a coherent master that
issues AcquireBlock to fetch lines and ReleaseData to write back victims.

These tests reuse the TileLink testbench kit in ``tl_helpers``: the master
drivers ``tl_acquire``, ``tl_get``, ``tl_put``, ``tl_release``, ``tl_grantack``
drive the client-facing ``in_bus``, and the coherent subordinate
``tl_c_responder`` (backed by a ``TLRamModel``) serves the outer ``out_bus`` --
reflecting the requested capability in Grant/GrantData, accepting
Release/ReleaseData and GrantAck, and never probing. ``run_sim`` runs the lot.

All processes honour the amaranth ``pysim`` clock model documented in
AGENTS.md: only a naked ``yield`` advances the clock, and signal reads/writes
between naked yields are coherent within one cycle.
"""

from amaranth import *
from amaranth.utils import log2_int

from roomsoc.interconnect import tilelink
from roomsoc.peripheral.l2cache import L2Cache

from tests.roomsoc.interconnect.tl_helpers import (
    TLRamModel, run_sim, tl_acquire, tl_c_responder, tl_get, tl_grantack,
    tl_put, tl_release)


# ---------------------------------------------------------------------------
# Cache configuration / DUT wrapper
# ---------------------------------------------------------------------------
def _l2_params(**overrides):
    """Small, fast-to-simulate default L2 geometry (1 KB, 2 ways, 8 B lines).

    ``client_source_map`` maps one client (index 0) onto all inner source IDs.
    Per-test overrides (e.g. ``block_bytes``, ``client_source_map``) replace
    top-level keys; ``in_bus``/``out_bus`` overrides replace those sub-dicts.
    """
    params = dict(
        capacity_kb=1,
        n_ways=2,
        block_bytes=8,
        inner_beat_bytes=8,
        outer_beat_bytes=8,
        n_mshrs=4,
        in_bus=dict(source_id_width=2, sink_id_width=2, size_width=4),
        out_bus=dict(source_id_width=2, sink_id_width=2),
        client_source_map={0: (0, 3)},
    )
    params.update(overrides)
    return params


def _six_mshr_params(**overrides):
    """Deployed l2m6 shape: four ordinary plus reserved BC/C MSHRs."""
    params = _l2_params(
        n_mshrs=6,
        out_bus=dict(source_id_width=3, sink_id_width=2),
    )
    params.update(overrides)
    return params


class L2Top(Elaboratable):
    """Bare L2Cache wrapper exposing ``in_bus`` and ``out_bus`` for driving."""

    def __init__(self, params):
        self.l2 = L2Cache(params)
        self.in_bus = self.l2.in_bus
        self.out_bus = self.l2.out_bus
        self.block_bytes = self.l2.block_bytes

    def elaborate(self, platform):
        m = Module()
        m.submodules.l2 = self.l2
        return m


# ---------------------------------------------------------------------------
# Backing model (TLRamModel) + int read helper for assertions
# ---------------------------------------------------------------------------
def _model(top, depth=1024):
    """Outer-memory golden model: word ``i`` holds ``0xC000 + i`` (distinct)."""
    return TLRamModel(data_width=top.out_bus.data_width,
                      depth=depth,
                      init=[0xC000 + i for i in range(depth)])


def _rd(model, addr, nbytes):
    """Little-endian ``nbytes`` at ``addr`` from a TLRamModel, as an int."""
    return int.from_bytes(model.get(addr, log2_int(nbytes)), "little")


# ---------------------------------------------------------------------------
# Client-side / orchestration helpers
# ---------------------------------------------------------------------------
def _acquire(bus, address, *, size, source, **kw):
    """``tl_acquire`` followed by the mandatory GrantAck, returning the grant
    tuple. The L2 frees an Acquire MSHR only once the GrantAck arrives."""
    res = yield from tl_acquire(bus, address, size=size, source=source, **kw)
    _op, _param, _src, d_sink, _data, _denied, _corrupt = res
    yield from tl_grantack(bus, sink=d_sink)
    return res


def _proc(genfunc, *args, **kwargs):
    """Bind args to a generator function, returning a no-arg generator function.

    ``amaranth.sim.Simulator.add_sync_process`` requires a no-arg generator
    function (it does ``yield from process()``); this wraps an argument-taking
    generator function so it can be passed to ``run_sim``.
    """

    def wrapper():
        yield from genfunc(*args, **kwargs)

    return wrapper


def _watchdog(done, limit=300000):
    """Fail loudly if the DUT deadlocks instead of completing the driver."""
    for _ in range(limit):
        if done[0]:
            return
        yield
    raise AssertionError(
        "L2 simulation deadlocked: driver did not signal completion within "
        f"{limit} cycles")


def _common(top, model, done):
    """Standard background processes: coherent outer memory + watchdog."""
    return [
        _proc(tl_c_responder, top.out_bus, model=model, done=done),
        _proc(_watchdog, done),
    ]


def _monitor_a_fires(top, done, addrs):
    """Record the address of every beat fired on ``out_bus.a`` (outer fetch)."""
    yield
    while not done[0]:
        if (yield top.out_bus.a.fire):
            addrs.append((yield top.out_bus.a.bits.address))
        yield


def _send_get_request(bus, address, *, size, source):
    """Send only the A beat of a Get, without waiting for its D response.

    The regular :func:`tl_get` intentionally serializes request and response.
    Splitting those phases lets tests fill several L2 MSHRs at once while
    retaining a single, unambiguous driver for each TileLink channel.
    """
    beat_bytes = bus.data_width // 8
    yield bus.a.bits.opcode.eq(tilelink.ChannelAOpcode.Get)
    yield bus.a.bits.param.eq(0)
    yield bus.a.bits.size.eq(size)
    yield bus.a.bits.source.eq(source)
    yield bus.a.bits.address.eq(address)
    yield bus.a.bits.mask.eq((1 << beat_bytes) - 1)
    yield bus.a.bits.data.eq(0)
    yield bus.a.bits.corrupt.eq(0)
    yield bus.a.valid.eq(1)
    yield
    while not (yield bus.a.ready):
        yield
    yield bus.a.valid.eq(0)


def _collect_get_responses(bus, expected, *, ready_fn=lambda cycle: 1,
                           timeout=10000):
    """Collect source-tagged Get responses, possibly under D backpressure."""
    remaining = dict(expected)
    stalled = None
    yield bus.d.ready.eq(ready_fn(0))
    yield

    for cycle in range(timeout):
        ready = (yield bus.d.ready)
        valid = (yield bus.d.valid)
        beat = ((yield bus.d.bits.opcode), (yield bus.d.bits.source),
                (yield bus.d.bits.data), (yield bus.d.bits.denied),
                (yield bus.d.bits.corrupt))

        if stalled is not None:
            assert valid, "D valid dropped while response was backpressured"
            assert beat == stalled, "D response changed while ready was low"

        if valid and ready:
            opcode, source, data, denied, corrupt = beat
            assert opcode == tilelink.ChannelDOpcode.AccessAckData.value
            assert source in remaining, f"unexpected/duplicate source {source}"
            assert (denied, corrupt) == (0, 0)
            assert data == remaining.pop(source)
            if not remaining:
                yield
                yield bus.d.ready.eq(0)
                return

        stalled = beat if valid and not ready else None
        yield bus.d.ready.eq(ready_fn(cycle + 1))
        yield

    raise AssertionError(f"timed out waiting for sources {sorted(remaining)}")


def _probe_responder(bus, clients, probes, done, *, beat_gap=0,
                     hold_source=None, respond_now=None, responded=None):
    """Respond to inner B-channel probes using per-client cache state.

    ``clients`` is keyed by the first source ID assigned to each client. Each
    present entry contains ``cap`` (``CapParam.toB`` or ``toT``), ``data``, and
    ``dirty``. Dirty clients return ProbeAckData; clean clients return
    ProbeAck. Every observed probe is appended to ``probes`` for assertions.
    ``beat_gap`` inserts invalid C cycles between data beats, which is legal
    TileLink traffic and useful for exercising BankedStore reservations.

    When ``hold_source`` is set, the probe addressed to that client is
    accepted but its response is deferred until ``respond_now[0]`` becomes
    true; the agent then finishes the response and sets ``responded[0]``.
    This lets a test land a ProbeAckData at an exact cycle (the SinkC
    way-CAM race regression).
    """
    beat_bytes = bus.data_width // 8
    yield bus.b.ready.eq(1)
    yield bus.c.valid.eq(0)
    yield

    while not done[0]:
        if not (yield bus.b.valid):
            yield
            continue

        opcode = (yield bus.b.bits.opcode)
        target = (yield bus.b.bits.param)
        size = (yield bus.b.bits.size)
        source = (yield bus.b.bits.source)
        address = (yield bus.b.bits.address)
        assert opcode == tilelink.ChannelBOpcode.Probe.value
        assert source in clients, f"probe targeted unknown client {source}"
        state = clients[source]
        old_cap = state["cap"]
        assert old_cap in (tilelink.CapParam.toB, tilelink.CapParam.toT)

        if old_cap == tilelink.CapParam.toT:
            report = (tilelink.ShrinkReportParam.TtoN
                      if target == tilelink.CapParam.toN.value else
                      tilelink.ShrinkReportParam.TtoB)
        else:
            report = (tilelink.ShrinkReportParam.BtoN
                      if target == tilelink.CapParam.toN.value else
                      tilelink.ShrinkReportParam.BtoB)

        has_data = state["dirty"]
        probes.append((source, address, target, has_data))
        yield  # accept B
        yield bus.b.ready.eq(0)

        if source == hold_source:
            while not respond_now[0]:
                yield

        yield bus.c.bits.opcode.eq(
            tilelink.ChannelCOpcode.ProbeAckData
            if has_data else tilelink.ChannelCOpcode.ProbeAck)
        yield bus.c.bits.param.eq(report)
        yield bus.c.bits.size.eq(size)
        yield bus.c.bits.source.eq(source)
        yield bus.c.bits.address.eq(address)
        yield bus.c.bits.corrupt.eq(0)
        yield bus.c.valid.eq(1)

        beats = max(1, (1 << size) // beat_bytes) if has_data else 1
        for beat in range(beats):
            yield bus.c.bits.data.eq(
                (state["data"] >> (beat * bus.data_width))
                & ((1 << bus.data_width) - 1))
            yield
            while not (yield bus.c.ready):
                yield
            if beat != beats - 1 and beat_gap:
                yield bus.c.valid.eq(0)
                for _ in range(beat_gap):
                    yield
                yield bus.c.valid.eq(1)

        yield bus.c.valid.eq(0)
        if target == tilelink.CapParam.toN.value:
            clients.pop(source)
        else:
            state["cap"] = tilelink.CapParam.toB
            state["dirty"] = False
        if source == hold_source:
            responded[0] = True
        yield bus.b.ready.eq(1)

    yield bus.b.ready.eq(0)
    yield bus.c.valid.eq(0)


def _send_release_data(bus, address, *, size, source, data,
                       param=tilelink.ShrinkReportParam.TtoN):
    """Drive a voluntary full-line ReleaseData on C, without collecting
    its ReleaseAck.

    Unlike :func:`tl_release` this does not touch the D channel, so it can
    be used while inner D is backpressured and the ReleaseAck is drained
    later with other stalled responses.
    """
    beats = (1 << size) // (bus.data_width // 8)
    yield bus.c.bits.opcode.eq(tilelink.ChannelCOpcode.ReleaseData)
    yield bus.c.bits.param.eq(param)
    yield bus.c.bits.size.eq(size)
    yield bus.c.bits.source.eq(source)
    yield bus.c.bits.address.eq(address)
    yield bus.c.bits.corrupt.eq(0)
    yield bus.c.valid.eq(1)

    for beat in range(beats):
        yield bus.c.bits.data.eq(
            (data >> (beat * bus.data_width)) & ((1 << bus.data_width) - 1))
        yield
        while not (yield bus.c.ready):
            yield

    yield bus.c.valid.eq(0)


# ===========================================================================
# Acquire path
# ===========================================================================
def test_l2_acquire_miss_returns_outer_data():
    top = L2Top(_l2_params())
    model = _model(top)
    done = [False]

    def driver():
        d_op, d_param, d_src, d_sink, data, denied, corrupt = \
            yield from _acquire(top.in_bus,
                                0,
                                size=3,
                                source=0,
                                grow_param=tilelink.GrowParam.NtoB)
        assert d_op == tilelink.ChannelDOpcode.GrantData.value
        assert d_src == 0
        assert (denied, corrupt) == (0, 0)
        assert data == _rd(model, 0, 8)
        done[0] = True

    run_sim(top, *_common(top, model, done), driver)


def test_l2_acquire_hit_skips_outer_fetch():
    top = L2Top(_l2_params())
    model = _model(top)
    done = [False]
    fetches = []

    def driver():
        # First acquire: miss -> one outer fetch.
        res = yield from _acquire(top.in_bus,
                                  0x40,
                                  size=3,
                                  source=0,
                                  grow_param=tilelink.GrowParam.NtoB)
        assert res[4] == _rd(model, 0x40, 8)
        n_after_miss = len(fetches)
        # Second acquire of the same block: hit -> no further outer fetch.
        res = yield from _acquire(top.in_bus,
                                  0x40,
                                  size=3,
                                  source=0,
                                  grow_param=tilelink.GrowParam.NtoB)
        assert res[4] == _rd(model, 0x40, 8)
        assert len(fetches) == n_after_miss, "hit must not fetch from outer"
        done[0] = True

    run_sim(top,
            *_common(top, model, done),
            _proc(_monitor_a_fires, top, done, fetches),
            driver)


# ===========================================================================
# Get path (cached and uncached)
# ===========================================================================
def test_l2_get_after_acquire_returns_cached_data():
    top = L2Top(_l2_params())
    model = _model(top)
    done = [False]
    fetches = []

    def driver():
        # Bring the block in via an acquire.
        yield from _acquire(top.in_bus,
                            0x40,
                            size=3,
                            source=0,
                            grow_param=tilelink.GrowParam.NtoB)
        n_after_miss = len(fetches)
        # A Get on the cached block must hit and return the same data.
        data, denied, corrupt = yield from tl_get(top.in_bus,
                                                  0x40,
                                                  size=3,
                                                  source=1)
        assert (denied, corrupt) == (0, 0)
        assert data == _rd(model, 0x40, 8)
        assert len(fetches) == n_after_miss, "Get on cached block must hit"
        done[0] = True

    run_sim(top,
            *_common(top, model, done),
            _proc(_monitor_a_fires, top, done, fetches),
            driver)


def test_l2_get_miss_fetches_from_outer():
    top = L2Top(_l2_params())
    model = _model(top)
    done = [False]

    def driver():
        # A Get on a never-acquired block forces an L2 miss + outer fetch.
        data, denied, corrupt = yield from tl_get(top.in_bus,
                                                  0x80,
                                                  size=3,
                                                  source=0)
        assert (denied, corrupt) == (0, 0)
        assert data == _rd(model, 0x80, 8)
        done[0] = True

    run_sim(top, *_common(top, model, done), driver)


def test_l2_four_queued_get_misses_default_window():
    """Queue four Gets through the default two-ordinary-MSHR window."""
    top = L2Top(_l2_params())
    model = _model(top)
    done = [False]
    addresses = [0x00, 0x18, 0x50, 0x98]
    expected = {
        source: _rd(model, address, 8)
        for source, address in enumerate(addresses)
    }

    def driver():
        yield top.in_bus.d.ready.eq(0)
        for source, address in enumerate(addresses):
            yield from _send_get_request(top.in_bus,
                                         address,
                                         size=3,
                                         source=source)
        yield from _collect_get_responses(top.in_bus, expected)
        done[0] = True

    run_sim(top, *_common(top, model, done), driver)


def test_l2_default_window_conflict_stress_with_backpressure():
    """Preserve the same-set replacement stress for the default window."""
    top = L2Top(_l2_params())
    model = _model(top)
    done = [False]

    def driver():
        for wave in range(4):
            requests = [((wave * 4 + source) * 0x200, source)
                        for source in range(4)]
            expected = {
                source: _rd(model, address, 8)
                for address, source in requests
            }

            yield top.in_bus.d.ready.eq(0)
            for address, source in requests:
                yield from _send_get_request(top.in_bus,
                                             address,
                                             size=3,
                                             source=source)

            ready_fn = lambda cycle, wave=wave: (cycle + wave) % 3 != 2
            yield from _collect_get_responses(top.in_bus,
                                              expected,
                                              ready_fn=ready_fn)

        done[0] = True

    run_sim(top, *_common(top, model, done), driver)


def test_l2m6_nested_release_probeackdata_way_cam_race():
    """Regression for the SinkC way-CAM race (l2_race_audit Finding 1).

    An ordinary MSHR owns a set while collecting a dirty ProbeAckData from
    one client.  A second client's voluntary ReleaseData for a *different
    tag in the same set* is nested into the reserved C MSHR, which then
    parks on the set waiting for SourceD (pinned busy by a stalled grant).
    The delayed ProbeAckData beats arrive while both MSHRs own the set;
    SinkC must resolve their bank writes to the ordinary MSHR's way.  A CAM
    that gives the reserved C MSHR top precedence misroutes them into the
    nested Release's way instead: the acquirer of the probed line is
    granted the stale bank contents and the line's eventual writeback
    loses the client's dirty data.
    """
    params = _six_mshr_params(
        block_bytes=64,
        outer_beat_bytes=64,
        port_factor=4,
        client_source_map={0: (0, 1), 1: (2, 3)},
    )
    top = L2Top(params)
    model = _model(top)
    done = [False]
    clients = {}
    probes = []
    respond_now = [False]
    responded = [False]
    size = log2_int(64)

    addr_a = 0x000  # set 0, tag 0: probed line, dirty at client 0
    addr_b = 0x400  # set 0, tag 1: nested-release line, dirty at client 1
    addr_c = 0x040  # set 1: blocker Get that keeps SourceD busy
    beats_a = [0xA17D000000000000 + beat for beat in range(8)]
    beats_b = [0xB17D000000000000 + beat for beat in range(8)]
    value_a = sum(value << (64 * beat) for beat, value in enumerate(beats_a))
    value_b = sum(value << (64 * beat) for beat, value in enumerate(beats_b))

    def driver():
        # Setup: two private dirty lines, same set, distinct tags/ways.
        res_a = yield from _acquire(top.in_bus,
                                    addr_a,
                                    size=size,
                                    source=0,
                                    grow_param=tilelink.GrowParam.NtoT)
        assert res_a[1] == tilelink.CapParam.toT.value
        clients[0] = dict(cap=tilelink.CapParam.toT,
                          data=value_a,
                          dirty=True)
        res_b = yield from _acquire(top.in_bus,
                                    addr_b,
                                    size=size,
                                    source=2,
                                    grow_param=tilelink.GrowParam.NtoT)
        assert res_b[1] == tilelink.CapParam.toT.value
        clients[2] = dict(cap=tilelink.CapParam.toT,
                          data=value_b,
                          dirty=True)
        assert probes == [], "setup must not probe (unexpected victim way)"

        # Pin SourceD busy: the blocker Get's grant stalls on D.
        yield top.in_bus.d.ready.eq(0)
        yield from _send_get_request(top.in_bus, addr_c, size=size, source=0)
        for _ in range(10000):
            if (yield top.in_bus.d.valid):
                break
            yield
        else:
            raise AssertionError("blocker grant never presented")

        # Client 1 acquires line A for T: an ordinary MSHR probes client 0,
        # the dirty owner.  The agent accepts the probe but holds the
        # answer until the nested Release below has landed.
        yield from _send_acquire_request(top.in_bus, addr_a,
                                         size=size, source=2,
                                         grow=tilelink.GrowParam.NtoT)
        for _ in range(10000):
            if probes:
                break
            yield
        else:
            raise AssertionError("probe to the dirty owner never issued")
        assert probes[0] == (0, addr_a, tilelink.CapParam.toN.value, True)

        # Client 1's L1 evicts line B (same set, other tag/way) while the
        # acquire is outstanding: nest_c routes the ReleaseData into the
        # reserved C MSHR, which cannot retire while SourceD is busy.
        yield from _send_release_data(top.in_bus, addr_b, size=size,
                                      source=3, data=value_b)
        for _ in range(4):
            yield
        assert (yield top.in_bus.d.valid), "SourceD left busy during window"

        # Answer the held probe: client 0's dirty ProbeAckData beats for
        # line A must be written to line A's way.
        respond_now[0] = True
        while not responded[0]:
            yield
        for _ in range(4):
            yield

        # Drain: blocker AccessAckData, the nested Release's ReleaseAck,
        # then client 1's GrantData for line A, which must carry client
        # 0's dirty bytes rather than the stale outer image.
        remaining = {
            (tilelink.ChannelDOpcode.AccessAckData.value, 0): 8,
            (tilelink.ChannelDOpcode.ReleaseAck.value, 3): 1,
            (tilelink.ChannelDOpcode.GrantData.value, 2): 8,
        }
        grant = []
        grant_sink = None
        yield top.in_bus.d.ready.eq(1)
        yield
        for _ in range(20000):
            if (yield top.in_bus.d.valid):
                opcode = (yield top.in_bus.d.bits.opcode)
                source = (yield top.in_bus.d.bits.source)
                key = (opcode, source)
                assert key in remaining, f"unexpected response {key}"
                assert remaining[key] > 0, f"duplicate response {key}"
                assert (yield top.in_bus.d.bits.denied) == 0
                assert (yield top.in_bus.d.bits.corrupt) == 0
                remaining[key] -= 1
                if opcode == tilelink.ChannelDOpcode.GrantData.value:
                    if not grant:
                        grant_sink = (yield top.in_bus.d.bits.sink)
                        assert (yield top.in_bus.d.bits.param) == \
                            tilelink.CapParam.toT.value
                    grant.append((yield top.in_bus.d.bits.data))
                if not any(remaining.values()):
                    break
            yield
        else:
            raise AssertionError(f"drain timed out: {remaining}")

        assert grant == beats_a, (
            f"GrantData for line A served stale bank contents {grant}; "
            "the dirty ProbeAckData beats were misrouted into the "
            "nested Release's way")
        yield from tl_grantack(top.in_bus, sink=grant_sink)
        clients[2] = dict(cap=tilelink.CapParam.toT,
                          data=sum(b << (64 * i) for i, b in enumerate(grant)),
                          dirty=False)

        # Replace both same-set lines; each writeback must carry its own
        # line's data to the outer memory.
        yield from tl_get(top.in_bus, 0x800, size=size, source=0)
        yield from tl_get(top.in_bus, 0xC00, size=size, source=0)
        for _ in range(10000):
            if _rd(model, addr_b, 64) == value_b:
                break
            yield
        else:
            raise AssertionError("line B writeback never reached memory")
        for _ in range(500):
            yield
        assert _rd(model, addr_b, 64) == value_b, \
            "line B corrupted in DRAM (release merge lost its data)"
        assert _rd(model, addr_a, 64) == value_a, \
            "line A writeback lost the client's dirty ProbeAckData"
        done[0] = True

    run_sim(top,
            *_common(top, model, done),
            _proc(_probe_responder, top.in_bus, clients, probes, done,
                  hold_source=0, respond_now=respond_now,
                  responded=responded),
            driver)


# ===========================================================================
# Put path (write-merge into cached lines)
# ===========================================================================
def test_l2_put_full_then_get_roundtrip():
    top = L2Top(_l2_params())
    model = _model(top)
    done = [False]
    value = 0x1122334455667788

    def driver():
        denied = yield from tl_put(top.in_bus,
                                   0x10,
                                   value,
                                   0xFF,
                                   size=3,
                                   source=0)
        assert denied == 0
        data, _, _ = yield from tl_get(top.in_bus, 0x10, size=3, source=1)
        assert data == value
        done[0] = True

    run_sim(top, *_common(top, model, done), driver)


def test_l2_put_partial_then_get_roundtrip():
    top = L2Top(_l2_params())
    model = _model(top)
    done = [False]
    addr = 0x18
    value = 0xAABBCCDD  # written into the low 4 bytes only (mask 0b00001111)

    def driver():
        denied = yield from tl_put(top.in_bus,
                                   addr,
                                   value,
                                   0b00001111,
                                   size=3,
                                   source=0,
                                   full=False)
        assert denied == 0
        data, _, _ = yield from tl_get(top.in_bus, addr, size=3, source=1)
        # High 4 bytes keep the model image; low 4 bytes are overwritten.
        expected = (_rd(model, addr, 8) & 0xFFFFFFFF00000000) | (
            value & 0xFFFFFFFF)
        assert data == expected
        done[0] = True

    run_sim(top, *_common(top, model, done), driver)


# ===========================================================================
# Writeback path
# ===========================================================================
def test_l2_dirty_writeback_on_eviction():
    """A dirty line written via Put must reach the outer model when evicted.

    Geometry: 2 ways, round-robin replacement. Addresses 0, 0x200, 0x400 share
    set 0 (offset=3, index=6 bits) and differ only in tag. Writing block 0,
    then touching two other tags in the same set, evicts block 0; the eviction
    is a ReleaseData that updates ``model``. A subsequent Get misses and reads
    the written-back value from the model.
    """
    top = L2Top(_l2_params())
    model = _model(top)
    done = [False]
    value = 0xCAFEF00DBAADF00D

    def driver():
        # Fill way 0 (set 0, tag 0) with a dirty write.
        denied = yield from tl_put(top.in_bus, 0, value, 0xFF, size=3,
                                   source=0)
        assert denied == 0
        # Read it back from the cache (still cached, clean read).
        cached, _, _ = yield from tl_get(top.in_bus, 0, size=3, source=1)
        assert cached == value
        # Touch two distinct tags in set 0 to force eviction of tag 0.
        yield from tl_get(top.in_bus, 0x200, size=3, source=0)
        yield from tl_get(top.in_bus, 0x400, size=3, source=0)
        # Block 0 was evicted (dirty): model must now hold the written value.
        assert _rd(model, 0, 8) == value
        # Re-read block 0 -> miss -> fetched back from the outer model.
        data, _, _ = yield from tl_get(top.in_bus, 0, size=3, source=0)
        assert data == value
        done[0] = True

    run_sim(top, *_common(top, model, done), driver)


def test_l2_release_data_then_get():
    """A client ReleaseData writes the block into the L2; a later Get hits."""
    top = L2Top(_l2_params())
    model = _model(top)
    done = [False]
    value = 0xDEADBEEFCAFEBABE

    def driver():
        # Acquire the block for writing (NtoT), then hand a dirty copy back.
        yield from _acquire(top.in_bus,
                            0x30,
                            size=3,
                            source=0,
                            grow_param=tilelink.GrowParam.NtoT)
        d_op, d_src, d_denied = yield from tl_release(
            top.in_bus,
            0x30,
            size=3,
            source=0,
            param=tilelink.ShrinkReportParam.TtoN,
            data=value)
        assert d_op == tilelink.ChannelDOpcode.ReleaseAck.value
        assert d_src == 0
        assert d_denied == 0
        # The released data now lives in the L2; a Get hits and returns it.
        data, _, _ = yield from tl_get(top.in_bus, 0x30, size=3, source=0)
        assert data == value
        done[0] = True

    run_sim(top, *_common(top, model, done), driver)


def test_l2_clean_release_then_get():
    """A clean BtoN Release drops the client but preserves the L2 data."""
    top = L2Top(_l2_params())
    model = _model(top)
    done = [False]
    address = 0x38

    def driver():
        yield from _acquire(top.in_bus,
                            address,
                            size=3,
                            source=0,
                            grow_param=tilelink.GrowParam.NtoB)
        d_op, d_src, d_denied = yield from tl_release(
            top.in_bus,
            address,
            size=3,
            source=0,
            param=tilelink.ShrinkReportParam.BtoN)
        assert d_op == tilelink.ChannelDOpcode.ReleaseAck.value
        assert (d_src, d_denied) == (0, 0)
        data, denied, corrupt = yield from tl_get(top.in_bus,
                                                  address,
                                                  size=3,
                                                  source=1)
        assert (denied, corrupt) == (0, 0)
        assert data == _rd(model, address, 8)
        done[0] = True

    run_sim(top, *_common(top, model, done), driver)


# ===========================================================================
# Multi-client sharing
# ===========================================================================
def test_l2_two_clients_share_readonly_block():
    """Two coherent clients acquiring the same block (NtoB) both observe the
    outer data without the L2 deadlocking on cross-client probes."""
    params = _l2_params(client_source_map={0: (0, 1), 1: (2, 3)})
    top = L2Top(params)
    model = _model(top)
    done = [False]
    expected = _rd(model, 0x40, 8)

    def driver():
        res0 = yield from _acquire(top.in_bus,
                                   0x40,
                                   size=3,
                                   source=0,
                                   grow_param=tilelink.GrowParam.NtoB)
        assert res0[4] == expected
        res1 = yield from _acquire(top.in_bus,
                                   0x40,
                                   size=3,
                                   source=2,
                                   grow_param=tilelink.GrowParam.NtoB)
        assert res1[4] == expected
        done[0] = True

    run_sim(top, *_common(top, model, done), driver)


def test_l2_exclusive_ownership_transfer_probes_old_client():
    """A second NtoT acquisition invalidates the previous trunk client."""
    params = _l2_params(client_source_map={0: (0, 1), 1: (2, 3)})
    top = L2Top(params)
    model = _model(top)
    done = [False]
    clients = {}
    probes = []
    fetches = []
    address = 0x40
    expected = _rd(model, address, 8)

    def driver():
        first = yield from _acquire(top.in_bus,
                                    address,
                                    size=3,
                                    source=0,
                                    grow_param=tilelink.GrowParam.NtoT)
        assert first[1] == tilelink.CapParam.toT.value
        assert first[4] == expected
        clients[0] = dict(cap=tilelink.CapParam.toT,
                          data=expected,
                          dirty=False)
        n_fetches = len(fetches)

        second = yield from _acquire(top.in_bus,
                                     address,
                                     size=3,
                                     source=2,
                                     grow_param=tilelink.GrowParam.NtoT)
        assert second[1] == tilelink.CapParam.toT.value
        assert second[4] == expected
        clients[2] = dict(cap=tilelink.CapParam.toT,
                          data=expected,
                          dirty=False)
        assert 0 not in clients
        assert probes == [(0, address, tilelink.CapParam.toN.value, False)]
        assert len(fetches) == n_fetches, "ownership transfer must use L2 data"
        done[0] = True

    run_sim(top,
            *_common(top, model, done),
            _proc(_probe_responder, top.in_bus, clients, probes, done),
            _proc(_monitor_a_fires, top, done, fetches),
            driver)


def test_l2_dirty_probeack_data_forwarded_to_new_sharer():
    """A dirty trunk client supplies the data when another client shares."""
    params = _l2_params(client_source_map={0: (0, 1), 1: (2, 3)})
    top = L2Top(params)
    model = _model(top)
    done = [False]
    clients = {}
    probes = []
    address = 0x48
    dirty_value = 0x8877665544332211

    def driver():
        yield from _acquire(top.in_bus,
                            address,
                            size=3,
                            source=0,
                            grow_param=tilelink.GrowParam.NtoT)
        # Model a write performed privately by client 0 after it received T.
        clients[0] = dict(cap=tilelink.CapParam.toT,
                          data=dirty_value,
                          dirty=True)

        shared = yield from _acquire(top.in_bus,
                                     address,
                                     size=3,
                                     source=2,
                                     grow_param=tilelink.GrowParam.NtoB)
        assert shared[1] == tilelink.CapParam.toB.value
        assert shared[4] == dirty_value
        clients[2] = dict(cap=tilelink.CapParam.toB,
                          data=dirty_value,
                          dirty=False)
        assert clients[0]["cap"] == tilelink.CapParam.toB
        assert not clients[0]["dirty"]
        assert probes == [(0, address, tilelink.CapParam.toB.value, True)]

        data, denied, corrupt = yield from tl_get(top.in_bus,
                                                  address,
                                                  size=3,
                                                  source=3)
        assert (denied, corrupt) == (0, 0)
        assert data == dirty_value
        done[0] = True

    run_sim(top,
            *_common(top, model, done),
            _proc(_probe_responder, top.in_bus, clients, probes, done),
            driver)


def test_l2_acquire_perm_upgrade_probes_other_sharer():
    """BtoT AcquirePerm invalidates the other branch and returns no data."""
    params = _l2_params(client_source_map={0: (0, 1), 1: (2, 3)})
    top = L2Top(params)
    model = _model(top)
    done = [False]
    clients = {}
    probes = []
    address = 0x50
    expected = _rd(model, address, 8)

    def driver():
        for source in (0, 2):
            result = yield from _acquire(top.in_bus,
                                         address,
                                         size=3,
                                         source=source,
                                         grow_param=tilelink.GrowParam.NtoB)
            assert result[4] == expected
            clients[source] = dict(cap=tilelink.CapParam.toB,
                                   data=expected,
                                   dirty=False)

        upgraded = yield from _acquire(
            top.in_bus,
            address,
            size=3,
            source=0,
            grow_param=tilelink.GrowParam.BtoT,
            opcode=tilelink.ChannelAOpcode.AcquirePerm)
        assert upgraded[0] == tilelink.ChannelDOpcode.Grant.value
        assert upgraded[1] == tilelink.CapParam.toT.value
        clients[0]["cap"] = tilelink.CapParam.toT
        assert 2 not in clients
        assert probes == [(2, address, tilelink.CapParam.toN.value, False)]
        done[0] = True

    run_sim(top,
            *_common(top, model, done),
            _proc(_probe_responder, top.in_bus, clients, probes, done),
            driver)


def test_l2_outer_probe_collects_dirty_inner_data():
    """An outer toN Probe cascades inward and returns dirty ProbeAckData."""
    top = L2Top(_l2_params())
    model = _model(top)
    done = [False]
    clients = {}
    inner_probes = []
    outer_probes = []
    outer_responses = []
    address = 0x58
    dirty_value = 0x1020304050607080

    def driver():
        yield from _acquire(top.in_bus,
                            address,
                            size=3,
                            source=0,
                            grow_param=tilelink.GrowParam.NtoT)
        clients[0] = dict(cap=tilelink.CapParam.toT,
                          data=dirty_value,
                          dirty=True)

        outer_probes.append((address, 3, tilelink.CapParam.toN, 1))
        for _ in range(10000):
            if outer_responses:
                break
            yield
        assert outer_responses, "outer Probe timed out"
        opcode, report, source, data = outer_responses[0]
        assert opcode == tilelink.ChannelCOpcode.ProbeAckData.value
        assert report == tilelink.ShrinkReportParam.TtoN.value
        assert source == 1
        assert data == dirty_value
        assert not clients
        assert inner_probes == [
            (0, address, tilelink.CapParam.toN.value, True)
        ]
        done[0] = True

    run_sim(
        top,
        _proc(tl_c_responder,
              top.out_bus,
              model=model,
              done=done,
              probes=outer_probes,
              probe_responses=outer_responses),
        _proc(_watchdog, done),
        _proc(_probe_responder, top.in_bus, clients, inner_probes, done),
        driver)


# ===========================================================================
# Multi-beat block (2 beats per line)
# ===========================================================================
def test_l2_multibeat_block_acquire_and_get():
    params = _l2_params(block_bytes=16, n_mshrs=4)
    top = L2Top(params)
    model = _model(top)
    done = [False]
    size = log2_int(16)

    def driver():
        d_op, d_param, d_src, d_sink, data, denied, corrupt = \
            yield from _acquire(top.in_bus,
                                0x20,
                                size=size,
                                source=0,
                                grow_param=tilelink.GrowParam.NtoB)
        assert d_op == tilelink.ChannelDOpcode.GrantData.value
        assert (denied, corrupt) == (0, 0)
        assert data == _rd(model, 0x20, 16)

        data, _, _ = yield from tl_get(top.in_bus, 0x20, size=size, source=1)
        assert data == _rd(model, 0x20, 16)
        done[0] = True

    run_sim(top, *_common(top, model, done), driver)


def test_l2_multibeat_dirty_probe_then_writeback():
    """Forward two dirty beats from a client, then write them back on eviction."""
    params = _l2_params(block_bytes=16,
                        n_mshrs=4,
                        client_source_map={0: (0, 1), 1: (2, 3)})
    top = L2Top(params)
    model = _model(top)
    done = [False]
    clients = {}
    probes = []
    address = 0x20
    size = log2_int(16)
    dirty_value = 0xFFEEDDCCBBAA99887766554433221100

    def driver():
        yield from _acquire(top.in_bus,
                            address,
                            size=size,
                            source=0,
                            grow_param=tilelink.GrowParam.NtoT)
        clients[0] = dict(cap=tilelink.CapParam.toT,
                          data=dirty_value,
                          dirty=True)

        shared = yield from _acquire(top.in_bus,
                                     address,
                                     size=size,
                                     source=2,
                                     grow_param=tilelink.GrowParam.NtoB)
        assert shared[4] == dirty_value
        clients[2] = dict(cap=tilelink.CapParam.toB,
                          data=dirty_value,
                          dirty=False)
        assert probes == [(0, address, tilelink.CapParam.toB.value, True)]

        # All three addresses map to set 2. The third fill evicts the dirty
        # shared line, first probing both branch clients and then releasing its
        # two data beats to outer memory.
        yield from tl_get(top.in_bus, address + 0x200, size=size, source=0)
        yield from tl_get(top.in_bus, address + 0x400, size=size, source=0)
        assert _rd(model, address, 16) == dirty_value
        assert sorted(probes[1:]) == sorted([
            (0, address, tilelink.CapParam.toN.value, False),
            (2, address, tilelink.CapParam.toN.value, False),
        ])
        done[0] = True

    run_sim(top,
            *_common(top, model, done),
            _proc(_probe_responder, top.in_bus, clients, probes, done),
            driver)


def test_l2_wide_outer_eviction_preserves_gapped_probeack_data():
    """A 512-bit eviction must not cut into a gapped 64-bit ProbeAckData.

    The conflict miss evicts a dirty client-owned line. SourceC is allowed to
    start once the first probe response beat arrives, so SinkC's noop bank
    reservations must keep the one-beat outer read stalled across each legal
    gap until all eight inner beats have updated the BankedStore.
    """
    params = _l2_params(block_bytes=64,
                        outer_beat_bytes=64,
                        port_factor=4,
                        n_mshrs=4)
    top = L2Top(params)
    model = _model(top)
    done = [False]
    clients = {}
    probes = []
    address = 0x40
    size = log2_int(64)
    set_stride = 0x200
    dirty_beats = [
        0x1111111111111111,
        0x2222222222222222,
        0x3333333333333333,
        0x4444444444444444,
        0x5555555555555555,
        0x6666666666666666,
        0x7777777777777777,
        0x0001003C00000000,
    ]
    dirty_value = sum(beat << (64 * i)
                      for i, beat in enumerate(dirty_beats))

    def driver():
        yield from _acquire(top.in_bus,
                            address,
                            size=size,
                            source=0,
                            grow_param=tilelink.GrowParam.NtoT)
        clients[0] = dict(cap=tilelink.CapParam.toT,
                          data=dirty_value,
                          dirty=True)

        # Fill the other way, then replace the client-owned dirty victim.
        yield from tl_get(top.in_bus,
                          address + set_stride,
                          size=size,
                          source=1)
        yield from tl_get(top.in_bus,
                          address + 2 * set_stride,
                          size=size,
                          source=1)

        assert probes == [
            (0, address, tilelink.CapParam.toN.value, True)
        ]
        actual = _rd(model, address, 64)
        assert actual == dirty_value, (
            f"dirty victim mismatch:\nactual   {actual:0128x}\n"
            f"expected {dirty_value:0128x}")
        done[0] = True

    run_sim(top,
            *_common(top, model, done),
            _proc(_probe_responder,
                  top.in_bus,
                  clients,
                  probes,
                  done,
                  beat_gap=1),
            driver)


def _send_put_request(bus, address, value, *, source):
    """Send only the A beat of a full 8-byte Put, without waiting for the
    AccessAck, so stores can be pipelined ahead of a following acquire."""
    beat_bytes = bus.data_width // 8
    yield bus.a.bits.opcode.eq(tilelink.ChannelAOpcode.PutFullData)
    yield bus.a.bits.param.eq(0)
    yield bus.a.bits.size.eq(log2_int(beat_bytes))
    yield bus.a.bits.source.eq(source)
    yield bus.a.bits.address.eq(address)
    yield bus.a.bits.mask.eq((1 << beat_bytes) - 1)
    yield bus.a.bits.data.eq(value)
    yield bus.a.bits.corrupt.eq(0)
    yield bus.a.valid.eq(1)
    yield
    while not (yield bus.a.ready):
        yield
    yield bus.a.valid.eq(0)


def _send_acquire_request(bus, address, *, size, source,
                          grow=tilelink.GrowParam.NtoB):
    """Send only the A beat of an AcquireBlock."""
    yield bus.a.bits.opcode.eq(tilelink.ChannelAOpcode.AcquireBlock)
    yield bus.a.bits.param.eq(grow)
    yield bus.a.bits.size.eq(size)
    yield bus.a.bits.source.eq(source)
    yield bus.a.bits.address.eq(address)
    yield bus.a.bits.mask.eq((1 << (bus.data_width // 8)) - 1)
    yield bus.a.bits.data.eq(0)
    yield bus.a.bits.corrupt.eq(0)
    yield bus.a.valid.eq(1)
    yield
    while not (yield bus.a.ready):
        yield
    yield bus.a.valid.eq(0)


def _collect_put_and_grant(bus, *, n_puts, grant_source, beats, stored,
                           round_idx, timeout=10000):
    """Drain ``n_puts`` AccessAcks and one full GrantData message.

    The puts and the acquire are in flight together, so their D responses
    may complete in any order; beats of the grant are reassembled from
    GrantData beats only. Meant to run as its own process so D is drained
    while the A-channel sender keeps transmitting (a TileLink master must
    sink D concurrently, or the L2's response queue backpressures the
    whole pipeline).
    """
    acks = 0
    grant = []
    grant_sink = None
    yield bus.d.ready.eq(1)
    yield
    for _ in range(timeout):
        if (yield bus.d.valid):
            opcode = (yield bus.d.bits.opcode)
            if opcode == tilelink.ChannelDOpcode.AccessAck.value:
                acks += 1
            elif opcode == tilelink.ChannelDOpcode.GrantData.value:
                assert (yield bus.d.bits.source) == grant_source
                if not grant:
                    grant_sink = (yield bus.d.bits.sink)
                grant.append((yield bus.d.bits.data))
            if acks >= n_puts and len(grant) >= beats:
                yield
                yield bus.d.ready.eq(0)
                data = sum(g << (64 * i) for i, g in enumerate(grant))
                for beat, value in stored.items():
                    got = (data >> (64 * beat)) & ((1 << 64) - 1)
                    assert got == value, (
                        f"round {round_idx}: grant beat {beat} is "
                        f"{got:#x}, expected the final store {value:#x} "
                        "(grant raced the put-buffer merge)")
                yield from tl_grantack(bus, sink=grant_sink)
                return data
        yield
    raise AssertionError(
        f"round {round_idx}: timed out ({acks}/{n_puts} acks, "
        f"{len(grant)}/{beats} grant beats)")


def test_l2_grant_waits_for_inflight_put_merges():
    """A grant must not read beats whose trailing put-merge has not landed.

    Stores stream into a cached line as one-beat Puts. Each Put's MSHR
    completes when SourceD emits the AccessAck from s3, but the put-buffer
    merge into the BankedStore commits later from s4. A grant scheduled
    right behind the final AccessAck has its beats read by SourceD's s1;
    without an ordering hazard the read of a beat can be presented before
    that beat's merge commits, and the non-transparent BankedStore returns
    the pre-store contents.

    Regression for the l1m4 stale-grant corruption (the granted beat
    carried the pre-store word 0xaaa315b3 while the storing client's
    newest merge for the same beat committed two cycles later).
    """
    params = _l2_params(block_bytes=64,
                        outer_beat_bytes=64,
                        port_factor=4,
                        n_mshrs=4)
    top = L2Top(params)
    model = _model(top)
    done = [False]
    clients = {}
    probes = []
    size = log2_int(64)
    beat_bytes = top.in_bus.data_width // 8
    rounds = 8
    round_data = {}
    setup_done = [False] * rounds
    round_done = [False] * rounds

    for r in range(rounds):
        address = 0x40 + r * 0x200
        stored = {}
        puts = []
        for step in range(4):
            for beat in (2, 4):
                value = (0x5EED0000 + (r << 8) + (step << 4) + beat)
                stored[beat] = value
                puts.append((address + beat * beat_bytes, value))
        round_data[r] = dict(address=address, expected=_rd(model, address,
                                                           64),
                             stored=stored, puts=puts)

    def tx():
        for r in range(rounds):
            info = round_data[r]
            # Populate the line without attaching an owning client. This
            # keeps the regression focused on the Put-merge/grant ordering
            # and avoids introducing a ProbeAck dependency.
            yield from _send_get_request(top.in_bus, info['address'],
                                         size=size, source=0)
            while not setup_done[r]:
                yield
            # Stores pipelined behind one another, then the acquire of a
            # second source of the same client, all without waiting for
            # the responses in between.
            for put_address, value in info['puts']:
                yield from _send_put_request(top.in_bus, put_address, value,
                                             source=0)
            yield from _send_acquire_request(top.in_bus, info['address'],
                                             size=size, source=1)
            while not round_done[r]:
                yield
        done[0] = True

    def rx():
        yield top.in_bus.d.ready.eq(1)
        for r in range(rounds):
            info = round_data[r]
            # Setup Get response (source 0, full line).
            setup = []
            for _ in range(10000):
                if (yield top.in_bus.d.valid):
                    assert (yield top.in_bus.d.bits.opcode) == \
                        tilelink.ChannelDOpcode.AccessAckData.value
                    assert (yield top.in_bus.d.bits.source) == 0
                    setup.append((yield top.in_bus.d.bits.data))
                    if len(setup) == 8:
                        # Consume the final D beat before switching to the
                        # AccessAck/grant collector. Reads are in-cycle in
                        # pysim; only this naked yield fires the handshake.
                        yield
                        break
                yield
            setup_done[r] = True
            # Puts + second acquire: collect acks and the racing grant.
            data = yield from _collect_put_and_grant(
                top.in_bus, n_puts=len(info['puts']), grant_source=1,
                beats=8, stored=info['stored'], round_idx=r)
            # The client model keeps holding the shared copy granted to
            # source 1, so a later eviction of this line probes it.
            clients[0] = dict(cap=tilelink.CapParam.toB,
                              data=data,
                              dirty=False)
            round_done[r] = True

    run_sim(top,
            *_common(top, model, done),
            _proc(_probe_responder, top.in_bus, clients, probes, done),
            tx, rx)
