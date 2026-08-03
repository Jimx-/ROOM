"""Phase 2: TileLink Arbiter tests.

Covers ``roomsoc.interconnect.tilelink.Arbiter`` -- the generic N->1
per-channel round-robin / ``lowest`` arbiter with burst locking used by
``dcache.py``, ``l3cache.py`` and ``groom/wrapper.py``.

  * idle: no spurious ``valid`` on the output.
  * routing: only the selected input sees ``ready`` and reaches the output.
  * Decoupled stability: a stalled payload cannot be displaced by a new offer.
  * round-robin rotation order (no source repeats while all contend).
  * ``lowest`` fixed priority (source 0 monopolises; idle sources skipped).
  * burst locking: two- and four-beat bursts hold the bus through backpressure
    until ``beats_left == 0``, even with a contender requesting mid-burst.
  * fairness under backpressure (no starvation, roughly equal RR shares).
  * functional routing across ChannelA/B/C/D/E -- E has no ``size`` field and
    therefore exercises the 1-bit ``beats_left`` branch.

All tests use the two-process Decoupled pattern (one driver per input plus a
monitor that drives ``ready`` and records accepted beats) with the pysim clock
model from AGENTS.md.
"""

import pytest

from amaranth import *

from roomsoc.interconnect import tilelink
from roomsoc.interconnect.stream import Decoupled
from tl_helpers import run_sim


# Channel-A bus parameters shared by most tests: data_width=64 -> beat_bytes=8.
A_KW = dict(addr_width=32, data_width=64, size_width=4, source_id_width=4)
BEAT_BYTES = 8


def _arb_top(n, cls, kw, *, policy="rr"):
    """Build a Top instantiating an N-input arbiter of ``cls``."""

    class Top(Elaboratable):

        def __init__(self):
            self.inputs = [Decoupled(cls, **kw) for _ in range(n)]
            self.arb = tilelink.Arbiter(cls, **kw, policy=policy)
            for inp in self.inputs:
                self.arb.add(inp)
            self.bus = self.arb.bus

        def elaborate(self, platform):
            m = Module()
            m.submodules.arb = self.arb
            return m

    return Top()


# ---------------------------------------------------------------------------
# Input driver factories (each returns a generator FUNCTION for add_sync_process;
# args are captured in the closure so the helpers can be parametrized per input).
# Each drives one Decoupled input and respects ``ready`` (which the arbiter
# routes back only when the input is selected).
# ---------------------------------------------------------------------------
def offer_continuous(inp, src, ncycles, *, beat_bytes=BEAT_BYTES):
    """Hold a single-beat Get offer (valid=1, stable bits) for ``ncycles``."""

    def proc():
        yield inp.bits.opcode.eq(tilelink.ChannelAOpcode.Get)
        yield inp.bits.size.eq(3)
        yield inp.bits.source.eq(src)
        yield inp.bits.address.eq(src * 0x10)
        yield inp.bits.mask.eq((1 << beat_bytes) - 1)
        yield inp.bits.data.eq(0)
        yield inp.bits.corrupt.eq(0)
        yield inp.valid.eq(1)
        for _ in range(ncycles):
            yield
        yield inp.valid.eq(0)
        for _ in range(5):
            yield

    return proc


def offer_gets(inp, src, count, *, address=None, beat_bytes=BEAT_BYTES):
    """Offer ``count`` single-beat Gets, counting accepted beats."""

    def proc():
        addr = src * 0x10 if address is None else address
        yield inp.bits.opcode.eq(tilelink.ChannelAOpcode.Get)
        yield inp.bits.size.eq(3)
        yield inp.bits.source.eq(src)
        yield inp.bits.address.eq(addr)
        yield inp.bits.mask.eq((1 << beat_bytes) - 1)
        yield inp.bits.data.eq(0)
        yield inp.bits.corrupt.eq(0)
        yield inp.valid.eq(1)
        fired = 0
        while fired < count:
            yield
            if (yield inp.ready):
                fired += 1
        yield inp.valid.eq(0)
        for _ in range(5):
            yield

    return proc


def offer_burst(inp, src, nbeats, *, size, beat_bytes=BEAT_BYTES,
                idle_after=25):
    """Offer an ``nbeats``-beat PutFull burst (``size`` sets the beat count),
    then idle. Respects ``ready`` so the burst is paced by the arbiter."""

    def proc():
        yield inp.bits.opcode.eq(tilelink.ChannelAOpcode.PutFullData)
        yield inp.bits.size.eq(size)
        yield inp.bits.source.eq(src)
        yield inp.bits.address.eq(src * 0x10)
        yield inp.bits.mask.eq((1 << beat_bytes) - 1)
        yield inp.bits.corrupt.eq(0)
        fired = 0
        while fired < nbeats:
            yield inp.bits.data.eq(src * 0x100 + fired)
            yield inp.valid.eq(1)
            yield
            guard = 0
            while not (yield inp.ready):
                yield
                guard += 1
                if guard > 200:
                    return
            fired += 1
        yield inp.valid.eq(0)
        for _ in range(idle_after):
            yield

    return proc


# ---------------------------------------------------------------------------
# Monitor factories (drive ``ready`` on the arbiter output, record beats).
# ---------------------------------------------------------------------------
def monitor_record(bus, served, ncycles, fields):
    """Always-ready monitor; appends the requested ``fields`` tuple per fire."""

    def proc():
        yield bus.ready.eq(1)
        yield
        for _ in range(ncycles):
            if (yield bus.fire):
                vals = []
                for f in fields:
                    vals.append((yield getattr(bus.bits, f)))
                served.append(tuple(vals) if len(vals) > 1 else vals[0])
            yield

    return proc


def monitor_backpressure(bus, served, ncycles, ready_fn, field="source"):
    """Monitor with a deterministic ``ready_fn(cycle)`` backpressure."""

    def proc():
        yield bus.ready.eq(1 if ready_fn(0) else 0)
        yield
        for c in range(ncycles):
            ready = (yield bus.ready)
            valid = (yield bus.valid)
            if ready and valid:
                served.append((yield getattr(bus.bits, field)))
            yield bus.ready.eq(1 if ready_fn(c + 1) else 0)
            yield

    return proc


# ---------------------------------------------------------------------------
# Tests
# ---------------------------------------------------------------------------
def test_arbiter_idle_no_spurious_valid():
    top = _arb_top(3, tilelink.ChannelA, A_KW)

    def proc():
        yield  # let combinational logic settle
        assert (yield top.bus.valid) == 0

    run_sim(top, proc)


def test_arbiter_routes_only_requesting_input():
    top = _arb_top(3, tilelink.ChannelA, A_KW)
    served = []
    run_sim(top,
            offer_gets(top.inputs[2], 2, 1),
            monitor_record(top.bus, served, 12, ["source", "address"]))
    assert served == [(2, 0x20)]


def test_arbiter_routes_ready_only_to_selected_input():
    top = _arb_top(3, tilelink.ChannelA, A_KW)

    def proc():
        for i in (1, 2):
            yield top.inputs[i].bits.opcode.eq(tilelink.ChannelAOpcode.Get)
            yield top.inputs[i].bits.size.eq(3)
            yield top.inputs[i].bits.source.eq(i)
            yield top.inputs[i].bits.address.eq(i * 0x10)
            yield top.inputs[i].valid.eq(1)
        yield top.bus.ready.eq(1)
        yield

        # Initial RR grant is source 1. Only that producer sees ready.
        assert (yield top.bus.valid) == 1
        assert (yield top.bus.bits.source) == 1
        ready = []
        for inp in top.inputs:
            ready.append((yield inp.ready))
        assert ready == [0, 1, 0]

        yield top.bus.ready.eq(0)
        yield
        ready = []
        for inp in top.inputs:
            ready.append((yield inp.ready))
        assert ready == [0, 0, 0]

    run_sim(top, proc)


def test_arbiter_stalled_output_is_stable_when_new_request_arrives():
    top = _arb_top(3, tilelink.ChannelA, A_KW)

    def proc():
        yield top.bus.ready.eq(0)
        yield top.inputs[2].bits.opcode.eq(tilelink.ChannelAOpcode.Get)
        yield top.inputs[2].bits.size.eq(3)
        yield top.inputs[2].bits.source.eq(2)
        yield top.inputs[2].bits.address.eq(0x2220)
        yield top.inputs[2].valid.eq(1)
        yield

        stalled = ((yield top.bus.bits.source),
                   (yield top.bus.bits.address))
        assert (yield top.bus.valid) == 1
        assert stalled == (2, 0x2220)

        # Source 1 has higher RR priority from the reset grant, but it arrives
        # after valid was already presented. Decoupled requires the selected
        # payload to remain unchanged until ready accepts it.
        yield top.inputs[1].bits.opcode.eq(tilelink.ChannelAOpcode.Get)
        yield top.inputs[1].bits.size.eq(3)
        yield top.inputs[1].bits.source.eq(1)
        yield top.inputs[1].bits.address.eq(0x1110)
        yield top.inputs[1].valid.eq(1)
        for _ in range(3):
            yield
            assert (yield top.bus.valid) == 1
            assert ((yield top.bus.bits.source),
                    (yield top.bus.bits.address)) == stalled

        yield top.bus.ready.eq(1)
        yield
        assert ((yield top.bus.bits.source),
                (yield top.bus.bits.address)) == stalled

    run_sim(top, proc)


def test_arbiter_round_robin_rotates():
    n = 3
    top = _arb_top(n, tilelink.ChannelA, A_KW)
    served = []
    procs = [offer_continuous(top.inputs[i], i, 14) for i in range(n)]
    procs.append(monitor_record(top.bus, served, 14, ["source"]))
    run_sim(top, *procs)

    steady = served[3:]
    assert len(steady) >= 2 * n, served
    # No source is granted twice in a row.
    for a, b in zip(steady, steady[1:]):
        assert a != b, steady
    # Every n-length window covers all sources (strict round-robin).
    for i in range(len(steady) - n + 1):
        assert set(steady[i:i + n]) == set(range(n)), steady


def test_arbiter_lowest_fixed_priority_monopolises_source_zero():
    n = 3
    top = _arb_top(n, tilelink.ChannelA, A_KW, policy="lowest")
    served = []
    procs = [offer_continuous(top.inputs[i], i, 14) for i in range(n)]
    procs.append(monitor_record(top.bus, served, 14, ["source"]))
    run_sim(top, *procs)
    assert len(served) > 0
    assert all(s == 0 for s in served), served


def test_arbiter_lowest_skips_idle_to_next_source():
    n = 3
    top = _arb_top(n, tilelink.ChannelA, A_KW, policy="lowest")
    served = []
    # Source 0 idle; sources 1 and 2 contend -> lowest (1) wins every cycle.
    procs = [offer_continuous(top.inputs[1], 1, 14),
             offer_continuous(top.inputs[2], 2, 14),
             monitor_record(top.bus, served, 14, ["source"])]
    run_sim(top, *procs)
    assert len(served) > 0
    assert all(s == 1 for s in served), served


def test_arbiter_burst_locking_holds_selection_across_beats():
    # Source 1 issues a 2-beat burst while source 0 requests continuously. The
    # two source-1 beats must be served back-to-back (the burst is not
    # interrupted) even though round-robin would otherwise rotate to source 0.
    n = 2
    top = _arb_top(n, tilelink.ChannelA, A_KW)
    served = []
    procs = [
        offer_continuous(top.inputs[0], 0, 28),
        # size=4, data_width=64 -> num_beats0 = 1 -> 2-beat burst.
        offer_burst(top.inputs[1], 1, 2, size=4, idle_after=20),
        monitor_record(top.bus, served, 28, ["source"]),
    ]
    run_sim(top, *procs)
    ones = [i for i, s in enumerate(served) if s == 1]
    assert len(ones) == 2, served
    assert ones[1] == ones[0] + 1, served  # contiguous -> burst uninterrupted


def test_arbiter_four_beat_lock_survives_backpressure():
    top = _arb_top(2, tilelink.ChannelA, A_KW)
    served = []

    def ready_fn(cycle):
        return cycle % 3 != 1

    run_sim(top,
            offer_continuous(top.inputs[0], 0, 60),
            offer_burst(top.inputs[1], 1, 4, size=5, idle_after=35),
            monitor_backpressure(top.bus, served, 60, ready_fn))

    ones = [i for i, source in enumerate(served) if source == 1]
    assert len(ones) == 4, served
    assert ones == list(range(ones[0], ones[0] + 4)), served


def test_arbiter_round_robin_fair_under_backpressure():
    n = 4
    top = _arb_top(n, tilelink.ChannelA, A_KW)
    served = []

    def ready_fn(c):
        return ((c * 7 + 3) >> 0) % 10 >= 4  # ~60% duty, deterministic

    procs = [offer_continuous(top.inputs[i], i, 200) for i in range(n)]
    procs.append(monitor_backpressure(top.bus, served, 200, ready_fn))
    run_sim(top, *procs)

    counts = [served.count(i) for i in range(n)]
    assert all(c > 0 for c in counts), counts  # no starvation
    assert max(counts) <= 2 * min(counts), counts  # roughly fair


# ---------------------------------------------------------------------------
# Per-channel instantiation (ChannelD, and ChannelE for the no-size branch).
# ---------------------------------------------------------------------------
def test_arbiter_channel_d_routes():
    d_kw = dict(data_width=64,
                size_width=4,
                source_id_width=4,
                sink_id_width=4)
    top = _arb_top(2, tilelink.ChannelD, d_kw)
    served = []

    def drive_d(inp, src, count):
        def proc():
            yield inp.bits.opcode.eq(tilelink.ChannelDOpcode.AccessAckData)
            yield inp.bits.size.eq(3)
            yield inp.bits.source.eq(src)
            yield inp.bits.sink.eq(src)
            yield inp.bits.denied.eq(0)
            yield inp.bits.corrupt.eq(0)
            yield inp.bits.data.eq(0)
            yield inp.valid.eq(1)
            fired = 0
            while fired < count:
                yield
                if (yield inp.ready):
                    fired += 1
            yield inp.valid.eq(0)
            for _ in range(5):
                yield

        return proc

    run_sim(top,
            drive_d(top.inputs[0], 5, 2),
            drive_d(top.inputs[1], 6, 2),
            monitor_record(top.bus, served, 14, ["source"]))
    # Both inputs served (round-robin), each twice.
    assert served.count(5) == 2 and served.count(6) == 2, served
    # Round-robin: no adjacent repeat.
    for a, b in zip(served, served[1:]):
        assert a != b, served


def test_arbiter_channel_e_no_size_branch():
    # ChannelE has no ``size`` field, so the arbiter takes the 1-bit
    # ``beats_left`` branch. Single-beat GrantAcks must still arbitrate.
    e_kw = dict(sink_id_width=4)
    top = _arb_top(2, tilelink.ChannelE, e_kw)
    served = []

    def drive_e(inp, sink_val, count):
        def proc():
            yield inp.bits.sink.eq(sink_val)
            yield inp.valid.eq(1)
            fired = 0
            while fired < count:
                yield
                if (yield inp.ready):
                    fired += 1
            yield inp.valid.eq(0)
            for _ in range(5):
                yield

        return proc

    run_sim(top,
            drive_e(top.inputs[0], 0xA, 2),
            drive_e(top.inputs[1], 0xB, 2),
            monitor_record(top.bus, served, 14, ["sink"]))
    assert served.count(0xA) == 2 and served.count(0xB) == 2, served
    for a, b in zip(served, served[1:]):
        assert a != b, served


@pytest.mark.parametrize("channel_cls,kw,opcode", [
    (tilelink.ChannelB,
     dict(addr_width=32, data_width=64, size_width=4, source_id_width=4),
     tilelink.ChannelBOpcode.Probe),
    (tilelink.ChannelC,
     dict(addr_width=32, data_width=64, size_width=4, source_id_width=4),
     tilelink.ChannelCOpcode.ReleaseData),
], ids=["channel-b", "channel-c"])
def test_arbiter_channels_b_and_c_route_payload(channel_cls, kw, opcode):
    top = _arb_top(2, channel_cls, kw)

    def proc():
        inp = top.inputs[1]
        yield inp.bits.opcode.eq(opcode)
        yield inp.bits.param.eq(3)
        yield inp.bits.size.eq(3)
        yield inp.bits.source.eq(9)
        yield inp.bits.address.eq(0x1234)
        yield inp.bits.data.eq(0xfeedfacecafebeef)
        yield inp.valid.eq(1)
        yield top.bus.ready.eq(1)
        yield

        assert (yield top.bus.valid) == 1
        assert (yield inp.ready) == 1
        assert (yield top.inputs[0].ready) == 0
        assert (yield top.bus.bits.opcode) == opcode.value
        assert (yield top.bus.bits.param) == 3
        assert (yield top.bus.bits.size) == 3
        assert (yield top.bus.bits.source) == 9
        assert (yield top.bus.bits.address) == 0x1234
        assert (yield top.bus.bits.data) == 0xfeedfacecafebeef

    run_sim(top, proc)
