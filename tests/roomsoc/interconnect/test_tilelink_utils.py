"""Phase 3 + Phase 4 + Phase 5: TileLink Fragmenter, CacheCork, Serializer.

Phase 3 — ``Fragmenter`` (``tilelink.py:654``)
    Splits oversized transactions into ``min_size``-sized fragments, tagging the
    output source with ``Cat(fragnum, toggle, orig_source)``.  A single Get is
    repeated once per fragment by the internal ``Repeater``; multi-beat Puts are
    grouped into ``min_size``-sized bursts.  On the D side, non-data responses
    for all but the last fragment are silently dropped so the master sees exactly
    one AccessAck; data responses are all forwarded.  The toggle bit flips
    between consecutive transactions to disambiguate stale D beats.

Phase 4 — ``CacheCork`` (``tilelink.py:402``)
    The TL-C (BCE) <-> TL-UH (AD) shim used by both AXI bridges and
    ``TileLink2Wishbone``.  It synthesizes D-channel beats: AcquireBlock becomes
    a downstream Get whose AccessAckData returns as GrantData; AcquirePerm
    returns a Grant with no downstream traffic; ReleaseData becomes a downstream
    PutFull whose AccessAck returns as ReleaseAck; plain Get/Put pass through
    with a source-ID shift (low bit = write flag).

Phase 5 — ``Serializer`` (``tilelink.py:911``)
    Source-lock gate on the A channel via ``flight`` / ``flight_id``.  While a
    transaction from source X is in flight (A accepted, D not yet returned), a
    new transaction from a *different* source is stalled; the same source is
    allowed through.  B/C/E pass through unchanged in coherent mode.

tl_c_responder self-tests
    Direct validation of the coherent subordinate responder gadget in isolation
    from any cache. Acquire grants (capability reflection + block data), the
    BtoT no-data grant, ReleaseData writeback into the model, a bare Release,
    and a multi-beat grant. The responder is driven entirely from the master
    side by the kit's own ``tl_acquire`` / ``tl_release`` / ``tl_grantack``
    drivers against a ``TLRamModel`` -- the same gadgets the L2 tests use on the
    inner side.

All tests use the two-process Decoupled pattern with the pysim clock model from
AGENTS.md (only a naked ``yield`` advances the cycle; reads/writes between naked
yields are coherent within one cycle).
"""

import pytest

from amaranth import *
from amaranth.utils import log2_int

from roomsoc.interconnect import tilelink

from tl_helpers import (TLSRAM, TLRamModel, tl_get, tl_put, tl_acquire,
                        tl_release, tl_grantack, tl_c_responder, run_sim)


def _init(depth, base=0x100):
    return [base + i for i in range(depth)]


# ===========================================================================
# Phase 4: CacheCork
# ===========================================================================
class CacheCorkTop(Elaboratable):
    """Coherent master -> CacheCork(in_bus) -> out_bus -> TLSRAM."""

    def __init__(self, *, data_width=32, has_bce=True, denied_addr=None):
        self.data_width = data_width
        self.in_bus = tilelink.Interface(addr_width=32,
                                         data_width=data_width,
                                         size_width=4,
                                         source_id_width=4,
                                         sink_id_width=4,
                                         has_bce=has_bce)
        self.out_bus = tilelink.Interface(addr_width=32,
                                          data_width=data_width,
                                          size_width=4,
                                          source_id_width=5,
                                          has_bce=False)
        self.sram = TLSRAM(addr_width=32,
                           data_width=data_width,
                           size_width=4,
                           source_id_width=5,
                           depth=256,
                           init=_init(256),
                           denied_addr=denied_addr)
        self.cork = tilelink.CacheCork(self.in_bus, self.out_bus)

    def elaborate(self, platform):
        m = Module()
        m.submodules.cork = self.cork
        m.submodules.sram = self.sram
        m.d.comb += self.out_bus.connect(self.sram.bus)
        return m


# ---------------------------------------------------------------------------
# has_bce=False passthrough
# ---------------------------------------------------------------------------
def test_cachecork_has_bce_false_passthrough():
    top = CacheCorkTop(has_bce=False)
    lg = log2_int(top.data_width // 8)

    def driver():
        data, denied, corrupt = yield from tl_get(top.in_bus,
                                                   0,
                                                   size=lg,
                                                   source=3)
        assert (data, denied, corrupt) == (0x100, 0, 0)
        denied = yield from tl_put(top.in_bus,
                                   8,
                                   0xcafef00d,
                                   0xf,
                                   size=lg,
                                   source=7)
        assert denied == 0
        data, _, _ = yield from tl_get(top.in_bus, 8, size=lg, source=3)
        assert data == 0xcafef00d

    run_sim(top, driver)


# ---------------------------------------------------------------------------
# AcquireBlock / AcquirePerm
# ---------------------------------------------------------------------------
def test_cachecork_acquire_block_ntob_grant_data():
    top = CacheCorkTop()
    lg = log2_int(top.data_width // 8)
    a_caps = []

    def driver():
        d_op, d_param, d_source, d_sink, data, d_denied, d_corrupt = \
            yield from tl_acquire(top.in_bus,
                                  0,
                                  size=lg,
                                  source=3,
                                  grow_param=tilelink.GrowParam.NtoB)
        assert d_op == tilelink.ChannelDOpcode.GrantData.value
        assert d_param == tilelink.CapParam.toT.value
        assert d_source == 3
        assert data == 0x100
        assert (d_denied, d_corrupt) == (0, 0)

    def monitor():
        for _ in range(300):
            if (yield top.sram.a_monitor.valid):
                a_caps.append(((yield top.sram.a_monitor.bits.opcode),
                               (yield top.sram.a_monitor.bits.source)))
            yield
        assert a_caps == [(tilelink.ChannelAOpcode.Get.value, (3 << 1) | 1)]

    run_sim(top, driver, monitor)


def test_cachecork_acquire_block_ntot_grant_data():
    top = CacheCorkTop()
    lg = log2_int(top.data_width // 8)

    def driver():
        d_op, d_param, d_source, _sink, data, _, _ = \
            yield from tl_acquire(top.in_bus,
                                  4,
                                  size=lg,
                                  source=6,
                                  grow_param=tilelink.GrowParam.NtoT)
        assert d_op == tilelink.ChannelDOpcode.GrantData.value
        assert d_param == tilelink.CapParam.toT.value
        assert d_source == 6
        assert data == 0x101

    run_sim(top, driver)


def test_cachecork_acquire_perm_btot_grant_no_downstream():
    top = CacheCorkTop()
    lg = log2_int(top.data_width // 8)
    a_fired = []

    def driver():
        d_op, d_param, d_source, _sink, data, _, _ = \
            yield from tl_acquire(top.in_bus,
                                  0,
                                  size=lg,
                                  source=5,
                                  opcode=tilelink.ChannelAOpcode.AcquirePerm,
                                  grow_param=tilelink.GrowParam.BtoT)
        assert d_op == tilelink.ChannelDOpcode.Grant.value
        assert d_source == 5
        assert data == 0

    def monitor():
        for _ in range(300):
            if (yield top.sram.a_monitor.valid):
                a_fired.append(True)
            yield
        assert a_fired == []

    run_sim(top, driver, monitor)


def test_cachecork_acquire_block_btot_grant_no_downstream():
    top = CacheCorkTop()
    lg = log2_int(top.data_width // 8)

    def driver():
        d_op, _param, d_source, _sink, data, _, _ = \
            yield from tl_acquire(top.in_bus,
                                  0,
                                  size=lg,
                                  source=2,
                                  grow_param=tilelink.GrowParam.BtoT)
        assert d_op == tilelink.ChannelDOpcode.Grant.value
        assert d_source == 2
        assert data == 0

    run_sim(top, driver)


# ---------------------------------------------------------------------------
# Release / ReleaseData
# ---------------------------------------------------------------------------
def test_cachecork_release_data_becomes_putfull_and_releaseack():
    top = CacheCorkTop()
    lg = log2_int(top.data_width // 8)
    a_caps = []

    def driver():
        d_op, d_source, d_denied = yield from tl_release(
            top.in_bus,
            0,
            size=lg,
            source=4,
            data=0xDEADBEEF)
        assert d_op == tilelink.ChannelDOpcode.ReleaseAck.value
        assert d_source == 4
        assert d_denied == 0
        # The data should have been written downstream.
        data, _, _ = yield from tl_get(top.in_bus, 0, size=lg, source=0)
        assert data == 0xDEADBEEF

    def monitor():
        for _ in range(400):
            if (yield top.sram.a_monitor.valid):
                a_caps.append(((yield top.sram.a_monitor.bits.opcode),
                               (yield top.sram.a_monitor.bits.source)))
            yield
        assert a_caps[0] == (tilelink.ChannelAOpcode.PutFullData.value,
                             4 << 1)

    run_sim(top, driver, monitor)


def test_cachecork_release_emits_releaseack_no_downstream():
    top = CacheCorkTop()
    lg = log2_int(top.data_width // 8)
    a_fired = []

    def driver():
        d_op, d_source, d_denied = yield from tl_release(
            top.in_bus, 0, size=lg, source=7)
        assert d_op == tilelink.ChannelDOpcode.ReleaseAck.value
        assert d_source == 7

    def monitor():
        for _ in range(300):
            if (yield top.sram.a_monitor.valid):
                a_fired.append(True)
            yield
        assert a_fired == []

    run_sim(top, driver, monitor)


# ---------------------------------------------------------------------------
# GrantAck absorbed
# ---------------------------------------------------------------------------
def test_cachecork_grantack_absorbed():
    top = CacheCorkTop()
    lg = log2_int(top.data_width // 8)

    def driver():
        # AcquirePerm to get a Grant, then GrantAck.
        yield from tl_acquire(top.in_bus,
                              0,
                              size=lg,
                              source=1,
                              opcode=tilelink.ChannelAOpcode.AcquirePerm,
                              grow_param=tilelink.GrowParam.BtoT)
        # E channel must be immediately ready.
        assert (yield top.in_bus.e.ready) == 1
        yield from tl_grantack(top.in_bus, sink=0)

    run_sim(top, driver)


# ---------------------------------------------------------------------------
# Plain Get / Put pass-through with source-ID shift
# ---------------------------------------------------------------------------
def test_cachecork_get_passes_through_as_access_ack_data():
    top = CacheCorkTop()
    lg = log2_int(top.data_width // 8)
    a_caps = []

    def driver():
        data, denied, corrupt = yield from tl_get(top.in_bus,
                                                   0,
                                                   size=lg,
                                                   source=3)
        assert (data, denied, corrupt) == (0x100, 0, 0)

    def monitor():
        for _ in range(300):
            if (yield top.sram.a_monitor.valid):
                a_caps.append(((yield top.sram.a_monitor.bits.opcode),
                               (yield top.sram.a_monitor.bits.source)))
            yield
        # Get source = (3 << 1) | 0 = 6 (write flag = 0 for reads).
        assert a_caps == [(tilelink.ChannelAOpcode.Get.value, 6)]

    run_sim(top, driver, monitor)


def test_cachecork_put_passes_through_as_access_ack():
    top = CacheCorkTop()
    lg = log2_int(top.data_width // 8)
    a_caps = []

    def driver():
        denied = yield from tl_put(top.in_bus,
                                   0,
                                   0xcafef00d,
                                   0xf,
                                   size=lg,
                                   source=3)
        assert denied == 0

    def monitor():
        for _ in range(300):
            if (yield top.sram.a_monitor.valid):
                a_caps.append(((yield top.sram.a_monitor.bits.opcode),
                               (yield top.sram.a_monitor.bits.source)))
            yield
        # PutFull source = (3 << 1) | 1 = 7 (write flag = 1).
        assert a_caps == [(tilelink.ChannelAOpcode.PutFullData.value, 7)]

    run_sim(top, driver, monitor)


def test_cachecork_putpartial_preserves_opcode_mask_and_write_tag():
    top = CacheCorkTop()
    lg = log2_int(top.data_width // 8)
    a_caps = []

    def driver():
        denied = yield from tl_put(top.in_bus,
                                   0,
                                   0xAABBCCDD,
                                   0b0101,
                                   size=lg,
                                   source=4,
                                   full=False)
        assert denied == 0
        data, _, _ = yield from tl_get(top.in_bus, 0, size=lg, source=0)
        assert data == 0x00BB01DD

    def monitor():
        for _ in range(300):
            if (yield top.sram.a_monitor.valid):
                a_caps.append(((yield top.sram.a_monitor.bits.opcode),
                               (yield top.sram.a_monitor.bits.source)))
            yield
        assert a_caps[0] == (tilelink.ChannelAOpcode.PutPartialData.value,
                             (4 << 1) | 1)

    run_sim(top, driver, monitor)


def test_cachecork_acquire_propagates_denied_and_corrupt():
    top = CacheCorkTop(denied_addr=0)
    lg = log2_int(top.data_width // 8)

    def driver():
        d_op, d_param, d_source, _sink, _data, denied, corrupt = \
            yield from tl_acquire(top.in_bus,
                                  0,
                                  size=lg,
                                  source=3,
                                  grow_param=tilelink.GrowParam.NtoB)
        assert d_op == tilelink.ChannelDOpcode.GrantData.value
        assert d_param == tilelink.CapParam.toT.value
        assert d_source == 3
        assert (denied, corrupt) == (1, 1)

    run_sim(top, driver)


# ---------------------------------------------------------------------------
# B channel never driven
# ---------------------------------------------------------------------------
def test_cachecork_b_channel_never_valid():
    top = CacheCorkTop()
    lg = log2_int(top.data_width // 8)

    def driver():
        yield from tl_acquire(top.in_bus,
                              0,
                              size=lg,
                              source=1,
                              grow_param=tilelink.GrowParam.NtoB)
        for _ in range(10):
            assert (yield top.in_bus.b.valid) == 0
            yield

    run_sim(top, driver)


# ---------------------------------------------------------------------------
# Multi-beat acquire
# ---------------------------------------------------------------------------
def test_cachecork_multibeat_acquire_block():
    top = CacheCorkTop(data_width=32)
    lg = log2_int(32 // 8)

    def driver():
        # size = lg + 1 -> 2-beat GrantData.
        d_op, d_param, d_source, _sink, data, _, _ = \
            yield from tl_acquire(top.in_bus,
                                  0,
                                  size=lg + 1,
                                  source=2,
                                  grow_param=tilelink.GrowParam.NtoB)
        assert d_op == tilelink.ChannelDOpcode.GrantData.value
        assert d_source == 2
        assert data == 0x100 | (0x101 << 32)

    run_sim(top, driver)


# ---------------------------------------------------------------------------
# Constructor error
# ---------------------------------------------------------------------------
@pytest.mark.filterwarnings("ignore::amaranth.hdl.ir.UnusedElaboratable")
def test_cachecork_source_id_width_mismatch_raises():
    import gc
    in_bus = tilelink.Interface(addr_width=32,
                                data_width=32,
                                size_width=4,
                                source_id_width=4,
                                has_bce=True)
    out_bus = tilelink.Interface(addr_width=32,
                                 data_width=32,
                                 size_width=4,
                                 source_id_width=4,
                                 has_bce=False)
    with pytest.raises(ValueError, match="Subordinate bus has source ID"):
        tilelink.CacheCork(in_bus, out_bus)
    del in_bus, out_bus
    gc.collect()


# ===========================================================================
# Phase 5: Serializer
# ===========================================================================
class SerTop(Elaboratable):
    """Bare Serializer for timing tests (no downstream slave)."""

    def __init__(self, *, has_bce=False):
        self.in_bus = tilelink.Interface(addr_width=32,
                                         data_width=32,
                                         size_width=4,
                                         source_id_width=4,
                                         sink_id_width=4,
                                         has_bce=has_bce)
        self.ser = tilelink.Serializer(self.in_bus)
        self.out_bus = self.ser.out_bus

    def elaborate(self, platform):
        m = Module()
        m.submodules.ser = self.ser
        return m


class SerRamTop(Elaboratable):
    """Serializer + TLSRAM for functional round-trip tests."""

    def __init__(self):
        self.in_bus = tilelink.Interface(addr_width=32,
                                         data_width=32,
                                         size_width=4,
                                         source_id_width=4)
        self.ser = tilelink.Serializer(self.in_bus)
        self.sram = TLSRAM(addr_width=32,
                           data_width=32,
                           size_width=4,
                           source_id_width=4,
                           depth=256,
                           init=_init(256))

    def elaborate(self, platform):
        m = Module()
        m.submodules.ser = self.ser
        m.submodules.sram = self.sram
        m.d.comb += self.ser.out_bus.connect(self.sram.bus)
        return m


def _drive_get_a(bus, source, address):
    """Drive a single-beat Get on the A channel (no D collection)."""
    yield bus.a.bits.opcode.eq(tilelink.ChannelAOpcode.Get)
    yield bus.a.bits.param.eq(0)
    yield bus.a.bits.size.eq(2)
    yield bus.a.bits.source.eq(source)
    yield bus.a.bits.address.eq(address)
    yield bus.a.bits.mask.eq(0xf)
    yield bus.a.bits.data.eq(0)
    yield bus.a.bits.corrupt.eq(0)
    yield bus.a.valid.eq(1)


def _fire_get_a(bus, source, address):
    """Drive a Get, wait until accepted, deassert valid, advance one edge.

    After returning, ``flight`` is set by sync and the next A beat can be
    offered on the following cycle.
    """
    yield from _drive_get_a(bus, source, address)
    for _ in range(50):
        if (yield bus.a.ready):
            break
        yield
    else:
        raise AssertionError("timed out waiting for Serializer A ready")
    yield bus.a.valid.eq(0)
    yield  # edge: A fires -> flight <- 1


def _responder(ob, fired, *, delay=2):
    """Downstream slave: accept A, record source, respond on D after ``delay``."""
    yield ob.a.ready.eq(1)
    yield ob.d.valid.eq(0)
    yield
    for _ in range(300):
        if (yield ob.a.fire):
            src = (yield ob.a.bits.source)
            fired.append(src)
            for _ in range(delay):
                yield
            yield ob.d.bits.opcode.eq(tilelink.ChannelDOpcode.AccessAckData)
            yield ob.d.bits.size.eq(2)
            yield ob.d.bits.source.eq(src)
            yield ob.d.bits.denied.eq(0)
            yield ob.d.bits.corrupt.eq(0)
            yield ob.d.bits.data.eq(0x100 + src)
            yield ob.d.valid.eq(1)
            yield
            yield ob.d.valid.eq(0)
        yield


# ---------------------------------------------------------------------------
# Different-source serialization
# ---------------------------------------------------------------------------
def test_serializer_different_source_stalled_until_d():
    top = SerTop()
    fired = []

    def downstream():
        yield from _responder(top.out_bus, fired)

    def master():
        ib = top.in_bus
        yield ib.d.ready.eq(1)
        yield from _fire_get_a(ib, 0, 0)
        # Offer source 1 — must be stalled while source 0 is in flight.
        yield from _drive_get_a(ib, 1, 0x10)
        yield  # edge: source 1 applied, stall takes effect
        stall = 0
        for _ in range(50):
            if (yield ib.a.ready):
                break
            stall += 1
            yield
        assert stall > 0, "source 1 must stall while source 0 in flight"
        yield ib.a.valid.eq(0)
        for _ in range(40):
            yield

    run_sim(top, master, downstream)
    assert 0 in fired and 1 in fired, fired
    assert fired.index(0) < fired.index(1), fired


# ---------------------------------------------------------------------------
# Same-source not stalled
# ---------------------------------------------------------------------------
def test_serializer_same_source_not_stalled():
    top = SerTop()

    def downstream():
        yield from _responder(top.out_bus, [])

    def master():
        ib = top.in_bus
        ob = top.out_bus
        yield ib.d.ready.eq(1)
        yield from _fire_get_a(ib, 0, 0)
        # Offer another Get from source 0 while source 0 is in flight.
        yield from _drive_get_a(ib, 0, 0x10)
        yield  # edge: source applied, stall computed
        # Same source: stall = a_first & flight & (flight_id==a_id) = 0.
        assert (yield ib.a.ready) == 1, "same source must not stall"
        assert (yield ob.a.valid) == 1
        assert (yield ob.a.bits.source) == 0
        yield ib.a.valid.eq(0)
        for _ in range(20):
            yield

    run_sim(top, master, downstream)


# ---------------------------------------------------------------------------
# Functional round-trip
# ---------------------------------------------------------------------------
def test_serializer_multisource_roundtrip():
    top = SerRamTop()
    lg = log2_int(32 // 8)

    def driver():
        data, _, _ = yield from tl_get(top.in_bus, 0, size=lg, source=0)
        assert data == 0x100
        data, _, _ = yield from tl_get(top.in_bus, 4, size=lg, source=1)
        assert data == 0x101
        denied = yield from tl_put(top.in_bus,
                                   0x20,
                                   0xdeadbeef,
                                   0xf,
                                   size=lg,
                                   source=2)
        assert denied == 0
        data, _, _ = yield from tl_get(top.in_bus, 0x20, size=lg, source=3)
        assert data == 0xdeadbeef

    run_sim(top, driver)


def test_serializer_multibeat_roundtrip():
    top = SerRamTop()
    lg = log2_int(32 // 8)

    def driver():
        value = 0x2222222211111111
        yield from tl_put(top.in_bus, 0, value, 0xff, size=lg + 1, source=0)
        data, _, _ = yield from tl_get(top.in_bus,
                                       0,
                                       size=lg + 1,
                                       source=1)
        assert data == value

    run_sim(top, driver)


def test_serializer_does_not_stall_later_a_beats():
    top = SerTop()
    fired = []

    def downstream():
        ob = top.out_bus
        yield ob.a.ready.eq(1)
        yield
        for _ in range(30):
            if (yield ob.a.fire):
                fired.append(((yield ob.a.bits.source),
                              (yield ob.a.bits.data)))
            yield

    def master():
        ib = top.in_bus
        yield ib.a.bits.opcode.eq(tilelink.ChannelAOpcode.PutFullData)
        yield ib.a.bits.param.eq(0)
        yield ib.a.bits.size.eq(3)  # Two 32-bit A beats.
        yield ib.a.bits.source.eq(2)
        yield ib.a.bits.address.eq(0)
        yield ib.a.bits.mask.eq(0xf)
        yield ib.a.bits.corrupt.eq(0)
        yield ib.a.bits.data.eq(0x11111111)
        yield ib.a.valid.eq(1)
        yield
        while not (yield ib.a.ready):
            yield

        # ``flight`` is now set, but only a transaction's first A beat may be
        # stalled. The second beat must continue without waiting for D.
        yield ib.a.bits.data.eq(0x22222222)
        yield
        assert (yield ib.a.ready) == 1
        yield ib.a.valid.eq(0)

        # A new transaction from another source remains blocked because no D
        # response has cleared the prior transaction's flight state.
        yield from _drive_get_a(ib, 3, 0x10)
        yield
        assert (yield ib.a.ready) == 0
        assert (yield top.out_bus.a.valid) == 0
        yield ib.a.valid.eq(0)
        yield

    run_sim(top, master, downstream)
    assert fired == [(2, 0x11111111), (2, 0x22222222)]


def test_serializer_propagates_a_and_d_backpressure():
    top = SerTop()

    def proc():
        ib = top.in_bus
        ob = top.out_bus

        yield ob.a.ready.eq(0)
        yield from _drive_get_a(ib, 5, 0x40)
        yield
        assert (yield ob.a.valid) == 1
        assert (yield ob.a.bits.source) == 5
        assert (yield ib.a.ready) == 0

        yield ob.a.ready.eq(1)
        yield
        assert (yield ib.a.ready) == 1
        yield ib.a.valid.eq(0)

        yield ib.d.ready.eq(0)
        yield ob.d.bits.opcode.eq(tilelink.ChannelDOpcode.AccessAckData)
        yield ob.d.bits.size.eq(2)
        yield ob.d.bits.source.eq(5)
        yield ob.d.bits.data.eq(0xDEADBEEF)
        yield ob.d.valid.eq(1)
        yield
        assert (yield ib.d.valid) == 1
        assert (yield ib.d.bits.data) == 0xDEADBEEF
        assert (yield ob.d.ready) == 0

        yield ib.d.ready.eq(1)
        yield
        assert (yield ob.d.ready) == 1

    run_sim(top, proc)


# ---------------------------------------------------------------------------
# ReleaseAck does not clear flight (d_to_a filter)
# ---------------------------------------------------------------------------
def test_serializer_releaseack_does_not_clear_flight():
    top = SerTop()
    fired = []

    def downstream():
        ob = top.out_bus
        yield ob.a.ready.eq(1)
        yield ob.d.valid.eq(0)
        yield
        for _ in range(300):
            if (yield ob.a.fire):
                src = (yield ob.a.bits.source)
                fired.append(src)
                # Send ReleaseAck first — must NOT clear flight.
                for _ in range(2):
                    yield
                yield ob.d.bits.opcode.eq(tilelink.ChannelDOpcode.ReleaseAck)
                yield ob.d.bits.size.eq(2)
                yield ob.d.bits.source.eq(src)
                yield ob.d.valid.eq(1)
                yield
                yield ob.d.valid.eq(0)
                for _ in range(2):
                    yield
                # Then send AccessAckData — clears flight.
                yield ob.d.bits.opcode.eq(tilelink.ChannelDOpcode.AccessAckData)
                yield ob.d.bits.size.eq(2)
                yield ob.d.bits.source.eq(src)
                yield ob.d.bits.denied.eq(0)
                yield ob.d.bits.corrupt.eq(0)
                yield ob.d.bits.data.eq(0x100)
                yield ob.d.valid.eq(1)
                yield
                yield ob.d.valid.eq(0)
            yield

    def master():
        ib = top.in_bus
        yield ib.d.ready.eq(1)
        yield from _fire_get_a(ib, 0, 0)
        # Offer source 1 — stays stalled until AccessAckData arrives.
        yield from _drive_get_a(ib, 1, 0x10)
        yield  # edge: source 1 applied, stall takes effect
        stall = 0
        for _ in range(50):
            if (yield ib.a.ready):
                break
            stall += 1
            yield
        # Downstream waits 2 cycles, sends ReleaseAck (1 cycle), waits 2 more,
        # then sends AccessAckData — so source 1 is stalled well past the
        # ReleaseAck.
        assert stall > 4, "ReleaseAck must not clear flight"
        yield ib.a.valid.eq(0)
        for _ in range(30):
            yield

    run_sim(top, master, downstream)
    assert 0 in fired and 1 in fired, fired


# ---------------------------------------------------------------------------
# B/C/E passthrough in coherent mode
# ---------------------------------------------------------------------------
def test_serializer_bce_passthrough():
    top = SerTop(has_bce=True)

    def proc():
        ib = top.in_bus
        ob = top.out_bus

        # B channel: out -> in (Probe forwarded to manager).
        yield ob.b.bits.opcode.eq(tilelink.ChannelBOpcode.Probe)
        yield ob.b.bits.param.eq(tilelink.CapParam.toB.value)
        yield ob.b.bits.size.eq(2)
        yield ob.b.bits.source.eq(3)
        yield ob.b.bits.address.eq(0x100)
        yield ob.b.valid.eq(1)
        yield ib.b.ready.eq(1)
        yield

        assert (yield ib.b.valid) == 1
        assert (yield ib.b.bits.source) == 3
        assert (yield ib.b.bits.address) == 0x100
        assert (yield ob.b.ready) == 1

        # C channel: in -> out (ProbeAck forwarded to subordinate).
        yield ib.c.bits.opcode.eq(tilelink.ChannelCOpcode.ProbeAck)
        yield ib.c.bits.param.eq(tilelink.CapParam.toN.value)
        yield ib.c.bits.size.eq(2)
        yield ib.c.bits.source.eq(3)
        yield ib.c.bits.address.eq(0x100)
        yield ib.c.valid.eq(1)
        yield ob.c.ready.eq(1)
        yield

        assert (yield ob.c.valid) == 1
        assert (yield ob.c.bits.source) == 3
        assert (yield ib.c.ready) == 1

        # E channel: in -> out (GrantAck forwarded to subordinate).
        yield ib.e.bits.sink.eq(5)
        yield ib.e.valid.eq(1)
        yield ob.e.ready.eq(1)
        yield

        assert (yield ob.e.valid) == 1
        assert (yield ob.e.bits.sink) == 5
        assert (yield ib.e.ready) == 1

    run_sim(top, proc)


# ===========================================================================
# Phase 3: Fragmenter
# ===========================================================================
class FragTop(Elaboratable):
    """Bare Fragmenter (no downstream slave) for A-side structural tests."""

    def __init__(self,
                 *,
                 data_width=32,
                 max_size=16,
                 min_size=4,
                 size_width=4,
                 source_id_width=4,
                 has_bce=False):
        self.data_width = data_width
        self.max_size = max_size
        self.min_size = min_size
        self.in_bus = tilelink.Interface(addr_width=32,
                                         data_width=data_width,
                                         size_width=size_width,
                                         source_id_width=source_id_width,
                                         has_bce=has_bce)
        self.frag = tilelink.Fragmenter(max_size=max_size,
                                        min_size=min_size,
                                        in_bus=self.in_bus)
        self.out_bus = self.frag.out_bus
        self.frag_bits = log2_int(max_size // min_size)

    def elaborate(self, platform):
        m = Module()
        m.submodules.frag = self.frag
        return m


class FragRamTop(Elaboratable):
    """Fragmenter + TLSRAM for round-trip and D-side reassembly tests."""

    def __init__(self,
                 *,
                 data_width=32,
                 max_size=16,
                 min_size=4,
                 depth=256,
                 init=None,
                 denied_addr=None):
        self.data_width = data_width
        self.max_size = max_size
        self.min_size = min_size
        self.in_bus = tilelink.Interface(addr_width=32,
                                         data_width=data_width,
                                         size_width=4,
                                         source_id_width=4)
        self.frag = tilelink.Fragmenter(max_size=max_size,
                                        min_size=min_size,
                                        in_bus=self.in_bus)
        self.out_bus = self.frag.out_bus
        self.sram = TLSRAM(addr_width=32,
                           data_width=data_width,
                           size_width=4,
                           source_id_width=self.out_bus.source_id_width,
                           depth=depth,
                           init=init if init is not None else _init(depth),
                           denied_addr=denied_addr)

    def elaborate(self, platform):
        m = Module()
        m.submodules.frag = self.frag
        m.submodules.sram = self.sram
        m.d.comb += self.out_bus.connect(self.sram.bus)
        return m


def _frag_decode_source(src, frag_bits):
    """Split a Fragmenter output source into (fragnum, toggle, orig_source)."""
    fragnum = src & ((1 << frag_bits) - 1)
    toggle = (src >> frag_bits) & 1
    orig = src >> (frag_bits + 1)
    return fragnum, toggle, orig


def _frag_encode_source(fragnum, toggle, orig, frag_bits):
    """Build ``Cat(fragnum, toggle, orig_source)`` as an integer."""
    return fragnum | (toggle << frag_bits) | (orig << (frag_bits + 1))


def _expected_fragments(base, size, min_size):
    """Expected (fragnum, address) pairs for each emitted fragment, in order."""
    nbytes = 1 << size
    nfrags = nbytes // min_size
    return [(nfrags - 1 - i, base + i * min_size) for i in range(nfrags)]


def _frag_drive_get_a(bus, address, size, source):
    """Drive a single Get A beat and hold valid until accepted."""
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
    for _ in range(200):
        if (yield bus.a.ready):
            break
        yield
    else:
        raise AssertionError("timed out waiting for Fragmenter Get acceptance")
    yield bus.a.valid.eq(0)
    yield


def _frag_drive_put_a(bus, address, data, mask, size, source, full=True):
    """Drive PutFull/PutPartial A beats without collecting D."""
    beat_bytes = bus.data_width // 8
    nbeats = max(1, (1 << size) // beat_bytes)
    opcode = (tilelink.ChannelAOpcode.PutFullData
              if full else tilelink.ChannelAOpcode.PutPartialData)
    yield bus.a.bits.opcode.eq(opcode)
    yield bus.a.bits.param.eq(0)
    yield bus.a.bits.size.eq(size)
    yield bus.a.bits.source.eq(source)
    yield bus.a.bits.address.eq(address)
    yield bus.a.bits.corrupt.eq(0)
    yield bus.a.valid.eq(1)
    for i in range(nbeats):
        beat_data = (data >> (i * bus.data_width)) & ((1 << bus.data_width) -
                                                      1)
        if full:
            beat_mask = (1 << beat_bytes) - 1
        else:
            beat_mask = (mask >> (i * beat_bytes)) & ((1 << beat_bytes) - 1)
        yield bus.a.bits.data.eq(beat_data)
        yield bus.a.bits.mask.eq(beat_mask)
        yield
        for _ in range(200):
            if (yield bus.a.ready):
                break
            yield
        else:
            raise AssertionError(
                f"timed out waiting for Fragmenter Put beat {i}")
    yield bus.a.valid.eq(0)
    yield


def _frag_capture_a(out_bus, caps, *, cycles=500, ready_fn=None):
    """Drive out_bus.a.ready and record every fired A beat into ``caps``.

    ``ready_fn``, if given, is called with the cycle index and drives ``ready``
    to exercise backpressure on the fragmenter's output.
    """
    yield out_bus.a.ready.eq(1 if ready_fn is None else ready_fn(0))
    yield
    for c in range(cycles):
        if (yield out_bus.a.fire):
            caps.append({
                'opcode':
                (yield out_bus.a.bits.opcode),
                'size':
                (yield out_bus.a.bits.size),
                'address':
                (yield out_bus.a.bits.address),
                'source':
                (yield out_bus.a.bits.source),
                'data':
                (yield out_bus.a.bits.data),
                'mask':
                (yield out_bus.a.bits.mask),
            })
        yield out_bus.a.ready.eq(1 if ready_fn is None else ready_fn(c + 1))
        yield


# ---------------------------------------------------------------------------
# Passthrough when min_size == max_size
# ---------------------------------------------------------------------------
def test_fragmenter_passthrough_when_min_eq_max():
    top = FragRamTop(max_size=4, min_size=4)
    lg = log2_int(32 // 8)

    def driver():
        data, _, _ = yield from tl_get(top.in_bus, 0, size=lg, source=3)
        assert data == 0x100
        denied = yield from tl_put(top.in_bus,
                                   0,
                                   0xCAFEF00D,
                                   0xf,
                                   size=lg,
                                   source=5)
        assert denied == 0
        data, _, _ = yield from tl_get(top.in_bus, 0, size=lg, source=3)
        assert data == 0xCAFEF00D

    run_sim(top, driver)


# ---------------------------------------------------------------------------
# Get splits into min_size fragments
# ---------------------------------------------------------------------------
def test_fragmenter_get_splits_into_min_size_fragments():
    top = FragTop(max_size=16, min_size=4)
    caps = []

    def driver():
        yield from _frag_drive_get_a(top.in_bus, 0x10, size=4, source=5)

    def capture():
        yield from _frag_capture_a(top.out_bus, caps)

    run_sim(top, driver, capture)

    expected = _expected_fragments(0x10, 4, 4)
    assert len(caps) == len(expected), caps
    for cap, (fragnum, addr) in zip(caps, expected):
        assert cap['opcode'] == tilelink.ChannelAOpcode.Get.value
        assert cap['size'] == log2_int(4)
        fn, toggle, orig = _frag_decode_source(cap['source'], top.frag_bits)
        assert fn == fragnum
        assert orig == 5
        assert cap['address'] == addr
    toggles = {
        _frag_decode_source(c['source'], top.frag_bits)[1]
        for c in caps
    }
    assert toggles == {1}, toggles


# ---------------------------------------------------------------------------
# PutFull splits, data preserved per beat, full mask regenerated
# ---------------------------------------------------------------------------
def test_fragmenter_putfull_splits_and_preserves_data():
    top = FragTop(max_size=16, min_size=4)
    caps = []
    beats = [0x11111111, 0x22222222, 0x33333333, 0x44444444]
    value = sum(b << (i * 32) for i, b in enumerate(beats))

    def driver():
        yield from _frag_drive_put_a(top.in_bus,
                                     0x20,
                                     value,
                                     0xff,
                                     size=4,
                                     source=2,
                                     full=True)

    def capture():
        yield from _frag_capture_a(top.out_bus, caps)

    run_sim(top, driver, capture)

    expected = _expected_fragments(0x20, 4, 4)
    assert len(caps) == len(expected), caps
    for i, (cap, (fragnum, addr)) in enumerate(zip(caps, expected)):
        assert cap['opcode'] == tilelink.ChannelAOpcode.PutFullData.value
        assert cap['size'] == log2_int(4)
        assert cap['data'] == beats[i]
        assert cap['mask'] == 0xf
        fn, _, orig = _frag_decode_source(cap['source'], top.frag_bits)
        assert fn == fragnum
        assert orig == 2
        assert cap['address'] == addr


# ---------------------------------------------------------------------------
# PutPartial preserves per-beat mask
# ---------------------------------------------------------------------------
def test_fragmenter_putpartial_preserves_mask():
    top = FragTop(max_size=16, min_size=4)
    caps = []
    beat_masks = [0b0001, 0b0010, 0b0100, 0b1000]
    beat_datas = [0xAA, 0xBB, 0xCC, 0xDD]
    value = sum(b << (i * 32) for i, b in enumerate(beat_datas))
    mask = sum(m << (i * 4) for i, m in enumerate(beat_masks))

    def driver():
        yield from _frag_drive_put_a(top.in_bus,
                                     0,
                                     value,
                                     mask,
                                     size=4,
                                     source=6,
                                     full=False)

    def capture():
        yield from _frag_capture_a(top.out_bus, caps)

    run_sim(top, driver, capture)

    assert len(caps) == 4, caps
    for i, cap in enumerate(caps):
        assert cap['opcode'] == tilelink.ChannelAOpcode.PutPartialData.value
        assert cap['mask'] == beat_masks[i]
        assert cap['data'] == beat_datas[i]


# ---------------------------------------------------------------------------
# Multi-beat fragments (min_size > beat_bytes)
# ---------------------------------------------------------------------------
def test_fragmenter_multibeat_fragments():
    top = FragTop(max_size=16, min_size=8)
    caps = []

    def driver():
        yield from _frag_drive_get_a(top.in_bus, 0, size=4, source=1)

    def capture():
        yield from _frag_capture_a(top.out_bus, caps)

    run_sim(top, driver, capture)

    expected = _expected_fragments(0, 4, 8)
    assert len(caps) == len(expected), caps
    for cap, (fragnum, addr) in zip(caps, expected):
        assert cap['size'] == log2_int(8)
        fn, _, orig = _frag_decode_source(cap['source'], top.frag_bits)
        assert fn == fragnum
        assert orig == 1
        assert cap['address'] == addr


# ---------------------------------------------------------------------------
# Boundary: size == log2(min_size) -> single fragment, no split
# ---------------------------------------------------------------------------
def test_fragmenter_boundary_size_equals_min_no_split():
    top = FragTop(max_size=16, min_size=4)
    caps = []

    def driver():
        yield from _frag_drive_get_a(top.in_bus, 0x40, size=2, source=7)

    def capture():
        yield from _frag_capture_a(top.out_bus, caps)

    run_sim(top, driver, capture)

    assert len(caps) == 1, caps
    fn, toggle, orig = _frag_decode_source(caps[0]['source'], top.frag_bits)
    assert fn == 0
    assert orig == 7
    assert caps[0]['size'] == 2
    assert caps[0]['address'] == 0x40


# ---------------------------------------------------------------------------
# Toggle bit flips between consecutive transactions
# ---------------------------------------------------------------------------
def test_fragmenter_toggle_flips_between_transactions():
    top = FragRamTop(max_size=16, min_size=4)
    a_srcs = []

    def driver():
        yield from tl_get(top.in_bus, 0, size=2, source=0)
        yield from tl_get(top.in_bus, 0, size=2, source=0)

    def monitor():
        yield top.out_bus.a.ready.eq(1)
        yield
        for _ in range(500):
            if (yield top.out_bus.a.fire):
                a_srcs.append((yield top.out_bus.a.bits.source))
            yield

    run_sim(top, driver, monitor)

    frag_bits = log2_int(16 // 4)
    assert len(a_srcs) == 2, a_srcs
    t0 = _frag_decode_source(a_srcs[0], frag_bits)[1]
    t1 = _frag_decode_source(a_srcs[1], frag_bits)[1]
    assert t0 != t1, (t0, t1)


# ---------------------------------------------------------------------------
# Get round-trip against TLSRAM
# ---------------------------------------------------------------------------
def test_fragmenter_get_roundtrip_against_sram():
    top = FragRamTop(max_size=16, min_size=4)

    def driver():
        data, denied, corrupt = yield from tl_get(top.in_bus,
                                                  0,
                                                  size=4,
                                                  source=3)
        assert (denied, corrupt) == (0, 0)
        expected = (0x100 | (0x101 << 32) | (0x102 << 64) | (0x103 << 96))
        assert data == expected

    run_sim(top, driver)


@pytest.mark.parametrize("size", [3, 4])
def test_fragmenter_put_roundtrip_against_sram(size):
    top = FragRamTop(max_size=16, min_size=4)
    nbeats = (1 << size) // 4
    value = sum((0x10 + i) << (i * 32) for i in range(nbeats))

    def driver():
        denied = yield from tl_put(top.in_bus,
                                   0,
                                   value,
                                   (1 << (nbeats * 4)) - 1,
                                   size=size,
                                   source=2)
        assert denied == 0
        data, _, _ = yield from tl_get(top.in_bus, 0, size=size, source=4)
        assert data == value

    run_sim(top, driver)


# ---------------------------------------------------------------------------
# Intermediate AccessAck beats dropped; only last forwarded to master
# ---------------------------------------------------------------------------
def test_fragmenter_drops_intermediate_accessack():
    top = FragRamTop(max_size=16, min_size=4)
    lg = log2_int(32 // 8)
    d_out_opcodes = []

    def driver():
        denied = yield from tl_put(top.in_bus,
                                   0,
                                   0xDEADBEEF,
                                   0xf,
                                   size=lg + 2,
                                   source=1)
        assert denied == 0

    def monitor():
        yield
        for _ in range(500):
            if (yield top.out_bus.d.fire):
                d_out_opcodes.append((yield top.out_bus.d.bits.opcode))
            yield

    run_sim(top, driver, monitor)
    assert len(d_out_opcodes) == 4, d_out_opcodes
    assert all(
        op == tilelink.ChannelDOpcode.AccessAck.value for op in d_out_opcodes)


def test_fragmenter_reassembles_multibeat_d_with_backpressure():
    top = FragTop(max_size=16, min_size=8)
    frag_bits = top.frag_bits
    orig_source = 6
    beats = [
        (1, 0x11111111),
        (1, 0x22222222),
        (0, 0x33333333),
        (0, 0x44444444),
    ]
    received = []

    def downstream():
        ob = top.out_bus
        yield ob.d.valid.eq(0)
        yield
        for fragnum, data in beats:
            yield ob.d.bits.opcode.eq(
                tilelink.ChannelDOpcode.AccessAckData)
            yield ob.d.bits.param.eq(0)
            yield ob.d.bits.size.eq(3)  # Two 32-bit D beats per fragment.
            yield ob.d.bits.source.eq(
                _frag_encode_source(fragnum, 1, orig_source, frag_bits))
            yield ob.d.bits.sink.eq(0)
            yield ob.d.bits.denied.eq(0)
            yield ob.d.bits.corrupt.eq(0)
            yield ob.d.bits.data.eq(data)
            yield ob.d.valid.eq(1)
            yield
            for _ in range(100):
                if (yield ob.d.ready):
                    break
                yield
            else:
                raise AssertionError("timed out driving Fragmenter D beat")
        yield ob.d.valid.eq(0)
        yield

    def upstream():
        ib = top.in_bus
        stalled = None
        yield ib.d.ready.eq(0)
        yield
        for cycle in range(100):
            valid = (yield ib.d.valid)
            ready = (yield ib.d.ready)
            payload = ((yield ib.d.bits.opcode), (yield ib.d.bits.size),
                       (yield ib.d.bits.source), (yield ib.d.bits.data))

            if stalled is not None:
                assert valid, "D valid dropped under backpressure"
                assert payload == stalled, "D payload changed while stalled"
            if valid and ready:
                received.append(payload)
                if len(received) == len(beats):
                    break

            stalled = payload if valid and not ready else None
            yield ib.d.ready.eq((cycle % 3) != 0)
            yield
        else:
            raise AssertionError("timed out collecting reassembled D beats")

    run_sim(top, downstream, upstream)

    assert received == [
        (tilelink.ChannelDOpcode.AccessAckData.value, 4, orig_source, data)
        for _, data in beats
    ]


def test_fragmenter_consumes_dropped_accessacks_while_upstream_blocked():
    top = FragTop(max_size=16, min_size=4)
    frag_bits = top.frag_bits
    orig_source = 5
    downstream_fired = []
    upstream_fired = []
    upstream_stalled = []

    def downstream():
        ob = top.out_bus
        yield ob.d.valid.eq(0)
        yield
        for fragnum in (3, 2, 1, 0):
            yield ob.d.bits.opcode.eq(tilelink.ChannelDOpcode.AccessAck)
            yield ob.d.bits.param.eq(0)
            yield ob.d.bits.size.eq(2)
            yield ob.d.bits.source.eq(
                _frag_encode_source(fragnum, 1, orig_source, frag_bits))
            yield ob.d.bits.sink.eq(0)
            yield ob.d.bits.denied.eq(0)
            yield ob.d.bits.corrupt.eq(0)
            yield ob.d.bits.data.eq(0)
            yield ob.d.valid.eq(1)
            yield
            for _ in range(100):
                if (yield ob.d.ready):
                    downstream_fired.append(fragnum)
                    break
                yield
            else:
                raise AssertionError("timed out driving AccessAck fragment")
        yield ob.d.valid.eq(0)
        yield

    def upstream():
        ib = top.in_bus
        yield ib.d.ready.eq(0)
        yield
        for cycle in range(30):
            if (yield ib.d.valid):
                payload = ((yield ib.d.bits.opcode),
                           (yield ib.d.bits.size),
                           (yield ib.d.bits.source))
                if not (yield ib.d.ready):
                    upstream_stalled.append(payload)
                else:
                    upstream_fired.append(payload)
            # Hold the master blocked long enough for fragments 3, 2, and 1
            # to be consumed through the Fragmenter's internal drop path.
            if cycle == 8:
                yield ib.d.ready.eq(1)
            yield

    run_sim(top, downstream, upstream)

    expected = (tilelink.ChannelDOpcode.AccessAck.value, 4, orig_source)
    assert downstream_fired == [3, 2, 1, 0]
    assert upstream_stalled
    assert set(upstream_stalled) == {expected}
    assert upstream_fired == [expected]


# ---------------------------------------------------------------------------
# A-side backpressure tolerance
# ---------------------------------------------------------------------------
def test_fragmenter_a_side_backpressure_tolerance():
    top = FragTop(max_size=16, min_size=4)
    caps = []

    def ready_fn(c):
        return c % 3 != 0

    def driver():
        yield from _frag_drive_get_a(top.in_bus, 0, size=4, source=1)

    def capture():
        yield from _frag_capture_a(top.out_bus, caps, ready_fn=ready_fn)

    run_sim(top, driver, capture)

    expected = _expected_fragments(0, 4, 4)
    assert len(caps) == len(expected), caps
    assert [c['address'] for c in caps] == [a for _, a in expected]
    for cap, (fragnum, _) in zip(caps, expected):
        fn, _, orig = _frag_decode_source(cap['source'], top.frag_bits)
        assert fn == fragnum
        assert orig == 1


# ---------------------------------------------------------------------------
# has_bce: C/E ready tied high (absorbed)
# ---------------------------------------------------------------------------
def test_fragmenter_has_bce_absorbs_c_and_e():
    top = FragTop(max_size=16, min_size=4, has_bce=True)

    def proc():
        assert (yield top.in_bus.c.ready) == 1
        assert (yield top.in_bus.e.ready) == 1

    run_sim(top, proc)


# ===========================================================================
# tl_c_responder self-tests
# ===========================================================================
class _ResponderBusTop(Elaboratable):
    """A bare coherent interface anchored in a module so pysim tracks it."""

    def __init__(self):
        self.bus = tilelink.Interface(addr_width=32,
                                      data_width=64,
                                      size_width=4,
                                      source_id_width=2,
                                      sink_id_width=2,
                                      has_bce=True)

    def elaborate(self, platform):
        m = Module()
        # A dummy sync register so the simulation has a 'sync' domain to clock.
        _anchor = Signal()
        m.d.sync += _anchor.eq(0)
        return m


def _responder_rd(model, addr, nbytes):
    return int.from_bytes(model.get(addr, log2_int(nbytes)), "little")


def _new_responder_model(depth=256):
    return TLRamModel(data_width=64,
                      depth=depth,
                      init=[0xC000 + i for i in range(depth)])


def _run_responder(model, driver_fn):
    """Run ``driver_fn(top, model)`` against a responder-backed coherent bus.

    ``driver_fn`` is a generator function taking ``(top, model)``. A watchdog
    turns a responder deadlock into a hard failure instead of a hang.
    """
    top = _ResponderBusTop()
    done = [False]

    def responder():
        yield from tl_c_responder(top.bus, model=model, done=done)

    def watchdog(limit=200000):
        for _ in range(limit):
            if done[0]:
                return
            yield
        raise AssertionError("responder test deadlocked")

    def drv():
        yield from driver_fn(top, model)
        done[0] = True

    run_sim(top, responder, watchdog, drv)


# ---------------------------------------------------------------------------
# Acquire -> Grant / GrantData
# ---------------------------------------------------------------------------
def test_responder_acquire_ntob_grants_data_toB():
    model = _new_responder_model()

    def driver(top, model):
        d_op, d_param, d_src, d_sink, data, denied, corrupt = \
            yield from tl_acquire(top.bus,
                                  0x10,
                                  size=3,
                                  source=1,
                                  grow_param=tilelink.GrowParam.NtoB)
        assert d_op == tilelink.ChannelDOpcode.GrantData.value
        assert d_param == tilelink.CapParam.toB.value
        assert d_src == 1
        assert (denied, corrupt) == (0, 0)
        assert data == _responder_rd(model, 0x10, 8)
        yield from tl_grantack(top.bus, sink=d_sink)

    _run_responder(model, driver)


def test_responder_acquire_ntot_grants_data_toT():
    model = _new_responder_model()

    def driver(top, model):
        d_op, d_param, _src, d_sink, data, _, _ = \
            yield from tl_acquire(top.bus,
                                  0x20,
                                  size=3,
                                  source=2,
                                  grow_param=tilelink.GrowParam.NtoT)
        assert d_op == tilelink.ChannelDOpcode.GrantData.value
        assert d_param == tilelink.CapParam.toT.value
        assert data == _responder_rd(model, 0x20, 8)
        yield from tl_grantack(top.bus, sink=d_sink)

    _run_responder(model, driver)


def test_responder_acquire_btot_grants_no_data():
    model = _new_responder_model()

    def driver(top, model):
        d_op, d_param, _src, d_sink, data, _, _ = \
            yield from tl_acquire(top.bus,
                                  0x30,
                                  size=3,
                                  source=3,
                                  grow_param=tilelink.GrowParam.BtoT)
        assert d_op == tilelink.ChannelDOpcode.Grant.value
        assert d_param == tilelink.CapParam.toT.value
        assert data == 0, "BtoT upgrade must not carry data"
        yield from tl_grantack(top.bus, sink=d_sink)

    _run_responder(model, driver)


# ---------------------------------------------------------------------------
# Release / ReleaseData
# ---------------------------------------------------------------------------
def test_responder_release_data_writeback_and_ack():
    model = _new_responder_model()
    addr = 0x40
    value = 0x0BADF00DDEADBEEF

    def driver(top, model):
        d_op, d_src, d_denied = yield from tl_release(
            top.bus,
            addr,
            size=3,
            source=1,
            param=tilelink.ShrinkReportParam.TtoN,
            data=value)
        assert d_op == tilelink.ChannelDOpcode.ReleaseAck.value
        assert d_src == 1
        assert d_denied == 0
        assert _responder_rd(model, addr, 8) == value, \
            "ReleaseData must commit to model"

    _run_responder(model, driver)


def test_responder_release_emits_ack_without_write():
    model = _new_responder_model()
    before = _responder_rd(model, 0x50, 8)

    def driver(top, model):
        d_op, d_src, _ = yield from tl_release(top.bus,
                                               0x50,
                                               size=3,
                                               source=2)
        assert d_op == tilelink.ChannelDOpcode.ReleaseAck.value
        assert d_src == 2
        assert _responder_rd(model, 0x50, 8) == before, \
            "bare Release must not write"

    _run_responder(model, driver)


# ---------------------------------------------------------------------------
# Multi-beat grant
# ---------------------------------------------------------------------------
def test_responder_multibeat_grant_data():
    model = _new_responder_model()

    def driver(top, model):
        size = log2_int(16)  # 16-byte line -> two 8-byte beats
        d_op, _param, _src, d_sink, data, denied, corrupt = \
            yield from tl_acquire(top.bus,
                                  0x60,
                                  size=size,
                                  source=0,
                                  grow_param=tilelink.GrowParam.NtoB)
        assert d_op == tilelink.ChannelDOpcode.GrantData.value
        assert (denied, corrupt) == (0, 0)
        assert data == _responder_rd(model, 0x60, 16)
        yield from tl_grantack(top.bus, sink=d_sink)

    _run_responder(model, driver)
