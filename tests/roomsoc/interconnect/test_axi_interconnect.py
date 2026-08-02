"""Isolated unit tests for the AXI SoC fabric (Phase 2).

Covers the interconnect components that ``build.py`` / ``soc.py`` rely on today
with zero prior coverage:

  * ``AXIFragmenter``         -- burst splitting + R/B last reassembly
  * ``AXIInterconnectP2P``    -- 1:1 wiring smoke
  * ``AXIArbiter``            -- N->1 round-robin with per-burst locking
  * ``AXIDecoder``            -- 1->N address-decode routing, latched select
  * ``AXIIDWidthConverter``   -- widen / narrow (serialize + id restore)
  * ``AXIInterconnectShared`` -- full N-master x M-slave fabric

Behavioural notes verified here (and intentionally NOT tested because the
hardware does not implement them):
  * ``AXIDecoder`` *stalls* (ready=0) on an unmapped address -- it does not
    synthesise DECERR.
  * ``AXIInterconnectShared`` accepts ``timeout_cycles`` but never uses it;
    there is no timeout/DECERR path.
"""

import pytest

from amaranth import *
from amaranth.sim import Simulator
from amaranth.utils import log2_int
from amaranth_soc.memory import MemoryMap

from roomsoc.interconnect.axi import (AXIFragmenter, AXIInterconnectP2P,
                                      AXIInterconnectShared, AXIInterface)
from roomsoc.interconnect.axi.axi_full import (AXIArbiter, AXIDecoder,
                                               AXIIDWidthConverter)

from axi_helpers import (AXIResponder, run_sim, axi_read, axi_read_burst,
                         axi_write, axi_write_burst)


class _Region:
    """Minimal region with ``.origin`` for AXIInterconnectShared's slave list."""

    def __init__(self, origin):
        self.origin = origin


def _slave_responder(origin, size, *, data_width=32, id_width=4, prefix=0xA0):
    """An AXIResponder slave whose bus carries a MemoryMap window.

    The slave bus address width is the *window* address width (log2(size));
    the decoder passes the window-offset address to the slave (it does not
    translate addresses). The memory is initialised with ``prefix`` in the top
    byte so two slaves are distinguishable by the data they return.
    """
    aw = log2_int(size)
    depth = size // (data_width // 8)
    init = [(prefix << (data_width - 8)) | (i & (1 << (data_width - 8)) - 1)
            for i in range(depth)]
    slave = AXIResponder(addr_width=aw,
                         data_width=data_width,
                         depth=depth,
                         id_width=id_width,
                         init=init)
    slave.bus.memory_map = MemoryMap(data_width=8, addr_width=aw)
    return slave


# ===========================================================================
# AXIFragmenter
# ===========================================================================
class FragmenterTop(Elaboratable):

    def __init__(self, *, data_width=32, max_size=8, max_flights=2):
        self.data_width = data_width
        in_bus = AXIInterface(addr_width=32,
                              data_width=data_width,
                              id_width=4)
        self.frag = AXIFragmenter(in_bus,
                                  max_size=max_size,
                                  max_flights=max_flights)
        self.ram = AXIResponder(addr_width=32,
                                data_width=data_width,
                                depth=256,
                                id_width=4,
                                init=list(range(256)))

    @property
    def in_bus(self):
        return self.frag.in_bus

    def elaborate(self, platform):
        m = Module()
        m.submodules.frag = self.frag
        m.submodules.ram = self.ram
        m.d.comb += self.frag.out_bus.connect(self.ram.bus)
        return m


def test_axi_fragmenter_splits_incr_read_into_sub_bursts():
    # data_width=32 (4-byte beats), max_size=8 -> max_size1=1 -> 2-beat
    # sub-bursts. A 4-beat INCR read must produce two 2-beat AR sub-bursts and
    # the initiator must see R.last only on the 4th (reassembled) beat.
    top = FragmenterTop(data_width=32, max_size=8)
    ar_seen = []

    def driver():
        rd = yield from axi_read_burst(top.in_bus,
                                       0,
                                       size=2,
                                       length=3,
                                       txn_id=1)
        assert [b[0] for b in rd] == [0, 1, 2, 3]
        assert [b[2] for b in rd] == [0, 0, 0, 1]  # last only on beat 4

    def monitor():
        for _ in range(400):
            if (yield top.ram.ar_monitor.valid):
                ar_seen.append(((yield top.ram.ar_monitor.bits.addr),
                                (yield top.ram.ar_monitor.bits.len)))
            yield
        assert ar_seen == [(0, 1), (8, 1)]  # two 2-beat sub-bursts

    run_sim(top, driver, monitor)


def test_axi_fragmenter_passthrough_when_max_size_covers_burst():
    # max_size=16 -> max_size1=3 (4-beat sub-bursts); a 2-beat burst fits in a
    # single sub-burst, so no fragmentation and R.last lands on beat 2.
    top = FragmenterTop(data_width=32, max_size=16)

    def driver():
        rd = yield from axi_read_burst(top.in_bus, 0, size=2, length=1, txn_id=1)
        assert [b[0] for b in rd] == [0, 1]
        assert [b[2] for b in rd] == [0, 1]

    run_sim(top, driver)


def test_axi_fragmenter_fixed_burst_becomes_single_beat_sub_bursts():
    # FIXED bursts are "bad" -> each sub-burst is one beat; all beats address
    # the same word. R.last must still be reassembled on the final beat.
    top = FragmenterTop(data_width=32, max_size=8)
    top.ram.init[0x40 // 4] = 0xAB

    def driver():
        # Manually drive a FIXED burst (the kit's axi_read_burst hardcodes INCR).
        bus = top.in_bus
        yield bus.ar.bits.addr.eq(0x40)
        yield bus.ar.bits.size.eq(2)
        yield bus.ar.bits.len.eq(3)
        from roomsoc.interconnect.axi.common import AXIBurst
        yield bus.ar.bits.burst.eq(AXIBurst.FIXED)
        yield bus.ar.bits.id.eq(7)
        yield bus.ar.valid.eq(1)
        yield
        while not (yield bus.ar.ready):
            yield
        yield bus.ar.valid.eq(0)

        beats = []
        yield bus.r.ready.eq(1)
        for _ in range(4):
            while not (yield bus.r.valid):
                yield
            beats.append(((yield bus.r.bits.data), (yield bus.r.bits.last)))
            yield
        yield bus.r.ready.eq(0)
        assert [b[0] for b in beats] == [0xAB] * 4
        assert [b[1] for b in beats] == [0, 0, 0, 1]

    run_sim(top, driver)


def test_axi_fragmenter_write_split_then_readback():
    # A 4-beat write is fragmented into two 2-beat sub-bursts; the initiator
    # consumes 4 W beats (last on 4) and sees a single B. Reading the region
    # back through the same fragmenter must return the written data in order.
    top = FragmenterTop(data_width=32, max_size=8)
    aw_seen = []
    beats = [(0x11111111, 0xf), (0x22222222, 0xf), (0x33333333, 0xf),
             (0x44444444, 0xf)]

    def driver():
        resp, _id = yield from axi_write_burst(top.in_bus,
                                               0x40,
                                               beats,
                                               size=2,
                                               txn_id=2)
        assert resp == 0
        rd = yield from axi_read_burst(top.in_bus,
                                       0x40,
                                       size=2,
                                       length=3,
                                       txn_id=3)
        assert [b[0] for b in rd] == [b[0] for b in beats]

    def monitor():
        for _ in range(600):
            if (yield top.ram.aw_monitor.valid):
                aw_seen.append((yield top.ram.aw_monitor.bits.len))
            yield
        assert aw_seen == [1, 1]  # two 2-beat sub-bursts

    run_sim(top, driver, monitor)


@pytest.mark.filterwarnings("ignore::amaranth.hdl.ir.UnusedElaboratable")
def test_axi_fragmenter_rejects_max_size_below_bus_width():
    # max_size is in bytes; it must be >= data_width//8.
    import gc
    in_bus = AXIInterface(addr_width=32, data_width=64, id_width=4)
    with pytest.raises(ValueError, match="Max transfer size"):
        AXIFragmenter(in_bus, max_size=7)  # 7 < 64/8 = 8
    del in_bus
    gc.collect()


# ===========================================================================
# AXIInterconnectP2P
# ===========================================================================
def test_axi_interconnect_p2p_roundtrip():
    master = AXIInterface(addr_width=32, data_width=32, id_width=4)
    ram = AXIResponder(addr_width=32, data_width=32, depth=16, id_width=4)
    top = Module()
    top.submodules.p2p = AXIInterconnectP2P(master, ram.bus)
    top.submodules.ram = ram

    def driver():
        resp, _id = yield from axi_write(master, 4, 0xdeadbeef, 0xf, size=2)
        assert resp == 0
        data, _r, _l, _i = yield from axi_read(master, 4, size=2)
        assert data == 0xdeadbeef

    sim = Simulator(top)
    sim.add_clock(1e-6)
    sim.add_sync_process(driver)
    sim.run()


# ===========================================================================
# AXIArbiter
# ===========================================================================
class ArbiterTop(Elaboratable):

    def __init__(self, n_masters, *, data_width=32, id_width=4):
        self.n = n_masters
        self.masters = [
            AXIInterface(addr_width=32, data_width=data_width, id_width=id_width)
            for _ in range(n_masters)
        ]
        self.arbiter = AXIArbiter(addr_width=32,
                                  data_width=data_width,
                                  id_width=id_width)
        for m in self.masters:
            self.arbiter.add(m)
        self.ram = AXIResponder(addr_width=32,
                                data_width=data_width,
                                depth=256,
                                id_width=id_width,
                                init=list(range(256)))

    def elaborate(self, platform):
        m = Module()
        m.submodules.arbiter = self.arbiter
        m.submodules.ram = self.ram
        m.d.comb += self.arbiter.bus.connect(self.ram.bus)
        return m


def test_axi_arbiter_two_masters_round_robin():
    # Two masters each issue a single concurrent read at a different address.
    # Both must complete with their own data; the per-burst lock guarantees no
    # R-beat is misrouted between them.
    top = ArbiterTop(2)

    def m0():
        data, _r, _l, _i = yield from axi_read(top.masters[0], 0, size=2)
        assert data == 0

    def m1():
        data, _r, _l, _i = yield from axi_read(top.masters[1], 8, size=2)
        assert data == 2  # word 8/4 = 2

    run_sim(top, m0, m1)


def test_axi_arbiter_locks_through_multi_beat_burst():
    # Master 0 runs a 4-beat burst while master 1 concurrently requests a
    # single beat. The lock must hold the grant for the whole burst; both must
    # observe contiguous, correct data. If the lock broke, R beats would be
    # misrouted and at least one master would see wrong data.
    top = ArbiterTop(2)

    def m0():
        rd = yield from axi_read_burst(top.masters[0], 0, size=2, length=3)
        assert [b[0] for b in rd] == [0, 1, 2, 3]
        assert [b[2] for b in rd] == [0, 0, 0, 1]

    def m1():
        data, _r, _l, _i = yield from axi_read(top.masters[1], 0x40, size=2)
        assert data == 0x40 // 4  # word 16

    run_sim(top, m0, m1)


def test_axi_arbiter_three_masters_all_served():
    top = ArbiterTop(3)

    def mk(master, addr):
        def driver():
            data, _r, _l, _i = yield from axi_read(master, addr, size=2)
            assert data == addr // 4
        return driver

    run_sim(top,
            mk(top.masters[0], 0x00),
            mk(top.masters[1], 0x10),
            mk(top.masters[2], 0x20))


def test_axi_arbiter_locks_and_routes_concurrent_writes():
    # The write grant has an independent lock and response route from reads.
    # Run a burst and a single-beat write concurrently, then verify both BID
    # values and both memory regions through the same arbiter.
    top = ArbiterTop(2)
    burst = [(0x11111111, 0xf), (0x22222222, 0xf), (0x33333333, 0xf),
             (0x44444444, 0xf)]

    def m0():
        resp, bid = yield from axi_write_burst(top.masters[0],
                                               0,
                                               burst,
                                               size=2,
                                               txn_id=1)
        assert (resp, bid) == (0, 1)
        rd = yield from axi_read_burst(top.masters[0],
                                       0,
                                       size=2,
                                       length=3,
                                       txn_id=3)
        assert [beat[0] for beat in rd] == [beat[0] for beat in burst]

    def m1():
        resp, bid = yield from axi_write(top.masters[1],
                                         0x40,
                                         0xdeadbeef,
                                         0xf,
                                         size=2,
                                         txn_id=2)
        assert (resp, bid) == (0, 2)
        data, _resp, _last, rid = yield from axi_read(top.masters[1],
                                                      0x40,
                                                      size=2,
                                                      txn_id=4)
        assert (data, rid) == (0xdeadbeef, 4)

    run_sim(top, m0, m1)


# ===========================================================================
# AXIDecoder
# ===========================================================================
class DecoderTop(Elaboratable):

    def __init__(self, *, data_width=32, id_width=4, window=0x1000):
        self.data_width = data_width
        self.window = window
        self.s0 = _slave_responder(0x0000, window,
                                   data_width=data_width,
                                   id_width=id_width,
                                   prefix=0xA0)
        self.s1 = _slave_responder(window, window,
                                   data_width=data_width,
                                   id_width=id_width,
                                   prefix=0xB0)
        self.decoder = AXIDecoder(addr_width=32,
                                  data_width=data_width,
                                  id_width=id_width)
        self.decoder.add(self.s0.bus, addr=0x0000)
        self.decoder.add(self.s1.bus, addr=window)

    @property
    def bus(self):
        return self.decoder.bus

    def elaborate(self, platform):
        m = Module()
        m.submodules.decoder = self.decoder
        m.submodules.s0 = self.s0
        m.submodules.s1 = self.s1
        return m


def test_axi_decoder_routes_to_correct_slave():
    top = DecoderTop()
    s0_hits = []
    s1_hits = []

    def driver():
        # The decoder passes the window *offset* to each slave (no address
        # translation), so reading window 0 and window 1 both hit offset 0 in
        # their respective slaves. The data prefix tells them apart.
        d0, _r, _l, _i = yield from axi_read(top.bus, 0x0000, size=2)
        assert d0 == 0xA0000000
        d1, _r, _l, _i = yield from axi_read(top.bus, top.window + 4, size=2)
        assert d1 == 0xB0000001  # slave s1, offset 4 -> word 1

    def monitor():
        for _ in range(400):
            if (yield top.s0.ar_monitor.valid):
                s0_hits.append((yield top.s0.ar_monitor.bits.addr))
            if (yield top.s1.ar_monitor.valid):
                s1_hits.append((yield top.s1.ar_monitor.bits.addr))
            yield
        assert s0_hits == [0x0000]
        assert s1_hits == [0x004]  # offset within window 1

    run_sim(top, driver, monitor)


def test_axi_decoder_translates_aligned_window_boundaries():
    # AXIDecoder forwards only the low subordinate-address bits. For an
    # aperture-aligned window this is equivalent to subtracting its base.
    # Exercise both ends of each aperture and the write path so an off-by-one
    # decode or an accidental absolute address is visible.
    top = DecoderTop()
    s0_ar = []
    s1_ar = []
    s1_aw = []

    def driver():
        last = top.window - 4
        d0, _r, _l, _i = yield from axi_read(top.bus, last, size=2)
        assert d0 == 0xA00003ff

        d1, _r, _l, _i = yield from axi_read(top.bus,
                                              top.window,
                                              size=2)
        assert d1 == 0xB0000000

        upper = 2 * top.window - 4
        resp, _id = yield from axi_write(top.bus,
                                         upper,
                                         0xcafef00d,
                                         0xf,
                                         size=2,
                                         txn_id=3)
        assert resp == 0
        d1, _r, _l, _i = yield from axi_read(top.bus, upper, size=2)
        assert d1 == 0xcafef00d

    def monitor():
        for _ in range(500):
            if (yield top.s0.ar_monitor.valid):
                s0_ar.append((yield top.s0.ar_monitor.bits.addr))
            if (yield top.s1.ar_monitor.valid):
                s1_ar.append((yield top.s1.ar_monitor.bits.addr))
            if (yield top.s1.aw_monitor.valid):
                s1_aw.append((yield top.s1.aw_monitor.bits.addr))
            yield
        assert s0_ar == [0xffc]
        assert s1_ar == [0x000, 0xffc]
        assert s1_aw == [0xffc]

    run_sim(top, driver, monitor)


@pytest.mark.filterwarnings("ignore::amaranth.hdl.ir.UnusedElaboratable")
def test_axi_decoder_rejects_misaligned_window():
    # Address truncation cannot translate a window whose base has nonzero low
    # aperture bits. Reject it rather than silently decoding/forwarding the
    # wrong range.
    import gc
    decoder = AXIDecoder(addr_width=32, data_width=32, id_width=4)
    sub = AXIInterface(addr_width=12, data_width=32, id_width=4)
    sub.memory_map = MemoryMap(addr_width=12, data_width=8)

    with pytest.raises(ValueError, match="must be aligned"):
        decoder.add(sub, addr=0x0800)

    del decoder, sub
    gc.collect()


def test_axi_decoder_latches_select_across_burst():
    # A multi-beat read to slave s0 must route *every* AR beat (well, one AR
    # with multiple R beats) to s0 only; the latched select keeps s0 selected
    # for the whole burst, and s1 must see no traffic at all.
    top = DecoderTop()
    s1_hits = []

    def driver():
        rd = yield from axi_read_burst(top.bus, 0x0000, size=2, length=3)
        assert [b[0] for b in rd] == [
            0xA0000000, 0xA0000001, 0xA0000002, 0xA0000003
        ]

    def monitor():
        for _ in range(400):
            if (yield top.s1.ar_monitor.valid):
                s1_hits.append((yield top.s1.ar_monitor.bits.addr))
            yield
        assert s1_hits == []

    run_sim(top, driver, monitor)


def test_axi_decoder_unmapped_address_stalls():
    # The decoder does not synthesise DECERR: an access to an unmapped address
    # is simply never accepted (AR ready stays low), so the transaction stalls.
    # Verify that no slave sees the access and the AR remains pending.
    top = DecoderTop()
    fired = [False, False]
    ar_pending = [False]

    def driver():
        bus = top.bus
        yield bus.ar.bits.addr.eq(0x8000_0000)  # outside both windows
        yield bus.ar.bits.size.eq(2)
        yield bus.ar.bits.len.eq(0)
        yield bus.ar.valid.eq(1)
        for _ in range(20):
            yield
        ar_pending[0] = (yield bus.ar.valid) and not (yield bus.ar.ready)
        yield bus.ar.valid.eq(0)
        yield

    def monitor():
        for _ in range(40):
            if (yield top.s0.ar_monitor.valid):
                fired[0] = True
            if (yield top.s1.ar_monitor.valid):
                fired[1] = True
            yield
        assert not fired[0] and not fired[1]
        assert ar_pending[0]

    run_sim(top, driver, monitor)


# ===========================================================================
# AXIIDWidthConverter
# ===========================================================================
def test_axi_id_width_converter_widens():
    # in id_width=2 -> out id_width=4 (zero-extended). A read id must round-trip.
    in_bus = AXIInterface(addr_width=32, data_width=32, id_width=2)
    out_bus = AXIInterface(addr_width=32, data_width=32, id_width=4)
    ram = AXIResponder(addr_width=32, data_width=32, depth=16, id_width=4)
    top = Module()
    top.submodules.conv = AXIIDWidthConverter(in_bus, out_bus)
    top.submodules.ram = ram
    top.d.comb += out_bus.connect(ram.bus)

    def driver():
        data, _r, _l, rid = yield from axi_read(in_bus, 0, size=2, txn_id=2)
        assert data == 0
        assert rid == 2

    sim = Simulator(top)
    sim.add_clock(1e-6)
    sim.add_sync_process(driver)
    sim.run()


def test_axi_id_width_converter_narrows_and_restores_id():
    # in id_width=4 -> out id_width=2. Only the low 2 bits cross, but the
    # converter captures the full in ID and restores it on the response. An ID
    # whose high bits differ (0b0101) must come back intact.
    in_bus = AXIInterface(addr_width=32, data_width=32, id_width=4)
    out_bus = AXIInterface(addr_width=32, data_width=32, id_width=2)
    ram = AXIResponder(addr_width=32, data_width=32, depth=16, id_width=2)
    top = Module()
    top.submodules.conv = AXIIDWidthConverter(in_bus, out_bus)
    top.submodules.ram = ram
    top.d.comb += out_bus.connect(ram.bus)

    def driver():
        data, _r, _l, rid = yield from axi_read(in_bus, 0, size=2, txn_id=0b0101)
        assert data == 0
        assert rid == 0b0101  # full 4-bit id restored despite 2-bit wire

    sim = Simulator(top)
    sim.add_clock(1e-6)
    sim.add_sync_process(driver)
    sim.run()


def test_axi_id_width_converter_restores_narrowed_write_id():
    # The write side has its own saved full-width ID and request lock. Verify
    # that a BID with nonzero truncated high bits is restored independently of
    # the already-covered read path.
    in_bus = AXIInterface(addr_width=32, data_width=32, id_width=4)
    out_bus = AXIInterface(addr_width=32, data_width=32, id_width=2)
    ram = AXIResponder(addr_width=32, data_width=32, depth=16, id_width=2)
    top = Module()
    top.submodules.conv = AXIIDWidthConverter(in_bus, out_bus)
    top.submodules.ram = ram
    top.d.comb += out_bus.connect(ram.bus)

    def driver():
        resp, bid = yield from axi_write(in_bus,
                                         0,
                                         0xdecafbad,
                                         0xf,
                                         size=2,
                                         txn_id=0b1101)
        assert (resp, bid) == (0, 0b1101)

    sim = Simulator(top)
    sim.add_clock(1e-6)
    sim.add_sync_process(driver)
    sim.run()


def test_axi_id_width_converter_narrow_serializes_two_reads():
    # When narrowing, the _RequestCounter allows only one outstanding txn, so
    # the second AR is held until the first R.last. Both must still return their
    # own restored full ID.
    in_bus = AXIInterface(addr_width=32, data_width=32, id_width=4)
    out_bus = AXIInterface(addr_width=32, data_width=32, id_width=2)
    ram = AXIResponder(addr_width=32,
                       data_width=32,
                       depth=16,
                       id_width=2,
                       read_latency=3)
    top = Module()
    top.submodules.conv = AXIIDWidthConverter(in_bus, out_bus)
    top.submodules.ram = ram
    top.d.comb += out_bus.connect(ram.bus)

    out_ar_fires = []

    def driver():
        d0, _r, _l, rid0 = yield from axi_read(in_bus, 0, size=2, txn_id=0b0011)
        d1, _r, _l, rid1 = yield from axi_read(in_bus, 4, size=2, txn_id=0b1100)
        assert rid0 == 0b0011
        assert rid1 == 0b1100

    def monitor():
        for _ in range(400):
            if (yield out_bus.ar.valid) and (yield out_bus.ar.ready):
                out_ar_fires.append((yield out_bus.ar.bits.addr))
            yield
        # Both reads crossed the 2-bit wire; serialization means two out ARs.
        assert len(out_ar_fires) == 2

    sim = Simulator(top)
    sim.add_clock(1e-6)
    sim.add_sync_process(driver)
    sim.add_sync_process(monitor)
    sim.run()


# ===========================================================================
# AXIInterconnectShared
# ===========================================================================
class SharedTop(Elaboratable):

    def __init__(self, *, data_width=32, window=0x1000):
        self.window = window
        self.m0 = AXIInterface(addr_width=32,
                               data_width=data_width,
                               id_width=4)
        self.m1 = AXIInterface(addr_width=32,
                               data_width=data_width,
                               id_width=4)
        self.s0 = _slave_responder(0x0000, window,
                                   data_width=data_width,
                                   id_width=4,
                                   prefix=0xA0)
        self.s1 = _slave_responder(window, window,
                                   data_width=data_width,
                                   id_width=4,
                                   prefix=0xB0)
        self.ic = AXIInterconnectShared(
            addr_width=32,
            data_width=data_width,
            masters=[self.m0, self.m1],
            slaves=[(_Region(0x0000), self.s0.bus),
                    (_Region(window), self.s1.bus)])

    def elaborate(self, platform):
        m = Module()
        m.submodules.ic = self.ic
        m.submodules.s0 = self.s0
        m.submodules.s1 = self.s1
        return m


def test_axi_interconnect_shared_two_masters_two_slaves_concurrent():
    # m0 writes then reads slave s0 while m1 concurrently reads slave s1.
    # Routing + arbitration must deliver each master the correct slave's data.
    top = SharedTop()

    def m0():
        resp, _id = yield from axi_write(top.m0, 0, 0xCAFEBABE, 0xf, size=2)
        assert resp == 0
        data, _r, _l, _i = yield from axi_read(top.m0, 0, size=2)
        assert data == 0xCAFEBABE

    def m1():
        data, _r, _l, _i = yield from axi_read(top.m1, top.window, size=2)
        assert data == 0xB0000000  # slave s1, window offset 0 -> word 0

    run_sim(top, m0, m1)


class SharedMixedIdTop(Elaboratable):
    """A shared fabric whose slave has a *narrower* id_width than the masters.

    AXIInterconnectShared must auto-wrap it in an AXIIDWidthConverter.
    """

    def __init__(self, *, data_width=32, window=0x1000):
        self.window = window
        self.m0 = AXIInterface(addr_width=32,
                               data_width=data_width,
                               id_width=4)
        self.s0 = _slave_responder(0x0000, window,
                                   data_width=data_width,
                                   id_width=2,
                                   prefix=0xA0)
        self.ic = AXIInterconnectShared(addr_width=32,
                                        data_width=data_width,
                                        masters=[self.m0],
                                        slaves=[(_Region(0x0000),
                                                 self.s0.bus)])

    def elaborate(self, platform):
        m = Module()
        m.submodules.ic = self.ic
        m.submodules.s0 = self.s0
        return m


def test_axi_interconnect_shared_wraps_narrower_slave_id_width():
    # Master id_width=4, slave id_width=2: the fabric must insert an
    # AXIIDWidthConverter transparently and the transaction must complete with
    # the master's full id restored.
    top = SharedMixedIdTop()

    def driver():
        data, _r, _l, rid = yield from axi_read(top.m0, 0, size=2, txn_id=0b0101)
        assert data == 0xA0000000
        assert rid == 0b0101

    run_sim(top, driver)
