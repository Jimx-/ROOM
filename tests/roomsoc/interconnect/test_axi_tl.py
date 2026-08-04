"""Isolated unit tests for the TileLink <-> AXI bridges (Phase 3).

Covers the two bridges in ``roomsoc/interconnect/axi/axi_full_to_tl.py`` that
previously had only script-only or zero coverage (see
``tests/roomsoc/interconnect/AXI_TEST_PLAN.md`` Phase 3):

  * ``TileLink2AXI``  -- TL manager -> AXI subordinate
      - TL Get/Put -> AXI AR / AW+W translation
      - mask -> strb, size -> len/size
      - ``max_flights`` outstanding (per-source-ID size queue depth limit)
      - ``burst_type`` reflected onto AR/AW
      - ``CacheCork`` adaptation when the TL bus ``has_bce``
  * ``AXI2Tilelink``  -- AXI manager -> TL subordinate
      - AR -> TL A Get, AW+W -> TL A PutPartialData
      - source-ID low-bit R/W disambiguation
      - ``max_flights`` count-based source tagging
      - TL ``denied``/``corrupt`` -> AXI SLVERR propagation
  * Round-trip TL->AXI->TL against a TL SRAM slave
  * Round-trip AXI->TL->AXI against an AXI SRAM slave

The AXI far-end uses the shared ``axi_helpers`` kit (``AXIResponder`` and the
master drivers); the TL far-end uses the shared ``tl_helpers`` kit
(``TLSRAM`` subordinate and ``tl_get``/``tl_put`` master drivers), whose
synthesizable RAM core is ``roomsoc.interconnect.tilelink.SRAM``.
"""

import pytest

from amaranth import *
from amaranth.hdl.rec import Direction
from amaranth.utils import log2_int

from roomsoc.interconnect import tilelink
from roomsoc.interconnect.axi import AXIInterface, AXI2Tilelink, TileLink2AXI
from roomsoc.interconnect.axi.common import AXIBurst, AXIResp

from axi_helpers import AXIResponder, run_sim, axi_read, axi_read_burst
from tl_helpers import TLSRAM, tl_get, tl_put, tl_acquire, tl_release


def axi_write_together(bus, addr, data, strb, *, size, txn_id=0):
    """Drive a single-beat AXI write with AW and W presented concurrently.

    ``AXI2Tilelink`` gates ``aw.ready`` on ``w.valid & w.last`` (it needs the
    full address+data to assemble a TL Put), so the sequential ``axi_write``
    helper -- which waits for ``aw.ready`` before raising ``w.valid`` -- would
    deadlock. Here AW and W (with WLAST) are asserted together; both fire in the
    same cycle, then B is collected. Returns ``(resp, id)``.
    """
    yield bus.aw.bits.addr.eq(addr)
    yield bus.aw.bits.size.eq(size)
    yield bus.aw.bits.len.eq(0)
    yield bus.aw.bits.burst.eq(AXIBurst.INCR)
    yield bus.aw.bits.id.eq(txn_id)
    yield bus.aw.valid.eq(1)
    yield bus.w.bits.data.eq(data)
    yield bus.w.bits.strb.eq(strb)
    yield bus.w.bits.last.eq(1)
    yield bus.w.valid.eq(1)
    yield
    while not (yield bus.aw.ready):
        yield
    # AW and W fire the same cycle (aw.ready implies w.ready here).
    yield bus.aw.valid.eq(0)
    yield bus.w.valid.eq(0)

    yield bus.b.ready.eq(1)
    while not (yield bus.b.valid):
        yield
    result = ((yield bus.b.bits.resp), (yield bus.b.bits.id))
    yield
    yield bus.b.ready.eq(0)
    return result


def _init(depth, base=0x100):
    """Predictable memory image: word i holds ``base + i``."""
    return [base + i for i in range(depth)]


# ===========================================================================
# TileLink2AXI  (TL manager -> AXI subordinate)
# ===========================================================================
class Tl2AxiTop(Elaboratable):

    def __init__(self,
                 *,
                 data_width=32,
                 source_id_width=4,
                 id_width=4,
                 max_flights=4,
                 burst_type='INCR',
                 has_bce=False,
                 r_resp=0,
                 b_resp=0):
        self.data_width = data_width
        self.tl = tilelink.Interface(addr_width=32,
                                     data_width=data_width,
                                     size_width=4,
                                     source_id_width=source_id_width,
                                     has_bce=has_bce)
        self.ram = AXIResponder(addr_width=32,
                                data_width=data_width,
                                depth=256,
                                id_width=id_width,
                                init=_init(256),
                                r_resp=r_resp,
                                b_resp=b_resp)
        self.bridge = TileLink2AXI(self.tl,
                                   self.ram.bus,
                                   max_flights=max_flights,
                                   burst_type=burst_type)

    def elaborate(self, platform):
        m = Module()
        m.submodules.bridge = self.bridge
        m.submodules.ram = self.ram
        return m


def test_tl2axi_get_translates_to_ar_and_round_trips():
    # A single-beat Get (size == lg) must become one AR (len=0, size=lg) whose
    # id carries the TL source, and the read data must round-trip to the D beat.
    top = Tl2AxiTop(source_id_width=4, id_width=4)
    lg = log2_int(top.data_width // 8)
    ar_caps = []

    def driver():
        data, denied, corrupt = yield from tl_get(top.tl, 0, size=lg, source=5)
        assert (data, denied, corrupt) == (0x100, 0, 0)

    def monitor():
        for _ in range(300):
            if (yield top.ram.ar_monitor.valid):
                ar_caps.append(
                    ((yield top.ram.ar_monitor.bits.addr),
                     (yield top.ram.ar_monitor.bits.len),
                     (yield top.ram.ar_monitor.bits.size),
                     (yield top.ram.bus.ar.bits.id)))
            yield
        assert ar_caps == [(0, 0, lg, 5)]

    run_sim(top, driver, monitor)


def test_tl2axi_put_translates_to_aw_w_and_mask_to_strb():
    # A single-beat PutFull must become one AW (len=0) + one W whose strb mirrors
    # the TL mask; the AXI B response must surface as a TL AccessAck.
    top = Tl2AxiTop(source_id_width=4, id_width=4)
    lg = log2_int(top.data_width // 8)
    aw_caps = []
    w_caps = []

    def driver():
        denied = yield from tl_put(top.tl,
                                   8,
                                   0xcafef00d,
                                   0xf,
                                   size=lg,
                                   source=2)
        assert denied == 0

    def monitor():
        for _ in range(300):
            if (yield top.ram.aw_monitor.valid):
                aw_caps.append(
                    ((yield top.ram.aw_monitor.bits.addr),
                     (yield top.ram.aw_monitor.bits.len),
                     (yield top.ram.bus.aw.bits.id)))
            if (yield top.ram.w_monitor.valid):
                w_caps.append(((yield top.ram.w_monitor.bits.data),
                               (yield top.ram.w_monitor.bits.strb)))
            yield
        assert aw_caps == [(8, 0, 2)]
        assert w_caps == [(0xcafef00d, 0xf)]

    run_sim(top, driver, monitor)


def test_tl2axi_put_partial_strb_fidelity():
    # PutPartial with a partial mask must reach the AXI W strb lane-for-lane so
    # only the addressed bytes are written.
    top = Tl2AxiTop(source_id_width=4, id_width=4, data_width=32)
    lg = log2_int(32 // 8)
    strb_caps = []

    def driver():
        # Pre-load word 0 with 0x11223344 via a full write, then overwrite the
        # upper half with 0xaabb0000 using a partial mask 0xc0... actually 0xc.
        yield from tl_put(top.tl, 0, 0x11223344, 0xf, size=lg, source=0)
        yield from tl_put(top.tl,
                          0,
                          0xaabb0000,
                          0xc,
                          size=lg,
                          source=0,
                          full=False)
        # Read back via a Get.
        data, _, _ = yield from tl_get(top.tl, 0, size=lg, source=0)
        assert data == 0xaabb3344

    def monitor():
        for _ in range(400):
            if (yield top.ram.w_monitor.valid):
                strb_caps.append((yield top.ram.w_monitor.bits.strb))
            yield
        assert strb_caps == [0xf, 0xc]

    run_sim(top, driver, monitor)


def test_tl2axi_size_maps_to_multibeat_len():
    # A Get whose size exceeds the bus width must expand to a multi-beat AXI
    # read: len = ((1 << size) >> lg) - 1 and size = lg. data_word[0..1] must
    # come back concatenated across the two D beats.
    top = Tl2AxiTop(data_width=32, source_id_width=4, id_width=4)
    lg = log2_int(32 // 8)
    ar_caps = []

    def driver():
        # size = lg + 1 -> 2-beat AXI read of words 0 and 1.
        data, denied, corrupt = yield from tl_get(top.tl,
                                                  0,
                                                  size=lg + 1,
                                                  source=1)
        assert denied == 0
        assert data == 0x100 | (0x101 << 32)

    def monitor():
        for _ in range(300):
            if (yield top.ram.ar_monitor.valid):
                ar_caps.append(
                    ((yield top.ram.ar_monitor.bits.addr),
                     (yield top.ram.ar_monitor.bits.len),
                     (yield top.ram.ar_monitor.bits.size)))
            yield
        assert ar_caps == [(0, 1, lg)]

    run_sim(top, driver, monitor)


def test_tl2axi_multibeat_put_maps_len_and_wlast():
    # A Put larger than one bus beat must emit one AXI AW describing the whole
    # burst and one W beat per TL A beat. AXIResponder's protocol checker
    # verifies WLAST placement; reading back checks ordering and persistence.
    top = Tl2AxiTop(data_width=32, source_id_width=4, id_width=4)
    lg = log2_int(32 // 8)
    aw_caps = []
    w_caps = []
    value = 0x2222222211111111

    def driver():
        denied = yield from tl_put(top.tl,
                                   0x20,
                                   value,
                                   0xff,
                                   size=lg + 1,
                                   source=2)
        assert denied == 0
        data, denied, corrupt = yield from tl_get(top.tl,
                                                  0x20,
                                                  size=lg + 1,
                                                  source=2)
        assert (data, denied, corrupt) == (value, 0, 0)
        assert (yield top.ram.protocol_error) == 0

    def monitor():
        for _ in range(400):
            if (yield top.ram.aw_monitor.valid):
                aw_caps.append(((yield top.ram.aw_monitor.bits.addr),
                                (yield top.ram.aw_monitor.bits.len),
                                (yield top.ram.aw_monitor.bits.size)))
            if (yield top.ram.w_monitor.valid):
                w_caps.append(((yield top.ram.w_monitor.bits.addr),
                               (yield top.ram.w_monitor.bits.data)))
            yield
        assert aw_caps == [(0x20, 1, lg)]
        assert w_caps == [(0x20, 0x11111111), (0x24, 0x22222222)]

    run_sim(top, driver, monitor)


@pytest.mark.parametrize("burst_type,code", [('FIXED', 0), ('INCR', 1),
                                             ('WRAP', 2)])
def test_tl2axi_burst_type_reflected_on_ar(burst_type, code):
    top = Tl2AxiTop(source_id_width=4, id_width=4, burst_type=burst_type)
    lg = log2_int(top.data_width // 8)
    burst_caps = []

    def driver():
        yield from tl_get(top.tl, 0, size=lg, source=1)

    def monitor():
        for _ in range(300):
            if (yield top.ram.ar_monitor.valid):
                burst_caps.append((yield top.ram.ar_monitor.bits.burst))
            yield
        assert burst_caps == [code]

    run_sim(top, driver, monitor)


def test_tl2axi_max_flights_limits_outstanding():
    # With max_flights=2 and the D channel held off, the per-source-ID size
    # queue must apply backpressure: not every offered read A beat is accepted,
    # and the number accepted stays on the order of max_flights (queue depth
    # plus a skid slot and one SyncFIFO register slot). Draining the responses
    # must then retire exactly the accepted beats with correct data.
    top = Tl2AxiMaxFlightsTop(max_flights=2)
    lg = log2_int(32 // 8)
    results = {}

    def driver():
        bus = top.tl
        beat_bytes = bus.data_width // 8
        # Hold D off so responses cannot retire and the size queue fills.
        yield bus.d.ready.eq(0)
        yield bus.a.bits.opcode.eq(tilelink.ChannelAOpcode.Get)
        yield bus.a.bits.param.eq(0)
        yield bus.a.bits.size.eq(lg)
        yield bus.a.bits.source.eq(0)
        yield bus.a.bits.address.eq(0)
        yield bus.a.bits.mask.eq((1 << beat_bytes) - 1)
        yield bus.a.bits.data.eq(0)
        yield bus.a.bits.corrupt.eq(0)
        yield bus.a.valid.eq(1)

        offered = 8
        accepted = 0
        last_ready_high = -1
        for cycle in range(offered + 20):
            yield
            if (yield bus.a.ready):
                accepted += 1
                last_ready_high = cycle
        yield bus.a.valid.eq(0)

        # Backpressure engaged: far fewer than offered were taken, and a.ready
        # stopped rising well before the end (the queue filled).
        assert accepted >= top.max_flights
        assert accepted <= top.max_flights + 2  # queue + skid + FIFO register
        assert last_ready_high < offered + 20 - 2

        # Drain exactly the accepted beats; each read word 0 -> 0x100.
        yield bus.d.ready.eq(1)
        got = []
        for _ in range(accepted):
            while not (yield bus.d.valid):
                yield
            got.append((yield bus.d.bits.data))
            yield
        yield bus.d.ready.eq(0)
        results['accepted'] = accepted
        results['data'] = got

    run_sim(top, driver)
    accepted = results['accepted']
    assert results['data'] == [0x100] * accepted


class Tl2AxiMaxFlightsTop(Elaboratable):

    def __init__(self, *, max_flights=2):
        self.max_flights = max_flights
        self.tl = tilelink.Interface(addr_width=32,
                                     data_width=32,
                                     size_width=4,
                                     source_id_width=4)
        self.ram = AXIResponder(addr_width=32,
                                data_width=32,
                                depth=256,
                                id_width=4,
                                read_latency=2,
                                init=_init(256))
        self.bridge = TileLink2AXI(self.tl,
                                   self.ram.bus,
                                   max_flights=max_flights)

    def elaborate(self, platform):
        m = Module()
        m.submodules.bridge = self.bridge
        m.submodules.ram = self.ram
        return m


def test_tl2axi_cache_cork_with_has_bce():
    # With a BCE TL bus the bridge inserts a CacheCork that shifts the source
    # by one bit (low bit = write flag). A Get (source=1) must surface on the
    # AXI side with id = (1 << 1) | 0 = 2, and data must round-trip.
    top = Tl2AxiTop(source_id_width=3, id_width=4, has_bce=True)
    lg = log2_int(top.data_width // 8)
    ar_ids = []

    def driver():
        data, denied, corrupt = yield from tl_get(top.tl, 0, size=lg, source=1)
        assert (data, denied, corrupt) == (0x100, 0, 0)

    def monitor():
        for _ in range(300):
            if (yield top.ram.ar_monitor.valid):
                ar_ids.append((yield top.ram.bus.ar.bits.id))
            yield
        assert ar_ids == [2]

    run_sim(top, driver, monitor)


def test_tl2axi_propagates_axi_errors_to_tilelink():
    # AXI read errors become corrupt AccessAckData responses; AXI write errors
    # become denied AccessAck responses.
    top = Tl2AxiTop(source_id_width=4,
                    id_width=4,
                    r_resp=AXIResp.SLVERR,
                    b_resp=AXIResp.SLVERR)
    lg = log2_int(top.data_width // 8)

    def driver():
        _data, denied, corrupt = yield from tl_get(top.tl,
                                                   0,
                                                   size=lg,
                                                   source=1)
        assert (denied, corrupt) == (0, 1)
        denied = yield from tl_put(top.tl,
                                   4,
                                   0xdeadbeef,
                                   0xf,
                                   size=lg,
                                   source=2)
        assert denied == 1

    run_sim(top, driver)


# ===========================================================================
# AXI2Tilelink  (AXI manager -> TL subordinate)
# ===========================================================================
class Axi2TlTop(Elaboratable):

    def __init__(self,
                 *,
                 data_width=32,
                 id_width=4,
                 max_flights=1,
                 denied_addr=None):
        self.max_flights = max_flights
        self.data_width = data_width
        self.axi = AXIInterface(addr_width=32,
                                data_width=data_width,
                                id_width=id_width)
        self.tl = AXI2Tilelink.get_adapted_interface(self.axi,
                                                     max_flights=max_flights)
        self.sram = TLSRAM(addr_width=32,
                           data_width=data_width,
                           size_width=4,
                           source_id_width=self.tl.source_id_width,
                           depth=256,
                           init=_init(256),
                           denied_addr=denied_addr)

    def elaborate(self, platform):
        m = Module()
        m.submodules.bridge = AXI2Tilelink(self.axi,
                                           self.tl,
                                           max_flights=self.max_flights)
        m.submodules.sram = self.sram
        m.d.comb += self.tl.connect(self.sram.bus)
        return m


@pytest.mark.parametrize("max_flights,count_bits", [(1, 0), (2, 1), (3, 2),
                                                       (4, 2), (5, 3)])
def test_axi2tl_adapted_source_width(max_flights, count_bits):
    # One low bit distinguishes reads from writes. The remaining added bits
    # enumerate outstanding transactions, rounded up for non-powers of two.
    axi = AXIInterface(addr_width=32, data_width=32, id_width=4)
    tl = AXI2Tilelink.get_adapted_interface(axi, max_flights=max_flights)
    assert tl.source_id_width == 4 + count_bits + 1


@pytest.mark.parametrize("bridge", ["axi2tl", "tl2axi"])
@pytest.mark.filterwarnings("ignore::amaranth.hdl.ir.UnusedElaboratable")
def test_axi_tl_bridges_reject_nonpositive_max_flights(bridge):
    import gc
    axi = AXIInterface(addr_width=32, data_width=32, id_width=4)
    tl = tilelink.Interface(addr_width=32,
                            data_width=32,
                            size_width=4,
                            source_id_width=5)
    with pytest.raises(ValueError, match="positive integer"):
        if bridge == "axi2tl":
            AXI2Tilelink(axi, tl, max_flights=0)
        else:
            TileLink2AXI(tl, axi, max_flights=0)
    del axi, tl
    gc.collect()


def test_axi2tl_read_translates_to_get_and_round_trips():
    # An AXI single-beat read must become a TL Get; the AccessAckData payload
    # must come back on the AXI R channel with the original id.
    top = Axi2TlTop(id_width=4, max_flights=1)

    def driver():
        data, resp, _last, rid = yield from axi_read(top.axi,
                                                     0,
                                                     size=2,
                                                     txn_id=3)
        assert (data, resp, rid) == (0x100, AXIResp.OKAY, 3)

    run_sim(top, driver)


def test_axi2tl_write_translates_to_put_and_strb_to_mask():
    # An AXI single-beat write must become a TL PutPartialData whose mask
    # mirrors the W strb; the TL AccessAck must surface as a B response.
    top = Axi2TlTop(id_width=4, max_flights=1)
    mask_caps = []

    def driver():
        resp, _id = yield from axi_write_together(top.axi,
                                         0,
                                         0xdeadbeef,
                                         0xf,
                                         size=2,
                                         txn_id=2)
        assert resp == AXIResp.OKAY

    def monitor():
        for _ in range(300):
            if (yield top.sram.a_monitor.valid):
                mask = (yield top.sram.bus.a.bits.mask)
                op = (yield top.sram.a_monitor.bits.opcode)
                mask_caps.append((op, mask))
            yield
        assert mask_caps == [(tilelink.ChannelAOpcode.PutPartialData.value, 0xf)]

    run_sim(top, driver, monitor)


def test_axi2tl_source_id_rw_disambiguation():
    # The bridge tags reads with source low bit 0 and writes with low bit 1 so
    # the TL D arbiter can route responses. A read of id=3 -> source 6, a write
    # of id=3 -> source 7.
    top = Axi2TlTop(id_width=4, max_flights=1)
    caps = []

    def driver():
        yield from axi_read(top.axi, 0, size=2, txn_id=3)
        yield from axi_write_together(top.axi, 4, 0xaa, 0xf, size=2, txn_id=3)

    def monitor():
        for _ in range(400):
            if (yield top.sram.a_monitor.valid):
                caps.append(((yield top.sram.a_monitor.bits.opcode),
                             (yield top.sram.a_monitor.bits.source)))
            yield
        assert caps == [(tilelink.ChannelAOpcode.Get.value, 6),
                        (tilelink.ChannelAOpcode.PutPartialData.value, 7)]

    run_sim(top, driver, monitor)


def test_axi2tl_max_flights_count_source_tagging():
    # With max_flights=4 (log_flights=2, added_bits=3) each outstanding read of
    # the same id gets a distinct 2-bit count baked into the TL source. Five
    # sequential reads of id=1 must reach the slave with sources
    # (1<<3)|0, (1<<3)|(1<<1), (1<<3)|(2<<1), (1<<3)|(3<<1), (1<<3)|0 -- i.e.
    # 8, 10, 12, 14, 8 -- and every AXI R response must recover id=1.
    top = Axi2TlTop(id_width=4, max_flights=4)
    src_caps = []
    r_ids = []

    def driver():
        for i in range(5):
            data, resp, _last, rid = yield from axi_read(top.axi,
                                                         i * 4,
                                                         size=2,
                                                         txn_id=1)
            assert (data, resp) == (0x100 + i, AXIResp.OKAY)
            r_ids.append(rid)

    def monitor():
        for _ in range(600):
            if (yield top.sram.a_monitor.valid):
                src_caps.append((yield top.sram.a_monitor.bits.source))
            yield

    run_sim(top, driver, monitor)
    assert src_caps == [8, 10, 12, 14, 8]
    assert r_ids == [1, 1, 1, 1, 1]


def test_axi2tl_max_flights_write_source_tagging():
    # Exercise the independent write counter and the B-channel ordering gate.
    # For id=1, added_bits=3 and the write discriminator is bit 0, giving
    # sources 9, 11, 13, 15 before the two-bit transaction count wraps.
    top = Axi2TlTop(id_width=4, max_flights=4)
    src_caps = []
    b_ids = []

    def driver():
        for i in range(5):
            resp, bid = yield from axi_write_together(top.axi,
                                                       i * 4,
                                                       0xA000 + i,
                                                       0xf,
                                                       size=2,
                                                       txn_id=1)
            assert resp == AXIResp.OKAY
            b_ids.append(bid)

    def monitor():
        for _ in range(600):
            if (yield top.sram.a_monitor.valid):
                assert ((yield top.sram.a_monitor.bits.opcode) ==
                        tilelink.ChannelAOpcode.PutPartialData.value)
                src_caps.append((yield top.sram.a_monitor.bits.source))
            yield

    run_sim(top, driver, monitor)
    assert src_caps == [9, 11, 13, 15, 9]
    assert b_ids == [1, 1, 1, 1, 1]


def test_axi2tl_propagates_slverr_from_denied():
    # A TL ``denied`` response must become AXI SLVERR on both R (read) and B
    # (write); a subsequent normal access must still return OKAY.
    top = Axi2TlTop(id_width=4,
                           max_flights=1,
                           denied_addr=0x10)

    def driver():
        _d, resp, _l, _i = yield from axi_read(top.axi, 0x10, size=2, txn_id=1)
        assert resp == AXIResp.SLVERR
        resp, _i = yield from axi_write_together(top.axi,
                                        0x10,
                                        0x1,
                                        0xf,
                                        size=2,
                                        txn_id=2)
        assert resp == AXIResp.SLVERR
        data, resp, _l, _i = yield from axi_read(top.axi, 0, size=2, txn_id=3)
        assert (data, resp) == (0x100, AXIResp.OKAY)

    run_sim(top, driver)


# ===========================================================================
# Round-trip TL -> AXI -> TL against a TL SRAM slave
# ===========================================================================
class TlAxiTlTop(Elaboratable):
    """tl_master --(TL)--> TileLink2AXI --(AXI)--> AXI2Tilelink --(TL)--> TLSRAM."""

    def __init__(self):
        self.tl_master = tilelink.Interface(addr_width=32,
                                            data_width=32,
                                            size_width=4,
                                            source_id_width=4)
        self.axi_mid = AXIInterface(addr_width=32, data_width=32, id_width=4)
        self.tl_far = AXI2Tilelink.get_adapted_interface(self.axi_mid,
                                                         max_flights=1)
        self.sram = TLSRAM(addr_width=32,
                           data_width=32,
                           size_width=4,
                           source_id_width=self.tl_far.source_id_width,
                           depth=256,
                           init=[0] * 256)

    def elaborate(self, platform):
        m = Module()
        m.submodules.tl2axi = TileLink2AXI(self.tl_master,
                                           self.axi_mid,
                                           max_flights=4)
        m.submodules.axi2tl = AXI2Tilelink(self.axi_mid,
                                           self.tl_far,
                                           max_flights=1)
        m.submodules.sram = self.sram
        m.d.comb += self.tl_far.connect(self.sram.bus)
        return m


def test_roundtrip_tl_axi_tl():
    # A write through both bridges must persist in the far TL SRAM and read back
    # unchanged, proving the source-ID round trip (<<1 / >>1) and data path are
    # mutually consistent.
    top = TlAxiTlTop()

    def driver():
        denied = yield from tl_put(top.tl_master,
                                   0,
                                   0xdeadbeef,
                                   0xf,
                                   size=2,
                                   source=3)
        assert denied == 0
        data, denied, corrupt = yield from tl_get(top.tl_master,
                                                  0,
                                                  size=2,
                                                  source=3)
        assert (data, denied, corrupt) == (0xdeadbeef, 0, 0)

    run_sim(top, driver)


# ===========================================================================
# Round-trip AXI -> TL -> AXI against an AXI SRAM slave
# ===========================================================================
class AxiTlAxiTop(Elaboratable):
    """axi_master --(AXI)--> AXI2Tilelink --(TL)--> TileLink2AXI --(AXI)--> AXIResponder."""

    def __init__(self):
        self.axi_master = AXIInterface(addr_width=32,
                                       data_width=32,
                                       id_width=4)
        self.tl_mid = tilelink.Interface(addr_width=32,
                                         data_width=32,
                                         size_width=4,
                                         source_id_width=4 + 1)
        self.axi_far = AXIInterface(addr_width=32, data_width=32, id_width=5)
        self.ram = AXIResponder(addr_width=32,
                                data_width=32,
                                depth=256,
                                id_width=5,
                                init=[0] * 256)

    def elaborate(self, platform):
        m = Module()
        m.submodules.axi2tl = AXI2Tilelink(self.axi_master,
                                           self.tl_mid,
                                           max_flights=1)
        m.submodules.tl2axi = TileLink2AXI(self.tl_mid,
                                           self.axi_far,
                                           max_flights=4)
        m.submodules.ram = self.ram
        m.d.comb += self.axi_far.connect(self.ram.bus)
        return m


def test_roundtrip_axi_tl_axi():
    # A write through both bridges must persist in the far AXI SRAM and read
    # back unchanged; the AXI id must survive the TL source shift both ways.
    top = AxiTlAxiTop()

    def driver():
        resp, _id = yield from axi_write_together(top.axi_master,
                                         0,
                                         0xcafef00d,
                                         0xf,
                                         size=2,
                                         txn_id=1)
        assert resp == AXIResp.OKAY
        data, resp, _last, rid = yield from axi_read(top.axi_master,
                                                     0,
                                                     size=2,
                                                     txn_id=1)
        assert (data, resp, rid) == (0xcafef00d, AXIResp.OKAY, 1)

    run_sim(top, driver)


# ===========================================================================
# Phase 7 -- Gap-fill: TL-C through CacheCork, burst edges, max_flights,
#            atomic opcodes
# ===========================================================================
def _axi_write_burst_together(bus, addr, beats, *, size, txn_id=0):
    """Drive a multi-beat AXI write with AW held valid across all W beats.

    ``AXI2Tilelink`` gates ``aw.ready`` on ``w.valid & w.last``, so AW must
    remain asserted until the final W beat.  Each entry in *beats* is a
    ``(data, strb)`` tuple.  Returns ``(resp, id)``.
    """
    yield bus.aw.bits.addr.eq(addr)
    yield bus.aw.bits.size.eq(size)
    yield bus.aw.bits.len.eq(len(beats) - 1)
    yield bus.aw.bits.burst.eq(AXIBurst.INCR)
    yield bus.aw.bits.id.eq(txn_id)
    yield bus.aw.valid.eq(1)
    for index, (data, strb) in enumerate(beats):
        yield bus.w.bits.data.eq(data)
        yield bus.w.bits.strb.eq(strb)
        yield bus.w.bits.last.eq(index == len(beats) - 1)
        yield bus.w.valid.eq(1)
        yield
        while not (yield bus.w.ready):
            yield
    yield bus.aw.valid.eq(0)
    yield bus.w.valid.eq(0)

    yield bus.b.ready.eq(1)
    while not (yield bus.b.valid):
        yield
    result = ((yield bus.b.bits.resp), (yield bus.b.bits.id))
    yield
    yield bus.b.ready.eq(0)
    return result


# ---------------------------------------------------------------------------
# TL-C through TileLink2AXI + CacheCork
# ---------------------------------------------------------------------------
@pytest.mark.parametrize("grow_param", [tilelink.GrowParam.NtoB,
                                         tilelink.GrowParam.NtoT])
def test_tl2axi_cachecork_acquire_block(grow_param):
    # AcquireBlock must surface on the AXI side as a Get (AR) whose id
    # carries the cork-shifted source, and return as GrantData.
    top = Tl2AxiTop(source_id_width=3, id_width=4, has_bce=True)
    lg = log2_int(top.data_width // 8)
    ar_ids = []

    def driver():
        d_op, d_param, d_source, _sink, data, d_denied, d_corrupt = \
            yield from tl_acquire(top.tl,
                                  0,
                                  size=lg,
                                  source=2,
                                  grow_param=grow_param)
        assert d_op == tilelink.ChannelDOpcode.GrantData.value
        assert d_param == tilelink.CapParam.toT.value
        assert d_source == 2
        assert data == 0x100
        assert (d_denied, d_corrupt) == (0, 0)

    def monitor():
        for _ in range(300):
            if (yield top.ram.ar_monitor.valid):
                ar_ids.append((yield top.ram.bus.ar.bits.id))
            yield
        assert ar_ids == [(2 << 1) | 1]

    run_sim(top, driver, monitor)


def test_tl2axi_cachecork_acquire_perm_no_downstream():
    # AcquirePerm(BtoT) returns a Grant immediately via the CacheCork; no AXI
    # traffic (AR/AW) must be generated.
    top = Tl2AxiTop(source_id_width=3, id_width=4, has_bce=True)
    lg = log2_int(top.data_width // 8)
    axi_traffic = []

    def driver():
        d_op, _param, d_source, _sink, data, _, _ = \
            yield from tl_acquire(top.tl,
                                  0,
                                  size=lg,
                                  source=3,
                                  opcode=tilelink.ChannelAOpcode.AcquirePerm,
                                  grow_param=tilelink.GrowParam.BtoT)
        assert d_op == tilelink.ChannelDOpcode.Grant.value
        assert d_source == 3
        assert data == 0

    def monitor():
        for _ in range(300):
            if (yield top.ram.ar_monitor.valid):
                axi_traffic.append('AR')
            if (yield top.ram.aw_monitor.valid):
                axi_traffic.append('AW')
            yield
        assert axi_traffic == []

    run_sim(top, driver, monitor)


def test_tl2axi_cachecork_release_data_writes_and_acks():
    # ReleaseData must become a downstream PutFull (AXI AW+W), the AXI B
    # response must surface as a ReleaseAck, and the data must persist.
    top = Tl2AxiTop(source_id_width=3, id_width=4, has_bce=True)
    lg = log2_int(top.data_width // 8)
    aw_caps = []
    w_caps = []

    def driver():
        d_op, d_source, d_denied = yield from tl_release(
            top.tl, 0, size=lg, source=4, data=0xDEADBEEF)
        assert d_op == tilelink.ChannelDOpcode.ReleaseAck.value
        assert d_source == 4
        assert d_denied == 0
        # Read back via a plain Get through the same cork+bridge path.
        data, _, _ = yield from tl_get(top.tl, 0, size=lg, source=0)
        assert data == 0xDEADBEEF

    def monitor():
        for _ in range(400):
            if (yield top.ram.aw_monitor.valid):
                aw_caps.append(((yield top.ram.aw_monitor.bits.addr),
                                (yield top.ram.aw_monitor.bits.len),
                                (yield top.ram.aw_monitor.bits.size),
                                (yield top.ram.bus.aw.bits.id)))
            if (yield top.ram.w_monitor.valid):
                w_caps.append(((yield top.ram.w_monitor.bits.data),
                               (yield top.ram.w_monitor.bits.strb)))
            yield

    run_sim(top, driver, monitor)
    assert aw_caps == [(0, 0, lg, 4 << 1)]
    assert w_caps == [(0xDEADBEEF, 0xf)]


# ---------------------------------------------------------------------------
# AXI2Tilelink burst-length edge cases
# ---------------------------------------------------------------------------
@pytest.mark.parametrize("length", [1, 3, 15])
def test_axi2tl_multibeat_read_incr(length):
    top = Axi2TlTop(id_width=4, max_flights=1)
    lg = log2_int(top.data_width // 8)

    def driver():
        beats = yield from axi_read_burst(top.axi,
                                          0,
                                          size=lg,
                                          length=length,
                                          txn_id=1)
        assert len(beats) == length + 1
        for i, (data, resp, last, _rid) in enumerate(beats):
            assert data == 0x100 + i
            assert resp == AXIResp.OKAY
            assert last == (i == length)

    run_sim(top, driver)


@pytest.mark.parametrize("nbeats", [2, 4, 16])
def test_axi2tl_multibeat_write_incr(nbeats):
    top = Axi2TlTop(id_width=4, max_flights=1)
    lg = log2_int(top.data_width // 8)
    pattern = [0x11110000 + i for i in range(nbeats)]
    beats = [(v, 0xf) for v in pattern]

    def driver():
        resp, bid = yield from _axi_write_burst_together(top.axi,
                                                         0,
                                                         beats,
                                                         size=lg,
                                                         txn_id=2)
        assert (resp, bid) == (AXIResp.OKAY, 2)
        read_beats = yield from axi_read_burst(top.axi,
                                               0,
                                               size=lg,
                                               length=nbeats - 1,
                                               txn_id=3)
        assert len(read_beats) == nbeats
        for i, (data, resp, last, rid) in enumerate(read_beats):
            assert (data, resp, last, rid) == (
                pattern[i], AXIResp.OKAY, i == nbeats - 1, 3)

    run_sim(top, driver)


def test_axi2tl_wrap4_aligned_read():
    # A WRAP4 burst at a boundary-aligned address is equivalent to INCR for
    # data-ordering purposes.  The bridge must handle the burst type without
    # error and return beats in linear order.
    top = Axi2TlTop(id_width=4, max_flights=1)
    lg = log2_int(top.data_width // 8)

    def driver():
        yield top.axi.ar.bits.addr.eq(0)
        yield top.axi.ar.bits.size.eq(lg)
        yield top.axi.ar.bits.len.eq(3)
        yield top.axi.ar.bits.burst.eq(AXIBurst.WRAP)
        yield top.axi.ar.bits.id.eq(1)
        yield top.axi.ar.valid.eq(1)
        yield
        while not (yield top.axi.ar.ready):
            yield
        yield top.axi.ar.valid.eq(0)

        yield top.axi.r.ready.eq(1)
        got = []
        for _ in range(4):
            while not (yield top.axi.r.valid):
                yield
            got.append((yield top.axi.r.bits.data))
            assert (yield top.axi.r.bits.resp) == AXIResp.OKAY
            yield
        yield top.axi.r.ready.eq(0)
        assert got == [0x100, 0x101, 0x102, 0x103]

    run_sim(top, driver)


@pytest.mark.parametrize("axi_size,tl_size", [(0, 0), (1, 1), (2, 2)])
def test_axi2tl_axi_size_maps_to_tl_size(axi_size, tl_size):
    top = Axi2TlTop(id_width=4, max_flights=1)
    caps = []

    def driver():
        yield from axi_read(top.axi, 0, size=axi_size, txn_id=1)

    def monitor():
        for _ in range(300):
            if (yield top.sram.a_monitor.valid):
                caps.append((yield top.sram.a_monitor.bits.size))
            yield

    run_sim(top, driver, monitor)
    assert caps[0] == tl_size


# ---------------------------------------------------------------------------
# max_flights: saturate, stall, then release
# ---------------------------------------------------------------------------
def test_tl2axi_max_flights_stall_then_release():
    top = Tl2AxiMaxFlightsTop(max_flights=2)
    lg = log2_int(32 // 8)
    counts = {'a': 0, 'ar': 0}

    def driver():
        bus = top.tl
        beat_bytes = bus.data_width // 8
        yield bus.d.ready.eq(0)

        # Offer Gets continuously until backpressure is sustained. A single
        # quiet monitor cycle is not sufficient evidence of saturation: skid
        # buffers can naturally insert bubbles before all slots are occupied.
        yield bus.a.bits.opcode.eq(tilelink.ChannelAOpcode.Get)
        yield bus.a.bits.param.eq(0)
        yield bus.a.bits.size.eq(lg)
        yield bus.a.bits.source.eq(0)
        yield bus.a.bits.address.eq(0)
        yield bus.a.bits.mask.eq((1 << beat_bytes) - 1)
        yield bus.a.bits.data.eq(0)
        yield bus.a.bits.corrupt.eq(0)
        yield bus.a.valid.eq(1)

        stalled_cycles = 0
        for _ in range(100):
            yield
            if (yield bus.a.ready):
                stalled_cycles = 0
            else:
                stalled_cycles += 1
            if stalled_cycles == 8:
                break
        else:
            raise AssertionError("size queue never reached sustained stall")
        saturated = counts['a']
        saturated_ar = counts['ar']
        assert top.max_flights <= saturated <= top.max_flights + 2

        # Retire exactly one D beat, then block D again. Exactly one additional
        # A request must be admitted before the queue returns to saturation.
        for _ in range(100):
            if (yield bus.d.valid):
                break
            yield
        else:
            raise AssertionError("no D response available at saturation")
        yield bus.d.ready.eq(1)
        yield
        yield bus.d.ready.eq(0)

        stalled_cycles = 0
        for _ in range(100):
            yield
            if (yield bus.a.ready):
                stalled_cycles = 0
            else:
                stalled_cycles += 1
            if stalled_cycles == 8:
                break
        else:
            raise AssertionError("queue did not stall again after one retire")
        yield bus.a.valid.eq(0)
        assert counts['a'] == saturated + 1
        assert counts['ar'] == saturated_ar + 1

    def monitor():
        for _ in range(300):
            if (yield top.tl.a.fire):
                counts['a'] += 1
            if (yield top.ram.ar_monitor.valid):
                counts['ar'] += 1
            yield

    run_sim(top, driver, monitor)


# ---------------------------------------------------------------------------
# Atomic opcodes: treated as reads (unsupported -- documented)
# ---------------------------------------------------------------------------
@pytest.mark.parametrize("opcode", [tilelink.ChannelAOpcode.ArithmeticData,
                                     tilelink.ChannelAOpcode.LogicalData])
def test_tl2axi_atomic_opcode_treated_as_read(opcode):
    # ArithmeticData / LogicalData are not recognised by ``Interface.has_data``
    # (only PutFull/PutPartial qualify), so the bridge routes them through the
    # read path (AR) and ignores the operand data on the W channel.  This test
    # documents that behaviour: the opcode surfaces as an AXI AR, no AW/W is
    # generated, and the response carries plain read data.
    top = Tl2AxiTop(source_id_width=4, id_width=4)
    lg = log2_int(top.data_width // 8)
    ar_fired = []
    aw_fired = []

    def driver():
        bus = top.tl
        beat_bytes = bus.data_width // 8
        yield bus.a.bits.opcode.eq(opcode)
        yield bus.a.bits.param.eq(0)
        yield bus.a.bits.size.eq(lg)
        yield bus.a.bits.source.eq(1)
        yield bus.a.bits.address.eq(0)
        yield bus.a.bits.mask.eq((1 << beat_bytes) - 1)
        yield bus.a.bits.data.eq(0x42)
        yield bus.a.bits.corrupt.eq(0)
        yield bus.a.valid.eq(1)
        yield
        while not (yield bus.a.ready):
            yield
        yield bus.a.valid.eq(0)

        yield bus.d.ready.eq(1)
        while not (yield bus.d.valid):
            yield
        data = (yield bus.d.bits.data)
        d_opcode = (yield bus.d.bits.opcode)
        d_source = (yield bus.d.bits.source)
        denied = (yield bus.d.bits.denied)
        corrupt = (yield bus.d.bits.corrupt)
        yield
        yield bus.d.ready.eq(0)
        assert data == 0x100
        assert d_opcode == tilelink.ChannelDOpcode.AccessAckData.value
        assert d_source == 1
        assert (denied, corrupt) == (0, 0)

    def monitor():
        for _ in range(300):
            if (yield top.ram.ar_monitor.valid):
                ar_fired.append(True)
            if (yield top.ram.aw_monitor.valid):
                aw_fired.append(True)
            yield

    run_sim(top, driver, monitor)
    assert len(ar_fired) == 1
    assert len(aw_fired) == 0
