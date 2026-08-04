"""Phase 6: TileLink2Wishbone bridge tests.

Covers ``roomsoc.interconnect.tilelink.TileLink2Wishbone`` -- the manager-side
bridge that converts TileLink Get/Put into Wishbone classic cycles.  The bridge
internally composes a ``CacheCork`` (when ``has_bce=True``) and a ``Fragmenter``
so it accepts full-size TL-C transactions and narrows them to the Wishbone data
width beat-by-beat.

  * Get -> WB read cycle -> D ``AccessAckData``; Put -> WB write cycle -> D
    ``AccessAck``.
  * ``mask`` byte-enable fidelity on the WB ``sel`` / ``dat_w`` lanes.
  * Multi-beat TL -> sequence of WB classic cycles.
  * Both ``has_bce=True`` (exercises ``CacheCork``) and ``has_bce=False`` paths.
  * Oversized Get/Put (exercises the internal ``Fragmenter``).
  * WB ``err`` -> D ``denied`` (writes) / ``corrupt`` (reads).
  * Non-zero ``base_addr`` address translation.

All tests use the two-process Decoupled pattern with the pysim clock model from
AGENTS.md (only a naked ``yield`` advances the cycle; reads/writes between naked
yields are coherent within one cycle).
"""

import pytest

from amaranth import *
from amaranth.utils import log2_int
from amaranth_soc.wishbone.bus import Interface as WBInterface

from roomsoc.interconnect import tilelink, wishbone

from tl_helpers import (tl_acquire, tl_get, tl_put, tl_release, run_sim)


def _init(depth, base=0x100):
    return [base + i for i in range(depth)]


class TL2WBTop(Elaboratable):
    """TileLink2Wishbone (manager) + wishbone.SRAM (subordinate)."""

    def __init__(self,
                 *,
                 data_width=32,
                 addr_width=32,
                 depth=256,
                 has_bce=False,
                 base_addr=0,
                 init=None):
        self.data_width = data_width
        self.tl = tilelink.Interface(addr_width=addr_width,
                                     data_width=data_width,
                                     size_width=4,
                                     source_id_width=4,
                                     sink_id_width=4,
                                     has_bce=has_bce)
        self.wb = WBInterface(data_width=data_width,
                              addr_width=addr_width,
                              granularity=8)
        self.bridge = tilelink.TileLink2Wishbone(self.tl,
                                                 self.wb,
                                                 base_addr=base_addr)
        self.depth = depth
        self._init = init if init is not None else _init(depth)

    def elaborate(self, platform):
        m = Module()
        mem = Memory(width=self.data_width,
                     depth=self.depth,
                     init=self._init)
        self.sram = wishbone.SRAM(mem, bus=self.wb)
        m.submodules.bridge = self.bridge
        m.submodules.sram = self.sram
        return m


class TL2WBErrTop(Elaboratable):
    """TileLink2Wishbone with a bare WB bus (features=['err']), no slave.

    A test process drives ``ack`` / ``err`` / ``dat_r`` directly so the
    error-propagation path can be exercised without a full SRAM model.
    """

    def __init__(self, *, data_width=32, addr_width=32, has_bce=False):
        self.data_width = data_width
        self.tl = tilelink.Interface(addr_width=addr_width,
                                     data_width=data_width,
                                     size_width=4,
                                     source_id_width=4,
                                     sink_id_width=4,
                                     has_bce=has_bce)
        self.wb = WBInterface(data_width=data_width,
                              addr_width=addr_width,
                              granularity=8,
                              features=['err'])
        self.bridge = tilelink.TileLink2Wishbone(self.tl, self.wb)

    def elaborate(self, platform):
        m = Module()
        m.submodules.bridge = self.bridge
        return m


def _wb_err_responder(wb, *, cycles=500):
    """Minimal WB slave that asserts ``ack`` and ``err`` on every cycle.

    Asserts ack one cycle after cyc&stb (matching wishbone.SRAM classic-cycle
    timing) and simultaneously pulses ``err`` so the bridge propagates it to D.
    """
    yield wb.ack.eq(0)
    yield wb.err.eq(0)
    yield
    for _ in range(cycles):
        if (yield wb.cyc) and (yield wb.stb):
            yield wb.ack.eq(1)
            yield wb.err.eq(1)
            yield
            yield wb.ack.eq(0)
            yield wb.err.eq(0)
            yield
        yield


def _wb_capture_first(wb, cap):
    """Capture the first WB cycle's master-side signals into ``cap[0]``."""
    yield
    for _ in range(300):
        if (yield wb.cyc) and (yield wb.stb):
            cap.append({
                'adr':
                (yield wb.adr),
                'dat_w':
                (yield wb.dat_w),
                'sel':
                (yield wb.sel),
                'we':
                (yield wb.we),
            })
            return
        yield


def _drive_get_without_d(bus, address, source):
    """Send one Get A beat without changing D ready."""
    beat_bytes = bus.data_width // 8
    yield bus.a.bits.opcode.eq(tilelink.ChannelAOpcode.Get)
    yield bus.a.bits.param.eq(0)
    yield bus.a.bits.size.eq(log2_int(beat_bytes))
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
        raise AssertionError("timed out waiting for TileLink A acceptance")
    yield bus.a.valid.eq(0)
    yield


# ---------------------------------------------------------------------------
# Get -> WB read -> AccessAckData
# ---------------------------------------------------------------------------
def test_tl2wb_get_read():
    top = TL2WBTop(depth=256, init=_init(256))
    lg = log2_int(32 // 8)

    def driver():
        data, denied, corrupt = yield from tl_get(top.tl, 0, size=lg, source=1)
        assert (data, denied, corrupt) == (0x100, 0, 0)
        data, _, _ = yield from tl_get(top.tl, 4, size=lg, source=2)
        assert data == 0x101

    run_sim(top, driver)


# ---------------------------------------------------------------------------
# PutFull -> WB write -> AccessAck, read back
# ---------------------------------------------------------------------------
def test_tl2wb_putfull_write_and_readback():
    top = TL2WBTop(depth=256)
    lg = log2_int(32 // 8)

    def driver():
        denied = yield from tl_put(top.tl,
                                   0,
                                   0xCAFEF00D,
                                   0xf,
                                   size=lg,
                                   source=3)
        assert denied == 0
        data, _, _ = yield from tl_get(top.tl, 0, size=lg, source=4)
        assert data == 0xCAFEF00D

    run_sim(top, driver)


# ---------------------------------------------------------------------------
# PutPartial: mask fidelity on WB sel / dat_w lanes
# ---------------------------------------------------------------------------
def test_tl2wb_putpartial_byte_mask():
    top = TL2WBTop(depth=256, init=[0] * 256)
    lg = log2_int(32 // 8)
    cap = []

    def driver():
        denied = yield from tl_put(top.tl,
                                   0,
                                   0xAABBCCDD,
                                   0b0101,
                                   size=lg,
                                   source=5,
                                   full=False)
        assert denied == 0
        data, _, _ = yield from tl_get(top.tl, 0, size=lg, source=6)
        assert data == 0x00BB00DD

    def monitor():
        yield from _wb_capture_first(top.wb, cap)

    run_sim(top, driver, monitor)
    assert cap[0]['sel'] == 0b0101
    assert cap[0]['dat_w'] == 0xAABBCCDD
    assert cap[0]['we'] == 1


# ---------------------------------------------------------------------------
# Multi-beat Get (Fragmenter splits into beat-sized fragments)
# ---------------------------------------------------------------------------
def test_tl2wb_multibeat_get():
    top = TL2WBTop(depth=256, init=_init(256))

    def driver():
        data, denied, corrupt = yield from tl_get(top.tl, 0, size=3, source=1)
        assert (denied, corrupt) == (0, 0)
        assert data == (0x100 | (0x101 << 32))

    run_sim(top, driver)


# ---------------------------------------------------------------------------
# Multi-beat Put
# ---------------------------------------------------------------------------
def test_tl2wb_multibeat_put():
    top = TL2WBTop(depth=256, init=[0] * 256)

    def driver():
        value = 0x22222222_11111111
        denied = yield from tl_put(top.tl, 0, value, 0xff, size=3, source=2)
        assert denied == 0
        data, _, _ = yield from tl_get(top.tl, 0, size=3, source=3)
        assert data == value

    run_sim(top, driver)


# ---------------------------------------------------------------------------
# has_bce=True exercises CacheCork
# ---------------------------------------------------------------------------
def test_tl2wb_has_bce_get():
    top = TL2WBTop(depth=256, init=_init(256), has_bce=True)
    lg = log2_int(32 // 8)

    def driver():
        data, denied, corrupt = yield from tl_get(top.tl, 0, size=lg, source=1)
        assert (data, denied, corrupt) == (0x100, 0, 0)

    run_sim(top, driver)


def test_tl2wb_has_bce_put():
    top = TL2WBTop(depth=256, has_bce=True)
    lg = log2_int(32 // 8)

    def driver():
        denied = yield from tl_put(top.tl,
                                   0x10,
                                   0xDEADBEEF,
                                   0xf,
                                   size=lg,
                                   source=2)
        assert denied == 0
        data, _, _ = yield from tl_get(top.tl, 0x10, size=lg, source=3)
        assert data == 0xDEADBEEF

    run_sim(top, driver)


def test_tl2wb_has_bce_acquire_and_release_data():
    top = TL2WBTop(depth=256, init=_init(256), has_bce=True)
    lg = log2_int(32 // 8)

    def driver():
        d_op, d_param, d_source, _sink, data, denied, corrupt = \
            yield from tl_acquire(top.tl,
                                  0,
                                  size=lg,
                                  source=5,
                                  grow_param=tilelink.GrowParam.NtoB)
        assert d_op == tilelink.ChannelDOpcode.GrantData.value
        assert d_param == tilelink.CapParam.toT.value
        assert d_source == 5
        assert (data, denied, corrupt) == (0x100, 0, 0)

        d_op, d_source, denied = yield from tl_release(top.tl,
                                                       4,
                                                       size=lg,
                                                       source=6,
                                                       data=0xDEADBEEF)
        assert d_op == tilelink.ChannelDOpcode.ReleaseAck.value
        assert d_source == 6
        assert denied == 0

        data, _, _ = yield from tl_get(top.tl, 4, size=lg, source=1)
        assert data == 0xDEADBEEF

    run_sim(top, driver)


# ---------------------------------------------------------------------------
# Oversized Get/Put (exercises Fragmenter with multiple fragments)
# ---------------------------------------------------------------------------
def test_tl2wb_oversized_get():
    top = TL2WBTop(depth=256, init=_init(256))

    def driver():
        data, denied, corrupt = yield from tl_get(top.tl, 0, size=4, source=1)
        assert (denied, corrupt) == (0, 0)
        expected = (0x100 | (0x101 << 32) | (0x102 << 64) | (0x103 << 96))
        assert data == expected

    run_sim(top, driver)


def test_tl2wb_oversized_put():
    top = TL2WBTop(depth=256, init=[0] * 256)

    def driver():
        value = sum((0x10 + i) << (i * 32) for i in range(4))
        denied = yield from tl_put(top.tl, 0, value, 0xff_ff, size=4, source=2)
        assert denied == 0
        data, _, _ = yield from tl_get(top.tl, 0, size=4, source=3)
        assert data == value

    run_sim(top, driver)


def test_tl2wb_64bit_partial_write_and_multibeat_readback():
    top = TL2WBTop(data_width=64, depth=64, init=[0] * 64)
    lg = log2_int(64 // 8)

    def driver():
        denied = yield from tl_put(top.tl,
                                   0,
                                   0xAABBCCDDEEFF0011,
                                   0b10000001,
                                   size=lg,
                                   source=2,
                                   full=False)
        assert denied == 0
        data, denied, corrupt = yield from tl_get(top.tl,
                                                  0,
                                                  size=lg,
                                                  source=3)
        assert (data, denied, corrupt) == (0xAA00000000000011, 0, 0)

        value = 0x4444444444444444_3333333333333333
        denied = yield from tl_put(top.tl,
                                   0x10,
                                   value,
                                   0xffff,
                                   size=lg + 1,
                                   source=4)
        assert denied == 0
        data, denied, corrupt = yield from tl_get(top.tl,
                                                  0x10,
                                                  size=lg + 1,
                                                  source=5)
        assert (data, denied, corrupt) == (value, 0, 0)

    run_sim(top, driver)


def test_tl2wb_stalled_d_blocks_next_wishbone_cycle():
    top = TL2WBErrTop()
    wb_requests = []

    def responder():
        wb = top.wb
        yield wb.ack.eq(0)
        yield wb.err.eq(0)
        yield
        for _ in range(300):
            if (yield wb.cyc) and (yield wb.stb):
                address = (yield wb.adr)
                wb_requests.append(address)
                yield wb.dat_r.eq(0x900 + address)
                yield wb.ack.eq(1)
                yield
                yield wb.ack.eq(0)
                yield
            yield

    def driver():
        bus = top.tl
        yield bus.d.ready.eq(0)
        yield from _drive_get_without_d(bus, 0, source=1)

        for _ in range(200):
            if (yield bus.d.valid):
                break
            yield
        else:
            raise AssertionError("timed out waiting for first D response")

        first_payload = ((yield bus.d.bits.opcode),
                         (yield bus.d.bits.source),
                         (yield bus.d.bits.data))
        assert first_payload == (tilelink.ChannelDOpcode.AccessAckData.value,
                                 1, 0x900)

        # The A side may buffer this request, but d_stall must prevent it from
        # starting another Wishbone cycle until the pending D beat is accepted.
        yield from _drive_get_without_d(bus, 4, source=2)
        for _ in range(6):
            assert (yield bus.d.valid) == 1
            assert ((yield bus.d.bits.opcode),
                    (yield bus.d.bits.source),
                    (yield bus.d.bits.data)) == first_payload
            assert wb_requests == [0]
            yield

        yield bus.d.ready.eq(1)
        yield

        for _ in range(200):
            if (yield bus.d.valid) and (yield bus.d.bits.source) == 2:
                assert (yield bus.d.bits.data) == 0x901
                break
            yield
        else:
            raise AssertionError("timed out waiting for second D response")
        yield

    run_sim(top, driver, responder)
    assert wb_requests == [0, 1]


# ---------------------------------------------------------------------------
# WB err -> D denied (writes) / corrupt (reads)
# ---------------------------------------------------------------------------
def test_tl2wb_wb_err_write_denied():
    top = TL2WBErrTop()
    lg = log2_int(32 // 8)

    def driver():
        denied = yield from tl_put(top.tl,
                                   0,
                                   0x12345678,
                                   0xf,
                                   size=lg,
                                   source=1)
        assert denied == 1

    def responder():
        yield from _wb_err_responder(top.wb)

    run_sim(top, driver, responder)


def test_tl2wb_wb_err_read_corrupt():
    top = TL2WBErrTop()
    lg = log2_int(32 // 8)

    def driver():
        data, denied, corrupt = yield from tl_get(top.tl, 0, size=lg, source=1)
        assert (denied, corrupt) == (0, 1)

    def responder():
        yield from _wb_err_responder(top.wb)

    run_sim(top, driver, responder)


# ---------------------------------------------------------------------------
# Non-zero base_addr address translation
# ---------------------------------------------------------------------------
def test_tl2wb_base_addr_offset():
    base = 0x1000
    top = TL2WBTop(depth=256, init=_init(256), base_addr=base)
    lg = log2_int(32 // 8)

    def driver():
        data, _, _ = yield from tl_get(top.tl, base, size=lg, source=1)
        assert data == 0x100
        data, _, _ = yield from tl_get(top.tl, base + 4, size=lg, source=2)
        assert data == 0x101

    run_sim(top, driver)
