"""Isolated unit tests for the AXI protocol bridges.

Covers the bridge directions that previously had no dedicated pytest coverage
(see ``tests/roomsoc/interconnect/AXI_TEST_PLAN.md`` Phase 1):

  * ``AXILite2AXI``         -- AXI-Lite master -> AXI slave
  * ``AXILite2Wishbone``    -- AXI-Lite master -> Wishbone slave
  * ``Wishbone2AXILite``    -- Wishbone master -> AXI-Lite slave (+ err path)
  * ``Wishbone2AXI``        -- Wishbone master -> AXI slave
  * ``AXI2Wishbone``        -- AXI master -> Wishbone slave (single/burst/narrow)
  * ``AXI2AXILite``         -- read response (SLVERR) propagation

The AXI and AXI-Lite far-ends are driven by the shared ``axi_helpers`` kit
(``AXIResponder``, ``AXILiteResponder``, master drivers); the Wishbone far-end
uses ``roomsoc.interconnect.wishbone.SRAM`` as a real slave and a tiny
classic-timing Wishbone master driver.
"""

import pytest

from amaranth import *
from amaranth.utils import log2_int

from roomsoc.interconnect.axi import (AXI2AXILite, AXI2Wishbone, AXIInterface,
                                      AXILite2AXI, AXILite2Wishbone,
                                      AXILiteInterface, Wishbone2AXI,
                                      Wishbone2AXILite)
import roomsoc.interconnect.wishbone as wb

from axi_helpers import (AXIResponder, AXILiteResponder, run_sim, axi_read,
                         axi_read_burst, axi_write, axi_write_burst)


# ---------------------------------------------------------------------------
# Wishbone master driver (classic single-cycle handshake)
# ---------------------------------------------------------------------------
def wb_read(bus, adr):
    yield bus.adr.eq(adr)
    yield bus.we.eq(0)
    yield bus.sel.eq((1 << len(bus.sel)) - 1)
    yield bus.cyc.eq(1)
    yield bus.stb.eq(1)
    yield
    while not (yield bus.ack):
        yield
    data = (yield bus.dat_r)
    err = (yield bus.err) if hasattr(bus, 'err') else 0
    yield bus.stb.eq(0)
    yield bus.cyc.eq(0)
    yield
    return data, err


def wb_write(bus, adr, data, sel=None):
    if sel is None:
        sel = (1 << len(bus.sel)) - 1
    yield bus.adr.eq(adr)
    yield bus.we.eq(1)
    yield bus.sel.eq(sel)
    yield bus.dat_w.eq(data)
    yield bus.cyc.eq(1)
    yield bus.stb.eq(1)
    yield
    while not (yield bus.ack):
        yield
    err = (yield bus.err) if hasattr(bus, 'err') else 0
    yield bus.stb.eq(0)
    yield bus.cyc.eq(0)
    yield bus.we.eq(0)
    yield
    return err


def _wb_addr_width(data_width):
    # AXI-Lite byte addresses are 32-bit; the bridges shift off log2(dw/8) bits
    # to form the Wishbone word address.
    return 32 - log2_int(data_width // 8)


# ===========================================================================
# AXILite2AXI
# ===========================================================================
class AXILite2AXITop(Elaboratable):

    def __init__(self,
                 *,
                 data_width=32,
                 id_width=8,
                 write_id=3,
                 read_id=5,
                 burst_type='INCR'):
        self.data_width = data_width
        self.master = AXILiteInterface(addr_width=32, data_width=data_width)
        self.ram = AXIResponder(addr_width=32,
                                data_width=data_width,
                                depth=64,
                                id_width=id_width)
        self.bridge = AXILite2AXI(self.master,
                                  self.ram.bus,
                                  write_id=write_id,
                                  read_id=read_id,
                                  burst_type=burst_type)

    def elaborate(self, platform):
        m = Module()
        m.submodules.bridge = self.bridge
        m.submodules.ram = self.ram
        return m


@pytest.mark.parametrize("burst_type,code", [('FIXED', 0), ('INCR', 1),
                                             ('WRAP', 2)])
def test_axilite2axi_translates_burst_type_and_ids(burst_type, code):
    # The bridge must emit single-beat (len=0) AXI bursts whose size matches
    # the bus width, whose burst type follows ``burst_type``, and whose AW/AR
    # ids follow ``write_id``/``read_id``. Data must round-trip.
    top = AXILite2AXITop(data_width=32,
                         write_id=3,
                         read_id=5,
                         burst_type=burst_type)
    lg = (top.data_width // 8).bit_length() - 1  # log2(dw/8)
    ar_caps = []
    aw_caps = []

    def driver():
        assert (yield from top.master.write(8, 0xcafef00d, 0xf)) == 0
        data, resp = yield from top.master.read(8)
        assert (data, resp) == (0xcafef00d, 0)

    def monitor():
        for _ in range(300):
            if (yield top.ram.bus.ar.valid) and (yield top.ram.bus.ar.ready):
                ar_caps.append(
                    ((yield top.ram.bus.ar.bits.addr),
                     (yield
                      top.ram.bus.ar.bits.len), (yield
                                                 top.ram.bus.ar.bits.size),
                     (yield
                      top.ram.bus.ar.bits.burst), (yield
                                                   top.ram.bus.ar.bits.id)))
            if (yield top.ram.bus.aw.valid) and (yield top.ram.bus.aw.ready):
                aw_caps.append(
                    ((yield top.ram.bus.aw.bits.addr),
                     (yield
                      top.ram.bus.aw.bits.len), (yield
                                                 top.ram.bus.aw.bits.size),
                     (yield
                      top.ram.bus.aw.bits.burst), (yield
                                                   top.ram.bus.aw.bits.id)))
            yield
        assert aw_caps == [(8, 0, lg, code, 3)]
        assert ar_caps == [(8, 0, lg, code, 5)]

    run_sim(top, driver, monitor)


def test_axilite2axi_wide_bus():
    # 64-bit variant: burst size must be 3 (8 bytes), data round-trips.
    top = AXILite2AXITop(data_width=64, write_id=1, read_id=2)
    ar_caps = []
    aw_caps = []

    def driver():
        assert (yield from top.master.write(0, 0x1122334455667788, 0xff)) == 0
        data, resp = yield from top.master.read(0)
        assert (data, resp) == (0x1122334455667788, 0)

    def monitor():
        for _ in range(300):
            if (yield top.ram.bus.ar.valid) and (yield top.ram.bus.ar.ready):
                ar_caps.append((yield top.ram.bus.ar.bits.size))
            if (yield top.ram.bus.aw.valid) and (yield top.ram.bus.aw.ready):
                aw_caps.append((yield top.ram.bus.aw.bits.size))
            yield
        assert aw_caps == [3]
        assert ar_caps == [3]

    run_sim(top, driver, monitor)


# ===========================================================================
# AXILite2Wishbone
# ===========================================================================
class AXILite2WishboneTop(Elaboratable):

    def __init__(self, *, data_width=32, depth=256, base_addr=0):
        self.data_width = data_width
        self.base_addr = base_addr
        self.master = AXILiteInterface(addr_width=32, data_width=data_width)
        self.bus = wb.Interface(addr_width=_wb_addr_width(data_width),
                                data_width=data_width,
                                granularity=8)
        self.mem = Memory(width=data_width, depth=depth, init=[0] * depth)
        self.sram = wb.SRAM(self.mem, bus=self.bus)

    def elaborate(self, platform):
        m = Module()
        m.submodules.bridge = AXILite2Wishbone(self.master,
                                               self.bus,
                                               base_addr=self.base_addr)
        m.submodules.sram = self.sram
        return m


def test_axilite2wishbone_roundtrip_and_base_addr():
    # base_addr is subtracted before the word-address shift, so an AXI-Lite
    # address of base+8 lands in SRAM word 2.
    top = AXILite2WishboneTop(data_width=32, base_addr=0x1000)

    def driver():
        assert (yield from top.master.write(0x1008, 0xdeadbeef, 0xf)) == 0
        data, resp = yield from top.master.read(0x1008)
        assert (data, resp) == (0xdeadbeef, 0)

    run_sim(top, driver)


def test_axilite2wishbone_arbitrates_simultaneous_read_and_write():
    # Exercise the actual AR-valid & AW-valid branch. At reset last_is_r=0, so
    # the read must be accepted first; the pending write must then complete
    # without either request being lost.
    top = AXILite2WishboneTop(data_width=32)
    accepted = []

    def driver():
        bus = top.master
        yield bus.ar.addr.eq(0)
        yield bus.ar.valid.eq(1)
        yield bus.aw.addr.eq(8)
        yield bus.aw.valid.eq(1)
        yield bus.w.data.eq(0xbbbbbbbb)
        yield bus.w.strb.eq(0xf)
        yield bus.w.valid.eq(1)
        yield bus.r.ready.eq(1)
        yield bus.b.ready.eq(1)
        yield

        ar_pending = True
        aw_pending = True
        w_pending = True
        read_data = None
        saw_b = False
        for _ in range(100):
            if ar_pending and (yield bus.ar.ready):
                accepted.append('read')
                ar_pending = False
                yield bus.ar.valid.eq(0)
            if aw_pending and (yield bus.aw.ready):
                accepted.append('write')
                aw_pending = False
                yield bus.aw.valid.eq(0)
            if w_pending and (yield bus.w.ready):
                w_pending = False
                yield bus.w.valid.eq(0)
            if (yield bus.r.valid):
                read_data = (yield bus.r.data)
            if (yield bus.b.valid):
                saw_b = True
            if not ar_pending and not aw_pending and not w_pending and saw_b:
                break
            yield

        assert accepted == ['read', 'write']
        assert read_data == 0
        assert saw_b

        data, resp = yield from bus.read(8)
        assert (data, resp) == (0xbbbbbbbb, 0)

    run_sim(top, driver)


# ===========================================================================
# Wishbone2AXILite
# ===========================================================================
class Wishbone2AXILiteTop(Elaboratable):

    def __init__(self,
                 *,
                 data_width=32,
                 base_addr=0,
                 error_addr=0x0c,
                 with_err=False):
        self.data_width = data_width
        self.master = wb.Interface(
            addr_width=_wb_addr_width(data_width),
            data_width=data_width,
            granularity=8,
            features={'err'} if with_err else frozenset())
        self.slave = AXILiteResponder(data_width=data_width,
                                      error_addr=error_addr)
        self.base_addr = base_addr

    def elaborate(self, platform):
        m = Module()
        m.submodules.bridge = Wishbone2AXILite(self.master,
                                               self.slave.bus,
                                               base_addr=self.base_addr)
        m.submodules.slave = self.slave
        return m


def test_wishbone2axilite_read_write_roundtrip():
    # AXILiteResponder read data is (0xa0 << (dw-8)) | (addr >> 2). With base=0
    # a Wishbone adr of 4 maps to AXI-Lite addr 16, so dat_r = 0xa0000000 | 4.
    top = Wishbone2AXILiteTop(data_width=32)
    reads = []
    writes = []

    def driver():
        data, err = yield from wb_read(top.master, 4)
        assert (data, err) == (0xa0000004, 0)
        err = yield from wb_write(top.master, 8, 0x12345678)
        assert err == 0

    def monitor():
        for _ in range(300):
            if (yield top.slave.read_monitor.valid):
                reads.append((yield top.slave.read_monitor.bits.addr))
            if (yield top.slave.write_monitor.valid):
                writes.append(((yield top.slave.write_monitor.bits.addr),
                               (yield top.slave.write_monitor.bits.data),
                               (yield top.slave.write_monitor.bits.strb)))
            yield
        assert reads == [16]
        assert writes == [(32, 0x12345678, 0xf)]

    run_sim(top, driver, monitor)


def test_wishbone2axilite_propagates_error_on_err_line():
    # An AXI-Lite resp != 0 must drive the bridge into its ERROR state: the
    # Wishbone cycle still acks, and (with the err feature) asserts wb.err.
    # error_addr default 0x0c -> byte addr -> Wishbone adr 3.
    top = Wishbone2AXILiteTop(data_width=32, error_addr=0x0c, with_err=True)

    def driver():
        _d, err = yield from wb_read(top.master, 3)
        assert err == 1
        err = yield from wb_write(top.master, 3, 0x1)
        assert err == 1

    run_sim(top, driver)


# ===========================================================================
# Wishbone2AXI  (Wishbone2AXILite + AXILite2AXI)
# ===========================================================================
class Wishbone2AXITop(Elaboratable):

    def __init__(self, *, data_width=32, base_addr=0):
        self.data_width = data_width
        self.master = wb.Interface(addr_width=_wb_addr_width(data_width),
                                   data_width=data_width,
                                   granularity=8)
        self.ram = AXIResponder(addr_width=32,
                                data_width=data_width,
                                depth=64,
                                id_width=4)
        self.base_addr = base_addr

    def elaborate(self, platform):
        m = Module()
        m.submodules.bridge = Wishbone2AXI(self.master,
                                           self.ram.bus,
                                           base_addr=self.base_addr)
        m.submodules.ram = self.ram
        return m


def test_wishbone2axi_roundtrip():
    top = Wishbone2AXITop(data_width=32)

    def driver():
        err = yield from wb_write(top.master, 2, 0xfeedface)
        assert err == 0
        data, err = yield from wb_read(top.master, 2)
        assert (data, err) == (0xfeedface, 0)

    run_sim(top, driver)


# ===========================================================================
# AXI2Wishbone  (AXI2AXILite + AXILite2Wishbone)
# ===========================================================================
class AXI2WishboneTop(Elaboratable):

    def __init__(self, *, data_width=32, depth=256, base_addr=0):
        self.data_width = data_width
        self.axi = AXIInterface(addr_width=32,
                                data_width=data_width,
                                id_width=4)
        self.bus = wb.Interface(addr_width=_wb_addr_width(data_width),
                                data_width=data_width,
                                granularity=8)
        self.mem = Memory(width=data_width, depth=depth, init=[0] * depth)
        self.sram = wb.SRAM(self.mem, bus=self.bus)
        self.base_addr = base_addr

    def elaborate(self, platform):
        m = Module()
        m.submodules.bridge = AXI2Wishbone(self.axi,
                                           self.bus,
                                           base_addr=self.base_addr)
        m.submodules.sram = self.sram
        return m


def test_axi2wishbone_single_write_then_read():
    top = AXI2WishboneTop(data_width=32)

    def driver():
        resp, _id = yield from axi_write(top.axi,
                                         16,
                                         0xdeadbeef,
                                         0xf,
                                         size=2,
                                         txn_id=1)
        assert resp == 0
        data, resp, _last, _rid = yield from axi_read(top.axi,
                                                      16,
                                                      size=2,
                                                      txn_id=2)
        assert (data, resp) == (0xdeadbeef, 0)

    run_sim(top, driver)


def test_axi2wishbone_burst_write_then_read():
    # A multi-beat AXI burst is fragmented by AXI2AXILite into one AXI-Lite
    # (hence one Wishbone) access per beat; data must persist and read back.
    top = AXI2WishboneTop(data_width=32)
    beats = [(0x11111111, 0xf), (0x22222222, 0xf), (0x33333333, 0xf),
             (0x44444444, 0xf)]

    def driver():
        resp, _id = yield from axi_write_burst(top.axi,
                                               0,
                                               beats,
                                               size=2,
                                               txn_id=1)
        assert resp == 0
        rd = yield from axi_read_burst(top.axi, 0, size=2, length=3, txn_id=2)
        assert [b[0] for b in rd] == [b[0] for b in beats]
        assert [b[2] for b in rd] == [0, 0, 0, 1]

    run_sim(top, driver)


def test_axi2wishbone_narrow_strobe():
    # A narrow write (size < bus) is represented as a bus-aligned AXI-Lite
    # access; the strobe must reach the Wishbone SRAM byte-enable port so only
    # the addressed bytes are committed. Write the upper half of word 0
    # (addr 2, size=1, strb 0xc) and read the full word back.
    top = AXI2WishboneTop(data_width=32)

    def driver():
        resp, _id = yield from axi_write(top.axi,
                                         2,
                                         0xccdd0000,
                                         0xc,
                                         size=1,
                                         txn_id=1)
        assert resp == 0
        data, _resp, _l, _i = yield from axi_read(top.axi, 0, size=2, txn_id=2)
        assert data == 0xccdd0000

    run_sim(top, driver)


# ===========================================================================
# AXI2AXILite read response propagation
# ===========================================================================
class AXI2AXILiteTop(Elaboratable):

    def __init__(self, *, data_width=32, error_addr=0x40):
        self.axi = AXIInterface(addr_width=32,
                                data_width=data_width,
                                id_width=4)
        self.slave = AXILiteResponder(data_width=data_width,
                                      error_addr=error_addr)

    def elaborate(self, platform):
        m = Module()
        m.submodules.bridge = AXI2AXILite(self.axi, self.slave.bus)
        m.submodules.slave = self.slave
        return m


def test_axi2axilite_propagates_slverr_on_read():
    # AXI2AXILite forwards the AXI-Lite read response verbatim; an access to
    # the responder's error address must surface as resp=2 on the AXI R beat
    # and remain stable while the AXI master applies backpressure.
    top = AXI2AXILiteTop(data_width=32, error_addr=0x40)

    def driver():
        _d, resp, _l, _i = yield from axi_read(top.axi, 0, size=2, txn_id=1)
        assert resp == 0

        bus = top.axi
        yield bus.ar.bits.addr.eq(0x40)
        yield bus.ar.bits.size.eq(2)
        yield bus.ar.bits.len.eq(0)
        yield bus.ar.bits.burst.eq(1)
        yield bus.ar.bits.id.eq(2)
        yield bus.ar.valid.eq(1)
        yield
        while not (yield bus.ar.ready):
            yield
        yield bus.ar.valid.eq(0)

        while not (yield bus.r.valid):
            yield
        stalled = ((yield bus.r.bits.data), (yield bus.r.bits.resp),
                   (yield bus.r.bits.last), (yield bus.r.bits.id))
        assert stalled[1:] == (2, 1, 2)
        for _ in range(4):
            assert (yield bus.r.valid) == 1
            assert ((yield bus.r.bits.data), (yield bus.r.bits.resp),
                    (yield bus.r.bits.last), (yield bus.r.bits.id)) == stalled
            yield

        yield bus.r.ready.eq(1)
        yield
        yield bus.r.ready.eq(0)

    run_sim(top, driver)


def test_axi2axilite_propagates_and_holds_slverr_on_write():
    # Hold BREADY low after issuing a write to the responder's error address.
    # The bridge must preserve BID/BRESP/BVALID until the AXI response fires.
    top = AXI2AXILiteTop(data_width=32, error_addr=0x40)

    def driver():
        bus = top.axi
        yield bus.aw.bits.addr.eq(0x40)
        yield bus.aw.bits.size.eq(2)
        yield bus.aw.bits.len.eq(0)
        yield bus.aw.bits.burst.eq(1)
        yield bus.aw.bits.id.eq(7)
        yield bus.aw.valid.eq(1)
        yield
        while not (yield bus.aw.ready):
            yield
        yield bus.aw.valid.eq(0)

        yield bus.w.bits.data.eq(0xdeadbeef)
        yield bus.w.bits.strb.eq(0xf)
        yield bus.w.bits.last.eq(1)
        yield bus.w.valid.eq(1)
        yield
        while not (yield bus.w.ready):
            yield
        yield bus.w.valid.eq(0)

        while not (yield bus.b.valid):
            yield
        for _ in range(4):
            assert (yield bus.b.valid) == 1
            assert (yield bus.b.bits.resp) == 2
            assert (yield bus.b.bits.id) == 7
            yield

        yield bus.b.ready.eq(1)
        yield
        yield bus.b.ready.eq(0)

    run_sim(top, driver)
