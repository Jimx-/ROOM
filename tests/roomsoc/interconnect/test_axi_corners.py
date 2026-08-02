"""AXI interface contracts, edge-case behavior, and construction matrix.

Fills the three gaps identified in ``AXI_TEST_PLAN.md`` Phase 5:

  * ``AXIResp`` -- EXOKAY and DECERR propagation through ``AXI2AXILite``
    (previously only SLVERR by integer value), plus a round-trip of all four
    ``AXIResp`` enum members through ``AXIResponder`` using the enum symbols.
  * ``AXIBurst`` -- WRAP at the fragmenter and converter layer; FIXED through
    ``AXI2AXILite`` (FIXED through ``AXIFragmenter`` was already covered by
    Phase 2).
  * Construction / elaboration matrix -- every public ``Elaboratable`` in the
    AXI package is constructed and fully elaborated under parametrized
    ``(data_width, addr_width, id_width, version)`` combinations, including
    ``axi3`` vs ``axi4``.
"""

import gc

import pytest

from amaranth import *
from amaranth.sim import Simulator
from amaranth.utils import log2_int
from amaranth_soc.memory import MemoryMap

from roomsoc.interconnect.axi import (
    AXI2AXILite,
    AXI2Wishbone,
    AXIFragmenter,
    AXIInterface,
    AXILite2AXI,
    AXILiteConverter,
    AXILiteInterface,
    AXIStreamArbiter,
    AXIStreamConverter,
    AXIStreamDepacketizer,
    AXIStreamPacketizer,
    AXIInterconnectP2P,
    AXIInterconnectShared,
    TileLink2AXI,
    AXI2Tilelink,
    Wishbone2AXI,
)
from roomsoc.interconnect.axi.axi_full import AXIArbiter, AXIDecoder
from roomsoc.interconnect.axi.axi_lite import (
    AXILiteDownConverter,
    AXILite2Wishbone,
    Wishbone2AXILite,
)
from roomsoc.interconnect.axi.common import AXIBurst, AXIResp
from roomsoc.interconnect import tilelink
from roomsoc.interconnect import wishbone as wb

from axi_helpers import (AXIResponder, AXILiteResponder, run_sim, axi_read,
                         axi_read_burst, axi_write, axi_write_burst)


# ===========================================================================
# Helpers
# ===========================================================================

class AXILiteFixedRespSlave(Elaboratable):
    """Minimal AXI-Lite slave that unconditionally returns a fixed ``resp``.

    Used to exercise EXOKAY / DECERR propagation through ``AXI2AXILite``; the
    shared ``AXILiteResponder`` only produces OKAY (0) or SLVERR (2).
    """

    def __init__(self, *, data_width=32, addr_width=32, resp=0):
        self.bus = AXILiteInterface(addr_width=addr_width, data_width=data_width)
        self.fixed_resp = resp

    def elaborate(self, platform):
        m = Module()
        bus = self.bus
        r = self.fixed_resp

        with m.FSM(name="read"):
            with m.State("IDLE"):
                m.d.comb += bus.ar.ready.eq(1)
                with m.If(bus.ar.valid & bus.ar.ready):
                    m.next = "R"
            with m.State("R"):
                m.d.comb += [
                    bus.r.valid.eq(1),
                    bus.r.data.eq(0xDEAD_BEEF),
                    bus.r.resp.eq(r),
                ]
                with m.If(bus.r.ready):
                    m.next = "IDLE"

        aw_s = Signal()
        w_s = Signal()
        with m.FSM(name="write"):
            with m.State("IDLE"):
                m.d.comb += [
                    bus.aw.ready.eq(~aw_s),
                    bus.w.ready.eq(~w_s),
                ]
                with m.If(bus.aw.valid & bus.aw.ready):
                    m.d.sync += aw_s.eq(1)
                with m.If(bus.w.valid & bus.w.ready):
                    m.d.sync += w_s.eq(1)
                with m.If((aw_s | (bus.aw.valid & bus.aw.ready)) &
                          (w_s | (bus.w.valid & bus.w.ready))):
                    m.d.sync += [aw_s.eq(0), w_s.eq(0)]
                    m.next = "B"
            with m.State("B"):
                m.d.comb += [
                    bus.b.valid.eq(1),
                    bus.b.resp.eq(r),
                ]
                with m.If(bus.b.ready):
                    m.next = "IDLE"

        return m


class AXI2AXILiteRespTop(Elaboratable):

    def __init__(self, *, data_width=32, resp=0):
        self.axi = AXIInterface(addr_width=32,
                                data_width=data_width,
                                id_width=4)
        self.slave = AXILiteFixedRespSlave(data_width=data_width,
                                           addr_width=32,
                                           resp=resp)

    def elaborate(self, platform):
        m = Module()
        m.submodules.bridge = AXI2AXILite(self.axi, self.slave.bus)
        m.submodules.slave = self.slave
        return m


class FragmenterReadTop(Elaboratable):
    """AXIFragmenter + AXIResponder with identity memory (mem[i] = i)."""

    def __init__(self, *, data_width=32, max_size=8, version="axi4"):
        in_bus = AXIInterface(addr_width=32,
                              data_width=data_width,
                              id_width=4,
                              version=version)
        self.frag = AXIFragmenter(in_bus, max_size=max_size)
        self.ram = AXIResponder(addr_width=32,
                                data_width=data_width,
                                depth=256,
                                id_width=4,
                                init=list(range(256)),
                                version=version)

    @property
    def in_bus(self):
        return self.frag.in_bus

    def elaborate(self, platform):
        m = Module()
        m.submodules.frag = self.frag
        m.submodules.ram = self.ram
        m.d.comb += self.frag.out_bus.connect(self.ram.bus)
        return m


class AXI2AXILiteBurstTop(Elaboratable):
    """AXI2AXILite + AXILiteResponder for burst-type verification."""

    def __init__(self, *, data_width=32):
        self.axi = AXIInterface(addr_width=32,
                                data_width=data_width,
                                id_width=4)
        self.slave = AXILiteResponder(data_width=data_width, error_addr=0xF00)

    def elaborate(self, platform):
        m = Module()
        m.submodules.bridge = AXI2AXILite(self.axi, self.slave.bus)
        m.submodules.slave = self.slave
        return m


def _drive_burst(bus, addr, *, size, length, burst, txn_id=0):
    """Drive a manual AXI read burst with an arbitrary burst type."""
    yield bus.ar.bits.addr.eq(addr)
    yield bus.ar.bits.size.eq(size)
    yield bus.ar.bits.len.eq(length)
    yield bus.ar.bits.burst.eq(burst)
    yield bus.ar.bits.id.eq(txn_id)
    yield bus.ar.valid.eq(1)
    yield
    while not (yield bus.ar.ready):
        yield
    yield bus.ar.valid.eq(0)

    result = []
    yield bus.r.ready.eq(1)
    for _ in range(length + 1):
        while not (yield bus.r.valid):
            yield
        result.append(((yield bus.r.bits.data), (yield bus.r.bits.last)))
        yield
    yield bus.r.ready.eq(0)
    return result


# ===========================================================================
# AXI interface contracts
# ===========================================================================

def test_axi_interface_rejects_unknown_version():
    with pytest.raises(ValueError, match="version must be"):
        AXIInterface(version="axi5")


def test_axi_interface_memory_map_contract():
    bus = AXIInterface(addr_width=16, data_width=32, id_width=2)

    with pytest.raises(NotImplementedError, match="does not have a memory map"):
        _ = bus.memory_map
    with pytest.raises(TypeError, match="MemoryMap"):
        bus.memory_map = object()

    wrong_width = MemoryMap(addr_width=15, data_width=8)
    with pytest.raises(ValueError, match="address width"):
        bus.memory_map = wrong_width

    memory_map = MemoryMap(addr_width=16, data_width=8)
    bus.memory_map = memory_map
    assert bus.memory_map is memory_map
    with pytest.raises(ValueError, match="frozen"):
        memory_map.add_resource(object(), name="late", size=1)


# ===========================================================================
# AXIResp: EXOKAY / DECERR propagation
# ===========================================================================

@pytest.mark.parametrize("resp", [AXIResp.EXOKAY, AXIResp.DECERR])
def test_axi2axilite_propagates_resp_on_read(resp):
    # AXI2AXILite forwards the AXI-Lite RRESP verbatim; EXOKAY (1) and
    # DECERR (3) must reach the AXI R channel unchanged.
    top = AXI2AXILiteRespTop(data_width=32, resp=resp)

    def driver():
        _d, rresp, _l, _i = yield from axi_read(top.axi, 0, size=2, txn_id=3)
        assert rresp == int(resp)

    run_sim(top, driver)


@pytest.mark.parametrize("resp", [AXIResp.EXOKAY, AXIResp.DECERR])
def test_axi2axilite_propagates_resp_on_write(resp):
    # The write-response accumulator latches the first non-zero sub-burst
    # resp. With a single-beat write there is one sub-burst, so EXOKAY and
    # DECERR must appear on the AXI B channel.
    top = AXI2AXILiteRespTop(data_width=32, resp=resp)

    def driver():
        bresp, _id = yield from axi_write(top.axi,
                                          0,
                                          0x1234,
                                          0xf,
                                          size=2,
                                          txn_id=5)
        assert bresp == int(resp)

    run_sim(top, driver)


@pytest.mark.parametrize("resp", [AXIResp.EXOKAY, AXIResp.DECERR])
def test_axi2axilite_multibeat_read_propagates_each_resp(resp):
    # Exercise response forwarding after the first beat, including RLAST and
    # transaction-ID reconstruction on the final AXI beat.
    top = AXI2AXILiteRespTop(data_width=32, resp=resp)

    def driver():
        beats = yield from axi_read_burst(top.axi,
                                          0,
                                          size=2,
                                          length=2,
                                          txn_id=6)
        assert [beat[1] for beat in beats] == [int(resp)] * 3
        assert [beat[2] for beat in beats] == [0, 0, 1]
        assert [beat[3] for beat in beats] == [6, 6, 6]

    run_sim(top, driver)


@pytest.mark.parametrize("resp", [AXIResp.EXOKAY, AXIResp.DECERR])
def test_axi2axilite_multibeat_write_accumulates_resp(resp):
    # AXI-Lite returns one B response per converted beat. AXI must expose one
    # response for the original burst and preserve the first non-OKAY value.
    top = AXI2AXILiteRespTop(data_width=32, resp=resp)
    beats = [(0x11111111, 0xf), (0x22222222, 0xf), (0x33333333, 0xf)]

    def driver():
        bresp, txn_id = yield from axi_write_burst(top.axi,
                                                   0,
                                                   beats,
                                                   size=2,
                                                   txn_id=7)
        assert bresp == int(resp)
        assert txn_id == 7

    run_sim(top, driver)


@pytest.mark.parametrize("resp",
                         [AXIResp.OKAY, AXIResp.EXOKAY, AXIResp.SLVERR,
                          AXIResp.DECERR])
def test_axi_resp_all_enum_values_round_trip(resp):
    # Every AXIResp enum member must survive an end-to-end AXI transaction
    # through AXIResponder, using the enum symbol (not a magic integer).
    class Top(Elaboratable):
        def __init__(self):
            self.axi = AXIInterface(addr_width=32, data_width=32, id_width=4)
            self.ram = AXIResponder(addr_width=32,
                                    data_width=32,
                                    depth=16,
                                    id_width=4,
                                    r_resp=resp,
                                    b_resp=resp)

        def elaborate(self, platform):
            m = Module()
            m.submodules.ram = self.ram
            m.d.comb += self.axi.connect(self.ram.bus)
            return m

    top = Top()

    def driver():
        _d, rresp, _l, _i = yield from axi_read(top.axi, 0, size=2, txn_id=1)
        assert rresp == int(resp)
        bresp, _id = yield from axi_write(top.axi, 0, 0xAA, 0xf, size=2,
                                          txn_id=2)
        assert bresp == int(resp)

    run_sim(top, driver)


# ===========================================================================
# AXIBurst: WRAP at the fragmenter and converter layer
# ===========================================================================

@pytest.mark.filterwarnings("ignore::amaranth.hdl.ir.UnusedElaboratable")
@pytest.mark.parametrize("version,len_width,size_width,lock_width,has_wid", [
    pytest.param("axi3", 4, 4, 2, True, id="axi3"),
    pytest.param("axi4", 8, 3, 1, False, id="axi4"),
])
def test_fragmenter_preserves_axi_version(version, len_width, size_width,
                                          lock_width, has_wid):
    # AXI3 and AXI4 have different address-channel shapes, and only AXI3 has
    # WID. Merely elaborating mismatched Records does not reliably diagnose a
    # default-AXI4 output because common fields still connect successfully.
    in_bus = AXIInterface(addr_width=32,
                          data_width=32,
                          id_width=4,
                          version=version)
    frag = AXIFragmenter(in_bus, max_size=8)

    assert frag.out_bus.version == version
    for channel in (frag.out_bus.ar, frag.out_bus.aw):
        assert len(channel.bits.len) == len_width
        assert len(channel.bits.size) == size_width
        assert len(channel.bits.lock) == lock_width
    assert hasattr(frag.out_bus.w.bits, "id") is has_wid
    del frag, in_bus
    gc.collect()


def test_fragmenter_axi3_preserves_wid_across_split_write():
    # AXI3 carries a write ID on every W beat. A 4-beat write is split into
    # two 2-beat downstream bursts, but every forwarded beat must retain WID.
    top = FragmenterReadTop(data_width=32, max_size=8, version="axi3")
    downstream_wids = []
    downstream_last = []
    beats = [(0x10 + i, 0xf) for i in range(4)]

    def driver():
        yield top.in_bus.w.bits.id.eq(9)
        resp, txn_id = yield from axi_write_burst(top.in_bus,
                                                  0x40,
                                                  beats,
                                                  size=2,
                                                  txn_id=3)
        assert resp == int(AXIResp.OKAY)
        assert txn_id == 3

    def monitor():
        for _ in range(500):
            bus = top.frag.out_bus
            if (yield bus.w.valid) and (yield bus.w.ready):
                downstream_wids.append((yield bus.w.bits.id))
                downstream_last.append((yield bus.w.bits.last))
            yield
        assert downstream_wids == [9, 9, 9, 9]
        assert downstream_last == [0, 1, 0, 1]

    run_sim(top, driver, monitor)


def test_fragmenter_axi3_max_length_read():
    # AXI3's four-bit LEN permits 16 beats. Exercise its upper boundary and
    # verify the fragmenter emits four 4-beat transactions without exposing
    # intermediate RLAST assertions upstream.
    top = FragmenterReadTop(data_width=32,
                            max_size=16,
                            version="axi3")
    downstream_lengths = []

    def driver():
        beats = yield from _drive_burst(top.in_bus,
                                        0,
                                        size=2,
                                        length=15,
                                        burst=AXIBurst.INCR,
                                        txn_id=5)
        assert [data for data, _last in beats] == list(range(16))
        assert [last for _data, last in beats] == [0] * 15 + [1]

    def monitor():
        for _ in range(800):
            if (yield top.ram.ar_monitor.valid):
                downstream_lengths.append(
                    (yield top.ram.ar_monitor.bits.len))
            yield
        assert downstream_lengths == [3, 3, 3, 3]

    run_sim(top, driver, monitor)


def test_fragmenter_wrap_burst_wraps_address():
    # A 4-beat WRAP read (size=2, addr=0x4) through the fragmenter must
    # return data in wrap order: mem[1], mem[2], mem[3], mem[0].  The
    # fragmenter splits into 2-beat sub-bursts (max_size=8) but the wrap
    # boundary is preserved by mux_addr.
    top = FragmenterReadTop(data_width=32, max_size=8)

    def driver():
        beats = yield from _drive_burst(top.in_bus,
                                        0x4,
                                        size=2,
                                        length=3,
                                        burst=AXIBurst.WRAP,
                                        txn_id=1)
        assert [d for d, _ in beats] == [1, 2, 3, 0]
        assert [l for _, l in beats] == [0, 0, 0, 1]

    run_sim(top, driver)


def test_axi2axilite_fixed_burst_addresses_same_word():
    # FIXED bursts through AXI2AXILite: each beat becomes a single AXI-Lite
    # access to the same address. The responder's read_monitor must see the
    # identical address four times.
    top = AXI2AXILiteBurstTop(data_width=32)
    addrs = []

    def driver():
        yield from _drive_burst(top.axi,
                                0x20,
                                size=2,
                                length=3,
                                burst=AXIBurst.FIXED,
                                txn_id=1)

    def monitor():
        for _ in range(500):
            if (yield top.slave.read_monitor.valid):
                addrs.append((yield top.slave.read_monitor.bits.addr))
            yield
        assert addrs == [0x20] * 4

    run_sim(top, driver, monitor)


def test_axi2axilite_wrap_burst_wraps_address():
    # WRAP bursts through AXI2AXILite: the _AXFragmenter (max_size1=0)
    # produces one AXI-Lite beat per AXI beat with wrap addresses. Starting
    # at 0x4 with 4 beats at size=2, the addresses must be 0x4, 0x8, 0xC, 0x0.
    top = AXI2AXILiteBurstTop(data_width=32)
    addrs = []

    def driver():
        yield from _drive_burst(top.axi,
                                0x4,
                                size=2,
                                length=3,
                                burst=AXIBurst.WRAP,
                                txn_id=1)

    def monitor():
        for _ in range(500):
            if (yield top.slave.read_monitor.valid):
                addrs.append((yield top.slave.read_monitor.bits.addr))
            yield
        assert addrs == [0x4, 0x8, 0xC, 0x0]

    run_sim(top, driver, monitor)


# ===========================================================================
# Constructor and fabric validation
# ===========================================================================

@pytest.mark.filterwarnings("ignore::amaranth.hdl.ir.UnusedElaboratable")
@pytest.mark.parametrize("master_args,slave_args,message", [
    ({"addr_width": 32, "data_width": 64},
     {"addr_width": 16, "data_width": 32}, "address widths must match"),
    ({"addr_width": 32, "data_width": 32},
     {"addr_width": 32, "data_width": 32}, "Master must be wider"),
    ({"addr_width": 32, "data_width": 48},
     {"addr_width": 32, "data_width": 32}, "ratio must be integral"),
    ({"addr_width": 32, "data_width": 48},
     {"addr_width": 32, "data_width": 16}, "power of two"),
])
def test_axilite_downconverter_rejects_invalid_geometry(master_args,
                                                        slave_args,
                                                        message):
    master = AXILiteInterface(**master_args)
    slave = AXILiteInterface(**slave_args)
    with pytest.raises(ValueError, match=message):
        AXILiteDownConverter(master, slave)
    del master, slave
    gc.collect()


@pytest.mark.filterwarnings("ignore::amaranth.hdl.ir.UnusedElaboratable")
def test_axi_arbiter_rejects_incompatible_initiators():
    arbiter = AXIArbiter(addr_width=32, data_width=32, id_width=4)

    with pytest.raises(TypeError, match="AXIInterface"):
        arbiter.add(AXILiteInterface(addr_width=32, data_width=32))
    with pytest.raises(ValueError, match="address width"):
        arbiter.add(AXIInterface(addr_width=16, data_width=32, id_width=4))
    with pytest.raises(ValueError, match="data width"):
        arbiter.add(AXIInterface(addr_width=32, data_width=64, id_width=4))

    del arbiter
    gc.collect()


@pytest.mark.filterwarnings("ignore::amaranth.hdl.ir.UnusedElaboratable")
def test_axi_decoder_rejects_invalid_subordinates():
    decoder = AXIDecoder(addr_width=32, data_width=32, id_width=4)

    with pytest.raises(TypeError, match="AXIInterface"):
        decoder.add(AXILiteInterface(addr_width=32, data_width=32))

    wide = AXIInterface(addr_width=12, data_width=64, id_width=4)
    wide.memory_map = MemoryMap(addr_width=12, data_width=8)
    with pytest.raises(ValueError, match="data width"):
        decoder.add(wide)

    # Sparse windows explicitly permit differing data widths.
    assert decoder.add(wide, sparse=True) == (0, 1 << 12, 1)

    del decoder, wide
    gc.collect()


# ===========================================================================
# Construction / elaboration matrix
# ===========================================================================

_MATRIX = [
    pytest.param(32, 32, 4, "axi4", id="32b-id4-axi4"),
    pytest.param(64, 32, 8, "axi4", id="64b-id8-axi4"),
    pytest.param(32, 32, 4, "axi3", id="32b-id4-axi3"),
]


def _elaborates(dut):
    """Force full elaboration; any construction error propagates as failure."""
    m = Module()
    m.submodules.dut = dut
    _guard = Signal()
    m.d.sync += _guard.eq(0)
    sim = Simulator(m)
    sim.add_clock(1e-6)
    sim.run()
    del dut, m
    gc.collect()


@pytest.mark.filterwarnings("ignore::amaranth.hdl.ir.UnusedElaboratable")
@pytest.mark.parametrize("data_width,addr_width,id_width,version", _MATRIX)
def test_construct_axi_lite_bridges(data_width, addr_width, id_width, version):
    axi = AXIInterface(addr_width=addr_width,
                       data_width=data_width,
                       id_width=id_width,
                       version=version)
    axil = AXILiteInterface(addr_width=addr_width, data_width=data_width)
    _elaborates(AXI2AXILite(axi, axil))

    axi2 = AXIInterface(addr_width=addr_width,
                        data_width=data_width,
                        id_width=id_width,
                        version=version)
    axil2 = AXILiteInterface(addr_width=addr_width, data_width=data_width)
    _elaborates(AXILite2AXI(axil2, axi2))

    axi3 = AXIInterface(addr_width=addr_width,
                        data_width=data_width,
                        id_width=id_width,
                        version=version)
    _elaborates(AXIFragmenter(axi3, max_size=data_width))


@pytest.mark.filterwarnings("ignore::amaranth.hdl.ir.UnusedElaboratable")
@pytest.mark.parametrize("data_width,addr_width,id_width,version", _MATRIX)
def test_construct_wishbone_bridges(data_width, addr_width, id_width, version):
    axi = AXIInterface(addr_width=addr_width,
                       data_width=data_width,
                       id_width=id_width,
                       version=version)
    wbus = wb.Interface(addr_width=addr_width,
                        data_width=data_width,
                        granularity=8)
    _elaborates(AXI2Wishbone(axi, wbus))

    axi2 = AXIInterface(addr_width=addr_width,
                        data_width=data_width,
                        id_width=id_width,
                        version=version)
    wbus2 = wb.Interface(addr_width=addr_width,
                         data_width=data_width,
                         granularity=8)
    _elaborates(Wishbone2AXI(wbus2, axi2))

    axil = AXILiteInterface(addr_width=addr_width, data_width=data_width)
    wbus3 = wb.Interface(addr_width=addr_width,
                         data_width=data_width,
                         granularity=8)
    _elaborates(AXILite2Wishbone(axil, wbus3))

    axil2 = AXILiteInterface(addr_width=addr_width, data_width=data_width)
    wbus4 = wb.Interface(addr_width=addr_width,
                         data_width=data_width,
                         granularity=8)
    _elaborates(Wishbone2AXILite(wbus4, axil2))


@pytest.mark.filterwarnings("ignore::amaranth.hdl.ir.UnusedElaboratable")
@pytest.mark.parametrize("data_width,addr_width", [
    pytest.param(32, 32, id="32-to-8"),
    pytest.param(64, 32, id="64-to-8"),
])
def test_construct_axilite_converter(data_width, addr_width):
    # Up- and down-conversion between wide and 8-bit AXI-Lite.
    wide = AXILiteInterface(addr_width=addr_width, data_width=data_width)
    narrow = AXILiteInterface(addr_width=addr_width, data_width=8)
    _elaborates(AXILiteConverter(wide, narrow))

    wide2 = AXILiteInterface(addr_width=addr_width, data_width=data_width)
    narrow2 = AXILiteInterface(addr_width=addr_width, data_width=8)
    _elaborates(AXILiteConverter(narrow2, wide2))


@pytest.mark.filterwarnings("ignore::amaranth.hdl.ir.UnusedElaboratable")
@pytest.mark.parametrize("data_width,addr_width,id_width,version", _MATRIX)
def test_construct_fabric(data_width, addr_width, id_width, version):
    _elaborates(
        AXIArbiter(addr_width=addr_width,
                   data_width=data_width,
                   id_width=id_width))
    _elaborates(
        AXIDecoder(addr_width=addr_width,
                   data_width=data_width,
                   id_width=id_width))

    m_axi = AXIInterface(addr_width=addr_width,
                         data_width=data_width,
                         id_width=id_width,
                         version=version)
    s_axi = AXIInterface(addr_width=addr_width,
                         data_width=data_width,
                         id_width=id_width,
                         version=version)
    _elaborates(AXIInterconnectP2P(m_axi, s_axi))


@pytest.mark.filterwarnings("ignore::amaranth.hdl.ir.UnusedElaboratable")
@pytest.mark.parametrize("data_width,addr_width,id_width", [
    pytest.param(32, 32, 4, id="32b-id4"),
    pytest.param(64, 32, 8, id="64b-id8"),
])
def test_construct_interconnect_shared(data_width, addr_width, id_width):
    from amaranth_soc.memory import MemoryMap

    class _Region:
        def __init__(self, origin):
            self.origin = origin

    masters = [
        AXIInterface(addr_width=addr_width,
                     data_width=data_width,
                     id_width=id_width) for _ in range(2)
    ]
    slaves = []
    for origin in (0x0000, 0x1000):
        aw = log2_int(0x1000)
        bus = AXIInterface(addr_width=aw,
                           data_width=data_width,
                           id_width=id_width)
        bus.memory_map = MemoryMap(data_width=8, addr_width=aw)
        slaves.append((_Region(origin), bus))
    _elaborates(
        AXIInterconnectShared(addr_width=addr_width,
                              data_width=data_width,
                              masters=masters,
                              slaves=slaves))


@pytest.mark.filterwarnings("ignore::amaranth.hdl.ir.UnusedElaboratable")
@pytest.mark.parametrize("data_width", [8, 32, 64])
def test_construct_stream_components(data_width):
    layout = [("field", 16)]
    _elaborates(AXIStreamPacketizer(Record, layout, data_width=data_width))
    _elaborates(AXIStreamDepacketizer(Record, layout, data_width=data_width))
    _elaborates(AXIStreamArbiter(3, data_width=data_width))
    _elaborates(AXIStreamConverter(dw_from=data_width, dw_to=max(8, data_width // 2)))
    if data_width <= 32:
        _elaborates(AXIStreamConverter(dw_from=data_width, dw_to=data_width * 2))


@pytest.mark.filterwarnings("ignore::amaranth.hdl.ir.UnusedElaboratable")
@pytest.mark.parametrize("data_width,addr_width,id_width,version", _MATRIX)
def test_construct_tilelink_bridges(data_width, addr_width, id_width, version):
    # AXI2Tilelink needs source_id_width >= id_width + added_bits, where
    # added_bits = (max_flights-1).bit_length() + 1.  For max_flights=2 that
    # is 2, so source_id_width = id_width + 2.
    axi = AXIInterface(addr_width=addr_width,
                       data_width=data_width,
                       id_width=id_width,
                       version=version)
    tl = tilelink.Interface(addr_width=addr_width,
                            data_width=data_width,
                            source_id_width=id_width + 2)
    _elaborates(AXI2Tilelink(axi, tl, max_flights=2))

    # TileLink2AXI needs id_width >= source_id_width + has_bce.
    tl2 = tilelink.Interface(addr_width=addr_width,
                             data_width=data_width,
                             source_id_width=id_width)
    axi2 = AXIInterface(addr_width=addr_width,
                        data_width=data_width,
                        id_width=id_width,
                        version=version)
    _elaborates(TileLink2AXI(tl2, axi2, max_flights=2))
