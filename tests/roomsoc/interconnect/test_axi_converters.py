import pytest

from amaranth import *
from amaranth.sim import Simulator

from roomsoc.interconnect.axi import (AXI2AXILite, AXIBurst2Beat,
                                      AXIConverter, AXIInterface,
                                      AXILiteConverter, AXILiteInterface)
from roomsoc.interconnect.axi.common import AXIBurst, AXIResp

from axi_helpers import (AXILiteResponder, AXIResponder, axi_read, axi_read_burst,
                         axi_write, axi_write_burst, axilite_write_split, run_sim)


class AXI2AXILiteTop(Elaboratable):

    def __init__(self,
                 axi_data_width=32,
                 version='axi4',
                 byte_addr_data=False,
                 error_addr=0x0c):
        self.axi = AXIInterface(addr_width=32,
                                data_width=axi_data_width,
                                id_width=3,
                                version=version)
        self.slave = AXILiteResponder(data_width=axi_data_width,
                                      byte_addr_data=byte_addr_data,
                                      error_addr=error_addr)

    def elaborate(self, platform):
        m = Module()
        m.submodules.bridge = AXI2AXILite(self.axi, self.slave.bus)
        m.submodules.slave = self.slave
        return m


class AXILiteConverterTop(Elaboratable):

    def __init__(self, master_dw=64, slave_dw=32):
        self.master = AXILiteInterface(addr_width=32, data_width=master_dw)
        self.slave = AXILiteResponder(data_width=slave_dw)

    def elaborate(self, platform):
        m = Module()
        m.submodules.converter = AXILiteConverter(self.master, self.slave.bus)
        m.submodules.slave = self.slave
        return m


class AXILiteUpConverterTop(Elaboratable):

    def __init__(self, master_dw=32, slave_dw=64, error_addr=0x10):
        self.master = AXILiteInterface(addr_width=32, data_width=master_dw)
        self.slave = AXILiteResponder(data_width=slave_dw,
                                      byte_addr_data=True,
                                      error_addr=error_addr)

    def elaborate(self, platform):
        m = Module()
        m.submodules.converter = AXILiteConverter(self.master, self.slave.bus)
        m.submodules.slave = self.slave
        return m


class AXIToNarrowAXILiteTop(Elaboratable):

    def __init__(self):
        self.axi = AXIInterface(addr_width=32, data_width=64, id_width=3)
        self.slave = AXILiteResponder(data_width=32)

    def elaborate(self, platform):
        m = Module()
        wide_axil = AXILiteInterface(addr_width=32, data_width=64)
        m.submodules.protocol_converter = AXI2AXILite(self.axi, wide_axil)
        m.submodules.width_converter = AXILiteConverter(
            wide_axil, self.slave.bus)
        m.submodules.slave = self.slave
        return m


class AXIConverterTop(Elaboratable):

    def __init__(self,
                 master_dw,
                 slave_dw,
                 *,
                 init=None,
                 r_resp=AXIResp.OKAY,
                 b_resp=AXIResp.OKAY):
        self.master = AXIInterface(addr_width=16,
                                   data_width=master_dw,
                                   id_width=3)
        self.slave = AXIResponder(addr_width=16,
                                  data_width=slave_dw,
                                  id_width=3,
                                  depth=64,
                                  init=init,
                                  r_resp=r_resp,
                                  b_resp=b_resp)

    def elaborate(self, platform):
        m = Module()
        m.submodules.converter = AXIConverter(self.master, self.slave.bus)
        m.submodules.slave = self.slave
        return m


@pytest.mark.parametrize(('burst', 'expected'), [
    (AXIBurst.FIXED, [0x08, 0x08, 0x08, 0x08]),
    (AXIBurst.INCR, [0x08, 0x0c, 0x10, 0x14]),
    (AXIBurst.WRAP, [0x08, 0x0c, 0x00, 0x04]),
])
def test_axi_burst2beat_addresses_and_backpressure(burst, expected):
    bus = AXIInterface(addr_width=8, data_width=32, id_width=3)
    dut = AXIBurst2Beat(bus.aw)

    def driver():
        yield bus.aw.bits.addr.eq(0x08)
        yield bus.aw.bits.len.eq(3)
        yield bus.aw.bits.size.eq(2)
        yield bus.aw.bits.burst.eq(burst)
        yield bus.aw.bits.id.eq(5)
        yield bus.aw.valid.eq(1)
        yield dut.out.ready.eq(1)
        yield

        beats = []
        stalled = None
        for cycle in range(20):
            valid = (yield dut.out.valid)
            beat = ((yield dut.out.bits.addr),
                    (yield dut.out.bits.id),
                    (yield dut.out.bits.first),
                    (yield dut.out.bits.last))
            ready = (yield dut.out.ready)

            if stalled is not None:
                assert valid
                assert beat == stalled
            if valid and ready:
                beats.append(beat)
                if beat[3]:
                    yield bus.aw.valid.eq(0)
            stalled = beat if valid and not ready else None
            yield dut.out.ready.eq(cycle not in (1, 2, 6))
            yield
            if len(beats) == 4:
                break

        assert [beat[0] for beat in beats] == expected
        assert [beat[1] for beat in beats] == [5] * 4
        assert [beat[2] for beat in beats] == [1, 0, 0, 0]
        assert [beat[3] for beat in beats] == [0, 0, 0, 1]

    run_sim(dut, driver)


def test_axi2axilite_protocol_conversion():
    top = AXI2AXILiteTop(axi_data_width=32)
    reads = []
    writes = []

    def driver():
        assert (yield from axi_read(top.axi, 4, size=2,
                                    txn_id=1)) == (0xa0000001, 0, 1, 1)
        assert (yield from axi_read_burst(top.axi,
                                          0x20,
                                          size=2,
                                          length=1,
                                          txn_id=7)) == [
                                              (0xa0000008, 0, 0, 7),
                                              (0xa0000009, 0, 1, 7),
                                          ]
        assert (yield from axi_write(top.axi,
                                     4,
                                     0xdeadbeef,
                                     0xf,
                                     size=2,
                                     txn_id=2)) == (0, 2)
        assert (yield from axi_write_burst(top.axi,
                                           0x30, [(0x01234567, 0xf),
                                                  (0x89abcdef, 0xf)],
                                           size=2,
                                           txn_id=3)) == (0, 3)

    def monitor():
        for _ in range(200):
            if (yield top.slave.read_monitor.valid):
                reads.append((yield top.slave.read_monitor.bits.addr))
            if (yield top.slave.write_monitor.valid):
                writes.append(((yield top.slave.write_monitor.bits.addr),
                               (yield top.slave.write_monitor.bits.data),
                               (yield top.slave.write_monitor.bits.strb)))
            yield

        assert reads == [4, 0x20, 0x24]
        assert writes == [(4, 0xdeadbeef, 0xf), (0x30, 0x01234567, 0xf),
                          (0x34, 0x89abcdef, 0xf)]

    sim = Simulator(top)
    sim.add_clock(1e-6)
    sim.add_sync_process(driver)
    sim.add_sync_process(monitor)
    sim.run()


@pytest.mark.filterwarnings("ignore::amaranth.hdl.ir.UnusedElaboratable")
def test_axi2axilite_rejects_width_mismatch():
    import gc

    axi = AXIInterface(addr_width=32, data_width=64, id_width=3)
    axil = AXILiteInterface(addr_width=32, data_width=32)
    with pytest.raises(ValueError, match="data widths must match"):
        AXI2AXILite(axi, axil)

    axil_addr = AXILiteInterface(addr_width=16, data_width=64)
    with pytest.raises(ValueError, match="address widths must match"):
        AXI2AXILite(axi, axil_addr)

    # Rejected bridges are never elaborated; drop references and collect them
    # here so amaranth emits the UnusedElaboratable warning within this
    # (filtered) test instead of leaking it into unrelated tests at shutdown.
    del axi, axil, axil_addr
    gc.collect()


def test_axi2axilite_axi3_variant():
    # The bridge must work with an AXI3 master: 4-bit len field and a WID on
    # the W channel. Exercise multi-beat read/write bursts within AXI3 limits.
    top = AXI2AXILiteTop(axi_data_width=32, version='axi3')
    assert top.axi.version == 'axi3'
    assert len(top.axi.ar.bits.len) == 4
    assert len(top.axi.aw.bits.len) == 4
    assert hasattr(top.axi.w.bits, 'id')
    reads = []
    writes = []

    def driver():
        assert (yield from axi_read_burst(top.axi,
                                          0x20,
                                          size=2,
                                          length=1,
                                          txn_id=5)) == [
                                              (0xa0000008, 0, 0, 5),
                                              (0xa0000009, 0, 1, 5),
                                          ]
        assert (yield from axi_write_burst(top.axi,
                                           0x30, [(0x01234567, 0xf),
                                                  (0x89abcdef, 0xf)],
                                           size=2,
                                           txn_id=6)) == (0, 6)

    def monitor():
        for _ in range(200):
            if (yield top.slave.read_monitor.valid):
                reads.append((yield top.slave.read_monitor.bits.addr))
            if (yield top.slave.write_monitor.valid):
                writes.append(((yield top.slave.write_monitor.bits.addr),
                               (yield top.slave.write_monitor.bits.data),
                               (yield top.slave.write_monitor.bits.strb)))
            yield

        assert reads == [0x20, 0x24]
        assert writes == [(0x30, 0x01234567, 0xf), (0x34, 0x89abcdef, 0xf)]

    sim = Simulator(top)
    sim.add_clock(1e-6)
    sim.add_sync_process(driver)
    sim.add_sync_process(monitor)
    sim.run()


def test_axi2axilite_narrow_byte_lanes():
    # Standalone (no downstream width converter): a narrow AXI access must map
    # to a single bus-aligned AXI-Lite transfer while the AXI byte lanes keep
    # their requested placement. Previously only checked through the composite
    # AXI -> wide-AXILite -> narrow-AXILite topology.
    top = AXI2AXILiteTop(axi_data_width=64, byte_addr_data=True)
    reads = []
    writes = []

    def driver():
        # Narrow read at addr 4 (size=2): AXI-Lite address is bus-aligned to 0
        # and the full word is returned; bytes [63:32] hold lane 4.
        data, resp, last, txn_id = yield from axi_read(top.axi,
                                                       4,
                                                       size=2,
                                                       txn_id=1)
        assert (data, resp, last, txn_id) == (0x0706050403020100, 0, 1, 1)
        # Narrow write at addr 4: strobe and data retain their upper-lane
        # placement on the AXI-Lite side.
        assert (yield from axi_write(top.axi,
                                     4,
                                     0xdeadbeef00000000,
                                     0xf0,
                                     size=2,
                                     txn_id=2)) == (0, 2)

    def monitor():
        for _ in range(100):
            if (yield top.slave.read_monitor.valid):
                reads.append((yield top.slave.read_monitor.bits.addr))
            if (yield top.slave.write_monitor.valid):
                writes.append(((yield top.slave.write_monitor.bits.addr),
                               (yield top.slave.write_monitor.bits.data),
                               (yield top.slave.write_monitor.bits.strb)))
            yield

        assert reads == [0]
        assert writes == [(0, 0xdeadbeef00000000, 0xf0)]

    sim = Simulator(top)
    sim.add_clock(1e-6)
    sim.add_sync_process(driver)
    sim.add_sync_process(monitor)
    sim.run()


def test_axilite_downconverter():
    top = AXILiteConverterTop()
    reads = []
    writes = []

    def driver():
        data, resp = yield from top.master.read(0)
        assert (data, resp) == (0xa0000001a0000000, 0)
        data, resp = yield from top.master.read(4)
        assert (data, resp) == (0xa0000002a0000001, 0)
        data, resp = yield from top.master.read(8)
        assert (data, resp) == (0xa0000003a0000002, 2)
        assert (yield from top.master.write(0, 0x1122334455667788, 0xff)) == 0
        assert (yield from top.master.write(0, 0xdeadbeef00000000, 0xf0)) == 0
        assert (yield from top.master.write(8, 0x1122334455667788, 0xff)) == 2

    def monitor():
        for _ in range(200):
            if (yield top.slave.read_monitor.valid):
                reads.append((yield top.slave.read_monitor.bits.addr))
            if (yield top.slave.write_monitor.valid):
                writes.append(((yield top.slave.write_monitor.bits.addr),
                               (yield top.slave.write_monitor.bits.data),
                               (yield top.slave.write_monitor.bits.strb)))
            yield

        assert reads == [0, 4, 4, 8, 8, 12]
        assert writes == [(0, 0x55667788, 0xf), (4, 0x11223344, 0xf),
                          (4, 0xdeadbeef, 0xf), (8, 0x55667788, 0xf),
                          (12, 0x11223344, 0xf)]

    sim = Simulator(top)
    sim.add_clock(1e-6)
    sim.add_sync_process(driver)
    sim.add_sync_process(monitor)
    sim.run()


def test_axilite_downconverter_ratio_4():
    top = AXILiteConverterTop(master_dw=64, slave_dw=16)
    reads = []
    writes = []

    def driver():
        data, resp = yield from top.master.read(0)
        assert (data, resp) == (0xa001a001a000a000, 0)
        data, resp = yield from top.master.read(8)
        assert (data, resp) == (0xa003a003a002a002, 2)
        assert (yield from top.master.write(0, 0x1122334455667788, 0xff)) == 0
        assert (yield from top.master.write(0, 0x1122334455667788, 0xf0)) == 0
        assert (yield from top.master.write(0x0c, 0x1122334455667788, 0x0f)) == 2

    def monitor():
        for _ in range(400):
            if (yield top.slave.read_monitor.valid):
                reads.append((yield top.slave.read_monitor.bits.addr))
            if (yield top.slave.write_monitor.valid):
                writes.append(((yield top.slave.write_monitor.bits.addr),
                               (yield top.slave.write_monitor.bits.data),
                               (yield top.slave.write_monitor.bits.strb)))
            yield

        assert reads == [0, 2, 4, 6, 8, 10, 12, 14]
        assert writes == [(0, 0x7788, 0x3), (2, 0x5566, 0x3),
                          (4, 0x3344, 0x3), (6, 0x1122, 0x3),
                          (4, 0x3344, 0x3), (6, 0x1122, 0x3),
                          (12, 0x7788, 0x3), (14, 0x5566, 0x3)]

    sim = Simulator(top)
    sim.add_clock(1e-6)
    sim.add_sync_process(driver)
    sim.add_sync_process(monitor)
    sim.run()


def test_axilite_downconverter_all_strobes_zero():
    top = AXILiteConverterTop(master_dw=64, slave_dw=32)
    writes = []
    saw_slave_aw = [False]
    saw_slave_w = [False]

    def driver():
        # A write whose strobes are all zero must still complete, but the
        # converter must issue no AW or W at all to the narrow slave.
        assert (yield from top.master.write(0, 0x1122334455667788, 0x00)) == 0

    def monitor():
        for _ in range(100):
            if (yield top.slave.bus.aw.valid):
                saw_slave_aw[0] = True
            if (yield top.slave.bus.w.valid):
                saw_slave_w[0] = True
            if (yield top.slave.write_monitor.valid):
                writes.append(((yield top.slave.write_monitor.bits.addr),
                               (yield top.slave.write_monitor.bits.data),
                               (yield top.slave.write_monitor.bits.strb)))
            yield

        assert writes == []
        assert not saw_slave_aw[0]
        assert not saw_slave_w[0]

    sim = Simulator(top)
    sim.add_clock(1e-6)
    sim.add_sync_process(driver)
    sim.add_sync_process(monitor)
    sim.run()


def test_axilite_downconverter_split_aw_w():
    top = AXILiteConverterTop(master_dw=64, slave_dw=32)
    writes = []

    def driver():
        assert (yield from axilite_write_split(top.master, 0,
                                               0x1122334455667788,
                                               0xff)) == 0
        assert (yield from axilite_write_split(top.master, 0,
                                               0x1122334455667788,
                                               0x0f)) == 0

    def monitor():
        for _ in range(200):
            if (yield top.slave.write_monitor.valid):
                writes.append(((yield top.slave.write_monitor.bits.addr),
                               (yield top.slave.write_monitor.bits.data),
                               (yield top.slave.write_monitor.bits.strb)))
            yield

        assert writes == [(0, 0x55667788, 0xf), (4, 0x11223344, 0xf),
                          (0, 0x55667788, 0xf)]

    sim = Simulator(top)
    sim.add_clock(1e-6)
    sim.add_sync_process(driver)
    sim.add_sync_process(monitor)
    sim.run()


def test_axilite_upconverter():
    top = AXILiteUpConverterTop(master_dw=32, slave_dw=64, error_addr=0x10)
    reads = []
    writes = []

    def driver():
        # Both lanes of slave word 0, then word 8, then an error address.
        data, resp = yield from top.master.read(0)
        assert (data, resp) == (0x03020100, 0)
        data, resp = yield from top.master.read(4)
        assert (data, resp) == (0x07060504, 0)
        data, resp = yield from top.master.read(8)
        assert (data, resp) == (0x0b0a0908, 0)
        data, resp = yield from top.master.read(12)
        assert (data, resp) == (0x0f0e0d0c, 0)
        data, resp = yield from top.master.read(0x10)
        assert (data, resp) == (0x13121110, 2)
        # Writes steer data/strobe into the addressed lane only.
        assert (yield from top.master.write(0, 0x11223344, 0xf)) == 0
        assert (yield from top.master.write(4, 0xdeadbeef, 0xf)) == 0
        assert (yield from top.master.write(8, 0xaabbccdd, 0xa)) == 0
        assert (yield from top.master.write(12, 0xeeff0011, 0xf)) == 0
        assert (yield from top.master.write(0x10, 0xcafef00d, 0xf)) == 2

    def monitor():
        for _ in range(400):
            if (yield top.slave.read_monitor.valid):
                reads.append((yield top.slave.read_monitor.bits.addr))
            if (yield top.slave.write_monitor.valid):
                writes.append(((yield top.slave.write_monitor.bits.addr),
                               (yield top.slave.write_monitor.bits.data),
                               (yield top.slave.write_monitor.bits.strb)))
            yield

        assert reads == [0, 0, 8, 8, 0x10]
        assert writes == [(0, 0x0000000011223344, 0x0f),
                          (0, 0xdeadbeef00000000, 0xf0),
                          (8, 0x00000000aabbccdd, 0x0a),
                          (8, 0xeeff001100000000, 0xf0),
                          (0x10, 0x00000000cafef00d, 0x0f)]

    sim = Simulator(top)
    sim.add_clock(1e-6)
    sim.add_sync_process(driver)
    sim.add_sync_process(monitor)
    sim.run()


def test_axilite_upconverter_ratio_4():
    top = AXILiteUpConverterTop(master_dw=16, slave_dw=64, error_addr=0x10)
    reads = []
    writes = []

    def driver():
        # All four lanes of slave word 0 are independently selectable.
        data, resp = yield from top.master.read(0)
        assert (data, resp) == (0x0100, 0)
        data, resp = yield from top.master.read(2)
        assert (data, resp) == (0x0302, 0)
        data, resp = yield from top.master.read(4)
        assert (data, resp) == (0x0504, 0)
        data, resp = yield from top.master.read(6)
        assert (data, resp) == (0x0706, 0)
        data, resp = yield from top.master.read(8)
        assert (data, resp) == (0x0908, 0)
        data, resp = yield from top.master.read(0x10)
        assert (data, resp) == (0x1110, 2)
        assert (yield from top.master.write(0, 0x1234, 0x3)) == 0
        assert (yield from top.master.write(2, 0x5678, 0x3)) == 0
        assert (yield from top.master.write(4, 0x9abc, 0x3)) == 0
        assert (yield from top.master.write(6, 0xdef0, 0x3)) == 0

    def monitor():
        for _ in range(400):
            if (yield top.slave.read_monitor.valid):
                reads.append((yield top.slave.read_monitor.bits.addr))
            if (yield top.slave.write_monitor.valid):
                writes.append(((yield top.slave.write_monitor.bits.addr),
                               (yield top.slave.write_monitor.bits.data),
                               (yield top.slave.write_monitor.bits.strb)))
            yield

        assert reads == [0, 0, 0, 0, 8, 0x10]
        assert writes == [(0, 0x0000000000001234, 0x03),
                          (0, 0x0000000056780000, 0x0c),
                          (0, 0x00009abc00000000, 0x30),
                          (0, 0xdef0000000000000, 0xc0)]

    sim = Simulator(top)
    sim.add_clock(1e-6)
    sim.add_sync_process(driver)
    sim.add_sync_process(monitor)
    sim.run()


def test_axi_to_narrow_axilite_preserves_narrow_byte_lanes():
    top = AXIToNarrowAXILiteTop()
    reads = []
    writes = []

    def driver():
        data, resp, last, txn_id = yield from axi_read(top.axi,
                                                       4,
                                                       size=2,
                                                       txn_id=1)
        assert (data >> 32, resp, last, txn_id) == (0xa0000001, 0, 1, 1)
        assert (yield from axi_write(top.axi,
                                     4,
                                     0xdeadbeef00000000,
                                     0xf0,
                                     size=2,
                                     txn_id=2)) == (0, 2)

    def monitor():
        for _ in range(100):
            if (yield top.slave.read_monitor.valid):
                reads.append((yield top.slave.read_monitor.bits.addr))
            if (yield top.slave.write_monitor.valid):
                writes.append(((yield top.slave.write_monitor.bits.addr),
                               (yield top.slave.write_monitor.bits.data),
                               (yield top.slave.write_monitor.bits.strb)))
            yield

        assert reads == [0, 4]
        assert writes == [(4, 0xdeadbeef, 0xf)]

    sim = Simulator(top)
    sim.add_clock(1e-6)
    sim.add_sync_process(driver)
    sim.add_sync_process(monitor)
    sim.run()


@pytest.mark.filterwarnings("ignore::amaranth.hdl.ir.UnusedElaboratable")
def test_axi_converter_rejects_fractional_width_ratios():
    import gc

    with pytest.raises(ValueError, match="down-converter ratio"):
        Simulator(AXIConverter(AXIInterface(data_width=48),
                               AXIInterface(data_width=32)))
    with pytest.raises(ValueError, match="up-converter ratio"):
        Simulator(AXIConverter(AXIInterface(data_width=32),
                               AXIInterface(data_width=48)))

    gc.collect()


def test_axi_converter_equal_width_passthrough():
    top = AXIConverterTop(32, 32, init=[0x12345678])
    reads = []
    writes = []

    def driver():
        assert (yield from axi_read(top.master, 0, size=2,
                                    txn_id=5)) == (0x12345678, 0, 1, 5)
        assert (yield from axi_write(top.master,
                                     4,
                                     0xdeadbeef,
                                     0x5,
                                     size=2,
                                     txn_id=6)) == (0, 6)

    def monitor():
        for _ in range(100):
            if (yield top.slave.ar_monitor.valid):
                reads.append(((yield top.slave.ar_monitor.bits.addr),
                              (yield top.slave.ar_monitor.bits.len),
                              (yield top.slave.ar_monitor.bits.size)))
            if (yield top.slave.w_monitor.valid):
                writes.append(((yield top.slave.w_monitor.bits.addr),
                               (yield top.slave.w_monitor.bits.data),
                               (yield top.slave.w_monitor.bits.strb)))
            yield

        assert reads == [(0, 0, 2)]
        assert writes == [(4, 0xdeadbeef, 0x5)]

    run_sim(top, driver, monitor)


def test_axi_downconverter_burst_data_strobes_and_responses():
    top = AXIConverterTop(64,
                          32,
                          init=[0x89abcdef, 0x01234567,
                                0x76543210, 0xfedcba98],
                          r_resp=AXIResp.SLVERR,
                          b_resp=AXIResp.SLVERR)
    ars = []
    aws = []
    writes = []

    def driver():
        assert (yield from axi_read_burst(top.master,
                                          0,
                                          size=3,
                                          length=1,
                                          txn_id=3)) == [
                                              (0x0123456789abcdef, 2, 0, 3),
                                              (0xfedcba9876543210, 2, 1, 3),
                                          ]
        assert (yield from axi_write_burst(
            top.master,
            0x10,
            [(0xaaaabbbbccccdddd, 0x0f),
             (0x1111222233334444, 0xf0)],
            size=3,
            txn_id=7)) == (2, 7)

    def monitor():
        for _ in range(200):
            if (yield top.slave.ar_monitor.valid):
                ars.append(((yield top.slave.ar_monitor.bits.addr),
                            (yield top.slave.ar_monitor.bits.len),
                            (yield top.slave.ar_monitor.bits.size),
                            (yield top.slave.ar_monitor.bits.burst)))
            if (yield top.slave.aw_monitor.valid):
                aws.append(((yield top.slave.aw_monitor.bits.addr),
                            (yield top.slave.aw_monitor.bits.len),
                            (yield top.slave.aw_monitor.bits.size),
                            (yield top.slave.aw_monitor.bits.burst)))
            if (yield top.slave.w_monitor.valid):
                writes.append(((yield top.slave.w_monitor.bits.addr),
                               (yield top.slave.w_monitor.bits.data),
                               (yield top.slave.w_monitor.bits.strb)))
            yield

        assert ars == [(0, 3, 2, AXIBurst.INCR)]
        assert aws == [(0x10, 3, 2, AXIBurst.INCR)]
        assert writes == [(0x10, 0xccccdddd, 0xf),
                          (0x14, 0xaaaabbbb, 0x0),
                          (0x18, 0x33334444, 0x0),
                          (0x1c, 0x11112222, 0xf)]
        assert not (yield top.slave.protocol_error)

    run_sim(top, driver, monitor)


def test_axi_downconverter_narrow_access_selects_addressed_lane():
    top = AXIConverterTop(64, 32, init=[0x11111111, 0x89abcdef])
    reads = []
    writes = []

    def driver():
        assert (yield from axi_read(top.master, 4, size=2,
                                    txn_id=1)) == (0x89abcdef00000000, 0, 1, 1)
        assert (yield from axi_write(top.master,
                                     4,
                                     0xdeadbeef00000000,
                                     0xf0,
                                     size=2,
                                     txn_id=2)) == (0, 2)

    def monitor():
        for _ in range(100):
            if (yield top.slave.ar_monitor.valid):
                reads.append(((yield top.slave.ar_monitor.bits.addr),
                              (yield top.slave.ar_monitor.bits.len),
                              (yield top.slave.ar_monitor.bits.size)))
            if (yield top.slave.w_monitor.valid):
                writes.append(((yield top.slave.w_monitor.bits.addr),
                               (yield top.slave.w_monitor.bits.data),
                               (yield top.slave.w_monitor.bits.strb)))
            yield

        assert reads == [(4, 0, 2)]
        assert writes == [(4, 0xdeadbeef, 0xf)]

    run_sim(top, driver, monitor)


def test_axi_downconverter_fixed_burst_uses_slow_path():
    top = AXIConverterTop(64,
                          32,
                          init=[0x89abcdef, 0x01234567],
                          r_resp=AXIResp.SLVERR,
                          b_resp=AXIResp.SLVERR)
    ars = []
    aws = []
    writes = []

    def driver():
        bus = top.master
        yield bus.ar.bits.addr.eq(0)
        yield bus.ar.bits.size.eq(3)
        yield bus.ar.bits.len.eq(1)
        yield bus.ar.bits.burst.eq(AXIBurst.FIXED)
        yield bus.ar.bits.id.eq(3)
        yield bus.ar.valid.eq(1)
        yield
        while not (yield bus.ar.ready):
            yield
        yield bus.ar.valid.eq(0)
        yield bus.r.ready.eq(1)
        read_beats = []
        while len(read_beats) < 2:
            if (yield bus.r.valid):
                read_beats.append(((yield bus.r.bits.data),
                                   (yield bus.r.bits.resp),
                                   (yield bus.r.bits.last),
                                   (yield bus.r.bits.id)))
            yield
        yield bus.r.ready.eq(0)
        assert read_beats == [(0x0123456789abcdef, 2, 0, 3),
                              (0x0123456789abcdef, 2, 1, 3)]

        yield bus.aw.bits.addr.eq(0x10)
        yield bus.aw.bits.size.eq(3)
        yield bus.aw.bits.len.eq(1)
        yield bus.aw.bits.burst.eq(AXIBurst.FIXED)
        yield bus.aw.bits.id.eq(5)
        yield bus.aw.valid.eq(1)
        yield
        while not (yield bus.aw.ready):
            yield
        yield bus.aw.valid.eq(0)
        for index, data in enumerate((0xaaaabbbbccccdddd,
                                      0x1111222233334444)):
            yield bus.w.bits.data.eq(data)
            yield bus.w.bits.strb.eq(0xff)
            yield bus.w.bits.last.eq(index == 1)
            yield bus.w.valid.eq(1)
            yield
            while not (yield bus.w.ready):
                yield
            yield bus.w.valid.eq(0)
        yield bus.b.ready.eq(1)
        while not (yield bus.b.valid):
            yield
        assert ((yield bus.b.bits.resp),
                (yield bus.b.bits.id)) == (AXIResp.SLVERR, 5)
        yield
        yield bus.b.ready.eq(0)

    def monitor():
        for _ in range(300):
            if (yield top.slave.ar_monitor.valid):
                ars.append(((yield top.slave.ar_monitor.bits.addr),
                            (yield top.slave.ar_monitor.bits.len),
                            (yield top.slave.ar_monitor.bits.size),
                            (yield top.slave.ar_monitor.bits.burst)))
            if (yield top.slave.aw_monitor.valid):
                aws.append(((yield top.slave.aw_monitor.bits.addr),
                            (yield top.slave.aw_monitor.bits.len),
                            (yield top.slave.aw_monitor.bits.size),
                            (yield top.slave.aw_monitor.bits.burst)))
            if (yield top.slave.w_monitor.valid):
                writes.append(((yield top.slave.w_monitor.bits.addr),
                               (yield top.slave.w_monitor.bits.data),
                               (yield top.slave.w_monitor.bits.strb)))
            yield

        narrow_burst = (1, 2, AXIBurst.INCR)
        assert ars == [(0, *narrow_burst), (0, *narrow_burst)]
        assert aws == [(0x10, *narrow_burst), (0x10, *narrow_burst)]
        assert writes == [(0x10, 0xccccdddd, 0xf),
                          (0x14, 0xaaaabbbb, 0xf),
                          (0x10, 0x33334444, 0xf),
                          (0x14, 0x11112222, 0xf)]
        assert not (yield top.slave.protocol_error)

    run_sim(top, driver, monitor)


def test_axi_upconverter_ratio_four_bursts_and_lane_steering():
    top = AXIConverterTop(16,
                          64,
                          init=[0x0706050403020100],
                          r_resp=AXIResp.SLVERR,
                          b_resp=AXIResp.SLVERR)
    ars = []
    aws = []
    writes = []

    def driver():
        assert (yield from axi_read_burst(top.master,
                                          0,
                                          size=1,
                                          length=3,
                                          txn_id=4)) == [
                                              (0x0100, 2, 0, 4),
                                              (0x0302, 2, 0, 4),
                                              (0x0504, 2, 0, 4),
                                              (0x0706, 2, 1, 4),
                                          ]
        assert (yield from axi_write_burst(
            top.master,
            0,
            [(0x1122, 0x3), (0x3344, 0x1),
             (0x5566, 0x2), (0x7788, 0x3)],
            size=1,
            txn_id=6)) == (2, 6)

    def monitor():
        for _ in range(300):
            if (yield top.slave.ar_monitor.valid):
                ars.append(((yield top.slave.ar_monitor.bits.addr),
                            (yield top.slave.ar_monitor.bits.len),
                            (yield top.slave.ar_monitor.bits.size)))
            if (yield top.slave.aw_monitor.valid):
                aws.append(((yield top.slave.aw_monitor.bits.addr),
                            (yield top.slave.aw_monitor.bits.len),
                            (yield top.slave.aw_monitor.bits.size)))
            if (yield top.slave.w_monitor.valid):
                writes.append(((yield top.slave.w_monitor.bits.data),
                               (yield top.slave.w_monitor.bits.strb)))
            yield

        assert ars == [(0, 0, 1), (2, 0, 1), (4, 0, 1), (6, 0, 1)]
        assert aws == [(0, 0, 1), (2, 0, 1), (4, 0, 1), (6, 0, 1)]
        assert writes == [(0x0000000000001122, 0x03),
                          (0x0000000033440000, 0x04),
                          (0x0000556600000000, 0x20),
                          (0x7788000000000000, 0xc0)]
        assert not (yield top.slave.protocol_error)

    run_sim(top, driver, monitor)
