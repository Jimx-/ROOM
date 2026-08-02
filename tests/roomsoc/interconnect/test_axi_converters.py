import pytest

from amaranth import *
from amaranth.sim import Simulator

from roomsoc.interconnect.axi import (AXI2AXILite, AXIInterface,
                                      AXILiteConverter, AXILiteInterface)

from axi_helpers import (AXILiteResponder, axi_read, axi_read_burst,
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
