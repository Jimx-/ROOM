"""Self-tests for the AXI/AXI-Lite/AXI-Stream testbench kit itself.

Validates the NEW components introduced by ``axi_helpers.py`` (the unified
``AXIResponder`` and the generalized stream drive/collect helpers) so the kit
has its own coverage before downstream phases rely on it.
"""

import pytest

from amaranth import *
from amaranth.sim import Simulator

from roomsoc.interconnect.axi import AXIInterface, AXILiteInterface
from roomsoc.interconnect.axi.axi_stream import AXIStreamInterface
from roomsoc.interconnect.axi.common import AXIBurst
from roomsoc.interconnect.stream import SkidBuffer

from axi_helpers import (AXIResponder, AXILiteResponder, run_sim, axi_read,
                         axi_read_burst, axi_write, axi_write_burst,
                         drive_stream, collect_stream)


class AXIResponderTop(Elaboratable):

    def __init__(self,
                 *,
                 addr_width=32,
                 data_width=64,
                 depth=1024,
                 id_width=4,
                 read_latency=2,
                 b_latency=2,
                 r_resp=0,
                 b_resp=0):
        self.data_width = data_width
        self.depth = depth
        self.axi = AXIInterface(addr_width=addr_width,
                                data_width=data_width,
                                id_width=id_width)
        self.ram = AXIResponder(addr_width=addr_width,
                                data_width=data_width,
                                depth=depth,
                                id_width=id_width,
                                read_latency=read_latency,
                                b_latency=b_latency,
                                r_resp=r_resp,
                                b_resp=b_resp)

    def elaborate(self, platform):
        m = Module()
        m.submodules.ram = self.ram
        m.d.comb += self.axi.connect(self.ram.bus)
        return m


class StreamPassthrough(Elaboratable):
    """Registered AXI-Stream loopback (skid buffer) for drive/collect tests.

    Routed through a real SkidBuffer so the elaborated fragment owns a ``sync``
    domain (a purely combinational passthrough would have none, and the shared
    ``run_sim`` helper needs a clocked domain).
    """

    def __init__(self, data_width=64):
        self.sink = AXIStreamInterface(data_width=data_width)
        self.source = AXIStreamInterface(data_width=data_width)

    def elaborate(self, platform):
        m = Module()
        buf = m.submodules.buf = SkidBuffer(self.sink)
        m.d.comb += [
            self.sink.connect(buf.enq),
            buf.deq.connect(self.source),
        ]
        return m


def test_axi_responder_write_then_read_roundtrip():
    # A full-width write must persist in the responder memory and read back
    # unchanged, exercising both FSMs of the unified slave.
    top = AXIResponderTop(data_width=64, depth=16)
    beat = 0x1122334455667788
    saw_write = []

    def driver():
        resp, _id = yield from axi_write(top.axi,
                                         8,
                                         beat,
                                         0xff,
                                         size=3,
                                         txn_id=1)
        assert resp == 0
        data, resp, last, _rid = yield from axi_read(top.axi,
                                                     8,
                                                     size=3,
                                                     txn_id=2)
        assert (data, resp, last) == (beat, 0, 1)

    def monitor():
        for _ in range(200):
            if (yield top.ram.w_monitor.valid):
                saw_write.append(((yield top.ram.w_monitor.bits.addr),
                                  (yield top.ram.w_monitor.bits.data),
                                  (yield top.ram.w_monitor.bits.strb)))
            yield
        assert saw_write == [(8, beat, 0xff)]

    run_sim(top, driver, monitor)


def test_axi_responder_burst_read_and_ar_monitor():
    top = AXIResponderTop(data_width=64, depth=32, read_latency=3)
    top.ram.init = list(range(32))
    ar_seen = []

    def driver():
        beats = yield from axi_read_burst(top.axi, 0, size=3, length=3, txn_id=5)
        assert [b[0] for b in beats] == [0, 1, 2, 3]
        assert [b[2] for b in beats] == [0, 0, 0, 1]
        assert all(b[1] == 0 for b in beats)
        assert all(b[3] == 5 for b in beats)

    def monitor():
        for _ in range(300):
            if (yield top.ram.ar_monitor.valid):
                ar_seen.append(((yield top.ram.ar_monitor.bits.addr),
                                (yield top.ram.ar_monitor.bits.len),
                                (yield top.ram.ar_monitor.bits.size),
                                (yield top.ram.ar_monitor.bits.burst)))
            yield
        assert ar_seen == [(0, 3, 3, int(AXIBurst.INCR))]

    run_sim(top, driver, monitor)


def test_axi_responder_burst_write_and_aw_monitor():
    top = AXIResponderTop(data_width=64, depth=32, b_latency=3)
    aw_seen = []

    def driver():
        beats = [(0xaaaaaaaaaaaaaaaa, 0xff), (0xbbbbbbbbbbbbbbbb, 0xff),
                 (0xcccccccccccccccc, 0xff), (0xdddddddddddddddd, 0xff)]
        resp, _id = yield from axi_write_burst(top.axi,
                                               0x40,
                                               beats,
                                               size=3,
                                               txn_id=7)
        assert resp == 0
        # Read it back through the same responder to confirm persistence.
        rd = yield from axi_read_burst(top.axi, 0x40, size=3, length=3, txn_id=8)
        assert [b[0] for b in rd] == [b[0] for b in beats]

    def monitor():
        for _ in range(400):
            if (yield top.ram.aw_monitor.valid):
                aw_seen.append(((yield top.ram.aw_monitor.bits.addr),
                                (yield top.ram.aw_monitor.bits.len),
                                (yield top.ram.aw_monitor.bits.size),
                                (yield top.ram.aw_monitor.bits.burst)))
            yield
        assert aw_seen == [(0x40, 3, 3, int(AXIBurst.INCR))]
        assert (yield top.ram.protocol_error) == 0

    run_sim(top, driver, monitor)


def test_axi_responder_injects_error_responses():
    top_r = AXIResponderTop(data_width=64, depth=16, r_resp=2)
    top_w = AXIResponderTop(data_width=64, depth=16, b_resp=3)

    def read_driver():
        _d, resp, _l, _i = yield from axi_read(top_r.axi, 0, size=3)
        assert resp == 2

    def write_driver():
        resp, _i = yield from axi_write(top_w.axi,
                                        0,
                                        0xdeadbeef,
                                        0xff,
                                        size=3)
        assert resp == 3

    run_sim(top_r, read_driver)
    run_sim(top_w, write_driver)


def test_axi_responder_narrow_upper_and_lower_lanes():
    top = AXIResponderTop(data_width=64, depth=16)
    top.ram.init[0] = 0x1122334455667788

    def driver():
        upper, _r, _l, _i = yield from axi_read(top.axi, 4, size=2, txn_id=1)
        assert upper == 0x1122334400000000
        lower, _r, _l, _i = yield from axi_read(top.axi, 0, size=2, txn_id=2)
        assert lower == 0x0000000055667788

    run_sim(top, driver)


def test_axilite_responder_roundtrip_and_monitors():
    # The lifted AXI-Lite slave must behave exactly as before: writes/reads
    # complete, the error address returns resp=2, and the monitor taps fire.
    m = Module()
    master = AXILiteInterface(addr_width=32, data_width=32)
    slave = AXILiteResponder(data_width=32, error_addr=0x10)
    m.submodules.slave = slave
    m.d.comb += master.connect(slave.bus)

    reads = []
    writes = []

    def driver():
        data, resp = yield from master.read(0)
        assert (data, resp) == (0xa0000000, 0)
        assert (yield from master.write(8, 0xcafef00d, 0xf)) == 0
        _d, err_resp = yield from master.read(0x10)
        assert err_resp == 2
        assert (yield from master.write(0x10, 0x1, 0xf)) == 2

    def monitor():
        for _ in range(300):
            if (yield slave.read_monitor.valid):
                reads.append((yield slave.read_monitor.bits.addr))
            if (yield slave.write_monitor.valid):
                writes.append(((yield slave.write_monitor.bits.addr),
                               (yield slave.write_monitor.bits.data),
                               (yield slave.write_monitor.bits.strb)))
            yield
        assert reads == [0, 0x10]
        assert writes == [(8, 0xcafef00d, 0xf), (0x10, 0x1, 0xf)]

    sim = Simulator(m)
    sim.add_clock(1e-6)
    sim.add_sync_process(driver)
    sim.add_sync_process(monitor)
    sim.run()


@pytest.mark.parametrize("ready_duty", [1.0, 0.5, 0.25])
def test_stream_drive_collect_roundtrip(ready_duty):
    # drive_stream -> passthrough -> collect_stream must reproduce the exact
    # bytes, with and without backpressure. A fractional ready_duty forces the
    # driver to stall on beats until ready rises, exercising valid-stable
    # handling. (duty must stay > 0 or the stream never drains.)
    dut = StreamPassthrough(data_width=64)
    beat_bytes = dut.sink.data_width // 8
    packets_in = [
        bytes(range(beat_bytes)),
        bytes(range(beat_bytes, 2 * beat_bytes)),
        bytes(range(2 * beat_bytes, 3 * beat_bytes - 2)),  # short tail beat
    ]
    packets_out = []

    def driver():
        yield from drive_stream(dut.sink, packets_in, gap=4)

    def collector():
        def ready_fn(cycle):
            # duty * 4 beats out of every 4 cycles are ready.
            return 1 if (cycle % 4) < int(round(ready_duty * 4)) else 0

        yield from collect_stream(dut.source,
                                  packets_out,
                                  timeout=4000,
                                  ready_fn=(None if ready_duty == 1.0
                                            else ready_fn))

    run_sim(dut, driver, collector)
    assert packets_out == packets_in


def test_stream_drive_bubbles_and_back_to_back():
    # Intra-frame bubbles (valid deasserted mid-frame) and gap=0 back-to-back
    # frames must still reconstruct cleanly.
    dut = StreamPassthrough(data_width=64)
    beat_bytes = dut.sink.data_width // 8
    packets_in = [
        bytes(range(beat_bytes * 3)),
        bytes(range(10, 10 + beat_bytes * 2)),
    ]
    packets_out = []

    def driver():
        yield from drive_stream(dut.sink,
                                packets_in,
                                gap=0,
                                bubble_cycles_fn=lambda p, b: (b % 2))

    def collector():
        yield from collect_stream(dut.source, packets_out, timeout=3000)

    run_sim(dut, driver, collector)
    assert packets_out == packets_in
