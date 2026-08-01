import pytest

from amaranth import *
from amaranth.hdl.rec import Direction
from amaranth.sim import Simulator
from amaranth.utils import log2_int

from roomsoc.interconnect.axi import AXIInterface
from roomsoc.interconnect.stream import Valid, Queue
from roomsoc.interconnect.axi.common import AXIBurst
from roomsoc.peripheral.dma import AXIDMAReader


class AXIReadRAM(Elaboratable):
    """Simple read-only AXI4 slave for simulation.

    Accepts up to ``ar_depth`` outstanding AR transactions into a FIFO, then
    streams R beats back with a programmable per-burst read latency, modelling
    a pipelined memory with non-zero access delay.
    """

    def __init__(self,
                 *,
                 addr_width,
                 data_width,
                 depth,
                 init,
                 read_latency=2,
                 ar_depth=4,
                 id_width=1,
                 r_resp=0,
                 version='axi4'):
        self.addr_width = addr_width
        self.data_width = data_width
        self.depth = depth
        self.init = init
        self.read_latency = read_latency
        self.ar_depth = ar_depth
        self.r_resp = r_resp
        self.bus = AXIInterface(addr_width=addr_width,
                                data_width=data_width,
                                id_width=id_width,
                                version=version)
        self.monitor = Valid(Record, [("addr", addr_width, Direction.FANOUT),
                                      ("len", 8, Direction.FANOUT),
                                      ("size", 3, Direction.FANOUT),
                                      ("burst", 2, Direction.FANOUT)])

    def elaborate(self, platform):
        m = Module()
        lg = log2_int(self.data_width // 8)

        mem = Memory(width=self.data_width, depth=self.depth, init=self.init)
        rport = m.submodules.rport = mem.read_port(domain='comb')

        ar_q = m.submodules.ar_q = Queue(self.ar_depth,
                                         Record, [("addr", self.addr_width),
                                                  ("len", 8), ("size", 3)])
        m.d.comb += [
            ar_q.enq.valid.eq(self.bus.ar.valid),
            ar_q.enq.bits.addr.eq(self.bus.ar.bits.addr),
            ar_q.enq.bits.len.eq(self.bus.ar.bits.len),
            ar_q.enq.bits.size.eq(self.bus.ar.bits.size),
            self.bus.ar.ready.eq(ar_q.enq.ready),
            self.monitor.valid.eq(self.bus.ar.valid & self.bus.ar.ready),
            self.monitor.bits.addr.eq(self.bus.ar.bits.addr),
            self.monitor.bits.len.eq(self.bus.ar.bits.len),
            self.monitor.bits.size.eq(self.bus.ar.bits.size),
            self.monitor.bits.burst.eq(self.bus.ar.bits.burst),
        ]

        beat = Signal(8)
        lat = Signal(range(self.read_latency + 1))
        base_word = Signal(self.addr_width - lg)
        total = Signal(8)
        transfer_size = Signal(3)
        upper_lane = Signal()

        m.d.comb += rport.addr.eq(base_word + beat)

        with m.FSM():
            with m.State('IDLE'):
                with m.If(ar_q.deq.valid):
                    m.d.sync += [
                        base_word.eq(ar_q.deq.bits.addr >> lg),
                        total.eq(ar_q.deq.bits.len),
                        transfer_size.eq(ar_q.deq.bits.size),
                        upper_lane.eq(ar_q.deq.bits.addr[lg - 1]),
                        beat.eq(0),
                        lat.eq(self.read_latency - 1),
                    ]
                    m.next = 'WAIT'

            with m.State('WAIT'):
                with m.If(lat != 0):
                    m.d.sync += lat.eq(lat - 1)
                with m.Else():
                    narrow_data = Mux(
                        upper_lane,
                        Cat(Const(0, self.data_width // 2),
                            rport.data[self.data_width // 2:]),
                        Cat(rport.data[:self.data_width // 2],
                            Const(0, self.data_width // 2)))
                    m.d.comb += [
                        self.bus.r.valid.eq(1),
                        self.bus.r.bits.data.eq(
                            Mux(transfer_size == lg - 1, narrow_data,
                                rport.data)),
                        self.bus.r.bits.last.eq(beat == total),
                        self.bus.r.bits.resp.eq(self.r_resp),
                        self.bus.r.bits.id.eq(0),
                    ]
                    with m.If(self.bus.r.ready):
                        with m.If(beat == total):
                            m.d.comb += ar_q.deq.ready.eq(1)
                            m.next = 'IDLE'
                        with m.Else():
                            m.d.sync += beat.eq(beat + 1)

        return m


class Top(Elaboratable):

    def __init__(self,
                 *,
                 addr_width=32,
                 data_width=64,
                 ram_depth=1024,
                 read_latency=3,
                 max_burst_beats=4,
                 max_outstanding=4,
                 r_resp=0,
                 burst_type='INCR'):
        self.addr_width = addr_width
        self.data_width = data_width
        self.ram_depth = ram_depth
        self.init = list(range(ram_depth))

        self.reader = AXIDMAReader(addr_width=addr_width,
                                   data_width=data_width,
                                   max_burst_beats=max_burst_beats,
                                   max_outstanding=max_outstanding,
                                   cmd_fifo_depth=4,
                                   data_fifo_depth=8,
                                   burst_type=burst_type)
        self.ram = AXIReadRAM(addr_width=addr_width,
                              data_width=data_width,
                              depth=ram_depth,
                              init=self.init,
                              read_latency=read_latency,
                              ar_depth=8,
                              r_resp=r_resp)

    def elaborate(self, platform):
        m = Module()
        m.submodules.reader = self.reader
        m.submodules.ram = self.ram
        m.d.comb += self.reader.bus.connect(self.ram.bus)
        return m


def run_sim(top,
            commands,
            cycles=1000,
            vcd=None,
            expected_errors=0,
            expected_ar=None,
            expected_data=None,
            min_peak_outstanding=None):
    reader = top.reader
    beat_bytes = top.data_width // 8

    expected = []
    for addr, length in commands:
        narrow = (length == beat_bytes // 2 and addr % (beat_bytes // 2) == 0)
        valid = narrow or (length != 0 and addr % beat_bytes == 0
                           and length % beat_bytes == 0)
        nbeats = 1 if narrow else length // beat_bytes if valid else 0
        base = addr // beat_bytes
        for b in range(nbeats):
            data = top.init[base + b]
            expected.append((data, 1 if b == nbeats - 1 else 0))

    if expected_data is not None:
        expected = expected_data

    received = []
    ar_seen = []
    done_count = 0
    error_count = 0
    peak_outstanding = [0]

    def tx_process():
        # Feed commands as single-cycle valid pulses. Signal assignments are
        # applied at the following clock edge (the naked yield), so each
        # command is presented for exactly one cycle and latched once.
        for addr, length in commands:
            yield reader.sink.bits.addr.eq(addr)
            yield reader.sink.bits.len.eq(length)
            yield reader.sink.valid.eq(1)
            yield
            while not (yield reader.sink.ready):
                yield
            yield reader.sink.valid.eq(0)
            yield
        for _ in range(cycles):
            yield

    def rx_process():
        nonlocal done_count, error_count
        # ready held high; valid/data/last are read in the same cycle (signal
        # yields do not advance the clock), then the naked yield fires that
        # beat and moves to the next. No cross-cycle race.
        yield reader.source.ready.eq(1)
        yield
        inflight = 0
        for _ in range(cycles):
            # Mirror the reader's outstanding accounting to measure how many
            # bursts were simultaneously in flight.
            ar_fire = (yield reader.bus.ar.valid) and (yield reader.bus.ar.ready)
            r_last_fire = ((yield reader.bus.r.valid)
                           and (yield reader.bus.r.ready)
                           and (yield reader.bus.r.bits.last))
            if ar_fire and not r_last_fire:
                inflight += 1
            elif (not ar_fire) and r_last_fire:
                inflight -= 1
            if inflight > peak_outstanding[0]:
                peak_outstanding[0] = inflight
            if (yield top.ram.monitor.valid):
                ar_seen.append(
                    ((yield top.ram.monitor.bits.addr),
                      (yield
                       top.ram.monitor.bits.len), (yield
                                                   top.ram.monitor.bits.size),
                      (yield top.ram.monitor.bits.burst)))
            if (yield reader.done):
                done_count += 1
            if (yield reader.error):
                error_count += 1
            if (yield reader.source.valid):
                data = (yield reader.source.bits.data)
                last = (yield reader.source.bits.last)
                received.append((data, last))
            yield

        assert received == expected
        assert done_count == len(commands)
        assert error_count == expected_errors
        if expected_ar is not None:
            assert ar_seen == expected_ar
        if min_peak_outstanding is not None:
            assert peak_outstanding[0] >= min_peak_outstanding, \
                peak_outstanding[0]

    sim = Simulator(top)
    sim.add_clock(1e-6)
    sim.add_sync_process(tx_process)
    sim.add_sync_process(rx_process)
    if vcd:
        with sim.write_vcd(vcd):
            sim.run()
    else:
        sim.run()


def test_axi_dma_reader_data_last_and_4kb_split():
    data_width = 64
    beat_bytes = data_width // 8
    commands = [
        (0x000, 4 * beat_bytes),
        (0x040, 16 * beat_bytes),
        (0xfe0, 8 * beat_bytes),
    ]
    top = Top(data_width=data_width,
              ram_depth=1024,
              read_latency=3,
              max_burst_beats=16,
              max_outstanding=4)
    incr = int(AXIBurst.INCR)
    expected_ar = [
        (0x000, 3, 3, incr),
        (0x040, 15, 3, incr),
        (0xfe0, 3, 3, incr),
        (0x1000, 3, 3, incr),
    ]
    run_sim(top, commands, expected_ar=expected_ar)


def test_axi_dma_reader_half_width_transfer():
    top = Top(data_width=64, ram_depth=1024)
    top.init[0] = 0x1122334455667788
    run_sim(top, [(4, 4)],
            expected_ar=[(4, 0, 2, int(AXIBurst.INCR))],
            expected_data=[(0x1122334400000000, 1)])


@pytest.mark.parametrize("command", [(0xffc, 8), (0x000, 15), (0x000, 0)])
def test_axi_dma_reader_rejects_invalid_descriptor(command):
    top = Top(data_width=64, ram_depth=1024, max_burst_beats=16)
    run_sim(top, [command], expected_errors=1, expected_ar=[])


def test_axi_dma_reader_reports_rresp_error():
    top = Top(data_width=64, ram_depth=1024, max_burst_beats=4, r_resp=2)
    run_sim(top, [(0, 8)], expected_errors=1)


def test_axi_dma_reader_half_width_lower_lane():
    # The existing half-width test only covers the upper lane (addr 4); exercise
    # the lower lane (addr 0), where source.data keeps the narrow payload in
    # [31:0] and leaves [63:32] unused.
    top = Top(data_width=64, ram_depth=1024)
    top.init[0] = 0x1122334455667788
    run_sim(top, [(0, 4)],
            expected_ar=[(0, 0, 2, int(AXIBurst.INCR))],
            expected_data=[(0x0000000055667788, 1)])


def test_axi_dma_reader_narrow_at_4kb_boundary():
    # A narrow transfer whose 4 bytes exactly fill the tail of a 4 KiB region
    # (0xffc..0xfff) must issue a single correctly-sized beat and not be split.
    top = Top(data_width=64, ram_depth=1024)
    top.init[0xffc // 8] = 0xaabbccdd11223344
    run_sim(top, [(0xffc, 4)],
            expected_ar=[(0xffc, 0, 2, int(AXIBurst.INCR))],
            expected_data=[(0xaabbccdd00000000, 1)])


def test_axi_dma_reader_narrow_multiple_outstanding():
    # Several narrow descriptors issued back to back must keep multiple bursts
    # in flight and return each lane's data in command order.
    top = Top(data_width=64,
              ram_depth=1024,
              read_latency=8,
              max_outstanding=4)
    top.init[0] = 0x1111222233334444
    top.init[1] = 0x5555666677778888
    incr = int(AXIBurst.INCR)
    run_sim(top, [(0, 4), (4, 4), (8, 4)],
            expected_ar=[(0, 0, 2, incr), (4, 0, 2, incr), (8, 0, 2, incr)],
            expected_data=[(0x0000000033334444, 1),
                           (0x1111222200000000, 1),
                           (0x0000000077778888, 1)],
            min_peak_outstanding=2)


def test_axi_dma_reader_rejects_wrap_narrow():
    # WRAP requires a power-of-two beat count > 1; a 1-beat narrow descriptor
    # is therefore silently rejected as invalid (no AR issued).
    top = Top(data_width=64,
              ram_depth=1024,
              max_burst_beats=4,
              burst_type='WRAP')
    run_sim(top, [(0, 4)], expected_errors=1, expected_ar=[], expected_data=[])
