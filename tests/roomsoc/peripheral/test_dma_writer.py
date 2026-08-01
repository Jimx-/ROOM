import pytest

from amaranth import *
from amaranth.hdl.rec import Direction
from amaranth.sim import Simulator
from amaranth.hdl.ir import Fragment
from amaranth.utils import log2_int

from roomsoc.interconnect.axi import AXIInterface
from roomsoc.interconnect.axi.common import AXIBurst
from roomsoc.interconnect.stream import Valid, Queue
from roomsoc.peripheral.dma import AXIDMAReader, AXIDMAWriter


class AXIWriteRAM(Elaboratable):
    """Simple write-only AXI4 slave for simulation.

    Accepts AW transactions into a FIFO, writes incoming W beats into a memory,
    and returns a B response (with a programmable latency) after each burst's
    final W beat. ``monitor`` taps every committed beat as (byte address, data)
    so the testbench can verify ordering and payload.
    """

    def __init__(self,
                 *,
                 addr_width,
                 data_width,
                 depth,
                 b_latency=2,
                 aw_depth=8,
                 id_width=1,
                 b_resp=0,
                 version='axi4'):
        self.addr_width = addr_width
        self.data_width = data_width
        self.depth = depth
        self.b_latency = b_latency
        self.aw_depth = aw_depth
        self.b_resp = b_resp
        self.bus = AXIInterface(addr_width=addr_width,
                                data_width=data_width,
                                id_width=id_width,
                                version=version)
        self.monitor = Valid(Record,
                             [("addr", addr_width, Direction.FANOUT),
                              ("data", data_width, Direction.FANOUT),
                              ("strb", data_width // 8, Direction.FANOUT)])
        self.aw_monitor = Valid(Record,
                                [("addr", addr_width, Direction.FANOUT),
                                 ("len", 8, Direction.FANOUT),
                                 ("size", 3, Direction.FANOUT),
                                 ("burst", 2, Direction.FANOUT)])
        self.protocol_error = Signal()

    def elaborate(self, platform):
        m = Module()
        lg = log2_int(self.data_width // 8)

        mem = Memory(width=self.data_width,
                     depth=self.depth,
                     init=[0] * self.depth)
        wport = m.submodules.wport = mem.write_port(granularity=8)

        aw_q = m.submodules.aw_q = Queue(self.aw_depth,
                                         Record, [("addr", self.addr_width),
                                                  ("len", 8)])
        m.d.comb += [
            aw_q.enq.valid.eq(self.bus.aw.valid),
            aw_q.enq.bits.addr.eq(self.bus.aw.bits.addr),
            aw_q.enq.bits.len.eq(self.bus.aw.bits.len),
            self.bus.aw.ready.eq(aw_q.enq.ready),
            self.aw_monitor.valid.eq(self.bus.aw.valid & self.bus.aw.ready),
            self.aw_monitor.bits.addr.eq(self.bus.aw.bits.addr),
            self.aw_monitor.bits.len.eq(self.bus.aw.bits.len),
            self.aw_monitor.bits.size.eq(self.bus.aw.bits.size),
            self.aw_monitor.bits.burst.eq(self.bus.aw.bits.burst),
        ]

        beat = Signal(8)
        base_word = Signal(self.addr_width - lg)
        total = Signal(8)
        lat = Signal(range(self.b_latency + 1))

        m.d.comb += [
            wport.addr.eq(base_word + beat),
            wport.data.eq(self.bus.w.bits.data),
        ]

        with m.FSM():
            with m.State('IDLE'):
                with m.If(aw_q.deq.valid):
                    m.d.sync += [
                        base_word.eq(aw_q.deq.bits.addr >> lg),
                        total.eq(aw_q.deq.bits.len),
                        beat.eq(0),
                    ]
                    m.next = 'WDATA'

            with m.State('WDATA'):
                w_fire = self.bus.w.valid & self.bus.w.ready
                m.d.comb += [
                    self.bus.w.ready.eq(1),
                    wport.en.eq(
                        w_fire.replicate(self.data_width // 8)
                        & self.bus.w.bits.strb),
                    self.monitor.valid.eq(w_fire),
                    self.monitor.bits.addr.eq((base_word + beat) << lg),
                    self.monitor.bits.data.eq(self.bus.w.bits.data),
                    self.monitor.bits.strb.eq(self.bus.w.bits.strb),
                ]
                with m.If(w_fire):
                    with m.If(self.bus.w.bits.last != (beat == total)):
                        m.d.sync += self.protocol_error.eq(1)
                    with m.If(beat == total):
                        m.d.sync += lat.eq(self.b_latency - 1)
                        m.next = 'BWAIT'
                    with m.Else():
                        m.d.sync += beat.eq(beat + 1)

            with m.State('BWAIT'):
                with m.If(lat != 0):
                    m.d.sync += lat.eq(lat - 1)
                with m.Else():
                    m.d.comb += [
                        self.bus.b.valid.eq(1),
                        self.bus.b.bits.resp.eq(self.b_resp),
                        self.bus.b.bits.id.eq(0),
                    ]
                    with m.If(self.bus.b.ready):
                        m.d.comb += aw_q.deq.ready.eq(1)
                        m.next = 'IDLE'

        return m


class Top(Elaboratable):

    def __init__(self,
                 *,
                 addr_width=32,
                 data_width=64,
                 ram_depth=1024,
                 b_latency=3,
                 max_burst_beats=4,
                 max_outstanding=4,
                 b_resp=0,
                 burst_type='INCR'):
        self.addr_width = addr_width
        self.data_width = data_width
        self.ram_depth = ram_depth

        self.writer = AXIDMAWriter(addr_width=addr_width,
                                   data_width=data_width,
                                   max_burst_beats=max_burst_beats,
                                   max_outstanding=max_outstanding,
                                   cmd_fifo_depth=4,
                                   burst_type=burst_type)
        self.ram = AXIWriteRAM(addr_width=addr_width,
                               data_width=data_width,
                               depth=ram_depth,
                               b_latency=b_latency,
                               aw_depth=8,
                               b_resp=b_resp)

    def elaborate(self, platform):
        m = Module()
        m.submodules.writer = self.writer
        m.submodules.ram = self.ram
        m.d.comb += self.writer.bus.connect(self.ram.bus)
        return m


def run_sim(top,
            commands,
            cycles=1000,
            vcd=None,
            expected_errors=0,
            expected_aw=None,
            expected_strb=None,
            expected_writes=None,
            min_peak_outstanding=None,
            feed_data=True):
    writer = top.writer
    ram = top.ram
    beat_bytes = top.data_width // 8

    expected = []
    data_beats = []
    counter = 1
    for addr, length in commands:
        narrow = (length == beat_bytes // 2 and addr % (beat_bytes // 2) == 0)
        valid = narrow or (length != 0 and addr % beat_bytes == 0
                           and length % beat_bytes == 0)
        nbeats = 1 if narrow else length // beat_bytes if valid else 0
        for b in range(nbeats):
            expected_addr = (addr & ~(beat_bytes - 1) if narrow else addr +
                             b * beat_bytes)
            if narrow:
                upper = bool(addr & (beat_bytes // 2))
                data = counter << (top.data_width // 2 if upper else 0)
                strb = ((1 << (beat_bytes // 2)) - 1) << (beat_bytes //
                                                          2 if upper else 0)
            else:
                data = counter
                strb = (1 << beat_bytes) - 1
            expected.append((expected_addr, data))
            data_beats.append((data, strb))
            counter += 1

    if expected_writes is not None:
        expected = expected_writes

    received = []
    strb_seen = []
    aw_seen = []
    done_count = 0
    error_count = 0
    peak_outstanding = [0]

    def cmd_process():
        for addr, length in commands:
            yield writer.sink.bits.addr.eq(addr)
            yield writer.sink.bits.len.eq(length)
            yield writer.sink.valid.eq(1)
            yield
            while not (yield writer.sink.ready):
                yield
            yield writer.sink.valid.eq(0)
            yield

    def data_process():
        # Rejected descriptors consume no W beats; skip feeding to avoid
        # deadlocking on data.ready when no burst will ever stream.
        if feed_data:
            for data, strb in data_beats:
                yield writer.data.bits.data.eq(data)
                yield writer.data.bits.strb.eq(strb)
                yield writer.data.valid.eq(1)
                yield
                while not (yield writer.data.ready):
                    yield
                yield writer.data.valid.eq(0)
                yield
        for _ in range(cycles):
            yield

    def collect_process():
        nonlocal done_count, error_count
        inflight = 0
        for _ in range(cycles):
            # Mirror the writer's outstanding accounting (AW vs B) to measure
            # how many bursts were simultaneously in flight.
            aw_fire = (yield writer.bus.aw.valid) and (yield writer.bus.aw.ready)
            b_fire = (yield writer.bus.b.valid) and (yield writer.bus.b.ready)
            if aw_fire and not b_fire:
                inflight += 1
            elif (not aw_fire) and b_fire:
                inflight -= 1
            if inflight > peak_outstanding[0]:
                peak_outstanding[0] = inflight
            if (yield ram.aw_monitor.valid):
                aw_seen.append(
                    ((yield
                       ram.aw_monitor.bits.addr), (yield
                                                   ram.aw_monitor.bits.len),
                      (yield
                       ram.aw_monitor.bits.size), (yield
                                                   ram.aw_monitor.bits.burst)))
            if (yield writer.done):
                done_count += 1
            if (yield writer.error):
                error_count += 1
            if (yield ram.monitor.valid):
                addr = (yield ram.monitor.bits.addr)
                data = (yield ram.monitor.bits.data)
                received.append((addr, data))
                strb_seen.append((yield ram.monitor.bits.strb))
            yield

        assert received == expected
        assert done_count == len(commands)
        assert error_count == expected_errors
        assert (yield ram.protocol_error) == 0
        if expected_aw is not None:
            assert aw_seen == expected_aw
        if expected_strb is not None:
            assert strb_seen == expected_strb
        if min_peak_outstanding is not None:
            assert peak_outstanding[0] >= min_peak_outstanding, \
                peak_outstanding[0]

    sim = Simulator(top)
    sim.add_clock(1e-6)
    sim.add_sync_process(cmd_process)
    sim.add_sync_process(data_process)
    sim.add_sync_process(collect_process)
    if vcd:
        with sim.write_vcd(vcd):
            sim.run()
    else:
        sim.run()


def test_axi_dma_writer_data_last_done_and_4kb_split():
    data_width = 64
    beat_bytes = data_width // 8
    commands = [
        (0x000, 4 * beat_bytes),
        (0x040, 16 * beat_bytes),
        (0xfe0, 8 * beat_bytes),
    ]
    top = Top(data_width=data_width,
              ram_depth=1024,
              b_latency=3,
              max_burst_beats=16,
              max_outstanding=4)
    incr = int(AXIBurst.INCR)
    expected_aw = [
        (0x000, 3, 3, incr),
        (0x040, 15, 3, incr),
        (0xfe0, 3, 3, incr),
        (0x1000, 3, 3, incr),
    ]
    run_sim(top, commands, expected_aw=expected_aw)


def test_axi_dma_writer_half_width_transfer():
    top = Top(data_width=64, ram_depth=1024)
    run_sim(top, [(4, 4)],
            expected_aw=[(4, 0, 2, int(AXIBurst.INCR))],
            expected_strb=[0xf0],
            expected_writes=[(0, 0x0000000100000000)])


@pytest.mark.parametrize("command", [(0xffc, 8), (0x000, 15), (0x000, 0)])
def test_axi_dma_writer_rejects_invalid_descriptor(command):
    top = Top(data_width=64, ram_depth=1024, max_burst_beats=16)
    run_sim(top, [command], expected_errors=1, expected_aw=[])


def test_axi_dma_writer_reports_bresp_error():
    top = Top(data_width=64, ram_depth=1024, max_burst_beats=4, b_resp=2)
    run_sim(top, [(0, 8)], expected_errors=1)


@pytest.mark.filterwarnings("ignore::amaranth.hdl.ir.UnusedElaboratable")
def test_axi_dma_wrap_bursts_are_legal():
    with pytest.raises(ValueError, match="WRAP max_burst_beats"):
        AXIDMAWriter(data_width=64, burst_type='WRAP', max_burst_beats=3)

    top = Top(data_width=64,
              ram_depth=1024,
              max_burst_beats=4,
              burst_type='WRAP')
    wrap = int(AXIBurst.WRAP)
    run_sim(top, [(0, 6 * 8)], expected_aw=[(0, 3, 3, wrap), (32, 1, 3, wrap)])


def test_axi3_dma_interfaces_elaborate():
    reader = AXIDMAReader(data_width=64, version='axi3')
    writer = AXIDMAWriter(data_width=64, version='axi3')
    assert len(reader.bus.ar.bits.len) == 4
    assert len(writer.bus.aw.bits.len) == 4
    assert len(writer.bus.w.bits.id) == writer.id_width
    Fragment.get(reader, None)
    Fragment.get(writer, None)


def test_axi_dma_writer_half_width_lower_lane():
    # The existing half-width test only covers the upper lane (addr 4); exercise
    # the lower lane (addr 0), where the W payload and strobe occupy [31:0].
    top = Top(data_width=64, ram_depth=1024)
    run_sim(top, [(0, 4)],
            expected_aw=[(0, 0, 2, int(AXIBurst.INCR))],
            expected_strb=[0x0f],
            expected_writes=[(0, 0x0000000000000001)])


def test_axi_dma_writer_narrow_multiple_outstanding():
    # Several narrow descriptors issued back to back must keep multiple bursts
    # outstanding (AW ahead of B) and commit each lane in command order.
    top = Top(data_width=64,
              ram_depth=1024,
              b_latency=8,
              max_outstanding=4)
    incr = int(AXIBurst.INCR)
    run_sim(top, [(0, 4), (4, 4), (8, 4)],
            expected_aw=[(0, 0, 2, incr), (4, 0, 2, incr), (8, 0, 2, incr)],
            expected_strb=[0x0f, 0xf0, 0x0f],
            expected_writes=[(0, 0x0000000000000001),
                             (0, 0x0000000200000000),
                             (8, 0x0000000000000003)],
            min_peak_outstanding=2)


def test_axi_dma_writer_rejects_wrap_narrow():
    # WRAP requires a power-of-two beat count > 1; a 1-beat narrow descriptor
    # is therefore silently rejected as invalid (no AW issued). feed_data=False
    # because a rejected descriptor never consumes a W beat.
    top = Top(data_width=64,
              ram_depth=1024,
              max_burst_beats=4,
              burst_type='WRAP')
    run_sim(top, [(0, 4)],
            expected_errors=1,
            expected_aw=[],
            expected_writes=[],
            feed_data=False)
