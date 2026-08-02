"""Reusable AXI / AXI-Lite / AXI-Stream testbench kit.

Single source of truth for the slave responders and master drivers that the
AXI tests previously reinvented in every file. Lifted and generalized from:

  * ``tests/roomsoc/interconnect/test_axi_converters.py`` (AXILiteTestSlave,
    axi_read / axi_write / axi_read_burst / axi_write_burst, axilite_write_split)
  * ``tests/roomsoc/peripheral/test_dma_reader.py`` (AXIReadRAM)
  * ``tests/roomsoc/peripheral/test_dma_writer.py`` (AXIWriteRAM)
  * ``tests/roomsoc/peripheral/test_rdma.py`` (_drive_rx_packets / _collect_tx_packets)

All helpers assume amaranth ``pysim`` with the clock model documented in
AGENTS.md: only a naked ``yield`` advances the cycle, and signal reads/writes
between naked yields are coherent within a single cycle. Drivers and collectors
follow the two-process Decoupled pattern (the receiver drives ``ready``).
"""

from amaranth import *
from amaranth.hdl.rec import Direction
from amaranth.sim import Simulator
from amaranth.utils import log2_int

from roomsoc.interconnect.axi import AXIInterface, AXILiteInterface
from roomsoc.interconnect.axi.axi_stream import AXIStreamInterface
from roomsoc.interconnect.axi.common import AXIBurst
from roomsoc.interconnect.stream import Valid, Queue


def run_sim(dut, *procs, vcd=None, clock_period=1e-6):
    """Elaborate ``dut`` and run ``procs`` (sync processes) on one clock.

    Replaces the per-test ``Simulator`` boilerplate. The optional ``vcd`` path
    mirrors the ``vcd`` argument the standalone simulation scripts accept.
    """
    sim = Simulator(dut)
    sim.add_clock(clock_period)
    for proc in procs:
        sim.add_sync_process(proc)
    if vcd:
        with sim.write_vcd(vcd):
            sim.run()
    else:
        sim.run()


class AXIResponder(Elaboratable):
    """Unified read+write AXI4 slave backed by a simple memory.

    Merges the behaviour of the read-only ``AXIReadRAM`` and write-only
    ``AXIWriteRAM`` so a single responder can round-trip data. The read and
    write channels are served by independent FSMs, exactly as in AXI:

      * AR path: up to ``ar_depth`` outstanding reads are queued, then streamed
        back after ``read_latency`` cycles. Half-width (``size == lg-1``) reads
        get upper/lower lane steering to match the legacy slave. ``r_resp`` is
        returned on every R beat.
      * AW/W/B path: up to ``aw_depth`` writes are queued; W beats commit to the
        memory through the byte write port, then a single B is returned after
        ``b_latency`` cycles. ``b_resp`` is returned on the B beat.

    Monitor taps (all ``Valid``, single-cycle pulses on the accepted beat):

      * ``ar_monitor``  -- (addr, len[8], size[3], burst[2]) on AR accept
      * ``aw_monitor``  -- (addr, len[8], size[3], burst[2]) on AW accept
      * ``w_monitor``   -- (addr, data, strb) on each committed W beat

    ``protocol_error`` asserts if ``w.last`` disagrees with ``aw.len`` on the
    final beat of a write burst.
    """

    def __init__(self,
                 *,
                 addr_width,
                 data_width,
                 depth,
                 init=None,
                 read_latency=2,
                 b_latency=2,
                 ar_depth=4,
                 aw_depth=8,
                 id_width=1,
                 r_resp=0,
                 b_resp=0,
                 version='axi4'):
        self.addr_width = addr_width
        self.data_width = data_width
        self.depth = depth
        self.init = [0] * depth if init is None else list(init)
        self.read_latency = read_latency
        self.b_latency = b_latency
        self.ar_depth = ar_depth
        self.aw_depth = aw_depth
        self.r_resp = r_resp
        self.b_resp = b_resp

        self.bus = AXIInterface(addr_width=addr_width,
                                data_width=data_width,
                                id_width=id_width,
                                version=version)

        aw_len_width = {'axi3': 4, 'axi4': 8}[version]
        self.ar_monitor = Valid(
            Record, [("addr", addr_width, Direction.FANOUT),
                     ("len", aw_len_width, Direction.FANOUT),
                     ("size", 3, Direction.FANOUT),
                     ("burst", 2, Direction.FANOUT)])
        self.aw_monitor = Valid(
            Record, [("addr", addr_width, Direction.FANOUT),
                     ("len", aw_len_width, Direction.FANOUT),
                     ("size", 3, Direction.FANOUT),
                     ("burst", 2, Direction.FANOUT)])
        self.w_monitor = Valid(
            Record, [("addr", addr_width, Direction.FANOUT),
                     ("data", data_width, Direction.FANOUT),
                     ("strb", data_width // 8, Direction.FANOUT)])
        self.protocol_error = Signal()

    def elaborate(self, platform):
        m = Module()
        lg = log2_int(self.data_width // 8)

        mem = Memory(width=self.data_width,
                     depth=self.depth,
                     init=self.init)
        rport = m.submodules.rport = mem.read_port(domain='comb')
        wport = m.submodules.wport = mem.write_port(granularity=8)

        bus = self.bus

        ar_q = m.submodules.ar_q = Queue(
            self.ar_depth,
            Record, [("addr", self.addr_width), ("len", 8), ("size", 3),
                     ("id", self.bus.id_width)])
        m.d.comb += [
            ar_q.enq.valid.eq(bus.ar.valid),
            ar_q.enq.bits.addr.eq(bus.ar.bits.addr),
            ar_q.enq.bits.len.eq(bus.ar.bits.len),
            ar_q.enq.bits.size.eq(bus.ar.bits.size),
            ar_q.enq.bits.id.eq(bus.ar.bits.id),
            bus.ar.ready.eq(ar_q.enq.ready),
            self.ar_monitor.valid.eq(bus.ar.valid & bus.ar.ready),
            self.ar_monitor.bits.addr.eq(bus.ar.bits.addr),
            self.ar_monitor.bits.len.eq(bus.ar.bits.len),
            self.ar_monitor.bits.size.eq(bus.ar.bits.size),
            self.ar_monitor.bits.burst.eq(bus.ar.bits.burst),
        ]

        aw_q = m.submodules.aw_q = Queue(
            self.aw_depth,
            Record, [("addr", self.addr_width), ("len", 8),
                     ("id", self.bus.id_width)])
        m.d.comb += [
            aw_q.enq.valid.eq(bus.aw.valid),
            aw_q.enq.bits.addr.eq(bus.aw.bits.addr),
            aw_q.enq.bits.len.eq(bus.aw.bits.len),
            aw_q.enq.bits.id.eq(bus.aw.bits.id),
            bus.aw.ready.eq(aw_q.enq.ready),
            self.aw_monitor.valid.eq(bus.aw.valid & bus.aw.ready),
            self.aw_monitor.bits.addr.eq(bus.aw.bits.addr),
            self.aw_monitor.bits.len.eq(bus.aw.bits.len),
            self.aw_monitor.bits.size.eq(bus.aw.bits.size),
            self.aw_monitor.bits.burst.eq(bus.aw.bits.burst),
        ]

        r_beat = Signal(8)
        r_lat = Signal(range(max(1, self.read_latency) + 1))
        r_base = Signal(self.addr_width - lg)
        r_total = Signal(8)
        r_size = Signal(3)
        r_upper = Signal()
        r_id = Signal(self.bus.id_width)

        m.d.comb += rport.addr.eq(r_base + r_beat)

        with m.FSM(name="read"):
            with m.State('IDLE'):
                with m.If(ar_q.deq.valid):
                    m.d.sync += [
                        r_base.eq(ar_q.deq.bits.addr >> lg),
                        r_total.eq(ar_q.deq.bits.len),
                        r_size.eq(ar_q.deq.bits.size),
                        r_upper.eq(ar_q.deq.bits.addr[lg - 1]),
                        r_id.eq(ar_q.deq.bits.id),
                        r_beat.eq(0),
                        r_lat.eq(self.read_latency - 1),
                    ]
                    m.next = 'WAIT'

            with m.State('WAIT'):
                with m.If(r_lat != 0):
                    m.d.sync += r_lat.eq(r_lat - 1)
                with m.Else():
                    narrow = Mux(
                        r_upper,
                        Cat(Const(0, self.data_width // 2),
                            rport.data[self.data_width // 2:]),
                        Cat(rport.data[:self.data_width // 2],
                            Const(0, self.data_width // 2)))
                    m.d.comb += [
                        bus.r.valid.eq(1),
                        bus.r.bits.data.eq(
                            Mux(r_size == lg - 1, narrow, rport.data)),
                        bus.r.bits.last.eq(r_beat == r_total),
                        bus.r.bits.resp.eq(self.r_resp),
                        bus.r.bits.id.eq(r_id),
                    ]
                    with m.If(bus.r.ready):
                        with m.If(r_beat == r_total):
                            m.d.comb += ar_q.deq.ready.eq(1)
                            m.next = 'IDLE'
                        with m.Else():
                            m.d.sync += r_beat.eq(r_beat + 1)

        w_beat = Signal(8)
        w_base = Signal(self.addr_width - lg)
        w_total = Signal(8)
        w_lat = Signal(range(max(1, self.b_latency) + 1))
        w_id = Signal(self.bus.id_width)

        m.d.comb += [
            wport.addr.eq(w_base + w_beat),
            wport.data.eq(bus.w.bits.data),
        ]

        with m.FSM(name="write"):
            with m.State('IDLE'):
                with m.If(aw_q.deq.valid):
                    m.d.sync += [
                        w_base.eq(aw_q.deq.bits.addr >> lg),
                        w_total.eq(aw_q.deq.bits.len),
                        w_id.eq(aw_q.deq.bits.id),
                        w_beat.eq(0),
                    ]
                    m.next = 'WDATA'

            with m.State('WDATA'):
                w_fire = bus.w.valid & bus.w.ready
                m.d.comb += [
                    bus.w.ready.eq(1),
                    wport.en.eq(
                        w_fire.replicate(self.data_width // 8)
                        & bus.w.bits.strb),
                    self.w_monitor.valid.eq(w_fire),
                    self.w_monitor.bits.addr.eq((w_base + w_beat) << lg),
                    self.w_monitor.bits.data.eq(bus.w.bits.data),
                    self.w_monitor.bits.strb.eq(bus.w.bits.strb),
                ]
                with m.If(w_fire):
                    with m.If(bus.w.bits.last != (w_beat == w_total)):
                        m.d.sync += self.protocol_error.eq(1)
                    with m.If(w_beat == w_total):
                        m.d.sync += w_lat.eq(self.b_latency - 1)
                        m.next = 'BWAIT'
                    with m.Else():
                        m.d.sync += w_beat.eq(w_beat + 1)

            with m.State('BWAIT'):
                with m.If(w_lat != 0):
                    m.d.sync += w_lat.eq(w_lat - 1)
                with m.Else():
                    m.d.comb += [
                        bus.b.valid.eq(1),
                        bus.b.bits.resp.eq(self.b_resp),
                        bus.b.bits.id.eq(w_id),
                    ]
                    with m.If(bus.b.ready):
                        m.d.comb += aw_q.deq.ready.eq(1)
                        m.next = 'IDLE'

        return m


class AXILiteResponder(Elaboratable):
    """AXI-Lite slave with read/write monitor taps.

    Lifted verbatim from ``AXILiteTestSlave``. ``error_addr`` drives ``resp=2``
    (SLVERR) on accesses that hit it. With ``byte_addr_data`` each byte lane of
    a read response holds its own byte address, so narrow lanes of a wide read
    are distinguishable downstream of a width converter.
    """

    def __init__(self,
                 *,
                 addr_width=32,
                 data_width=32,
                 error_addr=0x0c,
                 byte_addr_data=False):
        self.bus = AXILiteInterface(addr_width=addr_width,
                                    data_width=data_width)
        self.error_addr = error_addr
        self.byte_addr_data = byte_addr_data
        self.read_monitor = Valid(Record,
                                  [("addr", addr_width, Direction.FANOUT)])
        self.write_monitor = Valid(
            Record, [("addr", addr_width, Direction.FANOUT),
                     ("data", data_width, Direction.FANOUT),
                     ("strb", data_width // 8, Direction.FANOUT)])

    def elaborate(self, platform):
        m = Module()
        bus = self.bus

        m.d.sync += [
            self.read_monitor.valid.eq(0),
            self.write_monitor.valid.eq(0),
        ]

        read_addr = Signal(bus.addr_width)
        with m.FSM(name="read"):
            with m.State("IDLE"):
                m.d.comb += bus.ar.ready.eq(1)
                with m.If(bus.ar.valid):
                    m.d.sync += [
                        read_addr.eq(bus.ar.addr),
                        self.read_monitor.valid.eq(1),
                        self.read_monitor.bits.addr.eq(bus.ar.addr),
                    ]
                    m.next = "RESP"

            with m.State("RESP"):
                m.d.comb += bus.r.valid.eq(1)
                if self.byte_addr_data:
                    for i in range(bus.data_width // 8):
                        m.d.comb += bus.r.data[i * 8:(i + 1) * 8].eq(
                            (read_addr + i)[:8])
                else:
                    m.d.comb += bus.r.data.eq((0xa0 << (bus.data_width - 8))
                                              | (read_addr >> 2))
                m.d.comb += bus.r.resp.eq(
                    Mux(read_addr == self.error_addr, 2, 0))
                with m.If(bus.r.ready):
                    m.next = "IDLE"

        aw_seen = Signal()
        aw_addr = Signal(bus.addr_width)
        w_seen = Signal()
        w_data = Signal(bus.data_width)
        w_strb = Signal(bus.data_width // 8)
        write_resp = Signal(2)
        with m.FSM(name="write"):
            with m.State("IDLE"):
                aw_fire = bus.aw.valid & bus.aw.ready
                w_fire = bus.w.valid & bus.w.ready
                m.d.comb += [
                    bus.aw.ready.eq(~aw_seen),
                    bus.w.ready.eq(~w_seen),
                ]
                with m.If(aw_fire):
                    m.d.sync += [
                        aw_seen.eq(1),
                        aw_addr.eq(bus.aw.addr),
                    ]
                with m.If(w_fire):
                    m.d.sync += [
                        w_seen.eq(1),
                        w_data.eq(bus.w.data),
                        w_strb.eq(bus.w.strb),
                    ]
                with m.If((aw_seen | aw_fire) & (w_seen | w_fire)):
                    m.d.sync += [
                        aw_seen.eq(0),
                        w_seen.eq(0),
                        self.write_monitor.valid.eq(1),
                        self.write_monitor.bits.addr.eq(
                            Mux(aw_fire, bus.aw.addr, aw_addr)),
                        self.write_monitor.bits.data.eq(
                            Mux(w_fire, bus.w.data, w_data)),
                        self.write_monitor.bits.strb.eq(
                            Mux(w_fire, bus.w.strb, w_strb)),
                        write_resp.eq(
                            Mux(
                                Mux(aw_fire, bus.aw.addr, aw_addr)
                                == self.error_addr, 2, 0)),
                    ]
                    m.next = "RESP"

            with m.State("RESP"):
                m.d.comb += [
                    bus.b.valid.eq(1),
                    bus.b.resp.eq(write_resp),
                ]
                with m.If(bus.b.ready):
                    m.next = "IDLE"

        return m


def axi_read(bus, addr, *, size, length=0, txn_id=0):
    """Drive a single (or ``length+1``-beat) AXI read and return one R beat.

    Returns ``(data, resp, last, id)``. For ``length > 0`` only the first R beat
    is captured; use :func:`axi_read_burst` for full bursts.
    """
    yield bus.ar.bits.addr.eq(addr)
    yield bus.ar.bits.size.eq(size)
    yield bus.ar.bits.len.eq(length)
    yield bus.ar.bits.burst.eq(AXIBurst.INCR)
    yield bus.ar.bits.id.eq(txn_id)
    yield bus.ar.valid.eq(1)
    yield
    while not (yield bus.ar.ready):
        yield
    yield bus.ar.valid.eq(0)

    yield bus.r.ready.eq(1)
    while not (yield bus.r.valid):
        yield
    result = ((yield bus.r.bits.data), (yield bus.r.bits.resp),
              (yield bus.r.bits.last), (yield bus.r.bits.id))
    yield
    yield bus.r.ready.eq(0)
    return result


def axi_write(bus, addr, data, strb, *, size, length=0, txn_id=0):
    """Drive a single-beat AXI write and return ``(resp, id)``."""
    yield bus.aw.bits.addr.eq(addr)
    yield bus.aw.bits.size.eq(size)
    yield bus.aw.bits.len.eq(length)
    yield bus.aw.bits.burst.eq(AXIBurst.INCR)
    yield bus.aw.bits.id.eq(txn_id)
    yield bus.aw.valid.eq(1)
    yield
    while not (yield bus.aw.ready):
        yield
    yield bus.aw.valid.eq(0)

    yield bus.w.bits.data.eq(data)
    yield bus.w.bits.strb.eq(strb)
    yield bus.w.bits.last.eq(1)
    yield bus.w.valid.eq(1)
    yield
    while not (yield bus.w.ready):
        yield
    yield bus.w.valid.eq(0)

    yield bus.b.ready.eq(1)
    while not (yield bus.b.valid):
        yield
    result = ((yield bus.b.bits.resp), (yield bus.b.bits.id))
    yield
    yield bus.b.ready.eq(0)
    return result


def axi_read_burst(bus, addr, *, size, length, txn_id=0):
    """Drive an ``length+1``-beat INCR read; return ``[(data,resp,last,id),...]``."""
    yield bus.ar.bits.addr.eq(addr)
    yield bus.ar.bits.size.eq(size)
    yield bus.ar.bits.len.eq(length)
    yield bus.ar.bits.burst.eq(AXIBurst.INCR)
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
        result.append(((yield bus.r.bits.data), (yield bus.r.bits.resp),
                       (yield bus.r.bits.last), (yield bus.r.bits.id)))
        yield
    yield bus.r.ready.eq(0)
    return result


def axi_write_burst(bus, addr, beats, *, size, txn_id=0):
    """Drive a multi-beat INCR write.

    ``beats`` is a list of ``(data, strb)`` tuples. The last beat carries WLAST.
    Returns ``(resp, id)``.
    """
    yield bus.aw.bits.addr.eq(addr)
    yield bus.aw.bits.size.eq(size)
    yield bus.aw.bits.len.eq(len(beats) - 1)
    yield bus.aw.bits.burst.eq(AXIBurst.INCR)
    yield bus.aw.bits.id.eq(txn_id)
    yield bus.aw.valid.eq(1)
    yield
    while not (yield bus.aw.ready):
        yield
    yield bus.aw.valid.eq(0)

    for index, (data, strb) in enumerate(beats):
        yield bus.w.bits.data.eq(data)
        yield bus.w.bits.strb.eq(strb)
        yield bus.w.bits.last.eq(index == len(beats) - 1)
        yield bus.w.valid.eq(1)
        yield
        while not (yield bus.w.ready):
            yield
        yield bus.w.valid.eq(0)

    yield bus.b.ready.eq(1)
    while not (yield bus.b.valid):
        yield
    result = ((yield bus.b.bits.resp), (yield bus.b.bits.id))
    yield
    yield bus.b.ready.eq(0)
    return result


def axilite_write_split(bus, addr, data, strb, *, gap=3):
    """Issue an AXI-Lite write with AW and W driven on disjoint cycles.

    Exercises converters' independent AW/W capture path, which the combined
    ``AXILiteInterface.write`` helper never reaches. Returns ``resp``.
    """
    yield bus.aw.valid.eq(1)
    yield bus.aw.addr.eq(addr)
    yield bus.w.valid.eq(0)
    yield
    while not (yield bus.aw.ready):
        yield
    yield bus.aw.valid.eq(0)
    yield bus.aw.addr.eq(0)
    for _ in range(gap):
        yield
    yield bus.w.valid.eq(1)
    yield bus.w.data.eq(data)
    yield bus.w.strb.eq(strb)
    yield
    while not (yield bus.w.ready):
        yield
    yield bus.w.valid.eq(0)
    yield bus.w.strb.eq(0)
    yield bus.b.ready.eq(1)
    while not (yield bus.b.valid):
        yield
    resp = (yield bus.b.resp)
    yield bus.b.ready.eq(0)
    return resp


def drive_stream(stream, packets, gap=20, bubble_cycles_fn=None):
    """Drive complete byte-packets into an AXI-Stream ``sink``.

    Each packet is sliced into beats of ``data_width // 8`` bytes. ``gap`` idle
    cycles separate packets; lowering it (e.g. to 0) streams packets
    back-to-back and forces the DUT to assert backpressure. ``bubble_cycles_fn``,
    when supplied, receives ``(packet_index, beat_index)`` and returns the
    number of cycles ``valid`` is deasserted before that beat, allowing bubbles
    inside a frame.
    """
    beat_bytes = stream.data_width // 8
    yield stream.valid.eq(0)

    for packet_index, packet in enumerate(packets):
        for _ in range(gap):
            yield

        for beat_index, offset in enumerate(range(0, len(packet), beat_bytes)):
            bubble_cycles = (0 if bubble_cycles_fn is None
                             else bubble_cycles_fn(packet_index, beat_index))
            if bubble_cycles:
                yield stream.valid.eq(0)
                for _ in range(bubble_cycles):
                    yield

            beat = packet[offset:offset + beat_bytes]
            yield stream.bits.data.eq(
                int.from_bytes(beat, byteorder="little"))
            yield stream.bits.keep.eq((1 << len(beat)) - 1)
            yield stream.bits.last.eq(offset + len(beat) == len(packet))
            yield stream.valid.eq(1)

            yield
            while not (yield stream.ready):
                yield

        yield stream.valid.eq(0)

    yield stream.valid.eq(0)
    yield


def collect_stream(stream, packets, timeout=1000, ready_fn=None):
    """Collect complete byte-packets from an AXI-Stream ``source``.

    Appends reconstructed ``bytes`` packets to ``packets``. ``ready_fn``, if
    given, is called with the cycle index each iteration and drives ``ready``
    to exercise backpressure. The AXI-Stream valid-stable invariant is checked:
    once a beat is observed valid but not accepted, its payload and ``last``
    must not change until ``ready`` rises again.
    """
    beat_bytes = stream.data_width // 8
    current = bytearray()
    stalled_beat = None
    yield stream.ready.eq(1 if ready_fn is None else ready_fn(0))
    yield

    for cycle in range(timeout):
        ready = (yield stream.ready)
        valid = (yield stream.valid)
        beat = ((yield stream.bits.data), (yield stream.bits.keep),
                (yield stream.bits.last))

        if stalled_beat is not None:
            assert valid, "valid dropped while a beat was stalled"
            assert beat == stalled_beat, "beat changed while ready was low"

        fire = valid and ready
        before = bytes(current)
        if fire:
            data, keep, last = beat
            for lane in range(beat_bytes):
                if keep & (1 << lane):
                    current.append((data >> (lane * 8)) & 0xFF)
            if last:
                packets.append(bytes(current))
                current.clear()
        else:
            assert bytes(current) == before

        stalled_beat = beat if valid and not ready else None

        next_cycle = cycle + 1
        yield stream.ready.eq(
            1 if ready_fn is None else ready_fn(next_cycle))
        yield

    assert not current, "simulation ended in the middle of a packet"
