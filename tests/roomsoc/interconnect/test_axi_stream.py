"""Isolated unit tests for the AXI-Stream components (Phase 4).

Covers the AXI-Stream interface and four components in
``roomsoc/interconnect/axi/axi_stream.py`` that previously had zero or
integration-only coverage (see
``tests/roomsoc/interconnect/AXI_TEST_PLAN.md`` Phase 4):

  * ``AXIStreamInterface``    -- custom layouts, cloning, width metadata
  * ``AXIStreamPacketizer``   -- header prepend (aligned / unaligned / multi-beat,
                                 ``last`` semantics, back-to-back frames)
  * ``AXIStreamDepacketizer`` -- header strip, field parsing, truncation,
                                 mid-stream ``last``
  * ``AXIStreamConverter``    -- up/down/equal width adaptation, partial beats,
                                 reverse, backpressure, ratio errors
  * ``AXIStreamArbiter``      -- N-input round-robin, lock-to-last, backpressure

Also includes packetizer<->depacketizer round-trip tests for both aligned and
unaligned header configurations.

Uses the shared ``axi_helpers`` kit (``drive_stream`` / ``collect_stream`` /
``run_sim``) for byte-level stimulus and checking, plus small per-test beat
drivers for the arbiter where raw data values are more natural than byte
packets.
"""

import pytest

from amaranth import *
from amaranth.hdl.rec import Direction

from roomsoc.interconnect.axi.axi_stream import (
    AXIStreamArbiter,
    AXIStreamConverter,
    AXIStreamDepacketizer,
    AXIStreamInterface,
    AXIStreamPacketizer,
)

from axi_helpers import collect_stream, drive_stream, run_sim


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def fields_to_bytes(*fields):
    """Pack ``(width, value)`` pairs into little-endian bytes.

    The byte order matches a ``Record`` layout (first field at the LSB) and the
    lane order ``collect_stream`` produces, so the result can be compared
    directly against collected output and prepended to data when building
    depacketizer stimulus.
    """
    val = 0
    pos = 0
    for width, value in fields:
        val |= (value & ((1 << width) - 1)) << pos
        pos += width
    assert pos % 8 == 0, "header width must be byte-aligned"
    return val.to_bytes(pos // 8, "little")


class PacketizerDUT(Elaboratable):

    def __init__(self, header_layout, data_width):
        self.data_width = data_width
        self.pkt = AXIStreamPacketizer(Record, header_layout,
                                       data_width=data_width)
        self.sink = self.pkt.sink
        self.source = self.pkt.source
        self.header = self.pkt.header

    def elaborate(self, platform):
        m = Module()
        m.submodules.pkt = self.pkt
        return m


class DepacketizerDUT(Elaboratable):

    def __init__(self, header_layout, data_width):
        self.data_width = data_width
        self.dep = AXIStreamDepacketizer(Record, header_layout,
                                         data_width=data_width)
        self.sink = self.dep.sink
        self.source = self.dep.source
        self.header = self.dep.header

    def elaborate(self, platform):
        m = Module()
        m.submodules.dep = self.dep
        return m


class ArbiterDUT(Elaboratable):

    def __init__(self, n, data_width=32):
        self.n = n
        self.arb = AXIStreamArbiter(n, data_width=data_width)
        self.inp = self.arb.inp
        self.out = self.arb.out

    def elaborate(self, platform):
        m = Module()
        m.submodules.arb = self.arb
        return m


def drive_header(header, fields):
    """Yield ``eq`` for each (name, value) pair, then one naked yield to commit."""
    for name, value in fields.items():
        yield getattr(header, name).eq(value)
    yield


def drive_beats(stream, beats, gap=5):
    """Drive raw beats into ``stream``.

    Each entry is ``(data, last)``. ``gap`` idle cycles separate packets
    (delimited by a beat with ``last=1``).
    """
    yield stream.valid.eq(0)
    for data, last in beats:
        for _ in range(gap):
            yield
        yield stream.bits.data.eq(data)
        yield stream.bits.keep.eq(~0)
        yield stream.bits.last.eq(last)
        yield stream.valid.eq(1)
        yield
        while not (yield stream.ready):
            yield
        yield stream.valid.eq(0)
    yield stream.valid.eq(0)
    yield


def collect_beats(stream, expected, timeout=3000, ready_fn=None):
    """Collect raw ``(data, last)`` beats until ``expected`` count reached.

    Returns a list of ``(data, last)`` tuples. ``ready_fn``, if given, drives
    ``ready`` to exercise backpressure. Only beats that actually fire
    (``valid & ready``) are counted, so a held beat under backpressure is
    collected exactly once.
    """
    beats = []
    yield stream.ready.eq(1 if ready_fn is None else ready_fn(0))
    yield
    for cycle in range(timeout):
        valid = (yield stream.valid)
        ready = (yield stream.ready)
        if valid and ready:
            data = (yield stream.bits.data)
            last = (yield stream.bits.last)
            beats.append((data, last))
            if len(beats) >= expected:
                break
        yield stream.ready.eq(1 if ready_fn is None else ready_fn(cycle + 1))
        yield
    return beats


# ===========================================================================
# AXIStreamInterface
# ===========================================================================

def test_interface_custom_layout_is_reusable_and_cloneable():
    # Constructing an interface must not append AXI-Stream sidebands to the
    # caller-owned layout.  In particular, tuple layouts must work and clones
    # must retain the same payload shape without accumulating fields.
    layout = (("payload", 24, Direction.FANOUT),)

    stream = AXIStreamInterface(data_width=24,
                                keep_width=3,
                                id_width=2,
                                dest_width=3,
                                user_width=4,
                                layout=layout)
    clone = stream.clone(name="clone")

    assert layout == (("payload", 24, Direction.FANOUT),)
    for interface in (stream, clone):
        assert len(interface.bits.payload) == 24
        assert len(interface.bits.id) == 2
        assert len(interface.bits.dest) == 3
        assert len(interface.bits.user) == 4
        assert len(interface.bits.last) == 1
        assert not hasattr(interface.bits, "data")


def test_interface_default_widths_and_clone_metadata():
    stream = AXIStreamInterface(data_width=32,
                                keep_width=2,
                                id_width=3,
                                dest_width=4,
                                user_width=5)
    clone = stream.clone()

    assert clone.data_width == 32
    assert clone.keep_width == 2
    assert clone.id_width == 3
    assert clone.dest_width == 4
    assert clone.user_width == 5
    assert len(clone.bits.data) == 32
    assert len(clone.bits.keep) == 2


# ===========================================================================
# AXIStreamPacketizer
# ===========================================================================

def test_packetizer_aligned_single_beat_header():
    # data_width=64, header=64 bits (8 B) -> header_beats=1, aligned.
    # Output packet must be header_bytes ++ data_bytes.
    layout = [("src", 32), ("dst", 16), ("proto", 16)]
    dut = PacketizerDUT(layout, data_width=64)
    hdr_vals = {"src": 0xAABBCCDD, "dst": 0x1234, "proto": 0x5678}
    expected_hdr = fields_to_bytes((32, 0xAABBCCDD), (16, 0x1234), (16, 0x5678))
    beat_bytes = 8
    data = bytes(range(beat_bytes, beat_bytes + 2 * beat_bytes))
    out = []

    def driver():
        yield from drive_header(dut.header, hdr_vals)
        yield from drive_stream(dut.sink, [data], gap=4)

    def collector():
        yield from collect_stream(dut.source, out, timeout=2000)

    run_sim(dut, driver, collector)
    assert out == [expected_hdr + data]


def test_packetizer_aligned_multibeat_header():
    # data_width=32, header=64 bits (8 B) -> header_beats=2, aligned.
    layout = [("src", 32), ("dst", 32)]
    dut = PacketizerDUT(layout, data_width=32)
    hdr_vals = {"src": 0xDEADBEEF, "dst": 0xCAFEBABE}
    expected_hdr = fields_to_bytes((32, 0xDEADBEEF), (32, 0xCAFEBABE))
    beat_bytes = 4
    data = bytes(range(beat_bytes * 3))
    out = []

    def driver():
        yield from drive_header(dut.header, hdr_vals)
        yield from drive_stream(dut.sink, [data], gap=4)

    def collector():
        yield from collect_stream(dut.source, out, timeout=2000)

    run_sim(dut, driver, collector)
    assert out == [expected_hdr + data]


def test_packetizer_aligned_three_beat_header():
    # data_width=32, header=96 bits (12 B) -> header_beats=3, aligned.
    layout = [("a", 32), ("b", 32), ("c", 32)]
    dut = PacketizerDUT(layout, data_width=32)
    hdr_vals = {"a": 0x11111111, "b": 0x22222222, "c": 0x33333333}
    expected_hdr = fields_to_bytes((32, 0x11111111), (32, 0x22222222),
                                   (32, 0x33333333))
    beat_bytes = 4
    data = bytes(range(20, 20 + beat_bytes * 2))
    out = []

    def driver():
        yield from drive_header(dut.header, hdr_vals)
        yield from drive_stream(dut.sink, [data], gap=4)

    def collector():
        yield from collect_stream(dut.source, out, timeout=2000)

    run_sim(dut, driver, collector)
    assert out == [expected_hdr + data]


def test_packetizer_unaligned_subbeat_header():
    # data_width=64, header=24 bits (3 B) -> header_beats=0, leftover=3,
    # unaligned. Exercises the header_beats==0 path where the header fits
    # entirely within the first data beat's upper bytes.
    layout = [("a", 16), ("b", 8)]
    dut = PacketizerDUT(layout, data_width=64)
    hdr_vals = {"a": 0xBEEF, "b": 0x42}
    expected_hdr = fields_to_bytes((16, 0xBEEF), (8, 0x42))
    beat_bytes = 8
    data = bytes(range(beat_bytes * 2))
    out = []

    def driver():
        yield from drive_header(dut.header, hdr_vals)
        yield from drive_stream(dut.sink, [data], gap=4)

    def collector():
        yield from collect_stream(dut.source, out, timeout=2000)

    run_sim(dut, driver, collector)
    assert out == [expected_hdr + data]


def test_packetizer_unaligned_multibeat_header():
    # data_width=32, header=40 bits (5 B) -> header_beats=1, leftover=1,
    # unaligned. Header spans one full beat plus one byte of the next.
    layout = [("a", 32), ("b", 8)]
    dut = PacketizerDUT(layout, data_width=32)
    hdr_vals = {"a": 0x01020304, "b": 0x05}
    expected_hdr = fields_to_bytes((32, 0x01020304), (8, 0x05))
    beat_bytes = 4
    data = bytes(range(10, 10 + beat_bytes * 3))
    out = []

    def driver():
        yield from drive_header(dut.header, hdr_vals)
        yield from drive_stream(dut.sink, [data], gap=4)

    def collector():
        yield from collect_stream(dut.source, out, timeout=2000)

    run_sim(dut, driver, collector)
    assert out == [expected_hdr + data]


def test_packetizer_back_to_back_packets():
    # Two packets in succession: each must receive the full header prefix.
    layout = [("type", 16), ("len", 16)]
    dut = PacketizerDUT(layout, data_width=32)
    hdr_vals = {"type": 0xABCD, "len": 0x0010}
    expected_hdr = fields_to_bytes((16, 0xABCD), (16, 0x0010))
    beat_bytes = 4
    pkts = [bytes(range(beat_bytes)), bytes(range(10, 10 + beat_bytes * 2))]
    out = []

    def driver():
        yield from drive_header(dut.header, hdr_vals)
        yield from drive_stream(dut.sink, pkts, gap=2)

    def collector():
        yield from collect_stream(dut.source, out, timeout=3000)

    run_sim(dut, driver, collector)
    assert out == [expected_hdr + pkts[0], expected_hdr + pkts[1]]


def test_packetizer_short_packet_last_semantics():
    # A single-beat data packet (last=1 immediately) must still produce
    # header ++ data with last set on the final beat.
    layout = [("tag", 32)]
    dut = PacketizerDUT(layout, data_width=32)
    hdr_vals = {"tag": 0xFEEDFACE}
    expected_hdr = fields_to_bytes((32, 0xFEEDFACE))
    data = bytes([0xA0, 0xA1, 0xA2, 0xA3])
    out = []

    def driver():
        yield from drive_header(dut.header, hdr_vals)
        yield from drive_stream(dut.sink, [data], gap=4)

    def collector():
        yield from collect_stream(dut.source, out, timeout=2000)

    run_sim(dut, driver, collector)
    assert out == [expected_hdr + data]


# ===========================================================================
# AXIStreamDepacketizer
# ===========================================================================

def test_depacketizer_aligned_strips_header():
    # data_width=64, header=64 bits. The first beat (header) is consumed
    # internally; only the data bytes must emerge.
    layout = [("src", 32), ("dst", 16), ("proto", 16)]
    dut = DepacketizerDUT(layout, data_width=64)
    hdr_bytes = fields_to_bytes((32, 0xAABBCCDD), (16, 0x1234), (16, 0x5678))
    data = bytes(range(16))
    inp = [hdr_bytes + data]
    out = []

    def driver():
        yield from drive_stream(dut.sink, inp, gap=4)

    def collector():
        yield from collect_stream(dut.source, out, timeout=2000)

    run_sim(dut, driver, collector)
    assert out == [data]


def test_depacketizer_multibeat_strips_header():
    # data_width=32, header=64 bits (2 beats).
    layout = [("src", 32), ("dst", 32)]
    dut = DepacketizerDUT(layout, data_width=32)
    hdr_bytes = fields_to_bytes((32, 0xDEADBEEF), (32, 0xCAFEBABE))
    data = bytes(range(12))
    inp = [hdr_bytes + data]
    out = []

    def driver():
        yield from drive_stream(dut.sink, inp, gap=4)

    def collector():
        yield from collect_stream(dut.source, out, timeout=2000)

    run_sim(dut, driver, collector)
    assert out == [data]


def test_depacketizer_unaligned_strips_header():
    # data_width=32, header=40 bits (5 B): header_beats=1, leftover=1.
    layout = [("a", 32), ("b", 8)]
    dut = DepacketizerDUT(layout, data_width=32)
    hdr_bytes = fields_to_bytes((32, 0x01020304), (8, 0x05))
    data = bytes(range(10, 22))
    inp = [hdr_bytes + data]
    out = []

    def driver():
        yield from drive_stream(dut.sink, inp, gap=4)

    def collector():
        yield from collect_stream(dut.source, out, timeout=2000)

    run_sim(dut, driver, collector)
    assert out == [data]


def test_depacketizer_header_fields_parsed():
    # The parsed header Record must reflect the field values carried in the
    # first beats of the stream.
    layout = [("src", 32), ("dst", 16), ("proto", 16)]
    dut = DepacketizerDUT(layout, data_width=64)
    hdr_bytes = fields_to_bytes((32, 0x11223344), (16, 0x5566), (16, 0x7788))
    data = bytes(range(8))
    inp = [hdr_bytes + data]
    out = []
    captured = {}

    def driver():
        yield from drive_stream(dut.sink, inp, gap=4)

    def collector():
        yield from collect_stream(dut.source, out, timeout=2000)

    def monitor():
        for _ in range(500):
            if (yield dut.source.valid):
                captured["src"] = (yield dut.header.src)
                captured["dst"] = (yield dut.header.dst)
                captured["proto"] = (yield dut.header.proto)
                return
            yield

    run_sim(dut, driver, collector, monitor)
    assert out == [data]
    assert captured == {"src": 0x11223344, "dst": 0x5566, "proto": 0x7788}


def test_depacketizer_truncated_packet():
    # A packet whose only beat is the header (last=1 on the header beat)
    # must produce an empty data packet with last=1, not deadlock.
    layout = [("tag", 32)]
    dut = DepacketizerDUT(layout, data_width=32)
    hdr_bytes = fields_to_bytes((32, 0xDEADBEEF))
    inp = [hdr_bytes]  # no data at all
    out = []

    def driver():
        yield from drive_stream(dut.sink, inp, gap=4)

    def collector():
        yield from collect_stream(dut.source, out, timeout=2000)

    run_sim(dut, driver, collector)
    assert out == [b""]


def test_depacketizer_mid_stream_last():
    # last may arrive on any data beat; the depacketizer must propagate it
    # immediately and return to IDLE.
    layout = [("tag", 32)]
    dut = DepacketizerDUT(layout, data_width=32)
    hdr_bytes = fields_to_bytes((32, 0x12345678))
    data1 = bytes([1, 2, 3, 4, 5, 6, 7, 8])
    data2 = bytes([10, 20])
    inp = [hdr_bytes + data1, hdr_bytes + data2]
    out = []

    def driver():
        yield from drive_stream(dut.sink, inp, gap=6)

    def collector():
        yield from collect_stream(dut.source, out, timeout=3000)

    run_sim(dut, driver, collector)
    assert out == [data1, data2]


# ===========================================================================
# Packetizer <-> Depacketizer round-trip
# ===========================================================================

@pytest.mark.parametrize("data_width,layout,hdr_vals", [
    (64, [("src", 32), ("dst", 32)],
     {"src": 0xAAAABBBB, "dst": 0xCCCCDDDD}),
    (32, [("a", 24), ("b", 12), ("c", 4)],
     {"a": 0x123456, "b": 0x789, "c": 0x3}),
])
def test_packetizer_depacketizer_roundtrip(data_width, layout, hdr_vals):
    # pkt(depacketize(pkt(data))) == data, for both aligned and unaligned
    # header widths.
    beat_bytes = data_width // 8
    pkt = AXIStreamPacketizer(Record, layout, data_width=data_width)
    dep = AXIStreamDepacketizer(Record, layout, data_width=data_width)

    class Top(Elaboratable):
        def __init__(self):
            self.sink = AXIStreamInterface(data_width=data_width)
            self.source = AXIStreamInterface(data_width=data_width)
            self.header = pkt.header

        def elaborate(self, platform):
            mm = Module()
            mm.submodules.pkt = pkt
            mm.submodules.dep = dep
            mm.d.comb += [
                self.sink.connect(pkt.sink),
                pkt.source.connect(dep.sink),
                dep.source.connect(self.source),
            ]
            return mm

    top = Top()
    pkts_in = [
        bytes(range(beat_bytes)),
        bytes(range(10, 10 + beat_bytes * 3)),
    ]
    out = []

    def driver():
        yield from drive_header(top.header, hdr_vals)
        yield from drive_stream(top.sink, pkts_in, gap=4)

    def collector():
        yield from collect_stream(top.source, out, timeout=4000)

    run_sim(top, driver, collector)
    assert out == pkts_in


# ===========================================================================
# AXIStreamConverter
# ===========================================================================

def test_converter_down_preserves_bytes():
    # 64 -> 32: each wide beat is split into two narrow beats; the byte
    # stream collected on the narrow side must match the input.
    dut = AXIStreamConverter(dw_from=64, dw_to=32)
    beat_bytes = 8
    pkts = [
        bytes(range(beat_bytes * 2)),
        bytes(range(10, 10 + beat_bytes * 3)),
    ]
    out = []

    def driver():
        yield from drive_stream(dut.sink, pkts, gap=4)

    def collector():
        yield from collect_stream(dut.source, out, timeout=4000)

    run_sim(dut, driver, collector)
    assert out == pkts


def test_converter_up_preserves_bytes():
    # 32 -> 64: two narrow beats are packed into one wide beat; the byte
    # stream collected on the wide side must match the input.  Packet lengths
    # are kept to multiples of the ratio so this case exercises repeated full
    # wide beats; partial final beats are covered separately below.
    dut = AXIStreamConverter(dw_from=32, dw_to=64)
    beat_bytes = 4
    pkts = [
        bytes(range(beat_bytes * 4)),
        bytes(range(20, 20 + beat_bytes * 2)),
    ]
    out = []

    def driver():
        yield from drive_stream(dut.sink, pkts, gap=4)

    def collector():
        yield from collect_stream(dut.source, out, timeout=4000)

    run_sim(dut, driver, collector)
    assert out == pkts


def test_converter_up_flushes_partial_final_beat():
    # 32 -> 64 must emit immediately when last arrives in the first half of a
    # wide beat.  A preceding full packet makes stale upper keep bits visible.
    # This directly covers the early-last path that used ``self.sink.last``.
    dut = AXIStreamConverter(dw_from=32, dw_to=64)
    pkts = [bytes(range(8)), bytes([0xA0, 0xA1, 0xA2])]
    out = []

    def driver():
        yield from drive_stream(dut.sink, pkts, gap=1)

    def collector():
        yield from collect_stream(dut.source, out, timeout=3000)

    run_sim(dut, driver, collector)
    assert out == pkts


def test_converter_up_reverse_lane_order():
    # reverse=True places the first narrow input in the high half.
    dut = AXIStreamConverter(dw_from=32, dw_to=64, reverse=True)
    pkt = bytes(range(16))
    out = []

    def driver():
        yield from drive_stream(dut.sink, [pkt], gap=2)

    def collector():
        yield from collect_stream(dut.source, out, timeout=3000)

    run_sim(dut, driver, collector)
    expected = b"".join(pkt[i + 4:i + 8] + pkt[i:i + 4]
                        for i in range(0, len(pkt), 8))
    assert out == [expected]


def test_converter_equal_width_passthrough_with_backpressure():
    # The no-converter branch must preserve keep/last and hold its beat stable
    # while the downstream is stalled.
    converter = AXIStreamConverter(dw_from=32, dw_to=32)

    class Top(Elaboratable):
        def __init__(self):
            self.sink = converter.sink
            self.source = converter.source

        def elaborate(self, platform):
            m = Module()
            m.submodules.converter = converter
            # The bypass is purely combinational; retain a sync domain for
            # the shared cycle-driven simulation helpers.
            heartbeat = Signal()
            m.d.sync += heartbeat.eq(~heartbeat)
            return m

    dut = Top()
    pkts = [bytes(range(7)), bytes([0x80, 0x81])]
    out = []

    def driver():
        yield from drive_stream(dut.sink, pkts, gap=0)

    def collector():
        yield from collect_stream(dut.source,
                                  out,
                                  timeout=3000,
                                  ready_fn=lambda cycle: cycle % 4 != 0)

    run_sim(dut, driver, collector)
    assert out == pkts


def test_converter_down_up_roundtrip():
    # 64 -> 32 -> 64: the byte stream must survive a down then up conversion.
    down = AXIStreamConverter(dw_from=64, dw_to=32)
    up = AXIStreamConverter(dw_from=32, dw_to=64)

    class Top(Elaboratable):
        def __init__(self):
            self.sink = AXIStreamInterface(data_width=64)
            self.source = AXIStreamInterface(data_width=64)

        def elaborate(self, platform):
            m = Module()
            m.submodules.down = down
            m.submodules.up = up
            m.d.comb += [
                self.sink.connect(down.sink),
                down.source.connect(up.sink),
                up.source.connect(self.source),
            ]
            return m

    top = Top()
    beat_bytes = 8
    pkts = [bytes(range(beat_bytes * 3))]
    out = []

    def driver():
        yield from drive_stream(top.sink, pkts, gap=4)

    def collector():
        yield from collect_stream(top.source, out, timeout=4000)

    run_sim(top, driver, collector)
    assert out == pkts


def test_converter_reverse_down():
    # reverse=True on a 64->32 down-converter swaps the lane order within
    # each wide beat: the high narrow half is emitted first.
    dut = AXIStreamConverter(dw_from=64, dw_to=32, reverse=True)
    beat_bytes = 8
    pkt = bytes(range(beat_bytes * 2))  # B0..B15
    out = []

    def driver():
        yield from drive_stream(dut.sink, [pkt], gap=4)

    def collector():
        yield from collect_stream(dut.source, out, timeout=3000)

    run_sim(dut, driver, collector)
    expected = b""
    for i in range(0, len(pkt), beat_bytes):
        beat = pkt[i:i + beat_bytes]
        half = len(beat) // 2
        expected += beat[half:] + beat[:half]
    assert out == [expected]


@pytest.mark.filterwarnings("ignore::amaranth.hdl.ir.UnusedElaboratable")
@pytest.mark.parametrize("dw_from,dw_to", [(48, 32), (32, 48), (40, 64)])
def test_converter_rejects_non_integer_ratio(dw_from, dw_to):
    import gc
    dut = AXIStreamConverter(dw_from=dw_from, dw_to=dw_to)
    with pytest.raises(ValueError):
        dut.elaborate(None)
    del dut
    gc.collect()


# ===========================================================================
# AXIStreamArbiter
# ===========================================================================

def test_arbiter_two_input_round_robin():
    # With two inputs each driving one single-beat packet, both must appear
    # at the output (no starvation). The exact first grant depends on the
    # reset value of grant.bits, so we check the set of outputs, not order.
    dut = ArbiterDUT(2, data_width=32)
    beats_out = []

    def drv0():
        yield from drive_beats(dut.inp[0], [(0xAAAA, True)])

    def drv1():
        yield from drive_beats(dut.inp[1], [(0xBBBB, True)])

    def collector():
        result = yield from collect_beats(dut.out, expected=2, timeout=2000)
        beats_out.extend(result)

    run_sim(dut, drv0, drv1, collector)
    values = sorted(d for d, _ in beats_out)
    assert values == [0xAAAA, 0xBBBB]
    assert all(last for _, last in beats_out)


def test_arbiter_lock_to_last():
    # A multi-beat packet on one input must not be interleaved with beats
    # from another input: the arbiter locks onto the granted master until
    # last. We verify the four contiguous beats of input 0's packet survive
    # as an unbroken run.
    dut = ArbiterDUT(2, data_width=32)
    beats_out = []
    pkt0 = [(10 + i, i == 3) for i in range(4)]

    def drv0():
        yield from drive_beats(dut.inp[0], pkt0)

    def drv1():
        yield from drive_beats(dut.inp[1], [(99, True)])

    def collector():
        result = yield from collect_beats(dut.out, expected=5, timeout=3000)
        beats_out.extend(result)

    run_sim(dut, drv0, drv1, collector)
    data_seq = [d for d, _ in beats_out]
    # The four beats of pkt0 must be contiguous in the output stream.
    start = data_seq.index(10)
    assert data_seq[start:start + 4] == [10, 11, 12, 13]
    # The single beat from input 1 is somewhere outside that run.
    assert 99 in data_seq and data_seq.count(99) == 1
    # Every group's last is faithfully propagated.
    last_flags = [last for _, last in beats_out]
    assert last_flags.count(True) == 2


def test_arbiter_three_input():
    # Three inputs, one single-beat packet each: all three must arrive.
    dut = ArbiterDUT(3, data_width=32)
    beats_out = []

    def drv0():
        yield from drive_beats(dut.inp[0], [(0x100, True)])

    def drv1():
        yield from drive_beats(dut.inp[1], [(0x200, True)])

    def drv2():
        yield from drive_beats(dut.inp[2], [(0x300, True)])

    def collector():
        result = yield from collect_beats(dut.out, expected=3, timeout=3000)
        beats_out.extend(result)

    run_sim(dut, drv0, drv1, drv2, collector)
    values = sorted(d for d, _ in beats_out)
    assert values == [0x100, 0x200, 0x300]


def test_arbiter_backpressure():
    # With ready held low periodically, no beats must be lost and packets
    # must still arrive complete. The AXI-Stream valid-stable invariant is
    # checked by collect_stream; here we reuse the raw-beat collector with
    # a duty-cycled ready.
    dut = ArbiterDUT(2, data_width=32)
    beats_out = []
    pkt0 = [(i, i == 2) for i in range(3)]
    pkt1 = [(20, True)]

    def drv0():
        yield from drive_beats(dut.inp[0], pkt0)

    def drv1():
        yield from drive_beats(dut.inp[1], pkt1)

    def ready_fn(cycle):
        return 1 if cycle % 3 else 0

    def collector():
        result = yield from collect_beats(dut.out,
                                          expected=4,
                                          timeout=5000,
                                          ready_fn=ready_fn)
        beats_out.extend(result)

    run_sim(dut, drv0, drv1, collector)
    data_seq = [d for d, _ in beats_out]
    start = data_seq.index(0)
    assert data_seq[start:start + 3] == [0, 1, 2]
    assert 20 in data_seq
