"""Focused pytest coverage for the Ipv4Handler RX datapath.

These tests exercise the checksum/dropper corner cases of
``roomsoc.peripheral.net.ipv4`` directly: invalid checksums, packets
truncated inside the IPv4 header, IP option headers, and back-to-back
frames that fill the internal checksum queue.
"""

import pytest
from amaranth.sim import Settle, Simulator
from scapy.layers.inet import IP, IPOption, UDP
from scapy.packet import Raw

from roomsoc.peripheral.net import Ipv4Handler
from roomsoc.peripheral.net.ipv4 import Ipv4Checksum

MY_IP = "192.168.2.2"
PEER_IP = "192.168.2.1"


def _ip_packet(payload=b"rdma ipv4 handler test payload", **kwargs):
    return bytes(
        IP(src=PEER_IP, dst=MY_IP, **kwargs) / UDP(sport=1234, dport=8000) /
        Raw(load=payload))


def _corrupt_checksum(packet):
    return packet[:10] + b"\xbe\xef" + packet[12:]


def _drive_packets(dut, packets, gap=20):
    yield dut.data_in.valid.eq(0)

    for packet in packets:
        for _ in range(gap):
            yield

        for offset in range(0, len(packet), 8):
            beat = packet[offset:offset + 8]
            yield dut.data_in.bits.data.eq(
                int.from_bytes(beat, byteorder="little"))
            yield dut.data_in.bits.keep.eq((1 << len(beat)) - 1)
            yield dut.data_in.bits.last.eq(offset + len(beat) == len(packet))
            yield dut.data_in.valid.eq(1)

            yield
            while not (yield dut.data_in.ready):
                yield

        yield dut.data_in.valid.eq(0)

    yield dut.data_in.valid.eq(0)
    yield


def _collect(dut, stream, packets, timeout=1000):
    yield stream.ready.eq(1)
    yield

    current = bytearray()
    for _ in range(timeout):
        fire = (yield stream.valid) & (yield stream.ready)
        if fire:
            data = (yield stream.bits.data)
            keep = (yield stream.bits.keep)
            last = (yield stream.bits.last)

            for lane in range(8):
                if keep & (1 << lane):
                    current.append((data >> (lane * 8)) & 0xFF)

            if last:
                packets.append(bytes(current))
                current.clear()
        yield

    assert not current, "simulation ended in the middle of a forwarded frame"


def run_ipv4_handler(packets, *, gap=20, timeout=1000):
    dut = Ipv4Handler(data_width=64)

    forwarded = []

    def drive():
        yield dut.my_ip_addr.eq(0x0202a8c0)
        yield from _drive_packets(dut, packets, gap=gap)

    def collect():
        yield from _collect(dut, dut.udp_data_out, forwarded, timeout=timeout)

    def drain_mirrors():
        yield dut.tcp_data_out.ready.eq(1)
        yield dut.roce_data_out.ready.eq(1)
        yield

    simulator = Simulator(dut)
    simulator.add_clock(1e-6)
    simulator.add_sync_process(drive)
    simulator.add_sync_process(collect)
    simulator.add_sync_process(drain_mirrors)
    simulator.run()

    return forwarded


def test_valid_packet_is_forwarded():
    packet = _ip_packet()
    assert run_ipv4_handler([packet]) == [packet]


def test_invalid_checksum_is_dropped_and_pipeline_recovers():
    good = _ip_packet()
    bad = _corrupt_checksum(good)
    forwarded = run_ipv4_handler([bad, good])
    assert forwarded == [good]


def test_truncated_header_is_dropped_and_pipeline_recovers():
    good = _ip_packet()
    truncated = _ip_packet()[:8]
    forwarded = run_ipv4_handler([truncated, good])
    assert forwarded == [good]


def test_ip_options_header_is_forwarded():
    packet = _ip_packet(options=IPOption(b"\x01\x01\x01\x01"))
    assert packet[0] & 0x0F == 6  # IHL = 6 words exercises late beats
    assert run_ipv4_handler([packet]) == [packet]


def test_back_to_back_packets_are_all_forwarded():
    packets = [_ip_packet(payload=bytes([i] * 40)) for i in range(4)]
    forwarded = run_ipv4_handler(packets, gap=0)
    assert forwarded == packets


def test_ip_option_back_to_back_packets_are_all_forwarded():
    packets = [
        _ip_packet(payload=bytes([i] * 24), options=IPOption(b"\x01" * 4 * i))
        for i in range(4)
    ]
    forwarded = run_ipv4_handler(packets, gap=0)
    assert forwarded == packets


def _run_checksum(packets, data_width):
    dut = Ipv4Checksum(data_width, skip_checksum=False)
    beat_bytes = data_width // 8
    expected_beats = [(int.from_bytes(packet[offset:offset + beat_bytes],
                                      "little"),
                       (1 << len(packet[offset:offset + beat_bytes])) - 1,
                       offset + beat_bytes >= len(packet))
                      for packet in packets
                      for offset in range(0, len(packet), beat_bytes)]
    forwarded = []
    checksums = []

    def drive():
        for data, keep, last in expected_beats:
            yield dut.data_in.valid.eq(1)
            yield dut.data_in.bits.data.eq(data)
            yield dut.data_in.bits.keep.eq(keep)
            yield dut.data_in.bits.last.eq(last)
            for _ in range(100):
                yield Settle()
                ready = (yield dut.data_in.ready)
                yield
                if ready:
                    break
            else:
                pytest.fail("checksum input stalled")
        yield dut.data_in.valid.eq(0)

    def collect():
        yield dut.data_out.ready.eq(1)
        yield dut.checksum.ready.eq(1)
        for _ in range(200):
            yield Settle()
            if (yield dut.data_out.valid):
                forwarded.append(
                    ((yield dut.data_out.bits.data),
                     (yield dut.data_out.bits.keep), (yield
                                                      dut.data_out.bits.last)))
            if (yield dut.checksum.valid):
                checksums.append((yield dut.checksum.bits))
            yield

    simulator = Simulator(dut)
    simulator.add_clock(1e-6)
    simulator.add_sync_process(drive)
    simulator.add_sync_process(collect)
    simulator.run()
    assert forwarded == expected_beats
    assert len(checksums) == len(packets)
    return checksums


def test_checksum_truncated_packet_back_to_back_preserves_beats():
    good = _ip_packet()
    checksums = _run_checksum([good[:8], good], data_width=64)
    assert checksums[1] == 0


@pytest.mark.parametrize("data_width", [256, 512])
@pytest.mark.parametrize("option_bytes", [0, 4])
def test_checksum_wide_first_beat_excludes_payload(data_width, option_bytes):
    packet = _ip_packet(payload=b"",
                        options=IPOption(b"\x01" *
                                         option_bytes) if option_bytes else [])
    assert len(packet) * 8 <= data_width
    assert _run_checksum([packet], data_width) == [0]
