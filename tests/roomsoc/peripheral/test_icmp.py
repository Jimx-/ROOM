"""Focused pytest coverage for the IcmpServer.

These tests drive ``Ipv4Handler`` (with the UDP mirror tap enabled) and
``IcmpServer`` with scapy-built packets and compare the replies byte for
byte against scapy's own serialization, which validates the generated
IPv4 headers, the incremental echo-reply checksum and the from-scratch
destination-unreachable checksum.
"""

import pytest
from amaranth import Elaboratable, Module
from amaranth.sim import Simulator

from scapy.layers.inet import IP, UDP, ICMP
from scapy.packet import Raw

from roomsoc.interconnect.axi import AXIStreamInterface
from roomsoc.peripheral.net import IcmpServer, Ipv4Handler

MY_IP = "192.168.2.2"
PEER_IP = "192.168.2.1"
MY_IP_REG = 0x0202a8c0  # byte-swapped packing used across the stack
OPEN_PORTS = (8000, 4791)


class IcmpHarness(Elaboratable):

    def __init__(self, *, open_udp_ports=OPEN_PORTS, **icmp_kwargs):
        self.ipv4_handler = Ipv4Handler(data_width=64,
                                        udp_icmp_tap=True,
                                        enable_icmp=True)
        self.icmp_kwargs = dict(icmp_kwargs)
        self.icmp_kwargs.setdefault("open_udp_ports", open_udp_ports)
        self.icmp_server = IcmpServer(data_width=64, **self.icmp_kwargs)
        self.rx_data_in = AXIStreamInterface(data_width=64)
        self.tx_data_out = AXIStreamInterface(data_width=64, user_width=128)

    def elaborate(self, platform):
        m = Module()

        m.submodules.ipv4_handler = self.ipv4_handler
        m.submodules.icmp_server = self.icmp_server

        m.d.comb += [
            self.rx_data_in.connect(self.ipv4_handler.data_in),
            self.ipv4_handler.my_ip_addr.eq(MY_IP_REG),
            self.ipv4_handler.icmp_data_out.connect(
                self.icmp_server.rx_data_in),
            self.ipv4_handler.udp_icmp_data_out.connect(
                self.icmp_server.udp_rx_data_in),
            self.icmp_server.my_ip_addr.eq(MY_IP_REG),
            self.icmp_server.tx_data_out.connect(self.tx_data_out),
        ]

        return m


def _ip_header(**kwargs):
    return dict(tos=0, id=0, flags="DF", ttl=64, **kwargs)


def _echo_request(payload=b"", **icmp_kwargs):
    request = IP(src=PEER_IP, dst=MY_IP, **_ip_header()) / ICMP(
        type="echo-request", code=0, **icmp_kwargs)
    if payload:
        request = request / Raw(load=payload)
    return request


def _udp_packet(dport, payload=b"closed port probe"):
    return IP(src=PEER_IP, dst=MY_IP, **_ip_header()) / UDP(
        sport=1234, dport=dport) / Raw(load=payload)


def _expected_echo_reply(request):
    request_icmp = request[ICMP]
    payload = bytes(request_icmp.payload)
    reply = IP(src=MY_IP, dst=PEER_IP, **_ip_header()) / ICMP(
        type="echo-reply", code=0, id=request_icmp.id, seq=request_icmp.seq)
    if payload:
        reply = reply / Raw(load=payload)
    return bytes(reply)


def _expected_unreachable(original, cap=528):
    reply = IP(src=MY_IP, dst=PEER_IP, **_ip_header()) / ICMP(
        type=3, code=3) / Raw(load=bytes(original)[:cap])
    return bytes(reply)


def _drive_packets(stream, packets, gap=20):
    yield stream.valid.eq(0)

    for packet in packets:
        for _ in range(gap):
            yield

        for offset in range(0, len(packet), 8):
            beat = packet[offset:offset + 8]
            yield stream.bits.data.eq(int.from_bytes(beat, byteorder="little"))
            yield stream.bits.keep.eq((1 << len(beat)) - 1)
            yield stream.bits.last.eq(offset + len(beat) == len(packet))
            yield stream.valid.eq(1)

            yield
            while not (yield stream.ready):
                yield

        yield stream.valid.eq(0)

    yield stream.valid.eq(0)
    yield


def _collect(dut, replies, timeout=2000):
    yield dut.tx_data_out.ready.eq(1)
    yield

    current = bytearray()
    for _ in range(timeout):
        fire = (yield dut.tx_data_out.valid) & (yield dut.tx_data_out.ready)
        if fire:
            data = (yield dut.tx_data_out.bits.data)
            keep = (yield dut.tx_data_out.bits.keep)
            last = (yield dut.tx_data_out.bits.last)

            for lane in range(8):
                if keep & (1 << lane):
                    current.append((data >> (lane * 8)) & 0xFF)

            if last:
                replies.append(bytes(current))
                current.clear()
        yield

    assert not current, "simulation ended in the middle of a reply frame"


def run_icmp(packets, *, gap=20, timeout=2000, **harness_kwargs):
    dut = IcmpHarness(**harness_kwargs)

    replies = []

    def drive():
        yield from _drive_packets(dut.rx_data_in, packets, gap=gap)

    def collect():
        yield from _collect(dut, replies, timeout=timeout)

    def drain_mirrors():
        yield dut.ipv4_handler.tcp_data_out.ready.eq(1)
        yield dut.ipv4_handler.udp_data_out.ready.eq(1)
        yield dut.ipv4_handler.roce_data_out.ready.eq(1)
        yield

    simulator = Simulator(dut)
    simulator.add_clock(1e-6)
    simulator.add_sync_process(drive)
    simulator.add_sync_process(collect)
    simulator.add_sync_process(drain_mirrors)
    simulator.run()

    return replies


def test_echo_request_is_answered():
    request = _echo_request(b"icmp server echo payload", id=0x1234, seq=1)
    assert run_icmp([bytes(request)]) == [_expected_echo_reply(request)]


def test_empty_payload_echo_is_answered():
    request = _echo_request(id=0xbeef, seq=2)
    assert len(bytes(request)) == 28
    assert run_icmp([bytes(request)]) == [_expected_echo_reply(request)]


def test_ethernet_padding_is_trimmed():
    # Odd-length payload plus Ethernet minimum-frame padding: the reply
    # must mirror only the payload covered by the IP total length.
    request = _echo_request(b"pad", id=7, seq=3)
    raw = bytes(request)
    assert len(raw) == 31
    padded = raw + b"\x00" * (60 - len(raw))
    assert run_icmp([padded]) == [_expected_echo_reply(request)]


def test_bad_checksum_echo_is_dropped_then_recovers():
    good = _echo_request(b"recovery probe", id=0x55, seq=4)
    raw = bytearray(bytes(good))
    # Bytes 22-23 are the ICMP checksum field.
    raw[22] ^= 0xFF
    replies = run_icmp([bytes(raw), bytes(good)], gap=100)
    assert replies == [_expected_echo_reply(good)]


def test_non_echo_icmp_is_dropped():
    timestamp = IP(src=PEER_IP, dst=MY_IP, **_ip_header()) / ICMP(
        type=13, code=0, id=1, seq=1) / Raw(load=b"ts")
    other = _echo_request(b"still answered", id=9, seq=5)
    replies = run_icmp([bytes(timestamp), bytes(other)], gap=100)
    assert replies == [_expected_echo_reply(other)]


def test_oversized_echo_is_dropped_then_recovers():
    big = _echo_request(bytes(range(256)) * 2, id=0x77, seq=6)
    small = _echo_request(b"y" * 32, id=0x78, seq=7)
    replies = run_icmp([bytes(big), bytes(small)],
                       payload_depth=8,
                       unreachable_cap=64,
                       gap=100)
    assert replies == [_expected_echo_reply(small)]


def test_udp_closed_port_gets_unreachable():
    original = _udp_packet(9999)
    assert run_icmp([bytes(original)]) == [_expected_unreachable(original)]


def test_udp_open_ports_are_not_answered():
    packets = [bytes(_udp_packet(port)) for port in OPEN_PORTS]
    assert run_icmp(packets, gap=100) == []


def test_unreachable_embeds_at_most_cap_bytes():
    original = _udp_packet(9999, bytes(range(256)) * 3)
    assert len(bytes(original)) == 796
    replies = run_icmp([bytes(original)])
    assert replies == [_expected_unreachable(original)]
    assert len(replies[0]) == 20 + 8 + 528


def test_udp_fragment_is_not_answered():
    fragment = IP(src=PEER_IP, dst=MY_IP, id=7, flags="MF", frag=8,
                  ttl=64) / UDP(sport=1234, dport=9999) / Raw(load=b"frag")
    assert run_icmp([bytes(fragment)]) == []


def test_broadcast_udp_is_not_answered():
    broadcast = IP(src=PEER_IP, dst="255.255.255.255", **_ip_header()) / UDP(
        sport=1234, dport=9999) / Raw(load=b"bcast")
    assert run_icmp([bytes(broadcast)]) == []


def test_unreachable_and_echo_interleave():
    original = _udp_packet(9999, b"interleave")
    ping = _echo_request(b"interleave ping", id=0x99, seq=8)
    replies = run_icmp([bytes(original), bytes(ping)], gap=200)
    assert replies == [
        _expected_unreachable(original),
        _expected_echo_reply(ping),
    ]


def test_simultaneous_ingress_claims_favor_echo():
    dut = IcmpServer(data_width=64, open_udp_ports=OPEN_PORTS)
    ping = _echo_request(bytes(range(128)))
    udp = _udp_packet(9999, b"closed port")
    replies = []

    def drive_echo():
        yield dut.my_ip_addr.eq(MY_IP_REG)
        yield from _drive_packets(dut.rx_data_in, [bytes(ping)], gap=0)

    def drive_udp():
        # Align the first buffered UDP beat with the first echo payload beat.
        yield from _drive_packets(dut.udp_rx_data_in, [bytes(udp)], gap=4)

    def collect():
        yield from _collect(dut, replies, timeout=200)

    simulator = Simulator(dut)
    simulator.add_clock(1e-6)
    simulator.add_sync_process(drive_echo)
    simulator.add_sync_process(drive_udp)
    simulator.add_sync_process(collect)
    simulator.run()
    assert replies == [_expected_echo_reply(ping)]
