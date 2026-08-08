"""Golden-vector tests for the project's custom Scapy RoCE layers."""

import socket

import pytest
from scapy.layers.inet import IP, UDP
from scapy.layers.l2 import Ether
from scapy.packet import Raw

from roomsoc.peripheral.net import packet_codec as codec
from roomsoc.peripheral.net.packet_codec import (AETH, BTH, RETH, BthOpcode,
                                                 RDMAConnectionSetup)
from tests.roomsoc.peripheral.net_helpers import assert_valid_ipv4_checksum

DST_MAC = "12:34:56:78:9a:bc"
SRC_MAC = "70:66:55:5a:92:41"
SRC_IP = "192.168.2.1"
DST_IP = "192.168.2.2"


def build_roce(opcode,
               *,
               dst_mac=DST_MAC,
               src_mac=SRC_MAC,
               src_ip=SRC_IP,
               dst_ip=DST_IP,
               vaddr=0,
               r_key=0,
               dmalen=0,
               payload=b"",
               psn=0,
               dest_qp=2,
               src_port=500,
               dst_port=codec.ROCE_PORT,
               identification=1,
               dont_fragment=False,
               mode="roce"):
    payload = bytes(payload)
    pad_count = (-len(payload)) % 4
    bth = BTH(opcode=int(opcode),
              pad_count=pad_count,
              dest_qp=dest_qp,
              psn=psn)
    if opcode in (BthOpcode.RC_RDMA_WRITE_ONLY,
                  BthOpcode.RC_RDMA_READ_REQUEST):
        extended = RETH(vaddr=vaddr, r_key=r_key, dmalen=dmalen)
    else:
        extended = AETH()
    packet = (Ether(dst=dst_mac, src=src_mac)
              / IP(src=src_ip,
                   dst=dst_ip,
                   id=identification,
                   flags="DF" if dont_fragment else 0)
              / UDP(sport=src_port, dport=dst_port)
              / bth / extended / Raw(payload + b"\x00" * pad_count))
    return codec.finalize_roce(packet, mode=mode)


def roce_payload(packet):
    packet = Ether(packet)
    extended = packet[RETH] if packet.haslayer(RETH) else packet[AETH]
    payload_and_pad = bytes(extended.payload)[:-codec.ICRC_SIZE]
    pad_count = packet[BTH].pad_count
    return (payload_and_pad[:-pad_count] if pad_count else payload_and_pad,
            payload_and_pad[-pad_count:] if pad_count else b"")


def test_custom_headers_match_wire_layout():
    assert bytes(BTH(opcode=BthOpcode.RC_RDMA_WRITE_ONLY)) == bytes.fromhex(
        "0a00ffff0000000200000000")
    assert bytes(
        BTH(opcode=BthOpcode.RC_RDMA_READ_REQUEST,
            dest_qp=0x123456,
            psn=0xABCDEF,
            ack_req=1)) == bytes.fromhex(
                "0c00ffff0012345680abcdef")
    assert bytes(RETH(vaddr=0x1230, dmalen=16)) == bytes.fromhex(
        "00000000000012300000000000000010")
    assert bytes(AETH()) == b"\x00" * 4


def test_bth_flags_and_range_validation():
    bth = BTH(solicited=1, migration=1, pad_count=3, version=4)
    assert bytes(bth)[1] == 0xF4
    parsed = BTH(bytes(bth))
    assert (parsed.solicited, parsed.migration, parsed.pad_count,
            parsed.version) == (1, 1, 3, 4)
    with pytest.raises(ValueError):
        bytes(BTH(dest_qp=1 << 24))
    with pytest.raises(ValueError):
        bytes(BTH(psn=1 << 24))


def test_real_icrc_matches_driver_capture():
    captured = bytes.fromhex(
        "123456789abc7cb27d62c1ef0800"
        "4500004c000100004011f54cc0a80201c0a80202"
        "01f412b70038ffa6"
        "0a00ffff0000000200000000"
        "00000000000012300000000000000010"
        "000102030405060708090a0b0c0d0e0f"
        "ca47470e")
    assert_valid_ipv4_checksum(captured)
    assert codec.icrc_matches(captured)
    assert captured[-4:] == codec.compute_icrc(captured[14:-4])


def test_rtl_icrc_matches_hardware_captures():
    ack = bytes.fromhex(
        "241c04f325d7123456789abc0800"
        "45000030000040004011b569c0a80202c0a80201"
        "12b71f40001c0000"
        "1100ffff0000000200000000"
        "00000000"
        "392aa6c7")
    assert_valid_ipv4_checksum(ack)
    assert Ether(ack)[UDP].chksum == 0
    assert codec.icrc_matches(ack, mode="rtl")
    assert ack[-4:] == codec.compute_rtl_icrc(ack[14:-4])


def test_scapy_roce_write_matches_driver_packet():
    packet = build_roce(BthOpcode.RC_RDMA_WRITE_ONLY,
                        src_mac="7c:b2:7d:62:c1:ef",
                        vaddr=0x1230,
                        dmalen=16,
                        payload=bytes(range(16)))
    expected = bytes.fromhex(
        "123456789abc7cb27d62c1ef0800"
        "4500004c000100004011f54cc0a80201c0a80202"
        "01f412b70038ffa6"
        "0a00ffff0000000200000000"
        "00000000000012300000000000000010"
        "000102030405060708090a0b0c0d0e0f"
        "ca47470e")
    assert packet == expected


def test_scapy_roce_read_request_matches_driver_packet():
    packet = build_roce(BthOpcode.RC_RDMA_READ_REQUEST,
                        src_mac="7c:b2:7d:62:c1:ef",
                        vaddr=0x1230,
                        dmalen=16)
    expected = bytes.fromhex(
        "123456789abc7cb27d62c1ef0800"
        "4500003c000100004011f55cc0a80201c0a80202"
        "01f412b700286763"
        "0c00ffff0000000200000000"
        "00000000000012300000000000000010"
        "6e0f71ea")
    assert packet == expected


def test_scapy_rtl_packets_match_hardware_captures():
    ack = build_roce(BthOpcode.RC_ACKNOWLEDGE,
                     dst_mac="24:1c:04:f3:25:d7",
                     src_mac=DST_MAC,
                     src_ip=DST_IP,
                     dst_ip=SRC_IP,
                     src_port=codec.ROCE_PORT,
                     dst_port=8000,
                     identification=0,
                     dont_fragment=True,
                     mode="rtl")
    assert ack == bytes.fromhex(
        "241c04f325d7123456789abc0800"
        "45000030000040004011b569c0a80202c0a80201"
        "12b71f40001c0000"
        "1100ffff0000000200000000"
        "00000000"
        "392aa6c7")


@pytest.mark.parametrize("opcode,payload", [
    (BthOpcode.RC_RDMA_WRITE_ONLY, b"abcde"),
    (BthOpcode.RC_RDMA_READ_RESPONSE_ONLY, bytes(range(16))),
    (BthOpcode.RC_ACKNOWLEDGE, b""),
])
def test_scapy_roundtrip_and_padding(opcode, payload):
    packet = build_roce(opcode,
                        vaddr=0x1230,
                        dmalen=len(payload),
                        payload=payload)
    assert_valid_ipv4_checksum(packet)
    parsed = Ether(packet)
    assert parsed[BTH].opcode == opcode
    assert parsed[BTH].dest_qp == 2
    if parsed.haslayer(RETH):
        assert parsed[RETH].vaddr == 0x1230
        assert parsed[RETH].dmalen == len(payload)
    decoded, padding = roce_payload(packet)
    assert decoded == payload
    assert padding == b"\x00" * ((-len(payload)) % 4)
    assert codec.icrc_matches(packet)


def test_icrc_detects_corruption():
    packet = bytearray(
        build_roce(BthOpcode.RC_RDMA_WRITE_ONLY,
                   vaddr=0x1230,
                   dmalen=4,
                   payload=b"\xab\xcd\xef\xff"))
    packet[20] ^= 0xFF
    with pytest.raises(AssertionError, match="invalid IPv4 header checksum"):
        assert_valid_ipv4_checksum(packet)
    assert not codec.icrc_matches(packet)


def test_connection_setup_layer_matches_record_and_golden_packet():
    setup = RDMAConnectionSetup(
        local_qpn=2,
        remote_qpn=2,
        remote_ip=socket.inet_aton(SRC_IP) + b"\x00" * 12,
        remote_port=8000)
    packet = bytes(
        Ether(dst=DST_MAC, src=SRC_MAC)
        / IP(src=SRC_IP, dst=DST_IP, id=1)
        / UDP(sport=500, dport=8000) / setup)
    assert_valid_ipv4_checksum(packet)
    assert packet == bytes.fromhex(
        "123456789abc7066555a92410800"
        "4500003a000100004011f55ec0a80201c0a80202"
        "01f41f4000265251"
        "0200000002000000"
        "c0a80201000000000000000000000000"
        "401f00000000")
    parsed = Ether(packet)[RDMAConnectionSetup]
    assert parsed.local_qpn == 2
    assert parsed.remote_qpn == 2
    assert parsed.remote_ip[:4] == socket.inet_aton(SRC_IP)
    assert parsed.remote_port == 8000
