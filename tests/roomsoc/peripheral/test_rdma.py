"""End-to-end pytest coverage for the RDMA Ethernet simulation."""

import pytest

import random
import socket
from pathlib import Path

from amaranth import (Cat, Const, Elaboratable, Memory, Module, Mux, Record,
                      Signal)
from amaranth.hdl.rec import Direction
from amaranth.sim import Simulator
from scapy.layers.inet import IP, UDP
from scapy.layers.l2 import ARP, Ether
from scapy.packet import Raw

from roomsoc.interconnect import axi
from roomsoc.interconnect.axi import (AXIStreamArbiter,
                                     AXIStreamDepacketizer,
                                     AXIStreamInterface)
from roomsoc.interconnect.stream import Decoupled, Queue
import roomsoc.interconnect.wishbone as wb
from roomsoc.peripheral.dma import AXIDMAReader, AXIDMAWriter
from roomsoc.peripheral.net import (ArpServerSubnet, EthernetFramePadding,
                                    EthernetRouter, Ipv4Handler, MacIpEncoder,
                                    Rocev2Stack, UdpStack)
from roomsoc.peripheral.net import packet_codec as roce_codec
from roomsoc.peripheral.net.packet_codec import (AETH, BTH, RETH, BthOpcode,
                                                 RDMAConnectionSetup)
from roomsoc.peripheral.net.udp import UdpIpMetadata


def build_test_packets():
    """Build the ingress frames used by this RDMA simulation."""
    device_mac = b"\x12\x34\x56\x78\x9a\xbc"
    host_mac = b"$\x1c\x04\xf3%\xd7"
    driver_mac = b"pfUZ\x92A"
    alternate_driver_mac = b"|\xb2}b\xc1\xef"
    source_ip = "192.168.2.1"
    destination_ip = "192.168.2.2"

    dns_payload = bytes.fromhex(
        "716e81850001000000000000"
        "066d6f62696c65066576656e74730464617461"
        "096d6963726f736f667403636f6d00"
        "00010001")
    roce_common = {
        "dst_mac": device_mac,
        "src_ip": source_ip,
        "dst_ip": destination_ip,
    }

    return [
        _arp_request(host_mac, source_ip),
        _build_udp_packet(
            b"\x98\xee\xcb\xb6f\xb0",
            b".\x22\x04d>\xc4",
            "192.168.1.1",
            "192.168.1.107",
            53,
            51186,
            dns_payload,
            identification=0x3f99,
            dont_fragment=True),
        _connection_setup(
            driver_mac,
            local_qpn=2,
            remote_qpn=2,
            remote_ip=source_ip,
            remote_port=8000,
            initial_rx_psn=1),
        _build_roce_packet(
            src_mac=alternate_driver_mac,
            opcode=BthOpcode.RC_RDMA_WRITE_ONLY,
            vaddr=0x1230,
            dmalen=16,
            payload=bytes(range(16)),
            psn=1,
            **roce_common),
        _build_roce_packet(
            src_mac=driver_mac,
            opcode=BthOpcode.RC_RDMA_WRITE_ONLY,
            vaddr=0x1234,
            dmalen=4,
            payload=bytes.fromhex("abcdefff"),
            psn=2,
            **roce_common),
        _build_roce_packet(
            src_mac=alternate_driver_mac,
            opcode=BthOpcode.RC_RDMA_READ_REQUEST,
            vaddr=0x1230,
            dmalen=16,
            psn=3,
            **roce_common),
        _build_roce_packet(
            src_mac=driver_mac,
            opcode=BthOpcode.RC_RDMA_READ_REQUEST,
            vaddr=0x1230,
            dmalen=4,
            psn=4,
            **roce_common),
        _build_roce_packet(
            src_mac=driver_mac,
            opcode=BthOpcode.RC_RDMA_READ_REQUEST,
            vaddr=0x1234,
            dmalen=4,
            psn=5,
            **roce_common),
    ]


# Standard addressing shared by the focused scenarios. These mirror the
# literals baked into ``build_test_packets`` so every test drives identical
# DUT state (device = 192.168.2.2 / 12:34:56:78:9a:bc, peer = 192.168.2.1).
DEVICE_MAC = b"\x12\x34\x56\x78\x9a\xbc"
HOST_MAC = b"$\x1c\x04\xf3%\xd7"
DRIVER_MAC = b"pfUZ\x92A"
SOURCE_IP = "192.168.2.1"
DEVICE_IP = "192.168.2.2"

_ROCE_COMMON = {"dst_mac": DEVICE_MAC, "src_ip": SOURCE_IP, "dst_ip": DEVICE_IP}


def _mac(value):
    if isinstance(value, str):
        return value
    return ":".join(f"{byte:02x}" for byte in value)


def _build_udp_packet(dst_mac,
                      src_mac,
                      src_ip,
                      dst_ip,
                      src_port,
                      dst_port,
                      payload,
                      *,
                      identification=0,
                      dont_fragment=True):
    packet = (Ether(dst=_mac(dst_mac), src=_mac(src_mac))
              / IP(src=src_ip,
                   dst=dst_ip,
                   id=identification,
                   flags="DF" if dont_fragment else 0)
              / UDP(sport=src_port, dport=dst_port) / Raw(payload))
    return bytes(packet)


def _build_roce_packet(dst_mac,
                       src_mac,
                       src_ip,
                       dst_ip,
                       opcode,
                       *,
                       vaddr=0,
                       r_key=0,
                       dmalen=0,
                       payload=b"",
                       psn=0,
                       ack_req=0,
                       partition=0xffff,
                       dest_qp=2,
                       src_port=500,
                       dst_port=roce_codec.ROCE_PORT,
                       identification=1,
                       dont_fragment=False,
                       syndrome=0,
                       msn=0,
                       icrc_mode="roce"):
    payload = bytes(payload)
    if opcode == BthOpcode.RC_RDMA_WRITE_ONLY and dmalen != len(payload):
        raise ValueError("write dmalen must equal payload length")
    if opcode in (BthOpcode.RC_RDMA_READ_REQUEST,
                  BthOpcode.RC_ACKNOWLEDGE) and payload:
        raise ValueError(f"opcode 0x{int(opcode):02x} cannot carry payload")

    pad_count = (-len(payload)) % 4
    bth = BTH(opcode=int(opcode),
              partition=partition,
              dest_qp=dest_qp,
              ack_req=ack_req,
              psn=psn,
              pad_count=pad_count)
    if opcode in (BthOpcode.RC_RDMA_WRITE_ONLY,
                  BthOpcode.RC_RDMA_READ_REQUEST):
        extended = RETH(vaddr=vaddr, r_key=r_key, dmalen=dmalen)
    elif opcode in (BthOpcode.RC_RDMA_READ_RESPONSE_ONLY,
                    BthOpcode.RC_ACKNOWLEDGE):
        extended = AETH(syndrome=syndrome, msn=msn)
    else:
        raise ValueError(f"unsupported RoCE opcode: 0x{int(opcode):02x}")

    packet = (Ether(dst=_mac(dst_mac), src=_mac(src_mac))
              / IP(src=src_ip,
                   dst=dst_ip,
                   id=identification,
                   flags="DF" if dont_fragment else 0)
              / UDP(sport=src_port, dport=dst_port)
              / bth / extended / Raw(payload + b"\x00" * pad_count))
    return roce_codec.finalize_roce(packet, mode=icrc_mode)


def _roce_payload(packet):
    extended = packet[RETH] if packet.haslayer(RETH) else packet[AETH]
    payload_and_pad = bytes(extended.payload)[:-roce_codec.ICRC_SIZE]
    pad_count = packet[BTH].pad_count
    return payload_and_pad[:-pad_count] if pad_count else payload_and_pad


def _arp_request(src_mac, src_ip, target_ip=DEVICE_IP):
    """ARP request from *src_mac*/*src_ip* addressed to the device."""
    packet = (Ether(dst="ff:ff:ff:ff:ff:ff", src=_mac(src_mac))
              / ARP(op=1,
                    hwsrc=_mac(src_mac),
                    psrc=src_ip,
                    hwdst="00:00:00:00:00:00",
                    pdst=target_ip))
    return bytes(packet).ljust(60, b"\x00")


def _connection_setup(src_mac=DRIVER_MAC,
                      *,
                      local_qpn=2,
                      remote_qpn=2,
                      remote_ip=SOURCE_IP,
                      remote_port=8000,
                      initial_rx_psn=0):
    """UDP connection-setup frame that populates the QP connection table."""
    setup = RDMAConnectionSetup(
        local_qpn=local_qpn,
        remote_qpn=remote_qpn,
        remote_ip=socket.inet_aton(remote_ip) + b"\x00" * 12,
        remote_port=remote_port,
        initial_rx_psn=initial_rx_psn)
    return bytes(
        Ether(dst=_mac(DEVICE_MAC), src=_mac(src_mac))
        / IP(src=SOURCE_IP, dst=DEVICE_IP, id=1)
        / UDP(sport=500, dport=8000) / setup)


def _roce_write(vaddr,
                payload,
                src_mac=DRIVER_MAC,
                dest_qp=2,
                psn=0):
    """RC RDMA_WRITE_ONLY frame carrying *payload* at *vaddr*."""
    return _build_roce_packet(src_mac=src_mac,
                              opcode=BthOpcode.RC_RDMA_WRITE_ONLY,
                              vaddr=vaddr,
                              dmalen=len(payload),
                              payload=payload,
                              dest_qp=dest_qp,
                              psn=psn,
                              **_ROCE_COMMON)


def _roce_read_request(vaddr,
                       dmalen,
                       src_mac=DRIVER_MAC,
                       dest_qp=2,
                       psn=0):
    """RC RDMA_READ_REQUEST frame for *dmalen* bytes at *vaddr*."""
    return _build_roce_packet(src_mac=src_mac,
                              opcode=BthOpcode.RC_RDMA_READ_REQUEST,
                              vaddr=vaddr,
                              dmalen=dmalen,
                              dest_qp=dest_qp,
                              psn=psn,
                              **_ROCE_COMMON)


def _link_established(*ops, initial_rx_psn=1):
    """Prepend ARP + connection-setup so the RDMA op frames in *ops* get ACKed.

    The ARP request seeds the ARP table with the peer's MAC (without it the UDP
    ACKs cannot be addressed and the TX path stalls) and the connection-setup
    frame populates the QP connection table the RoCE TX path looks up.
    """
    return [
        _arp_request(HOST_MAC, SOURCE_IP),
        _connection_setup(initial_rx_psn=initial_rx_psn),
        *ops,
    ]


def test_connection_setup_psn_is_little_endian_32_bit():
    encoded = bytes(RDMAConnectionSetup(initial_rx_psn=0x123456))
    assert encoded[-4:] == b"\x56\x34\x12\x00"
    assert RDMAConnectionSetup(encoded).initial_rx_psn == 0x123456


class RDMAConnectionServer(Elaboratable):

    def __init__(self, data_width):
        self.data_width = data_width
        self.conn_req = Decoupled(Rocev2Stack.ConnectionRequest)
        self.rx_data_in = AXIStreamInterface(data_width=data_width)
        self.rx_meta_in = Decoupled(UdpIpMetadata)
        self.tx_data_out = AXIStreamInterface(data_width=data_width)
        self.tx_meta_out = Decoupled(UdpIpMetadata)

    def elaborate(self, platform):
        m = Module()

        payload_layout = [
            ('local_qpn', 32, Direction.FANOUT),
            ('remote_qpn', 32, Direction.FANOUT),
            ('remote_ip', 128, Direction.FANOUT),
            ('remote_port', 16, Direction.FANOUT),
            ('initial_rx_psn', 32, Direction.FANOUT),
        ]
        meta_q = m.submodules.meta_q = Queue(2, UdpIpMetadata, flow=False)
        depacketizer = m.submodules.depacketizer = AXIStreamDepacketizer(
            Record, payload_layout, data_width=self.data_width)
        m.d.comb += self.rx_meta_in.connect(meta_q.enq)

        setup_valid = Signal()

        with m.FSM():
            with m.State("IDLE"):
                with m.If(meta_q.deq.valid):
                    m.next = "EXTRACT"

            with m.State("EXTRACT"):
                m.d.comb += self.rx_data_in.connect(depacketizer.sink)
                with m.If(depacketizer.source.valid):
                    m.d.sync += [
                        setup_valid.eq(
                            (depacketizer.header.local_qpn[24:] == 0)
                            & (depacketizer.header.remote_qpn[24:] == 0)
                            & (depacketizer.header.initial_rx_psn[24:] == 0)),
                        self.conn_req.bits.local_qpn.eq(
                            depacketizer.header.local_qpn),
                        self.conn_req.bits.remote_qpn.eq(
                            depacketizer.header.remote_qpn),
                        self.conn_req.bits.remote_ip.eq(
                            depacketizer.header.remote_ip),
                        self.conn_req.bits.remote_port.eq(
                            depacketizer.header.remote_port),
                        self.conn_req.bits.initial_rx_psn.eq(
                            depacketizer.header.initial_rx_psn[:24]),
                    ]
                    m.next = "DRAIN"

            with m.State("DRAIN"):
                m.d.comb += depacketizer.source.ready.eq(1)
                with m.If(depacketizer.source.fire
                          & depacketizer.source.bits.last):
                    with m.If(setup_valid):
                        m.next = "WRITE"
                    with m.Else():
                        m.next = "REJECT"

            with m.State("REJECT"):
                m.d.comb += meta_q.deq.ready.eq(1)
                m.next = "IDLE"

            with m.State("WRITE"):
                m.d.comb += self.conn_req.valid.eq(1)
                with m.If(self.conn_req.fire):
                    m.next = "ACK_META"

            with m.State("ACK_META"):
                m.d.comb += [
                    self.tx_meta_out.bits.eq(meta_q.deq.bits),
                    self.tx_meta_out.bits.length.eq(self.data_width // 8),
                    self.tx_meta_out.valid.eq(1),
                ]
                with m.If(self.tx_meta_out.fire):
                    m.d.comb += meta_q.deq.ready.eq(1)
                    m.next = "ACK_DATA"

            with m.State("ACK_DATA"):
                m.d.comb += [
                    self.tx_data_out.bits.data.eq(0),
                    self.tx_data_out.bits.keep.eq(~0),
                    self.tx_data_out.bits.last.eq(1),
                    self.tx_data_out.valid.eq(1),
                ]
                with m.If(self.tx_data_out.fire):
                    m.next = "IDLE"

        return m


class DMAHandler(Elaboratable):

    def __init__(self, addr_width, data_width, fifo_depth=16):
        self.addr_width = addr_width
        self.data_width = data_width
        self.fifo_depth = fifo_depth

        self.bus = axi.AXIInterface(addr_width=addr_width,
                                    data_width=data_width)
        self.read_cmd = Decoupled(Rocev2Stack.MemoryCommand)
        self.read_data = AXIStreamInterface(data_width=data_width)
        self.write_cmd = Decoupled(Rocev2Stack.MemoryCommand)
        self.write_data = AXIStreamInterface(data_width=data_width)

    def elaborate(self, platform):
        m = Module()

        reader = m.submodules.reader = AXIDMAReader(
            bus=self.bus,
            cmd_fifo_depth=self.fifo_depth,
            data_fifo_depth=self.fifo_depth)
        writer = m.submodules.writer = AXIDMAWriter(
            bus=self.bus, cmd_fifo_depth=self.fifo_depth)

        read_active = Signal()
        read_narrow = Signal()
        read_upper_lane = Signal()
        m.d.comb += [
            reader.sink.valid.eq(self.read_cmd.valid & ~read_active),
            reader.sink.bits.addr.eq(self.read_cmd.bits.addr),
            reader.sink.bits.len.eq(self.read_cmd.bits.len),
            self.read_cmd.ready.eq(~read_active & reader.sink.ready),
        ]
        with m.If(self.read_cmd.fire):
            m.d.sync += [
                read_active.eq(1),
                read_narrow.eq(self.read_cmd.bits.len == 4),
                read_upper_lane.eq(self.read_cmd.bits.addr[2]),
            ]

        narrow_read_data = Mux(~read_upper_lane, reader.source.bits.data[:32],
                               reader.source.bits.data[32:])
        m.d.comb += [
            self.read_data.valid.eq(reader.source.valid & read_active),
            self.read_data.bits.data.eq(
                Mux(read_narrow, narrow_read_data, reader.source.bits.data)),
            self.read_data.bits.keep.eq(Mux(read_narrow, 0x0f, 0xff)),
            self.read_data.bits.last.eq(reader.source.bits.last),
            reader.source.ready.eq(read_active & self.read_data.ready),
        ]
        with m.If(self.read_data.fire & self.read_data.bits.last):
            m.d.sync += read_active.eq(0)

        write_active = Signal()
        write_narrow = Signal()
        write_upper_lane = Signal()
        m.d.comb += [
            writer.sink.valid.eq(self.write_cmd.valid & ~write_active),
            writer.sink.bits.addr.eq(self.write_cmd.bits.addr),
            writer.sink.bits.len.eq(self.write_cmd.bits.len),
            self.write_cmd.ready.eq(~write_active & writer.sink.ready),
        ]
        with m.If(self.write_cmd.fire):
            m.d.sync += [
                write_active.eq(1),
                write_narrow.eq(self.write_cmd.bits.len == 4),
                write_upper_lane.eq(self.write_cmd.bits.addr[2]),
            ]

        narrow_write_data = Mux(
            write_upper_lane,
            Cat(Const(0, 32), self.write_data.bits.data[:32]),
            Cat(self.write_data.bits.data[:32], Const(0, 32)))
        narrow_write_strb = Mux(write_upper_lane, 0xf0, 0x0f)
        m.d.comb += [
            writer.data.valid.eq(write_active & self.write_data.valid),
            writer.data.bits.data.eq(
                Mux(write_narrow, narrow_write_data,
                    self.write_data.bits.data)),
            writer.data.bits.strb.eq(
                Mux(write_narrow, narrow_write_strb,
                    (1 << (self.data_width // 8)) - 1)),
            self.write_data.ready.eq(write_active & writer.data.ready),
        ]
        with m.If(write_active & writer.done):
            m.d.sync += write_active.eq(0)

        return m


class Top(Elaboratable):
    SRAM_DEPTH = 256

    def __init__(self):
        self.rx_data_in = AXIStreamInterface(data_width=64)
        self.tx_data_out = AXIStreamInterface(data_width=64)
        self.sram_inspect_bus = wb.Interface(addr_width=29,
                                             data_width=64,
                                             granularity=8)

    def elaborate(self, platform):
        m = Module()

        eth_router = m.submodules.eth_router = EthernetRouter(data_width=64)
        arp_server = m.submodules.arp_server = ArpServerSubnet(data_width=64)
        ipv4_handler = m.submodules.ipv4_handler = Ipv4Handler(data_width=64)
        udp_stack = m.submodules.udp_stack = UdpStack(data_width=64, port=8000)
        roce_stack = m.submodules.roce_stack = Rocev2Stack(data_width=64)
        connection_server = m.submodules.connection_server = RDMAConnectionServer(
            data_width=64)
        mac_ip_encoder = m.submodules.mac_ip_encoder = MacIpEncoder(
            data_width=64)

        mac_addr = 0xbc9a78563412
        ip_addr = 0x0202a8c0

        ip_merger = m.submodules.ip_merger = AXIStreamArbiter(
            2, data_width=64, user_width=128)
        m.d.comb += [
            self.rx_data_in.connect(eth_router.data_in),
            eth_router.ipv4_data_out.connect(ipv4_handler.data_in),
            eth_router.arp_data_out.connect(arp_server.rx_data_in),
            eth_router.ipv6_data_out.ready.eq(1),
            arp_server.my_mac_addr.eq(mac_addr),
            arp_server.my_ip_addr.eq(ip_addr),
            ipv4_handler.my_ip_addr.eq(ip_addr),
            ipv4_handler.udp_data_out.connect(udp_stack.rx_data_in),
            ipv4_handler.tcp_data_out.ready.eq(1),
            udp_stack.my_ip_addr.eq(ip_addr),
            udp_stack.rx_data_out.connect(connection_server.rx_data_in),
            udp_stack.rx_meta_out.connect(connection_server.rx_meta_in),
            connection_server.tx_data_out.connect(udp_stack.tx_data_in),
            connection_server.tx_meta_out.connect(udp_stack.tx_meta_in),
            udp_stack.tx_data_out.connect(ip_merger.inp[0]),
            roce_stack.my_ip_addr.eq(ip_addr),
            connection_server.conn_req.connect(roce_stack.conn_req),
            roce_stack.tx_data_out.connect(ip_merger.inp[1]),
            ipv4_handler.roce_data_out.connect(roce_stack.rx_data_in),
            ip_merger.out.connect(mac_ip_encoder.data_in),
            mac_ip_encoder.my_mac_addr.eq(mac_addr),
            mac_ip_encoder.arp_table_req.connect(arp_server.arp_table_req),
            mac_ip_encoder.arp_table_resp.eq(arp_server.arp_table_resp),
        ]

        eth_merger = m.submodules.eth_merger = AXIStreamArbiter(
            2, data_width=64)
        frame_padding = m.submodules.frame_padding = EthernetFramePadding(
            data_width=64)
        m.d.comb += [
            arp_server.tx_data_out.connect(eth_merger.inp[0]),
            mac_ip_encoder.data_out.connect(eth_merger.inp[1]),
            eth_merger.out.connect(frame_padding.data_in),
            frame_padding.data_out.connect(self.tx_data_out),
        ]

        dma_handler = m.submodules.dma_handler = DMAHandler(addr_width=32,
                                                            data_width=64)
        m.d.comb += [
            roce_stack.mem_read_cmd.connect(dma_handler.read_cmd),
            roce_stack.mem_write_cmd.connect(dma_handler.write_cmd),
            roce_stack.mem_write_data.connect(dma_handler.write_data),
            dma_handler.read_data.connect(roce_stack.mem_read_data),
        ]

        dma_sram_bus = wb.Interface(addr_width=29,
                                    data_width=64,
                                    granularity=8)
        memory = Memory(width=64,
                        depth=self.SRAM_DEPTH,
                        init=range(self.SRAM_DEPTH))
        m.submodules.axi2wb = axi.AXI2Wishbone(dma_handler.bus, dma_sram_bus)
        sram_bus = wb.Interface(addr_width=29, data_width=64, granularity=8)
        with m.If(self.sram_inspect_bus.cyc):
            m.d.comb += self.sram_inspect_bus.connect(sram_bus)
        with m.Else():
            m.d.comb += dma_sram_bus.connect(sram_bus)
        m.submodules.sram = wb.SRAM(memory, bus=sram_bus)

        return m


def _drive_rx_packets(dut, packets, gap=20, bubble_cycles_fn=None):
    """Drive complete Ethernet frames into the DUT's RX AXI stream.

    *gap* idle cycles separate frames; lowering it (e.g. to 0) streams frames
    back-to-back and forces the DUT to assert RX backpressure itself.
    *bubble_cycles_fn*, when supplied, receives ``(packet_index, beat_index)``
    and returns the number of cycles for which ``valid`` is deasserted before
    that beat. This permits bubbles inside a frame, not just between frames.
    """
    yield dut.rx_data_in.valid.eq(0)

    for packet_index, packet in enumerate(packets):
        for _ in range(gap):
            yield

        for beat_index, offset in enumerate(range(0, len(packet), 8)):
            bubble_cycles = (0 if bubble_cycles_fn is None else
                             bubble_cycles_fn(packet_index, beat_index))
            if bubble_cycles:
                yield dut.rx_data_in.valid.eq(0)
                for _ in range(bubble_cycles):
                    yield

            beat = packet[offset:offset + 8]
            yield dut.rx_data_in.bits.data.eq(
                int.from_bytes(beat, byteorder="little"))
            yield dut.rx_data_in.bits.keep.eq((1 << len(beat)) - 1)
            yield dut.rx_data_in.bits.last.eq(offset + len(beat) == len(packet))
            yield dut.rx_data_in.valid.eq(1)

            yield
            while not (yield dut.rx_data_in.ready):
                yield

        # Keep valid asserted across beats, but lower it before any requested
        # inter-frame idle cycles. With gap=0 the next packet remains
        # back-to-back, as intended.
        yield dut.rx_data_in.valid.eq(0)

    yield dut.rx_data_in.valid.eq(0)
    yield


def _collect_tx_packets(dut, packets, timeout=1000, ready_fn=None):
    """Collect complete Ethernet frames from the DUT's TX AXI stream.

    *ready_fn*, if given, is called with the cycle index each iteration and its
    return value drives ``tx_data_out.ready`` to exercise TX backpressure. A
    beat is only collected when ready is asserted that cycle, matching the edge
    at which the beat actually fires.
    """
    current = bytearray()
    stalled_beat = None
    yield dut.tx_data_out.ready.eq(1 if ready_fn is None else ready_fn(0))
    yield

    for cycle in range(timeout):
        ready = (yield dut.tx_data_out.ready)
        valid = (yield dut.tx_data_out.valid)
        beat = ((yield dut.tx_data_out.bits.data),
                (yield dut.tx_data_out.bits.keep),
                (yield dut.tx_data_out.bits.last))

        # AXI-stream payload and control must remain unchanged from the cycle
        # after valid was observed until the stalled transfer is accepted.
        if stalled_beat is not None:
            assert valid, "TX valid dropped while a beat was stalled"
            assert beat == stalled_beat, "TX beat changed while ready was low"

        fire = valid and ready
        before = bytes(current)
        if fire:
            data, keep, last = beat

            for lane in range(8):
                if keep & (1 << lane):
                    current.append((data >> (lane * 8)) & 0xFF)

            if last:
                packets.append(bytes(current))
                current.clear()
        else:
            # The frame reconstruction is itself the transfer monitor: no
            # bytes may move unless both sides complete the handshake.
            assert bytes(current) == before

        stalled_beat = beat if valid and not ready else None

        next_cycle = cycle + 1
        yield dut.tx_data_out.ready.eq(
            1 if ready_fn is None else ready_fn(next_cycle))
        yield

    assert not current, "simulation ended in the middle of a TX frame"


def _capture_sram_after_image(dut, addresses, after_image, delay=1000):
    for _ in range(delay):
        yield

    bus = dut.sram_inspect_bus
    for address in addresses:
        yield bus.adr.eq(address)
        yield bus.we.eq(0)
        yield bus.sel.eq(0xff)
        yield bus.cyc.eq(1)
        yield bus.stb.eq(1)
        yield
        while not (yield bus.ack):
            yield
        after_image[address] = (yield bus.dat_r)
        yield bus.cyc.eq(0)
        yield bus.stb.eq(0)
        yield


def _write_packet_capture(path, ingress, egress):
    """Write a direction-marked hexdump accepted by ``text2pcap -D``."""
    with Path(path).open("w") as capture:
        for direction, packets in (("I", ingress), ("O", egress)):
            for packet in packets:
                for offset in range(0, len(packet), 16):
                    chunk = packet[offset:offset + 16]
                    octets = " ".join(f"{byte:02x}" for byte in chunk)
                    capture.write(f"{direction} {offset:06x} {octets}\n")
                capture.write(f"{direction} {len(packet):06x}\n")


def run_rdma(requests,
             dut=None,
             *,
             collect_timeout=1000,
             sram_addresses=(),
             sram_capture_delay=1000,
             tx_ready_fn=None,
             rx_gap=20,
             rx_bubble_cycles_fn=None):
    """Drive *requests* through the DUT and collect the TX responses.

    Each request frame is streamed into ``dut.rx_data_in`` with *rx_gap* idle
    cycles between frames. *rx_bubble_cycles_fn* can additionally deassert RX
    ``valid`` between beats of a frame. The TX stream is drained for
    *collect_timeout* cycles with ``ready`` driven by *tx_ready_fn* (``None`` =
    always ready, exercising TX backpressure otherwise). When *sram_addresses*
    is non-empty, those SRAM words are read back over the inspect bus after
    *sram_capture_delay* cycles. Returns ``(responses, sram_image)``.
    """
    if dut is None:
        dut = Top()

    responses = []
    sram_image = {}

    def drive_rx():
        yield from _drive_rx_packets(dut,
                                     requests,
                                     gap=rx_gap,
                                     bubble_cycles_fn=rx_bubble_cycles_fn)

    def collect_tx():
        yield from _collect_tx_packets(dut,
                                       responses,
                                       timeout=collect_timeout,
                                       ready_fn=tx_ready_fn)

    def inspect_sram():
        yield from _capture_sram_after_image(dut,
                                             sram_addresses,
                                             sram_image,
                                             delay=sram_capture_delay)

    simulator = Simulator(dut)
    simulator.add_clock(1e-6)
    simulator.add_sync_process(drive_rx)
    simulator.add_sync_process(collect_tx)
    if sram_addresses:
        simulator.add_sync_process(inspect_sram)
    simulator.run()
    return responses, sram_image


def test_rdma_write_ack_and_readback(request):
    requests = build_test_packets()
    first_word = (0x1230 // 8) % Top.SRAM_DEPTH
    responses, after_image = run_rdma(
        requests, sram_addresses=(first_word, first_word + 1))

    capture_path = request.config.getoption("--rdma-packet-capture")
    if capture_path is not None:
        _write_packet_capture(capture_path, requests, responses)

    assert len(responses) == 7

    arp_reply = Ether(responses[0])[ARP]
    assert arp_reply.op == 2
    assert arp_reply.psrc == "192.168.2.2"
    assert arp_reply.pdst == "192.168.2.1"

    connection_ack = Ether(responses[1])
    assert IP in connection_ack and UDP in connection_ack
    assert (connection_ack[UDP].sport, connection_ack[UDP].dport) == (8000,
                                                                      500)
    connection_payload = bytes(connection_ack[UDP].payload)
    assert connection_payload[:connection_ack[UDP].len - 8] == b"\x00" * 8

    roce = [Ether(frame) for frame in responses[2:]]
    assert [packet[BTH].opcode for packet in roce] == [
        BthOpcode.RC_ACKNOWLEDGE,
        BthOpcode.RC_ACKNOWLEDGE,
        BthOpcode.RC_RDMA_READ_RESPONSE_ONLY,
        BthOpcode.RC_RDMA_READ_RESPONSE_ONLY,
        BthOpcode.RC_RDMA_READ_RESPONSE_ONLY,
    ]
    assert all(roce_codec.icrc_matches(frame, mode="rtl")
               for frame in responses[2:])
    assert [_roce_payload(packet) for packet in roce] == [
        b"",
        b"",
        bytes.fromhex("00010203abcdefff08090a0b0c0d0e0f"),
        bytes.fromhex("00010203"),
        bytes.fromhex("abcdefff"),
    ]
    assert after_image == {
        first_word: 0xffefcdab03020100,
        first_word + 1: 0x0f0e0d0c0b0a0908,
    }


@pytest.mark.parametrize("req_mac, req_ip",
                         [(HOST_MAC, SOURCE_IP),
                          (DRIVER_MAC, "10.0.0.1")])
def test_arp_request_is_replied_with_device_address(req_mac, req_ip):
    # A lone ARP request is answered with exactly one reply whose sender
    # protocol address is the device and whose target protocol address echoes
    # the requester. (The server replies to any request, so the unrelated
    # requester IP still exercises the table-seeding path.)
    requests = [_arp_request(req_mac, req_ip)]
    responses, _ = run_rdma(requests)

    assert len(responses) == 1
    reply = Ether(responses[0])[ARP]
    assert reply.op == 2
    assert reply.psrc == DEVICE_IP
    assert reply.pdst == req_ip


@pytest.mark.parametrize("vaddr, payload", [
    (0x1230, bytes(range(8))),  # single full beat, lower word
    (0x1230, bytes(range(16))),  # two full beats
    (0x1260, bytes(range(24))),  # three full beats
    (0x1260, bytes(range(32))),  # four full beats
    (0x1230, bytes.fromhex("deadbeef")),  # narrow lower lane (addr[2] == 0)
    (0x1234, bytes.fromhex("abcdefff")),  # narrow upper lane (addr[2] == 1)
    (0x07F0, bytes(range(32))),  # multi-beat crossing the SRAM wrap (word 254 -> 0)
])
def test_rdma_write_then_read_roundtrip(vaddr, payload):
    # After link setup, a single WRITE followed by a READ of the same range
    # ACKs the write and echoes the written payload back. The matrix exercises
    # every DMA geometry the DMAHandler distinguishes -- full 8-byte beats,
    # narrow 4-byte transfers on each lane, and a burst that wraps around the
    # 256-word SRAM -- since both WRITE and READ apply the identical
    # address->word mapping, so the payload must round-trip in every case.
    requests = _link_established(
        _roce_write(vaddr, payload, psn=1),
        _roce_read_request(vaddr, len(payload), psn=2),
    )
    responses, _ = run_rdma(requests)

    assert len(responses) == 4
    roce = [Ether(frame) for frame in responses[2:]]
    assert [packet[BTH].opcode for packet in roce] == [
        BthOpcode.RC_ACKNOWLEDGE,
        BthOpcode.RC_RDMA_READ_RESPONSE_ONLY,
    ]
    assert _roce_payload(roce[1]) == payload


@pytest.mark.parametrize("qpn", [0, 2, 63])
def test_rdma_write_on_each_qpn_is_acked(qpn):
    # A connection set up on *qpn* lets a WRITE addressed to that dest_qp be
    # ACKed. qpn 0, 2 and 63 span the connection/state tables (max_qps == 64),
    # so any QP-indexing or cross-QP isolation bug surfaces. (Request list is
    # built by hand rather than via _link_established, which always seeds QP 2.)
    requests = [
        _arp_request(HOST_MAC, SOURCE_IP),
        _connection_setup(local_qpn=qpn, remote_qpn=qpn, initial_rx_psn=1),
        _roce_write(0x1230, bytes(range(8)), dest_qp=qpn, psn=1),
    ]
    responses, _ = run_rdma(requests)

    assert len(responses) == 3  # ARP reply + connection UDP ack + write ACK
    ack = Ether(responses[2])
    assert ack[BTH].opcode == BthOpcode.RC_ACKNOWLEDGE
    assert ack[BTH].dest_qp == qpn


def test_connection_setup_programs_nonzero_responder_psn():
    initial_psn = 0x123456
    accepted_addr = 0x1230
    rejected_addr = 0x1240
    accepted_word = (accepted_addr // 8) % Top.SRAM_DEPTH
    rejected_word = (rejected_addr // 8) % Top.SRAM_DEPTH
    payload = bytes.fromhex("0123456789abcdef")

    requests = _link_established(
        _roce_write(accepted_addr, payload, psn=initial_psn),
        _roce_write(rejected_addr, bytes(range(8)), psn=0),
        initial_rx_psn=initial_psn,
    )
    responses, sram = run_rdma(
        requests, sram_addresses=(accepted_word, rejected_word))

    # ARP + setup ACK + ACK for the matching request. The PSN-0 request is
    # rejected, proving the programmed value did not come from reset state.
    assert len(responses) == 3
    ack = Ether(responses[2])
    assert ack[BTH].opcode == BthOpcode.RC_ACKNOWLEDGE
    assert ack[BTH].psn == initial_psn
    assert sram == {
        accepted_word: int.from_bytes(payload, "little"),
        rejected_word: rejected_word,
    }


@pytest.mark.parametrize("invalid_field", [
    "local_qpn",
    "remote_qpn",
    "initial_rx_psn",
])
def test_connection_setup_rejects_values_wider_than_wire_protocol(
        invalid_field):
    setup_args = {invalid_field: 0x1000000}
    requests = [
        _arp_request(HOST_MAC, SOURCE_IP),
        _connection_setup(**setup_args),
    ]
    responses, _ = run_rdma(requests)

    # Invalid setup is drained without a connection-table write or setup ACK.
    assert len(responses) == 1
    assert ARP in Ether(responses[0])


def test_arp_table_resolves_multiple_peers():
    # Two peers with distinct top-byte IPs seed two ARP entries. Connections to
    # each peer then let two WRITE ACKs be emitted, each addressed to the
    # correct peer MAC -- exercising the ARP/connection tables with more than
    # one resident entry.
    peer_b_ip = "10.0.0.7"
    peer_b_mac = b"\xaa\xbb\xcc\xdd\xee\xff"
    requests = [
        _arp_request(HOST_MAC, SOURCE_IP),
        _arp_request(peer_b_mac, peer_b_ip),
        _connection_setup(local_qpn=2, remote_ip=SOURCE_IP, initial_rx_psn=1),
        _connection_setup(local_qpn=5, remote_ip=peer_b_ip, initial_rx_psn=1),
        _roce_write(0x1230, bytes(range(8)), dest_qp=2, psn=1),
        _roce_write(0x1240, bytes(range(8)), dest_qp=5, psn=1),
    ]
    responses, _ = run_rdma(requests)

    assert len(responses) == 6  # 2 ARP replies + 2 conn acks + 2 write ACKs
    ack_a = Ether(responses[4])
    ack_b = Ether(responses[5])
    assert ack_a[BTH].dest_qp == 2
    assert ack_b[BTH].dest_qp == 5
    assert ack_a.dst != ack_b.dst
    assert ack_a.dst == _mac(HOST_MAC)
    assert ack_b.dst == _mac(peer_b_mac)


def _roce_with_opcode(frame, opcode):
    """Return *frame* with a new opcode and a recomputed valid ICRC."""
    packet = Ether(frame)
    packet[BTH].opcode = opcode
    packet[RETH].payload.load = bytes(packet[RETH].payload
                                      )[:-roce_codec.ICRC_SIZE]
    return roce_codec.finalize_roce(packet)


def test_rdma_write_without_connection_is_dropped():
    # A WRITE arriving before any connection setup is rejected by the state
    # table (valid bit is clear), so there is no DMA side effect and no ACK.
    word = (0x1234 // 8) % Top.SRAM_DEPTH
    requests = [_roce_write(0x1234, bytes.fromhex("abcdefff"))]
    responses, sram = run_rdma(requests, sram_addresses=(word, ))

    assert responses == []
    assert sram == {word: word}


def test_unrelated_udp_port_is_filtered():
    # UDP traffic to a port the device does not listen on (neither the
    # connection-server port 8000 nor RoCE 4791) is dropped by the UDP stack,
    # so nothing is emitted.
    requests = [
        _build_udp_packet(DEVICE_MAC, DRIVER_MAC, SOURCE_IP, DEVICE_IP, 500,
                          9999, b"\x00" * 4),
    ]
    responses, _ = run_rdma(requests)

    assert responses == []


def test_roce_unsupported_opcode_is_dropped():
    # With link established a valid WRITE would be ACKed; patching its BTH
    # opcode to RC SEND_ONLY (0x04) -- neither a RETH nor AETH opcode the
    # depacketizer routes -- drops it before any DMA or ACK. Only the ARP reply
    # and connection-setup ack emerge, and SRAM is left untouched (each word
    # still reads back its init value, which equals its own index).
    word = (0x1230 // 8) % Top.SRAM_DEPTH
    requests = _link_established(
        _roce_with_opcode(_roce_write(0x1230, bytes(range(8))), 0x04),
    )
    responses, sram = run_rdma(requests, sram_addresses=(word, ))

    assert len(responses) == 2  # ARP reply + connection UDP ack only
    assert sram == {word: word}


def test_incoming_ack_does_not_advance_responder_psn():
    incoming_ack = _build_roce_packet(
        src_mac=DRIVER_MAC,
        opcode=BthOpcode.RC_ACKNOWLEDGE,
        psn=1,
        **_ROCE_COMMON,
    )
    requests = _link_established(
        incoming_ack,
        _roce_write(0x1230, bytes(range(8)), psn=1),
    )
    word = (0x1230 // 8) % Top.SRAM_DEPTH
    responses, sram = run_rdma(requests, sram_addresses=(word, ))

    # The endpoint has no requester state, so the incoming ACK is discarded.
    # The following responder request must still be accepted at PSN 1.
    assert len(responses) == 3, sram
    ack = Ether(responses[2])
    assert ack[BTH].opcode == BthOpcode.RC_ACKNOWLEDGE
    assert ack[BTH].psn == 1


def test_unsupported_write_fragment_does_not_advance_or_execute():
    rejected_addr = 0x1230
    accepted_addr = 0x1240
    rejected_word = (rejected_addr // 8) % Top.SRAM_DEPTH
    accepted_word = (accepted_addr // 8) % Top.SRAM_DEPTH
    rejected = _roce_with_opcode(
        _roce_write(rejected_addr, bytes.fromhex("deadbeefcafef00d"), psn=1),
        0x06,  # RC_RDMA_WRITE_FIRST; outside packet_codec's supported enum.
    )
    accepted_payload = bytes(range(8))
    requests = _link_established(
        rejected,
        _roce_write(accepted_addr, accepted_payload, psn=1),
    )
    responses, sram = run_rdma(
        requests, sram_addresses=(rejected_word, accepted_word))

    assert len(responses) == 3
    assert Ether(responses[2])[BTH].psn == 1
    assert sram == {
        rejected_word: rejected_word,
        accepted_word: int.from_bytes(accepted_payload, "little"),
    }


def _backpressure_scenario():
    return _link_established(
        _roce_write(0x1230, bytes(range(8)), psn=1),
        _roce_write(0x1234, bytes.fromhex("abcdefff"), psn=2),
        _roce_read_request(0x1230, 16, psn=3),
    )


@pytest.fixture(scope="module")
def backpressure_reference():
    reference, _ = run_rdma(_backpressure_scenario(), collect_timeout=1000)
    assert len(reference) == 5  # ARP + conn ack + 2 write acks + read response
    return reference


@pytest.mark.parametrize("stress", [
    # TX backpressure: the collector deasserts tx_data_out.ready on a repeating
    # schedule, from light (1-in-10) through heavy (1-in-2) plus a bursty duty.
    pytest.param(dict(tx_ready_fn=lambda c: c % 10 != 0), id="tx-ready-1-in-10"),
    pytest.param(dict(tx_ready_fn=lambda c: c % 3 != 0), id="tx-ready-1-in-3"),
    pytest.param(dict(tx_ready_fn=lambda c: c % 2 != 0), id="tx-ready-1-in-2"),
    pytest.param(dict(tx_ready_fn=lambda c: (c % 5) < 3), id="tx-ready-3on-2off"),
    # A prolonged mid-stream stall proves that a held beat remains stable and
    # that transmission resumes without loss when ready eventually returns.
    pytest.param(dict(tx_ready_fn=lambda c: not 100 <= c < 600),
                 id="tx-ready-long-stall"),
    # RX cadence: frames driven with fewer idle cycles between them, from
    # back-to-back (gap 0) up to a moderately relaxed spacing.
    pytest.param(dict(rx_gap=0), id="rx-gap-0"),
    pytest.param(dict(rx_gap=2), id="rx-gap-2"),
    pytest.param(dict(rx_gap=10), id="rx-gap-10"),
    # Deassert valid within every frame so RX cannot rely on contiguous beats.
    pytest.param(dict(rx_bubble_cycles_fn=lambda _packet, beat: 2
                      if beat and beat % 3 == 0 else 0),
                 id="rx-intra-frame-valid-bubbles"),
])
def test_rdma_backpressure_is_lossless(stress, backpressure_reference):
    # A well-behaved AXI-stream DUT must emit byte-identical frames -- just
    # slower -- whether the collector throttles TX ready on various duty cycles
    # or the driver tightens the inter-frame gap. The scenario produces five
    # frames (ARP reply, connection ack, two write acks, one read response);
    # comparing the whole frame list against an unstressed reference run catches
    # any reordering, dropped beat, spurious 'last', or payload corruption
    # under pressure.
    stressed, _ = run_rdma(_backpressure_scenario(),
                           collect_timeout=1000,
                           **stress)

    assert stressed == backpressure_reference


def _initial_sram_model():
    """Return the DUT SRAM's byte-addressed, little-endian initial image."""
    return bytearray().join(word.to_bytes(8, "little")
                            for word in range(Top.SRAM_DEPTH))


def _model_read(memory, vaddr, size):
    return bytes(memory[(vaddr + offset) % len(memory)]
                 for offset in range(size))


def _model_write(memory, vaddr, payload):
    for offset, byte in enumerate(payload):
        memory[(vaddr + offset) % len(memory)] = byte


def _random_trace(rng, count):
    """Build interleaved, overlapping operations and model each in order."""
    kinds = ["write"] * (count // 2) + ["read"] * (count - count // 2)
    while True:
        rng.shuffle(kinds)
        # Require a read followed later by a write; this rules out the old
        # all-writes-then-all-reads ordering for every seed.
        if "read" in kinds[:-1] and "write" in kinds[kinds.index("read") + 1:]:
            break

    memory = _initial_sram_model()
    operations = []
    expected = []
    hot_vaddr = 0x1230
    psn = 1
    for index, kind in enumerate(kinds):
        size = rng.choice((4, 8, 16, 24, 32))
        if index % 3 == 0:
            vaddr = hot_vaddr + (rng.choice((0, 4)) if size == 4 else 0)
        else:
            # A deliberately compact address window makes operations overlap;
            # larger transfers cover multiple candidate starting words.
            vaddr = hot_vaddr + rng.choice((-16, -8, 0, 8, 16))
            if size == 4:
                vaddr += rng.choice((0, 4))

        if kind == "write":
            payload = bytes(rng.randrange(256) for _ in range(size))
            operations.append(_roce_write(vaddr, payload, psn=psn))
            _model_write(memory, vaddr, payload)
            expected.append((BthOpcode.RC_ACKNOWLEDGE, b"", psn))
        else:
            operations.append(_roce_read_request(vaddr, size, psn=psn))
            expected.append((BthOpcode.RC_RDMA_READ_RESPONSE_ONLY,
                             _model_read(memory, vaddr, size), psn))
        psn += 1

    return operations, expected


@pytest.mark.parametrize("seed", [1, 2, 3, 4, 5])
def test_rdma_randomized_trace_roundtrips_and_survives_backpressure(seed):
    # Each seeded trace interleaves reads and writes over overlapping regions.
    # The software model is updated after every write and sampled at every read,
    # so responses prove operation ordering as well as byte-level semantics.
    # Replaying the trace under pressure independently checks losslessness.
    rng = random.Random(seed)
    operations, expected = _random_trace(rng, count=12)
    requests = _link_established(*operations)

    # Leave enough time between requests for each modeled operation to reach
    # SRAM before the next one is presented. This makes overlap deterministic
    # without conflating operation ordering with the DUT's queue depth.
    operation_gap = 50
    reference, _ = run_rdma(requests,
                            collect_timeout=1200,
                            rx_gap=operation_gap)

    assert len(reference) == 2 + len(operations)  # ARP + conn ack + each op
    operation_responses = [Ether(frame) for frame in reference[2:]]
    assert [(packet[BTH].opcode, _roce_payload(packet), packet[BTH].psn)
            for packet in operation_responses] == expected

    ready = lambda cycle, s=seed: ((cycle * 1103515245 + 12345 + s) >> 8) % 10 >= 3
    stressed, _ = run_rdma(requests,
                           collect_timeout=1200,
                           tx_ready_fn=ready,
                           rx_gap=operation_gap)
    assert stressed == reference


def test_replay_of_old_psn_is_rejected():
    # Two in-order writes are accepted; replaying the first PSN is rejected
    # without a second DMA side effect or ACK.
    word_a = (0x1230 // 8) % Top.SRAM_DEPTH
    word_b = (0x1260 // 8) % Top.SRAM_DEPTH
    payload_a = bytes(range(8))
    payload_b = bytes(range(8, 16))
    replay_payload = bytes.fromhex("deadbeefcafef00d")
    requests = _link_established(
        _roce_write(0x1230, payload_a, psn=1),
        _roce_write(0x1260, payload_b, psn=2),
        _roce_write(0x1230, replay_payload, psn=1),
    )
    responses, sram = run_rdma(requests, sram_addresses=(word_a, word_b))

    assert len(responses) == 4  # ARP + conn ack + 2 write ACKs (replay dropped)
    acks = [Ether(f) for f in responses[2:]]
    assert [a[BTH].psn for a in acks] == [1, 2]
    assert sram == {
        word_a: int.from_bytes(payload_a, "little"),
        word_b: int.from_bytes(payload_b, "little"),
    }


def test_future_psn_does_not_advance_state():
    # A future PSN is rejected without advancing resp_epsn; the expected PSN
    # that follows is still accepted.
    future_addr = 0x1230
    expected_addr = 0x1240
    future_word = (future_addr // 8) % Top.SRAM_DEPTH
    expected_word = (expected_addr // 8) % Top.SRAM_DEPTH
    future_payload = bytes.fromhex("deadbeefcafef00d")
    expected_payload = bytes(range(8))
    requests = _link_established(
        _roce_write(future_addr, future_payload, psn=100),
        _roce_write(expected_addr, expected_payload, psn=1),
    )
    responses, sram = run_rdma(
        requests, sram_addresses=(future_word, expected_word))

    assert len(responses) == 3  # ARP + conn ack + 1 ACK (future dropped)
    ack = Ether(responses[2])
    assert ack[BTH].psn == 1
    assert sram == {
        future_word: future_word,
        expected_word: int.from_bytes(expected_payload, "little"),
    }


def test_psn_wrap_across_boundary():
    # Three writes whose PSNs cross the 24-bit wrap are all accepted.
    addrs = [0x1230, 0x1238, 0x1240]
    words = [(a // 8) % Top.SRAM_DEPTH for a in addrs]
    payloads = [bytes(range(8)), bytes(range(8, 16)), bytes(range(16, 24))]
    requests = _link_established(
        _roce_write(addrs[0], payloads[0], psn=0xfffffe),
        _roce_write(addrs[1], payloads[1], psn=0xffffff),
        _roce_write(addrs[2], payloads[2], psn=0),
        initial_rx_psn=0xfffffe,
    )
    responses, sram = run_rdma(requests, sram_addresses=tuple(words))

    assert len(responses) == 5  # ARP + conn ack + 3 write ACKs
    acks = [Ether(f) for f in responses[2:]]
    assert [a[BTH].psn for a in acks] == [0xfffffe, 0xffffff, 0]
    assert sram == {
        words[i]: int.from_bytes(payloads[i], "little") for i in range(3)
    }


def test_two_qps_with_different_starting_psns_are_isolated():
    # Two QPs configured with different starting PSNs each accept their own
    # first request; neither QP's state interferes with the other.
    addrs = (0x1230, 0x1240, 0x1250, 0x1260, 0x1270)
    words = tuple((addr // 8) % Top.SRAM_DEPTH for addr in addrs)
    requests = [
        _arp_request(HOST_MAC, SOURCE_IP),
        _connection_setup(local_qpn=2, remote_qpn=2, initial_rx_psn=0x100),
        _connection_setup(local_qpn=5, remote_qpn=5, initial_rx_psn=0x200),
        _roce_write(addrs[0], bytes(range(8)), dest_qp=2, psn=0x100),
        _roce_write(addrs[1], bytes(range(8, 16)), dest_qp=5, psn=0x200),
        # A future QP-2 PSN must not affect QP 5 or advance QP 2.
        _roce_write(addrs[2], bytes(range(16, 24)), dest_qp=2, psn=0x102),
        _roce_write(addrs[3], bytes(range(24, 32)), dest_qp=5, psn=0x201),
        _roce_write(addrs[4], bytes(range(32, 40)), dest_qp=2, psn=0x101),
    ]
    responses, sram = run_rdma(requests, sram_addresses=words)

    assert len(responses) == 7  # ARP + 2 setup ACKs + 4 accepted writes
    acks = [Ether(f) for f in responses[3:]]
    assert [(ack[BTH].dest_qp, ack[BTH].psn) for ack in acks] == [
        (2, 0x100),
        (5, 0x200),
        (5, 0x201),
        (2, 0x101),
    ]
    assert sram == {
        words[0]: int.from_bytes(bytes(range(8)), "little"),
        words[1]: int.from_bytes(bytes(range(8, 16)), "little"),
        words[2]: words[2],
        words[3]: int.from_bytes(bytes(range(24, 32)), "little"),
        words[4]: int.from_bytes(bytes(range(32, 40)), "little"),
    }


@pytest.mark.parametrize("qpn", [64, 65, 0xffffff])
def test_out_of_range_qpn_is_dropped(qpn):
    # A write to a QPN >= max_qps is rejected by the state table range
    # check without DMA or ACK.
    word = (0x1230 // 8) % Top.SRAM_DEPTH
    requests = _link_established(
        _roce_write(0x1230, bytes(range(8)), dest_qp=qpn, psn=1),
    )
    responses, sram = run_rdma(requests, sram_addresses=(word, ))

    assert len(responses) == 2  # ARP + conn ack only
    assert sram == {word: word}
