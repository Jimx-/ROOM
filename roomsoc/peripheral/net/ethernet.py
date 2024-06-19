from enum import IntEnum
from amaranth import *
from amaranth.hdl.rec import Direction

from roomsoc.interconnect.axi import AXIStreamInterface, AXIStreamDepacketizer, AXIStreamPacketizer
from roomsoc.interconnect.stream import Decoupled, Valid

from .arp import ArpTableResponse

__all__ = ("EthernetRouter", "MacIpEncoder")

ETH_HEADER_LAYOUT = [
    ("dst_addr", 48, Direction.FANOUT),
    ("src_addr", 48, Direction.FANOUT),
    ("type", 16, Direction.FANOUT),
]


class EtherType(IntEnum):
    IPv4 = 0x0800
    ARP = 0x0806
    IPv6 = 0x86dd


class EthernetRouter(Elaboratable):

    def __init__(self, data_width):
        self.data_width = data_width

        self.data_in = AXIStreamInterface(data_width=data_width)

        self.arp_data_out = AXIStreamInterface(data_width=data_width)
        self.ipv4_data_out = AXIStreamInterface(data_width=data_width)
        self.ipv6_data_out = AXIStreamInterface(data_width=data_width)

    def elaborate(self, platform):
        m = Module()

        depacketizer = m.submodules.depacketizer = AXIStreamDepacketizer(
            Record, ETH_HEADER_LAYOUT, data_width=self.data_width)
        m.d.comb += self.data_in.connect(depacketizer.sink)

        ether_type = Cat(depacketizer.header.type[8:],
                         depacketizer.header.type[:8])
        with m.Switch(ether_type):
            with m.Case(EtherType.IPv4):
                m.d.comb += depacketizer.source.connect(self.ipv4_data_out)
            with m.Case(EtherType.ARP):
                m.d.comb += depacketizer.source.connect(self.arp_data_out)
            with m.Case(EtherType.IPv6):
                m.d.comb += depacketizer.source.connect(self.ipv6_data_out)
            with m.Default():
                m.d.comb += depacketizer.source.ready.eq(1)

        return m


class MacIpEncoder(Elaboratable):

    def __init__(self, data_width):
        self.data_width = data_width

        self.my_mac_addr = Signal(48)

        self.arp_table_req = Decoupled(Signal, 32)
        self.arp_table_resp = Valid(ArpTableResponse)

        self.data_in = AXIStreamInterface(data_width=data_width,
                                          user_width=128)
        self.data_out = AXIStreamInterface(data_width=data_width)

    def elaborate(self, platform):
        m = Module()

        packetizer = m.submodules.packetizer = AXIStreamPacketizer(
            Record, ETH_HEADER_LAYOUT, data_width=self.data_width)
        m.d.comb += [
            packetizer.header.src_addr.eq(self.my_mac_addr),
            packetizer.header.type.eq(0x0008),
            packetizer.source.connect(self.data_out),
        ]

        with m.FSM():
            with m.State("IDLE"):
                with m.If(self.data_in.valid):
                    m.d.comb += [
                        self.arp_table_req.bits.eq(
                            self.data_in.bits.user[-32:]),
                        self.arp_table_req.valid.eq(1),
                    ]

                    with m.If(self.arp_table_req.fire):
                        m.next = "ARP_REPLY"

            with m.State("ARP_REPLY"):
                with m.If(self.arp_table_resp.valid):
                    with m.If(self.arp_table_resp.bits.hit):
                        m.d.sync += packetizer.header.dst_addr.eq(
                            self.arp_table_resp.bits.mac_addr)
                        m.next = "FORWARD"
                    with m.Else():
                        m.next = "DROP"

            with m.State("FORWARD"):
                m.d.comb += self.data_in.connect(packetizer.sink)

                with m.If(self.data_out.fire & self.data_out.bits.last):
                    m.next = "IDLE"

            with m.State("DROP"):
                m.d.comb += self.data_in.ready.eq(1)

                with m.If(self.data_in.fire & self.data_in.bits.last):
                    m.next = "IDLE"

        return m


class EthernetFramePadding(Elaboratable):

    def __init__(self, data_width):
        self.data_width = data_width

        self.data_in = AXIStreamInterface(data_width=data_width)
        self.data_out = AXIStreamInterface(data_width=data_width)

    def elaborate(self, platform):
        m = Module()

        min_length = 60
        min_beats = min_length * 8 // self.data_width
        leftover = min_length % (self.data_width // 8)
        count = Signal(range(min_beats + 1))
        done = Signal(reset=1)

        with m.FSM():
            with m.State("IDLE"):
                with m.If(self.data_in.valid):
                    m.d.sync += [
                        done.eq(0),
                        count.eq(0),
                    ]

                    m.next = "FILL"

            with m.State("FILL"):
                last_padding = count == min_beats - (1 if leftover == 0 else 0)

                m.d.comb += [
                    self.data_in.connect(self.data_out),
                    self.data_out.bits.last.eq(last_padding
                                               & (done
                                                  | self.data_in.bits.last)),
                    self.data_out.valid.eq(self.data_in.valid | done),
                ]

                with m.If(self.data_in.fire & self.data_in.bits.last):
                    m.d.sync += done.eq(1)

                with m.If(self.data_out.fire):
                    with m.If(count < min_beats):
                        m.d.sync += count.eq(count + 1)

                        for i in range(self.data_width // 8):
                            with m.If(~self.data_in.bits.keep[i]):
                                m.d.comb += [
                                    self.data_out.bits.data[i * 8:(i + 1) *
                                                            8].eq(0),
                                    self.data_out.bits.keep[i].eq(1),
                                ]

                    if leftover != 0:
                        with m.If(count == min_beats):
                            for i in range(leftover):
                                with m.If(~self.data_in.bits.keep[i]):
                                    m.d.comb += [
                                        self.data_out.bits.data[i * 8:(i + 1) *
                                                                8].eq(0),
                                        self.data_out.bits.keep[i].eq(1),
                                    ]

                    with m.If(last_padding):
                        with m.If(done | self.data_in.bits.last):
                            m.next = "IDLE"
                        with m.Else():
                            m.next = "FORWARD"

            with m.State("FORWARD"):
                m.d.comb += self.data_in.connect(self.data_out)

                with m.If(self.data_out.fire & self.data_out.bits.last):
                    m.next = "IDLE"

        return m
