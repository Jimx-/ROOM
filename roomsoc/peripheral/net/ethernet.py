from enum import IntEnum
from amaranth import *
from amaranth.hdl.rec import Direction

from roomsoc.interconnect.axi import AXIStreamInterface, AXIStreamDepacketizer

__all__ = ("EthernetRouter", )

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

        return m
