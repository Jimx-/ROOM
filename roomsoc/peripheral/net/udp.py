from amaranth import *
from amaranth.hdl.rec import Direction

from roomsoc.interconnect.axi import AXIStreamInterface, AXIStreamDepacketizer
from roomsoc.interconnect.stream import Decoupled, Queue

from .ipv4 import Ipv4, Ipv4Metadata

__all__ = ("UdpStack", "UdpMetadata", "UdpIpMetadata")

UDP_HEADER_LAYOUT = [
    ("src_port", 16, Direction.FANOUT),
    ("dst_port", 16, Direction.FANOUT),
    ("length", 16, Direction.FANOUT),
    ("checksum", 16, Direction.FANOUT),
]


class UdpMetadata(Record):

    def __init__(self, name=None, src_loc_at=0):
        super().__init__([
            ('peer_port', 16, Direction.FANOUT),
            ('my_port', 16, Direction.FANOUT),
            ('length', 16, Direction.FANOUT),
        ],
                         name=name,
                         src_loc_at=1 + src_loc_at)


class UdpIpMetadata(Record):

    def __init__(self, name=None, src_loc_at=0):
        super().__init__([
            ('peer_addr', 128, Direction.FANOUT),
            ('peer_port', 16, Direction.FANOUT),
            ('my_port', 16, Direction.FANOUT),
            ('length', 16, Direction.FANOUT),
        ],
                         name=name,
                         src_loc_at=1 + src_loc_at)


class UdpDepacketizer(Elaboratable):

    def __init__(self, data_width):
        self.data_width = data_width

        self.data_in = AXIStreamInterface(data_width=data_width)
        self.data_out = AXIStreamInterface(data_width=data_width)
        self.meta_out = Decoupled(UdpMetadata)

    def elaborate(self, platform):
        m = Module()

        depacketizer = m.submodules.depacketizer = AXIStreamDepacketizer(
            Record, UDP_HEADER_LAYOUT, data_width=self.data_width)
        m.d.comb += self.data_in.connect(depacketizer.sink)

        with m.FSM():
            with m.State("IDLE"):
                with m.If(depacketizer.source.valid):
                    m.d.comb += [
                        self.meta_out.bits.peer_port.eq(
                            Cat(depacketizer.header.src_port[8:],
                                depacketizer.header.src_port[:8])),
                        self.meta_out.bits.my_port.eq(
                            Cat(depacketizer.header.dst_port[8:],
                                depacketizer.header.dst_port[:8])),
                        self.meta_out.bits.length.eq(
                            Cat(depacketizer.header.length[8:],
                                depacketizer.header.length[:8])),
                        self.meta_out.valid.eq(1),
                    ]

                    with m.If(self.meta_out.ready):
                        m.next = "FORWARD"

            with m.State("FORWARD"):
                m.d.comb += depacketizer.source.connect(self.data_out)

                with m.If(self.data_out.fire & self.data_out.bits.last):
                    m.next = "IDLE"

        return m


class UdpMetaMerger(Elaboratable):

    def __init__(self):
        self.ip_meta = Decoupled(Ipv4Metadata)
        self.udp_meta = Decoupled(UdpMetadata)
        self.udp_ip_meta = Decoupled(UdpIpMetadata)

    def elaborate(self, platform):
        m = Module()

        with m.If(self.ip_meta.valid & self.udp_meta.valid):
            m.d.comb += [
                self.udp_ip_meta.bits.peer_addr.eq(
                    self.ip_meta.bits.peer_addr),
                self.udp_ip_meta.bits.peer_port.eq(
                    self.udp_meta.bits.peer_port),
                self.udp_ip_meta.bits.my_port.eq(self.udp_meta.bits.my_port),
                self.udp_ip_meta.bits.length.eq(self.udp_meta.bits.length),
                self.udp_ip_meta.valid.eq(1),
            ]

            with m.If(self.udp_ip_meta.ready):
                m.d.comb += [
                    self.ip_meta.ready.eq(1),
                    self.udp_meta.ready.eq(1),
                ]

        return m


class UdpStack(Elaboratable):

    def __init__(self, data_width):
        self.data_width = data_width

        self.rx_data_in = AXIStreamInterface(data_width=data_width)
        self.rx_data_out = AXIStreamInterface(data_width=data_width)
        self.rx_meta_out = Decoupled(UdpIpMetadata)

        self.tx_data_in = AXIStreamInterface(data_width=data_width)
        self.tx_data_out = AXIStreamInterface(data_width=data_width)

    def elaborate(self, platform):
        m = Module()

        ipv4 = m.submodules.ipv4 = Ipv4(data_width=self.data_width)
        m.d.comb += self.rx_data_in.connect(ipv4.rx_data_in)

        depacketizer = m.submodules.depacketizer = UdpDepacketizer(
            data_width=self.data_width)
        m.d.comb += [
            ipv4.rx_data_out.connect(depacketizer.data_in),
            depacketizer.data_out.connect(self.rx_data_out),
        ]

        rx_meta_merger = m.submodules.rx_meta_merger = UdpMetaMerger()
        m.d.comb += [
            ipv4.rx_meta_out.connect(rx_meta_merger.ip_meta),
            depacketizer.meta_out.connect(rx_meta_merger.udp_meta),
        ]

        rx_meta_queue = m.submodules.rx_meta_queue = Queue(2, UdpIpMetadata)
        m.d.comb += [
            rx_meta_merger.udp_ip_meta.connect(rx_meta_queue.enq),
            rx_meta_queue.deq.connect(self.rx_meta_out),
        ]

        return m
