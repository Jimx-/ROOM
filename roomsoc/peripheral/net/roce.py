from amaranth import *
from amaranth.hdl.rec import Direction

from roomsoc.interconnect.axi import AXIStreamInterface, AXIStreamDepacketizer, AXIStreamPacketizer
from roomsoc.interconnect.stream import Decoupled, Queue

from .udp import UdpStack
from .crc import Crc
from .infiniband import InfiniBandTransportProtocol


class Rocev2Stack(Elaboratable):

    def __init__(self, data_width, port=4791):
        self.data_width = data_width
        self.port = port

        self.my_ip_addr = Signal(32)

        self.rx_data_in = AXIStreamInterface(data_width=data_width)

        self.tx_data_out = AXIStreamInterface(data_width=data_width)

    def elaborate(self, platform):
        m = Module()

        icrc = m.submodules.icrc = Crc(data_width=self.data_width)
        m.d.comb += [
            self.rx_data_in.connect(icrc.rx_data_in),
        ]

        udp_stack = m.submodules.udp_stack = UdpStack(
            data_width=self.data_width, port=self.port)
        m.d.comb += [
            udp_stack.my_ip_addr.eq(self.my_ip_addr),
            icrc.rx_data_out.connect(udp_stack.rx_data_in),
            udp_stack.tx_data_out.connect(self.tx_data_out),
        ]

        ib_stack = m.submodules.ib_stack = InfiniBandTransportProtocol(
            data_width=self.data_width)
        m.d.comb += udp_stack.rx_data_out.connect(ib_stack.rx_data_in)

        return m
