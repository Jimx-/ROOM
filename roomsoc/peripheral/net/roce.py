from amaranth import *
from amaranth.hdl.rec import Direction

from roomsoc.interconnect.axi import AXIStreamInterface, AXIStreamDepacketizer, AXIStreamPacketizer
from roomsoc.interconnect.stream import Decoupled, Queue

from .udp import UdpStack
from .crc import Crc
from .infiniband import InfiniBandTransportProtocol


class Rocev2Stack(Elaboratable):

    MemoryCommand = InfiniBandTransportProtocol.MemoryCommand

    def __init__(self, data_width, port=4791):
        self.data_width = data_width
        self.port = port

        self.my_ip_addr = Signal(32)

        self.mem_read_cmd = Decoupled(Rocev2Stack.MemoryCommand)
        self.mem_read_data = AXIStreamInterface(data_width=data_width)

        self.mem_write_cmd = Decoupled(Rocev2Stack.MemoryCommand)
        self.mem_write_data = AXIStreamInterface(data_width=data_width)

        self.rx_data_in = AXIStreamInterface(data_width=data_width)
        self.tx_data_out = AXIStreamInterface(data_width=data_width,
                                              user_width=128)

    def elaborate(self, platform):
        m = Module()

        icrc = m.submodules.icrc = Crc(data_width=self.data_width)
        m.d.comb += [
            self.rx_data_in.connect(icrc.rx_data_in),
            icrc.tx_data_out.connect(self.tx_data_out),
        ]

        udp_stack = m.submodules.udp_stack = UdpStack(
            data_width=self.data_width, port=self.port)
        m.d.comb += [
            udp_stack.my_ip_addr.eq(self.my_ip_addr),
            icrc.rx_data_out.connect(udp_stack.rx_data_in),
            udp_stack.tx_data_out.connect(icrc.tx_data_in),
        ]

        ib_stack = m.submodules.ib_stack = InfiniBandTransportProtocol(
            data_width=self.data_width, port=self.port)
        m.d.comb += [
            udp_stack.rx_data_out.connect(ib_stack.rx_data_in),
            ib_stack.mem_read_cmd.connect(self.mem_read_cmd),
            self.mem_read_data.connect(ib_stack.mem_read_data),
            ib_stack.mem_write_cmd.connect(self.mem_write_cmd),
            ib_stack.mem_write_data.connect(self.mem_write_data),
            ib_stack.tx_data_out.connect(udp_stack.tx_data_in),
            ib_stack.tx_meta_out.connect(udp_stack.tx_meta_in),
        ]

        return m
