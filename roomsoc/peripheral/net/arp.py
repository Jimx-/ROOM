from enum import IntEnum
from amaranth import *
from amaranth.hdl.rec import Direction

from roomsoc.interconnect.axi import AXIStreamInterface, AXIStreamDepacketizer, AXIStreamPacketizer
from roomsoc.interconnect.stream import Decoupled, Queue, Valid

ARP_HEADER_LAYOUT = [
    ("htype", 16, Direction.FANOUT),
    ("ptype", 16, Direction.FANOUT),
    ("hlen", 8, Direction.FANOUT),
    ("plen", 8, Direction.FANOUT),
    ("oper", 16, Direction.FANOUT),
    ("sha", 48, Direction.FANOUT),
    ("spa", 32, Direction.FANOUT),
    ("tha", 48, Direction.FANOUT),
    ("tpa", 32, Direction.FANOUT),
]

ETH_ARP_HEADER_LAYOUT = [
    ("dst_addr", 48, Direction.FANOUT),
    ("src_addr", 48, Direction.FANOUT),
    ("type", 16, Direction.FANOUT),
] + ARP_HEADER_LAYOUT


class ArpOperation(IntEnum):
    REQUEST = 0x100
    REPLY = 0x200


class ArpReplyMetadata(Record):

    def __init__(self, name=None, src_loc_at=0):
        super().__init__([
            ('src_mac_addr', 48, Direction.FANOUT),
            ('sha', 48, Direction.FANOUT),
            ('spa', 32, Direction.FANOUT),
        ],
                         name=name,
                         src_loc_at=1 + src_loc_at)


class ArpTableEntry(Record):

    def __init__(self, name=None, src_loc_at=0):
        super().__init__([
            ('ip_addr', 32, Direction.FANOUT),
            ('mac_addr', 48, Direction.FANOUT),
            ('valid', 1, Direction.FANOUT),
        ],
                         name=name,
                         src_loc_at=1 + src_loc_at)


class ArpTableResponse(Record):

    def __init__(self, name=None, src_loc_at=0):
        super().__init__([
            ('mac_addr', 48, Direction.FANOUT),
            ('hit', 1, Direction.FANOUT),
        ],
                         name=name,
                         src_loc_at=1 + src_loc_at)


class ArpDepacketizer(Elaboratable):

    def __init__(self, data_width):
        self.data_width = data_width

        self.my_ip_addr = Signal(32)

        self.data_in = AXIStreamInterface(data_width=data_width)

        self.arp_reply_out = Decoupled(ArpReplyMetadata)
        self.arp_table_write = Decoupled(ArpTableEntry)

    def elaborate(self, platform):
        m = Module()

        depacketizer = m.submodules.depacketizer = AXIStreamDepacketizer(
            Record, ARP_HEADER_LAYOUT, data_width=self.data_width)
        m.d.comb += self.data_in.connect(depacketizer.sink)

        with m.FSM():
            with m.State("IDLE"):
                with m.If(depacketizer.source.valid):
                    with m.Switch(depacketizer.header.oper):
                        with m.Case(ArpOperation.REQUEST):
                            m.d.comb += [
                                self.arp_reply_out.bits.src_mac_addr.eq(
                                    depacketizer.header.sha),
                                self.arp_reply_out.bits.sha.eq(
                                    depacketizer.header.sha),
                                self.arp_reply_out.bits.spa.eq(
                                    depacketizer.header.spa),
                                self.arp_reply_out.valid.eq(
                                    self.arp_table_write.ready),
                                self.arp_table_write.bits.ip_addr.eq(
                                    depacketizer.header.spa),
                                self.arp_table_write.bits.mac_addr.eq(
                                    depacketizer.header.sha),
                                self.arp_table_write.bits.valid.eq(1),
                                self.arp_table_write.valid.eq(1),
                            ]

                            with m.If(self.arp_reply_out.fire
                                      & self.arp_table_write.fire):
                                m.next = "DRAIN"

            with m.State("DRAIN"):
                m.d.comb += depacketizer.source.ready.eq(1)

                with m.If(depacketizer.source.fire
                          & depacketizer.source.bits.last):
                    m.next = "IDLE"

        m.d.comb += depacketizer.source.ready.eq(1)

        return m


class ArpPacketizer(Elaboratable):

    def __init__(self, data_width):
        self.data_width = data_width

        self.my_mac_addr = Signal(48)
        self.my_ip_addr = Signal(32)

        self.arp_reply_in = Decoupled(ArpReplyMetadata)

        self.data_out = AXIStreamInterface(data_width=data_width)

    def elaborate(self, platform):
        m = Module()

        reply_queue = m.submodules.reply_queue = Queue(2, ArpReplyMetadata)
        m.d.comb += self.arp_reply_in.connect(reply_queue.enq)

        header = Record(ETH_ARP_HEADER_LAYOUT)
        packetizer = m.submodules.packetizer = AXIStreamPacketizer(
            Record, ETH_ARP_HEADER_LAYOUT, data_width=self.data_width)
        m.d.comb += [
            packetizer.header.eq(header),
            packetizer.source.connect(self.data_out),
        ]

        with m.FSM():
            with m.State("IDLE"):

                with m.If(reply_queue.deq.valid):
                    m.d.comb += reply_queue.deq.ready.eq(1)

                    m.d.sync += [
                        header.dst_addr.eq(reply_queue.deq.bits.src_mac_addr),
                        header.src_addr.eq(self.my_mac_addr),
                        header.type.eq(0x0608),
                        header.htype.eq(0x0100),
                        header.ptype.eq(0x0008),
                        header.hlen.eq(6),
                        header.plen.eq(4),
                        header.oper.eq(ArpOperation.REPLY),
                        header.sha.eq(self.my_mac_addr),
                        header.spa.eq(self.my_ip_addr),
                        header.tha.eq(reply_queue.deq.bits.sha),
                        header.tpa.eq(reply_queue.deq.bits.spa),
                    ]

                    m.next = "PACKET"

            with m.State("PACKET"):
                m.d.comb += [
                    packetizer.sink.bits.last.eq(1),
                    packetizer.sink.valid.eq(1),
                ]

                with m.If(packetizer.sink.fire):
                    m.next = "IDLE"

        return m


class ArpTableSubnet(Elaboratable):

    def __init__(self):
        self.req = Decoupled(Signal, 32)
        self.resp = Valid(ArpTableResponse)

        self.write = Decoupled(ArpTableEntry)

    def elaborate(self, platform):
        m = Module()

        mem = Memory(width=len(self.write.bits), depth=256)

        mem_rport = m.submodules.mem_rport = mem.read_port(transparent=False)
        mem_rdata = ArpTableEntry()
        m.d.comb += [
            mem_rport.addr.eq(self.req.bits[24:32]),
            self.req.ready.eq(1),
            mem_rdata.eq(mem_rport.data),
            self.resp.bits.hit.eq(mem_rdata.valid),
            self.resp.bits.mac_addr.eq(mem_rdata.mac_addr),
        ]
        m.d.sync += self.resp.valid.eq(self.req.valid)

        mem_wport = m.submodules.mem_wport = mem.write_port()
        m.d.comb += [
            mem_wport.addr.eq(self.write.bits.ip_addr[24:32]),
            mem_wport.data.eq(self.write.bits),
            mem_wport.en.eq(self.write.valid),
            self.write.ready.eq(1),
        ]

        return m


class ArpServerSubnet(Elaboratable):

    def __init__(self, data_width):
        self.data_width = data_width

        self.my_mac_addr = Signal(48)
        self.my_ip_addr = Signal(32)

        self.arp_table_req = Decoupled(Signal, 32)
        self.arp_table_resp = Valid(ArpTableResponse)

        self.rx_data_in = AXIStreamInterface(data_width=data_width)

        self.tx_data_out = AXIStreamInterface(data_width=data_width)

    def elaborate(self, platform):
        m = Module()

        depacketizer = m.submodules.depacketizer = ArpDepacketizer(
            data_width=self.data_width)
        m.d.comb += [
            depacketizer.my_ip_addr.eq(self.my_ip_addr),
            self.rx_data_in.connect(depacketizer.data_in),
        ]

        packetizer = m.submodules.packetizer = ArpPacketizer(
            data_width=self.data_width)
        m.d.comb += [
            packetizer.my_mac_addr.eq(self.my_mac_addr),
            packetizer.my_ip_addr.eq(self.my_ip_addr),
            depacketizer.arp_reply_out.connect(packetizer.arp_reply_in),
            packetizer.data_out.connect(self.tx_data_out),
        ]

        arp_table = m.submodules.arp_table = ArpTableSubnet()
        m.d.comb += [
            self.arp_table_req.connect(arp_table.req),
            self.arp_table_resp.eq(arp_table.resp),
            depacketizer.arp_table_write.connect(arp_table.write),
        ]

        return m
