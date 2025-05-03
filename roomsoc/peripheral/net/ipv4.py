from amaranth import *
from amaranth.hdl.rec import Direction

from roomsoc.interconnect.axi import AXIStreamInterface, AXIStreamDepacketizer, AXIStreamPacketizer
from roomsoc.interconnect.stream import Decoupled, Queue, SkidBuffer

from .common import *
from .ip import IpProtocol

__all__ = ("Ipv4Metadata", "Ipv4Handler", "Ipv4")

IPv4_HEADER_LAYOUT = [
    ("ihl", 4, Direction.FANOUT),
    ("version", 4, Direction.FANOUT),
    ("tos", 8, Direction.FANOUT),
    ("total_len", 16, Direction.FANOUT),
    ("identification", 16, Direction.FANOUT),
    ("frag_offset_h", 5, Direction.FANOUT),
    ("flags", 3, Direction.FANOUT),
    ("frag_offset_l", 8, Direction.FANOUT),
    ("ttl", 8, Direction.FANOUT),
    ("protocol", 8, Direction.FANOUT),
    ("checksum", 16, Direction.FANOUT),
    ("src_addr", 32, Direction.FANOUT),
    ("dst_addr", 32, Direction.FANOUT),
]


class Ipv4Metadata(Record):

    def __init__(self, name=None, src_loc_at=0):
        super().__init__([
            ('peer_addr', 32, Direction.FANOUT),
            ('length', 16, Direction.FANOUT),
        ],
                         name=name,
                         src_loc_at=1 + src_loc_at)


class Ipv4HeaderExtract(Elaboratable):

    def __init__(self, data_width, handler):
        self.data_width = data_width
        self.handler = handler

        self.data_in = AXIStreamInterface(data_width=data_width)
        self.data_out = AXIStreamInterface(data_width=data_width)

    def elaborate(self, platform):
        m = Module()

        m.d.comb += self.data_in.connect(self.data_out)

        header = Decoupled(Record, IPv4_HEADER_LAYOUT)

        beat_bytes = self.data_width // 8
        header_beats = len(header.bits) // self.data_width
        header_leftover = (len(header.bits) // 8) % beat_bytes
        aligned = header_leftover == 0

        sr = Signal(len(header.bits), reset_less=True)
        sr_next = Signal(len(header.bits))
        sr_shift = Signal()
        sr_shift_leftover = Signal()
        count = Signal(range(max(header_beats + 1, 2)))
        done = Signal()

        m.d.comb += [
            sr_next.eq(sr),
            header.bits.eq(sr_next),
        ]

        if header_beats == 1 and header_leftover == 0:
            with m.If(sr_shift):
                m.d.comb += sr_next.eq(self.data_in.bits.data)
        else:
            with m.If(sr_shift):
                m.d.comb += sr_next.eq(
                    Cat(sr[beat_bytes * 8:], self.data_in.bits.data))
            with m.If(sr_shift_leftover):
                m.d.comb += sr_next.eq(
                    Cat(sr[header_leftover * 8:], self.data_in.bits.data))

        with m.If(sr_shift | sr_shift_leftover):
            m.d.sync += sr.eq(sr_next)

        m.d.comb += header.valid.eq((count == header_beats -
                                     (1 if aligned else 0))
                                    & self.data_in.valid & ~done)

        self.handler(m, header)

        with m.If(header.fire):
            m.d.sync += done.eq(1)
        with m.If(self.data_in.fire & self.data_in.bits.last):
            m.d.sync += done.eq(0)

        with m.If(self.data_in.valid):
            with m.If((count < header_beats) & ~done):
                m.d.comb += sr_shift.eq(1)

            if not aligned:
                with m.If((count == header_beats) & ~done):
                    m.d.comb += sr_shift_leftover.eq(1)

        with m.If(self.data_in.fire):
            with m.If(~done):
                m.d.sync += count.eq(count + 1)
            with m.If(self.data_in.bits.last):
                m.d.sync += count.eq(0)

        with m.If(header.valid):
            m.d.comb += [
                self.data_in.ready.eq(self.data_out.ready
                                      & header.ready),
                self.data_out.valid.eq(header.ready),
            ]

        return m


class Ipv4AddrCheck(Elaboratable):

    def __init__(self, data_width):
        self.data_width = data_width

        self.my_ip_addr = Signal(32)

        self.data_in = AXIStreamInterface(data_width=data_width)
        self.data_out = AXIStreamInterface(data_width=data_width)

        self._addr_valid_queue = Queue(6, Signal)
        self.addr_valid = Decoupled(Signal)

    def handle_header(self, m, header):
        m.d.comb += [
            self._addr_valid_queue.enq.bits.eq(
                (header.bits.dst_addr == self.my_ip_addr)
                | (header.bits.dst_addr == 0xffffffff)),
            self._addr_valid_queue.enq.valid.eq(header.valid),
            header.ready.eq(self._addr_valid_queue.enq.ready),
        ]

    def elaborate(self, platform):
        m = Module()

        header_extract = m.submodules.header_extract = Ipv4HeaderExtract(
            data_width=self.data_width, handler=self.handle_header)

        m.submodules.addr_valid_queue = self._addr_valid_queue

        m.d.comb += [
            self.data_in.connect(header_extract.data_in),
            header_extract.data_out.connect(self.data_out),
            self._addr_valid_queue.deq.connect(self.addr_valid),
        ]

        return m


class Ipv4Checksum(Elaboratable):

    def __init__(self, data_width, skip_checksum):
        self.data_width = data_width
        self.skip_checksum = skip_checksum

        self.data_in = AXIStreamInterface(data_width=data_width)
        self.data_out = AXIStreamInterface(data_width=data_width)

        self.checksum = Decoupled(Signal, 16)

    def elaborate(self, platform):
        m = Module()

        m.d.comb += self.data_in.connect(self.data_out)

        header_beats = (480 + self.data_width - 1) // self.data_width
        header_len = Signal(4)
        count = Signal(range(header_beats + 1))
        done = Signal()

        header_words = Signal(range(header_beats + 1))
        with m.Switch(header_len):
            for i in range(16):
                with m.Case(i):
                    m.d.comb += header_words.eq(
                        (i * 32 + self.data_width - 1) // self.data_width)

        subsum = [
            Signal(17, name=f"subsum{i}") for i in range(self.data_width >> 4)
        ]
        subsum_next = [
            Signal(17, name=f"subsum_next{i}")
            for i in range(self.data_width >> 4)
        ]
        m.d.comb += [sn.eq(s) for s, sn in zip(subsum, subsum_next)]

        for i in range(self.data_width >> 4):
            idx = count * (self.data_width >> 4) + i
            acc = subsum[i] + self.data_in.bits.data[i * 16:(i + 1) * 16]
            acc = (acc + (acc >> 16)) & 0xffff

            with m.If((not self.skip_checksum) | (idx != 5)):
                with m.If((idx >> 1) < Mux(
                        count == 0, self.data_in.bits.data[:4], header_len)):
                    m.d.comb += subsum_next[i].eq(acc)

        with m.If(self.data_in.fire):
            with m.If(~done):
                m.d.sync += count.eq(count + 1)
                m.d.sync += [s.eq(sn) for s, sn in zip(subsum, subsum_next)]

                with m.If(count == 0):
                    m.d.sync += header_len.eq(self.data_in.bits.data[:4])

            with m.If(count == header_words - 1):
                m.d.sync += done.eq(1)
            with m.If(self.data_in.bits.last):
                m.d.sync += [
                    count.eq(0),
                    done.eq(0),
                ]
                m.d.sync += [s.eq(0) for s in subsum]

        csum = sum(subsum_next)
        csum_queue = m.submodules.csum_queue = Queue(2, Signal, 16)
        m.d.comb += [
            csum_queue.enq.bits.eq((csum + (csum >> 16)) & 0xffff),
            csum_queue.enq.valid.eq(~done & self.data_in.valid
                                    & self.data_out.ready
                                    & ((count == header_words - 1)
                                       | self.data_in.bits.last)),
            csum_queue.deq.connect(self.checksum),
            self.checksum.bits.eq(~csum_queue.deq.bits),
        ]

        with m.If(~done & (count == header_words - 1)):
            m.d.comb += [
                self.data_in.ready.eq(self.data_out.ready
                                      & csum_queue.enq.ready),
                self.data_out.valid.eq(self.data_in.valid
                                       & csum_queue.enq.ready),
            ]

        return m


class Ipv4Dropper(Elaboratable):

    def __init__(self, data_width):
        self.data_width = data_width

        self.addr_valid = Decoupled(Signal)
        self.checksum = Decoupled(Signal, 16)

        self.data_in = AXIStreamInterface(data_width=data_width)
        self.data_out = AXIStreamInterface(data_width=data_width)

    def elaborate(self, platform):
        m = Module()

        header_beats = 480 // self.data_width

        q_in = m.submodules.q_in = Queue(header_beats + 2, Record,
                                         make_data_layout(self.data_width))
        m.d.comb += stream2queue(self.data_in, q_in)

        with m.FSM():
            with m.State("IDLE"):
                with m.If(self.addr_valid.valid & self.checksum.valid):
                    m.d.comb += [
                        self.addr_valid.ready.eq(1),
                        self.checksum.ready.eq(1),
                    ]

                    with m.If(self.addr_valid.bits
                              & (self.checksum.bits == 0)):
                        m.next = "FORWARD"
                    with m.Else():
                        m.next = "DROP"

            with m.State("FORWARD"):
                m.d.comb += queue2stream(q_in, self.data_out)

                with m.If(self.data_out.fire & self.data_out.bits.last):
                    m.next = "IDLE"

            with m.State("DROP"):
                m.d.comb += q_in.deq.ready.eq(1)

                with m.If(q_in.deq.fire & q_in.deq.bits.last):
                    m.next = "IDLE"
        return m


class Ipv4Dispatcher(Elaboratable):

    def __init__(self, data_width):
        self.data_width = data_width

        self.data_in = AXIStreamInterface(data_width=data_width)

        self.tcp_data_out = AXIStreamInterface(data_width=data_width)
        self.udp_data_out = AXIStreamInterface(data_width=data_width)

        self._protocol = SkidBuffer(Signal, 16)

    def handle_header(self, m, header):
        m.d.comb += [
            self._protocol.enq.bits.eq(header.bits.protocol),
            self._protocol.enq.valid.eq(header.valid),
            header.ready.eq(self._protocol.enq.ready),
        ]

    def elaborate(self, platform):
        m = Module()

        m.submodules.protocol = self._protocol

        header_extract = m.submodules.header_extract = Ipv4HeaderExtract(
            data_width=self.data_width, handler=self.handle_header)

        header_beats = 160 // self.data_width

        q_in = m.submodules.q_in = Queue(header_beats + 2, Record,
                                         make_data_layout(self.data_width))
        m.d.comb += stream2queue(self.data_in, q_in)

        m.d.comb += [
            self.data_in.connect(header_extract.data_in),
            stream2queue(header_extract.data_out, q_in),
        ]

        protocol = Signal(16)
        with m.FSM():
            with m.State("IDLE"):
                with m.If(self._protocol.deq.valid):
                    m.d.sync += protocol.eq(self._protocol.deq.bits)
                    m.d.comb += self._protocol.deq.ready.eq(1)
                    m.next = "FORWARD"

            with m.State("FORWARD"):
                with m.Switch(protocol):
                    with m.Case(IpProtocol.TCP):
                        m.d.comb += queue2stream(q_in, self.tcp_data_out)
                    with m.Case(IpProtocol.UDP):
                        m.d.comb += queue2stream(q_in, self.udp_data_out)
                    with m.Default():
                        m.d.comb += q_in.deq.ready.eq(1)

                with m.If(q_in.deq.fire & q_in.deq.bits.last):
                    m.next = "IDLE"

        return m


class Ipv4Handler(Elaboratable):

    def __init__(self, data_width):
        self.data_width = data_width

        self.my_ip_addr = Signal(32)

        self.data_in = AXIStreamInterface(data_width=data_width)

        self.tcp_data_out = AXIStreamInterface(data_width=data_width)
        self.udp_data_out = AXIStreamInterface(data_width=data_width)
        self.roce_data_out = AXIStreamInterface(data_width=data_width)

    def elaborate(self, platform):
        m = Module()

        addr_check = m.submodules.addr_check = Ipv4AddrCheck(
            data_width=self.data_width)

        checksum = m.submodules.checksum = Ipv4Checksum(
            data_width=self.data_width, skip_checksum=False)

        dropper = m.submodules.dropper = Ipv4Dropper(
            data_width=self.data_width)
        m.d.comb += [
            addr_check.addr_valid.connect(dropper.addr_valid),
            checksum.checksum.connect(dropper.checksum),
        ]

        dispatcher = m.submodules.dispatcher = Ipv4Dispatcher(
            data_width=self.data_width)

        udp_queue = m.submodules.udp_queue = Queue(
            2, Record, make_data_layout(self.data_width))
        m.d.comb += queue2stream(udp_queue, self.udp_data_out)
        roce_queue = m.submodules.roce_queue = Queue(
            2, Record, make_data_layout(self.data_width))
        m.d.comb += queue2stream(roce_queue, self.roce_data_out)

        m.d.comb += [
            addr_check.my_ip_addr.eq(self.my_ip_addr),
            self.data_in.connect(addr_check.data_in),
            addr_check.data_out.connect(checksum.data_in),
            checksum.data_out.connect(dropper.data_in),
            dropper.data_out.connect(dispatcher.data_in),
            dispatcher.tcp_data_out.connect(self.tcp_data_out),
            stream2queue(dispatcher.udp_data_out, udp_queue),
            stream2queue(dispatcher.udp_data_out, roce_queue),
            dispatcher.udp_data_out.ready.eq(udp_queue.enq.ready
                                             & roce_queue.enq.ready),
            udp_queue.enq.valid.eq(dispatcher.udp_data_out.valid
                                   & roce_queue.enq.ready),
            roce_queue.enq.valid.eq(dispatcher.udp_data_out.valid
                                    & udp_queue.enq.ready),
        ]

        return m


class Ipv4Depacketizer(Elaboratable):

    def __init__(self, data_width):
        self.data_width = data_width

        self.data_in = AXIStreamInterface(data_width=data_width)
        self.data_out = AXIStreamInterface(data_width=data_width)
        self.meta_out = Decoupled(Ipv4Metadata)

    def elaborate(self, platform):
        m = Module()

        depacketizer = m.submodules.depacketizer = AXIStreamDepacketizer(
            Record, IPv4_HEADER_LAYOUT, data_width=self.data_width)
        m.d.comb += self.data_in.connect(depacketizer.sink)

        with m.FSM():
            with m.State("IDLE"):
                with m.If(depacketizer.source.valid):
                    m.d.comb += [
                        self.meta_out.bits.peer_addr.eq(
                            depacketizer.header.src_addr),
                        self.meta_out.bits.length.eq(
                            Cat(depacketizer.header.total_len[8:],
                                depacketizer.header.total_len[:8])),
                        self.meta_out.valid.eq(1),
                    ]

                    with m.If(self.meta_out.ready):
                        m.next = "FORWARD"

            with m.State("FORWARD"):
                m.d.comb += depacketizer.source.connect(self.data_out)

                with m.If(self.data_out.fire & self.data_out.bits.last):
                    m.next = "IDLE"

        return m


class Ipv4Packetizer(Elaboratable):

    def __init__(self, data_width):
        self.data_width = data_width

        self.my_ip_addr = Signal(32)
        self.protocol = Signal(8)

        self.meta_in = Decoupled(Ipv4Metadata)
        self.data_in = AXIStreamInterface(data_width=data_width)
        self.data_out = AXIStreamInterface(data_width=data_width,
                                           user_width=128)

    def elaborate(self, platform):
        m = Module()

        header = Record(IPv4_HEADER_LAYOUT)
        sr = Signal(len(header))
        packetizer = m.submodules.packetizer = AXIStreamPacketizer(
            Record, IPv4_HEADER_LAYOUT, data_width=self.data_width)
        m.d.comb += [
            packetizer.header.eq(sr),
            packetizer.source.connect(self.data_out),
            self.data_out.bits.user[-32:].eq(packetizer.header.dst_addr),
        ]

        meta_queue = m.submodules.meta_queue = Queue(2,
                                                     Ipv4Metadata,
                                                     flow=False)
        m.d.comb += self.meta_in.connect(meta_queue.enq)

        packet_len = Signal.like(meta_queue.deq.bits.length)
        m.d.comb += packet_len.eq(meta_queue.deq.bits.length + 20)

        m.d.comb += [
            header.version.eq(0x4),
            header.ihl.eq(0x5),
            header.ttl.eq(0x40),
            header.total_len.eq(Cat(packet_len[8:], packet_len[:8])),
            header.flags.eq(0b010),
            header.dst_addr.eq(meta_queue.deq.bits.peer_addr),
            header.src_addr.eq(self.my_ip_addr),
            header.protocol.eq(self.protocol),
        ]

        csum_latency = 5
        checksum = m.submodules.checksum = Ipv4Checksum(data_width=32,
                                                        skip_checksum=True)
        csum_count = Signal(range(csum_latency))
        m.d.comb += checksum.data_out.ready.eq(1)

        with m.FSM():
            with m.State("IDLE"):
                m.d.comb += [
                    meta_queue.deq.ready.eq(checksum.data_in.ready),
                    checksum.data_in.bits.data.eq(header),
                    checksum.data_in.bits.keep.eq(~0),
                    checksum.data_in.valid.eq(meta_queue.deq.valid),
                ]

                with m.If(meta_queue.deq.fire):
                    m.d.sync += [
                        sr.eq(header),
                        csum_count.eq(0),
                    ]

                    m.next = "CHECKSUM_REQ"

            with m.State("CHECKSUM_REQ"):
                with m.Switch(csum_count):
                    for i in range(5):
                        with m.Case(i):
                            m.d.comb += [
                                checksum.data_in.bits.data.eq(
                                    sr[(i + 1) * 32:(i + 2) * 32]),
                                checksum.data_in.bits.keep.eq(~0),
                                checksum.data_in.bits.last.eq(
                                    i == csum_latency - 2),
                                checksum.data_in.valid.eq(1),
                            ]

                with m.If(checksum.data_in.fire):
                    m.d.sync += csum_count.eq(csum_count + 1)

                    with m.If(checksum.data_in.bits.last):
                        m.next = "CHECKSUM_RESP"

            with m.State("CHECKSUM_RESP"):
                m.d.comb += checksum.checksum.ready.eq(1)

                with m.If(checksum.checksum.fire):
                    m.d.comb += [
                        header.eq(sr),
                        header.checksum.eq(checksum.checksum.bits),
                    ]
                    m.d.sync += sr.eq(header)

                    m.next = "FORWARD"

            with m.State("FORWARD"):
                m.d.comb += self.data_in.connect(packetizer.sink)

                with m.If(self.data_out.fire & self.data_out.bits.last):
                    m.next = "IDLE"

        return m


class Ipv4(Elaboratable):

    def __init__(self, data_width):
        self.data_width = data_width

        self.my_ip_addr = Signal(32)
        self.protocol = Signal(8)

        self.rx_data_in = AXIStreamInterface(data_width=data_width)
        self.rx_data_out = AXIStreamInterface(data_width=data_width)
        self.rx_meta_out = Decoupled(Ipv4Metadata)

        self.tx_data_in = AXIStreamInterface(data_width=data_width)
        self.tx_meta_in = Decoupled(Ipv4Metadata)
        self.tx_data_out = AXIStreamInterface(data_width=data_width,
                                              user_width=128)

    def elaborate(self, platform):
        m = Module()

        # RX

        depacketizer = m.submodules.depacketizer = Ipv4Depacketizer(
            data_width=self.data_width)
        m.d.comb += [
            self.rx_data_in.connect(depacketizer.data_in),
            depacketizer.data_out.connect(self.rx_data_out),
        ]

        rx_meta_queue = m.submodules.rx_meta_queue = Queue(2, Ipv4Metadata)
        m.d.comb += [
            depacketizer.meta_out.connect(rx_meta_queue.enq),
            rx_meta_queue.deq.connect(self.rx_meta_out),
        ]

        # TX

        packetizer = m.submodules.packetizer = Ipv4Packetizer(
            data_width=self.data_width)
        m.d.comb += [
            packetizer.my_ip_addr.eq(self.my_ip_addr),
            packetizer.protocol.eq(self.protocol),
            self.tx_data_in.connect(packetizer.data_in),
            self.tx_meta_in.connect(packetizer.meta_in),
            packetizer.data_out.connect(self.tx_data_out),
        ]

        return m
