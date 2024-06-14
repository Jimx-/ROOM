from amaranth import *

from roomsoc.interconnect.axi import AXIStreamInterface
from roomsoc.interconnect.stream import Queue, Decoupled

from .common import *

__all__ = ("Crc", )


class CrcExtract(Elaboratable):

    def __init__(self, data_width):
        self.data_width = data_width

        self.data_in = AXIStreamInterface(data_width=data_width)
        self.data_out = AXIStreamInterface(data_width=data_width)

        self.crc = Decoupled(Signal, 32)

    def elaborate(self, platform):
        m = Module()

        crc_queue = m.submodules.crc_queue = Queue(2, Signal, 32)
        m.d.comb += crc_queue.deq.connect(self.crc)

        prev_word = Record(make_data_layout(self.data_width))

        with m.FSM():
            with m.State("FIRST"):
                m.d.comb += self.data_in.ready.eq(1)

                with m.If(self.data_in.fire):
                    m.d.sync += [
                        prev_word.data.eq(self.data_in.bits.data),
                        prev_word.keep.eq(self.data_in.bits.keep),
                    ]

                    with m.If(self.data_in.bits.last):
                        m.next = "LAST"
                    with m.Else():
                        m.next = "FORWARD"

            with m.State("FORWARD"):
                m.d.comb += self.data_in.ready.eq(self.data_out.ready
                                                  & (~self.data_in.bits.last
                                                     | crc_queue.enq.ready))

                with m.If(self.data_in.fire):
                    m.d.comb += [
                        self.data_out.bits.data.eq(prev_word.data),
                        self.data_out.bits.keep.eq(prev_word.keep),
                        self.data_out.valid.eq(1),
                    ]
                    m.d.sync += [
                        prev_word.data.eq(self.data_in.bits.data),
                        prev_word.keep.eq(self.data_in.bits.keep),
                    ]

                    with m.If(self.data_in.bits.last):
                        with m.If(self.data_in.bits.keep[4]):
                            m.next = "LAST"
                        with m.Else():
                            m.d.comb += [
                                self.data_out.bits.last.eq(1),
                                crc_queue.enq.bits.eq(
                                    self.data_out.bits.data[:32]),
                                crc_queue.enq.valid.eq(1),
                            ]
                            m.next = "FIRST"

            with m.State("LAST"):
                m.d.comb += [
                    self.data_in.ready.eq(self.data_out.ready
                                          & crc_queue.enq.ready),
                    self.data_out.bits.data.eq(prev_word.data),
                    self.data_out.bits.keep.eq(prev_word.keep),
                    self.data_out.bits.last.eq(1),
                    self.data_out.valid.eq(crc_queue.enq.ready),
                ]

                with m.If(self.data_out.fire):
                    m.d.comb += crc_queue.enq.valid.eq(1)

                    with m.Switch(prev_word.keep):
                        for i in range(len(prev_word.keep) // 4):
                            with m.Case(2**((i + 1) * 4) - 1):
                                m.d.comb += [
                                    crc_queue.enq.bits.eq(
                                        prev_word[i * 32:(i + 1) * 32]),
                                    self.data_out.bits.keep[i * 4:(i + 1) *
                                                            4].eq(0),
                                ]

                    m.next = "FIRST"

        return m


class CrcCalculate(Elaboratable):

    def __init__(self, data_width):
        self.data_width = data_width

        self.data_in = AXIStreamInterface(data_width=data_width)
        self.data_out = AXIStreamInterface(data_width=data_width)

        self.crc = Decoupled(Signal, 32)

    def elaborate(self, platform):
        m = Module()

        crc = Signal(32, reset=0xffffffff)

        q_in = m.submodules.q_in = Queue(2,
                                         Record,
                                         make_data_layout(self.data_width),
                                         flow=False)
        m.d.comb += stream2queue(self.data_in, q_in)

        q_out = m.submodules.q_out = Queue(2,
                                           Record,
                                           make_data_layout(self.data_width),
                                           flow=False)
        m.d.comb += queue2stream(q_out, self.data_out)

        crc_queue = m.submodules.crc_queue = Queue(2, Signal, 32)
        m.d.comb += crc_queue.deq.connect(self.crc)

        m.d.comb += [
            q_in.deq.connect(q_out.enq),
            q_in.deq.ready.eq(q_out.enq.ready
                              & (~q_in.deq.bits.last | crc_queue.enq.ready)),
        ]

        crc_next = Signal.like(crc)
        m.d.comb += crc_queue.enq.bits.eq(crc_next ^ 0xffffffff)

        crc_bytes = [
            Signal(32, name=f'crc_byte{i}')
            for i in range(self.data_width // 8 + 1)
        ]
        m.d.comb += crc_bytes[0].eq(crc)

        polynomial = Const(0xedb88320, 32)

        with m.If(q_in.deq.fire):
            for i in range(self.data_width // 8):
                m.d.comb += crc_bytes[i + 1].eq(crc_bytes[i])

                with m.If(q_in.deq.bits.keep[i]):
                    acc = crc_bytes[i] ^ q_in.deq.bits.data[i * 8:(i + 1) * 8]

                    for _ in range(8):
                        mask = Signal(32)
                        m.d.comb += mask.eq(-(acc & 1))
                        acc = (acc >> 1) ^ (polynomial & mask)

                    m.d.comb += crc_bytes[i + 1].eq(acc)

            m.d.comb += [
                crc_next.eq(crc_bytes[-1]),
                crc_queue.enq.valid.eq(q_in.deq.bits.last),
            ]
            m.d.sync += crc.eq(Mux(q_in.deq.bits.last, crc.reset, crc_next))

        return m


class CrcInsert(Elaboratable):

    def __init__(self, data_width):
        self.data_width = data_width

        self.data_in = AXIStreamInterface(data_width=data_width)
        self.data_out = AXIStreamInterface(data_width=data_width)

    def elaborate(self, platform):
        m = Module()

        crc_calc = m.submodules.crc_calc = CrcCalculate(
            data_width=self.data_width)
        m.d.comb += self.data_in.connect(crc_calc.data_in)

        with m.FSM():
            with m.State("FORWARD"):
                m.d.comb += crc_calc.data_out.connect(self.data_out)

                with m.If(crc_calc.data_out.fire
                          & crc_calc.data_out.bits.last):
                    with m.If(crc_calc.data_out.bits.keep[-1]):
                        m.d.comb += self.data_out.bits.last.eq(0)
                        m.next = "LAST"
                    with m.Else():
                        m.d.comb += crc_calc.crc.ready.eq(1)

                        with m.Switch(crc_calc.data_out.bits.keep):
                            for i in range(
                                    len(crc_calc.data_out.bits.keep) // 4):
                                with m.Case(2**(i * 4) - 1):
                                    m.d.comb += [
                                        self.data_out.bits.data[i * 32:(
                                            i + 1) * 32].eq(crc_calc.crc.bits),
                                        self.data_out.bits.keep[i * 4:(i + 1) *
                                                                4].eq(0b1111),
                                    ]

            with m.State("LAST"):
                m.d.comb += [
                    self.data_out.bits.data[:32].eq(crc_calc.crc.bits),
                    self.data_out.bits.keep[:4].eq(0b1111),
                    self.data_out.bits.last.eq(1),
                    self.data_out.valid.eq(1),
                ]

                with m.If(self.data_out.fire):
                    m.d.comb += crc_calc.crc.ready.eq(1)

                    m.next = "FORWARD"

        return m


class Crc(Elaboratable):

    def __init__(self, data_width):
        self.data_width = data_width

        self.rx_data_in = AXIStreamInterface(data_width=data_width)
        self.rx_data_out = AXIStreamInterface(data_width=data_width)

        self.tx_data_in = AXIStreamInterface(data_width=data_width)
        self.tx_data_out = AXIStreamInterface(data_width=data_width)

        self.crc_packet_drops = Signal(32)

    def elaborate(self, platform):
        m = Module()

        # RX

        rx_crc_extract = m.submodules.rx_crc_extract = CrcExtract(
            data_width=self.data_width)

        rx_crc_calc = m.submodules.rx_crc_calc = CrcCalculate(
            data_width=self.data_width)

        m.d.comb += [
            self.rx_data_in.connect(rx_crc_extract.data_in),
            rx_crc_extract.data_out.connect(rx_crc_calc.data_in),
            rx_crc_calc.data_out.connect(self.rx_data_out),
        ]

        with m.If(rx_crc_extract.crc.valid & rx_crc_extract.crc.valid):
            m.d.comb += [
                rx_crc_extract.crc.ready.eq(1),
                rx_crc_calc.crc.ready.eq(1),
            ]

            with m.If(rx_crc_extract.crc.bits != rx_crc_calc.crc.bits):
                m.d.sync += self.crc_packet_drops.eq(self.crc_packet_drops + 1)

        # TX

        tx_crc_insert = m.submodules.tx_crc_insert = CrcInsert(
            data_width=self.data_width)

        m.d.comb += [
            self.tx_data_in.connect(tx_crc_insert.data_in),
            tx_crc_insert.data_out.connect(self.tx_data_out),
        ]

        return m
