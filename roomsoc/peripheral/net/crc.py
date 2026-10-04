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

    def __init__(self,
                 data_width,
                 user_width=1,
                 *,
                 crc_width=32,
                 polynomial=0xedb88320,
                 initial_value=0xffffffff,
                 final_xor=0xffffffff):
        if data_width <= 0 or data_width % 8:
            raise ValueError("data_width must be a positive multiple of 8")
        if crc_width < 8:
            raise ValueError("crc_width must be at least 8")

        crc_mask = (1 << crc_width) - 1
        for name, value in (("polynomial", polynomial),
                            ("initial_value", initial_value), ("final_xor",
                                                               final_xor)):
            if not 0 <= value <= crc_mask:
                raise ValueError(f"{name} must fit in crc_width bits")

        self.data_width = data_width
        self.user_width = user_width
        self.crc_width = crc_width
        self.polynomial = polynomial
        self.initial_value = initial_value
        self.final_xor = final_xor

        self.data_in = AXIStreamInterface(data_width=data_width,
                                          user_width=user_width)
        self.data_out = AXIStreamInterface(data_width=data_width,
                                           user_width=user_width)

        self.crc = Decoupled(Signal, crc_width)

    def elaborate(self, platform):
        m = Module()

        crc = Signal(self.crc_width, reset=self.initial_value)

        # Compact the valid bytes before the input register boundary. The
        # original beat is carried separately so sparse keep and user metadata
        # pass through unchanged. Prefix counts avoid a serial byte-mux chain.
        byte_count = self.data_width // 8
        q_in = m.submodules.q_in = Queue(
            2,
            Record, [
                ("beat", self.data_in.bits.layout),
                ("packed", self.data_width),
                ("count", range(byte_count + 1)),
            ],
            flow=False)
        m.d.comb += [
            q_in.enq.valid.eq(self.data_in.valid),
            self.data_in.ready.eq(q_in.enq.ready),
            q_in.enq.bits.beat.eq(self.data_in.bits),
            q_in.enq.bits.count.eq(sum(self.data_in.bits.keep)),
        ]
        prefixes = []
        for lane in range(byte_count):
            prefix = Signal(range(byte_count + 1), name=f"prefix{lane}")
            m.d.comb += prefix.eq(sum(self.data_in.bits.keep[:lane]))
            prefixes.append(prefix)
        for slot in range(byte_count):
            selected = Const(0, 8)
            for lane in range(slot, byte_count):
                selected = selected | Mux(
                    self.data_in.bits.keep[lane] & (prefixes[lane] == slot),
                    self.data_in.bits.data.word_select(lane, 8), 0)
            m.d.comb += q_in.enq.bits.packed.word_select(slot, 8).eq(selected)

        q_out = m.submodules.q_out = Queue(2, self.data_out, flow=False)
        m.d.comb += q_out.deq.connect(self.data_out)

        # Keep the parallel CRC calculation on the input side of a
        # register boundary; bypass would extend it through CRC insertion
        # and into the transmit data FIFO. q_out is registered as well,
        # so the final data beat and its CRC remain available together.
        crc_queue = m.submodules.crc_queue = Queue(2,
                                                   Signal,
                                                   self.crc_width,
                                                   flow=False)
        m.d.comb += crc_queue.deq.connect(self.crc)

        m.d.comb += [
            q_out.enq.bits.eq(q_in.deq.bits.beat),
            q_out.enq.valid.eq(q_in.deq.valid
                               & (~q_in.deq.bits.beat.last
                                  | crc_queue.enq.ready)),
            q_in.deq.ready.eq(q_out.enq.ready
                              & (~q_in.deq.bits.beat.last
                                 | crc_queue.enq.ready)),
        ]

        crc_next = Signal.like(crc)
        m.d.comb += crc_queue.enq.bits.eq(crc_next ^ self.final_xor)

        # CRC is linear over GF(2). Expand the whole beat at elaboration
        # time into XOR masks instead of cascading eight asynchronous ROMs
        # and keep muxes. Each possible packed length has a parallel XOR
        # network; the registered byte count selects the result. This keeps
        # the feedback recurrence within one cycle (one beat per cycle).
        inputs = Cat(crc, q_in.deq.bits.packed)
        masks = [1 << bit for bit in range(self.crc_width)]
        candidates = [crc]
        for lane in range(byte_count):
            for bit in range(8):
                feedback = masks[0] ^ (1 << (self.crc_width + lane * 8 + bit))
                masks = masks[1:] + [0]
                masks = [
                    mask ^ feedback if (self.polynomial >> out) & 1 else mask
                    for out, mask in enumerate(masks)
                ]
            candidate = Signal(self.crc_width, name=f"crc_length{lane + 1}")
            for out, mask in enumerate(masks):
                terms = [
                    inputs[bit] for bit in range(len(inputs))
                    if (mask >> bit) & 1
                ]
                m.d.comb += candidate[out].eq(Cat(*terms).xor())
            candidates.append(candidate)
        m.d.comb += crc_next.eq(Array(candidates)[q_in.deq.bits.count])

        with m.If(q_in.deq.fire):
            m.d.comb += crc_queue.enq.valid.eq(q_in.deq.bits.beat.last)
            m.d.sync += crc.eq(
                Mux(q_in.deq.bits.beat.last, crc.reset, crc_next))

        return m


class CrcInsert(Elaboratable):

    def __init__(self, data_width, user_width):
        self.data_width = data_width
        self.user_width = user_width

        self.data_in = AXIStreamInterface(data_width=data_width,
                                          user_width=user_width)
        self.data_out = AXIStreamInterface(data_width=data_width,
                                           user_width=user_width)

    def elaborate(self, platform):
        m = Module()

        crc_calc = m.submodules.crc_calc = CrcCalculate(
            data_width=self.data_width, user_width=self.user_width)
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

    def __init__(self, data_width, user_width=128):
        self.data_width = data_width
        self.user_width = user_width

        self.rx_data_in = AXIStreamInterface(data_width=data_width)
        self.rx_data_out = AXIStreamInterface(data_width=data_width)

        self.tx_data_in = AXIStreamInterface(data_width=data_width,
                                             user_width=user_width)
        self.tx_data_out = AXIStreamInterface(data_width=data_width,
                                              user_width=user_width)

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

        with m.If(rx_crc_extract.crc.valid & rx_crc_calc.crc.valid):
            m.d.comb += [
                rx_crc_extract.crc.ready.eq(1),
                rx_crc_calc.crc.ready.eq(1),
            ]

            with m.If(rx_crc_extract.crc.bits != rx_crc_calc.crc.bits):
                m.d.sync += self.crc_packet_drops.eq(self.crc_packet_drops + 1)

        # TX

        tx_crc_insert = m.submodules.tx_crc_insert = CrcInsert(
            data_width=self.data_width, user_width=self.user_width)

        m.d.comb += [
            self.tx_data_in.connect(tx_crc_insert.data_in),
            tx_crc_insert.data_out.connect(self.tx_data_out),
        ]

        return m
