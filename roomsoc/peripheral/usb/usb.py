from amaranth import *
from amaranth.hdl.rec import Direction
from enum import IntEnum
import functools
import operator

from roomsoc.interconnect.stream import Decoupled, Valid


class UsbPid(IntEnum):
    OUT = 0b0001
    IN = 0b1001
    SOF = 0b0101
    SETUP = 0b1101
    DATA0 = 0b0011
    DATA1 = 0b1011
    DATA2 = 0b0111
    MDATA = 0b1111
    ACK = 0b0010
    NAK = 0b1010
    STALL = 0b1110
    NYET = 0b0110
    PRE = 0b1100
    ERR = 0b1100
    SPLIT = 0b1000
    PING = 0b0100


class UsbToken(Record):

    def __init__(self, name=None, src_loc_at=0):
        super().__init__([
            ('pid', UsbPid, Direction.FANOUT),
            ('data', 11, Direction.FANOUT),
        ],
                         name=name,
                         src_loc_at=1 + src_loc_at)


class UsbDataStream(Record):

    def __init__(self, name=None, src_loc_at=0):
        super().__init__([
            ('data', 8, Direction.FANOUT),
            ('last', 1, Direction.FANOUT),
        ],
                         name=name,
                         src_loc_at=1 + src_loc_at)


class UsbDataCrc(Elaboratable):

    def __init__(self, initial_value=0xffff):
        self.initial_value = initial_value

        self.clear = Signal()
        self.data = Valid(Signal, 8)

        self.crc = Signal(16, reset=initial_value)

    def _generate_data_crc(self, crc, data):
        xor_reduce = lambda bits: functools.reduce(operator.__xor__, bits)

        return Cat(
            xor_reduce(data) ^ xor_reduce(crc[8:16]),
            xor_reduce(data[0:7]) ^ xor_reduce(crc[9:16]),
            xor_reduce(data[6:8]) ^ xor_reduce(crc[8:10]),
            xor_reduce(data[5:7]) ^ xor_reduce(crc[9:11]),
            xor_reduce(data[4:6]) ^ xor_reduce(crc[10:12]),
            xor_reduce(data[3:5]) ^ xor_reduce(crc[11:13]),
            xor_reduce(data[2:4]) ^ xor_reduce(crc[12:14]),
            xor_reduce(data[1:3]) ^ xor_reduce(crc[13:15]),
            xor_reduce(data[0:2]) ^ xor_reduce(crc[14:16]) ^ crc[0],
            data[0] ^ crc[1] ^ crc[15],
            crc[2],
            crc[3],
            crc[4],
            crc[5],
            crc[6],
            xor_reduce(data) ^ xor_reduce(crc[7:16]),
        )

    def elaborate(self, platform):
        m = Module()

        crc = Signal(16, reset=self.initial_value)

        with m.If(self.clear):
            m.d.sync += crc.eq(self.initial_value)
        with m.Elif(self.data.valid):
            m.d.sync += crc.eq(self._generate_data_crc(crc, self.data.bits))

        m.d.comb += self.crc.eq(~crc[::-1])

        return m


class UsbDataTx(Elaboratable):

    class Request(Record):

        def __init__(self, name=None, src_loc_at=0):
            super().__init__([
                ('pid', UsbPid, Direction.FANOUT),
                ('zero_len', 1, Direction.FANOUT),
            ],
                             name=name,
                             src_loc_at=1 + src_loc_at)

    def __init__(self):
        self.req = Decoupled(UsbDataTx.Request)
        self.data = Decoupled(UsbDataStream)

        self.tx_data = Decoupled(UsbDataStream)
        self.tx_eop = Signal()

    def elaborate(self, platform):
        m = Module()

        crc = m.submodules.crc = UsbDataCrc()
        m.d.comb += [
            crc.data.valid.eq(self.data.fire),
            crc.data.bits.eq(self.data.bits),
        ]

        with m.FSM():
            with m.State('IDLE'):
                with m.If(self.req.valid):
                    m.next = 'PID'

            with m.State('PID'):
                m.d.comb += [
                    self.tx_data.bits.data.eq(
                        Cat(self.req.bits.pid, ~self.req.bits.pid)),
                    self.tx_data.valid.eq(1),
                    crc.clear.eq(1),
                ]

                with m.If(self.tx_data.fire):
                    with m.If(self.req.bits.zero_len):
                        m.next = 'CRC0'
                    with m.Else():
                        m.next = 'DATA'

            with m.State('DATA'):
                m.d.comb += [
                    self.tx_data.valid.eq(1),
                    self.tx_data.bits.data.eq(self.data.bits.data),
                ]

                with m.If(self.tx_data.fire):
                    m.d.comb += self.data.ready.eq(1)

                    with m.If(self.data.bits.last):
                        m.next = 'CRC0'

            with m.State('CRC0'):
                m.d.comb += [
                    self.tx_data.valid.eq(1),
                    self.tx_data.bits.data.eq(crc.crc[:8]),
                ]

                with m.If(self.tx_data.fire):
                    m.next = 'CRC1'

            with m.State('CRC1'):
                m.d.comb += [
                    self.tx_data.valid.eq(1),
                    self.tx_data.bits.data.eq(crc.crc[8:]),
                    self.tx_data.bits.last.eq(1),
                ]

                with m.If(self.tx_data.fire):
                    m.next = 'EOP'

            with m.State('EOP'):
                with m.If(self.tx_eop):
                    m.d.comb += self.req.ready.eq(1)

                    m.next = 'IDLE'

        return m


class UsbDataRx(Elaboratable):

    def __init__(self):
        self.start = Signal()
        self.done = Signal()

        self.rx_data = Valid(Signal, 8)
        self.rx_active = Signal()
        self.rx_stuffing = Signal()

        self.pid = Signal(UsbPid)
        self.pid_error = Signal()
        self.crc_error = Signal()
        self.stuffing_error = Signal()

        self.data = Valid(Signal, 8)

    def elaborate(self, platform):
        m = Module()

        crc = m.submodules.crc = UsbDataCrc()
        m.d.comb += crc.data.bits.eq(self.rx_data.bits)

        data = [Signal(8) for _ in range(2)]
        valids = Signal(2)

        with m.If(self.rx_data.valid):
            m.d.sync += [
                data[0].eq(self.rx_data.bits),
                data[1].eq(data[0]),
            ]
        m.d.comb += self.data.bits.eq(data[1])

        with m.If(self.done & self.rx_data.valid & self.rx_stuffing):
            m.d.sync += self.stuffing_error.eq(1)

        with m.FSM():
            with m.State('IDLE'):
                m.d.comb += self.done.eq(1)
                m.d.sync += [
                    self.pid_error.eq(0),
                    self.crc_error.eq(0),
                    self.stuffing_error.eq(0),
                ]

                with m.If(self.start):
                    m.next = 'WAIT_ACTIVE'

            with m.State('WAIT_ACTIVE'):
                with m.If(self.rx_active):
                    m.next = 'PID'

            with m.State('PID'):
                m.d.comb += crc.clear.eq(1)
                m.d.sync += valids.eq(0)

                with m.If(self.rx_data.valid):
                    m.d.sync += [
                        self.pid.eq(self.rx_data.bits[:4]),
                        self.pid_error.eq(
                            self.rx_data.bits[:4] != self.rx_data.bits[4:]),
                    ]

                    m.next = 'DATA'

            with m.State('DATA'):
                with m.If(~self.rx_active):
                    with m.If(~valids.all() | (crc.crc != 0x4ffe)):
                        m.d.sync += self.crc_error.eq(1)

                    m.next = 'IDLE'

                with m.Elif(self.rx_data.valid):
                    m.d.comb += crc.data.valid.eq(1)
                    m.d.sync += valids.eq(Cat(Const(1, 1), valids[0]))

                    m.d.comb += self.data.valid.eq(valids.all())

        return m


def _generate_crc_for_token(token):

    def xor_bits(*indices):
        bits = (token[len(token) - 1 - i] for i in indices)
        return functools.reduce(operator.__xor__, bits)

    return Cat(
        xor_bits(10, 9, 8, 5, 4, 2),
        ~xor_bits(10, 9, 8, 7, 4, 3, 1),
        xor_bits(10, 9, 8, 7, 6, 3, 2, 0),
        xor_bits(10, 7, 6, 4, 1),
        xor_bits(10, 9, 6, 5, 3, 0),
    )


class UsbTokenTx(Elaboratable):

    def __init__(self):
        self.token = Decoupled(UsbToken)

        self.tx_data = Decoupled(UsbDataStream)
        self.tx_eop = Signal()

    def elaborate(self, platform):
        m = Module()

        with m.FSM():
            with m.State('IDLE'):
                with m.If(self.token.valid):
                    m.next = 'PID'

            with m.State('PID'):
                m.d.comb += [
                    self.tx_data.bits.data.eq(
                        Cat(self.token.bits.pid, ~self.token.bits.pid)),
                    self.tx_data.bits.last.eq(
                        self.token.bits.pid == UsbPid.ACK),
                    self.tx_data.valid.eq(1),
                ]

                with m.If(self.tx_data.fire):
                    with m.If(self.tx_data.bits.last):
                        m.next = 'EOP'
                    with m.Else():
                        m.next = 'D0'

            with m.State('D0'):
                m.d.comb += [
                    self.tx_data.bits.data.eq(self.token.bits.data[:8]),
                    self.tx_data.valid.eq(1),
                ]

                with m.If(self.tx_data.fire):
                    m.next = 'D1'

            with m.State('D1'):
                crc = _generate_crc_for_token(self.token.bits.data)

                m.d.comb += [
                    self.tx_data.bits.data.eq(
                        Cat(self.token.bits.data[8:], crc)),
                    self.tx_data.bits.last.eq(1),
                    self.tx_data.valid.eq(1),
                ]

                with m.If(self.tx_data.fire):
                    m.next = 'EOP'

            with m.State('EOP'):
                with m.If(self.tx_eop):
                    m.d.comb += self.token.ready.eq(1)

                    m.next = 'IDLE'

        return m
