from amaranth import *
from amaranth.hdl.rec import Direction
import math

from room.utils import Decoupled, Valid

from .usb import UsbDataStream


class UsbLsFsPhyIo(Record):

    def __init__(self, name=None, src_loc_at=0):
        super().__init__([
            ('dp_i', 1, Direction.FANIN),
            ('dp_o', 1, Direction.FANOUT),
            ('dp_t', 1, Direction.FANOUT),
            ('dm_i', 1, Direction.FANIN),
            ('dm_o', 1, Direction.FANOUT),
            ('dm_t', 1, Direction.FANOUT),
        ],
                         name=name,
                         src_loc_at=1 + src_loc_at)


class UsbCtrlPort(Record):

    def __init__(self, name=None, src_loc_at=0):
        super().__init__([
            ('power', 1, Direction.FANOUT),
            ('connect_', 1, Direction.FANIN),
            ('disconnect', 1, Direction.FANIN),
            ('reset', [
                ('valid', 1, Direction.FANOUT),
                ('ready', 1, Direction.FANIN),
            ]),
        ],
                         name=name,
                         src_loc_at=1 + src_loc_at)


class UsbTxData(Record):

    def __init__(self, name=None, src_loc_at=0):
        super().__init__([
            ('data', 1, Direction.FANOUT),
            ('se0', 1, Direction.FANOUT),
            ('low_speed', 1, Direction.FANOUT),
        ],
                         name=name,
                         src_loc_at=1 + src_loc_at)


class UsbRxData(Record):

    def __init__(self, name=None, src_loc_at=0):
        super().__init__([
            ('data', 8, Direction.FANOUT),
            ('stuffing_error', 1, Direction.FANOUT),
        ],
                         name=name,
                         src_loc_at=1 + src_loc_at)


class NRZIEncoder(Elaboratable):

    def __init__(self):
        self.low_speed = Signal()

        self.clock_posedge = Signal()

        self.inp = Decoupled(Signal)
        self.out = Valid(UsbTxData)

    def elaborate(self, platform):
        m = Module()

        counter = Signal(3)
        state = Signal(reset=1)

        with m.If(self.inp.valid):
            m.d.comb += [
                self.out.valid.eq(1),
                self.out.bits.low_speed.eq(self.low_speed),
            ]

            with m.If(self.inp.bits):
                m.d.comb += self.out.bits.data.eq(state)

                with m.If(self.clock_posedge):
                    m.d.sync += counter.eq(counter + 1)
                    m.d.comb += self.inp.ready.eq(1)

                    with m.If(counter == 5):
                        m.d.sync += state.eq(~state)
                        m.d.comb += self.inp.ready.eq(0)

                    with m.Elif(counter == 6):
                        m.d.sync += counter.eq(0)

            with m.Else():
                m.d.comb += self.out.bits.data.eq(~state)

                with m.If(self.clock_posedge):
                    m.d.comb += self.inp.ready.eq(1)
                    m.d.sync += [
                        state.eq(~state),
                        counter.eq(0),
                    ]

        with m.Else():
            m.d.sync += [
                counter.eq(0),
                state.eq(1),
            ]

        return m


class UsbTxSerializer(Elaboratable):

    def __init__(self):
        self.inp = Decoupled(Signal, 8)
        self.out = Decoupled(Signal)

    def elaborate(self, platform):
        m = Module()

        counter = Signal(3)
        with m.If(self.inp.valid):
            with m.Switch(counter):
                for i in range(8):
                    with m.Case(i):
                        m.d.comb += self.out.bits.eq(self.inp.bits[i])

            m.d.comb += self.out.valid.eq(1)

            with m.If(self.out.fire):
                m.d.sync += counter.eq(counter + 1)
                with m.If(counter == 7):
                    m.d.comb += self.inp.ready.eq(1)

        with m.If(~self.inp.valid | self.inp.ready):
            m.d.sync += counter.eq(0)

        return m


class UsbPhyTx(Elaboratable):

    def __init__(self):
        self.low_speed = Signal()

        self.clock_posedge = Signal()

        self.tx_data = Decoupled(UsbDataStream)
        self.tx_eop = Signal()

        self.out = Valid(UsbTxData)

    def elaborate(self, platform):
        m = Module()

        encoder = m.submodules.encoder = NRZIEncoder()
        m.d.comb += encoder.clock_posedge.eq(self.clock_posedge)

        serializer = m.submodules.serializer = UsbTxSerializer()

        was_low_speed = Signal()
        counter = Signal()
        with m.FSM():
            with m.State('IDLE'):
                m.d.sync += was_low_speed.eq(self.low_speed)

                with m.If(self.tx_data.valid & self.clock_posedge):
                    m.next = 'PREAMBLE'

            with m.State('PREAMBLE'):
                m.d.comb += [
                    encoder.clock_posedge.eq(0),
                    encoder.inp.bits.eq(1),
                    encoder.inp.valid.eq(1),
                    encoder.low_speed.eq(0),
                    self.out.eq(encoder.out),
                ]

                with m.If(self.clock_posedge):
                    m.next = 'SYNC'

            with m.State('SYNC'):
                m.d.comb += [
                    serializer.inp.bits.eq(0x80),
                    serializer.inp.valid.eq(1),
                    serializer.out.connect(encoder.inp),
                    encoder.low_speed.eq(was_low_speed),
                    self.out.eq(encoder.out),
                ]

                with m.If(serializer.inp.fire):
                    m.next = 'DATA'

            with m.State('DATA'):
                m.d.comb += [
                    serializer.inp.bits.eq(self.tx_data.bits),
                    serializer.inp.valid.eq(1),
                    serializer.out.connect(encoder.inp),
                    encoder.low_speed.eq(was_low_speed),
                    self.out.eq(encoder.out),
                ]

                with m.If(serializer.inp.fire):
                    m.d.comb += self.tx_data.ready.eq(1)

                    with m.If(self.tx_data.bits.last):
                        m.d.sync += counter.eq(0)
                        m.next = 'EOP_0'

            with m.State('EOP_0'):
                m.d.comb += [
                    self.out.bits.se0.eq(1),
                    self.out.bits.low_speed.eq(was_low_speed),
                    self.out.valid.eq(1),
                ]

                with m.If(self.clock_posedge):
                    m.d.sync += counter.eq(counter + 1)

                    with m.If(counter == 1):
                        m.d.comb += self.tx_eop.eq(1)
                        m.next = 'EOP_1'

            with m.State('EOP_1'):
                m.d.comb += [
                    self.out.bits.data.eq(1),
                    self.out.bits.low_speed.eq(was_low_speed),
                    self.out.valid.eq(1),
                ]

                with m.If(self.clock_posedge):
                    m.d.sync += counter.eq(0)
                    m.next = 'EOP_2'

            with m.State('EOP_2'):
                with m.If(self.clock_posedge):
                    m.d.sync += counter.eq(counter + 1)

                    with m.If(counter == 1):
                        m.next = 'IDLE'

        return m


class UsbRxFilter(Elaboratable):

    def __init__(self, clk_freq):
        self.clk_freq = clk_freq

        self.low_speed = Signal()

        self.dp_i = Signal()
        self.dm_i = Signal()

        self.clock_data_in = Signal()

        self.dp = Signal()
        self.dm = Signal()
        self.se0 = Signal()

    def elaborate(self, platform):
        m = Module()

        fs_divisor = int(self.clk_freq // 12e6)
        ls_divisor = fs_divisor * 8
        divisor = Mux(self.low_speed, ls_divisor, fs_divisor)

        clock_count_in = Signal(range(ls_divisor))
        clear = Signal()
        m.d.sync += clock_count_in.eq(clock_count_in + 1)
        with m.If((clock_count_in == divisor - 1) | clear):
            m.d.sync += clock_count_in.eq(0)
        m.d.comb += self.clock_data_in.eq((clock_count_in == (divisor >> 1) -
                                           2)
                                          & ~clear)

        dp_i_last = Signal()
        dm_i_last = Signal()
        dp_i_edge = Signal()
        dm_i_edge = Signal()
        m.d.sync += [
            dp_i_last.eq(self.dp_i),
            dm_i_last.eq(self.dm_i),
        ]
        m.d.comb += [
            dp_i_edge.eq(self.dp_i ^ dp_i_last),
            dm_i_edge.eq(self.dm_i ^ dm_i_last),
            clear.eq(dp_i_edge | dm_i_edge),
        ]

        m.d.comb += [
            self.dp.eq(self.dp_i),
            self.dm.eq(self.dm_i),
            self.se0.eq(~self.dp_i & ~self.dm_i),
        ]

        return m


class UsbRxDecoder(Elaboratable):

    def __init__(self):
        self.low_speed_port = Signal()

        self.clock_data_in = Signal()
        self.wait_sync = Signal()

        self.d = Signal()
        self.out = Valid(Signal)

    def elaborate(self, platform):
        m = Module()

        state = Signal()

        with m.If(self.clock_data_in):
            m.d.comb += self.out.valid.eq(1)

            with m.If(state ^ self.d ^ self.low_speed_port):
                m.d.comb += self.out.bits.eq(0)
                m.d.sync += state.eq(~state)
            with m.Else():
                m.d.comb += self.out.bits.eq(1)

        with m.If(self.wait_sync):
            m.d.sync += state.eq(0)

        return m


class UsbRxDestuffer(Elaboratable):

    def __init__(self):
        self.wait_sync = Signal()

        self.inp = Valid(Signal)
        self.out = Valid(Signal)

        self.error = Signal()

    def elaborate(self, platform):
        m = Module()

        counter = Signal(3)
        do_unstuff = counter == 6

        with m.If(self.inp.valid):
            m.d.sync += counter.eq(counter + 1)
            with m.If(~self.inp.bits | do_unstuff):
                m.d.sync += counter.eq(0)

            with m.If(do_unstuff & self.inp.bits):
                m.d.comb += self.error.eq(1)

        with m.If(self.wait_sync):
            m.d.sync += counter.eq(0)

        m.d.comb += [
            self.out.valid.eq(self.inp.valid & ~do_unstuff),
            self.out.bits.eq(self.inp.bits),
        ]

        return m


class UsbPhyRx(Elaboratable):

    def __init__(self, fs_divisor):
        self.fs_divisor = fs_divisor

        self.low_speed_port = Signal()

        self.clock_data_in = Signal()
        self.enable = Signal()
        self.tx_valid = Signal()

        self.dp = Signal()
        self.dm = Signal()
        self.se0 = Signal()

        self.rx_active = Signal()
        self.rx_data = Valid(UsbRxData)

    def elaborate(self, platform):
        m = Module()

        is_j = (self.dp ^ self.low_speed_port) & (self.dm
                                                  == self.low_speed_port)
        is_k = (self.dp
                == self.low_speed_port) & (self.dm ^ self.low_speed_port)

        wait_sync = Signal()

        decoder = m.submodules.decoder = UsbRxDecoder()
        m.d.comb += [
            decoder.low_speed_port.eq(self.low_speed_port),
            decoder.clock_data_in.eq(self.clock_data_in),
            decoder.wait_sync.eq(wait_sync),
            decoder.d.eq(self.dp),
        ]

        destuffer = m.submodules.destuffer = UsbRxDestuffer()
        m.d.comb += [
            destuffer.wait_sync.eq(wait_sync),
            destuffer.inp.eq(decoder.out),
        ]

        rx_data = Signal(8)
        sr = Signal(8)
        m.d.comb += rx_data.eq(Cat(sr[1:], destuffer.out.bits))
        with m.If(destuffer.out.valid):
            m.d.sync += sr.eq(rx_data)

        sync_detected = (rx_data == 0b11010101) & destuffer.out.valid

        eop_max_delay = Mux(self.low_speed_port, self.fs_divisor * 24,
                            self.fs_divisor * 3)
        eop_min_delay = Mux(self.low_speed_port, self.fs_divisor * 32 // 3,
                            self.fs_divisor * 4 // 3)
        eop_counter = Signal(range(self.fs_divisor * 24 + 1))
        eop_detected = Signal()

        with m.If(self.se0):
            with m.If(eop_counter < eop_max_delay):
                m.d.sync += eop_counter.eq(eop_counter + 1)
        with m.Else():
            m.d.sync += eop_counter.eq(0)
        with m.If(is_j):
            with m.If((eop_counter >= eop_min_delay)
                      & ~(eop_counter == eop_max_delay)):
                m.d.comb += eop_detected.eq(1)

        counter = Signal(3)
        stuffing_error = Signal()

        with m.FSM():
            with m.State('IDLE'):
                m.d.comb += wait_sync.eq(1)
                m.d.sync += counter.eq(0)

                with m.If(sync_detected):
                    m.next = 'PACKET'

            with m.State('PACKET'):
                m.d.comb += [
                    self.rx_active.eq(1),
                    self.rx_data.bits.data.eq(rx_data),
                    self.rx_data.bits.stuffing_error.eq(stuffing_error),
                ]
                m.d.sync += stuffing_error.eq(destuffer.error)

                with m.If(destuffer.out.valid):
                    m.d.sync += counter.eq(counter + 1)

                    with m.If(counter == 7):
                        m.d.comb += self.rx_data.valid.eq(self.enable)

                        with m.If(stuffing_error):
                            m.next = 'ERROR'

                with m.If(eop_detected | self.tx_valid):
                    m.next = 'IDLE'

            with m.State('ERROR'):
                pass

        return m


class UsbLsFsPhyPort(Elaboratable):

    def __init__(self,
                 clk_freq,
                 fs_divisor,
                 t_DISCONNECT=2000.0,
                 t_CONNECT=2500.0,
                 t_RESET=2500.0):
        self.clk_freq = clk_freq
        self.fs_divisor = fs_divisor
        self.t_DISCONNECT = t_DISCONNECT
        self.t_CONNECT = t_CONNECT
        self.t_RESET = t_RESET

        self.rx_active = Signal()
        self.rx_data = Valid(UsbRxData)
        self.tx = Valid(UsbTxData)

        self.ctrl = UsbCtrlPort()
        self.usb = UsbLsFsPhyIo()

    def elaborate(self, platform):
        m = Module()

        self.usb.dp_o.reset_less = True
        self.usb.dp_t.reset_less = True
        self.usb.dm_o.reset_less = True
        self.usb.dm_t.reset_less = True

        dp_i = Signal()
        dp_o = Signal()
        dp_oe = Signal()
        dm_i = Signal()
        dm_o = Signal()
        dm_oe = Signal()
        se0_i = Signal()

        m.d.sync += [
            dp_i.eq(self.usb.dp_i),
            dm_i.eq(self.usb.dm_i),
            self.usb.dp_o.eq(dp_o),
            self.usb.dp_t.eq(~dp_oe),
            self.usb.dm_o.eq(dm_o),
            self.usb.dm_t.eq(~dm_oe),
        ]
        m.d.comb += se0_i.eq(~dp_i & ~dm_i)

        tx_enable = Signal()
        tx_data = Signal()
        tx_se0 = Signal()
        m.d.comb += [
            dp_oe.eq(tx_enable),
            dm_oe.eq(tx_enable),
            dp_o.eq(~tx_se0 & tx_data),
            dm_o.eq(~tx_se0 & ~tx_data),
        ]

        low_speed_port = Signal()

        filter = m.submodules.filter = UsbRxFilter(clk_freq=self.clk_freq)
        m.d.comb += [
            filter.dp_i.eq(dp_i),
            filter.dm_i.eq(dm_i),
        ]

        rx = m.submodules.rx = UsbPhyRx(fs_divisor=self.fs_divisor)
        m.d.comb += [
            rx.clock_data_in.eq(filter.clock_data_in),
            rx.tx_valid.eq(self.tx.valid),
            rx.dp.eq(filter.dp),
            rx.dm.eq(filter.dm),
            rx.se0.eq(filter.se0),
            self.rx_active.eq(rx.rx_active),
            self.rx_data.eq(rx.rx_data),
        ]

        tck = 1.0 / self.clk_freq * 1e9

        disconnect_cycles = math.ceil(self.t_DISCONNECT / tck)
        connect_cycles = math.ceil(self.t_CONNECT / tck)
        reset_cycles = math.ceil(self.t_RESET / tck)

        wait_counter = Signal(
            range(max(
                disconnect_cycles,
                connect_cycles,
                reset_cycles,
            )))

        with m.FSM():
            with m.State('POWER_OFF'):
                m.d.comb += [
                    tx_enable.eq(1),
                    tx_se0.eq(1),
                ]

                with m.If(self.ctrl.power):
                    m.d.sync += wait_counter.eq(0)
                    m.next = 'DISCONNECTED'

            with m.State('DISCONNECTED'):
                m.d.sync += wait_counter.eq(wait_counter + 1)
                with m.If(~filter.dp & ~filter.dm):
                    m.d.sync += wait_counter.eq(0)

                with m.If(wait_counter == connect_cycles - 1):
                    m.d.comb += self.ctrl.connect_.eq(1)
                    m.next = 'DISABLED'

            with m.State('DISABLED'):
                with m.If(self.ctrl.reset.valid):
                    with m.If(filter.dp ^ filter.dm):
                        m.d.sync += [
                            low_speed_port.eq(~filter.dp),
                            wait_counter.eq(0),
                        ]
                        m.next = 'RESET'

            with m.State('RESET'):
                m.d.comb += [
                    tx_enable.eq(1),
                    tx_se0.eq(1),
                ]

                with m.If(wait_counter == reset_cycles - 1):
                    with m.If(~self.tx.valid):
                        m.d.comb += self.ctrl.reset.ready.eq(1)
                        m.next = 'ENABLED'

                with m.Else():
                    m.d.sync += wait_counter.eq(wait_counter + 1)

            with m.State('ENABLED'):
                m.d.comb += [
                    tx_enable.eq(self.tx.valid),
                    tx_data.eq(self.tx.bits.data),
                    tx_se0.eq(self.tx.bits.se0),
                    rx.enable.eq(1),
                ]

        return m


class UsbLsFsPhy(Elaboratable):

    def __init__(self, clk_freq, port_count):
        self.clk_freq = clk_freq
        self.port_count = port_count

        self.low_speed = Signal()

        self.tx_data = Decoupled(UsbDataStream)
        self.tx_eop = Signal()

        self.rx_active = Signal()
        self.rx_data = Valid(UsbRxData)

        self.clock_posedge = Signal()

        self.usb_reset = Signal()
        self.ctrl = [
            UsbCtrlPort(name=f'ctrl_port{i}') for i in range(port_count)
        ]

        self.usb = [UsbLsFsPhyIo(name=f'usb{i}') for i in range(port_count)]

    def elaborate(self, platform):
        m = Module()

        fs_divisor = int(self.clk_freq // 12e6)
        ls_divisor = fs_divisor * 8
        divisor = Mux(self.low_speed, ls_divisor, fs_divisor)

        clock_state = Signal()
        clock_posedge = Signal()
        clock_count = Signal(range(ls_divisor // 2))
        with m.If(clock_count < (divisor >> 1) - 1):
            m.d.sync += [
                clock_posedge.eq(0),
                clock_count.eq(clock_count + 1),
            ]
        with m.Else():
            m.d.sync += [
                clock_state.eq(~clock_state),
                clock_posedge.eq(~clock_state),
                clock_count.eq(0),
            ]
        m.d.comb += self.clock_posedge.eq(clock_posedge)

        tx = m.submodules.tx = UsbPhyTx()
        m.d.comb += [
            tx.low_speed.eq(self.low_speed),
            tx.clock_posedge.eq(self.clock_posedge),
            self.tx_data.connect(tx.tx_data),
            self.tx_eop.eq(tx.tx_eop),
        ]

        ports = []
        for i, (ctrl, usb) in enumerate(zip(self.ctrl, self.usb)):
            port = UsbLsFsPhyPort(clk_freq=self.clk_freq,
                                  fs_divisor=fs_divisor)
            setattr(m.submodules, f'port{i}', port)
            ports.append(port)

            m.d.comb += [
                ctrl.connect(port.ctrl),
                port.usb.connect(usb),
                port.tx.eq(tx.out),
            ]

        rx_active = Cat(port.rx_active for port in ports)
        m.d.comb += self.rx_active.eq(rx_active.any())
        for i in reversed(range(len(rx_active))):
            with m.If(rx_active[i]):
                m.d.comb += self.rx_data.eq(ports[i].rx_data)

        return m
