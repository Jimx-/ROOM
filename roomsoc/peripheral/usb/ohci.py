from amaranth import *
from amaranth.hdl.rec import Direction
from amaranth.lib.fifo import SyncFIFO
from enum import IntEnum, Enum

from roomsoc.peripheral import Peripheral
from roomsoc.interconnect.axi import AXIStreamInterface, AXIStreamDepacketizer, AXIStreamConverter
from roomsoc.interconnect.stream import Decoupled, Valid, Queue

from .phy import UsbLowSpeedFullSpeedPhy, UsbRxData
from .usb import UsbPid, UsbToken, UsbDataStream, UsbTokenTx, UsbDataTx, UsbDataRx


class OhciReg(IntEnum):
    HCREVISION = 0x00
    HCCONTROL = 0x04
    HCCOMMANDSTATUS = 0x08
    HCINTERRUPTSTATUS = 0x0c
    HCINTERRUPTENABLE = 0x10
    HCINTERRUPTDISABLE = 0x14
    HCHCCA = 0x18
    HCPERIODCURRENTED = 0x1c
    HCCONTROLHEADED = 0x20
    HCCONTROLCURRENTED = 0x24
    HCBULKHEADED = 0x28
    HCBULKCURRENTED = 0x2c
    HCDONEHEAD = 0x30
    HCFMINTERVAL = 0x34
    HCFMREMAINING = 0x38
    HCFMNUMBER = 0x3c
    HCPERIODICSTART = 0x40
    HCLSTHRESHOLD = 0x44


_hccontrol_layout = [
    ('cbsr', 2),
    ('ple', 1),
    ('ie', 1),
    ('cle', 1),
    ('ble', 1),
    ('hcfs', 2),
    ('ir', 1),
    ('rwc', 1),
    ('rwe', 1),
    ('_rsvd0', 21),
]

_hccommandstatus_layout = [
    ('hcr', 1),
    ('clf', 1),
    ('blf', 1),
    ('ocr', 1),
    ('_rsvd0', 12),
    ('soc', 2),
    ('_rsvd1', 14),
]

_hcinterrupt_layout = [
    ('so', 1),
    ('wdh', 1),
    ('sf', 1),
    ('rd', 1),
    ('ue', 1),
    ('fno', 1),
    ('rhsc', 1),
    ('_rsvd0', 23),
    ('oc', 1),
    ('mie', 1),
]

_hcdonehead_layout = [
    ('_rsvd0', 4),
    ('dh', 28),
]

_hcfminterval_layout = [
    ('fi', 14),
    ('_rsvd0', 2),
    ('fsmps', 15),
    ('fit', 1),
]

_hcfmremaining_layout = [
    ('fr', 14),
    ('_rsvd0', 17),
    ('frt', 1),
]

_hcfmnumber_layout = [
    ('fn', 16),
    ('_rsvd0', 16),
]

_hcperiodicstart_layout = [
    ('ps', 14),
    ('_rsvd0', 18),
]


class OhciFunctionalState(IntEnum):
    USB_RESET = 0b00
    USB_RESUME = 0b01
    USB_OPERATIONAL = 0b10
    USB_SUSPEND = 0b11


class OhciFlowType(Enum):
    BULK = 0
    CONTROL = 1
    PERIODIC = 2


class OhciConditionCode(IntEnum):
    NO_ERROR = 0b0000
    CRC = 0b0001
    BIT_STUFFING = 0b0010
    DATA_TOGGLE_MISMATCH = 0b0011
    STALL = 0b0100
    DEVICE_NOT_RESP = 0b0101
    PID_CHECK_FAIL = 0b0110
    UNEXPECTED_PID = 0b0111
    DATA_OVERRUN = 0b1000
    DATA_UNDERRUN = 0b1001
    BUFFER_OVERRUN = 0b1100
    BUFFER_UNDERRUN = 0b1101
    NOT_ACCESSED = 0b1110


_ed_layout = [
    ('fa', 7, Direction.FANOUT),
    ('en', 4, Direction.FANOUT),
    ('d', 2, Direction.FANOUT),
    ('s', 1, Direction.FANOUT),
    ('k', 1, Direction.FANOUT),
    ('f', 1, Direction.FANOUT),
    ('mps', 11, Direction.FANOUT),
    ('_rsvd0', 9, Direction.FANOUT),
    ('tailp', 28, Direction.FANOUT),
    ('h', 1, Direction.FANOUT),
    ('c', 1, Direction.FANOUT),
    ('_rsvd1', 2, Direction.FANOUT),
    ('headp', 28, Direction.FANOUT),
    ('_rsvd2', 4, Direction.FANOUT),
    ('nexted', 28, Direction.FANOUT),
]

_td_layout = [
    ('_rsvd0', 18),
    ('r', 1),
    ('dp', 2),
    ('di', 3),
    ('t', 2),
    ('ec', 2),
    ('cc', 4),
    ('cbp', 32),
    ('_rsvd1', 4),
    ('nexttd', 28),
    ('be', 32),
]


class DirectionPid(IntEnum):
    SETUP = 0
    OUT = 1
    IN = 2


class DataDownConverter(Elaboratable):

    def __init__(self, data_width):
        self.data_width = data_width

        self.sink = AXIStreamInterface(data_width=data_width)
        self.source = Decoupled(UsbDataStream)

    def elaborate(self, platform):
        m = Module()

        source_q = m.submodules.source_q = Queue(2, UsbDataStream, flow=False)
        m.d.comb += source_q.deq.connect(self.source)

        source = source_q.enq

        if self.data_width == 8:
            m.d.comb += [
                source.valid.eq(self.sink.valid),
                source.bits.data.eq(self.sink.bits.data),
                source.bits.last.eq(self.sink.bits.last),
                self.sink.ready.eq(source.ready),
            ]
        else:
            beats = self.data_width // 8

            sel = Signal(range(beats))

            m.d.comb += source.valid.eq(self.sink.valid)
            with m.Switch(sel):
                for i in range(beats):
                    with m.Case(i):
                        m.d.comb += source.bits.data.eq(
                            self.sink.bits.data[i * 8:(i + 1) * 8])

                        if i == beats - 1:
                            m.d.comb += source.bits.last.eq(
                                self.sink.bits.last)
                        else:
                            m.d.comb += source.bits.last.eq(
                                self.sink.bits.last
                                & ~self.sink.bits.keep[i + 1])

            with m.If(source.fire):
                m.d.sync += sel.eq(sel + 1)

                with m.If(source.bits.last | (sel == beats - 1)):
                    m.d.comb += self.sink.ready.eq(1)
                    m.d.sync += sel.eq(0)

        return m


class DataUpConverter(Elaboratable):

    def __init__(self, data_width):
        self.data_width = data_width

        self.rx_done = Signal()

        self.sink = Valid(Signal, 8)
        self.source = AXIStreamInterface(data_width=data_width)

    def elaborate(self, platform):
        m = Module()

        had_data = Signal()
        rx_done_last = Signal()
        rx_done_edge = Signal()
        m.d.sync += rx_done_last.eq(self.rx_done)
        m.d.comb += rx_done_edge.eq(self.rx_done & ~rx_done_last)

        if self.data_width == 8:
            with m.If(rx_done_edge):
                m.d.comb += [
                    self.source.valid.eq(had_data),
                    source.bits.keep.eq(0),
                    source.bits.last.eq(1),
                ]
                m.d.sync += had_data.eq(0)
            with m.Else():
                m.d.comb += [
                    self.source.valid.eq(self.sink.valid),
                    self.source.bits.data.eq(self.sink.bits),
                    self.source.bits.keep.eq(1),
                ]
                m.d.sync += had_data.eq(1)
        else:
            beats = self.data_width // 8
            sel = Signal(range(beats))

            sr_next = Signal(self.data_width)
            sr = Signal(self.data_width)
            m.d.comb += sr_next.eq(sr)
            with m.Switch(sel):
                for i in range(beats):
                    with m.Case(i):
                        m.d.comb += sr_next[i * 8:(i + 1) * 8].eq(
                            self.sink.bits)
            with m.If(self.sink.valid):
                m.d.sync += sr.eq(sr_next)

            with m.If(self.sink.valid):
                m.d.comb += [
                    self.source.bits.data.eq(sr_next),
                    self.source.bits.keep.eq(~0),
                ]
                m.d.sync += [
                    sel.eq(sel + 1),
                    had_data.eq(1),
                ]

                with m.If(sel == (beats - 1)):
                    m.d.comb += self.source.valid.eq(1)
                    m.d.sync += sel.eq(0)

            with m.If(rx_done_edge):
                with m.Switch(sel):
                    for i in range(beats):
                        with m.Case(i):
                            m.d.comb += self.source.bits.keep.eq((1 << i) - 1)

                m.d.comb += [
                    self.source.bits.data.eq(sr),
                    self.source.valid.eq(had_data),
                    self.source.bits.last.eq(1),
                ]

                m.d.sync += had_data.eq(0)

        return m


class OhciEndpointHandler(Elaboratable):

    def __init__(self, data_width, fifo_depth):
        self.data_width = data_width
        self.fifo_depth = fifo_depth

        self.flow_type = Signal(OhciFlowType)
        self.ed_addr = Signal(32)
        self.next_ed = Valid(Signal, 32)
        self.packet_time = Signal(15)
        self.frame_time_hit = Signal()
        self.ls_threshold_hit = Signal(12)

        self.start = Signal()
        self.done = Signal()

        self.fill_list = Signal()
        self.load_intr_delay = Valid(Signal, 3)

        self.done_head = Signal(32)
        self.clear_done_head = Signal()

        self.tx_token = Decoupled(UsbToken)
        self.tx_req = Decoupled(UsbDataTx.Request)
        self.tx_data = Decoupled(UsbDataStream)

        self.rx_active = Signal()
        self.rx_data = Valid(UsbRxData)

        self.mem_read_cmd = Decoupled(OhciController.MemoryCommand)
        self.mem_read_data = AXIStreamInterface(data_width=self.data_width)

        self.mem_write_cmd = Decoupled(OhciController.MemoryCommand)
        self.mem_write_data = AXIStreamInterface(data_width=self.data_width)
        self.mem_write_done = Signal()

    def elaborate(self, platform):
        m = Module()

        data_rx = m.submodules.data_rx = UsbDataRx()
        m.d.comb += [
            data_rx.rx_data.bits.eq(self.rx_data.bits.data),
            data_rx.rx_data.valid.eq(self.rx_data.valid),
            data_rx.rx_active.eq(self.rx_active),
            data_rx.rx_stuffing.eq(self.rx_data.valid
                                   & self.rx_data.bits.stuffing_error),
        ]

        rx_fifo_flush = Signal()
        rx_fifo = m.submodules.rx_fifo = ResetInserter(rx_fifo_flush)(SyncFIFO(
            depth=self.fifo_depth // 2,
            width=self.data_width + self.data_width // 8 + 1))
        up_converter = m.submodules.up_converter = DataUpConverter(
            self.data_width)
        m.d.comb += [
            up_converter.rx_done.eq(data_rx.done),
            up_converter.sink.eq(data_rx.data),
        ]
        m.d.comb += [
            rx_fifo.w_data.eq(
                Cat(
                    up_converter.source.bits.data,
                    up_converter.source.bits.keep,
                    up_converter.source.bits.last,
                )),
            rx_fifo.w_en.eq(up_converter.source.valid),
            Cat(
                self.mem_write_data.bits.data,
                self.mem_write_data.bits.keep,
                self.mem_write_data.bits.last,
            ).eq(rx_fifo.r_data),
            self.mem_write_data.valid.eq(rx_fifo.r_rdy),
            rx_fifo.r_en.eq(self.mem_write_data.ready),
        ]

        tx_fifo = m.submodules.tx_fifo = SyncFIFO(depth=self.fifo_depth // 2,
                                                  width=self.data_width +
                                                  self.data_width // 8 + 1)
        down_converter = m.submodules.down_converter = DataDownConverter(
            self.data_width)
        m.d.comb += [
            tx_fifo.w_data.eq(
                Cat(
                    self.mem_read_data.bits.data,
                    self.mem_read_data.bits.keep,
                    self.mem_read_data.bits.last,
                )),
            tx_fifo.w_en.eq(self.mem_read_data.valid),
            self.mem_read_data.ready.eq(tx_fifo.w_rdy),
            Cat(
                down_converter.sink.bits.data,
                down_converter.sink.bits.keep,
                down_converter.sink.bits.last,
            ).eq(tx_fifo.r_data),
            down_converter.sink.valid.eq(tx_fifo.r_rdy),
            tx_fifo.r_en.eq(down_converter.sink.ready),
        ]

        ed_depacketizer = m.submodules.ed_depacketizer = AXIStreamDepacketizer(
            Record, _ed_layout, data_width=self.data_width)
        ed = Record(_ed_layout)
        m.d.comb += self.next_ed.bits.eq(ed.nexted)

        td_depacketizer = m.submodules.td_depacketizer = AXIStreamDepacketizer(
            Record, _td_layout, data_width=self.data_width)
        td = Record(_td_layout)
        td_write = Record(_td_layout)
        m.d.comb += td_write.eq(td)

        td_converter = m.submodules.td_converter = AXIStreamConverter(
            dw_from=len(td), dw_to=self.data_width)
        m.d.comb += [
            td_converter.sink.bits.data.eq(td_write),
            td_converter.sink.bits.keep.eq(~0),
            td_converter.sink.bits.last.eq(1),
        ]

        td_addr = ed.headp << 4

        token_type = Mux(ed.d[0] ^ ed.d[1], ed.d, td.dp)
        is_in = token_type == DirectionPid.IN

        buf_addr = td.cbp
        last_addr = td.be
        txn_len = last_addr - buf_addr + 1
        zero_len = td.cbp == 0
        data_phase = Mux(td.t[1], td.t[0], ed.c)
        td_retire = Signal()
        update_data_phase = Signal()
        skip_td_update = Signal()

        fs_time_check = Mux(zero_len, self.packet_time == 0, (txn_len << 3)
                            >= self.packet_time)
        time_check = (~ed.s & fs_time_check) | (ed.s & self.ls_threshold_hit)

        rx_pid_ok = self.rx_data.bits.data[:4] == ~self.rx_data.bits.data[4:]
        ack_rx_fired = Signal()
        ack_rx_pid = Signal(4)
        ack_rx_activated = Signal()
        ack_rx_stuffing_error = Signal()
        ack_rx_pid_error = Signal()

        done_head = Record(_hcdonehead_layout)
        m.d.comb += self.done_head.eq(done_head)
        with m.If(self.clear_done_head):
            m.d.sync += done_head.eq(0)

        with m.FSM():
            with m.State('IDLE'):
                m.d.comb += self.done.eq(1)

                with m.If(self.start):
                    m.d.sync += self.frame_time_hit.eq(0)
                    m.next = 'ED_READ_CMD'

            with m.State('ED_READ_CMD'):
                m.d.comb += [
                    self.mem_read_cmd.valid.eq(1),
                    self.mem_read_cmd.bits.addr.eq(self.ed_addr),
                    self.mem_read_cmd.bits.len.eq(len(ed) // 8),
                    tx_fifo.w_en.eq(0),
                ]

                with m.If(self.mem_read_cmd.fire):
                    m.next = 'ED_READ_RESP'

            with m.State('ED_READ_RESP'):
                m.d.comb += [
                    self.mem_read_data.connect(ed_depacketizer.sink),
                    tx_fifo.w_en.eq(0),
                ]

                with m.If(ed_depacketizer.source.valid):
                    m.d.comb += ed_depacketizer.source.ready.eq(1)
                    m.d.sync += ed.eq(ed_depacketizer.header)

                    m.next = 'ED_CHECK'

            with m.State('ED_CHECK'):
                with m.If(ed.h | ed.k | (ed.tailp == ed.headp)):
                    m.d.comb += self.next_ed.valid.eq(1)

                    m.next = 'IDLE'
                with m.Else():
                    m.next = 'TD_READ_CMD'

            with m.State('TD_READ_CMD'):
                m.d.comb += [
                    self.mem_read_cmd.valid.eq(1),
                    self.mem_read_cmd.bits.addr.eq(td_addr),
                    self.mem_read_cmd.bits.len.eq(len(td) // 8),
                    tx_fifo.w_en.eq(0),
                ]

                m.d.sync += [
                    td_retire.eq(0),
                    update_data_phase.eq(0),
                    skip_td_update.eq(0),
                ]

                with m.If(self.mem_read_cmd.fire):
                    m.next = 'TD_READ_RESP'

            with m.State('TD_READ_RESP'):
                m.d.comb += [
                    self.mem_read_data.connect(td_depacketizer.sink),
                    tx_fifo.w_en.eq(0),
                ]

                with m.If(td_depacketizer.source.valid):
                    m.d.comb += td_depacketizer.source.ready.eq(1)
                    m.d.sync += td.eq(td_depacketizer.header)

                    m.next = 'TD_CHECK'

            with m.State('TD_CHECK'):
                m.d.comb += self.fill_list.eq(1)

                m.next = 'TD_CHECK_TIME'

            with m.State('TD_CHECK_TIME'):
                with m.If(time_check):
                    m.d.sync += self.frame_time_hit.eq(1)
                    m.next = 'IDLE'
                with m.Else():
                    with m.If(is_in | zero_len):
                        m.next = 'TOKEN'
                    with m.Else():
                        m.next = 'BUF_READ_CMD'

            with m.State('BUF_READ_CMD'):
                m.d.comb += [
                    self.mem_read_cmd.valid.eq(1),
                    self.mem_read_cmd.bits.addr.eq(buf_addr),
                    self.mem_read_cmd.bits.len.eq(txn_len),
                ]

                with m.If(self.mem_read_cmd.fire):
                    m.next = 'TOKEN'

            with m.State('TOKEN'):
                m.d.comb += [
                    self.tx_token.valid.eq(1),
                    self.tx_token.bits.data.eq(Cat(ed.fa, ed.en)),
                ]

                with m.Switch(token_type):
                    with m.Case(DirectionPid.SETUP):
                        m.d.comb += self.tx_token.bits.pid.eq(UsbPid.SETUP)
                    with m.Case(DirectionPid.OUT):
                        m.d.comb += self.tx_token.bits.pid.eq(UsbPid.OUT)
                    with m.Case(DirectionPid.IN):
                        m.d.comb += self.tx_token.bits.pid.eq(UsbPid.IN)

                with m.If(self.tx_token.fire):
                    with m.If(is_in):
                        m.next = 'DATA_RX'
                    with m.Else():
                        m.next = 'DATA_TX'

            with m.State('DATA_RX'):
                m.d.comb += data_rx.start.eq(1)
                m.next = 'DATA_RX_CMD'

            with m.State('DATA_RX_CMD'):
                m.d.comb += [
                    self.mem_write_cmd.valid.eq(1),
                    self.mem_write_cmd.bits.addr.eq(buf_addr),
                    self.mem_write_cmd.bits.len.eq(txn_len),
                ]

                with m.If(self.mem_write_cmd.fire):
                    m.next = 'DATA_RX_WAIT'

            with m.State('DATA_RX_WAIT'):
                with m.If(data_rx.done):
                    m.next = 'DATA_RX_VALIDATE'

            with m.State('DATA_RX_VALIDATE'):
                m.next = 'DATA_RX_ABORT'

                pid_mismatched = data_rx.pid == Mux(data_phase, UsbPid.DATA0,
                                                    UsbPid.DATA1)
                pid_ok = ((data_rx.pid == UsbPid.DATA0) |
                          (data_rx.pid == UsbPid.DATA1)) & ~pid_mismatched

                m.d.sync += td.cc.eq(OhciConditionCode.NO_ERROR)
                with m.If(data_rx.stuffing_error):
                    m.d.sync += td.cc.eq(OhciConditionCode.BIT_STUFFING)
                with m.Elif(data_rx.pid_error):
                    m.d.sync += td.cc.eq(OhciConditionCode.PID_CHECK_FAIL)
                with m.Else():
                    with m.Switch(data_rx.pid):
                        with m.Case(UsbPid.NAK):
                            m.d.sync += skip_td_update.eq(1)
                        with m.Case(UsbPid.STALL):
                            m.d.sync += td.cc.eq(OhciConditionCode.STALL)
                        with m.Case(UsbPid.DATA0, UsbPid.DATA1):
                            with m.If(pid_mismatched):
                                m.d.sync += td.cc.eq(
                                    OhciConditionCode.DATA_TOGGLE_MISMATCH)
                            m.next = 'ACK_TX0'
                        with m.Default():
                            m.d.sync += td.cc.eq(
                                OhciConditionCode.UNEXPECTED_PID)

                with m.If(pid_ok):
                    with m.If(data_rx.crc_error):
                        m.d.sync += td.cc.eq(OhciConditionCode.CRC)

            with m.State('DATA_RX_ABORT'):
                m.d.comb += [
                    self.mem_write_data.valid.eq(1),
                    self.mem_write_data.bits.keep.eq(0),
                    self.mem_write_data.bits.last.eq(1),
                ]

                with m.If(self.mem_write_data.fire):
                    m.next = 'DATA_RX_WAIT_DMA'

            with m.State('ACK_TX0'):
                m.next = 'ACK_TX1'

            with m.State('ACK_TX1'):
                m.d.comb += [
                    self.tx_token.valid.eq(1),
                    self.tx_token.bits.pid.eq(UsbPid.ACK),
                ]

                with m.If(self.tx_token.fire):
                    m.next = 'DATA_RX_WAIT_DMA'

            with m.State('DATA_RX_WAIT_DMA'):
                with m.If(self.mem_write_done):
                    m.d.comb += rx_fifo_flush.eq(1)
                    m.next = 'UPDATE_TD'

            with m.State('DATA_TX'):
                m.d.comb += [
                    self.tx_req.valid.eq(1),
                    self.tx_req.bits.pid.eq(
                        Mux(data_phase, UsbPid.DATA1, UsbPid.DATA0)),
                    self.tx_req.bits.zero_len.eq(zero_len),
                    down_converter.source.connect(self.tx_data),
                ]

                with m.If(self.tx_req.fire):
                    m.d.sync += [
                        ack_rx_fired.eq(0),
                        ack_rx_activated.eq(0),
                        ack_rx_stuffing_error.eq(0),
                        ack_rx_pid_error.eq(0),
                    ]
                    m.next = 'ACK_RX'

            with m.State('ACK_RX'):

                with m.If(self.rx_data.valid):
                    m.d.sync += [
                        ack_rx_fired.eq(1),
                        ack_rx_pid.eq(self.rx_data.bits[:4]),
                        ack_rx_stuffing_error.eq(
                            self.rx_data.bits.stuffing_error),
                    ]

                    with m.If(ack_rx_fired | ~rx_pid_ok):
                        m.d.sync += ack_rx_pid_error.eq(1)

                with m.If(self.rx_active):
                    m.d.sync += ack_rx_activated.eq(1)

                with m.If(~self.rx_active & ack_rx_activated):
                    m.next = 'UPDATE_TD'

                    with m.If(~ack_rx_fired):
                        m.d.sync += td.cc.eq(OhciConditionCode.PID_CHECK_FAIL)
                    with m.Elif(ack_rx_stuffing_error):
                        m.d.sync += td.cc.eq(OhciConditionCode.BIT_STUFFING)
                    with m.Elif(ack_rx_pid_error):
                        m.d.sync += td.cc.eq(OhciConditionCode.PID_CHECK_FAIL)
                    with m.Else():
                        with m.Switch(ack_rx_pid):
                            with m.Case(UsbPid.ACK):
                                m.d.sync += td.cc.eq(
                                    OhciConditionCode.NO_ERROR)
                            with m.Case(UsbPid.NAK):
                                m.next = 'FINISH'
                            with m.Case(UsbPid.STALL):
                                m.d.sync += td.cc.eq(OhciConditionCode.STALL)
                            with m.Default():
                                m.d.sync += td.cc.eq(
                                    OhciConditionCode.UNEXPECTED_PID)

            with m.State('UPDATE_TD'):
                m.next = 'UPDATE_TD_CMD'

                m.d.sync += td.ec.eq(0)
                with m.Switch(td.cc):
                    with m.Case(OhciConditionCode.NO_ERROR):
                        m.d.sync += [
                            td_retire.eq(1),
                            update_data_phase.eq(1),
                        ]

                with m.If(skip_td_update):
                    m.d.sync += td_retire.eq(0)
                    m.next = 'FINISH'

            with m.State('UPDATE_TD_CMD'):
                m.d.comb += [
                    self.mem_write_cmd.valid.eq(1),
                    self.mem_write_cmd.bits.addr.eq(td_addr),
                    self.mem_write_cmd.bits.len.eq(len(td) // 8),
                ]

                with m.If(self.mem_write_cmd.fire):
                    m.next = 'UPDATE_TD_DATA'

            with m.State('UPDATE_TD_DATA'):
                m.d.comb += [
                    td_write.nexttd.eq(done_head.dh),
                    td_converter.sink.valid.eq(1),
                    td_converter.source.connect(self.mem_write_data),
                ]

                with m.If(td_converter.sink.fire):
                    m.next = 'UPDATE_ED_CMD'

            with m.State('UPDATE_ED_CMD'):
                m.d.comb += [
                    self.mem_write_cmd.valid.eq(1),
                    self.mem_write_cmd.bits.addr.eq(self.ed_addr + 8),
                    self.mem_write_cmd.bits.len.eq(4),
                ]

                with m.If(self.mem_write_cmd.fire):
                    m.next = 'UPDATE_ED_DATA'

            with m.State('UPDATE_ED_DATA'):
                m.d.comb += [
                    self.mem_write_data.valid.eq(1),
                    self.mem_write_data.bits.data.eq(
                        Cat(
                            ed.h,
                            data_phase ^ update_data_phase,
                            Const(0, 2),
                            td.nexttd,
                        )),
                    self.mem_write_data.bits.keep.eq(0xf),
                    self.mem_write_data.bits.last.eq(1),
                ]

                with m.If(self.mem_write_data.fire):
                    m.next = 'FINISH'

            with m.State('FINISH'):
                m.d.comb += self.next_ed.valid.eq(1)

                with m.If(td_retire):
                    m.d.comb += [
                        self.load_intr_delay.valid.eq(1),
                        self.load_intr_delay.bits.eq(td.di),
                    ]

                    m.d.sync += done_head.dh.eq(ed.headp)

                m.next = 'IDLE'

        return m


class OhciPriorityCounter(Elaboratable):

    def __init__(self):
        self.cbsr = Signal(2)
        self.tick = Signal()
        self.skip = Signal()
        self.flush = Signal()

        self.bulk = Signal()

    def elaborate(self, platform):
        m = Module()

        counter = Signal(2)
        with m.If(self.tick):
            m.d.sync += counter.eq(counter + 1)

        with m.If(self.skip
                  | (self.tick & (self.bulk | (counter == self.cbsr)))):
            m.d.sync += [
                self.bulk.eq(~self.bulk),
                counter.eq(0),
            ]

        with m.If(self.flush):
            m.d.sync += [
                self.bulk.eq(0),
                counter.eq(0),
            ]

        return m


class InterruptDelayCounter(Elaboratable):

    def __init__(self):
        self.tick = Signal()
        self.disable = Signal()
        self.done = Signal()

        self.load = Valid(Signal, 3)

    def elaborate(self, platform):
        m = Module()

        counter = Signal(3, reset=0b111)

        with m.If(self.tick & counter.any() & ~counter.all()):
            m.d.sync += counter.eq(counter - 1)

        with m.If(self.load.valid & (self.load.bits < counter)):
            m.d.sync += counter.eq(self.load.bits)

        with m.If(self.disable):
            m.d.sync += counter.eq(0b111)

        return m


class OhciController(Peripheral, Elaboratable):

    class MemoryCommand(Record):

        def __init__(self, name=None, src_loc_at=0):
            super().__init__([
                ('addr', 32, Direction.FANOUT),
                ('len', 32, Direction.FANOUT),
            ],
                             name=name,
                             src_loc_at=1 + src_loc_at)

    def __init__(self, data_width, clk_freq, *, name=None, fifo_depth=16):
        super().__init__(name=name)

        self.clk_freq = clk_freq
        self.data_width = data_width
        self.fifo_depth = fifo_depth

        self.mem_read_cmd = Decoupled(OhciController.MemoryCommand)
        self.mem_read_data = AXIStreamInterface(data_width=data_width)

        self.mem_write_cmd = Decoupled(OhciController.MemoryCommand)
        self.mem_write_data = AXIStreamInterface(data_width=data_width)
        self.mem_write_done = Signal()

        self.dp_i = Signal()
        self.dp_o = Signal()
        self.dp_t = Signal()
        self.dm_i = Signal()
        self.dm_o = Signal()
        self.dm_t = Signal()

        bank = self.csr_bank()
        self._hcrevision = bank.csr(32, 'r', addr=OhciReg.HCREVISION)
        self._hccontrol = bank.csr(32, 'rw', addr=OhciReg.HCCONTROL)
        self._hccommandstatus = bank.csr(32,
                                         'rw',
                                         addr=OhciReg.HCCOMMANDSTATUS)
        self._hcinterruptstatus = bank.csr(32,
                                           'rw',
                                           addr=OhciReg.HCINTERRUPTSTATUS)
        self._hcinterruptenable = bank.csr(32,
                                           'rw',
                                           addr=OhciReg.HCINTERRUPTENABLE)
        self._hcinterruptdisable = bank.csr(32,
                                            'rw',
                                            addr=OhciReg.HCINTERRUPTDISABLE)
        self._hchcca = bank.csr(32, 'rw', addr=OhciReg.HCHCCA)
        self._hcperiodcurrented = bank.csr(32,
                                           'rw',
                                           addr=OhciReg.HCPERIODCURRENTED)
        self._hccontrolheaded = bank.csr(32,
                                         'rw',
                                         addr=OhciReg.HCCONTROLHEADED)
        self._hccontrolcurrented = bank.csr(32,
                                            'rw',
                                            addr=OhciReg.HCCONTROLCURRENTED)
        self._hcbulkheaded = bank.csr(32, 'rw', addr=OhciReg.HCBULKHEADED)
        self._hcbulkcurrented = bank.csr(32,
                                         'rw',
                                         addr=OhciReg.HCBULKCURRENTED)
        self._hcdonehead = bank.csr(32, 'rw', addr=OhciReg.HCDONEHEAD)
        self._hcfminterval = bank.csr(32, 'rw', addr=OhciReg.HCFMINTERVAL)
        self._hcfmremaining = bank.csr(32, 'r', addr=OhciReg.HCFMREMAINING)
        self._hcfmnumber = bank.csr(32, 'r', addr=OhciReg.HCFMNUMBER)
        self._hcperiodicstart = bank.csr(32,
                                         'rw',
                                         addr=OhciReg.HCPERIODICSTART)
        self._hclsthreshold = bank.csr(32, 'rw', addr=OhciReg.HCLSTHRESHOLD)

        self._bridge = self.bridge(data_width=32, granularity=8, alignment=2)
        self.bus = self._bridge.bus

        self.irq = Signal()

        self._hchcca.r_data.reset = 0x100
        self._hccontrolheaded.r_data.reset = 0x10

    def elaborate(self, platform):
        m = Module()
        m.submodules.bridge = self._bridge

        phy = m.submodules.phy = UsbLowSpeedFullSpeedPhy(
            clk_freq=self.clk_freq)
        m.d.comb += [
            phy.dp_i.eq(self.dp_i),
            self.dp_o.eq(phy.dp_o),
            self.dp_t.eq(phy.dp_t),
            phy.dm_i.eq(self.dm_i),
            self.dm_o.eq(phy.dm_o),
            self.dm_t.eq(phy.dm_t),
        ]

        token_tx = m.submodules.token_tx = UsbTokenTx()
        data_tx = m.submodules.data_tx = UsbDataTx()

        m.d.comb += self._hcrevision.r_data.eq(0x110)

        hccontrol = Record(_hccontrol_layout)
        m.d.comb += self._hccontrol.r_data.eq(hccontrol)
        usb_operational = hccontrol.hcfs == OhciFunctionalState.USB_OPERATIONAL

        hccontrol.ple.reset = 1
        hccontrol.ble.reset = 1
        hccontrol.cle.reset = 1

        hccommandstatus = Record(_hccommandstatus_layout)
        m.d.comb += self._hccommandstatus.r_data.eq(hccommandstatus)

        hccommandstatus.blf.reset = 1
        hccommandstatus.clf.reset = 1

        irq_status = Record(_hcinterrupt_layout)
        irq_enable = Record(_hcinterrupt_layout)
        m.d.comb += [
            self._hcinterruptstatus.r_data.eq(irq_status),
            self._hcinterruptenable.r_data.eq(irq_enable),
            self._hcinterruptdisable.r_data.eq(irq_enable),
        ]
        with m.If(self._hcinterruptstatus.w_stb):
            m.d.sync += irq_status.eq(irq_status
                                      & ~self._hcinterruptstatus.w_data)
        with m.If(self._hcinterruptenable.w_stb):
            m.d.sync += [
                irq_enable.eq(self._hcinterruptenable.w_data),
                irq_enable._rsvd0.eq(0),
            ]
        with m.If(self._hcinterruptdisable.w_stb):
            m.d.sync += irq_enable.eq(irq_enable
                                      & ~self._hcinterruptdisable.w_data)

        irq_pending = irq_status & irq_enable
        do_irq = irq_pending.any() & irq_enable.mie
        m.d.comb += self.irq.eq(do_irq & ~hccontrol.ir)

        with m.If(self._hchcca.w_stb):
            m.d.sync += self._hchcca.r_data.eq(
                Cat(Const(0, 8), self._hchcca.w_data[8:]))

        hcfminterval = Record(_hcfminterval_layout)
        hcfmremaining = Record(_hcfmremaining_layout)
        hcfmnumber = Record(_hcfmnumber_layout)
        hcfminterval.fi.reset = 0x2edf
        m.d.comb += [
            self._hcfminterval.r_data.eq(hcfminterval),
            self._hcfmremaining.r_data.eq(hcfmremaining),
            self._hcfmnumber.r_data.eq(hcfmnumber),
        ]
        with m.If(self._hcfminterval.w_stb):
            m.d.sync += hcfminterval.eq(self._hcfminterval.w_data)

        hcfminterval.fsmps.reset = 0x2778  # TODO: Remove

        decrement_counter = Signal(3)
        skip_decrement = decrement_counter == 6
        m.d.sync += decrement_counter.eq(decrement_counter + 1)
        with m.If(skip_decrement):
            m.d.sync += decrement_counter.eq(0)

        frame_start = Signal()
        packet_counter = Signal(15)
        fmnumber_overflow = Signal()
        fmnumberp1 = Signal.like(hcfmnumber.fn)
        m.d.comb += fmnumberp1.eq(hcfmnumber.fn + 1)
        with m.If(usb_operational & phy.clock_posedge):
            m.d.sync += hcfmremaining.fr.eq(hcfmremaining.fr - 1)

            with m.If(packet_counter.any() & ~skip_decrement):
                m.d.sync += packet_counter.eq(packet_counter - 1)

            with m.If(hcfmremaining.fr == 0):
                m.d.comb += frame_start.eq(1)
                m.d.sync += [
                    hcfmremaining.fr.eq(hcfminterval.fi),
                    hcfmremaining.frt.eq(hcfminterval.fit),
                    hcfmnumber.fn.eq(hcfmnumber.fn + 1),
                    fmnumber_overflow.eq(fmnumberp1[-1] ^ hcfmnumber.fn[-1]),
                    packet_counter.eq(hcfminterval.fsmps),
                    decrement_counter.eq(0),
                ]

        priority = m.submodules.priority = OhciPriorityCounter()
        m.d.comb += priority.cbsr.eq(hccontrol.cbsr)

        intr_delay = m.submodules.intr_delay = InterruptDelayCounter()

        hccontrolheaded = self._hccontrolheaded.r_data
        hccontrolcurrented = self._hccontrolcurrented.r_data
        hcbulkheaded = self._hcbulkheaded.r_data
        hcbulkcurrented = self._hcbulkcurrented.r_data
        hcperiodcurrented = self._hcperiodcurrented.r_data

        with m.If(self._hccontrolheaded.w_stb):
            m.d.sync += hccontrolheaded.eq(
                Cat(Const(0, 4), self._hccontrolheaded.w_data[4:]))
        with m.If(self._hcbulkheaded.w_stb):
            m.d.sync += hcbulkheaded.eq(
                Cat(Const(0, 4), self._hcbulkheaded.w_data[4:]))

        ls_threshold = Signal(12, reset=0x628)
        m.d.comb += self._hclsthreshold.r_data.eq(ls_threshold)
        with m.If(self._hclsthreshold.w_stb):
            m.d.sync += ls_threshold.eq(self._hclsthreshold.w_data)

        ed_handler = m.submodules.ed_handler = OhciEndpointHandler(
            data_width=self.data_width, fifo_depth=self.fifo_depth)
        m.d.comb += [
            ed_handler.packet_time.eq(packet_counter),
            ed_handler.rx_active.eq(phy.rx_active),
            ed_handler.rx_data.eq(phy.rx_data),
            ed_handler.ls_threshold_hit.eq(hcfmremaining.fr < ls_threshold),
            intr_delay.load.eq(ed_handler.load_intr_delay),
            self._hcdonehead.r_data.eq(ed_handler.done_head),
        ]

        with m.If(ed_handler.next_ed.valid):
            with m.Switch(ed_handler.flow_type):
                with m.Case(OhciFlowType.BULK):
                    m.d.sync += hcbulkcurrented.eq(ed_handler.next_ed.bits)
                with m.Case(OhciFlowType.CONTROL):
                    m.d.sync += hccontrolcurrented.eq(ed_handler.next_ed.bits)
                with m.Case(OhciFlowType.PERIODIC):
                    m.d.sync += hcperiodcurrented.eq(ed_handler.next_ed.bits)

        with m.If(ed_handler.fill_list):
            with m.Switch(ed_handler.flow_type):
                with m.Case(OhciFlowType.BULK):
                    m.d.sync += hccommandstatus.blf.eq(1)
                with m.Case(OhciFlowType.CONTROL):
                    m.d.sync += hccommandstatus.clf.eq(1)

        fmnumber_converter = m.submodules.fmnumber_converter = AXIStreamConverter(
            dw_from=64, dw_to=self.data_width)

        hcperiodicstart = Record(_hcperiodicstart_layout)
        m.d.comb += self._hcperiodicstart.r_data.eq(hcperiodicstart)
        hcperiodicstart.ps.reset = 0x2edf  # TODO: Remove
        with m.If(self._hcperiodicstart.w_stb):
            m.d.sync += hcperiodicstart.eq(self._hcperiodicstart.w_data)

        reset_counter = Signal(range(481))

        allow_bulk = Signal()
        allow_control = Signal()
        allow_periodic = Signal()
        allow_isochronous = Signal()

        periodic_fetched = Signal()
        periodic_done = Signal()

        with m.FSM():
            with m.State('RESET'):
                m.d.comb += hccontrol.hcfs.eq(OhciFunctionalState.USB_RESET)

                m.d.sync += [
                    allow_periodic.eq(0),
                    reset_counter.eq(reset_counter + 1),
                ]

                with m.If(reset_counter == 480):
                    m.next = 'WAIT_SOF'

            with m.State('WAIT_SOF'):
                m.d.comb += hccontrol.hcfs.eq(
                    OhciFunctionalState.USB_OPERATIONAL)

                with m.If(frame_start):
                    m.next = 'SOF_TOKEN'

            with m.State('SOF_TOKEN'):
                write_done_head = Signal()

                m.d.comb += [
                    hccontrol.hcfs.eq(OhciFunctionalState.USB_OPERATIONAL),
                    token_tx.token.bits.pid.eq(UsbPid.SOF),
                    token_tx.token.bits.data.eq(self._hcfmnumber.r_data),
                    token_tx.token.valid.eq(1),
                    token_tx.tx_data.connect(phy.tx_data),
                    token_tx.tx_eop.eq(phy.tx_eop),
                ]
                m.d.sync += write_done_head.eq(intr_delay.done
                                               & ~irq_status.wdh)

                with m.If(token_tx.token.fire):
                    m.next = 'WRITE_FM_NUMBER_CMD'

            with m.State('WRITE_FM_NUMBER_CMD'):
                m.d.comb += hccontrol.hcfs.eq(
                    OhciFunctionalState.USB_OPERATIONAL)

                m.d.comb += [
                    self.mem_write_cmd.valid.eq(1),
                    self.mem_write_cmd.bits.addr.eq(self._hchcca.r_data
                                                    | 0x80),
                    self.mem_write_cmd.bits.len.eq(8),
                ]

                with m.If(self.mem_write_cmd.fire):
                    m.next = 'WRITE_FM_NUMBER_DATA'

            with m.State('WRITE_FM_NUMBER_DATA'):
                m.d.comb += hccontrol.hcfs.eq(
                    OhciFunctionalState.USB_OPERATIONAL)

                m.d.comb += [
                    fmnumber_converter.sink.valid.eq(1),
                    fmnumber_converter.sink.bits.data[:32].eq(
                        Cat(hcfmnumber.fn, Const(0, 16))),
                    fmnumber_converter.sink.bits.keep[:4].eq(~0),
                    fmnumber_converter.sink.bits.last.eq(1),
                    fmnumber_converter.source.connect(self.mem_write_data),
                ]

                with m.If(write_done_head):
                    m.d.comb += [
                        fmnumber_converter.sink.bits.data[32:].eq(
                            Cat(irq_pending.any(),
                                self._hcdonehead.r_data[1:])),
                        fmnumber_converter.sink.bits.keep[4:].eq(~0),
                    ]

                with m.If(fmnumber_converter.sink.fire):
                    m.d.sync += [
                        irq_status.sf.eq(1),
                        irq_status.fno.eq(fmnumber_overflow),
                        fmnumber_overflow.eq(0),
                    ]
                    m.d.comb += [
                        priority.flush.eq(1),
                        intr_delay.tick.eq(1),
                    ]

                    with m.If(write_done_head):
                        m.d.sync += irq_status.wdh.eq(1)
                        m.d.comb += [
                            ed_handler.clear_done_head.eq(1),
                            intr_delay.disable.eq(1),
                        ]

                    m.d.sync += [
                        allow_bulk.eq(hccontrol.ble),
                        allow_control.eq(hccontrol.cle),
                        allow_periodic.eq(hccontrol.ple),
                        allow_isochronous.eq(hccontrol.ie),
                        #
                        periodic_fetched.eq(0),
                        periodic_done.eq(0),
                    ]

                    m.next = 'ARBITER'

            with m.State('ARBITER'):
                m.d.comb += hccontrol.hcfs.eq(
                    OhciFunctionalState.USB_OPERATIONAL)

                with m.If(hccontrol.ble):
                    m.d.sync += allow_bulk.eq(1)
                with m.If(hccontrol.cle):
                    m.d.sync += allow_control.eq(1)

                with m.If(packet_counter == 0):
                    m.next = 'WAIT_SOF'
                with m.Elif(allow_periodic & ~periodic_done
                            & (hcfmremaining.fr <= hcperiodicstart.ps)):
                    with m.If(~periodic_fetched):
                        m.next = 'PERIODIC_HEAD_CMD'
                    with m.Else():
                        with m.If(hcperiodcurrented == 0):
                            m.d.sync += periodic_done.eq(1)
                        with m.Else():
                            m.d.sync += [
                                ed_handler.flow_type.eq(OhciFlowType.PERIODIC),
                                ed_handler.ed_addr.eq(hcperiodcurrented),
                            ]
                            m.d.comb += ed_handler.start.eq(1)
                            m.next = 'ENDPOINT'

                with m.Else():
                    m.d.comb += priority.skip.eq(1)

                    with m.If(priority.bulk):
                        with m.If(allow_bulk):
                            with m.If(hcbulkcurrented == 0):
                                with m.If(hccommandstatus.blf):
                                    m.d.sync += [
                                        hcbulkcurrented.eq(hcbulkheaded),
                                        hccommandstatus.blf.eq(0),
                                    ]
                                    m.d.comb += priority.skip.eq(0)

                            with m.Else():
                                m.d.sync += [
                                    ed_handler.flow_type.eq(OhciFlowType.BULK),
                                    ed_handler.ed_addr.eq(hcbulkcurrented),
                                ]
                                m.d.comb += [
                                    priority.skip.eq(0),
                                    ed_handler.start.eq(1),
                                ]
                                m.next = 'ENDPOINT'

                    with m.Elif(allow_control):
                        with m.If(hccontrolcurrented == 0):
                            with m.If(hccommandstatus.clf):
                                m.d.sync += [
                                    hccontrolcurrented.eq(hccontrolheaded),
                                    hccommandstatus.clf.eq(0),
                                ]
                                m.d.comb += priority.skip.eq(0)

                        with m.Else():
                            m.d.sync += [
                                ed_handler.flow_type.eq(OhciFlowType.CONTROL),
                                ed_handler.ed_addr.eq(hccontrolcurrented),
                            ]
                            m.d.comb += [
                                priority.skip.eq(0),
                                ed_handler.start.eq(1),
                            ]
                            m.next = 'ENDPOINT'

            with m.State('ENDPOINT'):
                m.d.comb += hccontrol.hcfs.eq(
                    OhciFunctionalState.USB_OPERATIONAL)

                m.d.comb += [
                    ed_handler.tx_token.connect(token_tx.token),
                    ed_handler.tx_req.connect(data_tx.req),
                    ed_handler.tx_data.connect(data_tx.data),
                    ed_handler.mem_read_cmd.connect(self.mem_read_cmd),
                    self.mem_read_data.connect(ed_handler.mem_read_data),
                    ed_handler.mem_write_cmd.connect(self.mem_write_cmd),
                    ed_handler.mem_write_data.connect(self.mem_write_data),
                    ed_handler.mem_write_done.eq(self.mem_write_done),
                    token_tx.tx_eop.eq(phy.tx_eop),
                    data_tx.tx_eop.eq(phy.tx_eop),
                ]

                with m.If(data_tx.tx_data.valid):
                    m.d.comb += data_tx.tx_data.connect(phy.tx_data)
                with m.Elif(token_tx.tx_data.valid):
                    m.d.comb += token_tx.tx_data.connect(phy.tx_data)

                with m.If(ed_handler.done):
                    with m.If(ed_handler.flow_type != OhciFlowType.PERIODIC):
                        m.d.comb += priority.tick.eq(1)

                    with m.If(ed_handler.frame_time_hit):
                        m.next = 'WAIT_SOF'
                    with m.Else():
                        m.next = 'ARBITER'

            with m.State('PERIODIC_HEAD_CMD'):
                m.d.comb += hccontrol.hcfs.eq(
                    OhciFunctionalState.USB_OPERATIONAL)

                m.d.comb += [
                    self.mem_read_cmd.valid.eq(1),
                    self.mem_read_cmd.bits.addr.eq(self._hchcca.r_data
                                                   | (hcfmnumber.fn[:4] << 2)),
                    self.mem_read_cmd.bits.len.eq(4),
                ]

                with m.If(self.mem_read_cmd.fire):
                    m.next = 'PERIODIC_HEAD_RESP'

            with m.State('PERIODIC_HEAD_RESP'):
                m.d.comb += hccontrol.hcfs.eq(
                    OhciFunctionalState.USB_OPERATIONAL)

                ph_converter = m.submodules.ph_converter = AXIStreamConverter(
                    dw_from=self.data_width, dw_to=32)

                m.d.comb += [
                    self.mem_read_data.connect(ph_converter.sink),
                    ph_converter.source.ready.eq(1),
                ]

                with m.If(ph_converter.source.fire):
                    m.d.sync += [
                        periodic_fetched.eq(1),
                        hcperiodcurrented.eq(ph_converter.source.bits),
                    ]
                    m.next = 'ARBITER'

        return m
