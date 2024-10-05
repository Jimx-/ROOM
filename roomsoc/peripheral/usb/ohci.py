from amaranth import *
from amaranth.hdl.rec import Direction
from amaranth.lib.fifo import SyncFIFO
from enum import IntEnum, Enum

from roomsoc.peripheral import Peripheral
from roomsoc.interconnect.axi import AXIStreamInterface, AXIStreamDepacketizer, AXIStreamConverter
from roomsoc.interconnect.stream import Decoupled, Valid, Queue

from .phy import UsbLsFsPhyIo, UsbLsFsPhy, UsbRxData
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
    HCRHDESCRIPTORA = 0x48
    HCRHDESCRIPTORB = 0x4c
    HCRHSTATUS = 0x50
    HCRHPORTSTATUS = 0x54


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

_hcrhdescriptora_layout = [
    ('ndp', 8),
    ('psm', 1),
    ('nps', 1),
    ('dt', 1),
    ('ocpm', 1),
    ('nocp', 1),
    ('_rsvd0', 11),
    ('potpgt', 8),
]

_hcrhdescriptorb_layout = [
    ('dr', 16),
    ('ppcm', 16),
]

_hcrhstatus_layout = [
    ('lps', 1),
    ('oci', 1),
    ('_rsvd0', 13),
    ('drwe', 1),
    ('lpsc', 1),
    ('ocic', 1),
    ('_rsvd1', 13),
    ('crwe', 1),
]

_hcrhportstatus_layout = [
    ('ccs', 1),
    ('pes', 1),
    ('pss', 1),
    ('poci', 1),
    ('prs', 1),
    ('_rsvd0', 3),
    ('pps', 1),
    ('lsda', 1),
    ('_rsvd1', 6),
    ('csc', 1),
    ('pesc', 1),
    ('pssc', 1),
    ('ocic', 1),
    ('prsc', 1),
    ('_rsvd2', 11),
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

_itd_layout = [
    ('sf', 16),
    ('_rsvd0', 5),
    ('di', 3),
    ('fc', 3),
    ('_rsvd1', 1),
    ('cc', 4),
    ('_rsvd2', 12),
    ('bp0', 20),
    ('_rsvd3', 5),
    ('nexttd', 27),
    ('be', 32),
    ('offset', 128),
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
        self.frame_number = Signal(16)
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

        rx_bytes = Signal(11)
        with m.If(data_rx.start):
            m.d.sync += rx_bytes.eq(0)
        with m.Elif(data_rx.data.valid):
            m.d.sync += rx_bytes.eq(rx_bytes + ~rx_bytes[-1])

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

        td_addr = ed.headp << 4

        td_depacketizer = m.submodules.td_depacketizer = AXIStreamDepacketizer(
            Record, _td_layout, data_width=self.data_width)
        td = Record(_td_layout)
        td_cbp = Mux(td_depacketizer.source.valid, td_depacketizer.header.cbp,
                     td.cbp)
        td_be = Mux(td_depacketizer.source.valid, td_depacketizer.header.be,
                    td.be)

        td_write = Record(_td_layout)
        m.d.comb += td_write.eq(td)

        td_converter = m.submodules.td_converter = AXIStreamConverter(
            dw_from=len(td), dw_to=self.data_width)
        m.d.comb += [
            td_converter.sink.bits.data.eq(td_write),
            td_converter.sink.bits.keep.eq(~0),
            td_converter.sink.bits.last.eq(1),
        ]

        token_type = Mux(ed.d[0] ^ ed.d[1], ed.d, td.dp)
        is_in = token_type == DirectionPid.IN

        itd_depacketizer = m.submodules.itd_depacketizer = AXIStreamDepacketizer(
            Record, _itd_layout, data_width=self.data_width)
        itd = Record(_itd_layout)
        itd_sf = Mux(itd_depacketizer.source.valid, itd_depacketizer.header.sf,
                     itd.sf)
        itd_fc = Mux(itd_depacketizer.source.valid, itd_depacketizer.header.fc,
                     itd.fc)
        itd_cbp = Mux(itd_depacketizer.source.valid,
                      itd_depacketizer.header.bp0, itd.bp0)
        itd_be = Mux(itd_depacketizer.source.valid, itd_depacketizer.header.be,
                     itd.be)

        itd_write = Record(_itd_layout)
        m.d.comb += itd_write.eq(itd)

        itd_converter = m.submodules.itd_converter = AXIStreamConverter(
            dw_from=len(itd), dw_to=self.data_width)
        m.d.comb += [
            itd_converter.sink.bits.data.eq(itd_write),
            itd_converter.sink.bits.keep.eq(~0),
            itd_converter.sink.bits.last.eq(1),
        ]

        iso_rel_fn = Signal.like(self.frame_number)
        m.d.comb += iso_rel_fn.eq(self.frame_number - itd_sf)
        iso_fn = iso_rel_fn[:len(itd_fc)]
        iso_too_early = iso_rel_fn[-1]
        iso_overrun = ~iso_too_early & (iso_rel_fn > itd_fc)
        iso_last = ~iso_too_early & ~iso_overrun & (iso_fn == itd_fc)
        iso_base = Signal(13)
        iso_base_next = Signal(13)
        iso_zero_len = Mux(iso_last, iso_base_next < iso_base,
                           iso_base_next == iso_base)

        xtd_cbp = Mux(ed.f, itd_cbp, td_cbp)
        xtd_be = Mux(ed.f, itd_be, td_be)
        xtd_cc = Signal(OhciConditionCode)

        xtd_single_page = xtd_cbp[12:] == xtd_be[12:]
        xtd_first_offset = Mux(ed.f, iso_base, td_cbp[:12])
        xtd_last_offset = Mux(ed.f, iso_base_next - Mux(iso_last, 0, 1),
                              Cat(td_be[:12], ~xtd_single_page))

        buf_offset = Signal(14)
        buf_addr = Cat(buf_offset[:12],
                       Mux(buf_offset[12], xtd_be[12:], xtd_cbp[12:]))
        last_addr = Signal(13)
        txn_len = last_addr - buf_offset + 1
        zero_len = Mux(ed.f, iso_zero_len, td.cbp == 0)
        data_phase = Mux(ed.f, 0, Mux(td.t[1], td.t[0], ed.c))
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

        rx_overflow = Signal()
        rx_underflow = Signal()
        rx_underflow_err = rx_underflow & ~td.r

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
                    self.mem_read_cmd.bits.len.eq(
                        Mux(ed.f, len(itd), len(td)) >> 3),
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
                m.d.comb += tx_fifo.w_en.eq(0)

                with m.If(ed.f):
                    m.d.comb += self.mem_read_data.connect(
                        itd_depacketizer.sink)

                    with m.If(itd_depacketizer.source.valid):
                        m.d.comb += itd_depacketizer.source.ready.eq(1)
                        m.d.sync += itd.eq(itd_depacketizer.header)

                        with m.Switch(iso_fn):
                            for i in range(8):
                                with m.Case(i):
                                    m.d.sync += iso_base.eq(
                                        itd_depacketizer.header.offset[i *
                                                                       16:(i +
                                                                           1) *
                                                                       16])

                                    if i != 7:
                                        m.d.sync += iso_base_next.eq(
                                            itd_depacketizer.header.offset[
                                                (i + 1) * 16:(i + 2) * 16])
                                    else:
                                        m.d.sync += iso_base_next.eq(
                                            Cat(
                                                itd_depacketizer.header.
                                                be[:12], xtd_single_page))

                        m.next = 'TD_CHECK'

                with m.Else():
                    m.d.comb += self.mem_read_data.connect(
                        td_depacketizer.sink)

                    with m.If(td_depacketizer.source.valid):
                        m.d.comb += td_depacketizer.source.ready.eq(1)
                        m.d.sync += td.eq(td_depacketizer.header)

                        m.next = 'TD_CHECK'

            with m.State('TD_CHECK'):
                m.d.comb += self.fill_list.eq(1)

                m.d.sync += [
                    buf_offset.eq(xtd_first_offset),
                    last_addr.eq(xtd_last_offset),
                ]

                m.next = 'TD_CHECK_TIME'

                with m.If(ed.f):
                    with m.If(iso_too_early):
                        m.next = 'FINISH'

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
                m.d.sync += [
                    rx_overflow.eq(0),
                    rx_underflow.eq(0),
                ]
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
                    underflow = rx_bytes < txn_len
                    m.d.sync += [
                        rx_underflow.eq(underflow),
                        rx_overflow.eq(~underflow & (rx_bytes != txn_len)),
                    ]

                    with m.If(zero_len):
                        m.d.sync += [
                            rx_underflow.eq(0),
                            rx_overflow.eq(rx_bytes.any()),
                        ]

                    with m.If(underflow):
                        m.d.sync += last_addr.eq(buf_offset + rx_bytes - 1)

                    m.next = 'DATA_RX_VALIDATE'

            with m.State('DATA_RX_VALIDATE'):
                m.next = 'DATA_RX_ABORT'

                pid_mismatched = data_rx.pid == Mux(data_phase, UsbPid.DATA0,
                                                    UsbPid.DATA1)
                pid_ok = (
                    (data_rx.pid == UsbPid.DATA0) |
                    (data_rx.pid == UsbPid.DATA1)) & (ed.f | ~pid_mismatched)

                m.d.sync += xtd_cc.eq(OhciConditionCode.NO_ERROR)
                with m.If(data_rx.stuffing_error):
                    m.d.sync += xtd_cc.eq(OhciConditionCode.BIT_STUFFING)
                with m.Elif(data_rx.pid_error):
                    m.d.sync += xtd_cc.eq(OhciConditionCode.PID_CHECK_FAIL)
                with m.Else():
                    with m.If(ed.f):
                        with m.Switch(data_rx.pid):
                            with m.Case(UsbPid.STALL, UsbPid.NAK):
                                m.d.sync += xtd_cc.eq(OhciConditionCode.STALL)
                            with m.Case(UsbPid.DATA0, UsbPid.DATA1):
                                m.next = 'DATA_RX_WAIT_DMA'
                            with m.Default():
                                m.d.sync += xtd_cc.eq(
                                    OhciConditionCode.UNEXPECTED_PID)

                    with m.Else():
                        with m.Switch(data_rx.pid):
                            with m.Case(UsbPid.NAK):
                                m.d.sync += skip_td_update.eq(1)
                            with m.Case(UsbPid.STALL):
                                m.d.sync += xtd_cc.eq(OhciConditionCode.STALL)
                            with m.Case(UsbPid.DATA0, UsbPid.DATA1):
                                with m.If(pid_mismatched):
                                    m.d.sync += xtd_cc.eq(
                                        OhciConditionCode.DATA_TOGGLE_MISMATCH)
                                m.next = 'ACK_TX0'
                            with m.Default():
                                m.d.sync += xtd_cc.eq(
                                    OhciConditionCode.UNEXPECTED_PID)

                with m.If(pid_ok):
                    with m.If(data_rx.crc_error):
                        m.d.sync += xtd_cc.eq(OhciConditionCode.CRC)
                    with m.Else():
                        with m.If(rx_underflow_err):
                            m.d.sync += xtd_cc.eq(
                                OhciConditionCode.DATA_UNDERRUN)
                        with m.Elif(rx_overflow):
                            m.d.sync += xtd_cc.eq(
                                OhciConditionCode.DATA_OVERRUN)

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

                    with m.If(ed.f):
                        m.next = 'UPDATE_TD'
                    with m.Else():
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

                with m.If(ed.f):
                    m.d.sync += td_retire.eq(iso_last)
                with m.Else():
                    m.d.sync += td.ec.eq(0)
                    with m.Switch(xtd_cc):
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
                    self.mem_write_cmd.bits.len.eq(
                        Mux(ed.f, len(itd), len(td)) >> 3),
                ]

                with m.If(self.mem_write_cmd.fire):
                    m.next = 'UPDATE_TD_DATA'

            with m.State('UPDATE_TD_DATA'):
                m.d.sync += ed.h.eq(~ed.f
                                    & (xtd_cc != OhciConditionCode.NO_ERROR))

                with m.If(ed.f):
                    with m.If(iso_overrun):
                        m.d.comb += itd_write.cc.eq(
                            OhciConditionCode.DATA_OVERRUN)
                    with m.Else():
                        with m.If(iso_last):
                            m.d.comb += itd_write.cc.eq(
                                OhciConditionCode.NO_ERROR)

                        psw_count = Mux(ed.d[0], 0, txn_len)[:12]
                        psw = Cat(psw_count, xtd_cc)

                        with m.Switch(iso_fn):
                            for i in range(8):
                                with m.Case(i):
                                    m.d.comb += itd_write.offset[i *
                                                                 16:(i + 1) *
                                                                 16].eq(psw)

                        with m.If(td_retire):
                            m.d.comb += itd_write.nexttd.eq(done_head.dh)

                        m.d.comb += [
                            itd_converter.sink.valid.eq(1),
                            itd_converter.source.connect(self.mem_write_data),
                        ]

                        with m.If(itd_converter.sink.fire):
                            m.next = 'UPDATE_ED_CMD'

                with m.Else():
                    m.d.comb += [
                        td_write.cc.eq(xtd_cc),
                        td_converter.sink.valid.eq(1),
                        td_converter.source.connect(self.mem_write_data),
                    ]

                    with m.If(td_retire):
                        m.d.comb += td_write.nexttd.eq(done_head.dh)

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
                with m.If(~(ed.f & iso_overrun)):
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

    def __init__(self,
                 data_width,
                 clk_freq,
                 *,
                 name=None,
                 fifo_depth=16,
                 port_count=1):
        super().__init__(name=name)

        self.clk_freq = clk_freq
        self.data_width = data_width
        self.fifo_depth = fifo_depth
        self.port_count = port_count

        self.mem_read_cmd = Decoupled(OhciController.MemoryCommand)
        self.mem_read_data = AXIStreamInterface(data_width=data_width)

        self.mem_write_cmd = Decoupled(OhciController.MemoryCommand)
        self.mem_write_data = AXIStreamInterface(data_width=data_width)
        self.mem_write_done = Signal()

        self.irq = Signal()

        self.usb = [UsbLsFsPhyIo(name=f'usb{i}') for i in range(port_count)]

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
        self._hcrhdescriptora = bank.csr(32,
                                         'rw',
                                         addr=OhciReg.HCRHDESCRIPTORA)
        self._hcrhdescriptorb = bank.csr(32,
                                         'rw',
                                         addr=OhciReg.HCRHDESCRIPTORB)
        self._hcrhstatus = bank.csr(32, 'rw', addr=OhciReg.HCRHSTATUS)
        self._hcrhportstatus = [
            bank.csr(32,
                     'rw',
                     addr=OhciReg.HCRHPORTSTATUS + i * 4,
                     name=f'hcrhportstatus{i}') for i in range(port_count)
        ]

        self._bridge = self.bridge(data_width=32, granularity=8, alignment=2)
        self.bus = self._bridge.bus

    def elaborate(self, platform):
        m = Module()
        m.submodules.bridge = self._bridge

        phy = m.submodules.phy = UsbLsFsPhy(clk_freq=self.clk_freq,
                                            port_count=self.port_count)
        for usb, phy_usb in zip(self.usb, phy.usb):
            m.d.comb += phy_usb.connect(usb)

        token_tx = m.submodules.token_tx = UsbTokenTx()
        data_tx = m.submodules.data_tx = UsbDataTx()

        m.d.comb += self._hcrevision.r_data.eq(0x110)

        hccontrol = Record(_hccontrol_layout)
        hccontrol_write = Record(_hccontrol_layout)
        m.d.comb += [
            self._hccontrol.r_data.eq(hccontrol),
            hccontrol_write.eq(self._hccontrol.w_data),
        ]
        usb_operational = hccontrol.hcfs == OhciFunctionalState.USB_OPERATIONAL

        with m.If(self._hccontrol.w_stb):
            m.d.sync += [
                hccontrol.ple.eq(hccontrol_write.ple),
                hccontrol.ble.eq(hccontrol_write.ble),
                hccontrol.cle.eq(hccontrol_write.cle),
            ]

        hccommandstatus = Record(_hccommandstatus_layout)
        hccommandstatus_write = Record(_hccommandstatus_layout)
        m.d.comb += [
            self._hccommandstatus.r_data.eq(hccommandstatus),
            hccommandstatus_write.eq(self._hccommandstatus.w_data),
        ]
        with m.If(self._hccommandstatus.w_stb):
            m.d.sync += [
                hccommandstatus.clf.eq(hccommandstatus_write.clf),
                hccommandstatus.blf.eq(hccommandstatus_write.blf),
                hccommandstatus.ocr.eq(hccommandstatus_write.ocr),
            ]

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
        hcfminterval_write = Record(_hcfminterval_layout)
        hcfmremaining = Record(_hcfmremaining_layout)
        hcfmnumber = Record(_hcfmnumber_layout)
        hcfminterval.fi.reset = 0x2edf
        m.d.comb += [
            self._hcfminterval.r_data.eq(hcfminterval),
            hcfminterval_write.eq(self._hcfminterval.w_data),
            self._hcfmremaining.r_data.eq(hcfmremaining),
            self._hcfmnumber.r_data.eq(hcfmnumber),
        ]
        with m.If(self._hcfminterval.w_stb):
            m.d.sync += [
                hcfminterval.fi.eq(hcfminterval_write.fi),
                hcfminterval.fsmps.eq(hcfminterval_write.fsmps),
                hcfminterval.fit.eq(hcfminterval_write.fit),
            ]

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
            ed_handler.frame_number.eq(hcfmnumber.fn),
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
            m.d.sync += hcperiodicstart.ps.eq(self._hcperiodicstart.w_data)

        allow_bulk = Signal()
        allow_control = Signal()
        allow_periodic = Signal()
        allow_isochronous = Signal()

        periodic_fetched = Signal()
        periodic_done = Signal()

        with m.FSM():
            with m.State('RESET'):
                m.d.comb += [
                    hccontrol.hcfs.eq(OhciFunctionalState.USB_RESET),
                    phy.usb_reset.eq(1),
                ]
                m.d.sync += allow_periodic.eq(0)

                with m.If(self._hccontrol.w_stb):
                    with m.Switch(hccontrol_write.hcfs):
                        with m.Case(OhciFunctionalState.USB_OPERATIONAL):
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
                    token_tx.token.bits.data.eq(hcfmnumber.fn),
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

        hcrhdescriptora_r = Record(_hcrhdescriptora_layout)
        hcrhdescriptora_w = Record(_hcrhdescriptora_layout)
        hcrhdescriptorb_r = Record(_hcrhdescriptorb_layout)
        hcrhdescriptorb_w = Record(_hcrhdescriptorb_layout)
        hcrhstatus_r = Record(_hcrhstatus_layout)
        hcrhstatus_w = Record(_hcrhstatus_layout)
        m.d.comb += [
            self._hcrhdescriptora.r_data.eq(hcrhdescriptora_r),
            hcrhdescriptora_w.eq(self._hcrhdescriptora.w_data),
            self._hcrhdescriptorb.r_data.eq(hcrhdescriptorb_r),
            hcrhdescriptorb_w.eq(self._hcrhdescriptorb.w_data),
            self._hcrhstatus.r_data.eq(hcrhstatus_r),
            hcrhstatus_w.eq(self._hcrhstatus.w_data),
        ]

        m.d.comb += [
            hcrhdescriptora_r.ndp.eq(self.port_count),
        ]

        with m.If(self._hcrhdescriptora.w_stb):
            m.d.sync += [
                hcrhdescriptora_r.nps.eq(hcrhdescriptora_w.nps),
                hcrhdescriptora_r.psm.eq(hcrhdescriptora_w.psm),
                hcrhdescriptora_r.ocpm.eq(hcrhdescriptora_w.ocpm),
                hcrhdescriptora_r.nocp.eq(hcrhdescriptora_w.nocp),
                hcrhdescriptora_r.potpgt.eq(hcrhdescriptora_w.potpgt),
            ]

        with m.If(self._hcrhdescriptorb.w_stb):
            m.d.sync += hcrhdescriptorb_r.eq(hcrhdescriptorb_w)

        with m.If(self._hcrhstatus.w_stb):
            m.d.sync += hcrhstatus_r.drwe.eq(hcrhstatus_w.drwe)

        for i, (port_status,
                port) in enumerate(zip(self._hcrhportstatus, phy.ctrl)):
            port_status_r = Record(_hcrhportstatus_layout,
                                   name=f'port_status_r{i}')
            port_status_w = Record(_hcrhportstatus_layout,
                                   name=f'port_status_w{i}')
            m.d.comb += [
                port_status.r_data.eq(port_status_r),
                port_status_w.eq(port_status.w_data),
            ]

            clear_port_enable = Signal(name=f'clear_port_enable{i}')
            set_port_enable = Signal(name=f'set_port_enable{i}')
            set_port_suspend = Signal(name=f'set_port_suspend{i}')
            clear_suspend_status = Signal(name=f'clear_suspend_status{i}')
            set_port_reset = Signal(name=f'set_port_reset{i}')
            set_port_power = Signal(name=f'set_port_power{i}')
            clear_port_power = Signal(name=f'clear_port_power{i}')
            with m.If(port_status.w_stb):
                m.d.comb += [
                    clear_port_enable.eq(port_status_w.ccs),
                    set_port_enable.eq(port_status_w.pes),
                    set_port_suspend.eq(port_status_w.pss),
                    clear_suspend_status.eq(port_status_w.poci),
                    set_port_reset.eq(port_status_w.prs),
                    set_port_power.eq(port_status_w.pps),
                    clear_port_power.eq(port_status_w.lsda),
                ]

            pps = Signal()
            with m.If(hcrhdescriptora_r.nps):
                m.d.sync += pps.eq(1)
            with m.Else():
                with m.If(hcrhdescriptora_r.psm & hcrhdescriptorb_r.ppcm[i]):
                    with m.If(set_port_power):
                        m.d.sync += pps.eq(1)
                    with m.Elif(clear_port_power):
                        m.d.sync += pps.eq(0)
                with m.Else():
                    with m.If(self._hcrhstatus.w_stb):
                        with m.If(hcrhstatus_w.lpsc):
                            m.d.sync += pps.eq(1)
                        with m.Elif(hcrhstatus_w.lps):
                            m.d.sync += pps.eq(0)
            m.d.comb += port.power.eq(pps)

            connected = Signal(name=f'connected{i}')
            ccs = Signal()
            ccs_last = Signal()
            m.d.sync += ccs_last.eq(ccs)
            with m.If(port.disconnect):
                m.d.sync += connected.eq(0)
            with m.Elif(port.connect_):
                m.d.sync += connected.eq(1)
            m.d.comb += [
                ccs.eq((connected | hcrhdescriptorb_r.dr[i])
                       & pps),
                port_status_r.ccs.eq(ccs),
            ]

            with m.If(set_port_reset & ccs):
                m.d.sync += port_status_r.prs.eq(1)
            m.d.comb += port.reset.valid.eq(port_status_r.prs)
            with m.If(port.reset.valid & port.reset.ready):
                m.d.sync += port_status_r.prs.eq(0)

            def create_status_change(name, set_, clear=False):
                with m.If(set_):
                    m.d.sync += [
                        irq_status.rhsc.eq(1),
                        getattr(port_status_r, name).eq(1),
                    ]

                with m.If(clear | (port_status.w_stb
                                   & getattr(port_status_w, name))):
                    m.d.sync += getattr(port_status_r, name).eq(0)

            create_status_change(
                'csc', (ccs ^ ccs_last) |
                ((set_port_enable | set_port_suspend | set_port_reset) & ~ccs))

        return m
