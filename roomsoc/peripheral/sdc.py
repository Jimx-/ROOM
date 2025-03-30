from amaranth import *
from amaranth.lib.fifo import SyncFIFO
from enum import IntEnum

from .peripheral import Peripheral
from .event import IRQLine


class SDControllerReg(IntEnum):
    ARGUMENT = 0x00
    COMMAND = 0x04
    RESP0 = 0x08
    RESP1 = 0x0c
    RESP2 = 0x10
    RESP3 = 0x14
    CONTROLLER = 0x18
    BUFFER = 0x1c
    CMD_TIMEOUT = 0x20
    DIVISOR = 0x24
    RESET = 0x28
    DATA_TIMEOUT = 0x2c
    CMD_ISR = 0x34
    CMD_IER = 0x38
    DATA_ISR = 0x3c
    DATA_IER = 0x40
    BLKSIZE = 0x44
    BLKCNT = 0x48


class CommandIntStatus(IntEnum):
    COMPLETED = 0
    ERROR = 1
    TIMEOUT = 2
    CRC_ERR = 3
    IDX_ERR = 4


class DataIntStatus(IntEnum):
    COMPLETED = 0
    ERROR = 1
    TIMEOUT = 2
    CRC_ERR = 3
    FIFO_ERR = 4


class SDCommandMaster(Elaboratable):

    class State(IntEnum):
        IDLE = 0
        EXECUTE = 1
        WAIT_BUSY = 2

    def __init__(self):
        self.clock_posedge = Signal()

        self.argument = Signal(32)
        self.command = Signal(32)
        self.start = Signal()
        self.timeout = Signal(32)

        self.cmd = Signal(40)
        self.start_xfr = Signal()
        self.has_response = Signal()
        self.long_response = Signal()
        self.go_idle = Signal()
        self.finish = Signal()
        self.response = Signal(120)
        self.crc_err = Signal()
        self.index_ok = Signal()

        self.response0 = Signal(32)
        self.response1 = Signal(32)
        self.response2 = Signal(32)
        self.response3 = Signal(32)

        self.int_status = Signal(8)
        self.int_status_rst = Signal()

    def elaborate(self, platform):
        m = Module()

        state = Signal(self.State)

        int_status = Signal.like(self.int_status)
        with m.If(state == self.State.IDLE):
            m.d.comb += self.int_status.eq(int_status)

        timeout_enable = Signal()
        timeout_counter = Signal(32)

        with m.If(self.clock_posedge):
            with m.Switch(state):
                with m.Case(self.State.IDLE):
                    m.d.sync += self.go_idle.eq(0)

                    with m.Switch(self.command[:2]):
                        with m.Case(0b10, 0b11):
                            m.d.sync += [
                                self.has_response.eq(1),
                                self.long_response.eq(1),
                            ]
                        with m.Case(0b01):
                            m.d.sync += [
                                self.has_response.eq(1),
                                self.long_response.eq(0),
                            ]
                        with m.Default():
                            m.d.sync += [
                                self.has_response.eq(0),
                                self.long_response.eq(0),
                            ]

                    m.d.sync += self.cmd.eq(
                        Cat(self.argument, self.command[8:14], 0b01))

                    m.d.sync += [
                        timeout_counter.eq(0),
                        timeout_enable.eq(self.timeout != 0),
                    ]

                    with m.If(self.start):
                        m.d.sync += [
                            self.start_xfr.eq(1),
                            state.eq(SDCommandMaster.State.EXECUTE),
                        ]

                with m.Case(self.State.EXECUTE):
                    m.d.sync += self.start_xfr.eq(0)

                    with m.If(timeout_enable
                              & (timeout_counter >= self.timeout)):
                        m.d.sync += [
                            int_status[CommandIntStatus.TIMEOUT].eq(1),
                            int_status[CommandIntStatus.ERROR].eq(1),
                            self.go_idle.eq(1),
                            state.eq(SDCommandMaster.State.IDLE),
                        ]

                    with m.Elif(self.finish):
                        with m.If(self.command[3] & self.crc_err):
                            m.d.sync += [
                                int_status[CommandIntStatus.CRC_ERR].eq(1),
                                int_status[CommandIntStatus.ERROR].eq(1),
                            ]

                        with m.If(self.command[4] & ~self.index_ok):
                            m.d.sync += [
                                int_status[CommandIntStatus.IDX_ERR].eq(1),
                                int_status[CommandIntStatus.ERROR].eq(1),
                            ]

                        with m.If(self.has_response):
                            m.d.sync += [
                                self.response0.eq(self.response[88:120]),
                                self.response1.eq(self.response[56:88]),
                                self.response2.eq(self.response[24:56]),
                                self.response3.eq(
                                    Cat(Repl(0, 8), self.response[:24])),
                            ]

                        m.d.sync += [
                            int_status[CommandIntStatus.COMPLETED].eq(1),
                            state.eq(SDCommandMaster.State.IDLE),
                        ]

                    with m.Elif(timeout_enable):
                        m.d.sync += timeout_counter.eq(timeout_counter + 1)

        with m.If(self.int_status_rst):
            m.d.sync += int_status.eq(0)

        return m


class CommandCRC7(Elaboratable):

    def __init__(self):
        self.bit = Signal()
        self.en = Signal()
        self.clear = Signal()

        self.crc = Signal(7)

    def elaborate(self, platform):
        m = Module()

        inv = self.bit ^ self.crc[-1]

        with m.If(self.en):
            m.d.sync += [
                self.crc[6].eq(self.crc[5]),
                self.crc[5].eq(self.crc[4]),
                self.crc[4].eq(self.crc[3]),
                self.crc[3].eq(self.crc[2] ^ inv),
                self.crc[2].eq(self.crc[1]),
                self.crc[1].eq(self.crc[0]),
                self.crc[0].eq(inv),
            ]

        with m.If(self.clear):
            m.d.sync += self.crc.eq(0)

        return m


class SDCommandPHY(Elaboratable):

    class State(IntEnum):
        INIT = 0
        IDLE = 1
        SETUP_CRC = 2
        WRITE = 3
        READ_WAIT = 4
        READ = 5
        FINISH_WRITE = 6
        FINISH_READ = 7

    def __init__(self):
        self.clock_posedge = Signal()
        self.clock_data_in = Signal()

        self.start = Signal()
        self.cmd = Signal(40)
        self.has_response = Signal()
        self.long_response = Signal()

        self.cmd_i = Signal()
        self.cmd_o = Signal(reset=1)
        self.cmd_oe = Signal(reset=1)
        self.finish = Signal()
        self.response = Signal(120)
        self.crc_err = Signal()
        self.index_ok = Signal()

    def elaborate(self, platform):
        m = Module()

        counter = Signal(range(256))
        resp_idx = Signal(range(128))
        cmd = Array(Signal(name=f'cmd{i}') for i in range(len(self.cmd)))
        resp = Array(Signal(name=f'resp{i}') for i in range(128))

        state = Signal(self.State, reset=self.State.INIT)

        resp_len = Signal(7)
        with_response = Signal()

        cmd_crc = m.submodules.cmd_crc = CommandCRC7()
        crc_enable = Signal()
        crc_in = Array(
            Signal(name=f'crc_in{i}') for i in range(len(cmd_crc.crc)))
        crc_val = Array(
            Signal(name=f'crc_val{i}') for i in range(len(cmd_crc.crc)))
        crc_match = Signal()
        m.d.comb += [
            cmd_crc.en.eq(self.clock_posedge & crc_enable),
            Cat(*crc_val).eq(cmd_crc.crc),
        ]

        cmd_data_i = Signal()
        with m.If(self.clock_data_in):
            m.d.sync += cmd_data_i.eq(self.cmd_i)

        with m.If(self.clock_posedge):
            with m.Switch(state):
                with m.Case(self.State.INIT):
                    m.d.sync += [
                        counter.eq(counter + 1),
                        self.cmd_oe.eq(1),
                        self.cmd_o.eq(1),
                    ]

                    with m.If(counter >= 4):
                        m.d.sync += state.eq(self.State.IDLE)

                with m.Case(self.State.IDLE):
                    m.d.sync += [
                        counter.eq(0),
                        resp_idx.eq(0),
                        self.cmd_oe.eq(0),
                        self.finish.eq(0),
                        cmd_crc.clear.eq(1),
                    ]

                    with m.If(self.start):
                        m.d.sync += [
                            resp_len.eq(Mux(self.long_response, 127, 39)),
                            with_response.eq(self.has_response),
                            Cat(*cmd).eq(self.cmd),
                            state.eq(self.State.SETUP_CRC),
                        ]

                with m.Case(self.State.SETUP_CRC):
                    m.d.sync += [
                        cmd_crc.clear.eq(0),
                        crc_enable.eq(1),
                        cmd_crc.bit.eq(cmd[39 - counter]),
                        state.eq(self.State.WRITE),
                    ]

                with m.Case(self.State.WRITE):
                    with m.If(counter < 40):
                        m.d.sync += [
                            self.cmd_oe.eq(1),
                            self.cmd_o.eq(cmd[39 - counter]),
                        ]

                        with m.If(counter < 39):
                            m.d.sync += cmd_crc.bit.eq(cmd[38 - counter])
                        with m.Else():
                            m.d.sync += crc_enable.eq(0)

                    with m.Elif(counter < 47):
                        m.d.sync += [
                            self.cmd_oe.eq(1),
                            self.cmd_o.eq(crc_val[46 - counter]),
                            crc_enable.eq(0),
                        ]

                    with m.Elif(counter == 47):
                        m.d.sync += [
                            self.cmd_oe.eq(1),
                            self.cmd_o.eq(1),
                        ]

                    with m.Else():
                        m.d.sync += [
                            self.cmd_oe.eq(0),
                            self.cmd_o.eq(1),
                        ]

                    m.d.sync += counter.eq(counter + 1)

                    with m.If((counter >= 48) & with_response):
                        m.d.sync += state.eq(self.State.READ_WAIT)
                    with m.Elif(counter >= 48):
                        m.d.sync += state.eq(self.State.FINISH_WRITE)

                with m.Case(self.State.FINISH_WRITE):
                    m.d.sync += [
                        self.finish.eq(1),
                        crc_enable.eq(0),
                        cmd_crc.clear.eq(1),
                        counter.eq(0),
                        self.cmd_oe.eq(0),
                        state.eq(self.State.IDLE),
                    ]

                with m.Case(self.State.READ_WAIT):
                    m.d.sync += [
                        crc_enable.eq(0),
                        cmd_crc.clear.eq(1),
                        counter.eq(1),
                        self.cmd_oe.eq(0),
                        resp[127].eq(cmd_data_i),
                    ]

                    with m.If(~cmd_data_i):
                        m.d.sync += state.eq(self.State.READ)

                with m.Case(self.State.READ):
                    m.d.sync += [
                        cmd_crc.clear.eq(0),
                        crc_enable.eq((resp_len != 127) | (counter > 7)),
                        self.cmd_oe.eq(0),
                    ]

                    with m.If(counter <= resp_len):
                        with m.If(counter < 8):
                            m.d.sync += resp[127 - counter].eq(cmd_data_i)
                        with m.Else():
                            m.d.sync += [
                                resp_idx.eq(resp_idx + 1),
                                resp[119 - resp_idx].eq(cmd_data_i),
                            ]

                        m.d.sync += cmd_crc.bit.eq(cmd_data_i)

                    with m.Elif(counter - resp_len <= 7):
                        m.d.sync += [
                            crc_in[(resp_len + 7) - counter].eq(cmd_data_i),
                            crc_enable.eq(0),
                        ]

                    with m.Else():
                        m.d.sync += [
                            crc_enable.eq(0),
                            crc_match.eq(Cat(*crc_val) == Cat(*crc_in)),
                        ]

                    m.d.sync += counter.eq(counter + 1)
                    with m.If(counter >= resp_len + 8):
                        m.d.sync += state.eq(self.State.FINISH_READ)

                with m.Case(self.State.FINISH_READ):
                    m.d.sync += [
                        self.finish.eq(1),
                        self.crc_err.eq(~crc_match),
                        self.index_ok.eq(
                            Cat(*cmd)[32:38] == Cat(*resp)[120:126]),
                        crc_enable.eq(0),
                        cmd_crc.clear.eq(1),
                        counter.eq(0),
                        self.cmd_oe.eq(0),
                        self.response.eq(Cat(*resp)[:120]),
                        state.eq(self.State.IDLE),
                    ]

        return m


class SDDataMaster(Elaboratable):

    class State(IntEnum):
        IDLE = 0
        START_RX = 1
        START_TX = 2
        TRANSFER = 3

    def __init__(self):
        self.clock_posedge = Signal()

        self.start_tx = Signal()
        self.start_rx = Signal()
        self.timeout = Signal(32)

        self.data_read = Signal()
        self.data_write = Signal()

        self.tx_fifo_en = Signal()
        self.rx_fifo_en = Signal()
        self.fifo_empty = Signal()
        self.fifo_full = Signal()

        self.xfr_complete = Signal()
        self.crc_error = Signal()

        self.int_status = Signal(8)
        self.int_status_rst = Signal()

    def elaborate(self, platform):
        m = Module()

        state = Signal(self.State)

        timeout_enable = Signal()
        timeout_counter = Signal(32)

        with m.If(self.clock_posedge):
            with m.Switch(state):
                with m.Case(self.State.IDLE):
                    m.d.sync += [
                        self.tx_fifo_en.eq(0),
                        self.rx_fifo_en.eq(0),
                        self.data_read.eq(0),
                        self.data_write.eq(0),
                        timeout_enable.eq(self.timeout != 0),
                        timeout_counter.eq(0),
                    ]

                    with m.If(self.start_rx):
                        m.d.sync += state.eq(self.State.START_RX)

                with m.Case(self.State.START_RX):
                    m.d.sync += [
                        self.tx_fifo_en.eq(0),
                        self.rx_fifo_en.eq(1),
                        self.data_read.eq(1),
                    ]

                    with m.If(~self.xfr_complete):
                        m.d.sync += state.eq(self.State.TRANSFER)

                with m.Case(self.State.TRANSFER):
                    m.d.sync += [
                        self.data_read.eq(0),
                        self.data_write.eq(0),
                    ]

                    with m.If((self.tx_fifo_en & self.fifo_empty)
                              | (self.rx_fifo_en & self.fifo_full)):
                        m.d.sync += [
                            self.int_status[DataIntStatus.ERROR].eq(1),
                            self.int_status[DataIntStatus.FIFO_ERR].eq(1),
                            self.data_read.eq(1),
                            self.data_write.eq(1),
                            state.eq(self.State.IDLE),
                        ]

                    with m.Elif(timeout_enable
                                & (timeout_counter >= self.timeout)):
                        m.d.sync += [
                            self.int_status[DataIntStatus.ERROR].eq(1),
                            self.int_status[DataIntStatus.TIMEOUT].eq(1),
                            self.data_read.eq(1),
                            self.data_write.eq(1),
                            state.eq(self.State.IDLE),
                        ]

                    with m.Elif(self.xfr_complete):
                        m.d.sync += [
                            state.eq(self.State.IDLE),
                            self.int_status[DataIntStatus.COMPLETED].eq(1),
                        ]

                        with m.If(self.crc_error):
                            m.d.sync += [
                                self.int_status[DataIntStatus.ERROR].eq(1),
                                self.int_status[DataIntStatus.CRC_ERR].eq(1),
                            ]

                    with m.Elif(timeout_enable):
                        m.d.sync += timeout_counter.eq(timeout_counter + 1)

        with m.If(self.int_status_rst):
            m.d.sync += self.int_status.eq(0)

        return m


class DataCRC16(Elaboratable):

    def __init__(self):
        self.bit = Signal()
        self.en = Signal()
        self.clear = Signal()

        self.crc = Signal(16)

    def elaborate(self, platform):
        m = Module()

        inv = self.bit ^ self.crc[-1]

        with m.If(self.en):
            for i in range(1, 16):
                m.d.sync += self.crc[i].eq(self.crc[i - 1])

            m.d.sync += [
                self.crc[12].eq(self.crc[11] ^ inv),
                self.crc[5].eq(self.crc[4] ^ inv),
                self.crc[0].eq(inv),
            ]

        with m.If(self.clear):
            m.d.sync += self.crc.eq(0)

        return m


class SDDataPHY(Elaboratable):

    class State(IntEnum):
        IDLE = 0
        READ_WAIT = 1
        READ_DATA = 2

    def __init__(self):
        self.clock_posedge = Signal()
        self.clock_data_in = Signal()

        self.fifo_wdata = Signal(32)
        self.fifo_we = Signal()
        self.fifo_rdata = Signal(32)
        self.fifo_re = Signal()

        self.dat_i = Signal(4)
        self.dat_o = Signal(4)
        self.dat_oe = Signal(4)

        self.start_tx = Signal()
        self.start_rx = Signal()
        self.block_size = Signal(12)
        self.block_count = Signal(16)
        self.bus_4bit = Signal()
        self.crc_error = Signal()
        self.busy = Signal()

    def elaborate(self, platform):
        m = Module()

        state = Signal(self.State)
        m.d.comb += self.busy.eq(state != self.State.IDLE)

        data_in = Signal(4)
        with m.If(self.clock_data_in):
            m.d.sync += data_in.eq(self.dat_i)

        last_din = Signal(4)

        crc_enable = Signal()
        crc_clear = Signal()
        crc_out = []
        for i in range(4):
            crc = DataCRC16()
            m.submodules += crc

            crc_val = Array(Signal() for _ in range(16))
            m.d.comb += [
                crc.bit.eq(last_din[i]),
                crc.en.eq(self.clock_posedge & crc_enable),
                crc.clear.eq(crc_clear),
                Cat(*crc_val).eq(crc.crc),
            ]

            crc_out.append(crc_val)

        data_index = Signal(range(32))
        block_count = Signal.like(self.block_count)
        bus_4bit = Signal()
        data_cycles = Signal(16)
        xfr_count = Signal.like(data_cycles)
        crc_bit = Signal(range(16))

        data_out = Array(Signal() for _ in range(32))
        m.d.comb += self.fifo_wdata.eq(Cat(*data_out))

        with m.If(self.clock_posedge):
            with m.Switch(state):
                with m.Case(self.State.IDLE):
                    m.d.sync += [
                        self.dat_oe.eq(0),
                        self.dat_o.eq(0b1111),
                        crc_enable.eq(0),
                        crc_clear.eq(1),
                        data_index.eq(0),
                        crc_bit.eq(15),
                        self.fifo_we.eq(0),
                        self.fifo_re.eq(0),
                        bus_4bit.eq(self.bus_4bit),
                        block_count.eq(self.block_count),
                        data_cycles.eq(
                            Mux(self.bus_4bit, (self.block_size << 1) + 2,
                                (self.block_size << 3) + 8)),
                    ]

                    with m.If(self.start_rx & ~self.start_tx):
                        m.d.sync += state.eq(self.State.READ_WAIT)

                with m.Case(self.State.READ_WAIT):
                    m.d.sync += [
                        self.dat_oe.eq(0),
                        last_din.eq(0),
                        xfr_count.eq(0),
                        data_index.eq(0),
                        crc_bit.eq(15),
                    ]

                    with m.If(~data_in[0]):
                        m.d.sync += [
                            crc_clear.eq(0),
                            crc_enable.eq(1),
                            state.eq(self.State.READ_DATA),
                        ]

                with m.Case(self.State.READ_DATA):
                    m.d.sync += [
                        last_din.eq(data_in),
                        xfr_count.eq(xfr_count + 1),
                    ]

                    with m.If(xfr_count < data_cycles):
                        with m.If(bus_4bit):
                            m.d.sync += [
                                self.fifo_we.eq((data_index[:3] == 7)
                                                |
                                                ((xfr_count == data_cycles - 1)
                                                 & ~block_count.any())),
                                data_out[31 - (data_index[:3] << 2)].eq(
                                    data_in[3]),
                                data_out[30 - (data_index[:3] << 2)].eq(
                                    data_in[2]),
                                data_out[29 - (data_index[:3] << 2)].eq(
                                    data_in[1]),
                                data_out[28 - (data_index[:3] << 2)].eq(
                                    data_in[0]),
                            ]
                        with m.Else():
                            m.d.sync += [
                                self.fifo_we.eq((data_index == 31)
                                                |
                                                ((xfr_count == data_cycles - 1)
                                                 & ~block_count.any())),
                                data_out[31 - data_index].eq(data_in[0]),
                            ]

                        m.d.sync += [
                            data_index.eq(data_index + 1),
                            self.crc_error.eq(0),
                        ]

                    with m.Elif(xfr_count == data_cycles):
                        m.d.sync += [
                            crc_enable.eq(0),
                            self.fifo_we.eq(0),
                        ]

                    with m.Elif(xfr_count <= data_cycles + 16):
                        with m.If(crc_out[0][crc_bit] != last_din[0]):
                            m.d.sync += self.crc_error.eq(1)
                        with m.If(bus_4bit):
                            with m.If(crc_out[1][crc_bit] != last_din[1]):
                                m.d.sync += self.crc_error.eq(1)
                            with m.If(crc_out[2][crc_bit] != last_din[2]):
                                m.d.sync += self.crc_error.eq(1)
                            with m.If(crc_out[3][crc_bit] != last_din[3]):
                                m.d.sync += self.crc_error.eq(1)

                        with m.If(crc_bit == 0):
                            m.d.sync += crc_clear.eq(1)
                        with m.Else():
                            m.d.sync += crc_bit.eq(crc_bit - 1)

                    with m.Elif((block_count != 0) & ~self.crc_error):
                        m.d.sync += [
                            block_count.eq(block_count - 1),
                            state.eq(self.State.READ_WAIT),
                        ]

                    with m.Else():
                        m.d.sync += state.eq(self.State.IDLE)

            with m.If(self.start_rx & self.start_tx):
                m.d.sync += state.eq(self.State.IDLE)

        return m


class SDControllerBase(Peripheral, Elaboratable):

    def __init__(self, *, name=None, src_loc_at=2):
        super().__init__(name=name, src_loc_at=src_loc_at)

        bank = self.csr_bank()
        self._argument = bank.csr(32, 'rw', addr=SDControllerReg.ARGUMENT)
        self._command = bank.csr(32, 'rw', addr=SDControllerReg.COMMAND)
        self._resp0 = bank.csr(32, 'r', addr=SDControllerReg.RESP0)
        self._resp1 = bank.csr(32, 'r', addr=SDControllerReg.RESP1)
        self._resp2 = bank.csr(32, 'r', addr=SDControllerReg.RESP2)
        self._resp3 = bank.csr(32, 'r', addr=SDControllerReg.RESP3)
        self._ctrl_setting = bank.csr(32,
                                      'rw',
                                      addr=SDControllerReg.CONTROLLER)
        self._buffer = bank.csr(32, 'rw', addr=SDControllerReg.BUFFER)
        self._cmd_timeout = bank.csr(32,
                                     'rw',
                                     addr=SDControllerReg.CMD_TIMEOUT)
        self._divisor = bank.csr(8, 'rw', addr=SDControllerReg.DIVISOR)
        self._reset = bank.csr(8, 'rw', addr=SDControllerReg.RESET)
        self._data_timeout = bank.csr(32,
                                      'rw',
                                      addr=SDControllerReg.DATA_TIMEOUT)
        self._cmd_isr = bank.csr(8, 'rw', addr=SDControllerReg.CMD_ISR)
        self._cmd_ier = bank.csr(8, 'rw', addr=SDControllerReg.CMD_IER)
        self._data_isr = bank.csr(8, 'rw', addr=SDControllerReg.DATA_ISR)
        self._data_ier = bank.csr(8, 'rw', addr=SDControllerReg.DATA_IER)

        self._blksize = bank.csr(12, 'rw', addr=SDControllerReg.BLKSIZE)
        self._blkcnt = bank.csr(16, 'rw', addr=SDControllerReg.BLKCNT)

        self._bridge = self.bridge(data_width=32, granularity=8, alignment=2)
        self.bus = self._bridge.bus
        self.irq = IRQLine()

    def elaborate(self, platform):
        m = Module()
        m.submodules.bridge = self._bridge
        return m


class SDController(SDControllerBase):

    def __init__(self, *, name=None, src_loc_at=1):
        super().__init__(name=name, src_loc_at=src_loc_at + 1)

        self.sdio_clk = Signal()
        self.sdio_cmd_i = Signal()
        self.sdio_cmd_o = Signal(reset_less=True)
        self.sdio_cmd_t = Signal()
        self.sdio_data_i = Signal(4)
        self.sdio_data_o = Signal(4)
        self.sdio_data_t = Signal(4)

        self.start = Signal()
        self.finish = Signal()

    def elaborate(self, platform):
        m = super().elaborate(platform)

        #
        # SDIO clock
        #

        sdio_reset = Signal()

        clock_state = Signal()
        clock_posedge = Signal()
        clock_data_in = Signal()
        clock_count = Signal.like(self._divisor.r_data)
        with m.If(clock_count < self._divisor.r_data):
            m.d.sync += [
                clock_posedge.eq(0),
                clock_data_in.eq(0),
                clock_count.eq(clock_count + 1),
            ]
        with m.Else():
            m.d.sync += [
                clock_state.eq(~clock_state),
                clock_posedge.eq(~clock_state),
                clock_data_in.eq(clock_state),
                clock_count.eq(0),
            ]
        m.d.comb += self.sdio_clk.eq(sdio_reset | clock_state)

        with m.If(clock_posedge):
            m.d.sync += sdio_reset.eq(self._ctrl_setting.r_data[1])

        #
        # Output signals
        #

        sdio_cmd_o = Signal.like(self.sdio_cmd_o, reset_less=True)
        sdio_cmd_oe = Signal.like(self.sdio_cmd_t, reset_less=True)
        sdio_data_o = Signal.like(self.sdio_data_o, reset_less=True)
        sdio_data_oe = Signal.like(self.sdio_data_t, reset_less=True)

        cmd_start_tx = Signal()

        with m.If(sdio_reset):
            m.d.sync += [
                self.sdio_cmd_o.eq(0),
                self.sdio_cmd_t.eq(0),
                self.sdio_data_o.eq(0),
                self.sdio_data_t.eq(0),
            ]
        with m.Else():
            m.d.sync += [
                self.sdio_cmd_o.eq(sdio_cmd_o),
                self.sdio_cmd_t.eq(~sdio_cmd_oe),
                self.sdio_data_o.eq(sdio_data_o),
                self.sdio_data_t.eq(~(sdio_data_oe
                                      | (cmd_start_tx &
                                         (self._command.r_data == 0)))),
            ]

        cmd_start = Signal()
        software_reset = Signal()
        ctrl_rst = Signal()
        with m.If(clock_posedge):
            m.d.sync += [
                cmd_start.eq(0),
                ctrl_rst.eq(software_reset),
            ]

        #
        # Register interface
        #

        with m.If(self._argument.w_stb):
            m.d.sync += [
                self._argument.r_data.eq(self._argument.w_data),
                cmd_start.eq(1),
            ]
        with m.If(self._command.w_stb):
            m.d.sync += self._command.r_data.eq(self._command.w_data)

        with m.If(self._ctrl_setting.w_stb):
            m.d.sync += self._ctrl_setting.r_data.eq(self._ctrl_setting.w_data)

        with m.If(self._cmd_timeout.w_stb):
            m.d.sync += self._cmd_timeout.r_data.eq(self._cmd_timeout.w_data)
        with m.If(self._data_timeout.w_stb):
            m.d.sync += self._data_timeout.r_data.eq(self._data_timeout.w_data)

        self._divisor.r_data.reset = 124
        with m.If(self._divisor.w_stb):
            m.d.sync += self._divisor.r_data.eq(self._divisor.w_data)
        with m.If(self._reset.w_stb):
            m.d.sync += software_reset.eq(self._reset.w_data[0])

        cmd_int_rst = Signal()
        with m.If(clock_posedge):
            m.d.sync += cmd_int_rst.eq(0)
        with m.If(self._cmd_isr.w_stb):
            m.d.sync += cmd_int_rst.eq(1)

        with m.If(self._cmd_ier.w_stb):
            m.d.sync += self._cmd_ier.r_data.eq(self._cmd_ier.w_data)

        data_int_rst = Signal()
        with m.If(clock_posedge):
            m.d.sync += data_int_rst.eq(0)
        with m.If(self._data_isr.w_stb):
            m.d.sync += data_int_rst.eq(1)

        with m.If(self._data_ier.w_stb):
            m.d.sync += self._data_ier.r_data.eq(self._data_ier.w_data)

        with m.If(self._blksize.w_stb):
            m.d.sync += self._blksize.r_data.eq(self._blksize.w_data)
        with m.If(self._blkcnt.w_stb):
            m.d.sync += self._blkcnt.r_data.eq(self._blkcnt.w_data)

        m.domains += ClockDomain('ctrl')
        m.d.comb += [
            ClockSignal('ctrl').eq(ClockSignal()),
            ResetSignal('ctrl').eq(ResetSignal() | ctrl_rst),
        ]

        #
        # Control plane
        #

        cmd_master = m.submodules.cmd_master = DomainRenamer('ctrl')(
            SDCommandMaster())
        m.d.comb += [
            cmd_master.clock_posedge.eq(clock_posedge),
            cmd_master.argument.eq(self._argument.r_data),
            cmd_master.command.eq(self._command.r_data),
            cmd_master.start.eq(cmd_start),
            cmd_master.timeout.eq(self._cmd_timeout.r_data),
            cmd_master.int_status_rst.eq(cmd_int_rst),
            cmd_start_tx.eq(cmd_master.start_xfr),
        ]

        m.domains += ClockDomain('cmd_phy')
        m.d.comb += [
            ClockSignal('cmd_phy').eq(ClockSignal()),
            ResetSignal('cmd_phy').eq(ResetSignal() | ctrl_rst
                                      | cmd_master.go_idle),
        ]

        cmd_phy = m.submodules.cmd_phy = DomainRenamer('cmd_phy')(
            SDCommandPHY())
        m.d.comb += [
            cmd_phy.clock_posedge.eq(clock_posedge),
            cmd_phy.clock_data_in.eq(clock_data_in),
            cmd_phy.start.eq(cmd_start_tx),
            cmd_phy.has_response.eq(cmd_master.has_response),
            cmd_phy.long_response.eq(cmd_master.long_response),
            cmd_phy.cmd.eq(cmd_master.cmd),
            cmd_phy.cmd_i.eq(self.sdio_cmd_i),
            sdio_cmd_o.eq(cmd_phy.cmd_o),
            sdio_cmd_oe.eq(cmd_phy.cmd_oe),
            cmd_master.finish.eq(cmd_phy.finish),
            cmd_master.response.eq(cmd_phy.response),
            cmd_master.crc_err.eq(cmd_phy.crc_err),
            cmd_master.index_ok.eq(cmd_phy.index_ok),
        ]

        m.d.comb += [
            self._resp0.r_data.eq(cmd_master.response0),
            self._resp1.r_data.eq(cmd_master.response1),
            self._resp2.r_data.eq(cmd_master.response2),
            self._resp3.r_data.eq(cmd_master.response3),
            self._cmd_isr.r_data.eq(cmd_master.int_status),
        ]

        m.d.comb += [
            self.start.eq(cmd_master.start_xfr),
            self.finish.eq(cmd_phy.finish),
        ]

        start_data_tx = Signal()
        start_data_rx = Signal()

        with m.If(ctrl_rst):
            m.d.sync += [
                start_data_tx.eq(0),
                start_data_rx.eq(0),
            ]
        with m.Elif(clock_posedge):
            m.d.sync += [
                start_data_tx.eq(0),
                start_data_rx.eq(0),
            ]

            with m.If(cmd_start):
                with m.Switch(self._command.r_data[5:7]):
                    with m.Case(0b01):
                        m.d.sync += start_data_rx.eq(1)
                    with m.Case(0b10, 0b11):
                        m.d.sync += start_data_tx.eq(1)

        #
        # Data plane
        #

        data_fifo = m.submodules.data_fifo = DomainRenamer('ctrl')(SyncFIFO(
            width=32, depth=256))

        data_master = m.submodules.data_master = DomainRenamer('ctrl')(
            SDDataMaster())

        m.d.comb += [
            data_master.clock_posedge.eq(clock_posedge),
            data_master.start_tx.eq(start_data_tx),
            data_master.start_rx.eq(start_data_rx),
            data_master.fifo_empty.eq(~data_fifo.r_rdy),
            data_master.fifo_full.eq(~data_fifo.w_rdy),
            data_master.timeout.eq(self._data_timeout.r_data),
            self._data_isr.r_data.eq(data_master.int_status),
            data_master.int_status_rst.eq(data_int_rst),
        ]

        data_phy = m.submodules.data_phy = DomainRenamer('ctrl')(SDDataPHY())
        m.d.comb += [
            data_phy.clock_posedge.eq(clock_posedge),
            data_phy.clock_data_in.eq(clock_data_in),
            data_phy.dat_i.eq(self.sdio_data_i),
            sdio_data_o.eq(data_phy.dat_o),
            sdio_data_oe.eq(data_phy.dat_oe),
            data_phy.start_tx.eq(data_master.data_write),
            data_phy.start_rx.eq(data_master.data_read),
            data_phy.block_size.eq(self._blksize.r_data),
            data_phy.block_count.eq(self._blkcnt.r_data),
            data_phy.bus_4bit.eq(self._ctrl_setting.r_data[0]),
            data_master.crc_error.eq(data_phy.crc_error),
            data_master.xfr_complete.eq(~data_phy.busy),
        ]

        m.d.comb += [
            data_phy.fifo_rdata.eq(data_fifo.r_data),
            data_fifo.r_en.eq(clock_posedge & data_phy.fifo_re),
            data_fifo.w_data.eq(data_phy.fifo_wdata),
            data_fifo.w_en.eq(clock_posedge & data_phy.fifo_we),
        ]

        m.d.comb += self._buffer.r_data.eq(data_fifo.r_data)
        with m.If(self._buffer.r_stb):
            m.d.comb += data_fifo.r_en.eq(1)

        m.d.comb += self.irq.eq(
            (self._cmd_isr.r_data & self._cmd_ier.r_data).any()
            | (self._data_isr.r_data & self._data_ier.r_data).any())

        return m


class MockSDController(SDControllerBase):

    def __init__(self, *, name=None, src_loc_at=1):
        super().__init__(name=name, src_loc_at=src_loc_at + 1)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        with m.If(self._argument.w_stb):
            m.d.sync += self._cmd_isr.r_data.eq(1)

        with m.If(self._cmd_isr.w_stb):
            m.d.sync += self._cmd_isr.r_data.eq(0)

        return m
