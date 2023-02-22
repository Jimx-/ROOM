from amaranth import *
from amaranth.lib.fifo import SyncFIFO

from ..peripheral import Peripheral


class SPIMaster(Peripheral, Elaboratable):

    def __init__(self,
                 *,
                 name=None,
                 data_width=8,
                 fifo_depth=16,
                 cs_bits=1,
                 with_csr=True,
                 **kwargs):
        super().__init__(name=name)

        self.data_width = data_width
        self.with_csr = with_csr

        self._rx_fifo = SyncFIFO(width=data_width, depth=fifo_depth)
        self._tx_fifo = SyncFIFO(width=data_width, depth=fifo_depth)

        if with_csr:
            bank = self.csr_bank()
            self._control = bank.csr(32, 'rw')
            self._status = bank.csr(32, 'r')
            self._data = bank.csr(data_width, 'rw')
            self._cs = bank.csr(cs_bits, 'rw')
            self._clk_divider = bank.csr(16, 'rw')

            self._bridge = self.bridge(data_width=32,
                                       granularity=8,
                                       alignment=2)
            self.bus = self._bridge.bus

        self.enabled = Signal()
        self.loopback = Signal()
        self.rx_reset = Signal()
        self.tx_reset = Signal()
        self.done = Signal()
        self.cs = Signal(cs_bits, reset=1)
        self.clk_divider = Signal(16)

        self.w_data = Signal(self.data_width)
        self.w_en = Signal()
        self.w_rdy = Signal()

        self.sclk = Signal()
        self.mosi = Signal()
        self.miso = Signal()
        self.cs_n = Signal(cs_bits)

    def elaborate(self, platform):
        m = Module()

        if self.with_csr:
            m.submodules.bridge = self._bridge

        self._rx_fifo = ResetInserter(self.rx_reset)(self._rx_fifo)
        self._tx_fifo = ResetInserter(self.tx_reset)(self._tx_fifo)

        m.submodules.rx_fifo = self._rx_fifo
        m.submodules.tx_fifo = self._tx_fifo

        clk_enable = Signal()
        xfer_enable = Signal()
        count = Signal(range(self.data_width))

        # Clock generation
        clk_divider = Signal(16)
        clk_rise = Signal()
        clk_fall = Signal()
        mosi_latch = Signal()
        miso_latch = Signal()
        m.d.comb += [
            clk_rise.eq(clk_divider == (self.clk_divider[1:] - 1)),
            clk_fall.eq(clk_divider == (self.clk_divider - 1)),
        ]
        m.d.sync += clk_divider.eq(clk_divider + 1)

        with m.If(clk_rise):
            m.d.sync += self.sclk.eq(clk_enable)
        with m.Elif(clk_fall):
            m.d.sync += [
                clk_divider.eq(0),
                self.sclk.eq(0),
            ]

        # Control state machine
        with m.FSM():
            with m.State('IDLE'):
                m.d.comb += self.done.eq(1)
                with m.If(self.enabled & self._tx_fifo.r_rdy):
                    m.d.comb += [
                        self.done.eq(0),
                        mosi_latch.eq(1),
                    ]
                    m.next = 'START'

            with m.State('START'):
                m.d.sync += count.eq(0)
                with m.If(clk_fall):
                    m.d.comb += xfer_enable.eq(1)
                    m.next = 'RUN'

            with m.State('RUN'):
                m.d.comb += [
                    clk_enable.eq(1),
                    xfer_enable.eq(1),
                ]

                with m.If(clk_fall):
                    m.d.sync += count.eq(count + 1)
                    with m.If(count == (self.data_width - 1)):
                        m.next = 'STOP'

            with m.State('STOP'):
                m.d.comb += xfer_enable.eq(1)
                with m.If(clk_rise):
                    m.d.comb += miso_latch.eq(1)
                    m.next = 'IDLE'

        # Chip select generation
        for i in range(len(self.cs_n)):
            cs = (self.cs[i] & xfer_enable)
            m.d.sync += self.cs_n[i].eq(~cs)

        # MOSI data generation
        mosi_data = Signal(self.data_width)
        mosi_array = Array(mosi_data[i] for i in range(self.data_width))
        mosi_sel = Signal(range(self.data_width))
        with m.If(mosi_latch):
            m.d.sync += [
                mosi_data.eq(self._tx_fifo.r_data),
                mosi_sel.eq(self.data_width - 1),
            ]
        with m.Elif(clk_fall):
            with m.If(xfer_enable):
                m.d.sync += self.mosi.eq(mosi_array[mosi_sel])
            m.d.sync += mosi_sel.eq(mosi_sel - 1)

        # MISO data capture
        miso_data = Signal(self.data_width)
        with m.If(clk_rise):
            with m.If(self.loopback):
                m.d.sync += miso_data.eq(Cat(self.mosi, miso_data))
            with m.Else():
                m.d.sync += miso_data.eq(Cat(self.miso, miso_data))

        # CSR
        if self.with_csr:
            with m.If(self._control.w_stb):
                m.d.sync += Cat(self.enabled, self.loopback, self.tx_reset,
                                self.rx_reset).eq(self._control.w_data)
            with m.Else():
                m.d.sync += [
                    self.tx_reset.eq(0),
                    self.rx_reset.eq(0),
                ]

            with m.If(self._cs.w_stb):
                m.d.sync += self.cs.eq(self._cs.w_data)

            with m.If(self._clk_divider.w_stb):
                m.d.sync += [
                    self.clk_divider.eq(self._clk_divider.w_data),
                    clk_divider.eq(0),
                ]

            m.d.comb += self._status.r_data.eq(
                Cat(self.done, ~self._rx_fifo.r_rdy, ~self._rx_fifo.w_rdy,
                    ~self._tx_fifo.r_rdy, ~self._tx_fifo.w_rdy))

            m.d.comb += [
                self._data.r_data.eq(self._rx_fifo.r_data),
                self._rx_fifo.r_en.eq(self._data.r_stb),
                self.w_en.eq(self._data.w_stb),
                self.w_data.eq(self._data.w_data),
            ]

        # FIFO interface
        m.d.comb += [
            self._rx_fifo.w_data.eq(miso_data),
            self._rx_fifo.w_en.eq(miso_latch),
            self._tx_fifo.w_en.eq(self.w_en),
            self._tx_fifo.w_data.eq(self.w_data),
            self._tx_fifo.r_en.eq(mosi_latch),
            self.w_rdy.eq(self._tx_fifo.w_rdy),
        ]

        return m
