from amaranth import *
from amaranth.lib.fifo import SyncFIFO

from amaranth_stdio.serial import AsyncSerial

from .peripheral import Peripheral


class UART(Peripheral, Elaboratable):

    def __init__(self, *, name=None, rx_depth=16, tx_depth=16, **kwargs):
        super().__init__(name=name)

        self._phy = AsyncSerial(**kwargs)
        self._rx_fifo = SyncFIFO(width=self._phy.rx.data.width, depth=rx_depth)
        self._tx_fifo = SyncFIFO(width=self._phy.tx.data.width, depth=tx_depth)

        bank = self.csr_bank()
        self._enabled = bank.csr(1, 'w')
        self._data = bank.csr(8, 'rw')
        self._status = bank.csr(3 + len(self._phy.rx.err), 'r')
        self._divisor = bank.csr(self._phy.divisor.width, "rw")

        self._bridge = self.bridge(data_width=32, granularity=8, alignment=2)
        self.bus = self._bridge.bus

        self.rx = Signal()
        self.tx = Signal()
        self.enabled = Signal()

    def elaborate(self, platform):
        m = Module()
        m.submodules.bridge = self._bridge

        m.submodules.phy = self._phy
        m.submodules.rx_fifo = self._rx_fifo
        m.submodules.tx_fifo = self._tx_fifo

        m.d.comb += self._divisor.r_data.eq(self._phy.divisor)
        with m.If(self._divisor.w_stb):
            m.d.sync += self._phy.divisor.eq(self._divisor.w_data)

        with m.If(self._enabled.w_stb):
            m.d.sync += self.enabled.eq(self._enabled.w_data)

        m.d.comb += self._status.r_data.eq(
            Cat(self._tx_fifo.w_rdy, self._rx_fifo.r_rdy, ~self._phy.tx.rdy,
                self._phy.rx.err))

        m.d.comb += [
            self._data.r_data.eq(self._rx_fifo.r_data),
            self._rx_fifo.r_en.eq(self._data.r_stb),
            self._rx_fifo.w_data.eq(self._phy.rx.data),
            self._rx_fifo.w_en.eq(self._phy.rx.rdy),
            self._phy.rx.ack.eq(self._rx_fifo.w_rdy),
            self._tx_fifo.w_en.eq(self._data.w_stb & self.enabled),
            self._tx_fifo.w_data.eq(self._data.w_data),
            self._phy.tx.data.eq(self._tx_fifo.r_data),
            self._phy.tx.ack.eq(self._tx_fifo.r_rdy),
            self._tx_fifo.r_en.eq(self._phy.tx.rdy),
            self.tx.eq(self._phy.tx.o),
            self._phy.rx.i.eq(self.rx),
        ]

        return m
