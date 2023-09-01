from amaranth import *

from roomsoc.peripheral import Peripheral


class GroomController(Peripheral, Elaboratable):

    def __init__(self, name=None):
        super().__init__(name=name)

        bank = self.csr_bank()
        self._scratch = bank.csr(32, 'r')

        self._bridge = self.bridge(data_width=32, granularity=8, alignment=2)
        self.bus = self._bridge.bus

    def elaborate(self, platform):
        m = Module()
        m.submodules.bridge = self._bridge

        m.d.comb += self._scratch.r_data.eq(0x12345678)

        return m
