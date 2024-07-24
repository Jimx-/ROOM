from amaranth import *

from roomsoc.peripheral import Peripheral


class GroomController(Peripheral, Elaboratable):

    def __init__(self, name=None):
        super().__init__(name=name)

        self.core_busy = Signal()

        self.core_enable = Signal()
        self.cache_enable = Signal()
        self.raster_enable = Signal()

        self.raster_tile_count = Signal(16)
        self.raster_tile_addr = Signal(32)
        self.raster_prim_addr = Signal(32)
        self.raster_prim_stride = Signal(16)

        bank = self.csr_bank()
        self._enable = bank.csr(32, 'rw')
        self._scratch = bank.csr(32, 'rw')
        self._raster_tile_count = bank.csr(16, 'rw')
        self._raster_tile_addr = bank.csr(32, 'rw')
        self._raster_prim_addr = bank.csr(32, 'rw')
        self._raster_prim_stride = bank.csr(16, 'rw')

        self._bridge = self.bridge(data_width=32, granularity=8, alignment=2)
        self.bus = self._bridge.bus

    def elaborate(self, platform):
        m = Module()
        m.submodules.bridge = self._bridge

        m.d.comb += self._enable.r_data.eq(
            Cat(self.core_busy, Const(0, 28), self.core_enable,
                self.cache_enable, self.raster_enable))
        with m.If(self._enable.w_stb):
            m.d.sync += Cat(self.core_enable, self.cache_enable,
                            self.raster_enable).eq(self._enable.w_data)

        self._scratch.r_data.reset = 0x12345678
        with m.If(self._scratch.w_stb):
            m.d.sync += self._scratch.r_data.eq(self._scratch.w_data)

        with m.If(self._raster_tile_count.w_stb):
            m.d.sync += self._raster_tile_count.r_data.eq(
                self._raster_tile_count.w_data)
        m.d.comb += self.raster_tile_count.eq(self._raster_tile_count.r_data)

        with m.If(self._raster_tile_addr.w_stb):
            m.d.sync += self._raster_tile_addr.r_data.eq(
                self._raster_tile_addr.w_data)
        m.d.comb += self.raster_tile_addr.eq(self._raster_tile_addr.r_data)

        with m.If(self._raster_prim_addr.w_stb):
            m.d.sync += self._raster_prim_addr.r_data.eq(
                self._raster_prim_addr.w_data)
        m.d.comb += self.raster_prim_addr.eq(self._raster_prim_addr.r_data)

        with m.If(self._raster_prim_stride.w_stb):
            m.d.sync += self._raster_prim_stride.r_data.eq(
                self._raster_prim_stride.w_data)
        m.d.comb += self.raster_prim_stride.eq(self._raster_prim_stride.r_data)

        return m
