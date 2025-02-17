from amaranth import *

from .peripheral import Peripheral


class CLINT(Peripheral, Elaboratable):

    def __init__(self, *, name=None, n_harts=1):
        super().__init__(name=name)

        self.n_harts = n_harts
        self._time_width = 64

        bank = self.csr_bank()
        self._msip = [
            bank.csr(32, 'rw', addr=i * 4, name=f'msip{i}')
            for i in range(self.n_harts)
        ]
        self._mtimecmp = [
            bank.csr(self._time_width,
                     'rw',
                     addr=0x4000 + i * 8,
                     name=f'mtimecmp{i}') for i in range(self.n_harts)
        ]
        self._mtime = bank.csr(self._time_width, 'rw', addr=0xbff8)

        self._bridge = self.bridge(data_width=32, granularity=8, alignment=2)
        self.bus = self._bridge.bus

        self.rtc_tick = Signal()

        self.msip = Signal(self.n_harts)
        self.mtip = Signal(self.n_harts)

    def elaborate(self, platform):
        m = Module()
        m.submodules.bridge = self._bridge

        time = Signal(self._time_width)
        with m.If(self.rtc_tick):
            m.d.sync += time.eq(time + 1)

        for i, msip in enumerate(self._msip):
            with m.If(msip.w_stb):
                m.d.sync += msip.r_data.eq(msip.w_data[0])

            m.d.comb += self.msip[i].eq(msip.r_data[0])

        for i, timecmp in enumerate(self._mtimecmp):
            with m.If(timecmp.w_stb):
                m.d.sync += timecmp.r_data.eq(timecmp.w_data)

            m.d.comb += self.mtip[i].eq(time >= timecmp.r_data)

        with m.If(self._mtime.w_stb):
            m.d.sync += time.eq(self._mtime.w_data)
        m.d.comb += self._mtime.r_data.eq(time)

        return m
