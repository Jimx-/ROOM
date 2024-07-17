from amaranth import *
from amaranth.lib.fifo import SyncFIFO
from amaranth.utils import log2_int

from .peripheral import Peripheral
from roomsoc.interconnect.stream import Decoupled


class WishboneDMAReader(Peripheral, Elaboratable):

    def __init__(self,
                 bus,
                 *,
                 name=None,
                 fifo_depth=16,
                 with_csr=False,
                 default_base=0,
                 default_length=0,
                 default_enable=0,
                 default_loop=0):
        super().__init__(name=name)

        self.wb_bus = bus
        self.fifo_depth = fifo_depth
        self.with_csr = with_csr

        if with_csr:
            self.default_base = default_base
            self.default_length = default_length
            self.default_enable = default_enable
            self.default_loop = default_loop

            bank = self.csr_bank()
            self._base = bank.csr(64, 'rw')
            self._length = bank.csr(32, 'rw')
            self._enable = bank.csr(1, 'rw')
            self._loop = bank.csr(1, 'rw')
            self._done = bank.csr(1, 'r')
            self._offset = bank.csr(32, 'r')

            self._bridge = self.bridge(data_width=32,
                                       granularity=8,
                                       alignment=2)
            self.bus = self._bridge.bus

        self.sink = Decoupled(Record, [("address", bus.addr_width),
                                       ("last", 1)])
        self.source = Decoupled(Record, [("data", bus.data_width),
                                         ("last", 1)])

    def elaborate(self, platform):
        m = Module()

        if self.with_csr:
            m.submodules.bridge = self._bridge

        fifo = m.submodules.fifo = SyncFIFO(depth=self.fifo_depth,
                                            width=self.wb_bus.data_width + 1)

        m.d.comb += [
            self.wb_bus.stb.eq(self.sink.valid & fifo.w_rdy),
            self.wb_bus.cyc.eq(self.sink.valid & fifo.w_rdy),
            self.wb_bus.we.eq(0),
            self.wb_bus.sel.eq(2**(self.wb_bus.data_width // 8) - 1),
            self.wb_bus.adr.eq(self.sink.bits.address),
            fifo.w_data.eq(Cat(self.wb_bus.dat_r, self.sink.bits.last)),
        ]

        with m.If(self.wb_bus.stb & self.wb_bus.ack):
            m.d.comb += [
                self.sink.ready.eq(1),
                fifo.w_en.eq(1),
            ]

        m.d.comb += [
            self.source.bits.eq(fifo.r_data),
            self.source.valid.eq(fifo.r_rdy),
            fifo.r_en.eq(self.source.ready),
        ]

        if self.with_csr:
            base = Signal(self.wb_bus.addr_width)
            length = Signal(self.wb_bus.addr_width)
            offset = Signal(32)

            self._base.r_data.reset = self.default_base
            self._length.r_data.reset = self.default_length
            self._enable.r_data.reset = self.default_enable
            self._loop.r_data.reset = self.default_loop

            with m.If(self._base.w_stb):
                m.d.sync += self._base.r_data.eq(self._base.w_data)
            with m.If(self._length.w_stb):
                m.d.sync += self._length.r_data.eq(self._length.w_data)
            with m.If(self._enable.w_stb):
                m.d.sync += self._enable.r_data.eq(self._enable.w_data)
            with m.If(self._loop.w_stb):
                m.d.sync += self._loop.r_data.eq(self._loop.w_data)

            shift = log2_int(self.wb_bus.data_width // 8)
            m.d.comb += [
                base.eq(self._base.r_data[shift:]),
                length.eq(self._length.r_data[shift:]),
                self._offset.r_data.eq(Cat(Const(0, shift), offset)),
            ]

            with m.FSM():
                with m.State('IDLE'):
                    m.d.sync += offset.eq(0)
                    with m.If(self._enable.r_data):
                        m.next = 'RUN'

                with m.State('RUN'):
                    with m.If(~self._enable.r_data):
                        m.next = 'IDLE'
                    with m.Else():
                        m.d.comb += [
                            self.sink.valid.eq(1),
                            self.sink.bits.last.eq(offset == (length - 1)),
                            self.sink.bits.address.eq(base + offset),
                        ]

                        with m.If(self.sink.ready):
                            m.d.sync += offset.eq(offset + 1)
                            with m.If(self.sink.bits.last):
                                with m.If(self._loop.r_data):
                                    m.d.sync += offset.eq(0)
                                with m.Else():
                                    m.next = 'IDLE'

        return m


class WishboneDMAWriter(Peripheral, Elaboratable):

    def __init__(self,
                 bus,
                 *,
                 name=None,
                 with_csr=False,
                 default_base=0,
                 default_length=0,
                 default_enable=0,
                 default_loop=0):
        super().__init__(name=name)

        self.wb_bus = bus
        self.with_csr = with_csr

        self._sink = Decoupled(Record, [("address", bus.addr_width),
                                        ("data", bus.data_width), ("last", 1)])
        self.sink = self._sink

        if with_csr:
            self.default_base = default_base
            self.default_length = default_length
            self.default_enable = default_enable
            self.default_loop = default_loop

            bank = self.csr_bank()
            self._base = bank.csr(64, 'rw')
            self._length = bank.csr(32, 'rw')
            self._enable = bank.csr(1, 'rw')
            self._loop = bank.csr(1, 'rw')
            self._done = bank.csr(1, 'r')
            self._offset = bank.csr(32, 'r')

            self._bridge = self.bridge(data_width=32,
                                       granularity=8,
                                       alignment=2)
            self.bus = self._bridge.bus

            self.sink = Decoupled(Record, [("data", bus.data_width),
                                           ("last", 1)])

    def elaborate(self, platform):
        m = Module()

        if self.with_csr:
            m.submodules.bridge = self._bridge

        m.d.comb += [
            self.wb_bus.stb.eq(self._sink.valid),
            self.wb_bus.cyc.eq(self._sink.valid),
            self.wb_bus.we.eq(1),
            self.wb_bus.sel.eq(2**(self.wb_bus.data_width // 8) - 1),
            self.wb_bus.adr.eq(self._sink.bits.address),
            self.wb_bus.dat_w.eq(self._sink.bits.data),
            self._sink.ready.eq(self.wb_bus.ack),
        ]

        if self.with_csr:
            base = Signal(self.wb_bus.addr_width)
            length = Signal(self.wb_bus.addr_width)
            offset = Signal(32)

            self._base.r_data.reset = self.default_base
            self._length.r_data.reset = self.default_length
            self._enable.r_data.reset = self.default_enable
            self._loop.r_data.reset = self.default_loop

            with m.If(self._base.w_stb):
                m.d.sync += self._base.r_data.eq(self._base.w_data)
            with m.If(self._length.w_stb):
                m.d.sync += self._length.r_data.eq(self._length.w_data)
            with m.If(self._enable.w_stb):
                m.d.sync += self._enable.r_data.eq(self._enable.w_data)
            with m.If(self._loop.w_stb):
                m.d.sync += self._loop.r_data.eq(self._loop.w_data)

            shift = log2_int(self.wb_bus.data_width // 8)
            m.d.comb += [
                base.eq(self._base.r_data[shift:]),
                length.eq(self._length.r_data[shift:]),
                self._offset.r_data.eq(Cat(Const(0, shift), offset)),
            ]

            with m.FSM():
                with m.State('IDLE'):
                    m.d.sync += offset.eq(0)
                    with m.If(self._enable.r_data):
                        m.next = 'RUN'

                with m.State('RUN'):
                    with m.If(~self._enable.r_data):
                        m.next = 'IDLE'
                    with m.Else():
                        m.d.comb += [
                            self._sink.valid.eq(self.sink.valid),
                            self._sink.bits.last.eq(self.sink.bits.last
                                                    | (offset == (length -
                                                                  1))),
                            self._sink.bits.address.eq(base + offset),
                            self._sink.bits.data.eq(self.sink.bits.data),
                            self.sink.ready.eq(self._sink.ready),
                        ]

                        with m.If(self.sink.fire):
                            m.d.sync += offset.eq(offset + 1)
                            with m.If(self.sink.bits.last):
                                with m.If(self._loop.r_data):
                                    m.d.sync += offset.eq(0)
                                with m.Else():
                                    m.next = 'IDLE'

        return m
