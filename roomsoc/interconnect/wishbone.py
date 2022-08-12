from amaranth import *
from amaranth_soc.wishbone.bus import *


class DownConverter(Elaboratable):

    def __init__(self, master, slave):
        self.master = master
        self.slave = slave

    def elaborate(self, platform):
        m = Module()

        dw_from = self.master.data_width
        dw_to = self.slave.data_width
        ratio = dw_from // dw_to

        counter = Signal(range(ratio))
        skip = Signal()

        with m.FSM():
            with m.State('IDLE'):
                m.d.sync += counter.eq(0)

                with m.If(self.master.cyc & self.master.stb):
                    m.next = 'CONVERT'

            with m.State('CONVERT'):
                m.d.comb += self.slave.adr.eq(Cat(counter, self.master.adr))

                for i in range(ratio):
                    with m.If(counter == i):
                        m.d.comb += self.slave.sel.eq(
                            self.master.sel[i * dw_to // 8:])

                    with m.If(~self.master.cyc):
                        m.d.comb += self.slave.cyc.eq(0)
                        m.next = 'IDLE'
                    with m.Elif(self.master.cyc & self.master.stb):
                        m.d.comb += [
                            skip.eq(self.slave.sel == 0),
                            self.slave.we.eq(self.master.we),
                            self.slave.cyc.eq(~skip),
                            self.slave.stb.eq(~skip),
                        ]

                        with m.If(self.slave.ack | skip):
                            m.d.sync += counter.eq(counter + 1)

                            with m.If(counter == (ratio - 1)):
                                m.d.comb += self.master.ack.eq(1)
                                m.next = 'IDLE'

        for i in range(ratio):
            with m.If(counter == i):
                m.d.comb += self.slave.dat_w.eq(self.master.dat_w[i * dw_to:])

        dat_r = Signal(dw_from)
        m.d.comb += self.master.dat_r.eq(Cat(dat_r[dw_to:], self.slave.dat_r))
        with m.If(self.slave.ack | skip):
            m.d.sync += dat_r.eq(self.master.dat_r)

        return m


class UpConverter(Elaboratable):

    def __init__(self, master, slave):
        self.master = master
        self.slave = slave

    def elaborate(self, platform):
        m = Module()

        dw_from = self.master.data_width
        dw_to = self.slave.data_width
        ratio = dw_to // dw_from
        lsb_width = Shape.cast(range(ratio)).width

        m.d.comb += self.master.connect(
            self.slave, exclude={'adr', 'sel', 'dat_r', 'dat_w'})

        for i in range(ratio):
            with m.If(self.master.adr[:lsb_width] == i):
                m.d.comb += [
                    self.slave.adr.eq(self.master.adr[lsb_width:]),
                    self.slave.sel[i * dw_from // 8:(i + 1) * dw_from // 8].eq(
                        self.master.sel),
                    self.slave.dat_w[i * dw_from:(i + 1) * dw_from].eq(
                        self.master.dat_w),
                    self.master.dat_r.eq(self.slave.dat_r[i * dw_from:(i + 1) *
                                                          dw_from]),
                ]

        return m


class Converter(Elaboratable):

    def __init__(self, master, slave):
        self.master = master
        self.slave = slave

    def elaborate(self, platform):
        m = Module()

        dw_from = self.master.data_width
        dw_to = self.slave.data_width

        if dw_from > dw_to:
            m.submodules.downconverter = DownConverter(self.master, self.slave)
        elif dw_from < dw_to:
            m.submodules.upconverter = UpConverter(self.master, self.slave)
        else:
            m.d.comb += self.master.connect(self.slave)

        return m


class SRAM(Elaboratable):

    def __init__(self, mem_or_size, bus=None):
        self.mem_or_size = mem_or_size

        if bus is None:
            bus = Interface()

        self.bus = bus

    def elaborate(self, platform):
        m = Module()

        bus_data_width = self.bus.data_width

        if isinstance(self.mem_or_size, Memory):
            mem = self.mem_or_size
        else:
            mem = Memory(width=bus_data_width,
                         depth=self.mem_or_size // (bus_data_width >> 3))

        read_port = m.submodules.read_port = mem.read_port()
        write_port = m.submodules.write_port = mem.write_port(granularity=8)

        m.d.comb += [
            write_port.en[i].eq(self.bus.cyc & self.bus.stb & self.bus.we
                                & self.bus.sel[i])
            for i in range(bus_data_width // 8)
        ]

        m.d.comb += [
            read_port.addr.eq(self.bus.adr[:len(read_port.addr)]),
            write_port.addr.eq(self.bus.adr[:len(write_port.addr)])
        ]

        m.d.comb += [
            self.bus.dat_r.eq(read_port.data),
            write_port.data.eq(self.bus.dat_w),
        ]

        m.d.sync += [
            self.bus.ack.eq(self.bus.cyc & self.bus.stb & ~self.bus.ack)
        ]

        return m
