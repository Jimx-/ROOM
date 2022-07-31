from amaranth import *
from amaranth.hdl.rec import DIR_FANIN, DIR_FANOUT


def make_wishbone_layout(data_width, adr_width):
    sel_width = data_width // 8
    return [("adr", adr_width, DIR_FANOUT), ("dat_w", data_width, DIR_FANOUT),
            ("dat_r", data_width, DIR_FANIN), ("sel", sel_width, DIR_FANOUT),
            ("cyc", 1, DIR_FANOUT), ("stb", 1, DIR_FANOUT),
            ("ack", 1, DIR_FANIN), ("we", 1, DIR_FANOUT),
            ("cti", 3, DIR_FANOUT), ("bte", 2, DIR_FANOUT),
            ("err", 1, DIR_FANIN)]


class Interface(Record):

    def __init__(self, data_width=32, adr_width=30, name=None):
        self.adr_width = adr_width
        self.data_width = data_width
        super().__init__(make_wishbone_layout(data_width=data_width,
                                              adr_width=adr_width),
                         name=name)
        self.adr.reset_less = True
        self.dat_w.reset_less = True
        self.dat_r.reset_less = True
        self.sel.reset_less = True

    @staticmethod
    def like(other):
        return Interface(other.data_width)

    def _do_transaction(self):
        yield self.cyc.eq(1)
        yield self.stb.eq(1)
        yield
        while not (yield self.ack):
            yield
        yield self.cyc.eq(0)
        yield self.stb.eq(0)

    def write(self, adr, dat, sel=None, cti=None, bte=None):
        if sel is None:
            sel = 2**len(self.sel) - 1
        yield self.adr.eq(adr)
        yield self.dat_w.eq(dat)
        yield self.sel.eq(sel)
        if cti is not None:
            yield self.cti.eq(cti)
        if bte is not None:
            yield self.bte.eq(bte)
        yield self.we.eq(1)
        yield from self._do_transaction()

    def read(self, adr, cti=None, bte=None):
        yield self.adr.eq(adr)
        yield self.we.eq(0)
        if cti is not None:
            yield self.cti.eq(cti)
        if bte is not None:
            yield self.bte.eq(bte)
        yield from self._do_transaction()
        return (yield self.dat_r)


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
