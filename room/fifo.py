from amaranth import *
from amaranth.lib.fifo import FIFOInterface


def _incr(signal, modulo):
    if modulo == 2**len(signal):
        return signal + 1
    else:
        return Mux(signal == modulo - 1, 0, signal + 1)


class SyncFIFO(Elaboratable, FIFOInterface):

    def __init__(self, *, width, depth, fwft=True):
        super().__init__(width=width, depth=depth, fwft=fwft)

        self.flush = Signal()
        self.level = Signal(range(depth + 1))

    def elaborate(self, platform):
        m = Module()
        if self.depth == 0:
            m.d.comb += [
                self.w_rdy.eq(0),
                self.r_rdy.eq(0),
            ]
            return m

        m.d.comb += [
            self.w_rdy.eq(self.level != self.depth),
            self.r_rdy.eq(self.level != 0),
            self.w_level.eq(self.level),
            self.r_level.eq(self.level),
        ]

        do_read = self.r_rdy & self.r_en
        do_write = self.w_rdy & self.w_en

        storage = Memory(width=self.width, depth=self.depth)
        w_port = m.submodules.w_port = storage.write_port()
        r_port = m.submodules.r_port = storage.read_port(
            domain="comb" if self.fwft else "sync", transparent=self.fwft)
        produce = Signal(range(self.depth))
        consume = Signal(range(self.depth))

        m.d.comb += [
            w_port.addr.eq(produce),
            w_port.data.eq(self.w_data),
            w_port.en.eq(self.w_en & self.w_rdy),
        ]
        with m.If(self.flush):
            m.d.sync += produce.eq(0)
        with m.Elif(do_write):
            m.d.sync += produce.eq(_incr(produce, self.depth))

        m.d.comb += [
            r_port.addr.eq(consume),
            self.r_data.eq(r_port.data),
        ]
        if not self.fwft:
            m.d.comb += r_port.en.eq(self.r_en)
        with m.If(self.flush):
            m.d.sync += consume.eq(0)
        with m.Elif(do_read):
            m.d.sync += consume.eq(_incr(consume, self.depth))

        with m.If(self.flush):
            m.d.sync += self.level.eq(0)
        with m.Elif(do_write & ~do_read):
            m.d.sync += self.level.eq(self.level + 1)
        with m.Elif(do_read & ~do_write):
            m.d.sync += self.level.eq(self.level - 1)

        return m
