from amaranth import *
from amaranth import tracer
from amaranth.lib.fifo import AsyncFIFO

import math


class Valid:

    def __init__(self, cls, *args, name=None, src_loc_at=0, **kwargs):
        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.bits = cls(name=f'{name}_bits', *args, **kwargs)
        self.valid = Signal(name=f'{name}_valid')

    @property
    def fire(self):
        return self.valid

    def eq(self, rhs):
        return [
            self.bits.eq(rhs.bits),
            self.valid.eq(rhs.valid),
        ]


class Decoupled:

    def __init__(self, cls, *args, name=None, src_loc_at=0, **kwargs):
        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.bits = cls(name=f'{name}_bits', *args, **kwargs)
        self.valid = Signal(name=f'{name}_valid')
        self.ready = Signal(name=f'{name}_ready')

    @property
    def fire(self):
        return self.valid & self.ready

    def connect(self, subord):
        stmts = []

        if getattr(self.bits, 'connect', None) is not None:
            stmts += self.bits.connect(subord.bits)
        else:
            stmts += [subord.bits.eq(self.bits)]

        stmts.append(subord.valid.eq(self.valid))
        stmts.append(self.ready.eq(subord.ready))

        return stmts


class ClockDomainCrossing(Elaboratable):

    def __init__(self,
                 cls,
                 *args,
                 from_domain='sync',
                 to_domain='sync',
                 depth=8,
                 **kwargs):
        self.from_domain = from_domain
        self.to_domain = to_domain
        self.depth = depth

        self.sink = Decoupled(cls, *args, **kwargs)
        self.source = Decoupled(cls, *args, **kwargs)

    def elaborate(self, platform):
        m = Module()

        if self.from_domain == self.to_domain:
            m.d.comb += self.sink.connect(self.source)
        else:
            cdc_param = dict(width=len(self.sink.bits),
                             depth=self.depth,
                             r_domain=self.to_domain,
                             w_domain=self.from_domain)

            if platform is None or getattr(
                    platform, 'get_async_fifo', None) is None or not callable(
                        getattr(platform, 'get_async_fifo')):
                cdc = m.submodules.cdc = AsyncFIFO(**cdc_param)
            else:
                cdc = m.submodules.cdc = platform.get_async_fifo(**cdc_param)

            m.d.comb += [
                cdc.w_data.eq(self.sink.bits),
                cdc.w_en.eq(self.sink.valid),
                self.sink.ready.eq(cdc.w_rdy),
                self.source.bits.eq(cdc.r_data),
                self.source.valid.eq(cdc.r_rdy),
                cdc.r_en.eq(self.source.ready),
            ]

        return m


def wrap_incr(signal, modulo):
    if modulo == 2**len(signal):
        return (signal + 1)[:len(signal)]
    else:
        return Mux(signal == modulo - 1, 0, signal + 1)


class Gearbox(Elaboratable):

    def __init__(self, i_dw, o_dw, msb_first=True):
        self.i_dw = i_dw
        self.o_dw = o_dw
        self.msb_first = msb_first

        self.sink = Decoupled(Signal, i_dw)
        self.source = Decoupled(Signal, o_dw)

    def elaborate(self, platform):
        m = Module()

        io_lcm = (self.i_dw * self.o_dw) // math.gcd(self.i_dw, self.o_dw)

        if (io_lcm // self.i_dw) < 2:
            io_lcm *= 2
        if (io_lcm // self.o_dw) < 2:
            io_lcm *= 2

        level = Signal(range(io_lcm))
        enq = Signal()
        w_count = Signal(range(io_lcm // self.i_dw))
        deq = Signal()
        r_count = Signal(range(io_lcm // self.o_dw))

        m.d.comb += [
            self.sink.ready.eq(level < (io_lcm - self.i_dw)),
            self.source.valid.eq(level >= self.o_dw),
            enq.eq(self.sink.fire),
            deq.eq(self.source.fire),
        ]

        with m.If(enq):
            m.d.sync += w_count.eq(wrap_incr(w_count, io_lcm // self.i_dw))
        with m.If(deq):
            m.d.sync += r_count.eq(wrap_incr(r_count, io_lcm // self.o_dw))

        with m.If(enq & ~deq):
            m.d.sync += level.eq(level + self.i_dw)
        with m.Elif(~enq & deq):
            m.d.sync += level.eq(level - self.o_dw)
        with m.Elif(enq & deq):
            m.d.sync += level.eq(level + self.i_dw - self.o_dw)

        data = Signal(io_lcm, reset_less=True)

        w_data = Signal(self.i_dw)
        if self.msb_first:
            m.d.comb += w_data.eq(self.sink.bits)
        else:
            m.d.comb += w_data.eq(self.sink.bits[::-1])
        with m.If(self.sink.fire):
            with m.Switch(w_count):
                for i in range(io_lcm // self.i_dw):
                    with m.Case(i):
                        m.d.sync += data[io_lcm - self.i_dw * (i + 1):io_lcm -
                                         self.i_dw * i].eq(w_data)

        r_data = Signal(self.o_dw)
        with m.Switch(r_count):
            for i in range(io_lcm // self.o_dw):
                with m.Case(i):
                    m.d.comb += r_data.eq(data[io_lcm - self.o_dw *
                                               (i + 1):io_lcm - self.o_dw * i])

        if self.msb_first:
            m.d.comb += self.source.bits.eq(r_data)
        else:
            m.d.comb += self.source.bits.eq(r_data[::-1])

        return m
