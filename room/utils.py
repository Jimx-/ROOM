from amaranth import *

from room.consts import *

from roomsoc.interconnect.stream import Valid, Decoupled


def wrap_incr(signal, modulo):
    if modulo == 2**len(signal):
        return (signal + 1)[:len(signal)]
    else:
        return Mux(signal == modulo - 1, 0, signal + 1)


def wrap_decr(signal, modulo):
    if modulo == 2**len(signal):
        return (signal - 1)[:len(signal)]
    else:
        return Mux(signal == 0, modulo - 1, signal - 1)


def is_older(lhs, rhs, head):
    return (lhs < rhs) ^ (lhs < head) ^ (rhs < head)


def sign_extend(x, length):
    if len(x) == length:
        return x

    return Cat(x, x[-1].replicate(length - len(x)))


def bit_extend(x, length, signed):
    if len(x) == length:
        return x

    return Mux(signed, sign_extend(x, length), Cat(x,
                                                   Const(0, length - len(x))))


def generate_imm(ip, sel):
    sign = ip[-1]
    i20_30 = Mux(sel == ImmSel.U, ip[8:19], Repl(sign, 11))
    i12_19 = Mux((sel == ImmSel.U) | (sel == ImmSel.J), ip[0:8], Repl(sign, 8))
    i11 = Mux(sel == ImmSel.U, 0,
              Mux((sel == ImmSel.J) | (sel == ImmSel.B), ip[8], sign))
    i5_10 = Mux(sel == ImmSel.U, 0, ip[14:19])
    i1_4 = Mux(sel == ImmSel.U, 0, ip[9:14])
    i0 = Mux((sel == ImmSel.S) | (sel == ImmSel.I), ip[8], 0)
    return Mux(sel == ImmSel.V, Cat(ip[3:18], Const(0, 16)),
               Cat(i0, i1_4, i5_10, i11, i12_19, i20_30, sign))


def generate_imm_type(ip):
    return ip[8:10]


def generate_imm_rm(ip):
    return ip[:3]


class Pipe(Elaboratable):

    def __init__(self, width, depth=1):
        self.width = width
        self.depth = depth

        self.in_valid = Signal()
        self.in_data = Signal(width)

        self.out = Valid(Signal, width)

    def elaborate(self, platform):
        m = Module()

        if self.depth == 0:
            m.d.comb += [
                self.out.valid.eq(self.in_valid),
                self.out.bits.eq(self.in_data),
            ]

        else:
            valids = [Signal(name=f's{i}_valid') for i in range(self.depth)]
            data = [
                Signal.like(self.in_data, name=f's{i}_data')
                for i in range(self.depth)
            ]

            m.d.sync += valids[0].eq(self.in_valid)
            with m.If(self.in_valid):
                m.d.sync += data[0].eq(self.in_data)

            for i in range(1, self.depth):
                m.d.sync += valids[i].eq(valids[i - 1])
                with m.If(valids[i - 1]):
                    m.d.sync += data[i].eq(data[i - 1])

            m.d.comb += [
                self.out.valid.eq(valids[-1]),
                self.out.bits.eq(data[-1]),
            ]

        return m


class Arbiter(Elaboratable):

    def __init__(self, n, cls, *args, **kwargs):
        self.n = n

        self.inp = [
            Decoupled(cls, *args, **kwargs, name=f'in{i}') for i in range(n)
        ]

        self.out = Decoupled(cls, *args, **kwargs)

        self.chosen = Signal(range(n))

    def elaborate(self, platform):
        m = Module()

        m.d.comb += self.out.valid.eq(0)

        for i in reversed(range(self.n)):
            with m.If(self.inp[i].valid):
                m.d.comb += [
                    self.out.valid.eq(1),
                    self.out.bits.eq(self.inp[i].bits),
                    self.chosen.eq(i),
                ]

        ready = 1
        for i in range(self.n):
            m.d.comb += self.inp[i].ready.eq(self.out.ready & ready)

            ready &= ~(self.inp[i].fire)

        return m


class PopCount(Elaboratable):

    def __init__(self, n):
        self.n = n

        self.inp = Signal(n)
        self.out = Signal(range(n + 1))

    def elaborate(self, platform):
        m = Module()

        def subcount(x):
            count = Signal.like(self.out)

            match len(x):
                case 0:
                    m.d.comb += count.eq(0)
                case 1:
                    m.d.comb += count.eq(x)
                case _:
                    left = subcount(x[:len(x) // 2])
                    right = subcount(x[len(x) // 2:])
                    m.d.comb += count.eq(left + right)

            return count

        m.d.comb += self.out.eq(subcount(self.inp))

        return m
