from amaranth import *

from room.consts import *
from room.branch import BranchUpdate


def generate_imm(ip, sel):
    sign = ip[-1]
    i20_30 = Mux(sel == ImmSel.U, ip[8:19], Repl(sign, 11))
    i12_19 = Mux((sel == ImmSel.U) | (sel == ImmSel.J), ip[0:8], Repl(sign, 8))
    i11 = Mux(sel == ImmSel.U, 0,
              Mux((sel == ImmSel.J) | (sel == ImmSel.B), ip[8], sign))
    i5_10 = Mux(sel == ImmSel.U, 0, ip[14:19])
    i1_4 = Mux(sel == ImmSel.U, 0, ip[9:14])
    i0 = Mux((sel == ImmSel.S) | (sel == ImmSel.I), ip[8], 0)
    return Cat(i0, i1_4, i5_10, i11, i12_19, i20_30, sign)


def generate_imm_type(ip):
    return ip[8:10]


def _incr(signal, modulo):
    if modulo == 2**len(signal):
        return (signal + 1)[:len(signal)]
    else:
        return Mux(signal == modulo - 1, 0, signal + 1)


class BranchKillableFIFO(Elaboratable):

    def __init__(self, entries, data_width, params, flow=True):
        self.entries = entries
        self.data_width = data_width
        self.flow = flow
        max_br_count = params['max_br_count']

        self.w_data = Signal(data_width)
        self.w_br_mask = Signal(max_br_count)
        self.w_en = Signal()
        self.w_rdy = Signal()

        self.r_data = Signal(data_width)
        self.r_br_mask = Signal(max_br_count)
        self.r_en = Signal()
        self.r_rdy = Signal()

        self.br_update = BranchUpdate(params)
        self.flush = Signal()

    def elaborate(self, platform):
        m = Module()

        mem = Array(
            Signal.like(self.w_data, name=f'mem{i}')
            for i in range(self.entries))
        valids = Array(Signal(name=f'valid{i}') for i in range(self.entries))
        br_masks = Array(
            Signal.like(self.w_br_mask, name=f'br_mask{i}')
            for i in range(self.entries))

        w_ptr = Signal(range(self.entries))
        r_ptr = Signal(range(self.entries))
        maybe_full = Signal()

        empty = (r_ptr == w_ptr) & ~maybe_full
        full = (r_ptr == w_ptr) & maybe_full

        do_enq = Signal()
        do_deq = Signal()

        m.d.comb += [
            do_enq.eq(self.w_en & self.w_rdy),
            do_deq.eq((self.r_en | ~valids[r_ptr])
                      & self.r_rdy),
        ]

        for i in range(self.entries):
            m.d.sync += valids[i].eq(
                valids[i] & ~self.br_update.br_mask_killed(br_masks[i])
                & ~self.flush)

            with m.If(valids[i]):
                m.d.sync += br_masks[i].eq(
                    self.br_update.get_new_br_mask(br_masks[i]))

        with m.If(do_enq):
            m.d.sync += [
                mem[w_ptr].eq(self.w_data),
                br_masks[w_ptr].eq(
                    self.br_update.get_new_br_mask(self.w_br_mask)),
                valids[w_ptr].eq(1),
                w_ptr.eq(_incr(w_ptr, self.entries)),
            ]

        with m.If(do_deq):
            m.d.sync += [
                valids[r_ptr].eq(0),
                r_ptr.eq(_incr(r_ptr, self.entries)),
            ]

        with m.If(do_enq ^ do_deq):
            m.d.sync += maybe_full.eq(do_enq)

        m.d.comb += self.w_rdy.eq(~full)

        m.d.comb += [
            self.r_rdy.eq(~empty & valids[r_ptr]
                          & ~self.br_update.br_mask_killed(br_masks[r_ptr])
                          & ~self.flush),
            self.r_data.eq(mem[r_ptr]),
            self.r_br_mask.eq(self.br_update.get_new_br_mask(br_masks[r_ptr])),
        ]

        if self.flow:
            with m.If(empty):
                m.d.comb += [
                    self.r_rdy.eq(self.w_en),
                    self.r_data.eq(self.w_data),
                    self.r_br_mask.eq(
                        self.br_update.get_new_br_mask(self.w_br_mask)),
                    do_deq.eq(0),
                ]

                with m.If(self.r_en):
                    m.d.comb += do_enq.eq(0)

        return m


class Pipe(Elaboratable):

    def __init__(self, width, depth=1):
        self.width = width
        self.depth = depth

        self.in_valid = Signal()
        self.in_data = Signal(width)

        self.out_valid = Signal()
        self.out_data = Signal(width)

    def elaborate(self, platform):
        m = Module()

        if self.depth == 0:
            m.d.comb += [
                self.out_valid.eq(self.in_valid),
                self.out_data.eq(self.in_data),
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
                self.out_valid.eq(valids[-1]),
                self.out_data.eq(data[-1]),
            ]

        return m


class Arbiter(Elaboratable):

    def __init__(self, width, n):
        self.n = n

        self.in_data = [Signal(width, name=f'in{i}') for i in range(n)]
        self.in_valid = Signal(n)
        self.in_ready = Signal(n)

        self.out_data = Signal(width)
        self.out_valid = Signal()
        self.out_ready = Signal()

    def elaborate(self, platform):
        m = Module()

        m.d.comb += self.out_valid.eq(0)

        for i in reversed(range(self.n)):
            with m.If(self.in_valid[i]):
                m.d.comb += [
                    self.out_valid.eq(1),
                    self.out_data.eq(self.in_data[i]),
                ]

        ready = 1
        for i in range(self.n):
            m.d.comb += self.in_ready[i].eq(self.out_ready & ready)

            ready &= ~(self.in_valid[i] & self.in_ready[i])

        return m
