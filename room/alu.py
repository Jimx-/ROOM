from amaranth import *
from amaranth import tracer

from room.consts import *

from roomsoc.interconnect.stream import Valid, Decoupled


class ALU(Elaboratable):

    def __init__(self, width):
        self.width = width

        self.fn = Signal(ALUOperator)
        self.dw = Signal(ALUWidth)
        self.in1 = Signal(width)
        self.in2 = Signal(width)
        self.out = Signal(width)

    def elaborate(self, platform):
        m = Module()

        is_sub = self.fn[3]
        is_cmp = self.fn >= ALUOperator.SLT
        is_cmp_unsigned = self.fn[1]

        #
        # ADD, SUB
        #

        adder_out = Signal(self.width)
        in2_inv = Mux(is_sub == 1, ~self.in2, self.in2)
        inv = Signal(self.width)
        m.d.comb += inv.eq(in2_inv)
        m.d.comb += adder_out.eq(self.in1 + in2_inv + is_sub)

        #
        # SLT, SLTU
        #

        in1_xor_in2 = self.in1 ^ in2_inv
        slt = Mux(
            self.in1[self.width - 1] == self.in2[self.width - 1],
            adder_out[self.width - 1],
            Mux(is_cmp_unsigned, self.in2[self.width - 1],
                self.in1[self.width - 1]))

        #
        # SLL, SRL, SRA
        #

        shin_r = Signal(self.width)

        if self.width == 32:
            shamt = self.in2[:5]
            m.d.comb += shin_r.eq(self.in1)
        else:
            shin_hi_32 = Repl(is_sub & self.in1[31], 32)
            shin_hi = Mux(self.dw == ALUWidth.DW_XLEN, self.in1[32:64],
                          shin_hi_32)
            shamt = Cat(self.in2[:5],
                        self.in2[5] & (self.dw == ALUWidth.DW_XLEN))
            m.d.comb += shin_r.eq(Cat(self.in1[:32], shin_hi))

        shin_l = Signal(self.width)
        for a, b in zip(shin_l, reversed(shin_r)):
            m.d.comb += a.eq(b)

        shin = Mux((self.fn == ALUOperator.SR) | (self.fn == ALUOperator.SRA),
                   shin_r, shin_l)

        shout_r = Signal(self.width)
        shout_l = Signal(self.width)
        m.d.comb += shout_r.eq(
            Cat(shin, is_sub & shin[self.width - 1]).as_signed() >> shamt)
        for a, b in zip(shout_l, reversed(shout_r)):
            m.d.comb += a.eq(b)
        shout = Mux((self.fn == ALUOperator.SR) | (self.fn == ALUOperator.SRA),
                    shout_r, 0) | Mux(self.fn == ALUOperator.SL, shout_l, 0)

        #
        # AND, OR, XOR
        #

        logic = Mux((self.fn == ALUOperator.XOR) |
                    (self.fn == ALUOperator.OR), in1_xor_in2, 0) | Mux(
                        (self.fn == ALUOperator.AND) |
                        (self.fn == ALUOperator.OR), self.in1 & self.in2, 0)

        shift_logic = (is_cmp & slt) | logic | shout

        out = Signal.like(self.out)
        m.d.comb += out.eq(
            Mux((self.fn == ALUOperator.ADD) | (self.fn == ALUOperator.SUB),
                adder_out, shift_logic))

        m.d.comb += self.out.eq(out)
        if self.width > 32:
            with m.If(self.dw == ALUWidth.DW_32):
                m.d.comb += self.out.eq(Cat(out[:32], Repl(out[31], 32)))

        return m


class MulDivReq(Record):

    def __init__(self, width, name=None, src_loc_at=0):
        super().__init__([
            ('fn', Shape.cast(ALUOperator).width),
            ('dw', Shape.cast(ALUWidth).width),
            ('in1', width),
            ('in2', width),
        ],
                         name=name,
                         src_loc_at=1 + src_loc_at)


class Multiplier(Elaboratable):

    def __init__(self, width, latency):
        self.width = width
        self.latency = latency

        self.req = Valid(MulDivReq, width)

        self.resp_data = Signal(width)

    def elaborate(self, platform):
        m = Module()

        in_req = MulDivReq(self.width, name='in')
        in_valid = Signal()
        m.d.sync += [
            in_req.eq(self.req.bits),
            in_valid.eq(self.req.valid),
        ]

        fn = in_req.fn
        h = (fn == ALUOperator.MULH) | (fn == ALUOperator.MULHU) | (
            fn == ALUOperator.MULHSU)
        half = (self.width > 32) & (in_req.dw == ALUWidth.DW_32)
        lhs_signed = (fn == ALUOperator.MULH) | (fn == ALUOperator.MULHSU)
        rhs_signed = (fn == ALUOperator.MULH)

        lhs = Cat(in_req.in1, (lhs_signed & in_req.in1[-1])).as_signed()
        rhs = Cat(in_req.in2, (rhs_signed & in_req.in2[-1])).as_signed()
        prod = lhs * rhs
        half_sext = Cat(prod[:self.width // 2],
                        Repl(prod[self.width // 2 - 1], self.width // 2))
        muxed = Mux(h, prod[self.width:self.width * 2],
                    Mux(half, half_sext, prod[:self.width]))

        valid = in_valid
        data = muxed

        for _ in range(self.latency - 1):
            next_valid = Signal()
            next_data = Signal.like(data)

            m.d.sync += next_valid.eq(valid)

            with m.If(valid):
                m.d.sync += next_data.eq(data)

            valid = next_valid
            data = next_data

        m.d.comb += self.resp_data.eq(data)

        return m


class IntDiv(Elaboratable):

    def __init__(self, width):
        self.width = width

        self.req = Decoupled(MulDivReq, width)
        self.resp = Decoupled(Signal, width)

        self.kill = Signal()

    def elaborate(self, platform):
        m = Module()

        req = MulDivReq(self.width, name='in')
        count = Signal(range(self.width + 1))
        neg_out = Signal()
        divisor = Signal(self.width)
        remainder = Signal(2 * self.width + 1)

        fn = self.req.bits.fn
        h = (fn == ALUOperator.REM) | (fn == ALUOperator.REMU)
        half_width = (self.width > 32) & (self.req.bits.dw == ALUWidth.DW_32)
        lhs_signed = (fn == ALUOperator.DIV) | (fn == ALUOperator.REM)
        rhs_signed = lhs_signed

        def sext(x, signed):
            sign = Mux(half_width, x[self.width // 2 - 1], x[-1]) & signed
            hi = Mux(half_width, Repl(sign, self.width // 2),
                     x[self.width // 2:])
            return Cat(x[:self.width // 2], hi), sign

        lhs_in, lhs_sign = sext(self.req.bits.in1, lhs_signed)
        rhs_in, rhs_sign = sext(self.req.bits.in2, rhs_signed)

        is_hi = Signal()
        res_hi = Signal()
        result = Mux(res_hi, remainder[self.width + 1:],
                     remainder[:self.width])

        with m.FSM():
            with m.State('IDLE'):
                m.d.comb += self.req.ready.eq(1)

                with m.If(self.req.fire & ~self.kill):
                    m.d.sync += [
                        req.eq(self.req.bits),
                        neg_out.eq(Mux(h, lhs_sign, lhs_sign ^ rhs_sign)),
                        count.eq(0),
                        divisor.eq(Cat(rhs_in, rhs_sign)),
                        remainder.eq(lhs_in),
                        is_hi.eq(h),
                        res_hi.eq(0),
                    ]

                    with m.If(lhs_sign | rhs_sign):
                        m.next = 'NEG_IN'
                    with m.Else():
                        m.next = 'DIV'

            with m.State('NEG_IN'):
                with m.If(self.kill):
                    m.next = 'IDLE'
                with m.Else():
                    with m.If(remainder[self.width - 1]):
                        m.d.sync += remainder[:self.width].eq(
                            -remainder[:self.width])
                    with m.If(divisor[self.width - 1]):
                        m.d.sync += divisor.eq(-divisor)
                    m.next = 'DIV'

            with m.State('DIV'):
                with m.If(self.kill):
                    m.next = 'IDLE'
                with m.Else():
                    d = remainder[self.width:2 * self.width +
                                  1] - divisor[:self.width]
                    less = d[self.width]
                    m.d.sync += [
                        remainder.eq(
                            Cat(
                                ~less, remainder[:self.width],
                                Mux(less, remainder[self.width:2 * self.width],
                                    d[:self.width]))),
                        count.eq(count + 1),
                    ]

                    with m.If(count == self.width):
                        m.d.sync += res_hi.eq(is_hi)

                        with m.If(neg_out):
                            m.next = 'NEG_OUT'
                        with m.Else():
                            m.next = 'DIV_DONE'

                    with m.If((count == 0) & (divisor == 0) & ~is_hi):
                        m.d.sync += neg_out.eq(0)

            with m.State('NEG_OUT'):
                with m.If(self.kill):
                    m.next = 'IDLE'
                with m.Else():
                    m.d.sync += [
                        remainder.eq(-result),
                        res_hi.eq(0),
                    ]

                    m.next = 'DIV_DONE'

            with m.State('DIV_DONE'):
                m.d.comb += self.resp.valid.eq(~self.kill)

                with m.If(self.kill | self.resp.fire):
                    m.next = 'IDLE'

        result_lo = result[:self.width // 2]
        result_hi = Mux(half_width,
                        Repl(result[self.width // 2 - 1], self.width // 2),
                        result[self.width // 2:])
        m.d.comb += self.resp.bits.eq(Cat(result_lo, result_hi))

        return m


class AMODataGen(Elaboratable):

    def __init__(self, width):
        self.width = width

        self.mask = Signal(width // 8)
        self.cmd = Signal(MemoryCommand)
        self.lhs = Signal(width)
        self.rhs = Signal(width)
        self.out = Signal(width)

    def elaborate(self, platform):
        m = Module()

        out = self.rhs

        wmask = Signal(self.width)
        for i in range(len(self.mask)):
            m.d.comb += wmask[i * 8:(i + 1) * 8].eq(Repl(self.mask[i], 8))

        m.d.comb += self.out.eq(wmask & out | ~wmask & self.lhs)

        return m
