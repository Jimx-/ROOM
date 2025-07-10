from amaranth import *

from room.consts import *
from room.utils import PopCount, sign_extend

from roomsoc.interconnect.stream import Valid, Decoupled


class ALU(Elaboratable):

    def __init__(self, width, use_zbb=False, use_zbs=False, use_zicond=False):
        self.width = width

        self.use_zbb = use_zbb
        self.use_zbs = use_zbs
        self.use_zicond = use_zicond

        self.fn = Signal(ALUOperator)
        self.dw = Signal(ALUWidth)
        self.in1 = Signal(width)
        self.in2 = Signal(width)
        self.out = Signal(width)

    def elaborate(self, platform):
        m = Module()

        is_sub = self.fn[3]
        is_cmp = (self.fn >= ALUOperator.SLT) & (self.fn <= ALUOperator.SGEU)
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
        in1_and_in2 = self.in1 & in2_inv
        slt = Mux(
            self.in1[self.width - 1] == self.in2[self.width - 1],
            adder_out[self.width - 1],
            Mux(is_cmp_unsigned, self.in2[self.width - 1],
                self.in1[self.width - 1]))
        cmp_out = self.fn[0] ^ Mux(self.fn[3], slt, ~in1_xor_in2.any())

        #
        # SLL, SRL, SRA
        #

        shin_r = Signal(self.width)

        if self.width == 32:
            shamt = self.in2[:5]
            m.d.comb += shin_r.eq(self.in1)
        else:
            shin_hi_32 = (is_sub & self.in1[31]).replicate(32)
            shin_hi = Mux(self.dw == ALUWidth.DW_XLEN, self.in1[32:64],
                          shin_hi_32)
            shamt = Cat(self.in2[:5],
                        self.in2[5] & (self.dw == ALUWidth.DW_XLEN))
            m.d.comb += shin_r.eq(Cat(self.in1[:32], shin_hi))

        shin_l = Signal(self.width)
        for a, b in zip(shin_l, reversed(shin_r)):
            m.d.comb += a.eq(b)

        shin = Mux(
            (self.fn == ALUOperator.SR) | (self.fn == ALUOperator.SRA) |
            (self.fn == ALUOperator.ROR) | (self.fn == ALUOperator.BEXT),
            shin_r, shin_l)

        shout_r = Signal(self.width)
        shout_l = Signal(self.width)
        m.d.comb += shout_r.eq(
            Cat(shin, is_sub & shin[self.width - 1]).as_signed() >> shamt)
        for a, b in zip(shout_l, reversed(shout_r)):
            m.d.comb += a.eq(b)
        shout = Mux((self.fn == ALUOperator.SR) | (self.fn == ALUOperator.SRA)
                    | (self.fn == ALUOperator.BEXT), shout_r, 0) | Mux(
                        self.fn == ALUOperator.SL, shout_l, 0)

        #
        # AND, OR, XOR
        #

        logic = Mux(
            (self.fn == ALUOperator.XOR) | (self.fn == ALUOperator.OR) |
            (self.fn == ALUOperator.ORN) |
            (self.fn == ALUOperator.XNOR), in1_xor_in2, 0) | Mux(
                (self.fn == ALUOperator.AND) | (self.fn == ALUOperator.OR) |
                (self.fn == ALUOperator.ORN) |
                (self.fn == ALUOperator.ANDN), in1_and_in2, 0)

        bext_mask = Signal(self.width)
        m.d.comb += bext_mask.eq(~0)
        if self.use_zbs:
            with m.If(self.fn == ALUOperator.BEXT):
                m.d.comb += bext_mask.eq(1)

        shift_logic = (is_cmp & slt) | logic | (shout & bext_mask)

        shift_logic_cond = shift_logic
        if self.use_zicond:
            in2_nez = self.in2.any()
            cond = Mux(((self.fn == ALUOperator.CZEQZ) & in2_nez) |
                       ((self.fn == ALUOperator.CZNEZ) & ~in2_nez), self.in1,
                       0)
            shift_logic_cond = shift_logic | cond

        #
        # CLZ, CTZ, CPOP, UNARY
        #

        if self.use_zbb:
            unary = Signal(self.width)

            ctlz_in = Signal(self.width)
            with m.Switch(Cat(self.in2[0], self.dw == ALUWidth.DW_32)):
                with m.Case(0b00):  # clz
                    m.d.comb += ctlz_in.eq(self.in1[::-1])
                with m.Case(0b01):  # ctz
                    m.d.comb += ctlz_in.eq(self.in1)
                with m.Case(0b10):  # clzw
                    m.d.comb += ctlz_in.eq(Cat(self.in1[31::-1], Const(1, 1)))
                with m.Case(0b11):  # ctzw
                    m.d.comb += ctlz_in.eq(Cat(self.in1[:32], Const(1, 1)))

            cpop_in = Signal(self.width)
            with m.If(self.in2[1]):
                m.d.comb += cpop_in.eq(
                    Mux(self.dw == ALUWidth.DW_32, self.in1[:32], self.in1))
            with m.Else():
                m.d.comb += cpop_in.eq(~0)
                for i in reversed(range(self.width)):
                    with m.If(ctlz_in[i]):
                        m.d.comb += cpop_in.eq((1 << i) - 1)

            popcount = m.submodules.popcount = PopCount(self.width)
            m.d.comb += popcount.inp.eq(cpop_in)

            in1_bytes = [self.in1[i:i + 8] for i in range(0, self.width, 8)]
            orc = Cat(x.any().replicate(8) for x in in1_bytes)
            rev8 = Cat(in1_bytes[::-1])

            with m.Switch(self.in2[:12]):
                with m.Case(0x287):  # orc.b
                    m.d.comb += unary.eq(orc)
                with m.Case(0x698 if self.width == 32 else 0x6b8):  # rev8
                    m.d.comb += unary.eq(rev8)
                with m.Case(0x080):  # zext.h
                    m.d.comb += unary.eq(self.in1[:16])
                with m.Case(0x604):  # sext.b
                    m.d.comb += unary.eq(sign_extend(self.in1[:8], self.width))
                with m.Case(0x605):  # sext.h
                    m.d.comb += unary.eq(sign_extend(self.in1[:16],
                                                     self.width))
                with m.Default():
                    m.d.comb += unary.eq(popcount.out)

        #
        # MIN, MAX
        #

        minmax = Mux(cmp_out, self.in2, self.in1)

        #
        # ROL, ROR
        #

        rot_shamt = Mux(self.dw == ALUWidth.DW_32, 32, self.width) - shamt
        rotin = Mux(self.fn[0], shin_r, shin_r[::-1])
        rotout_r = (rotin >> rot_shamt.as_unsigned())[:self.width]
        rotout_l = rotout_r[::-1]
        rotout = Mux(self.fn[0], rotout_r, rotout_l) | Mux(
            self.fn[0], shout_l, shout_r)

        out = Signal.like(self.out)
        with m.Switch(self.fn):
            with m.Case(ALUOperator.ADD, ALUOperator.SUB):
                m.d.comb += out.eq(adder_out)

            if self.use_zbb:
                with m.Case(ALUOperator.UNARY):
                    m.d.comb += out.eq(unary)

                with m.Case(ALUOperator.MAX, ALUOperator.MIN, ALUOperator.MAXU,
                            ALUOperator.MINU):
                    m.d.comb += out.eq(minmax)

                with m.Case(ALUOperator.ROL, ALUOperator.ROR):
                    m.d.comb += out.eq(rotout)

            with m.Default():
                m.d.comb += out.eq(shift_logic_cond)

        m.d.comb += self.out.eq(out)
        if self.width > 32:
            with m.If(self.dw == ALUWidth.DW_32):
                m.d.comb += self.out.eq(Cat(out[:32], out[31].replicate(32)))

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

        max = (self.cmd == MemoryCommand.AMO_MAX) | (self.cmd
                                                     == MemoryCommand.AMO_MAXU)
        min = (self.cmd == MemoryCommand.AMO_MIN) | (self.cmd
                                                     == MemoryCommand.AMO_MINU)
        add = self.cmd == MemoryCommand.AMO_ADD
        logic_and = (self.cmd == MemoryCommand.AMO_AND) | (
            self.cmd == MemoryCommand.AMO_OR)
        logic_xor = (self.cmd == MemoryCommand.AMO_XOR) | (
            self.cmd == MemoryCommand.AMO_OR)

        adder_mask = ~((~self.mask[3]) << 31)
        adder_out = (self.lhs & adder_mask) + (self.rhs & adder_mask)

        signed = (self.cmd == MemoryCommand.AMO_MAX) | (
            self.cmd == MemoryCommand.AMO_MIN)
        less = Signal()
        for w in (32, 64):
            if w <= self.width:
                with m.If(self.mask[w // 8 // 2]):
                    ltu = (self.lhs[:w].as_unsigned()
                           < self.rhs[:w].as_unsigned())
                    lt = (~(self.lhs[w - 1] ^ self.rhs[w - 1])
                          & ltu) | (self.lhs[w - 1] & ~self.rhs[w - 1])
                    m.d.comb += less.eq(Mux(signed, lt, ltu))

        minmax = Mux(Mux(less, min, max), self.lhs, self.rhs)
        logic = Mux(logic_and, self.lhs & self.rhs, 0) | Mux(
            logic_xor, self.lhs ^ self.rhs, 0)
        out = Mux(add, adder_out, Mux(logic_and | logic_xor, logic, minmax))

        wmask = Signal(self.width)
        for i in range(len(self.mask)):
            m.d.comb += wmask[i * 8:(i + 1) * 8].eq(Repl(self.mask[i], 8))

        m.d.comb += self.out.eq(wmask & out | ~wmask & self.lhs)

        return m
