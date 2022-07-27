from amaranth import *
from enum import IntEnum


class ALUOperator(IntEnum):
    ADD = 0b0011000,
    SUB = 0b0011001,
    ADDU = 0b0011010,
    SUBU = 0b0011011,
    ADDR = 0b0011100,
    SUBR = 0b0011101,
    ADDUR = 0b0011110,
    SUBUR = 0b0011111,

    XOR = 0b0101111,
    OR = 0b0101110,
    AND = 0b0010101,

    SRA = 0b0100100,
    SRL = 0b0100101,
    ROR = 0b0100110,
    SLL = 0b0100111,

    BEXT = 0b0101000,
    BEXTU = 0b0101001,
    BINS = 0b0101010,
    BCLR = 0b0101011,
    BSET = 0b0101100,
    BREV = 0b1001001,

    FF1 = 0b0110110,
    FL1 = 0b0110111,
    CNT = 0b0110100,
    CLB = 0b0110101,

    EXTS = 0b0111110,
    EXT = 0b0111111,

    LTS = 0b0000000,
    LTU = 0b0000001,
    LES = 0b0000100,
    LEU = 0b0000101,
    GTS = 0b0001000,
    GTU = 0b0001001,
    GES = 0b0001010,
    GEU = 0b0001011,
    EQ = 0b0001100,
    NE = 0b0001101,

    SLTS = 0b0000010,
    SLTU = 0b0000011,
    SLETS = 0b0000110,
    SLETU = 0b0000111,

    ABS = 0b0010100,
    CLIP = 0b0010110,
    CLIPU = 0b0010111,

    INS = 0b0101101,

    MIN = 0b0010000,
    MINU = 0b0010001,
    MAX = 0b0010010,
    MAXU = 0b0010011,

    DIVU = 0b0110000,
    DIV = 0b0110001,
    REMU = 0b0110010,
    REM = 0b0110011,

    SHUF = 0b0111010,
    SHUF2 = 0b0111011,
    PCKLO = 0b0111000,
    PCKHI = 0b0111001


class ALU(Elaboratable):

    def __init__(self):
        self.enable = Signal()

        self.operator = Signal(ALUOperator)
        self.operand_a = Signal(32)
        self.operand_b = Signal(32)

        self.result = Signal(32)
        self.cmp_result = Signal()

        self.ready = Signal()
        self.ex_ready = Signal()

    def elaborate(self, platform):
        m = Module()

        operand_a_rev = Signal(32)
        for i in range(32):
            m.d.comb += operand_a_rev[i].eq(self.operand_a[31 - i])

        adder_op_a = Signal(32)
        adder_op_b = Signal(32)
        adder_in_a = Signal(signed(36))
        adder_in_b = Signal(signed(36))
        adder_result_expanded = Signal(37)
        adder_result = Signal(32)

        adder_in_a_p = []
        adder_in_b_p = []
        for i in range(0, 32, 8):
            adder_in_a_p.append(Const(1, 1))
            adder_in_a_p.append(adder_op_a[i:i + 8])
            adder_in_b_p.append(Const(0, 1))
            adder_in_b_p.append(adder_op_b[i:i + 8])

        m.d.comb += [
            adder_op_a.eq(self.operand_a),
            adder_op_b.eq(self.operand_b),
            adder_in_a.eq(Cat(*adder_in_a_p)),
            adder_in_b.eq(Cat(*adder_in_b_p)),
            adder_result_expanded.eq(adder_in_a + adder_in_b),
            adder_result.eq(
                Cat(*[
                    adder_result_expanded[i + 1:i + 9]
                    for i in range(0, 36, 9)
                ])),
        ]

        shift_left = Signal()
        shift_arithmetic = Signal()

        shift_op_a = Signal(32)
        shift_op_a_32 = Signal(64)
        shift_amt = Signal(32)

        shift_result_right = Signal(32)
        shift_result_left = Signal(32)
        shift_result = Signal(32)

        m.d.comb += [
            shift_left.eq(self.operator == ALUOperator.SLL),
            shift_arithmetic.eq(self.operator == ALUOperator.SRA),
            shift_op_a.eq(Mux(shift_left, operand_a_rev, self.operand_a)),
            shift_op_a_32.eq(
                Mux(
                    self.operator == ALUOperator.ROR,
                    Cat(shift_op_a, shift_op_a),
                    Cat(Repl(shift_arithmetic & shift_op_a[31], 32),
                        shift_op_a))),
            shift_amt.eq(self.operand_b),
            shift_result_right.eq(shift_op_a_32 >> shift_amt[0:5]),
            shift_result.eq(
                Mux(shift_left, shift_result_left, shift_result_right)),
        ]

        for i in range(32):
            m.d.comb += shift_result_left[i].eq(shift_result_right[31 - i])

        with m.Switch(self.operator):
            with m.Case(ALUOperator.AND):
                m.d.comb += self.result.eq(self.operand_a & self.operand_b)
            with m.Case(ALUOperator.OR):
                m.d.comb += self.result.eq(self.operand_a | self.operand_b)
            with m.Case(ALUOperator.XOR):
                m.d.comb += self.result.eq(self.operand_a ^ self.operand_b)
            with m.Case(ALUOperator.ADD, ALUOperator.SUB):
                m.d.comb += self.result.eq(adder_result)
            with m.Case(ALUOperator.SLL, ALUOperator.SRL, ALUOperator.SRA,
                        ALUOperator.ROR):
                m.d.comb += self.result.eq(shift_result)

        m.d.comb += self.ready.eq(1)

        return m
