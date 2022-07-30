from amaranth import *
import riscvmodel.insn as insn


class RVCDecoder(Elaboratable):

    def __init__(self):
        self.instr_i = Signal(32)
        self.instr_o = Signal(32)

    def elaborate(self, platform):
        m = Module()

        with m.Switch(self.instr_i[0:2]):
            # C2
            with m.Case(2):
                with m.Switch(self.instr_i[13:16]):
                    with m.Case(4):
                        with m.If(self.instr_i[12] == 0):
                            with m.If(self.instr_i[2:7] == 0):
                                m.d.comb += self.instr_o.eq(
                                    Cat(
                                        Const(
                                            insn.InstructionJALR.field_opcode.
                                            value, 8), Const(0, 8),
                                        self.instr_i[7:12], Const(0, 12)))
                            with m.Else():
                                with m.If(self.instr_i[7:12] == 0):
                                    m.d.comb += self.instr_o.eq(
                                        Cat(
                                            Const(
                                                insn.InstructionADD.
                                                field_opcode.value,
                                                8), self.instr_i[7:12],
                                            Const(0, 8), self.instr_i[2:7],
                                            Const(0, 7)))
                                with m.Else():
                                    m.d.comb += self.instr_o.eq(
                                        Cat(
                                            Const(
                                                insn.InstructionADD.
                                                field_opcode.value,
                                                8), self.instr_i[7:12],
                                            Const(0, 8), self.instr_i[2:7],
                                            Const(0, 7)))

            with m.Case(3):
                m.d.comb += self.instr_o.eq(self.instr_i)

        return m
