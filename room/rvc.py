from amaranth import *
import riscvmodel.insn as insn


class RVCDecoder(Elaboratable):

    def __init__(self):
        self.instr_i = Signal(32)
        self.instr_o = Signal(32)

    def elaborate(self, platform):
        m = Module()

        instr_i = self.instr_i
        instr_o = self.instr_o

        OPV8 = lambda name: Const(
            getattr(insn, f'Instruction{name}').field_opcode.value, 8)

        with m.Switch(instr_i[0:2]):
            # C1
            with m.Case(0b01):
                with m.Switch(instr_i[13:16]):
                    with m.Case(0b001, 0b101):
                        m.d.comb += instr_o.eq(
                            Cat(
                                OPV8('JAL'),
                                ~instr_i[15],
                                Const(0, 4),
                                Repl(instr_i[12], 9),
                                instr_i[3:6],
                                instr_i[11],
                                instr_i[2],
                                instr_i[7],
                                instr_i[6],
                                instr_i[9:11],
                                instr_i[8],
                                instr_i[12],
                            ))

            # C2
            with m.Case(0b10):
                with m.Switch(instr_i[13:16]):
                    with m.Case(0b100):
                        with m.If(instr_i[12] == 0):
                            with m.If(instr_i[2:7] == 0):
                                m.d.comb += instr_o.eq(
                                    Cat(OPV8('JALR'), Const(0, 8),
                                        instr_i[7:12], Const(0, 12)))
                            with m.Else():
                                with m.If(instr_i[7:12] == 0):
                                    m.d.comb += instr_o.eq(
                                        Cat(OPV8('ADD'), instr_i[7:12],
                                            Const(0, 8), instr_i[2:7],
                                            Const(0, 7)))
                                with m.Else():
                                    m.d.comb += instr_o.eq(
                                        Cat(OPV8('ADD'), instr_i[7:12],
                                            Const(0, 8), instr_i[2:7],
                                            Const(0, 7)))

            with m.Case(3):
                m.d.comb += instr_o.eq(instr_i)

        return m
