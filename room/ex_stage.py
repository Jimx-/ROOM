from amaranth import *

from room.alu import ALU


class EXStage(Elaboratable):

    def __init__(self, decode_io):
        self.decode_io = decode_io

    def elaborate(self, platform):
        m = Module()

        decode_io = self.decode_io

        alu = m.submodules.alu = ALU()

        m.d.comb += [
            alu.enable.eq(decode_io.alu_en),
            alu.operator.eq(decode_io.alu_operator),
            alu.operand_a.eq(decode_io.alu_operand_a),
            alu.operand_b.eq(decode_io.alu_operand_b),
        ]

        m.d.comb += [
            decode_io.rf_alu_we_fw.eq(decode_io.rf_alu_we),
            decode_io.rf_alu_waddr_fw.eq(decode_io.rf_alu_waddr),
            decode_io.rf_alu_wdata_fw.eq(alu.result),
        ]

        m.d.comb += [decode_io.ex_ready.eq(alu.ready)]

        return m
