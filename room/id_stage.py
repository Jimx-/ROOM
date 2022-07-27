from amaranth import *
import riscvmodel.insn as insn
from enum import Enum

from room.alu import ALUOperator
from room.regfile import RegFile


class OpASelect(Enum):
    REGA = 0
    CURPC = 1
    IMM = 2
    REGB = 3


class OpBSelect(Enum):
    REGB = 0
    IMM = 1
    REGA = 2


class ImmBSelect(Enum):
    I = 0
    S = 1
    U = 2


class ForwardSelect(Enum):
    REGFILE = 0
    FW_EX = 1
    FW_WB = 2


class DecodeIO:

    def __init__(self):
        self.alu_en = Signal()
        self.alu_operator = Signal(ALUOperator)
        self.alu_operand_a = Signal(32)
        self.alu_operand_b = Signal(32)

        self.rf_alu_waddr = Signal(32)
        self.rf_alu_we = Signal()

        self.rf_alu_waddr_fw = Signal(32)
        self.rf_alu_wdata_fw = Signal(32)
        self.rf_alu_we_fw = Signal()

        self.id_valid = Signal()

        self.halt_id = Signal()
        self.ex_ready = Signal()


class Controller(Elaboratable):

    def __init__(self):
        self.pc_set = Signal()

        self.instr_req = Signal(reset=1)
        self.instr_valid = Signal()

        self.illegal_insn = Signal()

        self.is_decoding = Signal()
        self.deassert_we = Signal()

        self.rf_alu_we_fw = Signal()
        self.reg_d_alu_is_reg_a = Signal()
        self.reg_d_alu_is_reg_b = Signal()

        self.operand_a_fw_sel = Signal(ForwardSelect)
        self.operand_b_fw_sel = Signal(ForwardSelect)

    def elaborate(self, platform):
        m = Module()

        with m.FSM():
            with m.State('RESET'):
                m.d.comb += [self.instr_req.eq(0)]

                m.next = 'BOOT_SET'

            with m.State('BOOT_SET'):
                m.d.comb += [self.pc_set.eq(1), self.instr_req.eq(1)]
                m.next = 'FIRST_FETCH'

            with m.State('FIRST_FETCH'):
                m.next = 'DECODE'

            with m.State('DECODE'):
                with m.If(self.instr_valid):
                    m.d.comb += self.is_decoding.eq(1)

                m.next = 'DECODE'

        m.d.comb += [
            self.deassert_we.eq(~self.is_decoding | self.illegal_insn)
        ]

        with m.If(self.rf_alu_we_fw):
            with m.If(self.reg_d_alu_is_reg_a):
                m.d.comb += self.operand_a_fw_sel.eq(ForwardSelect.FW_EX)
            with m.If(self.reg_d_alu_is_reg_b):
                m.d.comb += self.operand_b_fw_sel.eq(ForwardSelect.FW_EX)

        return m


class Decoder(Elaboratable):

    def __init__(self):
        self.instr_data = Signal(32)

        self.rega_used = Signal()
        self.regb_used = Signal()
        self.regc_used = Signal()

        self.alu_en = Signal()
        self.alu_operator = Signal(ALUOperator, reset=ALUOperator.SLTU)
        self.alu_op_a_sel = Signal(OpASelect)
        self.alu_op_b_sel = Signal(OpBSelect)

        self.imm_b_sel = Signal(ImmBSelect)

        self.illegal_insn = Signal()
        self.deassert_we = Signal()

        self.rf_alu_we = Signal()

    def elaborate(self, platform):
        m = Module()

        alu_en = Signal(reset=1)
        rf_alu_we = Signal()

        with m.Switch(self.instr_data[0:7]):
            with m.Case(insn.InstructionAUIPC.field_opcode.value):
                m.d.comb += [
                    self.alu_op_a_sel.eq(OpASelect.CURPC),
                    self.alu_op_b_sel.eq(OpBSelect.IMM),
                    self.imm_b_sel.eq(ImmBSelect.U),
                    self.alu_operator.eq(ALUOperator.ADD),
                    rf_alu_we.eq(1),
                ]

            # Register-register
            with m.Case(insn.InstructionADDI.field_opcode.value):
                m.d.comb += [
                    self.alu_op_b_sel.eq(OpBSelect.IMM),
                    self.imm_b_sel.eq(ImmBSelect.I),
                    self.rega_used.eq(1),
                    rf_alu_we.eq(1),
                ]

                with m.Switch(self.instr_data[12:15]):
                    with m.Case(insn.InstructionADDI.field_funct3.value):
                        m.d.comb += self.alu_operator.eq(ALUOperator.ADD)
                    with m.Case(insn.InstructionSLTI.field_funct3.value):
                        m.d.comb += self.alu_operator.eq(ALUOperator.SLTS)
                    with m.Case(insn.InstructionSLTIU.field_funct3.value):
                        m.d.comb += self.alu_operator.eq(ALUOperator.SLTU)
                    with m.Case(insn.InstructionXORI.field_funct3.value):
                        m.d.comb += self.alu_operator.eq(ALUOperator.XOR)
                    with m.Case(insn.InstructionORI.field_funct3.value):
                        m.d.comb += self.alu_operator.eq(ALUOperator.OR)
                    with m.Case(insn.InstructionANDI.field_funct3.value):
                        m.d.comb += self.alu_operator.eq(ALUOperator.AND)

                    with m.Case(insn.InstructionSLLI.field_funct3.value):
                        m.d.comb += self.alu_operator.eq(ALUOperator.SLL)
                        with m.If(self.instr_data[25:32] != 0):
                            m.d.comb += self.illegal_insn.eq(1)

                    with m.Case(insn.InstructionSRLI.field_funct3.value):
                        with m.Switch(self.instr_data[25:32]):
                            with m.Case(
                                    insn.InstructionSRLI.field_funct7.value):
                                m.d.comb += self.alu_operator.eq(
                                    ALUOperator.SRL)
                            with m.Case(
                                    insn.InstructionSRAI.field_funct7.value):
                                m.d.comb += self.alu_operator.eq(
                                    ALUOperator.SRA)
                            with m.Default():
                                m.d.comb += self.illegal_insn.eq(1)

            with m.Case(insn.InstructionADD.field_opcode.value):
                pass

        m.d.comb += [
            self.alu_en.eq(Mux(self.deassert_we, 0, alu_en)),
            self.rf_alu_we.eq(Mux(self.deassert_we, 0, rf_alu_we)),
        ]

        return m


class IDStage(Elaboratable):

    def __init__(self, fetch_io):
        self.fetch_io = fetch_io
        self.io = DecodeIO()

    def elaborate(self, platform):
        m = Module()

        fetch_io = self.fetch_io
        instr_data = fetch_io.instr_data
        io = self.io

        ctrl = m.submodules.controller = Controller()
        decoder = m.submodules.decoder = Decoder()
        regfile = m.submodules.regfile = RegFile(2,
                                                 2,
                                                 addr_width=5,
                                                 data_width=32)

        m.d.comb += [
            fetch_io.pc_set.eq(ctrl.pc_set),
            fetch_io.instr_req.eq(ctrl.instr_req),
            decoder.instr_data.eq(instr_data),
            ctrl.instr_valid.eq(fetch_io.instr_valid),
            fetch_io.clear_instr_valid.eq(fetch_io.id_ready),
            ctrl.illegal_insn.eq(decoder.illegal_insn),
            decoder.deassert_we.eq(ctrl.deassert_we),
        ]

        imm_i_type = Signal(32)
        imm_s_type = Signal(32)
        imm_u_type = Signal(32)

        imm_b = Signal(32)

        m.d.comb += [
            imm_i_type.eq(Cat(instr_data[20:32], Repl(instr_data[31], 20))),
            imm_s_type.eq(
                Cat(instr_data[7:12], instr_data[25:32],
                    Repl(instr_data[31], 20))),
            imm_u_type.eq(Cat(Repl(0, 12), instr_data[12:32])),
        ]

        with m.Switch(decoder.imm_b_sel):
            with m.Case(ImmBSelect.I):
                m.d.comb += imm_b.eq(imm_i_type)
            with m.Case(ImmBSelect.S):
                m.d.comb += imm_b.eq(imm_s_type)
            with m.Case(ImmBSelect.U):
                m.d.comb += imm_b.eq(imm_u_type)

        regfile_addr_ra = Signal(5)
        regfile_data_ra = Signal(32)
        regfile_addr_rb = Signal(5)
        regfile_data_rb = Signal(32)
        regfile_waddr = Signal(5)
        regfile_alu_waddr = Signal(5)

        m.d.comb += [
            ctrl.rf_alu_we_fw.eq(io.rf_alu_we_fw),
            ctrl.reg_d_alu_is_reg_a.eq((io.rf_alu_waddr_fw == regfile_addr_ra)
                                       & decoder.rega_used
                                       & (regfile_addr_ra != 0)),
            ctrl.reg_d_alu_is_reg_b.eq((io.rf_alu_waddr_fw == regfile_addr_rb)
                                       & decoder.regb_used
                                       & (regfile_addr_rb != 0)),
        ]

        m.d.comb += [
            regfile_addr_ra.eq(instr_data[15:20]),
            regfile_addr_rb.eq(instr_data[20:25]),
            regfile_waddr.eq(instr_data[7:12]),
            regfile_alu_waddr.eq(regfile_waddr),
        ]

        operand_a_fw = Signal(32)
        with m.Switch(ctrl.operand_a_fw_sel):
            with m.Case(ForwardSelect.REGFILE):
                m.d.comb += operand_a_fw.eq(regfile_data_ra)
            with m.Case(ForwardSelect.FW_EX):
                m.d.comb += operand_a_fw.eq(io.rf_alu_wdata_fw)

        operand_b_fw = Signal(32)
        with m.Switch(ctrl.operand_b_fw_sel):
            with m.Case(ForwardSelect.REGFILE):
                m.d.comb += operand_b_fw.eq(regfile_data_rb)
            with m.Case(ForwardSelect.FW_EX):
                m.d.comb += operand_b_fw.eq(io.rf_alu_wdata_fw)

        alu_operand_a = Signal(32)
        with m.Switch(decoder.alu_op_a_sel):
            with m.Case(OpASelect.REGA):
                m.d.comb += alu_operand_a.eq(operand_a_fw)
            with m.Case(OpASelect.REGB):
                m.d.comb += alu_operand_a.eq(operand_b_fw)
            with m.Case(OpASelect.CURPC):
                m.d.comb += alu_operand_a.eq(0)
            with m.Case(OpASelect.IMM):
                m.d.comb += alu_operand_a.eq(0)

        alu_operand_b = Signal(32)
        with m.Switch(decoder.alu_op_b_sel):
            with m.Case(OpBSelect.REGA):
                m.d.comb += alu_operand_b.eq(operand_a_fw)
            with m.Case(OpBSelect.REGB):
                m.d.comb += alu_operand_b.eq(operand_b_fw)
            with m.Case(OpBSelect.IMM):
                m.d.comb += alu_operand_b.eq(imm_b)

        m.d.comb += [
            regfile.read_ports[0].addr.eq(regfile_addr_ra),
            regfile.read_ports[1].addr.eq(regfile_addr_rb),
            regfile_data_ra.eq(regfile.read_ports[0].data),
            regfile_data_rb.eq(regfile.read_ports[1].data),
            regfile.write_ports[1].en.eq(io.rf_alu_we_fw),
            regfile.write_ports[1].addr.eq(io.rf_alu_waddr_fw),
            regfile.write_ports[1].data.eq(io.rf_alu_wdata_fw),
        ]

        m.d.comb += [
            fetch_io.id_ready.eq(io.ex_ready),
            io.id_valid.eq((~io.halt_id) & fetch_io.id_ready)
        ]

        with m.If(io.id_valid):
            m.d.sync += io.alu_en.eq(decoder.alu_en)
            with m.If(decoder.alu_en):
                m.d.sync += [
                    io.alu_operator.eq(decoder.alu_operator),
                    io.alu_operand_a.eq(alu_operand_a),
                    io.alu_operand_b.eq(alu_operand_b),
                ]

            m.d.sync += io.rf_alu_we.eq(decoder.rf_alu_we),
            with m.If(decoder.rf_alu_we):
                m.d.sync += io.rf_alu_waddr.eq(regfile_alu_waddr)
        with m.Elif(io.ex_ready):
            m.d.sync += [
                io.rf_alu_we.eq(0),
                io.alu_en.eq(1),
                io.alu_operator.eq(ALUOperator.SLTU),
            ]

        return m
