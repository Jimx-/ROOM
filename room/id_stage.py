from amaranth import *
import riscvmodel.insn as insn

from room.consts import *
from room.types import MicroOp
from room.branch import BranchMaskAllocator, BranchUpdate
from room.exc import Cause


class IDDebug(Record):

    def __init__(self, params, name=None, src_loc_at=0):
        max_br_count = params['max_br_count']

        super().__init__([
            ('valid', 1),
            ('uop_id', MicroOp.ID_WIDTH),
            ('br_mask', max_br_count),
        ],
                         name=name,
                         src_loc_at=1 + src_loc_at)


class BranchSignals(Record):

    def __init__(self, vaddr_bits, name=None, src_loc_at=0):
        super().__init__([
            ('cfi_type', Shape.cast(CFIType).width),
            ('target', vaddr_bits),
        ],
                         name=name,
                         src_loc_at=1 + src_loc_at)


class BranchDecoder(Elaboratable):

    def __init__(self, vaddr_bits):
        self.inst = Signal(32)
        self.pc = Signal(vaddr_bits)

        self.out = BranchSignals(vaddr_bits=vaddr_bits)

    def elaborate(self, platform):
        m = Module()

        def compute_jal_target(pc, inst):
            j_imm32 = Cat(Const(0, 1), inst[21:25], inst[25:31], inst[20],
                          inst[12:20], Repl(inst[31], 12)).as_signed()
            return ((pc.as_signed() + j_imm32) & -2)[:len(pc)].as_unsigned()

        OPV = lambda name: getattr(insn, f'Instruction{name}'
                                   ).field_opcode.value

        with m.Switch(self.inst[0:7]):
            with m.Case(OPV('JAL')):
                m.d.comb += self.out.cfi_type.eq(CFIType.JAL)

            with m.Case(OPV('JALR')):
                m.d.comb += self.out.cfi_type.eq(CFIType.JALR)

            with m.Case(OPV('BEQ')):
                m.d.comb += self.out.cfi_type.eq(CFIType.BR)

        m.d.comb += self.out.target.eq(
            Mux(self.out.cfi_type == CFIType.BR, 0,
                compute_jal_target(self.pc, self.inst)))

        return m


class DecodeUnit(Elaboratable):

    def __init__(self, params):
        self.xlen = params['xlen']
        self.use_fpu = params['use_fpu']

        self.in_uop = MicroOp(params)
        self.out_uop = MicroOp(params)

        self.interrupt = Signal()
        self.interrupt_cause = Signal(self.xlen)

    def elaborate(self, platform):
        m = Module()

        insn_illegal = Signal(reset=0)

        inuop = self.in_uop
        uop = self.out_uop

        m.d.comb += [
            uop.eq(inuop),
            uop.ldst.eq(inuop.inst[7:12]),
            uop.lrs1.eq(inuop.inst[15:20]),
            uop.lrs2.eq(inuop.inst[20:25]),
            uop.lrs3.eq(inuop.inst[27:32]),
            uop.ldst_valid.eq((uop.dst_rtype != RegisterType.X) & ~(
                (uop.dst_rtype == RegisterType.FIX) & (uop.ldst == 0))),
        ]

        OPV = lambda name: getattr(insn, f'Instruction{name}'
                                   ).field_opcode.value
        F3 = lambda name: getattr(insn, f'Instruction{name}'
                                  ).field_funct3.value
        F7 = lambda name: getattr(insn, f'Instruction{name}'
                                  ).field_funct7.value
        OPIMM = lambda name: getattr(insn, f'Instruction{name}'
                                     ).field_imm.value

        UOPC = lambda x: uop.opcode.eq(x)
        ILL_INSN = insn_illegal.eq(1)

        imm_sel = Signal(ImmSel)

        IMM_SEL_I = imm_sel.eq(ImmSel.I)
        IMM_SEL_S = imm_sel.eq(ImmSel.S)
        IMM_SEL_J = imm_sel.eq(ImmSel.J)
        IMM_SEL_B = imm_sel.eq(ImmSel.B)
        IMM_SEL_U = imm_sel.eq(ImmSel.U)

        with m.Switch(inuop.inst[0:7]):

            #
            # Jump
            #

            with m.Case(OPV('JAL')):
                m.d.comb += [
                    UOPC(UOpCode.JAL),
                    uop.iq_type.eq(IssueQueueType.INT),
                    uop.fu_type.eq(FUType.JMP),
                    uop.dst_rtype.eq(RegisterType.FIX),
                    IMM_SEL_J,
                    uop.is_jal.eq(1),
                ]

            with m.Case(OPV('JALR')):
                m.d.comb += [
                    UOPC(UOpCode.JALR),
                    uop.iq_type.eq(IssueQueueType.INT),
                    uop.fu_type.eq(FUType.JMP),
                    uop.dst_rtype.eq(RegisterType.FIX),
                    uop.lrs1_rtype.eq(RegisterType.FIX),
                    IMM_SEL_I,
                    uop.is_jalr.eq(1),
                ]

            with m.Case(OPV('BEQ')):
                m.d.comb += [
                    uop.iq_type.eq(IssueQueueType.INT),
                    uop.fu_type.eq(FUType.ALU),
                    uop.lrs1_rtype.eq(RegisterType.FIX),
                    uop.lrs2_rtype.eq(RegisterType.FIX),
                    IMM_SEL_B,
                    uop.is_br.eq(1),
                ]

                with m.Switch(inuop.inst[12:15]):
                    with m.Case(F3('BEQ')):
                        m.d.comb += UOPC(UOpCode.BEQ)
                    with m.Case(F3('BNE')):
                        m.d.comb += UOPC(UOpCode.BNE)
                    with m.Case(F3('BGE')):
                        m.d.comb += UOPC(UOpCode.BGE)
                    with m.Case(F3('BGEU')):
                        m.d.comb += UOPC(UOpCode.BGEU)
                    with m.Case(F3('BLT')):
                        m.d.comb += UOPC(UOpCode.BLT)
                    with m.Case(F3('BLTU')):
                        m.d.comb += UOPC(UOpCode.BLTU)

            #
            # Store
            #

            with m.Case(OPV('SW')):
                m.d.comb += [
                    UOPC(UOpCode.STA),
                    uop.iq_type.eq(IssueQueueType.MEM),
                    uop.fu_type.eq(FUType.MEM),
                    uop.lrs1_rtype.eq(RegisterType.FIX),
                    uop.lrs2_rtype.eq(RegisterType.FIX),
                    IMM_SEL_S,
                    uop.uses_stq.eq(1),
                    uop.mem_cmd.eq(MemoryCommand.WRITE),
                    uop.mem_size.eq(inuop.inst[12:14]),
                ]

            #
            # Load
            #

            with m.Case(OPV('LW')):
                m.d.comb += [
                    UOPC(UOpCode.LD),
                    uop.iq_type.eq(IssueQueueType.MEM),
                    uop.fu_type.eq(FUType.MEM),
                    uop.dst_rtype.eq(RegisterType.FIX),
                    uop.lrs1_rtype.eq(RegisterType.FIX),
                    IMM_SEL_I,
                    uop.uses_ldq.eq(1),
                    uop.mem_cmd.eq(MemoryCommand.READ),
                    uop.mem_size.eq(inuop.inst[12:14]),
                    uop.mem_signed.eq(~inuop.inst[14]),
                ]

            with m.Case(OPV('AUIPC')):
                m.d.comb += [
                    UOPC(UOpCode.AUIPC),
                    uop.iq_type.eq(IssueQueueType.INT),
                    uop.fu_type.eq(FUType.JMP),
                    uop.dst_rtype.eq(RegisterType.FIX),
                    IMM_SEL_U,
                ]

            with m.Case(OPV('LUI')):
                m.d.comb += [
                    UOPC(UOpCode.LUI),
                    uop.iq_type.eq(IssueQueueType.INT),
                    uop.fu_type.eq(FUType.ALU),
                    uop.dst_rtype.eq(RegisterType.FIX),
                    IMM_SEL_U,
                ]

            #
            # Register-immediate
            #

            with m.Case(OPV('ADDI')):
                m.d.comb += [
                    uop.iq_type.eq(IssueQueueType.INT),
                    uop.fu_type.eq(FUType.ALU),
                    uop.dst_rtype.eq(RegisterType.FIX),
                    uop.lrs1_rtype.eq(RegisterType.FIX),
                    IMM_SEL_I,
                ]

                with m.Switch(inuop.inst[12:15]):
                    with m.Case(F3('ADDI')):
                        m.d.comb += UOPC(UOpCode.ADDI)
                    with m.Case(F3('SLTI')):
                        m.d.comb += UOPC(UOpCode.SLTI)
                    with m.Case(F3('SLTIU')):
                        m.d.comb += UOPC(UOpCode.SLTIU)
                    with m.Case(F3('XORI')):
                        m.d.comb += UOPC(UOpCode.XORI)
                    with m.Case(F3('ORI')):
                        m.d.comb += UOPC(UOpCode.ORI)
                    with m.Case(F3('ANDI')):
                        m.d.comb += UOPC(UOpCode.ANDI)

                    with m.Case(F3('SLLI')):
                        m.d.comb += UOPC(UOpCode.SLLI)
                        with m.If((inuop.inst[26:32] != 0) | (
                            (self.xlen == 32) & (inuop.inst[25] != 0))):
                            m.d.comb += ILL_INSN

                    with m.Case(F3('SRLI')):
                        with m.If((inuop.inst[26:30] != 0) | (
                            (self.xlen == 32) & (inuop.inst[25] != 0))):
                            m.d.comb += ILL_INSN
                        with m.Else():
                            with m.Switch(inuop.inst[30:32]):
                                with m.Case(0b00):
                                    m.d.comb += UOPC(UOpCode.SRLI)
                                with m.Case(0b01):
                                    m.d.comb += UOPC(UOpCode.SRAI)
                                with m.Default():
                                    m.d.comb += ILL_INSN

            if self.xlen == 64:
                # OP-IMM-32
                with m.Case(0b0011011):
                    m.d.comb += [
                        uop.iq_type.eq(IssueQueueType.INT),
                        uop.fu_type.eq(FUType.ALU),
                        uop.dst_rtype.eq(RegisterType.FIX),
                        uop.lrs1_rtype.eq(RegisterType.FIX),
                        IMM_SEL_I,
                    ]

                    with m.Switch(inuop.inst[12:15]):
                        with m.Case(F3('ADDI')):
                            m.d.comb += UOPC(UOpCode.ADDIW)

                        with m.Case(F3('SLLI')):
                            m.d.comb += UOPC(UOpCode.SLLIW)
                            with m.If(inuop.inst[25:32] != 0):
                                m.d.comb += ILL_INSN

                        with m.Case(F3('SRLI')):
                            with m.Switch(inuop.inst[25:32]):
                                with m.Case(F7('SRLI')):
                                    m.d.comb += UOPC(UOpCode.SRLIW)
                                with m.Case(F7('SRAI')):
                                    m.d.comb += UOPC(UOpCode.SRAIW)
                                with m.Default():
                                    m.d.comb += ILL_INSN

            #
            # Register-register
            #

            with m.Case(OPV('ADD')):
                m.d.comb += [
                    uop.iq_type.eq(IssueQueueType.INT),
                    uop.dst_rtype.eq(RegisterType.FIX),
                    uop.lrs1_rtype.eq(RegisterType.FIX),
                    uop.lrs2_rtype.eq(RegisterType.FIX),
                ]

                for name in [
                        'ADD', 'SUB', 'SLT', 'SLTU', 'XOR', 'OR', 'AND', 'SLL',
                        'SRL', 'SRA'
                ]:
                    with m.If((inuop.inst[25:31] == F7(name))
                              & (inuop.inst[12:15] == F3(name))):
                        m.d.comb += [
                            UOPC(getattr(UOpCode, name)),
                            uop.fu_type.eq(FUType.ALU),
                        ]

                for name in ['MUL', 'MULH', 'MULHU', 'MULHSU']:
                    with m.If((inuop.inst[25:31] == F7(name))
                              & (inuop.inst[12:15] == F3(name))):
                        m.d.comb += [
                            UOPC(getattr(UOpCode, name)),
                            uop.fu_type.eq(FUType.MUL),
                        ]

                for name in ['DIV', 'DIVU', 'REM', 'REMU']:
                    with m.If((inuop.inst[25:31] == F7(name))
                              & (inuop.inst[12:15] == F3(name))):
                        m.d.comb += [
                            UOPC(getattr(UOpCode, name)),
                            uop.fu_type.eq(FUType.DIV),
                        ]

            if self.xlen == 64:
                # OP-32
                with m.Case(0b0111011):
                    m.d.comb += [
                        uop.iq_type.eq(IssueQueueType.INT),
                        uop.dst_rtype.eq(RegisterType.FIX),
                        uop.lrs1_rtype.eq(RegisterType.FIX),
                        uop.lrs2_rtype.eq(RegisterType.FIX),
                    ]

                    for name in ['ADD', 'SUB', 'SLL', 'SRL', 'SRA']:
                        with m.If((inuop.inst[25:31] == F7(name))
                                  & (inuop.inst[12:15] == F3(name))):
                            m.d.comb += [
                                UOPC(getattr(UOpCode, name + 'W')),
                                uop.fu_type.eq(FUType.ALU),
                            ]

                    with m.If((inuop.inst[25:31] == F7('MUL'))
                              & (inuop.inst[12:15] == F3('MUL'))):
                        m.d.comb += [
                            UOPC(UOpCode.MULW),
                            uop.fu_type.eq(FUType.MUL),
                        ]

                    for name in ['DIV', 'DIVU', 'REM', 'REMU']:
                        with m.If((inuop.inst[25:31] == F7(name))
                                  & (inuop.inst[12:15] == F3(name))):
                            m.d.comb += [
                                UOPC(getattr(UOpCode, name + 'W')),
                                uop.fu_type.eq(FUType.DIV),
                            ]

            #
            # System
            #

            with m.Case(OPV('EBREAK')):
                m.d.comb += [
                    uop.clear_pipeline.eq(1),
                    uop.flush_on_commit.eq(1),
                ]

                with m.If(inuop.inst[12:15] == 0):
                    with m.If((uop.ldst == 0) & (uop.lrs1 == 0)):
                        with m.Switch(inuop.inst[20:]):
                            for name in ['ECALL', 'EBREAK']:
                                with m.Case(OPIMM(name)):
                                    m.d.comb += [
                                        UOPC(UOpCode.ERET),
                                        uop.iq_type.eq(IssueQueueType.INT),
                                        uop.fu_type.eq(FUType.CSR),
                                        IMM_SEL_I,
                                        uop.csr_cmd.eq(CSRCommand.I),
                                        uop.is_ecall.eq(1),
                                    ]

                            for name in ['SRET', 'MRET']:
                                with m.Case(OPIMM(name)):
                                    m.d.comb += [
                                        UOPC(UOpCode.ERET),
                                        uop.iq_type.eq(IssueQueueType.INT),
                                        uop.fu_type.eq(FUType.CSR),
                                        IMM_SEL_I,
                                        uop.csr_cmd.eq(CSRCommand.I),
                                    ]

                            with m.Case(0x7b2):  # DRET
                                m.d.comb += [
                                    UOPC(UOpCode.ERET),
                                    uop.iq_type.eq(IssueQueueType.INT),
                                    uop.fu_type.eq(FUType.CSR),
                                    IMM_SEL_I,
                                    uop.csr_cmd.eq(CSRCommand.I),
                                ]

                with m.Else():
                    m.d.comb += [
                        uop.iq_type.eq(IssueQueueType.INT),
                        uop.fu_type.eq(FUType.CSR),
                        uop.dst_rtype.eq(RegisterType.FIX),
                        IMM_SEL_I,
                    ]

                    for name in ['CSRRW', 'CSRRS', 'CSRRC']:
                        with m.If(inuop.inst[12:14] == (F3(name) & 3)):
                            if name[-1] == 'S' or name[-1] == 'C':
                                m.d.comb += uop.csr_cmd.eq(
                                    getattr(CSRCommand, name[-1])
                                    & ~Mux(uop.lrs1 == 0, CSRCommand.I, 0))
                            else:
                                m.d.comb += uop.csr_cmd.eq(
                                    getattr(CSRCommand, name[-1]))

                            with m.If(inuop.inst[14] == 1):
                                m.d.comb += [
                                    UOPC(getattr(UOpCode, name + 'I')),
                                    uop.lrs1_rtype.eq(RegisterType.PAS),
                                ]
                            with m.Else():
                                m.d.comb += [
                                    UOPC(getattr(UOpCode, name)),
                                    uop.lrs1_rtype.eq(RegisterType.FIX),
                                ]

            #
            # Fence
            #

            with m.Case(OPV('FENCE')):
                m.d.comb += [
                    uop.iq_type.eq(IssueQueueType.INT),
                ]

                with m.Switch(inuop.inst[12:15]):
                    with m.Case(F3('FENCE')):
                        m.d.comb += [
                            UOPC(UOpCode.FENCE),
                            uop.fu_type.eq(FUType.MEM),
                            uop.uses_stq.eq(1),
                            uop.is_fence.eq(1),
                        ]

                    with m.Case(F3('FENCEI')):
                        m.d.comb += [
                            UOPC(UOpCode.FENCEI),
                            uop.is_fencei.eq(1),
                        ]

            #
            # Floating-point unit
            #

            if self.use_fpu:
                #
                # FSW/FSD
                #

                with m.Case(0b0100111):
                    m.d.comb += [
                        uop.fp_valid.eq(1),
                        UOPC(UOpCode.STA),
                        uop.iq_type.eq(IssueQueueType.FMEM),
                        uop.fu_type.eq(FUType.F2IMEM),
                        uop.lrs1_rtype.eq(RegisterType.FIX),
                        uop.lrs2_rtype.eq(RegisterType.FLT),
                        IMM_SEL_S,
                        uop.uses_stq.eq(1),
                        uop.mem_cmd.eq(MemoryCommand.WRITE),
                        uop.mem_size.eq(inuop.inst[12:14]),
                    ]

                    with m.Switch(uop.mem_size):
                        with m.Case(0b010):
                            m.d.comb += uop.fp_single.eq(1)
                        with m.Case(0b011):
                            pass
                        with m.Default():
                            m.d.comb += ILL_INSN

                #
                # FLW/FLD
                #

                with m.Case(0b0000111):
                    m.d.comb += [
                        uop.fp_valid.eq(1),
                        UOPC(UOpCode.LD),
                        uop.iq_type.eq(IssueQueueType.MEM),
                        uop.fu_type.eq(FUType.MEM),
                        uop.dst_rtype.eq(RegisterType.FLT),
                        uop.lrs1_rtype.eq(RegisterType.FIX),
                        IMM_SEL_I,
                        uop.uses_ldq.eq(1),
                        uop.mem_cmd.eq(MemoryCommand.READ),
                        uop.mem_size.eq(inuop.inst[12:14]),
                        uop.mem_signed.eq(~inuop.inst[14]),
                    ]

                    with m.Switch(uop.mem_size):
                        with m.Case(0b010):
                            m.d.comb += uop.fp_single.eq(1)
                        with m.Case(0b011):
                            pass
                        with m.Default():
                            m.d.comb += ILL_INSN

                #
                # Floating point arithmetic
                #

                with m.Case(0b1010011):
                    m.d.comb += [
                        uop.iq_type.eq(IssueQueueType.FP),
                        uop.fu_type.eq(FUType.FPU),
                        uop.dst_rtype.eq(RegisterType.FLT),
                        uop.lrs1_rtype.eq(RegisterType.FLT),
                        uop.lrs2_rtype.eq(RegisterType.FLT),
                    ]

                    with m.Switch(inuop.inst[25:27]):
                        with m.Case(0b00):
                            m.d.comb += uop.fp_single.eq(1)
                        with m.Case(0b01):
                            pass
                        with m.Default():
                            m.d.comb += ILL_INSN

                    with m.Switch(inuop.inst[27:32]):
                        with m.Case(0b00000):  # fadd.fmt
                            m.d.comb += UOPC(
                                Mux(uop.fp_single, UOpCode.FADD_S,
                                    UOpCode.FADD_D))

                        with m.Case(0b00001):  # fsub.fmt
                            m.d.comb += UOPC(
                                Mux(uop.fp_single, UOpCode.FSUB_S,
                                    UOpCode.FSUB_D))

                        with m.Case(0b00010):  # fmul.fmt
                            m.d.comb += UOPC(
                                Mux(uop.fp_single, UOpCode.FMUL_S,
                                    UOpCode.FMUL_D))

                        with m.Default():
                            m.d.comb += ILL_INSN

                # Floating point fused arithmetic (fmadd.fmt, fmsub.fmt, fnmadd.fmt, fnmsub.fmt)
                with m.Case(0b1000011, 0b1000111, 0b1001011, 0b1001111):
                    m.d.comb += [
                        uop.iq_type.eq(IssueQueueType.FP),
                        uop.fu_type.eq(FUType.FPU),
                        uop.dst_rtype.eq(RegisterType.FLT),
                        uop.lrs1_rtype.eq(RegisterType.FLT),
                        uop.lrs2_rtype.eq(RegisterType.FLT),
                        uop.frs3_en.eq(1),
                    ]

                    with m.Switch(inuop.inst[25:27]):
                        with m.Case(0b00):
                            m.d.comb += uop.fp_single.eq(1)
                        with m.Case(0b01):
                            pass
                        with m.Default():
                            m.d.comb += ILL_INSN

                    with m.Switch(inuop.inst[0:7]):
                        with m.Case(0b1000011):
                            m.d.comb += UOPC(
                                Mux(uop.fp_single, UOpCode.FMADD_S,
                                    UOpCode.FMADD_D))

                        with m.Case(0b1000111):
                            m.d.comb += UOPC(
                                Mux(uop.fp_single, UOpCode.FMSUB_S,
                                    UOpCode.FMSUB_D))

                        with m.Case(0b1001011):
                            m.d.comb += UOPC(
                                Mux(uop.fp_single, UOpCode.FNMADD_S,
                                    UOpCode.FNMADD_D))

                        with m.Case(0b1001111):
                            m.d.comb += UOPC(
                                Mux(uop.fp_single, UOpCode.FNMSUB_S,
                                    UOpCode.FNMSUB_D))

            with m.Default():
                m.d.comb += ILL_INSN

        di20_25 = Mux((imm_sel == ImmSel.B) | (imm_sel == ImmSel.S),
                      inuop.inst[7:12], inuop.inst[20:25])
        m.d.comb += uop.imm_packed.eq(
            Cat(inuop.inst[12:20], di20_25, inuop.inst[25:32]))

        with m.If(self.interrupt):
            m.d.comb += [
                uop.exception.eq(1),
                uop.exc_cause.eq(self.interrupt_cause),
            ]
        with m.Elif(inuop.bp_debug_if):
            m.d.comb += [
                uop.exception.eq(1),
                uop.exc_cause.eq(Cause.DEBUG_TRIGGER),
            ]
        with m.Elif(inuop.bp_exc_if):
            m.d.comb += [
                uop.exception.eq(1),
                uop.exc_cause.eq(Cause.BREAKPOINT),
            ]
        with m.Elif(insn_illegal):
            m.d.comb += [
                uop.exception.eq(1),
                uop.exc_cause.eq(Cause.ILLEGAL_INSTRUCTION),
            ]

        return m


class DecodeStage(Elaboratable):

    def __init__(self, params, sim_debug=False):
        self.xlen = params['xlen']
        self.core_width = params['core_width']
        self.params = params
        self.sim_debug = sim_debug

        self.fetch_packet = [MicroOp(params) for _ in range(self.core_width)]
        self.fetch_packet_valid = Signal()

        self.uops = [
            MicroOp(params, name=f'dec_uop{i}') for i in range(self.core_width)
        ]

        self.valids = Signal(self.core_width)
        self.fire = Signal(self.core_width)
        self.ready = Signal()

        self.rollback = Signal()
        self.redirect_flush = Signal()
        self.dis_ready = Signal()

        self.br_update = BranchUpdate(self.params)
        self.flush_pipeline = Signal()

        self.interrupt = Signal()
        self.interrupt_cause = Signal(self.xlen)
        self.single_step = Signal()

        if sim_debug:
            self.id_debug = [
                IDDebug(params, name=f'id_debug{i}')
                for i in range(self.core_width)
            ]

    def elaborate(self, platform):
        m = Module()

        dec_finished_mask = Signal(self.core_width)

        single_stepped = Signal()
        with m.If(~self.single_step):
            m.d.sync += single_stepped.eq(0)
        with m.Elif(self.fire[0]):
            # Only allow one instruction to get past decode
            m.d.sync += single_stepped.eq(1)

        for i in range(self.core_width):
            dec = DecodeUnit(self.params)
            m.submodules += dec

            m.d.comb += [
                self.valids[i].eq(self.fetch_packet_valid
                                  & self.fetch_packet[i].valid
                                  & ~dec_finished_mask[i]),
                dec.in_uop.eq(self.fetch_packet[i]),
                self.uops[i].eq(dec.out_uop),
                dec.interrupt.eq(self.interrupt),
                dec.interrupt_cause.eq(self.interrupt_cause),
            ]

            with m.If(self.single_step & (single_stepped if (i == 0) else 1)):
                m.d.comb += [
                    dec.interrupt.eq(1),
                    dec.interrupt_cause.eq(Cause.DEBUG_INTERRUPT),
                ]

        #
        # Branch mask allocation
        #

        br_mask_alloc = m.submodules.br_mask_alloc = BranchMaskAllocator(
            self.params)
        m.d.comb += br_mask_alloc.br_update.eq(self.br_update)
        for w in range(self.core_width):
            uop = self.uops[w]

            m.d.comb += [
                br_mask_alloc.flush.eq(self.flush_pipeline),
                br_mask_alloc.is_branch[w].eq(~dec_finished_mask[w]
                                              & uop.allocate_brtag()),
                br_mask_alloc.reqs[w].eq(self.fire[w]
                                         & uop.allocate_brtag()),
                uop.br_tag.eq(br_mask_alloc.br_tag[w]),
                uop.br_mask.eq(br_mask_alloc.br_mask[w]),
            ]

        dec_hazards = [
            (valid &
             (~self.dis_ready | br_mask_full | self.rollback |
              (self.br_update.mispredict_mask != 0)
              | self.br_update.br_res.mispredict | self.redirect_flush))
            for valid, br_mask_full in zip(self.valids, br_mask_alloc.full)
        ]
        dec_stalls = Signal(self.core_width)
        m.d.comb += dec_stalls[0].eq(dec_hazards[0])
        for i in range(1, self.core_width):
            m.d.comb += dec_stalls[i].eq(dec_hazards[i] | dec_stalls[i - 1])

        m.d.comb += [
            self.fire.eq(self.valids & ~dec_stalls),
            self.ready.eq(self.fire[-1]),
        ]
        with m.If((self.ready == 1) | self.redirect_flush):
            m.d.sync += dec_finished_mask.eq(0)
        with m.Else():
            m.d.sync += dec_finished_mask.eq(dec_finished_mask | self.fire)

        if self.sim_debug:
            for w in range(self.core_width):
                id_debug = self.id_debug[w]

                m.d.comb += [
                    id_debug.valid.eq(self.fire[w]),
                    id_debug.uop_id.eq(self.uops[w].uop_id),
                    id_debug.br_mask.eq(self.uops[w].br_mask),
                ]

        return m
