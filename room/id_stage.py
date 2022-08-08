from amaranth import *
import riscvmodel.insn as insn

from room.consts import *
from room.types import MicroOp
from room.branch import BranchMaskAllocator, BranchUpdate


class BranchSignals(Record):

    def __init__(self, name=None):
        super().__init__([
            ('cfi_type', Shape.cast(CFIType).width),
            ('target', 32),
        ],
                         name=name)


class BranchDecoder(Elaboratable):

    def __init__(self):
        self.inst = Signal(32)
        self.pc = Signal(32)

        self.out = BranchSignals()

    def elaborate(self, platform):
        m = Module()

        def compute_jal_target(pc, inst):
            j_imm32 = Cat(Const(0, 1), inst[21:25], inst[25:31], inst[20],
                          inst[12:20], Repl(inst[31], 12)).as_signed()
            return ((pc.as_signed() + j_imm32) & -2)[:32].as_unsigned()

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
        self.in_uop = MicroOp(params)
        self.out_uop = MicroOp(params)

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
            uop.ldst_valid.eq((uop.dst_rtype != RegisterType.X) & ~(
                (uop.dst_rtype == RegisterType.FIX) & (uop.ldst == 0))),
        ]

        OPV = lambda name: getattr(insn, f'Instruction{name}'
                                   ).field_opcode.value
        F3 = lambda name: getattr(insn, f'Instruction{name}'
                                  ).field_funct3.value
        F7 = lambda name: getattr(insn, f'Instruction{name}'
                                  ).field_funct7.value

        UOPC = lambda x: uop.opcode.eq(x)
        ILL_INSN = insn_illegal.eq(1)

        imm_sel = Signal(ImmSel)

        IMM_SEL_I = imm_sel.eq(ImmSel.I)
        IMM_SEL_J = imm_sel.eq(ImmSel.J)
        IMM_SEL_B = imm_sel.eq(ImmSel.B)

        with m.Switch(inuop.inst[0:7]):
            with m.Case(OPV('JAL')):
                m.d.comb += [
                    UOPC(UOpCode.JAL),
                    uop.iq_type.eq(IssueQueueType.INT),
                    uop.fu_type.eq(FUType.JMP),
                    uop.dst_rtype.eq(RegisterType.FIX),
                    IMM_SEL_J,
                    uop.is_jal.eq(1),
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

            with m.Case(OPV('AUIPC')):
                pass

            # Register-immediate
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
                        with m.If(inuop.inst[25:32] != 0):
                            m.d.comb += ILL_INSN

                    with m.Case(F3('SRLI')):
                        with m.Switch(inuop.inst[25:32]):
                            with m.Case(F7('SRLI')):
                                m.d.comb += UOPC(UOpCode.SRLI)
                            with m.Case(F7('SRAI')):
                                m.d.comb += UOPC(UOpCode.SRAI)
                            with m.Default():
                                m.d.comb += ILL_INSN

            # Register-register
            with m.Case(OPV('ADD')):
                m.d.comb += [
                    uop.iq_type.eq(IssueQueueType.INT),
                    uop.dst_rtype.eq(RegisterType.FIX),
                    uop.lrs1_rtype.eq(RegisterType.FIX),
                    uop.lrs2_rtype.eq(RegisterType.FIX),
                ]

        di20_25 = Mux((imm_sel == ImmSel.B) | (imm_sel == ImmSel.S),
                      inuop.inst[7:12], inuop.inst[20:25])
        m.d.comb += uop.imm_packed.eq(
            Cat(inuop.inst[12:20], di20_25, inuop.inst[25:32]))

        return m


class DecodeStage(Elaboratable):

    def __init__(self, params):
        self.core_width = params['core_width']
        self.params = params

        self.fetch_packet = [MicroOp(params) for _ in range(self.core_width)]
        self.fetch_packet_valid = Signal()

        self.uops = [
            MicroOp(params, name=f'dec_uop{i}') for i in range(self.core_width)
        ]

        self.valids = Signal(self.core_width)
        self.fire = Signal(self.core_width)
        self.ready = Signal()

        self.redirect_flush = Signal()
        self.dis_ready = Signal()

        self.br_update = BranchUpdate(self.params)

    def elaborate(self, platform):
        m = Module()

        dec_finished_mask = Signal(self.core_width)

        for i in range(self.core_width):
            dec = DecodeUnit(self.params)
            m.submodules += dec

            m.d.comb += [
                self.valids[i].eq(self.fetch_packet_valid
                                  & self.fetch_packet[i].valid
                                  & ~dec_finished_mask[i]),
                dec.in_uop.eq(self.fetch_packet[i]),
                self.uops[i].eq(dec.out_uop),
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
                br_mask_alloc.is_branch[w].eq(~dec_finished_mask[w]
                                              & uop.allocate_brtag()),
                br_mask_alloc.reqs[w].eq(self.fire[w]
                                         & uop.allocate_brtag()),
                uop.br_tag.eq(br_mask_alloc.br_tag[w]),
                uop.br_mask.eq(br_mask_alloc.br_mask[w]),
            ]

        dec_hazards = [
            (valid &
             (~self.dis_ready | br_mask_full |
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

        return m
