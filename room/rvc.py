from amaranth import *
import riscvmodel.insn as insn


class RVCDecoder(Elaboratable):

    def __init__(self, xlen):
        self.xlen = xlen

        self.instr_i = Signal(32)
        self.instr_o = Signal(32)

    def elaborate(self, platform):
        m = Module()

        instr_i = self.instr_i
        instr_o = self.instr_o

        OPV7 = lambda name: Const(
            getattr(insn, f'Instruction{name}').field_opcode.value, 7)

        rs1p = Cat(instr_i[7:10], Const(1, 2))
        rs2p = Cat(instr_i[2:5], Const(1, 2))
        rs2 = instr_i[2:7]
        rd = instr_i[7:12]
        addi4spn_imm = Cat(Const(0, 2), instr_i[6], instr_i[5], instr_i[11:13],
                           instr_i[7:11])
        lw_imm = Cat(Const(0, 2), instr_i[6], instr_i[10:13], instr_i[5])
        ld_imm = Cat(Const(0, 3), instr_i[10:13], instr_i[5:7])
        lwsp_imm = Cat(Const(0, 2), instr_i[4:7], instr_i[12], instr_i[2:4])
        ldsp_imm = Cat(Const(0, 3), instr_i[5:7], instr_i[12], instr_i[2:5])
        swsp_imm = Cat(Const(0, 2), instr_i[9:13], instr_i[7:9])
        sdsp_imm = Cat(Const(0, 3), instr_i[10:13], instr_i[7:10])
        lui_imm = Cat(Const(0, 12), instr_i[2:7], instr_i[12].replicate(15))
        addi16sp_imm = Cat(Const(0, 4), instr_i[6], instr_i[2], instr_i[5],
                           instr_i[3:5], instr_i[12].replicate(3))
        addi_imm = Cat(instr_i[2:7], instr_i[12].replicate(7))
        j_imm = Cat(Const(0, 1), instr_i[3:6], instr_i[11], instr_i[2],
                    instr_i[7], instr_i[6], instr_i[9:11], instr_i[8],
                    instr_i[12].replicate(10))
        b_imm = Cat(Const(0, 1), instr_i[3:5], instr_i[10:12], instr_i[2],
                    instr_i[5:7], instr_i[12].replicate(5))
        shmat = Cat(instr_i[2:7], instr_i[12])

        x0 = Const(0, 5)
        ra = Const(1, 5)
        sp = Const(2, 5)

        with m.Switch(instr_i[0:2]):
            # C0
            with m.Case(0b00):
                with m.Switch(instr_i[13:16]):
                    with m.Case(0b000):  # c.addi4spn
                        m.d.comb += instr_o.eq(
                            Cat(OPV7('ADDI'), rs2p, Const(0, 3), sp,
                                addi4spn_imm))

                    with m.Case(0b001):  # c.fld
                        m.d.comb += instr_o.eq(
                            Cat(Const(0x07, 7), rs2p, Const(3, 3), rs1p,
                                ld_imm))

                    with m.Case(0b010):  # c.lw
                        m.d.comb += instr_o.eq(
                            Cat(OPV7('LW'), rs2p, Const(2, 3), rs1p, lw_imm))

                    with m.Case(0b011):
                        if self.xlen == 32:  # c.flw
                            m.d.comb += instr_o.eq(
                                Cat(Const(0x07, 7), rs2p, Const(2, 3), rs1p,
                                    lw_imm))
                        else:  # c.ld
                            m.d.comb += instr_o.eq(
                                Cat(OPV7('LD'), rs2p, Const(3, 3), rs1p,
                                    ld_imm))

                    with m.Case(0b101):  # c.fsd
                        m.d.comb += instr_o.eq(
                            Cat(Const(0x27, 7), ld_imm[:5], Const(3, 3), rs1p,
                                rs2p, ld_imm >> 5))

                    with m.Case(0b110):  # c.sw
                        m.d.comb += instr_o.eq(
                            Cat(OPV7('SW'), ld_imm[:5], Const(2, 3), rs1p,
                                rs2p, ld_imm >> 5))

                    with m.Case(0b111):
                        if self.xlen == 32:  # c.fsw
                            m.d.comb += instr_o.eq(
                                Cat(Const(0x27, 7), ld_imm[:5], Const(2, 3),
                                    rs1p, rs2p, ld_imm >> 5))

                        else:  # c.sd
                            m.d.comb += instr_o.eq(
                                Cat(OPV7('SD'), ld_imm[:5], Const(3, 3), rs1p,
                                    rs2p, ld_imm >> 5))

            # C1
            with m.Case(0b01):
                with m.Switch(instr_i[13:16]):
                    with m.Case(0b000):  # c.addi
                        m.d.comb += instr_o.eq(
                            Cat(OPV7('ADDI'), rd, Const(0, 3), rd, addi_imm))

                    with m.Case(0b001):
                        if self.xlen == 32:  # c.jal
                            m.d.comb += instr_o.eq(
                                Cat(OPV7('JAL'), ra, j_imm[12:20], j_imm[11],
                                    j_imm[1:11], j_imm[20]))
                        else:  # c.addiw
                            m.d.comb += instr_o.eq(
                                Cat(Mux(rd.any(), Const(0x1b, 7), 0), rd,
                                    Const(0, 3), rd, addi_imm))

                    with m.Case(0b010):  # c.li
                        m.d.comb += instr_o.eq(
                            Cat(OPV7('ADDI'), rd, Const(0, 3), x0, addi_imm))

                    with m.Case(0b011):
                        addi16sp = Cat(Mux(addi_imm.any(), OPV7('ADDI'), 0),
                                       rd, Const(0, 3), rd, addi16sp_imm)
                        lui = Cat(Mux(addi_imm.any(), OPV7('LUI'), 0), rd,
                                  lui_imm)
                        m.d.comb += instr_o.eq(
                            Mux((rd == x0) | (rd == sp), addi16sp, lui))

                    with m.Case(0b100):
                        srli = Cat(OPV7('ADDI'), rs1p, Const(5, 3), rs1p,
                                   shmat)
                        srai = srli | (1 << 30)
                        andi = Cat(OPV7('ADDI'), rs1p, Const(7, 3), rs1p,
                                   addi_imm)

                        with m.Switch(instr_i[10:12]):
                            with m.Case(0b00):  # c.srli
                                m.d.comb += instr_o.eq(srli)
                            with m.Case(0b01):  # c.srai
                                m.d.comb += instr_o.eq(srai)
                            with m.Case(0b10):  # c.andi
                                m.d.comb += instr_o.eq(andi)

                            with m.Case(0b11):
                                with m.Switch(Cat(instr_i[5:7], instr_i[12])):
                                    with m.Case(0b000):  # c.sub
                                        m.d.comb += instr_o.eq(
                                            Cat(OPV7('ADD'), rs1p, Const(0, 3),
                                                rs1p, rs2p) | (1 << 30))

                                    with m.Case(0b001):  # c.xor
                                        m.d.comb += instr_o.eq(
                                            Cat(OPV7('ADD'), rs1p, Const(4, 3),
                                                rs1p, rs2p))

                                    with m.Case(0b010):  # c.or
                                        m.d.comb += instr_o.eq(
                                            Cat(OPV7('ADD'), rs1p, Const(6, 3),
                                                rs1p, rs2p))

                                    with m.Case(0b011):  # c.and
                                        m.d.comb += instr_o.eq(
                                            Cat(OPV7('ADD'), rs1p, Const(7, 3),
                                                rs1p, rs2p))

                                    with m.Case(0b100):  # c.subw
                                        m.d.comb += instr_o.eq(
                                            Cat(Const(0x3b, 7), rs1p,
                                                Const(0, 3), rs1p, rs2p)
                                            | (1 << 30))

                                    with m.Case(0b101):  # c.addw
                                        m.d.comb += instr_o.eq(
                                            Cat(Const(0x3b, 7), rs1p,
                                                Const(0, 3), rs1p, rs2p))

                    with m.Case(0b101):  # c.j
                        m.d.comb += instr_o.eq(
                            Cat(OPV7('JAL'), x0, j_imm[12:20], j_imm[11],
                                j_imm[1:11], j_imm[20]))

                    with m.Case(0b110):  # c.beqz
                        m.d.comb += instr_o.eq(
                            Cat(OPV7('BEQ'), b_imm[11], b_imm[1:5],
                                Const(0, 3), rs1p, x0, b_imm[5:11], b_imm[12]))

                    with m.Case(0b111):  # c.bnez
                        m.d.comb += instr_o.eq(
                            Cat(OPV7('BEQ'), b_imm[11], b_imm[1:5],
                                Const(1, 3), rs1p, x0, b_imm[5:11], b_imm[12]))

            # C2
            with m.Case(0b10):
                with m.Switch(instr_i[13:16]):
                    with m.Case(0b000):  # c.slli
                        m.d.comb += instr_o.eq(
                            Cat(OPV7('ADDI'), rd, Const(1, 3), rd, shmat))

                    with m.Case(0b001):  # c.fldsp
                        m.d.comb += instr_o.eq(
                            Cat(Const(0x07, 7), rd, Const(3, 3), sp, ldsp_imm))

                    with m.Case(0b010):  # c.lwsp
                        m.d.comb += instr_o.eq(
                            Cat(OPV7('LW'), rd, Const(2, 3), sp, lwsp_imm))

                    with m.Case(0b011):
                        if self.xlen == 32:  # c.flwsp
                            m.d.comb += instr_o.eq(
                                Cat(Const(0x07, 7), rd, Const(2, 3), sp,
                                    lwsp_imm))
                        else:  # c.ldsp
                            m.d.comb += instr_o.eq(
                                Cat(OPV7('LD'), rd, Const(3, 3), sp, ldsp_imm))

                    with m.Case(0b100):
                        with m.If(instr_i[12] == 0):
                            with m.If((rs2 == 0) & rd.any()):  # c.jr
                                m.d.comb += instr_o.eq(
                                    Cat(OPV7('JALR'), Const(0, 8),
                                        instr_i[7:12], Const(0, 12)))
                            with m.Elif(rs2.any() & rd.any()):  # c.mv
                                m.d.comb += instr_o.eq(
                                    Cat(OPV7('ADD'), instr_i[7:12],
                                        Const(0, 8), instr_i[2:7], Const(0,
                                                                         7)))

                        with m.Else():
                            with m.If(rs2.any() & rd.any()):  # c.add
                                m.d.comb += instr_o.eq(
                                    Cat(OPV7('ADD'), rd, Const(0, 3), rd, rs2))
                            with m.Elif((rs2 == 0) & rd.any()):  # c.jalr
                                m.d.comb += instr_o.eq(
                                    Cat(OPV7('JALR'), ra, Const(0, 3), rd,
                                        rs2))
                            with m.Elif((rs2 == 0) & (rd == 0)):  # c.ebreak
                                m.d.comb += instr_o.eq(
                                    OPV7('EBREAK') | (1 << 20))

                    with m.Case(0b101):  # c.fsdsp
                        m.d.comb += instr_o.eq(
                            Cat(Const(0x27, 7), sdsp_imm[:5], Const(3, 3), sp,
                                rs2, sdsp_imm >> 5))

                    with m.Case(0b110):  # c.swsp
                        m.d.comb += instr_o.eq(
                            Cat(OPV7('SW'), swsp_imm[:5], Const(2, 3), sp, rs2,
                                swsp_imm >> 5))

                    with m.Case(0b111):
                        if self.xlen == 32:  # c.fswsp
                            m.d.comb += instr_o.eq(
                                Cat(Const(0x27, 7), swsp_imm[:5], Const(2, 3),
                                    sp, rs2, swsp_imm >> 5))

                        else:  # c.sdsp
                            m.d.comb += instr_o.eq(
                                Cat(OPV7('SD'), sdsp_imm[:5], Const(3, 3), sp,
                                    rs2, sdsp_imm >> 5))

            with m.Case(0b11):
                m.d.comb += instr_o.eq(instr_i)

        return m
