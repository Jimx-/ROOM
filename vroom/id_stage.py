from amaranth import *

from vroom.consts import *
from vroom.types import HasVectorParams, VMicroOp, VType
from vroom.utils import vlmul_to_lmul

from room.consts import RegisterType
from room.utils import Decoupled, Valid


class VIDDebug(HasVectorParams, Record):

    def __init__(self, params, name=None, src_loc_at=0):
        HasVectorParams.__init__(self, params)

        Record.__init__(self, [
            ('uop_id', VMicroOp.ID_WIDTH),
            ('vlmul', 3),
            ('vsew', 3),
            ('vl', self.vl_bits),
        ],
                        name=name,
                        src_loc_at=1 + src_loc_at)


class DecodeUnit(HasVectorParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.vtype = VType(params)
        self.vl = Signal(self.vl_bits)
        self.vxrm = Signal(VXRoundingMode)

        self.in_uop = VMicroOp(params)
        self.out_uop = VMicroOp(params)

    def elaborate(self, platform):
        m = Module()

        inuop = self.in_uop
        uop = self.out_uop

        m.d.comb += [
            uop.eq(inuop),
            uop.vlmul_mag.eq(self.vtype.vlmul_mag),
            uop.vlmul_sign.eq(self.vtype.vlmul_sign),
            uop.vsew.eq(self.vtype.vsew),
            uop.vta.eq(self.vtype.vta),
            uop.vma.eq(self.vtype.vma),
            uop.vill.eq(self.vtype.vill),
            uop.vm.eq(inuop.inst[25]),
            uop.vl.eq(self.vl),
            uop.vxrm.eq(self.vxrm),
            uop.funct6.eq(inuop.inst[26:32]),
            uop.funct3.eq(inuop.inst[12:15]),
            uop.ldst.eq(inuop.inst[7:12]),
            uop.lrs1.eq(inuop.inst[15:20]),
            uop.lrs2.eq(inuop.inst[20:25]),
            uop.lrs3_rtype.eq(RegisterType.VEC),
            uop.ldst_valid.eq((uop.dst_rtype != RegisterType.X) & ~(
                (uop.dst_rtype == RegisterType.FIX) & (uop.ldst == 0))),
        ]

        UOPC = lambda x: uop.opcode.eq(x)

        with m.Switch(inuop.inst[0:7]):
            with m.Case(0b0000111):  # vl*
                m.d.comb += [
                    UOPC(VOpCode.VLE),
                    uop.fu_type.eq(VFUType.MEM),
                    uop.dst_rtype.eq(RegisterType.VEC),
                    uop.is_ld.eq(1),
                    uop.mem_size.eq(inuop.inst[12:14]),
                ]

            with m.Case(0b0100111):  # vs*
                m.d.comb += [
                    UOPC(VOpCode.VSE),
                    uop.fu_type.eq(VFUType.MEM),
                    uop.is_st.eq(1),
                    uop.mem_size.eq(inuop.inst[12:14]),
                ]

            with m.Case(0b1010111):
                with m.Switch(uop.funct3):
                    with m.Case(0b000):  # OPIVV
                        m.d.comb += [
                            uop.fu_type.eq(VFUType.ALU),
                            uop.dst_rtype.eq(RegisterType.VEC),
                            uop.lrs1_rtype.eq(RegisterType.VEC),
                            uop.lrs2_rtype.eq(RegisterType.VEC),
                        ]

                        with m.Switch(uop.funct6):
                            with m.Case(0b000000):
                                m.d.comb += UOPC(VOpCode.VADD)
                            with m.Case(0b000010):
                                m.d.comb += UOPC(VOpCode.VSUB)
                            with m.Case(0b000100):
                                m.d.comb += UOPC(VOpCode.VMINU)
                            with m.Case(0b000101):
                                m.d.comb += UOPC(VOpCode.VMIN)
                            with m.Case(0b000110):
                                m.d.comb += UOPC(VOpCode.VMAXU)
                            with m.Case(0b000111):
                                m.d.comb += UOPC(VOpCode.VMAX)
                            with m.Case(0b001001):
                                m.d.comb += UOPC(VOpCode.VAND)
                            with m.Case(0b001010):
                                m.d.comb += UOPC(VOpCode.VOR)
                            with m.Case(0b001011):
                                m.d.comb += UOPC(VOpCode.VXOR)
                            with m.Case(0b001100):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.PERM),
                                    UOPC(VOpCode.VRGATHER),
                                ]
                            with m.Case(0b001110):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.PERM),
                                    UOPC(VOpCode.VRGATHEREI16),
                                ]
                            with m.Case(0b010000):
                                m.d.comb += UOPC(VOpCode.VADC)
                            with m.Case(0b010001):
                                m.d.comb += [
                                    UOPC(VOpCode.VMADC),
                                    uop.narrow_to_1.eq(1),
                                ]
                            with m.Case(0b010010):
                                m.d.comb += UOPC(VOpCode.VSBC)
                            with m.Case(0b010011):
                                m.d.comb += [
                                    UOPC(VOpCode.VMSBC),
                                    uop.narrow_to_1.eq(1),
                                ]
                            with m.Case(0b010111):
                                m.d.comb += UOPC(VOpCode.VMERGE)
                            with m.Case(0b011000):
                                m.d.comb += [
                                    UOPC(VOpCode.VMSEQ),
                                    uop.narrow_to_1.eq(1),
                                ]
                            with m.Case(0b011001):
                                m.d.comb += [
                                    UOPC(VOpCode.VMSNE),
                                    uop.narrow_to_1.eq(1),
                                ]
                            with m.Case(0b011010):
                                m.d.comb += [
                                    UOPC(VOpCode.VMSLTU),
                                    uop.narrow_to_1.eq(1),
                                ]
                            with m.Case(0b011011):
                                m.d.comb += [
                                    UOPC(VOpCode.VMSLT),
                                    uop.narrow_to_1.eq(1),
                                ]
                            with m.Case(0b011100):
                                m.d.comb += [
                                    UOPC(VOpCode.VMSLEU),
                                    uop.narrow_to_1.eq(1),
                                ]
                            with m.Case(0b011101):
                                m.d.comb += [
                                    UOPC(VOpCode.VMSLE),
                                    uop.narrow_to_1.eq(1),
                                ]
                            with m.Case(0b100000):
                                m.d.comb += UOPC(VOpCode.VSADDU)
                            with m.Case(0b100001):
                                m.d.comb += UOPC(VOpCode.VSADD)
                            with m.Case(0b100010):
                                m.d.comb += UOPC(VOpCode.VSSUBU)
                            with m.Case(0b100011):
                                m.d.comb += UOPC(VOpCode.VSSUB)
                            with m.Case(0b100101):
                                m.d.comb += UOPC(VOpCode.VSLL)
                            with m.Case(0b100111):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.MUL),
                                    UOPC(VOpCode.VSMUL),
                                ]
                            with m.Case(0b101000):
                                m.d.comb += UOPC(VOpCode.VSRL)
                            with m.Case(0b101001):
                                m.d.comb += UOPC(VOpCode.VSRA)
                            with m.Case(0b101010):
                                m.d.comb += UOPC(VOpCode.VSSRL)
                            with m.Case(0b101011):
                                m.d.comb += UOPC(VOpCode.VSSRA)
                            with m.Case(0b101100):
                                m.d.comb += [
                                    UOPC(VOpCode.VNSRL),
                                    uop.narrow.eq(1),
                                ]
                            with m.Case(0b101101):
                                m.d.comb += [
                                    UOPC(VOpCode.VNSRA),
                                    uop.narrow.eq(1),
                                ]
                            with m.Case(0b101110):
                                m.d.comb += [
                                    UOPC(VOpCode.VNCLIPU),
                                    uop.narrow.eq(1),
                                ]
                            with m.Case(0b101111):
                                m.d.comb += [
                                    UOPC(VOpCode.VNCLIP),
                                    uop.narrow.eq(1),
                                ]
                            with m.Case(0b110000):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.REDUCE),
                                    UOPC(VOpCode.VWREDSUMU),
                                    uop.widen.eq(1),
                                ]
                            with m.Case(0b110001):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.REDUCE),
                                    UOPC(VOpCode.VWREDSUM),
                                    uop.widen.eq(1),
                                ]

                    with m.Case(0b001):  # OPFVV
                        pass

                    with m.Case(0b010):  # OPMVV
                        m.d.comb += [
                            uop.fu_type.eq(VFUType.ALU),
                            uop.dst_rtype.eq(RegisterType.VEC),
                            uop.lrs1_rtype.eq(RegisterType.VEC),
                            uop.lrs2_rtype.eq(RegisterType.VEC),
                        ]

                        with m.Switch(uop.funct6):
                            with m.Case(0b000000):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.REDUCE),
                                    UOPC(VOpCode.VREDSUM),
                                ]
                            with m.Case(0b000001):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.REDUCE),
                                    UOPC(VOpCode.VREDAND),
                                ]
                            with m.Case(0b000010):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.REDUCE),
                                    UOPC(VOpCode.VREDOR),
                                ]
                            with m.Case(0b000011):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.REDUCE),
                                    UOPC(VOpCode.VREDXOR),
                                ]
                            with m.Case(0b000100):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.REDUCE),
                                    UOPC(VOpCode.VREDMINU),
                                ]
                            with m.Case(0b000101):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.REDUCE),
                                    UOPC(VOpCode.VREDMIN),
                                ]
                            with m.Case(0b000110):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.REDUCE),
                                    UOPC(VOpCode.VREDMAXU),
                                ]
                            with m.Case(0b000111):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.REDUCE),
                                    UOPC(VOpCode.VREDMAX),
                                ]
                            with m.Case(0b001000):
                                m.d.comb += UOPC(VOpCode.VAADDU)
                            with m.Case(0b001001):
                                m.d.comb += UOPC(VOpCode.VAADD)
                            with m.Case(0b001010):
                                m.d.comb += UOPC(VOpCode.VASUBU)
                            with m.Case(0b001011):
                                m.d.comb += UOPC(VOpCode.VASUB)
                            with m.Case(0b010000):  # VWXUNARY0
                                m.d.comb += [
                                    uop.dst_rtype.eq(RegisterType.FIX),
                                    uop.lrs1_rtype.eq(RegisterType.X),
                                ]

                                with m.Switch(uop.lrs1):
                                    with m.Case(0b00000):
                                        m.d.comb += UOPC(VOpCode.VMVXS)
                                    with m.Case(0b10000):
                                        m.d.comb += [
                                            uop.fu_type.eq(VFUType.MASK),
                                            UOPC(VOpCode.VCPOP),
                                        ]
                                    with m.Case(0b10001):
                                        m.d.comb += [
                                            uop.fu_type.eq(VFUType.MASK),
                                            UOPC(VOpCode.VFIRST),
                                        ]

                            with m.Case(0b010010):  # VXUNARY0
                                m.d.comb += uop.lrs1_rtype.eq(RegisterType.X)

                                with m.Switch(uop.lrs1):
                                    with m.Case(0b00010, 0b00100, 0b00110):
                                        m.d.comb += UOPC(VOpCode.VZEXT)
                                    with m.Case(0b00011, 0b00101, 0b00111):
                                        m.d.comb += UOPC(VOpCode.VSEXT)

                            with m.Case(0b010100):  # VMUNARY0
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.MASK),
                                    uop.lrs1_rtype.eq(RegisterType.X),
                                ]

                                with m.Switch(uop.lrs1):
                                    with m.Case(0b00001):
                                        m.d.comb += UOPC(VOpCode.VMSBF)
                                    with m.Case(0b00010):
                                        m.d.comb += UOPC(VOpCode.VMSOF)
                                    with m.Case(0b00011):
                                        m.d.comb += UOPC(VOpCode.VMSIF)
                                    with m.Case(0b10000):
                                        m.d.comb += UOPC(VOpCode.VIOTA)
                                    with m.Case(0b10001):
                                        m.d.comb += UOPC(VOpCode.VID)

                            with m.Case(0b010111):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.PERM),
                                    UOPC(VOpCode.VCOMPRESS),
                                ]
                            with m.Case(0b011000):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.MASK),
                                    UOPC(VOpCode.VMANDNOT),
                                ]
                            with m.Case(0b011001):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.MASK),
                                    UOPC(VOpCode.VMAND),
                                ]
                            with m.Case(0b011010):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.MASK),
                                    UOPC(VOpCode.VMOR),
                                ]
                            with m.Case(0b011011):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.MASK),
                                    UOPC(VOpCode.VMXOR),
                                ]
                            with m.Case(0b011100):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.MASK),
                                    UOPC(VOpCode.VMORNOT),
                                ]
                            with m.Case(0b011101):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.MASK),
                                    UOPC(VOpCode.VMNAND),
                                ]
                            with m.Case(0b011110):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.MASK),
                                    UOPC(VOpCode.VMNOR),
                                ]
                            with m.Case(0b011111):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.MASK),
                                    UOPC(VOpCode.VMXNOR),
                                ]
                            with m.Case(0b100000):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.DIV),
                                    UOPC(VOpCode.VDIVU),
                                ]
                            with m.Case(0b100001):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.DIV),
                                    UOPC(VOpCode.VDIV),
                                ]
                            with m.Case(0b100010):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.DIV),
                                    UOPC(VOpCode.VREMU),
                                ]
                            with m.Case(0b100011):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.DIV),
                                    UOPC(VOpCode.VREM),
                                ]
                            with m.Case(0b100100):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.MUL),
                                    UOPC(VOpCode.VMULHU),
                                ]
                            with m.Case(0b100101):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.MUL),
                                    UOPC(VOpCode.VMUL),
                                ]
                            with m.Case(0b100110):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.MUL),
                                    UOPC(VOpCode.VMULHSU),
                                ]
                            with m.Case(0b100111):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.MUL),
                                    UOPC(VOpCode.VMULH),
                                ]
                            with m.Case(0b101001):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.MUL),
                                    UOPC(VOpCode.VMADD),
                                ]
                            with m.Case(0b101011):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.MUL),
                                    UOPC(VOpCode.VNMSUB),
                                ]
                            with m.Case(0b101101):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.MUL),
                                    UOPC(VOpCode.VMACC),
                                ]
                            with m.Case(0b101111):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.MUL),
                                    UOPC(VOpCode.VNMSAC),
                                ]
                            with m.Case(0b110000):  # vwaddu
                                m.d.comb += [
                                    UOPC(VOpCode.VADDU),
                                    uop.widen.eq(1),
                                ]
                            with m.Case(0b110001):  # vwadd
                                m.d.comb += [
                                    UOPC(VOpCode.VADD),
                                    uop.widen.eq(1),
                                ]
                            with m.Case(0b110010):  # vwsubu
                                m.d.comb += [
                                    UOPC(VOpCode.VSUBU),
                                    uop.widen.eq(1),
                                ]
                            with m.Case(0b110011):  # vwsub
                                m.d.comb += [
                                    UOPC(VOpCode.VSUB),
                                    uop.widen.eq(1),
                                ]
                            with m.Case(0b110100):  # vwaddu.w
                                m.d.comb += [
                                    UOPC(VOpCode.VADDU),
                                    uop.widen2.eq(1),
                                ]
                            with m.Case(0b110101):  # vwadd.w
                                m.d.comb += [
                                    UOPC(VOpCode.VADD),
                                    uop.widen2.eq(1),
                                ]
                            with m.Case(0b110110):  # vwsubu.w
                                m.d.comb += [
                                    UOPC(VOpCode.VSUBU),
                                    uop.widen2.eq(1),
                                ]
                            with m.Case(0b110111):  # vwsub.w
                                m.d.comb += [
                                    UOPC(VOpCode.VSUB),
                                    uop.widen2.eq(1),
                                ]
                            with m.Case(0b111000):  # vwmulu
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.MUL),
                                    UOPC(VOpCode.VMULHU),
                                    uop.widen.eq(1),
                                ]
                            with m.Case(0b111010):  # vwmulsu
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.MUL),
                                    UOPC(VOpCode.VMULHSU),
                                    uop.widen.eq(1),
                                ]
                            with m.Case(0b111011):  # vwmul
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.MUL),
                                    UOPC(VOpCode.VMULH),
                                    uop.widen.eq(1),
                                ]
                            with m.Case(0b111100):  # vwmaccu
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.MUL),
                                    UOPC(VOpCode.VMACCU),
                                    uop.widen.eq(1),
                                ]
                            with m.Case(0b111101):  # vwmacc
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.MUL),
                                    UOPC(VOpCode.VMACC),
                                    uop.widen.eq(1),
                                ]
                            with m.Case(0b111111):  # vwmaccsu
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.MUL),
                                    UOPC(VOpCode.VMACCSU),
                                    uop.widen.eq(1),
                                ]

                    with m.Case(0b011):  # OPIVI
                        m.d.comb += [
                            uop.fu_type.eq(VFUType.ALU),
                            uop.dst_rtype.eq(RegisterType.VEC),
                            uop.lrs2_rtype.eq(RegisterType.VEC),
                        ]

                        with m.Switch(uop.funct6):
                            with m.Case(0b000000):
                                m.d.comb += UOPC(VOpCode.VADD)
                            with m.Case(0b000011):
                                m.d.comb += UOPC(VOpCode.VRSUB)
                            with m.Case(0b001001):
                                m.d.comb += UOPC(VOpCode.VAND)
                            with m.Case(0b001010):
                                m.d.comb += UOPC(VOpCode.VOR)
                            with m.Case(0b001011):
                                m.d.comb += UOPC(VOpCode.VXOR)
                            with m.Case(0b001100):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.PERM),
                                    UOPC(VOpCode.VRGATHER),
                                ]
                            with m.Case(0b001110):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.PERM),
                                    UOPC(VOpCode.VSLIDEUP),
                                ]
                            with m.Case(0b001111):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.PERM),
                                    UOPC(VOpCode.VSLIDEDOWN),
                                ]
                            with m.Case(0b010000):
                                m.d.comb += UOPC(VOpCode.VADC)
                            with m.Case(0b010001):
                                m.d.comb += [
                                    UOPC(VOpCode.VMADC),
                                    uop.narrow_to_1.eq(1),
                                ]
                            with m.Case(0b010111):
                                m.d.comb += UOPC(VOpCode.VMERGE)
                            with m.Case(0b011000):
                                m.d.comb += [
                                    UOPC(VOpCode.VMSEQ),
                                    uop.narrow_to_1.eq(1),
                                ]
                            with m.Case(0b011001):
                                m.d.comb += [
                                    UOPC(VOpCode.VMSNE),
                                    uop.narrow_to_1.eq(1),
                                ]
                            with m.Case(0b011100):
                                m.d.comb += [
                                    UOPC(VOpCode.VMSLEU),
                                    uop.narrow_to_1.eq(1),
                                ]
                            with m.Case(0b011101):
                                m.d.comb += [
                                    UOPC(VOpCode.VMSLE),
                                    uop.narrow_to_1.eq(1),
                                ]
                            with m.Case(0b100000):
                                m.d.comb += UOPC(VOpCode.VSADDU)
                            with m.Case(0b100001):
                                m.d.comb += UOPC(VOpCode.VSADD)
                            with m.Case(0b100101):
                                m.d.comb += UOPC(VOpCode.VSLL)
                            with m.Case(0b101000):
                                m.d.comb += UOPC(VOpCode.VSRL)
                            with m.Case(0b101001):
                                m.d.comb += UOPC(VOpCode.VSRA)
                            with m.Case(0b101010):
                                m.d.comb += UOPC(VOpCode.VSSRL)
                            with m.Case(0b101011):
                                m.d.comb += UOPC(VOpCode.VSSRA)
                            with m.Case(0b101100):
                                m.d.comb += [
                                    UOPC(VOpCode.VNSRL),
                                    uop.narrow.eq(1),
                                ]
                            with m.Case(0b101101):
                                m.d.comb += [
                                    UOPC(VOpCode.VNSRA),
                                    uop.narrow.eq(1),
                                ]
                            with m.Case(0b101110):
                                m.d.comb += [
                                    UOPC(VOpCode.VNCLIPU),
                                    uop.narrow.eq(1),
                                ]
                            with m.Case(0b101111):
                                m.d.comb += [
                                    UOPC(VOpCode.VNCLIP),
                                    uop.narrow.eq(1),
                                ]

                    with m.Case(0b100):  # OPIVX
                        m.d.comb += [
                            uop.fu_type.eq(VFUType.ALU),
                            uop.dst_rtype.eq(RegisterType.VEC),
                            uop.lrs1_rtype.eq(RegisterType.FIX),
                            uop.lrs2_rtype.eq(RegisterType.VEC),
                        ]

                        with m.Switch(uop.funct6):
                            with m.Case(0b000000):
                                m.d.comb += UOPC(VOpCode.VADD)
                            with m.Case(0b000010):
                                m.d.comb += UOPC(VOpCode.VSUB)
                            with m.Case(0b000011):
                                m.d.comb += UOPC(VOpCode.VRSUB)
                            with m.Case(0b000100):
                                m.d.comb += UOPC(VOpCode.VMINU)
                            with m.Case(0b000101):
                                m.d.comb += UOPC(VOpCode.VMIN)
                            with m.Case(0b000110):
                                m.d.comb += UOPC(VOpCode.VMAXU)
                            with m.Case(0b000111):
                                m.d.comb += UOPC(VOpCode.VMAX)
                            with m.Case(0b001001):
                                m.d.comb += UOPC(VOpCode.VAND)
                            with m.Case(0b001010):
                                m.d.comb += UOPC(VOpCode.VOR)
                            with m.Case(0b001011):
                                m.d.comb += UOPC(VOpCode.VXOR)
                            with m.Case(0b001100):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.PERM),
                                    UOPC(VOpCode.VRGATHER),
                                ]
                            with m.Case(0b001110):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.PERM),
                                    UOPC(VOpCode.VSLIDEUP),
                                ]
                            with m.Case(0b001111):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.PERM),
                                    UOPC(VOpCode.VSLIDEDOWN),
                                ]
                            with m.Case(0b010000):
                                m.d.comb += UOPC(VOpCode.VADC)
                            with m.Case(0b010001):
                                m.d.comb += [
                                    UOPC(VOpCode.VMADC),
                                    uop.narrow_to_1.eq(1),
                                ]
                            with m.Case(0b010010):
                                m.d.comb += UOPC(VOpCode.VSBC)
                            with m.Case(0b010011):
                                m.d.comb += [
                                    UOPC(VOpCode.VMSBC),
                                    uop.narrow_to_1.eq(1),
                                ]
                            with m.Case(0b010111):
                                m.d.comb += UOPC(VOpCode.VMERGE)
                            with m.Case(0b011000):
                                m.d.comb += [
                                    UOPC(VOpCode.VMSEQ),
                                    uop.narrow_to_1.eq(1),
                                ]
                            with m.Case(0b011001):
                                m.d.comb += [
                                    UOPC(VOpCode.VMSNE),
                                    uop.narrow_to_1.eq(1),
                                ]
                            with m.Case(0b011010):
                                m.d.comb += [
                                    UOPC(VOpCode.VMSLTU),
                                    uop.narrow_to_1.eq(1),
                                ]
                            with m.Case(0b011011):
                                m.d.comb += [
                                    UOPC(VOpCode.VMSLT),
                                    uop.narrow_to_1.eq(1),
                                ]
                            with m.Case(0b011100):
                                m.d.comb += [
                                    UOPC(VOpCode.VMSLEU),
                                    uop.narrow_to_1.eq(1),
                                ]
                            with m.Case(0b011101):
                                m.d.comb += [
                                    UOPC(VOpCode.VMSLE),
                                    uop.narrow_to_1.eq(1),
                                ]
                            with m.Case(0b011110):
                                m.d.comb += [
                                    UOPC(VOpCode.VMSGTU),
                                    uop.narrow_to_1.eq(1),
                                ]
                            with m.Case(0b011111):
                                m.d.comb += [
                                    UOPC(VOpCode.VMSGT),
                                    uop.narrow_to_1.eq(1),
                                ]
                            with m.Case(0b100000):
                                m.d.comb += UOPC(VOpCode.VSADDU)
                            with m.Case(0b100001):
                                m.d.comb += UOPC(VOpCode.VSADD)
                            with m.Case(0b100010):
                                m.d.comb += UOPC(VOpCode.VSSUBU)
                            with m.Case(0b100011):
                                m.d.comb += UOPC(VOpCode.VSSUB)
                            with m.Case(0b100101):
                                m.d.comb += UOPC(VOpCode.VSLL)
                            with m.Case(0b100111):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.MUL),
                                    UOPC(VOpCode.VSMUL),
                                ]
                            with m.Case(0b101000):
                                m.d.comb += UOPC(VOpCode.VSRL)
                            with m.Case(0b101001):
                                m.d.comb += UOPC(VOpCode.VSRA)
                            with m.Case(0b101010):
                                m.d.comb += UOPC(VOpCode.VSSRL)
                            with m.Case(0b101011):
                                m.d.comb += UOPC(VOpCode.VSSRA)
                            with m.Case(0b101100):
                                m.d.comb += [
                                    UOPC(VOpCode.VNSRL),
                                    uop.narrow.eq(1),
                                ]
                            with m.Case(0b101101):
                                m.d.comb += [
                                    UOPC(VOpCode.VNSRA),
                                    uop.narrow.eq(1),
                                ]
                            with m.Case(0b101110):
                                m.d.comb += [
                                    UOPC(VOpCode.VNCLIPU),
                                    uop.narrow.eq(1),
                                ]
                            with m.Case(0b101111):
                                m.d.comb += [
                                    UOPC(VOpCode.VNCLIP),
                                    uop.narrow.eq(1),
                                ]

                    with m.Case(0b101):  # OPFVF
                        pass

                    with m.Case(0b110):  # OPMVX
                        m.d.comb += [
                            uop.fu_type.eq(VFUType.ALU),
                            uop.dst_rtype.eq(RegisterType.VEC),
                            uop.lrs1_rtype.eq(RegisterType.FIX),
                            uop.lrs2_rtype.eq(RegisterType.VEC),
                        ]

                        with m.Switch(uop.funct6):
                            with m.Case(0b001110):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.PERM),
                                    UOPC(VOpCode.VSLIDE1UP),
                                ]
                            with m.Case(0b001111):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.PERM),
                                    UOPC(VOpCode.VSLIDE1DOWN),
                                ]
                            with m.Case(0b010000):  # VRXUNARY0
                                m.d.comb += [
                                    UOPC(VOpCode.VMVSX),
                                    uop.lrs2_rtype.eq(RegisterType.X),
                                    uop.vl.eq(1),
                                ]
                            with m.Case(0b100000):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.DIV),
                                    UOPC(VOpCode.VDIVU),
                                ]
                            with m.Case(0b100001):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.DIV),
                                    UOPC(VOpCode.VDIV),
                                ]
                            with m.Case(0b100010):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.DIV),
                                    UOPC(VOpCode.VREMU),
                                ]
                            with m.Case(0b100011):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.DIV),
                                    UOPC(VOpCode.VREM),
                                ]
                            with m.Case(0b100100):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.MUL),
                                    UOPC(VOpCode.VMULHU),
                                ]
                            with m.Case(0b100101):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.MUL),
                                    UOPC(VOpCode.VMUL),
                                ]
                            with m.Case(0b100110):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.MUL),
                                    UOPC(VOpCode.VMULHSU),
                                ]
                            with m.Case(0b100111):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.MUL),
                                    UOPC(VOpCode.VMULH),
                                ]
                            with m.Case(0b101001):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.MUL),
                                    UOPC(VOpCode.VMADD),
                                ]
                            with m.Case(0b101011):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.MUL),
                                    UOPC(VOpCode.VNMSUB),
                                ]
                            with m.Case(0b101101):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.MUL),
                                    UOPC(VOpCode.VMACC),
                                ]
                            with m.Case(0b101111):
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.MUL),
                                    UOPC(VOpCode.VNMSAC),
                                ]
                            with m.Case(0b110000):  # vwaddu
                                m.d.comb += [
                                    UOPC(VOpCode.VADDU),
                                    uop.widen.eq(1),
                                ]
                            with m.Case(0b110001):  # vwadd
                                m.d.comb += [
                                    UOPC(VOpCode.VADD),
                                    uop.widen.eq(1),
                                ]
                            with m.Case(0b110010):  # vwsubu
                                m.d.comb += [
                                    UOPC(VOpCode.VSUBU),
                                    uop.widen.eq(1),
                                ]
                            with m.Case(0b110011):  # vwsub
                                m.d.comb += [
                                    UOPC(VOpCode.VSUB),
                                    uop.widen.eq(1),
                                ]
                            with m.Case(0b110100):  # vwaddu.w
                                m.d.comb += [
                                    UOPC(VOpCode.VADDU),
                                    uop.widen2.eq(1),
                                ]
                            with m.Case(0b110101):  # vwadd.w
                                m.d.comb += [
                                    UOPC(VOpCode.VADD),
                                    uop.widen2.eq(1),
                                ]
                            with m.Case(0b110110):  # vwsubu.w
                                m.d.comb += [
                                    UOPC(VOpCode.VSUBU),
                                    uop.widen2.eq(1),
                                ]
                            with m.Case(0b110111):  # vwsub.w
                                m.d.comb += [
                                    UOPC(VOpCode.VSUB),
                                    uop.widen2.eq(1),
                                ]
                            with m.Case(0b111000):  # vwmulu
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.MUL),
                                    UOPC(VOpCode.VMULHU),
                                    uop.widen.eq(1),
                                ]
                            with m.Case(0b111010):  # vwmulsu
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.MUL),
                                    UOPC(VOpCode.VMULHSU),
                                    uop.widen.eq(1),
                                ]
                            with m.Case(0b111011):  # vwmul
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.MUL),
                                    UOPC(VOpCode.VMULH),
                                    uop.widen.eq(1),
                                ]
                            with m.Case(0b111100):  # vwmaccu
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.MUL),
                                    UOPC(VOpCode.VMACCU),
                                    uop.widen.eq(1),
                                ]
                            with m.Case(0b111101):  # vwmacc
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.MUL),
                                    UOPC(VOpCode.VMACC),
                                    uop.widen.eq(1),
                                ]
                            with m.Case(0b111110):  # vwmaccus
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.MUL),
                                    UOPC(VOpCode.VMACCUS),
                                    uop.widen.eq(1),
                                ]
                            with m.Case(0b111111):  # vwmaccsu
                                m.d.comb += [
                                    uop.fu_type.eq(VFUType.MUL),
                                    UOPC(VOpCode.VMACCSU),
                                    uop.widen.eq(1),
                                ]

        return m


class DecodeStage(HasVectorParams, Elaboratable):

    def __init__(self, params, sim_debug=False):
        super().__init__(params)
        self.sim_debug = sim_debug

        self.vtype = VType(params)
        self.vl = Signal(self.vl_bits)
        self.vxrm = Signal(VXRoundingMode)

        self.fetch_packet = Decoupled(VMicroOp, params)

        self.valid = Signal()
        self.uop = VMicroOp(params)
        self.ready = Signal()

        if sim_debug:
            self.id_debug = Valid(VIDDebug, params)

    def elaborate(self, platform):
        m = Module()

        dec_unit = m.submodules.dec_unit = DecodeUnit(self.params)
        m.d.comb += [
            dec_unit.in_uop.eq(self.fetch_packet.bits),
            dec_unit.vtype.eq(self.vtype),
            dec_unit.vl.eq(self.vl),
            dec_unit.vxrm.eq(self.vxrm),
        ]

        m.d.comb += [
            self.valid.eq(self.fetch_packet.valid),
            self.uop.eq(dec_unit.out_uop),
            self.fetch_packet.ready.eq(self.ready)
        ]

        if self.sim_debug:
            m.d.comb += [
                self.id_debug.valid.eq(self.valid & self.ready),
                self.id_debug.bits.uop_id.eq(self.uop.uop_id),
                self.id_debug.bits.vlmul.eq(
                    Cat(self.uop.vlmul_mag, self.uop.vlmul_sign)),
                self.id_debug.bits.vsew.eq(Cat(self.uop.vsew)),
                self.id_debug.bits.vl.eq(Cat(self.uop.vl)),
            ]

        return m


class VOpExpander(HasVectorParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.dec_valid = Signal()
        self.dec_uop = VMicroOp(params)
        self.dec_ready = Signal()

        self.expd_valid = Signal()
        self.expd_uop = VMicroOp(params)
        self.expd_ready = Signal()

    def elaborate(self, platform):
        m = Module()

        expd_uop = VMicroOp(self.params)
        expd_idx = Signal(3)
        expd_fire = self.expd_valid & self.expd_ready

        expd_count_start = Signal(3)
        expd_count = Signal(3)
        lmul = vlmul_to_lmul(self.expd_uop.vlmul_sign, self.expd_uop.vlmul_mag)
        emul_vd = vlmul_to_lmul(
            self.expd_uop.vlmul_sign, self.expd_uop.vlmul_mag +
            (self.expd_uop.mem_size - self.expd_uop.vsew))
        mask_single_vreg = self.expd_uop.fu_type_has(VFUType.MASK) & (
            self.expd_uop.opcode != VOpCode.VIOTA) & (self.expd_uop.opcode
                                                      != VOpCode.VID)
        with m.If(self.expd_uop.is_ld | self.expd_uop.is_st):
            m.d.comb += expd_count_start.eq(emul_vd - 1)
        with m.Elif((self.expd_uop.opcode == VOpCode.VMVSX)
                    | (self.expd_uop.opcode == VOpCode.VMVXS)
                    | mask_single_vreg):
            m.d.comb += expd_count_start.eq(0)
        with m.Elif((self.expd_uop.widen
                     & ~self.expd_uop.fu_type_has(VFUType.REDUCE))
                    | self.expd_uop.widen2
                    | self.expd_uop.narrow
                    | ((self.expd_uop.opcode == VOpCode.VRGATHEREI16)
                       & (self.expd_uop.vsew == 0))):
            m.d.comb += expd_count_start.eq(
                Mux(self.expd_uop.vlmul_sign, 1, (lmul << 1) - 1))
        with m.Else():
            m.d.comb += expd_count_start.eq(lmul - 1)

        is_ext = (expd_uop.opcode == VOpCode.VSEXT) | (expd_uop.opcode
                                                       == VOpCode.VZEXT)
        is_redu = expd_uop.fu_type_has(VFUType.REDUCE)
        is_gather16 = expd_uop.opcode == VOpCode.VRGATHEREI16
        is_vmunary0 = (expd_uop.opcode >= VOpCode.VMSBF) & (expd_uop.opcode
                                                            <= VOpCode.VID)

        lrs1_incr = Signal(3)
        with m.If(is_ext | is_redu
                  | (expd_uop.lrs1_rtype != RegisterType.VEC)):
            m.d.comb += lrs1_incr.eq(0)
        with m.Elif((expd_uop.widen & ~is_redu) | expd_uop.widen2
                    | expd_uop.narrow | (is_gather16 & (expd_uop.vsew == 2))):
            m.d.comb += lrs1_incr.eq(expd_idx >> 1)
        with m.Elif((is_gather16 & (expd_uop.vsew == 3))):
            m.d.comb += lrs1_incr.eq(expd_idx >> 2)
        with m.Else():
            m.d.comb += lrs1_incr.eq(expd_idx)

        lrs2_incr = Signal(3)
        with m.If((expd_uop.widen & ~is_redu)
                  | (is_ext & (expd_uop.lrs1[1:3] == 3))
                  | (is_gather16 & (expd_uop.vsew == 0))):
            m.d.comb += lrs2_incr.eq(expd_idx >> 1)
        with m.Elif(is_ext & (expd_uop.lrs1[1:3] == 2)):
            m.d.comb += lrs2_incr.eq(expd_idx >> 2)
        with m.Elif(is_ext & (expd_uop.lrs1[1:3] == 1)):
            m.d.comb += lrs2_incr.eq(expd_idx >> 3)
        with m.Elif(is_vmunary0):
            m.d.comb += lrs2_incr.eq(0)
        with m.Else():
            m.d.comb += lrs2_incr.eq(expd_idx)

        ldst_incr = Signal(3)
        with m.If(expd_uop.narrow | (is_gather16 & (expd_uop.vsew == 0))):
            m.d.comb += ldst_incr.eq(expd_idx >> 1)
        with m.Elif(is_redu | expd_uop.narrow_to_1):
            m.d.comb += ldst_incr.eq(0)
        with m.Else():
            m.d.comb += ldst_incr.eq(expd_idx)

        with m.FSM():
            with m.State('PASSTHRU'):
                m.d.comb += [
                    self.expd_valid.eq(self.dec_valid),
                    self.expd_uop.eq(self.dec_uop),
                    self.expd_uop.expd_idx.eq(0),
                    self.expd_uop.expd_end.eq(1),
                    self.dec_ready.eq(self.expd_ready),
                ]

                m.d.sync += expd_idx.eq(1)

                with m.If(expd_fire & expd_count_start.any()):
                    m.d.comb += self.expd_uop.expd_end.eq(0)
                    m.d.sync += [
                        expd_uop.eq(self.dec_uop),
                        expd_count.eq(expd_count_start),
                    ]

                    m.next = 'EXPAND'

            with m.State('EXPAND'):
                m.d.comb += [
                    self.expd_valid.eq(1),
                    self.expd_uop.eq(expd_uop),
                    self.expd_uop.expd_idx.eq(expd_idx),
                    self.expd_uop.expd_end.eq(expd_idx == expd_count),
                    self.expd_uop.ldst.eq(expd_uop.ldst + ldst_incr),
                    self.expd_uop.lrs1.eq(expd_uop.lrs1 + lrs1_incr),
                    self.expd_uop.lrs2.eq(expd_uop.lrs2 + lrs2_incr),
                ]

                with m.If(expd_fire):
                    m.d.sync += expd_idx.eq(expd_idx + 1)

                    with m.If(self.expd_uop.expd_end):
                        m.next = 'PASSTHRU'

        with m.If((self.expd_uop.narrow_to_1
                   | self.expd_uop.fu_type_has(VFUType.REDUCE))
                  & ~self.expd_uop.expd_end):
            m.d.comb += [
                self.expd_uop.ldst_valid.eq(0),
                self.expd_uop.dst_rtype.eq(RegisterType.X),
            ]
        with m.Elif(self.expd_uop.narrow
                    | ((self.expd_uop.opcode == VOpCode.VRGATHEREI16)
                       & (self.expd_uop.vsew == 0))):
            with m.If(~(self.expd_uop.expd_idx[0] | self.expd_uop.expd_end)):
                m.d.comb += [
                    self.expd_uop.ldst_valid.eq(0),
                    self.expd_uop.dst_rtype.eq(RegisterType.X),
                ]

        return m
