from amaranth import *
from amaranth.utils import log2_int

from vroom.consts import VOpCode, VFUType, VXRoundingMode
from vroom.types import HasVectorParams, VMicroOp


def vlmul_to_lmul(vlmul_sign, vlmul_mag):
    y0 = ~vlmul_mag.any() | vlmul_sign
    y1 = ~vlmul_sign & ~vlmul_mag[1] & vlmul_mag[0]
    y2 = ~vlmul_sign & vlmul_mag[1] & ~vlmul_mag[0]
    y3 = ~vlmul_sign & vlmul_mag.all()
    return Cat(y0, y1, y2, y3)


def get_round_inc(vxrm, v, d):
    return Mux(
        vxrm == VXRoundingMode.RNU, v[d - 1],
        Mux(vxrm == VXRoundingMode.RNE, v[d - 1] & (v[:d - 1].any() | v[d]),
            Mux(vxrm == VXRoundingMode.ROD, ~v[d] & v[:d].any(), 0)))


class EmulDecoder(HasVectorParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.uop = VMicroOp(params)

        self.vemul_vd_sign = Signal(1)
        self.vemul_vd_mag = Signal(2)
        self.vemul_vs1_sign = Signal(1)
        self.vemul_vs1_mag = Signal(2)
        self.vemul_vs2_sign = Signal(1)
        self.vemul_vs2_mag = Signal(2)

        self.emul_vd = Signal(4)
        self.emul_vs1 = Signal(4)
        self.emul_vs2 = Signal(4)

        self.veew_vd = Signal(3)
        self.veew_vs1 = Signal(3)
        self.veew_vs2 = Signal(3)

    def elaborate(self, platform):
        m = Module()

        vlmul = Cat(self.uop.vlmul_mag, self.uop.vlmul_sign)
        vsew = self.uop.vsew
        veew_ldst = self.uop.mem_size

        vemul_ldst = Signal(3)
        m.d.comb += vemul_ldst.eq(vlmul + (veew_ldst - self.uop.vsew))

        mask_single_vreg = self.uop.fu_type_has(VFUType.MASK) & (
            self.uop.opcode != VOpCode.VIOTA) & (self.uop.opcode
                                                 != VOpCode.VID)

        vemul_vd = Signal(3)
        with m.If(self.uop.is_ld | self.uop.is_st):
            m.d.comb += vemul_vd.eq(
                Mux(self.uop.mask_ls, 0,
                    Mux(self.uop.indexed, vlmul, vemul_ldst)))
        with m.Elif((self.uop.opcode == VOpCode.VMVSX)
                    | (self.uop.opcode == VOpCode.VMVXS)
                    | mask_single_vreg):
            m.d.comb += vemul_vd.eq(0)
        with m.Elif(self.uop.widen | self.uop.widen2):
            m.d.comb += vemul_vd.eq(vlmul + 1)
        with m.Else():
            m.d.comb += vemul_vd.eq(vlmul)

        vemul_vs1 = Signal(3)
        with m.If(((self.uop.opcode >= VOpCode.VMANDNOT)
                   & (self.uop.opcode <= VOpCode.VMXNOR))
                  | (self.uop.opcode == VOpCode.VCOMPRESS)):
            m.d.comb += vemul_vs1.eq(0)
        with m.Elif(self.uop.opcode == VOpCode.VRGATHEREI16):
            m.d.comb += vemul_vs1.eq(vlmul + 1 - self.uop.vsew)
        with m.Else():
            m.d.comb += vemul_vs1.eq(vlmul)

        vemul_vs2 = Signal(3)
        is_ext = (self.uop.opcode == VOpCode.VSEXT) | (self.uop.opcode
                                                       == VOpCode.VZEXT)
        with m.If((self.uop.is_ld | self.uop.is_st) & self.uop.indexed):
            m.d.comb += vemul_vs2.eq(vemul_ldst)
        with m.Elif(self.uop.widen2 | self.uop.narrow):
            m.d.comb += vemul_vs2.eq(vlmul + 1)
        with m.Elif(is_ext):
            with m.Switch(self.uop.lrs1[1:3]):
                for i in range(1, 4):
                    m.d.comb += vemul_vs2.eq(vlmul - (4 - i))
        with m.Elif(
                self.uop.fu_type_has(VFUType.MASK)
                | (self.uop.opcode == VOpCode.VMVSX)
                | (self.uop.opcode == VOpCode.VMVXS)
                | mask_single_vreg):
            m.d.comb += vemul_vs2.eq(0)
        with m.Else():
            m.d.comb += vemul_vs2.eq(vlmul)

        with m.If(self.uop.is_ld | self.uop.is_st):
            m.d.comb += self.veew_vd.eq(
                Mux(self.uop.mask_ls, 0, Mux(self.uop.indexed, vsew,
                                             veew_ldst)))
        with m.Elif(self.uop.narrow_to_1 | mask_single_vreg):
            m.d.comb += self.veew_vd.eq(7)
        with m.Elif(self.uop.widen | self.uop.widen2):
            m.d.comb += self.veew_vd.eq(vsew + 1)
        with m.Else():
            m.d.comb += self.veew_vd.eq(vsew)

        with m.If(((self.uop.opcode >= VOpCode.VMANDNOT)
                   & (self.uop.opcode <= VOpCode.VMXNOR))
                  | (self.uop.opcode == VOpCode.VCOMPRESS)):
            m.d.comb += self.veew_vs1.eq(7)
        with m.Elif(self.uop.opcode == VOpCode.VRGATHEREI16):
            m.d.comb += self.veew_vs1.eq(1)
        with m.Else():
            m.d.comb += self.veew_vs1.eq(vsew)

        with m.If((self.uop.is_ld | self.uop.is_st) & self.uop.indexed):
            m.d.comb += self.veew_vs2.eq(veew_ldst)
        with m.Elif(self.uop.widen2 | self.uop.narrow):
            m.d.comb += self.veew_vs2.eq(vsew + 1)
        with m.Elif(is_ext):
            with m.Switch(self.uop.lrs1[1:3]):
                for i in range(1, 4):
                    m.d.comb += self.veew_vs2.eq(vsew - (4 - i))
        with m.Else():
            m.d.comb += self.veew_vs2.eq(vsew)

        m.d.comb += [
            Cat(self.vemul_vd_mag, self.vemul_vd_sign).eq(vemul_vd),
            Cat(self.vemul_vs1_mag, self.vemul_vs1_sign).eq(vemul_vs1),
            Cat(self.vemul_vs2_mag, self.vemul_vs2_sign).eq(vemul_vs2),
            self.emul_vd.eq(
                vlmul_to_lmul(self.vemul_vd_sign, self.vemul_vd_mag)),
            self.emul_vs1.eq(
                vlmul_to_lmul(self.vemul_vs1_sign, self.vemul_vs1_mag)),
            self.emul_vs2.eq(
                vlmul_to_lmul(self.vemul_vs2_sign, self.vemul_vs2_mag)),
        ]

        return m


class TailGen(HasVectorParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.vl = Signal(self.vl_bits)
        self.uop_idx = Signal(range(8))
        self.eew = Signal(2)
        self.narrow = Signal()

        self.tail = Signal(self.vlen_bytes)

    def elaborate(self, platform):
        m = Module()

        shift = Signal(range(log2_int(self.vlen_bytes) + 1))
        with m.Switch(self.eew):
            for i in range(4):
                with m.Case(i):
                    m.d.comb += shift.eq(log2_int(self.vlen_bytes) - i)

        vreg_max_elems = 1 << shift
        rem_elems = Signal(self.vl_bits)
        m.d.comb += rem_elems.eq(self.vl - (
            Mux(self.narrow, self.uop_idx[1:], self.uop_idx) << shift))

        with m.If(rem_elems[-1]):
            m.d.comb += self.tail.eq(~0)
        with m.Elif(rem_elems >= vreg_max_elems):
            m.d.comb += self.tail.eq(0)
        with m.Else():
            with m.Switch(rem_elems):
                for i in range(self.vlen_bytes):
                    with m.Case(i):
                        with m.If(i < vreg_max_elems):
                            m.d.comb += self.tail.eq(~((1 << i) - 1))

        return m
