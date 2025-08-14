from amaranth import *
from amaranth.utils import log2_int

from vroom.consts import VXRoundingMode
from vroom.types import HasVectorParams


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
