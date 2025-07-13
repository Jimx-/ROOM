from amaranth import *

from vroom.types import HasVectorParams, VMicroOp

from room.consts import RegisterType
from room.utils import Decoupled


class DecodeUnit(HasVectorParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.in_uop = VMicroOp(params)
        self.out_uop = VMicroOp(params)

    def elaborate(self, platform):
        m = Module()

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

        return m


class DecodeStage(HasVectorParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.fetch_packet = Decoupled(VMicroOp, params)

        self.valid = Signal()
        self.uop = VMicroOp(params)
        self.ready = Signal()

    def elaborate(self, platform):
        m = Module()

        dec_unit = m.submodules.dec_unit = DecodeUnit(self.params)
        m.d.comb += dec_unit.in_uop.eq(self.fetch_packet.bits)

        m.d.comb += [
            self.valid.eq(self.fetch_packet.valid),
            self.uop.eq(dec_unit.out_uop),
            self.fetch_packet.ready.eq(self.ready)
        ]

        return m
