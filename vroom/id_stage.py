from amaranth import *

from vroom.consts import *
from vroom.types import HasVectorParams, VMicroOp, VType

from room.consts import RegisterType
from room.utils import Decoupled


class DecodeUnit(HasVectorParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.vtype = VType(params)

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
            uop.funct6.eq(inuop.inst[26:32]),
            uop.funct3.eq(inuop.inst[12:15]),
            uop.ldst.eq(inuop.inst[7:12]),
            uop.lrs1.eq(inuop.inst[15:20]),
            uop.lrs2.eq(inuop.inst[20:25]),
            uop.ldst_valid.eq((uop.dst_rtype != RegisterType.X) & ~(
                (uop.dst_rtype == RegisterType.FIX) & (uop.ldst == 0))),
        ]

        with m.Switch(inuop.inst[0:7]):
            with m.Case(0b0000111):  # vl*
                m.d.comb += [
                    uop.fu_type.eq(VFUType.MEM),
                    uop.dst_rtype.eq(RegisterType.VEC),
                    uop.is_ld.eq(1),
                ]

        return m


class DecodeStage(HasVectorParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.vtype = VType(params)

        self.fetch_packet = Decoupled(VMicroOp, params)

        self.valid = Signal()
        self.uop = VMicroOp(params)
        self.ready = Signal()

    def elaborate(self, platform):
        m = Module()

        dec_unit = m.submodules.dec_unit = DecodeUnit(self.params)
        m.d.comb += [
            dec_unit.in_uop.eq(self.fetch_packet.bits),
            dec_unit.vtype.eq(self.vtype),
        ]

        m.d.comb += [
            self.valid.eq(self.fetch_packet.valid),
            self.uop.eq(dec_unit.out_uop),
            self.fetch_packet.ready.eq(self.ready)
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

        expd_fire = self.expd_valid & self.expd_ready

        expd_count = Signal(3)
        expd_idx = Signal(3)
        expd_uop = VMicroOp(self.params)

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

                with m.If(expd_fire):
                    with m.If(~self.expd_uop.vlmul_sign
                              & self.expd_uop.vlmul_mag.any()):
                        m.d.comb += self.expd_uop.expd_end.eq(0)
                        m.d.sync += expd_uop.eq(self.expd_uop)

                        with m.Switch(self.expd_uop.vlmul_mag):
                            with m.Case(0b01):
                                m.d.sync += expd_count.eq(1)
                            with m.Case(0b10):
                                m.d.sync += expd_count.eq(3)
                            with m.Case(0b11):
                                m.d.sync += expd_count.eq(7)

                        m.next = 'EXPAND'

            with m.State('EXPAND'):
                m.d.comb += [
                    self.expd_valid.eq(1),
                    self.expd_uop.eq(expd_uop),
                    self.expd_uop.expd_idx.eq(expd_idx),
                    self.expd_uop.expd_end.eq(expd_idx == expd_count),
                    self.expd_uop.ldst.eq(expd_uop.ldst + expd_idx),
                    self.expd_uop.lrs1.eq(expd_uop.lrs1 + expd_idx),
                    self.expd_uop.lrs2.eq(expd_uop.lrs2 + expd_idx),
                ]

                with m.If(expd_fire):
                    m.d.sync += expd_idx.eq(expd_idx + 1)

                    with m.If(self.expd_uop.expd_end):
                        m.next = 'PASSTHRU'

        return m
