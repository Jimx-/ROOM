from amaranth import *

from groom.if_stage import FetchBundle, WarpStallReq

from room.types import HasCoreParams, MicroOp
from room.id_stage import DecodeUnit

from roomsoc.interconnect.stream import Valid, Decoupled


class DecodeStage(HasCoreParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.fetch_packet = Decoupled(FetchBundle, params)

        self.valid = Signal()
        self.wid = Signal(range(self.n_warps))
        self.uop = MicroOp(params)
        self.ready = Signal()

        self.stall_req = Valid(WarpStallReq, params)

    def elaborate(self, platform):
        m = Module()

        dec_unit = m.submodules.dec_unit = DecodeUnit(self.params)
        m.d.comb += dec_unit.in_uop.eq(self.fetch_packet.bits.uop)

        stall_warp = self.uop.is_br | self.uop.is_jal | self.uop.is_jalr | self.uop.is_ecall
        m.d.comb += [
            self.stall_req.valid.eq(self.fetch_packet.fire),
            self.stall_req.bits.wid.eq(self.fetch_packet.bits.wid),
            self.stall_req.bits.stall.eq(stall_warp),
        ]

        m.d.comb += [
            self.valid.eq(self.fetch_packet.valid),
            self.uop.eq(dec_unit.out_uop),
            self.wid.eq(self.fetch_packet.bits.wid),
            self.fetch_packet.ready.eq(self.ready)
        ]

        return m
