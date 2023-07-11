from amaranth import *

from groom.if_stage import FetchBundle, WarpStallReq

from room.consts import *
from room.types import HasCoreParams, MicroOp
from room.id_stage import DecodeUnit as CommonDecodeUnit

from roomsoc.interconnect.stream import Valid, Decoupled


class DecodeUnit(HasCoreParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)
        self.params = params

        self.in_uop = MicroOp(params)
        self.out_uop = MicroOp(params)

    def elaborate(self, platform):
        m = Module()

        decoder = m.submodules.decoder = CommonDecodeUnit(self.params)
        m.d.comb += decoder.in_uop.eq(self.in_uop)

        is_special = Signal()

        inuop = self.in_uop
        uop = MicroOp(self.params)
        STALL = uop.stall_warp.eq(1)

        m.d.comb += [
            uop.eq(inuop),
            uop.ldst.eq(inuop.inst[7:12]),
            uop.lrs1.eq(inuop.inst[15:20]),
            uop.lrs2.eq(inuop.inst[20:25]),
            uop.lrs3.eq(inuop.inst[27:32]),
            uop.ldst_valid.eq((uop.dst_rtype != RegisterType.X) & ~(
                (uop.dst_rtype == RegisterType.FIX) & (uop.ldst == 0))),
        ]

        with m.Switch(inuop.inst[0:7]):

            #
            # GPU control
            #
            with m.Case(0b1101011):
                m.d.comb += is_special.eq(1)

                with m.Switch(inuop.inst[12:15]):
                    with m.Case(0x0):  # gpu_tmc
                        m.d.comb += [
                            uop.opcode.eq(UOpCode.GPU_TMC),
                            uop.iq_type.eq(IssueQueueType.INT),
                            uop.fu_type.eq(FUType.GPU),
                            uop.lrs2_rtype.eq(RegisterType.FIX),
                            uop.imm_sel.eq(ImmSel.S),
                            STALL,
                        ]

                    with m.Case(0x1):  # gpu_wspawn
                        m.d.comb += [
                            uop.opcode.eq(UOpCode.GPU_WSPAWN),
                            uop.iq_type.eq(IssueQueueType.INT),
                            uop.fu_type.eq(FUType.GPU),
                            uop.lrs1_rtype.eq(RegisterType.FIX),
                            uop.lrs2_rtype.eq(RegisterType.FIX),
                            uop.imm_sel.eq(ImmSel.S),
                        ]

                    with m.Case(0x2):  # gpu_split
                        m.d.comb += [
                            uop.opcode.eq(UOpCode.GPU_SPLIT),
                            uop.iq_type.eq(IssueQueueType.INT),
                            uop.fu_type.eq(FUType.GPU),
                            uop.lrs2_rtype.eq(RegisterType.FIX),
                            uop.imm_sel.eq(ImmSel.S),
                            STALL,
                        ]

                    with m.Case(0x3):  # gpu_join
                        m.d.comb += [
                            uop.opcode.eq(UOpCode.GPU_JOIN),
                            uop.iq_type.eq(IssueQueueType.INT),
                            uop.fu_type.eq(FUType.GPU),
                        ]

                    with m.Case(0x4):  # gpu_barrier
                        m.d.comb += [
                            uop.opcode.eq(UOpCode.GPU_BARRIER),
                            uop.iq_type.eq(IssueQueueType.INT),
                            uop.fu_type.eq(FUType.GPU),
                            uop.lrs1_rtype.eq(RegisterType.FIX),
                            uop.lrs2_rtype.eq(RegisterType.FIX),
                            uop.imm_sel.eq(ImmSel.S),
                            STALL,
                        ]

                    with m.Case(0x5):  # gpu_pred
                        m.d.comb += [
                            uop.opcode.eq(UOpCode.GPU_PRED),
                            uop.iq_type.eq(IssueQueueType.INT),
                            uop.fu_type.eq(FUType.GPU),
                            uop.lrs2_rtype.eq(RegisterType.FIX),
                            uop.imm_sel.eq(ImmSel.S),
                            STALL,
                        ]

                    with m.Default():
                        m.d.comb += is_special.eq(0)

            #
            # Rasterizer
            #
            with m.Case(0b1011011):
                m.d.comb += is_special.eq(1)

                with m.Switch(inuop.inst[12:15]):
                    with m.Case(0x0):  # gpu_rast
                        m.d.comb += [
                            uop.opcode.eq(UOpCode.GPU_RAST),
                            uop.iq_type.eq(IssueQueueType.INT),
                            uop.fu_type.eq(FUType.GPU),
                            uop.dst_rtype.eq(RegisterType.FIX),
                        ]

                    with m.Default():
                        m.d.comb += is_special.eq(0)

        with m.If(is_special):
            m.d.comb += self.out_uop.eq(uop)
        with m.Else():
            m.d.comb += self.out_uop.eq(decoder.out_uop)

        return m


class DecodeStage(HasCoreParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.fetch_packet = Decoupled(FetchBundle, params)

        self.valid = Signal()
        self.wid = Signal(range(self.n_warps))
        self.uop = MicroOp(params)
        self.ready = Signal()

        self.stall_req = Valid(WarpStallReq, params)
        self.join_req = Valid(Signal, range(self.n_warps))

    def elaborate(self, platform):
        m = Module()

        dec_unit = m.submodules.dec_unit = DecodeUnit(self.params)
        m.d.comb += dec_unit.in_uop.eq(self.fetch_packet.bits.uop)

        stall_warp = self.uop.is_br | self.uop.is_jal | self.uop.is_jalr | self.uop.is_ecall | self.uop.stall_warp
        m.d.comb += [
            self.stall_req.valid.eq(self.fetch_packet.fire),
            self.stall_req.bits.wid.eq(self.fetch_packet.bits.wid),
            self.stall_req.bits.stall.eq(stall_warp),
        ]

        m.d.comb += [
            self.join_req.valid.eq(self.valid & self.ready
                                   & (self.uop.opcode == UOpCode.GPU_JOIN)),
            self.join_req.bits.eq(self.wid),
        ]

        m.d.comb += [
            self.valid.eq(self.fetch_packet.valid),
            self.uop.eq(dec_unit.out_uop),
            self.wid.eq(self.fetch_packet.bits.wid),
            self.fetch_packet.ready.eq(self.ready)
        ]

        return m
