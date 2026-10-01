from amaranth import *
from amaranth import tracer
from amaranth.hdl.ast import ValueCastable

from groom.if_stage import FetchBundle, WarpStallReq

from room.consts import *
from room.types import HasCoreParams, MicroOp
from room.id_stage import DecodeUnit as CommonDecodeUnit

from roomsoc.interconnect.stream import Valid, Decoupled


class IDDebug(HasCoreParams, ValueCastable):

    def __init__(self, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.wid = Signal(range(self.n_warps), name=f'{name}__wid')
        self.uop_id = Signal(MicroOp.ID_WIDTH, name=f'{name}__uop_id')
        self.tmask = Signal(self.n_threads, name=f'{name}__tmask')

    @ValueCastable.lowermethod
    def as_value(self):
        return Cat(self.wid, self.uop_id, self.tmask)

    def shape(self):
        return self.as_value().shape()

    def __len__(self):
        return len(Value.cast(self))

    def eq(self, rhs):
        return Value.cast(self).eq(Value.cast(rhs))


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
                            uop.dst_rtype.eq(RegisterType.FIX),
                            uop.lrs2_rtype.eq(RegisterType.FIX),
                            uop.imm_sel.eq(ImmSel.S),
                            STALL,
                        ]

                    with m.Case(0x3):  # gpu_join
                        m.d.comb += [
                            uop.opcode.eq(UOpCode.GPU_JOIN),
                            uop.iq_type.eq(IssueQueueType.INT),
                            uop.fu_type.eq(FUType.GPU),
                            uop.lrs1_rtype.eq(RegisterType.FIX),
                            STALL,
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

            #
            # Warp lane gather
            #
            with m.Case(0b0101011):
                m.d.comb += is_special.eq(1)

                with m.Switch(inuop.inst[12:15]):
                    with m.Case(0x0):  # wgather
                        m.d.comb += [
                            uop.opcode.eq(UOpCode.GPU_WGATHER),
                            uop.iq_type.eq(IssueQueueType.INT),
                            uop.fu_type.eq(FUType.WGATHER),
                            uop.dst_rtype.eq(RegisterType.FIX),
                            uop.lrs1_rtype.eq(RegisterType.FIX),
                            uop.lrs2_rtype.eq(RegisterType.FIX),
                            uop.lrs3_rtype.eq(RegisterType.FIX),
                            uop.imm_sel.eq(ImmSel.I),
                        ]

                    with m.Default():
                        m.d.comb += is_special.eq(0)

            #
            # Asynchronous global-to-shared copy
            #
            if self.use_async_copy:
                with m.Case(0b1111011):
                    m.d.comb += is_special.eq(1)

                    with m.Switch(inuop.inst[12:15]):
                        with m.Case(0x0):  # gcopy
                            m.d.comb += [
                                uop.opcode.eq(UOpCode.GPU_COPY_ISSUE),
                                uop.iq_type.eq(IssueQueueType.INT),
                                uop.fu_type.eq(FUType.COPY),
                                # The launch result in rd reports token
                                # rejection: 0 accepted, 1 dropped.
                                uop.dst_rtype.eq(RegisterType.FIX),
                                uop.lrs1_rtype.eq(RegisterType.FIX),
                                uop.lrs2_rtype.eq(RegisterType.FIX),
                                uop.imm_sel.eq(ImmSel.S),
                                STALL,
                            ]

                        with m.Case(0x1):  # gcopywait
                            m.d.comb += [
                                uop.opcode.eq(UOpCode.GPU_COPY_WAIT),
                                uop.iq_type.eq(IssueQueueType.INT),
                                uop.fu_type.eq(FUType.COPY),
                                uop.lrs1_rtype.eq(RegisterType.FIX),
                                uop.imm_sel.eq(ImmSel.S),
                                STALL,
                            ]

                        with m.Case(0x2):  # gcopystat
                            m.d.comb += [
                                uop.opcode.eq(UOpCode.GPU_COPY_STAT),
                                uop.iq_type.eq(IssueQueueType.INT),
                                uop.fu_type.eq(FUType.COPY),
                                uop.dst_rtype.eq(RegisterType.FIX),
                                uop.lrs1_rtype.eq(RegisterType.FIX),
                                uop.imm_sel.eq(ImmSel.S),
                            ]

                        with m.Default():
                            m.d.comb += is_special.eq(0)

        # Pack the immediate from this decoder's own imm_sel so S-format
        # custom ops (wspawn reads imm_packed[8:20] as its spawn offset)
        # carry the real store immediate, not the I-type rs2 field.
        di20_25 = Mux((uop.imm_sel == ImmSel.B) | (uop.imm_sel == ImmSel.S),
                      inuop.inst[7:12], inuop.inst[20:25])
        m.d.comb += uop.imm_packed.eq(
            Cat(inuop.inst[12:20], di20_25, inuop.inst[25:32]))

        with m.If(is_special):
            m.d.comb += self.out_uop.eq(uop)
        with m.Else():
            m.d.comb += self.out_uop.eq(decoder.out_uop)

        return m


class DecodeStage(HasCoreParams, Elaboratable):

    def __init__(self, params, sim_debug=False):
        super().__init__(params)

        self.sim_debug = sim_debug

        self.fetch_packet = Decoupled(FetchBundle, params)

        self.valid = Signal()
        self.wid = Signal(range(self.n_warps))
        self.uop = MicroOp(params)
        self.ready = Signal()

        self.stall_req = Valid(WarpStallReq, params)

        if sim_debug:
            self.id_debug = Valid(IDDebug, params)

    def elaborate(self, platform):
        m = Module()

        dec_unit = m.submodules.dec_unit = DecodeUnit(self.params)
        m.d.comb += dec_unit.in_uop.eq(self.fetch_packet.bits.uop)

        stall_warp = self.uop.is_br | self.uop.is_jal | self.uop.is_jalr | self.uop.is_ecall | self.uop.stall_warp
        copy_op = (self.uop.opcode == UOpCode.GPU_COPY_ISSUE) | (
            self.uop.opcode == UOpCode.GPU_COPY_WAIT)
        copy_park = copy_op & self.uop.tmask.any()
        # An empty-mask launch or wait is a no-op that neither parks nor
        # stalls: nothing will ever wake a warp stalled for it.
        copy_nopark = copy_op & ~self.uop.tmask.any()
        m.d.comb += [
            self.stall_req.valid.eq(self.fetch_packet.fire),
            self.stall_req.bits.wid.eq(self.fetch_packet.bits.wid),
            self.stall_req.bits.stall.eq(stall_warp & ~copy_nopark),
            self.stall_req.bits.copy.eq(stall_warp & copy_park),
        ]

        m.d.comb += [
            self.valid.eq(self.fetch_packet.valid),
            self.uop.eq(dec_unit.out_uop),
            self.wid.eq(self.fetch_packet.bits.wid),
            self.fetch_packet.ready.eq(self.ready)
        ]

        if self.sim_debug:
            m.d.comb += [
                self.id_debug.valid.eq(self.valid & self.ready),
                self.id_debug.bits.wid.eq(self.wid),
                self.id_debug.bits.uop_id.eq(self.uop.uop_id),
                self.id_debug.bits.tmask.eq(self.uop.tmask),
            ]

        return m
