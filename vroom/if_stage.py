from amaranth import *
from amaranth.lib.coding import PriorityEncoder
from amaranth.utils import log2_int

from vroom.types import HasVectorParams, VMicroOp, VType

from room.consts import RegisterType, UOpCode
from room.types import MicroOp
from room.fu import ExecReq, ExecResp
from room.branch import BranchKillableFIFO, BranchUpdate
from room.utils import Decoupled, is_older, generate_imm


class VecConfigUnit(HasVectorParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.vtype = VType(params)
        self.vl = Signal(self.vl_bits)

        self.req = Decoupled(ExecReq, self.xlen, params)
        self.resp = Decoupled(ExecResp, self.xlen, params)

    def elaborate(self, platform):
        m = Module()

        imm = generate_imm(self.req.bits.uop.imm_packed,
                           self.req.bits.uop.imm_sel)

        vtype_raw = VType(self.params)
        avl = Signal(self.xlen)
        with m.Switch(self.req.bits.uop.opcode):
            with m.Case(UOpCode.VSETVL):
                m.d.comb += [
                    vtype_raw.eq(self.req.bits.rs2_data[:8]),
                    avl.eq(self.req.bits.rs1_data),
                ]
            with m.Case(UOpCode.VSETVLI):
                m.d.comb += [
                    vtype_raw.eq(imm[5:13]),
                    avl.eq(self.req.bits.rs1_data),
                ]
            with m.Case(UOpCode.VSETIVLI):
                m.d.comb += [
                    vtype_raw.eq(imm[5:13]),
                    avl.eq(imm[:5]),
                ]

        vtype_new = VType(self.params)
        vill = (vtype_raw.vsew
                > self.max_vsew) | ~vtype_raw.lmul_ok() | vtype_raw.vill
        with m.If(~vill):
            m.d.comb += [
                vtype_new.eq(vtype_raw),
                vtype_new.vsew.eq(vtype_raw.vsew[:log2_int(self.max_vsew +
                                                           1)]),
            ]
        m.d.comb += vtype_new.vill.eq(vill)

        vl_new = Signal(self.vl_bits)
        with m.If(vill):
            m.d.comb += vl_new.eq(0)
        with m.Elif((self.req.bits.uop.lrs1_rtype == RegisterType.FIX)
                    & (self.req.bits.uop.ldst != 0)
                    & (self.req.bits.uop.lrs1 == 0)):
            m.d.comb += vl_new.eq(vtype_new.vlmax())
        with m.Else():
            avl_lsbs = avl[:log2_int(self.max_vlmax)]
            m.d.comb += vl_new.eq(
                Mux(avl_lsbs > vtype_new.vlmax(), vtype_new.vlmax(), avl_lsbs))

        with m.If(self.req.fire):
            m.d.sync += self.vtype.eq(vtype_new)
            with m.If(~((self.req.bits.uop.lrs1_rtype == RegisterType.FIX)
                        & (self.req.bits.uop.ldst == 0)
                        & (self.req.bits.uop.lrs1 == 0))):
                m.d.sync += self.vl.eq(vl_new)

        m.d.comb += [
            self.resp.bits.uop.eq(self.req.bits.uop),
            self.resp.bits.data.eq(vl_new),
            self.resp.valid.eq(self.req.valid),
            self.req.ready.eq(self.resp.ready),
        ]

        return m


class IFStage(HasVectorParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.vtype = VType(self.params)
        self.vl = Signal(self.vl_bits)

        self.req = Decoupled(ExecReq, self.xlen, params)
        self.resp = Decoupled(ExecResp, self.xlen, params)

        self.fetch_packet = Decoupled(VMicroOp, params)

        self.rob_head_idx = Signal(range(self.core_width * self.num_rob_rows))
        self.rob_pnr_idx = Signal(range(self.core_width * self.num_rob_rows))

        self.br_update = BranchUpdate(params)
        self.exception = Signal()

    def elaborate(self, platform):
        m = Module()

        vec_config = m.submodules.vec_config = VecConfigUnit(self.params)
        m.d.comb += [
            self.vtype.eq(vec_config.vtype),
            self.vl.eq(vec_config.vl),
        ]

        def should_flush(req):
            return ~is_older(req.uop.rob_idx, self.rob_pnr_idx,
                             self.rob_head_idx)

        fetch_buffer = m.submodules.fetch_buffer = BranchKillableFIFO(
            self.fetch_buffer_size,
            self.params,
            ExecReq,
            self.xlen,
            self.params,
            flush_fn=should_flush)
        m.d.comb += [
            fetch_buffer.w_data.eq(self.req.bits),
            fetch_buffer.w_br_mask.eq(self.req.bits.uop.br_mask),
            fetch_buffer.w_en.eq(self.req.valid),
            self.req.ready.eq(fetch_buffer.w_rdy),
        ]

        ftq_valid = Signal(self.ftq_size)
        ftq = [
            MicroOp(self.params, name=f'ftq{i}') for i in range(self.ftq_size)
        ]

        ftq_pe = PriorityEncoder(self.ftq_size)
        m.submodules += ftq_pe
        m.d.comb += ftq_pe.i.eq(~ftq_valid)
        next_ftq_idx = ftq_pe.o

        with m.If(self.fetch_packet.fire):
            with m.Switch(next_ftq_idx):
                for i in range(self.ftq_size):
                    with m.Case(i):
                        m.d.sync += [
                            ftq_valid[i].eq(1),
                            ftq[i].eq(fetch_buffer.r_data.uop),
                        ]

        next_uop_safe = is_older(fetch_buffer.r_data.uop.rob_idx,
                                 self.rob_pnr_idx, self.rob_head_idx)
        with m.Switch(fetch_buffer.r_data.uop.opcode):
            with m.Case(UOpCode.VSETVL, UOpCode.VSETVLI, UOpCode.VSETIVLI):
                m.d.comb += [
                    vec_config.req.valid.eq(fetch_buffer.r_rdy
                                            & next_uop_safe),
                    vec_config.req.bits.eq(fetch_buffer.r_data),
                    fetch_buffer.r_en.eq(vec_config.req.ready & next_uop_safe),
                ]

            with m.Default():
                m.d.comb += [
                    self.fetch_packet.valid.eq(fetch_buffer.r_rdy
                                               & next_uop_safe
                                               & ~ftq_pe.n),
                    fetch_buffer.r_en.eq(self.fetch_packet.ready
                                         & next_uop_safe
                                         & ~ftq_pe.n),
                    self.fetch_packet.bits.inst.eq(
                        fetch_buffer.r_data.uop.inst),
                    self.fetch_packet.bits.ftq_idx.eq(next_ftq_idx),
                    self.fetch_packet.bits.scalar_data.eq(
                        Mux(
                            fetch_buffer.r_data.uop.lrs1_rtype ==
                            RegisterType.FLT, fetch_buffer.r_data.rs3_data,
                            Mux(
                                fetch_buffer.r_data.uop.uses_ldq
                                | fetch_buffer.r_data.uop.uses_stq,
                                fetch_buffer.r_data.rs2_data,
                                fetch_buffer.r_data.rs1_data))),
                ]

        m.d.comb += vec_config.resp.connect(self.resp)

        return m
