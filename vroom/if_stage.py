from amaranth import *
from amaranth.lib.coding import PriorityEncoder
from amaranth.utils import log2_int

from vroom.types import HasVectorParams, VMicroOp, VType
from vroom.fu import ExecResp as VExecResp

from room.consts import RegisterType, UOpCode
from room.types import MicroOp
from room.fu import ExecReq, ExecResp
from room.branch import BranchKillableFIFO, BranchUpdate
from room.utils import is_older, generate_imm

from roomsoc.interconnect.stream import Valid, Decoupled, SkidBuffer


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

        out_buf = m.submodules.out_buf = SkidBuffer(ExecResp, self.xlen,
                                                    self.params)
        m.d.comb += [
            out_buf.enq.bits.uop.eq(self.req.bits.uop),
            out_buf.enq.bits.data.eq(vl_new),
            out_buf.enq.valid.eq(self.req.valid),
            self.req.ready.eq(out_buf.enq.ready),
            out_buf.deq.connect(self.resp),
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

        self.get_rs1 = Valid(Signal, range(self.ftq_size))
        self.get_rs1_data = Signal(self.xlen)
        self.get_rs2 = Valid(Signal, range(self.ftq_size))
        self.get_rs2_data = Signal(self.xlen)

        self.wb_req = Valid(VExecResp, params)

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
        ftq_wb = [
            Valid(Signal, self.xlen, name=f'ftq_wb{i}')
            for i in range(self.ftq_size)
        ]
        ftq = [
            MicroOp(self.params, name=f'ftq{i}') for i in range(self.ftq_size)
        ]
        ftq_rs1_data = [
            Signal(self.xlen, name=f'ftq_rs1_data{i}')
            for i in range(self.ftq_size)
        ]
        ftq_rs2_data = [
            Signal(self.xlen, name=f'ftq_rs2_data{i}')
            for i in range(self.ftq_size)
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
                            ftq_rs1_data[i].eq(fetch_buffer.r_data.rs1_data),
                            ftq_rs2_data[i].eq(fetch_buffer.r_data.rs2_data),
                        ]

        for req, resp, data in [
            (self.get_rs1, self.get_rs1_data, ftq_rs1_data),
            (self.get_rs2, self.get_rs2_data, ftq_rs2_data),
        ]:
            with m.If(req.valid):
                with m.Switch(req.bits):
                    for i in range(self.ftq_size):
                        with m.Case(i):
                            m.d.comb += resp.eq(data[i])

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
                    self.fetch_packet.bits.uop_id.eq(
                        fetch_buffer.r_data.uop.uop_id),
                    self.fetch_packet.bits.inst.eq(
                        fetch_buffer.r_data.uop.inst),
                    self.fetch_packet.bits.ftq_idx.eq(next_ftq_idx),
                ]

        with m.If(vec_config.resp.valid):
            m.d.comb += vec_config.resp.connect(self.resp)
        with m.Else():
            wb_grant = Signal(range(self.ftq_size))
            with m.If(self.wb_req.valid):
                m.d.comb += [
                    self.resp.valid.eq(1),
                    self.resp.bits.data.eq(self.wb_req.bits.rd_data),
                    wb_grant.eq(self.wb_req.bits.uop.ftq_idx),
                ]
                with m.Switch(self.wb_req.bits.uop.ftq_idx):
                    for i in range(self.ftq_size):
                        with m.Case(i):
                            m.d.comb += self.resp.bits.uop.eq(ftq[i])
                            m.d.sync += [
                                ftq_wb[i].valid.eq(1),
                                ftq_wb[i].bits.eq(self.wb_req.bits.rd_data),
                            ]

            with m.Else():
                wb_pe = PriorityEncoder(self.ftq_size)
                m.submodules += wb_pe
                m.d.comb += [
                    wb_pe.i.eq(Cat(wb.valid for wb in ftq_wb)),
                    wb_grant.eq(wb_pe.o),
                    self.resp.valid.eq(~wb_pe.n),
                ]
                with m.Switch(wb_pe.o):
                    for i in range(self.ftq_size):
                        with m.Case(i):
                            m.d.comb += [
                                self.resp.bits.uop.eq(ftq[i]),
                                self.resp.bits.data.eq(ftq_wb[i].bits),
                            ]

            with m.If(self.resp.fire):
                with m.Switch(wb_grant):
                    for i in range(self.ftq_size):
                        with m.Case(i):
                            m.d.sync += [
                                ftq_valid[i].eq(0),
                                ftq_wb[i].valid.eq(0),
                            ]

        return m
