from amaranth import *

from groom.fu import ExecReq, ExecResp, ALUUnit, MultiplierUnit, AddrGenUnit, \
    GPUControlUnit, DivUnit, IntToFPUnit, FPUUnit, FDivUnit
from groom.if_stage import BranchResolution, WarpControlReq

from room.consts import *
from room.types import HasCoreParams
from room.utils import Arbiter, generate_imm

from roomsoc.interconnect.stream import Decoupled, Valid, Queue


class ExecUnit(HasCoreParams, Elaboratable):

    def __init__(
        self,
        data_width,
        params,
        irf_write=False,
        frf_write=False,
        mem_irf_write=False,
        mem_frf_write=False,
        has_alu=False,
        has_mem=False,
        has_ifpu=False,
        has_gpu=False,
    ):
        super().__init__(params)

        self.data_width = data_width
        self.irf_write = irf_write
        self.frf_write = frf_write
        self.mem_irf_write = mem_irf_write
        self.mem_frf_write = mem_frf_write
        self.has_alu = has_alu
        self.has_mem = has_mem
        self.has_ifpu = has_ifpu
        self.has_gpu = has_gpu

        self.req = Decoupled(ExecReq, data_width, params)

        if self.irf_write:
            self.iresp = Valid(ExecResp, self.data_width, params)

        if self.frf_write:
            self.fresp = Decoupled(ExecResp, self.data_width, params)

        if self.mem_irf_write:
            self.mem_iresp = Decoupled(ExecResp, self.data_width, params)

        if self.mem_frf_write:
            self.mem_fresp = Decoupled(ExecResp, self.data_width, params)

        if has_alu:
            self.br_res = Valid(BranchResolution, params)

        if has_gpu:
            self.warp_ctrl = Valid(WarpControlReq, params)

        if has_mem:
            self.lsu_req = Decoupled(ExecResp, self.data_width, params)

    def elaborate(self, platform):
        m = Module()

        return m


class ALUExecUnit(ExecUnit):

    def __init__(self, params, has_ifpu=False):
        super().__init__(params['xlen'],
                         params,
                         irf_write=True,
                         mem_frf_write=has_ifpu,
                         has_alu=True,
                         has_mem=True,
                         has_ifpu=has_ifpu,
                         has_gpu=True)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        iresp_units = []
        alu = m.submodules.alu = ALUUnit(self.data_width,
                                         self.params,
                                         num_stages=1)
        iresp_units.append(alu)
        m.d.comb += [
            self.req.connect(alu.req),
            alu.req.valid.eq(self.req.valid
                             & ((self.req.bits.uop.fu_type == FUType.ALU)
                                | (self.req.bits.uop.fu_type == FUType.JMP)
                                | (self.req.bits.uop.fu_type == FUType.CSR))),
            self.br_res.eq(alu.br_res),
        ]

        imul = m.submodules.imul = MultiplierUnit(self.data_width, 3,
                                                  self.params)
        imul_queue = m.submodules.imul_queue = Queue(6,
                                                     ExecResp,
                                                     self.data_width,
                                                     self.params,
                                                     flow=True)
        imul_busy = Signal()

        imul_resp_busy = 0
        for iu in iresp_units:
            imul_resp_busy |= iu.resp.valid

        m.d.comb += [
            self.req.connect(imul.req),
            imul.req.valid.eq(self.req.valid
                              & (self.req.bits.uop.fu_type == FUType.MUL)),
            imul.resp.connect(imul_queue.enq),
            imul_queue.deq.ready.eq(~imul_resp_busy),
            imul_busy.eq(imul_queue.count.any()),
        ]
        iresp_units.append(imul)

        ifpu_busy = Signal()
        if self.has_ifpu:
            ifpu = m.submodules.ifpu = IntToFPUnit(self.data_width, 2,
                                                   self.params)
            m.d.comb += [
                self.req.connect(ifpu.req),
                ifpu.req.valid.eq(self.req.valid
                                  & self.req.bits.uop.fu_type_has(FUType.I2F)),
            ]

            ifpu_q = m.submodules.ifpu_q = Queue(5,
                                                 ExecResp,
                                                 self.data_width,
                                                 self.params,
                                                 flow=True)

            m.d.comb += [
                ifpu.resp.connect(ifpu_q.enq),
                ifpu_q.deq.connect(self.mem_fresp),
                ifpu_busy.eq(ifpu_q.count.any()),
            ]

        div = m.submodules.div = DivUnit(self.data_width, self.params)
        div_busy = Signal()

        div_resp_busy = 0
        for iu in iresp_units:
            div_resp_busy |= iu.resp.valid

        m.d.comb += [
            self.req.connect(div.req),
            div.req.valid.eq(self.req.valid
                             & (self.req.bits.uop.fu_type == FUType.DIV)),
            div.resp.ready.eq(~div_resp_busy),
            div_busy.eq(~div.req.ready),
        ]
        iresp_units.append(div)

        agu = m.submodules.agu = AddrGenUnit(self.params)

        m.d.comb += [
            agu.req.bits.eq(self.req.bits),
            agu.req.valid.eq(self.req.valid
                             & self.req.bits.uop.fu_type_has(FUType.MEM)),
            self.lsu_req.valid.eq(agu.resp.valid),
            self.lsu_req.bits.eq(agu.resp.bits),
        ]

        gpu = m.submodules.gpu = GPUControlUnit(self.params)
        iresp_units.append(gpu)
        m.d.comb += [
            self.req.connect(gpu.req),
            gpu.req.valid.eq(self.req.valid
                             & (self.req.bits.uop.fu_type == FUType.GPU)),
            self.warp_ctrl.eq(gpu.warp_ctrl),
        ]

        for iu in reversed(iresp_units):
            with m.If(iu.resp.valid):
                m.d.comb += [
                    self.iresp.valid.eq(1),
                    self.iresp.bits.eq(iu.resp.bits),
                ]

        m.d.comb += [
            self.iresp.bits.uop.csr_addr.eq(
                generate_imm(alu.resp.bits.uop.imm_packed, ImmSel.I)),
            self.iresp.bits.uop.csr_cmd.eq(alu.resp.bits.uop.csr_cmd),
        ]

        m.d.comb += self.req.ready.eq(1)
        with m.If(self.req.bits.uop.fu_type == FUType.MUL):
            m.d.comb += self.req.ready.eq(~imul_busy)
        with m.Elif(self.req.bits.uop.fu_type_has(FUType.I2F)):
            m.d.comb += self.req.ready.eq(~ifpu_busy)
        with m.Elif(self.req.bits.uop.fu_type == FUType.DIV):
            m.d.comb += self.req.ready.eq(~div_busy)
        with m.Elif(self.req.bits.uop.fu_type_has(FUType.MEM)):
            m.d.comb += self.req.ready.eq(self.lsu_req.ready)

        return m


class FPUExecUnit(ExecUnit):

    def __init__(self, params, name=None):
        super().__init__(params['flen'],
                         params,
                         frf_write=True,
                         mem_irf_write=True)
        self.name = name

    def elaborate(self, platform):
        m = super().elaborate(platform)

        fu_units = []

        fpu = m.submodules.fpu = FPUUnit(self.data_width, self.params)

        m.d.comb += [
            self.req.connect(fpu.req),
            fpu.req.valid.eq(self.req.valid
                             & (self.req.bits.uop.fu_type_has(FUType.FPU)
                                | self.req.bits.uop.fu_type_has(FUType.F2I))),
        ]

        fu_units.append(fpu)

        fdiv = m.submodules.fdiv = FDivUnit(self.data_width, self.params)
        fdiv_busy = Signal()
        fdiv_resp_busy = 0
        for fu in fu_units:
            fdiv_resp_busy |= fu.resp.valid

        m.d.comb += [
            self.req.connect(fdiv.req),
            fdiv.req.valid.eq(self.req.valid
                              & self.req.bits.uop.fu_type_has(FUType.FDIV)),
            fdiv.resp.ready.eq(~fdiv_resp_busy),
            fdiv_busy.eq(~fdiv.req.ready),
        ]

        fu_units.append(fdiv)

        fpiu_busy = Signal()
        fpiu_q = m.submodules.fpiu_q = Queue(6,
                                             ExecResp,
                                             self.data_width,
                                             self.params,
                                             flow=True)
        m.d.comb += [
            fpiu_q.enq.bits.eq(fpu.resp.bits),
            fpiu_q.enq.valid.eq(fpu.resp.valid
                                & fpu.resp.bits.uop.fu_type_has(FUType.F2I)
                                & (fpu.resp.bits.uop.opcode != UOpCode.STA)),
        ]

        fp_stq = m.submodules.fp_stq = Queue(3,
                                             ExecResp,
                                             self.data_width,
                                             self.params,
                                             flow=True)

        m.d.comb += [
            fp_stq.enq.bits.uop.eq(self.req.bits.uop),
            Cat(*fp_stq.enq.bits.data).eq(Cat(*self.req.bits.rs2_data)),
            fp_stq.enq.valid.eq(self.req.valid
                                & (self.req.bits.uop.opcode == UOpCode.STA)),
        ]

        resp_arb = m.submodules.resp_arb = Arbiter(2, ExecResp,
                                                   self.data_width,
                                                   self.params)

        m.d.comb += [
            fpiu_q.deq.connect(resp_arb.inp[0]),
            fp_stq.deq.connect(resp_arb.inp[1]),
            resp_arb.out.connect(self.mem_iresp),
        ]

        m.d.comb += fpiu_busy.eq(fpiu_q.count.any() | fp_stq.count.any())

        fresp_valid = 0
        for fu in reversed(fu_units):
            fresp_valid |= fu.resp.valid

            with m.If(fu.resp.valid):
                m.d.comb += [
                    Cat(*self.fresp.bits.data).eq(Cat(*fu.resp.bits.data)),
                    self.fresp.bits.uop.eq(fu.resp.bits.uop),
                ]

        m.d.comb += self.fresp.valid.eq(
            fresp_valid
            & ~(fpu.resp.valid & fpu.resp.bits.uop.fu_type_has(FUType.F2I)))

        m.d.comb += self.req.ready.eq(1)
        with m.If(self.req.bits.uop.fu_type_has(FUType.FDIV)):
            m.d.comb += self.req.ready.eq(~fdiv_busy)
        with m.Elif(self.req.bits.uop.fu_type_has(FUType.F2I)):
            m.d.comb += self.req.ready.eq(~fpiu_busy)

        return m
