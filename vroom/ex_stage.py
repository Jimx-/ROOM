from amaranth import *

from vroom.consts import *
from vroom.types import HasVectorParams, VMicroOp
from vroom.fu import ExecReq, ExecResp, VALUUnit, AddrGenUnit, VMultiplierUnit, VDivUnit, VReductionUnit, VMaskUnit, VFPUUnit, VFDivUnit
from vroom.perm import VPermutationUnit

from room.utils import Decoupled, Valid
from room.regfile import RFReadPort

from roomsoc.interconnect.stream import Queue


class VExecDebug(HasVectorParams, Record):

    def __init__(self, params, name=None, src_loc_at=0):
        HasVectorParams.__init__(self, params)

        Record.__init__(self, [
            ('uop_id', VMicroOp.ID_WIDTH),
            ('opcode', VOpCode),
            ('lrs1', range(32)),
            ('vs1_data', self.vlen),
            ('lrs2', range(32)),
            ('vs2_data', self.vlen),
            ('lrs3', range(32)),
            ('vs3_data', self.vlen),
            ('mask', self.vlen),
        ],
                        name=name,
                        src_loc_at=1 + src_loc_at)


class ExecUnit(HasVectorParams, Elaboratable):

    def __init__(self,
                 params,
                 vrf_write=False,
                 has_alu=False,
                 has_mem=False,
                 has_perm=False,
                 sim_debug=False):
        super().__init__(params)

        self.vrf_write = vrf_write
        self.has_alu = has_alu
        self.has_mem = has_mem
        self.sim_debug = sim_debug

        self.req = Decoupled(ExecReq, params)

        if self.vrf_write:
            self.vresp = Decoupled(ExecResp, params)

        if has_mem:
            self.lsu_req = Decoupled(ExecResp, params)

        if has_perm:
            self.perm_rd_port = RFReadPort(addr_width=5, data_width=self.vlen)

        if sim_debug:
            self.exec_debug = Valid(VExecDebug, params, name='ex_debug')

    def elaborate(self, platform):
        m = Module()

        if self.sim_debug:
            m.d.comb += [
                self.exec_debug.valid.eq(self.req.valid),
                self.exec_debug.bits.uop_id.eq(self.req.bits.uop.uop_id),
                self.exec_debug.bits.opcode.eq(self.req.bits.uop.opcode),
                self.exec_debug.bits.lrs1.eq(self.req.bits.uop.lrs1),
                self.exec_debug.bits.vs1_data.eq(self.req.bits.vs1_data),
                self.exec_debug.bits.lrs2.eq(self.req.bits.uop.lrs2),
                self.exec_debug.bits.vs2_data.eq(self.req.bits.vs2_data),
                self.exec_debug.bits.lrs3.eq(self.req.bits.uop.ldst),
                self.exec_debug.bits.vs3_data.eq(self.req.bits.vs3_data),
                self.exec_debug.bits.mask.eq(self.req.bits.mask),
            ]

        return m


class ALUExecUnit(ExecUnit):

    def __init__(self, params, sim_debug=False):
        super().__init__(params,
                         vrf_write=True,
                         has_alu=True,
                         has_mem=True,
                         has_perm=True,
                         sim_debug=sim_debug)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        vresp_units = []

        #
        # VALU
        #

        alu = m.submodules.alu = VALUUnit(self.params, num_stages=2)
        vresp_units.append(alu)
        m.d.comb += [
            self.req.connect(alu.req),
            alu.req.valid.eq(self.req.valid
                             & (self.req.bits.uop.fu_type == VFUType.ALU)),
        ]

        #
        # VMASK
        #

        vmask = m.submodules.vmask = VMaskUnit(self.params)
        vmask_queue = m.submodules.vmask_queue = Queue(6, ExecResp,
                                                       self.params)
        vmask_busy = Signal()

        vmask_resp_busy = 0
        for iu in vresp_units:
            vmask_resp_busy |= iu.resp.valid

        m.d.comb += [
            self.req.connect(vmask.req),
            vmask.req.valid.eq(self.req.valid
                               & (self.req.bits.uop.fu_type == VFUType.MASK)
                               & ~vmask_busy),
            vmask.resp.connect(vmask_queue.enq),
            vmask_queue.deq.ready.eq(~vmask_resp_busy),
            vmask_busy.eq(~vmask.req.ready | vmask_queue.count.any()),
        ]
        vresp_units.append(vmask_queue)

        #
        # VMUL
        #

        imul = m.submodules.imul = VMultiplierUnit(3, self.params)
        imul_queue = m.submodules.imul_queue = Queue(6, ExecResp, self.params)
        imul_busy = Signal()

        imul_resp_busy = 0
        for iu in vresp_units:
            if isinstance(iu, Queue):
                imul_resp_busy |= iu.deq.valid
            else:
                imul_resp_busy |= iu.resp.valid

        m.d.comb += [
            self.req.connect(imul.req),
            imul.req.valid.eq(self.req.valid
                              & (self.req.bits.uop.fu_type == VFUType.MUL)
                              & ~imul_busy),
            imul.resp.connect(imul_queue.enq),
            imul_queue.deq.ready.eq(~imul_resp_busy),
            imul_busy.eq(imul_queue.count.any()),
        ]
        vresp_units.append(imul_queue)

        #
        # REDUCE
        #

        iredu = m.submodules.iredu = VReductionUnit(self.params)
        iredu_queue = m.submodules.iredu_queue = Queue(6, ExecResp,
                                                       self.params)
        iredu_busy = Signal()

        iredu_resp_busy = 0
        for iu in vresp_units:
            if isinstance(iu, Queue):
                iredu_resp_busy |= iu.deq.valid
            else:
                iredu_resp_busy |= iu.resp.valid

        m.d.comb += [
            self.req.connect(iredu.req),
            iredu.req.valid.eq(self.req.valid
                               & (self.req.bits.uop.fu_type == VFUType.REDUCE)
                               & ~self.req.bits.uop.fp_valid
                               & ~iredu_busy),
            iredu.resp.connect(iredu_queue.enq),
            iredu_queue.deq.ready.eq(~iredu_resp_busy),
            iredu_busy.eq(iredu_queue.count.any()),
        ]
        vresp_units.append(iredu_queue)

        #
        # AGU
        #

        agu = m.submodules.agu = AddrGenUnit(self.params)

        m.d.comb += [
            agu.req.bits.eq(self.req.bits),
            agu.req.valid.eq(self.req.valid
                             & self.req.bits.uop.fu_type_has(VFUType.MEM)),
            self.lsu_req.valid.eq(agu.resp.valid),
            self.lsu_req.bits.eq(agu.resp.bits),
        ]

        #
        # VDIV
        #

        div = m.submodules.div = VDivUnit(self.params)
        div_busy = Signal()

        div_resp_busy = 0
        for iu in vresp_units:
            if isinstance(iu, Queue):
                div_resp_busy |= iu.deq.valid
            else:
                div_resp_busy |= iu.resp.valid

        m.d.comb += [
            self.req.connect(div.req),
            div.req.valid.eq(self.req.valid
                             & (self.req.bits.uop.fu_type == VFUType.DIV)),
            div.resp.ready.eq(~div_resp_busy),
            div_busy.eq(~div.req.ready),
        ]
        vresp_units.append(div)

        #
        # PERMUTE
        #

        perm = m.submodules.perm = VPermutationUnit(self.params)
        perm_busy = Signal()

        perm_resp_busy = 0
        for iu in vresp_units:
            if isinstance(iu, Queue):
                perm_resp_busy |= iu.deq.valid
            else:
                perm_resp_busy |= iu.resp.valid

        m.d.comb += [
            self.req.connect(perm.req),
            perm.req.valid.eq(self.req.valid
                              & (self.req.bits.uop.fu_type == VFUType.PERM)),
            perm.rd_port.connect(self.perm_rd_port),
            perm.resp.ready.eq(~perm_resp_busy),
            perm_busy.eq(~perm.req.ready),
        ]
        vresp_units.append(perm)

        for iu in reversed(vresp_units):
            if isinstance(iu, Queue):
                with m.If(iu.deq.valid):
                    m.d.comb += [
                        self.vresp.valid.eq(1),
                        self.vresp.bits.eq(iu.deq.bits),
                    ]
            else:
                with m.If(iu.resp.valid):
                    m.d.comb += [
                        self.vresp.valid.eq(1),
                        self.vresp.bits.eq(iu.resp.bits),
                    ]

        m.d.comb += self.req.ready.eq(1)
        with m.If(self.req.bits.uop.fu_type == VFUType.MUL):
            m.d.comb += self.req.ready.eq(~imul_busy)
        with m.If(self.req.bits.uop.fu_type == VFUType.MASK):
            m.d.comb += self.req.ready.eq(~vmask_busy)
        with m.Elif((self.req.bits.uop.fu_type == VFUType.REDUCE)
                    & ~self.req.bits.uop.fp_valid):
            m.d.comb += self.req.ready.eq(~iredu_busy)
        with m.Elif(self.req.bits.uop.fu_type == VFUType.DIV):
            m.d.comb += self.req.ready.eq(~div_busy)
        with m.Elif(self.req.bits.uop.fu_type == VFUType.PERM):
            m.d.comb += self.req.ready.eq(~perm_busy)
        with m.Elif(self.req.bits.uop.fu_type_has(VFUType.MEM)):
            m.d.comb += self.req.ready.eq(self.lsu_req.ready)

        return m


class FPUExecUnit(ExecUnit):

    def __init__(self, params, sim_debug=False):
        super().__init__(params, vrf_write=True, sim_debug=sim_debug)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        vresp_units = []

        #
        # FPU
        #

        fpu = m.submodules.fpu = VFPUUnit(self.params)
        fpu_queue = m.submodules.fpu_queue = Queue(6, ExecResp, self.params)

        fpu_busy = Signal()

        m.d.comb += [
            self.req.connect(fpu.req),
            fpu.req.valid.eq(self.req.valid & self.req.ready
                             & ((self.req.bits.uop.fu_type == VFUType.FPU)
                                | (self.req.bits.uop.fu_type == VFUType.F2I))),
            fpu.resp.connect(fpu_queue.enq),
            fpu_queue.deq.ready.eq(self.vresp.ready),
            fpu_busy.eq(fpu_queue.count.any()),
        ]

        vresp_units.append(fpu_queue)

        fdiv = m.submodules.fdiv = VFDivUnit(self.params)
        fdiv_busy = Signal()
        fdiv_resp_busy = 0
        for fu in vresp_units:
            if isinstance(fu, Queue):
                fdiv_resp_busy |= fu.deq.valid
            else:
                fdiv_resp_busy |= fu.resp.valid

        m.d.comb += [
            self.req.connect(fdiv.req),
            fdiv.req.valid.eq(self.req.valid
                              & self.req.bits.uop.fu_type_has(VFUType.FDIV)),
            fdiv.resp.ready.eq(~fdiv_resp_busy),
            fdiv_busy.eq(~fdiv.req.ready),
        ]

        vresp_units.append(fdiv)

        for iu in reversed(vresp_units):
            if isinstance(iu, Queue):
                with m.If(iu.deq.valid):
                    m.d.comb += [
                        self.vresp.valid.eq(1),
                        self.vresp.bits.eq(iu.deq.bits),
                    ]
            else:
                with m.If(iu.resp.valid):
                    m.d.comb += [
                        self.vresp.valid.eq(1),
                        self.vresp.bits.eq(iu.resp.bits),
                    ]

        with m.If((self.req.bits.uop.fu_type == VFUType.FPU)
                  | (self.req.bits.uop.fu_type == VFUType.F2I)):
            m.d.comb += self.req.ready.eq(~fpu_busy)
        with m.Elif(self.req.bits.uop.fu_type_has(VFUType.FDIV)):
            m.d.comb += self.req.ready.eq(~fdiv_busy)

        return m
