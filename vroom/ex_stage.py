from amaranth import *

from vroom.consts import *
from vroom.types import HasVectorParams, VMicroOp
from vroom.fu import ExecReq, ExecResp, VALUUnit, AddrGenUnit

from room.utils import Decoupled, Valid

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
            ('mask', self.vlen),
        ],
                        name=name,
                        src_loc_at=1 + src_loc_at)


class ExecUnit(HasVectorParams, Elaboratable):

    def __init__(self,
                 params,
                 irf_write=False,
                 frf_write=False,
                 mem_irf_write=False,
                 mem_frf_write=False,
                 has_alu=False,
                 has_mem=False,
                 sim_debug=False):
        super().__init__(params)

        self.irf_write = irf_write
        self.frf_write = frf_write
        self.mem_irf_write = mem_irf_write
        self.mem_frf_write = mem_frf_write
        self.has_alu = has_alu
        self.has_mem = has_mem
        self.sim_debug = sim_debug

        self.req = Decoupled(ExecReq, params)

        if self.irf_write:
            self.iresp = Valid(ExecResp, params)

        if self.frf_write:
            self.fresp = Decoupled(ExecResp, params)

        if self.mem_irf_write:
            self.mem_iresp = Decoupled(ExecResp, params)

        if self.mem_frf_write:
            self.mem_fresp = Decoupled(ExecResp, params)

        if has_mem:
            self.lsu_req = Decoupled(ExecResp, params)

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
                self.exec_debug.bits.mask.eq(self.req.bits.mask),
            ]

        return m


class ALUExecUnit(ExecUnit):

    def __init__(self, params, sim_debug=False):
        super().__init__(params,
                         irf_write=True,
                         has_alu=True,
                         has_mem=True,
                         sim_debug=sim_debug)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        iresp_units = []

        alu = m.submodules.alu = VALUUnit(self.params, num_stages=1)
        iresp_units.append(alu)
        m.d.comb += [
            self.req.connect(alu.req),
            alu.req.valid.eq(self.req.valid
                             & (self.req.bits.uop.fu_type == VFUType.ALU)),
        ]

        agu = m.submodules.agu = AddrGenUnit(self.params)

        m.d.comb += [
            agu.req.bits.eq(self.req.bits),
            agu.req.valid.eq(self.req.valid
                             & self.req.bits.uop.fu_type_has(VFUType.MEM)),
            self.lsu_req.valid.eq(agu.resp.valid),
            self.lsu_req.bits.eq(agu.resp.bits),
        ]

        for iu in reversed(iresp_units):
            if isinstance(iu, Queue):
                with m.If(iu.deq.valid):
                    m.d.comb += [
                        self.iresp.valid.eq(1),
                        self.iresp.bits.eq(iu.deq.bits),
                    ]
            else:
                with m.If(iu.resp.valid):
                    m.d.comb += [
                        self.iresp.valid.eq(1),
                        self.iresp.bits.eq(iu.resp.bits),
                    ]

        m.d.comb += self.req.ready.eq(1)
        with m.If(self.req.bits.uop.fu_type_has(VFUType.MEM)):
            m.d.comb += self.req.ready.eq(self.lsu_req.ready)

        return m
