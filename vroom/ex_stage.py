from amaranth import *

from vroom.consts import *
from vroom.types import HasVectorParams
from vroom.fu import ExecReq, ExecResp, VALUUnit, AddrGenUnit

from room.utils import Decoupled, Valid

from roomsoc.interconnect.stream import Queue


class ExecUnit(HasVectorParams, Elaboratable):

    def __init__(
        self,
        params,
        irf_write=False,
        frf_write=False,
        mem_irf_write=False,
        mem_frf_write=False,
        has_alu=False,
        has_mem=False,
    ):
        super().__init__(params)

        self.irf_write = irf_write
        self.frf_write = frf_write
        self.mem_irf_write = mem_irf_write
        self.mem_frf_write = mem_frf_write
        self.has_alu = has_alu
        self.has_mem = has_mem

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

    def elaborate(self, platform):
        m = Module()

        return m


class ALUExecUnit(ExecUnit):

    def __init__(self, params):
        super().__init__(params, irf_write=True, has_alu=True, has_mem=True)

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
