from amaranth import *

from vroom.consts import *
from vroom.types import HasVectorParams
from vroom.fu import ExecReq, ExecResp, AddrGenUnit

from room.utils import Decoupled, Valid


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

        agu = m.submodules.agu = AddrGenUnit(self.params)

        m.d.comb += [
            agu.req.bits.eq(self.req.bits),
            agu.req.valid.eq(self.req.valid
                             & self.req.bits.uop.fu_type_has(VFUType.MEM)),
            self.lsu_req.valid.eq(agu.resp.valid),
            self.lsu_req.bits.eq(agu.resp.bits),
        ]

        m.d.comb += self.req.ready.eq(1)
        with m.If(self.req.bits.uop.fu_type_has(VFUType.MEM)):
            m.d.comb += self.req.ready.eq(self.lsu_req.ready)

        return m
