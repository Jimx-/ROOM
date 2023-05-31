from amaranth import *

from groom.fu import ExecReq, ExecResp, ALUUnit
from groom.if_stage import BranchResolution

from room.consts import *
from room.types import HasCoreParams, MicroOp

from roomsoc.interconnect.stream import Decoupled, Valid


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
        has_ifpu=False,
    ):
        super().__init__(params)

        self.data_width = data_width
        self.irf_write = irf_write
        self.frf_write = frf_write
        self.mem_irf_write = mem_irf_write
        self.mem_frf_write = mem_frf_write
        self.has_alu = has_alu
        self.has_ifpu = has_ifpu

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
                         has_ifpu=has_ifpu)

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

        for iu in reversed(iresp_units):
            with m.If(iu.resp.valid):
                m.d.comb += [
                    self.iresp.valid.eq(1),
                    self.iresp.bits.eq(iu.resp.bits),
                ]

        return m
