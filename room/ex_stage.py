from amaranth import *

from room.consts import *
from room.alu import ExecReq, ExecResp, ALU
from room.types import MicroOp


class ExecUnit(Elaboratable):

    def __init__(self,
                 data_width,
                 params,
                 irf_read=False,
                 irf_write=False,
                 has_alu=False):
        self.params = params
        self.data_width = data_width
        self.irf_read = irf_read
        self.irf_write = irf_write
        self.has_alu = has_alu

        self.req = ExecReq(params, name='req')

        self.iresp = ExecResp(params, name='iresp')

    def elaborate(self, platform):
        m = Module()

        return m


class ALUExecUnit(ExecUnit):

    def __init__(self, params, has_alu=True, name=None):
        super().__init__(32,
                         params,
                         irf_read=True,
                         irf_write=has_alu,
                         has_alu=has_alu)
        self.name = name

    def elaborate(self, platform):
        m = super().elaborate(platform)

        iresp_units = []

        if self.has_alu:
            alu = m.submodules.alu = ALU(self.params)
            iresp_units.append(alu)

            m.d.comb += alu.req.eq(self.req)

        for iu in reversed(iresp_units):
            with m.If(iu.resp.valid):
                m.d.comb += self.iresp.eq(iu.resp)

        return m


class ExecUnits(Elaboratable):

    def __init__(self, params):
        self.exec_units = []
        self.issue_params = params['issue_params']

        self.irf_readers = 0
        self.irf_writers = 0

        int_width = self.issue_params[IssueQueueType.INT]['issue_width']
        for i in range(int_width):
            eu = ALUExecUnit(params, name=f'alu_int{i}')
            self.exec_units.append(eu)
            self.irf_readers += eu.irf_read
            self.irf_writers += eu.irf_write

        self.irf_read_ports = self.irf_readers * 2
        self.irf_write_ports = self.irf_writers

    def __getitem__(self, key):
        return self.exec_units[key]

    def __iter__(self):
        return iter(self.exec_units)

    def elaborate(self, platform):
        m = Module()

        for eu in self.exec_units:
            setattr(m.submodules, eu.name, eu)

        return m
