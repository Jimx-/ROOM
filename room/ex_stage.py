from amaranth import *

from room.consts import *
from room.alu import ExecReq, ExecResp, ALU, AddrGenUnit, MultiplierUnit, DivUnit, generate_imm
from room.if_stage import GetPCResp
from room.branch import BranchResolution, BranchUpdate
from room.types import MicroOp


class ExecUnit(Elaboratable):

    def __init__(self,
                 data_width,
                 params,
                 irf_read=False,
                 irf_write=False,
                 has_jmp_unit=False,
                 has_alu=False,
                 has_mul=False,
                 has_div=False,
                 has_mem=False,
                 has_csr=False):
        self.params = params
        self.data_width = data_width
        self.irf_read = irf_read
        self.irf_write = irf_write
        self.has_jmp_unit = has_jmp_unit
        self.has_alu = has_alu
        self.has_mul = has_mul
        self.has_div = has_div
        self.has_mem = has_mem
        self.has_csr = has_csr

        self.fu_types = Signal(FUType)

        self.req = ExecReq(params, name='req')

        self.iresp = ExecResp(params, name='iresp')

        self.br_update = BranchUpdate(params)

        self.br_res = has_alu and BranchResolution(params,
                                                   name='br_res') or None
        self.get_pc = None
        if has_jmp_unit:
            self.get_pc = GetPCResp(name='get_pc')

        self.lsu_req = None
        if has_mem:
            self.lsu_req = ExecResp(params, name='lsu_req')

    def elaborate(self, platform):
        m = Module()

        return m


class ALUExecUnit(ExecUnit):

    def __init__(self,
                 params,
                 has_jmp_unit=False,
                 has_alu=True,
                 has_mul=False,
                 has_div=False,
                 has_mem=False,
                 has_csr=False,
                 name=None):
        super().__init__(32,
                         params,
                         irf_read=True,
                         irf_write=has_alu,
                         has_jmp_unit=has_jmp_unit,
                         has_alu=has_alu,
                         has_mul=has_mul,
                         has_div=has_div,
                         has_mem=has_mem,
                         has_csr=has_csr)
        self.name = name

    def elaborate(self, platform):
        m = super().elaborate(platform)

        div_busy = Signal()
        fu_type_div = Signal(FUType)

        if self.has_div:
            with m.If(~div_busy):
                m.d.comb += fu_type_div.eq(FUType.DIV)

        m.d.comb += self.fu_types.eq((FUType.ALU if self.has_alu else 0)
                                     | (FUType.MUL if self.has_mul else 0)
                                     | fu_type_div
                                     | (FUType.JMP if self.has_jmp_unit else 0)
                                     | (FUType.MEM if self.has_mem else 0)
                                     | (FUType.CSR if self.has_csr else 0))

        iresp_units = []

        if self.has_alu:
            alu = m.submodules.alu = ALU(self.params, is_jmp=self.has_jmp_unit)
            iresp_units.append(alu)

            m.d.comb += [
                alu.req.eq(self.req),
                alu.req.valid.eq(self.req.valid
                                 & ((self.req.uop.fu_type == FUType.ALU)
                                    | (self.req.uop.fu_type == FUType.JMP)
                                    | (self.req.uop.fu_type == FUType.CSR))),
                alu.br_update.eq(self.br_update),
                self.br_res.eq(alu.br_res),
            ]

            if self.has_jmp_unit:
                m.d.comb += alu.get_pc.eq(self.get_pc)

        if self.has_mul:
            imul = m.submodules.imul = MultiplierUnit(3, self.params)
            iresp_units.append(imul)

            m.d.comb += [
                imul.req.eq(self.req),
                imul.req.valid.eq(self.req.valid
                                  & (self.req.uop.fu_type == FUType.MUL)),
                imul.br_update.eq(self.br_update),
            ]

        if self.has_div:
            div = m.submodules.div = DivUnit(self.params)

            div_resp_busy = 0
            for iu in iresp_units:
                div_resp_busy |= iu.resp.valid

            m.d.comb += [
                div.req.eq(self.req),
                div.req.valid.eq(self.req.valid
                                 & (self.req.uop.fu_type == FUType.DIV)),
                div.br_update.eq(self.br_update),
                div.resp.ready.eq(~div_resp_busy),
                div_busy.eq(~div.resp.ready | div.req.valid),
            ]

            iresp_units.append(div)

        if self.has_mem:
            agu = m.submodules.agu = AddrGenUnit(self.params)

            m.d.comb += [
                agu.req.eq(self.req),
                agu.br_update.eq(self.br_update),
                self.lsu_req.eq(agu.resp),
            ]

        if self.irf_write:
            for iu in reversed(iresp_units):
                with m.If(iu.resp.valid):
                    m.d.comb += self.iresp.eq(iu.resp)

            if self.has_alu:
                m.d.comb += [
                    self.iresp.uop.csr_addr.eq(
                        generate_imm(alu.resp.uop.imm_packed, ImmSel.I)),
                    self.iresp.uop.csr_cmd.eq(alu.resp.uop.csr_cmd),
                ]

        return m


class ExecUnits(Elaboratable):

    def __init__(self, params):
        self.exec_units = []
        self.issue_params = params['issue_params']

        self.irf_readers = 0
        self.irf_writers = 0

        mem_width = self.issue_params[IssueQueueType.MEM]['issue_width']
        for i in range(mem_width):
            eu = ALUExecUnit(params,
                             has_alu=False,
                             has_mem=True,
                             name=f'mem{i}')
            self.exec_units.append(eu)
            self.irf_readers += eu.irf_read
            self.irf_writers += eu.irf_write

        int_width = self.issue_params[IssueQueueType.INT]['issue_width']
        for i in range(int_width):
            eu = ALUExecUnit(params,
                             has_jmp_unit=(i == 0),
                             has_csr=(i == (1 % int_width)),
                             has_mul=(i == (2 % int_width)),
                             has_div=(i == (3 % int_width)),
                             name=f'alu_int{i}')
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
