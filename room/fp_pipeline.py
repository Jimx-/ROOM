from amaranth import *

from room.consts import *
from room.types import MicroOp
from room.alu import ExecResp
from room.regfile import RegisterFile
from room.branch import BranchUpdate
from room.issue import IssueUnit


class FPPipeline(Elaboratable):

    def __init__(self, params):
        self.flen = params['flen']
        self.mem_width = params['mem_width']
        self.params = params

        self.issue_params = params['issue_params'][IssueQueueType.FP]
        self.num_wakeup_ports = self.issue_params[
            'issue_width'] + self.mem_width
        self.dispatch_width = self.issue_params['dispatch_width']

        self.dis_uops = [
            MicroOp(params, name=f'dis_uop{i}')
            for i in range(self.dispatch_width)
        ]
        self.dis_valids = Signal(self.dispatch_width)
        self.iq_ready = Signal(self.dispatch_width)

        self.mem_wb_ports = [
            ExecResp(params, name=f'mem_wb_ports{i}')
            for i in range(self.mem_width)
        ]

        self.wakeups = [
            ExecResp(params, name=f'wakeup{i}')
            for i in range(self.num_wakeup_ports)
        ]

        self.br_update = BranchUpdate(params)

        self.flush_pipeline = Signal()

    def elaborate(self, platform):
        m = Module()

        #
        # Dispatch
        #

        issue_unit = m.submodules.issue_unit = IssueUnit(
            self.issue_params['issue_width'], self.issue_params['num_entries'],
            self.issue_params['dispatch_width'], self.num_wakeup_ports,
            self.params)

        for quop, duop in zip(issue_unit.dis_uops, self.dis_uops):
            m.d.comb += quop.eq(duop)
        m.d.comb += [
            issue_unit.dis_valids.eq(self.dis_valids),
            self.iq_ready.eq(issue_unit.ready),
            issue_unit.br_update.eq(self.br_update),
            issue_unit.flush_pipeline.eq(self.flush_pipeline),
        ]

        #
        # Register read
        #

        fregfile = m.submodules.fregfile = RegisterFile(
            rports=4,
            wports=4,
            num_regs=self.params['num_fp_pregs'],
            data_width=self.flen)

        #
        # Writeback
        #

        for wp, fresp in zip(fregfile.write_ports[:self.mem_width],
                             self.mem_wb_ports):
            m.d.sync += [
                wp.valid.eq(fresp.valid & fresp.uop.rf_wen()
                            & (fresp.uop.dst_rtype == RegisterType.FLT)),
                wp.addr.eq(fresp.uop.pdst),
                wp.data.eq(fresp.data),
            ]

        #
        # Commit
        #

        for wb, fresp in zip(self.wakeups[:self.mem_width], self.mem_wb_ports):
            m.d.comb += wb.eq(fresp)

        return m
