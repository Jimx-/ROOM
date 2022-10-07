from amaranth import *

from room.consts import *
from room.types import MicroOp
from room.alu import ExecResp
from room.regfile import RegisterFile, RegisterRead
from room.branch import BranchUpdate
from room.issue import IssueUnit
from room.ex_stage import ExecUnits


class FPPipeline(Elaboratable):

    def __init__(self, params, sim_debug=False):
        self.xlen = params['xlen']
        self.flen = params['flen']
        self.mem_width = params['mem_width']
        self.params = params
        self.sim_debug = sim_debug

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
            ExecResp(self.flen, params, name=f'mem_wb_ports{i}')
            for i in range(self.mem_width)
        ]

        self.wakeups = [
            ExecResp(self.flen, params, name=f'wakeup{i}')
            for i in range(self.num_wakeup_ports)
        ]

        self.to_lsu = ExecResp(self.xlen, params)

        self.br_update = BranchUpdate(params)

        self.flush_pipeline = Signal()

    def elaborate(self, platform):
        m = Module()

        exec_units = m.submodules.exec_units = ExecUnits(
            True, self.params, sim_debug=self.sim_debug)

        #
        # Dispatch
        #

        issue_unit = m.submodules.issue_unit = IssueUnit(
            self.issue_params['issue_width'], self.issue_params['num_entries'],
            self.issue_params['dispatch_width'], self.num_wakeup_ports,
            IssueQueueType.FP, self.params)

        for quop, duop in zip(issue_unit.dis_uops, self.dis_uops):
            m.d.comb += quop.eq(duop)
        m.d.comb += [
            issue_unit.dis_valids.eq(self.dis_valids),
            self.iq_ready.eq(issue_unit.ready),
            issue_unit.br_update.eq(self.br_update),
            issue_unit.flush_pipeline.eq(self.flush_pipeline),
        ]

        #
        # Issue
        #

        for iss_uop, iss_valid, iss_fu_typ, eu in zip(issue_unit.iss_uops,
                                                      issue_unit.iss_valids,
                                                      issue_unit.fu_types,
                                                      exec_units):
            m.d.comb += iss_fu_typ.eq(eu.fu_types)

            # Disallow back-to-back floating point division
            if eu.has_fdiv:
                fdiv_issued = Signal()
                m.d.sync += fdiv_issued.eq(iss_valid
                                           & (iss_uop.fu_type == FUType.FDIV))
                m.d.comb += iss_fu_typ.eq(
                    Mux(fdiv_issued, eu.fu_types & ~FUType.FDIV, eu.fu_types))

        #
        # Wakeup
        #

        for wu, wb in zip(issue_unit.wakeup_ports, self.wakeups):
            m.d.comb += [
                wu.valid.eq(wb.valid),
                wu.pdst.eq(wb.uop.pdst),
            ]

        #
        # Register read
        #

        fregfile = m.submodules.fregfile = RegisterFile(
            rports=exec_units.frf_read_ports,
            wports=exec_units.frf_write_ports + self.mem_width,
            num_regs=self.params['num_fp_pregs'],
            data_width=self.flen)

        fregread = m.submodules.fregread = RegisterRead(
            issue_width=issue_unit.issue_width,
            num_rports=exec_units.frf_read_ports,
            rports_array=[3] * exec_units.frf_readers,
            reg_width=self.flen,
            params=self.params)

        for frr_uop, iss_uop in zip(fregread.iss_uops, issue_unit.iss_uops):
            m.d.comb += frr_uop.eq(iss_uop)
        m.d.comb += fregread.iss_valids.eq(issue_unit.iss_valids)

        for frr_rp, rp in zip(fregread.read_ports, fregfile.read_ports):
            m.d.comb += frr_rp.connect(rp)

        m.d.comb += [
            fregread.br_update.eq(self.br_update),
            fregread.kill.eq(self.flush_pipeline),
        ]

        #
        # Execute
        #

        for eu, req in zip([eu for eu in exec_units if eu.frf_read],
                           fregread.exec_reqs):
            m.d.comb += [
                eu.req.eq(req),
                eu.req.kill.eq(self.flush_pipeline),
            ]

        for eu in exec_units:
            m.d.comb += eu.br_update.eq(self.br_update)

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

        for eu, wp in zip([eu for eu in exec_units if eu.frf_write],
                          fregfile.write_ports[self.mem_width:]):
            m.d.comb += [
                wp.valid.eq(eu.fresp.valid & eu.fresp.uop.rf_wen()),
                wp.addr.eq(eu.fresp.uop.pdst),
                wp.data.eq(eu.fresp.data),
            ]

        fpiu_unit = [eu for eu in exec_units if eu.has_fpiu][0]
        fpiu_is_stq = fpiu_unit.mem_iresp.uop.opcode == UOpCode.STA
        m.d.comb += [
            self.to_lsu.eq(fpiu_unit.mem_iresp),
            self.to_lsu.valid.eq(fpiu_unit.mem_iresp.valid
                                 & fpiu_unit.mem_iresp.ready & fpiu_is_stq),
            fpiu_unit.mem_iresp.ready.eq(self.to_lsu.ready),
        ]

        #
        # Commit
        #

        for wb, fresp in zip(self.wakeups[:self.mem_width], self.mem_wb_ports):
            m.d.comb += wb.eq(fresp)

        for wb, eu in zip(self.wakeups[self.mem_width:],
                          [eu for eu in exec_units if eu.frf_write]):
            m.d.comb += [
                wb.eq(eu.fresp),
                wb.valid.eq(eu.fresp.valid
                            & (eu.fresp.uop.dst_rtype == RegisterType.FLT)),
            ]

        return m
