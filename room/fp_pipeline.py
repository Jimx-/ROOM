from amaranth import *

from room.consts import *
from room.types import HasCoreParams, MicroOp
from room.fu import ExecResp
from room.regfile import RegisterFile, RegisterRead, WritebackDebug
from room.branch import BranchUpdate
from room.issue import IssueUnit
from room.ex_stage import ExecUnits
from room.utils import Arbiter

from roomsoc.interconnect.stream import Valid, Decoupled


class FPPipeline(HasCoreParams, Elaboratable):

    def __init__(self, params, sim_debug=False):
        super().__init__(params)

        self.sim_debug = sim_debug

        self.iq_params = self.issue_params[IssueQueueType.FP]
        self.num_wakeup_ports = self.iq_params['issue_width'] + self.mem_width
        self.dispatch_width = self.iq_params['dispatch_width']

        self.dis_uops = [
            MicroOp(params, name=f'dis_uop{i}')
            for i in range(self.dispatch_width)
        ]
        self.dis_valids = Signal(self.dispatch_width)
        self.iq_ready = Signal(self.dispatch_width)

        self.mem_wb_ports = [
            Decoupled(ExecResp, self.flen, params, name=f'mem_wb_ports{i}')
            for i in range(self.mem_width)
        ]

        self.wakeups = [
            Valid(ExecResp, self.flen, params, name=f'wakeup{i}')
            for i in range(self.num_wakeup_ports)
        ]

        self.from_int = Decoupled(ExecResp, self.flen, params)
        self.to_int = Decoupled(ExecResp, self.xlen, params)
        self.to_lsu = Decoupled(ExecResp, self.xlen, params)

        self.br_update = BranchUpdate(params)

        self.flush_pipeline = Signal()

        if self.sim_debug:
            self.wb_debug = [
                Valid(WritebackDebug, params, name=f'wb_debug{i}')
                for i in range(self.num_wakeup_ports)
            ]

    def elaborate(self, platform):
        m = Module()

        exec_units = m.submodules.exec_units = ExecUnits(
            True, self.params, sim_debug=self.sim_debug)

        #
        # Dispatch
        #

        issue_unit = m.submodules.issue_unit = IssueUnit(
            self.iq_params['issue_width'], self.iq_params['num_entries'],
            self.iq_params['dispatch_width'], self.num_wakeup_ports,
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
                wu.bits.pdst.eq(wb.bits.uop.pdst),
            ]

        #
        # Register read
        #

        fregfile = m.submodules.fregfile = RegisterFile(
            rports=exec_units.frf_read_ports,
            wports=exec_units.frf_write_ports + self.mem_width,
            num_regs=self.num_fp_pregs,
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
                req.connect(eu.req),
                eu.req.bits.kill.eq(self.flush_pipeline),
            ]

        for eu in exec_units:
            m.d.comb += eu.br_update.eq(self.br_update)

        #
        # Writeback
        #

        mem_wbarb = m.submodules.mem_wbarb = Arbiter(2, ExecResp, self.flen,
                                                     self.params)

        m.d.comb += [
            self.mem_wb_ports[0].connect(mem_wbarb.inp[0]),
            self.from_int.connect(mem_wbarb.inp[1]),
        ]

        m.d.sync += [
            fregfile.write_ports[0].valid.eq(
                mem_wbarb.out.valid & mem_wbarb.out.bits.uop.rf_wen()
                & (mem_wbarb.out.bits.uop.dst_rtype == RegisterType.FLT)),
            fregfile.write_ports[0].bits.addr.eq(mem_wbarb.out.bits.uop.pdst),
            fregfile.write_ports[0].bits.data.eq(mem_wbarb.out.bits.data),
        ]

        if self.sim_debug:
            wb_debug = self.wb_debug[0]

            m.d.comb += [
                wb_debug.valid.eq(fregfile.write_ports[0].valid),
                wb_debug.bits.data.eq(fregfile.write_ports[0].bits.data),
            ]

            m.d.sync += [
                wb_debug.bits.uop_id.eq(mem_wbarb.out.bits.uop.uop_id),
                wb_debug.bits.pdst.eq(mem_wbarb.out.bits.uop.pdst),
            ]

        for i, (wp, fresp) in enumerate(
                zip(fregfile.write_ports[1:self.mem_width],
                    self.mem_wb_ports[1:]), 1):
            m.d.sync += [
                wp.valid.eq(fresp.valid & fresp.bits.uop.rf_wen()
                            & (fresp.bits.uop.dst_rtype == RegisterType.FLT)),
                wp.bits.addr.eq(fresp.bits.uop.pdst),
                wp.bits.data.eq(fresp.bits.data),
            ]

            if self.sim_debug:
                wb_debug = self.wb_debug[i]

                m.d.comb += [
                    wb_debug.valid.eq(wp.valid),
                    wb_debug.bits.data.eq(wp.bits.data),
                ]

                m.d.sync += [
                    wb_debug.bits.uop_id.eq(fresp.bits.uop.uop_id),
                    wb_debug.bits.pdst.eq(fresp.bits.uop.pdst),
                ]

        for i, (eu, wp) in enumerate(
                zip([eu for eu in exec_units if eu.frf_write],
                    fregfile.write_ports[self.mem_width:]), self.mem_width):
            m.d.comb += [
                wp.valid.eq(eu.fresp.valid & eu.fresp.bits.uop.rf_wen()),
                wp.bits.addr.eq(eu.fresp.bits.uop.pdst),
                wp.bits.data.eq(eu.fresp.bits.data),
            ]

            if self.sim_debug:
                wb_debug = self.wb_debug[i]

                m.d.comb += [
                    wb_debug.valid.eq(wp.valid),
                    wb_debug.bits.uop_id.eq(eu.fresp.bits.uop.uop_id),
                    wb_debug.bits.pdst.eq(eu.fresp.bits.uop.pdst),
                    wb_debug.bits.data.eq(wp.bits.data),
                ]

        fpiu_unit = [eu for eu in exec_units if eu.has_fpiu][0]
        fpiu_is_stq = fpiu_unit.mem_iresp.bits.uop.opcode == UOpCode.STA
        m.d.comb += [
            self.to_int.bits.eq(fpiu_unit.mem_iresp.bits),
            self.to_int.valid.eq(fpiu_unit.mem_iresp.valid
                                 & fpiu_unit.mem_iresp.ready & ~fpiu_is_stq),
            self.to_lsu.bits.eq(fpiu_unit.mem_iresp.bits),
            self.to_lsu.valid.eq(fpiu_unit.mem_iresp.valid
                                 & fpiu_unit.mem_iresp.ready & fpiu_is_stq),
            fpiu_unit.mem_iresp.ready.eq(self.to_int.ready
                                         & self.to_lsu.ready),
        ]

        #
        # Commit
        #

        m.d.comb += [
            mem_wbarb.out.ready.eq(1),
            self.wakeups[0].eq(mem_wbarb.out),
        ]

        for wb, fresp in zip(self.wakeups[1:self.mem_width],
                             self.mem_wb_ports[1:]):
            m.d.comb += wb.eq(fresp)

        for wb, eu in zip(self.wakeups[self.mem_width:],
                          [eu for eu in exec_units if eu.frf_write]):
            m.d.comb += [
                wb.eq(eu.fresp),
                wb.valid.eq(eu.fresp.valid
                            &
                            (eu.fresp.bits.uop.dst_rtype == RegisterType.FLT)),
            ]

        return m
