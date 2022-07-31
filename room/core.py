from amaranth import *

from room.consts import *
from room.alu import ExecResp
from room.types import MicroOp

from room.if_stage import IFStage
from room.id_stage import DecodeStage
from room.rename import RenameStage
from room.dispatch import Dispatcher
from room.issue import IssueUnit
from room.rob import ReorderBuffer
from room.regfile import RegisterFile, RegisterRead
from room.ex_stage import ExecUnits

from roomsoc.interconnect import wishbone


class Core(Elaboratable):

    def __init__(self, params):
        self.params = params
        self.core_width = params['core_width']

        self.ibus = wishbone.Interface(data_width=params['fetch_width'] * 16,
                                       adr_width=32)

    def elaborate(self, platform):
        m = Module()

        #
        # Instruction fetch
        #

        if_stage = m.submodules.if_stage = IFStage(self.ibus, self.params)

        dec_ready = Signal()

        dis_fire = Signal(self.core_width)
        dis_ready = Signal()

        #
        # Decoding
        #

        dec_stage = m.submodules.decode_stage = DecodeStage(self.params)
        for a, b in zip(dec_stage.fetch_packet, if_stage.fetch_packet):
            m.d.comb += a.eq(b)
        m.d.comb += [
            dec_stage.fetch_packet_valid.eq(if_stage.fetch_packet_valid),
            dec_stage.dis_ready.eq(dis_ready),
            dec_ready.eq(dec_stage.ready),
            if_stage.fetch_packet_ready.eq(dec_ready),
        ]

        #
        # Renaming
        #

        exec_units = m.submodules.exec_units = ExecUnits(self.params)

        num_int_iss_wakeup_ports = exec_units.irf_write_ports
        num_int_ren_wakeup_ports = num_int_iss_wakeup_ports

        ren_stage = m.submodules.rename_stage = RenameStage(
            num_int_ren_wakeup_ports, self.params)
        for a, b in zip(ren_stage.dec_uops, dec_stage.uops):
            m.d.comb += a.eq(b)
        m.d.comb += [
            ren_stage.dec_fire.eq(dec_stage.fire),
            ren_stage.dis_fire.eq(dis_fire),
            ren_stage.dis_ready.eq(dis_ready),
        ]

        #
        # Dispatcher
        #

        dispatcher = m.submodules.dispatcher = Dispatcher(self.params)

        issue_units = dict()
        for typ, qp in self.params['issue_params'].items():
            iq = IssueUnit(qp['issue_width'], qp['num_entries'],
                           qp['dispatch_width'], num_int_iss_wakeup_ports,
                           self.params)
            setattr(m.submodules, f'issue_unit_{str(typ).split(".")[-1]}', iq)
            issue_units[typ] = iq

        dis_valids = ren_stage.ren2_mask
        dis_uops = [
            MicroOp(self.params, name=f'dis_uop{i}')
            for i in range(self.core_width)
        ]

        for dis_uop, ren2_uop in zip(dis_uops, ren_stage.ren2_uops):
            m.d.comb += dis_uop.eq(ren2_uop)

        rob_ready = Signal()
        m.d.comb += rob_ready.eq(1)

        dis_hazards = [(v & (~rob_ready | ren_stall | ~iq_ready))
                       for v, ren_stall, iq_ready in zip(
                           dis_valids, ren_stage.stalls, dispatcher.ready)]
        dis_stalls = Signal(self.core_width)
        m.d.comb += dis_stalls[0].eq(dis_hazards[0])
        for i in range(1, self.core_width):
            m.d.comb += dis_stalls[i].eq(dis_hazards[i] | dis_stalls[i - 1])

        m.d.comb += [
            dis_fire.eq(dis_valids & ~dis_stalls),
            dis_ready.eq(~dis_stalls[-1]),
        ]

        for ren_uop, dis_uop in zip(dispatcher.ren_uops, dis_uops):
            m.d.comb += ren_uop.eq(dis_uop)
        m.d.comb += dispatcher.ren_valids.eq(dis_fire)

        #
        # ROB
        #

        rob = m.submodules.rob = ReorderBuffer(exec_units.irf_write_ports,
                                               self.params)

        for enq_uop, dis_uop in zip(rob.enq_uops, dis_uops):
            m.d.comb += enq_uop.eq(dis_uop)
        m.d.comb += [
            rob.enq_valids.eq(dis_fire),
            rob.enq_partial_stalls.eq(dis_stalls[-1]),
            rob_ready.eq(rob.ready),
        ]

        m.d.comb += ren_stage.commit.eq(rob.commit_req)

        for w, dis_uop in enumerate(dis_uops):
            if self.core_width == 1:
                m.d.comb += dis_uop.rob_idx.eq(rob.tail_idx)
            else:
                width = Shape.cast(range(self.core_width)).width
                m.d.comb += dis_uop.rob_idx.eq(
                    Cat(Const(w, width), rob.tail_idx >> width))

        #
        # Issue queue
        #

        for (typ, qp), duops, dvalids, dready in zip(
                self.params['issue_params'].items(), dispatcher.dis_uops,
                dispatcher.dis_valids, dispatcher.iq_ready):
            iq = issue_units[typ]
            for quop, duop in zip(iq.dis_uops, duops):
                m.d.comb += [quop.eq(duop)]
            m.d.comb += [iq.dis_valids.eq(dvalids), dready.eq(iq.ready)]

        #
        # Wakeup (issue & rename)
        #

        int_iss_wakeups = []
        int_ren_wakeups = []

        for i, eu in enumerate(exec_units):
            if eu.irf_write:
                resp = eu.iresp

                wakeup = ExecResp(self.params, name=f'iss_ren_wakeup{i}')
                m.d.comb += [
                    wakeup.uop.eq(resp.uop),
                    wakeup.valid.eq(resp.valid & resp.uop.rf_wen()
                                    & resp.uop.dst_rtype == RegisterType.FIX),
                ]

                int_iss_wakeups.append(wakeup)
                int_ren_wakeups.append(wakeup)

        for rwp, wu in zip(ren_stage.wakeup_ports, int_ren_wakeups):
            m.d.comb += [
                rwp.valid.eq(wu.valid),
                rwp.pdst.eq(wu.uop.pdst),
            ]

        for iu in issue_units.values():
            for iwp, wu in zip(iu.wakeup_ports, int_iss_wakeups):
                m.d.comb += [
                    iwp.valid.eq(wu.valid),
                    iwp.pdst.eq(wu.uop.pdst),
                ]

        #
        # Register read
        #

        iss_uops = []
        iss_valids = Signal(
            sum([
                p['issue_width'] for p in self.params['issue_params'].values()
            ]))
        for iq in issue_units.values():
            iss_uops.extend(iq.iss_uops)
        m.d.comb += iss_valids.eq(
            Cat([iq.iss_valids for iq in issue_units.values()]))

        iregfile = m.submodules.iregfile = RegisterFile(
            exec_units.irf_read_ports, exec_units.irf_write_ports,
            self.params['num_pregs'], 32)

        iregread = m.submodules.iregread = RegisterRead(
            sum([
                p['issue_width'] for p in self.params['issue_params'].values()
            ]), exec_units.irf_read_ports, [2] * exec_units.irf_readers,
            self.params)

        for irr_uop, iss_uop in zip(iregread.iss_uops, iss_uops):
            m.d.comb += irr_uop.eq(iss_uop)
        m.d.comb += iregread.iss_valids.eq(iss_valids)

        for irr_rp, rp in zip(iregread.read_ports, iregfile.read_ports):
            m.d.comb += irr_rp.connect(rp)

        #
        # Execute
        #

        for eu, req in zip([eu for eu in exec_units if eu.irf_read],
                           iregread.exec_reqs):
            m.d.comb += eu.req.eq(req)

        #
        # Writeback
        #

        for eu, wp in zip([eu for eu in exec_units if eu.irf_write],
                          iregfile.write_ports):
            m.d.comb += [
                wp.valid.eq(eu.iresp.valid & eu.iresp.uop.rf_wen()
                            & eu.iresp.uop.dst_rtype == RegisterType.FIX),
                wp.addr.eq(eu.iresp.uop.pdst),
                wp.data.eq(eu.iresp.data),
            ]

        #
        # Commit
        #

        for rob_wb, eu in zip(rob.wb_resps,
                              [eu for eu in exec_units if eu.irf_write]):
            m.d.comb += rob_wb.eq(eu.iresp)

        return m
