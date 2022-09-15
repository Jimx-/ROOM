from amaranth import *
from amaranth.utils import log2_int

from room.consts import *
from room.alu import ExecResp
from room.types import MicroOp

from room.if_stage import IFStage
from room.id_stage import DecodeStage
from room.rename import RenameStage
from room.dispatch import Dispatcher
from room.issue import IssueUnit
from room.rob import ReorderBuffer, FlushType
from room.regfile import RegisterFile, RegisterRead
from room.ex_stage import ExecUnits
from room.branch import BranchUpdate, BranchResolution
from room.lsu import LoadStoreUnit
from room.csr import CSRFile
from room.exc import ExceptionUnit, CoreInterrupts
from room.breakpoint import BreakpointUnit

from roomsoc.interconnect import wishbone


class Core(Elaboratable):

    def __init__(self, params):
        self.params = params
        self.xlen = params['xlen']
        self.core_width = params['core_width']

        self.interrupts = CoreInterrupts()
        self.debug_entry = Signal(32)

        ibus_addr_shift = Shape.cast(range(params['fetch_bytes'])).width
        self.ibus = wishbone.Interface(data_width=params['fetch_width'] * 16,
                                       addr_width=32 - ibus_addr_shift,
                                       granularity=8,
                                       name='ibus')
        self.dbus = wishbone.Interface(data_width=self.xlen,
                                       addr_width=32 -
                                       log2_int(self.xlen // 8),
                                       granularity=8,
                                       name='dbus')

        self.periph_buses = [self.ibus, self.dbus]

    @staticmethod
    def validate_params(params):
        xlen = params['xlen']
        vaddr_bits = params['vaddr_bits']
        params['vaddr_bits_extended'] = vaddr_bits + (vaddr_bits < xlen)

        params['fetch_bytes'] = params['fetch_width'] << 1
        params['mem_width'] = params['issue_params'][
            IssueQueueType.MEM]['issue_width']

        return params

    def elaborate(self, platform):
        m = Module()

        mem_width = self.params['mem_width']

        csr = m.submodules.csr = CSRFile(width=32)

        #
        # Exception
        #

        exc_unit = m.submodules.exc_unit = ExceptionUnit()
        csr.add_csrs(exc_unit.iter_csrs())

        m.d.comb += exc_unit.interrupts.eq(self.interrupts)

        #
        # Breakpoint unit
        #

        bp_unit = m.submodules.bp_unit = BreakpointUnit(self.params)
        csr.add_csrs(bp_unit.iter_csrs())

        m.d.comb += bp_unit.debug.eq(exc_unit.debug_mode)

        #
        # Instruction fetch
        #

        if_stage = m.submodules.if_stage = IFStage(self.ibus, self.params)

        for if_bp, bp in zip(if_stage.bp, bp_unit.bp):
            m.d.comb += if_bp.eq(bp)

        dec_ready = Signal()

        dis_fire = Signal(self.core_width)
        dis_ready = Signal()

        #
        # Decoding
        #

        br_update = BranchUpdate(self.params, name='br_update')

        dec_stage = m.submodules.decode_stage = DecodeStage(self.params)
        for a, b in zip(dec_stage.fetch_packet, if_stage.fetch_packet):
            m.d.comb += a.eq(b)
        m.d.comb += [
            dec_stage.fetch_packet_valid.eq(if_stage.fetch_packet_valid),
            dec_stage.dis_ready.eq(dis_ready),
            dec_stage.br_update.eq(br_update),
            dec_stage.redirect_flush.eq(if_stage.redirect_flush),
            dec_ready.eq(dec_stage.ready),
            if_stage.fetch_packet_ready.eq(dec_ready),
            dec_stage.interrupt.eq(exc_unit.interrupt),
            dec_stage.interrupt_cause.eq(exc_unit.interrupt_cause),
            dec_stage.single_step.eq(exc_unit.single_step),
        ]

        #
        # Renaming
        #

        exec_units = m.submodules.exec_units = ExecUnits(self.params)

        num_int_iss_wakeup_ports = exec_units.irf_write_ports + mem_width
        num_int_ren_wakeup_ports = num_int_iss_wakeup_ports

        ren_stage = m.submodules.rename_stage = RenameStage(
            num_int_ren_wakeup_ports, self.params)
        for a, b in zip(ren_stage.dec_uops, dec_stage.uops):
            m.d.comb += a.eq(b)
        m.d.comb += [
            ren_stage.kill.eq(if_stage.redirect_flush),
            ren_stage.dec_fire.eq(dec_stage.fire),
            ren_stage.dis_fire.eq(dis_fire),
            ren_stage.dis_ready.eq(dis_ready),
            ren_stage.br_update.eq(br_update),
        ]

        #
        # Dispatcher
        #

        lsu = m.submodules.lsu = LoadStoreUnit(self.dbus, self.params)

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
        rob_empty = Signal()

        dis_prior_valid = Signal(self.core_width)
        dis_prior_unique = Signal(self.core_width)
        for w in range(1, self.core_width):
            m.d.comb += [
                dis_prior_valid[w].eq(dis_prior_valid[w - 1]
                                      | dis_valids[w - 1]),
                dis_prior_unique[w].eq(dis_prior_unique[w - 1]
                                       | (dis_valids[w - 1]
                                          & dis_uops[w - 1].clear_pipeline)),
            ]

        wait_for_empty_pipeline = Signal(self.core_width)
        for w in range(self.core_width):
            m.d.comb += wait_for_empty_pipeline[w].eq(
                dis_uops[w].clear_pipeline & (~rob_empty | dis_prior_valid[w]))

        dis_hazards = [(dis_valids[w] &
                        (~rob_ready | ren_stage.stalls[w] |
                         (lsu.ldq_full[w] & dis_uops[w].uses_ldq) |
                         (lsu.stq_full[w] & dis_uops[w].uses_stq)
                         | ~dispatcher.ready[w] | wait_for_empty_pipeline[w]
                         | dis_prior_unique[w] | if_stage.redirect_flush))
                       for w in range(self.core_width)]
        dis_stalls = Signal(self.core_width)
        m.d.comb += dis_stalls[0].eq(dis_hazards[0])
        for i in range(1, self.core_width):
            m.d.comb += dis_stalls[i].eq(dis_hazards[i] | dis_stalls[i - 1])

        m.d.comb += [
            dis_fire.eq(dis_valids & ~dis_stalls),
            dis_ready.eq(~dis_stalls[-1]),
        ]

        for w in range(self.core_width):
            m.d.comb += dis_uops[w].prs1.eq(
                Mux(ren_stage.ren2_uops[w].lrs1_rtype == RegisterType.FIX,
                    ren_stage.ren2_uops[w].prs1, ren_stage.ren2_uops[w].lrs1))

        for w in range(self.core_width):
            m.d.comb += [
                dis_uops[w].ldq_idx.eq(lsu.dis_ldq_idx[w]),
                dis_uops[w].stq_idx.eq(lsu.dis_stq_idx[w]),
            ]

        for ren_uop, dis_uop in zip(dispatcher.ren_uops, dis_uops):
            m.d.comb += ren_uop.eq(dis_uop)
        m.d.comb += dispatcher.ren_valids.eq(dis_fire)

        #
        # ROB
        #

        rob = m.submodules.rob = ReorderBuffer(
            exec_units.irf_write_ports + mem_width, self.params)
        rob_flush_d1 = Record(rob.flush.layout)
        m.d.sync += rob_flush_d1.eq(rob.flush)

        for enq_uop, dis_uop in zip(rob.enq_uops, dis_uops):
            m.d.comb += enq_uop.eq(dis_uop)
        m.d.comb += [
            rob.enq_valids.eq(dis_fire),
            rob.enq_partial_stalls.eq(dis_stalls[-1]),
            rob.br_update.eq(br_update),
            rob_ready.eq(rob.ready),
            rob_empty.eq(rob.empty),
        ]

        m.d.comb += [
            dec_stage.rollback.eq(rob.commit_req.rollback),
            dec_stage.flush_pipeline.eq(rob_flush_d1.valid),
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
        # Frontend redirect
        #

        # Delay exception vector for 2 cycles
        evec_d1 = Signal.like(exc_unit.exc_vector)
        evec_d2 = Signal.like(exc_unit.exc_vector)
        m.d.sync += [
            evec_d1.eq(exc_unit.exc_vector),
            evec_d2.eq(evec_d1),
        ]

        with m.If(rob_flush_d1.valid):
            m.d.comb += [
                if_stage.redirect_valid.eq(1),
                if_stage.redirect_flush.eq(1),
                if_stage.redirect_ftq_idx.eq(rob_flush_d1.ftq_idx),
            ]

            flush_pc = if_stage.get_pc[0].pc + rob_flush_d1.pc_lsb
            with m.Switch(rob_flush_d1.flush_type):
                with m.Case(FlushType.EXCEPT):
                    m.d.comb += if_stage.redirect_pc.eq(exc_unit.exc_vector)
                with m.Case(FlushType.ERET):
                    m.d.comb += if_stage.redirect_pc.eq(evec_d2)
                with m.Case(FlushType.NEXT):
                    m.d.comb += if_stage.redirect_pc.eq(
                        flush_pc + Mux(rob_flush_d1.is_rvc, 2, 4))
                with m.Case(FlushType.REFETCH):
                    m.d.comb += if_stage.redirect_pc.eq(flush_pc)

        with m.Elif(br_update.br_res.mispredict & ~rob_flush_d1.valid):
            uop = br_update.br_res.uop
            uop_pc = if_stage.get_pc[1].pc | uop.pc_lsb
            npc = uop_pc + Mux(uop.is_rvc, 2, 4)
            br_target = uop_pc + br_update.br_res.target_offset
            bj_addr = Mux(br_update.br_res.cfi_type == CFIType.JALR,
                          br_update.br_res.jalr_target, br_target)
            redirect_target = Mux(br_update.br_res.pc_sel == PCSel.PC_PLUS_4,
                                  npc, bj_addr)

            m.d.comb += [
                if_stage.redirect_valid.eq(1),
                if_stage.redirect_pc.eq(redirect_target),
                if_stage.redirect_flush.eq(1),
                if_stage.redirect_ftq_idx.eq(uop.ftq_idx),
            ]
        with m.Elif(br_update.mispredict_mask != 0):
            m.d.comb += if_stage.redirect_flush.eq(1)

        com_ftq_idx = Signal.like(rob.commit_req.uops[0].ftq_idx)
        for w in range(self.core_width):
            with m.If(rob.commit_req.valids[w]):
                m.d.comb += com_ftq_idx.eq(rob.commit_req.uops[w].ftq_idx)
        m.d.comb += [
            if_stage.commit.eq(
                Mux(rob.commit_exc.valid, rob.commit_exc.ftq_idx,
                    com_ftq_idx)),
            if_stage.commit_valid.eq((rob.commit_req.valids != 0)
                                     | rob.commit_exc.valid),
        ]

        # ECALL needs to write PC into CSR before commit
        ecall_commit = Signal.like(if_stage.commit)
        ecall_commit_valid = Signal.like(if_stage.commit_valid)
        m.d.sync += [
            ecall_commit.eq(0),
            ecall_commit_valid.eq(0),
        ]

        for w in reversed(range(self.core_width)):
            with m.If(dis_fire[w] & dis_uops[w].is_ecall):
                m.d.sync += [
                    ecall_commit.eq(dis_uops[w].ftq_idx),
                    ecall_commit_valid.eq(1),
                ]

        with m.If(ecall_commit_valid):
            m.d.comb += [
                if_stage.commit.eq(ecall_commit),
                if_stage.commit_valid.eq(1),
            ]

        #
        # Issue queue
        #

        for (typ, qp), duops, dvalids, dready in zip(
                self.params['issue_params'].items(), dispatcher.dis_uops,
                dispatcher.dis_valids, dispatcher.iq_ready):
            iq = issue_units[typ]
            for quop, duop in zip(iq.dis_uops, duops):
                m.d.comb += [quop.eq(duop)]
            m.d.comb += [
                iq.dis_valids.eq(dvalids),
                dready.eq(iq.ready),
                iq.br_update.eq(br_update),
                iq.flush_pipeline.eq(rob_flush_d1.valid),
            ]

        #
        # Wakeup (issue & rename)
        #

        int_iss_wakeups = []
        int_ren_wakeups = []

        for w in range(mem_width):
            resp = lsu.exec_iresps[w]

            wakeup = ExecResp(self.params, name=f'iss_ren_wakeup{w}')
            m.d.comb += [
                wakeup.uop.eq(resp.uop),
                wakeup.valid.eq(resp.valid & resp.uop.rf_wen()
                                & resp.uop.dst_rtype == RegisterType.FIX),
            ]

            int_iss_wakeups.append(wakeup)
            int_ren_wakeups.append(wakeup)

        for i, eu in enumerate(exec_units, mem_width):
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

        iqs_mem_int = [
            issue_units[typ]
            for typ in (IssueQueueType.MEM, IssueQueueType.INT)
        ]

        iss_uops = []
        iss_fu_types = []
        iss_valids = Signal(
            sum([
                p['issue_width'] for p in self.params['issue_params'].values()
            ]))
        for iq in iqs_mem_int:
            iss_uops.extend(iq.iss_uops)
            iss_fu_types.extend(iq.fu_types)
        m.d.comb += iss_valids.eq(Cat([iq.iss_valids for iq in iqs_mem_int]))

        iregfile = m.submodules.iregfile = RegisterFile(
            rports=exec_units.irf_read_ports,
            wports=exec_units.irf_write_ports + mem_width,
            num_regs=self.params['num_pregs'],
            data_width=self.xlen)

        iregread = m.submodules.iregread = RegisterRead(
            issue_width=sum([
                p['issue_width'] for p in self.params['issue_params'].values()
            ]),
            num_rports=exec_units.irf_read_ports,
            rports_array=[2] * exec_units.irf_readers,
            reg_width=self.xlen,
            params=self.params)

        for irr_uop, iss_uop in zip(iregread.iss_uops, iss_uops):
            m.d.comb += irr_uop.eq(iss_uop)
        m.d.comb += iregread.iss_valids.eq(iss_valids)

        for iss_fu_typ, eu in zip(iss_fu_types, exec_units):
            m.d.comb += iss_fu_typ.eq(eu.fu_types)

        for irr_rp, rp in zip(iregread.read_ports, iregfile.read_ports):
            m.d.comb += irr_rp.connect(rp)

        m.d.comb += [
            iregread.br_update.eq(br_update),
            iregread.kill.eq(rob_flush_d1.valid),
        ]

        #
        # CSR register file
        #

        csr_port = csr.access_port()
        csr_exec_unit = [eu for eu in exec_units if eu.has_csr][0]

        m.d.comb += [
            csr_port.addr.eq(csr_exec_unit.iresp.uop.csr_addr),
            csr_port.cmd.eq(
                csr_exec_unit.iresp.uop.csr_cmd
                & ~Mux(csr_exec_unit.iresp.valid, 0, CSRCommand.I)),
            csr_port.w_data.eq(csr_exec_unit.iresp.data),
        ]

        #
        # Execute
        #

        for eu, req in zip([eu for eu in exec_units if eu.irf_read],
                           iregread.exec_reqs):
            m.d.comb += [
                eu.req.eq(req),
                eu.req.kill.eq(rob_flush_d1.valid),
            ]

        br_infos = [
            BranchResolution(self.params, name=f'br_res{i}')
            for i in range(self.core_width)
        ]

        for eu in exec_units:
            m.d.comb += eu.br_update.eq(br_update)

        for br_res, eu in zip(br_infos,
                              [eu for eu in exec_units if eu.has_alu]):
            m.d.sync += [
                br_res.eq(eu.br_res),
                br_res.valid.eq(eu.br_res.valid & ~rob.flush.valid),
            ]

        #
        # Get PC for jump unit
        #

        jmp_pc_req = Signal.like(iss_uops[0].ftq_idx)
        jmp_pc_valid = Signal()

        with m.If(jmp_pc_valid):
            m.d.comb += if_stage.get_pc_idx[0].eq(jmp_pc_req)
        with m.Elif(rob.flush.valid):
            m.d.comb += if_stage.get_pc_idx[0].eq(rob.flush.ftq_idx)

        for eu, iss_uop, iss_valid in zip(exec_units, iss_uops, iss_valids):
            if not eu.has_jmp_unit: continue

            m.d.sync += [
                jmp_pc_valid.eq(iss_valid & (iss_uop.fu_type == FUType.JMP)),
                jmp_pc_req.eq(iss_uop.ftq_idx),
            ]

            m.d.comb += [
                eu.get_pc.pc.eq(if_stage.get_pc[0].pc),
                eu.get_pc.next_valid.eq(if_stage.get_pc[0].next_valid),
                eu.get_pc.next_pc.eq(if_stage.get_pc[0].next_pc),
            ]

        #
        # Load/store unit
        #

        for w in range(self.core_width):
            m.d.comb += [
                lsu.dis_valids[w].eq(dis_fire[w]),
                lsu.dis_uops[w].eq(dis_uops[w]),
            ]

        for req, eu in zip(lsu.exec_reqs,
                           [eu for eu in exec_units if eu.has_mem]):
            m.d.sync += [
                req.eq(eu.lsu_req),
            ]

        m.d.comb += [
            lsu.commit.eq(rob.commit_req),
            lsu.br_update.eq(br_update),
            lsu.exception.eq(rob_flush_d1.valid),
        ]

        #
        # Branch resolution
        #

        max_br_count = self.params['max_br_count']

        resolve_mask = Const(0, max_br_count)
        mispredict_mask = Const(0, max_br_count)
        for br_res in br_infos:
            resolve_mask |= (br_res.valid << br_res.uop.br_tag)
            mispredict_mask |= (
                (br_res.valid & br_res.mispredict) << br_res.uop.br_tag)

        m.d.comb += [
            br_update.resolve_mask.eq(resolve_mask),
            br_update.mispredict_mask.eq(mispredict_mask),
        ]

        mispredict_val = 0
        oldest_res = BranchResolution(self.params)
        m.d.comb += oldest_res.eq(br_infos[0])
        for br_res in br_infos:
            next_res = BranchResolution(self.params)

            this_valid = br_res.valid & br_res.mispredict
            sel = ~mispredict_val | (this_valid & (
                (br_res.uop.rob_idx < oldest_res.uop.rob_idx) ^
                (br_res.uop.rob_idx < rob.head_idx) ^
                (oldest_res.uop.rob_idx < rob.head_idx)))

            mispredict_val |= this_valid

            with m.If(sel == 1):
                m.d.comb += next_res.eq(br_res)
            with m.Else():
                m.d.comb += next_res.eq(oldest_res)

            oldest_res = next_res

        jmp_unit = [eu for eu in exec_units if eu.has_jmp_unit][0]
        jalr_target_d1 = Signal(32)
        m.d.sync += [
            br_update.br_res.eq(oldest_res),
            br_update.br_res.mispredict.eq(mispredict_val),
            jalr_target_d1.eq(jmp_unit.br_res.jalr_target),
            br_update.br_res.jalr_target.eq(jalr_target_d1),
        ]

        m.d.comb += if_stage.get_pc_idx[1].eq(oldest_res.uop.ftq_idx)

        #
        # Writeback
        #

        for wp, iresp in zip(iregfile.write_ports[:mem_width],
                             lsu.exec_iresps):
            m.d.comb += [
                wp.valid.eq(iresp.valid & iresp.uop.rf_wen()
                            & iresp.uop.dst_rtype == RegisterType.FIX),
                wp.addr.eq(iresp.uop.pdst),
                wp.data.eq(iresp.data),
            ]

        for eu, wp in zip([eu for eu in exec_units if eu.irf_write],
                          iregfile.write_ports[mem_width:]):
            m.d.comb += [
                wp.valid.eq(eu.iresp.valid & eu.iresp.uop.rf_wen()
                            & eu.iresp.uop.dst_rtype == RegisterType.FIX),
                wp.addr.eq(eu.iresp.uop.pdst),
            ]

            if eu.has_csr:
                m.d.comb += wp.data.eq(
                    Mux(eu.iresp.uop.csr_cmd != CSRCommand.X, csr_port.r_data,
                        eu.iresp.data))
            else:
                m.d.comb += wp.data.eq(eu.iresp.data)

        #
        # Commit
        #

        for rob_wb, iresp in zip(rob.wb_resps[:mem_width], lsu.exec_iresps):
            m.d.comb += rob_wb.eq(iresp)

        for rob_wb, eu in zip(rob.wb_resps[mem_width:],
                              [eu for eu in exec_units if eu.irf_write]):
            m.d.comb += rob_wb.eq(eu.iresp)

        for a, b in zip(rob.lsu_clear_busy_idx, lsu.clear_busy_idx):
            m.d.comb += a.eq(b)
        m.d.comb += rob.lsu_clear_busy_valids.eq(lsu.clear_busy_valids)

        #
        # Exception
        #

        commit_exc_pc_lsb_d1 = Signal.like(rob.commit_exc.pc_lsb)
        m.d.comb += [
            exc_unit.epc.eq(if_stage.get_pc[0].commit_pc +
                            commit_exc_pc_lsb_d1),
            exc_unit.debug_entry.eq(self.debug_entry),
            exc_unit.system_insn.eq(csr_port.cmd == CSRCommand.I),
            exc_unit.system_insn_imm.eq(csr_port.addr),
            exc_unit.commit.eq(rob.commit_req.valids[0]),
        ]

        m.d.sync += [
            commit_exc_pc_lsb_d1.eq(rob.commit_exc.pc_lsb),
            exc_unit.exception.eq(rob.commit_exc.valid),
            exc_unit.cause.eq(rob.commit_exc.cause),
        ]

        return m
