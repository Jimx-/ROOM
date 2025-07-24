from amaranth import *
from amaranth.utils import log2_int

from room.consts import *
from room.fu import ExecResp
from room.types import HasCoreParams, MicroOp

from room.if_stage import IFStage, IFDebug
from room.id_stage import DecodeStage, IDDebug
from room.rename import RenameStage
from room.dispatch import Dispatcher
from room.issue import IssueUnitUnordered, IssueUnitOrdered
from room.rob import ReorderBuffer, FlushType
from room.regfile import RegisterFile, RegisterRead, WritebackDebug
from room.ex_stage import ExecUnits, ExecDebug
from room.branch import GlobalHistory, BranchUpdate, BranchResolution
from room.lsu import LoadStoreUnit, LSUDebug
from room.csr import CSRFile
from room.exc import ExceptionUnit, CoreInterrupts, Cause
from room.breakpoint import BreakpointUnit
from room.fp_pipeline import FPPipeline
from room.mmu import PageTableWalker, CoreMemRequest
from room.utils import Arbiter

from roomsoc.interconnect.stream import Valid, Decoupled
from roomsoc.interconnect import wishbone, tilelink as tl


class CommitDebug(Record):

    def __init__(self, name=None, src_loc_at=0):
        super().__init__([
            ('uop_id', MicroOp.ID_WIDTH),
        ],
                         name=name,
                         src_loc_at=1 + src_loc_at)


class ExceptionDebug(HasCoreParams, Record):

    def __init__(self, params, name=None, src_loc_at=0):
        HasCoreParams.__init__(self, params)

        Record.__init__(self, [
            ('cause', self.xlen),
            ('inst', 32),
        ],
                        name=name,
                        src_loc_at=1 + src_loc_at)


class CoreDebug(HasCoreParams):

    def __init__(self, params):
        super().__init__(params)

        int_width = self.issue_params[IssueQueueType.INT]['issue_width']

        self.if_debug = [
            Valid(IFDebug, name=f'if_debug{i}')
            for i in range(self.fetch_width)
        ]

        self.id_debug = [
            Valid(IDDebug, params, name=f'id_debug{i}')
            for i in range(self.core_width)
        ]

        self.ex_debug = [
            Valid(ExecDebug, params, name=f'ex_debug{i}')
            for i in range(int_width)
        ]

        self.mem_debug = [
            Valid(LSUDebug, params, name=f'mem_debug{i}')
            for i in range(self.mem_width)
        ]

        self.wb_debug = [
            Valid(WritebackDebug, params, name=f'wb_debug{i}')
            for i in range(self.mem_width + int_width + self.use_vector)
        ]

        if self.use_fpu:
            fp_width = self.issue_params[IssueQueueType.FP]['issue_width']

            self.fp_wb_debug = [
                Valid(WritebackDebug, params, name=f'fp_wb_debug{i}')
                for i in range(self.mem_width + fp_width)
            ]

        self.commit_debug = [
            Valid(CommitDebug, name=f'commit_debug{i}')
            for i in range(self.core_width)
        ]

        self.exc_debug = Valid(ExceptionDebug, params)

        self.branch_resolve = Signal(self.max_br_count)
        self.branch_mispredict = Signal(self.max_br_count)
        self.flush_pipeline = Signal()

    def eq(self, rhs):
        ret = []

        for l, r in zip(self.if_debug, rhs.if_debug):
            ret.append(l.eq(r))

        for l, r in zip(self.id_debug, rhs.id_debug):
            ret.append(l.eq(r))

        for l, r in zip(self.ex_debug, rhs.ex_debug):
            ret.append(l.eq(r))

        for l, r in zip(self.mem_debug, rhs.mem_debug):
            ret.append(l.eq(r))

        for l, r in zip(self.wb_debug, rhs.wb_debug):
            ret.append(l.eq(r))

        if self.use_fpu:
            for l, r in zip(self.fp_wb_debug, rhs.fp_wb_debug):
                ret.append(l.eq(r))

        for l, r in zip(self.commit_debug, rhs.commit_debug):
            ret.append(l.eq(r))

        ret += [
            self.exc_debug.eq(rhs.exc_debug),
            self.branch_resolve.eq(rhs.branch_resolve),
            self.branch_mispredict.eq(rhs.branch_mispredict),
            self.flush_pipeline.eq(rhs.flush_pipeline),
        ]

        return ret


class Core(HasCoreParams, Elaboratable):

    def __init__(self, params, sim_debug=False):
        super().__init__(params)

        self.sim_debug = sim_debug

        self.reset_vector = Signal(32)

        self.interrupts = CoreInterrupts()
        self.debug_entry = Signal(32)
        self.debug_exception = Signal(32)

        self.periph_buses = []

        if params.get('icache_params') is None:
            ibus_addr_shift = Shape.cast(range(self.fetch_bytes)).width
            self.ibus = wishbone.Interface(data_width=self.fetch_width * 16,
                                           addr_width=32 - ibus_addr_shift,
                                           granularity=8,
                                           name='ibus')

            self.periph_buses.append(self.ibus)
        else:
            self.ibus = tl.Interface(data_width=self.fetch_width * 16,
                                     addr_width=32,
                                     size_width=3,
                                     source_id_width=4,
                                     sink_id_width=4,
                                     name='ibus')

        self.dbus_mmio = None
        self.core_bus = None

        if params.get('dcache_params') is None:
            self.dbus = wishbone.Interface(data_width=self.xlen,
                                           addr_width=32 -
                                           log2_int(self.xlen // 8),
                                           granularity=8,
                                           name='dbus')

            self.periph_buses.append(self.dbus)
        else:
            self.dbus = tl.Interface(data_width=self.xlen,
                                     addr_width=32,
                                     size_width=3,
                                     source_id_width=4,
                                     sink_id_width=4,
                                     has_bce=True,
                                     name='dbus')

            self.dbus_mmio = tl.Interface(data_width=self.xlen,
                                          addr_width=32,
                                          size_width=3,
                                          source_id_width=1,
                                          name='dbus_mmio')

            self.periph_buses.append(self.dbus_mmio)

        if params.get('icache_params') is not None and params.get(
                'dcache_params') is not None:
            self.core_bus = tl.Interface(data_width=self.xlen,
                                         addr_width=32,
                                         size_width=3,
                                         source_id_width=4,
                                         sink_id_width=4,
                                         has_bce=True)

        if sim_debug:
            self.core_debug = CoreDebug(params)

    @property
    def pma_regions(self):
        return self.params.get('pma_regions')

    @pma_regions.setter
    def pma_regions(self, value):
        self.params['pma_regions'] = value

    def elaborate(self, platform):
        m = Module()

        csr = m.submodules.csr = CSRFile(self.params, width=self.xlen)
        m.d.comb += csr.seip.eq(self.interrupts.seip)

        #
        # Exception
        #

        exc_unit = m.submodules.exc_unit = ExceptionUnit(self.params)
        csr.add_csrs(exc_unit.iter_csrs())

        m.d.comb += [
            exc_unit.interrupts.eq(self.interrupts),
            csr.prv.eq(exc_unit.prv),
        ]

        #
        # Breakpoint unit
        #

        bp_unit = m.submodules.bp_unit = BreakpointUnit(self.params)
        csr.add_csrs(bp_unit.iter_csrs())

        m.d.comb += bp_unit.debug.eq(exc_unit.debug_mode)

        #
        # Floating point pipeline
        #

        if self.use_fpu:
            fp_pipeline = m.submodules.fp_pipeline = FPPipeline(
                self.params, sim_debug=self.sim_debug)

            if self.sim_debug:
                for a, b in zip(self.core_debug.fp_wb_debug,
                                fp_pipeline.wb_debug):
                    m.d.comb += a.eq(b)

        #
        # Instruction fetch
        #

        if_stage = m.submodules.if_stage = IFStage(self.ibus,
                                                   self.params,
                                                   sim_debug=self.sim_debug)

        m.d.comb += [
            if_stage.reset_vector.eq(self.reset_vector),
            if_stage.prv.eq(exc_unit.prv),
            if_stage.status.eq(exc_unit.mstatus.r),
        ]

        for if_bp, bp in zip(if_stage.bp, bp_unit.bp):
            m.d.comb += if_bp.eq(bp)

        if self.sim_debug:
            for l, r in zip(self.core_debug.if_debug, if_stage.if_debug):
                m.d.comb += l.eq(r)

        dec_ready = Signal()

        dis_fire = Signal(self.core_width)
        dis_ready = Signal()

        #
        # Decoding
        #

        br_update = BranchUpdate(self.params, name='br_update')

        dec_stage = m.submodules.decode_stage = DecodeStage(
            self.params, sim_debug=self.sim_debug)
        for a, b in zip(dec_stage.fetch_packet, if_stage.fetch_packet):
            m.d.comb += a.eq(b)
        for a, b in zip(dec_stage.csr_decode, csr.decode):
            m.d.comb += a.connect(b)
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

        if self.sim_debug:
            for l, r in zip(self.core_debug.id_debug, dec_stage.id_debug):
                m.d.comb += l.eq(r)

        #
        # Renaming
        #

        exec_units = m.submodules.exec_units = ExecUnits(
            False, self.params, sim_debug=self.sim_debug)

        num_fast_wakeup_ports = len([eu for eu in exec_units if eu.can_bypass])
        num_always_bypass = len([eu for eu in exec_units if eu.always_bypass])

        num_vec_wakeup_ports = 1 if self.use_vector else 0

        num_int_iss_wakeup_ports = exec_units.irf_write_ports + self.mem_width + num_fast_wakeup_ports - num_always_bypass + num_vec_wakeup_ports
        num_int_ren_wakeup_ports = num_int_iss_wakeup_ports
        num_fp_wakeup_ports = len(fp_pipeline.wakeups) if self.use_fpu else 0

        ren_stage = m.submodules.rename_stage = RenameStage(
            num_pregs=self.num_int_pregs,
            num_wakeup_ports=num_int_ren_wakeup_ports,
            is_float=False,
            params=self.params)

        fp_ren_stage = None
        if self.use_fpu:
            fp_ren_stage = m.submodules.fp_rename_stage = RenameStage(
                num_pregs=self.num_fp_pregs,
                num_wakeup_ports=num_fp_wakeup_ports,
                is_float=True,
                params=self.params)

        for a, b in zip(ren_stage.dec_uops, dec_stage.uops):
            m.d.comb += a.eq(b)
        m.d.comb += [
            ren_stage.kill.eq(if_stage.redirect_flush),
            ren_stage.dec_fire.eq(dec_stage.fire),
            ren_stage.dis_fire.eq(dis_fire),
            ren_stage.dis_ready.eq(dis_ready),
            ren_stage.br_update.eq(br_update),
        ]

        if self.use_fpu:
            for a, b in zip(fp_ren_stage.dec_uops, dec_stage.uops):
                m.d.comb += a.eq(b)
            m.d.comb += [
                fp_ren_stage.kill.eq(if_stage.redirect_flush),
                fp_ren_stage.dec_fire.eq(dec_stage.fire),
                fp_ren_stage.dis_fire.eq(dis_fire),
                fp_ren_stage.dis_ready.eq(dis_ready),
                fp_ren_stage.br_update.eq(br_update),
            ]

        #
        # Dispatcher
        #

        lsu = m.submodules.lsu = LoadStoreUnit(self.dbus,
                                               self.dbus_mmio,
                                               self.params,
                                               sim_debug=self.sim_debug)
        m.d.comb += [
            lsu.prv.eq(exc_unit.dprv),
            lsu.status.eq(exc_unit.mstatus.r),
        ]

        dispatcher = m.submodules.dispatcher = Dispatcher(self.params)

        issue_units = dict()
        for typ, qp in self.issue_params.items():
            if typ != IssueQueueType.FP and (self.use_vector
                                             or typ != IssueQueueType.VEC):
                issue_unit_cls = IssueUnitUnordered
                if typ == IssueQueueType.VEC:
                    issue_unit_cls = IssueUnitOrdered

                iq = issue_unit_cls(qp['issue_width'], qp['num_entries'],
                                    qp['dispatch_width'],
                                    num_int_iss_wakeup_ports, typ, self.params)
                setattr(m.submodules, f'issue_unit_{str(typ).split(".")[-1]}',
                        iq)
                issue_units[typ] = iq

        dis_valids = Signal.like(ren_stage.ren2_mask)
        dis_uops = [
            MicroOp(self.params, name=f'dis_uop{i}')
            for i in range(self.core_width)
        ]
        ren_stalls = Signal.like(ren_stage.stalls)

        m.d.comb += [
            dis_valids.eq(ren_stage.ren2_mask),
            ren_stalls.eq(ren_stage.stalls),
        ]
        for dis_uop, ren2_uop in zip(dis_uops, ren_stage.ren2_uops):
            m.d.comb += dis_uop.eq(ren2_uop)

        for w in range(self.core_width):
            i_uop = ren_stage.ren2_uops[w]
            f_uop = fp_ren_stage.ren2_uops[
                w] if self.use_fpu else ren_stage.ren2_uops[w]

            m.d.comb += [
                dis_uops[w].prs1.eq(
                    Mux(
                        dis_uops[w].lrs1_rtype == RegisterType.FLT,
                        f_uop.prs1,
                        Mux(dis_uops[w].lrs1_rtype == RegisterType.FIX,
                            i_uop.prs1, dis_uops[w].lrs1),
                    )),
                dis_uops[w].prs2.eq(
                    Mux(dis_uops[w].lrs2_rtype == RegisterType.FLT, f_uop.prs2,
                        i_uop.prs2)),
                dis_uops[w].prs3.eq(f_uop.prs3),
                dis_uops[w].pdst.eq(
                    Mux(dis_uops[w].dst_rtype == RegisterType.FLT, f_uop.pdst,
                        i_uop.pdst)),
                dis_uops[w].stale_pdst.eq(
                    Mux(dis_uops[w].dst_rtype == RegisterType.FLT,
                        f_uop.stale_pdst, i_uop.stale_pdst)),
                dis_uops[w].prs1_busy.eq((
                    (dis_uops[w].lrs1_rtype == RegisterType.FIX)
                    & i_uop.prs1_busy) | (
                        (dis_uops[w].lrs1_rtype == RegisterType.FLT)
                        & f_uop.prs1_busy)),
                dis_uops[w].prs2_busy.eq((
                    (dis_uops[w].lrs2_rtype == RegisterType.FIX)
                    & i_uop.prs2_busy) | (
                        (dis_uops[w].lrs2_rtype == RegisterType.FLT)
                        & f_uop.prs2_busy)),
                dis_uops[w].prs3_busy.eq(dis_uops[w].frs3_en
                                         & f_uop.prs3_busy),
                ren_stalls[w].eq(
                    ren_stage.stalls[w]
                    | (fp_ren_stage.stalls[w] if self.use_fpu else 0)),
            ]

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
                        (~rob_ready | ren_stalls[w] |
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
            exec_units.irf_write_ports + self.mem_width + num_fp_wakeup_ports,
            self.params)
        rob_flush_d1 = Valid(Record, rob.flush.bits.layout)
        m.d.sync += rob_flush_d1.eq(rob.flush)

        for enq_uop, dis_uop in zip(rob.enq_uops, dis_uops):
            m.d.comb += enq_uop.eq(dis_uop)
        m.d.comb += [
            rob.enq_valids.eq(dis_fire),
            rob.enq_partial_stalls.eq(dis_stalls[-1]),
            rob.br_update.eq(br_update),
            rob.csr_stall.eq(exc_unit.csr_stall),
            rob_ready.eq(rob.ready),
            rob_empty.eq(rob.empty),
        ]

        m.d.comb += [
            if_stage.flush_icache.eq(
                Cat(*Array((rob.commit_req.valids[i]
                            & rob.commit_req.uops[i].is_fencei)
                           for i in range(self.core_width))) != 0),
            dec_stage.rollback.eq(rob.commit_req.rollback),
            dec_stage.flush_pipeline.eq(rob_flush_d1.valid),
        ]

        m.d.comb += ren_stage.commit.eq(rob.commit_req)
        if self.use_fpu:
            m.d.comb += fp_ren_stage.commit.eq(rob.commit_req)

        for w, dis_uop in enumerate(dis_uops):
            if self.core_width == 1:
                m.d.comb += dis_uop.rob_idx.eq(rob.tail_idx)
            else:
                width = Shape.cast(range(self.core_width)).width
                m.d.comb += dis_uop.rob_idx.eq(
                    Cat(Const(w, width), rob.tail_idx >> width))

        if self.sim_debug:
            for w in range(self.core_width):
                commit_debug = self.core_debug.commit_debug[w]

                m.d.comb += [
                    commit_debug.valid.eq(rob.commit_req.valids[w]),
                    commit_debug.bits.uop_id.eq(rob.commit_req.uops[w].uop_id),
                ]

            exc_debug = self.core_debug.exc_debug
            m.d.comb += [
                exc_debug.valid.eq(rob.commit_exc.valid),
                exc_debug.bits.cause.eq(rob.commit_exc.bits.cause),
                exc_debug.bits.inst.eq(rob.commit_exc.bits.inst),
            ]

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
                if_stage.redirect_ftq_idx.eq(rob_flush_d1.bits.ftq_idx),
                if_stage.redirect_ghist.br_not_taken.eq(1),
            ]

            flush_pc = if_stage.get_pc[0].pc + rob_flush_d1.bits.pc_lsb - Mux(
                rob_flush_d1.bits.edge_inst, 2, 0)
            with m.Switch(rob_flush_d1.bits.flush_type):
                with m.Case(FlushType.EXCEPT):
                    m.d.comb += if_stage.redirect_pc.eq(exc_unit.exc_vector)
                with m.Case(FlushType.ERET):
                    m.d.comb += if_stage.redirect_pc.eq(evec_d2)
                with m.Case(FlushType.NEXT):
                    m.d.comb += if_stage.redirect_pc.eq(
                        flush_pc + Mux(rob_flush_d1.bits.is_rvc, 2, 4))
                with m.Case(FlushType.REFETCH):
                    m.d.comb += if_stage.redirect_pc.eq(flush_pc)

        with m.Elif(br_update.br_res.mispredict & ~rob_flush_d1.valid):
            uop = br_update.br_res.uop
            uop_pc = if_stage.get_pc[1].pc | uop.pc_lsb
            npc = uop_pc + Mux(uop.is_rvc | uop.edge_inst, 2, 4)
            br_target = uop_pc + br_update.br_res.target_offset - Mux(
                uop.edge_inst, 2, 0)
            bj_addr = Mux(br_update.br_res.cfi_type == CFIType.JALR,
                          br_update.br_res.jalr_target, br_target)
            redirect_target = Mux(br_update.br_res.pc_sel == PCSel.PC_PLUS_4,
                                  npc, bj_addr)

            use_same_ghist = (br_update.br_res.cfi_type
                              == CFIType.BR) & ~br_update.br_res.taken & (
                                  npc[log2_int(self.fetch_bytes):]
                                  == uop_pc[log2_int(self.fetch_bytes):])

            next_ghist = GlobalHistory(self.params)
            cfi_idx = uop.pc_lsb[1:]
            if_stage.get_pc[1].ghist.update(
                m,
                branches=if_stage.get_pc[1].entry.br_mask,
                cfi_taken=br_update.br_res.taken,
                cfi_is_br=(br_update.br_res.cfi_type == CFIType.BR),
                cfi_idx=cfi_idx,
                cfi_valid=1,
                cfi_is_call=if_stage.get_pc[1].entry.cfi_is_call &
                (if_stage.get_pc[1].entry.cfi_idx == cfi_idx),
                cfi_is_ret=if_stage.get_pc[1].entry.cfi_is_ret &
                (if_stage.get_pc[1].entry.cfi_idx == cfi_idx),
                new_ghist=next_ghist)

            m.d.comb += [
                if_stage.redirect_valid.eq(1),
                if_stage.redirect_pc.eq(redirect_target),
                if_stage.redirect_flush.eq(1),
                if_stage.redirect_ftq_idx.eq(uop.ftq_idx),
                if_stage.redirect_ghist.eq(
                    Mux(use_same_ghist, if_stage.get_pc[1].ghist, next_ghist)),
                if_stage.redirect_ghist.br_not_taken.eq(use_same_ghist),
            ]
        with m.Elif(br_update.mispredict_mask != 0):
            m.d.comb += if_stage.redirect_flush.eq(1)

        com_ftq_idx = Signal.like(rob.commit_req.uops[0].ftq_idx)
        for w in range(self.core_width):
            with m.If(rob.commit_req.valids[w]):
                m.d.comb += com_ftq_idx.eq(rob.commit_req.uops[w].ftq_idx)
        m.d.comb += [
            if_stage.commit.eq(
                Mux(rob.commit_exc.valid, rob.commit_exc.bits.ftq_idx,
                    com_ftq_idx)),
            if_stage.commit_valid.eq((rob.commit_req.valids != 0)
                                     | rob.commit_exc.valid),
            if_stage.br_update.eq(br_update),
        ]

        if self.use_fpu:
            m.d.comb += [
                fp_pipeline.br_update.eq(br_update),
                fp_pipeline.flush_pipeline.eq(rob_flush_d1.valid),
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

        m.d.sync += if_stage.sfence.valid.eq(0)
        for w in range(self.mem_width):
            with m.If(lsu.exec_reqs[w].bits.sfence.valid):
                m.d.sync += if_stage.sfence.eq(lsu.exec_reqs[w].bits.sfence)

        if self.sim_debug:
            m.d.comb += self.core_debug.flush_pipeline.eq(rob_flush_d1.valid)

        #
        # Issue queue
        #

        for (typ, qp), duops, dvalids, dready in zip(self.issue_params.items(),
                                                     dispatcher.dis_uops,
                                                     dispatcher.dis_valids,
                                                     dispatcher.iq_ready):
            if typ == IssueQueueType.FP:
                if self.use_fpu:
                    for quop, duop in zip(fp_pipeline.dis_uops, duops):
                        m.d.comb += quop.eq(duop)

                    m.d.comb += [
                        fp_pipeline.dis_valids.eq(dvalids),
                        dready.eq(fp_pipeline.iq_ready),
                    ]
            elif self.use_vector or typ != IssueQueueType.VEC:
                iq = issue_units[typ]

                for quop, duop in zip(iq.dis_uops, duops):
                    m.d.comb += quop.eq(duop)

                m.d.comb += [
                    iq.dis_valids.eq(dvalids),
                    dready.eq(iq.ready),
                    iq.br_update.eq(br_update),
                    iq.flush_pipeline.eq(rob_flush_d1.valid),
                ]

        #
        # Wakeup (issue & rename)
        #

        iqs_mem_int = [
            issue_units[typ]
            for typ in (IssueQueueType.MEM, IssueQueueType.INT)
        ]
        if self.use_vector:
            iqs_mem_int.append(issue_units[IssueQueueType.VEC])

        iss_uops = []
        iss_fu_types = []
        iss_valids = Signal(
            sum([p['issue_width'] for p in self.issue_params.values()]))
        for iq in iqs_mem_int:
            iss_uops.extend(iq.iss_uops)
            iss_fu_types.extend(iq.fu_types)
        m.d.comb += iss_valids.eq(Cat(iq.iss_valids for iq in iqs_mem_int))

        mem_wbarb = m.submodules.mem_wbarb = Arbiter(1 + self.use_fpu,
                                                     ExecResp, self.xlen,
                                                     self.params)

        m.d.comb += lsu.exec_iresps[0].connect(mem_wbarb.inp[0])

        int_iss_wakeups = []
        int_ren_wakeups = []

        arb_wakeup = Valid(ExecResp,
                           self.xlen,
                           self.params,
                           name=f'iss_ren_wakeup0')
        m.d.comb += [
            arb_wakeup.bits.eq(mem_wbarb.out.bits),
            arb_wakeup.valid.eq(
                mem_wbarb.out.fire
                & mem_wbarb.out.bits.uop.rf_wen()
                & (mem_wbarb.out.bits.uop.dst_rtype == RegisterType.FIX)),
        ]

        int_iss_wakeups.append(arb_wakeup)
        int_ren_wakeups.append(arb_wakeup)

        for w in range(1, self.mem_width):
            resp = lsu.exec_iresps[w]

            wakeup = Valid(ExecResp,
                           self.xlen,
                           self.params,
                           name=f'iss_ren_slow_wakeup{w}')
            m.d.comb += [
                wakeup.bits.uop.eq(resp.bits.uop),
                wakeup.valid.eq(resp.valid & resp.bits.uop.rf_wen()
                                &
                                (resp.bits.uop.dst_rtype == RegisterType.FIX)),
            ]

            int_iss_wakeups.append(wakeup)
            int_ren_wakeups.append(wakeup)

        for i, eu in enumerate(exec_units):
            if eu.irf_write:
                resp = eu.iresp

                if eu.can_bypass:
                    fast_wakeup = Valid(
                        ExecResp,
                        self.xlen,
                        self.params,
                        name=f'iss_ren_fast_wakeup{self.mem_width + i}')
                    m.d.comb += [
                        fast_wakeup.bits.uop.eq(iss_uops[i]),
                        fast_wakeup.valid.eq(
                            iss_valids[i]
                            & iss_uops[i].bypassable
                            & (iss_uops[i].dst_rtype == RegisterType.FIX)
                            & iss_uops[i].ldst_valid),
                    ]

                    int_iss_wakeups.append(fast_wakeup)
                    int_ren_wakeups.append(fast_wakeup)

                if not eu.always_bypass:
                    slow_wakeup = Valid(
                        ExecResp,
                        self.xlen,
                        self.params,
                        name=f'iss_ren_slow_wakeup{self.mem_width + i}')
                    m.d.comb += [
                        slow_wakeup.bits.uop.eq(resp.bits.uop),
                        slow_wakeup.valid.eq(
                            resp.valid & resp.bits.uop.rf_wen()
                            & ~resp.bits.uop.bypassable
                            & (resp.bits.uop.dst_rtype == RegisterType.FIX)),
                    ]

                    int_iss_wakeups.append(slow_wakeup)
                    int_ren_wakeups.append(slow_wakeup)

        for rwp, wu in zip(ren_stage.wakeup_ports, int_ren_wakeups):
            m.d.comb += rwp.eq(wu)

        for iu in issue_units.values():
            for iwp, wu in zip(iu.wakeup_ports, int_iss_wakeups):
                m.d.comb += [
                    iwp.valid.eq(wu.valid),
                    iwp.bits.pdst.eq(wu.bits.uop.pdst),
                ]

        if self.use_fpu:
            for rwp, wu in zip(fp_ren_stage.wakeup_ports, fp_pipeline.wakeups):
                m.d.comb += rwp.eq(wu)

        #
        # Register read
        #

        iregfile = m.submodules.iregfile = RegisterFile(
            rports=exec_units.irf_read_ports,
            wports=exec_units.irf_write_ports + self.mem_width,
            num_regs=self.num_int_pregs,
            data_width=self.xlen,
            bypassable_mask=[False] * self.mem_width +
            exec_units.bypassable_mask)

        iregread = m.submodules.iregread = RegisterRead(
            issue_width=sum(
                [p['issue_width'] for p in self.issue_params.values()]),
            num_rports=exec_units.irf_read_ports,
            rports_array=[2] * exec_units.irf_readers,
            num_bypass_ports=exec_units.bypass_ports,
            reg_width=self.xlen,
            params=self.params)

        for irr_uop, iss_uop in zip(iregread.iss_uops, iss_uops):
            m.d.comb += irr_uop.eq(iss_uop)
        m.d.comb += iregread.iss_valids.eq(iss_valids)

        for iss_uop, iss_valid, iss_fu_typ, eu in zip(iss_uops, iss_valids,
                                                      iss_fu_types,
                                                      exec_units):
            m.d.comb += iss_fu_typ.eq(eu.fu_types)

            # Disallow back-to-back integer division
            if eu.has_div:
                div_issued = Signal()
                m.d.sync += div_issued.eq(iss_valid
                                          & (iss_uop.fu_type == FUType.DIV))
                m.d.comb += iss_fu_typ.eq(
                    Mux(div_issued, eu.fu_types & ~FUType.DIV, eu.fu_types))

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
            csr_port.addr.eq(csr_exec_unit.iresp.bits.uop.csr_addr),
            csr_port.cmd.eq(
                csr_exec_unit.iresp.bits.uop.csr_cmd
                & ~Mux(csr_exec_unit.iresp.valid, 0, CSRCommand.I)),
            csr_port.w_data.eq(csr_exec_unit.iresp.bits.data),
        ]

        #
        # Execute
        #

        bypass_idx = 0
        for eu, req in zip([eu for eu in exec_units if eu.irf_read],
                           iregread.exec_reqs):
            m.d.comb += [
                req.connect(eu.req),
                eu.req.bits.kill.eq(rob_flush_d1.valid),
            ]

            if eu.can_bypass:
                for byp in eu.bypass:
                    m.d.comb += iregread.bypass[bypass_idx].eq(byp)
                    bypass_idx += 1

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

        if self.sim_debug:
            for l, r in zip(self.core_debug.ex_debug, exec_units.exec_debug):
                m.d.comb += l.eq(r)

        #
        # Get PC for jump unit
        #

        jmp_pc_req = Signal.like(iss_uops[0].ftq_idx)
        jmp_pc_valid = Signal()

        with m.If(rob.flush.valid):
            m.d.comb += if_stage.get_pc_idx[0].eq(rob.flush.bits.ftq_idx)
        with m.Elif(jmp_pc_valid):
            m.d.comb += if_stage.get_pc_idx[0].eq(jmp_pc_req)
        with m.Elif(dec_stage.get_pc_idx.valid):
            m.d.comb += [
                if_stage.get_pc_idx[0].eq(dec_stage.get_pc_idx.bits),
                dec_stage.get_pc_idx.ready.eq(1),
            ]

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

        m.d.comb += rob.exc_fetch_pc.eq(if_stage.get_pc[0].pc)

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
            lsu.rob_head_idx.eq(rob.head_idx),
            lsu.exception.eq(rob_flush_d1.valid),
            rob.lsu_exc.eq(lsu.lsu_exc),
            lsu.commit_load_at_head.eq(rob.commit_load_at_head),
        ]

        if self.use_fpu:
            m.d.comb += fp_pipeline.to_lsu.connect(lsu.fp_std)

        if self.sim_debug:
            for l, r in zip(self.core_debug.mem_debug, lsu.lsu_debug):
                m.d.comb += l.eq(r)

        #
        # Branch resolution
        #

        resolve_mask = Const(0, self.max_br_count)
        mispredict_mask = Const(0, self.max_br_count)
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
        jalr_target_d1 = Signal(self.vaddr_bits_extended)
        m.d.sync += [
            br_update.br_res.eq(oldest_res),
            br_update.br_res.mispredict.eq(mispredict_val),
            jalr_target_d1.eq(jmp_unit.br_res.jalr_target),
            br_update.br_res.jalr_target.eq(jalr_target_d1),
        ]

        m.d.comb += if_stage.get_pc_idx[1].eq(oldest_res.uop.ftq_idx)

        if self.sim_debug:
            m.d.comb += [
                self.core_debug.branch_resolve.eq(br_update.resolve_mask),
                self.core_debug.branch_mispredict.eq(
                    br_update.mispredict_mask),
            ]

        #
        # Writeback
        #

        m.d.comb += [
            iregfile.write_ports[0].valid.eq(
                mem_wbarb.out.valid & mem_wbarb.out.bits.uop.rf_wen()
                & (mem_wbarb.out.bits.uop.dst_rtype == RegisterType.FIX)),
            iregfile.write_ports[0].bits.addr.eq(mem_wbarb.out.bits.uop.pdst),
            iregfile.write_ports[0].bits.data.eq(mem_wbarb.out.bits.data),
            mem_wbarb.out.ready.eq(iregfile.write_ports[0].ready),
        ]

        if self.sim_debug:
            wb_debug = self.core_debug.wb_debug[0]

            m.d.comb += [
                wb_debug.valid.eq(iregfile.write_ports[0].valid),
                wb_debug.bits.uop_id.eq(mem_wbarb.out.bits.uop.uop_id),
                wb_debug.bits.pdst.eq(mem_wbarb.out.bits.uop.pdst),
                wb_debug.bits.data.eq(iregfile.write_ports[0].bits.data),
            ]

        for i, (wp, iresp) in enumerate(
                zip(iregfile.write_ports[1:self.mem_width],
                    lsu.exec_iresps[1:]), 1):
            m.d.comb += [
                wp.valid.eq(iresp.valid & iresp.bits.uop.rf_wen()
                            & (iresp.bits.uop.dst_rtype == RegisterType.FIX)),
                wp.bits.addr.eq(iresp.bits.uop.pdst),
                wp.bits.data.eq(iresp.bits.data),
            ]

            if self.sim_debug:
                wb_debug = self.core_debug.wb_debug[i]

                m.d.comb += [
                    wb_debug.valid.eq(wp.valid),
                    wb_debug.bits.uop_id.eq(iresp.bits.uop.uop_id),
                    wb_debug.bits.pdst.eq(iresp.bits.uop.pdst),
                    wb_debug.bits.data.eq(wp.bits.data),
                ]

        for i, (eu, wp) in enumerate(
                zip([eu for eu in exec_units if eu.irf_write],
                    iregfile.write_ports[self.mem_width:]), self.mem_width):
            m.d.comb += [
                wp.valid.eq(eu.iresp.valid & eu.iresp.bits.uop.rf_wen()
                            &
                            (eu.iresp.bits.uop.dst_rtype == RegisterType.FIX)),
                wp.bits.addr.eq(eu.iresp.bits.uop.pdst),
            ]

            if eu.has_csr:
                m.d.comb += wp.bits.data.eq(
                    Mux(eu.iresp.bits.uop.csr_cmd != CSRCommand.X,
                        csr_port.r_data, eu.iresp.bits.data))
            else:
                m.d.comb += wp.bits.data.eq(eu.iresp.bits.data)

            if self.sim_debug:
                wb_debug = self.core_debug.wb_debug[i]

                m.d.comb += [
                    wb_debug.valid.eq(wp.valid),
                    wb_debug.bits.uop_id.eq(eu.iresp.bits.uop.uop_id),
                    wb_debug.bits.pdst.eq(eu.iresp.bits.uop.pdst),
                    wb_debug.bits.data.eq(wp.bits.data),
                ]

        if self.use_fpu:
            for wp, fresp in zip(fp_pipeline.mem_wb_ports, lsu.exec_fresps):
                m.d.comb += fresp.connect(wp)

            m.d.comb += fp_pipeline.to_int.connect(mem_wbarb.inp[1])

            ifpu_unit = [eu for eu in exec_units if eu.has_ifpu][0]
            m.d.comb += ifpu_unit.mem_fresp.connect(fp_pipeline.from_int)

        #
        # Commit
        #

        m.d.comb += rob.wb_resps[0].eq(mem_wbarb.out)

        for rob_wb, iresp in zip(rob.wb_resps[1:self.mem_width],
                                 lsu.exec_iresps[1:]):
            m.d.comb += rob_wb.eq(iresp)

        for rob_wb, eu in zip(rob.wb_resps[self.mem_width:],
                              [eu for eu in exec_units if eu.irf_write]):
            m.d.comb += rob_wb.eq(eu.iresp)

        if self.use_fpu:
            for rob_wb, fresp in zip(
                    rob.wb_resps[self.mem_width + exec_units.irf_write_ports:],
                    fp_pipeline.wakeups):
                m.d.comb += rob_wb.eq(fresp)

        for a, b in zip(rob.lsu_clear_busy, lsu.clear_busy):
            m.d.comb += a.eq(b)

        #
        # Exception
        #

        tval_valid = exc_unit.exception & (
            (exc_unit.cause == Cause.BREAKPOINT) |
            (exc_unit.cause == Cause.LOAD_MISALIGNED) |
            (exc_unit.cause == Cause.STORE_MISALIGNED) |
            (exc_unit.cause == Cause.LOAD_ACCESS_FAULT) |
            (exc_unit.cause == Cause.STORE_ACCESS_FAULT) |
            (exc_unit.cause == Cause.FETCH_ACCESS_FAULT) |
            (exc_unit.cause == Cause.LOAD_PAGE_FAULT) |
            (exc_unit.cause == Cause.STORE_PAGE_FAULT) |
            (exc_unit.cause == Cause.FETCH_PAGE_FAULT))

        commit_exc_pc_lsb_d1 = Signal.like(rob.commit_exc.bits.pc_lsb)
        commit_exc_inst_d1 = Signal.like(rob.commit_exc.bits.inst)
        commit_exc_badaddr_d1 = Signal.like(rob.commit_exc.bits.badaddr)
        commit_exc_edge_inst_d1 = Signal()
        m.d.comb += [
            exc_unit.epc.eq(if_stage.get_pc[0].commit_pc +
                            commit_exc_pc_lsb_d1 -
                            Mux(commit_exc_edge_inst_d1, 2, 0)),
            exc_unit.debug_entry.eq(self.debug_entry),
            exc_unit.debug_exception.eq(self.debug_exception),
            exc_unit.system_insn.eq(csr_port.cmd == CSRCommand.I),
            exc_unit.system_insn_imm.eq(csr_port.addr),
            exc_unit.commit.eq(rob.commit_req.valids[0]),
            exc_unit.tval.eq(
                Mux(
                    tval_valid, commit_exc_badaddr_d1,
                    Mux(exc_unit.cause == Cause.ILLEGAL_INSTRUCTION,
                        commit_exc_inst_d1, 0))),
            exc_unit.set_fs_dirty.eq(rob.commit_req.fflags.valid),
        ]

        m.d.sync += [
            commit_exc_pc_lsb_d1.eq(rob.commit_exc.bits.pc_lsb),
            commit_exc_inst_d1.eq(rob.commit_exc.bits.inst),
            commit_exc_badaddr_d1.eq(rob.commit_exc.bits.badaddr),
            commit_exc_edge_inst_d1.eq(rob.commit_exc.bits.edge_inst),
            exc_unit.exception.eq(rob.commit_exc.valid),
            exc_unit.cause.eq(rob.commit_exc.bits.cause),
        ]

        if self.use_vector:
            vec_exec_unit = [eu for eu in exec_units if eu.has_vec][0]
            m.d.comb += [
                vec_exec_unit.rob_head_idx.eq(rob.head_idx),
                vec_exec_unit.rob_pnr_idx.eq(rob.pnr_idx),
                vec_exec_unit.exception.eq(exc_unit.exception),
            ]

            csr.add_csrs(vec_exec_unit.iter_csrs())

        #
        # Virtual memory
        #

        core_mem_users = []

        if self.use_vm:
            ptw_users = [(if_stage.ptw_req, if_stage.ptw_resp),
                         (lsu.ptw_req, lsu.ptw_resp)]
            ptw_arbiter = m.submodules.ptw_arbiter = Arbiter(
                len(ptw_users), PageTableWalker.Request, self.params)
            for (req, _), inp in zip(ptw_users, ptw_arbiter.inp):
                m.d.comb += req.connect(inp)

            ptw_resp_dst = Signal(range(len(ptw_users)))
            with m.If(ptw_arbiter.out.fire):
                m.d.sync += ptw_resp_dst.eq(ptw_arbiter.chosen)

            ptw = m.submodules.ptw = PageTableWalker(self.params)
            csr.add_csrs(ptw.iter_csrs())
            m.d.comb += [
                if_stage.ptbr.eq(ptw.satp.r),
                lsu.ptbr.eq(ptw.satp.r),
                ptw_arbiter.out.connect(ptw.req),
            ]
            core_mem_users.append((ptw.mem_req, ptw.mem_nack, ptw.mem_resp))

            with m.Switch(ptw_resp_dst):
                for i, (_, resp) in enumerate(ptw_users):
                    with m.Case(i):
                        m.d.comb += resp.eq(ptw.resp)

        if self.use_vector:
            core_mem_users.append(
                (vec_exec_unit.vec_mem_req, vec_exec_unit.vec_mem_nack,
                 vec_exec_unit.vec_mem_resp))

        if len(core_mem_users) > 0:
            core_mem_arbiter = m.submodules.core_mem_arbiter = Arbiter(
                len(core_mem_users), CoreMemRequest, self.params)
            for (req, _, _), inp in zip(core_mem_users, core_mem_arbiter.inp):
                m.d.comb += req.connect(inp)
            m.d.comb += core_mem_arbiter.out.connect(lsu.core_req)

            core_mem_grant = Signal(range(len(core_mem_users)))
            with m.If(core_mem_arbiter.out.fire):
                m.d.sync += core_mem_grant.eq(core_mem_arbiter.chosen)

            with m.Switch(core_mem_grant):
                for i, (_, nack, resp) in enumerate(core_mem_users):
                    with m.Case(i):
                        m.d.comb += [
                            nack.eq(lsu.core_nack),
                            resp.eq(lsu.core_resp),
                        ]

        #
        # I-D bus
        #

        if self.core_bus is not None:
            a_arbiter = m.submodules.a_arbiter = tl.Arbiter(
                tl.ChannelA,
                data_width=self.dbus.data_width,
                addr_width=self.dbus.addr_width,
                size_width=self.dbus.size_width,
                source_id_width=self.dbus.source_id_width)

            ibus_a = Decoupled(tl.ChannelA,
                               data_width=self.ibus.data_width,
                               addr_width=self.ibus.addr_width,
                               size_width=self.ibus.size_width,
                               source_id_width=self.ibus.source_id_width)
            m.d.comb += [
                self.ibus.a.connect(ibus_a),
                ibus_a.bits.source.eq(
                    Cat(self.ibus.a.bits.source[:-1], Const(1, 1))),
            ]

            a_arbiter.add(ibus_a)
            a_arbiter.add(self.dbus.a)

            m.d.comb += [
                a_arbiter.bus.connect(self.core_bus.a),
                self.dbus.c.connect(self.core_bus.c),
                self.core_bus.b.connect(self.dbus.b),
                self.dbus.e.connect(self.core_bus.e),
            ]

            m.d.comb += [
                self.ibus.d.valid.eq(self.core_bus.d.valid
                                     & self.core_bus.d.bits.source[-1]),
                self.ibus.d.bits.eq(self.core_bus.d.bits),
                self.dbus.d.valid.eq(self.core_bus.d.valid
                                     & ~self.core_bus.d.bits.source[-1]),
                self.dbus.d.bits.eq(self.core_bus.d.bits),
            ]

            with m.If(~self.core_bus.d.bits.source[-1]):
                m.d.comb += self.core_bus.d.ready.eq(self.dbus.d.ready)
            with m.Else():
                m.d.comb += self.core_bus.d.ready.eq(self.ibus.d.ready)

        return m
