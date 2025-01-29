from amaranth import *
from amaranth import tracer
from amaranth.lib.coding import PriorityEncoder
from amaranth.utils import log2_int

from room.consts import *
from room.rvc import RVCDecoder
from room.id_stage import BranchDecoder, BranchSignals
from room.types import HasCoreParams, MicroOp
from room.breakpoint import Breakpoint, BreakpointMatcher
from room.exc import MStatus
from room.icache import ICache
from room.utils import wrap_incr, sign_extend
from room.mmu import PTBR, PageTableWalker
from room.tlb import TLB, TLBResp, SFenceReq
from room.branch import GlobalHistory, FTQEntry, GetPCResp, BranchUpdate
from room.bpd import BranchPredictor, BranchPredictionUpdate, BranchPredictions

from roomsoc.interconnect.stream import Decoupled, Valid, Queue


class IFDebug(Record):

    def __init__(self, name=None, src_loc_at=0):
        super().__init__([
            ('uop_id', MicroOp.ID_WIDTH),
            ('pc', 64),
            ('inst', 32),
        ],
                         name=name,
                         src_loc_at=1 + src_loc_at)


class FetchBundle(HasCoreParams):

    def __init__(self, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.pc = Signal(self.vaddr_bits_extended, name=f'{name}_pc')
        self.next_pc = Signal(self.vaddr_bits_extended, name=f'{name}_next_pc')
        self.edge_inst = Signal(name=f'{name}_edge_inst')
        self.insts = [
            Signal(32, name=f'{name}_inst{i}') for i in range(self.fetch_width)
        ]
        self.exp_insts = [
            Signal(32, name=f'{name}_exp_inst{i}')
            for i in range(self.fetch_width)
        ]
        self.mask = Signal(self.fetch_width)

        self.cfi_valid = Signal(name=f'{name}_cfi_valid')
        self.cfi_idx = Signal(range(self.fetch_width), name=f'{name}_cfi_idx')
        self.cfi_type = Signal(CFIType, name=f'{name}_cfi_type')
        self.cfi_npc_plus4 = Signal(name=f'{name}__cfi_npc_plus4')
        self.cfi_is_call = Signal(name=f'{name}__cfi_is_call')
        self.cfi_is_ret = Signal(name=f'{name}__cfi_is_ret')

        self.br_mask = Signal(self.fetch_width, name=f'{name}__br_mask')

        self.ftq_idx = Signal(range(self.ftq_size), name=f'{name}_ftq_size')

        self.exc_pf_if = Signal(name=f'{name}_exc_pf_if')
        self.exc_ae_if = Signal(name=f'{name}_exc_ae_if')

        self.bp_exc_if = Signal(self.fetch_width, name=f'{name}_bp_exc_if')
        self.bp_debug_if = Signal(self.fetch_width, name=f'{name}_bp_debug_if')

        self.ghist = GlobalHistory(params)
        self.bpd_meta = Signal(self.bpd_meta_length, name=f'{name}__bpd_meta')

    def eq(self, rhs):
        ret = [
            self.pc.eq(rhs.pc),
            self.next_pc.eq(rhs.next_pc),
            self.edge_inst.eq(rhs.edge_inst),
            self.mask.eq(rhs.mask),
            self.br_mask.eq(rhs.br_mask),
            self.cfi_valid.eq(rhs.cfi_valid),
            self.cfi_idx.eq(rhs.cfi_idx),
            self.cfi_type.eq(rhs.cfi_type),
            self.cfi_npc_plus4.eq(rhs.cfi_npc_plus4),
            self.cfi_is_call.eq(rhs.cfi_is_call),
            self.cfi_is_ret.eq(rhs.cfi_is_ret),
            self.ftq_idx.eq(rhs.ftq_idx),
            self.exc_pf_if.eq(rhs.exc_pf_if),
            self.exc_ae_if.eq(rhs.exc_ae_if),
            self.bp_exc_if.eq(rhs.bp_exc_if),
            self.bp_debug_if.eq(rhs.bp_debug_if),
            self.ghist.eq(rhs.ghist),
            self.bpd_meta.eq(rhs.bpd_meta),
        ]
        for inst, rinst in zip(self.insts, rhs.insts):
            ret.append(inst.eq(rinst))
        for inst, rinst in zip(self.exp_insts, rhs.exp_insts):
            ret.append(inst.eq(rinst))
        return ret


class FetchBuffer(HasCoreParams, Elaboratable):

    def __init__(self, params, sim_debug=False):
        super().__init__(params)

        self.sim_debug = sim_debug

        self.depth = self.fetch_buffer_size

        self.w_data = FetchBundle(params)
        self.r_data = [
            MicroOp(params, name=f'fb_uop{i}') for i in range(self.core_width)
        ]
        self.w_rdy = Signal()
        self.r_rdy = Signal()
        self.w_en = Signal()
        self.r_en = Signal()
        self.flush = Signal()

        if self.sim_debug:
            self.if_debug = [
                Valid(IFDebug, name=f'if_debug{i}')
                for i in range(self.fetch_width)
            ]

    def elaborate(self, platform):
        m = Module()

        num_entries = self.depth
        num_rows = self.depth // self.core_width

        mem = Array(
            MicroOp(self.params, name=f'mem{i}') for i in range(self.depth))

        head = Signal(num_rows, reset=1)
        tail = Signal(num_entries, reset=1)

        maybe_full = Signal()
        do_enq = Signal()
        do_deq = Signal()

        in_uops = [
            MicroOp(self.params, name=f'in_uop{i}')
            for i in range(self.fetch_width)
        ]
        in_mask = Signal(self.fetch_width)

        for w in range(self.fetch_width):
            pc = self.w_data.pc + (w << 1)
            m.d.comb += [
                in_mask[w].eq(self.w_data.mask[w] & self.w_en),
                in_uops[w].ftq_idx.eq(self.w_data.ftq_idx),
                in_uops[w].pc_lsb.eq(pc),
                in_uops[w].inst.eq(self.w_data.exp_insts[w]),
                in_uops[w].is_rvc.eq(self.w_data.insts[w][0:2] != 3),
                in_uops[w].taken.eq((self.w_data.cfi_idx == w)
                                    & self.w_data.cfi_valid),
                in_uops[w].exc_ae_if.eq(self.w_data.exc_ae_if),
                in_uops[w].exc_pf_if.eq(self.w_data.exc_pf_if),
                in_uops[w].bp_exc_if.eq(self.w_data.bp_exc_if[w]),
                in_uops[w].bp_debug_if.eq(self.w_data.bp_debug_if[w]),
            ]

            if w == 0:
                with m.If(self.w_data.edge_inst):
                    m.d.comb += in_uops[w].edge_inst.eq(1)

        if self.sim_debug:
            id_counter = Signal(MicroOp.ID_WIDTH)
            next_id = id_counter

            for w in range(self.fetch_width):
                pc = self.w_data.pc + (w << 1) - Mux(in_uops[w].edge_inst, 2,
                                                     0)
                m.d.comb += [
                    in_uops[w].uop_id.eq(next_id),
                    self.if_debug[w].valid.eq(do_enq & in_mask[w]),
                    self.if_debug[w].bits.uop_id.eq(in_uops[w].uop_id),
                    self.if_debug[w].bits.inst.eq(
                        Mux(in_uops[w].is_rvc, self.w_data.insts[w][:16],
                            self.w_data.insts[w])),
                    self.if_debug[w].bits.pc.eq(
                        sign_extend(pc[:self.vaddr_bits_extended], self.xlen)),
                ]

                next_id = Mux(do_enq & in_mask[w], next_id + 1, next_id)

            m.d.sync += id_counter.eq(next_id)

        wr_slots = [
            Signal(num_entries, name=f'wr_slot{i}')
            for i in range(self.fetch_width)
        ]
        next_slot = tail
        for wr_slot, mask in zip(wr_slots, in_mask):
            m.d.comb += wr_slot.eq(next_slot)
            next_slot = Mux(
                mask,
                Cat(next_slot[num_entries - 1], next_slot[:num_entries - 1]),
                next_slot)

        for w in range(self.fetch_width):
            for s in range(num_entries):
                with m.If(do_enq & in_mask[w] & wr_slots[w][s]):
                    m.d.sync += mem[s].eq(in_uops[w])

        wr_mask = Const(0, num_entries)
        for w in range(1, self.fetch_width):
            wr_mask |= Cat(tail[num_entries - w:num_entries],
                           tail[:num_entries - w])

        might_hit_head = 0
        at_head = 0
        for r in range(num_rows):
            might_hit_head |= head[r] & wr_mask[r * self.core_width]
            at_head |= head[r] & tail[r * self.core_width]

        m.d.comb += [
            do_enq.eq(~(at_head & maybe_full | might_hit_head)),
            self.w_rdy.eq(do_enq),
        ]

        tail_collisions = Array(
            Signal(self.core_width, name=f'tc{i}') for i in range(num_rows))
        for i in range(num_entries):
            m.d.comb += tail_collisions[i // self.core_width][
                i % self.core_width].eq(head[i // self.core_width]
                                        & (~maybe_full
                                           | (i % self.core_width != 0))
                                        & tail[i])
        slot_hit_tail = Signal(self.core_width)
        for i in range(num_rows):
            with m.If(head[i]):
                m.d.comb += slot_hit_tail.eq(tail_collisions[i])
        will_hit_tail = slot_hit_tail != 0

        m.d.comb += do_deq.eq(self.r_en & ~will_hit_tail)

        hit_mask = slot_hit_tail
        for i in range(self.core_width):
            hit_mask |= (hit_mask << 1)

        deq_valids = ~hit_mask[0:self.core_width]

        for i in range(num_rows):
            with m.If(head[i]):
                for w in range(self.core_width):
                    m.d.comb += [
                        self.r_data[w].eq(mem[i * self.core_width + w]),
                        self.r_data[w].valid.eq(deq_valids[w]),
                    ]
        m.d.comb += self.r_rdy.eq(deq_valids != 0)

        with m.If(do_enq):
            m.d.sync += tail.eq(next_slot)
            with m.If(in_mask != 0):
                m.d.sync += maybe_full.eq(1)

        with m.If(do_deq):
            m.d.sync += [
                head.eq(Cat(head[num_rows - 1], head[:num_rows - 1])),
                maybe_full.eq(0),
            ]

        with m.If(self.flush):
            m.d.sync += [
                head.eq(1),
                tail.eq(1),
                maybe_full.eq(0),
            ]

        return m


class FetchTargetQueue(HasCoreParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.w_data = FetchBundle(params)
        self.w_idx = Signal(range(self.ftq_size))
        self.w_en = Signal()
        self.w_rdy = Signal()

        self.deq_idx = Valid(Signal, range(self.ftq_size))

        self.redirect_idx = Valid(Signal, range(self.ftq_size))

        self.get_pc_idx = [
            Signal(range(self.ftq_size), name=f'get_pc_idx{i}')
            for i in range(2)
        ]
        self.get_pc = [GetPCResp(params, name=f'get_pc{i}') for i in range(2)]

        self.br_update = BranchUpdate(params)

        self.bpd_update = Valid(BranchPredictionUpdate, params)

    def elaborate(self, platform):
        m = Module()

        pc_lsb_w = Shape.cast(range(self.fetch_bytes)).width

        w_ptr = Signal(range(self.ftq_size), reset=1)
        deq_ptr = Signal(range(self.ftq_size))
        bpd_ptr = Signal(range(self.ftq_size))

        full = (wrap_incr(wrap_incr(w_ptr, self.ftq_size), self.ftq_size)
                == bpd_ptr) | (wrap_incr(w_ptr, self.ftq_size) == bpd_ptr)

        pcs = Array(
            Signal(self.vaddr_bits_extended - pc_lsb_w, name=f'pc{i}')
            for i in range(self.ftq_size))
        meta = Memory(width=self.bpd_meta_length, depth=self.ftq_size)
        mem = Array(
            FTQEntry(self.params, name=f'entry{i}')
            for i in range(self.ftq_size))
        ghist = [
            Memory(width=len(self.w_data.ghist), depth=self.ftq_size)
            for _ in range(2)
        ]

        prev_entry = FTQEntry(self.params)
        prev_ghist = GlobalHistory(self.params)

        with m.If(self.w_en & self.w_rdy):
            new_entry = FTQEntry(self.params)
            m.d.comb += [
                new_entry.cfi_valid.eq(self.w_data.cfi_valid),
                new_entry.cfi_idx.eq(self.w_data.cfi_idx),
                new_entry.cfi_taken.eq(self.w_data.cfi_valid),
                new_entry.cfi_type.eq(self.w_data.cfi_type),
                new_entry.cfi_npc_plus4.eq(self.w_data.cfi_npc_plus4),
                new_entry.br_mask.eq(self.w_data.br_mask),
            ]

            new_ghist = GlobalHistory(self.params)
            with m.If(self.w_data.ghist.br_not_taken):
                m.d.comb += new_ghist.eq(self.w_data.ghist)
            with m.Else():
                prev_ghist.update(m,
                                  branches=prev_entry.br_mask,
                                  cfi_taken=prev_entry.cfi_taken,
                                  cfi_is_br=(prev_entry.br_mask &
                                             (1 << prev_entry.cfi_idx)).any(),
                                  cfi_idx=prev_entry.cfi_idx,
                                  cfi_valid=prev_entry.cfi_valid,
                                  cfi_is_call=prev_entry.cfi_is_call,
                                  cfi_is_ret=prev_entry.cfi_is_ret,
                                  new_ghist=new_ghist)

            m.d.sync += [
                pcs[w_ptr].eq(self.w_data.pc >> pc_lsb_w),
                mem[w_ptr].eq(new_entry),
                w_ptr.eq(wrap_incr(w_ptr, self.ftq_size)),
            ]

            meta_wport = m.submodules.meta_wport = meta.write_port()
            m.d.comb += [
                meta_wport.addr.eq(w_ptr),
                meta_wport.data.eq(self.w_data.bpd_meta),
                meta_wport.en.eq(1),
            ]

            for i, ghist_mem in enumerate(ghist):
                ghist_wport = ghist_mem.write_port()
                setattr(m.submodules, f'ghist_wport{i}', ghist_wport)
                m.d.comb += [
                    ghist_wport.addr.eq(w_ptr),
                    ghist_wport.data.eq(new_ghist),
                    ghist_wport.en.eq(1),
                ]

            m.d.sync += [
                prev_entry.eq(new_entry),
                prev_ghist.eq(new_ghist),
            ]

        m.d.comb += self.w_idx.eq(w_ptr)

        with m.If(self.deq_idx.valid):
            m.d.sync += deq_ptr.eq(self.deq_idx.bits)

        redirect_valid_d1 = Signal()
        redirect_idx_d1 = Signal.like(self.redirect_idx.bits)
        m.d.sync += [
            redirect_valid_d1.eq(self.redirect_idx.valid),
            redirect_idx_d1.eq(self.redirect_idx.bits),
        ]

        bpd_update_mispredict = Signal()
        bpd_update_mispredict_d1 = Signal()
        bpd_update_repair = Signal()
        bpd_update_repair_d1 = Signal()
        bpd_repair_idx = Signal(range(self.ftq_size))
        bpd_end_idx = Signal(range(self.ftq_size))
        bpd_repair_pc = Signal.like(pcs[0])
        m.d.sync += [
            bpd_update_mispredict_d1.eq(bpd_update_mispredict),
            bpd_update_repair_d1.eq(bpd_update_repair),
        ]

        bpd_idx = Signal(range(self.ftq_size))
        bpd_entry = FTQEntry(self.params)
        bpd_pc = Signal.like(pcs[0])
        m.d.comb += bpd_idx.eq(
            Mux(
                self.redirect_idx.valid, self.redirect_idx.bits,
                Mux(bpd_update_mispredict | bpd_update_repair, bpd_repair_idx,
                    bpd_ptr)))
        m.d.sync += [
            bpd_entry.eq(mem[bpd_idx]),
            bpd_pc.eq(pcs[bpd_idx]),
        ]

        meta_rport = m.submodules.meta_rport = meta.read_port(
            transparent=False)
        bpd_meta = Signal(self.bpd_meta_length)
        m.d.comb += [
            meta_rport.addr.eq(bpd_idx),
            bpd_meta.eq(meta_rport.data),
        ]

        ghist_rport0 = m.submodules.ghist_rport0 = ghist[0].read_port(
            transparent=False)
        bpd_ghist = GlobalHistory(self.params)
        m.d.comb += [
            ghist_rport0.addr.eq(bpd_idx),
            bpd_ghist.eq(ghist_rport0.data),
        ]

        br_res_mispredict_d1 = Signal()
        br_res_ftq_idx_d1 = Signal.like(self.br_update.br_res.uop.ftq_idx)
        w_ptr_d1 = Signal.like(w_ptr)
        m.d.sync += [
            br_res_mispredict_d1.eq(self.br_update.br_res.mispredict),
            br_res_ftq_idx_d1.eq(self.br_update.br_res.uop.ftq_idx),
            w_ptr_d1.eq(w_ptr),
        ]

        with m.If(self.redirect_idx.valid):
            m.d.sync += [
                bpd_update_mispredict.eq(0),
                bpd_update_repair.eq(0),
            ]
        with m.Elif(br_res_mispredict_d1):
            m.d.sync += [
                bpd_update_mispredict.eq(1),
                bpd_repair_idx.eq(br_res_ftq_idx_d1),
                bpd_end_idx.eq(w_ptr_d1),
            ]
        with m.Elif(bpd_update_mispredict):
            m.d.sync += [
                bpd_update_mispredict.eq(0),
                bpd_update_repair.eq(1),
                bpd_repair_idx.eq(wrap_incr(bpd_repair_idx, self.ftq_size))
            ]
        with m.Elif(bpd_update_repair & bpd_update_mispredict_d1):
            m.d.sync += [
                bpd_repair_pc.eq(bpd_pc),
                bpd_repair_idx.eq(wrap_incr(bpd_repair_idx, self.ftq_size))
            ]
        with m.Elif(bpd_update_repair):
            m.d.sync += bpd_repair_idx.eq(
                wrap_incr(bpd_repair_idx, self.ftq_size))
            with m.If((wrap_incr(bpd_repair_idx, self.ftq_size) == bpd_end_idx)
                      | (bpd_pc == bpd_repair_pc)):
                m.d.sync += bpd_update_repair.eq(0)

        do_commit_update = Signal()
        m.d.comb += do_commit_update.eq(
            ~bpd_update_mispredict
            & ~bpd_update_repair
            & (bpd_ptr != deq_ptr)
            & (wrap_incr(bpd_ptr, self.ftq_size) != w_ptr)
            & ~self.br_update.br_res.mispredict
            & ~self.redirect_idx.valid & ~redirect_valid_d1)

        with m.If(do_commit_update):
            m.d.sync += bpd_ptr.eq(wrap_incr(bpd_ptr, self.ftq_size))

        first_empty = Signal(reset=1)
        do_bpd_update = Signal()
        m.d.sync += do_bpd_update.eq(do_commit_update | bpd_update_mispredict
                                     | bpd_update_repair)

        with m.If(do_bpd_update):
            cfi_mask = Signal(self.fetch_width)
            with m.Switch(bpd_entry.cfi_idx):
                for w in range(self.fetch_width):
                    with m.Case(w):
                        m.d.comb += [
                            cfi_mask.eq((1 << (w + 1)) - 1),
                            self.bpd_update.bits.cfi_is_br.eq(
                                bpd_entry.br_mask[w]),
                        ]

            m.d.comb += [
                self.bpd_update.valid.eq(
                    ~first_empty
                    & (bpd_entry.cfi_valid | bpd_entry.br_mask.any())
                    & ~(bpd_update_repair_d1 & (bpd_pc == bpd_repair_pc))),
                self.bpd_update.bits.is_mispredict_update.eq(
                    bpd_update_mispredict_d1),
                self.bpd_update.bits.is_repair_update.eq(bpd_update_repair_d1),
                self.bpd_update.bits.pc.eq(bpd_pc << pc_lsb_w),
                self.bpd_update.bits.br_mask.eq(
                    Mux(bpd_entry.cfi_valid, cfi_mask & bpd_entry.br_mask,
                        bpd_entry.br_mask)),
                self.bpd_update.bits.cfi_idx.valid.eq(bpd_entry.cfi_valid),
                self.bpd_update.bits.cfi_idx.bits.eq(bpd_entry.cfi_idx),
                self.bpd_update.bits.cfi_mispredicted.eq(
                    bpd_entry.cfi_mispredicted),
                self.bpd_update.bits.cfi_taken.eq(bpd_entry.cfi_taken),
                self.bpd_update.bits.cfi_is_jal.eq(
                    (bpd_entry.cfi_type == CFIType.JAL)
                    | (bpd_entry.cfi_type == CFIType.JALR)),
                self.bpd_update.bits.ghist.eq(bpd_ghist.history),
                self.bpd_update.bits.meta.eq(bpd_meta),
            ]

            m.d.sync += first_empty.eq(0)

        m.d.comb += self.w_rdy.eq(~full | do_commit_update)

        redirect_new_entry = FTQEntry(self.params)
        m.d.sync += redirect_new_entry.eq(mem[self.redirect_idx.bits])

        with m.If(self.redirect_idx.valid):
            m.d.sync += w_ptr.eq(
                wrap_incr(self.redirect_idx.bits, self.ftq_size))

            with m.If(self.br_update.br_res.mispredict):
                m.d.sync += [
                    redirect_new_entry.cfi_valid.eq(1),
                    redirect_new_entry.cfi_idx.eq(
                        self.br_update.br_res.uop.pc_lsb >> 1),
                    redirect_new_entry.cfi_mispredicted.eq(1),
                    redirect_new_entry.cfi_taken.eq(
                        self.br_update.br_res.taken),
                ]

        with m.Elif(redirect_valid_d1):
            m.d.sync += [
                prev_entry.eq(redirect_new_entry),
                prev_ghist.eq(bpd_ghist),
                mem[redirect_idx_d1].eq(redirect_new_entry),
            ]

        ghist_rport1 = m.submodules.ghist_rport1 = ghist[1].read_port(
            transparent=False)

        for i, (get_pc_idx,
                get_pc) in enumerate(zip(self.get_pc_idx, self.get_pc)):
            next_idx = wrap_incr(get_pc_idx, self.ftq_size)
            next_is_w = (next_idx == w_ptr) & self.w_en & self.w_rdy

            if i == 1:
                m.d.comb += [
                    ghist_rport1.addr.eq(get_pc_idx),
                    get_pc.ghist.eq(ghist_rport1.data),
                ]

            m.d.sync += [
                get_pc.pc.eq(pcs[get_pc_idx] << pc_lsb_w),
                get_pc.commit_pc.eq(pcs[Mux(self.deq_idx.valid, self.deq_idx.
                                            bits, deq_ptr)] << pc_lsb_w),
                get_pc.next_valid.eq((next_idx != w_ptr) | next_is_w),
                get_pc.next_pc.eq(
                    Mux(next_is_w, self.w_data.pc, pcs[next_idx] << pc_lsb_w)),
            ]

        return m


class IFStage(HasCoreParams, Elaboratable):

    def __init__(self, ibus, params, sim_debug=False):
        super().__init__(params)

        self.reset_vector = Signal(self.vaddr_bits_extended)

        self.ibus = ibus
        self.enable_icache = params.get('icache_params') is not None
        self.sim_debug = sim_debug

        self.fetch_addr_shift = Shape.cast(range(self.fetch_bytes)).width

        if not self.enable_icache:
            assert self.fetch_bytes * 8 == self.ibus.data_width

        self.fetch_packet = [
            MicroOp(params, name=f'fetch_uop{i}')
            for i in range(self.core_width)
        ]
        self.fetch_packet_valid = Signal()
        self.fetch_packet_ready = Signal()

        self.commit = Signal(range(self.ftq_size))
        self.commit_valid = Signal()

        self.get_pc_idx = [
            Signal(self.ftq_size, name=f'get_pc_idx{i}') for i in range(2)
        ]
        self.get_pc = [GetPCResp(params, name=f'get_pc{i}') for i in range(2)]

        self.br_update = BranchUpdate(params)

        self.redirect_valid = Signal()
        self.redirect_pc = Signal(self.vaddr_bits_extended)
        self.redirect_flush = Signal()
        self.redirect_ftq_idx = Signal(range(self.ftq_size))
        self.redirect_ghist = GlobalHistory(self.params)

        self.flush_icache = Signal()

        self.bp = [
            Breakpoint(self.params, name=f'bp{i}')
            for i in range(self.num_breakpoints)
        ]

        self.prv = Signal(PrivilegeMode)
        self.status = MStatus(self.xlen)
        self.ptbr = PTBR(self.xlen)

        self.ptw_req = Decoupled(PageTableWalker.Request, params)
        self.ptw_resp = Valid(PageTableWalker.Response, params)

        self.sfence = Valid(SFenceReq, params)

        if self.sim_debug:
            self.if_debug = [
                Valid(IFDebug, name=f'if_debug{i}')
                for i in range(self.fetch_width)
            ]

    def elaborate(self, platform):
        m = Module()

        def fetch_mask(addr):
            off = (addr >> 1)[0:Shape.cast(range(self.fetch_width)).width]
            return (((1 << self.fetch_width) - 1) << off)[0:self.fetch_width]

        def fetch_align(addr):
            lsb_width = Shape.cast(range(self.fetch_bytes)).width
            return Cat(Const(0, lsb_width), addr[lsb_width:])

        def next_fetch(addr):
            return fetch_align(addr) + self.fetch_width * 2

        if self.enable_icache:
            icache = m.submodules.icache = ICache(self.ibus, self.params)
            m.d.comb += icache.invalidate.eq(self.flush_icache)

            icache_params = self.params['icache_params']
            n_tlb_sets = icache_params['n_tlb_sets']
            n_tlb_ways = icache_params['n_tlb_ways']
            tlb = m.submodules.tlb = TLB(req_width=1,
                                         params=self.params,
                                         log_max_size=log2_int(
                                             self.fetch_bytes),
                                         n_sets=n_tlb_sets,
                                         n_ways=n_tlb_ways,
                                         n_banks=1)
            m.d.comb += [
                tlb.ptw_req.connect(self.ptw_req),
                tlb.ptw_resp.eq(self.ptw_resp),
            ]

        bpd = m.submodules.bpd = BranchPredictor(self.params)

        #
        # F0 - Next PC select
        #

        s0_vpc = Signal(self.vaddr_bits_extended)
        s0_valid = Signal()
        s0_ghist = GlobalHistory(self.params)
        s0_is_replay = Signal()
        s0_replay_ppc = Signal(self.paddr_bits)
        s0_replay_resp = TLBResp(self.params)

        reset = Signal(reset=1)
        reset_d1 = Signal(reset_less=True)
        m.d.sync += [
            reset.eq(0),
            reset_d1.eq(reset),
        ]

        with m.If(reset_d1 & ~reset):
            m.d.comb += [
                s0_vpc.eq(self.reset_vector),
                s0_valid.eq(1),
            ]

        if self.enable_icache:
            m.d.comb += [
                icache.req.bits.addr.eq(s0_vpc),
                icache.req.valid.eq(s0_valid),
                tlb.prv.eq(self.prv),
                tlb.status.eq(self.status),
                tlb.ptbr.eq(self.ptbr),
                tlb.req[0].valid.eq(s0_valid),
                tlb.req[0].bits.vaddr.eq(s0_vpc),
                tlb.req[0].bits.size.eq(log2_int(self.fetch_bytes)),
                tlb.sfence.eq(self.sfence),
            ]

        m.d.comb += [
            bpd.f0_req.valid.eq(s0_valid),
            bpd.f0_req.bits.pc.eq(s0_vpc),
            bpd.f0_req.bits.ghist.eq(s0_ghist),
        ]

        #
        # F1 - ICache Access
        #

        s1_vpc = Signal.like(s0_vpc)
        s1_valid = Signal()
        s1_ghist = GlobalHistory(self.params)
        s1_is_replay = Signal()
        s1_replay_ppc = Signal.like(s0_replay_ppc)
        s1_replay_resp = TLBResp(self.params)
        s1_tlb_resp = TLBResp(self.params)
        f1_clear = Signal()

        m.d.sync += [
            s1_vpc.eq(s0_vpc),
            s1_valid.eq(s0_valid),
            s1_ghist.eq(s0_ghist),
            s1_is_replay.eq(s0_is_replay),
            s1_replay_ppc.eq(s0_replay_ppc),
            s1_replay_resp.eq(s0_replay_resp),
        ]

        s1_ppc = Signal(self.paddr_bits)

        if self.enable_icache:
            s1_tlb_miss = ~s1_is_replay & tlb.resp[0].bits.miss

            m.d.comb += [
                s1_ppc.eq(
                    Mux(s1_is_replay, s1_replay_ppc, tlb.resp[0].bits.paddr)),
                s1_tlb_resp.eq(
                    Mux(s1_is_replay, s1_replay_resp, tlb.resp[0].bits)),
                icache.s1_paddr.eq(s1_ppc),
                icache.s1_kill.eq(tlb.resp[0].bits.miss | f1_clear),
            ]

            f1_predicted_target = Signal(self.vaddr_bits_extended)
            m.d.comb += f1_predicted_target.eq(next_fetch(s1_vpc))

            with m.If(s1_valid & ~s1_tlb_miss):
                m.d.comb += [
                    s0_valid.eq(~(s1_tlb_resp.ae.inst | s1_tlb_resp.pf.inst)),
                    s0_vpc.eq(f1_predicted_target),
                    s0_ghist.eq(s1_ghist),
                ]

        #
        # F2 - ICache Response
        #

        s2_vpc = Signal.like(s1_vpc)
        s2_valid = Signal()
        s2_ppc = Signal.like(s1_ppc)
        s2_ghist = GlobalHistory(self.params)
        s2_is_replay_reg = Signal()
        s2_is_replay = s2_is_replay_reg & s2_valid
        s2_tlb_resp = TLBResp(self.params)
        s2_tlb_miss = Signal()
        s2_exception = s2_valid & (s2_tlb_resp.ae.inst
                                   | s2_tlb_resp.pf.inst) & ~s2_is_replay
        f2_clear = Signal()
        f3_ready = Signal()

        if self.enable_icache:
            m.d.comb += icache.s2_kill.eq(s2_exception)
            m.d.sync += [
                s2_vpc.eq(s1_vpc),
                s2_valid.eq(s1_valid & ~f1_clear),
                s2_ppc.eq(s1_ppc),
                s2_ghist.eq(s1_ghist),
                s2_is_replay_reg.eq(s1_is_replay),
                s2_tlb_resp.eq(s1_tlb_resp),
                s2_tlb_miss.eq(s1_tlb_miss),
            ]

            f2_predicted_target = Signal(self.vaddr_bits_extended)
            m.d.comb += f2_predicted_target.eq(next_fetch(s2_vpc))

            with m.If((s2_valid & ~icache.resp.valid)
                      | (s2_valid & icache.resp.valid & ~f3_ready)):
                m.d.comb += [
                    s0_valid.eq(~(s2_tlb_resp.ae.inst | s2_tlb_resp.pf.inst)
                                | s2_is_replay | s2_tlb_miss),
                    s0_vpc.eq(s2_vpc),
                    s0_ghist.eq(s2_ghist),
                    s0_is_replay.eq(s2_valid & icache.resp.valid),
                    f1_clear.eq(1),
                ]

            with m.Elif(s2_valid & f3_ready):
                with m.If((s1_valid & (s1_vpc != f2_predicted_target))
                          | ~s1_valid):
                    m.d.comb += [
                        f1_clear.eq(1),
                        s0_valid.eq(~(
                            (s2_tlb_resp.ae.inst | s2_tlb_resp.pf.inst)
                            & ~s2_is_replay)),
                        s0_vpc.eq(f2_predicted_target),
                        s0_ghist.eq(s2_ghist),
                    ]

            m.d.comb += [
                s0_replay_resp.eq(s2_tlb_resp),
                s0_replay_ppc.eq(s2_ppc),
            ]

        else:
            ibus_addr = Mux(s1_valid & ~f1_clear, s1_vpc, s2_vpc)
            m.d.comb += [
                self.ibus.adr.eq(ibus_addr[self.fetch_addr_shift:]),
                self.ibus.cyc.eq((s1_valid & ~f1_clear)
                                 | (s2_valid & ~f2_clear)),
                self.ibus.stb.eq(self.ibus.cyc),
                self.ibus.sel.eq(~0),
            ]

            m.d.sync += s2_vpc.eq(Mux(s1_valid & ~f1_clear, s1_vpc, s2_vpc))

            with m.If(s1_valid & ~f1_clear):
                m.d.sync += s2_valid.eq(1)
            with m.Elif((self.ibus.ack & f3_ready) | f2_clear):
                m.d.sync += s2_valid.eq(0)

        #
        # F3 - Pre-decoding
        #

        f3_clear = Signal()

        f3_pipe_reg = Signal(self.fetch_bytes * 8 + 2 + len(s2_vpc) +
                             len(s2_ghist))
        f3_empty = Signal(reset=1)

        f4_ready = Signal()

        if self.enable_icache:
            f3_w_en = s2_valid & ~f2_clear & (icache.resp.valid | (
                (s2_tlb_resp.ae.inst | s2_tlb_resp.pf.inst) & ~s2_tlb_miss))
            f3_w_data = Cat(icache.resp.bits.data, s2_tlb_resp.ae.inst,
                            s2_tlb_resp.pf.inst, s2_vpc, s2_ghist)
        else:
            f3_w_en = s2_valid & ~f2_clear & self.ibus.ack
            f3_w_data = Cat(self.ibus.dat_r, Const(0, 2), s2_vpc,
                            Const(0, len(s2_ghist)))

        m.d.comb += f3_ready.eq(f3_empty)

        with m.If(f4_ready):
            m.d.comb += f3_ready.eq(1)
            m.d.sync += f3_empty.eq(1)

        with m.If(f3_w_en & f3_ready):
            m.d.sync += [
                f3_empty.eq(0),
                f3_pipe_reg.eq(f3_w_data),
            ]

        with m.If(f3_clear):
            m.d.sync += f3_empty.eq(1)

        f3_was_ready = Signal()
        m.d.sync += f3_was_ready.eq(f3_ready)

        f3_bpd_resp = m.submodules.f3_bpd_resp = Queue(1,
                                                       BranchPredictions,
                                                       self.params,
                                                       flow=True,
                                                       pipe=True,
                                                       has_flush=True)
        m.d.comb += [
            f3_bpd_resp.enq.valid.eq(~f3_empty & f3_was_ready),
            f3_bpd_resp.enq.bits.eq(bpd.f3_resp),
            f3_bpd_resp.deq.ready.eq(f4_ready),
            f3_bpd_resp.flush.eq(f3_clear),
        ]

        s3_valid = Signal()
        s3_pc = Signal.like(s2_vpc)
        s3_data = Signal(self.fetch_bytes * 8)
        s3_exc_ae = Signal()
        s3_exc_pf = Signal()
        s3_ghist = GlobalHistory(self.params)
        assert len(Cat(s3_data, s3_exc_ae, s3_exc_pf, s3_pc,
                       s3_ghist)) == len(f3_pipe_reg)
        m.d.comb += [
            s3_valid.eq(~f3_empty),
            Cat(s3_data, s3_exc_ae, s3_exc_pf, s3_pc,
                s3_ghist).eq(f3_pipe_reg),
        ]

        f3_prev_half = Signal(16)
        f3_prev_half_valid = Signal()

        f3_imemresp_mask = Signal(self.fetch_width)
        f3_aligned_pc = fetch_align(s3_pc)
        f3_insts = [Signal(32) for _ in range(self.fetch_width)]
        f3_exp_insts = [Signal(32) for _ in range(self.fetch_width)]
        f3_mask = Signal(self.fetch_width)
        f3_br_mask = Signal(self.fetch_width)
        f3_is_rvc = Signal(self.fetch_width)
        f3_redirects = Signal(self.fetch_width)
        f3_targets = Array(
            Signal(self.vaddr_bits_extended, name=f'f3_target{i}')
            for i in range(self.fetch_width))
        f3_plus4_mask = Signal(self.fetch_width)
        f3_cfi_types = Array(
            Signal(CFIType, name=f'f3_cfi_type{i}')
            for i in range(self.fetch_width))
        f3_fetch_bundle = FetchBundle(self.params, name='f3_fetch_bundle')

        m.d.comb += f3_imemresp_mask.eq(fetch_mask(s3_pc))

        for binst, inst in zip(f3_fetch_bundle.insts, f3_insts):
            m.d.comb += binst.eq(inst)
        for binst, inst in zip(f3_fetch_bundle.exp_insts, f3_exp_insts):
            m.d.comb += binst.eq(inst)
        m.d.comb += [
            f3_fetch_bundle.mask.eq(f3_mask),
            f3_fetch_bundle.br_mask.eq(f3_br_mask),
            f3_fetch_bundle.pc.eq(f3_aligned_pc),
            f3_fetch_bundle.exc_ae_if.eq(s3_exc_ae),
            f3_fetch_bundle.exc_pf_if.eq(s3_exc_pf),
        ]

        redirects_found = 0
        for w in range(self.fetch_width):
            valid = Signal()

            bp_matcher = BreakpointMatcher(self.params)
            m.submodules += bp_matcher
            for m_bp, bp in zip(bp_matcher.bp, self.bp):
                m.d.comb += m_bp.eq(bp)

            br_sigs = BranchSignals(vaddr_bits=self.vaddr_bits_extended,
                                    name=f'br_sigs{w}')

            if w == 0:
                inst0 = Cat(f3_prev_half, s3_data[0:16])
                inst1 = s3_data[0:32]
                dec0 = RVCDecoder(self.xlen)
                dec1 = RVCDecoder(self.xlen)
                br_dec0 = BranchDecoder(self.vaddr_bits_extended)
                br_dec1 = BranchDecoder(self.vaddr_bits_extended)
                m.submodules += [dec0, dec1, br_dec0, br_dec1]

                pc0 = f3_aligned_pc - 2
                pc1 = f3_aligned_pc

                m.d.comb += [
                    dec0.instr_i.eq(inst0),
                    dec1.instr_i.eq(inst1),
                    br_dec0.inst.eq(dec0.instr_o),
                    br_dec0.pc.eq(pc0),
                    br_dec1.inst.eq(dec1.instr_o),
                    br_dec1.pc.eq(pc1),
                ]

                with m.If(f3_prev_half_valid):
                    m.d.comb += [
                        f3_insts[w].eq(inst0),
                        f3_exp_insts[w].eq(dec0.instr_o),
                        bp_matcher.pc.eq(pc0),
                        f3_fetch_bundle.edge_inst.eq(1),
                        br_sigs.eq(br_dec0.out),
                    ]
                with m.Else():
                    m.d.comb += [
                        f3_insts[w].eq(inst1),
                        f3_exp_insts[w].eq(dec1.instr_o),
                        bp_matcher.pc.eq(pc1),
                        br_sigs.eq(br_dec1.out),
                    ]

                m.d.comb += [valid.eq(1)]

            else:
                inst = Signal(32)
                pc = f3_aligned_pc + (w * 2)
                dec = RVCDecoder(self.xlen)
                br_dec = BranchDecoder(self.vaddr_bits_extended)
                m.submodules += [dec, br_dec]

                m.d.comb += [
                    f3_insts[w].eq(inst),
                    dec.instr_i.eq(inst),
                    br_dec.inst.eq(dec.instr_o),
                    br_dec.pc.eq(pc),
                    f3_exp_insts[w].eq(dec.instr_o),
                    bp_matcher.pc.eq(pc),
                    br_sigs.eq(br_dec.out),
                ]

                if w == 1:
                    m.d.comb += [
                        inst.eq(s3_data[16:48]),
                        # insts[0] is not a 32-bit instruction
                        valid.eq(f3_prev_half_valid
                                 | ~(f3_mask[w - 1] &
                                     (f3_insts[w - 1][0:2] == 3))),
                    ]
                elif w == self.fetch_width - 1:
                    m.d.comb += [
                        inst.eq(Cat(s3_data[-16:], Const(0, 16))),
                        # Last instruction is not a 32-bit instruction and this is a RVC instruction
                        valid.eq(~((f3_mask[w - 1]
                                    & (f3_insts[w - 1][0:2] == 3))
                                   | (f3_insts[w][0:2] == 3))),
                    ]
                else:
                    m.d.comb += [
                        inst.eq(s3_data[w * 16:w * 16 + 32]),
                        # Last instruction is not a 32-bit instruction
                        valid.eq(~(f3_mask[w - 1]
                                   & (f3_insts[w - 1][0:2] == 3)))
                    ]

            m.d.comb += [
                f3_is_rvc[w].eq(f3_insts[w][0:2] != 3),
                f3_mask[w].eq(s3_valid & valid & f3_imemresp_mask[w]
                              & ~redirects_found),
                f3_targets[w].eq(br_sigs.target),
            ]

            if w == 0:
                m.d.comb += f3_plus4_mask[w].eq(
                    ((f3_insts[w][0:2] == 3) & ~f3_prev_half_valid))
            else:
                m.d.comb += f3_plus4_mask[w].eq((f3_insts[w][0:2] == 3))

            m.d.comb += f3_redirects[w].eq(f3_mask[w] & (
                (br_sigs.cfi_type == CFIType.JAL)
                | (br_sigs.cfi_type == CFIType.JALR)
                | (self.use_bpd & (br_sigs.cfi_type == CFIType.BR)
                   & f3_bpd_resp.deq.bits.preds[w].taken)))

            m.d.comb += [
                f3_cfi_types[w].eq(br_sigs.cfi_type),
                f3_br_mask[w].eq(f3_mask[w]
                                 & (br_sigs.cfi_type == CFIType.BR)),
            ]

            m.d.comb += [
                f3_fetch_bundle.bp_exc_if[w].eq(bp_matcher.exc_if),
                f3_fetch_bundle.bp_debug_if[w].eq(bp_matcher.debug_if),
            ]

            redirects_found |= f3_redirects[w]

        last_inst = f3_insts[self.fetch_width - 1][0:16]
        with m.If(s3_valid & f4_ready):
            m.d.sync += [
                f3_prev_half.eq(last_inst),
                f3_prev_half_valid.eq(~(f3_mask[self.fetch_width - 2]
                                        & ~f3_is_rvc[self.fetch_width - 2])
                                      & (last_inst[0:2] == 3)),
            ]
        with m.Elif(f3_clear):
            m.d.sync += f3_prev_half_valid.eq(0)

        f3_prio_enc = m.submodules.f3_prio_enc = PriorityEncoder(
            self.fetch_width)
        m.d.comb += [
            f3_prio_enc.i.eq(f3_redirects),
            f3_fetch_bundle.cfi_type.eq(f3_cfi_types[f3_fetch_bundle.cfi_idx]),
            f3_fetch_bundle.cfi_valid.eq(~f3_prio_enc.n),
            f3_fetch_bundle.cfi_idx.eq(f3_prio_enc.o),
        ]
        with m.Switch(f3_fetch_bundle.cfi_idx):
            for w in range(self.fetch_width):
                with m.Case(w):
                    m.d.comb += f3_fetch_bundle.cfi_npc_plus4.eq(
                        f3_plus4_mask[w])

        m.d.comb += [
            f3_fetch_bundle.ghist.eq(s3_ghist),
            f3_fetch_bundle.bpd_meta.eq(f3_bpd_resp.deq.bits.meta),
        ]

        f3_predicted_target = Signal(self.vaddr_bits_extended)
        m.d.comb += [
            f3_predicted_target.eq(
                Mux(
                    f3_fetch_bundle.cfi_valid &
                    (f3_fetch_bundle.cfi_type != CFIType.JALR),
                    f3_targets[f3_fetch_bundle.cfi_idx],
                    next_fetch(f3_fetch_bundle.pc))),
            f3_fetch_bundle.next_pc.eq(f3_predicted_target),
        ]

        f3_predicted_ghist = GlobalHistory(self.params)
        f3_fetch_bundle.ghist.update(
            m,
            branches=f3_fetch_bundle.br_mask,
            cfi_taken=f3_fetch_bundle.cfi_valid,
            cfi_is_br=(f3_fetch_bundle.br_mask &
                       (1 << f3_fetch_bundle.cfi_idx)).any(),
            cfi_idx=f3_fetch_bundle.cfi_idx,
            cfi_valid=f3_fetch_bundle.cfi_valid,
            cfi_is_call=f3_fetch_bundle.cfi_is_call,
            cfi_is_ret=f3_fetch_bundle.cfi_is_ret,
            new_ghist=f3_predicted_ghist)

        with m.If(s3_valid & f4_ready):
            with m.If(f3_fetch_bundle.cfi_valid):
                m.d.sync += f3_prev_half_valid.eq(0)

            with m.If((s2_valid & (s2_vpc != f3_predicted_target))
                      | (~s2_valid & s1_valid
                         & (s1_vpc != f3_predicted_target))
                      | (~s2_valid & ~s1_valid)):
                m.d.comb += [
                    f1_clear.eq(s1_valid),
                    f2_clear.eq(s2_valid),
                    s0_valid.eq(~(f3_fetch_bundle.exc_ae_if
                                  | f3_fetch_bundle.exc_pf_if)),
                    s0_vpc.eq(f3_predicted_target),
                    s0_ghist.eq(f3_predicted_ghist),
                    s0_is_replay.eq(0),
                ]

        #
        # F4 - Fetch Buffer
        #

        f4_clear = Signal()
        fb = m.submodules.fb = FetchBuffer(self.params,
                                           sim_debug=self.sim_debug)
        ftq = m.submodules.ftq = FetchTargetQueue(self.params)

        m.d.comb += [
            f4_ready.eq(fb.w_rdy & ftq.w_rdy),
            fb.w_data.eq(f3_fetch_bundle),
            fb.w_data.ftq_idx.eq(ftq.w_idx),
            fb.w_en.eq(s3_valid & ftq.w_rdy & ~f3_clear),
            fb.r_en.eq(self.fetch_packet_ready),
            self.fetch_packet_valid.eq(fb.r_rdy),
            ftq.w_data.eq(f3_fetch_bundle),
            ftq.w_en.eq(s3_valid & fb.w_rdy & ~f3_clear),
        ]

        for fp, rd in zip(self.fetch_packet, fb.r_data):
            m.d.comb += fp.eq(rd)

        m.d.comb += [
            ftq.deq_idx.bits.eq(self.commit),
            ftq.deq_idx.valid.eq(self.commit_valid),
            ftq.br_update.eq(self.br_update),
            bpd.update.eq(ftq.bpd_update),
        ]

        for a, b in zip(ftq.get_pc_idx, self.get_pc_idx):
            m.d.comb += a.eq(b)
        for a, b in zip(self.get_pc, ftq.get_pc):
            m.d.comb += a.eq(b)

        #
        # Redirect
        #

        with m.If(self.sfence.valid):
            m.d.comb += [
                f1_clear.eq(1),
                f2_clear.eq(1),
                f3_clear.eq(1),
                f4_clear.eq(1),
                fb.flush.eq(1),
                s0_valid.eq(0),
                s0_is_replay.eq(0),
            ]
        with m.Elif(self.redirect_flush):
            m.d.comb += [
                f1_clear.eq(1),
                f2_clear.eq(1),
                f3_clear.eq(1),
                f4_clear.eq(1),
                fb.flush.eq(1),
                s0_valid.eq(self.redirect_valid),
                s0_vpc.eq(self.redirect_pc),
                s0_ghist.eq(self.redirect_ghist),
                s0_is_replay.eq(0),
                ftq.redirect_idx.valid.eq(self.redirect_valid),
                ftq.redirect_idx.bits.eq(self.redirect_ftq_idx),
            ]
            m.d.sync += f3_prev_half_valid.eq(0)

        #
        # Debug signals
        #

        if self.sim_debug:
            for l, r in zip(self.if_debug, fb.if_debug):
                m.d.comb += l.eq(r)

        return m
