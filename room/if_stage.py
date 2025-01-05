from amaranth import *
from amaranth import tracer
from amaranth.lib.coding import PriorityEncoder
from amaranth.utils import log2_int

from room.consts import *
from room.rvc import RVCDecoder
from room.id_stage import BranchDecoder
from room.types import HasCoreParams, MicroOp
from room.fu import GetPCResp
from room.breakpoint import Breakpoint, BreakpointMatcher
from room.exc import MStatus
from room.icache import ICache
from room.utils import wrap_incr
from room.mmu import PTBR, PageTableWalker
from room.tlb import TLB, TLBResp, SFenceReq

from roomsoc.interconnect.stream import Decoupled, Valid


class IFDebug(Record):

    def __init__(self, name=None, src_loc_at=0):
        super().__init__([
            ('uop_id', MicroOp.ID_WIDTH),
            ('pc', 32),
            ('inst', 32),
        ],
                         name=name,
                         src_loc_at=1 + src_loc_at)


class FetchBundle(HasCoreParams):

    def __init__(self, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.pc = Signal(32, name=f'{name}_pc')
        self.next_pc = Signal(32, name=f'{name}_next_pc')
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

        self.ftq_idx = Signal(range(self.ftq_size), name=f'{name}_ftq_size')

        self.exc_pf_if = Signal(name=f'{name}_exc_pf_if')
        self.exc_ae_if = Signal(name=f'{name}_exc_ae_if')

        self.bp_exc_if = Signal(self.fetch_width, name=f'{name}_bp_exc_if')
        self.bp_debug_if = Signal(self.fetch_width, name=f'{name}_bp_debug_if')

    def eq(self, rhs):
        ret = [
            self.pc.eq(rhs.pc),
            self.next_pc.eq(rhs.next_pc),
            self.mask.eq(rhs.mask),
            self.cfi_valid.eq(rhs.cfi_valid),
            self.cfi_idx.eq(rhs.cfi_idx),
            self.cfi_type.eq(rhs.cfi_type),
            self.ftq_idx.eq(rhs.ftq_idx),
            self.exc_pf_if.eq(rhs.exc_pf_if),
            self.exc_ae_if.eq(rhs.exc_ae_if),
            self.bp_exc_if.eq(rhs.bp_exc_if),
            self.bp_debug_if.eq(rhs.bp_debug_if),
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
                in_uops[w].is_rvc.eq(in_uops[w].inst[0:2] != 3),
                in_uops[w].taken.eq((self.w_data.cfi_idx == w)
                                    & self.w_data.cfi_valid),
                in_uops[w].exc_ae_if.eq(self.w_data.exc_ae_if),
                in_uops[w].exc_pf_if.eq(self.w_data.exc_pf_if),
                in_uops[w].bp_exc_if.eq(self.w_data.bp_exc_if[w]),
                in_uops[w].bp_debug_if.eq(self.w_data.bp_debug_if[w]),
            ]

        if self.sim_debug:
            id_counter = Signal(MicroOp.ID_WIDTH)
            next_id = id_counter

            for w in range(self.fetch_width):
                pc = self.w_data.pc + (w << 1)
                m.d.comb += [
                    in_uops[w].uop_id.eq(next_id),
                    self.if_debug[w].valid.eq(in_mask[w]),
                    self.if_debug[w].bits.uop_id.eq(in_uops[w].uop_id),
                    self.if_debug[w].bits.inst.eq(in_uops[w].inst),
                    self.if_debug[w].bits.pc.eq(pc),
                ]

                next_id = Mux(in_mask[w], next_id + 1, next_id)

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

        self.deq_idx = Signal(range(self.ftq_size))
        self.deq_valid = Signal()

        self.redirect_idx = Signal(range(self.ftq_size))
        self.redirect_valid = Signal()

        self.get_pc_idx = [
            Signal(range(self.ftq_size), name=f'get_pc_idx{i}')
            for i in range(2)
        ]
        self.get_pc = [GetPCResp(name=f'get_pc{i}') for i in range(2)]

    def elaborate(self, platform):
        m = Module()

        pc_lsb_w = Shape.cast(range(self.fetch_bytes)).width

        w_ptr = Signal(range(self.ftq_size), reset=1)
        deq_ptr = Signal(range(self.ftq_size))

        full = (wrap_incr(wrap_incr(w_ptr, self.ftq_size), self.ftq_size)
                == deq_ptr) | (wrap_incr(w_ptr, self.ftq_size) == deq_ptr)

        pcs = Array(
            Signal(32 - pc_lsb_w, name=f'pc{i}') for i in range(self.ftq_size))

        with m.If(self.w_en & self.w_rdy):
            m.d.sync += [
                pcs[w_ptr].eq(self.w_data.pc >> pc_lsb_w),
                w_ptr.eq(wrap_incr(w_ptr, self.ftq_size)),
            ]

        m.d.comb += self.w_idx.eq(w_ptr)

        with m.If(self.deq_valid):
            m.d.sync += deq_ptr.eq(self.deq_idx)

        m.d.comb += self.w_rdy.eq(~full)

        with m.If(self.redirect_valid):
            m.d.sync += w_ptr.eq(wrap_incr(self.redirect_idx, self.ftq_size))

        for get_pc_idx, get_pc in zip(self.get_pc_idx, self.get_pc):
            next_idx = wrap_incr(get_pc_idx, self.ftq_size)
            next_is_w = (next_idx == w_ptr) & self.w_en & self.w_rdy

            m.d.sync += [
                get_pc.pc.eq(pcs[get_pc_idx] << pc_lsb_w),
                get_pc.commit_pc.eq(pcs[Mux(self.deq_valid, self.deq_idx,
                                            deq_ptr)] << pc_lsb_w),
                get_pc.next_valid.eq((next_idx != w_ptr) | next_is_w),
                get_pc.next_pc.eq(Mux(next_is_w, self.w_data.pc,
                                      pcs[next_idx])),
            ]

        return m


class IFStage(HasCoreParams, Elaboratable):

    def __init__(self, ibus, params, sim_debug=False):
        super().__init__(params)

        self.reset_vector = Signal(32)

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
        self.get_pc = [GetPCResp(name=f'get_pc{i}') for i in range(2)]

        self.redirect_valid = Signal()
        self.redirect_pc = Signal(32)
        self.redirect_flush = Signal()
        self.redirect_ftq_idx = Signal(range(self.ftq_size))

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

        #
        # F0 - Next PC select
        #

        s0_vpc = Signal(32)
        s0_valid = Signal()
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

        #
        # F1 - ICache Access
        #

        s1_vpc = Signal.like(s0_vpc)
        s1_valid = Signal()
        s1_is_replay = Signal()
        s1_replay_ppc = Signal.like(s0_replay_ppc)
        s1_replay_resp = TLBResp(self.params)
        s1_tlb_resp = TLBResp(self.params)
        f1_clear = Signal()

        m.d.sync += [
            s1_vpc.eq(s0_vpc),
            s1_valid.eq(s0_valid),
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

            f1_predicted_target = Signal(32)
            m.d.comb += f1_predicted_target.eq(next_fetch(s1_vpc))

            with m.If(s1_valid & ~s1_tlb_miss):
                m.d.comb += [
                    s0_valid.eq(~(s1_tlb_resp.ae.inst | s1_tlb_resp.pf.inst)),
                    s0_vpc.eq(f1_predicted_target),
                ]

        #
        # F2 - ICache Response
        #

        s2_vpc = Signal.like(s1_vpc)
        s2_valid = Signal()
        s2_ppc = Signal.like(s1_ppc)
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
                s2_is_replay_reg.eq(s1_is_replay),
                s2_tlb_resp.eq(s1_tlb_resp),
                s2_tlb_miss.eq(s1_tlb_miss),
            ]

            f2_predicted_target = Signal(32)
            m.d.comb += f2_predicted_target.eq(next_fetch(s2_vpc))

            with m.If((s2_valid & ~icache.resp.valid)
                      | (s2_valid & icache.resp.valid & ~f3_ready)):
                m.d.comb += [
                    s0_valid.eq(~(s2_tlb_resp.ae.inst | s2_tlb_resp.pf.inst)
                                | s2_is_replay | s2_tlb_miss),
                    s0_vpc.eq(s2_vpc),
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

        f3_pipe_reg = Signal(self.fetch_bytes * 8 + 2 + len(s2_vpc))
        f3_empty = Signal(reset=1)

        f4_ready = Signal()

        if self.enable_icache:
            f3_w_en = s2_valid & ~f2_clear & (icache.resp.valid | (
                (s2_tlb_resp.ae.inst | s2_tlb_resp.pf.inst) & ~s2_tlb_miss))
            f3_w_data = Cat(icache.resp.bits.data, s2_tlb_resp.ae.inst,
                            s2_tlb_resp.pf.inst, s2_vpc)
        else:
            f3_w_en = s2_valid & ~f2_clear & self.ibus.ack
            f3_w_data = Cat(self.ibus.dat_r, Const(0, 2), s2_vpc)

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

        s3_valid = Signal()
        s3_pc = Signal(32)
        s3_data = Signal(self.fetch_bytes * 8)
        s3_exc_ae = Signal()
        s3_exc_pf = Signal()
        m.d.comb += [
            s3_valid.eq(~f3_empty),
            Cat(s3_data, s3_exc_ae, s3_exc_pf, s3_pc).eq(f3_pipe_reg),
        ]

        f3_prev_half = Signal(16)
        f3_prev_half_valid = Signal()

        f3_imemresp_mask = Signal(self.fetch_width)
        f3_aligned_pc = fetch_align(s3_pc)
        f3_insts = [Signal(32) for _ in range(self.fetch_width)]
        f3_exp_insts = [Signal(32) for _ in range(self.fetch_width)]
        f3_mask = Signal(self.fetch_width)
        f3_is_rvc = Signal(self.fetch_width)
        f3_redirects = Signal(self.fetch_width)
        f3_targets = Array(
            Signal(32, name=f'f3_target{i}') for i in range(self.fetch_width))
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
                    pass
                with m.Else():
                    m.d.comb += [
                        f3_insts[w].eq(inst1),
                        f3_exp_insts[w].eq(dec1.instr_o),
                        bp_matcher.pc.eq(pc1),
                    ]
                    br_sigs = br_dec1.out

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
                ]

                br_sigs = br_dec.out

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
                f3_plus4_mask[w].eq(
                    ((f3_insts[w][0:2] == 3) & ~f3_prev_half_valid))
            else:
                f3_plus4_mask[w].eq((f3_insts[w][0:2] == 3))

            m.d.comb += f3_redirects[w].eq(f3_mask[w] & (
                (br_sigs.cfi_type == CFIType.JAL)
                | (br_sigs.cfi_type == CFIType.JALR)))

            m.d.comb += f3_cfi_types[w].eq(br_sigs.cfi_type)

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
                                      & (last_inst[0:2] != 3)),
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

        f3_predicted_target = Signal(32)
        m.d.comb += [
            f3_predicted_target.eq(
                Mux(
                    f3_fetch_bundle.cfi_valid &
                    (f3_fetch_bundle.cfi_type != CFIType.JALR),
                    f3_targets[f3_fetch_bundle.cfi_idx],
                    next_fetch(f3_fetch_bundle.pc))),
            f3_fetch_bundle.next_pc.eq(f3_predicted_target),
        ]

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
            ftq.deq_idx.eq(self.commit),
            ftq.deq_valid.eq(self.commit_valid),
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
                ftq.redirect_valid.eq(self.redirect_valid),
                ftq.redirect_idx.eq(self.redirect_ftq_idx),
            ]

        #
        # Debug signals
        #

        if self.sim_debug:
            for l, r in zip(self.if_debug, fb.if_debug):
                m.d.comb += l.eq(r)

        return m
