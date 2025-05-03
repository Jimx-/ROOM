from amaranth import *
from amaranth import tracer
from amaranth.utils import log2_int

from room.consts import *
from room.types import HasCoreParams, MicroOp
from room.fu import ExecResp
from room.branch import BranchUpdate
from room.rob import CommitReq, Exception
from room.exc import Cause, MStatus
from room.utils import wrap_incr, is_older, sign_extend
from room.dcache import LoadGen, StoreGen, DCache, SimpleDCache
from room.mmu import PTBR, PageTableWalker, CoreMemRequest, CoreMemResponse
from room.tlb import TLB

from roomsoc.interconnect.stream import Valid, Decoupled


class LSUDebug(HasCoreParams, Record):

    def __init__(self, params, name=None, src_loc_at=0):
        HasCoreParams.__init__(self, params)

        Record.__init__(self, [
            ('uop_id', MicroOp.ID_WIDTH),
            ('opcode', UOpCode),
            ('addr', self.xlen),
            ('data', self.xlen),
            ('prs1', range(self.num_pregs)),
            ('prs2', range(self.num_pregs)),
        ],
                        name=name,
                        src_loc_at=1 + src_loc_at)


class RRPriorityEncoder(Elaboratable):

    def __init__(self, width, is_head=True):
        self.width = width
        self.is_head = is_head

        self.i = Signal(width)
        self.head_or_tail = Signal(range(width))
        self.o = Signal(range(width))
        self.n = Signal()

    def elaborate(self, platform):
        m = Module()

        if self.is_head:
            for j in reversed(range(self.width)):
                with m.If(self.i[j]):
                    m.d.comb += self.o.eq(j)

            for j in reversed(range(self.width)):
                with m.If(self.i[j] & (j >= self.head_or_tail)):
                    m.d.comb += self.o.eq(j)
        else:
            for j in range(self.width):
                with m.If(self.i[j]):
                    m.d.comb += self.o.eq(j)

            for j in range(self.width):
                with m.If(self.i[j] & (j < self.head_or_tail)):
                    m.d.comb += self.o.eq(j)

        m.d.comb += self.n.eq(self.i == 0)
        return m


class LDQEntry(HasCoreParams):

    def __init__(self, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.valid = Signal(name=f'{name}_valid')
        self.uop = MicroOp(params, name=f'{name}_uop')

        self.addr = Signal(self.core_max_addr_bits, name=f'{name}_addr')
        self.addr_valid = Signal(name=f'{name}_addr_valid')
        self.is_vaddr = Signal(name=f'{name}_is_vaddr')
        self.addr_uncacheable = Signal(name=f'{name}_addr_uncacheable')

        self.executed = Signal(name=f'{name}_executed')
        self.succeeded = Signal(name=f'{name}_succeeded')
        self.order_fail = Signal(name=f'{name}_order_fail')

        self.st_dep_mask = Signal(self.stq_size, name=f'{name}_st_dep_mask')
        self.next_stq_idx = Signal(range(self.stq_size),
                                   name=f'{name}_next_stq_idx')

        self.forwarded = Signal(name=f'{name}_forwarded')
        self.forward_stq_idx = Signal(range(self.stq_size),
                                      name=f'{name}_forward_stq_idx')

    def eq(self, rhs):
        attrs = [
            'valid',
            'uop',
            'addr',
            'addr_valid',
            'is_vaddr',
            'executed',
            'succeeded',
            'st_dep_mask',
        ]
        return [getattr(self, a).eq(getattr(rhs, a)) for a in attrs]


class STQEntry(HasCoreParams):

    def __init__(self, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.valid = Signal(name=f'{name}_valid')
        self.uop = MicroOp(params, name=f'{name}_uop')

        self.addr = Signal(self.core_max_addr_bits, name=f'{name}_addr')
        self.addr_valid = Signal(name=f'{name}_addr_valid')
        self.is_vaddr = Signal(name=f'{name}_is_vaddr')
        self.data = Signal(self.xlen, name=f'{name}_data')
        self.data_valid = Signal(name=f'{name}_data_valid')

        self.committed = Signal(name=f'{name}_committed')
        self.succeeded = Signal(name=f'{name}_succeeded')

    def eq(self, rhs):
        attrs = [
            'valid',
            'uop',
            'addr',
            'addr_valid',
            'is_vaddr',
            'data',
            'data_valid',
            'committed',
            'succeeded',
        ]
        return [getattr(self, a).eq(getattr(rhs, a)) for a in attrs]


def gen_byte_mask(addr, size):
    return Mux(
        size == 0, 1 << addr[:3],
        Mux(size == 1, 3 << (addr[1:3] << 1),
            Mux(size == 2, Mux(addr[2], 0xf0, 0xf), Mux(size == 3, 0xff, 0))))


class _CoreRequestState(IntEnum):
    READY = 0
    S1 = 1
    S2 = 2
    S2_NACK = 3
    WAIT = 4
    REPLAY = 5
    DEAD = 6


class LoadStoreUnit(HasCoreParams, Elaboratable):

    def __init__(self, dbus, dbus_mmio, params, sim_debug=False):
        super().__init__(params)

        self.sim_debug = sim_debug
        self.enable_dcache = params.get('dcache_params') is not None

        self.dbus = dbus
        self.dbus_mmio = dbus_mmio

        self.dis_uops = [
            MicroOp(params, name=f'dis_uop{i}') for i in range(self.core_width)
        ]
        self.dis_valids = Signal(self.core_width)

        self.ldq_full = Signal(self.core_width)
        self.stq_full = Signal(self.core_width)

        self.dis_ldq_idx = [
            Signal(range(self.ldq_size), name=f'dis_ldq_idx{i}')
            for i in range(self.core_width)
        ]
        self.dis_stq_idx = [
            Signal(range(self.stq_size), name=f'dis_stq_idx{i}')
            for i in range(self.core_width)
        ]

        self.exec_reqs = [
            Valid(ExecResp, self.xlen, params, name=f'exec_req{i}')
            for i in range(self.mem_width)
        ]

        self.fp_std = Decoupled(ExecResp, self.xlen, params)

        self.exec_iresps = [
            Decoupled(ExecResp, self.xlen, params, name=f'exec_iresp{i}')
            for i in range(self.mem_width)
        ]

        self.exec_fresps = [
            Decoupled(ExecResp, self.xlen, params, name=f'exec_fresp{i}')
            for i in range(self.mem_width)
        ]

        self.core_req = Decoupled(CoreMemRequest, params)
        self.core_nack = Signal()
        self.core_resp = Valid(CoreMemResponse, params)

        self.clear_busy = [
            Valid(Signal,
                  range(self.core_width * self.num_rob_rows),
                  name=f'clear_busy{i}') for i in range(self.mem_width + 1)
        ]

        self.commit = CommitReq(params)
        self.exception = Signal()

        self.commit_load_at_head = Signal()

        self.br_update = BranchUpdate(params)
        self.rob_head_idx = Signal(range(self.core_width * self.num_rob_rows))

        self.lsu_exc = Valid(Exception, params, name='lsu_exc')

        self.prv = Signal(PrivilegeMode)
        self.status = MStatus(self.xlen)
        self.ptbr = PTBR(self.xlen)

        self.ptw_req = Decoupled(PageTableWalker.Request, params)
        self.ptw_resp = Valid(PageTableWalker.Response, params)

        if self.sim_debug:
            self.lsu_debug = [
                Valid(LSUDebug, params, name=f'lsu_debug{i}')
                for i in range(self.mem_width)
            ]

    def elaborate(self, platform):
        m = Module()

        exception_d1 = Signal()
        exception_d2 = Signal()
        m.d.sync += [
            exception_d1.eq(self.exception),
            exception_d2.eq(exception_d1),
        ]
        s1_exc_valid = Signal()

        dcache_cls = DCache if self.enable_dcache else SimpleDCache
        dcache = m.submodules.dcache = DomainRenamer('dcache')(dcache_cls(
            self.dbus, self.dbus_mmio, self.params))

        tlb = m.submodules.tlb = TLB(req_width=self.mem_width,
                                     params=self.params,
                                     log_max_size=log2_int(self.fetch_bytes),
                                     n_sets=self.n_dtlb_sets,
                                     n_ways=self.n_dtlb_ways,
                                     n_banks=self.mem_width)
        m.d.comb += [
            tlb.prv.eq(self.prv),
            tlb.status.eq(self.status),
            tlb.ptbr.eq(self.ptbr),
            tlb.ptw_req.connect(self.ptw_req),
            tlb.ptw_resp.eq(self.ptw_resp),
        ]
        tlb_miss_ready = Signal()
        m.d.sync += tlb_miss_ready.eq(tlb.miss_ready)

        ldq = Array(
            LDQEntry(self.params, name=f'ldq{i}')
            for i in range(self.ldq_size))
        stq = Array(
            STQEntry(self.params, name=f'stq{i}')
            for i in range(self.stq_size))

        ldq_addr = Array(
            Signal.like(ldq[i].addr, name=f'ldq_addr{i}')
            for i in range(self.ldq_size))
        ldq_is_vaddr = Array(
            Signal(name=f'ldq_is_vaddr{i}') for i in range(self.ldq_size))
        for i in range(self.ldq_size):
            m.d.comb += [
                ldq_addr[i].eq(ldq[i].addr),
                ldq_is_vaddr[i].eq(ldq[i].is_vaddr),
            ]

        stq_addr = Array(
            Signal.like(stq[i].addr, name=f'stq_addr{i}')
            for i in range(self.stq_size))
        stq_is_vaddr = Array(
            Signal(name=f'stq_is_vaddr{i}') for i in range(self.stq_size))
        for i in range(self.stq_size):
            m.d.comb += [
                stq_addr[i].eq(stq[i].addr),
                stq_is_vaddr[i].eq(stq[i].is_vaddr),
            ]

        ldq_head = Signal(range(self.ldq_size))
        ldq_tail = Signal.like(ldq_head)
        stq_head = Signal(range(self.stq_size))
        stq_tail = Signal.like(stq_head)
        stq_commit_head = Signal.like(stq_head)
        stq_execute_head = Signal.like(stq_head)

        core_req_state = Signal(_CoreRequestState,
                                reset=_CoreRequestState.READY)
        core_req = CoreMemRequest(self.params)

        clear_store = Signal()
        live_store_mask = Signal(self.stq_size)
        next_live_store_mask = Mux(clear_store,
                                   live_store_mask & ~(1 << stq_head),
                                   live_store_mask)

        with m.If(clear_store):
            for i in range(self.ldq_size):
                m.d.sync += ldq[i].st_dep_mask.eq(ldq[i].st_dep_mask
                                                  & ~(1 << stq_head))

        ldq_w_idx = ldq_tail
        stq_w_idx = stq_tail

        for w in range(self.core_width):
            m.d.comb += [
                self.ldq_full[w].eq(
                    wrap_incr(ldq_w_idx, self.ldq_size) == ldq_head),
                self.stq_full[w].eq(
                    wrap_incr(stq_w_idx, self.stq_size) == stq_head),
                self.dis_ldq_idx[w].eq(ldq_w_idx),
                self.dis_stq_idx[w].eq(stq_w_idx),
            ]

            dis_w_ldq = self.dis_valids[w] & self.dis_uops[w].uses_ldq
            dis_w_stq = self.dis_valids[w] & self.dis_uops[w].uses_stq

            with m.If(dis_w_ldq):
                m.d.sync += [
                    ldq[self.dis_ldq_idx[w]].valid.eq(1),
                    ldq[self.dis_ldq_idx[w]].uop.eq(self.dis_uops[w]),
                    ldq[self.dis_ldq_idx[w]].addr_valid.eq(0),
                    ldq[self.dis_ldq_idx[w]].executed.eq(0),
                    ldq[self.dis_ldq_idx[w]].succeeded.eq(0),
                    ldq[self.dis_ldq_idx[w]].order_fail.eq(0),
                    ldq[self.dis_ldq_idx[w]].forwarded.eq(0),
                    ldq[self.dis_ldq_idx[w]].st_dep_mask.eq(
                        next_live_store_mask),
                    ldq[self.dis_ldq_idx[w]].next_stq_idx.eq(stq_w_idx),
                ]
            with m.Elif(dis_w_stq):
                m.d.sync += [
                    stq[self.dis_stq_idx[w]].valid.eq(1),
                    stq[self.dis_stq_idx[w]].uop.eq(self.dis_uops[w]),
                    stq[self.dis_stq_idx[w]].addr_valid.eq(0),
                    stq[self.dis_stq_idx[w]].data_valid.eq(0),
                    stq[self.dis_stq_idx[w]].committed.eq(0),
                    stq[self.dis_stq_idx[w]].succeeded.eq(0),
                ]

            ldq_w_idx = Mux(dis_w_ldq, wrap_incr(ldq_w_idx, self.ldq_size),
                            ldq_w_idx)

            next_live_store_mask = Mux(dis_w_stq,
                                       next_live_store_mask | (1 << stq_w_idx),
                                       next_live_store_mask)
            stq_w_idx = Mux(dis_w_stq, wrap_incr(stq_w_idx, self.stq_size),
                            stq_w_idx)

        m.d.sync += [
            ldq_tail.eq(ldq_w_idx),
            stq_tail.eq(stq_w_idx),
        ]

        if self.sim_debug:
            for lsu_debug, req in zip(self.lsu_debug, self.exec_reqs):
                m.d.comb += [
                    lsu_debug.valid.eq(req.valid),
                    lsu_debug.bits.uop_id.eq(req.bits.uop.uop_id),
                    lsu_debug.bits.opcode.eq(req.bits.uop.opcode),
                    lsu_debug.bits.addr.eq(
                        sign_extend(req.bits.addr, self.xlen)),
                    lsu_debug.bits.data.eq(req.bits.data),
                    lsu_debug.bits.prs1.eq(req.bits.uop.prs1),
                    lsu_debug.bits.prs2.eq(req.bits.uop.prs2),
                ]

        s0_block_load_mask = Array(
            Signal(name=f's0_block_load_mask{i}')
            for i in range(self.ldq_size))
        s1_block_load_mask = Array(
            Signal(name=f's1_block_load_mask{i}')
            for i in range(self.ldq_size))
        s2_block_load_mask = Array(
            Signal(name=f's2_block_load_mask{i}')
            for i in range(self.ldq_size))

        m.d.sync += [
            Cat(*s1_block_load_mask).eq(Cat(*s0_block_load_mask)),
            Cat(*s2_block_load_mask).eq(Cat(*s1_block_load_mask)),
        ]

        ldq_retry_enc = RRPriorityEncoder(self.ldq_size)
        m.submodules += ldq_retry_enc
        for i in range(self.ldq_size):
            m.d.comb += ldq_retry_enc.i[i].eq(ldq[i].addr_valid
                                              & ldq[i].is_vaddr
                                              & ~s0_block_load_mask[i]
                                              & ~s1_block_load_mask[i])
        m.d.comb += ldq_retry_enc.head_or_tail.eq(ldq_head)
        ldq_retry_idx = Signal(range(self.ldq_size))
        m.d.sync += ldq_retry_idx.eq(ldq_retry_enc.o)
        ldq_retry_e = ldq[ldq_retry_idx]
        s1_ldq_retry_idx = Signal(range(self.ldq_size))
        m.d.sync += s1_ldq_retry_idx.eq(ldq_retry_idx)

        ldq_wakeup_enc = RRPriorityEncoder(self.ldq_size)
        m.submodules += ldq_wakeup_enc
        for i in range(self.ldq_size):
            m.d.comb += ldq_wakeup_enc.i[i].eq(ldq[i].addr_valid
                                               & ~ldq[i].executed
                                               & ~ldq[i].succeeded
                                               & ~ldq[i].is_vaddr
                                               & ~s0_block_load_mask[i]
                                               & ~s1_block_load_mask[i])
        m.d.comb += ldq_wakeup_enc.head_or_tail.eq(ldq_head)
        ldq_wakeup_idx = Signal(range(self.ldq_size))
        m.d.sync += ldq_wakeup_idx.eq(ldq_wakeup_enc.o)
        ldq_wakeup_e = ldq[ldq_wakeup_idx]
        s1_ldq_wakeup_idx = Signal(range(self.ldq_size))
        m.d.sync += s1_ldq_wakeup_idx.eq(ldq_wakeup_idx)

        stq_retry_enc = RRPriorityEncoder(self.stq_size)
        m.submodules += stq_retry_enc
        for i in range(self.stq_size):
            m.d.comb += stq_retry_enc.i[i].eq(stq[i].addr_valid
                                              & stq_is_vaddr[i])
        m.d.comb += stq_retry_enc.head_or_tail.eq(stq_commit_head)
        stq_retry_idx = Signal(range(self.stq_size))
        m.d.sync += stq_retry_idx.eq(stq_retry_enc.o)
        stq_retry_e = stq[stq_retry_idx]
        s1_stq_retry_idx = Signal(range(self.stq_size))
        m.d.sync += s1_stq_retry_idx.eq(stq_retry_idx)

        stq_commit_e = stq[stq_execute_head]

        can_fire_load_incoming = Signal(self.mem_width)
        can_fire_sta_incoming = Signal(self.mem_width)
        can_fire_std_incoming = Signal(self.mem_width)
        can_fire_stad_incoming = Signal(self.mem_width)
        can_fire_sfence = Signal(self.mem_width)
        can_fire_core_incoming = Signal(self.mem_width)
        can_fire_core_retry = Signal(self.mem_width)
        can_fire_load_retry = Signal(self.mem_width)
        can_fire_load_wakeup = Signal(self.mem_width)
        can_fire_sta_retry = Signal(self.mem_width)
        can_fire_store_commit = Signal(self.mem_width)

        for w in range(self.mem_width):
            with m.If(self.exec_reqs[w].valid
                      & self.exec_reqs[w].bits.sfence.valid):
                m.d.comb += can_fire_sfence.eq(~0)

        s0_executing_loads = Array(
            Signal(name=f's0_executing{i}') for i in range(self.ldq_size))
        s1_executing_loads = Array(
            Signal(name=f's1_executing{i}') for i in range(self.ldq_size))
        s1_set_executed = Array(
            Signal(name=f's1_set_executed({i})') for i in range(self.ldq_size))
        for a, b in zip(s1_executing_loads, s0_executing_loads):
            m.d.sync += a.eq(b)
        for a, b in zip(s1_set_executed, s1_executing_loads):
            m.d.comb += a.eq(b)

        for w in range(self.mem_width):
            m.d.comb += [
                can_fire_load_incoming[w].eq(
                    self.exec_reqs[w].valid
                    & self.exec_reqs[w].bits.uop.is_load),
                can_fire_sta_incoming[w].eq(
                    self.exec_reqs[w].valid
                    & self.exec_reqs[w].bits.uop.is_sta
                    & ~self.exec_reqs[w].bits.uop.is_std),
                can_fire_std_incoming[w].eq(
                    self.exec_reqs[w].valid
                    & self.exec_reqs[w].bits.uop.is_std
                    & ~self.exec_reqs[w].bits.uop.is_sta),
                can_fire_stad_incoming[w].eq(
                    self.exec_reqs[w].valid
                    & self.exec_reqs[w].bits.uop.is_sta
                    & self.exec_reqs[w].bits.uop.is_std),
                can_fire_load_retry[w].eq(ldq_retry_e.valid
                                          & ldq_retry_e.addr_valid
                                          & ldq_retry_e.is_vaddr
                                          & ~s1_block_load_mask[ldq_retry_idx]
                                          & ~s2_block_load_mask[ldq_retry_idx]
                                          & ~ldq_retry_e.order_fail
                                          & (w == self.mem_width - 1)
                                          & tlb_miss_ready),
                can_fire_load_wakeup[w].eq(
                    ldq_wakeup_e.valid
                    & ldq_wakeup_e.addr_valid
                    & ~ldq_wakeup_e.executed
                    & ~ldq_wakeup_e.succeeded
                    & ~ldq_wakeup_e.is_vaddr
                    & ~s1_executing_loads[ldq_wakeup_idx]
                    & ~ldq_wakeup_e.order_fail
                    & ~s1_block_load_mask[ldq_wakeup_idx]
                    & ~s2_block_load_mask[ldq_wakeup_idx]
                    & (w == self.mem_width - 1)
                    & (~ldq_wakeup_e.addr_uncacheable
                       | (self.commit_load_at_head &
                          (ldq_head == ldq_wakeup_idx)
                          & (ldq_wakeup_e.st_dep_mask == 0)))),
                can_fire_sta_retry[w].eq(
                    stq_retry_e.valid
                    & stq_retry_e.addr_valid
                    & stq_is_vaddr[stq_retry_idx]
                    & (w == self.mem_width - 1)
                    & tlb_miss_ready
                    & ~Cat(
                        (i != w) & can_fire_std_incoming[i] &
                        (self.exec_reqs[i].bits.uop.stq_idx == stq_retry_idx)
                        for i in range(self.mem_width)).any()),
                can_fire_store_commit[w].eq(
                    stq_commit_e.valid
                    & ~s1_exc_valid
                    & ~stq_commit_e.uop.exception
                    & ~stq_commit_e.uop.is_fence
                    & (w == 0)
                    & (stq_commit_e.committed
                       | (stq_commit_e.uop.is_amo & stq_commit_e.addr_valid
                          & ~stq_commit_e.is_vaddr
                          & stq_commit_e.data_valid))),
            ]

        will_fire_load_incoming = Signal(self.mem_width)
        will_fire_sta_incoming = Signal(self.mem_width)
        will_fire_std_incoming = Signal(self.mem_width)
        will_fire_stad_incoming = Signal(self.mem_width)
        will_fire_sfence = Signal(self.mem_width)
        will_fire_core_incoming = Signal(self.mem_width)
        will_fire_core_retry = Signal(self.mem_width)
        will_fire_load_retry = Signal(self.mem_width)
        will_fire_sta_retry = Signal(self.mem_width)
        will_fire_load_wakeup = Signal(self.mem_width)
        will_fire_store_commit = Signal(self.mem_width)

        exec_tlb_valid = Signal(self.mem_width)
        for w in range(self.mem_width):
            # Cat(tlb_avail, dc_avail, cam_avail, rob_avail)
            avail_flags = Const(0b1111, 4)

            def sched(can_fire, use_flags, avail_flags, will_fire):
                m.d.comb += will_fire.eq((can_fire.replicate(len(avail_flags))
                                          & ~(use_flags & ~avail_flags)).all())
                new_avail_flags = Signal.like(
                    avail_flags,
                    name=f'{can_fire.value.name}_avail{can_fire.start}')
                m.d.comb += new_avail_flags.eq(
                    avail_flags
                    & ~(will_fire.replicate(len(avail_flags)) & use_flags))
                return new_avail_flags

            avail_flags = sched(can_fire_load_incoming[w], Const(0b0111, 4),
                                avail_flags, will_fire_load_incoming[w])
            avail_flags = sched(can_fire_stad_incoming[w], Const(0b1101, 4),
                                avail_flags, will_fire_stad_incoming[w])
            avail_flags = sched(can_fire_sta_incoming[w], Const(0b1101, 4),
                                avail_flags, will_fire_sta_incoming[w])
            avail_flags = sched(can_fire_std_incoming[w], Const(0b1000, 4),
                                avail_flags, will_fire_std_incoming[w])
            avail_flags = sched(can_fire_sfence[w], Const(0b1001, 4),
                                avail_flags, will_fire_sfence[w])
            avail_flags = sched(can_fire_core_incoming[w], Const(0b0011, 4),
                                avail_flags, will_fire_core_incoming[w])
            avail_flags = sched(can_fire_core_retry[w], Const(0b0011, 4),
                                avail_flags, will_fire_core_retry[w])
            avail_flags = sched(can_fire_load_retry[w], Const(0b0111, 4),
                                avail_flags, will_fire_load_retry[w])
            avail_flags = sched(can_fire_sta_retry[w], Const(0b1101, 4),
                                avail_flags, will_fire_sta_retry[w])
            avail_flags = sched(can_fire_load_wakeup[w], Const(0b0110, 4),
                                avail_flags, will_fire_load_wakeup[w])
            avail_flags = sched(can_fire_store_commit[w], Const(0b0010, 4),
                                avail_flags, will_fire_store_commit[w])

            m.d.comb += exec_tlb_valid[w].eq(~avail_flags[0])

        #
        # TLB request
        #

        exec_tlb_uop = [
            MicroOp(self.params, name=f'exec_tlb_uop{w}')
            for w in range(self.mem_width)
        ]
        exec_tlb_vaddr = [
            Signal.like(self.exec_reqs[0].bits.addr, name=f'exec_tlb_vaddr{w}')
            for w in range(self.mem_width)
        ]
        exec_tlb_size = [
            Signal.like(self.exec_reqs[0].bits.uop.mem_size,
                        name=f'exec_tlb_size{w}')
            for w in range(self.mem_width)
        ]
        exec_tlb_cmd = [
            Signal(MemoryCommand, name=f'exec_tlb_cmd{w}')
            for w in range(self.mem_width)
        ]
        exec_tlb_passthru = Signal(self.mem_width)
        for w in range(self.mem_width):
            with m.If(will_fire_load_incoming[w] | will_fire_sta_incoming[w]
                      | will_fire_stad_incoming[w]):
                m.d.comb += [
                    exec_tlb_uop[w].eq(self.exec_reqs[w].bits.uop),
                    exec_tlb_vaddr[w].eq(self.exec_reqs[w].bits.addr),
                    exec_tlb_size[w].eq(self.exec_reqs[w].bits.uop.mem_size),
                    exec_tlb_cmd[w].eq(self.exec_reqs[w].bits.uop.mem_cmd),
                ]
            with m.Elif(will_fire_sfence[w]):
                m.d.comb += [
                    exec_tlb_uop[w].eq(self.exec_reqs[w].bits.uop),
                    exec_tlb_vaddr[w].eq(
                        self.exec_reqs[w].bits.sfence.bits.vaddr),
                    exec_tlb_size[w].eq(self.exec_reqs[w].bits.uop.mem_size),
                    exec_tlb_cmd[w].eq(self.exec_reqs[w].bits.uop.mem_cmd),
                ]
            with m.Elif(will_fire_load_retry[w]):
                m.d.comb += [
                    exec_tlb_uop[w].eq(ldq_retry_e.uop),
                    exec_tlb_vaddr[w].eq(ldq_retry_e.addr),
                    exec_tlb_size[w].eq(ldq_retry_e.uop.mem_size),
                    exec_tlb_cmd[w].eq(ldq_retry_e.uop.mem_cmd),
                ]
            with m.Elif(will_fire_sta_retry[w]):
                m.d.comb += [
                    exec_tlb_uop[w].eq(stq_retry_e.uop),
                    exec_tlb_vaddr[w].eq(stq_retry_e.addr),
                    exec_tlb_size[w].eq(stq_retry_e.uop.mem_size),
                    exec_tlb_cmd[w].eq(stq_retry_e.uop.mem_cmd),
                ]
            with m.Elif(will_fire_core_incoming[w]):
                m.d.comb += [
                    exec_tlb_vaddr[w].eq(core_req.addr),
                    exec_tlb_size[w].eq(core_req.size),
                    exec_tlb_cmd[w].eq(core_req.cmd),
                    exec_tlb_passthru[w].eq(core_req.phys),
                ]

        for w in range(self.mem_width):
            m.d.comb += [
                tlb.req[w].valid.eq(
                    exec_tlb_valid[w]
                    & ~self.br_update.uop_killed(exec_tlb_uop[w])),
                tlb.req[w].bits.vaddr.eq(exec_tlb_vaddr[w]),
                tlb.req[w].bits.size.eq(exec_tlb_size[w]),
                tlb.req[w].bits.cmd.eq(exec_tlb_cmd[w]),
                tlb.req[w].bits.passthru.eq(exec_tlb_passthru[w]),
            ]

            with m.If(will_fire_sfence[w]
                      & self.exec_reqs[w].bits.sfence.valid):
                m.d.comb += tlb.sfence.eq(self.exec_reqs[w].bits.sfence)

        for w in range(self.mem_width):
            with m.If(will_fire_load_incoming[w]):
                ldq_idx = self.exec_reqs[w].bits.uop.ldq_idx
                m.d.sync += [
                    ldq[ldq_idx].addr.eq(self.exec_reqs[w].bits.addr),
                    ldq[ldq_idx].addr_valid.eq(1),
                    ldq[ldq_idx].is_vaddr.eq(1),
                    ldq[ldq_idx].uop.pdst.eq(self.exec_reqs[w].bits.uop.pdst),
                ]

            with m.If(will_fire_sta_incoming[w] | will_fire_stad_incoming[w]):
                stq_idx = self.exec_reqs[w].bits.uop.stq_idx
                m.d.sync += [
                    stq[stq_idx].addr.eq(self.exec_reqs[w].bits.addr),
                    stq[stq_idx].addr_valid.eq(1),
                    stq[stq_idx].is_vaddr.eq(1),
                    stq[stq_idx].uop.pdst.eq(self.exec_reqs[w].bits.uop.pdst),
                ]

            if w == 0:
                m.d.comb += self.fp_std.ready.eq(~will_fire_std_incoming[w]
                                                 & ~will_fire_stad_incoming[w])
            fp_std_fire = self.fp_std.valid & self.fp_std.ready & (w == 0)

            with m.If(will_fire_std_incoming[w] | will_fire_stad_incoming[w]
                      | fp_std_fire):
                stq_idx = Mux(
                    will_fire_std_incoming[w] | will_fire_stad_incoming[w],
                    self.exec_reqs[w].bits.uop.stq_idx,
                    self.fp_std.bits.uop.stq_idx)

                m.d.sync += [
                    stq[stq_idx].data.eq(
                        Mux(
                            will_fire_std_incoming[w]
                            | will_fire_stad_incoming[w],
                            self.exec_reqs[w].bits.data,
                            self.fp_std.bits.data)),
                    stq[stq_idx].data_valid.eq(1),
                ]

        will_fire_stdf_incoming = self.fp_std.valid & self.fp_std.ready

        #
        # Memory access
        #

        store_gen = m.submodules.store_gen = StoreGen(max_size=self.xlen // 8)
        m.d.comb += [
            store_gen.typ.eq(stq_commit_e.uop.mem_size),
            store_gen.addr.eq(0),
            store_gen.data_in.eq(stq_commit_e.data),
        ]

        m.d.comb += [
            dcache.br_update.eq(self.br_update),
            dcache.exception.eq(self.exception),
        ]

        for w in range(self.mem_width):
            dmem_req = dcache.req[w]

            with m.If(will_fire_load_incoming[w]):
                m.d.comb += [
                    dmem_req.valid.eq(1),
                    dmem_req.bits.uop.eq(self.exec_reqs[w].bits.uop),
                    dmem_req.bits.addr.eq(self.exec_reqs[w].bits.addr),
                    s0_executing_loads[self.exec_reqs[w].bits.uop.ldq_idx].eq(
                        dmem_req.fire),
                ]
            with m.Elif(will_fire_load_retry[w]):
                m.d.comb += [
                    dmem_req.valid.eq(1),
                    dmem_req.bits.uop.eq(exec_tlb_uop[w]),
                    dmem_req.bits.addr.eq(exec_tlb_vaddr[w]),
                    s0_executing_loads[ldq_retry_idx].eq(dmem_req.fire),
                ]
            with m.Elif(will_fire_load_wakeup[w]):
                m.d.comb += [
                    dmem_req.valid.eq(1),
                    dmem_req.bits.uop.eq(ldq_wakeup_e.uop),
                    dmem_req.bits.addr.eq(ldq_wakeup_e.addr),
                    s0_executing_loads[ldq_wakeup_idx].eq(dmem_req.fire),
                ]
            with m.Elif(will_fire_store_commit[w]):
                m.d.comb += [
                    dmem_req.valid.eq(1),
                    dmem_req.bits.uop.eq(stq_commit_e.uop),
                    dmem_req.bits.addr.eq(stq_commit_e.addr),
                    dmem_req.bits.data.eq(store_gen.data_out),
                ]

                m.d.sync += [
                    stq_execute_head.eq(
                        Mux(dmem_req.ready,
                            wrap_incr(stq_execute_head, self.stq_size),
                            stq_execute_head)),
                    stq[stq_execute_head].succeeded.eq(0),
                ]
            with m.Elif(will_fire_core_incoming[w]):
                m.d.comb += [
                    dmem_req.valid.eq(1),
                    dmem_req.bits.uop.mem_cmd.eq(core_req.cmd),
                    dmem_req.bits.uop.mem_size.eq(core_req.size),
                    dmem_req.bits.uop.mem_signed.eq(core_req.signed),
                    dmem_req.bits.addr.eq(core_req.addr),
                    dmem_req.bits.from_core.eq(1),
                ]
            with m.Elif(will_fire_core_retry[w]):
                m.d.comb += [
                    dmem_req.valid.eq(1),
                    dmem_req.bits.uop.mem_cmd.eq(core_req.cmd),
                    dmem_req.bits.uop.mem_size.eq(core_req.size),
                    dmem_req.bits.uop.mem_signed.eq(core_req.signed),
                    dmem_req.bits.addr.eq(core_req.addr),
                    dmem_req.bits.from_core.eq(1),
                ]

        for i in range(self.ldq_size):
            with m.If(s1_set_executed[i]):
                m.d.sync += ldq[i].executed.eq(1)

        dmem_req_fired = Signal(self.mem_width)

        fired_load_incoming = Signal(self.mem_width)
        fired_load_retry = Signal(self.mem_width)
        fired_load_wakeup = Signal(self.mem_width)
        fired_sta_incoming = Signal(self.mem_width)
        fired_std_incoming = Signal(self.mem_width)
        fired_stdf_incoming = Signal()
        fired_stad_incoming = Signal(self.mem_width)
        fired_sfence = Signal(self.mem_width)
        fired_sta_retry = Signal(self.mem_width)

        s1_incoming_uop = [
            MicroOp(self.params, name=f's1_incoming_uop{i}')
            for i in range(self.mem_width)
        ]

        s1_ldq_incoming_e = [
            LDQEntry(self.params, name=f's1_ldq_incoming_e{i}')
            for i in range(self.mem_width)
        ]
        s1_ldq_retry_e = [
            LDQEntry(self.params, name=f's1_ldq_retry_e{i}')
            for i in range(self.mem_width)
        ]
        s1_ldq_wakeup_e = [
            LDQEntry(self.params, name=f's1_ldq_wakeup_e{i}')
            for i in range(self.mem_width)
        ]

        s1_stq_incoming_e = [
            STQEntry(self.params, name=f's1_stq_incoming_e{i}')
            for i in range(self.mem_width)
        ]
        s1_stq_retry_e = [
            STQEntry(self.params, name=f's1_sta_retry_e{i}')
            for i in range(self.mem_width)
        ]

        s1_ldq_e = [
            LDQEntry(self.params, name=f's1_ldq_e{i}')
            for i in range(self.mem_width)
        ]

        s1_stq_e = [
            STQEntry(self.params, name=f's1_stq_e{i}')
            for i in range(self.mem_width)
        ]

        s1_mem_addr = [
            Signal(32, name=f's1_mem_addr{i}') for i in range(self.mem_width)
        ]

        s1_req_addr = [
            Signal(32, name=f's1_req_addr{i}') for i in range(self.mem_width)
        ]

        s1_ma_ld = Signal(self.mem_width)
        s1_ma_st = Signal(self.mem_width)

        for w in range(self.mem_width):
            req_killed = self.br_update.uop_killed(self.exec_reqs[w].bits.uop)

            with m.If(fired_load_incoming[w]):
                m.d.comb += s1_ldq_e[w].eq(s1_ldq_incoming_e[w])
            with m.Elif(fired_load_retry[w]):
                m.d.comb += s1_ldq_e[w].eq(s1_ldq_retry_e[w])
            with m.Elif(fired_load_wakeup[w]):
                m.d.comb += s1_ldq_e[w].eq(s1_ldq_wakeup_e[w])

            with m.If(fired_sta_incoming[w] | fired_stad_incoming[w]):
                m.d.comb += s1_stq_e[w].eq(s1_stq_incoming_e[w])
            with m.Elif(fired_sta_retry[w]):
                m.d.comb += s1_stq_e[w].eq(s1_stq_retry_e[w])

            m.d.sync += [
                dmem_req_fired[w].eq(dcache.req[w].fire),
                s1_incoming_uop[w].eq(self.exec_reqs[w].bits.uop),
                s1_incoming_uop[w].br_mask.eq(
                    self.br_update.get_new_br_mask(
                        self.exec_reqs[w].bits.uop.br_mask)),
                fired_load_incoming[w].eq(will_fire_load_incoming[w]
                                          & ~req_killed),
                fired_load_retry[w].eq(
                    will_fire_load_retry[w]
                    & dcache.req[w].fire
                    & ~self.br_update.uop_killed(ldq_retry_e.uop)),
                fired_sta_retry[w].eq(
                    will_fire_sta_retry[w]
                    & ~self.br_update.uop_killed(stq_retry_e.uop)),
                fired_load_wakeup[w].eq(
                    will_fire_load_wakeup[w]
                    & dcache.req[w].fire
                    & ~self.br_update.uop_killed(ldq_wakeup_e.uop)),
                fired_sta_incoming[w].eq(will_fire_sta_incoming[w]
                                         & ~req_killed),
                fired_std_incoming[w].eq(will_fire_std_incoming[w]
                                         & ~req_killed),
                fired_stad_incoming[w].eq(will_fire_stad_incoming[w]
                                          & ~req_killed),
                s1_ldq_incoming_e[w].eq(
                    ldq[self.exec_reqs[w].bits.uop.ldq_idx]),
                s1_ldq_incoming_e[w].uop.br_mask.eq(
                    self.br_update.get_new_br_mask(
                        ldq[self.exec_reqs[w].bits.uop.ldq_idx].uop.br_mask)),
                s1_ldq_retry_e[w].eq(ldq_retry_e),
                s1_ldq_retry_e[w].uop.br_mask.eq(
                    self.br_update.get_new_br_mask(ldq_retry_e.uop.br_mask)),
                s1_ldq_wakeup_e[w].eq(ldq_wakeup_e),
                s1_ldq_wakeup_e[w].uop.br_mask.eq(
                    self.br_update.get_new_br_mask(ldq_wakeup_e.uop.br_mask)),
                s1_stq_incoming_e[w].eq(
                    stq[self.exec_reqs[w].bits.uop.stq_idx]),
                s1_stq_incoming_e[w].uop.br_mask.eq(
                    self.br_update.get_new_br_mask(
                        stq[self.exec_reqs[w].bits.uop.stq_idx].uop.br_mask)),
                s1_stq_retry_e[w].eq(stq_retry_e),
                s1_stq_retry_e[w].uop.br_mask.eq(
                    self.br_update.get_new_br_mask(stq_retry_e.uop.br_mask)),
                s1_mem_addr[w].eq(dcache.req[w].bits.addr),
                s1_req_addr[w].eq(self.exec_reqs[w].bits.addr),
                #
                s1_ma_ld[w].eq(will_fire_load_incoming[w]
                               & self.exec_reqs[w].bits.mem_exc.valid
                               & ~req_killed),
                s1_ma_st[w].eq((will_fire_sta_incoming[w]
                                | will_fire_stad_incoming[w])
                               & self.exec_reqs[w].bits.mem_exc.valid
                               & ~req_killed),
            ]

            with m.If(will_fire_load_wakeup[w] & dcache.req[w].fire):
                m.d.comb += s0_block_load_mask[ldq_wakeup_idx].eq(1)
            with m.Elif(will_fire_load_incoming[w]):
                m.d.comb += s0_block_load_mask[
                    self.exec_reqs[w].bits.uop.ldq_idx].eq(1)
            with m.Elif(will_fire_load_retry[w]):
                m.d.comb += s0_block_load_mask[ldq_retry_idx].eq(1)

        s1_sfence_valid = Signal(self.mem_width)
        m.d.sync += fired_sfence.eq(will_fire_sfence)
        for w in range(self.mem_width):
            m.d.sync += s1_sfence_valid[w].eq(
                self.exec_reqs[w].valid & self.exec_reqs[w].bits.sfence.valid)

        stdf_killed = self.br_update.uop_killed(self.fp_std.bits.uop)
        fired_stdf_uop = MicroOp(self.params)

        m.d.sync += [
            fired_stdf_incoming.eq(will_fire_stdf_incoming & ~stdf_killed),
            fired_stdf_uop.eq(self.fp_std.bits.uop),
            fired_stdf_uop.br_mask.eq(
                self.br_update.get_new_br_mask(self.fp_std.bits.uop.br_mask)),
        ]

        #
        # TLB response
        #

        s1_tlb_paddr = [
            Signal(self.paddr_bits, name=f's1_tlb_addr{w}')
            for w in range(self.mem_width)
        ]
        s1_tlb_miss = Signal(self.mem_width)
        s1_tlb_uncacheable = Signal(self.mem_width)
        for w in range(self.mem_width):
            m.d.comb += [
                s1_tlb_paddr[w].eq(tlb.resp[w].bits.paddr),
                s1_tlb_miss[w].eq(tlb.resp[w].valid
                                  & tlb.resp[w].bits.miss),
                s1_tlb_uncacheable[w].eq(~tlb.resp[w].bits.cacheable),
            ]

        s1_tlb_pf_ld = Signal(self.mem_width)
        s1_tlb_pf_st = Signal(self.mem_width)
        s1_tlb_ae_ld = Signal(self.mem_width)
        s1_tlb_ae_st = Signal(self.mem_width)

        for w in range(self.mem_width):
            m.d.comb += dcache.s1_paddr[w].eq(s1_mem_addr[w])

            with m.If(fired_load_incoming[w] | fired_load_retry[w]):
                ldq_idx = s1_ldq_e[w].uop.ldq_idx
                with m.If(s1_tlb_miss[w] | s1_tlb_uncacheable[w]
                          | s1_tlb_pf_ld[w] | s1_tlb_ae_ld[w]):
                    m.d.comb += [
                        dcache.s1_kill[w].eq(dmem_req_fired[w]),
                        s1_set_executed[ldq_idx].eq(0),
                    ]
                with m.If(~s1_tlb_miss[w]):
                    m.d.comb += [
                        dcache.s1_paddr[w].eq(s1_tlb_paddr[w]),
                        ldq_addr[ldq_idx].eq(s1_tlb_paddr[w]),
                        ldq_is_vaddr[ldq_idx].eq(0),
                    ]
                    m.d.sync += [
                        ldq[ldq_idx].addr.eq(ldq_addr[ldq_idx]),
                        ldq[ldq_idx].is_vaddr.eq(0),
                        ldq[ldq_idx].addr_uncacheable.eq(
                            s1_tlb_uncacheable[w]),
                    ]

            with m.If(fired_sta_incoming[w] | fired_stad_incoming[w]
                      | fired_sta_retry[w]):
                stq_idx = s1_stq_e[w].uop.stq_idx
                with m.If(~s1_tlb_miss[w]):
                    m.d.comb += [
                        stq_addr[stq_idx].eq(s1_tlb_paddr[w]),
                        stq_is_vaddr[stq_idx].eq(0),
                    ]
                    m.d.sync += [
                        stq[stq_idx].addr.eq(stq_addr[stq_idx]),
                        stq[stq_idx].is_vaddr.eq(0),
                    ]

        s1_exc_uops = [
            MicroOp(self.params, name=f's1_exc_uop{w}')
            for w in range(self.mem_width)
        ]
        s1_exc_vaddrs = [
            Signal.like(exec_tlb_vaddr[0], name=f's1_exc_vaddr{w}')
            for w in range(self.mem_width)
        ]
        for w in range(self.mem_width):
            m.d.sync += [
                s1_exc_uops[w].eq(exec_tlb_uop[w]),
                s1_exc_uops[w].br_mask.eq(
                    self.br_update.get_new_br_mask(exec_tlb_uop[w].br_mask)),
                s1_exc_vaddrs[w].eq(exec_tlb_vaddr[w]),
            ]

        s1_exc_valids = Signal(self.mem_width)
        s1_exc_causes = [
            Signal(Cause, name=f's1_exc_cause{w}')
            for w in range(self.mem_width)
        ]
        for w in range(self.mem_width):
            m.d.comb += [
                s1_tlb_pf_ld[w].eq(tlb.resp[w].valid & tlb.resp[w].bits.pf.ld
                                   & s1_exc_uops[w].uses_ldq),
                s1_tlb_pf_st[w].eq(tlb.resp[w].valid & tlb.resp[w].bits.pf.st
                                   & s1_exc_uops[w].uses_stq),
                s1_tlb_ae_ld[w].eq(tlb.resp[w].valid & tlb.resp[w].bits.ae.ld
                                   & s1_exc_uops[w].uses_ldq),
                s1_tlb_ae_st[w].eq(tlb.resp[w].valid & tlb.resp[w].bits.ae.st
                                   & s1_exc_uops[w].uses_stq),
                s1_exc_valids[w].eq((s1_tlb_pf_ld[w] | s1_tlb_pf_st[w]
                                     | s1_tlb_ae_ld[w] | s1_tlb_ae_st[w]
                                     | s1_ma_ld[w] | s1_ma_st[w])
                                    & ~exception_d1),
                s1_exc_causes[w].eq(
                    Mux(
                        s1_ma_ld[w], Cause.LOAD_MISALIGNED,
                        Mux(
                            s1_ma_st[w], Cause.STORE_MISALIGNED,
                            Mux(
                                s1_tlb_pf_ld[w], Cause.LOAD_PAGE_FAULT,
                                Mux(
                                    s1_tlb_pf_st[w], Cause.STORE_PAGE_FAULT,
                                    Mux(s1_tlb_ae_ld[w],
                                        Cause.LOAD_ACCESS_FAULT,
                                        Cause.STORE_ACCESS_FAULT)))))),
            ]

        m.d.comb += s1_exc_valid.eq(s1_exc_valids.any())
        s1_exc_cause = Signal(Cause)
        s1_exc_uop = MicroOp(self.params)
        s1_exc_vaddr = Signal.like(exec_tlb_vaddr[0])
        m.d.comb += [
            s1_exc_cause.eq(s1_exc_causes[0]),
            s1_exc_uop.eq(s1_exc_uops[0]),
            s1_exc_vaddr.eq(s1_exc_vaddrs[0]),
        ]

        s1_exc_found = s1_exc_valids[0]
        oldest_exc_rob_idx = s1_exc_uops[0].rob_idx
        for w in range(1, self.mem_width):
            cur_is_older = (s1_exc_valids[w] & is_older(
                s1_exc_uops[w].rob_idx, oldest_exc_rob_idx,
                self.rob_head_idx)) | ~s1_exc_found

            with m.If(cur_is_older):
                m.d.comb += [
                    s1_exc_cause.eq(s1_exc_causes[w]),
                    s1_exc_uop.eq(s1_exc_uops[w]),
                    s1_exc_vaddr.eq(s1_exc_vaddrs[w]),
                ]

            s1_exc_found |= s1_exc_valids[w]
            oldest_exc_rob_idx = Mux(cur_is_older, s1_exc_uops[w].rob_idx,
                                     oldest_exc_rob_idx)

        for w in range(self.mem_width):
            with m.If(s1_exc_valids[w]):
                with m.If(s1_exc_uops[w].uses_ldq):
                    m.d.sync += ldq[s1_exc_uops[w].ldq_idx].uop.exception.eq(1)
                with m.Else():
                    m.d.sync += stq[s1_exc_uops[w].stq_idx].uop.exception.eq(1)

        #
        # Clear ROB busy
        #

        clr_bsy_valids = Signal(self.mem_width)
        clr_bsy_rob_idx = [
            Signal.like(self.clear_busy[0].bits, name=f'clr_bsy_rob_idx{i}')
            for i in range(self.mem_width)
        ]
        clr_bsy_br_mask = [
            Signal.like(self.br_update.resolve_mask,
                        name=f'clr_bsy_br_mask{i}')
            for i in range(self.mem_width)
        ]

        for w in range(self.mem_width):
            m.d.sync += [
                clr_bsy_valids[w].eq(0),
                clr_bsy_rob_idx[w].eq(0),
                clr_bsy_br_mask[w].eq(0),
            ]

            with m.If(fired_stad_incoming[w]):
                m.d.sync += [
                    clr_bsy_valids[w].
                    eq(s1_stq_incoming_e[w].valid
                       & ~s1_stq_incoming_e[w].uop.is_amo
                       & ~s1_tlb_miss[w]
                       & ~self.br_update.uop_killed(s1_stq_incoming_e[w].uop)),
                    clr_bsy_rob_idx[w].eq(s1_stq_incoming_e[w].uop.rob_idx),
                    clr_bsy_br_mask[w].eq(
                        self.br_update.get_new_br_mask(
                            s1_stq_incoming_e[w].uop.br_mask)),
                ]
            with m.Elif(fired_sta_incoming[w]):
                m.d.sync += [
                    clr_bsy_valids[w].
                    eq(s1_stq_incoming_e[w].valid
                       & s1_stq_incoming_e[w].data_valid
                       & ~s1_stq_incoming_e[w].uop.is_amo
                       & ~s1_tlb_miss[w]
                       & ~self.br_update.uop_killed(s1_stq_incoming_e[w].uop)),
                    clr_bsy_rob_idx[w].eq(s1_stq_incoming_e[w].uop.rob_idx),
                    clr_bsy_br_mask[w].eq(
                        self.br_update.get_new_br_mask(
                            s1_stq_incoming_e[w].uop.br_mask)),
                ]
            with m.Elif(fired_std_incoming[w]):
                m.d.sync += [
                    clr_bsy_valids[w].
                    eq(s1_stq_incoming_e[w].valid
                       & s1_stq_incoming_e[w].addr_valid
                       & ~stq[s1_stq_incoming_e[w].uop.stq_idx].is_vaddr
                       & ~s1_stq_incoming_e[w].uop.is_amo
                       & ~self.br_update.uop_killed(s1_stq_incoming_e[w].uop)),
                    clr_bsy_rob_idx[w].eq(s1_stq_incoming_e[w].uop.rob_idx),
                    clr_bsy_br_mask[w].eq(
                        self.br_update.get_new_br_mask(
                            s1_stq_incoming_e[w].uop.br_mask)),
                ]
            with m.Elif(fired_sfence[w]):
                m.d.sync += [
                    clr_bsy_valids[w].eq(s1_sfence_valid[w]),
                    clr_bsy_rob_idx[w].eq(s1_incoming_uop[w].rob_idx),
                    clr_bsy_br_mask[w].eq(
                        self.br_update.get_new_br_mask(
                            s1_incoming_uop[w].br_mask)),
                ]
            with m.Elif(fired_sta_retry[w]):
                m.d.sync += [
                    clr_bsy_valids[w].eq(
                        s1_stq_retry_e[w].valid
                        & s1_stq_retry_e[w].data_valid
                        & ~s1_stq_retry_e[w].uop.is_amo
                        & ~s1_tlb_miss[w]
                        & ~self.br_update.uop_killed(s1_stq_retry_e[w].uop)),
                    clr_bsy_rob_idx[w].eq(s1_stq_retry_e[w].uop.rob_idx),
                    clr_bsy_br_mask[w].eq(
                        self.br_update.get_new_br_mask(
                            s1_stq_retry_e[w].uop.br_mask)),
                ]

            m.d.comb += [
                self.clear_busy[w].valid.eq(
                    clr_bsy_valids[w]
                    & ~self.br_update.br_mask_killed(clr_bsy_br_mask[w])
                    & ~self.exception & ~exception_d1 & ~exception_d2),
                self.clear_busy[w].bits.eq(clr_bsy_rob_idx[w]),
            ]

        stdf_clr_bsy_valid = Signal()
        stdf_clr_bsy_rob_idx = Signal.like(self.clear_busy[0].bits)
        stdf_clr_bsy_br_mask = Signal.like(self.br_update.resolve_mask)

        m.d.sync += [
            stdf_clr_bsy_valid.eq(0),
            stdf_clr_bsy_rob_idx.eq(0),
            stdf_clr_bsy_br_mask.eq(0),
        ]

        with m.If(fired_stdf_incoming):
            stq_idx = fired_stdf_uop.stq_idx

            m.d.sync += [
                stdf_clr_bsy_valid.eq(
                    stq[stq_idx].valid
                    & stq[stq_idx].addr_valid
                    & ~stq_is_vaddr[stq_idx]
                    & ~self.br_update.uop_killed(fired_stdf_uop)),
                stdf_clr_bsy_rob_idx.eq(fired_stdf_uop.rob_idx),
                stdf_clr_bsy_br_mask.eq(
                    self.br_update.get_new_br_mask(fired_stdf_uop.br_mask)),
            ]

        m.d.comb += [
            self.clear_busy[self.mem_width].valid.eq(
                stdf_clr_bsy_valid
                & ~self.br_update.br_mask_killed(stdf_clr_bsy_br_mask)
                & ~self.exception & ~exception_d1 & ~exception_d2),
            self.clear_busy[self.mem_width].bits.eq(stdf_clr_bsy_rob_idx),
        ]

        #
        # Memory ordering
        #

        do_ld_search = Cat(
            ((fired_load_incoming[w] | fired_load_retry[w]) & ~s1_tlb_miss[w])
            | fired_load_wakeup[w] for w in range(self.mem_width))
        do_st_search = Cat((fired_sta_incoming[w] | fired_stad_incoming[w]
                            | fired_sta_retry[w]) & ~s1_tlb_miss[w]
                           for w in range(self.mem_width))

        cam_addr = [
            Signal(32, name=f'cam_addr{i}') for i in range(self.mem_width)
        ]
        cam_uop = [
            MicroOp(self.params, name=f'cam_uop{i}')
            for i in range(self.mem_width)
        ]
        cam_mask = [
            Signal(8, name=f'cam_mask{i}') for i in range(self.mem_width)
        ]

        cam_ldq_idx = [
            Signal(range(self.ldq_size), name=f'cam_ldq_idx{i}')
            for i in range(self.mem_width)
        ]

        cam_stq_idx = [
            Signal(range(self.stq_size), name=f'cam_stq_idx{i}')
            for i in range(self.mem_width)
        ]

        can_forward = Signal(self.mem_width)

        for w in range(self.mem_width):
            m.d.comb += [
                cam_addr[w].eq(
                    Mux(
                        fired_load_incoming[w] | fired_load_retry[w]
                        | fired_sta_incoming[w] | fired_stad_incoming[w]
                        | fired_sta_retry[w], s1_tlb_paddr[w],
                        s1_mem_addr[w])),
                cam_uop[w].eq(
                    Mux(do_st_search[w], s1_stq_e[w].uop,
                        Mux(do_ld_search[w], s1_ldq_e[w].uop, 0))),
                cam_mask[w].eq(gen_byte_mask(cam_addr[w],
                                             cam_uop[w].mem_size)),
                cam_ldq_idx[w].eq(
                    Mux(
                        fired_load_incoming[w], s1_incoming_uop[w].ldq_idx,
                        Mux(fired_load_wakeup[w], s1_ldq_wakeup_idx,
                            Mux(fired_load_retry[w], s1_ldq_retry_idx, 0)))),
                cam_stq_idx[w].eq(
                    Mux(fired_sta_incoming[w] | fired_stad_incoming[w],
                        s1_incoming_uop[w].stq_idx,
                        Mux(fired_sta_retry[w], s1_stq_retry_idx, 0))),
                can_forward[w].eq(
                    Mux(fired_load_incoming[w] | fired_load_retry[w],
                        ~s1_tlb_uncacheable[w],
                        ~ldq[cam_ldq_idx[w]].addr_uncacheable)),
            ]

        ldst_addr_matches = [
            Signal(self.stq_size, name=f'ldst_addr_matches{i}')
            for i in range(self.mem_width)
        ]
        ldst_forward_matches = [
            Signal(self.stq_size, name=f'ldst_forward_matches{i}')
            for i in range(self.mem_width)
        ]

        failed_loads = Signal(self.ldq_size)

        s1_forward_valid = Signal(self.mem_width)
        s1_forward_stq_idx = [
            Signal(range(self.stq_size), name=f's1_forward_stq_idx{i}')
            for i in range(self.mem_width)
        ]

        s2_forward_valid = Signal(self.mem_width)
        s2_forward_ldq_idx = [
            Signal(range(self.ldq_size), name=f's2_forward_ldq_idx{i}')
            for i in range(self.mem_width)
        ]
        s2_forward_stq_idx = [
            Signal(range(self.stq_size), name=f's2_forward_stq_idx{i}')
            for i in range(self.mem_width)
        ]

        for i in range(self.ldq_size):
            addr_matches = Cat(ldq_addr[i][3:] == cam_addr[w][3:]
                               for w in range(self.mem_width))

            load_mask = gen_byte_mask(ldq_addr[i], ldq[i].uop.mem_size)

            load_forwarders = Cat(s2_forward_valid[w]
                                  & (s2_forward_ldq_idx[w] == i)
                                  for w in range(self.mem_width))
            load_forward_stq_idx = Signal(range(self.stq_size))

            m.d.comb += load_forward_stq_idx.eq(ldq[i].forward_stq_idx)
            for w in range(self.mem_width):
                with m.If(load_forwarders[w]):
                    m.d.comb += load_forward_stq_idx.eq(s2_forward_stq_idx[w])

            for w in range(self.mem_width):
                with m.If(do_st_search[w] & ldq[i].valid & ldq[i].addr_valid
                          & (ldq[i].executed | ldq[i].succeeded
                             | (load_forwarders != 0))
                          & ~ldq_is_vaddr[i]
                          & ((ldq[i].st_dep_mask & (1 << cam_stq_idx[w])) != 0)
                          & addr_matches[w]
                          & ((load_mask & cam_mask[w]) != 0)):

                    forwarder_is_older = is_older(load_forward_stq_idx,
                                                  cam_stq_idx[w],
                                                  ldq[i].next_stq_idx)

                    with m.If(~ldq[i].forwarded
                              | ((load_forward_stq_idx != cam_stq_idx[w])
                                 & forwarder_is_older)):
                        m.d.comb += failed_loads[i].eq(1)
                        m.d.sync += ldq[i].order_fail.eq(1)

                with m.If(do_ld_search[w] & ldq[i].valid & ldq[i].addr_valid
                          & ~ldq_is_vaddr[i] & addr_matches[w]
                          & ((load_mask & cam_mask[w]) != 0)):
                    searcher_is_older = is_older(cam_ldq_idx[w], i, ldq_head)
                    with m.If(~searcher_is_older & (cam_ldq_idx[w] != i)):
                        with m.If(~(ldq[i].executed & ldq[i].succeeded)):
                            m.d.comb += [
                                can_forward[w].eq(0),
                                dcache.s1_kill[w].eq(dmem_req_fired[w]),
                                s1_set_executed[cam_ldq_idx[w]].eq(0),
                            ]

        for i in range(self.stq_size):
            addr_matches = Cat((stq[i].addr_valid & ~stq_is_vaddr[i]
                                & (stq_addr[i][3:] == cam_addr[w][3:]))
                               for w in range(self.mem_width))

            write_mask = gen_byte_mask(stq_addr[i], stq[i].uop.mem_size)

            for w in range(self.mem_width):
                with m.If(do_ld_search[w] & stq[i].valid
                          & s1_ldq_e[w].st_dep_mask[i]):
                    with m.If(((cam_mask[w] & write_mask) == cam_mask[w])
                              & addr_matches[w] & ~stq[i].uop.is_fence
                              & can_forward[w]):
                        m.d.comb += [
                            ldst_addr_matches[w][i].eq(1),
                            ldst_forward_matches[w][i].eq(1),
                            dcache.s1_kill[w].eq(dmem_req_fired[w]),
                            s1_set_executed[cam_ldq_idx[w]].eq(0),
                        ]
                    with m.Elif(((cam_mask[w] & write_mask) != 0)
                                & addr_matches[w]):
                        m.d.comb += [
                            ldst_addr_matches[w][i].eq(1),
                            dcache.s1_kill[w].eq(dmem_req_fired[w]),
                            s1_set_executed[cam_ldq_idx[w]].eq(0),
                        ]
                    with m.Elif(stq[i].uop.is_fence | stq[i].uop.is_amo):
                        m.d.comb += [
                            ldst_addr_matches[w][i].eq(1),
                            dcache.s1_kill[w].eq(dmem_req_fired[w]),
                            s1_set_executed[cam_ldq_idx[w]].eq(0),
                        ]

        for w in range(self.mem_width):
            enc = RRPriorityEncoder(self.stq_size, is_head=False)
            m.submodules += enc
            m.d.comb += [
                enc.i.eq(ldst_addr_matches[w]),
                enc.head_or_tail.eq(cam_uop[w].stq_idx),
                s1_forward_stq_idx[w].eq(enc.o),
                s1_forward_valid[w].eq(((ldst_forward_matches[w]
                                         & (1 << s1_forward_stq_idx[w])) != 0)
                                       & ~self.br_update.uop_killed(cam_uop[w])
                                       & ~self.exception & ~exception_d1),
            ]

            m.d.sync += [
                s2_forward_valid[w].eq(s1_forward_valid[w]),
                s2_forward_ldq_idx[w].eq(cam_ldq_idx[w]),
                s2_forward_stq_idx[w].eq(s1_forward_stq_idx[w]),
            ]

        ld_fail_enc = RRPriorityEncoder(self.ldq_size)
        m.submodules += ld_fail_enc
        m.d.comb += [
            ld_fail_enc.i.eq(failed_loads),
            ld_fail_enc.head_or_tail.eq(ldq_head),
        ]

        #
        # Throw exception
        #

        exc_valid = Signal()

        ld_exc_valid = ~ld_fail_enc.n
        ld_exc_uop = ldq[ld_fail_enc.o].uop

        use_mem_exc = (s1_exc_valid
                       & is_older(s1_exc_uop.rob_idx, ld_exc_uop.rob_idx,
                                  self.rob_head_idx)) | ~ld_exc_valid
        exc_uop = MicroOp(self.params)
        m.d.comb += exc_uop.eq(Mux(use_mem_exc, s1_exc_uop, ld_exc_uop))

        m.d.sync += [
            exc_valid.eq((ld_exc_valid | s1_exc_valid) & ~self.exception
                         & ~self.br_update.uop_killed(exc_uop)),
            self.lsu_exc.bits.uop.eq(exc_uop),
            self.lsu_exc.bits.uop.br_mask.eq(
                self.br_update.get_new_br_mask(exc_uop.br_mask)),
            self.lsu_exc.bits.cause.eq(
                Mux(use_mem_exc, s1_exc_cause, Cause.MEM_ORDERING_FAULT)),
            self.lsu_exc.bits.badaddr.eq(s1_exc_vaddr),
        ]

        m.d.comb += self.lsu_exc.valid.eq(
            exc_valid & ~self.exception
            & ~self.br_update.uop_killed(self.lsu_exc.bits.uop))

        #
        # Writeback
        #

        dmem_resp_fire = Signal(self.mem_width)

        for w in range(self.mem_width):
            with m.If(dcache.nack[w].valid):
                with m.If(dcache.nack[w].bits.from_core):
                    pass
                with m.Elif(dcache.nack[w].bits.uop.uses_ldq):
                    m.d.sync += ldq[
                        dcache.nack[w].bits.uop.ldq_idx].executed.eq(0)
                with m.Elif(
                        is_older(dcache.nack[w].bits.uop.stq_idx,
                                 stq_execute_head, stq_head)):
                    m.d.sync += stq_execute_head.eq(
                        dcache.nack[w].bits.uop.stq_idx)

            with m.If(dcache.resp[w].valid):
                with m.If(dcache.resp[w].bits.uop.uses_ldq):
                    ldq_idx = dcache.resp[w].bits.uop.ldq_idx
                    iresp = self.exec_iresps[w]
                    fresp = self.exec_fresps[w]

                    m.d.comb += [
                        iresp.bits.uop.eq(ldq[ldq_idx].uop),
                        fresp.bits.uop.eq(ldq[ldq_idx].uop),
                        iresp.valid.eq(
                            ldq[ldq_idx].uop.dst_rtype == RegisterType.FIX),
                        fresp.valid.eq(
                            ldq[ldq_idx].uop.dst_rtype == RegisterType.FLT),
                        iresp.bits.data.eq(dcache.resp[w].bits.data),
                        fresp.bits.data.eq(dcache.resp[w].bits.data),
                        dmem_resp_fire[w].eq(1),
                    ]

                    m.d.sync += ldq[ldq_idx].succeeded.eq(iresp.valid
                                                          | fresp.valid)
                with m.If(dcache.resp[w].bits.uop.uses_stq):
                    stq_idx = dcache.resp[w].bits.uop.stq_idx
                    m.d.comb += dmem_resp_fire[w].eq(1)
                    m.d.sync += stq[stq_idx].succeeded.eq(1)

                    with m.If(dcache.resp[w].bits.uop.is_amo):
                        m.d.comb += [
                            self.exec_iresps[w].valid.eq(1),
                            self.exec_iresps[w].bits.uop.eq(stq[stq_idx].uop),
                            self.exec_iresps[w].bits.data.eq(
                                dcache.resp[w].bits.data),
                        ]

            with m.If(~dmem_resp_fire[w] & s2_forward_valid[w]):
                ldq_e = ldq[s2_forward_ldq_idx[w]]
                stq_e = stq[s2_forward_stq_idx[w]]

                data_valid = stq_e.data_valid
                live = ~self.br_update.uop_killed(ldq_e.uop)

                load_gen = LoadGen(max_size=8)
                store_gen = StoreGen(max_size=8)
                m.submodules += [load_gen, store_gen]

                m.d.comb += [
                    store_gen.typ.eq(stq_e.uop.mem_size),
                    store_gen.addr.eq(stq_e.addr),
                    store_gen.data_in.eq(stq_e.data),
                    load_gen.typ.eq(ldq_e.uop.mem_size),
                    load_gen.signed.eq(ldq_e.uop.mem_signed),
                    load_gen.addr.eq(ldq_e.addr),
                    load_gen.data_in.eq(store_gen.data_out),
                ]

                m.d.comb += [
                    iresp.bits.uop.eq(ldq_e.uop),
                    iresp.bits.data.eq(load_gen.data_out),
                    iresp.valid.eq((ldq_e.uop.dst_rtype == RegisterType.FIX)
                                   & data_valid & live),
                ]

                with m.If(data_valid & live):
                    m.d.sync += [
                        ldq_e.succeeded.eq(1),
                        ldq_e.forwarded.eq(1),
                        ldq_e.forward_stq_idx.eq(s2_forward_stq_idx[w]),
                    ]

        #
        # Branch mispredict
        #

        st_brkilled_mask = Signal(self.stq_size)

        for i in range(self.stq_size):
            with m.If(stq[i].valid):
                m.d.sync += stq[i].uop.br_mask.eq(
                    self.br_update.get_new_br_mask(stq[i].uop.br_mask))

                with m.If(self.br_update.uop_killed(stq[i].uop)):
                    m.d.sync += [
                        stq[i].valid.eq(0),
                        stq[i].addr_valid.eq(0),
                        stq[i].data_valid.eq(0),
                    ]
                    m.d.comb += st_brkilled_mask[i].eq(1)

        for i in range(self.ldq_size):
            with m.If(ldq[i].valid):
                m.d.sync += ldq[i].uop.br_mask.eq(
                    self.br_update.get_new_br_mask(ldq[i].uop.br_mask))

                with m.If(self.br_update.uop_killed(ldq[i].uop)):
                    m.d.sync += [
                        ldq[i].valid.eq(0),
                        ldq[i].addr_valid.eq(0),
                    ]

        with m.If(self.br_update.br_res.mispredict & ~self.exception):
            m.d.sync += [
                stq_tail.eq(self.br_update.br_res.uop.stq_idx),
                ldq_tail.eq(self.br_update.br_res.uop.ldq_idx),
            ]

        #
        # Commit
        #

        next_ldq_head = ldq_head
        next_stq_commit_head = stq_commit_head

        for w in range(self.core_width):
            commit_store = self.commit.valids[w] & self.commit.uops[w].uses_stq
            commit_load = self.commit.valids[w] & self.commit.uops[w].uses_ldq
            idx = Mux(commit_store, next_stq_commit_head, next_ldq_head)

            with m.If(commit_store):
                m.d.sync += stq[idx].committed.eq(1)
            with m.Elif(commit_load):
                m.d.sync += [
                    ldq[idx].valid.eq(0),
                    ldq[idx].addr_valid.eq(0),
                    ldq[idx].executed.eq(0),
                    ldq[idx].succeeded.eq(0),
                    ldq[idx].order_fail.eq(0),
                    ldq[idx].forwarded.eq(0),
                ]

            next_stq_commit_head = Mux(
                commit_store, wrap_incr(next_stq_commit_head, self.stq_size),
                next_stq_commit_head)
            next_ldq_head = Mux(commit_load,
                                wrap_incr(next_ldq_head, self.ldq_size),
                                next_ldq_head)

        m.d.sync += [
            stq_commit_head.eq(next_stq_commit_head),
            ldq_head.eq(next_ldq_head),
        ]

        with m.If(stq[stq_head].valid & stq[stq_head].committed):
            m.d.comb += clear_store.eq(stq[stq_head].uop.is_fence
                                       | stq[stq_head].succeeded)

        with m.If(clear_store):
            m.d.sync += [
                stq[stq_head].valid.eq(0),
                stq[stq_head].addr_valid.eq(0),
                stq[stq_head].data_valid.eq(0),
                stq[stq_head].committed.eq(0),
                stq[stq_head].succeeded.eq(0),
                stq_head.eq(wrap_incr(stq_head, self.stq_size)),
            ]

            with m.If(stq[stq_head].uop.is_fence):
                m.d.sync += stq_execute_head.eq(
                    wrap_incr(stq_execute_head, self.stq_size))

        #
        # Core request
        #

        with m.Switch(core_req_state):
            with m.Case(_CoreRequestState.READY):
                m.d.comb += self.core_req.ready.eq(1)

                with m.If(self.core_req.fire):
                    m.d.sync += [
                        core_req.eq(self.core_req.bits),
                        core_req_state.eq(_CoreRequestState.S1),
                    ]

            with m.Case(_CoreRequestState.S1):
                m.d.comb += can_fire_core_incoming[-1].eq(1)

                with m.If(will_fire_core_incoming[-1] & dcache.req[-1].fire):
                    m.d.sync += core_req_state.eq(_CoreRequestState.S2)
                with m.Else():
                    m.d.sync += core_req_state.eq(_CoreRequestState.S2_NACK)

            with m.Case(_CoreRequestState.S2_NACK):
                m.d.comb += self.core_nack.eq(1)
                m.d.sync += core_req_state.eq(_CoreRequestState.READY)

            with m.Case(_CoreRequestState.S2):
                m.d.sync += core_req_state.eq(_CoreRequestState.WAIT)

            with m.Case(_CoreRequestState.WAIT):
                for w in range(self.mem_width):
                    with m.If(dcache.resp[w].valid
                              & dcache.resp[w].bits.from_core):
                        m.d.comb += [
                            self.core_resp.valid.eq(1),
                            self.core_resp.bits.has_data.eq(1),
                            self.core_resp.bits.data.eq(
                                dcache.resp[w].bits.data),
                        ]
                        m.d.sync += core_req_state.eq(_CoreRequestState.READY)
                    with m.Elif(dcache.nack[w].valid
                                & dcache.nack[w].bits.from_core):
                        m.d.sync += core_req_state.eq(_CoreRequestState.REPLAY)

            with m.Case(_CoreRequestState.REPLAY):
                m.d.comb += can_fire_core_retry[-1].eq(1)

                with m.If(will_fire_core_retry[-1] & dcache.req[-1].fire):
                    m.d.sync += core_req_state.eq(_CoreRequestState.WAIT)

            with m.Case(_CoreRequestState.DEAD):
                for w in range(self.mem_width):
                    with m.If(dcache.resp[w].valid
                              & dcache.resp[w].bits.from_core):
                        m.d.sync += core_req_state.eq(_CoreRequestState.READY)

        #
        # Exception
        #

        st_exc_killed_mask = Signal(self.stq_size)

        with m.If(self.exception):
            m.d.sync += [
                ldq_head.eq(0),
                ldq_tail.eq(0),
            ]

            for i in range(self.ldq_size):
                m.d.sync += [
                    ldq[i].valid.eq(0),
                    ldq[i].addr_valid.eq(0),
                    ldq[i].executed.eq(0),
                ]

            m.d.sync += stq_tail.eq(stq_commit_head)
            for i in range(self.stq_size):
                with m.If(~stq[i].committed & ~stq[i].succeeded):
                    m.d.sync += [
                        stq[i].valid.eq(0),
                        stq[i].addr_valid.eq(0),
                        stq[i].data_valid.eq(0),
                    ]

                    m.d.comb += st_exc_killed_mask[i].eq(1)

        m.d.sync += live_store_mask.eq(next_live_store_mask
                                       & ~st_brkilled_mask
                                       & ~st_exc_killed_mask)

        return m
