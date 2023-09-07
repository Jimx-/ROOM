from amaranth import *
from amaranth.hdl.rec import DIR_FANOUT

from groom.csr import AutoCSR, CSR, BankedCSR, CSRAccess
import groom.csrnames as gpucsrnames

from room.types import HasCoreParams, MicroOp
from room.icache import ICache

from roomsoc.interconnect.stream import Valid, Decoupled


class IFetchReq(HasCoreParams, Record):

    def __init__(self, params, name=None, src_loc_at=0):
        HasCoreParams.__init__(self, params)

        Record.__init__(self, [
            ('wid', range(self.n_warps), DIR_FANOUT),
            ('pc', 32, DIR_FANOUT),
            ('tmask', self.n_threads, DIR_FANOUT),
        ],
                        name=name,
                        src_loc_at=1 + src_loc_at)


class WarpStallReq(HasCoreParams, Record):

    def __init__(self, params, name=None, src_loc_at=0):
        HasCoreParams.__init__(self, params)

        Record.__init__(self, [
            ('wid', range(self.n_warps), DIR_FANOUT),
            ('stall', 1, DIR_FANOUT),
        ],
                        name=name,
                        src_loc_at=1 + src_loc_at)


class WarpControlReq(HasCoreParams, Record):

    def __init__(self, params, name=None, src_loc_at=0):
        HasCoreParams.__init__(self, params)

        Record.__init__(self, [
            ('wid', range(self.n_warps), DIR_FANOUT),
            ('tmc', [
                ('valid', 1, DIR_FANOUT),
                ('mask', self.n_threads, DIR_FANOUT),
            ]),
            ('wspawn', [
                ('valid', 1, DIR_FANOUT),
                ('mask', self.n_warps, DIR_FANOUT),
                ('pc', 32, DIR_FANOUT),
            ]),
            ('split', [
                ('valid', 1, DIR_FANOUT),
                ('diverged', 1, DIR_FANOUT),
                ('then_mask', self.n_threads, DIR_FANOUT),
                ('else_mask', self.n_threads, DIR_FANOUT),
                ('pc', 32, DIR_FANOUT),
            ]),
            ('barrier', [
                ('valid', 1, DIR_FANOUT),
                ('id', range(self.n_barriers), DIR_FANOUT),
                ('count', range(self.n_warps), DIR_FANOUT),
            ]),
        ],
                        name=name,
                        src_loc_at=1 + src_loc_at)


class BranchResolution(HasCoreParams, Record):

    def __init__(self, params, name=None, src_loc_at=0):
        HasCoreParams.__init__(self, params)

        Record.__init__(self, [
            ('wid', range(self.n_warps), DIR_FANOUT),
            ('taken', 1, DIR_FANOUT),
            ('target', 32, DIR_FANOUT),
        ],
                        name=name,
                        src_loc_at=1 + src_loc_at)


class IPDomStack(HasCoreParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.layout = [
            ('pc', 32),
            ('mask', self.n_threads),
        ]

        self.w_data1 = Record(self.layout)
        self.w_data2 = Record(self.layout)
        self.w_index = Signal()
        self.w_en = Signal()

        self.r_data = Record(self.layout)
        self.r_index = Signal()
        self.r_en = Signal()

    def elaborate(self, platform):
        m = Module()

        mem = Memory(depth=self.n_threads, width=2 * len(self.r_data))
        r_ptr = Signal(range(self.n_threads + 1))
        w_ptr = Signal(range(self.n_threads + 1))

        index = Array(Signal(name=f'index{i}') for i in range(self.n_threads))

        with m.If(self.w_en):
            m.d.sync += [
                index[w_ptr].eq(self.w_index),
                r_ptr.eq(w_ptr),
                w_ptr.eq(w_ptr + 1),
            ]
        with m.Elif(self.r_en):
            m.d.sync += [
                index[r_ptr].eq(1),
                w_ptr.eq(w_ptr - index[r_ptr]),
                r_ptr.eq(r_ptr - index[r_ptr]),
            ]

        m.d.comb += self.r_index.eq(index[r_ptr])

        rport = m.submodules.rport = mem.read_port(domain='comb')
        m.d.comb += [
            rport.addr.eq(r_ptr),
            self.r_data.eq(
                Mux(self.r_index, rport.data[len(self.r_data):],
                    rport.data[:len(self.r_data)])),
        ]

        wport = m.submodules.wport = mem.write_port()
        m.d.comb += [
            wport.addr.eq(w_ptr),
            wport.data.eq(Cat(self.w_data1, self.w_data2)),
            wport.en.eq(self.w_en),
        ]

        return m


class WarpScheduler(HasCoreParams, AutoCSR, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.reset_vector = Signal(32)

        self.ifetch_req = Decoupled(IFetchReq, params)

        self.stall_req = Valid(WarpStallReq, params)
        self.br_res = Valid(BranchResolution, params)
        self.warp_ctrl = Valid(WarpControlReq, params)
        self.join_req = Valid(Signal, range(self.n_warps))

        self.tmask = BankedCSR(CSR, gpucsrnames.tmask,
                               [('value', self.n_threads, CSRAccess.RO)],
                               params)

        self.busy = Signal()

    def elaborate(self, platform):
        m = Module()

        reset = Signal(reset=1)
        reset_d1 = Signal(reset_less=True)
        m.d.sync += [
            reset.eq(0),
            reset_d1.eq(reset),
        ]

        req_stall = (self.ifetch_req.valid & ~self.ifetch_req.ready) | reset

        active_warps = Signal(self.n_warps, reset=1)
        stalled_warps = Signal(self.n_warps)
        ready_warps = Signal(self.n_warps)
        barrier_masks = Array(
            Signal(self.n_warps, name=f'barrier_mask{i}')
            for i in range(self.n_barriers))

        with m.If(self.warp_ctrl.valid & self.warp_ctrl.bits.wspawn.valid):
            m.d.sync += active_warps.eq(self.warp_ctrl.bits.wspawn.mask)
        with m.Elif(self.warp_ctrl.valid & self.warp_ctrl.bits.tmc.valid):
            with m.Switch(self.warp_ctrl.bits.wid):
                for w in range(self.n_threads):
                    with m.Case(w):
                        m.d.sync += active_warps[w].eq(
                            self.warp_ctrl.bits.tmc.mask.any())

        barrier_stall_mask = 0
        for b in range(self.n_barriers):
            barrier_stall_mask |= barrier_masks[b]

        m.d.comb += ready_warps.eq(active_warps
                                   & ~(stalled_warps | barrier_stall_mask))

        schedule_wid = Signal(range(self.n_warps))
        schedule_valid = Signal()
        for i in reversed(range(self.n_warps)):
            with m.If(ready_warps[i]):
                m.d.comb += [
                    schedule_wid.eq(i),
                    schedule_valid.eq(1),
                ]

        wspawn_valid = Signal(self.n_warps)
        wspawn_pc = [
            Signal(32, name=f'wspawn_pc{i}') for i in range(self.n_warps)
        ]

        thread_masks = Array(
            Signal(self.n_threads, name=f'thread_mask{i}', reset=int(i == 0))
            for i in range(self.n_warps))
        warp_pcs = Array(
            Signal(32, name=f'warp_pcs{i}') for i in range(self.n_warps))

        for w in range(self.n_warps):
            m.d.comb += self.tmask.warps[w].r.eq(thread_masks[w])

        with m.If(reset_d1 & ~reset):
            m.d.comb += [
                wspawn_valid.eq(1),
                wspawn_pc[0].eq(self.reset_vector),
            ]
            m.d.sync += warp_pcs[0].eq(self.reset_vector)
        with m.Elif(self.warp_ctrl.valid & self.warp_ctrl.bits.wspawn.valid):
            m.d.comb += [
                wspawn_valid.eq(self.warp_ctrl.bits.wspawn.mask
                                & ~Const(1, self.n_warps)),
                Cat(*wspawn_pc).eq(
                    Repl(self.warp_ctrl.bits.wspawn.pc, self.n_warps)),
            ]

            for w in range(self.n_warps):
                with m.If(wspawn_valid[w]):
                    m.d.sync += [
                        warp_pcs[w].eq(wspawn_pc[w]),
                        thread_masks[w].eq(1),
                    ]

        with m.If(self.warp_ctrl.valid & self.warp_ctrl.bits.tmc.valid):
            with m.Switch(self.warp_ctrl.bits.wid):
                for i in range(self.n_warps):
                    with m.Case(i):
                        m.d.sync += [
                            thread_masks[i].eq(self.warp_ctrl.bits.tmc.mask),
                            stalled_warps[i].eq(0),
                        ]

        with m.If(self.warp_ctrl.valid & self.warp_ctrl.bits.barrier.valid):
            active_barrier_count = 0
            for b in barrier_masks[self.warp_ctrl.bits.barrier.id]:
                active_barrier_count += b

            with m.Switch(self.warp_ctrl.bits.wid):
                for i in range(self.n_warps):
                    with m.Case(i):
                        m.d.sync += stalled_warps[i].eq(0)

                        with m.If(active_barrier_count ==
                                  self.warp_ctrl.bits.barrier.count):
                            m.d.sync += barrier_masks[
                                self.warp_ctrl.bits.barrier.id].eq(0)
                        with m.Else():
                            m.d.sync += barrier_masks[
                                self.warp_ctrl.bits.barrier.id][i].eq(1)

        with m.If(self.warp_ctrl.valid & self.warp_ctrl.bits.split.valid):
            with m.Switch(self.warp_ctrl.bits.wid):
                for i in range(self.n_warps):
                    with m.Case(i):
                        m.d.sync += stalled_warps[i].eq(0)

                        with m.If(self.warp_ctrl.bits.split.diverged):
                            m.d.sync += thread_masks[i].eq(
                                self.warp_ctrl.bits.split.then_mask)

        with m.If(self.br_res.valid):
            with m.Switch(self.br_res.bits.wid):
                for i in range(self.n_warps):
                    with m.Case(i):
                        with m.If(self.br_res.bits.taken):
                            m.d.sync += warp_pcs[i].eq(self.br_res.bits.target)
                        m.d.sync += stalled_warps[i].eq(0)

        with m.If(schedule_valid & ~req_stall):
            for i in range(self.n_warps):
                with m.If(schedule_wid == i):
                    m.d.sync += stalled_warps[i].eq(1)

        with m.If(self.ifetch_req.fire):
            m.d.sync += warp_pcs[self.ifetch_req.bits.wid].eq(
                self.ifetch_req.bits.pc + 4)

        with m.If(self.stall_req.valid):
            for i in range(self.n_warps):
                with m.If(self.stall_req.bits.wid == i):
                    m.d.sync += stalled_warps[i].eq(self.stall_req.bits.stall)

        #
        # IPDom stack
        #

        for w in range(self.n_warps):
            stack = IPDomStack(self.params)
            setattr(m.submodules, f'ipdom{w}', stack)

            m.d.comb += [
                stack.w_en.eq(self.warp_ctrl.valid
                              & self.warp_ctrl.bits.split.valid
                              & (self.warp_ctrl.bits.wid == w)),
                stack.w_data1.pc.eq(self.warp_ctrl.bits.split.pc),
                stack.w_data1.mask.eq(self.warp_ctrl.bits.split.else_mask),
                stack.w_data2.mask.eq(thread_masks[w]),
                stack.w_index.eq(~self.warp_ctrl.bits.split.diverged),
            ]

            with m.If(self.join_req.valid & (self.join_req.bits == w)):
                m.d.comb += stack.r_en.eq(1)
                m.d.sync += thread_masks[w].eq(stack.r_data.mask)

                with m.If(~stack.r_index):
                    m.d.sync += warp_pcs[w].eq(stack.r_data.pc)

        #
        # Output
        #

        with m.If(~req_stall):
            m.d.sync += [
                self.ifetch_req.valid.eq(schedule_valid),
                self.ifetch_req.bits.wid.eq(schedule_wid),
            ]

            for i in range(self.n_warps):
                with m.If(schedule_wid == i):
                    m.d.sync += [
                        self.ifetch_req.bits.pc.eq(
                            Mux(wspawn_valid[i], wspawn_pc[i], warp_pcs[i])),
                        self.ifetch_req.bits.tmask.eq(
                            Mux(wspawn_valid[i], 1, thread_masks[i])),
                    ]

        m.d.comb += self.busy.eq(active_warps.any())

        return m


class FetchBundle(HasCoreParams):

    def __init__(self, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.wid = Signal(range(self.n_warps))
        self.uop = MicroOp(params, name=f'{name}_uop')

    def eq(self, rhs):
        ret = [
            self.uop.eq(rhs.uop),
            self.wid.eq(rhs.wid),
        ]

        return ret


class IFStage(HasCoreParams, AutoCSR, Elaboratable):

    def __init__(self, ibus, params):
        super().__init__(params)

        self.ibus = ibus

        self.reset_vector = Signal(32)

        self.fetch_packet = Decoupled(FetchBundle, params)

        self.stall_req = Valid(WarpStallReq, params)
        self.br_res = Valid(BranchResolution, params)
        self.warp_ctrl = Valid(WarpControlReq, params)
        self.join_req = Valid(Signal, range(self.n_warps))

        self._warp_sched = WarpScheduler(self.params)

        self.busy = Signal()

    def elaborate(self, platform):
        m = Module()

        icache = m.submodules.icache = ICache(self.ibus, self.params)

        warp_sched = m.submodules.warp_sched = self._warp_sched
        m.d.comb += [
            warp_sched.reset_vector.eq(self.reset_vector),
            warp_sched.stall_req.eq(self.stall_req),
            warp_sched.br_res.eq(self.br_res),
            warp_sched.warp_ctrl.eq(self.warp_ctrl),
            warp_sched.join_req.eq(self.join_req),
            self.busy.eq(warp_sched.busy),
        ]

        #
        # F0 - Next PC select
        #

        s0_valid = Signal()
        s0_vpc = Signal(32)
        s0_wid = Signal(range(self.n_warps))
        s0_tmask = Signal(self.n_threads)
        m.d.comb += [
            s0_valid.eq(warp_sched.ifetch_req.valid),
            s0_vpc.eq(warp_sched.ifetch_req.bits.pc),
            s0_wid.eq(warp_sched.ifetch_req.bits.wid),
            s0_tmask.eq(warp_sched.ifetch_req.bits.tmask),
            warp_sched.ifetch_req.ready.eq(1),
        ]

        m.d.comb += [
            icache.req.bits.addr.eq(s0_vpc),
            icache.req.valid.eq(s0_valid),
        ]

        #
        # F1 - ICache Access
        #

        s1_vpc = Signal.like(s0_vpc)
        s1_wid = Signal.like(s0_wid)
        s1_tmask = Signal.like(s0_tmask)
        s1_valid = Signal()
        f1_clear = Signal()

        m.d.sync += [
            s1_vpc.eq(s0_vpc),
            s1_wid.eq(s0_wid),
            s1_tmask.eq(s0_tmask),
            s1_valid.eq(s0_valid),
        ]

        s1_ppc = s1_vpc

        m.d.comb += [
            icache.s1_paddr.eq(s1_ppc),
            icache.s1_kill.eq(f1_clear),
        ]

        #
        # F2 - ICache Response
        #

        s2_vpc = Signal.like(s1_vpc)
        s2_wid = Signal.like(s1_wid)
        s2_tmask = Signal.like(s1_tmask)
        s2_valid = Signal()
        s2_ppc = Signal.like(s1_ppc)

        m.d.sync += [
            s2_vpc.eq(s1_vpc),
            s2_wid.eq(s1_wid),
            s2_tmask.eq(s1_tmask),
            s2_valid.eq(s1_valid & ~f1_clear),
            s2_ppc.eq(s1_ppc),
        ]

        with m.If((s2_valid & ~icache.resp.valid)
                  | (s2_valid & icache.resp.valid & ~self.fetch_packet.ready)):
            m.d.comb += [
                s0_valid.eq(1),
                s0_vpc.eq(s2_vpc),
                s0_wid.eq(s2_wid),
                s0_tmask.eq(s2_tmask),
                warp_sched.ifetch_req.ready.eq(0),
            ]

        m.d.comb += [
            self.fetch_packet.valid.eq(s2_valid & icache.resp.valid),
            self.fetch_packet.bits.uop.valid.eq(1),
            self.fetch_packet.bits.uop.inst.eq(icache.resp.bits.data),
            self.fetch_packet.bits.uop.pc.eq(s2_vpc),
            self.fetch_packet.bits.uop.tmask.eq(s2_tmask),
            self.fetch_packet.bits.wid.eq(s2_wid),
        ]

        return m
