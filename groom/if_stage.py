from amaranth import *
from amaranth.hdl.rec import DIR_FANOUT

from groom.csr import AutoCSR, BankedCSR, CSRAccess
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


class WarpScheduler(HasCoreParams, AutoCSR, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.reset_vector = Signal(32)

        self.ifetch_req = Decoupled(IFetchReq, params)

        self.stall_req = Valid(WarpStallReq, params)
        self.br_res = Valid(BranchResolution, params)

        self.tmask = BankedCSR(gpucsrnames.tmask,
                               [('value', self.n_threads, CSRAccess.RO)],
                               params)

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

        ready_warps = active_warps & ~stalled_warps

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
        warps_pc = Array(
            Signal(32, name=f'warps_pc{i}') for i in range(self.n_warps))

        for w in range(self.n_warps):
            m.d.comb += self.tmask.r[w].eq(thread_masks[w])

        with m.If(reset_d1 & ~reset):
            m.d.comb += [
                wspawn_valid.eq(1),
                wspawn_pc[0].eq(self.reset_vector),
            ]
            m.d.sync += warps_pc[0].eq(self.reset_vector)

        with m.If(self.br_res.valid):
            with m.Switch(self.br_res.bits.wid):
                for i in range(self.n_warps):
                    with m.Case(i):
                        with m.If(self.br_res.bits.taken):
                            m.d.sync += warps_pc[i].eq(self.br_res.bits.target)
                        m.d.sync += stalled_warps[i].eq(0)

        with m.If(schedule_valid & ~req_stall):
            for i in range(self.n_warps):
                with m.If(schedule_wid == i):
                    m.d.sync += stalled_warps[i].eq(1)

        with m.If(self.ifetch_req.fire):
            m.d.sync += warps_pc[self.ifetch_req.bits.wid].eq(
                self.ifetch_req.bits.pc + 4)

        with m.If(self.stall_req.valid):
            for i in range(self.n_warps):
                with m.If(self.stall_req.bits.wid == i):
                    m.d.sync += stalled_warps[i].eq(self.stall_req.bits.stall)

        with m.If(~req_stall):
            m.d.sync += [
                self.ifetch_req.valid.eq(schedule_valid),
                self.ifetch_req.bits.wid.eq(schedule_wid),
            ]

            for i in range(self.n_warps):
                with m.If(schedule_wid == i):
                    m.d.sync += [
                        self.ifetch_req.bits.pc.eq(
                            Mux(wspawn_valid[i], wspawn_pc[i], warps_pc[i])),
                        self.ifetch_req.bits.tmask.eq(
                            Mux(wspawn_valid[i], 1, thread_masks[i])),
                    ]

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

        self._warp_sched = WarpScheduler(self.params)

    def elaborate(self, platform):
        m = Module()

        icache = m.submodules.icache = ICache(self.ibus, self.params)

        warp_sched = m.submodules.warp_sched = self._warp_sched
        m.d.comb += [
            warp_sched.reset_vector.eq(self.reset_vector),
            warp_sched.stall_req.eq(self.stall_req),
            warp_sched.br_res.eq(self.br_res),
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
