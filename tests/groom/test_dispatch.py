import json
from pathlib import Path

import pytest
from amaranth import *
from amaranth.sim import Passive, Simulator

import room  # noqa
from groom.dispatch import Dispatcher
from groom.issue import Scoreboard
from groom.ex_stage import ALUExecUnit
from groom.lsu import LoadStoreUnit
from groom.regfile import RegisterFile, RegisterRead
from room.consts import (FUType, IssueQueueType, MemoryCommand, RegisterType,
                         UOpCode)
from room.types import HasCoreParams, MicroOp

TEST_PARAMS = {
    'is_groom': True,
    'xlen': 32,
    'flen': 32,
    'use_fpu': True,
    'vaddr_bits': 32,
    'io_regions': {},
    'fma_latency': 4,
    'n_cores': 1,
    'n_warps': 4,
    'n_threads': 4,
    'n_barriers': 4,
    'use_raster': False,
    'issue_params': {
        'queue_depth': 4,
    },
}


class DispatchIssue(HasCoreParams, Elaboratable):
    """Dispatcher plus integer and FP scoreboards, mimicking the core
    wiring: registered per-warp readiness hints gate arbitration, while
    sb_uop lookahead validates queued and passthrough grants before fire."""

    def __init__(self, params, use_fp=True):
        super().__init__(params)
        self.use_fp = use_fp

        self.dec_valid = Signal()
        self.dec_wid = Signal(range(self.n_warps))
        self.dec_uop = MicroOp(params)
        self.dec_ready = Signal()

        # Stand-in for the register-read stage ready.
        self.downstream_ready = Signal()

        self.wakeup_valid = Signal()
        self.wakeup_rtype = Signal(RegisterType)
        self.wakeup_wid = Signal(range(self.n_warps))
        self.wakeup_ldst = Signal(5)

    def elaborate(self, platform):
        m = Module()

        dispatcher = m.submodules.dispatcher = Dispatcher(self.params)
        isb = m.submodules.isb = Scoreboard(is_float=False, params=self.params)
        if self.use_fp:
            fsb = m.submodules.fsb = Scoreboard(is_float=True,
                                                params=self.params)

        fp_dis_ready = fsb.dis_ready if self.use_fp else 1
        fp_head_ready = (fsb.head_ready if self.use_fp else Const(
            -1, unsigned(self.n_warps)))

        m.d.comb += [
            dispatcher.dec_valid.eq(self.dec_valid),
            dispatcher.dec_wid.eq(self.dec_wid),
            dispatcher.dec_uop.eq(self.dec_uop),
            self.dec_ready.eq(dispatcher.dec_ready),
            dispatcher.lsu_occupied.eq(0),
            dispatcher.lsu_split_addr.eq(0),
            isb.dis_uop.eq(dispatcher.dis_uop),
            isb.dis_wid.eq(dispatcher.dis_wid),
            isb.sb_uop.eq(dispatcher.sb_uop),
            isb.sb_wid.eq(dispatcher.sb_wid),
            isb.dis_valid.eq(dispatcher.dis_valid
                             & self.downstream_ready & fp_dis_ready),
            isb.wakeup.valid.eq(self.wakeup_valid
                                & (self.wakeup_rtype == RegisterType.FIX)),
            isb.wakeup.bits.wid.eq(self.wakeup_wid),
            isb.wakeup.bits.ldst.eq(self.wakeup_ldst),
            dispatcher.head_ready.eq(isb.head_ready & fp_head_ready),
        ]
        for i in range(self.n_warps):
            m.d.comb += isb.head_uops[i].eq(dispatcher.head_uops[i])

        if self.use_fp:
            m.d.comb += [
                fsb.dis_uop.eq(dispatcher.dis_uop),
                fsb.dis_wid.eq(dispatcher.dis_wid),
                fsb.sb_uop.eq(dispatcher.sb_uop),
                fsb.sb_wid.eq(dispatcher.sb_wid),
                fsb.dis_valid.eq(dispatcher.dis_valid
                                 & self.downstream_ready
                                 & isb.dis_ready),
                fsb.wakeup.valid.eq(self.wakeup_valid
                                    & (self.wakeup_rtype == RegisterType.FLT)),
                fsb.wakeup.bits.wid.eq(self.wakeup_wid),
                fsb.wakeup.bits.ldst.eq(self.wakeup_ldst),
            ]
            for i in range(self.n_warps):
                m.d.comb += fsb.head_uops[i].eq(dispatcher.head_uops[i])

        fire = Signal()
        m.d.comb += [
            dispatcher.dis_ready.eq(isb.dis_ready & fp_dis_ready
                                    & self.downstream_ready),
            fire.eq(dispatcher.dis_valid & dispatcher.dis_ready),
        ]

        self.fire = fire
        self.dis_valid = dispatcher.dis_valid
        self.dis_uop = dispatcher.dis_uop
        self.dis_ready = dispatcher.dis_ready
        self.head_ready = dispatcher.head_ready
        self.isb_dis_ready = isb.dis_ready
        self.fsb_dis_ready = fp_dis_ready if isinstance(fp_dis_ready,
                                                        Value) else None
        self.sb_uop_id = dispatcher.sb_uop.uop_id

        return m


def make_dut(use_fp=True):
    return DispatchIssue(dict(TEST_PARAMS, use_fpu=use_fp), use_fp=use_fp)


def uop_fields(uop_id,
               dst=None,
               dst_rtype=RegisterType.FIX,
               rs1=None,
               rs1_rtype=RegisterType.FIX,
               rs2=None,
               rs2_rtype=RegisterType.FIX,
               rs3=None,
               frs3_en=True,
               iq_type=IssueQueueType.INT):
    # Always emit the complete register-field set: dec_uop is a shared
    # record whose fields persist across sends.
    fields = [
        ('uop_id', uop_id),
        ('opcode', UOpCode.ADDI),
        ('iq_type', iq_type),
        ('fu_type', FUType.ALU),
        ('tmask', 0b1111),
        ('ldst', dst if dst is not None else 0),
        ('ldst_valid', int(dst is not None)),
        ('dst_rtype', dst_rtype if dst is not None else RegisterType.X),
        ('lrs1', rs1 if rs1 is not None else 0),
        ('lrs1_rtype', rs1_rtype if rs1 is not None else RegisterType.X),
        ('lrs2', rs2 if rs2 is not None else 0),
        ('lrs2_rtype', rs2_rtype if rs2 is not None else RegisterType.X),
        ('lrs3', rs3 if rs3 is not None else 0),
        ('frs3_en', int(rs3 is not None and frs3_en)),
    ]
    return fields


def run_sim(dut, script, monitor=None):
    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(script)
    if monitor is not None:
        sim.add_sync_process(monitor)
    sim.run()


def make_env(dut):
    """Common process helpers: cycle-counted ticks, decode sends and
    wakeup pulses. Returns (tick, send, wakeup, fires, head0)."""
    fires = []
    head_hist = {}
    state = {'cycle': 0}

    def sample():
        valid = (yield dut.dis_valid)
        ready = (yield dut.dis_ready)
        if valid and ready:
            fires.append((state['cycle'], (yield dut.dis_uop.uop_id)))
        head_hist[state['cycle']] = (yield dut.head_ready) & 1

    def tick():
        yield from sample()
        yield
        state['cycle'] += 1

    def send(fields, wid):
        for name, value in fields:
            yield getattr(dut.dec_uop, name).eq(value)
        yield dut.dec_wid.eq(wid)
        yield dut.dec_valid.eq(1)
        assert (yield dut.dec_ready)
        yield from tick()
        yield dut.dec_valid.eq(0)

    def wakeup(wid, ldst, rtype=RegisterType.FIX):
        yield dut.wakeup_valid.eq(1)
        yield dut.wakeup_rtype.eq(rtype)
        yield dut.wakeup_wid.eq(wid)
        yield dut.wakeup_ldst.eq(ldst)
        yield from tick()
        yield dut.wakeup_valid.eq(0)

    def head0(cycle):
        return head_hist[cycle]

    return tick, send, wakeup, fires, head0


def init_process(dut, script):
    tick, send, wakeup, fires, head0 = make_env(dut)

    def process():
        yield dut.downstream_ready.eq(1)
        yield dut.dec_valid.eq(0)
        yield dut.wakeup_valid.eq(0)
        for _ in range(2):
            yield from tick()
        yield from script(tick, send, wakeup)
        for _ in range(4):
            yield from tick()

    return process, fires, head0


def test_busy_head_masked_until_wakeup():
    dut = make_dut()

    def script(tick, send, wakeup):
        # Warp 0 producer writing x5 fires one cycle after decode.
        yield from send(uop_fields(1, dst=5), 0)
        yield from tick()

        # Consumer at the warp 0 head: masked from the next cycle.
        yield from send(uop_fields(2, rs1=5), 0)
        yield from tick()

        # Ready sibling in warp 1 fires while warp 0 is masked.
        yield from send(uop_fields(3), 1)
        yield from tick()
        yield from tick()

        yield from wakeup(0, 5)

    process, fires, head0 = init_process(dut, script)
    run_sim(dut, process)

    assert fires == [(4, 1), (8, 3), (12, 2)]
    assert head0(6) == 0 and head0(8) == 0 and head0(10) == 0
    assert head0(11) == 1


# The integer scoreboard intentionally mirrors the existing FP rs3 check too.
# Exercise both busy tables, rather than assuming rs3 only consults FP state.
DEPENDENCIES = [
    pytest.param(RegisterType.FIX, {'rs1': 5}, id='int-rs1'),
    pytest.param(RegisterType.FIX, {'rs2': 5}, id='int-rs2'),
    pytest.param(RegisterType.FIX, {'dst': 5}, id='int-waw'),
    pytest.param(RegisterType.FLT, {
        'rs1': 5,
        'rs1_rtype': RegisterType.FLT
    },
                 id='fp-rs1'),
    pytest.param(RegisterType.FLT, {
        'rs2': 5,
        'rs2_rtype': RegisterType.FLT
    },
                 id='fp-rs2'),
    pytest.param(RegisterType.FLT, {
        'dst': 5,
        'dst_rtype': RegisterType.FLT
    },
                 id='fp-waw'),
    pytest.param(RegisterType.FLT, {
        'rs3': 5,
        'iq_type': IssueQueueType.FP
    },
                 id='fp-rs3'),
    pytest.param(RegisterType.FIX, {
        'rs3': 5,
        'iq_type': IssueQueueType.FP
    },
                 id='int-rs3'),
]


@pytest.mark.parametrize('rtype,consumer', DEPENDENCIES)
def test_fire_advance_grant_blocked_by_lookahead(rtype, consumer):
    dut = make_dut()

    def script(tick, send, wakeup):
        # Park the producer in the dispatch slot so the consumer
        # enqueues into the FIFO behind it.
        yield dut.downstream_ready.eq(0)
        yield from send(uop_fields(1, dst=5, dst_rtype=rtype), 0)
        yield from send(uop_fields(2, **consumer), 0)

        # Release: A fires; B may be granted (fire-advance) but must not
        # fire until the writeback wakeup clears x5.
        yield dut.downstream_ready.eq(1)
        yield from tick()
        yield from tick()
        # B occupies the slot this cycle, blocked by the sb_uop flags.
        valid = (yield dut.dis_valid)
        uop_id = (yield dut.dis_uop.uop_id)
        assert valid and uop_id == 2, 'B should occupy the slot after A'
        assert not (yield dut.dis_ready), 'B must be blocked by sb_uop'
        assert (yield dut.head_ready) & 1 == 0
        yield from tick()

        for _ in range(3):
            yield from tick()
        yield from wakeup(0, 5, rtype=rtype)

    process, fires, head0 = init_process(dut, script)
    run_sim(dut, process)

    assert fires == [(5, 1), (13, 2)]
    assert head0(6) == 0 and head0(7) == 0 and head0(11) == 0
    assert head0(12) == 1


def test_independent_back_to_back():
    dut = make_dut()

    def script(tick, send, wakeup):
        yield from send(uop_fields(1), 0)
        yield from send(uop_fields(2), 0)

    process, fires, head0 = init_process(dut, script)
    run_sim(dut, process)

    assert fires == [(4, 1), (5, 2)]


def test_busy_passthrough_stalls_then_yields_to_ready_warp():
    dut = make_dut()

    def script(tick, send, wakeup):
        yield from send(uop_fields(1, dst=5), 0)
        yield from tick()

        # Park a ready warp 1 uop in the slot with a second one queued.
        yield dut.downstream_ready.eq(0)
        yield from send(uop_fields(3), 1)
        yield from send(uop_fields(4), 1)

        # Phase 1 permits a busy passthrough to preempt the parked grant.
        # The actual-grant scoreboard must block it, then the readiness
        # hint must let warp 1 take the slot back on the next arbitration.
        yield from send(uop_fields(2, rs1=5), 0)
        yield from tick()
        assert (yield dut.dis_valid)
        assert (yield dut.dis_uop.uop_id) == 2
        assert not (yield dut.isb_dis_ready)

        # Warp 1 drains first; the bypassed uop stays buffered.
        yield dut.downstream_ready.eq(1)
        yield from tick()
        yield from tick()
        yield from tick()
        yield from wakeup(0, 5)

    process, fires, head0 = init_process(dut, script)
    run_sim(dut, process)

    assert fires == [(4, 1), (9, 3), (10, 4), (14, 2)]
    assert head0(8) == 0, 'bypassed uop accepted and masked'


def test_fp_raw_masked_until_fp_wakeup():
    dut = make_dut()

    def script(tick, send, wakeup):
        yield from send(
            uop_fields(1,
                       dst=5,
                       dst_rtype=RegisterType.FLT,
                       iq_type=IssueQueueType.FP), 0)
        yield from tick()

        yield from send(
            uop_fields(2,
                       rs1=5,
                       rs1_rtype=RegisterType.FLT,
                       iq_type=IssueQueueType.FP), 0)
        yield from tick()
        yield from tick()
        yield from tick()
        yield from wakeup(0, 5, rtype=RegisterType.FLT)

    process, fires, head0 = init_process(dut, script)
    run_sim(dut, process)

    assert fires == [(4, 1), (11, 2)]
    assert head0(6) == 0 and head0(9) == 0
    assert head0(10) == 1


def test_fix_busy_does_not_mask_fp_reader():
    dut = make_dut()

    def script(tick, send, wakeup):
        yield from send(uop_fields(1, dst=5), 0)
        yield from tick()
        yield from send(
            uop_fields(2,
                       rs1=5,
                       rs1_rtype=RegisterType.FLT,
                       iq_type=IssueQueueType.FP), 0)
        yield from tick()

    process, fires, head0 = init_process(dut, script)
    run_sim(dut, process)

    assert fires == [(4, 1), (6, 2)]


def test_integer_only_harness_dependency():
    dut = make_dut(use_fp=False)

    def script(tick, send, wakeup):
        yield from send(uop_fields(1, dst=5), 0)
        yield from tick()
        yield from send(uop_fields(2, rs1=5), 0)
        yield from tick()
        yield from tick()
        yield from tick()
        yield from wakeup(0, 5)

    process, fires, head0 = init_process(dut, script)
    run_sim(dut, process)

    assert fires == [(4, 1), (11, 2)]
    assert head0(6) == 0 and head0(10) == 1


@pytest.mark.parametrize('rtype,consumer', DEPENDENCIES)
@pytest.mark.parametrize('wid', [0, 3])
def test_passthrough_lookahead_checks_same_cycle_reservation(
        rtype, consumer, wid):
    dut = make_dut()
    events = []

    def monitor():
        yield Passive()
        while True:
            if (yield dut.fire):
                events.append((yield dut.dis_uop.uop_id))
            yield

    def script(tick, send, wakeup):
        # With an empty FIFO, B arrives exactly as A fires. busy_regs is
        # still clear, so sb_uop must consult busy_regs_n to block B's fire.
        yield from send(uop_fields(1, dst=5, dst_rtype=rtype), wid)
        yield from send(uop_fields(2, **consumer), wid)
        assert (yield dut.dis_valid)
        assert (yield dut.dis_uop.uop_id) == 1
        assert (yield dut.dis_ready)
        yield from tick()
        # Busy passthrough is allowed, but must never read stale operands.
        assert (yield dut.dis_valid)
        assert (yield dut.dis_uop.uop_id) == 2
        assert not (yield dut.dis_ready)
        assert (yield dut.head_ready) & (1 << wid) == 0

        # Neither another warp's writeback nor another register class's
        # writeback may release this dependency.
        yield from wakeup((wid + 1) % dut.n_warps, 5, rtype=rtype)
        yield from tick()
        other_rtype = (RegisterType.FLT
                       if rtype == RegisterType.FIX else RegisterType.FIX)
        yield from wakeup(wid, 5, rtype=other_rtype)
        yield from tick()
        assert not (yield dut.dis_valid)
        assert (yield dut.head_ready) & (1 << wid) == 0

        yield from wakeup(wid, 5, rtype=rtype)

    process, _, _ = init_process(dut, script)
    run_sim(dut, process, monitor)
    assert events == [1, 2], 'accepted instructions must fire exactly once'


@pytest.mark.parametrize('rtype', [RegisterType.FIX, RegisterType.FLT])
@pytest.mark.parametrize('consumer', [
    pytest.param({
        'rs3': 5,
        'frs3_en': False,
        'iq_type': IssueQueueType.FP
    },
                 id='rs3-disabled'),
    pytest.param({
        'rs3': 5,
        'iq_type': IssueQueueType.INT
    }, id='non-fp-rs3'),
])
def test_unused_rs3_does_not_stall(rtype, consumer):
    dut = make_dut()

    def script(tick, send, wakeup):
        yield from send(uop_fields(1, dst=5, dst_rtype=rtype), 0)
        yield from tick()
        # No wakeup: a spurious rs3 dependency would block indefinitely.
        yield from send(uop_fields(2, **consumer), 0)

    process, fires, _ = init_process(dut, script)
    run_sim(dut, process)
    assert fires == [(4, 1), (6, 2)]


def test_queued_independent_instructions_fire_back_to_back():
    dut = make_dut()

    def script(tick, send, wakeup):
        yield dut.downstream_ready.eq(0)
        # Distinct destinations exercise reservation without introducing
        # WAW. B and C must use FIFO advancement, not decode passthrough.
        for uop_id in range(1, 4):
            yield from send(uop_fields(uop_id, dst=uop_id), 0)
        yield dut.downstream_ready.eq(1)

    process, fires, _ = init_process(dut, script)
    run_sim(dut, process)
    assert [uop_id for _, uop_id in fires] == [1, 2, 3]
    cycles = [cycle for cycle, _ in fires]
    assert cycles == list(range(cycles[0], cycles[0] + 3))


@pytest.mark.parametrize('first_wakeup', [RegisterType.FIX, RegisterType.FLT])
def test_mixed_register_dependencies_require_both_wakeups(first_wakeup):
    dut = make_dut()

    def script(tick, send, wakeup):
        yield from send(uop_fields(1, dst=5), 0)
        yield from send(uop_fields(2, dst=5, dst_rtype=RegisterType.FLT), 0)
        yield from tick()
        yield from send(
            uop_fields(3, rs1=5, rs2=5, rs2_rtype=RegisterType.FLT), 0)
        yield from tick()
        yield from wakeup(0, 5, rtype=first_wakeup)
        for _ in range(3):
            yield from tick()
        assert not (yield dut.dis_valid)
        assert (yield dut.head_ready) & 1 == 0
        assert [uop_id for _, uop_id in fires] == [1, 2]
        second_wakeup = (RegisterType.FLT if first_wakeup == RegisterType.FIX
                         else RegisterType.FIX)
        yield from wakeup(0, 5, rtype=second_wakeup)

    process, fires, _ = init_process(dut, script)
    run_sim(dut, process)
    assert [uop_id for _, uop_id in fires] == [1, 2, 3]


class IntMemPath(HasCoreParams, Elaboratable):
    """Dispatch-to-LSU integer path of the core, without fetch, FP or CSRs.

    With lsu_gate=False both the dispatcher mask and the regread entry gate
    are disabled, reproducing the pre-fix behavior.
    """

    def __init__(self, params, lsu_gate=True):
        super().__init__(params)
        self.lsu_gate = lsu_gate

        self.dec_valid = Signal()
        self.dec_wid = Signal(range(params['n_warps']))
        self.dec_uop = MicroOp(params)
        self.dec_ready = Signal()

    def elaborate(self, platform):
        m = Module()

        dispatcher = m.submodules.dispatcher = Dispatcher(self.params)
        scoreboard = m.submodules.scoreboard = Scoreboard(is_float=False,
                                                          params=self.params)
        lsu = m.submodules.lsu = LoadStoreUnit(self.params)

        m.d.comb += [
            dispatcher.dec_valid.eq(self.dec_valid),
            dispatcher.dec_wid.eq(self.dec_wid),
            dispatcher.dec_uop.eq(self.dec_uop),
            self.dec_ready.eq(dispatcher.dec_ready),
            scoreboard.dis_uop.eq(dispatcher.dis_uop),
            scoreboard.dis_wid.eq(dispatcher.dis_wid),
            scoreboard.sb_uop.eq(dispatcher.sb_uop),
            scoreboard.sb_wid.eq(dispatcher.sb_wid),
            dispatcher.head_ready.eq(scoreboard.head_ready),
        ]
        for i in range(self.n_warps):
            m.d.comb += scoreboard.head_uops[i].eq(dispatcher.head_uops[i])

        iregfile = m.submodules.iregfile = RegisterFile(
            rports=self.n_threads * 2,
            wports=self.n_threads,
            num_regs=32 * self.n_warps,
            data_width=self.xlen)

        iregread = m.submodules.iregread = RegisterRead(
            num_rports=self.n_threads * 2,
            rports_array=[2] * self.n_threads,
            reg_width=self.xlen,
            params=self.params)

        dis_is_int = (dispatcher.dis_uop.iq_type &
                      (IssueQueueType.INT | IssueQueueType.MEM)) != 0
        if self.lsu_gate:
            dis_is_lsu = dispatcher.dis_uop.fu_type_has(FUType.MEM)
            dis_split_sta = (
                (dispatcher.dis_uop.opcode == UOpCode.STA)
                & (dispatcher.dis_uop.lrs2_rtype != RegisterType.FIX))
            lsu_dis_ready = (
                ~dis_is_lsu
                | ~lsu.warp_memory.bit_select(dispatcher.dis_wid, 1)
                | (dis_split_sta
                   & lsu.warp_split_addr.bit_select(dispatcher.dis_wid, 1)))
            m.d.comb += [
                dispatcher.lsu_occupied.eq(lsu.warp_memory),
                dispatcher.lsu_split_addr.eq(lsu.warp_split_addr),
            ]
        else:
            lsu_dis_ready = Const(1)
            m.d.comb += [
                dispatcher.lsu_occupied.eq(0),
                dispatcher.lsu_split_addr.eq(0),
            ]

        dis_ready = (~dis_is_int | iregread.dis_ready) & lsu_dis_ready

        m.d.comb += [
            iregread.dis_valid.eq(dispatcher.dis_valid & dis_is_int
                                  & scoreboard.dis_ready & lsu_dis_ready),
            iregread.dis_uop.eq(dispatcher.dis_uop),
            iregread.dis_wid.eq(dispatcher.dis_wid),
            scoreboard.dis_valid.eq(dispatcher.dis_valid & dis_ready),
            dispatcher.dis_ready.eq(scoreboard.dis_ready & dis_ready),
        ]

        for irr_rp, rp in zip(iregread.read_ports, iregfile.read_ports):
            m.d.comb += irr_rp.connect(rp)

        exec_unit = m.submodules.exec_unit = ALUExecUnit(self.params,
                                                         has_ifpu=False,
                                                         has_raster=False)

        m.d.comb += [
            iregread.exec_req.connect(exec_unit.req),
            exec_unit.lsu_req.connect(lsu.exec_req),
            lsu.exec_iresp.ready.eq(1),
            lsu.exec_fresp.ready.eq(1),
        ]

        self.lsu_exec_req = lsu.exec_req
        self.warp_memory = lsu.warp_memory
        self.exec_iresp = exec_unit.iresp
        self.dcache_req = lsu.dcache_req
        self.dcache_resp = lsu.dcache_resp

        return m


def store_fields(uop_id):
    return [
        ('uop_id', uop_id),
        ('opcode', UOpCode.STA),
        ('iq_type', IssueQueueType.MEM),
        ('fu_type', FUType.MEM),
        ('lrs1_rtype', RegisterType.FIX),
        ('lrs2_rtype', RegisterType.FIX),
        ('uses_stq', 1),
        ('mem_cmd', MemoryCommand.WRITE),
        ('mem_size', 2),
        ('tmask', 0b1111),
    ]


def alu_fields(uop_id):
    return [
        ('uop_id', uop_id),
        ('opcode', UOpCode.ADDI),
        ('iq_type', IssueQueueType.INT),
        ('fu_type', FUType.ALU),
        ('lrs1_rtype', RegisterType.FIX),
        ('tmask', 0b1111),
    ]


def run_hol_scenario(lsu_gate):
    with open(
            Path(__file__).resolve().parents[2] /
            'config/groom/default.json') as f:
        params = json.load(f)

    dut = IntMemPath(params, lsu_gate=lsu_gate)
    sim = Simulator(dut)
    sim.add_clock(1e-6)

    events = {
        'a_req': None,
        'b_req': None,
        'alu_iresp': None,
        'warp0_free': None,
        'b_seen_at_exec_req': [],
        'a_req_lanes': 0,
        'b_req_lanes': 0,
    }

    def drive_uop(fields, wid):
        for name, value in fields:
            yield getattr(dut.dec_uop, name).eq(value)
        yield dut.dec_wid.eq(wid)
        yield dut.dec_valid.eq(1)
        yield
        yield dut.dec_valid.eq(0)

    def sample_events(cycle):
        if (yield dut.dcache_req[0].valid):
            uop_id = (yield dut.dcache_req[0].bits.uop.uop_id)
            if uop_id == 1 and events['a_req'] is None:
                events['a_req'] = cycle
        for t in range(params['n_threads']):
            if (yield dut.dcache_req[t].valid):
                uop_id = (yield dut.dcache_req[t].bits.uop.uop_id)
                if uop_id == 1:
                    events['a_req_lanes'] += 1
                elif uop_id == 2 and events['b_req'] is None:
                    events['b_req'] = cycle
                if uop_id == 2:
                    events['b_req_lanes'] += 1
        if ((yield dut.exec_iresp.valid)
                and (yield dut.exec_iresp.bits.uop.uop_id) == 3
                and events['alu_iresp'] is None):
            events['alu_iresp'] = cycle
        if ((yield dut.lsu_exec_req.valid)
                and (yield dut.lsu_exec_req.bits.uop.uop_id) == 2):
            events['b_seen_at_exec_req'].append(cycle)
        if ((yield dut.warp_memory[0]) == 0 and events['a_req'] is not None
                and events['warp0_free'] is None and cycle > events['a_req']):
            events['warp0_free'] = cycle

    def respond_lanes(wid):
        for t in range(params['n_threads']):
            yield dut.dcache_resp[t].valid.eq(1)
            yield dut.dcache_resp[t].bits.uop.lsq_wid.eq(wid)
            yield dut.dcache_resp[t].bits.uop.lsq_tid.eq(t)
        yield
        for t in range(params['n_threads']):
            yield dut.dcache_resp[t].valid.eq(0)
        yield

    def process():
        cycle = 0

        for t in range(params['n_threads']):
            yield dut.dcache_req[t].ready.eq(1)
            yield dut.dcache_resp[t].valid.eq(0)
        yield dut.dec_valid.eq(0)

        for _ in range(3):
            yield
            cycle += 1

        # Warp 0 store A: allocates and occupies the warp 0 LSQ entry.
        yield from drive_uop(store_fields(1), 0)
        for _ in range(50):
            yield from sample_events(cycle)
            if events['a_req'] is not None:
                break
            yield
            cycle += 1
        assert events['a_req'] is not None, 'store A never issued'

        # Hold A unresponded so the warp 0 slot stays occupied.
        for _ in range(3):
            yield
            cycle += 1

        # Warp 0 store B (head-of-line candidate) and warp 1 ALU op.
        yield from drive_uop(store_fields(2), 0)
        yield from drive_uop(alu_fields(3), 1)

        # Watch the blocked window: B must not enter the LSU request port,
        # the warp 1 ALU op may execute freely.
        for _ in range(15):
            yield from sample_events(cycle)
            yield
            cycle += 1

        alu_done_before_response = events['alu_iresp'] is not None

        # Complete store A lane by lane.
        yield from respond_lanes(0)
        for _ in range(50):
            yield from sample_events(cycle)
            if events['b_req'] is not None:
                break
            yield
            cycle += 1

        # Complete store B and drain.
        yield from respond_lanes(0)
        for _ in range(10):
            yield from sample_events(cycle)
            yield
            cycle += 1

        events['alu_done_before_response'] = alu_done_before_response
        events['end_cycle'] = cycle

    sim.add_sync_process(process)
    sim.run()

    return events


def test_lsu_gate_releases_other_warps_during_blocked_store():
    events = run_hol_scenario(lsu_gate=True)

    assert events['a_req_lanes'] == 4
    assert events['b_req_lanes'] == 4
    assert events['b_req'] is not None, 'store B never issued after A'
    assert events['warp0_free'] is not None, 'warp 0 slot never freed'
    assert events['alu_iresp'] is not None, 'warp 1 ALU op never completed'

    # The independent warp 1 op completed while warp 0 was still blocked,
    # and strictly before store B could enter the LSU.
    assert events['alu_iresp'] < events['b_req']
    assert events['alu_iresp'] < events['warp0_free']

    # Store B stayed out of the shared execution output until the slot freed.
    blocked = [
        c for c in events['b_seen_at_exec_req']
        if events['a_req'] < c < events['warp0_free']
    ]
    assert blocked == []

    # Per-warp order: B only after A completed.
    assert events['warp0_free'] <= events['b_req']


def test_without_lsu_gate_store_blocks_other_warps():
    events = run_hol_scenario(lsu_gate=False)

    assert events['a_req'] is not None

    # Pre-fix behavior: store B wedges at the LSU request port and the
    # warp 1 ALU op cannot complete until the warp 0 store finishes.
    assert events['alu_iresp'] is None or events['alu_iresp'] > events['a_req']
    assert events['b_seen_at_exec_req'], 'expected store B stuck at exec_req'
    assert events['b_req'] is not None
    assert events['alu_iresp'] is not None
