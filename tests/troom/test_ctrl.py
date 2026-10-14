"""Focused pysim tests for the troom launch controller (``troom.ctrl``).

The controller is exercised through its public AXI-Lite interface: an
``TroomAXILiteBridge`` adapter feeds the GROOM-style CSR bridge, so every
test drives independent AW/W arrival, byte strobes, and CSR read
synchronization exactly as the FPGA host would.
"""

from amaranth import *
from amaranth.sim import Passive, Simulator

import pytest

from troom.abi import (
    CAPS_ENGINE_COUNTERS,
    CAPS_MM_EPI_COUNTERS,
    CAPS_FMT0,
    CAPS_WATCHDOG,
    CMD_DESC_BYTES,
    CONTROL_CLEAR,
    CONTROL_START,
    ERROR_INDEX_NONE,
    REG_ABI_VERSION,
    REG_ACCEPTED_ID,
    REG_BEAT_BYTES,
    REG_CAPS,
    REG_CMD_ALIGN,
    REG_CMD_BASE_HI,
    REG_CMD_BASE_LO,
    REG_CMD_COUNT,
    REG_CMD_FORMAT,
    REG_COMP_ALIGN,
    REG_COMP_BASE_HI,
    REG_COMP_BASE_LO,
    REG_COMP_CAPACITY,
    REG_COMPLETED_COUNT,
    REG_CONTROL,
    REG_CYCLES,
    REG_EPI_CAST_OPS,
    REG_ERROR_CODE,
    REG_ERROR_INDEX,
    REG_FINISHED_ID,
    REG_ID,
    REG_MAX_CYCLES,
    REG_MEM_BASE,
    REG_MEM_SIZE,
    REG_STATUS,
    REG_SUBMIT_ID,
    REG_TMEM_BYTES,
    STATUS_BUSY,
    STATUS_DONE,
    STATUS_ERROR,
    STATUS_QUIESCENT,
    STATUS_REJECTED,
    TROOM_ABI_VERSION,
    TROOM_ID,
    WrapperError,
)
from troom.ctrl import TroomAXILiteBridge, TroomController
from troom.types import HasTRoomParams

from roomsoc.interconnect.axi import AXILiteInterface

PARAMS = dict(
    addr_width=32,
    data_width=64,
    id_width=4,
    seg_len=32,
    n_outputs=2,
    n_tokens=1,
    tmem_bytes=16 * 1024,
    n_tmem_banks=4,
    cmd_queue_depth=4,
    n_events=8,
    seq_bits=8,
)

MEM_BASE = 0x001000
MEM_SIZE = 0x010000


class CtrlTop(Elaboratable):
    """Controller behind the AXI-Lite-to-Wishbone adapter."""

    def __init__(self, params, **kwargs):
        self.dut = TroomController(params, **kwargs)
        self.axil = AXILiteInterface(data_width=32, addr_width=32)

    def elaborate(self, platform):
        m = Module()
        m.submodules.dut = self.dut
        m.submodules.adapt = TroomAXILiteBridge(self.axil, self.dut)
        return m


def run_ctrl(top, proc, monitor=None):
    sim = Simulator(top)
    sim.add_clock(1e-6)
    sim.add_sync_process(proc)
    if monitor is not None:
        sim.add_sync_process(monitor)
    sim.run()


def cwrite(axil, offset, value, strb=None):
    """One CSR write plus settle cycles for the CSR write path."""
    resp = (yield from axil.write(offset, value, strb))
    assert resp == 0
    for _ in range(4):
        yield


def cread(axil, offset):
    data, resp = (yield from axil.read(offset))
    assert resp == 0
    return data


def program_config(axil, **overrides):
    config = dict(
        submit_id=0x11,
        cmd_base=MEM_BASE + 0x0000,
        cmd_count=4,
        comp_base=MEM_BASE + 0x1000,
        capacity=8,
        fmt=0,
        max_cycles=0,
    )
    config.update(overrides)
    yield from cwrite(axil, REG_SUBMIT_ID, config['submit_id'])
    yield from cwrite(axil, REG_CMD_BASE_LO, config['cmd_base'])
    yield from cwrite(axil, REG_CMD_BASE_HI, config.get('cmd_base_hi', 0))
    yield from cwrite(axil, REG_CMD_COUNT, config['cmd_count'])
    yield from cwrite(axil, REG_CMD_FORMAT, config['fmt'])
    yield from cwrite(axil, REG_COMP_BASE_LO, config['comp_base'])
    yield from cwrite(axil, REG_COMP_CAPACITY, config['capacity'])
    yield from cwrite(axil, REG_COMP_BASE_HI, config.get('comp_base_hi', 0))
    yield from cwrite(axil, REG_MAX_CYCLES, config['max_cycles'] & 0xffffffff)
    yield from cwrite(axil, REG_MAX_CYCLES + 4, config['max_cycles'] >> 32)
    return config


def start(axil):
    yield from cwrite(axil, REG_CONTROL, 1 << CONTROL_START)


def split_write(axil, addr, data, strb, gap=3):
    """AXI-Lite write with W delayed ``gap`` cycles behind AW.

    Unlike ``axilite_write_split`` from the shared kit, this driver
    holds AW valid (address stable) until the transaction completes,
    matching slaves that only accept AW together with W.
    """
    yield axil.aw.valid.eq(1)
    yield axil.aw.addr.eq(addr)
    yield axil.w.valid.eq(0)
    for _ in range(gap):
        yield
    yield axil.w.valid.eq(1)
    yield axil.w.data.eq(data)
    yield axil.w.strb.eq(strb)
    yield
    while not (yield axil.w.ready):
        yield
    yield axil.aw.valid.eq(0)
    yield axil.w.valid.eq(0)
    yield axil.w.strb.eq(0)
    yield axil.b.ready.eq(1)
    while not (yield axil.b.valid):
        yield
    resp = (yield axil.b.resp)
    yield axil.b.ready.eq(0)
    for _ in range(4):
        yield
    return resp


def test_identity_caps_and_parameters():
    top = CtrlTop(PARAMS, mem_base=MEM_BASE, mem_size=MEM_SIZE)
    hp = HasTRoomParams(PARAMS)
    axil = top.axil

    def proc():
        yield top.dut.quiescent_i.eq(1)
        assert (yield from cread(axil, REG_ID)) == TROOM_ID
        assert (yield from cread(axil, REG_ABI_VERSION)) == TROOM_ABI_VERSION
        caps = ((1 << CAPS_FMT0) | (1 << CAPS_WATCHDOG)
                | (1 << CAPS_ENGINE_COUNTERS)
                | (1 << CAPS_MM_EPI_COUNTERS))
        assert (yield from cread(axil, REG_CAPS)) == caps
        assert (yield from cread(axil, REG_MEM_BASE)) == MEM_BASE
        assert (yield from cread(axil, REG_MEM_SIZE)) == MEM_SIZE
        assert (yield from cread(axil, REG_BEAT_BYTES)) == hp.beat_bytes
        assert (yield from cread(axil, REG_TMEM_BYTES)) == hp.tmem_bytes
        assert (yield from cread(axil, REG_CMD_ALIGN)) == hp.beat_bytes
        assert (yield from cread(axil,
                                 REG_COMP_ALIGN)) == max(hp.beat_bytes, 8)
        # Status starts idle and quiescent.
        assert (yield from cread(axil, REG_STATUS)) == (1 << STATUS_QUIESCENT)
        # Terminal registers start cleared.
        assert (yield from cread(axil, REG_ERROR_CODE)) == WrapperError.NONE
        assert (yield from cread(axil, REG_ERROR_INDEX)) == ERROR_INDEX_NONE
        assert (yield from cread(axil, REG_ACCEPTED_ID)) == 0
        assert (yield from cread(axil, REG_FINISHED_ID)) == 0
        assert (yield from cread(axil, REG_COMPLETED_COUNT)) == 0

    run_ctrl(top, proc)


def test_staging_readback_and_64bit_max_cycles():
    top = CtrlTop(PARAMS, mem_base=MEM_BASE, mem_size=MEM_SIZE)
    axil = top.axil

    def proc():
        yield top.dut.quiescent_i.eq(1)
        yield from program_config(axil, max_cycles=0x1234_5678_9abc_def0)
        assert (yield from cread(axil, REG_SUBMIT_ID)) == 0x11
        assert (yield from cread(axil, REG_CMD_COUNT)) == 4
        # 64-bit register: write low then high (commit on high), read
        # back low then high for a coherent snapshot.
        assert (yield from cread(axil, REG_MAX_CYCLES)) == 0x9abc_def0
        assert (yield from cread(axil, REG_MAX_CYCLES + 4)) == 0x1234_5678
        # The cycle counter runs.
        first = (yield from cread(axil, REG_CYCLES))
        for _ in range(20):
            yield
        second = (yield from cread(axil, REG_CYCLES))
        assert second > first

    run_ctrl(top, proc)


def test_start_accepts_and_captures_configuration_atomically():
    top = CtrlTop(PARAMS, mem_base=MEM_BASE, mem_size=MEM_SIZE)
    axil = top.axil
    dut = top.dut
    pulses = []

    def monitor():
        yield Passive()
        while True:
            if (yield dut.start):
                pulses.append(1)
            yield

    def proc():
        yield dut.quiescent_i.eq(1)
        config = yield from program_config(axil)
        yield from start(axil)

        status = yield from cread(axil, REG_STATUS)
        assert status & (1 << STATUS_BUSY)
        assert not status & (1 << STATUS_REJECTED)
        assert (yield from cread(axil, REG_ACCEPTED_ID)) == 0x11
        assert (yield dut.active_submit_id) == 0x11
        assert (yield dut.active_cmd_base) == config['cmd_base']
        assert (yield dut.active_cmd_count) == 4
        assert (yield dut.active_comp_base) == config['comp_base']
        assert (yield dut.active_max_cycles) == 0

        # Later staging writes do not disturb the captured launch.
        yield from cwrite(axil, REG_SUBMIT_ID, 0x99)
        yield from cwrite(axil, REG_CMD_COUNT, 7)
        assert (yield dut.active_submit_id) == 0x11
        assert (yield dut.active_cmd_count) == 4

        assert len(pulses) == 1

    run_ctrl(top, proc, monitor)


def test_start_rejected_while_busy_or_not_quiescent():
    top = CtrlTop(PARAMS, mem_base=MEM_BASE, mem_size=MEM_SIZE)
    axil = top.axil
    dut = top.dut
    pulses = []

    def monitor():
        yield Passive()
        while True:
            if (yield dut.start):
                pulses.append(1)
            yield

    def proc():
        yield dut.quiescent_i.eq(1)
        yield from program_config(axil)
        yield from start(axil)
        assert len(pulses) == 1

        # Busy: a second start is rejected without touching identity.
        yield from cwrite(axil, REG_SUBMIT_ID, 0x22)
        yield from start(axil)
        status = yield from cread(axil, REG_STATUS)
        assert status & (1 << STATUS_REJECTED)
        assert not status & (1 << STATUS_DONE)
        assert not status & (1 << STATUS_ERROR)
        assert (yield from cread(axil, REG_ACCEPTED_ID)) == 0x11
        assert (yield from cread(axil, REG_FINISHED_ID)) == 0
        assert len(pulses) == 1

        # Complete the run, then start again while not quiescent.
        yield dut.done_i.eq(1)
        for _ in range(4):
            yield
        yield dut.done_i.eq(0)
        yield dut.quiescent_i.eq(0)
        yield from start(axil)
        status = yield from cread(axil, REG_STATUS)
        assert status & (1 << STATUS_REJECTED)
        assert status & (1 << STATUS_DONE)  # previous run result intact
        assert (yield from cread(axil, REG_ACCEPTED_ID)) == 0x11
        assert len(pulses) == 1

    run_ctrl(top, proc, monitor)


@pytest.mark.parametrize(
    'overrides,code',
    [
        (dict(fmt=1), WrapperError.BAD_FORMAT),
        (dict(fmt=4), WrapperError.BAD_FORMAT),
        (dict(cmd_count=0), WrapperError.BAD_COUNT),
        (dict(cmd_base_hi=1), WrapperError.ADDR_OVERFLOW),
        (dict(comp_base_hi=2), WrapperError.ADDR_OVERFLOW),
        (dict(cmd_base=MEM_BASE + 4), WrapperError.BAD_ALIGN),
        (dict(comp_base=MEM_BASE + 0x1004), WrapperError.BAD_ALIGN),
        (dict(cmd_base=MEM_BASE + MEM_SIZE - 2 * CMD_DESC_BYTES,
              cmd_count=4), WrapperError.BAD_RANGE),
        (dict(comp_base=MEM_BASE + MEM_SIZE - 16,
              capacity=8), WrapperError.BAD_RANGE),
        (dict(cmd_base=0x8), WrapperError.BAD_RANGE),  # below memory window
        (dict(capacity=3), WrapperError.BAD_CAPACITY),
        (dict(comp_base=MEM_BASE + 2 * CMD_DESC_BYTES - 8),
         WrapperError.BUF_OVERLAP),
    ])
def test_configuration_faults_reject_the_launch(overrides, code):
    top = CtrlTop(PARAMS, mem_base=MEM_BASE, mem_size=MEM_SIZE)
    axil = top.axil
    dut = top.dut

    def proc():
        yield dut.quiescent_i.eq(1)
        yield from program_config(axil, submit_id=0x77, **overrides)
        yield from start(axil)

        status = yield from cread(axil, REG_STATUS)
        assert status & (1 << STATUS_REJECTED)
        assert status & (1 << STATUS_ERROR)
        assert not status & (1 << STATUS_BUSY)
        assert not status & (1 << STATUS_DONE)
        assert (yield from cread(axil, REG_ERROR_CODE)) == code
        assert (yield from cread(axil, REG_ERROR_INDEX)) == \
            ERROR_INDEX_NONE
        # The rejected launch terminated immediately with its identity.
        assert (yield from cread(axil, REG_FINISHED_ID)) == 0x77
        assert (yield from cread(axil, REG_ACCEPTED_ID)) == 0
        assert not (yield dut.start)

    run_ctrl(top, proc)


def test_successful_run_reports_done_with_launch_id():
    top = CtrlTop(PARAMS, mem_base=MEM_BASE, mem_size=MEM_SIZE)
    axil = top.axil
    dut = top.dut

    def proc():
        yield dut.quiescent_i.eq(1)
        yield from program_config(axil)
        yield dut.completed_i.eq(4)
        yield from start(axil)

        # Quiescent without done changes nothing.
        for _ in range(8):
            yield
        assert (yield from cread(axil, REG_STATUS)) & (1 << STATUS_BUSY)
        assert (yield from cread(axil, REG_COMPLETED_COUNT)) == 4

        yield dut.done_i.eq(1)
        for _ in range(4):
            yield
        status = yield from cread(axil, REG_STATUS)
        assert not status & (1 << STATUS_BUSY)
        assert status & (1 << STATUS_DONE)
        assert not status & (1 << STATUS_ERROR)
        assert (yield from cread(axil, REG_FINISHED_ID)) == 0x11

        # done_i may drop; terminal status is stable.
        yield dut.done_i.eq(0)
        for _ in range(8):
            yield
        assert (yield from cread(axil, REG_STATUS)) & (1 << STATUS_DONE)

    run_ctrl(top, proc)


def test_first_error_is_latched_and_dominates_done():
    top = CtrlTop(PARAMS, mem_base=MEM_BASE, mem_size=MEM_SIZE)
    axil = top.axil
    dut = top.dut

    def proc():
        yield dut.quiescent_i.eq(1)
        yield from program_config(axil)
        yield from start(axil)

        # Hold the run active: drop quiescence while the error sources
        # fire so the terminal transition is observable.
        yield dut.quiescent_i.eq(0)
        for _ in range(2):
            yield

        # Fetch fault first, then a completion fault: only the first is
        # retained.
        yield dut.fetch_err.eq(1)
        yield dut.fetch_err_code.eq(WrapperError.FETCH_ERROR)
        yield dut.fetch_err_index.eq(3)
        for _ in range(2):
            yield
        yield dut.fetch_err.eq(0)
        yield dut.comp_err.eq(1)
        yield dut.comp_err_code.eq(WrapperError.COMP_ERROR)
        yield dut.comp_err_index.eq(ERROR_INDEX_NONE)
        for _ in range(2):
            yield
        yield dut.comp_err.eq(0)

        assert (yield from cread(axil, REG_ERROR_CODE)) == \
            WrapperError.FETCH_ERROR
        assert (yield from cread(axil, REG_ERROR_INDEX)) == 3
        assert (yield from cread(axil, REG_STATUS)) & (1 << STATUS_ERROR)
        assert (yield from cread(axil, REG_STATUS)) & (1 << STATUS_BUSY)
        assert (yield dut.abort)

        # Logical completion cannot turn an errored run into DONE.
        yield dut.done_i.eq(1)
        yield dut.quiescent_i.eq(1)
        for _ in range(4):
            yield
        status = yield from cread(axil, REG_STATUS)
        assert not status & (1 << STATUS_BUSY)
        assert not status & (1 << STATUS_DONE)
        assert status & (1 << STATUS_ERROR)
        assert (yield from cread(axil, REG_FINISHED_ID)) == 0x11
        assert not (yield dut.abort)

    run_ctrl(top, proc)


def test_control_clear_clears_terminal_status_only():
    top = CtrlTop(PARAMS, mem_base=MEM_BASE, mem_size=MEM_SIZE)
    axil = top.axil
    dut = top.dut

    def proc():
        yield dut.quiescent_i.eq(1)
        yield from program_config(axil)
        yield dut.completed_i.eq(4)
        yield from start(axil)
        # Hold the run active for the busy-phase checks.
        yield dut.quiescent_i.eq(0)
        for _ in range(4):
            yield

        # CLEAR while busy is ignored, and a busy start is rejected.
        yield from start(axil)
        status = yield from cread(axil, REG_STATUS)
        assert status & (1 << STATUS_BUSY)
        assert status & (1 << STATUS_REJECTED)
        yield from cwrite(axil, REG_CONTROL, 1 << CONTROL_CLEAR)
        status = yield from cread(axil, REG_STATUS)
        assert status & (1 << STATUS_BUSY)
        assert status & (1 << STATUS_REJECTED)

        # Finish, then CLEAR drops the stickies but keeps data
        # registers (finished id, accepted id, counters).
        yield dut.done_i.eq(1)
        yield dut.quiescent_i.eq(1)
        for _ in range(4):
            yield
        assert not (yield from cread(axil, REG_STATUS)) & (1 << STATUS_BUSY)
        yield from cwrite(axil, REG_CONTROL, 1 << CONTROL_CLEAR)
        status = yield from cread(axil, REG_STATUS)
        assert status == (1 << STATUS_QUIESCENT)
        assert (yield from cread(axil, REG_FINISHED_ID)) == 0x11
        assert (yield from cread(axil, REG_ACCEPTED_ID)) == 0x11
        assert (yield from cread(axil, REG_COMPLETED_COUNT)) == 4

    run_ctrl(top, proc)


def test_watchdog_expires_and_reports_sentinel_index():
    top = CtrlTop(PARAMS, mem_base=MEM_BASE, mem_size=MEM_SIZE)
    axil = top.axil
    dut = top.dut

    def proc():
        yield dut.quiescent_i.eq(1)
        yield from program_config(axil, max_cycles=25)
        yield from start(axil)
        # Hold the run active so abort is observable before the
        # terminal transition.
        yield dut.quiescent_i.eq(0)

        status = 0
        for _ in range(60):
            yield
            status = yield from cread(axil, REG_STATUS)
            if status & (1 << STATUS_ERROR):
                break
        assert status & (1 << STATUS_ERROR)
        assert not status & (1 << STATUS_DONE)
        assert status & (1 << STATUS_BUSY)
        assert (yield from cread(axil, REG_ERROR_CODE)) == \
            WrapperError.WATCHDOG
        assert (yield from cread(axil, REG_ERROR_INDEX)) == \
            ERROR_INDEX_NONE
        assert (yield dut.abort)

        # A watchdog hit is terminal once quiescent.
        yield dut.quiescent_i.eq(1)
        for _ in range(4):
            yield
        status = yield from cread(axil, REG_STATUS)
        assert not status & (1 << STATUS_BUSY)
        assert (yield from cread(axil, REG_FINISHED_ID)) == 0x11

    run_ctrl(top, proc)


def test_control_partial_writes_cannot_trigger_actions():
    top = CtrlTop(PARAMS, mem_base=MEM_BASE, mem_size=MEM_SIZE)
    axil = top.axil
    dut = top.dut
    pulses = []

    def monitor():
        yield Passive()
        while True:
            if (yield dut.start):
                pulses.append(1)
            yield

    def proc():
        yield dut.quiescent_i.eq(1)
        yield from program_config(axil)
        start_bits = (1 << CONTROL_START) | (1 << CONTROL_CLEAR)

        # A write covering only byte lanes 0..2 of the CONTROL word
        # never completes the register, so action bits set there are
        # inert -- even though lane 2 overlaps the word.
        yield from cwrite(axil, REG_CONTROL, start_bits, strb=0b0111)
        assert not pulses
        assert not (yield from cread(axil, REG_STATUS)) \
            & (1 << STATUS_BUSY)

        # Writing only the committing byte launches.
        yield from cwrite(axil, REG_CONTROL, 1 << CONTROL_START, strb=0b1000)
        assert len(pulses) == 1
        assert (yield from cread(axil, REG_STATUS)) & (1 << STATUS_BUSY)

    run_ctrl(top, proc, monitor)


def test_write_zero_does_not_start_and_independent_aw_w():
    top = CtrlTop(PARAMS, mem_base=MEM_BASE, mem_size=MEM_SIZE)
    axil = top.axil
    dut = top.dut
    pulses = []

    def monitor():
        yield Passive()
        while True:
            if (yield dut.start):
                pulses.append(1)
            yield

    def proc():
        yield dut.quiescent_i.eq(1)
        # A full CONTROL write with no action bits set does nothing.
        yield from cwrite(axil, REG_CONTROL, 0)
        assert not pulses

        # Independent AW/W arrival with a gap still configures (and
        # starts) correctly.
        resp = (yield from split_write(axil, REG_SUBMIT_ID, 0x5a, 0xf))
        assert resp == 0
        for _ in range(4):
            yield
        assert (yield from cread(axil, REG_SUBMIT_ID)) == 0x5a

        yield from program_config(axil)
        resp = (yield from split_write(axil, REG_CONTROL, 1 << CONTROL_START,
                                       0xf))
        assert resp == 0
        for _ in range(4):
            yield
        assert len(pulses) == 1
        assert (yield from cread(axil, REG_ACCEPTED_ID)) == 0x11

    run_ctrl(top, proc, monitor)


@pytest.mark.parametrize('offset', [REG_CONTROL + 1, 0x50, 0x130, 0x40000010])
def test_invalid_write_does_not_alias_control(offset):
    top = CtrlTop(PARAMS, mem_base=MEM_BASE, mem_size=MEM_SIZE)

    def proc():
        yield top.dut.quiescent_i.eq(1)
        yield from program_config(top.axil)
        assert (yield from top.axil.write(offset, 1 << CONTROL_START)) == 2
        assert not (yield from cread(top.axil,
                                     REG_STATUS)) & (1 << STATUS_BUSY)
        _, resp = yield from top.axil.read(offset)
        assert resp == 2

    run_ctrl(top, proc)


def test_write_to_read_only_breakdown_register_rejected():
    """The M3b window is readable but never writable; a write attempt
    must SLVERR without disturbing control state."""
    top = CtrlTop(PARAMS, mem_base=MEM_BASE, mem_size=MEM_SIZE)

    def proc():
        yield top.dut.quiescent_i.eq(1)
        yield from program_config(top.axil)
        assert (yield from top.axil.write(REG_EPI_CAST_OPS,
                                          1 << CONTROL_START)) == 2
        assert not (yield from cread(top.axil, REG_STATUS)) \
            & (1 << STATUS_BUSY)
        _, resp = yield from top.axil.read(REG_EPI_CAST_OPS)
        assert resp == 0

    run_ctrl(top, proc)


def test_range_check_includes_command_tail_padding():
    top = CtrlTop(PARAMS, mem_base=MEM_BASE, mem_size=0x1044)

    def proc():
        yield top.dut.quiescent_i.eq(1)
        yield from program_config(top.axil,
                                  cmd_base=MEM_BASE + 0x1000,
                                  cmd_count=1,
                                  comp_base=MEM_BASE,
                                  capacity=1)
        yield from start(top.axil)
        assert (yield from cread(top.axil,
                                 REG_ERROR_CODE)) == WrapperError.BAD_RANGE

    run_ctrl(top, proc)


def test_configuration_rejection_clears_previous_done():
    top = CtrlTop(PARAMS, mem_base=MEM_BASE, mem_size=MEM_SIZE)

    def proc():
        dut, axil = top.dut, top.axil
        yield dut.quiescent_i.eq(1)
        yield from program_config(axil)
        yield from start(axil)
        yield dut.done_i.eq(1)
        yield
        yield
        yield dut.done_i.eq(0)
        assert (yield from cread(axil, REG_STATUS)) & (1 << STATUS_DONE)
        yield from cwrite(axil, REG_CMD_COUNT, 0)
        yield from start(axil)
        status = yield from cread(axil, REG_STATUS)
        assert status & (1 << STATUS_ERROR)
        assert not status & (1 << STATUS_DONE)

    run_ctrl(top, proc)


def test_staging_byte_strobes_preserve_other_registers():
    top = CtrlTop(PARAMS)

    def proc():
        axil = top.axil
        yield from cwrite(axil, REG_SUBMIT_ID, 0x12345678)
        yield from cwrite(axil, REG_CMD_COUNT, 0xdeadbeef)
        yield from cwrite(axil, REG_SUBMIT_ID, 0xab, strb=1)
        assert (yield from cread(axil, REG_SUBMIT_ID)) == 0x123456ab
        yield from cwrite(axil, REG_SUBMIT_ID, 0xcd000000, strb=8)
        assert (yield from cread(axil, REG_SUBMIT_ID)) == 0xcd3456ab
        yield from cwrite(axil, REG_SUBMIT_ID, 0, strb=0)
        assert (yield from cread(axil, REG_SUBMIT_ID)) == 0xcd3456ab
        yield from cwrite(axil, REG_MAX_CYCLES, 0x87654321)
        yield from cwrite(axil, REG_CMD_COUNT, 0xabcdef01)
        yield from cwrite(axil, REG_MAX_CYCLES + 4, 0x12345678)
        assert (yield from cread(axil, REG_MAX_CYCLES)) == 0x87654321
        assert (yield from cread(axil, REG_MAX_CYCLES + 4)) == 0x12345678

    run_ctrl(top, proc)


def test_zero_strobe_does_not_commit_watchdog_low_word():
    top = CtrlTop(PARAMS)

    def proc():
        axil = top.axil
        yield from cwrite(axil, REG_MAX_CYCLES, 123)
        yield from cwrite(axil, REG_MAX_CYCLES + 4, 0, strb=0)
        assert (yield from cread(axil, REG_MAX_CYCLES)) == 0
        yield from cwrite(axil, REG_MAX_CYCLES + 4, 0)
        assert (yield from cread(axil, REG_MAX_CYCLES)) == 123

    run_ctrl(top, proc)


def test_w_before_aw_and_response_backpressure():
    top = CtrlTop(PARAMS)

    def proc():
        axil = top.axil
        yield axil.w.data.eq(0x12345678)
        yield axil.w.strb.eq(0xf)
        yield axil.w.valid.eq(1)
        yield
        assert (yield axil.w.ready)
        yield axil.w.valid.eq(0)
        for _ in range(5):
            yield
            assert not (yield axil.b.valid)
        yield axil.aw.addr.eq(REG_SUBMIT_ID)
        yield axil.aw.valid.eq(1)
        yield
        assert (yield axil.aw.ready)
        yield axil.aw.valid.eq(0)
        for _ in range(100):
            if (yield axil.b.valid):
                break
            yield
        else:
            raise AssertionError('missing write response')
        for _ in range(5):
            assert (yield axil.b.valid)
            assert (yield axil.b.resp) == 0
            yield
        yield axil.b.ready.eq(1)
        yield
        yield axil.b.ready.eq(0)
        yield
        yield axil.ar.addr.eq(REG_SUBMIT_ID)
        yield axil.ar.valid.eq(1)
        yield
        assert (yield axil.ar.ready)
        yield axil.ar.valid.eq(0)
        for _ in range(100):
            if (yield axil.r.valid):
                break
            yield
        else:
            raise AssertionError('missing read response')
        for _ in range(5):
            assert (yield axil.r.valid)
            assert (yield axil.r.data) == 0x12345678
            assert (yield axil.r.resp) == 0
            yield
        yield axil.r.ready.eq(1)
        yield
        yield axil.r.ready.eq(0)
        yield
        assert (yield from axil.write(REG_ID, 0)) == 2
        _, resp = yield from axil.read(REG_CONTROL)
        assert resp == 2

    run_ctrl(top, proc)
