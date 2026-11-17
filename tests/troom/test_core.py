"""Focused pysim tests for the troom command contract.

``WAIT`` gates ordered commands against asynchronous completion sources
such as DMA completions or against earlier commands. Issue is ordered,
so these tests only pair ``WAIT`` with events
that are already set when the ``WAIT`` is fetched.
"""

from amaranth.sim import Simulator

import pytest

from troom.consts import Opcode, Status, VecOp
from troom.core import Core
from troom.types import HasTRoomParams

from tests.sim import run_test

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


def run_streams(dut, *procs):
    sim = Simulator(dut)
    sim.add_clock(1e-6)
    for proc in procs:
        sim.add_sync_process(proc)
    sim.run()


def send_cmd(cmd, opcode, seq, evt=0):
    """Issue one command and wait for it to be accepted."""
    yield cmd.bits.opcode.eq(int(opcode))
    yield cmd.bits.seq.eq(seq)
    yield cmd.bits.evt.eq(evt)
    yield cmd.valid.eq(1)
    yield
    while not (yield cmd.fire):
        yield
    yield cmd.valid.eq(0)
    yield


def drain_until_done(resp, results, dut, limit=200):
    """Record completions keyed by sequence number until ``done``."""
    yield resp.ready.eq(1)
    for _ in range(limit):
        if (yield resp.valid):
            seq = (yield resp.bits.seq)
            status = (yield resp.bits.status)
            results[seq] = Status(status)
        yield
        if (yield dut.done):
            break


def test_signal_wait_end_completes_sequence():
    dut = Core(PARAMS)
    results = {}

    def tx():
        for _ in range(4):
            yield
        yield from send_cmd(dut.cmd, Opcode.SIGNAL, 1, evt=1)
        yield from send_cmd(dut.cmd, Opcode.WAIT, 2, evt=1)
        yield from send_cmd(dut.cmd, Opcode.END, 3)

    def rx():
        yield from drain_until_done(dut.resp, results, dut)
        assert results == {
            1: Status.OK,
            2: Status.OK,
            3: Status.OK,
        }
        assert (yield dut.cmds_done) == 3

    run_streams(dut, tx, rx)


def test_wait_blocks_until_event_is_set():
    """A WAIT fetched with its event clear produces no completion.

    A later SIGNAL cannot unblock it under ordered issue, so this test
    drives the WAIT alone and checks the engine stays busy with nothing
    on ``resp`` and ``done`` low.
    """
    dut = Core(PARAMS)

    def tx():
        for _ in range(4):
            yield
        yield from send_cmd(dut.cmd, Opcode.WAIT, 1, evt=3)

    def rx():
        yield dut.resp.ready.eq(1)
        for _ in range(40):
            assert not (yield dut.resp.valid)
            yield
        assert not (yield dut.done)
        assert (yield dut.busy)
        assert (yield dut.cmds_done) == 0

    run_streams(dut, tx, rx)


def test_wait_clears_event_for_next_waiter():
    """A retired WAIT clears its flag, so a second WAIT blocks again."""
    dut = Core(PARAMS)

    def tx():
        for _ in range(4):
            yield
        yield from send_cmd(dut.cmd, Opcode.SIGNAL, 1, evt=2)
        yield from send_cmd(dut.cmd, Opcode.WAIT, 2, evt=2)
        yield from send_cmd(dut.cmd, Opcode.WAIT, 3, evt=2)

    def rx():
        yield dut.resp.ready.eq(1)
        for _ in range(40):
            if (yield dut.resp.valid):
                # Only the SIGNAL and the first WAIT complete.
                assert (yield dut.resp.bits.seq) in (1, 2)
            yield
        assert (yield dut.cmds_done) == 2
        assert not (yield dut.done)

    run_streams(dut, tx, rx)


def test_wait_clear_keeps_simultaneous_completion_set():
    """Pin both sides of the completion/WAIT collision boundary.

    Delay 100 retires the first WAIT on the vector completion's set edge.
    Removing evt_set_now loses that set and deadlocks the second WAIT.
    Delay 101 merges the two producers before the first WAIT, so its second
    WAIT must block under the event-level protocol. Recalibrate these two
    delays if the vector pipeline latency changes.
    """
    finished = []
    for delay in (100, 101):
        dut = Core(PARAMS)
        got = []
        hung = []

        def tx():
            for _ in range(4):
                yield
            yield from send_cmd(dut.cmd, Opcode.SIGNAL, 1, evt=4)
            # Engine producer whose completion sets evt=4; r/m bases
            # point at the low TMEM (the tiny k keeps it fast).
            yield dut.cmd.bits.opcode.eq(int(Opcode.VECTOR))
            yield dut.cmd.bits.seq.eq(2)
            yield dut.cmd.bits.evt.eq(4)
            yield dut.cmd.bits.vec_op.eq(int(VecOp.VEC_MUL))
            yield dut.cmd.bits.k.eq(32)
            yield dut.cmd.bits.a_base.eq(0)
            yield dut.cmd.bits.m_base.eq(0)
            yield dut.cmd.bits.r_base.eq(0x100)
            yield dut.cmd.valid.eq(1)
            yield
            while not (yield dut.cmd.fire):
                yield
            yield dut.cmd.valid.eq(0)
            for _ in range(delay):
                yield
            yield from send_cmd(dut.cmd, Opcode.WAIT, 3, evt=4)
            yield from send_cmd(dut.cmd, Opcode.WAIT, 4, evt=4)
            yield from send_cmd(dut.cmd, Opcode.END, 5)

        def rx():
            yield dut.resp.ready.eq(1)
            for _ in range(400):
                if (yield dut.resp.valid):
                    got.append((yield dut.resp.bits.seq))
                yield
                if (yield dut.done):
                    return
            hung.append(delay)

        run_streams(dut, tx, rx)
        assert got[:1] == [1], (delay, got)
        assert 3 in got, (
            f'delay={delay}: the first WAIT never retired ({got})')
        if hung:
            # Past the collision: the by-design merge hang.
            assert got == [
                1, 2, 3
            ], (f'delay={delay}: unexpected partial completion {got}')
        else:
            assert 4 in got and 5 in got, (delay, got)
            finished.append(delay)
    assert finished == [100], f'collision boundary changed: {finished}'


def test_datapath_opcodes_report_status():
    dut = Core(PARAMS)
    results = {}

    program = [
        (Opcode.LOAD_2D, 1),
        (Opcode.STORE_2D, 2),
        (Opcode.QUANTIZE, 3),
        (Opcode.MATMUL, 4),
        (Opcode.VECTOR, 5),
        (Opcode.REDUCE, 6),
        (15, 7),  # reserved encoding
        (Opcode.SQUANTIZE, 8),
        (Opcode.SMATMUL, 9),
        (Opcode.END, 10),
    ]

    def tx():
        for _ in range(4):
            yield
        for opcode, seq in program:
            yield from send_cmd(dut.cmd, opcode, seq)

    def rx():
        yield from drain_until_done(dut.resp, results, dut)
        assert results == {
            # DMA is implemented; zero-length descriptors are invalid.
            1: Status.BAD_ALIGN,
            2: Status.BAD_ALIGN,
            # The legacy matrix opcodes are retired.
            3: Status.BAD_OPCODE,
            4: Status.BAD_OPCODE,
            # The vector unit is implemented; the zero element count
            # (and the default VEC_ADD sub-op for REDUCE) is a bad
            # shape.
            5: Status.BAD_SHAPE,
            6: Status.BAD_SHAPE,
            7: Status.BAD_OPCODE,
            8: Status.BAD_SHAPE,
            9: Status.BAD_SHAPE,
            10: Status.OK,
        }
        assert (yield dut.cmds_done) == 10

    run_streams(dut, tx, rx)


def test_axi_master_stays_idle():
    dut = Core(PARAMS)

    def proc():
        for _ in range(16):
            yield
        assert (yield dut.bus.aw.valid) == 0
        assert (yield dut.bus.ar.valid) == 0
        assert (yield dut.bus.w.valid) == 0

    run_test(dut, proc, sync=True)


def test_seq_bits_is_parameterizable():
    # PARAMS uses a non-default seq_bits=8, so the behavioral tests above
    # already exercise the narrowed field; check widths directly here.
    dut = Core(PARAMS)

    def proc():
        for _ in range(2):
            yield

    run_test(dut, proc, sync=True)
    assert len(dut.cmd.bits.seq) == 8
    assert len(dut.resp.bits.seq) == 8

    wide_params = dict(PARAMS, seq_bits=32)
    assert HasTRoomParams(wide_params).seq_bits == 32

    with pytest.raises(ValueError):
        HasTRoomParams(dict(PARAMS, seq_bits=0))
