"""Focused pysim tests for the troom completion writer
(``troom.completion``).

Records are driven into ``sink`` and the resulting AXI writes are
reconstructed from the shared ``AXIResponder`` write monitor, checking
record order, byte placement, strobed partial final beats, 4 KiB burst
splits, committed-record accounting, and error handling.
"""

from amaranth import *
from amaranth.sim import Passive, Simulator

import random

from troom.abi import (
    ERROR_INDEX_NONE,
    WrapperError,
    pack_completion,
)
from troom.completion import CompletionWriter
from troom.consts import Status
from troom.types import HasTRoomParams

from tests.roomsoc.interconnect.axi_helpers import AXIResponder

COMP_PARAMS = dict(
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

WIDE_PARAMS = dict(COMP_PARAMS, data_width=128)

HP = HasTRoomParams(COMP_PARAMS)
BEAT = HP.beat_bytes


class CompTop(Elaboratable):
    """Completion writer attached to the AXI responder memory."""

    def __init__(self, params, *, fifo_depth=None, b_latency=2, b_resp=0):
        hp = HasTRoomParams(params)
        self.dut = CompletionWriter(params, fifo_depth=fifo_depth)
        self.ram = AXIResponder(addr_width=hp.addr_width,
                                data_width=hp.data_width,
                                id_width=hp.id_width,
                                depth=4096,
                                init=[0] * 4096,
                                b_latency=b_latency,
                                b_resp=b_resp)

    def elaborate(self, platform):
        m = Module()
        m.submodules.dut = self.dut
        m.submodules.ram = self.ram
        m.d.comb += self.dut.bus.connect(self.ram.bus)
        return m


def run_comp(top, *procs):
    sim = Simulator(top)
    sim.add_clock(1e-6)
    for proc in procs:
        sim.add_sync_process(proc)
    sim.run()


def try_send(top, seq, status, timeout=400):
    """Offer one record; abandon it if the writer stops accepting."""
    sink = top.dut.sink
    yield sink.bits.seq.eq(seq)
    yield sink.bits.status.eq(int(status))
    yield sink.valid.eq(1)
    yield
    for _ in range(timeout):
        if (yield sink.fire):
            yield sink.valid.eq(0)
            yield
            return True
        yield
    yield sink.valid.eq(0)
    yield
    return False


def send_records(top, records, gap_seq=None):
    for i, (seq, status) in enumerate(records):
        sent = (yield from try_send(top, seq, status))
        if not sent:
            return
        if gap_seq is not None:
            for _ in range(gap_seq[i % len(gap_seq)]):
                yield


def w_collect(top, writes):

    def proc():
        yield Passive()
        mon = top.ram.w_monitor
        while True:
            if (yield mon.valid):
                writes.append(
                    ((yield mon.bits.addr), (yield
                                             mon.bits.data), (yield
                                                              mon.bits.strb)))
            yield

    return proc


def aw_watch(top, bursts):

    def proc():
        yield Passive()
        mon = top.ram.aw_monitor
        while True:
            if (yield mon.valid):
                bursts.append(((yield mon.bits.addr), (yield mon.bits.len)))
            yield

    return proc


def reconstruct(writes):
    mem = {}
    for addr, data, strb in writes:
        for lane in range(strb.bit_length()):
            if (strb >> lane) & 1:
                mem[addr + lane] = (data >> (8 * lane)) & 0xff
    return mem


def check_log(writes, base, records):
    expected = b''.join(
        pack_completion(seq, status) for seq, status in records)
    mem = reconstruct(writes)
    for i, byte in enumerate(expected):
        assert mem.get(base + i) == byte, \
            f'byte {i}: {mem.get(base + i)!r} != {byte!r}'
    stray = [a for a in mem if not base <= a < base + len(expected)]
    assert not stray, f'writes outside the log: {stray[:8]}'


def launch(top, base, count):
    dut = top.dut
    yield dut.comp_base.eq(base)
    yield dut.comp_count.eq(count)
    yield dut.start.eq(1)
    yield
    yield dut.start.eq(0)
    yield


def wait_terminal(top, timeout=20000):
    dut = top.dut
    for _ in range(timeout):
        if (yield dut.drained) or (yield dut.idle):
            break
        yield
    else:
        raise AssertionError('writer did not terminate')
    for _ in range(16):
        yield
    assert not (yield top.ram.protocol_error)


def ok_records(n, start=1):
    return [(start + i, Status.OK) for i in range(n)]


def test_writes_records_in_order_with_accounting():
    records = ok_records(32)
    base = 0x800
    top = CompTop(COMP_PARAMS)
    writes = []

    def main():
        yield from launch(top, base, len(records))
        yield from send_records(top, records)
        yield from wait_terminal(top)
        assert (yield top.dut.drained)
        assert not (yield top.dut.err)
        assert (yield top.dut.completed) == len(records)
        assert not (yield top.dut.active)
        check_log(writes, base, records)

    run_comp(top, main, w_collect(top, writes))


def test_final_partial_beat_is_strobed():
    # 128-bit beats hold two records; five records end mid-beat.
    hp = HasTRoomParams(WIDE_PARAMS)
    assert hp.beat_bytes == 16
    records = ok_records(5)
    base = 0x810
    top = CompTop(WIDE_PARAMS)
    writes = []

    def main():
        yield from launch(top, base, len(records))
        yield from send_records(top, records)
        yield from wait_terminal(top)
        assert (yield top.dut.drained)
        assert (yield top.dut.completed) == 5
        check_log(writes, base, records)
        # The trailing beat carries exactly one strobed record: five
        # records fill two 16-byte beats plus 8 trailing bytes.
        full = (1 << 16) - 1
        partials = [(a, s) for a, d, s in writes if s != full]
        assert partials == [(base + 32, 0x00ff)], partials

    run_comp(top, main, w_collect(top, writes))


def test_bursts_split_at_4k_boundaries():
    records = ok_records(16)
    base = 0xff0
    top = CompTop(COMP_PARAMS)
    writes = []
    bursts = []

    def main():
        yield from launch(top, base, len(records))
        yield from send_records(top, records)
        yield from wait_terminal(top)
        assert (yield top.dut.drained)
        check_log(writes, base, records)
        assert bursts
        for addr, ln in bursts:
            end = addr + (ln + 1) * BEAT
            assert addr >> 12 == (end - 1) >> 12, \
                f'burst [{addr:#x},{end:#x}) crosses a 4 KiB boundary'

    run_comp(top, main, w_collect(top, writes), aw_watch(top, bursts))


def test_write_error_marks_log_incomplete():
    records = ok_records(32)
    base = 0x800
    top = CompTop(COMP_PARAMS, b_resp=2)
    writes = []

    def main():
        yield from launch(top, base, len(records))
        yield from send_records(top, records)
        yield from wait_terminal(top)
        assert (yield top.dut.err)
        assert not (yield top.dut.drained)
        assert (yield top.dut.idle)
        assert (yield top.dut.err_code) == int(WrapperError.COMP_ERROR)
        assert (yield top.dut.err_index) == ERROR_INDEX_NONE
        # The first burst's B response failed: nothing is committed.
        assert (yield top.dut.completed) == 0

    run_comp(top, main, w_collect(top, writes))


def test_failed_command_is_reported_but_collected():
    records = ok_records(5)
    records[3] = (4, Status.BAD_ALIGN)
    base = 0xc00
    top = CompTop(COMP_PARAMS)
    writes = []

    def main():
        yield from launch(top, base, len(records))
        yield from send_records(top, records)
        yield from wait_terminal(top)
        assert (yield top.dut.drained)
        assert (yield top.dut.err)
        assert (yield top.dut.err_code) == int(WrapperError.CMD_FAILED)
        assert (yield top.dut.err_index) == 3
        assert (yield top.dut.completed) == 5
        check_log(writes, base, records)

    run_comp(top, main, w_collect(top, writes))


def test_stream_longer_than_record_fifo():
    records = ok_records(64, start=0x10)
    base = 0x400
    top = CompTop(COMP_PARAMS, fifo_depth=4)
    writes = []
    rng = random.Random(3)
    gaps = [rng.randint(0, 3) for _ in range(8)]

    def main():
        yield from launch(top, base, len(records))
        yield from send_records(top, records, gaps)
        yield from wait_terminal(top)
        assert (yield top.dut.drained)
        assert not (yield top.dut.err)
        assert (yield top.dut.completed) == 64
        check_log(writes, base, records)

    run_comp(top, main, w_collect(top, writes))


def test_flush_terminates_early_and_flushes_partial_beat():
    # 128-bit beats: nine records = four full beats + one partial.
    records = ok_records(9)
    base = 0x820
    top = CompTop(WIDE_PARAMS)
    writes = []

    def main():
        dut = top.dut
        yield from launch(top, base, 16)  # only nine will arrive
        yield from send_records(top, records)
        yield dut.flush.eq(1)
        yield from wait_terminal(top)
        assert (yield top.dut.idle)
        assert not (yield top.dut.drained)
        assert not (yield top.dut.err)
        assert (yield dut.completed) == 9
        check_log(writes, base, records)
        full = (1 << 16) - 1
        partials = [(a, s) for a, d, s in writes if s != full]
        assert partials == [(base + 64, 0x00ff)], partials

    run_comp(top, main, w_collect(top, writes))


def test_slow_write_responses_still_complete():
    records = ok_records(12)
    base = 0x900
    top = CompTop(COMP_PARAMS, b_latency=9)
    writes = []

    def main():
        yield from launch(top, base, len(records))
        yield from send_records(top, records)
        yield from wait_terminal(top)
        assert (yield top.dut.drained)
        assert (yield top.dut.completed) == 12
        check_log(writes, base, records)

    run_comp(top, main, w_collect(top, writes))


def test_partial_beat_does_not_leak_into_next_launch():
    top = CompTop(WIDE_PARAMS)
    writes = []

    def main():
        yield from launch(top, 0x800, 1)
        yield from send_records(top, [(0xff, Status.BAD_ALIGN)])
        yield from wait_terminal(top)
        writes.clear()
        yield from launch(top, 0x900, 1)
        yield from send_records(top, [(1, Status.OK)])
        yield from wait_terminal(top)
        check_log(writes, 0x900, [(1, Status.OK)])

    run_comp(top, main, w_collect(top, writes))


def test_first_write_failure_freezes_count_and_relaunch_clears_buffers():
    response = Signal(2, reset=2)
    top = CompTop(COMP_PARAMS, b_latency=20, b_resp=response)
    writes = []

    def responses():
        yield Passive()
        while not ((yield top.dut.bus.b.valid) and
                   (yield top.dut.bus.b.ready)):
            yield
        yield response.eq(0)
        yield

    def main():
        yield from launch(top, 0x800, 32)
        yield from send_records(top, ok_records(32))
        yield from wait_terminal(top)
        assert (yield top.dut.fatal)
        assert (yield top.dut.completed) == 0
        writes.clear()
        records = ok_records(8, start=0x80)
        yield from launch(top, 0xa00, len(records))
        yield from send_records(top, records)
        yield from wait_terminal(top)
        assert not (yield top.dut.err)
        assert (yield top.dut.completed) == len(records)
        check_log(writes, 0xa00, records)

    run_comp(top, main, responses, w_collect(top, writes))
