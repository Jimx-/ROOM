"""Focused pysim tests for the troom command fetcher (``troom.fetch``).

The fetcher is driven against the shared ``AXIResponder`` memory model:
the command buffer is packed with the frozen format-0 serialization
from ``troom.abi`` and blitted into the responder's memory image, so
field-level mismatches localize to the fetcher's beat-to-word assembly.
"""

from amaranth import *
from amaranth.sim import Passive, Simulator

import random

from troom.abi import (
    CMD_FIELDS,
    WrapperError,
    command_field_shapes,
    fetch_length,
    pack_buffer,
)
from troom.consts import Opcode
from troom.fetch import CommandFetcher
from troom.types import HasTRoomParams

from tests.roomsoc.interconnect.axi_helpers import AXIResponder

FETCH_PARAMS = dict(
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

BEAT = HasTRoomParams(FETCH_PARAMS).beat_bytes
SHAPES = command_field_shapes(HasTRoomParams(FETCH_PARAMS))


class FetchTop(Elaboratable):
    """Fetcher attached to the AXI responder memory."""

    def __init__(self, params, image, *, read_latency=2, r_resp=0):
        hp = HasTRoomParams(params)
        depth = (len(image) + hp.beat_bytes - 1) // hp.beat_bytes
        init = [
            int.from_bytes(image[o:o + hp.beat_bytes], 'little')
            for o in range(0, len(image), hp.beat_bytes)
        ]
        self.dut = CommandFetcher(params)
        self.ram = AXIResponder(addr_width=hp.addr_width,
                                data_width=hp.data_width,
                                id_width=hp.id_width,
                                depth=depth,
                                init=init,
                                read_latency=read_latency,
                                r_resp=r_resp)

    def elaborate(self, platform):
        m = Module()
        m.submodules.dut = self.dut
        m.submodules.ram = self.ram
        m.d.comb += self.dut.bus.connect(self.ram.bus)
        return m


def signal_cmd(seq, evt=0):
    return dict(opcode=int(Opcode.SIGNAL),
                evt=evt,
                seq=seq,
                fmt=0,
                vec_op=0,
                k=0,
                w_base=0,
                w_stride=0,
                r_base=0,
                a_base=0,
                m_base=0,
                dram_addr=0,
                tmem_addr=0,
                row_bytes=0,
                rows=0,
                dram_stride=0,
                tmem_stride=0)


def rich_cmd(seq, opcode=Opcode.STORE_2D):
    """A descriptor touching every field with distinctive values."""
    return dict(
        opcode=int(opcode),
        evt=seq % 8,
        seq=seq,
        fmt=3,
        vec_op=5,
        k=1000,
        w_base=0x1234,
        w_stride=0x40,
        r_base=0x2100,
        a_base=0x321,
        m_base=0x456,
        dram_addr=0x8000 + 4 * seq,
        tmem_addr=0x3ffc + 0,
        row_bytes=0x1234,
        rows=0xbeef,
        dram_stride=0x1000,
        tmem_stride=0x38,
    )


def end_cmd(seq):
    return dict(signal_cmd(seq), opcode=int(Opcode.END))


def make_image(size, base, cmds):
    image = bytearray(size)
    buf = pack_buffer(cmds, SHAPES)
    image[base:base + len(buf)] = buf
    return bytes(image)


def run_fetch(top, *procs):
    sim = Simulator(top)
    sim.add_clock(1e-6)
    for proc in procs:
        sim.add_sync_process(proc)
    sim.run()


def snapshot(dut):
    cmd = {}
    for name in CMD_FIELDS:
        cmd[name] = yield getattr(dut.cmd.bits, name)
    return cmd


def collector(top, got, ready_seq):
    """Build a passive process recording fired commands under a
    deterministic ready pattern."""

    def proc():
        yield Passive()
        dut = top.dut
        yield dut.cmd.ready.eq(ready_seq[0])
        i = 0
        while True:
            if (yield dut.cmd.fire):
                got.append((yield from snapshot(dut)))
            yield
            i += 1
            yield dut.cmd.ready.eq(ready_seq[i % len(ready_seq)])

    return proc


def ar_watch(top, bursts):

    def proc():
        yield Passive()
        mon = top.ram.ar_monitor
        while True:
            if (yield mon.valid):
                bursts.append(((yield mon.bits.addr), (yield mon.bits.len)))
            yield

    return proc


def launch_and_wait(top, base, count, timeout=30000):
    dut = top.dut
    yield dut.cmd_base.eq(base)
    yield dut.cmd_count.eq(count)
    yield dut.start.eq(1)
    yield
    yield dut.start.eq(0)
    for _ in range(timeout):
        if (yield dut.done) or (yield dut.err):
            break
        yield
    else:
        raise AssertionError('fetcher did not terminate')
    for _ in range(16):
        yield
    assert not (yield top.ram.protocol_error)


def test_fetches_and_decodes_format0_descriptors():
    cmds = [
        rich_cmd(1),
        signal_cmd(2, evt=1),
        rich_cmd(3),
        signal_cmd(4, evt=2),
        end_cmd(5)
    ]
    top = FetchTop(FETCH_PARAMS, make_image(0x800, 0x80, cmds))
    got = []

    def main():
        yield from launch_and_wait(top, 0x80, len(cmds))
        assert (yield top.dut.done)
        assert not (yield top.dut.err)
        assert not (yield top.dut.active)
        assert got == cmds

    run_fetch(top, main, collector(top, got, [1]))


def test_backpressure_preserves_order_and_content():
    cmds = [rich_cmd(1 + i, opcode=Opcode.LOAD_2D) for i in range(6)]
    cmds.append(end_cmd(7))
    top = FetchTop(FETCH_PARAMS, make_image(0x800, 0x100, cmds))
    got = []
    rng = random.Random(7)
    ready = [rng.randint(0, 1) for _ in range(32)]

    def main():
        yield from launch_and_wait(top, 0x100, len(cmds))
        assert (yield top.dut.done)
        assert not (yield top.dut.err)
        assert got == cmds

    run_fetch(top, main, collector(top, got, ready))


def test_tail_padding_reads_whole_beats_only():
    cmds = [signal_cmd(1, evt=1), signal_cmd(2, evt=2), end_cmd(3)]
    base = 0x100
    total = fetch_length(len(cmds), BEAT)
    image = make_image(base + total, base, cmds)
    top = FetchTop(FETCH_PARAMS, image)
    got = []
    bursts = []

    def main():
        yield from launch_and_wait(top, base, len(cmds))
        assert (yield top.dut.done)
        assert got == cmds
        assert sum((ln + 1) * BEAT for _, ln in bursts) == total

    run_fetch(top, main, collector(top, got, [1]), ar_watch(top, bursts))


def test_bursts_are_bounded_and_split_at_4k_boundaries():
    count = 100
    cmds = [signal_cmd(1 + i, evt=i % 8) for i in range(count - 1)]
    cmds.append(end_cmd(count))
    base = 0x800
    total = fetch_length(count, BEAT)
    top = FetchTop(FETCH_PARAMS, make_image(0x4000, base, cmds))
    got = []
    bursts = []

    def main():
        yield from launch_and_wait(top, base, count)
        assert (yield top.dut.done)
        assert len(got) == count
        assert got[42] == cmds[42]
        assert sum((ln + 1) * BEAT for _, ln in bursts) == total
        assert bursts
        for addr, ln in bursts:
            beats = ln + 1
            assert 1 <= beats <= 16
            end = addr + beats * BEAT
            assert (addr & 0xfff) == 0 or addr >> 12 == (end - 1) >> 12, \
                f'burst [{addr:#x},{end:#x}) crosses a 4 KiB boundary'

    run_fetch(top, main, collector(top, got, [1]), ar_watch(top, bursts))


def test_early_end_faults_before_submission():
    cmds = [
        signal_cmd(1, evt=1),
        dict(signal_cmd(2), opcode=int(Opcode.END)),
        signal_cmd(3, evt=2)
    ]
    top = FetchTop(FETCH_PARAMS, make_image(0x800, 0x80, cmds))
    got = []

    def main():
        yield from launch_and_wait(top, 0x80, len(cmds))
        assert (yield top.dut.err)
        assert not (yield top.dut.done)
        assert (yield top.dut.err_code) == int(WrapperError.DECODE_ERROR)
        assert (yield top.dut.err_index) == 1
        # Only the command before the early END was submitted.
        assert got == [cmds[0]]

    run_fetch(top, main, collector(top, got, [1]))


def test_missing_final_end_faults():
    cmds = [signal_cmd(1, evt=1), signal_cmd(2, evt=1)]
    top = FetchTop(FETCH_PARAMS, make_image(0x800, 0x80, cmds))
    got = []

    def main():
        yield from launch_and_wait(top, 0x80, len(cmds))
        assert (yield top.dut.err)
        assert (yield top.dut.err_code) == int(WrapperError.DECODE_ERROR)
        assert (yield top.dut.err_index) == 1
        assert got == [cmds[0]]

    run_fetch(top, main, collector(top, got, [1]))


def test_unrepresentable_field_word_faults():
    cmds = [end_cmd(1)]
    image = bytearray(make_image(0x800, 0x80, cmds))
    # Opcode word carries bits above the 4-bit opcode field.
    image[0x80:0x84] = (0x20).to_bytes(4, 'little')
    top = FetchTop(FETCH_PARAMS, bytes(image))
    got = []

    def main():
        yield from launch_and_wait(top, 0x80, 1)
        assert (yield top.dut.err)
        assert (yield top.dut.err_code) == int(WrapperError.DECODE_ERROR)
        assert (yield top.dut.err_index) == 0
        assert got == []

    run_fetch(top, main, collector(top, got, [1]))


def test_read_error_faults_with_fetch_error():
    cmds = [end_cmd(1)]
    top = FetchTop(FETCH_PARAMS, make_image(0x800, 0x80, cmds), r_resp=2)
    got = []

    def main():
        yield from launch_and_wait(top, 0x80, 1)
        assert (yield top.dut.err)
        assert not (yield top.dut.done)
        assert (yield top.dut.err_code) == int(WrapperError.FETCH_ERROR)
        assert (yield top.dut.err_index) == 0
        assert got == []

    run_fetch(top, main, collector(top, got, [1]))


def test_abort_stops_submission_and_drains():
    count = 8
    cmds = [signal_cmd(1 + i, evt=1) for i in range(count - 1)]
    cmds.append(end_cmd(count))
    top = FetchTop(FETCH_PARAMS, make_image(0x800, 0x80, cmds), read_latency=4)
    got = []

    def main():
        dut = top.dut
        yield dut.cmd_base.eq(0x80)
        yield dut.cmd_count.eq(count)
        yield dut.start.eq(1)
        yield
        yield dut.start.eq(0)

        while len(got) < 2:
            yield
        yield dut.abort.eq(1)
        after = len(got)

        for _ in range(6000):
            yield
            if not (yield dut.active):
                break
        else:
            raise AssertionError('fetcher did not drain after abort')
        assert not (yield dut.done)
        assert not (yield dut.err)
        # At most one command already in flight when abort landed.
        assert len(got) <= after + 1
        assert not (yield top.ram.protocol_error)

    run_fetch(top, main, collector(top, got, [1]))


def test_slow_memory_still_completes():
    cmds = [rich_cmd(1), signal_cmd(2, evt=3), end_cmd(3)]
    top = FetchTop(FETCH_PARAMS,
                   make_image(0x800, 0x180, cmds),
                   read_latency=9)
    got = []

    def main():
        yield from launch_and_wait(top, 0x180, len(cmds))
        assert (yield top.dut.done)
        assert got == cmds

    run_fetch(top, main, collector(top, got, [1]))
