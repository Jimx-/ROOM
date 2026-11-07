"""Focused pysim tests for the troom command fetcher (``troom.fetch``).

The fetcher is driven against the shared ``AXIResponder`` memory model:
the command buffer is packed with the frozen format-0 serialization
from ``troom.abi`` and blitted into the responder's memory image, so
field-level mismatches localize to the fetcher's beat-to-word assembly.
"""

from amaranth import *
from amaranth.sim import Passive, Simulator

import random
import pytest

from troom.abi import (
    CMD_DESC_WORDS,
    CMD_FIELDS,
    CMD_INDIRECT_WORDS,
    PARAM_PAGE_BYTES,
    PARAM_SLOT_BITS,
    WrapperError,
    command_field_shapes,
    fetch_length,
    pack_buffer,
    pack_indirect,
)
from troom.consts import Opcode
from troom.fetch import CommandFetcher
from troom.types import HasTRoomParams

from tests.roomsoc.interconnect.axi_helpers import AXIResponder
from tests.troom.test_fa_qstage import (
    CMD_BUF,
    COMP_BUF,
    MAXC,
    N_EFF,
    N_HEAD,
    WRAP_BASE,
    WRAP_IMAGE,
    WRAP_OUT,
    WSHAPES,
    wrap_image,
    wrap_program,
)
from tests.troom.test_wrapper import (
    WrapperTop,
    collect_writes,
    launch,
    run_wrap,
)

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


# ---------------------------------------------------------------------
# E3-d-e parameter-page indirection
# ---------------------------------------------------------------------

PAGE_BASE = 0x400
PAGE_WORDS = 1024


@pytest.mark.parametrize('width', [64, 512])
@pytest.mark.parametrize('read_error', [False, True],
                         ids=['abort', 'read-error'])
def test_param_page_recovery_drains(width, read_error):
    params = dict(FETCH_PARAMS, data_width=width)
    top = FetchTop(params,
                   bytes(0x2000),
                   read_latency=4,
                   r_resp=2 if read_error else 0)
    got = []

    def main():
        dut = top.dut
        yield dut.cmd_base.eq(0x1800)
        yield dut.cmd_count.eq(1)
        yield dut.param_base.eq(PAGE_BASE)
        yield dut.start.eq(1)
        yield
        yield dut.start.eq(0)

        # Wait for an outstanding page burst before interrupting it.
        for _ in range(200):
            if (yield dut.bus.r.valid):
                break
            yield
        else:
            raise AssertionError('parameter-page read did not start')
        assert (yield dut.active)
        if not read_error:
            yield dut.abort.eq(1)

        for _ in range(6000):
            yield
            if not (yield dut.active):
                break
        else:
            raise AssertionError('parameter-page recovery did not drain')

        assert not (yield dut.done)
        assert (yield dut.err) == read_error
        if read_error:
            assert (yield dut.err_code) == int(WrapperError.FETCH_ERROR)
            assert (yield dut.err_index) == 0
        assert got == []
        assert not (yield top.ram.protocol_error)

    run_fetch(top, main, collector(top, got, [1]))


def marked_image(cmds, page_values, cmd_base=0x1800):
    """Command buffer with marker words patched in, plus a page image.

    ``page_values`` maps slot -> 32-bit word. The image holds the page
    at PAGE_BASE and the commands at cmd_base; markers are patched into
    the packed buffer as raw words (pack_command would reject bit 31).
    """
    image = bytearray(PAGE_BASE + PARAM_PAGE_BYTES + 0x2000)
    buf = bytearray()
    for cmd in cmds:
        raw = bytearray(pack_buffer([cmd], SHAPES))
        for i in CMD_INDIRECT_WORDS:
            if f'__mark{i}' in cmd:
                raw[4 * i:4 * i + 4] = \
                    pack_indirect(cmd[f'__mark{i}']).to_bytes(4, 'little')
        buf += raw
    image[cmd_base:cmd_base + len(buf)] = buf
    for slot, value in page_values.items():
        image[PAGE_BASE + 4 * slot:PAGE_BASE + 4 * slot + 4] = \
            value.to_bytes(4, 'little')
    return bytes(image), cmd_base


def launch_with_page(top, base, count, page=PAGE_BASE, timeout=30000):
    dut = top.dut
    yield dut.cmd_base.eq(base)
    yield dut.cmd_count.eq(count)
    if page is not None:
        yield dut.param_base.eq(page)
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


def test_param_page_substitutes_markers():
    cmds = [
        dict(rich_cmd(1), __mark5=0, __mark11=1, __mark14=2),
        rich_cmd(2),
        dict(rich_cmd(3), __mark5=3),
        dict(rich_cmd(4), __mark11=4, __mark14=5),
        end_cmd(5),
    ]
    page = {
        0: 1000,  # k slot
        1: 0x9000 + 123,  # dram_addr slot
        2: 0xbeef,  # rows slot
        3: 999,
        4: 0x8000 + 77,
        5: 0x1234,
    }
    image, base = marked_image(cmds, page)
    top = FetchTop(FETCH_PARAMS, image)
    got = []
    expect = []
    for cmd in cmds:
        e = {k: v for k, v in cmd.items() if not k.startswith('__')}
        if '__mark5' in cmd:
            e['k'] = page[cmd['__mark5']]
        if '__mark11' in cmd:
            e['dram_addr'] = page[cmd['__mark11']]
        if '__mark14' in cmd:
            e['rows'] = page[cmd['__mark14']]
        expect.append(e)

    def main():
        yield from launch_with_page(top, base, len(cmds))
        assert (yield top.dut.done)
        assert not (yield top.dut.err)
        assert got == expect

    run_fetch(top, main, collector(top, got, [1]))


@pytest.mark.parametrize('width', [32, 64, 128, 256, 512])
def test_param_page_dense_markers_preserve_dram_addresses(width):
    cmds = []
    page = {i: 16 + i for i in range(96)}
    expect = []
    for i in range(32):
        cmd = dict(rich_cmd(i + 1), __mark5=3 * i, __mark14=3 * i + 2)
        cmd['dram_addr'] = 0x80000000 + 4 * i
        if i % 2:
            cmd['__mark11'] = 3 * i + 1
            page[3 * i + 1] = 0x80000100 + 4 * i
        cmds.append(cmd)
        e = {k: v for k, v in cmd.items() if not k.startswith('__')}
        for word, field in ((5, 'k'), (11, 'dram_addr'), (14, 'rows')):
            if f'__mark{word}' in cmd:
                e[field] = page[cmd[f'__mark{word}']]
        expect.append(e)
    cmds.append(end_cmd(33))
    expect.append(end_cmd(33))
    image, base = marked_image(cmds, page)
    top = FetchTop(dict(FETCH_PARAMS, data_width=width), image)
    got = []

    def main():
        yield from launch_with_page(top, base, len(cmds))
        assert (yield top.dut.done)
        assert not (yield top.dut.err)
        assert got == expect

    run_fetch(top, main, collector(top, got, [0, 1, 0, 0, 1, 1, 1]))


def test_param_page_markers_under_backpressure():
    cmds = [dict(rich_cmd(1 + i), __mark11=(i % 7)) for i in range(6)]
    cmds.append(end_cmd(7))
    page = {i % 7: 0x8000 + 16 * (i % 7) for i in range(6)}
    image, base = marked_image(cmds, page)
    top = FetchTop(FETCH_PARAMS, image)
    got = []
    rng = random.Random(11)
    ready = [rng.randint(0, 1) for _ in range(32)]

    expect = []
    for cmd in cmds:
        e = {k: v for k, v in cmd.items() if not k.startswith('__')}
        e['dram_addr'] = page[cmd['__mark11']] if '__mark11' in cmd \
            else e['dram_addr']
        expect.append(e)

    def main():
        yield from launch_with_page(top, base, len(cmds))
        assert (yield top.dut.done)
        assert not (yield top.dut.err)
        assert got == expect

    run_fetch(top, main, collector(top, got, ready))


def test_param_page_prefetch_precedes_command_fetch():
    cmds = [dict(rich_cmd(1), __mark5=9), end_cmd(2)]
    page = {9: 4000}
    image, base = marked_image(cmds, page)
    top = FetchTop(FETCH_PARAMS, image)
    got = []
    bursts = []

    def main():
        yield from launch_with_page(top, base, len(cmds))
        assert (yield top.dut.done)
        assert not (yield top.dut.err)
        assert got[0]["k"] == 4000
        # The first read is the whole page at PAGE_BASE; command
        # chunks follow.
        assert bursts[0][0] == PAGE_BASE
        assert sum((ln + 1) * BEAT for _, ln in bursts) \
            == PARAM_PAGE_BYTES + fetch_length(len(cmds), BEAT)

    run_fetch(top, main, collector(top, got, [1]), ar_watch(top, bursts))


def test_marker_without_page_faults_decode():
    # A marker in a width-limited word (k) on a page-less launch stays
    # raw and fails the field-width check. (A marker in dram_addr --
    # a full 32-bit field -- cannot fail `fits`; the backend never
    # emits markers without a page, and run-time DMA range checks
    # catch stale marked buffers there. See the abi.py caveat.)
    cmds = [dict(rich_cmd(1), __mark5=0), end_cmd(2)]
    image, base = marked_image(cmds, {0: 0x9000})
    top = FetchTop(FETCH_PARAMS, image)
    got = []

    def main():
        yield from launch_with_page(top, base, len(cmds), page=None)
        assert (yield top.dut.err)
        assert (yield top.dut.err_code) == int(WrapperError.DECODE_ERROR)
        assert (yield top.dut.err_index) == 0
        assert got == []

    run_fetch(top, main, collector(top, got, [1]))


def test_dirty_marker_bits_stay_raw_and_fault():
    # A "marker" with junk between the slot bits and bit 31 must not
    # resolve: the raw word stays in the view and fails the fits
    # check on its high bits (k is width-limited).
    cmds = [rich_cmd(1), end_cmd(2)]
    image = bytearray(0x2000)
    base = 0x180
    buf = bytearray(pack_buffer(cmds, SHAPES))
    junk = 0x80000000 | (0x155 << PARAM_SLOT_BITS) | 3
    buf[4 * 5:4 * 5 + 4] = junk.to_bytes(4, 'little')
    image[base:base + len(buf)] = buf
    for slot in range(8):
        image[PAGE_BASE + 4 * slot:PAGE_BASE + 4 * slot + 4] = \
            (0x9000 + slot).to_bytes(4, 'little')
    top = FetchTop(FETCH_PARAMS, bytes(image))
    got = []

    def main():
        yield from launch_with_page(top, base, len(cmds))
        assert (yield top.dut.err)
        assert (yield top.dut.err_code) == int(WrapperError.DECODE_ERROR)

    run_fetch(top, main, collector(top, got, [1]))


def test_resolved_value_overflow_faults():
    cmds = [dict(rich_cmd(1), __mark5=0), end_cmd(2)]
    # k's field width is far below 32 bits: a page word with high bits
    # set must fail validation exactly like an oversized immediate.
    image, base = marked_image(cmds, {0: 1 << 30})
    top = FetchTop(FETCH_PARAMS, image)
    got = []

    def main():
        yield from launch_with_page(top, base, len(cmds))
        assert (yield top.dut.err)
        assert (yield top.dut.err_code) == int(WrapperError.DECODE_ERROR)
        assert (yield top.dut.err_index) == 0
        assert got == []

    run_fetch(top, main, collector(top, got, [1]))


def test_bit31_in_other_word_faults():
    cmds = [rich_cmd(1), end_cmd(2)]
    image = bytearray(0x2000)
    base = 0x180
    buf = bytearray(pack_buffer(cmds, SHAPES))
    buf[4 * 4:4 * 4 + 4] = (0x80000000 | 1).to_bytes(4, 'little')
    image[base:base + len(buf)] = buf
    top = FetchTop(FETCH_PARAMS, bytes(image))
    got = []

    def main():
        yield from launch_with_page(top, base, len(cmds), page=None)
        assert (yield top.dut.err)
        assert (yield top.dut.err_code) == int(WrapperError.DECODE_ERROR)

    run_fetch(top, main, collector(top, got, [1]))


# Parameter-page marker resolution under parked command offers.
# Board-matched geometry: the deployed image fetches over a 512-bit
# AXI master, so one beat carries a descriptor's words 0..15 and the
# straddling/bank dynamics match the device that exhibited the race.
BOARD_PARAMS = dict(FETCH_PARAMS, data_width=512)


def wait_cmd(seq, evt=3):
    return dict(opcode=int(Opcode.WAIT),
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


def vbuild_cmd(seq, k, evt=7):
    return dict(opcode=int(Opcode.VBUILD),
                evt=evt,
                seq=seq,
                fmt=0,
                vec_op=0,
                k=k,
                w_base=0,
                w_stride=0,
                r_base=0x200,
                a_base=0,
                m_base=0x300,
                dram_addr=0,
                tmem_addr=0,
                row_bytes=0,
                rows=0,
                dram_stride=0,
                tmem_stride=0)


def page_image(pairs):
    img = bytearray(PARAM_PAGE_BYTES)
    for slot, val in pairs.items():
        img[4 * slot:4 * slot + 4] = val.to_bytes(4, 'little')
    return bytes(img)


def test_pending_offer_loses_marker_fill_and_reads_stale_resolver():

    seed = vbuild_cmd(0, k=35)  # even bank: fills 35
    filler = [wait_cmd(i) for i in range(1, 12)]  # descriptors 1..11
    marked = vbuild_cmd(12, k=pack_indirect(27))  # even bank: page[27]=3
    tail = [wait_cmd(13), wait_cmd(14)]
    end = dict(wait_cmd(15), opcode=int(Opcode.END))
    cmds = [seed] + filler + [marked] + tail + [end]

    base = 0x80
    image = bytearray(0x2000)
    # shapes=None: full 32-bit words, so the marker tag fits in k's
    # word (the fetcher's width check sees the RESOLVED view).
    buf = pack_buffer(cmds)
    image[base:base + len(buf)] = buf
    image[0x1000:0x1000 + PARAM_PAGE_BYTES] = page_image({27: 3})

    top = FetchTop(BOARD_PARAMS, bytes(image), read_latency=8)

    got_rec = []

    def drive():

        def proc():
            dut = top.dut
            yield dut.cmd_base.eq(base)
            yield dut.cmd_count.eq(len(cmds))
            yield dut.param_base.eq(0x1000)
            yield dut.start.eq(1)
            yield
            yield dut.start.eq(0)
            # Model the core: accept the first eight offers (the queue
            # filling while parked), then hold acceptance so the next
            # completing offer stays pending (cp=1) while the marked
            # descriptor's beat first presents.
            fires = 0
            cycle = 0
            while True:
                want = 1 if fires < 8 else 0
                yield dut.cmd.ready.eq(want)
                if (yield dut.cmd.fire):
                    fires += 1
                    fired = {}
                    for n in CMD_FIELDS:
                        fired[n] = yield getattr(dut.cmd.bits, n)
                    got_rec.append(fired)
                if (yield dut.done) or (yield dut.err):
                    break
                if cycle > 400:
                    # drain: accept everything left
                    yield dut.cmd.ready.eq(1)
                    if (yield dut.cmd.fire):
                        fired = {}
                        for n in CMD_FIELDS:
                            fired[n] = yield getattr(dut.cmd.bits, n)
                        got_rec.append(fired)
                    if (yield dut.done) or (yield dut.err):
                        break
                cycle += 1
                yield
            for _ in range(16):
                yield

        return proc

    run_fetch(top, drive())

    by_seq = {c['seq']: c for c in got_rec}
    assert 0 in by_seq, got_rec
    assert by_seq[0]['k'] == 35, by_seq[0]  # the seed resolved
    assert 12 in by_seq, f'marked descriptor never fired: {[c["seq"] for c in got_rec]}'
    resolved = by_seq[12]['k']
    # Correct behavior: the marker resolves to page[27] == 3. The
    # defect substitutes the stale same-bank fill (35) or leaves the
    # raw marker tag (0x7ffffc1b); both are wrong.
    assert resolved == 3, (f'marked k resolved to {resolved:#x} '
                           f'(stale/raw, want the page value 3)')


def test_exact_completion_park_loses_marker_fill():
    """The pinned board trigger, fetcher-level (S4 RTL fix cycle).

    A completed descriptor's offer parks as ``cp`` ONLY on an exact
    beat completion (``new_wc == 17``; a straddling completion keeps
    the beat at the FIFO head with ``cp`` low). With 512-bit beats the
    exact-completion points are beats 16 + 17k, so descriptor 15's
    offer is the first one that can park; its completing beat is then
    CONSUMED, and the next beat -- descriptor 16's words 0..15, which
    carry every indirectible position (5/11/14 <= 15) -- first
    presents under the parked offer. The resolver queue term gated on
    ``~cp`` drops that beat's fills and ``new_present`` is one-shot,
    so the view later substitutes the stale same-(bank, word) page
    value: on the board, the K-chain VBUILD's marked ``k`` read the
    preceding V window's prefix (35) and faulted BAD_SHAPE.

    Drive: fire descriptors 0..14 (fills the bank-0 k resolver with
    the seed's page value 35), hold acceptance so descriptor 15 parks
    exactly, let descriptor 16's beat present under the park, then
    drain. The marked descriptor must resolve to ITS page value (3).
    """
    seed = vbuild_cmd(0, k=pack_indirect(27))  # even bank
    filler = [wait_cmd(i) for i in range(1, 16)]  # descs 1..15
    marked = vbuild_cmd(16, k=pack_indirect(28))  # even bank
    tail = [wait_cmd(17)]
    end = dict(wait_cmd(18), opcode=int(Opcode.END))
    cmds = [seed] + filler + [marked] + tail + [end]

    base = 0x80
    image = bytearray(0x4000)
    buf = pack_buffer(cmds)
    image[base:base + len(buf)] = buf
    image[0x1000:0x1000 + PARAM_PAGE_BYTES] = page_image({27: 35, 28: 3})

    top = FetchTop(BOARD_PARAMS, bytes(image), read_latency=8)

    got_rec = []

    def drive():

        def proc():
            dut = top.dut
            yield dut.cmd_base.eq(base)
            yield dut.cmd_count.eq(len(cmds))
            yield dut.param_base.eq(0x1000)
            yield dut.start.eq(1)
            yield
            yield dut.start.eq(0)

            # Phase 1: accept until descriptors 0..14 have fired.
            while len(got_rec) < 15:
                yield dut.cmd.ready.eq(1)
                if (yield dut.cmd.fire):
                    fired = {}
                    for n in CMD_FIELDS:
                        fired[n] = yield getattr(dut.cmd.bits, n)
                    got_rec.append(fired)
                yield
            # Phase 2: hold acceptance off long enough for descriptor
            # 15's exact-completion offer to park (cp=1) and for the
            # marked descriptor 16's beat to first present under it.
            for _ in range(200):
                yield dut.cmd.ready.eq(0)
                yield
            # Phase 3: drain.
            for _ in range(4000):
                yield dut.cmd.ready.eq(1)
                if (yield dut.cmd.fire):
                    fired = {}
                    for n in CMD_FIELDS:
                        fired[n] = yield getattr(dut.cmd.bits, n)
                    got_rec.append(fired)
                if (yield dut.done) or (yield dut.err):
                    break
                yield
            for _ in range(16):
                yield

        return proc

    run_fetch(top, drive())

    by_seq = {c['seq']: c for c in got_rec}
    assert 0 in by_seq and by_seq[0]['k'] == 35, 'seed fill missing'
    assert 16 in by_seq, (
        f'marked descriptor never fired: {[c["seq"] for c in got_rec]}')
    resolved = by_seq[16]['k']
    assert resolved == 3, (
        f'marked k resolved to {resolved:#x} '
        f'(want the page value 3; 35 = the stale bank-0 seed fill)')


# Wrapper integration: marked descriptors under core backpressure.
# Reserve the parameter page immediately after the compact DRAM image.
PAGE_IMG = WRAP_IMAGE

# The packed command buffer starts at descriptor 0; word ``w`` of
# descriptor ``i`` sits at byte 4*(17*i + w) inside it.


def _marked_wrap_run(staged, read_latency, b_latency):
    """Run the qstage wrapper list with EVERY indirectible word of
    EVERY mid-list descriptor routed through the parameter page.

    Each marked word gets its own page slot holding the descriptor's
    original value, so correct resolution is byte-identical to the
    unmarked program while a lost fill substitutes a foreign
    descriptor's same-bank value (the board's silent-corruption
    class) or a raw/zero word (the launch-failure class).
    """
    import random

    from troom.abi import (
        CMD_DESC_WORDS,
        pack_buffer,
        pack_indirect,
    )

    src_img = wrap_image(random.Random(20261101))
    prog = wrap_program(staged)
    buf = bytearray(pack_buffer(prog, WSHAPES))
    page = bytearray(PARAM_PAGE_BYTES)

    # Word -> field name for the indirectible positions.
    word_field = {5: 'k', 11: 'dram_addr', 14: 'rows'}
    slot = 0
    for i, cmd in enumerate(prog):
        for w, field in word_field.items():
            if field not in cmd:
                continue
            value = cmd[field]
            if value == 0:
                continue
            buf[i * 4 * CMD_DESC_WORDS + 4 * w:
                i * 4 * CMD_DESC_WORDS + 4 * w + 4] = \
                pack_indirect(slot).to_bytes(4, 'little')
            page[4 * slot:4 * slot + 4] = value.to_bytes(4, 'little')
            slot += 1
    assert 4 * slot <= len(page), 'page overflow'

    src_img[CMD_BUF:CMD_BUF + len(buf)] = buf
    assert len(
        src_img) == PAGE_IMG, 'parameter page must follow the DRAM image'
    src_img.extend(page)
    top = WrapperTop(WRAP_BASE,
                     bytes(src_img),
                     read_latency=read_latency,
                     b_latency=b_latency)
    img = bytearray(src_img)
    info = {}

    def main():
        res = yield from launch(top,
                                img,
                                submit_id=7,
                                cmd_base=CMD_BUF,
                                count=len(prog),
                                comp_base=COMP_BUF,
                                capacity=len(prog) + 1,
                                max_cycles=MAXC,
                                param_base=PAGE_IMG)
        info.update(res)

    run_wrap(top, main, collect_writes(top, img))
    outs = {
        h: bytes(img[WRAP_OUT + 0x1000 * h:WRAP_OUT + 0x1000 * h + N_EFF * 4])
        for h in range(N_HEAD)
    }
    return info, outs


def test_wrapper_marked_staged_list_matches_serial():
    """Marked-everything serial and staged lists through the full
    wrapper must agree byte-for-byte and complete cleanly.

    Integration coverage for the S4 fetch fix at the level closest to
    the board (fetcher + core queue + mid-list WAIT parks + page
    resolution e2e). It does NOT deterministically hit the lost-fill
    window -- that depends on cycle-level pacing this harness did not
    exhibit pre-fix (verified: it passes on the defective RTL); the
    deterministic pin is the fetcher-level test above, and the board
    A/B on the rebuilt image is the silicon gate.
    """
    ser_info, ser = _marked_wrap_run(False, read_latency=2, b_latency=2)
    assert ser_info['code'] == 0, ser_info
    assert all(s == 0 for _, s in ser_info['records']), ser_info['records']
    for lat in (2, 6):
        stg_info, stg = _marked_wrap_run(True, read_latency=lat, b_latency=lat)
        assert stg_info['code'] == 0, (lat, stg_info)
        assert all(s == 0 for _, s in stg_info['records']), \
            (lat, stg_info['records'])
        for h in range(N_HEAD):
            assert stg[h] == ser[h], (
                f'lat={lat} head {h}: marked staged output differs '
                f'from marked serial')
