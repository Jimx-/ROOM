"""Wrapper-level tests for the troom FPGA host interface.

``TroomWrapper`` is exercised end to end through its public ports: an
AXI-Lite host drives the register ABI, and a shared ``AXIResponder``
memory model serves the wrapper's arbitrated AXI master. The command
buffer, tensor data, and completion log all live in that one memory,
so the tests check the full loop the plan's milestone 3 requires:
launch, drain, terminal status, safe per-run re-arm, DRAM
preservation, and watchdog recovery with outstanding traffic.
"""

from amaranth import *
from amaranth.sim import Passive, Simulator

import pytest

from troom.abi import (
    CAPS_ENGINE_COUNTERS,
    CAPS_MM_EPI_COUNTERS,
    CONTROL_START,
    ERROR_INDEX_NONE,
    REG_ARRAY_STARVE,
    REG_CAPS,
    REG_CMD_BASE_HI,
    REG_CMD_BASE_LO,
    REG_CMD_COUNT,
    REG_CMD_FORMAT,
    REG_CMDS_DONE,
    REG_COMP_BASE_HI,
    REG_COMP_BASE_LO,
    REG_COMP_CAPACITY,
    REG_COMPLETED_COUNT,
    REG_CONTROL,
    REG_DMA_STALLS,
    REG_DRAM_BYTES,
    REG_EPI_ACTIVE_CYCLES,
    REG_EPI_ADD_OPS,
    REG_EPI_CAST_OPS,
    REG_EPI_MUL_OPS,
    REG_EPI_RECORDS,
    REG_EPI_STALLS,
    REG_ERROR_CODE,
    REG_ERROR_INDEX,
    REG_FINISHED_ID,
    REG_MAX_CYCLES,
    REG_MM_CREDIT_STALLS,
    REG_MM_DRAIN_CYCLES,
    REG_MM_RUN_CYCLES,
    REG_RUN_CYCLES,
    REG_STATUS,
    REG_SUBMIT_ID,
    REG_TMEM_CONFLICTS,
    REG_USEFUL_MACS,
    STATUS_BUSY,
    STATUS_DONE,
    STATUS_ERROR,
    STATUS_QUIESCENT,
    WrapperError,
    command_field_shapes,
    pack_buffer,
    pack_completion,
)
from troom.consts import Opcode, Status, VecOp
from troom.types import HasTRoomParams
from troom.wrapper import TroomWrapper

from tests.roomsoc.interconnect.axi_helpers import AXIResponder
from tests.troom.model import rdc_result, vec_word

WRAP_PARAMS = dict(
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

HP = HasTRoomParams(WRAP_PARAMS)
BEAT = HP.beat_bytes
SHAPES = command_field_shapes(HP)

CMD_BASE = 0x2000
COMP_BASE = 0x2800
BAD_BASE = 0x3000


class WrapperTop(Elaboratable):
    """Wrapper attached to the shared AXI responder memory.

    ``mmio_base``/``mem_base`` place the design in the device address
    space (both default to the zero-based map of the focused unit
    tests); ``troom_main.wrapper_sim`` passes the RDMA stack's map so
    the simulation exercises the same decoding the generated RTL uses.
    """

    def __init__(self,
                 params,
                 image,
                 *,
                 read_latency=2,
                 b_latency=2,
                 mmio_base=0,
                 mem_base=0):
        hp = HasTRoomParams(params)
        beat = hp.beat_bytes
        depth = (len(image) + beat - 1) // beat
        init = [
            int.from_bytes(image[o:o + beat], 'little')
            for o in range(0, len(image), beat)
        ]
        self.mmio_base = mmio_base
        self.mem_base = mem_base
        self.aw_enable = Signal(reset=1)
        self.wrapper = TroomWrapper(params,
                                    mem_base=mem_base,
                                    mem_size=len(image),
                                    mmio_base=mmio_base)
        self.ram = AXIResponder(addr_width=hp.addr_width,
                                data_width=hp.data_width,
                                id_width=hp.id_width,
                                depth=depth,
                                init=init,
                                read_latency=read_latency,
                                b_latency=b_latency,
                                base_addr=mem_base)

    def elaborate(self, platform):
        m = Module()
        m.submodules.wrapper = self.wrapper
        m.submodules.ram = self.ram
        m.d.comb += [
            self.wrapper.bus.connect(self.ram.bus),
            self.ram.bus.aw.valid.eq(self.wrapper.bus.aw.valid
                                     & self.aw_enable),
            self.wrapper.bus.aw.ready.eq(self.ram.bus.aw.ready
                                         & self.aw_enable),
        ]
        return m


def run_wrap(top, *procs):
    sim = Simulator(top)
    sim.add_clock(1e-6)
    for proc in procs:
        sim.add_sync_process(proc)
    sim.run()


def wrap_cmd(opcode, seq, evt=0, **desc):
    cmd = dict(opcode=int(opcode),
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
    cmd.update(desc)
    return cmd


def good_program(vec_n, beat_bytes=BEAT):
    """The load/compute/store workload from the core smoke test."""
    vout_tmem, rout_tmem = 0x1000, 0x1400
    vout_dram, rout_dram = 0x1800, 0x1c00
    load = dict(dram_addr=0x1000,
                tmem_addr=0x0000,
                row_bytes=64,
                rows=4,
                dram_stride=128,
                tmem_stride=64)
    store = dict(dram_addr=0x1400,
                 tmem_addr=0x0000,
                 row_bytes=64,
                 rows=4,
                 dram_stride=128,
                 tmem_stride=64)
    return [
        wrap_cmd(Opcode.LOAD_2D, 1, evt=1, **load),
        wrap_cmd(Opcode.WAIT, 2, evt=1),
        wrap_cmd(Opcode.STORE_2D, 3, evt=2, **store),
        wrap_cmd(Opcode.WAIT, 4, evt=2),
        wrap_cmd(Opcode.VECTOR,
                 5,
                 evt=3,
                 vec_op=int(VecOp.VEC_MUL),
                 k=vec_n,
                 a_base=0x0000,
                 m_base=0x0000,
                 r_base=vout_tmem),
        wrap_cmd(Opcode.WAIT, 6, evt=3),
        wrap_cmd(Opcode.REDUCE,
                 7,
                 evt=4,
                 vec_op=int(VecOp.RDC_SUM),
                 k=vec_n,
                 a_base=vout_tmem,
                 m_base=0x0000,
                 r_base=rout_tmem),
        wrap_cmd(Opcode.WAIT, 8, evt=4),
        wrap_cmd(Opcode.STORE_2D,
                 9,
                 evt=5,
                 dram_addr=vout_dram,
                 tmem_addr=vout_tmem,
                 row_bytes=4 * vec_n,
                 rows=1,
                 dram_stride=0,
                 tmem_stride=0),
        wrap_cmd(Opcode.STORE_2D,
                 10,
                 evt=6,
                 dram_addr=rout_dram,
                 tmem_addr=rout_tmem,
                 row_bytes=beat_bytes,
                 rows=1,
                 dram_stride=0,
                 tmem_stride=0),
        wrap_cmd(Opcode.WAIT, 11, evt=5),
        wrap_cmd(Opcode.WAIT, 12, evt=6),
        wrap_cmd(Opcode.SIGNAL, 13, evt=7),
        wrap_cmd(Opcode.WAIT, 14, evt=7),
        wrap_cmd(Opcode.END, 15),
    ]


def watchdog_program():
    """A load followed by a WAIT whose event never arrives."""
    load = dict(dram_addr=0x1000,
                tmem_addr=0x0000,
                row_bytes=64,
                rows=4,
                dram_stride=128,
                tmem_stride=64)
    return [
        wrap_cmd(Opcode.LOAD_2D, 1, evt=1, **load),
        wrap_cmd(Opcode.WAIT, 2, evt=2),  # event 2 never set
        wrap_cmd(Opcode.END, 3),
    ]


def bad_end_program():
    """An END placed before the final descriptor."""
    return [
        wrap_cmd(Opcode.SIGNAL, 1, evt=1),
        dict(wrap_cmd(Opcode.SIGNAL, 2, evt=3), opcode=int(Opcode.END)),
        wrap_cmd(Opcode.SIGNAL, 3, evt=1),
    ]


def rebase_program(cmds, base):
    """Shift a program's DRAM offsets to absolute device addresses."""
    return [dict(cmd, dram_addr=cmd['dram_addr'] + base) for cmd in cmds]


def make_image(extra_buffers=()):
    image = bytearray(16 * 1024)
    for off in range(0, len(image), BEAT):
        image[off:off + BEAT] = (0x1000 + off).to_bytes(BEAT, 'little')
    for base, cmds in extra_buffers:
        buf = pack_buffer(cmds, SHAPES)
        image[base:base + len(buf)] = buf
    return image


def pattern_word(addr):
    """The sim image's deterministic fill pattern, as a 32-bit word."""
    beat_off = addr & ~(BEAT - 1)
    value = 0x1000 + beat_off
    return (value >> (8 * (addr & (BEAT - 1)))) & 0xffffffff


def expected_outputs(vec_n):
    tile_words = [pattern_word(0x1000 + 4 * i) for i in range(vec_n)]
    vec_exp = [vec_word(VecOp.VEC_MUL, w, w) for w in tile_words]
    rdc_exp = rdc_result(VecOp.RDC_SUM, vec_exp)
    return tile_words, vec_exp, rdc_exp


def collect_writes(top, image):

    def proc():
        yield Passive()
        mon = top.ram.w_monitor
        while True:
            if (yield mon.valid):
                addr = (yield mon.bits.addr)
                data = (yield mon.bits.data)
                strb = (yield mon.bits.strb)
                for lane in range(top.wrapper.beat_bytes):
                    if (strb >> lane) & 1:
                        image[addr - top.mem_base + lane] = \
                            (data >> (8 * lane)) & 0xff
            yield

    return proc


def mmio_write(top, offset, value):
    axil = top.wrapper.axil
    resp = (yield from axil.write(top.mmio_base + offset, value))
    assert resp == 0


def mmio_read(top, offset):
    axil = top.wrapper.axil
    data, resp = (yield from axil.read(top.mmio_base + offset))
    assert resp == 0
    return data


def program_launch(top,
                   *,
                   submit_id,
                   cmd_base,
                   count,
                   comp_base,
                   capacity,
                   max_cycles=0):
    # Buffer bases are image offsets; the wrapper is programmed with
    # absolute device addresses.
    yield from mmio_write(top, REG_SUBMIT_ID, submit_id)
    yield from mmio_write(top, REG_CMD_BASE_LO, top.mem_base + cmd_base)
    yield from mmio_write(top, REG_CMD_BASE_HI, 0)
    yield from mmio_write(top, REG_CMD_COUNT, count)
    yield from mmio_write(top, REG_CMD_FORMAT, 0)
    yield from mmio_write(top, REG_COMP_BASE_LO, top.mem_base + comp_base)
    yield from mmio_write(top, REG_COMP_BASE_HI, 0)
    yield from mmio_write(top, REG_COMP_CAPACITY, capacity)
    yield from mmio_write(top, REG_MAX_CYCLES, max_cycles & 0xffffffff)
    yield from mmio_write(top, REG_MAX_CYCLES + 4, max_cycles >> 32)


def wait_terminal(top, polls=8000):
    for _ in range(polls):
        status = (yield from mmio_read(top, REG_STATUS))
        if not status & (1 << STATUS_BUSY):
            return status
        for _ in range(4):
            yield
    raise AssertionError('launch did not terminate')


def read_records(image, base, count):
    records = []
    for i in range(count):
        seq = int.from_bytes(image[base + 8 * i:base + 8 * i + 4], 'little')
        status = int.from_bytes(image[base + 8 * i + 4:base + 8 * i + 8],
                                'little')
        records.append((seq, status))
    return records


def launch(top,
           image,
           *,
           submit_id,
           cmd_base,
           count,
           comp_base,
           capacity,
           comp_count=None,
           max_cycles=0):
    """Run one launch through the public interfaces and check its
    completions landed in memory."""
    yield from program_launch(top,
                              submit_id=submit_id,
                              cmd_base=cmd_base,
                              count=count,
                              comp_base=comp_base,
                              capacity=capacity,
                              max_cycles=max_cycles)
    yield from mmio_write(top, REG_CONTROL, 1 << CONTROL_START)
    status = yield from wait_terminal(top)
    finished = (yield from mmio_read(top, REG_FINISHED_ID))
    code = (yield from mmio_read(top, REG_ERROR_CODE))
    index = (yield from mmio_read(top, REG_ERROR_INDEX))
    completed = (yield from mmio_read(top, REG_COMPLETED_COUNT))
    assert finished == submit_id
    records = read_records(image, comp_base, completed)
    return {
        'status': status,
        'finished': finished,
        'code': code,
        'index': index,
        'completed': completed,
        'records': records,
    }


def check_tensor_outputs(image, vec_n):
    _, vec_exp, rdc_exp = expected_outputs(vec_n)
    got = [
        int.from_bytes(image[0x1800 + 4 * i:0x1800 + 4 * i + 4], 'little')
        for i in range(vec_n)
    ]
    assert got == vec_exp, 'vector multiply mismatch'
    got_r = int.from_bytes(image[0x1c00:0x1c04], 'little')
    assert got_r == rdc_exp, (hex(got_r), hex(rdc_exp), 'reduction mismatch')
    for r in range(4):
        src = 0x1000 + r * 128
        dst = 0x1400 + r * 128
        assert image[dst:dst + 64] == image[src:src + 64], \
            f'loopback row {r} mismatch'


def test_two_successive_launches_preserve_dram():
    vec_n = 32
    image = make_image([(CMD_BASE, good_program(vec_n))])
    top = WrapperTop(WRAP_PARAMS, bytes(image))

    # The responder image holds the same pattern; expected outputs
    # are computed from it.
    def main():
        result = yield from launch(top,
                                   image,
                                   submit_id=0x11,
                                   cmd_base=CMD_BASE,
                                   count=15,
                                   comp_base=COMP_BASE,
                                   capacity=16)
        assert result['status'] & (1 << STATUS_DONE)
        assert not result['status'] & (1 << STATUS_ERROR)
        assert result['code'] == WrapperError.NONE
        assert result['completed'] == 15
        assert result['records'] == [(1 + i, int(Status.OK))
                                     for i in range(15)]
        check_tensor_outputs(image, vec_n)
        assert not (yield top.ram.protocol_error)

        after_first = bytes(image)

        # Second launch with a new identity: prior terminal status
        # clears on acceptance and DRAM outside the run's written
        # ranges is preserved.
        yield from program_launch(top,
                                  submit_id=0x22,
                                  cmd_base=CMD_BASE,
                                  count=15,
                                  comp_base=COMP_BASE,
                                  capacity=16)
        yield from mmio_write(top, REG_CONTROL, 1 << CONTROL_START)
        status = (yield from mmio_read(top, REG_STATUS))
        assert status & (1 << STATUS_BUSY)
        assert not status & (1 << STATUS_DONE)

        status = yield from wait_terminal(top)
        assert status & (1 << STATUS_DONE)
        assert not status & (1 << STATUS_ERROR)
        assert (yield from mmio_read(top, REG_FINISHED_ID)) == 0x22
        assert (yield from mmio_read(top, REG_COMPLETED_COUNT)) == 15
        assert read_records(image, COMP_BASE, 15) == \
            [(1 + i, int(Status.OK)) for i in range(15)]
        check_tensor_outputs(image, vec_n)
        assert not (yield top.ram.protocol_error)

        # Deterministic program: the whole memory image is identical
        # after the second run (no stray writes, DRAM preserved).
        assert bytes(image) == after_first

    run_wrap(top, main, collect_writes(top, image))


def test_watchdog_recovery_drains_outstanding_traffic():
    vec_n = 32
    image = make_image([(CMD_BASE, good_program(vec_n)),
                        (BAD_BASE, watchdog_program())])
    # Delayed responses leave transactions outstanding when the
    # watchdog expires.
    top = WrapperTop(WRAP_PARAMS, bytes(image), read_latency=6, b_latency=6)
    pristine = bytes(image)

    def main():
        result = yield from launch(top,
                                   image,
                                   submit_id=0x77,
                                   cmd_base=BAD_BASE,
                                   count=3,
                                   comp_base=COMP_BASE,
                                   capacity=4,
                                   max_cycles=80)
        status = result['status']
        assert status & (1 << STATUS_ERROR)
        assert not status & (1 << STATUS_DONE)
        assert result['code'] == WrapperError.WATCHDOG
        assert result['index'] == ERROR_INDEX_NONE
        # The load before the blocked WAIT completed and was the only
        # record committed.
        assert result['completed'] == 1
        assert result['records'] == [(1, int(Status.OK))]
        # No transaction was discarded by the recovery reset.
        assert not (yield top.ram.protocol_error)
        # Only the completion record changed in memory.
        changed = [a for a in range(len(image)) if image[a] != pristine[a]]
        assert changed, 'completion record missing'
        assert all(COMP_BASE <= a < COMP_BASE + 8 for a in changed), \
            changed[:16]
        assert image[COMP_BASE:COMP_BASE + 8] == \
            pack_completion(1, int(Status.OK))

        # Recovery leaves the wrapper usable: a clean relaunch of the
        # full workload succeeds.
        result = yield from launch(top,
                                   image,
                                   submit_id=0x88,
                                   cmd_base=CMD_BASE,
                                   count=15,
                                   comp_base=COMP_BASE,
                                   capacity=16)
        assert result['status'] & (1 << STATUS_DONE)
        assert result['completed'] == 15
        assert result['records'] == [(1 + i, int(Status.OK))
                                     for i in range(15)]
        check_tensor_outputs(image, vec_n)
        assert not (yield top.ram.protocol_error)

    run_wrap(top, main, collect_writes(top, image))


def test_fetch_fault_recovery_and_relaunch():
    vec_n = 32
    image = make_image([(CMD_BASE, good_program(vec_n)),
                        (BAD_BASE, bad_end_program())])
    top = WrapperTop(WRAP_PARAMS, bytes(image))

    def main():
        result = yield from launch(top,
                                   image,
                                   submit_id=0x55,
                                   cmd_base=BAD_BASE,
                                   count=3,
                                   comp_base=COMP_BASE,
                                   capacity=4)
        assert result['status'] & (1 << STATUS_ERROR)
        assert not result['status'] & (1 << STATUS_DONE)
        assert result['code'] == WrapperError.DECODE_ERROR
        assert result['index'] == 1
        assert result['completed'] == 1
        assert result['records'] == [(1, int(Status.OK))]
        assert not (yield top.ram.protocol_error)

        result = yield from launch(top,
                                   image,
                                   submit_id=0x66,
                                   cmd_base=CMD_BASE,
                                   count=15,
                                   comp_base=COMP_BASE,
                                   capacity=16)
        assert result['status'] & (1 << STATUS_DONE)
        assert result['completed'] == 15
        check_tensor_outputs(image, vec_n)
        assert not (yield top.ram.protocol_error)

    run_wrap(top, main, collect_writes(top, image))


def test_command_error_continues_through_end():
    cmds = [wrap_cmd(Opcode.LOAD_2D, 1, dram_addr=0x1001, row_bytes=8, rows=1)]
    cmds += [wrap_cmd(Opcode.SIGNAL, i, evt=1) for i in range(2, 20)]
    cmds += [wrap_cmd(Opcode.END, 20)]
    image = make_image([(CMD_BASE, cmds)])
    top = WrapperTop(WRAP_PARAMS, bytes(image))

    def main():
        result = yield from launch(top,
                                   image,
                                   submit_id=1,
                                   cmd_base=CMD_BASE,
                                   count=len(cmds),
                                   comp_base=COMP_BASE,
                                   capacity=len(cmds))
        assert result['code'] == WrapperError.CMD_FAILED
        assert result['index'] == 0
        assert result['status'] & (1 << STATUS_ERROR)
        assert not result['status'] & (1 << STATUS_DONE)
        assert result['completed'] == len(cmds)
        assert result['records'] == [(1, Status.BAD_ALIGN)
                                     ] + [(i, Status.OK) for i in range(2, 21)]

    run_wrap(top, main, collect_writes(top, image))


@pytest.mark.parametrize('data_width', [128, 256, 512])
def test_wide_wrapper_relaunch(data_width):
    cmds = [
        wrap_cmd(Opcode.SIGNAL, 0x7f, evt=1),
        wrap_cmd(Opcode.WAIT, 2, evt=1),
        wrap_cmd(Opcode.END, 3)
    ]
    image = make_image([(CMD_BASE, cmds)])
    top = WrapperTop(dict(WRAP_PARAMS, data_width=data_width), bytes(image))

    def main():
        for submit_id in (1, 2):
            result = yield from launch(top,
                                       image,
                                       submit_id=submit_id,
                                       cmd_base=CMD_BASE,
                                       count=3,
                                       comp_base=COMP_BASE,
                                       capacity=3)
            assert result['status'] & (1 << STATUS_DONE)
            assert result['records'] == [(0x7f, Status.OK), (2, Status.OK),
                                         (3, Status.OK)]
            assert not (yield top.ram.protocol_error)

    run_wrap(top, main, collect_writes(top, image))


def test_watchdog_remains_armed_after_command_error():
    cmds = [
        wrap_cmd(Opcode.LOAD_2D, 1, dram_addr=0x1001, row_bytes=8, rows=1),
        wrap_cmd(Opcode.WAIT, 2, evt=7),
        wrap_cmd(Opcode.END, 3)
    ]
    image = make_image([(CMD_BASE, cmds)])
    top = WrapperTop(WRAP_PARAMS, bytes(image))

    def main():
        result = yield from launch(top,
                                   image,
                                   submit_id=1,
                                   cmd_base=CMD_BASE,
                                   count=3,
                                   comp_base=COMP_BASE,
                                   capacity=3,
                                   max_cycles=150)
        assert result['code'] == WrapperError.CMD_FAILED
        assert result['status'] & (1 << STATUS_ERROR)
        assert result['status'] & (1 << STATUS_QUIESCENT)
        assert result['records'] == [(1, Status.BAD_ALIGN)]

    run_wrap(top, main, collect_writes(top, image))


def test_watchdog_does_not_reset_stalled_write_address():
    cmds = [
        wrap_cmd(Opcode.STORE_2D,
                 1,
                 dram_addr=0x1000,
                 row_bytes=64,
                 rows=4,
                 dram_stride=128,
                 tmem_stride=64),
        wrap_cmd(Opcode.WAIT, 2, evt=7),
        wrap_cmd(Opcode.END, 3)
    ]
    image = make_image([(CMD_BASE, cmds)])
    top = WrapperTop(WRAP_PARAMS, bytes(image))

    def main():
        yield top.aw_enable.eq(0)
        yield from program_launch(top,
                                  submit_id=1,
                                  cmd_base=CMD_BASE,
                                  count=3,
                                  comp_base=COMP_BASE,
                                  capacity=3,
                                  max_cycles=100)
        yield from mmio_write(top, REG_CONTROL, 1 << CONTROL_START)
        for _ in range(300):
            if (yield top.wrapper.ctrl.abort):
                break
            yield
        else:
            raise AssertionError('watchdog did not expire')
        assert (yield top.wrapper.bus.aw.valid)
        for _ in range(50):
            assert not (yield top.wrapper.core_reset)
            yield
        status = (yield from mmio_read(top, REG_STATUS))
        assert status & (1 << STATUS_BUSY)
        assert not status & (1 << STATUS_QUIESCENT)
        yield top.aw_enable.eq(1)
        status = yield from wait_terminal(top)
        assert status & (1 << STATUS_ERROR)
        assert status & (1 << STATUS_QUIESCENT)
        assert (yield from mmio_read(top,
                                     REG_ERROR_CODE)) == WrapperError.WATCHDOG
        assert not (yield top.ram.protocol_error)

    run_wrap(top, main, collect_writes(top, image))


@pytest.mark.parametrize(
    'dram_addr,rows,stride',
    [
        (0x40000000, 1, 64),  # above the image-sized memory window
        (0x3fc0, 2, 64),  # first row in window, extent crosses its end
        (0xffffffc0, 2, 64),  # stride wraps the address space to zero
    ])
def test_tensor_extent_rejected_without_bus_traffic(dram_addr, rows, stride):
    """Out-of-window tensor descriptors complete BAD_RANGE with no
    memory traffic; the wrapper stays usable afterwards."""
    row_bytes = 64
    cmds = [
        wrap_cmd(Opcode.STORE_2D,
                 1,
                 dram_addr=dram_addr,
                 row_bytes=row_bytes,
                 rows=rows,
                 dram_stride=stride,
                 tmem_stride=64),
        wrap_cmd(Opcode.SIGNAL, 2, evt=1),
        wrap_cmd(Opcode.END, 3),
    ]
    image = make_image([(CMD_BASE, cmds)])
    top = WrapperTop(WRAP_PARAMS, bytes(image))
    pristine = bytes(image)

    def main():
        result = yield from launch(top,
                                   image,
                                   submit_id=0x31,
                                   cmd_base=CMD_BASE,
                                   count=3,
                                   comp_base=COMP_BASE,
                                   capacity=3)
        assert result['status'] & (1 << STATUS_ERROR)
        assert result['code'] == WrapperError.CMD_FAILED
        assert result['index'] == 0
        assert result['records'] == [(1, int(Status.BAD_RANGE)),
                                     (2, int(Status.OK)), (3, int(Status.OK))]
        assert not (yield top.ram.protocol_error)
        # Nothing below the completion log was written: the engine
        # rejected the descriptor before issuing any request.
        changed = [a for a in range(len(image)) if image[a] != pristine[a]]
        assert changed, 'completion record missing'
        assert all(COMP_BASE <= a < COMP_BASE + 3 * 8
                   for a in changed), changed[:16]

    run_wrap(top, main, collect_writes(top, image))


def read_counters(top):
    """Snapshot counter window, 64-bit registers read low word first."""
    values = {}
    for name, off in [('cmds_done', REG_CMDS_DONE),
                      ('run_cycles', REG_RUN_CYCLES),
                      ('dram_bytes', REG_DRAM_BYTES),
                      ('dma_stalls', REG_DMA_STALLS),
                      ('tmem_conflicts', REG_TMEM_CONFLICTS),
                      ('useful_macs', REG_USEFUL_MACS),
                      ('array_starve', REG_ARRAY_STARVE),
                      ('mm_credit_stalls', REG_MM_CREDIT_STALLS),
                      ('mm_run_cycles', REG_MM_RUN_CYCLES),
                      ('mm_drain_cycles', REG_MM_DRAIN_CYCLES),
                      ('epi_records', REG_EPI_RECORDS),
                      ('epi_active_cycles', REG_EPI_ACTIVE_CYCLES),
                      ('epi_stalls', REG_EPI_STALLS),
                      ('epi_cast_ops', REG_EPI_CAST_OPS),
                      ('epi_mul_ops', REG_EPI_MUL_OPS),
                      ('epi_add_ops', REG_EPI_ADD_OPS)]:
        lo = yield from mmio_read(top, off)
        hi = yield from mmio_read(top, off + 4)
        values[name] = lo | (hi << 32)
    return values


def test_engine_counter_snapshot():
    """The counter window freezes each launch's engine counters.

    The good-program workload moves a known number of tensor-DMA bytes
    (two 4x64 loads/stores, one 128-byte and one beat-wide store), and
    a delayed responder must show up as DMA stall cycles. The snapshot
    stays readable after the next launch re-arms and resets the live
    counters.
    """
    vec_n = 32
    image = make_image([(CMD_BASE, good_program(vec_n))])
    top = WrapperTop(WRAP_PARAMS, bytes(image), read_latency=6, b_latency=6)

    def main():
        assert (yield from mmio_read(top, REG_CAPS)) \
            & (1 << CAPS_ENGINE_COUNTERS)
        assert (yield from mmio_read(top, REG_CAPS)) \
            & (1 << CAPS_MM_EPI_COUNTERS)

        result = yield from launch(top,
                                   image,
                                   submit_id=0x91,
                                   cmd_base=CMD_BASE,
                                   count=15,
                                   comp_base=COMP_BASE,
                                   capacity=16)
        assert result['status'] & (1 << STATUS_DONE)

        counters = yield from read_counters(top)
        assert counters['cmds_done'] == 15
        # loads/stores: 2 * 4 * 64 bytes, then 4 * vec_n and one beat.
        assert counters['dram_bytes'] == 2 * 4 * 64 + 4 * vec_n + BEAT
        assert counters['run_cycles'] > 0
        assert counters['dma_stalls'] > 0, 'latency must stall the DMA'
        assert counters['useful_macs'] == 0
        assert counters['array_starve'] == 0
        # No matmul in this workload: the M3b breakdown window must
        # freeze zeros, not stale or fabricated values.
        assert counters['mm_credit_stalls'] == 0
        assert counters['mm_run_cycles'] == 0
        assert counters['mm_drain_cycles'] == 0
        assert counters['epi_records'] == 0
        assert counters['epi_active_cycles'] == 0
        assert counters['epi_stalls'] == 0
        assert counters['epi_cast_ops'] == 0
        assert counters['epi_mul_ops'] == 0
        assert counters['epi_add_ops'] == 0
        first_cycles = counters['run_cycles']

        # A second launch re-arms the core and resets its live
        # counters; the window must hold the second launch's values
        # (identical here: the workload is deterministic).
        result = yield from launch(top,
                                   image,
                                   submit_id=0x92,
                                   cmd_base=CMD_BASE,
                                   count=15,
                                   comp_base=COMP_BASE,
                                   capacity=16)
        assert result['status'] & (1 << STATUS_DONE)
        counters = yield from read_counters(top)
        assert counters['cmds_done'] == 15
        assert counters['dram_bytes'] == 2 * 4 * 64 + 4 * vec_n + BEAT
        assert counters['run_cycles'] == first_cycles

    run_wrap(top, main, collect_writes(top, image))


def test_engine_counter_snapshot_fault_preserves_partial_values():
    """A watchdog-terminated launch freezes the counters of the work
    completed before the fault instead of the recovery reset's zeros."""
    vec_n = 32
    image = make_image([(CMD_BASE, good_program(vec_n)),
                        (BAD_BASE, watchdog_program())])
    top = WrapperTop(WRAP_PARAMS, bytes(image))

    def main():
        # The load retires well inside the budget; the WAIT after it
        # blocks until the watchdog expires.
        result = yield from launch(top,
                                   image,
                                   submit_id=0x93,
                                   cmd_base=BAD_BASE,
                                   count=3,
                                   comp_base=COMP_BASE,
                                   capacity=4,
                                   max_cycles=200)
        assert result['code'] == WrapperError.WATCHDOG

        # The load before the blocked WAIT finished; only its bytes
        # crossed the DMA master before the fault froze the window.
        counters = yield from read_counters(top)
        assert counters['dram_bytes'] == 4 * 64
        assert counters['cmds_done'] == 1
        assert counters['run_cycles'] > 0

        # The next clean launch replaces the fault snapshot.
        result = yield from launch(top,
                                   image,
                                   submit_id=0x94,
                                   cmd_base=CMD_BASE,
                                   count=15,
                                   comp_base=COMP_BASE,
                                   capacity=16)
        assert result['status'] & (1 << STATUS_DONE)
        counters = yield from read_counters(top)
        assert counters['cmds_done'] == 15
        assert counters['dram_bytes'] == 2 * 4 * 64 + 4 * vec_n + BEAT

    run_wrap(top, main, collect_writes(top, image))


def test_rdma_map_launch_and_mmio_region_rejected():
    """The wrapper runs at the RDMA stack's map: registers decode at
    0x40000000, DRAM sits above 2 GiB, and a tensor address inside the
    MMIO aperture is rejected before any memory traffic."""
    vec_n = 32
    # The RDMA core's map (rdma_test.py); mirrored from troom_main.
    mmio, dram = 0x40000000, 0x80000000
    # 0x40001000 is already the absolute MMIO-aperture address below
    # DRAM, so this program is *not* rebased.
    bad = [
        wrap_cmd(Opcode.LOAD_2D, 1, dram_addr=0x40001000, row_bytes=64,
                 rows=1),
        wrap_cmd(Opcode.SIGNAL, 2, evt=1),
        wrap_cmd(Opcode.END, 3)
    ]
    image = make_image([
        (CMD_BASE, rebase_program(good_program(vec_n), dram)),
        (BAD_BASE, bad),
    ])
    top = WrapperTop(WRAP_PARAMS, bytes(image), mmio_base=mmio, mem_base=dram)

    def main():
        # A descriptor aimed at the MMIO aperture fails as a command
        # error; nothing below DRAM is ever addressed.
        result = yield from launch(top,
                                   image,
                                   submit_id=0x41,
                                   cmd_base=BAD_BASE,
                                   count=3,
                                   comp_base=COMP_BASE,
                                   capacity=4)
        assert result['status'] & (1 << STATUS_ERROR)
        assert result['code'] == WrapperError.CMD_FAILED
        assert result['records'] == [(1, int(Status.BAD_RANGE)),
                                     (2, int(Status.OK)), (3, int(Status.OK))]
        assert not (yield top.ram.protocol_error)

        # The full workload then runs through the mapped window.
        result = yield from launch(top,
                                   image,
                                   submit_id=0x42,
                                   cmd_base=CMD_BASE,
                                   count=15,
                                   comp_base=COMP_BASE,
                                   capacity=16)
        assert result['status'] & (1 << STATUS_DONE)
        assert result['completed'] == 15
        check_tensor_outputs(image, vec_n)
        assert not (yield top.ram.protocol_error)

    run_wrap(top, main, collect_writes(top, image))
