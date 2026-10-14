"""Reference-model agreement tests for the troom command contract.

The executable model in ``tests/troom/model.py`` defines the semantics
the RTL must match for the milestone-1 subset: ordered completions,
event semantics, 2D descriptor movement, and the alignment/range/bus
error statuses. These tests check the model in isolation and then run
one program through both the model and the RTL engine (Core plus the
shared ``AXIResponder``) comparing completions and final memory state.
"""

from amaranth import *
from amaranth.sim import Passive, Simulator

from troom.consts import Opcode, Status
from troom.core import Core
from troom.types import HasTRoomParams

from tests.roomsoc.interconnect.axi_helpers import AXIResponder
from tests.troom.model import TRoomModel, dma_cmd, simple_cmd

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
    dma_queue_depth=4,
)

BEAT = 8
# DRAM image sized to cover the program's regions plus one full-TMEM
# loopback store, keeping the responder's write-port memory small.
IMAGE_BYTES = 0x5000


def beat_word(addr):
    return ((0x1000 + addr // 8) * 0x0001000100010001) & ((1 << 64) - 1)


def fresh_dram(size=IMAGE_BYTES):
    dram = bytearray(size)
    for off in range(0, len(dram), BEAT):
        dram[off:off + BEAT] = beat_word(off).to_bytes(BEAT, 'little')
    return dram


def test_model_load_store_movement_and_statuses():
    model = TRoomModel(PARAMS, fresh_dram(0x1000), bytearray(16 * 1024))

    completions = model.run([
        dma_cmd(Opcode.LOAD_2D,
                1,
                evt=1,
                dram_addr=0x100,
                tmem_addr=0x40,
                row_bytes=32,
                rows=3,
                dram_stride=64,
                tmem_stride=64),
        simple_cmd(Opcode.WAIT, 2, evt=1),
        dma_cmd(Opcode.STORE_2D,
                3,
                evt=2,
                dram_addr=0x300,
                tmem_addr=0x40,
                row_bytes=32,
                rows=3,
                dram_stride=48,
                tmem_stride=64),
        simple_cmd(Opcode.WAIT, 4, evt=2),
        simple_cmd(Opcode.END, 5),
    ])
    assert completions == [(1, Status.OK), (2, Status.OK), (3, Status.OK),
                           (4, Status.OK), (5, Status.OK)]
    for r in range(3):
        assert model.dram[0x300 + r * 48:0x300 + r * 48 + 32] == \
            fresh_dram(0x1000)[0x100 + r * 64:0x100 + r * 64 + 32]


def test_model_rejects_misaligned_and_out_of_range():
    dram = fresh_dram(0x1000)
    tmem = bytearray(16 * 1024)
    model = TRoomModel(PARAMS, dram, tmem)

    completions = model.run([
        # row_bytes not a beat multiple.
        dma_cmd(Opcode.LOAD_2D,
                1,
                evt=1,
                dram_addr=0x100,
                tmem_addr=0,
                row_bytes=12,
                rows=1,
                dram_stride=16,
                tmem_stride=16),
        # Rows 0..1 fit, row 2 starts past the end of TMEM.
        dma_cmd(Opcode.LOAD_2D,
                2,
                evt=2,
                dram_addr=0x100,
                tmem_addr=16 * 1024 - 32,
                row_bytes=32,
                rows=3,
                dram_stride=32,
                tmem_stride=32),
        simple_cmd(Opcode.WAIT, 3, evt=1),
        simple_cmd(Opcode.WAIT, 4, evt=2),
        # Forced bus error still sets the event for the waiter.
        dma_cmd(Opcode.STORE_2D,
                5,
                evt=3,
                dram_addr=0x400,
                tmem_addr=0,
                row_bytes=32,
                rows=1,
                dram_stride=32,
                tmem_stride=32,
                force_error=True),
        simple_cmd(Opcode.WAIT, 6, evt=3),
        simple_cmd(Opcode.END, 7),
    ])
    assert completions == [
        (1, Status.BAD_ALIGN),
        (2, Status.BAD_RANGE),
        (3, Status.OK),
        (4, Status.OK),
        (5, Status.DMA_ERROR),
        (6, Status.OK),
        (7, Status.OK),
    ]
    # The in-range prefix of the BAD_RANGE load still landed in TMEM.
    assert tmem[16 * 1024 - 32:16 * 1024] == dram[0x100:0x100 + 32]
    # A forced-error store still moves its bytes and fails the
    # response, matching the RTL writer's data-then-B ordering.
    assert dram[0x400:0x400 + 32] == tmem[0:32]


class CoreHarness(Elaboratable):
    """Core attached to the shared AXI responder."""

    def __init__(self, params, image):
        self.params = params
        hp = HasTRoomParams(params)
        self.core = Core(params)

        depth = len(image) // hp.beat_bytes
        init = [
            int.from_bytes(image[o:o + hp.beat_bytes], 'little')
            for o in range(0, len(image), hp.beat_bytes)
        ]
        self.ram = AXIResponder(addr_width=hp.addr_width,
                                data_width=hp.data_width,
                                id_width=hp.id_width,
                                depth=depth,
                                init=init)

    def elaborate(self, platform):
        m = Module()
        m.submodules.core = self.core
        m.submodules.ram = self.ram
        m.d.comb += self.core.bus.connect(self.ram.bus)
        return m


def dram_write_collector(harness, image):
    """Return a sync process mirroring committed W beats into the image."""

    def collector():
        yield Passive()
        while True:
            if (yield harness.ram.w_monitor.valid):
                addr = (yield harness.ram.w_monitor.bits.addr)
                data = (yield harness.ram.w_monitor.bits.data)
                strb = (yield harness.ram.w_monitor.bits.strb)
                for lane in range(BEAT):
                    if (strb >> lane) & 1:
                        image[addr + lane] = (data >> (8 * lane)) & 0xff
            yield

    return collector


def send_cmd(cmd, entry):
    yield cmd.bits.opcode.eq(int(entry['opcode']))
    yield cmd.bits.seq.eq(entry['seq'])
    yield cmd.bits.evt.eq(entry['evt'])
    for name in ('dram_addr', 'tmem_addr', 'row_bytes', 'rows', 'dram_stride',
                 'tmem_stride'):
        if name in entry:
            yield getattr(cmd.bits, name).eq(entry[name])
    yield cmd.valid.eq(1)
    yield
    while not (yield cmd.fire):
        yield
    yield cmd.valid.eq(0)
    yield


def test_core_matches_reference_model():
    program = [
        dma_cmd(Opcode.LOAD_2D,
                1,
                evt=1,
                dram_addr=0x1000,
                tmem_addr=0x0,
                row_bytes=32,
                rows=3,
                dram_stride=64,
                tmem_stride=64),
        simple_cmd(Opcode.WAIT, 2, evt=1),
        dma_cmd(Opcode.LOAD_2D,
                3,
                evt=2,
                dram_addr=0x2000,
                tmem_addr=0x1000,
                row_bytes=16,
                rows=4,
                dram_stride=16,
                tmem_stride=32),
        simple_cmd(Opcode.WAIT, 4, evt=2),
        dma_cmd(Opcode.STORE_2D,
                5,
                evt=3,
                dram_addr=0x3000,
                tmem_addr=0x0,
                row_bytes=32,
                rows=3,
                dram_stride=64,
                tmem_stride=64),
        simple_cmd(Opcode.WAIT, 6, evt=3),
        simple_cmd(Opcode.SIGNAL, 7, evt=7),
        simple_cmd(Opcode.WAIT, 8, evt=7),
        # Misaligned row length: BAD_ALIGN, event still set for the WAIT.
        dma_cmd(Opcode.LOAD_2D,
                9,
                evt=4,
                dram_addr=0x1400,
                tmem_addr=0x2000,
                row_bytes=12,
                rows=1,
                dram_stride=16,
                tmem_stride=16),
        simple_cmd(Opcode.WAIT, 10, evt=4),
        # Full-TMEM loopback so the final TMEM state is observable in
        # DRAM. The single row makes the strides unused; tmem_stride
        # must still fit its field width.
        dma_cmd(Opcode.STORE_2D,
                11,
                evt=5,
                dram_addr=0x1000,
                tmem_addr=0x0,
                row_bytes=16 * 1024,
                rows=1,
                dram_stride=0,
                tmem_stride=0),
        simple_cmd(Opcode.WAIT, 12, evt=5),
        simple_cmd(Opcode.END, 13),
    ]

    model = TRoomModel(PARAMS, fresh_dram(), bytearray(16 * 1024))
    expected = model.run(program)

    image = fresh_dram()
    dut = CoreHarness(PARAMS, image)
    results = {}

    def tx():
        for _ in range(4):
            yield
        for entry in program:
            yield from send_cmd(dut.core.cmd, entry)

    def rx():
        yield dut.core.resp.ready.eq(1)
        for _ in range(200000):
            if (yield dut.core.resp.valid):
                seq = (yield dut.core.resp.bits.seq)
                results[seq] = Status((yield dut.core.resp.bits.status))
            yield
            if (yield dut.core.done):
                break
        else:
            raise TimeoutError('engine did not finish')
        assert not (yield dut.ram.protocol_error)

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(dram_write_collector(dut, image))
    sim.add_sync_process(tx)
    sim.add_sync_process(rx)
    sim.run()

    assert results == dict(expected)
    assert image == model.dram, \
        'final DRAM state must match the reference model'
    assert image[0x1000:0x1000 + 16 * 1024] == bytes(model.tmem), \
        'loopback store must expose the final TMEM state'
