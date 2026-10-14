"""Focused pysim tests for the troom tensor DMA.

Covers 2D descriptor expansion (rows, strides, gaps), TMEM steering,
completion semantics (final beat committed for loads, final B response
for stores), queued back-to-back commands, overlapping load/store
engines, alignment/range/bus-error statuses, the DRAM memory-window
check (including stride wraparound), and the counters. The bus
side runs against the shared ``AXIResponder`` from the repo's AXI
testbench kit; a collector mirrors its committed W beats into the
Python-side DRAM image for store verification.
"""

from amaranth import *
from amaranth.hdl.rec import Record
from amaranth.sim import Passive, Simulator

import pytest

from troom.consts import Status
from troom.dma import DMA
from troom.tmem import TMEM
from troom.types import HasTRoomParams

from roomsoc.interconnect.axi import AXIInterface
from roomsoc.interconnect.stream import Decoupled

from tests.roomsoc.interconnect.axi_helpers import AXIResponder

PARAMS = dict(
    addr_width=32,
    data_width=64,
    id_width=4,
    tmem_bytes=16 * 1024,
    n_tmem_banks=4,
    dma_queue_depth=4,
)

BEAT = 8


class Harness(Elaboratable):
    """DMA + TMEM wired together behind an ``AXIResponder``, with a test
    read port muxed onto the TMEM read path."""

    def __init__(self,
                 params,
                 image,
                 *,
                 r_latency=2,
                 b_latency=2,
                 r_resp=0,
                 b_resp=0,
                 mem_base=0,
                 mem_size=None):
        self.params = params
        hp = HasTRoomParams(params)
        self.hp = hp

        self.bus = AXIInterface(data_width=hp.data_width,
                                addr_width=hp.addr_width,
                                id_width=hp.id_width)
        self.tmem = TMEM(params)
        self.dma = DMA(params, self.bus, mem_base=mem_base, mem_size=mem_size)

        depth = len(image) // hp.beat_bytes
        init = [
            int.from_bytes(image[o:o + hp.beat_bytes], 'little')
            for o in range(0, len(image), hp.beat_bytes)
        ]
        self.ram = AXIResponder(addr_width=hp.addr_width,
                                data_width=hp.data_width,
                                id_width=hp.id_width,
                                depth=depth,
                                init=init,
                                read_latency=r_latency,
                                b_latency=b_latency,
                                r_resp=r_resp,
                                b_resp=b_resp)

        self.test_rreq = Decoupled(Record, [('addr', hp.tmem_addr_width)])
        self.test_rresp = Decoupled(Record, [('data', hp.data_width)])

    def elaborate(self, platform):
        m = Module()
        m.submodules.tmem = self.tmem
        m.submodules.dma = self.dma
        m.submodules.ram = self.ram

        m.d.comb += [
            self.bus.connect(self.ram.bus),
            self.dma.tmem_w.connect(self.tmem.write),
        ]

        # The test port has priority; the DMA store path holds its
        # request when both are presented.
        owner_test = Signal()
        with m.If(self.tmem.rreq.fire):
            m.d.sync += owner_test.eq(self.test_rreq.fire)

        m.d.comb += [
            self.tmem.rreq.valid.eq(self.test_rreq.valid
                                    | self.dma.rreq.valid),
            self.tmem.rreq.bits.addr.eq(
                Mux(self.test_rreq.valid, self.test_rreq.bits.addr,
                    self.dma.rreq.bits.addr)),
            self.test_rreq.ready.eq(self.tmem.rreq.ready
                                    & self.test_rreq.valid),
            self.dma.rreq.ready.eq(self.tmem.rreq.ready
                                   & ~self.test_rreq.valid),
            self.test_rresp.valid.eq(self.tmem.rresp.valid & owner_test),
            self.test_rresp.bits.data.eq(self.tmem.rresp.bits.data),
            self.dma.rresp.valid.eq(self.tmem.rresp.valid & ~owner_test),
            self.dma.rresp.bits.data.eq(self.tmem.rresp.bits.data),
            self.tmem.rresp.ready.eq(
                Mux(owner_test, self.test_rresp.ready, self.dma.rresp.ready)),
        ]
        return m


def beat_word(addr):
    """Deterministic DRAM pattern for the beat at byte address ``addr``."""
    return ((0x1000 + addr // 8) * 0x0001000100010001) & ((1 << 64) - 1)


def fresh_image(size=8 * 1024):
    """Patterned DRAM image; sized to keep the responder memory small."""
    image = bytearray(size)
    for off in range(0, len(image), BEAT):
        image[off:off + BEAT] = beat_word(off).to_bytes(BEAT, 'little')
    return image


def dram_write_collector(harness, image):
    """Return a sync process mirroring the responder's committed W beats
    into the Python-side DRAM image."""

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


def send_desc(sink, store, **fields):
    yield sink.bits.store.eq(store)
    for name, value in fields.items():
        yield getattr(sink.bits, name).eq(value)
    yield sink.valid.eq(1)
    yield
    while not (yield sink.fire):
        yield
    yield sink.valid.eq(0)
    yield


def wait_dma_done(dut, timeout=50000):
    for _ in range(timeout):
        if (yield dut.dma.done):
            return Status((yield dut.dma.status))
        yield
    raise TimeoutError('DMA command did not complete')


def tmem_read(dut, addr):
    rreq, rresp = dut.test_rreq, dut.test_rresp
    yield rreq.bits.addr.eq(addr)
    yield rreq.valid.eq(1)
    yield
    while not (yield rreq.fire):
        yield
    yield rreq.valid.eq(0)
    yield
    while not (yield rresp.valid):
        yield
    data = (yield rresp.bits.data)
    yield rresp.ready.eq(1)
    yield
    yield rresp.ready.eq(0)
    yield
    return data


def run_harness(dut, image, *procs):
    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(dram_write_collector(dut, image))
    for proc in procs:
        sim.add_sync_process(proc)
    sim.run()


def test_load_2d_expands_rows_and_strides():
    image = fresh_image()
    dut = Harness(PARAMS, image)

    dram_addr = 0x100
    tmem_addr = 0x40
    rows, row_bytes = 4, 64
    dram_stride, tmem_stride = 128, 256

    def proc():
        yield from send_desc(dut.dma.sink,
                             store=0,
                             dram_addr=dram_addr,
                             tmem_addr=tmem_addr,
                             row_bytes=row_bytes,
                             rows=rows,
                             dram_stride=dram_stride,
                             tmem_stride=tmem_stride)
        status = yield from wait_dma_done(dut)
        assert status == Status.OK
        for r in range(rows):
            for off in range(0, row_bytes, BEAT):
                data = yield from tmem_read(dut,
                                            tmem_addr + r * tmem_stride + off)
                assert data == beat_word(dram_addr + r * dram_stride +
                                         off), (r, off)
        assert (yield dut.dma.dram_bytes) == rows * row_bytes
        assert not (yield dut.dma.active)
        assert not (yield dut.ram.protocol_error)

    run_harness(dut, image, proc)


def test_load_commands_queue_back_to_back():
    """A second load is accepted while the first still streams."""
    image = fresh_image()
    dut = Harness(PARAMS, image, r_latency=4)

    big = dict(dram_addr=0x1000,
               tmem_addr=0x000,
               row_bytes=128,
               rows=16,
               dram_stride=128,
               tmem_stride=128)
    small = dict(dram_addr=0x1c00,
                 tmem_addr=0x2000,
                 row_bytes=64,
                 rows=1,
                 dram_stride=64,
                 tmem_stride=64)

    seen = {}

    def proc():
        yield from send_desc(dut.dma.sink, store=0, **big)
        yield from send_desc(dut.dma.sink, store=0, **small)
        # The second command was accepted into the DMA queue while the
        # first transfer was still active.
        assert (yield dut.dma.active)
        seen['queued'] = (yield dut.dma.queued)
        for _ in range(2):
            status = yield from wait_dma_done(dut)
            assert status == Status.OK
        for off in (0, 64, 120):
            data = yield from tmem_read(dut, big['tmem_addr'] + off)
            assert data == beat_word(big['dram_addr'] + off)
        data = yield from tmem_read(dut, small['tmem_addr'])
        assert data == beat_word(small['dram_addr'])

    run_harness(dut, image, proc)
    assert seen['queued'] == 1


def test_store_2d_roundtrip_and_completion():
    """Loads retire on the last TMEM beat, stores on the final B response."""
    image = fresh_image()
    dut = Harness(PARAMS, image)

    load = dict(dram_addr=0x800,
                tmem_addr=0x80,
                row_bytes=32,
                rows=4,
                dram_stride=32,
                tmem_stride=64)
    store = dict(dram_addr=0x1000,
                 tmem_addr=0x80,
                 row_bytes=32,
                 rows=4,
                 dram_stride=48,
                 tmem_stride=64)

    def proc():
        yield from send_desc(dut.dma.sink, store=0, **load)
        assert (yield from wait_dma_done(dut)) == Status.OK
        yield from send_desc(dut.dma.sink, store=1, **store)
        assert (yield from wait_dma_done(dut)) == Status.OK

        for r in range(store['rows']):
            src = load['dram_addr'] + r * load['row_bytes']
            dst = store['dram_addr'] + r * store['dram_stride']
            assert image[dst:dst +
                         store['row_bytes']] == image[src:src +
                                                      load['row_bytes']]

        assert (yield
                dut.dma.dram_bytes) == ((load['rows'] * load['row_bytes']) +
                                        (store['rows'] * store['row_bytes']))

    run_harness(dut, image, proc)


def test_store_overlaps_active_load():
    """The store engine runs concurrently with an active load."""
    image = fresh_image()
    dut = Harness(PARAMS, image, r_latency=3)

    seed = dict(dram_addr=0x1c00,
                tmem_addr=0x2000,
                row_bytes=64,
                rows=2,
                dram_stride=64,
                tmem_stride=64)
    load = dict(dram_addr=0x1000,
                tmem_addr=0x0,
                row_bytes=128,
                rows=8,
                dram_stride=128,
                tmem_stride=128)
    store = dict(dram_addr=0xa00,
                 tmem_addr=0x2000,
                 row_bytes=64,
                 rows=2,
                 dram_stride=64,
                 tmem_stride=64)

    def proc():
        yield from send_desc(dut.dma.sink, store=0, **seed)
        assert (yield from wait_dma_done(dut)) == Status.OK
        # A big load and a store of unrelated regions issued
        # back-to-back; both engines run concurrently.
        yield from send_desc(dut.dma.sink, store=0, **load)
        yield from send_desc(dut.dma.sink, store=1, **store)
        for _ in range(2):
            assert (yield from wait_dma_done(dut)) == Status.OK

        for r in range(store['rows']):
            for off in range(0, store['row_bytes'], BEAT):
                dst = store['dram_addr'] + r * store['dram_stride'] + off
                assert image[dst:dst + BEAT] == \
                    beat_word(seed['dram_addr'] +
                              r * seed['dram_stride'] + off).to_bytes(
                                  BEAT, 'little')
        for off in (0, 64, 120):
            data = yield from tmem_read(dut, load['tmem_addr'] + off)
            assert data == beat_word(load['dram_addr'] + off)
        assert not (yield dut.ram.protocol_error)

    run_harness(dut, image, proc)


@pytest.mark.parametrize(
    'field, value',
    [
        ('row_bytes', 12),  # not a beat multiple
        ('rows', 0),
        ('dram_addr', 4),  # unaligned address
        ('tmem_addr', 4),
        ('dram_stride', 12),
        ('tmem_stride', 4),
        ('row_bytes', 0),
    ])
def test_load_misaligned_descriptor_rejected(field, value):
    image = fresh_image()
    dut = Harness(PARAMS, image)

    desc = dict(dram_addr=0x100,
                tmem_addr=0x0,
                row_bytes=16,
                rows=2,
                dram_stride=16,
                tmem_stride=16)
    desc[field] = value

    def proc():
        yield from send_desc(dut.dma.sink, store=0, **desc)
        status = yield from wait_dma_done(dut)
        assert status == Status.BAD_ALIGN
        assert (yield dut.dma.dram_bytes) == 0

    run_harness(dut, image, proc)


def test_load_out_of_tmem_range_keeps_prefix():
    image = fresh_image()
    dut = Harness(PARAMS, image)

    # Row 1 starts exactly at the end of TMEM: only row 0 is moved.
    load = dict(dram_addr=0x100,
                tmem_addr=16 * 1024 - 64,
                row_bytes=64,
                rows=2,
                dram_stride=64,
                tmem_stride=64)

    def proc():
        yield from send_desc(dut.dma.sink, store=0, **load)
        assert (yield from wait_dma_done(dut)) == Status.BAD_RANGE
        for off in (0, 32, 56):
            data = yield from tmem_read(dut, load['tmem_addr'] + off)
            assert data == beat_word(load['dram_addr'] + off)
        assert (yield dut.dma.dram_bytes) == 64

    run_harness(dut, image, proc)


def test_load_read_fault_reports_dma_error():
    image = fresh_image()
    dut = Harness(PARAMS, image, r_resp=2)

    load = dict(dram_addr=0x200,
                tmem_addr=0x100,
                row_bytes=32,
                rows=2,
                dram_stride=32,
                tmem_stride=32)

    def proc():
        yield from send_desc(dut.dma.sink, store=0, **load)
        assert (yield from wait_dma_done(dut)) == Status.DMA_ERROR

    run_harness(dut, image, proc)


def test_store_write_fault_reports_dma_error():
    image = fresh_image()
    dut = Harness(PARAMS, image, b_resp=2)

    load = dict(dram_addr=0x100,
                tmem_addr=0x0,
                row_bytes=64,
                rows=1,
                dram_stride=64,
                tmem_stride=64)
    store = dict(dram_addr=0x400,
                 tmem_addr=0x0,
                 row_bytes=64,
                 rows=1,
                 dram_stride=64,
                 tmem_stride=64)

    def proc():
        yield from send_desc(dut.dma.sink, store=0, **load)
        assert (yield from wait_dma_done(dut)) == Status.OK
        yield from send_desc(dut.dma.sink, store=1, **store)
        assert (yield from wait_dma_done(dut)) == Status.DMA_ERROR

    run_harness(dut, image, proc)


@pytest.mark.parametrize('store', [False, True])
def test_dram_window_rejects_out_of_range(store):
    """A descriptor whose 2D extent leaves the DRAM window completes
    BAD_RANGE without issuing any bus traffic."""
    image = fresh_image()
    pristine = bytes(image)
    # Window [0x1000, 0x2000), inside the patterned image.
    dut = Harness(PARAMS, image, mem_base=0x1000, mem_size=0x1000)

    def proc():
        yield from send_desc(
            dut.dma.sink,
            store=store,
            dram_addr=0x800,  # below the window
            tmem_addr=0x40,
            row_bytes=64,
            rows=1,
            dram_stride=64,
            tmem_stride=64)
        assert (yield from wait_dma_done(dut)) == Status.BAD_RANGE

        yield from send_desc(
            dut.dma.sink,
            store=store,
            dram_addr=0x1fc0,  # last row crosses
            tmem_addr=0x40,
            row_bytes=64,
            rows=2,
            dram_stride=64,
            tmem_stride=64)
        assert (yield from wait_dma_done(dut)) == Status.BAD_RANGE

        assert (yield dut.dma.dram_bytes) == 0
        assert not (yield dut.ram.protocol_error)

        # Boundary: an extent ending exactly at the window end runs.
        yield from send_desc(dut.dma.sink,
                             store=store,
                             dram_addr=0x1fc0,
                             tmem_addr=0x40,
                             row_bytes=64,
                             rows=1,
                             dram_stride=64,
                             tmem_stride=64)
        assert (yield from wait_dma_done(dut)) == Status.OK
        assert (yield dut.dma.dram_bytes) == 64
        assert not (yield dut.ram.protocol_error)

    run_harness(dut, image, proc)

    if store:
        # The boundary store committed only TMEM (zeros) at its row.
        assert image[0x1fc0:0x2000] == bytes(64)
        assert (image[:0x1fc0] + image[0x2000:]) == \
            pristine[:0x1fc0] + pristine[0x2000:]
    else:
        assert bytes(image) == pristine


@pytest.mark.parametrize('store', [False, True])
def test_dram_stride_wrap_rejected(store):
    """A stride that wraps the address space is caught by the widened
    extent check even when the window covers the whole space."""
    image = fresh_image()
    pristine = bytes(image)
    dut = Harness(PARAMS, image)  # window [0, 2**32)

    def proc():
        yield from send_desc(dut.dma.sink,
                             store=store,
                             dram_addr=0xffffffc0,
                             tmem_addr=0x40,
                             row_bytes=64,
                             rows=2,
                             dram_stride=64,
                             tmem_stride=64)
        assert (yield from wait_dma_done(dut)) == Status.BAD_RANGE
        assert (yield dut.dma.dram_bytes) == 0
        assert not (yield dut.ram.protocol_error)

    run_harness(dut, image, proc)
    assert bytes(image) == pristine
