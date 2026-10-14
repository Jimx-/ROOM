"""Focused pysim tests for the banked tensor memory.

These exercise the port contract directly: always-ready writes, one
outstanding read with held response data, the beat-interleaved bank
mapping (observed through same-bank read/write conflicts), and the
observation counters.
"""

from amaranth import *
from amaranth.sim import Simulator

import pytest

from troom.tmem import TMEM
from troom.types import HasTRoomParams

PARAMS = dict(
    addr_width=32,
    data_width=64,
    tmem_bytes=16 * 1024,
    n_tmem_banks=4,
)


def run_streams(dut, *procs):
    sim = Simulator(dut)
    sim.add_clock(1e-6)
    for proc in procs:
        sim.add_sync_process(proc)
    sim.run()


def beat_value(addr):
    """Deterministic per-beat pattern derived from the byte address."""
    return ((addr // 8) * 0x0001000100010001) & ((1 << 64) - 1)


def tmem_write(write, addr, data):
    yield write.bits.addr.eq(addr)
    yield write.bits.data.eq(data)
    yield write.valid.eq(1)
    yield
    while not (yield write.fire):
        yield
    yield write.valid.eq(0)
    yield


def tmem_read(rreq, rresp, addr, *, hold=0):
    yield rreq.bits.addr.eq(addr)
    yield rreq.valid.eq(1)
    yield
    while not (yield rreq.fire):
        yield
    yield rreq.valid.eq(0)
    # The response is available one cycle after acceptance.
    yield
    while not (yield rresp.valid):
        yield
    data = (yield rresp.bits.data)
    for _ in range(hold):
        # Backpressure: valid and data must hold stable.
        yield
        assert (yield rresp.valid)
        assert (yield rresp.bits.data) == data
    yield rresp.ready.eq(1)
    yield
    yield rresp.ready.eq(0)
    yield
    return data


def test_write_then_read_roundtrip():
    dut = TMEM(PARAMS)
    addrs = [0, 8, 16, 24, 32, 1000, 8192, 16376]

    def proc():
        for addr in addrs:
            yield from tmem_write(dut.write, addr, beat_value(addr))
        assert (yield dut.write_beats) == len(addrs)
        for addr in addrs:
            data = yield from tmem_read(dut.rreq, dut.rresp, addr)
            assert data == beat_value(addr), hex(addr)
        assert (yield dut.read_beats) == len(addrs)
        assert (yield dut.conflicts) == 0

    run_streams(dut, proc)


def test_read_response_backpressure_holds_data():
    dut = TMEM(PARAMS)

    def proc():
        yield from tmem_write(dut.write, 64, beat_value(64))
        data = yield from tmem_read(dut.rreq, dut.rresp, 64, hold=4)
        assert data == beat_value(64)
        # The port is free again after the response fires.
        data = yield from tmem_read(dut.rreq, dut.rresp, 64)
        assert data == beat_value(64)

    run_streams(dut, proc)


def test_single_outstanding_read():
    dut = TMEM(PARAMS)

    def proc():
        yield from tmem_write(dut.write, 0, 0xA1)
        yield from tmem_write(dut.write, 8, 0xB2)

        # First read is accepted immediately; drop valid in the same
        # cycle so exactly one request is taken.
        yield dut.rreq.bits.addr.eq(0)
        yield dut.rreq.valid.eq(1)
        yield
        assert (yield dut.rreq.fire)
        yield dut.rreq.valid.eq(0)
        yield

        # A second request cannot be accepted while the response is
        # pending and unconsumed.
        yield dut.rreq.bits.addr.eq(8)
        yield dut.rreq.valid.eq(1)
        yield
        assert not (yield dut.rreq.ready)

        # Consume the first response; its fire frees the port for the
        # held request in the same cycle.
        yield dut.rresp.ready.eq(1)
        yield
        assert (yield dut.rresp.fire)
        assert (yield dut.rresp.bits.data) == 0xA1
        assert (yield dut.rreq.fire)
        yield dut.rreq.valid.eq(0)
        yield dut.rresp.ready.eq(0)
        yield

        while not (yield dut.rresp.valid):
            yield
        assert (yield dut.rresp.bits.data) == 0xB2
        yield dut.rresp.ready.eq(1)
        yield
        yield dut.rresp.ready.eq(0)

    run_streams(dut, proc)


def test_same_bank_read_write_conflict():
    """A read into the bank being written is back-pressured and counted.

    With 4 banks of 8-byte beats, addresses 0 and 32 map to the same
    bank (word indices 0 and 4); the read must wait for the write cycle
    to pass, and the old contents at 32 stay intact.
    """
    dut = TMEM(PARAMS)

    def wr():
        for _ in range(5):
            yield
        yield dut.write.bits.addr.eq(0)
        yield dut.write.bits.data.eq(0xDEADBEEF)
        yield dut.write.valid.eq(1)
        yield  # the write commits in this cycle
        yield dut.write.valid.eq(0)
        yield

    def rd():
        yield from tmem_write(dut.write, 32, beat_value(32))
        for _ in range(3):
            yield
        yield dut.rreq.bits.addr.eq(32)
        yield dut.rreq.valid.eq(1)
        yield  # same cycle as the write: same bank, must stall
        assert not (yield dut.rreq.ready)
        while not (yield dut.rreq.fire):
            yield
        yield dut.rreq.valid.eq(0)
        yield
        while not (yield dut.rresp.valid):
            yield
        assert (yield dut.rresp.bits.data) == beat_value(32)
        yield dut.rresp.ready.eq(1)
        yield
        assert (yield dut.conflicts) == 1

    run_streams(dut, wr, rd)


def test_different_bank_no_conflict():
    """Addresses 0 and 8 live in different banks and never conflict."""
    dut = TMEM(PARAMS)

    def wr():
        for _ in range(5):
            yield
        yield dut.write.bits.addr.eq(0)
        yield dut.write.bits.data.eq(0xDEADBEEF)
        yield dut.write.valid.eq(1)
        yield
        yield dut.write.valid.eq(0)
        yield

    def rd():
        yield from tmem_write(dut.write, 8, beat_value(8))
        for _ in range(3):
            yield
        yield dut.rreq.bits.addr.eq(8)
        yield dut.rreq.valid.eq(1)
        yield  # same cycle as the write, different bank: accepted now
        assert (yield dut.rreq.ready)
        while not (yield dut.rreq.fire):
            yield
        yield dut.rreq.valid.eq(0)
        yield
        while not (yield dut.rresp.valid):
            yield
        assert (yield dut.rresp.bits.data) == beat_value(8)
        yield dut.rresp.ready.eq(1)
        yield
        assert (yield dut.conflicts) == 0

    run_streams(dut, wr, rd)


def test_tmem_params_validation():
    with pytest.raises(ValueError):
        # 16392 is not 4 banks x 8 bytes divisible.
        HasTRoomParams(dict(PARAMS, tmem_bytes=16 * 1024 + 8))

    with pytest.raises(ValueError):
        # row_bytes_bits too narrow to express one beat.
        HasTRoomParams(dict(PARAMS, row_bytes_bits=3))

    with pytest.raises(ValueError):
        HasTRoomParams(dict(PARAMS, rows_bits=0))

    with pytest.raises(ValueError):
        HasTRoomParams(dict(PARAMS, dma_queue_depth=0))

    # The reference geometry is valid, including the full 256 KiB.
    HasTRoomParams(dict(tmem_bytes=256 * 1024, n_tmem_banks=4))
