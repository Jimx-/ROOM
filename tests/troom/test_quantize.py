"""Focused pysim tests for the troom activation loader.

The loader copies a host-quantized activation image from two dense
TMEM regions (quants, then per-segment metadata) into the matrix
unit's ACT RAM through its upload port. These tests drive real banked
TMEM contents, check every uploaded entry field by field at several
beat widths (a wide beat spans several segments' quants and many
metadata entries), pin-based backpressure, and descriptor rejection.
"""

from amaranth import *
from amaranth.sim import Passive, Simulator

import pytest

from troom.consts import Status
from troom.quantize import ActLoader
from troom.tmem import TMEM
from troom.types import HasTRoomParams

PARAMS = dict(
    addr_width=32,
    data_width=64,
    id_width=4,
    seg_len=32,
    n_outputs=2,
    n_tokens=1,
    tmem_bytes=16 * 1024,
    n_tmem_banks=4,
    act_max_k=1024,
)

BEAT = 8
A_BASE = 0x100
M_BASE = 0x400


def act_image(nsegs, rng):
    """Build per-segment ACT entries plus region byte images."""
    segs = []
    for _ in range(nsegs):
        quants = [rng.randint(-128, 127) for _ in range(32)]
        s_act = rng.randint(0, 0xFFFFFFFF)
        sum_a = sum(quants)
        segs.append(dict(quants=quants, s_act=s_act, sum_a=sum_a))
    qbytes = bytearray()
    for s in segs:
        qbytes += bytes(q & 0xFF for q in s['quants'])
    mbytes = bytearray()
    for s in segs:
        mbytes += s['s_act'].to_bytes(4, 'little')
        mbytes += (s['sum_a'] & 0xFFFF).to_bytes(2, 'little')
        mbytes += b'\x00\x00'
    return segs, bytes(qbytes), bytes(mbytes)


def pad(data, beat):
    return data + bytes((-len(data)) % beat)


class Harness(Elaboratable):
    """ActLoader reading from a banked TMEM; writes are captured."""

    def __init__(self, params):
        self.params = params
        self.tmem = TMEM(params)
        self.loader = ActLoader(params)
        self.pin = Signal()

    def elaborate(self, platform):
        m = Module()
        m.submodules.tmem = self.tmem
        m.submodules.loader = self.loader
        m.d.comb += [
            self.loader.tmem_rreq.connect(self.tmem.rreq),
            self.tmem.rresp.connect(self.loader.tmem_rresp),
            self.loader.act_wr.ready.eq(~self.pin),
        ]
        return m


def run_load(params, segs, qbytes, mbytes, *, pin_cycles=0, deadline=20000):
    hp = HasTRoomParams(params)
    dut = Harness(params)
    writes = []
    result = {}

    def tx():
        # Fill TMEM with the two region images through the write port.
        qimg = pad(qbytes, hp.beat_bytes)
        mimg = pad(mbytes, hp.beat_bytes)
        for off in range(0, len(qimg), hp.beat_bytes):
            word = int.from_bytes(qimg[off:off + hp.beat_bytes], 'little')
            yield from tmem_write(dut.tmem.write, A_BASE + off, word)
        for off in range(0, len(mimg), hp.beat_bytes):
            word = int.from_bytes(mimg[off:off + hp.beat_bytes], 'little')
            yield from tmem_write(dut.tmem.write, M_BASE + off, word)

        sink = dut.loader.sink
        yield sink.bits.a_base.eq(A_BASE)
        yield sink.bits.m_base.eq(M_BASE)
        yield sink.bits.k.eq(len(segs) * 32)
        yield sink.valid.eq(1)
        yield
        while not (yield sink.fire):
            yield
        yield sink.valid.eq(0)
        yield

        for i in range(deadline):
            yield
            if pin_cycles and i < pin_cycles:
                yield dut.pin.eq(1)
            else:
                yield dut.pin.eq(0)
            if (yield dut.loader.done):
                result['status'] = Status((yield dut.loader.status))
                result['segs'] = (yield dut.loader.segs)
                break
        else:
            raise TimeoutError('loader did not finish')

    def watch():
        yield Passive()
        wr = dut.loader.act_wr
        while True:
            if (yield wr.fire):
                quants = (yield wr.bits.quants)
                writes.append(
                    dict(
                        seg=(yield wr.bits.seg),
                        quants=[(quants >> (8 * i)) & 0xFF for i in range(32)],
                        s_act=(yield wr.bits.s_act),
                        sum_a=(yield wr.bits.sum_a),
                    ))
            yield

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(tx)
    sim.add_sync_process(watch)
    sim.run()
    return writes, result


def tmem_write(write, addr, word):
    yield write.bits.addr.eq(addr)
    yield write.bits.data.eq(word)
    yield write.valid.eq(1)
    yield
    while not (yield write.fire):
        yield
    yield write.valid.eq(0)
    yield


def check_entries(writes, segs):
    assert len(writes) == len(segs)
    for w, s in zip(writes, segs):
        assert w['seg'] == segs.index(s), 'segment order'
        exp_q = [q & 0xFF for q in s['quants']]
        got_q = [q - 256 if q >= 128 else q for q in w['quants']]
        assert got_q == s['quants'], (w['seg'], got_q[:4], s['quants'][:4])
        assert w['s_act'] == s['s_act'], w['seg']
        exp_sum = s['sum_a'] if s['sum_a'] < 32768 else s['sum_a'] - 65536
        assert w['sum_a'] == exp_sum, w['seg']


def test_act_load_entries_beat64():
    import random

    rng = random.Random(7)
    segs, qbytes, mbytes = act_image(6, rng)
    writes, res = run_load(dict(PARAMS), segs, qbytes, mbytes)
    assert res['status'] is Status.OK
    assert res['segs'] == 6
    check_entries(writes, segs)


def test_act_load_entries_beat512():
    """One beat spans two segments' quants and eight metadata rows."""
    import random

    rng = random.Random(8)
    params = dict(PARAMS, data_width=512, n_tmem_banks=8)
    segs, qbytes, mbytes = act_image(11, rng)
    writes, res = run_load(params, segs, qbytes, mbytes)
    assert res['status'] is Status.OK
    check_entries(writes, segs)


def test_act_load_single_segment():
    import random

    rng = random.Random(9)
    segs, qbytes, mbytes = act_image(1, rng)
    writes, res = run_load(dict(PARAMS), segs, qbytes, mbytes)
    assert res['status'] is Status.OK
    check_entries(writes, segs)


def test_act_load_rejects_region_size_overflow():
    """A large ACT capacity must not wrap the TMEM range check."""
    dut = ActLoader(dict(PARAMS, act_max_k=65536))

    def tx():
        yield dut.sink.bits.k.eq(65536)
        yield dut.sink.bits.a_base.eq(0)
        yield dut.sink.bits.m_base.eq(0)
        yield dut.sink.valid.eq(1)
        yield
        assert (yield dut.done)
        assert (yield dut.status) == Status.BAD_RANGE
        assert not (yield dut.tmem_rreq.valid)

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(tx)
    sim.run()


def test_act_load_pinned_between_segments():
    """Backpressure mid-load only stalls; completion still true."""
    import random

    rng = random.Random(10)
    segs, qbytes, mbytes = act_image(5, rng)
    writes, res = run_load(dict(PARAMS), segs, qbytes, mbytes, pin_cycles=30)
    assert res['status'] is Status.OK
    check_entries(writes, segs)


@pytest.mark.parametrize('k,a_base,m_base,why', [
    (33, A_BASE, M_BASE, 'k not a multiple of 32'),
    (0, A_BASE, M_BASE, 'k zero'),
    (2048, A_BASE, M_BASE, 'k beyond act_max_k'),
    (64, A_BASE + 1, M_BASE, 'quants base misaligned'),
    (64, A_BASE, M_BASE + 4, 'meta base misaligned'),
    (1024, 0x3F80, M_BASE, 'quants region off the end'),
    (1024, A_BASE, 0x3F80, 'meta region off the end'),
])
def test_act_load_rejects_bad_descriptors(k, a_base, m_base, why):
    dut = Harness(dict(PARAMS))
    result = {}

    def tx():
        yield from tmem_write(dut.tmem.write, 0, 0)
        sink = dut.loader.sink
        yield sink.bits.a_base.eq(a_base)
        yield sink.bits.m_base.eq(m_base)
        yield sink.bits.k.eq(k)
        yield sink.valid.eq(1)
        for _ in range(16):
            yield
            if (yield dut.loader.done):
                result['status'] = Status((yield dut.loader.status))
                return
        raise TimeoutError(f'no completion for {why}')

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(tx)
    sim.run()

    want = (Status.BAD_ALIGN if 'misaligned' in why else
            Status.BAD_RANGE if 'off the end' in why else Status.BAD_SHAPE)
    assert result['status'] is want, why
