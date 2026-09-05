"""pysim tests for the multi-MSHR L1 D-cache (2026-08-29 survey).

The RTLSIM sweep deadlocked for ``--l1-dcache-mshrs 2`` and segfaulted for 4
while 1 worked. The primary defect was the line-buffer request layout in
``room/dcache.py``: ``('offset', self.refill_cycles, DIR_FANOUT)`` declared an
eight-bit field instead of a three-bit one, so ``Cat(offset, id)`` was wider
than the line-buffer address port. Truncation put every refill in entries 0-7
and made drains select entries 0-15 from cache-line address bit 6 instead of
the MSHR id, corrupting load data whenever the two disagreed. Tests here:

* ``test_line_buffer_req_addr`` asserts the request address exactly covers the
  line-buffer entries (structural).
* ``test_dcache_overlapping_refill_line_buffer_isolation`` overlaps two
  refills whose address bit 6 disagrees with the owning MSHR id, the
  combination that deterministically returned stale data before the fix.
* ``test_dcache_two_concurrent_misses`` drives the DCache directly against a
  cooperative TileLink-C memory. Two lane loads to different lines must both
  allocate MSHRs and complete. This isolates the cache itself.
* ``test_cluster_dbus_source_remap_round_trip`` checks the Cluster wrapper's
  ``make_source``/``unpack_source`` pair.
* ``test_dcache_l2_*`` run the DCache against the real L2 and a coherent
  outer memory, including dirty write-back round-trips.

All processes follow the amaranth pysim clock model documented in AGENTS.md:
only a naked ``yield`` advances the clock.
"""

import json
from pathlib import Path

import pytest
from amaranth import *
from amaranth.sim import Simulator
from amaranth.utils import bits_for, log2_int

import room
from room.consts import MemoryCommand
from room.dcache import (DCache, LineBufferReadReq, LineBufferWriteReq,
                         MSHR, MSHRFile)
from roomsoc.interconnect import tilelink as tl
from roomsoc.interconnect.stream import Decoupled
from roomsoc.peripheral.l2cache import L2Cache
from tests.roomsoc.interconnect.tl_helpers import TLRamModel, tl_c_responder


def _core_params(n_mshrs):
    with open(Path(room.__file__).parent.parent
              / 'config/groom/default.json') as f:
        params = json.load(f)
    params['dcache_params']['n_mshrs'] = n_mshrs
    params['pma_regions'] = [(0, 0x40000000, 'rw', True)]
    return params


def _line_value(address):
    line = address >> 6
    return ((line + 1) << 24) | 0xA5


@pytest.mark.parametrize('n_mshrs', [1, 2, 4])
def test_line_buffer_req_addr(n_mshrs):
    params = _core_params(n_mshrs)
    for cls in (LineBufferReadReq, LineBufferWriteReq):
        req = cls(params)
        depth = req.n_mshrs * req.refill_cycles
        port_bits = log2_int(depth)
        assert len(req.offset) == log2_int(req.refill_cycles), (
            f'{cls.__name__} offset field is {len(req.offset)} bits for '
            f'{req.refill_cycles} refill beats; a wider field overlaps the '
            f'id field and the {port_bits}-bit line-buffer address port')
        assert req.addr().shape().width <= port_bits + 1, (
            f'{cls.__name__} request address {req.addr().shape().width} bits '
            f'exceeds the {port_bits}-bit line-buffer address port (the '
            f'leftover bit is the constant-zero id of n_mshrs=1)')
        entries = {(id << len(req.offset)) | offset
                   for id in range(req.n_mshrs)
                   for offset in range(req.refill_cycles)}
        assert entries == set(range(depth)), (
            f'{cls.__name__} (id, offset) does not enumerate every '
            f'line-buffer entry exactly once')


class DCacheTB(Elaboratable):

    def __init__(self, params):
        dcache_params = params['dcache_params']
        source_bits = bits_for(dcache_params['n_mshrs']
                               + dcache_params['n_iomshrs'] + 1)
        self.dcache_params = dcache_params
        self.dbus = tl.Interface(data_width=64,
                                 addr_width=32,
                                 size_width=3,
                                 source_id_width=source_bits,
                                 sink_id_width=2,
                                 has_bce=True,
                                 name='dbus')
        self.dbus_mmio = tl.Interface(data_width=64,
                                      addr_width=32,
                                      size_width=3,
                                      source_id_width=source_bits,
                                      name='dbus_mmio')
        self.dcache = DCache(self.dbus, self.dbus_mmio, params)

    def elaborate(self, platform):
        m = Module()
        m.submodules.dcache = self.dcache
        return m


def tl_memory_process(tb):
    dbus = tb.dbus
    dbus_mmio = tb.dbus_mmio

    yield dbus.a.ready.eq(0)
    yield dbus.c.ready.eq(1)
    yield dbus.e.ready.eq(1)
    yield dbus_mmio.a.ready.eq(1)
    yield dbus_mmio.d.valid.eq(0)

    while True:
        yield dbus.a.ready.eq(1)
        while not ((yield dbus.a.valid) and (yield dbus.a.ready)):
            yield
        opcode = (yield dbus.a.bits.opcode)
        source = (yield dbus.a.bits.source)
        address = (yield dbus.a.bits.address)
        size = (yield dbus.a.bits.size)
        yield dbus.a.ready.eq(0)
        yield

        nbeats = max(1, (1 << size) // (dbus.data_width // 8))
        if opcode == tl.ChannelAOpcode.AcquirePerm:
            beats = [(tl.ChannelDOpcode.Grant, None)]
        else:
            beats = [(tl.ChannelDOpcode.GrantData, beat)
                     for beat in range(nbeats)]
        for d_opcode, beat in beats:
            yield dbus.d.bits.opcode.eq(d_opcode)
            yield dbus.d.bits.param.eq(tl.CapParam.toT)
            yield dbus.d.bits.source.eq(source)
            yield dbus.d.bits.sink.eq(1)
            yield dbus.d.bits.size.eq(size)
            yield dbus.d.bits.denied.eq(0)
            yield dbus.d.bits.corrupt.eq(0)
            if beat is not None:
                yield dbus.d.bits.data.eq(_line_value(address) | (beat << 8))
            yield dbus.d.valid.eq(1)
            while not ((yield dbus.d.valid) and (yield dbus.d.ready)):
                yield
            yield
        yield dbus.d.valid.eq(0)

        while not ((yield dbus.e.valid) and (yield dbus.e.ready)):
            yield
        yield


def monitor_process(tb, responses, nacks):
    dcache = tb.dcache
    while True:
        for w in range(len(dcache.resp)):
            if (yield dcache.resp[w].valid):
                responses.append((w, (yield dcache.resp[w].bits.uop.uop_id),
                                  (yield dcache.resp[w].bits.data)))
            if (yield dcache.nack[w].valid):
                nacks.append((w, (yield dcache.nack[w].bits.uop.uop_id)))
        yield


def load_process(tb, lane, addr, uop_id, responses, nacks, timeout=5000):
    req = tb.dcache.req[lane]

    def matches(events):
        return [event for event in events if event[1] == uop_id]

    for _ in range(256):
        yield req.bits.addr.eq(addr)
        yield req.bits.uop.mem_cmd.eq(MemoryCommand.READ)
        yield req.bits.uop.mem_size.eq(2)
        yield req.bits.uop.uop_id.eq(uop_id)
        yield req.bits.uop.ldst.eq(uop_id)
        yield req.bits.uop.uses_ldq.eq(1)
        yield req.valid.eq(1)
        yield
        while not (yield req.ready):
            yield
        yield req.valid.eq(0)
        yield

        for _ in range(timeout):
            if matches(responses):
                return
            if matches(nacks):
                nacks.remove(matches(nacks)[0])
                break
            yield
        else:
            pytest.fail(f'load uop_id={uop_id} timed out')
    pytest.fail(f'load uop_id={uop_id} exhausted retries')


def _load_coro(tb, lane, addr, uop_id, responses, nacks):
    def coro():
        yield from load_process(tb, lane, addr, uop_id, responses, nacks)

    return coro


def _run_dcache_loads(tb, lane_delays, uop_addrs, responses, nacks,
                      max_cycles=3000):
    sim = Simulator(tb)
    sim.add_clock(1e-6)

    def memory():
        yield from tl_memory_process(tb)

    def monitor():
        yield from monitor_process(tb, responses, nacks)

    sim.add_sync_process(memory)
    sim.add_sync_process(monitor)
    for (lane, delay), (uop_id, addr) in zip(lane_delays, uop_addrs):
        if delay:
            def delayed(delay=delay, lane=lane, addr=addr, uop_id=uop_id):
                for _ in range(delay):
                    yield
                yield from load_process(tb, lane, addr, uop_id, responses,
                                        nacks)
            sim.add_sync_process(delayed)
        else:
            sim.add_sync_process(
                _load_coro(tb, lane, addr, uop_id, responses, nacks))
    for _ in range(max_cycles):
        if not sim.advance():
            break

    return {uop_id: data for _, uop_id, data in responses}


@pytest.mark.parametrize('n_mshrs', [1, 2, 4])
def test_dcache_two_concurrent_misses(n_mshrs):
    params = _core_params(n_mshrs)
    tb = DCacheTB(params)
    addrs = [0x1000, 0x2000]

    got = _run_dcache_loads(
        tb,
        lane_delays=[(0, 0), (1, 0)],
        uop_addrs=[(0x10 + lane, addr) for lane, addr in enumerate(addrs)],
        responses=[],
        nacks=[])

    assert got == {
        0x10: _line_value(addrs[0]),
        0x11: _line_value(addrs[1]),
    }


@pytest.mark.parametrize('n_mshrs', [2, 4])
def test_dcache_overlapping_refill_line_buffer_isolation(n_mshrs):
    """Two overlapping refills whose address bit 6 disagrees with the MSHR id.

    The MSHRFile grants the lowest-index free MSHR first, so the immediate
    lane-0 miss is owned by MSHR 0 and the delayed lane-1 miss by MSHR 1.
    Address bit 6 is chosen opposite to the owning id (0x1040 has bit 6 set,
    0x2000 clears it). With the historical oversized offset field, the
    truncated line-buffer address used that bit to select entries 8-15 for
    the drain while every refill wrote only entries 0-7, so the MSHR-0 load
    deterministically returned zeros.
    """
    params = _core_params(n_mshrs)
    tb = DCacheTB(params)
    addrs = [0x1040, 0x2000]
    assert [(addr >> 6) & 1 for addr in addrs] == [1, 0]

    got = _run_dcache_loads(
        tb,
        lane_delays=[(0, 0), (1, 25)],
        uop_addrs=[(0x10 + lane, addr) for lane, addr in enumerate(addrs)],
        responses=[],
        nacks=[])

    assert got == {
        0x10: _line_value(addrs[0]),
        0x11: _line_value(addrs[1]),
    }


@pytest.mark.parametrize('n_mshrs', [1, 2, 4])
def test_mshr_mem_grant_source_width(n_mshrs):
    params = _core_params(n_mshrs)
    mshr_file = MSHRFile(params)
    assert len(mshr_file.mem_grant.bits.source) == mshr_file.source_id_bits, (
        'MSHRFile D-channel source is not source_id_bits wide; grants '
        'addressed to higher MSHRs truncate and alias onto lower ones')
    mshr = MSHR(0, params)
    assert len(mshr.mem_grant.bits.source) == mshr.source_id_bits, (
        'MSHR D-channel source is not source_id_bits wide')


def interleaved_grant_process(tb, reply_order):
    dbus = tb.dbus
    dbus_mmio = tb.dbus_mmio

    yield dbus.a.ready.eq(0)
    yield dbus.c.ready.eq(1)
    yield dbus.e.ready.eq(1)
    yield dbus_mmio.a.ready.eq(1)
    yield dbus_mmio.d.valid.eq(0)

    pending = {}
    while len(pending) < len(reply_order):
        yield dbus.a.ready.eq(1)
        while not ((yield dbus.a.valid) and (yield dbus.a.ready)):
            yield
        pending[(yield dbus.a.bits.source)] = (
            (yield dbus.a.bits.address), (yield dbus.a.bits.size),
            (yield dbus.a.bits.opcode))
        yield
    yield dbus.a.ready.eq(0)

    for source in reply_order:
        address, size, opcode = pending[source]
        nbeats = max(1, (1 << size) // (dbus.data_width // 8))
        if opcode == tl.ChannelAOpcode.AcquirePerm:
            beats = [(tl.ChannelDOpcode.Grant, None)]
        else:
            beats = [(tl.ChannelDOpcode.GrantData, beat)
                     for beat in range(nbeats)]
        for d_opcode, beat in beats:
            yield dbus.d.bits.opcode.eq(d_opcode)
            yield dbus.d.bits.param.eq(tl.CapParam.toT)
            yield dbus.d.bits.source.eq(source)
            yield dbus.d.bits.sink.eq(1)
            yield dbus.d.bits.size.eq(size)
            yield dbus.d.bits.denied.eq(0)
            yield dbus.d.bits.corrupt.eq(0)
            if beat is not None:
                yield dbus.d.bits.data.eq(_line_value(address) | (beat << 8))
            yield dbus.d.valid.eq(1)
            while not ((yield dbus.d.valid) and (yield dbus.d.ready)):
                yield
            yield
        yield dbus.d.valid.eq(0)

        while not ((yield dbus.e.valid) and (yield dbus.e.ready)):
            yield
        yield


def test_dcache_mshr_grant_source_routing():
    """Four outstanding MSHRs receive grants in a hostile interleaved order.

    The responder collects all four acquires before answering, so MSHRs 0-3
    are simultaneously outstanding, then returns grants as sources 3, 1, 2,
    0 with per-line data. With the historical one-bit mem_grant source
    field, sources 2 and 3 truncated onto MSHRs 0 and 1, so beats reached a
    foreign MSHR and the owning load returned another line's data or
    starved.
    """
    params = _core_params(4)
    tb = DCacheTB(params)
    # Keep the misses in distinct sets. Four cold misses to the same set all
    # select the same not-yet-updated victim way, so the fourth request can be
    # victim-nacked until a refill completes. Since this responder deliberately
    # waits for all four acquires before returning any grant, same-set addresses
    # would deadlock the testbench before exercising D-source routing.
    addrs = [0x1000, 0x1040, 0x1080, 0x10c0]

    responses = []
    nacks = []
    sim = Simulator(tb)
    sim.add_clock(1e-6)

    def memory():
        yield from interleaved_grant_process(tb, [3, 1, 2, 0])

    def monitor():
        yield from monitor_process(tb, responses, nacks)

    sim.add_sync_process(memory)
    sim.add_sync_process(monitor)
    for lane in range(4):
        sim.add_sync_process(
            _load_coro(tb, lane, addrs[lane], 0x10 + lane, responses, nacks))
    for _ in range(1800):
        if not sim.advance():
            break

    assert {uop_id: data for _, uop_id, data in responses} == {
        0x10 + lane: _line_value(addr) for lane, addr in enumerate(addrs)}


class SourceRemapTB(Elaboratable):

    def __init__(self, cluster):
        self.cluster = cluster
        width = cluster.source_id_width
        self.src = Signal(width)
        self.mapped = Signal(width)
        self.rt_is_dbus = Signal()
        self.rt_core = Signal(cluster.core_bits)
        self.rt_src = Signal(width)

    def elaborate(self, platform):
        m = Module()
        cluster = self.cluster
        probe = Signal()
        m.d.sync += probe.eq(0)
        m.d.comb += self.mapped.eq(cluster.make_source(True, 0, self.src))
        is_dbus, core_id, src_id = cluster.unpack_source(self.mapped)
        m.d.comb += [
            self.rt_is_dbus.eq(is_dbus),
            self.rt_core.eq(core_id),
            self.rt_src.eq(src_id),
        ]
        return m


@pytest.mark.parametrize('n_mshrs', [1, 2, 4])
def test_cluster_dbus_source_remap_round_trip(n_mshrs):
    from groom.wrapper import Cluster

    params = dict(n_cores_per_cluster=2,
                  n_clusters=2,
                  core_params={},
                  l2cache_params=dict(in_bus=dict(source_id_width=8,
                                                  sink_id_width=4)),
                  io_regions={})
    cluster = Cluster(0, params)
    tb = SourceRemapTB(cluster)

    n_iomshrs = _core_params(n_mshrs)['dcache_params']['n_iomshrs']
    dbus_sources = list(range(n_mshrs)) + [n_mshrs + n_iomshrs]

    sim = Simulator(tb)
    sim.add_clock(1e-6)

    def process():
        for src in dbus_sources:
            yield tb.src.eq(src)
            yield
            mapped = (yield tb.mapped)
            rt = (yield tb.rt_src)
            assert (yield tb.rt_is_dbus), (
                f'dbus source {src} no longer identified as dbus '
                f'(mapped={mapped:0b})')
            assert (yield tb.rt_core) == 0
            assert rt == src, (f'dbus source {src} maps to {mapped:0b} and '
                               f'round-trips to {rt}; the L2 grant would be '
                               f'routed to the wrong MSHR')

    sim.add_sync_process(process)
    sim.run()


class DCacheL2TB(Elaboratable):
    """One cluster's DCache talking to the real L2, mirroring GroomWrapper."""

    def __init__(self, core_params, l2_params):
        in_source = l2_params['in_bus']['source_id_width']
        self.cluster_bits = Shape.cast(
            range(len(l2_params['client_source_map']) + 1)).width
        cluster_source = in_source - self.cluster_bits

        self.dcache_bus = tl.Interface(data_width=64,
                                       addr_width=32,
                                       size_width=3,
                                       source_id_width=cluster_source,
                                       sink_id_width=l2_params['in_bus']
                                       ['sink_id_width'],
                                       has_bce=True,
                                       name='dcache_bus')
        self.dbus_mmio = tl.Interface(data_width=64,
                                      addr_width=32,
                                      size_width=3,
                                      source_id_width=cluster_source,
                                      name='dbus_mmio')
        self.dcache = DCache(self.dcache_bus, self.dbus_mmio, core_params)
        self.l2 = L2Cache(l2_params)

    def elaborate(self, platform):
        m = Module()
        m.submodules.dcache = self.dcache
        m.submodules.l2 = self.l2

        cb = self.cluster_bits
        dbus = self.dcache_bus
        in_bus = self.l2.in_bus

        dbus_a = Decoupled(tl.ChannelA,
                           data_width=dbus.data_width,
                           addr_width=dbus.addr_width,
                           size_width=dbus.size_width,
                           source_id_width=in_bus.source_id_width)
        m.d.comb += [
            dbus.a.connect(dbus_a),
            dbus_a.bits.source.eq(
                Cat(dbus.a.bits.source, Const(0, cb))),
            dbus_a.connect(in_bus.a),
        ]

        dbus_c = Decoupled(tl.ChannelC,
                           data_width=dbus.data_width,
                           addr_width=dbus.addr_width,
                           size_width=dbus.size_width,
                           source_id_width=in_bus.source_id_width)
        m.d.comb += [
            dbus.c.connect(dbus_c),
            dbus_c.bits.source.eq(
                Cat(dbus.c.bits.source, Const(0, cb))),
            dbus_c.connect(in_bus.c),
        ]

        m.d.comb += dbus.e.connect(in_bus.e)
        m.d.comb += in_bus.d.connect(dbus.d)
        m.d.comb += in_bus.b.connect(dbus.b)

        m.d.comb += [
            self.dbus_mmio.a.ready.eq(1),
            self.dbus_mmio.d.valid.eq(0),
        ]
        return m


def _l2_params(n_mshrs):
    return dict(
        capacity_kb=64,
        n_ways=8,
        block_bytes=64,
        inner_beat_bytes=8,
        outer_beat_bytes=8,
        n_mshrs=n_mshrs,
        in_bus=dict(source_id_width=8, sink_id_width=4, size_width=3),
        out_bus=dict(source_id_width=2, sink_id_width=1),
        client_source_map={0: (0, 0b111110), 1: (1 << 6, 1 << 6 | 0b111110)},
    )


@pytest.mark.parametrize('n_mshrs', [1, 2, 4])
def test_dcache_l2_concurrent_misses(n_mshrs):
    core_params = _core_params(n_mshrs)
    tb = DCacheL2TB(core_params, _l2_params(4))
    depth = 16384
    model = TLRamModel(data_width=tb.l2.out_bus.data_width,
                       depth=depth,
                       init=[0xC000 + i for i in range(depth)])
    addrs = [0x1000, 0x2000, 0x3000, 0x4000]
    done = [False]

    sim = Simulator(tb)
    sim.add_clock(1e-6)

    responses = []
    nacks = []

    def outer():
        yield from tl_c_responder(tb.l2.out_bus, model=model, done=done)

    def monitor():
        yield from monitor_process(tb, responses, nacks)

    def finisher():
        for _ in range(400):
            yield
        done[0] = True

    sim.add_sync_process(outer)
    sim.add_sync_process(monitor)
    for lane, addr in enumerate(addrs):
        sim.add_sync_process(
            _load_coro(tb, lane, addr, 0x10 + lane, responses, nacks))
    sim.add_sync_process(finisher)
    for _ in range(5000):
        if not sim.advance():
            break

    got = {uop_id: data for _, uop_id, data in responses}
    expected = {
        0x10 + lane: int.from_bytes(model.get(addr, 2), 'little')
        for lane, addr in enumerate(addrs)
    }
    assert got == expected


def store_process(tb, lane, addr, value, uop_id, responses, nacks,
                  timeout=5000):
    req = tb.dcache.req[lane]

    def matches(events):
        return [event for event in events if event[1] == uop_id]

    for _ in range(256):
        yield req.bits.addr.eq(addr)
        yield req.bits.uop.mem_cmd.eq(MemoryCommand.WRITE)
        yield req.bits.uop.mem_size.eq(2)
        yield req.bits.uop.uop_id.eq(uop_id)
        yield req.bits.uop.ldst.eq(uop_id)
        yield req.bits.uop.uses_stq.eq(1)
        yield req.bits.data.eq(value)
        yield req.valid.eq(1)
        yield
        while not (yield req.ready):
            yield
        yield req.valid.eq(0)
        yield

        for _ in range(timeout):
            if matches(responses):
                return
            if matches(nacks):
                nacks.remove(matches(nacks)[0])
                break
            yield
        else:
            pytest.fail(f'store uop_id={uop_id} timed out')
    pytest.fail(f'store uop_id={uop_id} exhausted retries')


@pytest.mark.parametrize('n_mshrs', [1, 2])
def test_dcache_l2_dirty_eviction_roundtrip(n_mshrs):
    core_params = _core_params(n_mshrs)
    tb = DCacheL2TB(core_params, _l2_params(4))
    depth = 16384
    model = TLRamModel(data_width=tb.l2.out_bus.data_width,
                       depth=depth,
                       init=[0xC000 + i for i in range(depth)])
    dirty_addr = 0x1000
    written = 0xDEADBEEF
    evictors = [0x1000 + (i + 1) * 0x4000 for i in range(8)]
    done = [False]

    sim = Simulator(tb)
    sim.add_clock(1e-6)

    responses = []
    nacks = []

    def outer():
        yield from tl_c_responder(tb.l2.out_bus, model=model, done=done)

    def monitor():
        yield from monitor_process(tb, responses, nacks)

    def workload():
        yield from load_process(tb, 0, dirty_addr, 0x20, responses, nacks)
        yield from store_process(tb, 0, dirty_addr, written, 0x21, responses,
                                 nacks)
        for i, addr in enumerate(evictors):
            yield from load_process(tb, i % 4, addr, 0x30 + i, responses,
                                    nacks)
        yield from load_process(tb, 0, dirty_addr, 0x40, responses, nacks)
        for _ in range(400):
            yield
        done[0] = True

    sim.add_sync_process(outer)
    sim.add_sync_process(monitor)
    sim.add_sync_process(workload)
    for _ in range(20000):
        if not sim.advance():
            break

    got = {uop_id: data for _, uop_id, data in responses}
    assert got.get(0x40) == written, (
        f'reload after dirty eviction returned {got.get(0x40)!r}; '
        f'write-back of the dirty line was lost (got={got})')
