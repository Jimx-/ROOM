import random
from collections import deque

import pytest

from amaranth import Module
from amaranth.sim import Passive, Simulator

from troom.consts import Format, Status
from troom.stream import (
    ReadPort,
    ReadRegion,
    ReadTopology,
    StreamGemv,
    StreamMultiPortReader,
    pack_stream_q8_0,
)
from troom.platforms.zu15 import zu15_read_topology
from tests.troom.model import split_sg1_payload
from tests.troom.test_stream import (
    PARAMS,
    act_write,
    check_records,
    make_tile,
    push_desc,
    record_fields,
)


def test_hp_ports_issue_independently_and_retire_in_order():
    params = dict(PARAMS, data_width=512)
    dut = StreamMultiPortReader(params, zu15_read_topology(params))
    hp_buses = tuple(dut.aux_buses.values())
    sim = Simulator(dut)
    sim.add_clock(1e-6)

    def drive():
        req, presp = dut.rreq, dut.presp
        yield dut.pair_en.eq(1)
        addresses = [0x1000 + index * 1024 for index in range(4)]
        seen = set()
        for bus in hp_buses:
            yield bus.ar.ready.eq(1)
        yield dut.bus.ar.ready.eq(1)
        yield presp.ready.eq(1)

        # Hold all R channels silent until every port has issued its burst.
        yield req.valid.eq(1)
        yield req.bits.beats.eq(2)
        for addr in addresses:
            yield req.bits.addr.eq(addr)
            yield
            assert (yield req.ready)
            for index, bus in enumerate(hp_buses):
                if (yield bus.ar.valid):
                    assert index not in seen
                    assert (yield bus.ar.bits.addr) == addresses[index]
                    assert (yield bus.ar.bits.len) == 1
                    seen.add(index)
        yield req.valid.eq(0)
        for _ in range(10):
            yield
            for index, bus in enumerate(hp_buses):
                if (yield bus.ar.valid):
                    assert index not in seen
                    assert (yield bus.ar.bits.addr) == addresses[index]
                    assert (yield bus.ar.bits.len) == 1
                    seen.add(index)
            assert not (yield dut.bus.ar.valid)
        assert seen == set(range(4))

        # Return later stripes first and require the HP stores to accept
        # them while the merger is still waiting for the first stripe.
        for index in (3, 2, 1):
            bus = hp_buses[index]
            yield bus.r.valid.eq(1)
            for beat in range(2):
                yield bus.r.bits.data.eq(2 * index + beat)
                yield bus.r.bits.last.eq(beat == 1)
                yield
                assert (yield bus.r.ready)
                assert not (yield presp.valid)
            yield bus.r.valid.eq(0)

        # The head stripe's beats retire as an ordered pair; hold the
        # sink and require the pair to present and hold.
        yield presp.ready.eq(0)
        bus = hp_buses[0]
        yield bus.r.valid.eq(1)
        for beat in range(2):
            yield bus.r.bits.data.eq(beat)
            yield bus.r.bits.last.eq(beat == 1)
            yield
            assert (yield bus.r.ready)
        yield bus.r.valid.eq(0)
        yield
        for _ in range(3):
            yield
            assert (yield presp.valid)
            assert (yield presp.bits.data0) == 0
            assert (yield presp.bits.data1) == 1
            assert (yield presp.bits.dual)

        # Two beats per cycle across stripe boundaries: four fires
        # carry the eight ordered beats as four pairs.
        yield presp.ready.eq(1)
        received = []
        for _ in range(8):
            yield
            if (yield presp.fire):
                received.append(((yield presp.bits.data0),
                                 (yield presp.bits.data1), (yield
                                                            presp.bits.dual)))
        assert received == [(0, 1, 1), (2, 3, 1), (4, 5, 1), (6, 7, 1)]

    sim.add_sync_process(drive)
    sim.run()


@pytest.mark.parametrize("pl_beats, profile", [
    (None, 'zu15'),
    (0, 'zu15'),
    (13, 'zu15'),
    (31, 'zu15'),
    (13, 'generic'),
    (None, 'generic'),
    (None, 'fast'),
    (9, 'fast'),
    (31, 'fast'),
    (9, 'fast-lean'),
    (10, 'fast-five'),
    (11, 'fast-five'),
])
def test_sg1_routes_payload_and_scales(pl_beats, profile):
    n_outputs = 5 if profile == 'fast-five' else 4
    params = dict(PARAMS,
                  data_width=512,
                  n_outputs=n_outputs,
                  sg_min_burst_beats=16)
    if profile in ('fast', 'fast-lean', 'fast-five'):
        params.update(sg_pay_fifo=256, sg_credits=256)
    if profile == 'generic':
        topology = ReadTopology(
            ports=(ReadPort('read0', 1,
                            8), ReadPort('shared', 0,
                                         32), ReadPort('read1', 2, 8)),
            regions=(ReadRegion(0x100000, 0x200000, (1, )),
                     ReadRegion(0x300000, 0x400000, (2, 0), stripe_bytes=512),
                     ReadRegion(0x500000, 0x600000, (1, ))),
            primary_region=0,
            scale_region=2,
            shared_port=1,
            secondary_region=1,
            retirement_lanes=1,
        )
    else:
        topology = zu15_read_topology(params,
                                      fast_hp=profile
                                      in ('fast', 'fast-lean', 'fast-five'),
                                      lean=profile
                                      in ('fast-lean', 'fast-five'))
    primary, secondary = topology.payload_regions
    primary_port, = primary.ports
    gemv = StreamGemv(params, read_topology=topology)
    reader = StreamMultiPortReader(params, topology)
    m = Module()
    m.submodules.gemv = gemv
    m.submodules.reader = reader
    m.d.comb += [
        gemv.mem_rreq.connect(reader.rreq),
        reader.presp.connect(gemv.mem_rresp),
        reader.mresp.connect(gemv.mem_sresp),
        reader.pair_en.eq(gemv.supply.pair_ok),
        reader.triple_en.eq(gemv.supply.triple_ok)
    ]
    k, rows = 992, 16
    tile = make_tile(random.Random(4),
                     k,
                     rows,
                     Format.Q8_0,
                     n_outputs=n_outputs)
    payload, meta = pack_stream_q8_0(tile['wsegs'],
                                     k,
                                     rows,
                                     n_outputs=n_outputs)
    # A short first stripe exercises clipping below the minimum request size.
    stripe = secondary.stripe_bytes
    payload_base = secondary.start + stripe - 64
    meta_base = topology.scales.start + 0x1000
    pl_base = primary.start + 0x2000
    if pl_beats is not None:
        pl_payload, ps_payload = split_sg1_payload(payload, pl_beats)
        assert len(pl_payload) + len(ps_payload) == len(payload)
        if pl_beats == 0:
            assert len(pl_payload) - len(ps_payload) == 512
    else:
        pl_payload, ps_payload = b'', payload
    seen = [set() for _ in topology.ports]
    records = []
    triple_cycles = []
    retired = bytearray()

    def memory(bus, port):
        yield Passive()
        port_width = topology.ports[port].axi_data_width or params['data_width']
        beat_bytes = port_width // 8
        regions = ((meta_base, meta),
                   (pl_base,
                    pl_payload)) if port == primary_port else ((payload_base,
                                                                ps_payload), )
        pending = deque()
        latency_step = 19 if profile == 'fast' else 3
        yield bus.ar.ready.eq(1)
        for cycle in range(6000):
            if (yield bus.ar.valid) and (yield bus.ar.ready):
                addr = yield bus.ar.bits.addr
                beats = (yield bus.ar.bits.len) + 1
                assert 1 << (yield bus.ar.bits.size) == beat_bytes
                base, image = next((base, data) for base, data in regions
                                   if base <= addr < base + len(data))
                assert addr + beat_bytes * beats <= base + (len(image) +
                                                            63) // 64 * 64
                if port != primary_port:
                    assert secondary.ports[(addr // stripe) %
                                           len(secondary.ports)] == port
                    assert addr // stripe == (addr + beat_bytes * beats -
                                              1) // stripe
                for beat in range(beats):
                    beat_addr = addr + beat_bytes * beat
                    assert beat_addr not in seen[port]
                    seen[port].add(beat_addr)
                    pending.append(
                        (cycle + 2 + (4 - port) * latency_step,
                         image[beat_addr - base:beat_addr - base + beat_bytes],
                         beat == beats - 1))
            if (yield bus.r.valid) and (yield bus.r.ready):
                pending.popleft()
            if pending and pending[0][0] <= cycle:
                _, data, last = pending[0]
                yield bus.r.valid.eq(1)
                yield bus.r.bits.data.eq(int.from_bytes(data, 'little'))
                yield bus.r.bits.last.eq(last)
            else:
                yield bus.r.valid.eq(0)
            yield

    def drive():
        for index, entry in enumerate(tile['act']):
            yield from act_write(gemv, index, entry)
        yield gemv.segs.ready.eq(1)
        if pl_beats is not None:
            yield gemv.sink.bits.ps_base.eq(payload_base)
            yield gemv.sink.bits.pl_beats.eq(pl_beats)
        yield from push_desc(gemv, Format.Q8_0, k, rows,
                             payload_base if pl_beats is None else pl_base,
                             meta_base)
        for cycle in range(5000):
            if profile in ('fast', 'fast-five') and (yield reader.presp.fire):
                lanes = yield reader.presp.bits.beat_count
                if lanes == 3:
                    triple_cycles.append(cycle)
                for lane in range(lanes):
                    data = yield reader.presp.bits[f'data{lane}']
                    offset = len(retired)
                    expected = payload[offset:offset + 64]
                    assert data.to_bytes(64,
                                         'little') == expected, (offset, lane,
                                                                 lanes)
                    retired.extend(expected)
            if (yield gemv.segs.fire):
                record = {}
                for field in record_fields(n_outputs):
                    record[field] = yield getattr(gemv.segs.bits, field)
                records.append(record)
            if (yield gemv.done):
                assert (yield gemv.status) == Status.OK
                break
            yield
        else:
            raise AssertionError('SG1 did not finish')
        if profile == 'fast' and pl_beats == 9:
            assert triple_cycles
        yield

        if pl_beats is not None:
            yield gemv.sink.bits.ps_base.eq(secondary.end - 64)
            yield gemv.sink.bits.p_base.eq(pl_base)
            yield gemv.sink.valid.eq(1)
            for _ in range(20):
                yield
                if (yield gemv.sink.fire):
                    assert (yield gemv.done)
                    assert (yield gemv.status) == Status.BAD_RANGE
                    break
            else:
                raise AssertionError(
                    'SG1 did not reject the striped PS extent')
            yield gemv.sink.valid.eq(0)
            yield
            return

        for bad_payload, bad_meta in ((0, meta_base), (secondary.end - 64,
                                                       meta_base),
                                      (payload_base, secondary.start)):
            yield gemv.sink.bits.p_base.eq(bad_payload)
            yield gemv.sink.bits.m_base.eq(bad_meta)
            yield gemv.sink.valid.eq(1)
            for _ in range(20):
                yield
                if (yield gemv.sink.fire):
                    assert (yield gemv.done)
                    assert (yield gemv.status) == Status.BAD_RANGE
                    break
            else:
                raise AssertionError('SG1 did not reject the invalid extent')
            yield gemv.sink.valid.eq(0)
            yield

    sim = Simulator(m)
    sim.add_clock(1e-6)
    for port, axi_reader in enumerate(reader.readers):
        sim.add_sync_process(lambda bus=axi_reader.bus, port=port:
                             (yield from memory(bus, port)))
    sim.add_sync_process(drive)
    sim.run()
    check_records(records, tile['exp'], n_outputs=n_outputs)
    if profile == 'fast-five':
        assert triple_cycles
    hp_beat_bytes = topology.ports[secondary.ports[0]].axi_data_width or 512
    hp_beat_bytes //= 8
    assert set().union(*(seen[port] for port in secondary.ports)) == set(
        range(payload_base, payload_base + len(ps_payload), hp_beat_bytes))
    assert seen[primary_port] == (
        set(range(pl_base, pl_base + len(pl_payload), 64))
        | set(range(meta_base, meta_base + len(meta), 64)))


def test_striped_dual_beat_timed_delivery_bit_exact():
    """Records stay bit-exact under board-like AR/R service timing.

    PL answers 38 cycles after the AR then one beat per cycle with a
    one-cycle gap; each HP port answers 35 cycles after the AR then
    one beat per four cycles. Exercises the pair retire path, the
    stripe-boundary crossings and the pair-wide pay_q at a 128-beat
    booking window.
    """
    k, rows = 1024, 16
    params = dict(PARAMS,
                  data_width=512,
                  n_outputs=4,
                  sg_min_burst_beats=16,
                  sg_pay_fifo=128,
                  sg_credits=128)
    topology = zu15_read_topology(params)
    gemv = StreamGemv(params, read_topology=topology)
    reader = StreamMultiPortReader(params, topology)
    m = Module()
    m.submodules.gemv = gemv
    m.submodules.reader = reader
    m.d.comb += [
        gemv.mem_rreq.connect(reader.rreq),
        reader.presp.connect(gemv.mem_rresp),
        reader.mresp.connect(gemv.mem_sresp),
        reader.pair_en.eq(gemv.supply.pair_ok)
    ]
    tile = make_tile(random.Random(3), k, rows, Format.Q8_0, n_outputs=4)
    payload, meta = pack_stream_q8_0(tile['wsegs'], k, rows, n_outputs=4)
    pl_payload, ps_payload = split_sg1_payload(payload, 16)
    ps_image = ps_payload + bytes(-len(ps_payload) % 64)
    pl_base, ps_base, meta_base = 0x80010000, 0x01000000, 0x81000000
    stats = {'cycles': 0}
    records = []

    def timed_hp(bus):
        yield Passive()
        pending = deque()
        for cycle in range(20000):
            if stats['cycles']:
                break
            yield bus.ar.ready.eq(1)
            if (yield bus.ar.valid) and (yield bus.ar.ready):
                addr = yield bus.ar.bits.addr
                beats = (yield bus.ar.bits.len) + 1
                for beat in range(beats):
                    off = addr + 64 * beat - ps_base
                    pending.append((cycle + 35 + 4 * beat,
                                    ps_image[off:off + 64], beat == beats - 1))
            if (yield bus.r.valid) and (yield bus.r.ready):
                pending.popleft()
            if pending and pending[0][0] <= cycle:
                _, data, last = pending[0]
                yield bus.r.valid.eq(1)
                yield bus.r.bits.data.eq(int.from_bytes(data, 'little'))
                yield bus.r.bits.last.eq(last)
            else:
                yield bus.r.valid.eq(0)
            yield

    def timed_pl(bus):
        yield Passive()
        regions = ((pl_base, pl_payload), (meta_base, meta))
        pending = deque()
        last_free = 0
        for cycle in range(20000):
            if stats['cycles']:
                break
            yield bus.ar.ready.eq(1)
            if (yield bus.ar.valid) and (yield bus.ar.ready):
                addr = yield bus.ar.bits.addr
                beats = (yield bus.ar.bits.len) + 1
                base, image = next(
                    (b, d) for b, d in regions if b <= addr < b + len(d))
                start = max(cycle + 38, last_free + 1)
                for beat in range(beats):
                    off = addr + 64 * beat - base
                    pending.append(
                        (start + beat, image[off:off + 64], beat == beats - 1))
                last_free = start + beats
            if (yield bus.r.valid) and (yield bus.r.ready):
                pending.popleft()
            if pending and pending[0][0] <= cycle:
                _, data, last = pending[0]
                yield bus.r.valid.eq(1)
                yield bus.r.bits.data.eq(int.from_bytes(data, 'little'))
                yield bus.r.bits.last.eq(last)
            else:
                yield bus.r.valid.eq(0)
            yield

    def drive():
        for index, entry in enumerate(tile['act']):
            yield from act_write(gemv, index, entry)
        yield gemv.segs.ready.eq(1)
        yield gemv.sink.bits.ps_base.eq(ps_base)
        yield gemv.sink.bits.pl_beats.eq(16)
        yield from push_desc(gemv, Format.Q8_0, k, rows, pl_base, meta_base)
        for cycle in range(20000):
            if (yield gemv.segs.fire):
                record = {}
                for field in record_fields(4):
                    record[field] = yield getattr(gemv.segs.bits, field)
                records.append(record)
            if (yield gemv.done):
                stats['cycles'] = cycle
                break
            yield
        else:
            raise AssertionError('SG1 did not finish')

    sim = Simulator(m)
    sim.add_clock(1e-6)
    sim.add_sync_process(drive)
    for bus in reader.aux_buses.values():
        sim.add_sync_process(lambda bus=bus: (yield from timed_hp(bus)))
    sim.add_sync_process(lambda: (yield from timed_pl(reader.bus)))
    sim.run()
    check_records(records, tile['exp'], n_outputs=4)


@pytest.mark.parametrize("fast_hp,n_outputs",
                         [(False, 4), *((True, lanes)
                                        for lanes in range(1, 9))])
@pytest.mark.parametrize("fmt", [Format.Q4_K, Format.Q6_K, Format.Q5_0])
def test_striped_kquant_and_q5(fmt, fast_hp, n_outputs):
    """K-quant pairs assemble at 128/192-byte pitch; Q5_0 falls back.

    Q6_K/Q4_K accept two-beat retirement (their pitch completes at
    most one unit per beat pair), while Q5_0's 20-byte pitch must stay
    at one beat per cycle; both must produce record-exact output
    through the striped dual path.
    """
    if fmt in (Format.Q4_K, Format.Q6_K):
        from troom.stream import pack_stream_kquant

        def pack(w, k, r, n_outputs):
            return pack_stream_kquant(fmt, w, k, r, n_outputs)
    else:
        from troom.stream import pack_stream_q5_0 as pack
    params = dict(PARAMS,
                  data_width=512,
                  n_outputs=n_outputs,
                  sg_min_burst_beats=16,
                  sg_pay_fifo=128,
                  sg_credits=128)
    topology = zu15_read_topology(params,
                                  fast_hp=fast_hp,
                                  lean=fast_hp and n_outputs != 4)
    gemv = StreamGemv(params, read_topology=topology)
    reader = StreamMultiPortReader(params, topology)
    m = Module()
    m.submodules.gemv = gemv
    m.submodules.reader = reader
    m.d.comb += [
        gemv.mem_rreq.connect(reader.rreq),
        reader.presp.connect(gemv.mem_rresp),
        reader.mresp.connect(gemv.mem_sresp),
        reader.pair_en.eq(gemv.supply.pair_ok),
        reader.triple_en.eq(gemv.supply.triple_ok)
    ]
    # Cross the scale ring wrap and finish with a partial lane group.
    k, rows = 512, 11
    tile = make_tile(random.Random(21), k, rows, fmt, n_outputs=n_outputs)
    payload, meta = pack(tile['wsegs'], k, rows, n_outputs=n_outputs)
    pl_payload, ps_payload = split_sg1_payload(payload, 16)
    ps_image = ps_payload + bytes(-len(ps_payload) % 64)
    pl_base, ps_base, meta_base = 0x80010000, 0x01000000, 0x81000000
    stats = {'cycles': 0}
    records = []

    def memory(bus, port):
        yield Passive()
        regions = ((meta_base, meta), (pl_base, pl_payload)) if port == 4 \
            else ((ps_base, ps_image),)
        beat_bytes = 32 if fast_hp and port < 4 else 64
        pending = deque()
        yield bus.ar.ready.eq(1)
        for cycle in range(8000):
            if stats['cycles']:
                break
            if (yield bus.ar.valid) and (yield bus.ar.ready):
                addr = yield bus.ar.bits.addr
                beats = (yield bus.ar.bits.len) + 1
                base, image = next(
                    (b, d) for b, d in regions if b <= addr < b + len(d))
                for beat in range(beats):
                    off = addr + beat_bytes * beat - base
                    pending.append(
                        (cycle + 2 + 3 * port, image[off:off + beat_bytes],
                         beat == beats - 1))
            if (yield bus.r.valid) and (yield bus.r.ready):
                pending.popleft()
            if pending and pending[0][0] <= cycle:
                _, data, last = pending[0]
                yield bus.r.valid.eq(1)
                yield bus.r.bits.data.eq(int.from_bytes(data, 'little'))
                yield bus.r.bits.last.eq(last)
            else:
                yield bus.r.valid.eq(0)
            yield

    def drive():
        for index, entry in enumerate(tile['act']):
            yield from act_write(gemv, index, entry)
        yield gemv.segs.ready.eq(1)
        yield gemv.sink.bits.ps_base.eq(ps_base)
        yield gemv.sink.bits.pl_beats.eq(16)
        yield from push_desc(gemv, fmt, k, rows, pl_base, meta_base)
        for cycle in range(8000):
            if (yield reader.presp.fire):
                assert (yield reader.presp.bits.beat_count) <= (
                    1 if fmt == Format.Q5_0 else 2)
            if (yield gemv.segs.fire):
                record = {}
                for field in record_fields(n_outputs):
                    record[field] = yield getattr(gemv.segs.bits, field)
                records.append(record)
            if (yield gemv.done):
                stats['cycles'] = cycle
                break
            yield
        else:
            raise AssertionError('SG1 did not finish')

    sim = Simulator(m)
    sim.add_clock(1e-6)
    sim.add_sync_process(drive)
    for port, bus in enumerate([*reader.aux_buses.values(), reader.bus]):
        sim.add_sync_process(lambda bus=bus, port=port:
                             (yield from memory(bus, port)))
    sim.run()
    check_records(records, tile['exp'], n_outputs=n_outputs)


def test_dual_retire_matches_request_order_under_skew():
    """Random stripes, ports, lengths and skews; pairs stay ordered.

    The merger must reproduce the exact request-order payload byte
    sequence on presp (one or two beats per fire, crossing stripe
    boundaries and odd-length tails) while the single scale burst
    returns on mresp, with AR/R timing and sink readiness randomized.
    """
    rng = random.Random(11)
    params = dict(PARAMS,
                  data_width=512,
                  sg_min_burst_beats=1,
                  sg_pay_fifo=8,
                  sg_credits=8)
    dut = StreamMultiPortReader(params, zu15_read_topology(params))
    sim = Simulator(dut)
    sim.add_clock(1e-6)

    stripes = []
    si_of = {}
    pl_next = 0x80000000
    ps_next = [0x1000 + (port << 10) for port in range(4)]

    def add(addr, beats, meta):
        si_of[addr] = len(stripes)
        stripes.append((addr, beats, meta))

    # Fill HP0 while retirement is stalled, then wrap through the same
    # store on another full stripe before exercising short random tails.
    add(ps_next[0], 16, False)
    add(ps_next[0] + 0x1000, 16, False)
    ps_next[0] += 0x2000

    for _ in range(40):
        port = rng.randrange(5)
        beats = rng.randrange(1, 5)
        if port == 4:
            add(pl_next, beats, False)
            pl_next += 64 * beats
        else:
            addr = ps_next[port]
            # Keep each stripe inside its port's own 1 KiB window: a
            # cursor that landed on the window edge slides into a
            # neighbour port's window and aliases its addresses.
            if ((addr >> 10) & 3) != port or \
                    (addr & 0x3ff) + 64 * beats > 0x400:
                addr = (addr & ~0xfff) + 0x1000 + (port << 10)
            ps_next[port] = addr + 64 * beats
            add(addr, beats, False)
    add(pl_next, 2, True)  # one scale burst on the PL port
    payload_seq = [(si << 8) | beat
                   for si, (addr, beats, meta) in enumerate(stripes)
                   if not meta for beat in range(beats)]
    scale_seq = [(len(stripes) - 1) << 8 | beat for beat in range(2)]

    def memory(bus):
        yield Passive()
        pending = deque()
        for cycle in range(4000):
            yield bus.ar.ready.eq(rng.random() < 0.7)
            if (yield bus.ar.valid) and (yield bus.ar.ready):
                addr = yield bus.ar.bits.addr
                beats = (yield bus.ar.bits.len) + 1
                si = si_of[addr]
                for beat in range(beats):
                    pending.append((cycle + 1 + rng.randrange(5),
                                    (si << 8) | beat, beat == beats - 1))
            if (yield bus.r.valid) and (yield bus.r.ready):
                pending.popleft()
            if pending and pending[0][0] <= cycle:
                _, word, last = pending[0]
                yield bus.r.valid.eq(1)
                yield bus.r.bits.data.eq(word)
                yield bus.r.bits.last.eq(last)
            else:
                yield bus.r.valid.eq(0)
            yield

    retired, scales = [], []

    def collect():
        yield Passive()
        while True:
            if (yield dut.presp.fire):
                retired.append((yield dut.presp.bits.data0))
                if (yield dut.presp.bits.dual):
                    retired.append((yield dut.presp.bits.data1))
            if (yield dut.mresp.fire):
                scales.append((yield dut.mresp.bits.data))
            yield

    def throttle():
        yield Passive()
        for _ in range(50):
            yield
        assert (yield dut.stores[0].count) == 16
        while True:
            yield dut.presp.ready.eq(rng.random() < 0.8)
            yield

    def drive():
        req = dut.rreq
        yield dut.pair_en.eq(1)
        yield dut.mresp.ready.eq(1)
        for addr, beats, meta in stripes:
            yield req.valid.eq(1)
            yield req.bits.addr.eq(addr)
            yield req.bits.beats.eq(beats)
            yield req.bits.meta.eq(meta)
            yield
            while not (yield req.ready):
                yield
            yield req.valid.eq(0)
            yield
            if rng.random() < 0.3:
                yield
        for _ in range(4000):
            if len(retired) == len(payload_seq) and \
                    len(scales) == len(scale_seq):
                break
            yield
        else:
            raise AssertionError('retirement did not drain')
        assert retired == payload_seq
        assert scales == scale_seq

    sim.add_sync_process(collect)
    sim.add_sync_process(throttle)
    sim.add_sync_process(drive)
    for bus in [*dut.aux_buses.values(), dut.bus]:
        sim.add_sync_process(lambda bus=bus: (yield from memory(bus)))
    sim.run()
