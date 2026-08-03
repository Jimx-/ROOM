"""Phase 1: TileLink bundle & Interface plumbing tests.

Covers the combinational core of ``roomsoc.interconnect.tilelink`` that every
responder, cache, and bridge relies on:

  * ``Interface.has_data`` / ``Interface.is_request`` opcode classification for
    every A/B/C/D opcode (and the B/E fall-through).
  * ``Interface.num_beats0`` burst sizing across opcodes and sizes.
  * ``Interface.count`` first/last/done beat tracking for 1/2/4-beat bursts.
  * The ``tilelink_get`` / ``tilelink_put_full_data`` /
    ``tilelink_access_ack`` / ``tilelink_access_ack_data`` payload builders --
    every field asserted across a sweep of arguments.
  * ``Interface`` construction (``has_bce`` selects B/C/E, width params
    propagate) and ``connect()`` wiring (AD and BCE, both directions).

Combinational checks use ``room.test.run_test`` (no clock, ``yield Settle()``);
the ``count()`` beat tracking is clocked (``sync=True``).
"""

import pytest

from amaranth import *
from amaranth.sim import Settle
from amaranth_soc.memory import MemoryMap

from roomsoc.interconnect import tilelink
from room.test import run_test


# ===========================================================================
# Opcode classification tables (mirrors Interface.has_data / is_request).
# ===========================================================================
A_HAS_DATA = {
    tilelink.ChannelAOpcode.PutFullData: 1,
    tilelink.ChannelAOpcode.PutPartialData: 1,
    tilelink.ChannelAOpcode.ArithmeticData: 0,
    tilelink.ChannelAOpcode.LogicalData: 0,
    tilelink.ChannelAOpcode.Get: 0,
    tilelink.ChannelAOpcode.Intent: 0,
    tilelink.ChannelAOpcode.AcquireBlock: 0,
    tilelink.ChannelAOpcode.AcquirePerm: 0,
}
C_HAS_DATA = {
    tilelink.ChannelCOpcode.AccessAck: 0,
    tilelink.ChannelCOpcode.AccessAckData: 0,
    tilelink.ChannelCOpcode.HintAck: 0,
    tilelink.ChannelCOpcode.ProbeAck: 0,
    tilelink.ChannelCOpcode.ProbeAckData: 1,
    tilelink.ChannelCOpcode.Release: 0,
    tilelink.ChannelCOpcode.ReleaseData: 1,
}
D_HAS_DATA = {
    tilelink.ChannelDOpcode.AccessAck: 0,
    tilelink.ChannelDOpcode.AccessAckData: 1,
    tilelink.ChannelDOpcode.HintAck: 0,
    tilelink.ChannelDOpcode.Grant: 0,
    tilelink.ChannelDOpcode.GrantData: 1,
    tilelink.ChannelDOpcode.ReleaseAck: 0,
}
C_IS_REQUEST = {
    tilelink.ChannelCOpcode.AccessAck: 0,
    tilelink.ChannelCOpcode.AccessAckData: 0,
    tilelink.ChannelCOpcode.HintAck: 0,
    tilelink.ChannelCOpcode.ProbeAck: 0,
    tilelink.ChannelCOpcode.ProbeAckData: 0,
    tilelink.ChannelCOpcode.Release: 1,
    tilelink.ChannelCOpcode.ReleaseData: 1,
}
D_IS_REQUEST = {
    tilelink.ChannelDOpcode.AccessAck: 0,
    tilelink.ChannelDOpcode.AccessAckData: 0,
    tilelink.ChannelDOpcode.HintAck: 0,
    tilelink.ChannelDOpcode.Grant: 1,
    tilelink.ChannelDOpcode.GrantData: 1,
    tilelink.ChannelDOpcode.ReleaseAck: 0,
}


class _ClassifyHarness(Elaboratable):
    """Drive a channel ``bits`` record and expose one classifier bit."""

    def __init__(self, fn, channel_cls, **channel_kw):
        self._fn = fn
        self.bits = channel_cls(**channel_kw)
        self.out = Signal()

    def elaborate(self, platform):
        m = Module()
        m.d.comb += self.out.eq(self._fn(self.bits))
        return m


@pytest.mark.parametrize("opcode,expected", list(A_HAS_DATA.items()),
                         ids=[op.name for op in A_HAS_DATA])
def test_has_data_channel_a(opcode, expected):
    h = _ClassifyHarness(tilelink.Interface.has_data, tilelink.ChannelA,
                         addr_width=32, data_width=64, size_width=4)

    def proc():
        yield h.bits.opcode.eq(opcode)
        yield Settle()
        assert (yield h.out) == expected

    run_test(h, proc)


@pytest.mark.parametrize("opcode", list(tilelink.ChannelBOpcode),
                         ids=[op.name for op in tilelink.ChannelBOpcode])
def test_has_data_channel_b_always_false(opcode):
    # has_data has no ChannelB branch -> falls through to ``return False``.
    h = _ClassifyHarness(tilelink.Interface.has_data, tilelink.ChannelB,
                         addr_width=32, data_width=64, size_width=4)

    def proc():
        yield h.bits.opcode.eq(opcode)
        yield Settle()
        assert (yield h.out) == 0

    run_test(h, proc)


@pytest.mark.parametrize("opcode,expected", list(C_HAS_DATA.items()),
                         ids=[op.name for op in C_HAS_DATA])
def test_has_data_channel_c(opcode, expected):
    h = _ClassifyHarness(tilelink.Interface.has_data, tilelink.ChannelC,
                         addr_width=32, data_width=64, size_width=4)

    def proc():
        yield h.bits.opcode.eq(opcode)
        yield Settle()
        assert (yield h.out) == expected

    run_test(h, proc)


@pytest.mark.parametrize("opcode,expected", list(D_HAS_DATA.items()),
                         ids=[op.name for op in D_HAS_DATA])
def test_has_data_channel_d(opcode, expected):
    h = _ClassifyHarness(tilelink.Interface.has_data, tilelink.ChannelD,
                         data_width=64, size_width=4)

    def proc():
        yield h.bits.opcode.eq(opcode)
        yield Settle()
        assert (yield h.out) == expected

    run_test(h, proc)


@pytest.mark.parametrize("channel_cls,opcode", [
    *((tilelink.ChannelA, opcode) for opcode in tilelink.ChannelAOpcode),
    *((tilelink.ChannelB, opcode) for opcode in tilelink.ChannelBOpcode),
], ids=[
    *(f"A-{opcode.name}" for opcode in tilelink.ChannelAOpcode),
    *(f"B-{opcode.name}" for opcode in tilelink.ChannelBOpcode),
])
def test_is_request_channels_a_and_b_always_true(channel_cls, opcode):
    # Both request channels are unconditional, including opcodes which do not
    # carry data.  Sweep each enum so a future opcode addition is covered too.
    h = _ClassifyHarness(tilelink.Interface.is_request, channel_cls,
                         addr_width=32, data_width=64, size_width=4)

    def proc():
        yield h.bits.opcode.eq(opcode)
        yield Settle()
        assert (yield h.out) == 1

    run_test(h, proc)


@pytest.mark.parametrize("opcode,expected", list(C_IS_REQUEST.items()),
                         ids=[op.name for op in C_IS_REQUEST])
def test_is_request_channel_c(opcode, expected):
    h = _ClassifyHarness(tilelink.Interface.is_request, tilelink.ChannelC,
                         addr_width=32, data_width=64, size_width=4)

    def proc():
        yield h.bits.opcode.eq(opcode)
        yield Settle()
        assert (yield h.out) == expected

    run_test(h, proc)


@pytest.mark.parametrize("opcode,expected", list(D_IS_REQUEST.items()),
                         ids=[op.name for op in D_IS_REQUEST])
def test_is_request_channel_d(opcode, expected):
    h = _ClassifyHarness(tilelink.Interface.is_request, tilelink.ChannelD,
                         data_width=64, size_width=4)

    def proc():
        yield h.bits.opcode.eq(opcode)
        yield Settle()
        assert (yield h.out) == expected

    run_test(h, proc)


def test_is_request_channel_e_always_false():
    # is_request(ChannelE) has no branch -> ``return False``.
    h = _ClassifyHarness(tilelink.Interface.is_request, tilelink.ChannelE,
                         sink_id_width=2)

    def proc():
        yield h.bits.sink.eq(1)
        yield Settle()
        assert (yield h.out) == 0

    run_test(h, proc)


# ===========================================================================
# num_beats0 burst sizing.
# ===========================================================================
def _expected_num_beats0(has_data, size, beat_bytes):
    if not has_data:
        return 0
    raw = 1 << size
    if raw < beat_bytes:
        return 0
    return (raw // beat_bytes) - 1


class _NumBeatsHarness(Elaboratable):

    def __init__(self, channel_cls, **channel_kw):
        self.bits = channel_cls(**channel_kw)
        self.out = Signal(16)

    def elaborate(self, platform):
        m = Module()
        m.d.comb += self.out.eq(tilelink.Interface.num_beats0(self.bits))
        return m


# data_width=64 -> beat_bytes=8. Get carries no data (always 1 beat);
# PutFull/PutPartial scale with size.
_A_NUM_BEATS_CASES = [
    (tilelink.ChannelAOpcode.Get, size, 0) for size in range(0, 7)
] + [
    (op, size, _expected_num_beats0(True, size, 8))
    for op in (tilelink.ChannelAOpcode.PutFullData,
               tilelink.ChannelAOpcode.PutPartialData)
    for size in range(0, 7)
]


@pytest.mark.parametrize(
    "opcode,size,expected", _A_NUM_BEATS_CASES,
    ids=[f"{o.name}-sz{s}" for o, s, _ in _A_NUM_BEATS_CASES])
def test_num_beats0_channel_a(opcode, size, expected):
    h = _NumBeatsHarness(tilelink.ChannelA,
                         addr_width=32, data_width=64, size_width=4)

    def proc():
        yield h.bits.opcode.eq(opcode)
        yield h.bits.size.eq(size)
        yield Settle()
        assert (yield h.out) == expected

    run_test(h, proc)


_C_NUM_BEATS = [
    (tilelink.ChannelCOpcode.ReleaseData, 1),
    (tilelink.ChannelCOpcode.ProbeAckData, 1),
    (tilelink.ChannelCOpcode.Release, 0),
    (tilelink.ChannelCOpcode.ProbeAck, 0),
]


@pytest.mark.parametrize("opcode,has_data", _C_NUM_BEATS,
                         ids=[op.name for op, _ in _C_NUM_BEATS])
def test_num_beats0_channel_c(opcode, has_data):
    h = _NumBeatsHarness(tilelink.ChannelC,
                         addr_width=32, data_width=64, size_width=4)

    def proc():
        for size in range(0, 7):
            yield h.bits.opcode.eq(opcode)
            yield h.bits.size.eq(size)
            yield Settle()
            assert (yield h.out) == _expected_num_beats0(has_data, size, 8)

    run_test(h, proc)


_D_NUM_BEATS = [
    (tilelink.ChannelDOpcode.AccessAckData, 1),
    (tilelink.ChannelDOpcode.GrantData, 1),
    (tilelink.ChannelDOpcode.AccessAck, 0),
    (tilelink.ChannelDOpcode.Grant, 0),
]


@pytest.mark.parametrize("opcode,has_data", _D_NUM_BEATS,
                         ids=[op.name for op, _ in _D_NUM_BEATS])
def test_num_beats0_channel_d(opcode, has_data):
    h = _NumBeatsHarness(tilelink.ChannelD, data_width=64, size_width=4)

    def proc():
        for size in range(0, 7):
            yield h.bits.opcode.eq(opcode)
            yield h.bits.size.eq(size)
            yield Settle()
            assert (yield h.out) == _expected_num_beats0(has_data, size, 8)

    run_test(h, proc)


def test_num_beats0_channel_e_is_zero():
    # num_beats0(ChannelE) returns 0 unconditionally.
    h = _NumBeatsHarness(tilelink.ChannelE, sink_id_width=2)

    def proc():
        yield h.bits.sink.eq(3)
        yield Settle()
        assert (yield h.out) == 0

    run_test(h, proc)


@pytest.mark.parametrize("opcode", list(tilelink.ChannelBOpcode),
                         ids=[op.name for op in tilelink.ChannelBOpcode])
def test_num_beats0_channel_b_is_zero(opcode):
    # B has data/size fields, but Interface.has_data deliberately classifies
    # it as data-less.  Exercise that less-obvious path at a large size.
    h = _NumBeatsHarness(tilelink.ChannelB,
                         addr_width=32, data_width=64, size_width=4)

    def proc():
        yield h.bits.opcode.eq(opcode)
        yield h.bits.size.eq(6)
        yield Settle()
        assert (yield h.out) == 0

    run_test(h, proc)


# ===========================================================================
# count() beat tracking (clocked).
# ===========================================================================
class _CountHarness(Elaboratable):

    def __init__(self, channel_cls, **channel_kw):
        self.bits = channel_cls(**channel_kw)
        self.fire = Signal()
        self.first = Signal()
        self.last = Signal()
        self.done = Signal()
        self.cnt = Signal(16)

    def elaborate(self, platform):
        m = Module()
        f, l, d, c = tilelink.Interface.count(m, self.bits, self.fire)
        m.d.comb += [
            self.first.eq(f),
            self.last.eq(l),
            self.done.eq(d),
            self.cnt.eq(c),
        ]
        return m


def _expected_count_trace(nbeats):
    """Expected (first, last, done) per beat of an nbeats-long burst."""
    trace = []
    for i in range(nbeats):
        first = 1 if i == 0 else 0
        last = 1 if i == nbeats - 1 else 0
        trace.append((first, last, last))  # done == last when fire=1
    return trace


# (opcode, size, nbeats) for data_width=64 (beat_bytes=8): sizes that produce
# 1/2/4-beat bursts, plus a Get (no data -> always 1 beat).
_COUNT_CASES = [
    (tilelink.ChannelAOpcode.PutFullData, 3, 1),  # raw=8  -> 1 beat
    (tilelink.ChannelAOpcode.PutFullData, 4, 2),  # raw=16 -> 2 beats
    (tilelink.ChannelAOpcode.PutFullData, 5, 4),  # raw=32 -> 4 beats
    (tilelink.ChannelAOpcode.Get, 4, 1),          # no data -> 1 beat
]


@pytest.mark.parametrize(
    "opcode,size,nbeats", _COUNT_CASES,
    ids=[f"{o.name}-sz{s}-n{n}" for o, s, n in _COUNT_CASES])
def test_count_tracks_first_last_done(opcode, size, nbeats):
    h = _CountHarness(tilelink.ChannelA,
                      addr_width=32, data_width=64, size_width=4)
    expected = _expected_count_trace(nbeats)
    seen = []

    def proc():
        yield h.bits.opcode.eq(opcode)
        yield h.bits.size.eq(size)
        yield h.fire.eq(1)
        yield  # let fire take effect (it was 0 during the setup cycle)
        for _ in range(nbeats):
            seen.append(((yield h.first), (yield h.last), (yield h.done)))
            yield  # advance one edge -> counter steps while fire=1
        # The last beat's edge reset the counter to 0, so the next beat is first.
        # (Do not issue another yield with fire still high -- that would reload
        # the counter and restart the burst.)
        assert (yield h.first) == 1

    run_test(h, proc, sync=True)
    assert seen == expected


def test_count_freezes_while_not_firing_and_reports_beat_index():
    h = _CountHarness(tilelink.ChannelA,
                      addr_width=32, data_width=64, size_width=4)

    def proc():
        yield h.bits.opcode.eq(tilelink.ChannelAOpcode.PutPartialData)
        yield h.bits.size.eq(5)  # four beats
        yield h.fire.eq(1)
        yield  # make fire active
        assert ((yield h.first), (yield h.last), (yield h.done),
                (yield h.cnt)) == (1, 0, 0, 0)
        yield h.fire.eq(0)
        yield  # fire beat 0, then make fire inactive

        assert ((yield h.first), (yield h.last), (yield h.done),
                (yield h.cnt)) == (0, 0, 0, 1)

        # Backpressure must not change the counter or assert done.
        for _ in range(3):
            yield
            assert ((yield h.first), (yield h.last), (yield h.done),
                    (yield h.cnt)) == (0, 0, 0, 1)

        # Resume and observe the remaining beat indices, including done only
        # on the final firing beat.
        yield h.fire.eq(1)
        yield  # make fire active; counter is still stalled
        assert ((yield h.done), (yield h.cnt)) == (0, 1)
        yield
        assert ((yield h.last), (yield h.done), (yield h.cnt)) == (0, 0, 2)
        yield
        assert ((yield h.last), (yield h.done), (yield h.cnt)) == (1, 1, 3)
        yield h.fire.eq(0)
        yield
        assert (yield h.first) == 1

    run_test(h, proc, sync=True)


def test_count_does_not_wrap_on_64_beat_burst():
    h = _CountHarness(tilelink.ChannelA,
                      addr_width=32, data_width=64, size_width=4)

    def proc():
        yield h.bits.opcode.eq(tilelink.ChannelAOpcode.PutFullData)
        yield h.bits.size.eq(9)  # 512 bytes / 8 bytes per beat = 64 beats
        yield h.fire.eq(1)
        yield  # make fire active

        for beat in range(64):
            assert (yield h.cnt) == beat
            assert (yield h.first) == (beat == 0)
            assert (yield h.last) == (beat == 63)
            assert (yield h.done) == (beat == 63)
            yield
        assert (yield h.first) == 1

    run_test(h, proc, sync=True)


# ===========================================================================
# Payload builders.
# ===========================================================================
class _ABuilderHarness(Elaboratable):
    """Drives an A-channel builder from input signals so tests can sweep args."""

    def __init__(self, builder, *, data_width=32, addr_width=32, size_width=4,
                 source_id_width=4, with_data=False):
        self.bus = tilelink.Interface(addr_width=addr_width,
                                      data_width=data_width,
                                      size_width=size_width,
                                      source_id_width=source_id_width)
        self.builder = builder
        self.address = Signal(addr_width)
        self.size = Signal(size_width)
        self.source = Signal(source_id_width)
        self.mask = Signal(data_width // 8)
        self.data = Signal(data_width)
        self._with_data = with_data

    def elaborate(self, platform):
        m = Module()
        if self._with_data:
            stmts = self.builder(self.bus, self.address, self.data, self.size,
                                 self.mask, self.source)
        else:
            stmts = self.builder(self.bus, self.address, self.size, self.mask,
                                 self.source)
        m.d.comb += stmts
        return m


@pytest.mark.parametrize("address,size,source",
                         [(0, 2, 0), (0x1000, 3, 7), (0xdeadbeef, 5, 5)])
def test_tilelink_get_builder_fields(address, size, source):
    mask = (1 << (32 // 8)) - 1
    h = _ABuilderHarness(tilelink.Interface.tilelink_get)

    def proc():
        yield h.address.eq(address)
        yield h.size.eq(size)
        yield h.source.eq(source)
        yield h.mask.eq(mask)
        yield Settle()
        a = h.bus.a.bits
        assert (yield a.opcode) == tilelink.ChannelAOpcode.Get.value
        assert (yield a.param) == 0
        assert (yield a.size) == size
        assert (yield a.source) == source
        assert (yield a.address) == address
        assert (yield a.mask) == mask
        assert (yield a.corrupt) == 0

    run_test(h, proc)


@pytest.mark.parametrize("address,size,source",
                         [(0, 2, 0), (0x80, 4, 3), (0x20, 3, 4)])
def test_tilelink_put_full_data_builder_fields(address, size, source):
    mask = (1 << (32 // 8)) - 1
    data = 0xcafef00d
    h = _ABuilderHarness(tilelink.Interface.tilelink_put_full_data,
                         with_data=True)

    def proc():
        yield h.address.eq(address)
        yield h.data.eq(data)
        yield h.size.eq(size)
        yield h.source.eq(source)
        yield h.mask.eq(mask)
        yield Settle()
        a = h.bus.a.bits
        assert (yield a.opcode) == tilelink.ChannelAOpcode.PutFullData.value
        assert (yield a.param) == 0
        assert (yield a.size) == size
        assert (yield a.source) == source
        assert (yield a.address) == address
        assert (yield a.mask) == mask
        assert (yield a.data) == data
        assert (yield a.corrupt) == 0

    run_test(h, proc)


@pytest.mark.parametrize("size,source,sink,denied",
                         [(2, 0, 0, 0), (5, 7, 1, 1), (3, 4, 3, 0)])
def test_tilelink_access_ack_builder_fields(size, source, sink, denied):
    bus = tilelink.Interface(addr_width=32, data_width=32, size_width=4,
                             source_id_width=4, sink_id_width=4)

    class Top(Elaboratable):

        def elaborate(self, platform):
            m = Module()
            m.d.comb += bus.tilelink_access_ack(size, source, sink, denied)
            return m

    def proc():
        yield Settle()
        d = bus.d.bits
        assert (yield d.opcode) == tilelink.ChannelDOpcode.AccessAck.value
        assert (yield d.param) == 0
        assert (yield d.size) == size
        assert (yield d.source) == source
        assert (yield d.sink) == sink
        assert (yield d.denied) == denied
        assert (yield d.corrupt) == 0

    run_test(Top(), proc)


@pytest.mark.parametrize("size,source,sink,corrupt",
                         [(2, 0, 0, 0), (5, 7, 1, 1), (3, 4, 3, 0)])
def test_tilelink_access_ack_data_builder_fields(size, source, sink, corrupt):
    data = 0x12345678
    bus = tilelink.Interface(addr_width=32, data_width=32, size_width=4,
                             source_id_width=4, sink_id_width=4)

    class Top(Elaboratable):

        def elaborate(self, platform):
            m = Module()
            m.d.comb += bus.tilelink_access_ack_data(data, size, source, sink,
                                                     corrupt)
            return m

    def proc():
        yield Settle()
        d = bus.d.bits
        assert (yield d.opcode) == tilelink.ChannelDOpcode.AccessAckData.value
        assert (yield d.param) == 0
        assert (yield d.size) == size
        assert (yield d.source) == source
        assert (yield d.sink) == sink
        assert (yield d.denied) == 0
        assert (yield d.corrupt) == corrupt
        assert (yield d.data) == data

    run_test(Top(), proc)


# ===========================================================================
# Interface construction & connect().
# ===========================================================================
def test_interface_ad_only_without_bce():
    bus = tilelink.Interface(addr_width=32, data_width=64, size_width=4,
                             source_id_width=3)
    assert bus.has_bce is False
    assert bus.data_width == 64
    assert bus.addr_width == 32
    assert bus.size_width == 4
    assert bus.source_id_width == 3
    assert hasattr(bus, "a") and hasattr(bus, "d")
    for ch in ("b", "c", "e"):
        assert not hasattr(bus, ch)


def test_interface_bce_includes_all_channels():
    bus = tilelink.Interface(addr_width=64, data_width=64, size_width=4,
                             source_id_width=5, sink_id_width=3, has_bce=True)
    assert bus.has_bce is True
    assert bus.sink_id_width == 3
    for ch in ("a", "b", "c", "d", "e"):
        assert hasattr(bus, ch)


def test_interface_memory_map_validation_and_freezing():
    bus = tilelink.Interface(addr_width=12, data_width=32)

    with pytest.raises(NotImplementedError):
        _ = bus.memory_map
    with pytest.raises(TypeError):
        bus.memory_map = object()
    with pytest.raises(ValueError, match="data width"):
        bus.memory_map = MemoryMap(addr_width=12, data_width=32)
    with pytest.raises(ValueError, match="address width"):
        bus.memory_map = MemoryMap(addr_width=11, data_width=8)

    memory_map = MemoryMap(addr_width=12, data_width=8)
    bus.memory_map = memory_map
    assert bus.memory_map is memory_map


_CONNECT_MATRIX = [
    dict(addr_width=32, data_width=32, size_width=4, source_id_width=4),
    dict(addr_width=32, data_width=64, size_width=4, source_id_width=4),
    dict(addr_width=32, data_width=32, size_width=1, source_id_width=1),
    dict(addr_width=32, data_width=64, size_width=4, source_id_width=4,
         sink_id_width=3, has_bce=True),
    dict(addr_width=64, data_width=64, size_width=3, source_id_width=8,
         sink_id_width=4, has_bce=True),
]


@pytest.mark.parametrize("kw", _CONNECT_MATRIX,
                         ids=["32b-ad", "64b-ad", "min-ad", "64b-bce", "wide-bce"])
def test_interface_connect_elaborates(kw):
    # Both buses share the same widths; connect() must wire AD (and BCE if
    # present) without width-mismatch errors.

    class Top(Elaboratable):

        def __init__(self):
            self.m_bus = tilelink.Interface(**kw)
            self.s_bus = tilelink.Interface(**kw)

        def elaborate(self, platform):
            m = Module()
            m.d.comb += self.m_bus.connect(self.s_bus)
            return m

    def proc():
        yield Settle()

    run_test(Top(), proc)


class _ConnectTop(Elaboratable):

    def __init__(self, has_bce=False):
        kw = dict(addr_width=32, data_width=32, size_width=4,
                  source_id_width=4, has_bce=has_bce)
        if has_bce:
            kw["sink_id_width"] = 3
        self.has_bce = has_bce
        self.m_bus = tilelink.Interface(**kw)
        self.s_bus = tilelink.Interface(**kw)

    def elaborate(self, platform):
        m = Module()
        m.d.comb += self.m_bus.connect(self.s_bus)
        return m


def test_connect_ad_routes_a_forward_and_d_backward():
    # connect() wires A manager->subordinate and D subordinate->manager, with
    # ready flowing the opposite way on each.
    top = _ConnectTop(has_bce=False)

    def proc():
        m, s = top.m_bus, top.s_bus
        # A forward: m.a -> s.a, ready s.a -> m.a.
        yield m.a.valid.eq(1)
        yield m.a.bits.address.eq(0x40)
        yield m.a.bits.source.eq(6)
        yield s.a.ready.eq(1)
        yield Settle()
        assert (yield s.a.valid) == 1
        assert (yield s.a.bits.address) == 0x40
        assert (yield s.a.bits.source) == 6
        assert (yield m.a.ready) == 1
        # D backward: s.d -> m.d, ready m.d -> s.d.
        yield s.d.valid.eq(1)
        yield s.d.bits.source.eq(6)
        yield s.d.bits.data.eq(0xabcd)
        yield m.d.ready.eq(1)
        yield Settle()
        assert (yield m.d.valid) == 1
        assert (yield m.d.bits.source) == 6
        assert (yield m.d.bits.data) == 0xabcd
        assert (yield s.d.ready) == 1

    run_test(top, proc)


def test_connect_bce_routes_all_five_channels():
    # A/C/E flow manager->subordinate; B/D flow subordinate->manager; each
    # channel's ready flows opposite its valid.
    top = _ConnectTop(has_bce=True)

    def proc():
        m, s = top.m_bus, top.s_bus
        yield m.a.valid.eq(1)
        yield m.a.bits.source.eq(1)
        yield s.a.ready.eq(1)
        yield m.c.valid.eq(1)
        yield m.c.bits.source.eq(2)
        yield s.c.ready.eq(1)
        yield m.e.valid.eq(1)
        yield m.e.bits.sink.eq(0)
        yield s.e.ready.eq(1)
        yield s.b.valid.eq(1)
        yield s.b.bits.source.eq(3)
        yield m.b.ready.eq(1)
        yield s.d.valid.eq(1)
        yield s.d.bits.source.eq(4)
        yield m.d.ready.eq(1)
        yield Settle()

        assert (yield s.a.valid) == 1 and (yield s.a.bits.source) == 1
        assert (yield m.a.ready) == 1
        assert (yield s.c.valid) == 1 and (yield s.c.bits.source) == 2
        assert (yield m.c.ready) == 1
        assert (yield s.e.valid) == 1 and (yield s.e.bits.sink) == 0
        assert (yield m.e.ready) == 1
        assert (yield m.b.valid) == 1 and (yield m.b.bits.source) == 3
        assert (yield s.b.ready) == 1
        assert (yield m.d.valid) == 1 and (yield m.d.bits.source) == 4
        assert (yield s.d.ready) == 1

    run_test(top, proc)
