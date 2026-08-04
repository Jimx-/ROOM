"""Reusable TileLink testbench kit.

Single source of truth for the TL slave responder and master drivers that
TileLink tests previously reinvented per file. Mirrors ``axi_helpers.py`` and
the ``wishbone.SRAM`` API.

  * ``TLSRAM``     -- test slave = ``tilelink.SRAM`` + ``denied_addr`` error
                      injection + ``a_monitor`` tap. Lifted from the inline
                      test slave in ``test_axi_tl.py``.
  * ``tl_get``     -- TL-UL Get master driver.
  * ``tl_put``     -- TL-UL PutFull/PutPartial master driver.
  * ``collect_d``  -- D-channel collector enforcing the valid-stable invariant.
  * ``TLRamModel`` -- byte-addressable golden model for randomized tests.
  * ``run_sim``    -- re-exported from ``axi_helpers``.

The synthesizable RAM slave itself lives in
``roomsoc.interconnect.tilelink.SRAM``; this module adds only simulation
conveniences (monitor taps, error injection, a Python golden model, driver
coroutines). All helpers assume amaranth ``pysim`` with the clock model
documented in AGENTS.md: only a naked ``yield`` advances the cycle, and signal
reads/writes between naked yields are coherent within one cycle. The receiver
drives ``ready``.
"""

from amaranth import *
from amaranth.hdl.rec import Direction

from roomsoc.interconnect import tilelink
from roomsoc.interconnect.stream import Valid

from axi_helpers import run_sim


class TLSRAM(Elaboratable):
    """Test TileLink-UL SRAM: ``tilelink.SRAM`` + ``denied_addr`` + monitor.

    A drop-in replacement for the test slave previously inlined in
    ``test_axi_tl.py``. Same constructor shape (``depth`` / ``init`` /
    ``denied_addr``) so existing call sites work unchanged; the synthesizable
    core is delegated to :class:`roomsoc.interconnect.tilelink.SRAM`, and this
    wrapper layers on the simulation conveniences a testbench needs:

      * ``denied_addr`` -- deny (and corrupt on reads) any transaction whose
        first-beat address matches, so SLVERR / error-propagation paths can be
        exercised.
      * ``a_monitor``   -- ``Valid`` tap latching ``(address, opcode, size,
        source)`` on the first A beat of each transaction.
    """

    def __init__(self,
                 *,
                 addr_width=32,
                 data_width=32,
                 size_width=4,
                 source_id_width=4,
                 depth=256,
                 init=None,
                 denied_addr=None):
        self.bus = tilelink.Interface(addr_width=addr_width,
                                      data_width=data_width,
                                      size_width=size_width,
                                      source_id_width=source_id_width)
        self.depth = depth
        self.init = [0] * depth if init is None else list(init)
        self.denied_addr = denied_addr
        self._mem = Memory(width=data_width, depth=depth, init=self.init)
        self.a_monitor = Valid(
            Record, [("address", addr_width, Direction.FANOUT),
                     ("opcode", tilelink.ChannelAOpcode, Direction.FANOUT),
                     ("size", size_width, Direction.FANOUT),
                     ("source", source_id_width, Direction.FANOUT)])

    def elaborate(self, platform):
        m = Module()
        bus = self.bus

        # Optional error injection: deny any transaction whose first-beat
        # address matches. tilelink.SRAM samples ``error`` at the first beat.
        if self.denied_addr is not None:
            error = Signal()
            m.d.comb += error.eq(bus.a.bits.address == self.denied_addr)
        else:
            error = None

        m.submodules.sram = tilelink.SRAM(self._mem, bus=bus, error=error)

        # Monitor tap: latch the first A beat of each transaction.
        a_first, _, _, _ = tilelink.Interface.count(m, bus.a.bits, bus.a.fire)
        m.d.comb += [
            self.a_monitor.valid.eq(bus.a.fire & a_first),
            self.a_monitor.bits.address.eq(bus.a.bits.address),
            self.a_monitor.bits.opcode.eq(bus.a.bits.opcode),
            self.a_monitor.bits.size.eq(bus.a.bits.size),
            self.a_monitor.bits.source.eq(bus.a.bits.source),
        ]

        return m


# ---------------------------------------------------------------------------
# TileLink-UL master drivers (amaranth pysim sync coroutines)
#
# Only a naked ``yield`` advances the clock; all reads/writes between two naked
# yields are coherent within one cycle. The receiver drives ``ready``.
# ---------------------------------------------------------------------------
def tl_get(bus, address, *, size, source=0):
    """Issue a TL Get (single A beat) and collect the D response.

    Returns ``(data, denied, corrupt)`` where ``data`` is the little-endian
    concatenation of all D beats (``(1 << size) // beat_bytes`` of them).
    """
    beat_bytes = bus.data_width // 8
    nbeats = max(1, (1 << size) // beat_bytes)

    yield bus.a.bits.opcode.eq(tilelink.ChannelAOpcode.Get)
    yield bus.a.bits.param.eq(0)
    yield bus.a.bits.size.eq(size)
    yield bus.a.bits.source.eq(source)
    yield bus.a.bits.address.eq(address)
    yield bus.a.bits.mask.eq((1 << beat_bytes) - 1)
    yield bus.a.bits.data.eq(0)
    yield bus.a.bits.corrupt.eq(0)
    yield bus.a.valid.eq(1)
    yield
    while not (yield bus.a.ready):
        yield
    yield bus.a.valid.eq(0)

    yield bus.d.ready.eq(1)
    data = 0
    denied = 0
    corrupt = 0
    for i in range(nbeats):
        while not (yield bus.d.valid):
            yield
        denied |= (yield bus.d.bits.denied)
        corrupt |= (yield bus.d.bits.corrupt)
        data |= (yield bus.d.bits.data) << (i * bus.data_width)
        yield
    yield bus.d.ready.eq(0)
    return data, denied, corrupt


def tl_put(bus, address, data, mask, *, size, source=0, full=True):
    """Issue a TL PutFull/PutPartial and collect the single AccessAck.

    ``nbeats = (1 << size) // beat_bytes`` A beats are driven. ``mask`` is the
    per-byte mask for the whole transaction (each beat uses its own lane
    slice). Returns ``denied``.
    """
    beat_bytes = bus.data_width // 8
    nbeats = max(1, (1 << size) // beat_bytes)
    opcode = (tilelink.ChannelAOpcode.PutFullData
              if full else tilelink.ChannelAOpcode.PutPartialData)

    yield bus.a.bits.opcode.eq(opcode)
    yield bus.a.bits.param.eq(0)
    yield bus.a.bits.size.eq(size)
    yield bus.a.bits.source.eq(source)
    yield bus.a.bits.address.eq(address)
    yield bus.a.bits.corrupt.eq(0)
    yield bus.a.valid.eq(1)

    for i in range(nbeats):
        beat_data = (data >> (i * bus.data_width)) & ((1 << bus.data_width) -
                                                      1)
        if full:
            beat_mask = (1 << beat_bytes) - 1
        else:
            beat_mask = (mask >> (i * beat_bytes)) & ((1 << beat_bytes) - 1)
        yield bus.a.bits.data.eq(beat_data)
        yield bus.a.bits.mask.eq(beat_mask)
        yield
        while not (yield bus.a.ready):
            yield

    yield bus.a.valid.eq(0)

    yield bus.d.ready.eq(1)
    while not (yield bus.d.valid):
        yield
    denied = (yield bus.d.bits.denied)
    yield
    yield bus.d.ready.eq(0)
    return denied


def collect_d(bus, beats, results, *, timeout=1000, ready_fn=None):
    """Collect exactly ``beats`` D beats; append each to ``results``.

    Each appended entry is ``(data, denied, corrupt, opcode)``. Enforces the
    Decoupled valid-stable invariant: once a beat is observed valid but not
    accepted, its payload must not change until ``ready`` rises again.
    ``ready_fn``, if given, is called with the cycle index each iteration and
    drives ``ready`` to exercise backpressure.
    """
    yield bus.d.ready.eq(1 if ready_fn is None else ready_fn(0))
    yield

    stalled = None
    got = 0
    for cycle in range(timeout):
        ready = (yield bus.d.ready)
        valid = (yield bus.d.valid)
        beat = ((yield bus.d.bits.data), (yield bus.d.bits.denied),
                (yield bus.d.bits.corrupt), (yield bus.d.bits.opcode))

        if stalled is not None:
            assert valid, "valid dropped while a beat was stalled"
            assert beat == stalled, "beat changed while ready was low"

        if valid and ready:
            results.append(beat)
            got += 1
            if got == beats:
                break

        stalled = beat if (valid and not ready) else None

        next_cycle = cycle + 1
        yield bus.d.ready.eq(1 if ready_fn is None else ready_fn(next_cycle))
        yield

    assert got == beats, f"timed out: collected {got}/{beats} D beats"


class TLRamModel:
    """Byte-addressable golden RAM model for randomized TL tests.

    Keep one in lockstep with the stimulus and compare against the DUT's
    ``TLSRAM`` (or its D-channel responses). ``get`` / ``put`` take the same
    ``address`` / ``size`` / ``mask`` arguments the TL opcodes carry, so the
    model mirrors what a correct slave should retire.
    """

    def __init__(self, *, data_width=32, depth=256, init=None):
        self.data_width = data_width
        self.beat_bytes = data_width // 8
        self.depth = depth
        self.mem = bytearray(depth * self.beat_bytes)
        if init is not None:
            for i, word in enumerate(init):
                self._write_word(i, word)

    def _write_word(self, index, value):
        off = index * self.beat_bytes
        self.mem[off:off + self.beat_bytes] = (value & (
            (1 << self.data_width) - 1)).to_bytes(self.beat_bytes, "little")

    def get(self, address, size):
        """Return the little-endian bytes a Get of ``2**size`` bytes reads."""
        nbytes = 1 << size
        return bytes(self.mem[address:address + nbytes])

    def put(self, address, data, mask):
        """Apply a PutPartial/PutFull byte mask; return the post-write bytes."""
        for i in range(self.beat_bytes):
            if mask & (1 << i):
                self.mem[address + i] = (data >> (i * 8)) & 0xFF
        return bytes(self.mem[address:address + self.beat_bytes])


# ---------------------------------------------------------------------------
# TileLink-C master drivers (coherent BCE operations)
#
# Used by CacheCork (Phase 4) and L2 cache (Phase 8) tests. Each driver issues
# a single coherent transaction on the BCE channels and collects the D response.
# ---------------------------------------------------------------------------
def tl_acquire(bus,
               address,
               *,
               size,
               source,
               grow_param=tilelink.GrowParam.NtoB,
               opcode=tilelink.ChannelAOpcode.AcquireBlock):
    """Issue a TL-C AcquireBlock or AcquirePerm and collect Grant/GrantData.

    Acquires carry no A-channel data, so a single A beat is driven. The D
    response beat count depends on whether the eventual responder returns data:
    AcquireBlock(NtoB/NtoT) -> GrantData (``size``-dependent beats);
    AcquirePerm or AcquireBlock(BtoT) -> Grant (one beat, no data).

    Returns ``(opcode, param, source, sink, data, denied, corrupt)`` where
    ``opcode`` is the :class:`ChannelDOpcode` value actually received.
    """
    beat_bytes = bus.data_width // 8
    expect_data = (opcode == tilelink.ChannelAOpcode.AcquireBlock
                   and grow_param != tilelink.GrowParam.BtoT)
    d_beats = max(1, (1 << size) // beat_bytes) if expect_data else 1

    yield bus.a.bits.opcode.eq(opcode)
    yield bus.a.bits.param.eq(grow_param)
    yield bus.a.bits.size.eq(size)
    yield bus.a.bits.source.eq(source)
    yield bus.a.bits.address.eq(address)
    yield bus.a.bits.mask.eq((1 << beat_bytes) - 1)
    yield bus.a.bits.data.eq(0)
    yield bus.a.bits.corrupt.eq(0)
    yield bus.a.valid.eq(1)
    yield
    while not (yield bus.a.ready):
        yield
    yield bus.a.valid.eq(0)

    yield bus.d.ready.eq(1)
    data = 0
    d_opcode = d_param = d_source = d_sink = d_denied = d_corrupt = 0
    for i in range(d_beats):
        while not (yield bus.d.valid):
            yield
        if i == 0:
            d_opcode = (yield bus.d.bits.opcode)
            d_param = (yield bus.d.bits.param)
            d_source = (yield bus.d.bits.source)
            d_sink = (yield bus.d.bits.sink)
            d_denied = (yield bus.d.bits.denied)
            d_corrupt = (yield bus.d.bits.corrupt)
        data |= (yield bus.d.bits.data) << (i * bus.data_width)
        yield
    yield bus.d.ready.eq(0)
    return d_opcode, d_param, d_source, d_sink, data, d_denied, d_corrupt


def tl_release(bus,
               address,
               *,
               size,
               source,
               param=tilelink.ShrinkReportParam.TtoB,
               data=None):
    """Issue a TL-C Release or ReleaseData and collect ReleaseAck on D.

    When ``data`` is not None, drives ReleaseData (``size``-dependent C beats);
    otherwise drives Release (single beat, no data). Returns
    ``(opcode, source, denied)`` from the ReleaseAck.
    """
    beat_bytes = bus.data_width // 8
    has_data = data is not None
    opcode = (tilelink.ChannelCOpcode.ReleaseData if has_data
              else tilelink.ChannelCOpcode.Release)
    c_beats = max(1, (1 << size) // beat_bytes) if has_data else 1

    yield bus.c.bits.opcode.eq(opcode)
    yield bus.c.bits.param.eq(param)
    yield bus.c.bits.size.eq(size)
    yield bus.c.bits.source.eq(source)
    yield bus.c.bits.address.eq(address)
    yield bus.c.bits.corrupt.eq(0)
    yield bus.c.valid.eq(1)

    for i in range(c_beats):
        if has_data:
            beat = (data >> (i * bus.data_width)) & ((1 << bus.data_width) - 1)
            yield bus.c.bits.data.eq(beat)
        yield
        while not (yield bus.c.ready):
            yield

    yield bus.c.valid.eq(0)

    yield bus.d.ready.eq(1)
    while not (yield bus.d.valid):
        yield
    d_opcode = (yield bus.d.bits.opcode)
    d_source = (yield bus.d.bits.source)
    d_denied = (yield bus.d.bits.denied)
    yield
    yield bus.d.ready.eq(0)
    return d_opcode, d_source, d_denied


def tl_grantack(bus, *, sink):
    """Issue a single GrantAck (E channel) beat and wait for acceptance."""
    yield bus.e.bits.sink.eq(sink)
    yield bus.e.valid.eq(1)
    yield
    while not (yield bus.e.ready):
        yield
    yield bus.e.valid.eq(0)
