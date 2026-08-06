"""Reusable TileLink testbench kit.

Single source of truth for the TL slave responder and master drivers that
TileLink tests previously reinvented per file. Mirrors ``axi_helpers.py`` and
the ``wishbone.SRAM`` API.

  * ``TLSRAM``        -- test slave = ``tilelink.SRAM`` + ``denied_addr`` error
                         injection + ``a_monitor`` tap. Lifted from the inline
                         test slave in ``test_axi_tl.py``.
  * ``tl_get``        -- TL-UL Get master driver.
  * ``tl_put``        -- TL-UL PutFull/PutPartial master driver.
  * ``collect_d``     -- D-channel collector enforcing the valid-stable invariant.
  * ``TLRamModel``    -- byte-addressable golden model for randomized tests.
  * ``tl_acquire``/``tl_release``/``tl_grantack`` -- TL-C coherent master drivers.
  * ``drive_d``       -- D-channel emitter (responder side), complement of
                         ``collect_d``.
  * ``tl_c_responder`` -- coherent TL-C memory subordinate (the "outer" memory
                         behind a coherent manager such as an L2/L3 cache).
  * ``run_sim``       -- re-exported from ``axi_helpers``.

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

try:
    from .axi_helpers import run_sim
except ImportError:
    # Interconnect tests historically import this module directly after pytest
    # adds this directory to sys.path.
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


# ---------------------------------------------------------------------------
# TileLink-C coherent subordinate (responder)
#
# The coherent-memory far-end for a TL-C manager such as an L2/L3 cache's
# out_bus, or any coherent master. Backed by the golden TLRamModel store, and
# the enabling "TLCResponder" piece the TL_TEST_PLAN deferred to Phase 8.
# ---------------------------------------------------------------------------
def drive_d(bus, *, opcode, param, source, sink, size, beats, data_fn):
    """Drive ``beats`` D beats on ``bus`` as the D-side master (responder).

    The complement of :func:`collect_d`: each beat is held valid (payload
    stable) until accepted, honouring the Decoupled valid-stable invariant.
    ``data_fn(i)`` returns beat ``i``'s data payload, or ``None`` for a
    no-data Grant/ReleaseAck (the data field is driven 0).
    """
    yield bus.d.bits.opcode.eq(opcode)
    yield bus.d.bits.param.eq(param)
    yield bus.d.bits.size.eq(size)
    yield bus.d.bits.source.eq(source)
    yield bus.d.bits.sink.eq(sink)
    yield bus.d.bits.denied.eq(0)
    yield bus.d.bits.corrupt.eq(0)
    for i in range(beats):
        yield bus.d.bits.data.eq(data_fn(i) if data_fn else 0)
        yield bus.d.valid.eq(1)
        yield
        while not (yield bus.d.ready):
            yield
    yield bus.d.valid.eq(0)
    yield


def _serve_acquire(bus, model, beat_bytes):
    """Accept one AcquireBlock on A and answer with Grant/GrantData on D.

    Grant beat counts come from the request's ``size`` field (a coherent
    manager sets it to log2 of its line size), so any line geometry is served.
    """
    param = (yield bus.a.bits.param)
    source = (yield bus.a.bits.source)
    address = (yield bus.a.bits.address)
    size = (yield bus.a.bits.size)

    yield bus.a.ready.eq(1)
    yield  # A beat fires at this edge
    yield bus.a.ready.eq(0)

    if param == tilelink.GrowParam.BtoT.value:
        # Permission upgrade: the manager already holds the line's data.
        yield from drive_d(bus,
                           opcode=tilelink.ChannelDOpcode.Grant,
                           param=tilelink.CapParam.toT,
                           source=source,
                           sink=0,
                           size=size,
                           beats=1,
                           data_fn=None)
        return

    gparam = (tilelink.CapParam.toT
              if param == tilelink.GrowParam.NtoT.value
              else tilelink.CapParam.toB)
    beats = max(1, (1 << size) // beat_bytes)
    block = model.get(address, size)  # bytes, little-endian, length 1<<size

    def read_beat(i, block=block, beat_bytes=beat_bytes):
        return int.from_bytes(block[i * beat_bytes:(i + 1) * beat_bytes],
                              "little")

    yield from drive_d(bus,
                       opcode=tilelink.ChannelDOpcode.GrantData,
                       param=gparam,
                       source=source,
                       sink=0,
                       size=size,
                       beats=beats,
                       data_fn=read_beat)


def _serve_release(bus, model, beat_bytes):
    """Accept one Release/ReleaseData on C and answer with ReleaseAck on D.

    ReleaseData beats are committed to ``model`` (dirty writeback); a bare
    Release carries no data.
    """
    opcode = (yield bus.c.bits.opcode)
    source = (yield bus.c.bits.source)
    address = (yield bus.c.bits.address)
    size = (yield bus.c.bits.size)
    has_data = opcode == tilelink.ChannelCOpcode.ReleaseData.value
    beats = max(1, (1 << size) // beat_bytes) if has_data else 1

    yield bus.c.ready.eq(1)
    yield
    got = 0
    for _ in range(beats * 64 + 32):
        if (yield bus.c.fire):
            if has_data:
                beat = (yield bus.c.bits.data)
                model.put(address + got * beat_bytes, beat,
                          (1 << beat_bytes) - 1)
            got += 1
            if got == beats:
                break
        yield
    yield bus.c.ready.eq(0)

    yield from drive_d(bus,
                       opcode=tilelink.ChannelDOpcode.ReleaseAck,
                       param=0,
                       source=source,
                       sink=0,
                       size=size,
                       beats=1,
                       data_fn=None)


def _issue_probe(bus, request, responses, beat_bytes):
    """Issue one outer Probe and collect the cache's ProbeAck response."""
    address, size, param, source = request
    yield bus.b.bits.opcode.eq(tilelink.ChannelBOpcode.Probe)
    yield bus.b.bits.param.eq(param)
    yield bus.b.bits.size.eq(size)
    yield bus.b.bits.source.eq(source)
    yield bus.b.bits.address.eq(address)
    yield bus.b.bits.mask.eq((1 << beat_bytes) - 1)
    yield bus.b.valid.eq(1)
    yield
    while not (yield bus.b.ready):
        yield
    yield bus.b.valid.eq(0)

    yield bus.c.ready.eq(1)
    while not (yield bus.c.valid):
        yield
    opcode = (yield bus.c.bits.opcode)
    report = (yield bus.c.bits.param)
    response_source = (yield bus.c.bits.source)
    has_data = opcode == tilelink.ChannelCOpcode.ProbeAckData.value
    beats = max(1, (1 << size) // beat_bytes) if has_data else 1
    data = 0
    for beat in range(beats):
        while not (yield bus.c.valid):
            yield
        data |= (yield bus.c.bits.data) << (beat * bus.data_width)
        yield
    yield bus.c.ready.eq(0)
    responses.append((opcode, report, response_source, data))


def tl_c_responder(bus, *, model, done, probes=None, probe_responses=None):
    """Coherent TileLink-C memory subordinate (the "outer" memory).

    Models the coherent memory behind a TL-C manager such as an L2/L3 cache's
    ``out_bus``. It is the D-side master and the A/C/E/B slave:

      * ``AcquireBlock`` on A -> ``GrantData`` (NtoB->toB, NtoT->toT); a BtoT
        permission upgrade returns a no-data ``Grant`` (toT).
      * ``Release``/``ReleaseData`` on C -> ``ReleaseAck``; ``ReleaseData``
        beats are committed to ``model`` (dirty writeback).
      * ``GrantAck`` on E is always accepted.
      * ``Probe`` on B is never issued -- an outer memory does not probe down
        into the cache.

    ``model`` is a :class:`TLRamModel` (built with ``data_width=bus.data_width``)
    backing the address space, or any object exposing
    ``get(address, size) -> bytes`` and ``put(address, data, mask)``. Grant beat
    counts are derived from each request's ``size`` field, so any line size is
    handled. ``done`` is a one-element list ``[False]`` the driving coroutine
    flips to ``[True]`` once its stimulus is complete, so the responder stops
    and ``run_sim`` can return; pair it with a watchdog coroutine to turn a
    DUT deadlock into a hard test failure instead of a hang.

    The loop services one outer transaction at a time, which suffices for
    sequential single-manager traffic; a pipelined manager simply sees
    backpressure on the channels it cannot yet service. Multi-outstanding stress
    traffic would need a synthesizable responder instead. If ``probes`` and
    ``probe_responses`` lists are supplied, the driver may append
    ``(address, size, CapParam, source)`` requests; the responder issues each
    on B and appends ``(opcode, report, source, data)`` from the C response.
    """
    beat_bytes = bus.data_width // 8

    yield bus.b.valid.eq(0)
    yield bus.e.ready.eq(1)
    yield bus.a.ready.eq(0)
    yield bus.c.ready.eq(0)
    yield bus.d.valid.eq(0)
    yield

    while not done[0]:
        if probes:
            request = probes.pop(0)
            yield from _issue_probe(bus, request, probe_responses, beat_bytes)
            continue
        if (yield bus.a.valid) and not (yield bus.d.valid):
            yield from _serve_acquire(bus, model, beat_bytes)
            continue
        if (yield bus.c.valid) and not (yield bus.d.valid):
            yield from _serve_release(bus, model, beat_bytes)
            continue
        yield

    yield bus.d.valid.eq(0)
    yield bus.a.ready.eq(0)
    yield bus.c.ready.eq(0)
