"""Unit tests for the synthesizable TileLink SRAM slave (``tilelink.SRAM``).

Exercises ``roomsoc.interconnect.tilelink.SRAM`` directly -- not the test-only
``TLSRAM`` wrapper -- so the production RTL gets coverage independent of the
AXI bridges. Covers Get/Put (single + multi-beat), ``read_only``, the external
``error`` hook, and the ``mem_or_size`` / ``bus`` constructor forms. Driven via
the shared ``tl_helpers`` kit (``tl_get`` / ``tl_put`` / ``run_sim``).
"""

import pytest

from amaranth import *
from amaranth.utils import log2_int

from roomsoc.interconnect import tilelink
from tl_helpers import tl_get, tl_put, run_sim


def _init(depth, base=0x100):
    return [base + i for i in range(depth)]


class SramTop(Elaboratable):
    """tilelink.SRAM with an address-matched external ``error`` input.

    Mirrors how a SoC would wire a protection check into the slave: the
    ``error`` signal is driven combinationally from the request address and
    fed into ``tilelink.SRAM(error=...)``.
    """

    def __init__(self,
                 *,
                 data_width=32,
                 depth=256,
                 init=None,
                 read_only=False,
                 denied_addr=None):
        self.data_width = data_width
        self.depth = depth
        self.read_only = read_only
        self.denied_addr = denied_addr
        self.bus = tilelink.Interface(addr_width=32,
                                      data_width=data_width,
                                      size_width=4,
                                      source_id_width=4)
        self._init = [0] * depth if init is None else list(init)
        self.error = Signal() if denied_addr is not None else None

    def elaborate(self, platform):
        m = Module()
        mem = Memory(width=self.data_width,
                     depth=self.depth,
                     init=self._init)
        m.submodules.sram = tilelink.SRAM(mem,
                                          read_only=self.read_only,
                                          bus=self.bus,
                                          error=self.error)
        if self.error is not None:
            m.d.comb += self.error.eq(
                self.bus.a.bits.address == self.denied_addr)
        return m


def test_sram_get_returns_init_image():
    top = SramTop(depth=256, init=_init(256))
    lg = log2_int(top.data_width // 8)

    def driver():
        data, denied, corrupt = yield from tl_get(top.bus, 0, size=lg, source=1)
        assert (data, denied, corrupt) == (0x100, 0, 0)
        data, _, _ = yield from tl_get(top.bus, 8, size=lg, source=2)
        assert data == 0x102

    run_sim(top, driver)


def test_sram_putfull_persists_and_reads_back():
    top = SramTop(depth=256)
    lg = log2_int(top.data_width // 8)

    def driver():
        denied = yield from tl_put(top.bus, 0x10, 0xcafef00d, 0xf, size=lg, source=0)
        assert denied == 0
        data, denied, corrupt = yield from tl_get(top.bus, 0x10, size=lg, source=0)
        assert (data, denied, corrupt) == (0xcafef00d, 0, 0)

    run_sim(top, driver)


def test_sram_putpartial_mask_fidelity():
    top = SramTop(depth=256, init=_init(256))
    lg = log2_int(top.data_width // 8)

    def driver():
        # Pre-load 0x11223344, then overwrite the upper two bytes with 0xaabb.
        yield from tl_put(top.bus, 0, 0x11223344, 0xf, size=lg, source=0)
        yield from tl_put(top.bus, 0, 0xaabb0000, 0xc, size=lg, source=0, full=False)
        data, _, _ = yield from tl_get(top.bus, 0, size=lg, source=0)
        assert data == 0xaabb3344

    run_sim(top, driver)


def test_sram_multibeat_putpartial_uses_each_beat_mask():
    top = SramTop(depth=256, init=[0x44332211, 0x88776655] + [0] * 254)
    lg = log2_int(top.data_width // 8)

    def driver():
        # Beat 0 updates lanes 0/2; beat 1 independently updates lanes 1/3.
        value = 0xb4b3b2b1_a4a3a2a1
        denied = yield from tl_put(top.bus, 0, value, 0xa5,
                                   size=lg + 1, source=5, full=False)
        assert denied == 0
        data, denied, corrupt = yield from tl_get(top.bus, 0,
                                                  size=lg + 1, source=5)
        assert (data, denied, corrupt) == (0xb477b255_44a322a1, 0, 0)

    run_sim(top, driver)


def test_sram_multibeat_get_and_put_roundtrip():
    top = SramTop(depth=256, init=_init(256))
    lg = log2_int(top.data_width // 8)
    value = 0x2222222211111111  # two beats of 32 bits

    def driver():
        denied = yield from tl_put(top.bus, 0x20, value, 0xff, size=lg + 1, source=3)
        assert denied == 0
        data, denied, corrupt = yield from tl_get(top.bus, 0x20, size=lg + 1, source=3)
        assert (data, denied, corrupt) == (value, 0, 0)
        # Untouched neighbour words survive.
        data, _, _ = yield from tl_get(top.bus, 0, size=lg, source=3)
        assert data == 0x100

    run_sim(top, driver)


def test_sram_multibeat_read_holds_response_under_backpressure():
    top = SramTop(depth=256,
                  init=[0x11111111, 0x22222222] + [0] * 254)
    bus = top.bus
    lg = log2_int(top.data_width // 8)

    def driver():
        yield bus.a.bits.opcode.eq(tilelink.ChannelAOpcode.Get)
        yield bus.a.bits.param.eq(0)
        yield bus.a.bits.size.eq(lg + 1)
        yield bus.a.bits.source.eq(6)
        yield bus.a.bits.address.eq(0)
        yield bus.a.bits.mask.eq(0xf)
        yield bus.a.bits.corrupt.eq(0)
        yield bus.a.valid.eq(1)
        yield bus.d.ready.eq(0)
        yield  # make A valid active
        yield bus.a.valid.eq(0)
        yield  # accept A and arm the first D beat

        first = ((yield bus.d.bits.opcode), (yield bus.d.bits.size),
                 (yield bus.d.bits.source), (yield bus.d.bits.data))
        assert (yield bus.d.valid) == 1
        assert first == (tilelink.ChannelDOpcode.AccessAckData.value,
                         lg + 1, 6, 0x11111111)

        # While D is blocked, no new A transaction may be accepted and every
        # response field must remain stable.
        yield bus.a.valid.eq(1)
        yield bus.a.bits.address.eq(0x40)
        for _ in range(3):
            assert (yield bus.a.ready) == 0
            assert (yield bus.d.valid) == 1
            assert ((yield bus.d.bits.opcode), (yield bus.d.bits.size),
                    (yield bus.d.bits.source),
                    (yield bus.d.bits.data)) == first
            yield

        # Accept only beat 0, then stall beat 1 and verify it does not advance.
        yield bus.a.valid.eq(0)
        yield bus.d.ready.eq(1)
        yield  # make ready active; beat 0 is still presented
        assert (yield bus.d.bits.data) == 0x11111111
        yield bus.d.ready.eq(0)
        yield  # accept beat 0, then make ready inactive
        second = ((yield bus.d.bits.opcode), (yield bus.d.bits.size),
                  (yield bus.d.bits.source), (yield bus.d.bits.data))
        assert second == (tilelink.ChannelDOpcode.AccessAckData.value,
                          lg + 1, 6, 0x22222222)
        for _ in range(2):
            assert (yield bus.d.valid) == 1
            assert ((yield bus.d.bits.opcode), (yield bus.d.bits.size),
                    (yield bus.d.bits.source),
                    (yield bus.d.bits.data)) == second
            yield

        yield bus.d.ready.eq(1)
        yield  # make ready active
        yield bus.d.ready.eq(0)
        yield  # accept final D beat
        assert (yield bus.d.valid) == 0
        assert (yield bus.a.ready) == 1

        # The blocked A request was not consumed; a fresh transaction works.
        data, denied, corrupt = yield from tl_get(bus, 0,
                                                  size=lg, source=7)
        assert (data, denied, corrupt) == (0x11111111, 0, 0)

    run_sim(top, driver)


@pytest.mark.parametrize("data_width", [32, 64])
def test_sram_wide_data_roundtrip(data_width):
    top = SramTop(data_width=data_width, depth=256, init=_init(256))
    lg = log2_int(data_width // 8)
    beat_mask = (1 << data_width) - 1
    value = 0xfedcba9876543210 & beat_mask

    def driver():
        yield from tl_put(top.bus, 0x40, value, (1 << (data_width // 8)) - 1,
                          size=lg, source=7)
        data, _, _ = yield from tl_get(top.bus, 0x40, size=lg, source=7)
        assert data == value

    run_sim(top, driver)


def test_sram_read_only_denies_writes_and_does_not_commit():
    top = SramTop(depth=256, init=_init(256), read_only=True)
    lg = log2_int(top.data_width // 8)

    def driver():
        denied = yield from tl_put(top.bus, 0, 0xdeadbeef, 0xf, size=lg, source=0)
        assert denied == 1
        # The write must not have committed; the init image is intact.
        data, denied, corrupt = yield from tl_get(top.bus, 0, size=lg, source=0)
        assert (data, denied, corrupt) == (0x100, 0, 0)

    run_sim(top, driver)


def test_sram_error_hook_denies_read_and_write():
    # An address-matched ``error`` denies reads (corrupt=1) and writes (no
    # commit). A clean access elsewhere still succeeds, and the denied-address
    # word retains its init value -- the SRAM returns the real memory content
    # alongside the denied/corrupt flags, proving the error write did not land.
    top = SramTop(depth=256, init=_init(256), denied_addr=0x10)
    lg = log2_int(top.data_width // 8)

    def driver():
        _data, denied, corrupt = yield from tl_get(top.bus, 0x10, size=lg, source=1)
        assert (denied, corrupt) == (1, 1)
        denied = yield from tl_put(top.bus, 0x10, 0xdeadbeef, 0xf, size=lg, source=2)
        assert denied == 1
        # Clean access at a different address still succeeds.
        data, denied, corrupt = yield from tl_get(top.bus, 0, size=lg, source=3)
        assert (data, denied, corrupt) == (0x100, 0, 0)
        # The denied-address word is unchanged (init 0x104); the denied/corrupt
        # flags mark it untrusted but the underlying memory proves no commit.
        data, denied, corrupt = yield from tl_get(top.bus, 0x10, size=lg, source=4)
        assert (data, denied, corrupt) == (0x104, 1, 1)

    run_sim(top, driver)


def test_sram_error_is_sampled_only_on_first_write_beat():

    class ErrorTop(Elaboratable):

        def __init__(self):
            self.bus = tilelink.Interface(addr_width=32,
                                          data_width=32,
                                          size_width=4,
                                          source_id_width=4)
            self.error = Signal()

        def elaborate(self, platform):
            m = Module()
            mem = Memory(width=32, depth=16,
                         init=[0x11111111, 0x22222222] + [0] * 14)
            m.submodules.sram = tilelink.SRAM(mem, bus=self.bus,
                                              error=self.error)
            return m

    top = ErrorTop()
    bus = top.bus
    lg = log2_int(bus.data_width // 8)

    def two_beat_put(value, first_error, second_error, source):
        yield bus.a.bits.opcode.eq(tilelink.ChannelAOpcode.PutFullData)
        yield bus.a.bits.param.eq(0)
        yield bus.a.bits.size.eq(lg + 1)
        yield bus.a.bits.source.eq(source)
        yield bus.a.bits.address.eq(0)
        yield bus.a.bits.mask.eq(0xf)
        yield bus.a.bits.corrupt.eq(0)
        yield bus.a.valid.eq(1)

        yield top.error.eq(first_error)
        yield bus.a.bits.data.eq(value & 0xffffffff)
        yield
        assert (yield bus.a.ready) == 1

        yield top.error.eq(second_error)
        yield bus.a.bits.data.eq(value >> 32)
        yield
        yield bus.a.valid.eq(0)
        yield top.error.eq(0)

        yield bus.d.ready.eq(1)
        while not (yield bus.d.valid):
            yield
        denied = (yield bus.d.bits.denied)
        yield
        yield bus.d.ready.eq(0)
        return denied

    def driver():
        # A first-beat error denies the whole burst even after error clears.
        denied = yield from two_beat_put(0xdddddddd_cccccccc, 1, 0, 1)
        assert denied == 1
        data, _, _ = yield from tl_get(bus, 0, size=lg + 1, source=2)
        assert data == 0x22222222_11111111

        # Conversely, a later error cannot retroactively deny an accepted
        # transaction whose first beat passed the permission check.
        denied = yield from two_beat_put(0xbbbbbbbb_aaaaaaaa, 0, 1, 3)
        assert denied == 0
        data, denied, corrupt = yield from tl_get(bus, 0,
                                                  size=lg + 1, source=4)
        assert (data, denied, corrupt) == (0xbbbbbbbb_aaaaaaaa, 0, 0)

    run_sim(top, driver)


def test_sram_constructor_accepts_memory_and_byte_size():
    # mem_or_size may be a pre-built Memory (carrying init) or a byte count;
    # bus may be omitted (auto-created). Both forms must elaborate and serve a
    # single-beat Get against their image.

    class Holder(Elaboratable):
        def __init__(self, sram):
            self.sram = sram

        def elaborate(self, platform):
            m = Module()
            m.submodules.sram = self.sram
            return m

    # Memory form: explicit bus + init-bearing Memory.
    mem = Memory(width=32, depth=4, init=[0xaa, 0xbb, 0xcc, 0xdd])
    mem_bus = tilelink.Interface(addr_width=32,
                                 data_width=32,
                                 size_width=4,
                                 source_id_width=4)
    mem_top = Holder(tilelink.SRAM(mem, bus=mem_bus))
    lg = log2_int(32 // 8)

    def mem_driver():
        data, _, _ = yield from tl_get(mem_bus, 0, size=lg, source=0)
        assert data == 0xaa
        data, _, _ = yield from tl_get(mem_bus, 4, size=lg, source=0)
        assert data == 0xbb

    run_sim(mem_top, mem_driver)

    # Int byte-size form: no bus, so the default 32/32 interface is created.
    # 16 bytes // (32//8) = 4 zero-initialised words; size_width=1 admits size=1.
    int_sram = tilelink.SRAM(16)
    int_top = Holder(int_sram)

    def int_driver():
        data, _, _ = yield from tl_get(int_sram.bus, 0, size=1, source=0)
        assert data == 0

    run_sim(int_top, int_driver)
