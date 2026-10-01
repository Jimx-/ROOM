"""Core-level simulation harness for groom.

Backs a whole `groom.core.Core` with an instruction ROM on its fetch bus so
that programs can be run through fetch, decode, issue, register read,
execute and writeback without a memory system. Writebacks are observed
through the core's `sim_debug` writeback stream, so test programs must not
rely on loads or stores. Instruction encoders live in
`tests.groom.encoding`.
"""

from amaranth import *
from amaranth.sim import Passive, Simulator
from amaranth.utils import log2_int

from roomsoc.interconnect import tilelink as tl

from groom.core import Core


class TLFetchROM(Elaboratable):
    """TileLink-Uncached slave serving the icache's block Get refills from
    an instruction image of 32-bit words placed at their natural byte
    addresses."""

    def __init__(self, ibus, image):
        assert ibus.data_width % 32 == 0
        self.ibus = ibus
        self.image = list(image)
        self.beat_words = ibus.data_width // 32

    def elaborate(self, platform):
        m = Module()
        ibus = self.ibus
        beat_bytes = ibus.data_width // 8

        words = self.image + [0] * (-len(self.image) % self.beat_words)
        init = []
        for i in range(0, len(words), self.beat_words):
            beat = words[i:i + self.beat_words]
            value = 0
            for j, word in enumerate(beat):
                value |= (word & 0xffffffff) << (32 * j)
            init.append(value)

        mem = Memory(width=ibus.data_width, depth=len(init), init=init)
        m.submodules.rport = rport = mem.read_port(domain='comb')

        a_addr = Signal.like(ibus.a.bits.address)
        a_size = Signal.like(ibus.a.bits.size)
        a_source = Signal.like(ibus.a.bits.source)
        beat = Signal(16)
        beats = Signal(16)

        with m.FSM():
            with m.State('IDLE'):
                m.d.comb += ibus.a.ready.eq(1)
                with m.If(ibus.a.fire):
                    m.d.sync += [
                        a_addr.eq(ibus.a.bits.address),
                        a_size.eq(ibus.a.bits.size),
                        a_source.eq(ibus.a.bits.source),
                        beat.eq(0),
                        beats.eq(
                            Const(1, 16) <<
                            (ibus.a.bits.size -
                             log2_int(beat_bytes)).as_unsigned()),
                    ]
                    m.next = 'RESP'

            with m.State('RESP'):
                m.d.comb += [
                    ibus.d.valid.eq(1),
                    ibus.d.bits.opcode.eq(tl.ChannelDOpcode.AccessAckData),
                    ibus.d.bits.param.eq(0),
                    ibus.d.bits.size.eq(a_size),
                    ibus.d.bits.source.eq(a_source),
                    ibus.d.bits.sink.eq(0),
                    ibus.d.bits.denied.eq(0),
                    ibus.d.bits.corrupt.eq(0),
                    rport.addr.eq(a_addr[log2_int(beat_bytes):] + beat),
                    ibus.d.bits.data.eq(rport.data),
                ]
                with m.If(ibus.d.fire):
                    with m.If(beat == beats - 1):
                        m.next = 'IDLE'
                    with m.Else():
                        m.d.sync += beat.eq(beat + 1)

        return m


class GroomCoreSim(Elaboratable):
    """A groom core with its instruction bus backed by a ROM, for
    simulation. `sim_debug` is forced on so writebacks are observable via
    `core.core_debug.wb_debug`; there is no data memory, so programs must
    not issue loads or stores."""

    def __init__(self, params, image, reset_vector=0):
        self.params = params
        self.image = list(image)
        self.reset_vector = reset_vector
        self.core = Core(params, sim_debug=True)

    def elaborate(self, platform):
        m = Module()
        m.submodules.core = self.core
        m.submodules.rom = TLFetchROM(self.core.ibus, self.image)
        m.d.comb += self.core.reset_vector.eq(self.reset_vector)
        return m


def wb_monitor(core, events):
    """Passive sync process collecting every writeback visible on the
    core's writeback debug stream into `events` as dicts."""

    wb = core.core_debug.wb_debug
    n_threads = core.n_threads

    def proc():
        yield Passive()
        while True:
            if (yield wb.valid):
                data = []
                for t in range(n_threads):
                    data.append((yield wb.bits.data[t]))
                events.append({
                    'wid': (yield wb.bits.wid),
                    'uop_id': (yield wb.bits.uop_id),
                    'tmask': (yield wb.bits.tmask),
                    'ldst': (yield wb.bits.ldst),
                    'data': data,
                })
            yield

    return proc


def run_core_sim(dut, script, monitor=None):
    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(script)
    if monitor is not None:
        sim.add_sync_process(monitor)
    sim.run()
