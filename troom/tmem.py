"""Banked tensor memory for the troom accelerator.

TMEM is deliberately not a generic bus SRAM: it is a purpose-built
banked memory with one DMA-fill write path and one compute/read path, as
the troom plan requires. Overlap (filling one tile while reading
another) is provided by real port concurrency, not by hoping two
addresses in one contested bank stay out of each other's way.

Bank mapping: beat interleaving. Consecutive ``beat_bytes``-sized words
map to consecutive banks, so a sequential DMA fill touches banks in
round robin and sustains one beat per cycle while each individual bank
is written only once every ``n_tmem_banks`` cycles. Regions spaced by
``n_tmem_banks * beat_bytes`` share a bank; regions spaced by one beat
never do. Wider compute read parallelism (multiple banks per cycle for
the MXU) attaches to the same banks in a later milestone.

Per-cycle port budget of the initial integration:

* one write beat on the DMA fill path (``write``),
* one read request/response (``rreq``/``rresp``), used by the DMA store
  path now and by compute later through an arbiter.

Each bank therefore needs one write port and one read port. Reads
conservatively yield to a write beat mapped to the same bank in the same
cycle; the lost cycles are counted in ``conflicts``. A true dual-port
primitive could avoid some of these stalls, but the conservative rule
keeps behavior identical when a bank maps to a primitive with weaker
read-during-write guarantees, and makes concurrent load/store streaming
contention observable instead of latent. Reads are never lost, only
delayed.

A read response is available one cycle after its request is accepted and
is held stable until it fires. Reads concurrent with a write to the same
word are a scheduling error under the command contract (events order
tile ownership); their returned data is not specified.
"""

from amaranth import *
from amaranth.hdl.rec import Direction, Record
from amaranth.utils import log2_int

from troom.types import HasTRoomParams

from roomsoc.interconnect.stream import Decoupled


class TMEM(HasTRoomParams, Elaboratable):
    """Banked tensor memory with independent fill and read paths.

    ``write`` accepts one beat per cycle at a beat-aligned byte address
    and is always ready (single writer, no write contention). ``rreq``
    accepts one outstanding read; ``rresp`` returns the beat one cycle
    later. A read request presented in a cycle where ``write`` commits a
    beat into the same bank is back-pressured and counted in
    ``conflicts``.
    """

    def __init__(self, params):
        super().__init__(params)

        self.write = Decoupled(
            Record, [('addr', self.tmem_addr_width, Direction.FANOUT),
                     ('data', self.data_width, Direction.FANOUT)])

        self.rreq = Decoupled(
            Record, [('addr', self.tmem_addr_width, Direction.FANOUT)])
        self.rresp = Decoupled(Record,
                               [('data', self.data_width, Direction.FANOUT)])

        # Observation counters; see the plan's scheduling section.
        self.conflicts = Signal(48)
        self.write_beats = Signal(48)
        self.read_beats = Signal(48)

    def elaborate(self, platform):
        m = Module()

        lg = self.lg_beat
        lg_banks = log2_int(self.n_tmem_banks)

        wr_word = self.write.bits.addr[lg:]
        wr_bank = wr_word[:lg_banks]
        req_word = self.rreq.bits.addr[lg:]
        req_bank = req_word[:lg_banks]

        wports = []
        rports = []
        for i in range(self.n_tmem_banks):
            bank = Memory(width=self.data_width,
                          depth=self.tmem_bank_words,
                          name=f'bank{i}')
            wport = m.submodules[f'bank{i}_wport'] = bank.write_port()
            # Non-transparent: an enable is available to hold response
            # data, and a read colliding with a write returns the old
            # word.
            rport = m.submodules[f'bank{i}_rport'] = bank.read_port(
                domain='sync', transparent=False)
            wports.append(wport)
            rports.append(rport)

        # Write path: decode the target bank combinationally and commit
        # the beat at the clock edge.
        for i in range(self.n_tmem_banks):
            m.d.comb += [
                wports[i].addr.eq(wr_word[lg_banks:]),
                wports[i].data.eq(self.write.bits.data),
                wports[i].en.eq(self.write.fire & (wr_bank == i)),
            ]
        m.d.comb += self.write.ready.eq(1)

        # Read path: the bank's read port samples the request address
        # combinationally and is enabled only on the accepting edge, so
        # the response data is held stable however long the consumer
        # back-pressures rresp.
        for i in range(self.n_tmem_banks):
            m.d.comb += [
                rports[i].addr.eq(req_word[lg_banks:]),
                rports[i].en.eq(self.rreq.fire & (req_bank == i)),
            ]

        pending = Signal()
        rsp_bank = Signal(lg_banks)
        with m.If(self.rreq.fire):
            m.d.sync += rsp_bank.eq(req_bank)

        with m.If(self.rresp.fire & ~self.rreq.fire):
            m.d.sync += pending.eq(0)
        with m.Elif(self.rreq.fire):
            m.d.sync += pending.eq(1)

        read_data = Signal(self.data_width)
        for i in range(self.n_tmem_banks):
            with m.If(rsp_bank == i):
                m.d.comb += read_data.eq(rports[i].data)

        conflict = self.rreq.valid & self.write.fire & (req_bank == wr_bank)
        slot_free = ~pending | self.rresp.fire

        m.d.comb += [
            self.rreq.ready.eq(slot_free & ~conflict),
            self.rresp.valid.eq(pending),
            self.rresp.bits.data.eq(read_data),
        ]

        with m.If(conflict & slot_free):
            m.d.sync += self.conflicts.eq(self.conflicts + 1)
        with m.If(self.write.fire):
            m.d.sync += self.write_beats.eq(self.write_beats + 1)
        with m.If(self.rresp.fire):
            m.d.sync += self.read_beats.eq(self.read_beats + 1)

        return m
