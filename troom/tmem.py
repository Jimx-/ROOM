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

The P2a split-TMEM slice (2026-11-03) doubles that budget by address
partition: the space halves into two clusters at the top word bit, each
with its own write port and read request/response pair, so traffic in
different halves never contends (the maintenance staging and the
attention chains are the intended residents).

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

    With ``tmem_clusters == 2`` (the P2a split-TMEM slice) the address
    space halves into two independent clusters at the top word bit:
    cluster A keeps ``write``/``rreq``/``rresp`` for ``[0, half)``,
    cluster B adds ``write_b``/``rreq_b``/``rresp_b`` for
    ``[half, tmem_bytes)``. Each cluster is the full historical
    structure at half bank depth — one write port and one read port
    per bank, beat-interleaved, with its own one-outstanding read slot
    and conflict counter. The address partition is a routing contract
    (the core presents each beat to the cluster its address names): a
    beat presented to a cluster's port group with a foreign address
    is a scheduling error with unspecified results, the same standing
    class as a read concurrent with a write to the same word.
    ``tmem_clusters == 1`` reproduces the historical
    single-cluster behavior exactly. The public counters sum across
    clusters, so A-only traffic observes the historical counts.
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
        if self.tmem_clusters == 2:
            self.write_b = Decoupled(
                Record, [('addr', self.tmem_addr_width, Direction.FANOUT),
                         ('data', self.data_width, Direction.FANOUT)])
            self.rreq_b = Decoupled(
                Record, [('addr', self.tmem_addr_width, Direction.FANOUT)])
            self.rresp_b = Decoupled(
                Record, [('data', self.data_width, Direction.FANOUT)])

        # Observation counters; see the plan's scheduling section. The
        # public words sum the clusters; per-cluster signals stay on
        # the attributes for pysim.
        self.conflicts = Signal(48)
        self.write_beats = Signal(48)
        self.read_beats = Signal(48)
        self.conflicts_a = Signal(48)
        self.write_beats_a = Signal(48)
        self.read_beats_a = Signal(48)
        if self.tmem_clusters == 2:
            self.conflicts_b = Signal(48)
            self.write_beats_b = Signal(48)
            self.read_beats_b = Signal(48)

    def _build_cluster(self, m, tag, write, rreq, rresp, half):
        """One cluster: the historical bank group at full or half depth.

        ``half`` selects the split-TMEM row decode: the cluster owns the
        address range whose top word bit equals ``tag``'s low bit, and
        the row index drops that bit. Returns the cluster's
        (conflicts, write_beats, read_beats) counters.
        """
        lg = self.lg_beat
        lg_banks = log2_int(self.n_tmem_banks)
        tag = tag if self.tmem_clusters == 2 else ''

        lg_words = log2_int(self.tmem_bank_words * self.n_tmem_banks)
        lg_row_hi = lg_words - 1 if half else lg_words

        wr_word = write.bits.addr[lg:]
        wr_bank = wr_word[:lg_banks]
        req_word = rreq.bits.addr[lg:]
        req_bank = req_word[:lg_banks]

        depth = self.tmem_bank_words >> (1 if half else 0)
        wports = []
        rports = []
        for i in range(self.n_tmem_banks):
            bank = Memory(width=self.data_width,
                          depth=depth,
                          name=f'bank{tag}{i}')
            wport = m.submodules[f'bank{tag}{i}_wport'] = bank.write_port()
            # Non-transparent: an enable is available to hold response
            # data, and a read colliding with a write returns the old
            # word.
            rport = m.submodules[f'bank{tag}{i}_rport'] = bank.read_port(
                domain='sync', transparent=False)
            wports.append(wport)
            rports.append(rport)

        # Write path: decode the target bank combinationally and commit
        # the beat at the clock edge.
        for i in range(self.n_tmem_banks):
            m.d.comb += [
                wports[i].addr.eq(wr_word[lg_banks:lg_row_hi]),
                wports[i].data.eq(write.bits.data),
                wports[i].en.eq(write.fire & (wr_bank == i)),
            ]
        m.d.comb += write.ready.eq(1)

        # Read path: the bank's read port samples the request address
        # combinationally and is enabled only on the accepting edge, so
        # the response data is held stable however long the consumer
        # back-pressures rresp.
        for i in range(self.n_tmem_banks):
            m.d.comb += [
                rports[i].addr.eq(req_word[lg_banks:lg_row_hi]),
                rports[i].en.eq(rreq.fire & (req_bank == i)),
            ]

        pending = Signal()
        rsp_bank = Signal(lg_banks)
        with m.If(rreq.fire):
            m.d.sync += rsp_bank.eq(req_bank)

        with m.If(rresp.fire & ~rreq.fire):
            m.d.sync += pending.eq(0)
        with m.Elif(rreq.fire):
            m.d.sync += pending.eq(1)

        read_data = Signal(self.data_width)
        for i in range(self.n_tmem_banks):
            with m.If(rsp_bank == i):
                m.d.comb += read_data.eq(rports[i].data)

        conflict = rreq.valid & write.fire & (req_bank == wr_bank)
        slot_free = ~pending | rresp.fire

        m.d.comb += [
            rreq.ready.eq(slot_free & ~conflict),
            rresp.valid.eq(pending),
            rresp.bits.data.eq(read_data),
        ]

        conflicts = Signal(48)
        wbeats = Signal(48)
        rbeats = Signal(48)
        with m.If(conflict & slot_free):
            m.d.sync += conflicts.eq(conflicts + 1)
        with m.If(write.fire):
            m.d.sync += wbeats.eq(wbeats + 1)
        with m.If(rresp.fire):
            m.d.sync += rbeats.eq(rbeats + 1)
        return conflicts, wbeats, rbeats

    def elaborate(self, platform):
        m = Module()

        ca, cwa, cra = self._build_cluster(m,
                                           'a',
                                           self.write,
                                           self.rreq,
                                           self.rresp,
                                           half=self.tmem_clusters == 2)
        m.d.comb += [
            self.conflicts_a.eq(ca),
            self.write_beats_a.eq(cwa),
            self.read_beats_a.eq(cra),
        ]
        if self.tmem_clusters == 2:
            cb, cwb, crb = self._build_cluster(m,
                                               'b',
                                               self.write_b,
                                               self.rreq_b,
                                               self.rresp_b,
                                               half=True)
            m.d.comb += [
                self.conflicts_b.eq(cb),
                self.write_beats_b.eq(cwb),
                self.read_beats_b.eq(crb),
                self.conflicts.eq(ca + cb),
                self.write_beats.eq(cwa + cwb),
                self.read_beats.eq(cra + crb),
            ]
        else:
            m.d.comb += [
                self.conflicts.eq(ca),
                self.write_beats.eq(cwa),
                self.read_beats.eq(cra),
            ]

        return m
