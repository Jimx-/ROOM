"""Direct-stream GEMV prototype for the troom redesign (phase B).

First slice of the streaming-architecture-redesign plan's phase B: a
simulation prototype of the device packing, reader/metadata queues,
decoder, and integer array for one format, integrated with the existing
MAC array and FP epilogue records. Q8_0 is the control-validation
format; Q5_0/Q6_K follow once the skeleton is proven.

Device image v2 ("SG1", Q8_0 + Q5_0 + the K-quant super-blocks). The
GGUF block order is repacked once at model preparation into two linear
DRAM regions with the weights in *consumption* order -- group-major,
K within group, output lane within block -- so the reader is a purely
linear burst walker and the decoder needs no GGUF block reordering at
all. The group width is the consuming unit's ``n_outputs`` (a pair at
2, a quad at 4 -- the D2 wider-join image):

* payload region at ``p_base``: for each output-row group, for each
  32-element block j, for each lane, the block's payload bytes in GGUF
  byte order. Q8_0 contributes 32 signed INT8 weights in logical K
  order (already the consumed form); Q5_0 contributes the 20 payload
  bytes of its 22-byte GGUF block -- the ``qh`` high-bit plane as a
  little-endian u32 followed by the 16 nibble bytes -- so the decoder
  performs a real GGUF byte decode. K-quants chunk at super-block
  granularity: each (group, super-block, lane) contributes one payload
  chunk -- Q6_K's ql||qh planes (192 B) or Q4_K's nibble plane
  (128 B) -- whose 8 windows the decoder decodes in place. A short
  final group contributes only its real rows' blocks. The region
  carries no padding: acceptance requires the bus beat to divide the
  format's payload extent (``rows * k`` bytes for Q8_0, ``rows * k *
  5/8`` for Q5_0, ``rows * (k/256) * 192`` for Q6_K, ``rows *
  (k/256) * 128`` for Q4_K) -- unchanged by the group width, which
  only reorders the bytes.
* meta region at ``m_base``: one entry per window for Q8_0/Q5_0 (the
  FP16 ``d`` bit pattern, 2 bytes) or per chunk for the K-quants
  (Q6_K's sc8||d tail, 18 bytes; Q4_K's d||dmin||k4-scales tail,
  16 bytes -- the GGUF scale bytes verbatim), in the same order as
  the payload, padded with zeroes to the final beat only. K-quant
  entries generalize the per-window (d, dmin, sc, mi) tuple the plan
  describes, at one tuple-set per super-block.

The K-major group interleave is the plan's "interleaving independent
output rows" option: at the 512-bit deployment width a Q8_0 beat (64
bytes) is exactly one n=2 launch slot's two lane segments, so a
sustained one-beat-per-cycle reader feeds one slot per cycle with O(1)
row buffering instead of staging whole rows. A Q5_0 slot pair is 40
bytes, so Q5_0 beats are *not* window-aligned (64 mod 20 = 4) and one
beat carries 3.2 windows -- more than the n=2 join's two windows per
cycle; the pair-interleaved unit therefore saturates at two Q5_0
windows per cycle (0.625 beats/cycle at 512 bits), a lane-count limit
the phase B/C/D2 measurements pin on the join. The D2 wider join
(n_outputs=4, the quad image) lifts emission to four windows per
cycle: every canonical format's beat demand then fits (Q5_0 3.2, Q6_K
2.67, Q4_K 4, Q8_0 2 windows per 64-B beat), the memory becomes the
binding stage at exactly one beat per cycle, and 128 MAC lanes cover
the census's 83.9 mix-average / 109.7 Q4_K useful-MAC demand.

Reader. One request/response port serves both regions with tagged
bursts: a request carries a beat-aligned address, a beat count (up to
``sg_burst_beats``, trimmed to what remains and to FIFO space), and the
region tag; a response returns one beat per cycle with the tag. Each
region keeps an outstanding-beat credit (``sg_credits``) and is only
issued burst space it has FIFO room for (``count + outstanding + burst
<= depth``), so a response can never overrun its queue. Meta has
request priority: it is 1/16 of the traffic and the join consumes it
at the payload rate, so starving it while the payload FIFO still has
room would stall the decoder pointlessly.

Decoder. Payload beats feed a unit assembler: a register file of
whole units (32/20-byte windows for Q8_0/Q5_0, 192/128-byte
super-block chunks for the K-quants) plus one partial-unit register,
so a unit becomes visible -- and emittable -- the cycle its last byte
lands, without any full-width variable shifts (the phase B
RTL-mapping rule for phase C). Emission is in order, at most
n_outputs units per cycle (one per lane queue); Q5_0 windows decode
(nibble | qh bit << 4, centred to 8 bits) before the push, and the
bonded lane group of one chunk index decodes together from the
resident chunks. A window whose successor shares its lane (short-tail
cells' consecutive lane-0 windows) simply emits in a later cycle. The
meta stream mirrors the unit discipline with per-format entry pitch
(one FP16 ``d`` per window for Q8_0/Q5_0; the GGUF scale tail per
chunk for the K-quants) in a small byte-granular buffer; emission
never exceeds n_outputs units per cycle, so it serves all formats
unchanged.

Unit. ``StreamGemv`` combines ``StreamSupply`` with the shared ActRAM,
``MacArray`` and ``MmSegment`` records in ``matrix.py``. Launch joining
and credit accounting preserve the numerical contract frozen in
``tests/troom/model.py``. The descriptor names the two DRAM regions, tile geometry is derived from
``k``/``rows``, a completion additionally waits for the reader to
quiesce and flushes the queues (the meta tail pad must not leak into
the next tile), and the reader's own counters (payload/meta beats,
padding bytes, bursts issued, port idle under work) are exposed for
the phase B rate accounting.
"""

from math import gcd

from dataclasses import dataclass

from amaranth import *
from amaranth.hdl.rec import Direction, Record
from amaranth.utils import log2_int

from troom.abi import SG1_DEFAULT_PL_BEATS as SG1_DEFAULT_PRIMARY_BEATS
from troom.abi import (MM_STARVATION_CLASSES, MM_STARVATION_COUNTERS,
                       MM_STARVATION_MASKS, SG1_STRIPE_BEATS)
from troom.consts import Format, Status
from troom.matrix import ActRAM, ActWord, MacArray, MmSegment, WSegWord
from troom.types import rtl_div_const, HasTRoomParams

from roomsoc.interconnect.axi import AXIInterface
from roomsoc.interconnect.axi.common import AXIBurst
from roomsoc.interconnect.stream import Decoupled, Queue

# Window size in bytes: one reduction segment's Q8_0 payload (32
# signed bytes, already the consumed form) or its Q5_0 payload (the
# GGUF block's 20 bytes with the FP16 d split to meta). The meta
# granularity is one FP16 d per window for both formats.
WIN_BYTES = 32
Q5_WIN_BYTES = 20
# K-quant super-block payloads: Q6_K's ql||qh planes (192 B) and
# Q4_K's nibble plane (128 B), GGUF byte order, with each block's
# scale tail (Q6_K sc8||d = 18 B, Q4_K d||dmin||k4-scales = 16 B)
# split verbatim to the meta region as one entry per chunk. A
# super-block chunk serves 8 windows (256 elements) of one row.
Q6_CHUNK_BYTES = 192
Q4_CHUNK_BYTES = 128
Q6_META_BYTES = 18
Q4_META_BYTES = 16
# Meta assembly buffer floor: two chunk entries (bonded pair) plus a
# beat; n_outputs=4 widens it (self.meta_buf_bytes) to hold the bonded
# quad's entries (4 * 18 B) beside an incoming beat. The payload side
# no longer uses a flat buffer (see UnitAssembler).
SG_META_BUF_BYTES = 128


def stream_pairs(rows):
    """Output-row pairs a tile of ``rows`` rows occupies (SG1 at
    n_outputs=2)."""
    return stream_groups(rows, 2)


def stream_groups(rows, n_outputs):
    """Output-row groups of ``n_outputs`` rows a tile occupies.

    The SG1 consumption order interleaves whole groups (pairs at
    n_outputs=2, quads at n_outputs=4 -- the D2 "SG2" quad image);
    the group width must equal the consuming StreamSupply's
    n_outputs or the decode reads the wrong lanes.
    """
    return (rows + n_outputs - 1) // n_outputs


def rtl_stream_groups(rows, lanes):
    return rtl_div_const(rows + (lanes - 1), lanes)[:len(rows)]


def pack_stream_q8_0(wsegs, k, rows, n_outputs=2):
    """Pack segment dicts into SG1 payload and meta byte strings.

    ``wsegs`` is row-major, K order (the order ``model.pack_row``
    consumes): ``rows * (k // 32)`` dicts with signed-8 ``w`` lists and
    an FP16 ``d`` bit pattern each. Returns ``(payload, meta)``; the
    caller pads ``meta`` to the bus beat when building the DRAM image.
    """
    nsegs = k // 32
    assert len(wsegs) == rows * nsegs
    payload = bytearray()
    meta = bytearray()
    for grp in range(stream_groups(rows, n_outputs)):
        for j in range(nsegs):
            for lane in range(n_outputs):
                row = n_outputs * grp + lane
                if row >= rows:
                    break
                seg = wsegs[row * nsegs + j]
                payload += bytes(x & 0xFF for x in seg['w'])
                meta += seg['d'].to_bytes(2, 'little')
    return bytes(payload), bytes(meta)


def unpack_stream_q8_0(payload, meta, k, rows, n_outputs=2):
    """Mirror of :func:`pack_stream_q8_0`, returning the segment dicts."""
    nsegs = k // 32
    segs = [None] * (rows * nsegs)
    it = iter(meta)
    off = 0
    for grp in range(stream_groups(rows, n_outputs)):
        for j in range(nsegs):
            for lane in range(n_outputs):
                row = n_outputs * grp + lane
                if row >= rows:
                    break
                d = int.from_bytes(bytes((next(it), next(it))), 'little')
                segs[row * nsegs + j] = dict(w=[
                    b - 256 if b >= 128 else b
                    for b in payload[off:off + WIN_BYTES]
                ],
                                             d=d,
                                             dmin=0,
                                             mi=0,
                                             sc0=0,
                                             sc1=0)
                off += WIN_BYTES
    assert off == len(payload)
    return segs


def pack_stream_q5_0(wsegs, k, rows, n_outputs=2):
    """Pack segment dicts into SG1 payload and meta byte strings.

    Same pair-major consumption order as :func:`pack_stream_q8_0`, but
    each segment contributes its 20 real GGUF payload bytes -- ``qh``
    as a little-endian u32, then the 16 nibble bytes -- with the FP16
    ``d`` split to the meta region, so the decoder exercises actual
    GGUF byte decode. ``wsegs`` entries carry weights centred at -16.
    """
    nsegs = k // 32
    assert len(wsegs) == rows * nsegs
    payload = bytearray()
    meta = bytearray()
    for grp in range(stream_groups(rows, n_outputs)):
        for j in range(nsegs):
            for lane in range(n_outputs):
                row = n_outputs * grp + lane
                if row >= rows:
                    break
                seg = wsegs[row * nsegs + j]
                qs = bytearray(16)
                qh = 0
                for i in range(WIN_BYTES):
                    code = seg['w'][i] + 16
                    assert 0 <= code <= 31
                    if i < 16:
                        qs[i] |= code & 0xF
                    else:
                        qs[i - 16] |= (code & 0xF) << 4
                    qh |= ((code >> 4) & 1) << i
                payload += qh.to_bytes(4, 'little')
                payload += bytes(qs)
                meta += seg['d'].to_bytes(2, 'little')
    return bytes(payload), bytes(meta)


def unpack_stream_q5_0(payload, meta, k, rows, n_outputs=2):
    """Mirror of :func:`pack_stream_q5_0`, returning the segment dicts."""
    nsegs = k // 32
    segs = [None] * (rows * nsegs)
    it = iter(meta)
    off = 0
    for grp in range(stream_groups(rows, n_outputs)):
        for j in range(nsegs):
            for lane in range(n_outputs):
                row = n_outputs * grp + lane
                if row >= rows:
                    break
                d = int.from_bytes(bytes((next(it), next(it))), 'little')
                qh = int.from_bytes(payload[off:off + 4], 'little')
                qs = payload[off + 4:off + Q5_WIN_BYTES]
                segs[row * nsegs + j] = dict(w=[(((qs[i] & 0xF) if i < 16 else
                                                  (qs[i - 16] >> 4))
                                                 | (((qh >> i) & 1) << 4)) - 16
                                                for i in range(WIN_BYTES)],
                                             d=d,
                                             dmin=0,
                                             mi=0,
                                             sc0=0,
                                             sc1=0)
                off += Q5_WIN_BYTES
    assert off == len(payload)
    return segs


def pack_stream_kquant(fmt, wsegs, k, rows, n_outputs=2):
    """Pack K-quant segment dicts into SG1 payload and meta strings.

    Consumption order matches the other packers at *chunk* granularity:
    for each output-row pair, for each 8-segment super-block, for each
    lane, one payload chunk (Q6_K: ql||qh, 192 B; Q4_K: qs, 128 B --
    the GGUF block bytes verbatim) followed later by the same block's
    scale tail as one meta entry (Q6_K: sc8||d, 18 B; Q4_K:
    d||dmin||k4-scales, 16 B -- also verbatim), so the decoder
    reproduces ``unpack_row`` exactly from the two regions.
    """
    from tests.troom.model import pack_row

    nsegs = k // 32
    chunks = nsegs // 8
    assert len(wsegs) == rows * nsegs
    assert chunks * 8 == nsegs, f'{fmt} needs k a multiple of 256'
    payload = bytearray()
    meta = bytearray()
    for grp in range(stream_groups(rows, n_outputs)):
        for sb in range(chunks):
            for lane in range(n_outputs):
                row = n_outputs * grp + lane
                if row >= rows:
                    break
                blk = pack_row(
                    fmt, wsegs[row * nsegs + 8 * sb:row * nsegs + 8 * sb + 8])
                if fmt is Format.Q6_K:
                    payload += blk[:Q6_CHUNK_BYTES]
                    meta += blk[Q6_CHUNK_BYTES:]
                    assert len(meta) % Q6_META_BYTES == 0
                else:
                    meta += blk[:Q4_META_BYTES]
                    payload += blk[Q4_META_BYTES:]
    return bytes(payload), bytes(meta)


def unpack_stream_kquant(fmt, payload, meta, k, rows, n_outputs=2):
    """Mirror of :func:`pack_stream_kquant` via the reference decoder.

    Reassembles each GGUF block from its chunk and scale entry --
    Q6_K: block = chunk || entry; Q4_K: block = entry || chunk -- and
    decodes with ``unpack_row``.
    """
    from tests.troom.model import unpack_row

    nsegs = k // 32
    chunks = nsegs // 8
    segs = [None] * (rows * nsegs)
    chunk_b = Q6_CHUNK_BYTES if fmt is Format.Q6_K else Q4_CHUNK_BYTES
    entry_b = Q6_META_BYTES if fmt is Format.Q6_K else Q4_META_BYTES
    p_off = 0
    m_off = 0
    for grp in range(stream_groups(rows, n_outputs)):
        for sb in range(chunks):
            for lane in range(n_outputs):
                row = n_outputs * grp + lane
                if row >= rows:
                    break
                chunk = payload[p_off:p_off + chunk_b]
                entry = meta[m_off:m_off + entry_b]
                blk = (chunk + entry if fmt is Format.Q6_K else entry + chunk)
                for j, s in enumerate(unpack_row(fmt, blk)):
                    segs[row * nsegs + 8 * sb + j] = s
                p_off += chunk_b
                m_off += entry_b
    assert p_off == len(payload) and m_off == len(meta)
    return segs


class StreamMatmulDesc(HasTRoomParams, Record):
    """Streaming MATMUL descriptor naming the two SG1 regions.

    ``k`` is the shared reduction length (a whole number of 32-element
    windows with ``rows * k`` a whole number of bus beats so the
    payload region carries no padding), ``rows`` the output row count,
    and ``p_base``/``m_base`` the beat-aligned payload/meta bases.
    """

    def __init__(self, params, name=None, src_loc_at=0):
        HasTRoomParams.__init__(self, params)
        Record.__init__(self, [
            ('fmt', Format, Direction.FANOUT),
            ('k', self.lg_act_k + 1, Direction.FANOUT),
            ('rows', self.rows_bits, Direction.FANOUT),
            ('ps_base', self.addr_width, Direction.FANOUT),
            ('pl_beats', 5, Direction.FANOUT),
            ('early_weights', 1, Direction.FANOUT),
            ('p_base', self.addr_width, Direction.FANOUT),
            ('m_base', self.addr_width, Direction.FANOUT),
        ],
                        name=name,
                        src_loc_at=src_loc_at + 1)


class StreamReadReq(HasTRoomParams, Record):
    """One burst read: beat-aligned ``addr``, ``beats`` beats, ``meta``
    selects the region whose FIFO takes the responses."""

    def __init__(self, params, name=None, src_loc_at=0):
        HasTRoomParams.__init__(self, params)
        Record.__init__(self, [
            ('addr', self.addr_width, Direction.FANOUT),
            ('beats', 9, Direction.FANOUT),
            ('meta', 1, Direction.FANOUT),
        ],
                        name=name,
                        src_loc_at=src_loc_at + 1)


class StreamReadResp(HasTRoomParams, Record):
    """One returned beat, tagged with its region."""

    def __init__(self, params, name=None, src_loc_at=0):
        HasTRoomParams.__init__(self, params)
        Record.__init__(self, [
            ('data', self.data_width, Direction.FANOUT),
            ('meta', 1, Direction.FANOUT),
        ],
                        name=name,
                        src_loc_at=src_loc_at + 1)


class StreamPayPair(HasTRoomParams, Record):
    """Ordered payload beats, with a third lane on widened readers."""

    def __init__(self, params, name=None, src_loc_at=0, *, triple=False):
        HasTRoomParams.__init__(self, params)
        layout = [
            ('data0', self.data_width, Direction.FANOUT),
            ('data1', self.data_width, Direction.FANOUT),
            ('dual', 1, Direction.FANOUT),
        ]
        if triple:
            layout += [
                ('data2', self.data_width, Direction.FANOUT),
                ('triple', 1, Direction.FANOUT),
            ]
        Record.__init__(self, layout, name=name, src_loc_at=src_loc_at + 1)

    @property
    def beat_count(self):
        return 1 + self.dual + getattr(self, 'triple', 0)


class StreamScaleBeat(HasTRoomParams, Record):
    """One returned scale beat on the striped path's separate port."""

    def __init__(self, params, name=None, src_loc_at=0):
        HasTRoomParams.__init__(self, params)
        Record.__init__(self, [
            ('data', self.data_width, Direction.FANOUT),
        ],
                        name=name,
                        src_loc_at=src_loc_at + 1)


class UnitAssembler(Elaboratable):
    """Assemble a payload beat stream into whole fixed-pitch units.

    The phase B pysim decoder used one flat byte buffer with variable
    shifts (beat placed at the valid count, units shifted out); in RTL
    that maps to a 5120-bit barrel-shifter pair, which the phase B
    step-2/3 notes flag as unacceptable for the phase C route. This
    module is the equivalent structure without wide variable shifts:
    completed units live stationary in a circular bank array, one
    partial-unit register holds the bytes of the unit still arriving,
    and a byte phase names the partial's valid length. The equivalence
    rests on the decoder's invariant that consumption is always a whole
    number of units, so the assembly always holds whole units followed
    by the partial tail of the next one.

    The bank file is a pointer structure, not a shifting one: a
    completion is written exactly once, into the bank at the file tail
    (``(head + ucnt + k) % depth``), under a local write enable, and
    never moves again; ``consume`` advances the head pointer instead of
    shifting the visible sequence down. ``can_take`` guarantees
    ``ucnt + cu <= depth``, so the tail never wraps over live units.
    This removes the per-cycle whole-file steering network (the placed
    fanout report's 5,770-load ``ucnt[2]`` and the consume-indexed
    re-selection of every stored unit) at identical cycle semantics.
    ``clr`` resets only the validity/count/phase state: stale bank
    payload bits are unreachable because every bank read is gated by
    ``ucnt``, and a later completion overwrites its bank in full.

    Per cycle: a firing ``beat`` appends ``beat_bytes`` bytes at
    ``ph``; up to four units complete (``pitches`` bound completions
    and ``can_take`` guarantees ``ucnt + cu <= depth``) and are
    visible on ``slot0``..``slot{slots-1}`` the *same* cycle
    (bypass), so a consumer can emit a unit whose last byte lands this
    cycle; ``consume`` then releases the first units of the visible
    sequence. ``pitch`` is runtime but constrained to ``pitches``,
    which keeps every slice below a constant-offset mux. ``slots``
    names the emission width: at n_outputs=4 the decoder emits four
    units per cycle (flat windows, or the four bonded K-quant chunks
    of one super-block group), so depth must cover the bonded group
    plus the streaming margin (the phase-B b3 lesson: the file held
    one streaming chunk too few and starved the bonded pair).

    Three structural rules keep the mux fabric small, and each is an
    identity on every reachable state:

    * Only the phase bits some pitch can reach get a shift stage, and
      the sub-beat stages span one lane. A pitch with a sub-beat phase
      completes at most four units per intake, so its delivery must
      already be single-lane (``beat1_in``/``beat2_in`` low) for the
      completion count to be exact.
    * A completion index past the pitch's bound (four, or MAX_COMP for
      the 32-byte pitch) is never built; the golden model reads zero
      there, because the view is zero above the arrived bytes and
      ``part`` is zero above ``ph``.
    * The slot bypass is the bank write steering: the completion that
      becomes visible unit v is written to the bank column v reads.
    """

    MAX_COMP = 4  # completions per beat across the payload pitches

    def __init__(self,
                 *,
                 beat_bytes,
                 unit_bytes,
                 depth,
                 pitches,
                 slots=2,
                 banked_slots=False,
                 dual=False,
                 triple=False):
        assert beat_bytes <= unit_bytes
        assert all(p <= unit_bytes for p in pitches)
        self.non_power_of_two_banks = banked_slots and (slots &
                                                        (slots - 1)) != 0
        if self.non_power_of_two_banks:
            assert depth == 2 * slots
        else:
            assert depth & (depth - 1) == 0
        assert 1 <= slots <= depth
        assert banked_slots or slots <= (6 if triple else self.MAX_COMP)
        self.bb = beat_bytes
        self.unit_bytes = unit_bytes
        self.depth = depth
        self.pitches = tuple(pitches)
        self.slots = slots
        # banked_slots: logical unit v is slot[(rotation + v) % slots].
        # Rotate 32-byte decoded results instead of 192-byte chunks.
        # Wide units complete once per beat; flat units fit in 32 bytes.
        self.banked_slots = banked_slots
        if banked_slots:
            assert all(p <= WIN_BYTES or p > beat_bytes for p in pitches)
        # dual: a second beat lane may land in the same cycle, so the
        # intake accepts up to 2*beat_bytes bytes per cycle. The
        # caller must hold beat1_in low whenever the runtime pitch can
        # complete more than MAX_COMP units across two beats (pitch 20
        # with 64-byte beats); pitches of 32 and above stay within
        # MAX_COMP at both widths.
        self.dual = dual
        self.triple = triple
        if triple:
            assert dual and beat_bytes == 64 and depth >= 6
            self.MAX_COMP = 6
            self.beat2 = Signal(beat_bytes * 8)
            self.beat2_in = Signal()
        if dual:
            assert beat_bytes * 2 <= unit_bytes or min(pitches) > beat_bytes

        self.beat = Signal(beat_bytes * 8)
        self.beat_in = Signal()
        if dual:
            self.beat1 = Signal(beat_bytes * 8)
            self.beat1_in = Signal()
        self.pitch = Signal(8)
        self.consume = Signal(range(slots + 1))
        self.clr = Signal()

        self.avail = Signal(range(depth + 1))
        self.can_take = Signal()
        self.slot = [
            Signal(unit_bytes * 8, name=f'slot{i}') for i in range(slots)
        ]
        self.rotation = Signal(max((slots - 1).bit_length(), 1))

        # State (attributes for waveform observation). ``uf[i]`` is
        # bank i of the stationary circular file; ``head`` names the
        # bank holding visible unit 0.
        self.uf = [Signal(unit_bytes * 8, name=f'uf{i}') for i in range(depth)]
        self.head = Signal(range(depth))
        self.ucnt = Signal(range(depth + 1))
        self.part = Signal(unit_bytes * 8)
        self.ph = Signal(8)

    def completion_bound(self, pitch):
        """Most units one intake can complete at ``pitch``.

        ``cu_full`` is a compare sum over exactly this many terms, so no
        completion at or past the bound is ever counted or enabled.
        """
        return self.MAX_COMP if pitch == WIN_BYTES else 4

    def elaborate(self, platform):
        m = Module()
        ub, depth, bb = self.unit_bytes, self.depth, self.bb
        lg_bb = log2_int(bb)

        # The logical assembly view: the partial unit's ``ph`` bytes
        # followed by the intake's lanes -- the part register is kept
        # zero above ``ph`` (enforced by construction below), so the
        # view is the part OR the lanes shifted up by ``ph``. Pitch is
        # fixed until clr. Since transfers append whole beats, a phase
        # is a multiple of gcd(beat_bytes, pitch): one shift stage per
        # bit every reachable phase can set, no others.
        phase_mask = 0
        fine_max = 0
        for pitch in self.pitches:
            for phase in range(0, pitch, gcd(bb, pitch)):
                phase_mask |= phase
                fine_max = max(fine_max, phase & (bb - 1))
        lanes = [self.beat]
        tot = self.ph + bb
        if self.dual:
            lanes.append(Mux(self.beat1_in, self.beat1, 0))
            tot = tot + Mux(self.beat1_in, bb, 0)
        if self.triple:
            lanes.append(Mux(self.beat2_in, self.beat2, 0))
            tot = tot + Mux(self.beat2_in, bb, 0)

        view_bits = (ub + bb * len(lanes)) * 8
        shift = Signal(8)
        m.d.comb += shift.eq(self.ph & phase_mask)
        aligned = Cat(*lanes)[:view_bits]
        if fine_max:
            # Sub-beat stages run first, over one lane plus the largest
            # fine phase: a pitch with a sub-beat phase completes at
            # most four units per intake, which already requires its
            # delivery on one lane, so nothing above that span can move.
            # Shifts add, so the stage order is free.
            fine_span = 8 * (bb + fine_max)
            low = aligned[:fine_span]
            for bit in range(lg_bb):
                if phase_mask & (1 << bit):
                    low = Mux(shift[bit], (low << (8 << bit))[:fine_span], low)
            aligned = Cat(low, aligned[fine_span:])
        for bit in range(lg_bb, phase_mask.bit_length()):
            aligned = Mux(shift[bit], (aligned << (8 << bit))[:view_bits],
                          aligned)
        stream = Cat(self.part, Const(0, bb * len(lanes) * 8)) | aligned
        stream_bits = len(stream)

        # Completion count without a runtime divider (the step-4
        # review's $div hazard): ``pitch`` is constrained to
        # ``pitches`` and the phase invariant ``ph < pitch`` bounds
        # ``tot = ph + beat_bytes``, so for each supported pitch the
        # quotient equals a bounded compare sum (cu <= 4; the extreme
        # is pitch 20 with 64-byte beats: tot <= 83 < 5*20). The
        # residual phase is a per-pitch constant-multiply subtraction
        # off the same selection, so no $div and no runtime $mul is
        # generated (isolated A/B: 589 -> 71 LUT-class, DSP48E2
        # dropped). Same-cycle completion bypass and can_take
        # semantics unchanged; the golden-model phase-coverage tests
        # pin every reachable phase at both beat widths.
        cu_full = Const(0, 3)
        rem = tot
        for p in self.pitches:
            c = sum((tot >= i * p)
                    for i in range(1,
                                   self.completion_bound(p) + 1))[:3]
            cu_full = Mux(self.pitch == p, c, cu_full)
            rem = Mux(self.pitch == p, tot - c * p, rem)
        cu = Mux(self.beat_in, cu_full, 0)

        m.d.comb += self.avail.eq(self.ucnt + cu)
        m.d.comb += self.can_take.eq((self.ucnt + cu_full) <= depth)

        # This cycle's completions: units [k*pitch, (k+1)*pitch) of
        # the view, a constant slice per candidate pitch (cases past
        # the stream are dead states and read as zero). A completion
        # index the pitch cannot reach stays zero.
        comp = []
        for k in range(self.MAX_COMP):
            per_p = []
            for p in self.pitches:
                lo, hi = 8 * p * k, 8 * p * (k + 1)
                # The third lane is enabled only for the 32-byte pitch.
                # A wide unit can complete only once on the two-beat path.
                intake_bytes = bb * (1 + self.dual +
                                     (self.triple and p == WIN_BYTES))
                reachable = k < self.completion_bound(p)
                if self.banked_slots:
                    reachable = reachable and k * p < intake_bytes
                if reachable and hi <= stream_bits:
                    per_p.append(
                        Cat(stream[lo:hi], Const(0, ub * 8 - (hi - lo))))
                else:
                    per_p.append(Const(0, ub * 8))
            expr = per_p[0]
            for p, e in zip(self.pitches[1:], per_p[1:]):
                expr = Mux(self.pitch == p, e, expr)
            completed = Signal(ub * 8, name=f'completed{k}')
            m.d.comb += completed.eq(expr)
            comp.append(completed)

        lgh = len(self.head)
        lg_slots = (self.slots - 1).bit_length()

        def wrap(index):
            if self.non_power_of_two_banks:
                return Mux(index >= depth, index - depth, index)
            return index[:lgh]

        def bank_at(sel):
            expr = self.uf[0]
            for i in range(1, self.depth):
                expr = Mux(sel == i, self.uf[i], expr)
            return expr

        # Stationary bank file: write every completion, including those
        # bypassed to the consumer this cycle.  Such writes target freed
        # banks and are harmless: can_take bounds ucnt + cu <= depth, so
        # no completion can overwrite an old live unit.  Keeping consume
        # out of the wide bank enables cuts the emission-decision
        # feedback path.  Completion k always targets (tail + k) % depth.
        # Select its data once per bank residue, then use a narrow local
        # enable; do not build a priority write/hold mux per bank and bit.
        tail = Signal(lgh)
        m.d.comb += tail.eq(wrap(self.head + self.ucnt))
        write_banks = depth if self.non_power_of_two_banks else min(
            depth, 8 if self.triple else 4)
        # In the banked production configuration the wide pitches
        # complete at most one unit.  Broadcast its suffix to every bank.
        # For a narrow pitch completion zero already zero-extends that
        # suffix, including when a different completion takes this bank.
        share_suffix = self.banked_slots and all(p <= WIN_BYTES or p >= bb *
                                                 (1 + self.dual)
                                                 for p in self.pitches)
        write_bits = min(ub, WIN_BYTES) * 8 if share_suffix else ub * 8
        write_data = []
        for residue in range(write_banks):
            expr = Const(0, write_bits)
            for k in range(self.MAX_COMP):
                if self.non_power_of_two_banks:
                    hit = tail == (residue - k) % depth
                else:
                    hit = (tail + k)[:log2_int(write_banks)] == residue
                if depth < self.MAX_COMP:
                    hit = hit & (k < cu)
                expr = expr | Mux(hit, comp[k][:write_bits], 0)
            if share_suffix:
                expr = Cat(expr, comp[0][write_bits:])
            selected = Signal(ub * 8, name=f'write_data{residue}')
            m.d.comb += selected.eq(expr)
            write_data.append(selected)

        if self.non_power_of_two_banks:
            head_row = self.head >= self.slots
            m.d.comb += self.rotation.eq(
                Mux(head_row, self.head - self.slots, self.head))
        else:
            m.d.comb += self.rotation.eq(
                self.head[:lg_slots] if self.banked_slots and lg_slots else 0)
        for v in range(self.slots):
            # Visible unit v: the bank copy when ucnt > v, else this
            # cycle's completion v - ucnt bypasses to the slot.
            if self.banked_slots:
                # Column v holds logical offset (v - head) % slots.
                # Columns below head's column read the next bank row.
                # This works for arbitrary heads, including every
                # short-tail superblock; no alignment assumption.
                offset = Signal(max(lg_slots, 1), name=f'offset{v}')
                bypass = Const(0, WIN_BYTES * 8)
                if self.non_power_of_two_banks:
                    next_row = v < self.rotation
                    m.d.comb += offset.eq(
                        Mux(next_row, v + self.slots - self.rotation,
                            v - self.rotation))
                    row_sel = head_row ^ next_row
                    selected = Mux(row_sel, self.uf[v + self.slots],
                                   self.uf[v])
                    # The completion that becomes visible unit v is
                    # written to the bank this column reads (its queue
                    # index is ucnt + v - offset and its bank is
                    # (head + ucnt + k) % depth), so the write steering
                    # already holds it: no second selection per slot.
                    bypass = Mux(row_sel,
                                 write_data[v + self.slots][:WIN_BYTES * 8],
                                 write_data[v][:WIN_BYTES * 8])
                else:
                    row = Signal(max(lgh - lg_slots, 1), name=f'bank_row{v}')
                    m.d.comb += [
                        offset.eq((
                            v - self.rotation)[:lg_slots] if lg_slots else 0),
                        row.eq((self.head[lg_slots:] + (v < self.rotation)
                                )[:lgh - lg_slots] if lgh > lg_slots else 0),
                    ]
                    selected = Const(0, ub * 8)
                    for r in range(depth // self.slots):
                        selected = Mux(row == r, self.uf[r * self.slots + v],
                                       selected)
                        bypass = Mux(
                            row == r, write_data[(r * self.slots + v) %
                                                 write_banks][:WIN_BYTES * 8],
                            bypass)
                # Only the narrow flat prefix needs the completion
                # bypass. A >32-B unit completes at most once per
                # beat, so its valid bypass can only be completion 0.
                expr = Mux(self.ucnt > offset, selected,
                           Cat(bypass, comp[0][WIN_BYTES * 8:]))
            else:
                expr = comp[v]
                for k in range(v + 1):
                    expr = Mux(self.ucnt == k, comp[v - k], expr)
                expr = Mux(self.ucnt > v, bank_at((self.head + v)[:lgh]), expr)
            m.d.comb += self.slot[v].eq(expr)

        for b in range(depth):
            enable = Const(0)
            for k in range(self.MAX_COMP):
                if self.non_power_of_two_banks:
                    hit = tail == (b - k) % depth
                else:
                    hit = (tail + k)[:lgh] == b
                enable = enable | ((k < cu) & hit)
            with m.If(self.beat_in & enable):
                m.d.sync += self.uf[b].eq(write_data[b % len(write_data)])

        # Residual: the view past this intake's completions, right
        # aligned.  Only the offsets some (pitch, completion count) can
        # select are built; share identical offsets.  Each is read at
        # the widest pitch that selects it: the view is zero above the
        # arrived bytes and the residual is shorter than the pitch, so
        # residual bytes at or above the pitch are already zero.
        offsets = {}
        for pitch in self.pitches:
            lanes_n = 3 if self.triple and pitch == WIN_BYTES else 1 + self.dual
            max_bytes = bb * lanes_n
            max_completed = min(self.completion_bound(pitch),
                                (max_bytes + pitch - 1) // pitch)
            for completed in range(bb // pitch, max_completed + 1):
                offset = pitch * completed
                hit = (self.pitch == pitch) & (cu == completed)
                if offset not in offsets:
                    offsets[offset] = [Const(0), 0]
                offsets[offset][0] = offsets[offset][0] | hit
                offsets[offset][1] = max(offsets[offset][1], pitch)
        part_sliced = Const(0, ub * 8)
        for offset, (hit, widest) in sorted(offsets.items()):
            lo = offset * 8
            if lo < stream_bits:
                keep = min(ub, widest) * 8
                data = Cat(stream[lo:lo + keep], Const(0, ub * 8 - keep))
                part_sliced = part_sliced | Mux(hit, data, 0)
        part_next = Mux(self.beat_in, part_sliced, self.part)

        m.d.sync += self.ucnt.eq(
            Mux(self.clr, 0, self.ucnt + cu - self.consume))
        m.d.sync += self.part.eq(Mux(self.clr, 0, part_next))
        m.d.sync += self.ph.eq(
            Mux(self.clr, 0, Mux(self.beat_in, rem, self.ph)))
        m.d.sync += self.head.eq(
            Mux(self.clr, 0, wrap(self.head + self.consume)))

        return m


@dataclass(frozen=True)
class ReadPort:
    name: str
    issue_queue_depth: int
    return_buffer_beats: int
    sync_read_buffer: bool = True
    axi_data_width: int | None = None


@dataclass(frozen=True)
class ReadRegion:
    """Half-open logical window with port selection based on absolute addresses."""

    start: int
    end: int
    ports: tuple[int, ...]
    stripe_bytes: int | None = None

    def contains(self, start, end):
        return (start >= self.start) & (start < self.end) & (end <= self.end)

    def port_for(self, addr):
        if self.stripe_bytes is None:
            return Const(self.ports[0])
        shift = log2_int(self.stripe_bytes)
        return Array(Const(port)
                     for port in self.ports)[addr[shift:shift +
                                                  log2_int(len(self.ports))]]

    def beats_to_boundary(self, addr, lg_beat):
        shift = log2_int(self.stripe_bytes)
        return (1 << (shift - lg_beat)) - addr[lg_beat:shift]


@dataclass(frozen=True)
class ReadTopology:
    """SG1 routing and buffering. Region and port references are tuple indices."""

    ports: tuple[ReadPort, ...]
    regions: tuple[ReadRegion, ...]
    primary_region: int
    scale_region: int
    shared_port: int
    secondary_region: int | None = None
    retirement_lanes: int = 1
    dual_bank_shared_port: bool = False

    @property
    def payload_regions(self):
        primary = self.regions[self.primary_region]
        if self.secondary_region is None:
            return (primary, )
        return (primary, self.regions[self.secondary_region])

    @property
    def scales(self):
        return self.regions[self.scale_region]

    @property
    def striped(self):
        return self.secondary_region is not None

    def validate(self, addr_width, beat_bytes):
        region_ids = (self.primary_region, self.scale_region)
        if self.striped:
            region_ids += (self.secondary_region, )
        if any(not 0 <= i < len(self.regions) for i in region_ids):
            raise ValueError('invalid SG1 region reference')
        if self.striped and self.primary_region == self.secondary_region:
            raise ValueError('SG1 striping requires distinct payload regions')
        if not 0 <= self.shared_port < len(self.ports):
            raise ValueError('invalid shared SG1 port')
        if self.retirement_lanes not in (1, 2, 3):
            raise ValueError('SG1 supports one to three retirement lanes')
        if (self.striped or self.retirement_lanes > 1) and beat_bytes != 64:
            raise ValueError(
                'SG1 striping and dual retirement require 64-byte beats')
        names = {port.name for port in self.ports}
        if '' in names or len(names) != len(self.ports):
            raise ValueError('SG1 port names must be nonempty and unique')
        for port in self.ports:
            if port.issue_queue_depth < 0 or port.return_buffer_beats < 2:
                raise ValueError('invalid SG1 port buffering')
            if self.retirement_lanes == 3 and port.return_buffer_beats < 4:
                raise ValueError(
                    'Three-lane retirement requires at least four beats per port'
                )
            if port.return_buffer_beats & (port.return_buffer_beats - 1):
                raise ValueError(
                    'SG1 return buffer depth must be a power of two')
        last_end = 0
        for region in sorted(self.regions, key=lambda region: region.start):
            if not (last_end <= region.start < region.end <= 1 << addr_width
                    and
                    region.start % beat_bytes == region.end % beat_bytes == 0):
                raise ValueError(
                    'SG1 regions must be disjoint, beat-aligned windows')
            last_end = region.end
            count = len(region.ports)
            if (not count or len(set(region.ports)) != count
                    or any(not 0 <= i < len(self.ports)
                           for i in region.ports)):
                raise ValueError('invalid SG1 region ports')
            stripe = region.stripe_bytes
            if count == 1:
                if stripe is not None:
                    raise ValueError(
                        'a direct SG1 region cannot specify an interleave')
            elif (count & (count - 1) or stripe is None or stripe < beat_bytes
                  or stripe & (stripe - 1) or stripe * count > 1 << addr_width
                  or any(
                      self.ports[i].return_buffer_beats < stripe // beat_bytes
                      for i in region.ports)):
                raise ValueError(
                    'invalid SG1 interleave geometry or buffering')
        if self.scales.stripe_bytes is not None:
            raise ValueError('SG1 scales require a direct region')
        for i, port in enumerate(self.ports):
            if port.axi_data_width not in (None, beat_bytes * 8):
                if (port.axi_data_width != beat_bytes * 4
                        or i in (self.shared_port, self.scales.ports[0])):
                    raise ValueError(
                        'Only dedicated payload ports may use half-width AXI')


class StreamSupply(HasTRoomParams, Elaboratable):
    """SG1 reader and format decoder feeding per-lane segment queues.

    Configuration is held by the unit: geometry inputs (including the
    format, which fixes the window's payload size and decode) are
    valid on the ``start`` pulse and stay held for the run.
    """

    def __init__(self, params, *, read_topology=None):
        super().__init__(params)
        self.read_topology = read_topology
        self.split_responses = read_topology is not None
        self.striped = self.split_responses and read_topology.striped
        self.dual_retirement = (self.split_responses
                                and read_topology.retirement_lanes >= 2)
        self.triple_retirement = (self.split_responses
                                  and read_topology.retirement_lanes == 3)
        self.secondary_base = Signal(self.addr_width)
        self.primary_beats = Signal(5)

        if self.data_width > 512:
            raise ValueError(
                f'stream prototype supports up to 512-bit beats, not '
                f'{self.data_width}')
        # Emission skid lanes and per-format capacities sized by the
        # group width: the decoder emits up to n_outputs units per
        # cycle, and the K-quant bonded group needs its n_outputs
        # chunks resident plus the streaming margin (b3's one-chunk
        # starvation), so the unit file holds the group plus margin
        # (two rows of banks for non-power-of-two lane counts). The meta buffer
        # must serve the bonded group's entries
        # (n * 18 B worst case) while still admitting a full beat.
        self.lg_lane = max((self.n_outputs - 1).bit_length(), 1)
        min_depth = 8 if self.n_outputs > 2 or self.triple_retirement else 4
        if self.n_outputs & (self.n_outputs - 1):
            self.asm_depth = 2 * self.n_outputs
        else:
            self.asm_depth = max(min_depth, 2 * self.n_outputs)
        self.meta_buf_bytes = max(
            SG_META_BUF_BYTES,
            self.n_outputs * Q6_META_BYTES + self.beat_bytes)
        self.circular_meta = self.split_responses and self.beat_bytes == 64

        self.mem_rreq = Decoupled(StreamReadReq, self.params)
        if self.split_responses:
            # Payload beats arrive in order with up to two per response
            # on their own port, scales on a second port that bypasses
            # the payload ordering entirely.
            self.mem_rresp = Decoupled(StreamPayPair,
                                       self.params,
                                       triple=self.triple_retirement)
            self.mem_sresp = Decoupled(StreamScaleBeat, self.params)
        else:
            self.mem_rresp = Decoupled(StreamReadResp, self.params)

        self.seg_q = [
            Queue(self.mm_seg_fifo,
                  WSegWord,
                  self.params,
                  flow=self.sg_lane_bypass) for _ in range(self.n_outputs)
        ]
        self.segs = [q.deq for q in self.seg_q]

        # Register the request before AXI arbitration. Reserve capacity
        # when this queue accepts, including requests not yet on the bus.
        self.req_q = Queue(2, StreamReadReq, self.params, flow=False)

        if self.split_responses:
            self.pay_q = Queue(self.sg_pay_fifo,
                               StreamPayPair,
                               self.params,
                               flow=False,
                               has_flush=True,
                               use_sync_read_mem=True,
                               triple=self.triple_retirement)
            # Pair formation gate: formats whose pitch completes at
            # most MAX_COMP units across two beats (everything but
            # Q5_0's 20-byte pitch) may retire two beats per cycle.
            # The gate lives here so the merger never forms a pair the
            # assembler would have to halve.
            self.pair_ok = Signal()
            self.triple_ok = Signal()
        else:
            self.pay_q = Queue(self.sg_pay_fifo,
                               Record,
                               [('data', self.data_width, Direction.FANOUT)],
                               flow=False,
                               has_flush=True)
        self.meta_q = Queue(self.sg_meta_fifo,
                            Record,
                            [('data', self.data_width, Direction.FANOUT)],
                            flow=False,
                            has_flush=True)

        self.run = Signal()
        self.start = Signal()
        self.fmt = Signal(Format)
        self.rows = Signal(self.rows_bits)
        # lg_act_segs + 1: a full-width k (nsegs == 2**lg_act_segs,
        # e.g. k == act_max_k) must not truncate the wrap compare.
        self.nsegs = Signal(self.lg_act_segs + 1)
        self.p_base = Signal(self.addr_width)
        self.m_base = Signal(self.addr_width)
        self.p_total = Signal(48)
        self.m_total = Signal(48)

        self.quiescent = Signal()

        # Reader observation: beats accepted per region, bursts issued,
        # and cycles the port sat idle while either region still had
        # work (downstream backpressure and arbitration bubbles).
        self.pay_beats = Signal(48)
        self.meta_beats = Signal(48)
        self.bursts = Signal(48)
        self.port_idle = Signal(48)

        # Reader cursors and decoder state, kept as attributes for
        # waveform and testbench observation (the supply
        # discipline). The payload assembly lives in ``pay_asm``
        # (whole units + one partial unit); the meta side keeps a
        # small flat byte buffer (bounded by self.meta_buf_bytes).
        self.p_addr = Signal(self.addr_width)
        self.p_rem = Signal(48)
        self.m_addr = Signal(self.addr_width)
        self.m_rem = Signal(48)
        self.p_out = Signal(range(min(self.sg_pay_fifo, self.sg_credits) + 1))
        self.m_out = Signal(range(min(self.sg_meta_fifo, self.sg_credits) + 1))
        # Reserved beats = queued requests + in-flight responses + FIFO
        # contents. Only decoder dequeue releases a reservation. Landing
        # moves a beat from outstanding to stored without changing used.
        self.p_used = Signal(range(self.sg_pay_fifo + 1))
        self.m_used = Signal(range(self.sg_meta_fifo + 1))
        self.pair = Signal(self.rows_bits)
        self.j = Signal(self.lg_act_segs)
        self.lane = Signal(self.lg_lane)
        # Group-walk geometry, latched at start (run-constant): the
        # short tail group's lane count and exact groups remaining.
        # Saturation is safe only inside the bounded successor chain,
        # never in the persistent counter (large tiles would reach
        # their short tail prematurely).
        self.tail_lanes = Signal(self.lg_lane + 1)
        self.grp_rem = Signal(self.rows_bits)
        if not self.circular_meta:
            self.mdata = Signal(self.meta_buf_bytes * 8)
        self.mhave = Signal(8)
        if self.mm_starvation_counters:
            self.payload_short = Signal()
            self.scale_short = Signal()
            self.payload_eligible = Signal()
            self.scale_eligible = Signal()
        self.pay_asm = UnitAssembler(beat_bytes=self.beat_bytes,
                                     unit_bytes=Q6_CHUNK_BYTES,
                                     depth=self.asm_depth,
                                     pitches=(Q5_WIN_BYTES, WIN_BYTES,
                                              Q4_CHUNK_BYTES, Q6_CHUNK_BYTES),
                                     slots=self.n_outputs,
                                     banked_slots=True,
                                     dual=self.dual_retirement,
                                     triple=self.triple_retirement)

        # Emission skid: the registered boundary between assembly and
        # decoding. The emission decision (the fire vector, unit
        # release, meta consumption, window advance) is count
        # arithmetic and resolves in the decision cycle; the *data* it
        # captured -- the whole-unit slots, the chunk-relative window
        # indices, the extracted scale fields, and the row/lane
        # targets -- is registered here and pushed into the lane
        # queues the following cycle, with the queues' capacity
        # reservation covering the one in-flight push (``lane_ready``
        # counts pending pushes). The push always finds space, so the
        # skid is a pure pipeline stage: payload and metadata
        # advancement stay synchronized and steady-state throughput is
        # unchanged, at one cycle of emission latency per record
        # stream (not per record).
        fld_widths = (('d', 16), ('dmin', 16), ('mi', 6), ('sc0', 8), ('sc1',
                                                                       8))
        self.s_v = [Signal(name=f's_v{i}') for i in range(self.n_outputs)]
        self.s_lane = [
            Signal(self.lg_lane, name=f's_lane{i}')
            for i in range(self.n_outputs)
        ]
        self.s_row = [
            Signal(self.rows_bits, name=f's_row{i}')
            for i in range(self.n_outputs)
        ]
        self.s_slot = [
            Signal(Q6_CHUNK_BYTES * 8, name=f's_slot{i}')
            for i in range(self.n_outputs)
        ]
        self.s_rotation = Signal(self.lg_lane)
        self.s_jn = [Signal(3, name=f's_jn{i}') for i in range(self.n_outputs)]
        self.s_f = [{
            n: Signal(w, name=f's_f{i}_{n}')
            for n, w in fld_widths
        } for i in range(self.n_outputs)]

    def elaborate(self, platform):
        m = Module()

        m.submodules.req_q = self.req_q
        m.d.comb += self.req_q.deq.connect(self.mem_rreq)
        m.submodules.pay_q = self.pay_q
        m.submodules.meta_q = self.meta_q
        m.submodules.pay_asm = self.pay_asm
        for i, q in enumerate(self.seg_q):
            setattr(m.submodules, f'seg_q{i}', q)

        bb = self.beat_bytes
        n = self.n_outputs
        lg_lane = self.lg_lane

        # Group walk: ``pair`` names an n_outputs-wide output-row group
        # (a pair at n=2, a quad at n=4); the last group may be short
        # (``tail_lanes`` lanes, 1..n), which masks those cells'
        # windows and the join's lanes. The geometry is run-constant:
        # it is computed once from ``rows`` and latched at ``start``
        # (the Register-once-extents discipline -- the first n=4 route
        # measured the whole cur_rows -> groups/tail -> successor
        # chain -> emission-fire -> bank-write-enable cone as the
        # failing family). ``grp_rem`` remains exact across cycles;
        # only the bounded successor chain uses a saturated distance
        # to the tail instead of per-successor group-index compares.
        tail_lanes = self.tail_lanes
        grp_rem = self.grp_rem
        groups_n = rtl_stream_groups(self.rows, n)
        tail_n = (self.rows - (groups_n - 1) * n)[:lg_lane + 1]

        #
        # Reader: two linear region walkers sharing the tagged port.
        # Meta has request priority; payload keeps the port whenever
        # meta cannot issue.
        #
        p_addr, p_rem = self.p_addr, self.p_rem
        m_addr, m_rem = self.m_addr, self.m_rem
        p_out, m_out = self.p_out, self.m_out

        def burst_size(rem, used, outstanding, depth):
            # Both differences are nonnegative by the reservation
            # invariant. Keep local arithmetic at capacity width; only
            # the final tail comparison touches the 48-bit region length.
            room = Signal(range(depth + 1))
            credit = Signal(range(min(depth, self.sg_credits) + 1))
            cap = Signal(range(self.sg_burst_beats + 1))
            burst = Signal(range(self.sg_burst_beats + 1))
            m.d.comb += [
                room.eq(depth - used),
                credit.eq(min(depth, self.sg_credits) - outstanding),
                cap.eq(
                    Mux(
                        room < credit,
                        Mux(room < self.sg_burst_beats, room,
                            self.sg_burst_beats),
                        Mux(credit < self.sg_burst_beats, credit,
                            self.sg_burst_beats))),
                burst.eq(Mux(rem < cap, rem, cap)),
            ]
            return burst

        payload_addr = p_addr
        if self.striped:
            secondary_addr = Signal(self.addr_width)
            phase = Signal(5)
            primary_beats = self.primary_beats
            striped = self.secondary_base != 0
            use_secondary = striped & (phase >= primary_beats)
            payload_addr = Mux(use_secondary, secondary_addr, p_addr)
            phase_left = Mux(use_secondary, SG1_STRIPE_BEATS - phase,
                             primary_beats - phase)
            with m.If(self.start):
                m.d.sync += [
                    secondary_addr.eq(self.secondary_base),
                    phase.eq(0)
                ]
        p_burst = burst_size(p_rem, self.p_used, p_out, self.sg_pay_fifo)
        m_burst = burst_size(m_rem, self.m_used, m_out, self.sg_meta_fifo)
        # Accumulate freed payload reservations before another request.
        # Greedy single-beat refills fragment a linear stream into tiny
        # AXI transactions. Clip the threshold to the payload capacity
        # and outstanding limit, and admit the final short region tail,
        # so batching cannot strand a small FIFO or terminal remainder.
        p_min = min(self.sg_min_burst_beats, self.sg_pay_fifo, self.sg_credits)
        stripe_tail = Const(0)
        if self.read_topology is not None:
            for region in self.read_topology.payload_regions:
                if region.stripe_bytes is not None:
                    stripe_left = region.beats_to_boundary(
                        payload_addr, self.lg_beat)
                    at_boundary = (region.contains(payload_addr, payload_addr)
                                   & (p_burst >= stripe_left))
                    p_burst = Mux(at_boundary, stripe_left, p_burst)
                    stripe_tail |= at_boundary
        if self.striped:
            phase_limit = striped & (p_burst >= phase_left)
            p_burst = Mux(phase_limit, phase_left, p_burst)
            stripe_tail |= phase_limit
        p_want = self.run & (p_burst != 0) & ((p_burst >= p_min)
                                              | (p_burst == p_rem)
                                              | stripe_tail)
        # Metadata defaults to eager refills: batching it requires more
        # lead time/storage ahead of the much larger payload stream.
        m_min = min(self.sg_meta_min_burst_beats, self.sg_meta_fifo,
                    self.sg_credits)
        m_want = self.run & (m_burst != 0) & ((m_burst >= m_min)
                                              | (m_burst == m_rem))
        request = self.req_q.enq
        m.d.comb += [
            request.valid.eq(p_want | m_want),
            request.bits.addr.eq(Mux(m_want, m_addr, payload_addr)),
            request.bits.beats.eq(Mux(m_want, m_burst, p_burst)),
            request.bits.meta.eq(m_want),
        ]

        if self.split_responses:
            # Payload pairs land whole; each fire retires one or two
            # beats. Scales arrive independently on mem_sresp.
            rpair = self.mem_rresp
            mbeat = self.mem_sresp
            m.d.comb += [
                rpair.ready.eq(self.pay_q.enq.ready),
                self.pay_q.enq.valid.eq(rpair.valid),
                self.pay_q.enq.bits.eq(rpair.bits),
                mbeat.ready.eq(self.meta_q.enq.ready),
                self.meta_q.enq.valid.eq(mbeat.valid),
                self.meta_q.enq.bits.data.eq(mbeat.bits.data),
            ]
        else:
            rresp = self.mem_rresp
            m.d.comb += rresp.ready.eq(
                Mux(rresp.bits.meta, self.meta_q.enq.ready,
                    self.pay_q.enq.ready))
            m.d.comb += [
                self.pay_q.enq.valid.eq(rresp.fire & ~rresp.bits.meta),
                self.pay_q.enq.bits.data.eq(rresp.bits.data),
                self.meta_q.enq.valid.eq(rresp.fire & rresp.bits.meta),
                self.meta_q.enq.bits.data.eq(rresp.bits.data),
            ]

        # Bookings and landings can fire in the same cycle; the
        # outstanding counters must fold both into one assignment or
        # the later statement would mask the earlier update.
        rreq_fire = request.fire
        p_book = rreq_fire & ~m_want
        m_book = rreq_fire & m_want
        if self.split_responses:
            p_land = Mux(self.mem_rresp.fire, self.mem_rresp.bits.beat_count,
                         0)
            m_land = Mux(self.mem_sresp.fire, 1, 0)
            p_deq = Mux(self.pay_q.deq.fire, self.pay_q.deq.bits.beat_count, 0)
        else:
            rresp = self.mem_rresp
            p_land = Mux(rresp.fire & ~rresp.bits.meta, 1, 0)
            m_land = Mux(rresp.fire & rresp.bits.meta, 1, 0)
            p_deq = Mux(self.pay_q.deq.fire, 1, 0)
        m.d.sync += [
            p_out.eq(p_out + Mux(p_book, p_burst, 0) - p_land),
            m_out.eq(m_out + Mux(m_book, m_burst, 0) - m_land),
            self.p_used.eq(self.p_used + Mux(p_book, p_burst, 0) - p_deq),
            self.m_used.eq(self.m_used + Mux(m_book, m_burst, 0) -
                           self.meta_q.deq.fire),
        ]
        with m.If(p_land != 0):
            m.d.sync += self.pay_beats.eq(self.pay_beats + p_land)
        with m.If(m_land):
            m.d.sync += self.meta_beats.eq(self.meta_beats + m_land)

        with m.If(self.mem_rreq.fire):
            m.d.sync += self.bursts.eq(self.bursts + 1)
        with m.If(rreq_fire):
            with m.If(m_want):
                m.d.sync += [
                    m_addr.eq(m_addr + (m_burst << self.lg_beat)),
                    m_rem.eq(m_rem - m_burst),
                ]
            with m.Else():
                m.d.sync += p_rem.eq(p_rem - p_burst)
                if self.striped:
                    with m.If(striped):
                        m.d.sync += phase.eq(phase + p_burst)
                    with m.If(use_secondary):
                        m.d.sync += secondary_addr.eq(
                            secondary_addr + (p_burst << self.lg_beat))
                    with m.Else():
                        m.d.sync += p_addr.eq(p_addr +
                                              (p_burst << self.lg_beat))
                else:
                    m.d.sync += p_addr.eq(p_addr + (p_burst << self.lg_beat))

        with m.If(self.run & ((p_rem | m_rem) != 0) & ~self.mem_rreq.fire):
            m.d.sync += self.port_idle.eq(self.port_idle + 1)

        #
        # Decoder: payload beats append to a byte-granular assembly
        # buffer; windows emit in order, at most n_outputs per cycle
        # (one per lane queue). Q8_0/Q5_0 windows are contiguous
        # 32/20-byte ranges consumed per emission; K-quant windows live
        # inside 192/128-byte super-block chunks that stay resident
        # until their 8th window emits, and the bonded lane group of
        # one chunk index decodes together from the resident chunks.
        # The meta stream mirrors this: one 2-byte entry per window
        # (Q8_0/Q5_0) or one 18/16-byte GGUF scale tail per chunk
        # (Q6_K/Q4_K), assembled in its own byte buffer and extracted
        # at the same pitch.
        #
        # Window state (pair, j, lane) names the next window to emit;
        # ``advance`` yields the following window's state.
        #
        pair, j, lane = self.pair, self.j, self.lane
        mhave = self.mhave

        def advance(pl, jj, ln, dist):
            """Expressions for the window following (pl, jj, ln).

            ``j``/``pair`` advance only when a cell's last lane window
            completes (lane t-1 of the cell's t lanes). ``dist`` is
            the exact-below-8 count of groups *after* the current one
            (saturated at 7 on entry to this bounded chain, while
            ``grp_rem`` stays exact); it decrements on a wrap, so the
            per-successor cell size is two small muxes instead of a
            group-index compare. The chain never needs distances
            beyond n+1 = 5, where the saturation is still exact.
            """
            t = Mux(dist == 0, tail_lanes, n)
            lnx = Cat(ln, Const(0, (lg_lane + 1) - lg_lane))
            cell_end = (lnx + 1) == t
            jj1 = jj + 1
            wrap = jj1 == self.nsegs
            step = cell_end & wrap
            return (
                pl + step,
                Mux(
                    cell_end,
                    Mux(wrap, Const(0, self.lg_act_segs),
                        jj1[:self.lg_act_segs]), jj),
                Mux(cell_end, Const(0, lg_lane), (ln + 1)[:lg_lane]),
                Mux(step, Mux(dist == 0, Const(0, 3), dist - 1), dist),
                step,
            )

        # states[0] is the next window; states[k] its k-th successor.
        # The emission of a cycle takes a prefix of this chain (up to
        # n windows, all targeting distinct lane queues), and the
        # state advance on fire[k] resumes at states[k + 1]. Each
        # level is materialised into plain signals before use: the
        # decoders and scale-field extractors build hundreds of
        # per-weight and per-byte selects driven by a state's j/lane,
        # and an unmaterialised Mux chain would be copied into every
        # one of them (the first n=4 elaboration measured 3,959 copies
        # of the cell-lanes mux and a 239k-line supply family).
        st_pair = [
            Signal(self.rows_bits, name=f'st_pair{k}') for k in range(n + 1)
        ]
        st_j = [
            Signal(self.lg_act_segs, name=f'st_j{k}') for k in range(n + 1)
        ]
        st_lane = [Signal(lg_lane, name=f'st_lane{k}') for k in range(n + 1)]
        st_dist = [Signal(3, name=f'st_dist{k}') for k in range(n + 1)]
        st_crossed = [Signal(name=f'st_crossed{k}') for k in range(n + 1)]
        m.d.comb += [
            st_pair[0].eq(pair),
            st_j[0].eq(j),
            st_lane[0].eq(lane),
            st_dist[0].eq(Mux(grp_rem > 7, 7, (grp_rem[:3] - 1)[:3])),
            st_crossed[0].eq(0),
        ]
        for k in range(1, n + 1):
            a_pl, a_j, a_lane, a_dist, a_step = advance(
                st_pair[k - 1], st_j[k - 1], st_lane[k - 1], st_dist[k - 1])
            m.d.comb += [
                st_pair[k].eq(a_pl),
                st_j[k].eq(a_j),
                st_lane[k].eq(a_lane),
                st_dist[k].eq(a_dist),
                st_crossed[k].eq(st_crossed[k - 1] | a_step),
            ]
        states = list(zip(st_pair, st_j, st_lane, st_dist))

        pay = self.pay_q.deq
        if not self.split_responses:
            data = pay.bits.data
        # Format selects: payload pitch (bytes per window for the
        # flat formats, per 8-window chunk for the K-quants), meta
        # entry pitch, and the chunked flag.
        is_q5 = self.fmt == Format.Q5_0
        is_q6 = self.fmt == Format.Q6_K
        is_q4 = self.fmt == Format.Q4_K
        is_kq = is_q6 | is_q4
        if self.split_responses:
            # The merger consumes this to decide whether pairs may
            # form at all; keep the assembler-side gate as defense in
            # depth for directly wired testbenches.
            m.d.comb += [
                self.pair_ok.eq(self.dual_retirement & ~is_q5),
                self.triple_ok.eq(self.triple_retirement
                                  & (self.fmt == Format.Q8_0)),
            ]
        pitch = Mux(
            is_q5, Q5_WIN_BYTES,
            Mux(is_q6, Q6_CHUNK_BYTES, Mux(is_q4, Q4_CHUNK_BYTES, WIN_BYTES)))
        epitch = Mux(is_q6, Q6_META_BYTES, Mux(is_q4, Q4_META_BYTES, 2))

        def lane_ready(ln):
            # Capacity check including the skid's in-flight push: a
            # push decided last cycle lands this cycle, and the queue
            # pointers only reflect it at the end of the cycle, so the
            # stored count must be padded by the pending push before
            # a new emission may reserve space for its own (next-cycle)
            # push. Dequeues only free space, so the check is
            # conservative-correct; a decided push therefore always
            # finds ``enq.ready`` asserted the following cycle.
            # ``ln`` may be a runtime lane signal (a queue select) or
            # a Python constant (the bonded group's lanes).
            pend = Const(0)
            for i in range(n):
                pend = pend | (self.s_v[i] & (self.s_lane[i] == ln))
            count = self.seg_q[0].count
            for l in range(1, n):
                count = Mux(ln == l, self.seg_q[l].count, count)
            return (count + pend) < self.mm_seg_fifo

        # Payload assembly: beats enter the unit file; whole units
        # (windows or chunks) become visible -- and emittable -- the
        # cycle their last byte lands. Intake is independent of
        # emission; the file's ``can_take`` bound plus the reader's
        # issue credits mean an accepted beat never drops bytes.
        pay_asm = self.pay_asm
        if self.split_responses:
            # Q5_0's 20-byte pitch can complete more than MAX_COMP
            # units across two 64-byte beats, so its delivery stays at
            # one beat per cycle; every other format's pitch (32 and
            # above) accepts the pair.
            m.d.comb += [
                pay_asm.beat.eq(pay.bits.data0),
                pay_asm.pitch.eq(pitch),
                pay_asm.clr.eq(self.start),
            ]
            if self.dual_retirement:
                m.d.comb += [
                    pay_asm.beat1.eq(pay.bits.data1),
                    pay_asm.beat1_in.eq(pay.valid & pay.bits.dual & ~is_q5),
                ]
            if self.triple_retirement:
                m.d.comb += [
                    pay_asm.beat2.eq(pay.bits.data2),
                    pay_asm.beat2_in.eq(pay.valid & pay.bits.triple
                                        & self.triple_ok),
                ]
            beat_in = pay.valid & pay_asm.can_take
            m.d.comb += [
                pay.ready.eq(beat_in),
                pay_asm.beat_in.eq(beat_in),
            ]
        else:
            m.d.comb += [
                pay_asm.beat.eq(data),
                pay_asm.pitch.eq(pitch),
                pay_asm.clr.eq(self.start),
            ]
            beat_in = pay.valid & pay_asm.can_take
            m.d.comb += [
                pay.ready.eq(beat_in),
                pay_asm.beat_in.eq(beat_in),
            ]

        # Scale entries are spaced by epitch bytes. Extraction uses the
        # resident count and consumption advances with the emissions.
        mq = self.meta_q.deq
        meta_in = mq.valid & self.run & (mhave <= self.meta_buf_bytes - bb)
        m.d.comb += mq.ready.eq(meta_in)
        if self.circular_meta:
            # Keep full beats in place and advance the read position in
            # halfwords. A beat-aligned write pointer follows the resident
            # data. Four words cover the buffer plus its initial phase.
            scale_words = [
                Signal(self.data_width, reset_less=True, name=f'scale_word{i}')
                for i in range(4)
            ]
            phase_bits = log2_int(bb // 2)
            scale_rd = Signal(phase_bits + 2)
            scale_wr = Signal(2)
            scale_bank = scale_rd[phase_bits:]
            scale_phase = scale_rd[:phase_bits]
            for i, word in enumerate(scale_words):
                with m.If(meta_in & (scale_wr == i)):
                    m.d.sync += word.eq(mq.bits.data)
            with m.If(meta_in):
                m.d.sync += scale_wr.eq(scale_wr + 1)

            # Eligibility uses the resident count, so a beat accepted this
            # cycle is first visible to scale extraction on the next edge.
            view_bytes = self.n_outputs * Q6_META_BYTES
            span_bytes = view_bytes + bb - 2
            scale_span = []
            for offset in range((span_bytes + bb - 1) // bb):
                width = min(bb, span_bytes - offset * bb) * 8
                word = Const(0, width)
                for i, data in enumerate(scale_words):
                    word = word | Mux(scale_bank == (i - offset) % 4,
                                      data[:width], 0)
                scale_span.append(word)
            mmrg = Signal(view_bytes * 8)
            m.d.comb += mmrg.eq(Cat(*scale_span) >> (scale_phase * 16))
        else:
            mdata = self.mdata
            mzext = Cat(mq.bits.data,
                        Const(0, self.meta_buf_bytes * 8 - self.data_width))
            mmrg = Signal(self.meta_buf_bytes * 8)
            m.d.comb += mmrg.eq(mdata | (
                Mux(meta_in, mzext, Const(0, self.meta_buf_bytes * 8)) <<
                (mhave * 8)))

        # Chunk-relative window index for the K-quants (j mod 8).
        jn = j[:3]
        jn7 = jn == 7

        # Window materialisation. Emission e reads the payload at
        # pitch*e (a flat window or a resident chunk) and its scale
        # entry at epitch*e.
        def q5_decode(seg):
            qh = seg[:32]
            w = []
            for i in range(WIN_BYTES):
                if i < 16:
                    nib = seg[32 + 8 * i:32 + 8 * i + 4]
                else:
                    nib = seg[32 + 8 * (i - 16) + 4:32 + 8 * (i - 16) + 8]
                w.append((Cat(nib, qh[i]).as_unsigned() + 240)[:8])
            return Cat(*w)

        def q6_decode(chunk, wj):
            """Q6_K window wj of one resident ql||qh chunk.

            Fixed-slice extraction (the phase B b3 open item: no wide
            dynamic shifters): the ql byte for weight l of window wj
            lives at 32*(2*h + q0) + l and the qh byte at 128 + 32*h
            + l, so every weight reads constant byte slices and the
            runtime selection collapses to the shared controls
            (2*h + q0) and h on 8:1 bit-muxes -- instead of a
            full-chunk barrel shift per weight.
            """
            h = wj[2]
            q = wj[:2]
            grp_sel = Cat(q[0], h)  # 2*h + q0, the ql byte group
            w = []
            for l in range(WIN_BYTES):
                qlb = Const(0, 8)
                for g in range(4):
                    off = 8 * (32 * g + l)
                    qlb = Mux(grp_sel == g, chunk[off:off + 8], qlb)
                nib = Mux(q[1], qlb[4:8], qlb[:4])
                qhb = Mux(h, chunk[8 * (160 + l):8 * (160 + l) + 8],
                          chunk[8 * (128 + l):8 * (128 + l) + 8])
                hi2 = Mux(
                    q == 3, qhb[6:8],
                    Mux(q == 2, qhb[4:6], Mux(q == 1, qhb[2:4], qhb[:2])))
                w.append((Cat(nib, hi2).as_unsigned() + 224)[:8])
            return Cat(*w)

        def q4_decode(chunk, wj):
            """Q4_K window wj of one resident nibble chunk.

            Fixed-slice extraction: weight l's byte lives at
            32*(wj >> 1) + l, so each weight reads four constant byte
            slices under the shared control (wj >> 1); wj[0] picks
            the nibble. No runtime byte address, no wide shifts.
            """
            g = wj >> 1
            w = []
            for l in range(WIN_BYTES):
                byte = Const(0, 8)
                for gg in range(4):
                    off = 8 * (32 * gg + l)
                    byte = Mux(g == gg, chunk[off:off + 8], byte)
                nib = Mux(wj[0], byte[4:8], byte[:4])
                w.append(Cat(nib, Const(0, 4)))
            return Cat(*w)

        n_jn = states[1][1][:3]

        # Scale fields: Q5_0/Q8_0 carry d alone; a Q6_K entry is the
        # block's sc8||d tail (sc pair at 8*(jn>>2) + 2*(jn&3)); a
        # Q4_K entry is d||dmin||k4-scales with get_scale_min_k4's
        # scattered 6-bit gathers.
        def q6_fields(entry, wj):
            """Q6_K sc8||d entry: fixed-slice scale extraction.

            The sc pair lives at byte 8*(wj >> 2) + 2*(wj & 3) of the
            16-byte sc8 region: one 2-way 8-byte half select by h and
            a 4-way byte select by q, shared by sc0/sc1; d is a fixed
            slice of the entry tail.
            """
            h = wj[2]
            q = wj[:2]
            sc_half = Mux(h, entry[64:128], entry[0:64])
            sc0 = Mux(
                q == 3, sc_half[48:56],
                Mux(q == 2, sc_half[32:40],
                    Mux(q == 1, sc_half[16:24], sc_half[:8])))
            sc1 = Mux(
                q == 3, sc_half[56:64],
                Mux(q == 2, sc_half[40:48],
                    Mux(q == 1, sc_half[24:32], sc_half[8:16])))
            return [
                ('d', entry[Q6_META_BYTES * 8 - 16:Q6_META_BYTES * 8]),
                ('dmin', Const(0, 16)),
                ('mi', Const(0, 6)),
                ('sc0', sc0),
                ('sc1', sc1),
            ]

        def q4_fields(entry, wj):
            """Q4_K d||dmin||k4-scales entry: fixed-slice extraction.

            get_scale_min_k4's gathers read byte lanes of the 8-byte
            k4 region (entry bytes 4..11) at fixed relationships to
            wj: 4+wj, 8+wj, and 4+(wj-4 mod 8). Each is an 8-way byte
            mux over constant slices sharing the wj-derived selector
            structure; no runtime byte address is computed.
            """

            def byte_lane(base, idx):
                expr = entry[8 * base:8 * base + 8]
                for i in range(1, 8):
                    expr = Mux(idx == i,
                               entry[8 * (base + i):8 * (base + i) + 8], expr)
                return expr

            b_jn = byte_lane(4, wj)
            b_jn4 = byte_lane(8, wj)
            b_jnm4 = byte_lane(4, wj[:3] - 4)
            lo = ~wj[2]
            return [
                ('d', entry[:16]),
                ('dmin', entry[16:32]),
                ('mi', Mux(lo, b_jn4[:6], Cat(b_jn4[4:8], b_jn[6:8]))),
                ('sc0',
                 Cat(Mux(lo, b_jn[:6], Cat(b_jn4[:4], b_jnm4[6:8])),
                     Const(0, 2))),
                ('sc1', Const(0, 8)),
            ]

        def fields(e):
            """Scale fields of emission ``e``'s entry, at the entry
            pitch of each format class (constant offsets per class)."""
            lo6 = Q6_META_BYTES * 8 * e
            lo4 = Q4_META_BYTES * 8 * e
            q6e = mmrg[lo6:lo6 + Q6_META_BYTES * 8]
            q4e = mmrg[lo4:lo4 + Q4_META_BYTES * 8]
            de = mmrg[16 * e:16 * e + 16]
            wj = states[e][1][:3]
            q6f = q6_fields(q6e, wj)
            q4f = q4_fields(q4e, wj)
            out = []
            for name, _ in q6f:
                q6v = dict(q6f)[name]
                q4v = dict(q4f)[name]
                flat = de if name == 'd' else Const(0, len(q6v))
                out.append((name, Mux(is_q6, q6v, Mux(is_q4, q4v, flat))))
            return dict(out)

        flds = [fields(e) for e in range(n)]
        rows_e = [(states[e][0] * n + states[e][2])[:self.rows_bits]
                  for e in range(n)]

        # Emission decision: a prefix of the window chain fires when
        # each window's payload unit, scale entry, and lane queue
        # (counting in-flight pushes) admit it, every emission targets
        # a distinct lane queue, and K-quant groups stay bonded: a
        # chunk group's windows all decode from their own resident
        # chunks at fixed bases, so the group's first window waits for
        # the whole group (``t`` lanes of its cell) and the cycle
        # emits exactly the group -- never a split. Flat windows emit
        # independently (a cell prefix may pair with the next cell's
        # head lanes). The decision is squelched on the (registered)
        # start pulse: the assembler/decoder state is being
        # re-initialised that cycle, so no stale window may book queue
        # space.
        avail = pay_asm.avail
        flat_meta_ok = [mhave >= 2 * (e + 1) for e in range(n)]
        # The bonded group covers t lanes; its chunks and entries must
        # all be resident and every target queue ready.
        t0 = Mux(grp_rem == 1, tail_lanes, n)
        kq_mneed = Mux(is_q6, (t0 << 4) + (t0 << 1), t0 << 4)
        kq_grp_ok = (avail >= t0) & (mhave >= kq_mneed)
        if self.mm_starvation_counters:
            m.d.comb += [
                self.payload_short.eq(avail < Mux(is_kq, t0, 1)),
                self.scale_short.eq(mhave < Mux(is_kq, kq_mneed, 2)),
                self.payload_eligible.eq(p_want),
                self.scale_eligible.eq(m_want),
            ]
        for k in range(n):
            kq_grp_ok = kq_grp_ok & Mux(t0 > k, lane_ready(k), Const(1))
        fire = []
        for e in range(n):
            base = (avail >= (e + 1)) & flat_meta_ok[e] \
                & lane_ready(states[e][2])
            if e == 0:
                f = ~self.start & base & Mux(is_kq, kq_grp_ok, Const(1))
            else:
                distinct = Const(1)
                for k in range(e):
                    distinct = distinct & (states[e][2] != states[k][2])
                f = (fire[e - 1] & distinct & base
                     & Mux(is_kq,
                           Const(e) < t0, Const(1)))
            fire.append(f)

        emits_now = Signal(range(n + 1))
        m.d.comb += emits_now.eq(sum(fire))

        # The decided record is captured into the skid registers --
        # the raw slots (the decode reads the registered copy, so the
        # queue's LUTRAM read, the byte-phase merge, the completion
        # slices and the bank selects all end at these register
        # D-inputs), the chunk-relative window indices, the extracted
        # scale fields, and the row/lane targets. The push itself
        # fires from these registers next cycle.
        m.d.sync += self.s_rotation.eq(pay_asm.rotation)
        for e in range(n):
            m.d.sync += [
                self.s_v[e].eq(fire[e]),
                self.s_lane[e].eq(states[e][2]),
                self.s_row[e].eq(rows_e[e]),
                self.s_slot[e].eq(pay_asm.slot[e]),
                self.s_jn[e].eq(states[e][1][:3]),
            ]
            for name in self.s_f[e]:
                m.d.sync += self.s_f[e][name].eq(flds[e][name])

        # Deferred decode: the record's weight words are produced the
        # cycle after the decision, from the registered slots -- a
        # real register boundary between assembly and decoding (the
        # Queue's flow=False deq is a combinational LUTRAM read, so
        # without this cut a payload path ran FIFO pointer -> LUTRAM
        # -> byte-phase merge -> unit selection -> quantized decode ->
        # lane selection -> segment FIFO in one cycle).
        def decode_unit(slot, jn_s):
            return Mux(
                is_q5, q5_decode(slot[:Q5_WIN_BYTES * 8]),
                Mux(
                    is_q6, q6_decode(slot, jn_s),
                    Mux(is_q4, q4_decode(slot[:Q4_CHUNK_BYTES * 8], jn_s),
                        slot[:WIN_BYTES * 8])))

        # Decode once per bank column. A Python expression is inlined at
        # every use by Amaranth; feeding it into n lane muxes otherwise
        # elaborates n copies of each weight decoder (quadratic growth).
        bank_w = [
            Signal(self.seg_len * 8, name=f'decoded_bank{e}') for e in range(n)
        ]
        for e in range(n):
            # Bonded K-quant emissions share jn; flat decodes do not
            # use it. Carry the assembler bank rotation across the skid.
            m.d.comb += bank_w[e].eq(decode_unit(self.s_slot[e], self.s_jn[0]))

        # Distinct lanes within a decided emission set mean each queue
        # takes at most one push per cycle. Each push always finds
        # space (the decision reserved it with the pending-push-padded
        # count), so no skid stall path exists.
        for l, q in enumerate(self.seg_q):
            hits = [self.s_v[e] & (self.s_lane[e] == l) for e in range(n)]
            enq_valid = Const(0)
            wsel = Const(0, self.seg_len * 8)
            rowsel = Const(0, self.rows_bits)
            fsel = {
                name: Const(0, len(sig))
                for name, sig in self.s_f[0].items()
            }
            # Distinct destination lanes make these selects one-hot.
            for e, hit in enumerate(hits):
                enq_valid = enq_valid | hit
                rowsel = rowsel | Mux(hit, self.s_row[e], 0)
                for name in fsel:
                    fsel[name] = fsel[name] | Mux(hit, self.s_f[e][name], 0)
            # Combine rotation and lane selection in the control path so
            # each lane selects directly from the decoded bank words.
            for bank, words in enumerate(bank_w):
                select = Const(0)
                for e, hit in enumerate(hits):
                    select = select | (hit &
                                       (self.s_rotation == (bank - e) % n))
                wsel = wsel | Mux(select, words, 0)
            m.d.comb += q.enq.valid.eq(enq_valid)
            m.d.comb += [
                q.enq.bits.w.eq(wsel),
                q.enq.bits.d.eq(fsel['d']),
                q.enq.bits.dmin.eq(fsel['dmin']),
                q.enq.bits.mi.eq(fsel['mi']),
                q.enq.bits.sc0.eq(fsel['sc0']),
                q.enq.bits.sc1.eq(fsel['sc1']),
                q.enq.bits.row.eq(rowsel),
            ]

        # Unit release: flat formats consume one unit (window) per
        # emission; K-quants hold each chunk resident until its 8th
        # window emits (the bonded group at j == 7 releases one chunk
        # and one entry per emitted window; an odd tail releases its
        # own chunks only). The release is gated on the emission
        # actually firing: jn7 alone must never free a chunk or shift
        # its entry out while backpressure holds the window, or the
        # 8th window's data is discarded live (fire[e] implies
        # fire[e-1], so the group releases exactly when its windows
        # emit).
        m.d.comb += pay_asm.consume.eq(
            Mux(is_kq, Mux(jn7 & fire[0], emits_now, 0), emits_now))
        # These byte counts are even for every supported scale entry.
        flat_shift = epitch * emits_now
        kq_shift = epitch * Mux(jn7 & fire[0], emits_now, 0)
        meta_shift = Mux(is_kq, kq_shift, flat_shift)
        if self.circular_meta:
            m.d.sync += scale_rd.eq(scale_rd + meta_shift[1:])
        else:
            # Keep the flat buffer zero above the resident byte count.
            flat_vals = tuple(2 * e for e in range(n + 1))
            kq_vals = tuple(
                sorted({
                    p * e
                    for p in (Q6_META_BYTES, Q4_META_BYTES)
                    for e in range(n + 1)
                }))
            flat_next = Const(0, self.meta_buf_bytes * 8)
            for v in flat_vals:
                flat_next = Mux(flat_shift == v, mmrg >> (8 * v), flat_next)
            kq_next = Const(0, self.meta_buf_bytes * 8)
            for v in kq_vals:
                kq_next = Mux(kq_shift == v, mmrg >> (8 * v), kq_next)
            mdata_next = Mux(is_kq, kq_next, flat_next)
            m.d.sync += mdata.eq(mdata_next)
        m.d.sync += mhave.eq(mhave + Mux(meta_in, bb, 0) - meta_shift)

        # Window state advance on emission: resume at the successor of
        # the last fired emission. fire[e] implies fire[e-1], so plain
        # sequential Ifs give the highest fired e the last assignment
        # (If/Elif priority); ascending order is required. The group
        # counter decrements on a group crossing. A fired prefix has
        # distinct lanes, so it cannot cross two groups (that would
        # repeat lane zero). Keep a floor of one at the walk's end.
        for e in range(n):
            with m.If(fire[e]):
                m.d.sync += [
                    pair.eq(states[e + 1][0]),
                    j.eq(states[e + 1][1]),
                    lane.eq(states[e + 1][2]),
                    grp_rem.eq(
                        Mux(grp_rem == 1, 1, grp_rem - st_crossed[e + 1])),
                ]

        #
        # Start: latch cursors from the configuration inputs on the
        # accepting edge and reset all decode state. ``start`` is a
        # registered pulse from the owning unit (StreamGemv), so this
        # re-initialisation fans out from a register boundary, not
        # from the command-acceptance cone; the skid is squelched the
        # same edge so no stale record can book queue space.
        #
        with m.If(self.start):
            m.d.sync += [
                p_addr.eq(self.p_base),
                m_addr.eq(self.m_base),
                p_rem.eq(self.p_total),
                m_rem.eq(self.m_total),
                p_out.eq(0),
                m_out.eq(0),
                self.p_used.eq(0),
                self.m_used.eq(0),
                pair.eq(0),
                j.eq(0),
                lane.eq(0),
                grp_rem.eq(groups_n),
                tail_lanes.eq(tail_n),
                mhave.eq(0),
            ]
            if self.circular_meta:
                m.d.sync += [scale_rd.eq(0), scale_wr.eq(0)]
            else:
                m.d.sync += mdata.eq(0)
            # Redundant with the ~start squelch in fire[0], kept as the
            # original's explicit same-edge reset of the skid.
            for e in range(n):
                m.d.sync += self.s_v[e].eq(0)

        m.d.comb += self.quiescent.eq((p_rem == 0) & (m_rem == 0)
                                      & (p_out == 0) & (m_out == 0))

        return m


class StreamGemv(HasTRoomParams, Elaboratable):
    """Streaming GEMV unit: SG1 supply + the proven MAC array.

    Accepts one ``StreamMatmulDesc`` at a time, launches row-pair slots
    with joined operands, output credits, ACT read-ahead and a
    ``MmSegment`` stream, and completes after the last record is
    dequeued, the reader is quiescent, and the queues are flushed of
    the meta tail pad. All four canonical formats are supported:
    Q8_0 (control validation), Q5_0 (flat GGUF byte decode), and the
    Q6_K/Q4_K super-blocks (chunked decode with GGUF scale tails in
    the meta stream; k must be a multiple of 256). Anything else, a
    ``k`` that is not a whole number of windows (or super-blocks) or
    does not fit ACT RAM, a payload extent that is not a whole number
    of beats, zero rows, unaligned region bases, or a payload/meta
    extent outside the device memory window complete immediately with
    ``BAD_SHAPE``/``BAD_ALIGN``/``BAD_RANGE`` without touching the port.
    """

    def __init__(self,
                 params,
                 *,
                 mem_base=0,
                 mem_size=None,
                 read_topology=None):
        super().__init__(params)

        # Device memory window bounding every DRAM byte the reader
        # issues (the DMA discipline). ``None`` admits the whole
        # address space; a descriptor whose payload or meta extent
        # leaves the window completes immediately with BAD_RANGE
        # without touching the port.
        if mem_size is None:
            mem_size = 1 << self.addr_width
        if mem_base < 0 or mem_size <= 0:
            raise ValueError('memory window must be non-empty')
        if mem_base + mem_size > 1 << self.addr_width:
            raise ValueError(
                f'memory window [{mem_base:#x}, {mem_base + mem_size:#x}) '
                f'exceeds the {self.addr_width}-bit address space')
        if read_topology is not None:
            read_topology.validate(self.addr_width, self.beat_bytes)
        self.mem_base = mem_base
        self.mem_size = mem_size

        self.sink = Decoupled(StreamMatmulDesc, self.params)
        self.segs = Decoupled(MmSegment, self.params)
        self.actram = ActRAM(self.params)
        self.read_topology = read_topology
        self.split_responses = read_topology is not None
        self.striped = self.split_responses and read_topology.striped
        self.supply = StreamSupply(self.params, read_topology=read_topology)
        self.mac = MacArray(self.params)

        self.act_wr = self.actram.wr
        self.mem_rreq = self.supply.mem_rreq
        self.mem_rresp = self.supply.mem_rresp
        if self.split_responses:
            self.mem_sresp = self.supply.mem_sresp
            self.pair_ok = self.supply.pair_ok
            self.triple_ok = self.supply.triple_ok

        self.done = Signal()
        self.status = Signal(Status)
        self.active = Signal()
        self.start_ok = Signal()
        self.act_fill_busy = Signal()
        self.act_commit_pending = Signal()

        # Launch-side observation.
        self.slots = Signal(48)
        self.bubbles = Signal(48)
        self.starve_cycles = Signal(48)
        self.credit_stalls = Signal(48)
        self.useful_macs = Signal(48)
        self.run_cycles = Signal(48)
        self.drain_cycles = Signal(48)
        self.feed_beats = Signal(48)
        if self.mm_starvation_counters:
            self.diag = {
                name: Signal(48, name=f'mm_diag_{name}')
                for name in MM_STARVATION_COUNTERS
            }
            self.dma_beat = Signal()
            self.payload_request = Signal(
                range(len(read_topology.ports) +
                      1) if self.split_responses else 1)
            self.scale_request = Signal()

        # Reader-side observation (phase B accounting).
        self.pay_beats = Signal(48)
        self.meta_beats = Signal(48)
        self.meta_pad_bytes = Signal(48)
        self.bursts = Signal(48)
        self.port_idle = Signal(48)

        # Launch-join cursor state (observation).
        self.join_pair = Signal(self.rows_bits)
        self.join_seg = Signal(self.lg_act_segs)
        self.join_rd_idx = Signal(self.lg_act_segs + 1)
        self.join_act_cnt = Signal(range(self.mm_act_fifo + 1))

    def elaborate(self, platform):
        m = Module()

        m.submodules.actram = self.actram
        m.submodules.supply = self.supply
        m.submodules.mac = self.mac

        out_q = m.submodules.out_q = Queue(self.mm_out_depth, MmSegment,
                                           self.params)
        m.d.comb += [
            self.mac.segs.connect(out_q.enq),
            out_q.deq.connect(self.segs),
        ]

        n = self.n_outputs

        cur_fmt = Signal(Format)
        cur_k = Signal(self.lg_act_k + 1)
        cur_rows = Signal(self.rows_bits)
        cur_p_base = Signal(self.addr_width)
        cur_m_base = Signal(self.addr_width)

        nsegs = cur_k >> 5
        # Group count at this unit's width: ceil(rows / n_outputs)
        # (pairs at n=2, quads at n=4).
        pairs = rtl_stream_groups(cur_rows, n)
        # Extent arithmetic duplicated per format: every pitch and unit
        # shift is a Python constant here (shift+add; no runtime-pitch
        # operand feeding a multiplier cascade) and the format only
        # selects between completed products.
        ext_w = self.rows_bits + (self.lg_act_k + 1) + 8

        def _extents(rows, k, tag):
            u_flat = k >> 5
            u_kq = k >> 8
            p_sel, m_sel = {}, {}
            for fmt, units, pitch, epitch in ((Format.Q8_0, u_flat, WIN_BYTES,
                                               2), (Format.Q5_0, u_flat,
                                                    Q5_WIN_BYTES, 2),
                                              (Format.Q6_K, u_kq,
                                               Q6_CHUNK_BYTES, Q6_META_BYTES),
                                              (Format.Q4_K, u_kq,
                                               Q4_CHUNK_BYTES, Q4_META_BYTES)):
                p_sel[fmt] = Signal(ext_w, name=f'{tag}_p_{fmt.name}')
                m_sel[fmt] = Signal(ext_w, name=f'{tag}_m_{fmt.name}')
                m.d.comb += [
                    p_sel[fmt].eq(rows * units * pitch),
                    m_sel[fmt].eq(rows * units * epitch),
                ]
            return p_sel, m_sel

        def _pick(fmt_sig, sel):
            out = sel[Format.Q8_0]
            for fmt in (Format.Q5_0, Format.Q6_K, Format.Q4_K):
                out = Mux(fmt_sig == fmt, sel[fmt], out)
            return out

        # Pipelined accept, three registered stages. Core presents the
        # descriptor as a combinational fanout of its command-queue
        # head and holds it stable until fire, so IDLE can latch the
        # offer into o_* registers without firing. EXT computes the
        # per-format extent products from those registers into
        # p_bytes_r/m_bytes_r/p_total_r/m_total_r: the design's only
        # extent multiply, with register endpoints on both sides. CHK
        # validates from registered values alone and fires -- accept
        # latches cur_*, sets the registered start_r pulse and enters
        # RUN; the supply initializes from registers during RUN's
        # first cycle. Reject pulses done with the BAD_* status at the
        # fire cycle exactly as the single-cycle accept did, two
        # cycles later. No path from Core's command registers crosses
        # a multiplier, an extent compare, or the supply's decode
        # state in the same cycle: the acceptance cone ends at
        # register D-inputs (cur_*, start_r, the FSM state), and the
        # supply's re-initialization fans out from start_r.
        p_bytes_r = Signal(ext_w)
        m_bytes_r = Signal(ext_w)
        p_total_r = Signal(ext_w)
        m_total_r = Signal(ext_w)

        offer = self.sink.bits
        o_fmt = Signal(Format)
        o_k = Signal(self.lg_act_k + 1)
        o_rows = Signal(self.rows_bits)
        o_p_base = Signal(self.addr_width)
        o_m_base = Signal(self.addr_width)
        o_secondary_base = Signal(self.addr_width)
        o_primary_beats = Signal(5)
        cur_secondary_base = Signal(self.addr_width)
        cur_primary_beats = Signal(5)
        primary_total = Signal(ext_w)

        off_p_sel, off_m_sel = _extents(o_rows, o_k, 'off')
        off_p_bytes = _pick(o_fmt, off_p_sel)
        off_m_bytes = _pick(o_fmt, off_m_sel)

        off_kq = ((o_fmt == Format.Q6_K) | (o_fmt == Format.Q4_K))

        fmt_ok = ((o_fmt == Format.Q8_0)
                  & Const(Format.Q8_0 in self.fmt_pos, 1)) | (
                      (o_fmt == Format.Q5_0)
                      & Const(Format.Q5_0 in self.fmt_pos, 1)) | (
                          (o_fmt == Format.Q6_K)
                          & Const(Format.Q6_K in self.fmt_pos, 1)) | (
                              (o_fmt == Format.Q4_K)
                              & Const(Format.Q4_K in self.fmt_pos, 1))

        shape_ok = (fmt_ok
                    & (o_k >= self.seg_len)
                    & (o_k <= self.act_max_k)
                    & ~o_k[:5].any()
                    & Mux(off_kq, ~o_k[5:8].any(), Const(1))
                    & (o_rows != 0)
                    & ((p_bytes_r & Const(self.beat_bytes - 1)) == 0))
        align_ok = ~(o_p_base[:self.lg_beat].any()
                     | o_m_base[:self.lg_beat].any())
        # Whole-region window check with guard bits beyond the address
        # width (the DMA loader's discipline), so an extent that would
        # wrap is rejected rather than issued. ``m_total_r`` already
        # includes the meta tail pad.
        ext_bits = self.addr_width + self.rows_bits + 2
        p_span = Signal(ext_bits)
        m_span = Signal(ext_bits)
        mem_end = Signal(ext_bits)
        p_end = o_p_base + p_span
        if self.read_topology is None:
            p_in_range = (o_p_base >= self.mem_base) & (p_end <= mem_end)
            m_in_range = (o_m_base >= self.mem_base) & (o_m_base + m_span
                                                        <= mem_end)
        else:
            primary, *secondary = self.read_topology.payload_regions
            p_in_range = primary.contains(o_p_base, p_end)
            m_in_range = self.read_topology.scales.contains(
                o_m_base, o_m_base + m_span)
            if self.striped:
                secondary, = secondary
                p_in_range |= secondary.contains(o_p_base, p_end)
                secondary_span = (p_total_r - primary_total) << self.lg_beat
                stripe_range = (
                    primary.contains(
                        o_p_base, o_p_base + (primary_total << self.lg_beat))
                    & secondary.contains(o_secondary_base,
                                         o_secondary_base + secondary_span))
                p_in_range = Mux(o_secondary_base != 0, stripe_range,
                                 p_in_range)
                align_ok &= ~o_secondary_base[:self.lg_beat].any()
        if not self.striped:
            shape_ok &= o_secondary_base == 0
        m.d.comb += [
            p_span.eq(p_total_r << self.lg_beat),
            m_span.eq(m_total_r << self.lg_beat),
            mem_end.eq(self.mem_base + self.mem_size),
        ]
        range_ok = p_in_range & m_in_range
        ok = shape_ok & align_ok & range_ok

        pair = self.join_pair
        seg = self.join_seg
        rd_idx = self.join_rd_idx
        rd_issue = Signal()
        # With more segments than queue slots, the read cursor can only
        # cross one row group ahead of the consumer. Stop exactly at
        # the end of the last group and keep short attention unchanged.
        wrap_act = (Const(self.sg_act_wrap and self.mm_act_fifo > 1) &
                    (cur_fmt == Format.Q8_0)
                    & (cur_k >= 1024) & (nsegs > self.mm_act_fifo))
        wrap_read = wrap_act & (rd_idx == nsegs) & (pair + 1 < pairs)

        rd_pending = Signal()
        m.d.sync += rd_pending.eq(rd_issue)
        act_cnt = self.join_act_cnt
        act_e = [
            ActWord(self.params, name=f'act_e{i}')
            for i in range(self.mm_act_fifo)
        ]
        resv = Signal(range(self.mm_out_depth + 1))

        start_r = Signal()
        # A PV command may start its reader while the preceding ACT
        # fill is active. Keep both commit and ACT reads behind that
        # fill, while the usual weight queues absorb early returns.
        defer_act = Signal()
        commit_act = Signal()
        m.d.comb += [
            commit_act.eq(self.act_commit_pending & ~self.act_fill_busy),
            self.actram.commit.eq((self.start_ok & ~defer_act) | commit_act),
        ]
        with m.If(commit_act):
            m.d.sync += self.act_commit_pending.eq(0)

        rd_word = ActWord(self.params)
        m.d.comb += [
            rd_word.quants.eq(self.actram.rd_quants),
            rd_word.s_act.eq(self.actram.rd_s_act),
            rd_word.sum_a.eq(self.actram.rd_sum_a),
        ]

        if self.mm_starvation_counters:
            seen_issue = Signal()
        with m.FSM() as fsm:
            with m.State('IDLE'):
                m.d.comb += self.sink.ready.eq(0)
                with m.If(self.sink.valid):
                    # Bits are stable until fire (Core's queue head is
                    # immutable across the offer), so latching now and
                    # firing from CHK observes the same descriptor.
                    m.d.sync += [
                        o_fmt.eq(offer.fmt),
                        o_k.eq(offer.k),
                        o_rows.eq(offer.rows),
                        o_p_base.eq(offer.p_base),
                        o_m_base.eq(offer.m_base),
                        o_secondary_base.eq(offer.ps_base),
                        o_primary_beats.eq(offer.pl_beats),
                    ]
                    m.next = 'EXT'

            with m.State('EXT'):
                m.d.sync += [
                    p_bytes_r.eq(off_p_bytes),
                    m_bytes_r.eq(off_m_bytes),
                    p_total_r.eq(off_p_bytes >> self.lg_beat),
                    m_total_r.eq((off_m_bytes +
                                  (self.beat_bytes - 1)) >> self.lg_beat),
                ]
                m.next = 'STRIPE_EXT' if self.striped else 'CHK'

            if self.striped:
                with m.State('STRIPE_EXT'):
                    tail = p_total_r[:5]
                    primary_beats = Mux(o_primary_beats == 0,
                                        SG1_DEFAULT_PRIMARY_BEATS,
                                        o_primary_beats)
                    m.d.sync += primary_total.eq(
                        (p_total_r >> 5) * primary_beats +
                        Mux(tail < primary_beats, tail, primary_beats))
                    m.next = 'CHK'

            with m.State('CHK'):
                # Valid may have dropped after IDLE (epi.ready/halt
                # gate it in Core); the bits cannot change, so wait
                # here with ready asserted until the offer returns.
                m.d.comb += self.sink.ready.eq(1)
                # ``start_ok`` is the accept-cycle pulse Core consumes
                # (epilogue start and the reader error-tracker clear):
                # it fans out to register enables only, so keeping it
                # combinational here does not reintroduce the
                # acceptance cone into the datapath.
                m.d.comb += self.start_ok.eq(self.sink.fire & ok)
                m.d.comb += defer_act.eq(offer.early_weights
                                         & self.act_fill_busy)

                with m.If(self.sink.fire & ok):
                    m.d.sync += [
                        cur_fmt.eq(o_fmt),
                        self.act_commit_pending.eq(defer_act),
                        cur_k.eq(o_k),
                        cur_rows.eq(o_rows),
                        cur_p_base.eq(o_p_base),
                        cur_m_base.eq(o_m_base),
                        cur_secondary_base.eq(o_secondary_base),
                        cur_primary_beats.eq(
                            Mux(o_primary_beats == 0,
                                SG1_DEFAULT_PRIMARY_BEATS, o_primary_beats)),
                        # The supply's initialization pulse is
                        # *registered*: the entire command-acceptance
                        # cone (epilogue availability, fire, checks)
                        # ends at these D-inputs, and the supply
                        # re-initializes from registers next cycle
                        # instead of from combinational control that
                        # reaches through the assembler into the
                        # segment queues' write data.
                        start_r.eq(1),
                    ]
                    m.d.sync += [
                        pair.eq(0),
                        seg.eq(0),
                        rd_idx.eq(0),
                        act_cnt.eq(0),
                        resv.eq(0),
                        self.meta_pad_bytes.eq((m_total_r << self.lg_beat) -
                                               m_bytes_r),
                    ]
                    if self.mm_starvation_counters:
                        m.d.sync += seen_issue.eq(0)
                    m.next = 'RUN'
                with m.Elif(self.sink.fire):
                    m.d.comb += [
                        self.done.eq(1),
                        self.status.eq(
                            Mux(
                                shape_ok,
                                Mux(align_ok, Status.BAD_RANGE,
                                    Status.BAD_ALIGN), Status.BAD_SHAPE)),
                    ]
                    m.next = 'IDLE'

            with m.State('RUN'):
                m.d.sync += start_r.eq(0)
                unmasked = [(pair * n + p) < cur_rows for p in range(n)]
                ready = [
                    ~unmasked[p] | self.supply.segs[p].valid for p in range(n)
                ]
                act_ok = act_cnt != 0
                operands_ok = act_ok & Cat(ready).all()
                credit_ok = resv < self.mm_out_depth
                seg_fire = operands_ok & credit_ok

                head = act_e[0]
                meta = self.mac.meta_in
                m.d.comb += [
                    meta.valid.eq(seg_fire),
                    meta.fmt.eq(cur_fmt),
                    meta.row_mask.eq(Cat(unmasked)),
                    meta.s_act.eq(Mux(seg_fire, head.s_act, 0)),
                    meta.sum_a.eq(Mux(seg_fire, head.sum_a, 0)),
                    meta.seg_k.eq(seg),
                    meta.row_last.eq(seg + 1 == nsegs),
                    self.mac.a_in.eq(Mux(seg_fire, head.quants, 0)),
                ]
                for p in range(n):
                    feed_p = seg_fire & unmasked[p]
                    whead = self.supply.segs[p].bits
                    lane_row = (pair * n + p)[:self.rows_bits]
                    m.d.comb += [
                        self.mac.w_in[p].eq(Mux(feed_p, whead.w, 0)),
                        getattr(meta, f'row{p}').eq(
                            Mux(unmasked[p], whead.row, lane_row)),
                        getattr(meta, f'd{p}').eq(Mux(feed_p, whead.d, 0)),
                        getattr(meta,
                                f'dmin{p}').eq(Mux(feed_p, whead.dmin, 0)),
                        getattr(meta, f'mi{p}').eq(Mux(feed_p, whead.mi, 0)),
                        getattr(meta, f'sc{p}a').eq(Mux(feed_p, whead.sc0, 0)),
                        getattr(meta, f'sc{p}b').eq(Mux(feed_p, whead.sc1, 0)),
                        self.supply.seg_q[p].deq.ready.eq(feed_p),
                    ]

                m.d.comb += rd_issue.eq(~self.act_commit_pending
                                        & ((rd_idx != nsegs) | wrap_read)
                                        & (act_cnt +
                                           rd_pending < self.mm_act_fifo))
                push = rd_pending
                m.d.comb += self.actram.rd_seg.eq(
                    Mux(wrap_read, 0, rd_idx[:self.lg_act_segs]))
                with m.If(rd_issue):
                    m.d.sync += rd_idx.eq(Mux(wrap_read, 1, rd_idx + 1))
                with m.If(seg_fire & ~push):
                    m.d.sync += act_cnt.eq(act_cnt - 1)
                with m.Elif(push & ~seg_fire):
                    m.d.sync += act_cnt.eq(act_cnt + 1)
                with m.If(seg_fire):
                    for i in range(self.mm_act_fifo - 1):
                        m.d.sync += act_e[i].eq(act_e[i + 1])
                with m.If(push):
                    tgt = act_cnt - seg_fire
                    for i in range(self.mm_act_fifo):
                        with m.If(tgt == i):
                            m.d.sync += act_e[i].eq(rd_word)

                with m.If(seg_fire & ~self.segs.fire):
                    m.d.sync += resv.eq(resv + 1)
                with m.Elif(self.segs.fire & ~seg_fire):
                    m.d.sync += resv.eq(resv - 1)

                n_active = sum(unmasked)
                if self.mm_starvation_counters:
                    lane_short = ~Cat(ready).all()
                    supply = self.supply
                    # These are decoder-head observations, which can refer
                    # to a later segment than the consumer on an empty lane.
                    # The supply clears its cursors and assemblers on
                    # start_r, so observations from that edge are unassigned.
                    payload_short = lane_short & ~start_r & supply.payload_short
                    scale_short = lane_short & ~start_r & supply.scale_short
                    mask = Cat(~act_ok, lane_short, payload_short, scale_short)
                    if self.split_responses:
                        presp = self.mem_rresp
                        payload_beat = Mux(presp.fire, presp.bits.beat_count,
                                           0)
                        scale_beat = self.mem_sresp.fire
                    else:
                        resp = self.mem_rresp
                        payload_beat = Mux(resp.fire & ~resp.bits.meta, 1, 0)
                        scale_beat = resp.fire & resp.bits.meta
                    events = {
                        'commands':
                        start_r,
                        'run':
                        Const(1),
                        'issue':
                        seg_fire,
                        'credit':
                        operands_ok & ~credit_ok,
                        'first_issue_wait':
                        ~seen_issue & ~seg_fire,
                        'payload_beats':
                        payload_beat,
                        'scale_beats':
                        scale_beat,
                        'payload_requests':
                        self.payload_request,
                        'scale_requests':
                        self.scale_request,
                        'payload_eligible_short':
                        payload_short & supply.payload_eligible,
                        'scale_eligible_short':
                        scale_short & supply.scale_eligible,
                        'payload_outstanding_gap':
                        payload_short & (supply.p_out != 0) & ~payload_beat,
                        'scale_outstanding_gap':
                        scale_short & (supply.m_out != 0) & ~scale_beat,
                        'dma_overlap':
                        self.dma_beat,
                    }
                    events.update({
                        f'mask_{value:02x}': mask == value
                        for value in MM_STARVATION_MASKS
                    })
                    with m.If(seg_fire):
                        m.d.sync += seen_issue.eq(1)
                    # Shape classes for Qwen3's 128-element heads and
                    # 64-slot PV windows. Unrelated GEMVs may alias them.
                    qk = (cur_fmt == Format.Q8_0) & (cur_k == 128)
                    pv = (cur_fmt == Format.Q8_0) & (cur_k == 64) & (cur_rows
                                                                     == 128)
                    for kind, selected in zip(MM_STARVATION_CLASSES,
                                              (~(qk | pv), qk, pv)):
                        with m.If(selected):
                            for field, event in events.items():
                                counter = self.diag[f'{kind}_{field}']
                                with m.If(event):
                                    m.d.sync += counter.eq(counter + event)
                            with m.If(seg_fire):
                                macs = self.diag[f'{kind}_macs']
                                m.d.sync += macs.eq(macs +
                                                    self.seg_len * n_active)
                m.d.sync += self.run_cycles.eq(self.run_cycles + 1)
                with m.If(seg_fire):
                    m.d.sync += [
                        self.slots.eq(self.slots + 1),
                        self.useful_macs.eq(self.useful_macs +
                                            self.seg_len * n_active),
                    ]
                with m.Else():
                    m.d.sync += self.bubbles.eq(self.bubbles + 1)
                    with m.If(~operands_ok):
                        m.d.sync += self.starve_cycles.eq(self.starve_cycles +
                                                          1)
                    with m.Elif(~credit_ok):
                        m.d.sync += self.credit_stalls.eq(self.credit_stalls +
                                                          1)

                with m.If(seg_fire):
                    with m.If(seg + 1 == nsegs):
                        m.d.sync += seg.eq(0)
                        with m.If(pair + 1 == pairs):
                            m.next = 'DRAIN'
                        with m.Else():
                            m.d.sync += pair.eq(pair + 1)
                        with m.If(~wrap_act):
                            m.d.sync += rd_idx.eq(0)
                    with m.Else():
                        m.d.sync += seg.eq(seg + 1)

            with m.State('DRAIN'):
                m.d.sync += self.drain_cycles.eq(self.drain_cycles + 1)
                with m.If(self.segs.fire):
                    m.d.sync += resv.eq(resv - 1)
                with m.If((resv == 0) & self.supply.quiescent):
                    m.next = 'FLUSH'

            with m.State('FLUSH'):
                # Drop the meta tail pad so it cannot leak into the
                # next tile; the reader is quiescent, so nothing else
                # arrives.
                m.d.comb += [
                    self.supply.pay_q.flush.eq(1),
                    self.supply.meta_q.flush.eq(1),
                    self.done.eq(1),
                    self.status.eq(Status.OK),
                ]
                m.next = 'IDLE'

        m.d.comb += [
            # Exactly the RUN window: the accept pipeline (EXT/CHK)
            # must not enable the walkers, matching the supply's
            # cycle view of the two-state FSM this replaces.
            self.supply.run.eq(fsm.ongoing('RUN')),
            # Registered one-cycle init pulse: high during RUN's
            # first cycle only (CHK sets it, RUN clears it). The
            # supply's start-edge samples read the cur_* registers --
            # latched at the CHK->RUN edge -- so no combinational
            # path from the offer or the fire decision reaches the
            # supply.
            self.supply.start.eq(start_r),
            # start_ok stays the accept-cycle pulse for Core (epilogue
            # start, error-tracker clear); the supply's
            # re-initialization fans out from the registered start_r.
            self.supply.fmt.eq(cur_fmt),
            self.supply.rows.eq(cur_rows),
            self.supply.nsegs.eq(nsegs),
            self.supply.p_base.eq(cur_p_base),
            self.supply.m_base.eq(cur_m_base),
            self.supply.secondary_base.eq(cur_secondary_base),
            self.supply.primary_beats.eq(cur_primary_beats),
            self.supply.p_total.eq(p_total_r),
            self.supply.m_total.eq(m_total_r),
            self.actram.pin.eq(~fsm.ongoing('IDLE')),
            self.active.eq(~fsm.ongoing('IDLE')),
            self.feed_beats.eq(self.supply.pay_beats + self.supply.meta_beats),
            self.pay_beats.eq(self.supply.pay_beats),
            self.meta_beats.eq(self.supply.meta_beats),
            self.bursts.eq(self.supply.bursts),
            self.port_idle.eq(self.supply.port_idle),
        ]

        return m


class StreamAxiReader(HasTRoomParams, Elaboratable):
    """Phase-C integration adapter: tagged bursts onto one AXI read port.

    The supply issues beat-aligned requests of 1..256 beats; AXI forbids
    an INCR burst crossing a 4 KiB boundary, so a request whose extent
    crosses one is latched and issued as consecutive page-clipped
    sub-bursts, each carrying the request's region tag. Requests that
    fit a page issue straight from the request stream in the same
    cycle, preserving the phase-B one-request-per-cycle issue rate
    between boundary crossings (a split costs the latch cycle only).

    All bursts use one AXI ID, so R beats return in AR order; a tag
    FIFO pushed at AR issue and popped on the burst's final R beat
    routes every beat to its region. The tag FIFO's occupancy is the
    outstanding-burst bound: AR issue stalls when it fills, and since
    every outstanding burst returns its beats in order, tags drain
    whenever the port makes progress -- a saturated reader cannot
    wedge itself or the port. ``err`` pulses for each accepted non-OKAY
    RRESP; the command layer accumulates it over the current command.
    """

    def __init__(self, params, *, bus_name=None, axi_data_width=None):
        super().__init__(params)

        axi_data_width = axi_data_width or self.data_width
        if axi_data_width not in (self.data_width, self.data_width // 2):
            raise ValueError(
                'AXI reads must use full or half logical beat width')
        self.half_width = axi_data_width != self.data_width
        self.bus = AXIInterface(data_width=axi_data_width,
                                addr_width=self.addr_width,
                                id_width=self.id_width,
                                name=bus_name)
        self.rreq = Decoupled(StreamReadReq, self.params)
        self.rresp = Decoupled(StreamReadResp, self.params)

        self.err = Signal()

        # Tag entries for the worst outstanding set the supply's credit
        # discipline admits: every outstanding beat of both streams in
        # its own 1-beat sub-burst, plus one page split per stream.
        if self.mm_starvation_counters:
            self.payload_request = Signal()
            self.scale_request = Signal()
        self.tag_depth = 2 * self.sg_credits \
            + ((2 * self.sg_credits * self.beat_bytes) >> 12) + 4

        # Observation (units of the named thing).
        self.bursts = Signal(48)
        self.beats = Signal(48)

    def elaborate(self, platform):
        m = Module()

        tag_q = m.submodules.tag_q = self.tag_q = Queue(
            self.tag_depth, Record, [('meta', 1, Direction.FANOUT)])

        req = self.rreq
        lg = self.lg_beat

        # Beats remaining to the 4 KiB boundary (addresses arrive beat
        # aligned by the unit's accept-time check); a request ending
        # exactly at the boundary does not split. ``beats`` is 9 bits
        # with 1..256 legal, so the high bit alone forces the holder
        # only for a 256-beat request, which a page never holds.
        to_page = (0x1000 - (req.bits.addr & 0xfff)) >> lg
        direct = Signal()
        max_beats = 128 if self.half_width else 256
        m.d.comb += direct.eq((req.bits.beats < max_beats)
                              & (req.bits.beats <= to_page))

        # Split holding register: addr/rem advance per issued sub-burst.
        # Kept as attributes for waveform/testbench observation (the
        # supply discipline).
        busy = self.busy = Signal()
        hold_addr = self.hold_addr = Signal(self.addr_width)
        hold_rem = self.hold_rem = Signal(9)
        hold_meta = Signal()
        hold_page = (0x1000 - (hold_addr & 0xfff)) >> lg
        if self.half_width and self.beat_bytes < 32:
            hold_page = Mux(hold_page <= max_beats, hold_page, max_beats)
        hold_burst = Mux(hold_rem <= hold_page, hold_rem, hold_page)

        # The request is consumed when its AR is accepted: directly for
        # a whole-request burst, or at the holder's *final sub-burst*
        # for a page split. Without the final-sub-burst term a split
        # request stayed at the head after the holder retired, reloaded,
        # and re-issued forever -- duplicated beats overran the region
        # FIFO's reserved space and wedged the port; without the bus
        # accept on the direct term a backpressured AR consumed its
        # request unissued and the beats were lost. (Both found by the
        # llama.cpp raw-command suite against the Verilator image:
        # region bases three beats before a 4 KiB boundary.)
        ar_direct = req.valid & ~busy & direct & tag_q.enq.ready
        ar_hold = busy & tag_q.enq.ready
        ar_fire = self.bus.ar.valid & self.bus.ar.ready
        last_sub = ar_fire & ~ar_direct & (hold_rem == hold_burst)
        req_ready_direct = ar_direct & self.bus.ar.ready
        burst_beats = Mux(ar_direct, req.bits.beats, hold_burst)
        axi_beats = burst_beats << int(self.half_width)
        m.d.comb += [
            self.bus.ar.valid.eq(ar_direct | ar_hold),
            self.bus.ar.bits.addr.eq(Mux(ar_direct, req.bits.addr, hold_addr)),
            self.bus.ar.bits.len.eq(axi_beats - 1),
            self.bus.ar.bits.size.eq(lg - int(self.half_width)),
            self.bus.ar.bits.burst.eq(AXIBurst.INCR),
            self.bus.ar.bits.lock.eq(0),
            self.bus.ar.bits.prot.eq(0),
            self.bus.ar.bits.cache.eq(0b0011),
            self.bus.ar.bits.qos.eq(0),
            self.bus.ar.bits.region.eq(0),
            self.bus.ar.bits.id.eq(0),
            req.ready.eq(req_ready_direct | last_sub),
        ]

        m.d.comb += [
            tag_q.enq.valid.eq(ar_fire),
            tag_q.enq.bits.meta.eq(Mux(ar_direct, req.bits.meta, hold_meta)),
        ]

        if self.mm_starvation_counters:
            m.d.comb += [
                self.payload_request.eq(ar_fire & ~tag_q.enq.bits.meta),
                self.scale_request.eq(ar_fire & tag_q.enq.bits.meta),
            ]
        with m.If(ar_fire):
            m.d.sync += self.bursts.eq(self.bursts + 1)
            with m.If(~ar_direct):
                m.d.sync += [
                    hold_addr.eq(hold_addr + (hold_burst << lg)),
                    hold_rem.eq(hold_rem - hold_burst),
                ]
                with m.If(hold_rem == hold_burst):
                    m.d.sync += busy.eq(0)

        with m.If(~busy & req.valid & ~direct):
            m.d.sync += [
                busy.eq(1),
                hold_addr.eq(req.bits.addr),
                hold_rem.eq(req.bits.beats),
                hold_meta.eq(req.bits.meta),
            ]

        # R beats flow straight through with the head tag; the tag pops
        # on the burst's final beat, matching the in-order single-ID
        # response stream.
        r_fire = self.bus.r.valid & self.bus.r.ready
        r_last = r_fire & self.bus.r.bits.last
        if self.half_width:
            low = Signal(self.data_width // 2, reset_less=True)
            have_low = Signal()
            m.d.comb += [
                self.rresp.valid.eq(self.bus.r.valid & have_low),
                self.rresp.bits.data.eq(Cat(low, self.bus.r.bits.data)),
                self.bus.r.ready.eq(~have_low | self.rresp.ready),
            ]
            with m.If(r_fire):
                m.d.sync += have_low.eq(~have_low)
                with m.If(~have_low):
                    m.d.sync += low.eq(self.bus.r.bits.data)
        else:
            m.d.comb += [
                self.rresp.valid.eq(self.bus.r.valid),
                self.rresp.bits.data.eq(self.bus.r.bits.data),
                self.bus.r.ready.eq(self.rresp.ready),
            ]
        m.d.comb += [
            self.rresp.bits.meta.eq(tag_q.deq.bits.meta),
            tag_q.deq.ready.eq(r_last),
        ]
        with m.If(self.rresp.fire):
            m.d.sync += self.beats.eq(self.beats + 1)
        m.d.comb += self.err.eq(r_fire & (self.bus.r.bits.resp != 0))

        return m


def pick_one(items, index):
    """``items[index]`` as a nested Mux: one small mux per bit.

    A dynamic index over a list must not go through ``Array``: its
    lowering expands to a per-element select that synthesizes to many
    times the logic.
    """
    expr = items[-1]
    for i in range(len(items) - 2, -1, -1):
        expr = Mux(index == i, items[i], expr)
    return expr


class BeatStore(Elaboratable):
    """One write per cycle and independently read consecutive FIFO beats."""

    def __init__(self,
                 width,
                 depth,
                 *,
                 use_sync_read_mem=False,
                 banks=2,
                 name=None,
                 src_loc_at=0):
        assert banks in (2, 4) and depth >= banks
        self.width = width
        self.depth = depth
        self.banks = banks
        self.use_sync_read_mem = use_sync_read_mem
        self.w_data = Signal(width, name='w_data')
        self.w_en = Signal(name='w_en')
        self.count = Signal(range(depth + 1), name='count')
        self.head_bank = Signal(range(banks))
        self.forwarded = Signal(width, reset_less=True)
        self.forward = Signal(banks)
        self.bank_data = [
            Signal(width, name=f'bank{bank}_data') for bank in range(banks)
        ]
        self.pop = Signal(range(banks + 1), name='pop')
        self.clr = Signal(name='clr')

    def elaborate(self, platform):
        m = Module()
        lg = log2_int(self.depth)
        rd = Signal(lg)
        wr = Signal(lg)
        w_en = self.w_en & (self.count < self.depth)
        rd_next = Mux(self.clr, 0, rd + self.pop)[:lg]
        # BRAM reads the post-pop addresses so both heads are ready on
        # the next edge. A simultaneous write can supply either new head.
        read_head = rd_next if self.use_sync_read_mem else rd
        bank_bits = log2_int(self.banks)
        ram_style = 'block' if self.use_sync_read_mem else 'distributed'
        if self.use_sync_read_mem:
            with m.If(w_en):
                m.d.sync += self.forwarded.eq(self.w_data)
        for bank in range(self.banks):
            wraps = read_head[:bank_bits] > bank
            addr = (read_head[bank_bits:] + wraps)[:lg - bank_bits]
            mem = Memory(width=self.width,
                         depth=self.depth // self.banks,
                         attrs={'ram_style': ram_style})
            wport = mem.write_port()
            if self.use_sync_read_mem:
                rport = mem.read_port(transparent=False)
            else:
                rport = mem.read_port(domain='comb')
            m.submodules[f'bank{bank}_write'] = wport
            m.submodules[f'bank{bank}_read'] = rport
            bank_write = w_en & (wr[:bank_bits] == bank)
            if self.use_sync_read_mem:
                m.d.sync += self.forward[bank].eq(bank_write
                                                  & (wr[bank_bits:] == addr))
            m.d.comb += [
                wport.addr.eq(wr[bank_bits:]),
                wport.data.eq(self.w_data),
                wport.en.eq(bank_write),
                rport.addr.eq(addr),
                self.bank_data[bank].eq(rport.data),
            ]
        m.d.comb += self.head_bank.eq(rd[:bank_bits])
        m.d.sync += [
            rd.eq(rd_next),
            wr.eq(Mux(self.clr, 0, (wr + w_en)[:lg])),
            self.count.eq(Mux(self.clr, 0, self.count + w_en - self.pop)),
        ]
        return m


class StreamMultiPortReader(HasTRoomParams, Elaboratable):
    """Independent AXI readers with request-ordered payload retirement."""

    def __init__(self, params, read_topology):
        super().__init__(params)
        read_topology.validate(self.addr_width, self.beat_bytes)
        self.read_topology = read_topology
        self.readers = [
            StreamAxiReader(params,
                            bus_name=port.name,
                            axi_data_width=port.axi_data_width)
            for port in read_topology.ports
        ]
        self.bus = self.readers[read_topology.shared_port].bus
        self.aux_buses = {
            port.name: reader.bus
            for index, (
                port,
                reader) in enumerate(zip(read_topology.ports, self.readers))
            if index != read_topology.shared_port
        }
        self.rreq = Decoupled(StreamReadReq, params)
        self.presp = Decoupled(StreamPayPair,
                               params,
                               triple=read_topology.retirement_lanes == 3)
        self.mresp = Decoupled(StreamScaleBeat, params)
        self.pair_en = Signal()
        self.triple_en = Signal()
        self.err = Signal()
        self.payload_request = Signal(range(len(self.readers) + 1))
        self.scale_request = Signal()

    def elaborate(self, platform):
        m = Module()
        topology = self.read_topology
        req, presp, mresp = self.rreq, self.presp, self.mresp
        triple = topology.retirement_lanes == 3
        pair_gate = self.pair_en & (topology.retirement_lanes >= 2)
        port_shape = range(len(self.readers))
        port = Signal(port_shape)
        mapped = Signal()
        addr = req.bits.addr
        with m.If(req.bits.meta):
            m.d.comb += [
                mapped.eq(topology.scales.contains(addr, addr)),
                port.eq(topology.scales.ports[0]),
            ]
        with m.Else():
            for region in topology.payload_regions:
                with m.If(region.contains(addr, addr)):
                    m.d.comb += [mapped.eq(1), port.eq(region.port_for(addr))]

        m.submodules.order = order = Queue(
            self.sg_credits,
            Record, [
                ('port', port_shape, Direction.FANOUT),
                ('beats', 9, Direction.FANOUT),
            ],
            flow=True)
        stores = []
        ready = []
        for index, (config,
                    reader) in enumerate(zip(topology.ports, self.readers)):
            # Name the reader after its port (pl, hp0..): a generic
            # reader{index} collides with other wrapper DMA readers (the
            # loader's reader1) in the generated Verilog module namespace.
            setattr(m.submodules, config.name, reader)
            dual_bank = topology.dual_bank_shared_port and index == topology.shared_port
            store = BeatStore(self.data_width,
                              config.return_buffer_beats,
                              use_sync_read_mem=config.sync_read_buffer,
                              banks=4 if triple and not dual_bank else 2)
            setattr(m.submodules, f'store{index}', store)
            stores.append(store)

            sink = reader.rreq
            if config.issue_queue_depth:
                issue = Queue(config.issue_queue_depth,
                              StreamReadReq,
                              self.params,
                              flow=False)
                setattr(m.submodules, f'issue{index}', issue)
                m.d.comb += issue.deq.connect(reader.rreq)
                sink = issue.enq
            ready.append(sink.ready)
            m.d.comb += [
                sink.valid.eq(req.valid & mapped & (port == index)
                              & (req.bits.meta | order.enq.ready)),
                sink.bits.eq(req.bits),
                reader.rresp.ready.eq(
                    Mux(reader.rresp.bits.meta, mresp.ready, store.count
                        < config.return_buffer_beats)),
                store.w_en.eq(reader.rresp.fire & ~reader.rresp.bits.meta),
                store.w_data.eq(reader.rresp.bits.data),
            ]

        self.stores = stores
        self.order_q = order
        scale_reader = self.readers[topology.scales.ports[0]]
        selected_ready = Array(ready)[port]
        m.d.comb += [
            req.ready.eq(mapped & selected_ready
                         & (req.bits.meta | order.enq.ready)),
            order.enq.valid.eq(req.valid & mapped & ~req.bits.meta
                               & selected_ready),
            order.enq.bits.port.eq(port),
            order.enq.bits.beats.eq(req.bits.beats),
            mresp.valid.eq(scale_reader.rresp.valid
                           & scale_reader.rresp.bits.meta),
            mresp.bits.data.eq(scale_reader.rresp.bits.data),
        ]

        # Ordered dual-beat retirement. ``cur`` names the stripe whose
        # beats retire next and ``nxt`` the following request, so a
        # retire can take the head stripe's final beat and the next
        # stripe's first beat in one cycle instead of bubbling at
        # every stripe boundary. Kept as attributes for waveform and
        # testbench observation.
        self.cur_v = cur_v = Signal(name='cur_v')
        self.cur_port = cur_port = Signal(port_shape, name='cur_port')
        self.cur_left = cur_left = Signal(9, name='cur_left')
        self.nxt_v = nxt_v = Signal(name='nxt_v')
        self.nxt_port = nxt_port = Signal(port_shape, name='nxt_port')
        self.nxt_left = nxt_left = Signal(9, name='nxt_left')
        counts = Array(s.count for s in stores)

        cur_count = counts[cur_port]
        nxt_count = counts[nxt_port]
        same_port = nxt_port == cur_port
        s0_ok = cur_v & (cur_count != 0)

        # Cross at most one request boundary. Leave one beat in the
        # next request so a retirement only consumes one order entry.
        def can_retire(lanes):
            in_current = (cur_left >= lanes) & (cur_count >= lanes)
            next_needed = (lanes - cur_left)[:2]
            across = ((cur_left < lanes) & nxt_v & (cur_count >= cur_left) &
                      (nxt_left > next_needed) &
                      (nxt_count >= Mux(same_port, lanes, next_needed)))
            return cur_v & (in_current | across)

        dual_sel = pair_gate & can_retire(2)
        triple_sel = dual_sel & self.triple_en & can_retire(
            3) if triple else Const(0)
        if triple and topology.dual_bank_shared_port:
            # The shared PL store can supply at most two beats. A triple
            # can still span PL and HP if the third beat completes the batch.
            triple_sel = triple_sel & ((cur_port != topology.shared_port) |
                                       ((cur_left < 3) & ~same_port))
        retire_count = Signal(range(4))
        m.d.comb += retire_count.eq(1 + dual_sel + triple_sel)

        for lane in range(topology.retirement_lanes if triple else 2):
            # If both requests select the same store, its head-relative
            # index already spans the boundary.
            from_next = (cur_left <= lane) & ~same_port
            lane_port = Mux(from_next, nxt_port, cur_port)
            lane_offset = Mux(from_next, lane - cur_left, lane)
            lane_data = Const(0, self.data_width)
            for i, store in enumerate(stores):
                bank_bits = log2_int(store.banks)
                selected_bank = (store.head_bank + lane_offset)[:bank_bits]
                port_hit = lane_port == i
                if lane == 2 and store.banks == 2:
                    port_hit = port_hit & from_next
                # A hit store has exactly one selected bank, so pick it,
                # then apply the store's forwarding, then the port: three
                # small muxes per bit (a 4:1, a 2:1 and one port term)
                # instead of one AND-OR term per (port, bank).
                picked = pick_one(store.bank_data, selected_bank)
                if store.use_sync_read_mem:
                    forwarded = pick_one(
                        [store.forward[bank] for bank in range(store.banks)],
                        selected_bank)
                    picked = Mux(forwarded, store.forwarded, picked)
                lane_data = lane_data | Mux(port_hit, picked, 0)
            m.d.comb += presp.bits[f'data{lane}'].eq(lane_data)
        m.d.comb += [
            presp.valid.eq(s0_ok),
            presp.bits.dual.eq(dual_sel),
        ]
        if triple:
            m.d.comb += presp.bits.triple.eq(triple_sel)

        fired = presp.fire
        cons_cross = fired & (cur_left < retire_count)
        cons_exhaust = fired & (cur_left == retire_count)
        next_pop = Signal(range(3))
        m.d.comb += next_pop.eq(Mux(cons_cross, retire_count - cur_left, 0))
        for i, store in enumerate(stores):
            cur_pop = Mux(fired & (cur_port == i),
                          Mux(cons_cross, cur_left, retire_count), 0)
            m.d.comb += store.pop.eq(cur_pop + Mux(nxt_port == i, next_pop, 0))

        cl1 = Mux(cons_cross, nxt_left - next_pop,
                  Mux(fired, cur_left - retire_count, cur_left))
        cp1 = Mux(cons_cross, nxt_port, cur_port)
        cv1 = Mux(cons_cross, 1, Mux(cons_exhaust, 0, cur_v))
        rotate = ~cv1 & nxt_v
        m.d.sync += [
            cur_v.eq(Mux(rotate, 1, cv1)),
            cur_port.eq(Mux(rotate, nxt_port, cp1)),
            cur_left.eq(Mux(rotate, nxt_left, cl1)),
        ]
        # ``nxt`` is consumed both by a full rotation and by a cross
        # retire that spends one of its beats; either way it must be
        # refilled from the order queue the same edge, or a later
        # rotation would replay the whole stripe.
        nxt_used = rotate | cons_cross
        m.d.comb += order.deq.ready.eq(nxt_used | ~nxt_v)
        nxt_load = (nxt_used | ~nxt_v) & order.deq.valid
        with m.If(nxt_load):
            m.d.sync += [
                nxt_v.eq(1),
                nxt_port.eq(order.deq.bits.port),
                nxt_left.eq(order.deq.bits.beats),
            ]
        with m.Elif(nxt_used):
            m.d.sync += nxt_v.eq(0)

        m.d.comb += self.err.eq(
            Cat(*(reader.err for reader in self.readers)).any())
        if self.mm_starvation_counters:
            m.d.comb += [
                self.payload_request.eq(
                    sum(reader.payload_request for reader in self.readers)),
                self.scale_request.eq(scale_reader.scale_request),
            ]
        return m
