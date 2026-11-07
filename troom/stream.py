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

from amaranth import *
from amaranth.hdl.rec import Direction, Record
from amaranth.utils import log2_int

from troom.consts import Format, Status
from troom.matrix import ActRAM, ActWord, MacArray, MmSegment, WSegWord
from troom.types import HasTRoomParams

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
    """

    MAX_COMP = 4  # completions per beat across the payload pitches

    def __init__(self,
                 *,
                 beat_bytes,
                 unit_bytes,
                 depth,
                 pitches,
                 slots=2,
                 banked_slots=False):
        assert beat_bytes <= unit_bytes
        assert all(p <= unit_bytes for p in pitches)
        assert depth & (depth - 1) == 0
        assert 1 <= slots <= self.MAX_COMP
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
            assert slots & (slots - 1) == 0
            assert depth >= slots
            assert all(p <= WIN_BYTES or p > beat_bytes for p in pitches)

        self.beat = Signal(beat_bytes * 8)
        self.beat_in = Signal()
        self.pitch = Signal(8)
        self.consume = Signal(3)
        self.clr = Signal()

        self.avail = Signal(range(depth + 1))
        self.can_take = Signal()
        self.slot = [
            Signal(unit_bytes * 8, name=f'slot{i}') for i in range(slots)
        ]
        self.rotation = Signal(max(log2_int(slots), 1))

        # State (attributes for waveform observation). ``uf[i]`` is
        # bank i of the stationary circular file; ``head`` names the
        # bank holding visible unit 0.
        self.uf = [Signal(unit_bytes * 8, name=f'uf{i}') for i in range(depth)]
        self.head = Signal(log2_int(depth))
        self.ucnt = Signal(range(depth + 1))
        self.part = Signal(unit_bytes * 8)
        self.ph = Signal(8)

    def elaborate(self, platform):
        m = Module()
        ub, depth, bb = self.unit_bytes, self.depth, self.bb

        # The logical assembly view: the partial unit's ``ph`` bytes
        # followed by the beat's ``bb`` -- the part register is kept
        # zero above ``ph`` (enforced by construction below), so the
        # view is the part OR the beat shifted up by ``ph``. The only
        # variable movement is one beat-width shifter; every unit
        # slice below is a constant-offset selection.
        bs = (Cat(self.beat, Const(0, ub * 8)) <<
              (self.ph * 8))[:(ub + bb) * 8]
        stream = Cat(self.part, Const(0, bb * 8)) | bs
        stream_bits = len(stream)
        tot = self.ph + bb  # valid bytes once this cycle's beat lands

        # Completion count without a runtime divider (the step-4
        # review's $div hazard): ``pitch`` is constrained to
        # ``pitches`` and the phase invariant ``ph < pitch`` bounds
        # tot = ph + beat_bytes, so for each supported pitch the
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
            c = ((tot >= 4 * p).as_unsigned() + (tot >= 3 * p).as_unsigned() +
                 (tot >= 2 * p).as_unsigned() + (tot >= p).as_unsigned())[:3]
            cu_full = Mux(self.pitch == p, c, cu_full)
            rem = Mux(self.pitch == p, tot - c * p, rem)
        cu = Mux(self.beat_in, cu_full, 0)

        m.d.comb += self.avail.eq(self.ucnt + cu)
        m.d.comb += self.can_take.eq((self.ucnt + cu_full) <= depth)

        # This cycle's completions: units [k*pitch, (k+1)*pitch) of
        # the view, a constant slice per candidate pitch (cases past
        # the stream are dead states and read as zero).
        comp = []
        for k in range(self.MAX_COMP):
            per_p = []
            for p in self.pitches:
                lo, hi = 8 * p * k, 8 * p * (k + 1)
                if hi <= stream_bits:
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

        # Stationary bank file: visible unit v reads bank
        # (head + v) % depth -- a depth-way select per bit -- when the
        # file holds it (ucnt > v); otherwise this cycle's completion
        # v - ucnt bypasses to the slot the same cycle. Slots past the
        # file and the completions are dead states (can_take bounds
        # the sequence) and read as zero.
        def bank_at(sel):
            expr = self.uf[0]
            for i in range(1, self.depth):
                expr = Mux(sel == i, self.uf[i], expr)
            return expr

        lgh = log2_int(self.depth)
        lg_slots = log2_int(self.slots)
        m.d.comb += self.rotation.eq(self.head[:lg_slots] if lg_slots else 0)
        for v in range(self.slots):
            # Visible unit v: the bank copy when ucnt > v, else this
            # cycle's completion v - ucnt bypasses to the slot.
            if self.banked_slots:
                # Column v holds logical offset (v - head) % slots.
                # Columns below head's column read the next bank row.
                # This works for arbitrary heads, including every
                # short-tail superblock; no alignment assumption.
                offset = Signal(max(lg_slots, 1), name=f'offset{v}')
                row = Signal(max(lgh - lg_slots, 1), name=f'bank_row{v}')
                m.d.comb += [
                    offset.eq((v -
                               self.rotation)[:lg_slots] if lg_slots else 0),
                    row.eq((self.head[lg_slots:] + (v < self.rotation)
                            )[:lgh - lg_slots] if lgh > lg_slots else 0),
                ]
                selected = Const(0, ub * 8)
                for r in range(depth // self.slots):
                    selected = Mux(row == r, self.uf[r * self.slots + v],
                                   selected)
                # Only the narrow flat prefix needs a multi-completion
                # bypass mux. A >32-B unit completes at most once per
                # beat, so its valid bypass can only be completion 0.
                bypass = Const(0, WIN_BYTES * 8)
                for k in range(self.MAX_COMP):
                    bypass = Mux(offset == self.ucnt + k,
                                 comp[k][:WIN_BYTES * 8], bypass)
                expr = Mux(self.ucnt > offset, selected,
                           Cat(bypass, comp[0][WIN_BYTES * 8:]))
            else:
                expr = comp[v]
                for k in range(v + 1):
                    expr = Mux(self.ucnt == k, comp[v - k], expr)
                expr = Mux(self.ucnt > v, bank_at((self.head + v)[:lgh]), expr)
            m.d.comb += self.slot[v].eq(expr)

        # The next partial unit: the view's bytes past the last
        # completion, a constant slice selected by (pitch, cu) -- a
        # plain select chain (a Switch over concatenated values does
        # not const-fold reliably).
        part_sliced = Const(0, ub * 8)
        for p in self.pitches:
            for k in range(self.MAX_COMP + 1):
                lo = 8 * p * k
                if lo >= stream_bits:
                    src = Const(0, ub * 8)
                elif lo + ub * 8 <= stream_bits:
                    src = stream[lo:lo + ub * 8]
                else:
                    # Only the stream's remaining bytes exist; the
                    # rest of the register stays zero (the invariant
                    # that part is clean above ph).
                    src = Cat(stream[lo:], Const(0,
                                                 ub * 8 - (stream_bits - lo)))
                part_sliced = Mux((self.pitch == p) & (cu == k), src,
                                  part_sliced)
        part_next = Mux(self.beat_in, part_sliced, self.part)

        # Write every completion, including those bypassed to the
        # consumer this cycle.  Such writes target freed banks and are
        # harmless: can_take bounds ucnt + cu <= depth, so no completion
        # can overwrite an old live unit.  Keeping consume out of the
        # wide bank enables cuts the emission-decision feedback path.
        # Completion k always targets (tail + k) % depth.  Select its
        # data once per bank residue, then use a narrow local enable;
        # do not build a priority write/hold mux per bank and bit.
        tail = Signal(lgh)
        m.d.comb += tail.eq(self.head + self.ucnt)
        write_data = []
        for residue in range(min(depth, self.MAX_COMP)):
            selected = Signal(ub * 8, name=f'write_data{residue}')
            expr = Const(0, ub * 8)
            for k in range(self.MAX_COMP):
                hit = (((tail + k) % min(depth, self.MAX_COMP)) == residue)
                if depth < self.MAX_COMP:
                    hit = hit & (k < cu)
                expr = expr | Mux(hit, comp[k], Const(0, ub * 8))
            m.d.comb += selected.eq(expr)
            write_data.append(selected)
        for b in range(self.depth):
            enable = Const(0)
            for k in range(self.MAX_COMP):
                dst = (tail + k)[:lgh]
                enable = enable | ((k < cu) & (dst == b))
            with m.If(self.beat_in & enable):
                m.d.sync += self.uf[b].eq(write_data[b % len(write_data)])
        m.d.sync += self.ucnt.eq(
            Mux(self.clr, 0, self.ucnt + cu - self.consume))
        m.d.sync += self.part.eq(Mux(self.clr, 0, part_next))
        m.d.sync += self.ph.eq(
            Mux(self.clr, 0, Mux(self.beat_in, rem, self.ph)))
        m.d.sync += self.head.eq(
            Mux(self.clr, 0, (self.head + self.consume)[:lgh]))

        return m


class StreamSupply(HasTRoomParams, Elaboratable):
    """SG1 reader and format decoder feeding per-lane segment queues.

    Configuration is held by the unit: geometry inputs (including the
    format, which fixes the window's payload size and decode) are
    valid on the ``start`` pulse and stay held for the run.
    """

    def __init__(self, params):
        super().__init__(params)

        if self.data_width > 512:
            raise ValueError(
                f'stream prototype supports up to 512-bit beats, not '
                f'{self.data_width}')
        if self.n_outputs not in (2, 4):
            raise ValueError(
                'the SG1 image interleave matches n_outputs: supported '
                f'widths are 2 and 4, not {self.n_outputs}')

        # Emission skid lanes and per-format capacities sized by the
        # group width: the decoder emits up to n_outputs units per
        # cycle, and the K-quant bonded group needs its n_outputs
        # chunks resident plus the streaming margin (b3's one-chunk
        # starvation), so the unit file holds the group plus margin
        # (the circular bank file wraps by truncation: powers of two
        # only). The meta buffer must serve the bonded group's entries
        # (n * 18 B worst case) while still admitting a full beat.
        self.lg_lane = max(log2_int(self.n_outputs), 1)
        self.asm_depth = 4 if self.n_outputs <= 2 else 8
        self.meta_buf_bytes = max(
            SG_META_BUF_BYTES,
            self.n_outputs * Q6_META_BYTES + self.beat_bytes)

        self.mem_rreq = Decoupled(StreamReadReq, self.params)
        self.mem_rresp = Decoupled(StreamReadResp, self.params)

        self.seg_q = [
            Queue(self.mm_seg_fifo, WSegWord, self.params, flow=False)
            for _ in range(self.n_outputs)
        ]
        self.segs = [q.deq for q in self.seg_q]

        # Register the request before AXI arbitration. Reserve capacity
        # when this queue accepts, including requests not yet on the bus.
        self.req_q = Queue(2, StreamReadReq, self.params, flow=False)

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
        self.tail_lanes = Signal(max(log2_int(self.n_outputs), 1) + 1)
        self.grp_rem = Signal(self.rows_bits)
        self.mdata = Signal(self.meta_buf_bytes * 8)
        self.mhave = Signal(8)
        self.pay_asm = UnitAssembler(beat_bytes=self.beat_bytes,
                                     unit_bytes=Q6_CHUNK_BYTES,
                                     depth=self.asm_depth,
                                     pitches=(Q5_WIN_BYTES, WIN_BYTES,
                                              Q4_CHUNK_BYTES, Q6_CHUNK_BYTES),
                                     slots=self.n_outputs,
                                     banked_slots=True)

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
        self.s_rotation = Signal(max(log2_int(self.n_outputs), 1))
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
        lgn = log2_int(n)
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
        groups_n = ((self.rows + (n - 1)) >> lgn)[:self.rows_bits]
        tail_n = (self.rows - ((groups_n - 1) << lgn))[:lg_lane + 1]

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

        p_burst = burst_size(p_rem, self.p_used, p_out, self.sg_pay_fifo)
        m_burst = burst_size(m_rem, self.m_used, m_out, self.sg_meta_fifo)
        # Accumulate freed payload reservations before another request.
        # Greedy single-beat refills fragment a linear stream into tiny
        # AXI transactions. Clip the threshold to the payload capacity
        # and outstanding limit, and admit the final short region tail,
        # so batching cannot strand a small FIFO or terminal remainder.
        p_min = min(self.sg_min_burst_beats, self.sg_pay_fifo, self.sg_credits)
        p_want = self.run & (p_burst != 0) & ((p_burst >= p_min)
                                              | (p_burst == p_rem))
        # Metadata defaults to eager refills: batching it requires more
        # lead time/storage ahead of the much larger payload stream.
        m_min = min(self.sg_meta_min_burst_beats, self.sg_meta_fifo,
                    self.sg_credits)
        m_want = self.run & (m_burst != 0) & ((m_burst >= m_min)
                                              | (m_burst == m_rem))
        request = self.req_q.enq
        m.d.comb += [
            request.valid.eq(p_want | m_want),
            request.bits.addr.eq(Mux(m_want, m_addr, p_addr)),
            request.bits.beats.eq(Mux(m_want, m_burst, p_burst)),
            request.bits.meta.eq(m_want),
        ]

        rresp = self.mem_rresp
        m.d.comb += rresp.ready.eq(
            Mux(rresp.bits.meta, self.meta_q.enq.ready, self.pay_q.enq.ready))
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
        p_land = rresp.fire & ~rresp.bits.meta
        m_land = rresp.fire & rresp.bits.meta
        m.d.sync += [
            p_out.eq(p_out + Mux(p_book, p_burst, 0) - Mux(p_land, 1, 0)),
            m_out.eq(m_out + Mux(m_book, m_burst, 0) - Mux(m_land, 1, 0)),
            self.p_used.eq(self.p_used + Mux(p_book, p_burst, 0) -
                           self.pay_q.deq.fire),
            self.m_used.eq(self.m_used + Mux(m_book, m_burst, 0) -
                           self.meta_q.deq.fire),
        ]
        with m.If(p_land):
            m.d.sync += self.pay_beats.eq(self.pay_beats + 1)
        with m.If(m_land):
            m.d.sync += self.meta_beats.eq(self.meta_beats + 1)

        with m.If(self.mem_rreq.fire):
            m.d.sync += self.bursts.eq(self.bursts + 1)
        with m.If(rreq_fire):
            with m.If(m_want):
                m.d.sync += [
                    m_addr.eq(m_addr + (m_burst << self.lg_beat)),
                    m_rem.eq(m_rem - m_burst),
                ]
            with m.Else():
                m.d.sync += [
                    p_addr.eq(p_addr + (p_burst << self.lg_beat)),
                    p_rem.eq(p_rem - p_burst),
                ]

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
        mdata, mhave = self.mdata, self.mhave

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
        data = pay.bits.data

        # Format selects: payload pitch (bytes per window for the
        # flat formats, per 8-window chunk for the K-quants), meta
        # entry pitch, and the chunked flag.
        is_q5 = self.fmt == Format.Q5_0
        is_q6 = self.fmt == Format.Q6_K
        is_q4 = self.fmt == Format.Q4_K
        is_kq = is_q6 | is_q4
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

        # Meta assembly: the same byte-granular discipline for scale
        # entries. Entries live at ``epitch`` pitch; extraction reads
        # this cycle's view, consumption shifts with the emissions.
        mq = self.meta_q.deq
        meta_in = mq.valid & self.run & (mhave <= self.meta_buf_bytes - bb)
        m.d.comb += mq.ready.eq(meta_in)
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
        rows_e = [((states[e][0] << lgn) + states[e][2])[:self.rows_bits]
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

        emits_now = Signal(3)
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
            # use it. Rotate only the narrow decoded words, carrying
            # the assembler's column rotation across the skid.
            m.d.comb += bank_w[e].eq(decode_unit(self.s_slot[e], self.s_jn[0]))
        w_e = [Signal(self.seg_len * 8, name=f'decoded{e}') for e in range(n)]
        for e in range(n):
            m.d.comb += w_e[e].eq(
                Array(bank_w)[(self.s_rotation +
                               e)[:lgn]] if lgn else bank_w[0])

        # Distinct lanes within a decided emission set mean each queue
        # takes at most one push per cycle. Each push always finds
        # space (the decision reserved it with the pending-push-padded
        # count), so no skid stall path exists.
        for l, q in enumerate(self.seg_q):
            enq_valid = Const(0)
            wsel = Const(0, self.seg_len * 8)
            rowsel = Const(0, self.rows_bits)
            fsel = {
                name: Const(0, len(sig))
                for name, sig in self.s_f[0].items()
            }
            for e in range(n):
                hit = self.s_v[e] & (self.s_lane[e] == l)
                enq_valid = enq_valid | hit
                wsel = Mux(hit, w_e[e], wsel)
                rowsel = Mux(hit, self.s_row[e], rowsel)
                for name in fsel:
                    fsel[name] = Mux(hit, self.s_f[e][name], fsel[name])
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
        # Meta release: ``meta_shift`` is bounded per format class --
        # epitch * {0..n} with epitch 2 flat and 16/18 chunked -- so
        # the consumption is a small constant-shift mux per class
        # (flat the even bytes to 2*n, K-quant the pitch multiples to
        # n*pitch) merged by ``is_kq``, not one wide select over the
        # whole buffer. The zero-above-valid invariant is preserved:
        # every merged beat is zero-extended and every shift is within
        # the valid count, so the tail above ``mhave`` stays zero.
        # Field extraction reads this cycle's view and lands in the
        # skid registers, keeping the meta side's advancement
        # synchronized with the payload's (P2-4: registered
        # extraction).
        flat_shift = epitch * emits_now
        kq_shift = epitch * Mux(jn7 & fire[0], emits_now, 0)
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
        meta_shift = Mux(is_kq, kq_shift, flat_shift)
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
                mdata.eq(0),
                mhave.eq(0),
            ]
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

    def __init__(self, params, *, mem_base=0, mem_size=None):
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
        self.mem_base = mem_base
        self.mem_size = mem_size

        self.sink = Decoupled(StreamMatmulDesc, self.params)
        self.segs = Decoupled(MmSegment, self.params)
        self.actram = ActRAM(self.params)
        self.supply = StreamSupply(self.params)
        self.mac = MacArray(self.params)

        self.act_wr = self.actram.wr
        self.mem_rreq = self.supply.mem_rreq
        self.mem_rresp = self.supply.mem_rresp

        self.done = Signal()
        self.status = Signal(Status)
        self.active = Signal()
        self.start_ok = Signal()

        # Launch-side observation.
        self.slots = Signal(48)
        self.bubbles = Signal(48)
        self.starve_cycles = Signal(48)
        self.credit_stalls = Signal(48)
        self.useful_macs = Signal(48)
        self.run_cycles = Signal(48)
        self.drain_cycles = Signal(48)
        self.feed_beats = Signal(48)

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
        lgn = log2_int(n)

        cur_fmt = Signal(Format)
        cur_k = Signal(self.lg_act_k + 1)
        cur_rows = Signal(self.rows_bits)
        cur_p_base = Signal(self.addr_width)
        cur_m_base = Signal(self.addr_width)

        nsegs = cur_k >> 5
        pair_row = Cat(cur_rows, Const(0, lgn)) if lgn else cur_rows
        # Group count at this unit's width: ceil(rows / n_outputs)
        # (pairs at n=2, quads at n=4).
        pairs = ((cur_rows + (n - 1)) >> lgn)[:self.rows_bits]
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
        m.d.comb += [
            p_span.eq(p_total_r << self.lg_beat),
            m_span.eq(m_total_r << self.lg_beat),
            mem_end.eq(self.mem_base + self.mem_size),
        ]
        range_ok = ((o_p_base >= self.mem_base)
                    & (o_m_base >= self.mem_base)
                    & ((o_p_base + p_span) <= mem_end)
                    & ((o_m_base + m_span) <= mem_end))
        ok = shape_ok & align_ok & range_ok

        pair = self.join_pair
        seg = self.join_seg
        rd_idx = self.join_rd_idx
        rd_issue = Signal()
        rd_pending = Signal()
        m.d.sync += rd_pending.eq(rd_issue)
        act_cnt = self.join_act_cnt
        act_e = [
            ActWord(self.params, name=f'act_e{i}')
            for i in range(self.mm_act_fifo)
        ]
        resv = Signal(range(self.mm_out_depth + 1))

        start_r = Signal()
        rd_word = ActWord(self.params)
        m.d.comb += [
            rd_word.quants.eq(self.actram.rd_quants),
            rd_word.s_act.eq(self.actram.rd_s_act),
            rd_word.sum_a.eq(self.actram.rd_sum_a),
        ]

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
                with m.If(self.sink.fire & ok):
                    m.d.sync += [
                        cur_fmt.eq(o_fmt),
                        cur_k.eq(o_k),
                        cur_rows.eq(o_rows),
                        cur_p_base.eq(o_p_base),
                        cur_m_base.eq(o_m_base),
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
                unmasked = [((pair << lgn) + p) < pair_row for p in range(n)]
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
                    lane_row = ((pair << lgn) + p)[:self.rows_bits]
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

                m.d.comb += rd_issue.eq((rd_idx != nsegs)
                                        & (act_cnt +
                                           rd_pending < self.mm_act_fifo))
                push = rd_pending
                m.d.comb += self.actram.rd_seg.eq(rd_idx[:self.lg_act_segs])
                with m.If(rd_issue):
                    m.d.sync += rd_idx.eq(rd_idx + 1)
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
                            m.d.sync += [pair.eq(pair + 1), rd_idx.eq(0)]
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
            # P2a banked ACT RAM: the accept pulse commits the loaded
            # bank for reading and rotates the write bank. The core's
            # accept discipline (a stream command is offered only
            # with the loader quiescent) keeps every commit off an
            # in-flight fill; single-bank builds ignore the pulse.
            self.actram.commit.eq(self.start_ok),
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

    def __init__(self, params):
        super().__init__(params)

        self.bus = AXIInterface(data_width=self.data_width,
                                addr_width=self.addr_width,
                                id_width=self.id_width)
        self.rreq = Decoupled(StreamReadReq, self.params)
        self.rresp = Decoupled(StreamReadResp, self.params)

        self.err = Signal()

        # Tag entries for the worst outstanding set the supply's credit
        # discipline admits: every outstanding beat of both streams in
        # its own 1-beat sub-burst, plus one page split per stream.
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
        m.d.comb += direct.eq(~req.bits.beats[8] & (req.bits.beats <= to_page))

        # Split holding register: addr/rem advance per issued sub-burst.
        # Kept as attributes for waveform/testbench observation (the
        # supply discipline).
        busy = self.busy = Signal()
        hold_addr = self.hold_addr = Signal(self.addr_width)
        hold_rem = self.hold_rem = Signal(9)
        hold_meta = Signal()
        hold_page = (0x1000 - (hold_addr & 0xfff)) >> lg
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
        m.d.comb += [
            self.bus.ar.valid.eq(ar_direct | ar_hold),
            self.bus.ar.bits.addr.eq(Mux(ar_direct, req.bits.addr, hold_addr)),
            self.bus.ar.bits.len.eq(
                Mux(ar_direct, req.bits.beats - 1, hold_burst - 1)),
            self.bus.ar.bits.size.eq(lg),
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
        m.d.comb += [
            self.rresp.valid.eq(self.bus.r.valid),
            self.rresp.bits.data.eq(self.bus.r.bits.data),
            self.rresp.bits.meta.eq(tag_q.deq.bits.meta),
            self.bus.r.ready.eq(self.rresp.ready),
            tag_q.deq.ready.eq(r_last),
        ]
        with m.If(r_fire):
            m.d.sync += self.beats.eq(self.beats + 1)
        m.d.comb += self.err.eq(r_fire & (self.bus.r.bits.resp != 0))

        return m
