"""Device-side V-window tile builder for FA shadow maintenance (M5).

The ggml-troom backend mirrors each layer's offloaded F16 KV pool as
device-resident quantized shadows. The K shadow is slot-major Q8_0
rows; the V shadow is per (kv head, 64-slot window) a 64-row
transposed Q8_0 tile whose reduction axis is the slots, so P.V is a
plain MATMUL against the tile. Host-side tile rebuilds cost measured
milliseconds per token (the M5 host half removed the dead-tail
waste; this engine, the M5 RTL half, removes the per-token rebuild
traffic itself).

One ``VBUILD`` descriptor builds one (head, window) tile:

* ``a_base`` names a TMEM image of the pool window -- slot-major, 64
  FP16 dims per slot (a 128-byte row per slot), the first ``k``
  slots valid -- which a preceding ``LOAD_2D`` fills straight from
  the F16 pool (one strided transfer per head slice);
* ``r_base`` names the tile output: 64 rows (one per dim) of two
  Q8_0 blocks over the slots (a 68-byte row), each row zero-padded
  to a whole bus beat -- the beat-padded row stride the backend and
  the matrix unit's Q8_0 fetch already use.

The engine keeps the window in a local 8 KiB buffer, so the
transpose is a per-row gather (buffer word ``j*32 + (d >> 1)`` holds
dims ``2w, 2w+1`` of slot ``j``; halfword ``d & 1`` is the row-``d``
value), never a TMEM traffic pattern. Two gather buffers let the next
block transpose while the current block moves through the quantizer.

Numeric contract: per 32-slot block, exactly the frozen VEC_QUANT
sequence of ``tests/troom/model.py`` (``vec_quant_segments``),
re-expressed over the FP16 window values. The amax folds as a
magnitude fold over the FP16 patterns that never lets a NaN win (the
``Fp16Widen`` of the winning pattern equals the FP32 fmax of the
widened magnitudes for every finite/inf/NaN mix, because widening is
monotonic on magnitudes); then

.. code-block:: text

    a   = widen(max |x|)                     (non-negative)
    sc  = R(a * fp32(1/127));  d16 = fp16_RNE(sc)    (the Q8_0 block d)
    r   = frozen recip sequence of a          (FPURec seed + two Newton
                                               steps; +0 for a == 0 or
                                               a >= 2^126, where the
                                               quants collapse to zero)
    q_i = q8(R(R(widen(x_i) * r) * 127.0))    (RNE INT8, saturating)

Tail slots at and above ``k`` contribute exactly zero: the gather
returns +0 for them, matching the reference's zero-padded segments
and the host builder's dead-slot zero rows. The RTL, the pysim model
(``model.vbuild_quant_row``), and the backend's host-side tile
builder are bit-frozen together, so a V tile is byte-identical no
matter which side built it.

Dead-tail early-out (2026-10-19): because those tail slots quantize
to exactly zero AND their +0 can never win the amax fold, the engine
never streams them -- each 32-slot block gathers and quantizes only
its live prefix (``min(live - 32*blk, 32)`` elements), and a block
whose widened amax is exactly zero (a fully-dead block, or live data
that is all zeros) skips its quant stream entirely, leaving the
block's zero bytes from the row clear. The emitted tile is unchanged
byte for byte; only the work skipped differs.

A descriptor whose live count is outside 1..64 completes at accept
with ``BAD_SHAPE``, whose bases are not beat aligned with
``BAD_ALIGN``, and whose window or tile extent leaves TMEM with
``BAD_RANGE`` -- all without reading TMEM.

SG1 drain mode (E1-V, 2026-10-28): a descriptor whose ``m_base`` is
nonzero stages, instead of the legacy row-major tile, the two SG1
images the streaming engine consumes -- payload (4096 bytes: for each
four-row group, for each 32-slot block, for each lane, the block's 32
quant bytes) at ``r_base`` and meta (256 bytes: the FP16 d of the
same (group, block, lane) cells, row-major four bytes per row) at
``m_base``. The quantized bytes and scales are the identical frozen
values in a different placement; the retired regions are
byte-identical to the host builder's ``troom_sg1_pack`` output for
the same window. The mode requires a 64-byte bus beat (the deployed
geometry) and completes otherwise with ``BAD_SHAPE``; every row's
full contribution is written, so dead-tail and zero-amax skips leave
the same zero bytes the host path emits. ``m_base == 0`` keeps the
legacy drain bit for bit; see
``results/troom-e1-v-20261028/DESIGN.md`` for the ABI rationale.

K orientation (E1-K, 2026-10-28): a descriptor whose ``rows`` field
is 16 (``VB_K_ROWS``; the field is ignored -- always zero -- by every
earlier VBUILD contract) builds a K-cache group instead: the same
slot-major window image now holds the group's 16 slot rows, the
quantizer's output rows are the slots (each a 68-byte two-block Q8_0
row over the 64 dims), and ``k`` names the group's live-slot prefix
(0..16; 0 builds the all-zero group without reading the pool).
Gathering runs along the dim axis within a slot row instead of
across slots, and the staging extents shrink to the group's images:
payload 1024 bytes, meta 64 bytes (one beat). The staged bytes are
byte-identical to the host builder's ``troom_sg1_pack(..., 64, 16)``
output for the same group; the quantizer itself is untouched bit for
bit. K mode requires the SG1 drain (``m_base != 0``) and the
64-byte beat, allows ``rows`` only 0 or 16, and rejects ``k > 16``
-- all at accept with ``BAD_SHAPE``; see
``results/troom-e1-k-20261028/DESIGN.md``.

Partial-extent drain (P2d-a, 2026-11-05): the SG1 staging layout
interleaves both 32-slot V blocks (and every K row) inside each 64 B
beat, so a sliced STORE can retire one V block's payload runs (16 runs
of 128 B at 256 B stride -- beat multiples) or one K lane pair's beats
(2 runs of 64 B at 128 B stride), but never sub-beat meta cells; the
meta must leave as whole beats. A partial descriptor (``rows`` 1, 2 or
17, all requiring ``m_base != 0``) therefore rebuilds only the dirty
extent *and composes the meta beats from the staging the host loaded*:

* ``rows == 1``/``2`` (V partial, block 0/1): the fill reads only the
  named block's live prefix (words ``32*32*blk`` up, i.e. the host
  LOADs just the dirty block's slots re-based at the window image's
  natural offset), every row gathers/quantizes only that block (the
  other block bypasses even the zero-amax scale round), the drain
  writes only that block's payload chunks and meta halfword cells --
  at their *natural* staging offsets -- and each 64 B meta beat is
  pre-read from ``m_base`` into the accumulator as the row pass
  enters its sixteenth row, so the composed beats carry the loaded
  (retained) cells of the other block.
* ``rows == 17`` (K partial): ``k`` is the group's live prefix (1..16).
  The host LOADs only the dirty lane pair re-based at buffer row 0.
  Parity derives the valid input rows (1 for odd ``k``, 2 otherwise).
  Payload stays rebased for a sliced STORE, but meta scatters at the
  original pair base ``(k - 1) & ~1``. All other meta cells retain
  their pre-read bytes and no other payload beats are written.

The staging bytes a partial run writes are bit-identical to the same
extents of a full run over the same inputs (the per-block/per-row
quant sequence is independent of the extent); the unwritten extents
hold the host-loaded previous build's output, and the host's sliced
stores retire exactly the rebuilt extents, so the composed DRAM image
is byte-identical to a full rebuild's. See
``results/troom-p2da-dirtyblock-20261105/DESIGN.md``.

Append mode (P2d-b, 2026-11-06): a descriptor whose ``rows`` field is
18/19 appends ONE slot to an open 32-slot block under the per-token
scale contract (``results/troom-p2db-phasea-20261106/DESIGN.md``):

* the fill reads just the appended slot's 128 B row re-based at the
  buffer (the host LOADs the slot to the window image), and the walk
  is the K-mode gather direction over that one row -- the two
  32-element halves share a SINGLE amax fold, one scale sequence, and
  one d16 written to both block slots of the 68-byte assembly, so the
  64 quant bytes are the frozen sequence of ``model
  .vbuild_quant_row_token``;
* the drain patches, never rewrites: block ``blk``'s 32 payload beats
  are pre-read and recomposed with two bytes each (the appended
  position p in both lanes' chunks, p = (k-1) & 31), and ``m_base``
  names the per-window SCALE ROW (256 B = four beats of 64 WIDENED
  F32 words) whose word k-1 takes this token's widened d16 -- the
  P2d-a meta compose applied to the payload and scale-row extents.
  The row stores FP32, not FP16: the march's P fold multiplies the
  softmax numerators by the widened per-slot scales elementwise
  (VEC_MUL reads F32 words and no widen op exists), so the append
  composes the exact widened value (``Fp16Widen`` of the d16) and
  the backend seeds unappended/sealed words with 1.0 (the exact
  identity under the fold);
* the meta staging is not touched: the open block's cells stay the
  uploaded 1.0s until a rows-1/2 partial (the seal) rewrites the
  block byte-identically to the adopted P2d-a build.

An append descriptor requires the SG1 drain and the 64-byte beat,
rejects ``k`` outside its block's range at accept with ``BAD_SHAPE``,
and checks ``m_base`` as the four-beat (256 B) scale-row extent.
"""

from amaranth import *
from amaranth.hdl.rec import Direction, Record

from room.consts import RoundingMode
from room.fpu import FPFormat, FPUFMA, FPUOperator, FPURec

from troom.consts import Status
from troom.convert import Fp16Widen, Fp32To16
from troom.types import HasTRoomParams
from troom.vector import F127_BITS, INV127_BITS

from roomsoc.interconnect.stream import Decoupled

# Fixed FA window geometry (TROOM_FA_HEAD_DIM and TROOM_FA_WINDOW in
# the backend): 64 slots of 64 FP16 dims each.
VB_SLOTS = 64
VB_DIMS = 64
VB_SLOT_BYTES = VB_DIMS * 2
VB_SLOT_WORDS = VB_SLOT_BYTES // 4
VB_ROW_RAW = 2 * 34  # two Q8_0 blocks: d16 + 32 quants each
VB_SG1_PAYLOAD = VB_DIMS * 2 * 32  # 64 rows x 2 blocks x 32 quant bytes
VB_SG1_META = VB_DIMS * 2 * 2  # 64 rows x 2 blocks x 2-byte FP16 d
VB_SG1_BEAT = 64  # the deployed bus beat; SG1 staging writes whole 64B beats
VB_K_ROWS = 16  # K group slots (rows == 16 selects the K orientation)
VB_K_SG1_PAYLOAD = VB_K_ROWS * 2 * 32  # 16 rows x 2 blocks x 32 quant bytes
VB_K_SG1_META = VB_K_ROWS * 2 * 2  # 16 rows x 2 blocks x 2-byte FP16 d
VB_SG1_SROW = VB_SLOTS * 4  # P2d-b scale row: 64 widened-F32 words

# Partial-extent drain codes (P2d-a, descriptor ``rows`` field). The
# SG1 drain of a partial run writes only the rebuilt extents at their
# natural staging offsets -- the payload chunks and meta halfword
# cells of one 32-slot V block, or the payload beats and meta cells of
# the leading K rows -- and pre-reads each meta staging beat so the
# composed 64 B beats carry the retained cells of the extents the host
# loaded and will not rebuild. The unwritten staging extents are never
# stored: the host slices its LOADs/STOREs to the same extents.
VB_ROWS_V_B0 = 1  # V partial: block-0 extent only (1 <= k <= 32)
VB_ROWS_V_B1 = 2  # V partial: block-1 extent only (33 <= k <= 64)
VB_ROWS_K_PART = VB_K_ROWS + 1  # K partial: rows < k only (1 <= k <= 16)

# Append-mode codes (P2d-b): one slot appended to an open block under
# the per-token scale. ``k`` keeps the window's live prefix (the
# appended slot is k-1), so the block and its position derive from the
# same ranges as the partial codes above.
VB_ROWS_V_APP0 = 18  # V append into open block 0 (1 <= k <= 32)
VB_ROWS_V_APP1 = 19  # V append into open block 1 (33 <= k <= 64)


class VBuildDesc(HasTRoomParams, Record):
    """V-window build descriptor: window base, tile base, live slots.

    A nonzero ``m_base`` selects the SG1 drain: ``r_base`` becomes the
    payload staging base and ``m_base`` the meta staging base. A
    ``rows`` value of ``VB_K_ROWS`` (16) selects the K orientation:
    16 slot output rows over the 64 dims, staging extents 1024/64
    bytes; the partial codes 1/2 (one 32-slot V block extent) and 17
    (one K lane pair) additionally slice the SG1 drain to the
    rebuilt extents; the append codes 18/19 (P2d-b) patch one
    appended slot's bytes into an open block's payload beats and the
    widened-d16 word of the four-beat F32 scale row at ``m_base``;
    every other nonzero ``rows`` is rejected at accept.
    """

    def __init__(self, params, name=None, src_loc_at=0):
        HasTRoomParams.__init__(self, params)
        Record.__init__(self, [
            ('a_base', self.tmem_addr_width, Direction.FANOUT),
            ('r_base', self.tmem_addr_width, Direction.FANOUT),
            ('m_base', self.tmem_addr_width, Direction.FANOUT),
            ('k', self.lg_act_k + 1, Direction.FANOUT),
            ('rows', self.rows_bits, Direction.FANOUT),
        ],
                        name=name,
                        src_loc_at=src_loc_at + 1)


class VBuildUnit(HasTRoomParams, Elaboratable):
    """Transposing Q8_0 tile builder over an FP16 window in TMEM.

    A descriptor is accepted in ``IDLE``; a rejected one completes at
    accept without side effects. A well-formed descriptor fills its
    local window buffer from TMEM, builds all 64 tile rows, streams
    the padded tile back to TMEM, and completes (``done``) when the
    final tile beat commits. The unit runs one command at a time.
    """

    def __init__(self, params):
        super().__init__(params)

        self.sink = Decoupled(VBuildDesc, self.params)
        self.tmem_rreq = Decoupled(
            Record, [('addr', self.tmem_addr_width, Direction.FANOUT)])
        self.tmem_rresp = Decoupled(
            Record, [('data', self.data_width, Direction.FANOUT)])
        self.tmem_w = Decoupled(
            Record, [('addr', self.tmem_addr_width, Direction.FANOUT),
                     ('data', self.data_width, Direction.FANOUT)])

        self.done = Signal()
        self.status = Signal(Status)
        self.active = Signal()

        # Observation counters.
        self.tiles = Signal(48)
        self.rows = Signal(48)
        self.beats = Signal(48)
        self.run_cycles = Signal(48)
        self.fill_cycles = Signal(48)
        self.gather_cycles = Signal(48)
        self.scale_cycles = Signal(48)
        self.quant_cycles = Signal(48)
        self.drain_cycles = Signal(48)
        self.dead_blocks = Signal(48)
        self.zero_blocks = Signal(48)

        self.row_pad = -(-VB_ROW_RAW // self.beat_bytes) * self.beat_bytes
        if VB_SLOT_BYTES % self.beat_bytes:
            raise ValueError('a slot row must be whole beats')

    def elaborate(self, platform):
        m = Module()

        lg = self.lg_beat
        bb = self.beat_bytes
        aw = self.tmem_addr_width
        bw = self.data_width // 32

        fmul_s = m.submodules.fmul_s = FPUFMA(32, FPFormat.S, latency=3)
        fmul_q1 = m.submodules.fmul_q1 = FPUFMA(32, FPFormat.S, latency=3)
        fmul_q2 = m.submodules.fmul_q2 = FPUFMA(32, FPFormat.S, latency=3)
        fadd_s = m.submodules.fadd_s = FPUFMA(32, FPFormat.S, latency=3)
        frec = m.submodules.frec = FPURec(32, FPFormat.S, latency=3)
        f16w = m.submodules.f16w = Fp16Widen()
        f16n = m.submodules.f16n = Fp32To16()
        # P2d-b: the append's dedicated d16 widen (the token scale's
        # F32 row word). Not shared with f16w: the scale completion
        # issues it right where the quant stream's element widens are
        # about to start, and the shared unit's two-cycle pipeline
        # would collide at the QUANT entry.
        f16wa = m.submodules.f16wa = Fp16Widen()

        # Local window buffer: 64 slots x 32 words. One sync write
        # port (the fill) and one synchronous read port (the gather),
        # allowing block RAM inference. The phases are disjoint.
        buf = Memory(width=32, depth=VB_SLOTS * VB_SLOT_WORDS)
        buf_w = m.submodules.buf_w = buf.write_port()
        buf_r = m.submodules.buf_r = buf.read_port(transparent=False)

        # ------------------------------------------------------------------
        # Descriptor validation, on the offered combination. A nonzero
        # m_base selects the SG1 drain: its staging extents are the
        # payload/meta images (4096/256 bytes) instead of the legacy
        # tile, and the mode itself requires the deployed 64-byte beat.
        # A rows value of VB_K_ROWS selects the K orientation: 16 slot
        # rows over the dims, group staging extents (1024/64 bytes), k
        # = live slots 0..16, and the SG1 drain; any other nonzero
        # rows is a shape error.
        # ------------------------------------------------------------------
        offer = self.sink.bits
        offer_sg1 = offer.m_base != 0
        # The K orientation: the full code (16) and the partial
        # row-extent code (17) share the 16-row geometry.
        offer_krow = (offer.rows == VB_K_ROWS) \
            | (offer.rows == VB_ROWS_K_PART)
        offer_vb0 = offer.rows == VB_ROWS_V_B0
        offer_vb1 = offer.rows == VB_ROWS_V_B1
        offer_kp = offer.rows == VB_ROWS_K_PART
        offer_va0 = offer.rows == VB_ROWS_V_APP0
        offer_va1 = offer.rows == VB_ROWS_V_APP1
        offer_va = offer_va0 | offer_va1
        # K partial k carries the group's live prefix, not merely the
        # rebased input length. Keep the pair's original metadata position.
        offer_live = Mux(offer_kp, Mux(offer.k[0], 1, 2), offer.k)
        # Partial-extent code latched for the run: 0 full, 1 V block 0,
        # 2 V block 1, 3 K row extent.
        offer_part = Mux(offer_vb0, 1, Mux(offer_vb1, 2, Mux(offer_kp, 3, 0)))
        a_end_o = Mux(offer_va, offer.a_base + VB_SLOT_BYTES,
                      offer.a_base + offer_live * VB_SLOT_BYTES)
        r_end_o = Mux(
            offer_krow, offer.r_base + VB_K_SG1_PAYLOAD,
            Mux(offer_sg1, offer.r_base + VB_SG1_PAYLOAD,
                offer.r_base + 64 * self.row_pad))
        # An append's m_base names the four-beat F32 scale row, not
        # the 256-B meta staging (the same extent, different content).
        m_end_o = Mux(
            offer_va, offer.m_base + VB_SG1_SROW,
            Mux(offer_krow, offer.m_base + VB_K_SG1_META,
                offer.m_base + VB_SG1_META))
        shape_bad = ((offer.rows != 0) & ~offer_krow & ~offer_vb0
                     & ~offer_vb1 & ~offer_kp & ~offer_va) \
            | Mux(offer_krow,
                  ~offer_sg1 | (offer.k > VB_K_ROWS),
                  (offer.k == 0) | (offer.k > VB_SLOTS)) \
            | Mux(offer_vb0,
                  ~offer_sg1 | (offer.k == 0) | (offer.k > 32), 0) \
            | Mux(offer_vb1,
                  ~offer_sg1 | (offer.k <= 32) | (offer.k > VB_SLOTS), 0) \
            | Mux(offer_kp,
                  ~offer_sg1 | (offer.k == 0) | (offer.k > VB_K_ROWS), 0) \
            | Mux(offer_va0,
                  ~offer_sg1 | (offer.k == 0) | (offer.k > 32), 0) \
            | Mux(offer_va1,
                  ~offer_sg1 | (offer.k <= 32) | (offer.k > VB_SLOTS), 0) \
            | (offer_sg1 & (self.beat_bytes != VB_SG1_BEAT))
        align_bad = offer.a_base[:lg].any() | offer.r_base[:lg].any() \
            | (offer_sg1 & offer.m_base[:lg].any())
        range_bad = (a_end_o > self.tmem_bytes) \
            | (r_end_o > self.tmem_bytes) \
            | (offer_sg1 & (m_end_o > self.tmem_bytes))
        bad = shape_bad | align_bad | range_bad

        # Latched run state.
        a_base = Signal(aw)
        r_base = Signal(aw)
        m_base = Signal(aw)
        sg1 = Signal()  # SG1 drain (payload+meta staging), not legacy tile
        krow = Signal()  # K orientation: 16 slot rows over the dims
        part = Signal(2)  # partial-extent code (see offer_part)
        live = Signal(7)
        pair_base = Signal(4)
        part_vb0 = part == 1
        part_vb1 = part == 2
        part_vb = part_vb0 | part_vb1  # V block-extent partial
        part_blk = part == 2  # the rebuilt V block index
        part_k = part == 3  # K row-extent partial
        vapp = Signal()  # append mode: one slot under the token scale
        app_blk = Signal()  # the appended-to open block
        app_pos = Signal(5)  # the slot's position inside that block
        app_w = Signal(6)  # append drain beat: 0..31 payload, 32..35 srow
        app_ph = Signal()  # beat latched, write pending
        app_rd = Signal()  # a beat pre-read is outstanding
        app_d32 = Signal(32)  # the widened token d16 (the row's patch word)

        # Phase membership, driven inside the FSM below.
        in_fill = Signal()
        in_gather = Signal()
        in_scale = Signal()
        in_quant = Signal()
        in_drain = Signal()
        in_sg1w = Signal()  # SG1 staging beat writes
        in_appw = Signal()  # append-mode patch drain
        running = in_fill | in_gather | in_scale | in_quant | in_drain \
            | in_sg1w | in_appw

        # ------------------------------------------------------------------
        # Fill: one outstanding TMEM read at a time; each returned
        # beat is absorbed into the buffer one 32-bit word per cycle.
        # A block-1 partial fills from word 32*32 up (the host LOADs
        # just the dirty block's slots re-based at the window image's
        # natural offset), so the read address tracks the same offset.
        # ------------------------------------------------------------------
        fill_word = Signal(12)  # words stored
        fill_total = Signal(12)  # words to store (live * 32)
        fill_wlo = Signal(12)  # first buffer word of the fill
        fill_beat = Signal(12)  # beats requested
        fill_data = Signal(self.data_width)
        fill_have = Signal()  # a beat is being absorbed
        fill_wcnt = Signal(range(bw + 1))
        rd_pend = Signal()

        # ------------------------------------------------------------------
        # Meta staging pre-read (partial extents): before the row pass
        # writes a 64 B meta beat, the beat's current staging content
        # is read into the meta_hw accumulator, so the composed beats
        # carry the retained cells of the extents the host loaded and
        # this run does not rebuild. One beat covers sixteen rows, and
        # the trigger fires as the pass enters its first row -- dozens
        # of cycles before that row's scatter -- with a DRAINLOAD-side
        # wait closing the race for the fastest (zero-amax) rows. The
        # response latch lives beside the meta accumulator below, so a
        # same-cycle scatter (priority: the scatter's fresh d16) always
        # wins over the pre-read's retained byte.
        # ------------------------------------------------------------------
        pre_rd = Signal()  # a pre-read beat is outstanding
        pre_cnt = Signal(3)  # next meta beat to pre-read (V: 0..3, K: 0)

        # One beat at a time: the next request issues only after the
        # previous beat fully absorbed into the buffer, so a returned
        # beat can never clobber an absorption still in flight. The
        # pre-read shares the read port after the fill completes (the
        # phases are disjoint; a response routes by its pending flag).
        more_fill = ~rd_pend & ~fill_have & (fill_beat * bw < fill_total)
        m.d.comb += [
            buf_w.addr.eq(fill_word),
            buf_w.data.eq(fill_data[:32]),
            buf_w.en.eq(fill_have),
        ]
        with m.If(fill_have):
            m.d.sync += [
                fill_data.eq(fill_data >> 32),
                fill_word.eq(fill_word + 1),
                fill_wcnt.eq(fill_wcnt + 1),
            ]
            with m.If(fill_wcnt + 1 == bw):
                m.d.sync += fill_have.eq(0)

        # ------------------------------------------------------------------
        # Gather: one element per cycle. The fold keeps the winning FP16
        # magnitude pattern; NaN patterns never win (>= 0x7c01), and
        # tail slots at or above `live` read as +0. In V orientation
        # the element is slot `blk*32+gi` of dim row `row`; in K
        # orientation it is dim `blk*32+gi` of slot row `row`, so the
        # gather walks inside one buffer row and the liveness test is
        # whole-row (`row < live`).
        # ------------------------------------------------------------------
        row = Signal(6)
        blk = Signal()
        gi = Signal(6)  # requests issued, including the terminal count 32
        gather_valid = Signal()
        gather_index = Signal(5)
        gather_live = Signal()
        gather_par = Signal()  # K: the issued element's dim parity
        xmax16 = Signal(16)
        xnan = Signal()  # a live slot of this block is a NaN pattern
        xbuf0 = Array(Signal(16, name=f'xb0_{i}') for i in range(32))
        xbuf1 = Array(Signal(16, name=f'xb1_{i}') for i in range(32))
        cur_bank = Signal()

        # Gather the next block while the current block quantizes.
        pgi = Signal(6)
        pgather_valid = Signal()
        pgather_index = Signal(5)
        pgather_live = Signal()
        pgather_par = Signal()
        pxmax16 = Signal(16)
        pxnan = Signal()
        prefetch_done = Signal()

        # The last output row: dim 63 (V) or slot 15 (K).
        last_row = Mux(krow, VB_K_ROWS - 1, VB_DIMS - 1)
        has_next = (blk == 0) | (row != last_row)
        next_blk = Mux(blk == 0, 1, 0)
        next_row = Mux(blk == 0, row, row + 1)

        # Dead-tail early-out: the live prefix of block b is
        # clamp(live - 32*b, 0, 32). Slots at or above it quantize to
        # exactly zero bytes and fold +0 into the amax, so gathering
        # or streaming them is skippable work, never a numeric change.
        # In K orientation liveness is whole-row instead: a live slot's
        # two blocks are always full (the dims are always all present)
        # and a dead slot row gathers nothing.
        lo_live = Signal(6)  # live slots of block 0 (1..32)
        hi_live = Signal(6)  # live slots of block 1 (0..32)
        m.d.comb += [
            lo_live.eq(Mux(live > 32, 32, live)),
            hi_live.eq(Mux(live > 32, (live - 32)[:6], 0)),
        ]
        # A V partial run never gathers the unselected block: its
        # staging extents are the host-loaded previous build's bytes,
        # so skipping the zero-amax scale round as well changes no
        # output byte -- only the rebuilt extent ever reaches staging.
        lo_eff = Mux(part_vb1, 0, lo_live)
        hi_eff = Mux(part_vb0, 0, hi_live)
        # A K row-extent run at or above its live prefix: the row's
        # meta cells keep the pre-read (loaded) bytes instead of the
        # zeros a full run's dead tail writes.
        part_k_row_dead = part_k & (row >= live)
        cur_live = Mux(krow, Mux(row < live, 32, 0),
                       Mux(blk == 0, lo_eff, hi_eff))
        # An append never prefetches: its second gather half is the
        # same row's dims 32..63, not another row.
        nxt_live = Mux(
            vapp, 0,
            Mux(krow, Mux(next_row < live, 32, 0), Mux(blk == 0, hi_eff,
                                                       lo_eff)))

        # Pre-read issue (see the fill-side comment above): fires as
        # the row pass gathers the first row of a meta beat's sixteen.
        pre_go = (part != 0) & ~pre_rd \
            & (pre_cnt < Mux(krow, 1, 4)) & (row[0:4] == 0) \
            & (row[4:6] == pre_cnt[0:2]) & in_gather
        pre_addr = m_base + (pre_cnt[0:2] << 6)

        # Append-mode pre-read: one outstanding beat at a time, the
        # patch drain's compose source. Beat w < 32 is block app_blk's
        # payload beat (group w>>1, pair w&1); beats 32..35 are the
        # four-beat F32 scale row at m_base.
        app_req = in_appw & ~app_rd & ~app_ph
        app_raddr = Mux(
            app_w[5], m_base + (app_w[:5] << 6),
            r_base + (app_w[1:6] << 8) + (app_w[0] << 6) + (app_blk << 7))

        # The unit's single TMEM read port: the fill while filling,
        # then the meta pre-reads of a partial run and the append
        # drain's beat pre-reads.
        m.d.comb += [
            self.tmem_rreq.valid.eq((in_fill & more_fill) | pre_go
                                    | app_req),
            self.tmem_rreq.bits.addr.eq(
                Mux(
                    app_req, app_raddr,
                    Mux(pre_go, pre_addr,
                        a_base + (fill_wlo << 2) + (fill_beat << lg)))),
            self.tmem_rresp.ready.eq(running),
        ]
        with m.If(in_fill & more_fill & self.tmem_rreq.fire):
            m.d.sync += [rd_pend.eq(1), fill_beat.eq(fill_beat + 1)]
        with m.If(pre_go & self.tmem_rreq.fire):
            m.d.sync += pre_rd.eq(1)
        with m.If(app_req & self.tmem_rreq.fire):
            m.d.sync += app_rd.eq(1)
        with m.If(rd_pend & self.tmem_rresp.fire):
            m.d.sync += [
                rd_pend.eq(0),
                fill_have.eq(1),
                fill_data.eq(self.tmem_rresp.bits.data),
                fill_wcnt.eq(0),
            ]

        # Buffer word j*32 + (d >> 1) is Cat(d[1:6], i, b); the
        # halfword is d & 1. Delay the index and live mask alongside
        # the RAM read; row/blk stay fixed until the last response lands.
        # K orientation: word row*32 + (blk*32+gi >> 1), halfword gi & 1.
        gather_go = in_gather & (gi < cur_live)
        prefetch_go = in_quant & has_next & (pgi < nxt_live)
        m.d.comb += [
            buf_r.addr.eq(
                Mux(
                    krow,
                    Mux(prefetch_go, Cat(pgi[1:5], next_blk, next_row[:6]),
                        Cat(gi[1:5], blk, row[:6])),
                    Mux(prefetch_go, Cat(next_row[1:6], pgi[:5], next_blk),
                        Cat(row[1:6], gi[:5], blk)))),
            buf_r.en.eq(gather_go | prefetch_go),
        ]
        m.d.sync += [
            gather_valid.eq(gather_go),
            pgather_valid.eq(prefetch_go),
        ]
        with m.If(gather_go):
            m.d.sync += [
                gi.eq(gi + 1),
                gather_index.eq(gi[:5]),
                gather_live.eq(Mux(krow, row < live,
                                   Cat(gi[:5], blk) < live)),
                gather_par.eq(gi[0]),
            ]
        with m.If(prefetch_go):
            m.d.sync += [
                pgi.eq(pgi + 1),
                pgather_index.eq(pgi[:5]),
                pgather_live.eq(
                    Mux(krow, next_row < live,
                        Cat(pgi[:5], next_blk) < live)),
                pgather_par.eq(pgi[0]),
            ]
        xh_now = Signal(16)
        mag_now = Signal(16)
        nan_now = Signal()
        m.d.comb += [
            xh_now.eq(
                Mux(gather_live, (buf_r.data >>
                                  (16 * Mux(krow, gather_par, row[0])))[:16],
                    0)),
            mag_now.eq(xh_now & 0x7fff),
            nan_now.eq(mag_now >= 0x7c01),
        ]
        with m.If(in_gather & gather_valid):
            with m.If(cur_bank):
                m.d.sync += xbuf1[gather_index].eq(xh_now)
            with m.Else():
                m.d.sync += xbuf0[gather_index].eq(xh_now)
            with m.If(nan_now):
                m.d.sync += xnan.eq(1)
            with m.If(~nan_now & (mag_now > xmax16)):
                m.d.sync += xmax16.eq(mag_now)

        pxh_now = Signal(16)
        pmag_now = Signal(16)
        pnan_now = Signal()
        m.d.comb += [
            pxh_now.eq(
                Mux(pgather_live,
                    (buf_r.data >>
                     (16 * Mux(krow, pgather_par, next_row[0])))[:16], 0)),
            pmag_now.eq(pxh_now & 0x7fff),
            pnan_now.eq(pmag_now >= 0x7c01),
        ]
        with m.If(in_quant & pgather_valid):
            with m.If(cur_bank):
                m.d.sync += xbuf0[pgather_index].eq(pxh_now)
            with m.Else():
                m.d.sync += xbuf1[pgather_index].eq(pxh_now)
            with m.If(pnan_now):
                m.d.sync += pxnan.eq(1)
            with m.If(~pnan_now & (pmag_now > pxmax16)):
                m.d.sync += pxmax16.eq(pmag_now)
            with m.If(pgather_index == nxt_live - 1):
                m.d.sync += prefetch_done.eq(1)

        # ------------------------------------------------------------------
        # Scale micro-sequencer (steps issue at even numbers and
        # capture at odd ones, mirroring troom.vector's VEC_QUANT
        # SCALE phase):
        #   0     issue f16w(xmax16)
        #   1     capture a; classify: a == 0 short-circuits (sc, r,
        #         d16 <- 0), a >= 2^126 jumps to the scale product,
        #         otherwise seed + two Newton iterations
        #   2/3   seed: issue frec(a), capture y
        #   4/5   t = R(a*y)        6/7  t = R(2-t)      8/9   y = R(y*t)
        #   10..15  the second iteration
        #   16/17 sc = R(a * 1/127); then d16 = fp16_RNE(sc)
        # ------------------------------------------------------------------
        s_step = Signal(5)
        s_wait = Signal()
        zero_blk = Signal()  # this block's amax classified exactly zero
        a_bits = Signal(32)
        q_y = Signal(32)
        q_t = Signal(32)
        sc = Signal(32)
        r_inv = Signal(32)
        d16 = Signal(16)
        nrm_pend = Signal()  # sc captured, awaiting the narrow
        nrm_iss = Signal()
        scale_done = Signal()

        s_issue = in_scale & ~s_wait & (s_step != 0) & (s_step != 1)
        s_seed_go = s_issue & (s_step == 2)
        s_it_go = s_issue & ((s_step == 4) | (s_step == 10))
        s_sub_go = s_issue & ((s_step == 6) | (s_step == 12))
        s_upd_go = s_issue & ((s_step == 8) | (s_step == 14))
        s_sc_go = s_issue & (s_step == 16)

        # The classify reads the arriving widened amax directly.
        w_out = f16w.out
        w_e = w_out[23:31]

        with m.If(in_scale & ~s_wait & (s_step == 0)):
            m.d.sync += [s_step.eq(1), s_wait.eq(1)]
        with m.If(in_scale & s_wait):
            with m.If((s_step == 1) & f16w.out_valid):
                m.d.sync += a_bits.eq(w_out)
                with m.If(w_e == 0):
                    # Zero block: park the sequencer awaiting a widen
                    # that never comes (step 0 must not re-issue while
                    # the FSM drains scale_done into the skip below --
                    # a ghost element would enter the shared widen
                    # pipeline).
                    m.d.sync += [
                        sc.eq(0),
                        r_inv.eq(0),
                        d16.eq(0),
                        zero_blk.eq(1),
                        s_step.eq(1),
                        s_wait.eq(1),
                        scale_done.eq(1),
                    ]
                with m.Elif(w_e >= 253):
                    m.d.sync += [r_inv.eq(0), s_step.eq(16), s_wait.eq(0)]
                with m.Else():
                    m.d.sync += [s_step.eq(2), s_wait.eq(0)]
            with m.Elif((s_step == 3) & frec.out.valid):
                m.d.sync += [
                    q_y.eq(frec.out.bits.data),
                    s_step.eq(4),
                    s_wait.eq(0)
                ]
            with m.Elif(((s_step == 5) | (s_step == 11)) & fmul_s.out.valid):
                m.d.sync += [
                    q_t.eq(fmul_s.out.bits.data),
                    s_step.eq(s_step + 1),
                    s_wait.eq(0)
                ]
            with m.Elif(((s_step == 7) | (s_step == 13)) & fadd_s.out.valid):
                m.d.sync += [
                    q_t.eq(fadd_s.out.bits.data),
                    s_step.eq(s_step + 1),
                    s_wait.eq(0)
                ]
            with m.Elif(((s_step == 9) | (s_step == 15)) & fmul_s.out.valid):
                m.d.sync += [
                    q_y.eq(fmul_s.out.bits.data),
                    s_step.eq(s_step + 1),
                    s_wait.eq(0)
                ]
                # The last Newton product is the quantizer's r.
                with m.If(s_step == 15):
                    m.d.sync += r_inv.eq(fmul_s.out.bits.data)
            with m.Elif((s_step == 17) & fmul_s.out.valid):
                m.d.sync += [
                    sc.eq(fmul_s.out.bits.data),
                    s_wait.eq(0),
                    nrm_pend.eq(1),
                    nrm_iss.eq(0),
                ]
        # An issue step advances to its capture step under the wait
        # flag; the capture blocks above clear it.
        with m.If(s_seed_go | s_it_go | s_sub_go | s_upd_go | s_sc_go):
            m.d.sync += [s_step.eq(s_step + 1), s_wait.eq(1)]

        # The scale product's FP32->FP16 narrowing.
        m.d.comb += [
            f16n.inp_valid.eq(nrm_pend & ~nrm_iss),
            f16n.inp.eq(sc),
        ]
        with m.If(nrm_pend & ~nrm_iss):
            m.d.sync += nrm_iss.eq(1)
        with m.If(nrm_pend & nrm_iss & f16n.out_valid):
            m.d.sync += [
                d16.eq(f16n.out),
                nrm_pend.eq(0),
                scale_done.eq(1),
            ]

        # FP unit issue decode; the contexts are mutually exclusive.
        m.d.comb += [
            fmul_s.inp.valid.eq(s_it_go | s_upd_go | s_sc_go),
            fmul_s.inp.bits.fn.eq(FPUOperator.MUL),
            fmul_s.inp.bits.rm.eq(RoundingMode.RNE),
            fmul_s.inp.bits.in3.eq(0),
            fmul_s.inp.bits.in1.eq(
                Mux(s_sc_go, a_bits, Mux(s_upd_go, q_y, a_bits))),
            fmul_s.inp.bits.in2.eq(
                Mux(s_sc_go, Const(INV127_BITS, 32), Mux(s_upd_go, q_t, q_y))),
            fadd_s.inp.valid.eq(s_sub_go),
            fadd_s.inp.bits.fn.eq(FPUOperator.ADD),
            fadd_s.inp.bits.fn_mod.eq(1),
            fadd_s.inp.bits.rm.eq(RoundingMode.RNE),
            fadd_s.inp.bits.in1.eq(Const(0x40000000, 32)),
            fadd_s.inp.bits.in2.eq(0),
            fadd_s.inp.bits.in3.eq(q_t),
            frec.inp.valid.eq(s_seed_go),
            frec.inp.bits.fn.eq(FPUOperator.REC),
            frec.inp.bits.fn_mod.eq(0),
            frec.inp.bits.rm.eq(RoundingMode.RNE),
            frec.inp.bits.in1.eq(a_bits),
            frec.inp.bits.in2.eq(0),
            frec.inp.bits.in3.eq(0),
        ]

        # ------------------------------------------------------------------
        # Quant stream: x -> widen -> R(x * r) -> R(t * 127) -> q8,
        # one element issued per cycle. The q8 byte writes the row in
        # issue order on the second multiplier's landing, so the row
        # byte cursor advances once per landed element.
        # ------------------------------------------------------------------
        q_iss = Signal(6)
        q_land = Signal(6)
        wbyte = Signal(7)  # next row byte to write, 0..67
        # The row assembly keeps 17 little-endian words; quant bytes
        # land strictly in order (word wbyte >> 2, byte wbyte & 3),
        # and each block's d16 pair is static: block 0 in the low
        # half of word 0, block 1 in the high half of word 8 (byte
        # 34). Dynamic whole-row bit selects synthesize as a full
        # crossbar, so the writes stay word-cased.
        rowwords = Array(Signal(32, name=f'rw{i}') for i in range(17))
        rowbuf = Cat(rowwords)

        # The amax widen and the element stream share Fp16Widen; the
        # scale phase completes before the quant phase starts.
        amax_widen_go = in_scale & ~s_wait & (s_step == 0)
        quant_go = in_quant & (q_iss != cur_live)
        quant_x = Signal(16)
        f16w_go = amax_widen_go | quant_go
        m.d.comb += [
            quant_x.eq(Mux(cur_bank, xbuf1[q_iss[:5]], xbuf0[q_iss[:5]])),
            f16w.inp_valid.eq(f16w_go),
            f16w.inp.eq(Mux(in_scale, xmax16, quant_x)),
        ]

        # The append's dedicated d16 widen: issued once at the scale
        # completion, latched two cycles later; the drain reads the
        # word long after (the whole quant walk runs between).
        with m.If(f16wa.out_valid):
            m.d.sync += app_d32.eq(f16wa.out)

        # Bounded RNE int8 helper of the second product (the frozen
        # bit helper of troom.vector / q8_rne_bits in the model).
        # The result is registered once: the row-write switch below
        # references the register, so the helper's barrel shifters
        # are not replicated per case.
        p_bits = fmul_q2.out.bits.data
        qp_e = p_bits[23:31]
        qp_man = Cat(p_bits[:23], Const(1, 1))
        qp_sh = (Const(150, 9) - qp_e.as_signed())[:5].as_unsigned()
        qp_k = (qp_sh - Const(1, 5)).as_unsigned()
        qp_ip = (qp_man >> qp_sh)[:8]
        qp_guard = (qp_man >> qp_k)[0]
        qp_sticky = (qp_man << (Const(24, 6) - qp_k).as_unsigned())[:24].any()
        qp_rne = qp_ip[:8] + Mux(qp_guard & (qp_sticky | qp_ip[0]), Const(
            1, 8), Const(0, 8))
        qp_val = Mux(qp_e < 126, Const(0, 8),
                     Mux(qp_rne > 127, Const(127, 8), qp_rne[:8]))
        qp_sat = qp_e >= 134
        q_byte = Mux(p_bits[31],
                     Mux(qp_sat, Const(0x80, 8), (-qp_val)[:8].as_unsigned()),
                     Mux(qp_sat, Const(0x7f, 8), qp_val))
        q_byte_r = Signal(8)
        q_land_v = Signal()  # fmul_q2.out.valid delayed one cycle
        m.d.sync += [
            q_byte_r.eq(q_byte),
            q_land_v.eq(fmul_q2.out.valid),
        ]

        m.d.comb += [
            fmul_q1.inp.valid.eq(f16w.out_valid & in_quant),
            fmul_q1.inp.bits.fn.eq(FPUOperator.MUL),
            fmul_q1.inp.bits.rm.eq(RoundingMode.RNE),
            fmul_q1.inp.bits.in3.eq(0),
            fmul_q1.inp.bits.in1.eq(f16w.out),
            fmul_q1.inp.bits.in2.eq(r_inv),
            fmul_q2.inp.valid.eq(fmul_q1.out.valid & in_quant),
            fmul_q2.inp.bits.fn.eq(FPUOperator.MUL),
            fmul_q2.inp.bits.rm.eq(RoundingMode.RNE),
            fmul_q2.inp.bits.in3.eq(0),
            fmul_q2.inp.bits.in1.eq(fmul_q1.out.bits.data),
            fmul_q2.inp.bits.in2.eq(Const(F127_BITS, 32)),
        ]
        with m.If(quant_go):
            m.d.sync += q_iss.eq(q_iss + 1)

        # The byte lands in issue order one cycle after the second
        # product completes (the q8 helper's registered result).
        land_now = in_quant & q_land_v
        with m.If(land_now):
            with m.Switch(wbyte[2:7]):
                for k in range(17):
                    with m.Case(k):
                        # Static byte lanes avoid dynamic bit-select
                        # lowering's partially assigned RTL temporaries.
                        with m.Switch(wbyte[:2]):
                            for lane in range(4):
                                with m.Case(lane):
                                    m.d.sync += rowwords[k][8 * lane:8 *
                                                            (lane +
                                                             1)].eq(q_byte_r)
            m.d.sync += [
                wbyte.eq(wbyte + 1),
                q_land.eq(q_land + 1),
            ]

        # ------------------------------------------------------------------
        # Tile drain: the padded row shifts out as whole beats.
        # ------------------------------------------------------------------
        beats_per_row = self.row_pad // bb
        dbeat = Signal(range(beats_per_row + 1))
        drain_sr = Signal(8 * self.row_pad)
        drain_prefetched = Signal()

        # ------------------------------------------------------------------
        # SG1 staging (nonzero m_base, deployed 64-byte beat only): the
        # row's two 32-byte quant chunks and d16 pair land in the
        # stream consumption order -- group-major, block within group,
        # lane within block. Byte offsets from DESIGN.md:
        #   chunk(row, j) -> r_base + ((row >> 2) * 8 + j * 4 + (row & 3)) * 32
        #   d16(row, j)   -> m_base + (row >> 4) * 64 + ((row >> 2) & 3) * 16
        #                    + j * 8 + (row & 3) * 2
        # A 64-byte payload beat covers a lane pair of one block, so
        # beats are written when an odd lane completes (the even lane's
        # chunks wait in hold registers); one meta beat covers sixteen
        # rows (four groups) and is written when row & 15 == 15, each
        # row scattering its two d16 halfwords to their (group, block,
        # lane) cells. Dead-tail and zero-amax skips leave zeros in
        # rowwords, and every row's full contribution is written, so
        # staging needs no pre-clear.
        # ------------------------------------------------------------------
        sg1_widx = Signal(2)
        if self.beat_bytes == VB_SG1_BEAT:
            # Little-endian byte order: the row's LOW bytes are the
            # chunk's LOW bits, so the Cat runs from the chunk's byte
            # 0 (row byte 2) upward.
            chunk0 = Cat(rowwords[0][16:32], rowwords[1], rowwords[2],
                         rowwords[3], rowwords[4], rowwords[5], rowwords[6],
                         rowwords[7], rowwords[8][0:16])
            chunk1 = Cat(rowwords[9], rowwords[10], rowwords[11], rowwords[12],
                         rowwords[13], rowwords[14], rowwords[15],
                         rowwords[16])
            hold_c0 = Signal(256, name='hold_c0')
            hold_c1 = Signal(256, name='hold_c1')
            # One halfword cell per (group, block, lane) of a 64-byte
            # meta beat; an Array (not a dynamic bit-select on one
            # wide signal, which lowers to a latched comb
            # read-modify-write) keeps the writes latch-free.
            meta_hw = Array(Signal(16, name=f'mhw{i}') for i in range(32))
            # Pre-read latch (partial runs): the loaded beat's 32
            # halfwords enter the accumulator here -- ABOVE the row
            # scatter below in elaboration order, so a same-cycle
            # scatter (a freshly quantized d16 of this very row) wins
            # the cell, exactly the compose a partial run needs.
            with m.If(pre_rd & self.tmem_rresp.fire):
                m.d.sync += [
                    pre_rd.eq(0),
                    pre_cnt.eq(pre_cnt + 1),
                ]
                for i in range(32):
                    m.d.sync += meta_hw[i].eq(
                        self.tmem_rresp.bits.data[16 * i:16 * i + 16])
            # Append-mode compose (P2d-b): latch the pre-read beat the
            # same way, then scatter this run's two patch bytes (a
            # payload beat: the appended position of both lanes'
            # chunks) or the token d16 (the scale-row beat) into the
            # LATCHED response -- the compose must not race the
            # accumulator's old content, so the patched halfwords are
            # rebuilt from the response directly.
            with m.If(app_rd & self.tmem_rresp.fire):
                m.d.sync += [app_rd.eq(0), app_ph.eq(1)]
                for i in range(32):
                    m.d.sync += meta_hw[i].eq(
                        self.tmem_rresp.bits.data[16 * i:16 * i + 16])
            app_hw0 = app_pos[1:5]
            app_hw1 = Cat(app_pos[1:5], 1)  # (p >> 1) + 16
            app_rh0 = self.tmem_rresp.bits.data.word_select(app_hw0, 16)
            app_rh1 = self.tmem_rresp.bits.data.word_select(app_hw1, 16)
            # Beat w's lanes are dims 2w and 2w+1: chunk 0 below w 16,
            # chunk 1 above, byte 2*(w mod 16) (+1 for the odd lane).
            app_we = Cat(0, app_w[0:4])
            app_wo = Cat(1, app_w[0:4])
            app_be = Mux(~app_w[4], chunk0.word_select(app_we, 8),
                         chunk1.word_select(app_we, 8))
            app_bo = Mux(~app_w[4], chunk0.word_select(app_wo, 8),
                         chunk1.word_select(app_wo, 8))
            with m.If(app_rd & self.tmem_rresp.fire & ~app_w[5]):
                # Byte p of the beat is the even lane's chunk, 32+p the
                # odd lane's; the lane within the halfword is p & 1.
                with m.If(app_pos[0]):
                    m.d.sync += meta_hw[app_hw0].eq(Cat(app_rh0[:8], app_be))
                    m.d.sync += meta_hw[app_hw1].eq(Cat(app_rh1[:8], app_bo))
                with m.Else():
                    m.d.sync += meta_hw[app_hw0].eq(Cat(app_be, app_rh0[8:]))
                    m.d.sync += meta_hw[app_hw1].eq(Cat(app_bo, app_rh1[8:]))
            # Scale-row beats (app_w >= 32): patch WORD k-1 of the
            # 256-B F32 row with the widened token d16. The word spans
            # halfword cells 2*((k-1)&15) and +1 of beat
            # 32 + ((k-1) >> 4); every other cell keeps the latched
            # response (the 1.0s the host seeded and the previous
            # appends' words). Cat is LSB-first in this tree: the
            # beat index is (blk << 1) | p[4] and the cell base is
            # (p & 15) << 1.
            app_sw0 = Cat(0, app_pos[0:4])  # 2 * ((k-1) & 15)
            with m.If(app_rd & self.tmem_rresp.fire & app_w[5]
                      & (app_w == (32 + Cat(app_pos[4], app_blk)))):
                m.d.sync += meta_hw[app_sw0].eq(app_d32[:16])
                m.d.sync += meta_hw[app_sw0 + 1].eq(app_d32[16:])
            # This row's two halfword cells inside its 64-byte meta
            # beat: group g' = (row >> 2) & 3 contributes j0 at
            # 8*g' + lane and j1 at 8*g' + 4 + lane.
            meta_row = Signal(4)
            m.d.comb += meta_row.eq(row[:4] + Mux(part_k, pair_base, 0))
            meta_hw0 = (meta_row[2:4] << 3) + meta_row[0:2]
            meta_hw1 = (meta_row[2:4] << 3) + 4 + meta_row[0:2]
            sg1_meta_now = row[:4].all()  # rows 15, 31, 47, 63
            # Partial payload gating: a V block-extent run writes only
            # its block's chunks; a K row-extent run writes only the
            # beats of lane pairs whose first row is live (the pair's
            # dead odd lane lands as zeros -- the dead-tail bytes the
            # full drain writes for the same group).
            pay_ok = Mux(part_vb, sg1_widx[0] == part_blk,
                         Mux(part_k, row <= live, 1))
            sg1_wr = in_sg1w & (((sg1_widx != 2) & pay_ok)
                                | ((sg1_widx == 2) & sg1_meta_now))
            sg1_waddr = Mux(
                sg1_widx == 2, m_base + (row[4:6] << 6),
                r_base + (row[2:6] << 8) + (row[1] << 6) + (sg1_widx[0] << 7))
            sg1_wdata = Mux(
                sg1_widx == 2, Cat(*meta_hw),
                Mux(sg1_widx[0], Cat(hold_c1, chunk1), Cat(hold_c0, chunk0)))
            # A gated beat advances the walk in the cycle it would have
            # written, so a partially-gated sequence (V block extent,
            # K dead pairs) still reaches the meta beat and completes.
            sg1_adv = (sg1_wr & self.tmem_w.fire) | (~sg1_wr & in_sg1w)
            with m.If(sg1_wr & self.tmem_w.fire):
                m.d.sync += [
                    sg1_widx.eq(sg1_widx + 1),
                    self.beats.eq(self.beats + 1),
                ]
            with m.If(~sg1_wr & in_sg1w):
                m.d.sync += sg1_widx.eq(sg1_widx + 1)
            sg1_last = in_sg1w & (((sg1_widx == 1) & ~sg1_meta_now
                                   | (sg1_widx == 2)) & sg1_adv)
            app_waddr = Mux(
                app_w[5], m_base + (app_w[:5] << 6),
                r_base + (app_w[1:6] << 8) + (app_w[0] << 6) + (app_blk << 7))
            app_wdata = Cat(*meta_hw)[:self.data_width]
        else:
            # Unreachable: an SG1 descriptor completes BAD_SHAPE at
            # accept below the deployed beat, so in_sg1w never asserts.
            sg1_wr = Const(0)
            sg1_waddr = Const(0, aw)
            sg1_wdata = Const(0, 8 * VB_SG1_BEAT)
            sg1_last = Const(0)
            app_waddr = Const(0, aw)
            app_wdata = Const(0, self.data_width)

        m.d.comb += [
            self.tmem_w.valid.eq(in_drain | (in_sg1w & sg1_wr)
                                 | (in_appw & app_ph)),
            self.tmem_w.bits.addr.eq(
                Mux(
                    in_appw, app_waddr,
                    Mux(in_sg1w, sg1_waddr,
                        r_base + (row * self.row_pad) + (dbeat << lg)))),
            self.tmem_w.bits.data.eq(
                Mux(
                    in_appw, app_wdata,
                    Mux(in_sg1w, sg1_wdata[:self.data_width],
                        drain_sr[:self.data_width]))),
        ]

        # ------------------------------------------------------------------
        # Main FSM.
        # ------------------------------------------------------------------
        def start_block():
            m.d.sync += [gi.eq(0), xmax16.eq(0), xnan.eq(0)]

        def start_scale():
            m.d.sync += [s_step.eq(0), s_wait.eq(0), zero_blk.eq(0)]

        def clear_row():
            for k in range(17):
                m.d.sync += rowwords[k].eq(0)

        def advance_row():
            """Next-row transition shared by the drain paths: bump the
            row cursor, clear the row assembly, and enter the next
            row's scale (prefetched) or gather."""
            with m.If(row == last_row):
                m.next = 'DONE'
            with m.Else():
                m.d.sync += [
                    row.eq(row + 1),
                    blk.eq(0),
                    wbyte.eq(0),
                    cur_bank.eq(~cur_bank),
                ]
                clear_row()
                with m.If(drain_prefetched):
                    m.d.sync += [
                        xmax16.eq(pxmax16),
                        xnan.eq(pxnan),
                    ]
                    start_scale()
                    m.next = 'SCALE'
                with m.Else():
                    start_block()
                    m.next = 'GATHER'

        with m.FSM(name='vbuild') as fsm:
            with m.State('IDLE'):
                m.d.comb += [
                    self.sink.ready.eq(1),
                    self.done.eq(self.sink.fire & bad),
                    self.status.eq(
                        Mux(shape_bad, Status.BAD_SHAPE,
                            Mux(align_bad, Status.BAD_ALIGN,
                                Status.BAD_RANGE))),
                ]
                with m.If(self.sink.fire & ~bad):
                    m.d.sync += [
                        a_base.eq(offer.a_base),
                        r_base.eq(offer.r_base),
                        m_base.eq(offer.m_base),
                        sg1.eq(offer_sg1),
                        krow.eq(offer_krow | offer_va),
                        part.eq(offer_part),
                        live.eq(offer_live[:7]),
                        pair_base.eq((offer.k - 1) & 14),
                        vapp.eq(offer_va),
                        app_blk.eq(offer_va1),
                        app_pos.eq((offer.k - 1)[:5]),
                        app_w.eq(0),
                        app_ph.eq(0),
                        app_rd.eq(0),
                        fill_wlo.eq(Mux(offer_vb1, 32 * VB_SLOT_WORDS, 0)),
                        fill_word.eq(Mux(offer_vb1, 32 * VB_SLOT_WORDS, 0)),
                        fill_total.eq(
                            Mux(offer_va, VB_SLOT_WORDS,
                                offer_live * VB_SLOT_WORDS)),
                        fill_beat.eq(0),
                        fill_have.eq(0),
                        fill_wcnt.eq(0),
                        rd_pend.eq(0),
                        pre_rd.eq(0),
                        pre_cnt.eq(0),
                        row.eq(0),
                        blk.eq(0),
                        cur_bank.eq(0),
                        gi.eq(0),
                        xmax16.eq(0),
                        xnan.eq(0),
                        pgi.eq(0),
                        pxmax16.eq(0),
                        pxnan.eq(0),
                        prefetch_done.eq(0),
                        drain_prefetched.eq(0),
                        wbyte.eq(0),
                        q_iss.eq(0),
                        q_land.eq(0),
                        s_step.eq(0),
                        s_wait.eq(0),
                        nrm_pend.eq(0),
                        nrm_iss.eq(0),
                        sg1_widx.eq(0),
                    ]
                    m.next = 'FILL'

            with m.State('FILL'):
                m.d.comb += in_fill.eq(1)
                with m.If((fill_word == fill_total) & ~fill_have & ~rd_pend):
                    m.d.sync += [row.eq(0), blk.eq(0), wbyte.eq(0)]
                    clear_row()
                    start_block()
                    m.next = 'GATHER'

            with m.State('GATHER'):
                m.d.comb += in_gather.eq(1)
                with m.If(cur_live == 0):
                    with m.If(part_vb1 & (blk == 0)):
                        # A block-1 partial never touches block 0: its
                        # staging extents are the loaded previous
                        # build's bytes, so skip straight past the
                        # zero-amax scale round to the selected block
                        # (realigning the byte cursor to block 1's
                        # fixed 34-byte boundary, as the quant-path
                        # transition does).
                        m.d.sync += [
                            blk.eq(1),
                            wbyte.eq(34),
                            cur_bank.eq(~cur_bank),
                        ]
                        start_block()
                    with m.Else():
                        # The last live slot folds this cycle (an
                        # empty block skips straight through); the
                        # scale sequencer arms on the next.
                        start_scale()
                        m.next = 'SCALE'
                with m.Elif(gather_valid & (gather_index == cur_live - 1)):
                    with m.If(vapp & (blk == 0)):
                        # The append's second gather half: dims 32..63
                        # of the same slot continue the SAME amax fold
                        # (no xmax16/xnan reset, no scale yet) into the
                        # other bank.
                        m.d.sync += [
                            blk.eq(1),
                            gi.eq(0),
                            cur_bank.eq(~cur_bank),
                        ]
                        m.next = 'GATHER'
                    with m.Else():
                        # The last live slot folds this cycle; the scale
                        # sequencer arms on the next.
                        start_scale()
                        m.next = 'SCALE'

            with m.State('SCALE'):
                m.d.comb += in_scale.eq(1)
                with m.If(scale_done):
                    # The d16 pair is static per block: block 0 in
                    # word 0's low half (bytes 0-1), block 1 in word
                    # 8's high half (bytes 34-35). An append runs its
                    # single scale at blk 1 and writes BOTH slots with
                    # the one per-token d16.
                    with m.If(blk == 0):
                        m.d.sync += rowwords[0][:16].eq(d16)
                    with m.Else():
                        m.d.sync += rowwords[8][16:32].eq(d16)
                    with m.If(vapp):
                        m.d.sync += rowwords[0][:16].eq(d16)
                        # Widen the token d16 into its F32 row word
                        # now (the dedicated unit is idle in SCALE);
                        # the drain patches it beats later.
                        m.d.comb += [f16wa.inp_valid.eq(1), f16wa.inp.eq(d16)]
                        # The single scale ran at blk 1 (the gather's
                        # second half); re-arm the quant walk as the
                        # blk-0 pass so chunk 0 quantizes dims 0..31
                        # from bank 0 first, then the QUANT transition
                        # above walks bank 1's dims 32..63.
                        m.d.sync += [blk.eq(0), cur_bank.eq(0)]
                    m.d.sync += [
                        wbyte.eq(wbyte + 2),
                        q_iss.eq(0),
                        q_land.eq(0),
                        scale_done.eq(0),
                    ]
                    with m.If(zero_blk & ~xnan):
                        # Zero-amax early-out: a block whose live
                        # elements are all exactly zero quantizes to
                        # 32 zero bytes; the row clear already holds
                        # them, so the quant stream is skipped. A NaN
                        # live element must NOT skip: it is excluded
                        # from the amax fold, but its own byte is
                        # NaN * 0 -> NaN -> q8 saturates, not zero.
                        m.d.sync += wbyte.eq(wbyte + 34)
                        with m.If(cur_live == 0):
                            m.d.sync += self.dead_blocks.eq(self.dead_blocks +
                                                            1)
                        with m.Else():
                            m.d.sync += self.zero_blocks.eq(self.zero_blocks +
                                                            1)
                        with m.If(blk == 0):
                            m.d.sync += [blk.eq(1), cur_bank.eq(~cur_bank)]
                            start_block()
                            m.next = 'GATHER'
                        with m.Else():
                            m.d.sync += drain_prefetched.eq(0)
                            m.next = 'DRAINLOAD'
                    with m.Else():
                        m.d.sync += [
                            pgi.eq(0),
                            pxmax16.eq(0),
                            pxnan.eq(0),
                            prefetch_done.eq(~has_next | (nxt_live == 0)),
                        ]
                        m.next = 'QUANT'

            with m.State('QUANT'):
                m.d.comb += in_quant.eq(1)
                with m.If((q_iss == cur_live) & (q_land == cur_live)
                          & prefetch_done):
                    m.d.sync += [q_iss.eq(0), q_land.eq(0)]
                    with m.If(blk == 0):
                        with m.If(vapp):
                            # The append's second quant half: dims
                            # 32..63 from the other bank under the
                            # SAME scale (no second scale pass), their
                            # bytes at the fixed 36-byte boundary.
                            m.d.sync += [
                                blk.eq(1),
                                cur_bank.eq(~cur_bank),
                                wbyte.eq(36),
                                drain_prefetched.eq(0),
                            ]
                            m.next = 'QUANT'
                        with m.Elif(part_vb0):
                            # A block-0 partial is done with this row
                            # once block 0 lands: block 1's extents
                            # stay loaded, so skip its zero-amax round
                            # entirely (and re-enter GATHER -- no
                            # prefetch is outstanding).
                            m.d.sync += drain_prefetched.eq(0)
                            m.next = 'DRAINLOAD'
                        with m.Else():
                            m.d.sync += [
                                blk.eq(1),
                                cur_bank.eq(~cur_bank),
                                xmax16.eq(pxmax16),
                                xnan.eq(pxnan),
                                # Realign the byte cursor: a capped
                                # quant stream stopped at 2 + cur_live,
                                # but the next block's bytes start at
                                # its fixed 34-byte slot boundary.
                                wbyte.eq(34),
                            ]
                            start_scale()
                            m.next = 'SCALE'
                    with m.Else():
                        # A partial run never prefetches the next row's
                        # selected block (nxt_live is 0 on the exit
                        # side), so its rows always re-enter GATHER; an
                        # append never leaves its one row either.
                        m.d.sync += drain_prefetched.eq((row != last_row)
                                                        & (part == 0) & ~vapp)
                        m.next = 'DRAINLOAD'

            with m.State('DRAINLOAD'):
                with m.If(vapp):
                    # Append-mode drain: the row assembly is the token
                    # row; APPW pre-reads, patches and writes back the
                    # 32 payload beats and the four scale-row beats.
                    m.d.sync += [
                        self.rows.eq(self.rows + 1),
                        app_w.eq(0),
                        app_ph.eq(0),
                        app_rd.eq(0),
                    ]
                    m.next = 'APPW'
                with m.Elif(~sg1):
                    m.d.sync += [
                        drain_sr.eq(
                            Cat(rowbuf,
                                Const(0, 8 * self.row_pad - 8 * VB_ROW_RAW))),
                        dbeat.eq(0),
                    ]
                    m.next = 'DRAIN'
                with m.Elif(pre_rd):
                    # A partial run's meta pre-read still outstanding:
                    # hold the scatter one cycle so the loaded beat
                    # lands first (see the pre-read comment above).
                    pass
                with m.Else():
                    # Row assembly complete: scatter the row's two d16
                    # cells into the meta beat accumulator, hold the
                    # even lane's chunks, and (odd lane) emit the
                    # payload beat pair in SG1W. The legacy shift-out
                    # above is unused in this mode.
                    m.d.sync += self.rows.eq(self.rows + 1)
                    if self.beat_bytes == VB_SG1_BEAT:
                        # Partial gating: a V block-extent run writes
                        # only its block's cells; a K row-extent run
                        # writes only rows < live (dead rows keep the
                        # pre-read cells instead of today's zeros).
                        meta_go0 = ~part_vb1 & ~part_k_row_dead
                        meta_go1 = ~part_vb0 & ~part_k_row_dead
                        with m.If(meta_go0):
                            m.d.sync += meta_hw[meta_hw0].eq(rowwords[0][:16])
                        with m.If(meta_go1):
                            m.d.sync += meta_hw[meta_hw1].eq(
                                rowwords[8][16:32])
                        with m.If(~row[0]):
                            m.d.sync += [
                                hold_c0.eq(chunk0),
                                hold_c1.eq(chunk1),
                            ]
                            advance_row()
                        with m.Else():
                            m.d.sync += sg1_widx.eq(0)
                            m.next = 'SG1W'

            with m.State('SG1W'):
                # SG1 staging writes: payload beat pair (widx 0, 1)
                # plus the meta beat on sixteenth rows (widx 2).
                m.d.comb += in_sg1w.eq(1)
                with m.If(sg1_last):
                    advance_row()

            with m.State('APPW'):
                # Append-mode patch drain: each beat was pre-read into
                # the accumulator with its two patch bytes (or the
                # widened d16 word) scattered on the response; write
                # the composed beat back, then advance to the next.
                m.d.comb += in_appw.eq(1)
                with m.If(app_ph & self.tmem_w.fire):
                    m.d.sync += self.beats.eq(self.beats + 1)
                    with m.If(app_w == 35):
                        m.next = 'DONE'
                    with m.Else():
                        m.d.sync += [app_w.eq(app_w + 1), app_ph.eq(0)]

            with m.State('DRAIN'):
                m.d.comb += in_drain.eq(1)
                with m.If(self.tmem_w.fire):
                    m.d.sync += [
                        drain_sr.eq(drain_sr >> self.data_width),
                        dbeat.eq(dbeat + 1),
                        self.beats.eq(self.beats + 1),
                    ]
                    with m.If(dbeat + 1 == beats_per_row):
                        m.d.sync += self.rows.eq(self.rows + 1)
                        advance_row()

            with m.State('DONE'):
                m.d.comb += [
                    self.done.eq(1),
                    self.status.eq(Status.OK),
                ]
                m.d.sync += self.tiles.eq(self.tiles + 1)
                m.next = 'IDLE'

        m.d.comb += self.active.eq(~fsm.ongoing('IDLE'))

        with m.If(in_fill):
            m.d.sync += self.fill_cycles.eq(self.fill_cycles + 1)
        with m.If(in_gather | prefetch_go):
            m.d.sync += self.gather_cycles.eq(self.gather_cycles + 1)
        with m.If(in_scale):
            m.d.sync += self.scale_cycles.eq(self.scale_cycles + 1)
        with m.If(in_quant):
            m.d.sync += self.quant_cycles.eq(self.quant_cycles + 1)
        with m.If(in_drain | in_sg1w | in_appw):
            m.d.sync += self.drain_cycles.eq(self.drain_cycles + 1)
        with m.If(running):
            m.d.sync += self.run_cycles.eq(self.run_cycles + 1)
        return m
