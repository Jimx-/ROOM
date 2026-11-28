"""Device-side V-window tile builder for FA shadow maintenance (M5).

Geometry extension: with ``vbuild_max_head_dim=128``, rows bit 15 selects
128 dimensions for this descriptor; without it the existing 64-dimension
ABI is unchanged. The window remains 64 slots. Wide K rows contain four
Q8_0 blocks, while wide V tiles contain 128 rows of two blocks. Partial
updates preserve the untouched cells of the selected metadata beat.

The ggml-troom backend mirrors each layer's offloaded F16 KV pool as
device-resident quantized shadows. The K shadow is slot-major Q8_0
rows; the V shadow is per (kv head, 64-slot window) a 64/128-row
transposed Q8_0 tile whose reduction axis is the slots, so P.V is a
plain MATMUL against the tile. Host-side tile rebuilds cost measured
milliseconds per token (the M5 host half removed the dead-tail
waste; this engine, the M5 RTL half, removes the per-token rebuild
traffic itself). Native append modes exist at both widths -- the
per-token V append (64-dim P2d-b, and the Q3c 128-dim candidate-A
form under bit 15) and the Q3c K append (rows 20) -- and may be
disabled entirely with ``vbuild_native_append=False`` for production
builds.

One ``VBUILD`` descriptor builds one (head, window) tile:

* ``a_base`` names a TMEM image of the pool window -- slot-major, 64/128
  FP16 dims per slot (a 128/256-byte row per slot), the first ``k``
  slots valid -- which a preceding ``LOAD_2D`` fills straight from
  the F16 pool (one strided transfer per head slice);
* ``r_base`` names the tile output: 64/128 rows (one per dim) of two
  Q8_0 blocks over the slots (a 68-byte row), each row zero-padded
  to a whole bus beat -- the beat-padded row stride the backend and
  the matrix unit's Q8_0 fetch already use.

The engine keeps the window in a local 8/16 KiB buffer, so the
transpose is a per-row gather (buffer word ``j*(dims/2) + (d >> 1)``
holds the dimension pair containing ``d`` in slot ``j``; halfword
``d & 1`` is the row-``d`` value), never a TMEM traffic pattern. Two gather buffers let the next
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

Wide append and K append (Q3c, 2026-11-10,
plans/2026-11-09-troom-qwen3-hill-climb.md): the final-format
writer prototype of the frozen candidate-A contract
(``results/troom-qwen3-append-contract-20261110/CONTRACT.md``).

* ``rows`` 18/19 with bit 15 set (128 dimensions): the same per-token
  append generalized to four 32-dim segments sharing ONE amax fold
  over all 128 dims (candidate A: one FP16 scale per token per head,
  quantized once at append and never re-quantized). The gather banks
  extend to four; the single d16 lands in all four assembly slots;
  the drain patches 64 payload beats (two bytes each) plus the four
  scale-row beats -- 68 composed beats, no meta write anywhere. The
  meta cells stay the birth-seeded FP16 1.0 for the window's whole
  lifetime (the A-keep reader: the PV epilogue's first multiply is
  the exact identity and every scale rides the P side).
* ``rows`` 20 (``VB_ROWS_K_APP``, either width): K direct append. The
  token's row quantizes with the frozen PER-BLOCK K sequence
  (numerically the installed K contract; only who writes the bytes
  when changes), then patches token ``k-1``'s extents in the group
  image in place: each block's whole 32-B chunk into its beat's lane
  (a chunk is half a beat) and the four 2-B meta cells, all
  pre-read composed so neighboring packed tokens keep their bytes.
  ``k`` is the group's live prefix 1..16; ``m_base`` names the K meta
  staging (not a scale row). Overwrite of a slot goes through the
  same primitive.

Both modes exist to remove the separate rebuild path (the Q3 target):
ordinary append writes the final persistent representation with one
bounded beat-composed write per token per head and never re-reads
history beyond beat composition.

K4 selective same-contract requant (D2, 2026-11-12): ``rows`` 3 with
bit 15 (128 dimensions, SG1 drain required) appends one slot to the
window's open block while KEEPING the installed temporal block
contract byte-identical. The retained state is, per dim row, the
block's exact winning magnitude pattern and the frozen reciprocal
bits it produced (the D2 negative control proved the rounded d16
alone insufficient); a row whose new value does not exceed the
retained pattern keeps its amax and therefore its r, d16 and every
historical byte, so only its new column byte is written -- quantized
under the stored r bits through the standard quant pipeline. A row
whose pattern strictly rises (or any row, under the heal flag k bit
7) runs the ordinary gather/fold/scale/quant row pass over the
block's live prefix. The drain composes whole 64 B beats pre-read
from the staging images: the open block's 64 payload beats (risen
chunks replace, non-risen patch byte ``(k-1)&31``), the 8 meta beats
(risen rows' d16 cells) and the 16 state beats at ``m_base +
dims*4`` (risen rows' pattern/r words; two alternating accumulators
keep the next beat's pre-read from clobbering the composing one).
Emitted bytes equal the temporal partial rebuild's at every live
prefix given the state contract; the host maintains that state
beside the payload/meta shadows and re-derives it via heal after any
fallback rebuild (rewind/overwrite/grow), which themselves stay the
installed chains.

Non-quad staging (2026-11-25/26,
plans/2026-11-23-troom-qwen3-breakthrough.md): a build whose
``n_outputs`` is not 4 interleaves the SG1 staging in
``n_outputs``-row groups -- the consuming stream engine's lane
count -- instead of the frozen quad layout. The cell order itself is
unchanged (group-major, block within group, lane within block; a
short final group drops its missing lanes exactly like the on-wire
pack), so the V images keep their sizes; only the group width moves.

* V full and partial extents (rows 0, 1/2): payload beats pair
  staging-consecutive chunks of the wider group. At an odd group
  width the boundary beat between the row's two blocks straddles them
  (block 0's last lane with block 1's lane 0), so the drain holds
  block 1's lane-0 chunk for the group's span and composes that beat
  at the group's last row; the meta beats (sixteen rows each) can
  straddle group boundaries, so the d16 scatter alternates between two
  beat accumulators. A partial run pre-reads its straddle beats and
  composes the retained neighbor half from two parity-banked
  registers.
* The V append (rows 18/19) patches block ``blk``'s chunk byte p in
  every dim row's cell: the drain walks the distinct beats holding
  those cells (at five lanes 38 at 64 dims, 77 at 128), pre-reads each
  and patches
  the one or two of its rows in it -- a group's last even cell lands
  alone in a straddle beat's low half and block 1's lane 0 in its
  high half, each over the retained neighbor. The scale row stays
  the four-beat F32 row.
* K4 (rows 3|WIDE) composes the open block's beats from the same
  cell positions: the block's payload slice is its beats in image
  order (77 at five lanes: three per full group, two in the
  three-lane tail), a row composes when its cell is a beat's high
  half or sits alone in a straddle beat's low half, the pre-read banks
  cycle by beat rank, and the meta cells follow the cell index. The
  row-indexed state image is unchanged.
* The K orientation (rows 16/17/20) differs in kind: the K image is
  ONE per-head stream of slot rows, and at five lanes a maintenance
  block's cells straddle the five-row groups (16 is not a multiple of
  five), so a block cannot be staged on its own.
  The descriptor's rows bits 5..11 therefore carry the lane offset of
  the first rebuilt slot -- bits 5..7 its lane, bits 8..11 its group
  index mod 16, which fixes the meta slice's beat alignment -- and
  the run stages the whole interleave window its rows touch (four
  groups for a block at five lanes, one or two for a pair or a token):
  ``r_base``
  and ``m_base`` name the window's payload beats and its beat-aligned
  meta slice of the PRIOR stream image, which the host LOADs first.
  The unit pre-reads them, lands its rows' cells (dead rows as zero
  rows) and writes every beat back, the neighboring blocks' cells
  riding the pre-read; the host STOREs the window to the image at
  the same offsets. The stream is consumed in whole groups (its row
  count a multiple of ``n_outputs``), so the layout is static. The
  field must be zero on every other mode and on every quad build.

The lane count enters as ``n_outputs`` throughout these drains. A build
supports one through eight lanes (the K modes use a three-bit lane offset).

What a build cannot reach it does not build (2026-11-27,
plans/2026-11-23-troom-qwen3-breakthrough.md,
results/troom-vbuild-congestion-20261127). The production image packed
this unit into islands at 96-99% LUT density whose worst paths were
control cones in front of wide register banks. Five rewrites, each an
identity on every reachable state:

* The append modes (rows 18/19 and the K append, row 20) are elaborated
  only on a build that enables them (``vbuild_native_append``). A build
  without native append rejects those descriptors at accept exactly as
  before; its image has no APPW state, no append register and none of
  the disabled drain's terms in the cones of the TMEM write port and the
  meta accumulator. Synthesis cannot find this on its own -- the
  parameter gates the accept, not the state machine -- so the image
  carried the whole append datapath (about 3,900 LUTs out of context,
  six of the accumulator's nine write sources) behind a state it never
  entered.
* The legacy drain's shift register ends with the row instead of the
  padded row: above the first beat only the row's own bytes are ever
  non-zero, because the padding loads zero and every shift moves zero
  into the bits it vacates.
* The gather and quant bank selectors do not carry the K4 walk's block.
  A K4 run leaves FILL for K4ROW and visits neither GATHER, SCALE nor
  QUANT; with the block in the selector, banks 2 and 3 (the heal and
  wide-append banks) carried a gather decode they could never pass.
* The K4 staging position -- group, lane, cell, compose role, beat rank
  and meta beat of a tile row -- is a constant table over the open
  block and the row. The arithmetic it replaces, a constant division,
  two products and a subtraction in carry chains, was the head of the
  unit's worst path, twenty logic levels from ``k4_land_row`` to the
  meta accumulator.
* The row pass's advance condition is what the general step rule reduces
  to at the last slot -- the armed meta write, if any, fired -- instead
  of the geometry that ``entry_v`` and ``entry_stall`` derive from the
  lane position, and the row assembly clears one cycle after the row
  pass (``rowclr``) rather than in its last cycle. Nothing writes the
  assembly in the first cycle of GATHER, SCALE, KDRAIN or DONE, and no
  path reads it there.
"""

from amaranth import *
from amaranth.hdl.rec import Direction, Record

from room.consts import RoundingMode
from room.fpu import FPFormat, FPUOperator, FPURec

from troom.consts import Status
from troom.fpu import FPAdd32, FPMul32
from troom.convert import Fp16Widen, Fp32To16
from troom.types import HasTRoomParams
from troom.vector import F127_BITS, INV127_BITS

from roomsoc.interconnect.stream import Decoupled

# Legacy FA window geometry: 64 slots of 64 FP16 dims each. Wide
# descriptors select 128 dims; the slot count remains fixed.
VB_SLOTS = 64
VB_DIMS = 64
VB_SLOT_BYTES = VB_DIMS * 2
VB_ROW_RAW = 2 * 34  # two Q8_0 blocks: d16 + 32 quants each
VB_SG1_BEAT = 64  # the deployed bus beat; SG1 staging writes whole 64B beats
VB_K_ROWS = 16  # K group slots (rows == 16 selects the K orientation)
VB_SG1_SROW = VB_SLOTS * 4  # P2d-b scale row: 64 widened-F32 words

# Partial-extent drain codes (P2d-a, descriptor ``rows`` field). The
# SG1 drain of a partial run writes only the rebuilt extents at their
# natural staging offsets -- the payload chunks and meta halfword
# cells of one 32-slot V block, or the payload beats and meta cells of
# the dirty K lane pair -- and pre-reads each meta staging beat so the
# composed 64 B beats carry the retained cells of the extents the host
# loaded and will not rebuild. The unwritten staging extents are never
# stored: the host slices its LOADs/STOREs to the same extents.
VB_ROWS_V_B0 = 1  # V partial: block-0 extent only (1 <= k <= 32)
VB_ROWS_V_B1 = 2  # V partial: block-1 extent only (33 <= k <= 64)
VB_ROWS_K_PART = VB_K_ROWS + 1  # K partial: dirty lane pair only (1 <= k <= 16)

# K4 selective same-contract requant (D2, 2026-11-12,
# plans/2026-11-09-troom-qwen3-hill-climb.md): one slot appended to the
# window's OPEN block, wide (bit 15) only. Rows whose retained exact
# winning magnitude pattern strictly rises re-quantize through the
# ordinary per-row sequence; every other row writes just its new
# column byte quantized under the state's stored frozen-reciprocal
# bits (bit-identical to the temporal rebuild's r because the amax is
# unchanged). ``k`` keeps the window's live prefix (1..64) with bit 7
# the HEAL flag -- requantize every row and rewrite the whole state,
# the re-derivation pass after fallback rebuilds. The state image --
# per dim row [pattern:16 | r:32] -- is staged at m_base + dims*4 (the
# open block's dims*8 bytes at m_base + dims*4 + blk*dims*8), pre-read
# composed like the meta beats.
VB_ROWS_V_K4 = 3
VB_K4_HEAL = 7  # the k-field bit selecting the heal pass

# Append-mode codes (P2d-b): one slot appended to an open block under
# the per-token scale. ``k`` keeps the window's live prefix (the
# appended slot is k-1), so the block and its position derive from the
# same ranges as the partial codes above.
VB_ROWS_V_APP0 = 18  # V append into open block 0 (1 <= k <= 32)
VB_ROWS_V_APP1 = 19  # V append into open block 1 (33 <= k <= 64)

# Q3c direct-append code: one K token's final bytes patched into its
# group image (either width), the per-block K contract quantized from
# the token's row and composed beat-wise in place. ``k`` is the
# group's live prefix; the appended token is k-1.
VB_ROWS_K_APP = 20

# Wide-append drain geometry (Q3c): a 128-dim V append patches 64
# payload beats plus the four-beat F32 scale row; a K append patches
# one beat per 32-dim block plus its meta beat.
VB_APP_WIDE_PAY_BEATS = 64
VB_APP_WIDE_TOTAL_BEATS = VB_APP_WIDE_PAY_BEATS + 4


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
    the Q3c code 20 patches one K token's final bytes into its group
    image (payload lanes and meta cells, pre-read composed, ``m_base``
    the K meta staging); every other nonzero ``rows`` is rejected at
    accept.

    On a build whose ``n_outputs`` is not 4, the K modes (16/17/20)
    read ``rows`` bits 5..11 as the lane offset of the first rebuilt
    slot (bits 5..7 its lane, 8..11 its group index mod 16) and stage
    the whole interleave window of that run (see the module
    docstring); bits 12..14 stay reserved, and bits 5..14 must be
    zero on every other mode and on every quad build.
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
        # D1 phase-split riders (2026-11-12,
        # plans/2026-11-09-troom-qwen3-hill-climb.md): exclusive-
        # partition helpers surfaced through the register ABI.
        # ``active_cycles`` counts every non-IDLE cycle (the unit's
        # occupancy), closing the gap ``run_cycles`` leaves at the
        # DRAINLOAD dispatch state and the per-tile DONE cycle;
        # ``dispatch_cycles`` is that dispatch/hold state alone (the
        # pre-read hold included). ``fill_wait_cycles`` is the fill's
        # non-absorbing subset (request issue/grant plus response
        # latency), ``drain_wait_cycles`` the drain-side TMEM waits
        # (write backpressure and the append drain's beat pre-read
        # issue/latency). ``prefetch_cycles`` counts the next-block
        # gather prefetch, which the legacy ``gather_cycles`` and
        # ``quant_cycles`` both include (gather_cycles is incremented
        # for prefetch_go during QUANT); subtracting it makes the
        # phase totals an exact partition:
        # active = fill + (gather - prefetch) + scale + quant
        #          + drain + dispatch + tiles.
        self.active_cycles = Signal(48)
        self.dispatch_cycles = Signal(48)
        self.fill_wait_cycles = Signal(48)
        self.drain_wait_cycles = Signal(48)
        self.prefetch_cycles = Signal(48)

        self.row_pad = -(-VB_ROW_RAW // self.beat_bytes) * self.beat_bytes
        if VB_SLOT_BYTES % self.beat_bytes:
            raise ValueError('a slot row must be whole beats')

    def elaborate(self, platform):
        m = Module()

        lg = self.lg_beat
        bb = self.beat_bytes
        aw = self.tmem_addr_width
        bw = self.data_width // 32
        max_dims = self.vbuild_max_head_dim
        max_blocks = max_dims // 32
        row_words = max_blocks * 34 // 4
        max_slot_words = max_dims // 2
        # rows bit 15 extends the geometry; the low bits retain the ABI.
        wide = Signal()
        # Reused expressions cross explicit combinational signals.
        # Python Value aliases would expand at every downstream use,
        # multiplying the RTLIL control/selector trees during lowering.
        dims_value = Mux(wide, 128, 64)
        dims = Signal.like(dims_value)
        m.d.comb += dims.eq(dims_value)
        slot_words_value = Mux(wide, 64, 32)
        slot_words = Signal.like(slot_words_value)
        m.d.comb += slot_words.eq(slot_words_value)

        fmul_s = m.submodules.fmul_s = FPMul32()
        fmul_q1 = m.submodules.fmul_q1 = FPMul32()
        fmul_q2 = m.submodules.fmul_q2 = FPMul32()
        fadd_s = m.submodules.fadd_s = FPAdd32()
        frec = m.submodules.frec = FPURec(32, FPFormat.S, latency=3)
        f16w = m.submodules.f16w = Fp16Widen()
        f16n = m.submodules.f16n = Fp32To16()
        # The append modes (rows 18/19 and the K append, row 20) exist
        # only on a build that enables them. A build without native
        # append rejects those descriptors at accept (BAD_SHAPE, in the
        # validation below), so none of their state, datapath or drain
        # is elaborated there: the signals fold to constants and the
        # FSM has no APPW state.
        app_on = self.vbuild_native_append
        # P2d-b: the append's dedicated d16 widen (the token scale's
        # F32 row word). Not shared with f16w: the scale completion
        # issues it right where the quant stream's element widens are
        # about to start, and the shared unit's two-cycle pipeline
        # would collide at the QUANT entry.
        if app_on:
            f16wa = m.submodules.f16wa = Fp16Widen()
        # D3b: the chain engine's dedicated widen. Not shared with
        # f16w (the element stream's unit): a chain's step-0 widen
        # issues the same cycle a cursor element may widen.
        f16ws = m.submodules.f16ws = Fp16Widen()

        # Local window buffer: 64 slots x 32/64 words, held as 4-lane
        # 128-bit words (D3a: the fill absorbs up to four 32-bit words
        # per cycle from each returned beat; buffer
        # word w lives at lane w&3 of memory word w>>2). One sync
        # write port (the fill) and one synchronous read port (the
        # gather), allowing block RAM inference. Fill and gather are
        # disjoint; wide xbuf banks also support K4 heal prefetch.
        FILL_LANES = 4
        buf = Memory(width=32 * FILL_LANES,
                     depth=VB_SLOTS * max_slot_words // FILL_LANES)
        buf_w = m.submodules.buf_w = buf.write_port()
        buf_r = m.submodules.buf_r = buf.read_port(transparent=False)

        # ------------------------------------------------------------------
        # Descriptor validation, on the offered combination. A nonzero
        # m_base selects the SG1 drain: its staging extents are the
        # payload/meta images (4096/256 bytes) instead of the legacy
        # tile, and the mode itself requires the deployed 64-byte beat.
        # A rows value of VB_K_ROWS selects the K orientation: 16 slot
        # rows over the dims, group staging extents (1024/64 bytes), k
        # = live slots 0..16, and the SG1 drain. Partial, K4 and append
        # mode codes below select their own extents and validation.
        # ------------------------------------------------------------------
        offer = self.sink.bits
        offer_sg1 = Signal()
        m.d.comb += offer_sg1.eq(offer.m_base != 0)
        offer_wide = (offer.rows[15] if len(offer.rows) > 15 else Const(0))
        offer_reserved = (offer.rows[16:].any()
                          if len(offer.rows) > 16 else Const(0))
        # The mode code is rows bits 0..4. Bits 5..11 are the K
        # staging lane offset (non-quad K modes only: the first rebuilt
        # slot's lane and its group index mod 16) and bits 12..14 stay
        # reserved; both must be zero everywhere else, exactly as the
        # 15-bit code compare required before the field existed.
        offer_code = offer.rows[:5]
        offer_koff = (offer.rows[5:12] if len(offer.rows) > 5 else Const(0, 0))
        offer_loff = (offer.rows[5:8] if len(offer.rows) >= 8 else Const(0, 3))
        offer_g16 = (offer.rows[8:12] if len(offer.rows) >= 12 else Const(
            0, 4))
        offer_rsv = (offer.rows[12:15].any()
                     if len(offer.rows) > 12 else Const(0))
        nonquad = self.n_outputs != 4
        offer_dims_value = Mux(offer_wide, 128, 64)
        offer_dims = Signal.like(offer_dims_value)
        m.d.comb += offer_dims.eq(offer_dims_value)
        offer_slot_bytes_value = Mux(offer_wide, 256, 128)
        offer_slot_bytes = Signal.like(offer_slot_bytes_value)
        m.d.comb += offer_slot_bytes.eq(offer_slot_bytes_value)
        # The K orientation: the full code (16), the partial
        # row-extent code (17) and the Q3c append code (20) share the
        # 16-row group geometry.
        offer_krow = Signal()
        m.d.comb += offer_krow.eq((offer_code == VB_K_ROWS) \
            | (offer_code == VB_ROWS_K_PART) \
            | (offer_code == VB_ROWS_K_APP))
        offer_vb0 = Signal()
        m.d.comb += offer_vb0.eq(offer_code == VB_ROWS_V_B0)
        offer_vb1 = Signal()
        m.d.comb += offer_vb1.eq(offer_code == VB_ROWS_V_B1)
        offer_kp = Signal()
        m.d.comb += offer_kp.eq(offer_code == VB_ROWS_K_PART)
        offer_va0 = Signal()
        m.d.comb += offer_va0.eq(offer_code == VB_ROWS_V_APP0)
        offer_va1 = Signal()
        m.d.comb += offer_va1.eq(offer_code == VB_ROWS_V_APP1)
        offer_va = Signal()
        m.d.comb += offer_va.eq(offer_va0 | offer_va1)
        offer_ka = Signal()
        m.d.comb += offer_ka.eq(offer_code == VB_ROWS_K_APP)
        offer_app = Signal()
        m.d.comb += offer_app.eq(offer_va | offer_ka)
        offer_k4 = Signal()
        m.d.comb += offer_k4.eq(offer_code == VB_ROWS_V_K4)
        # K4's k field strips the heal bit for every live-count use;
        # the open block is the window's upper half past live 32.
        offer_k4_live = offer.k[:7]
        offer_k4_blk = Signal()
        m.d.comb += offer_k4_blk.eq(offer_k4_live > 32)
        # K partial k carries the group's live prefix, not merely the
        # rebased input length. Keep the pair's original metadata position.
        offer_live_value = Mux(offer_kp, Mux(offer.k[0], 1, 2),
                               Mux(offer_k4, offer_k4_live, offer.k))
        offer_live = Signal.like(offer_live_value)
        m.d.comb += offer_live.eq(offer_live_value)
        # Partial-extent code latched for the run: 0 full, 1 V block 0,
        # 2 V block 1, 3 K row extent.
        offer_part_value = Mux(offer_vb0, 1,
                               Mux(offer_vb1, 2, Mux(offer_kp, 3, 0)))
        offer_part = Signal.like(offer_part_value)
        m.d.comb += offer_part.eq(offer_part_value)
        # An append (either family) reads exactly one slot row. Its
        # extent is validated on a build that can accept one; elsewhere
        # an append descriptor is rejected by shape_bad whatever this
        # bound says.
        a_end_o_value = offer.a_base + offer_live * offer_slot_bytes
        if app_on:
            a_end_o_value = Mux(offer_app, offer.a_base + offer_slot_bytes,
                                a_end_o_value)
        a_end_o = Signal.like(a_end_o_value)
        m.d.comb += a_end_o.eq(a_end_o_value)
        # ---- non-quad K staging window (offer-time geometry) ----
        # A K run on a non-quad build stages the WHOLE interleave
        # window its rebuilt rows touch -- every n_outputs-row group
        # containing one of them -- at r_base (payload, whole 64-B
        # beats) and m_base (meta, the window's beat-aligned slice of
        # the stream meta). The window is `cells` cells wide, a cell
        # being one slot's 32-B chunk (or 2-B d16) of one block; the
        # meta cell index is the payload cell index. `mhwoff` is the
        # first cell's halfword offset inside its beat, fixed by the
        # first group's index mod 16 because a period of 16 groups
        # spans a whole number of beats (16 * blocks * lanes * 2 B is
        # a multiple of 64: blocks * lanes is even).
        _kwin_max_cells = 0
        _kwin_mb_max = 1
        if nonquad:
            _L = self.n_outputs
            _knb = (2, max_blocks)

            def _kwin_cells(nq, wide, loff):
                ng = (loff + nq - 1) // _L + 1
                return ng * _knb[wide] * _L

            _kwin_max_cells = max(
                _kwin_cells(16, _w, lo) for _w in (0, 1) for lo in range(_L))
            for _w in (0, 1):
                for loff in range(_L):
                    for nq in (16, 2, 1):
                        cells = _kwin_cells(nq, _w, loff)
                        for g in range(16):
                            mhwoff = ((g * _knb[_w] * _L * 2) % 64) // 2
                            _kwin_mb_max = max(_kwin_mb_max,
                                               (mhwoff + cells + 31) // 32)
        kcells_o = Const(0, 8)
        kmhw_o = Const(0, 6)
        if nonquad:
            kcells_o = Signal(range(_kwin_max_cells + 1))
            with m.Switch(Cat(offer_loff, offer_wide, offer_kp, offer_ka)):
                for loff in range(_L):
                    for _wide in (0, 1):
                        for kp, ka in ((0, 0), (1, 0), (0, 1)):
                            nq = 2 if kp else (1 if ka else 16)
                            with m.Case(loff | (_wide << 3) | (kp << 4)
                                        | (ka << 5)):
                                m.d.comb += kcells_o.eq(
                                    _kwin_cells(nq, _wide, loff))
                with m.Default():
                    m.d.comb += kcells_o.eq(0)
            kmhw_o = Signal(6)
            with m.Switch(Cat(offer_g16, offer_wide)):
                for _wide in (0, 1):
                    for g in range(16):
                        with m.Case(g | (_wide << 4)):
                            m.d.comb += kmhw_o.eq(
                                ((g * _knb[_wide] * _L * 2) % 64) // 2)
                with m.Default():
                    m.d.comb += kmhw_o.eq(0)
        # The window's payload and meta beat counts.
        kpb_o = (kcells_o >> 1)
        kmb_o = ((kmhw_o + kcells_o + 31) >> 5)[:5]
        r_end_o_value = Mux(
            offer_krow,
            Mux(Const(nonquad), offer.r_base + (kpb_o << 6),
                offer.r_base + VB_K_ROWS * offer_dims),
            Mux(offer_sg1, offer.r_base + offer_dims * 64,
                offer.r_base + offer_dims * self.row_pad))
        r_end_o = Signal.like(r_end_o_value)
        m.d.comb += r_end_o.eq(r_end_o_value)
        # A V append's m_base names the four-beat F32 scale row, not
        # the 256-B meta staging (the same extent, different content);
        # a K append's names the K meta staging like every K mode; a
        # K4 append's names the meta staging with the open block's
        # state image at m_base + dims*4 (the range grows by dims*8).
        m_end_o_value = Mux(
            offer_k4, offer.m_base + offer_dims * 4 + offer_dims * 8,
            Mux(
                offer_krow,
                Mux(Const(nonquad), offer.m_base + (kmb_o << 6),
                    offer.m_base + VB_K_ROWS * (offer_dims >> 4)),
                offer.m_base + offer_dims * 4))
        if app_on:
            m_end_o_value = Mux(offer_va, offer.m_base + VB_SG1_SROW,
                                m_end_o_value)
        m_end_o = Signal.like(m_end_o_value)
        m.d.comb += m_end_o.eq(m_end_o_value)
        # The lane-offset field is only meaningful on a K mode of a
        # non-quad build: anywhere else, and the reserved bits
        # everywhere, must read zero.
        koff_bad = Signal()
        m.d.comb += koff_bad.eq((((offer_koff != 0) | offer_rsv)
                                 & Mux(offer_krow, Const(not nonquad), 1))
                                | (offer_krow & Const(nonquad)
                                   & (offer_rsv |
                                      (offer_loff >= self.n_outputs)
                                      | Const(len(offer.rows) < 12))))
        shape_bad = Signal()
        # Every mode is accepted at every lane count (the non-quad builds
        # carry their own drains); on a non-quad build the K modes'
        # lane-offset field is checked by koff_bad above.
        m.d.comb += shape_bad.eq(offer_reserved | ((offer_code != 0) & ~offer_krow & ~offer_vb0
                     & ~offer_vb1 & ~offer_kp & ~offer_va & ~offer_ka
                     & ~offer_k4) \
            | koff_bad \
            | Mux(offer_krow,
                  ~offer_sg1 | (offer.k > VB_K_ROWS),
                  (offer_live == 0) | (offer_live > VB_SLOTS)) \
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
            | Mux(offer_ka,
                  ~offer_sg1 | (offer.k == 0) | (offer.k > VB_K_ROWS), 0) \
            | Mux(offer_k4,
                  ~offer_sg1 | ~offer_wide | offer.k[8:].any()
                  | (offer_k4_live == 0) | (offer_k4_live > VB_SLOTS), 0) \
            | (offer_sg1 & (self.beat_bytes != VB_SG1_BEAT)) \
            | (offer_wide & Const(max_dims != 128)) \
            | (offer_app & Const(not self.vbuild_native_append)) \
            | (offer_k4 & Const(not self.vbuild_k4_selective)))
        align_bad = Signal()
        m.d.comb += align_bad.eq(offer.a_base[:lg].any() | offer.r_base[:lg].any() \
            | (offer_sg1 & offer.m_base[:lg].any()))
        range_bad = Signal()
        m.d.comb += range_bad.eq((a_end_o > self.tmem_bytes) \
            | (r_end_o > self.tmem_bytes) \
            | (offer_sg1 & (m_end_o > self.tmem_bytes)))
        bad = Signal()
        m.d.comb += bad.eq(shape_bad | align_bad | range_bad)

        # Latched run state.
        a_base = Signal(aw)
        r_base = Signal(aw)
        m_base = Signal(aw)
        sg1 = Signal()  # SG1 drain (payload+meta staging), not legacy tile
        krow = Signal()  # K orientation: 16 slot rows over the dims
        part = Signal(2)  # partial-extent code (see offer_part)
        live = Signal(7)
        pair_base = Signal(4)
        part_vb0 = Signal()
        m.d.comb += part_vb0.eq(part == 1)
        part_vb1 = Signal()
        m.d.comb += part_vb1.eq(part == 2)
        part_vb = Signal()
        m.d.comb += part_vb.eq(part_vb0 | part_vb1)
        part_blk = Signal()
        m.d.comb += part_blk.eq(part == 2)
        part_k = Signal()
        m.d.comb += part_k.eq(part == 3)
        # Generalized staging position (n_outputs != 4 builds only): the
        # row's interleave group and lane within it, latched beside
        # `row` and advanced with it so the staging index formulas read
        # registers instead of dividing by the group width. The meta
        # beat walks and the armed write live here too (not inside the
        # 64-byte-beat block) so the IDLE reset can reach them at any
        # beat width; below the deployed beat they are dead state.
        if self.n_outputs != 4:
            _lanes = self.n_outputs
            sgrp = Signal(range(max_dims // _lanes + 1))
            slane = Signal(range(_lanes))
            _mb = max_dims * 2 // 32  # meta beats of the widest tile
            mbeat0 = Signal(range(_mb + 1))
            mbeat1 = Signal(range(_mb + 1))
            mwr_v = Signal()
            mwr_beat = Signal(range(_mb))
            strret_v = Signal(2)
            # The K staging window (non-quad K modes): the payload
            # store holds the window's whole 64-B beats -- one 32-B
            # cell per half, written by the 256-bit enable lane -- so
            # a rebuilt row's chunk is a whole-half store write and
            # the retained neighbours ride the pre-read. The meta
            # window is small enough for register banks (one per
            # beat, halfword cells). kloff/kpb/kmb/kmhw are the
            # run-constant geometry latched at accept; kpp/kpm walk
            # the pre-read, kpj a row's block patches, kdb the drain.
            _kwin_depth = _kwin_max_cells // 2
            kwin = Memory(width=self.data_width,
                          depth=_kwin_depth,
                          name='kwin')
            m.submodules.kwin_w = kwin_w = kwin.write_port(
                granularity=max(1, self.data_width // 2))
            m.submodules.kwin_r = kwin_r = kwin.read_port(transparent=False)
            kloff = Signal(3)
            kpb = Signal(range(_kwin_depth + 1))
            kmb = Signal(range(_kwin_mb_max + 1))
            kmhw = Signal(6)
            kpp = Signal.like(kpb)
            kpm = Signal.like(kmb)
            kpre_idx = Signal(range(_kwin_depth))
            kpj = Signal(2)
            kdb = Signal(range(_kwin_depth + _kwin_mb_max + 1))
            kdb_d = Signal.like(kdb)
            metab = Array(
                Array(Signal(16, name=f'mkb{b}_{i}') for i in range(32))
                for b in range(_kwin_mb_max))
        # Append-mode state is build-conditional (see app_on): constants
        # on a build without native append.
        vapp = Signal() if app_on else Const(
            0)  # append: one slot, token scale
        kapp = Signal() if app_on else Const(0)  # Q3c K append: one token
        app_blk = Signal()  # the appended-to open block (K4 shares it)
        app_pos = Signal(5)  # the slot's position inside that block
        app_w = Signal(7) if app_on else Const(0, 7)  # append drain beat
        app_ph = Signal() if app_on else Const(
            0)  # beat latched, write pending
        app_rd = Signal() if app_on else Const(
            0)  # a beat pre-read outstanding
        app_d32 = Signal(32) if app_on else Const(0, 32)  # widened token d16
        kr = Signal(4) if app_on else Const(0, 4)  # K append's token index k-1
        # K4 selective requant (rows 3): latched mode state.
        k4 = Signal()  # this run is a K4 selective append
        k4_heal = Signal()  # requantize every row, rewrite the state
        k4_bufd = Signal()  # the walk's buffer port is warmed for `row`
        k4_done_wait = Signal()  # last row: hold until beat writes drain
        k4_hold_risen = Signal()  # the even lane of the current pair rose
        k4_hold_chunk = Signal(8 * 32)  # the even lane's fresh chunk
        k4_hold_byte = Signal(8)  # the even lane's quick column byte
        # D3a pipelined quick stream: rows dispatch one per cycle, the
        # single column-byte elements flow through the shared quant
        # pipeline, and the landing engine consumes them in issue order
        # ~11 cycles later. k4q_pend bounds the dispatch lead over the
        # landing cursor (the state-bank parity protocol keeps today's
        # 8-row lead invariant); k4_rpipe aligns each dispatched row's
        # retained r bits with its element's arrival at the first
        # multiplier (the Fp16Widen's two-cycle latency).
        k4q_pend = Signal(4)  # elements issued, not yet landed
        k4_land_row = Signal(7)  # the row whose completion lands next
        k4_rpipe0 = Signal(32)
        k4_rpipe1 = Signal(32)
        # ------------------------------------------------------------------
        # D3b: the decoupled risen pipeline (all K4 runs). The
        # walker dispatches past risen rows; each divert allocates a
        # chain context that runs today's exact scale op sequence on
        # the shared FP units under a fixed-priority grant, and an
        # element cursor issues every row's elements strictly in row
        # order (quick rows from the dispatch FIFO, risen rows
        # buffer-direct once their chain is done), so landings stay
        # in order and the D3a landing engine keeps its backbone.
        # ------------------------------------------------------------------
        EC_CTXS = 8  # qualified probe throughput; more contexts add arbitration
        EC_FIFO = 16  # dispatch FIFO depth
        ec_dispatch_done = Signal()
        # Chain contexts: per-ctx step sequencer + arithmetic regs.
        # Steps: 0 widen (f16ws, L2, capture+classify), 1 seed (frec,
        # L3), 2/5 Newton mul, 3/6 sub, 4/7 upd (7 also latches r),
        # 8 the scale product (both classify paths converge), 9 the
        # narrow (f16n, L2, capture -> done). Fixed-latency countdown:
        # cnt <- L at issue, decrement while > 1, capture at cnt == 1
        # (every unit is an in-order stall-free pipe, so capture is
        # exactly issue + L).
        EC_ST = dict(widen=0,
                     seed=1,
                     it1m=2,
                     it1s=3,
                     it1u=4,
                     it2m=5,
                     it2s=6,
                     it2u=7,
                     sc=8,
                     nrm=9)
        ec_st = Array(Signal(4, name=f'ecst{i}') for i in range(EC_CTXS))
        ec_cnt = Array(Signal(3, name=f'eccnt{i}') for i in range(EC_CTXS))
        ec_alloc = Array(Signal(name=f'ecal{i}') for i in range(EC_CTXS))
        ec_done = Array(Signal(name=f'ecdn{i}') for i in range(EC_CTXS))
        ec_xm16 = Array(Signal(16, name=f'ecxm{i}') for i in range(EC_CTXS))
        # a is overwritten by the scale product after its last multiply
        # input use; y's final Newton result stays as r through completion.
        ec_a = Array(Signal(32, name=f'eca{i}') for i in range(EC_CTXS))
        ec_y = Array(Signal(32, name=f'ecy{i}') for i in range(EC_CTXS))
        ec_t = Array(Signal(32, name=f'ect{i}') for i in range(EC_CTXS))
        ec_d16 = Array(Signal(16, name=f'ecd{i}') for i in range(EC_CTXS))
        # Dispatch FIFO: {xh, risen, ctx} per dispatched row.
        ec_f_xh = Array(Signal(16, name=f'efx{i}') for i in range(EC_FIFO))
        ec_f_risen = Array(Signal(name=f'efr{i}') for i in range(EC_FIFO))
        ec_f_ctx = Array(Signal(4, name=f'efc{i}') for i in range(EC_FIFO))
        ec_fhead = Signal(4)
        ec_ftail = Signal(4)
        ec_fcnt = Signal(5)  # 0..16
        # Element cursor: ec_row/ec_ii issue elements in row order;
        # ec_rqi is the risen-row buffer read pointer (the reads run
        # ahead through port steals, at most one element in the port
        # (ec_steal_d1) and one latched in ec_x (ec_xv)).
        ec_row = Signal(7)
        ec_ii = Signal(6)
        ec_rqi = Signal(6)
        ec_x = Signal(16)
        ec_xv = Signal()
        ec_steal_d1 = Signal()
        # A risen row's last element is in flight (issues stop until
        # its completion fires, so the pipeline is drained at the
        # completion and a parked completion never sees another
        # landing); a parked completion (its meta beat was in flight)
        # holds the ctx and the finalized chunk.
        ec_rfly = Signal()
        ec_pend = Signal()
        ec_pend_ctx = Signal(4)
        ec_chunk = Signal(8 * 32)  # a parked completion's finalized chunk
        chunk_assm = Signal(8 * 32)  # a risen row's chunk bytes (in order)
        ec_land_cnt = Signal(5)  # chunk bytes landed so far
        # Quick elements can land after write backpressure starts. Keep
        # their bytes until the in-order compose has payload credit.
        # Empty-queue bypass preserves the unstalled landing schedule.
        ec_qdata = Array(Signal(8, name=f'ecqb{i}') for i in range(16))
        ec_qhead = Signal(4)
        ec_qtail = Signal(4)
        ec_qcnt = Signal(5)
        # Landing-tag pipe, depth 9 = issue -> landing latency
        # (f16w 2 + fmul_q1 3 + fmul_q2 3 + the q_land_v register 1).
        EC_TAGS = 9
        ec_tag_risen = [Signal(name=f'ectr{i}') for i in range(EC_TAGS)]
        ec_tag_last = [Signal(name=f'ectl{i}') for i in range(EC_TAGS)]
        ec_tag_ctx = [Signal(4, name=f'ectc{i}') for i in range(EC_TAGS)]
        # The state accumulators: one flat Array of 64 halfwords -- two
        # banks of 32 (8 rows x 4 halfwords) alternating by beat parity
        # so beat g+1's pre-read never clobbers the composing beat g.
        k4st = Array(Signal(16, name=f'k4st{i}') for i in range(64))
        k4st_v = Signal(2)  # per-bank: the beat's pre-read landed
        # Single-outstanding K4 staging read (TMEM takes one): the
        # class (0 state, 1 meta, 2 payload) and, for state, the beat
        # index (its parity picks the bank).
        k4_rd_busy = Signal()
        k4_rd_cls = Signal(2)
        k4_rd_beat = Signal(4)
        # Next-beat request cursors (state/meta pre-request beat 0 at
        # accept; then state g+1 at each dispatched row&7==0, meta g+1
        # when the previous meta beat's write retires, payload w at
        # each dispatched even row -- gated so a leading response can
        # never clobber an un-composed bank: payload pair P is
        # requested only while P stays within one pair of the landing
        # cursor, state beat g+2 only after beat g's write retired,
        # meta beat g+1 likewise).
        k4st_next = Signal(5)  # 0..16 (16 = every state beat requested)
        k4met_next = Signal(4)  # 0..8 (8 = every meta beat requested)
        # The open block's payload slice holds `_k4pos`'s beats in image
        # order: 64 on the quad, 77 at five lanes, 128 at one lane, so the
        # payload cursor is sized by the slice (see _k4pos for the layout)
        # and never narrower than the quad's.
        _k4groups = max_dims // self.n_outputs
        _k4tail = max_dims - _k4groups * self.n_outputs
        _k4bpg = (self.n_outputs + 1) // 2
        _k4tbg = (_k4tail + 1) // 2
        K4_PAY_BEATS = _k4groups * _k4bpg + _k4tbg
        k4pay_next = Signal(range(max(65, K4_PAY_BEATS + 1)))
        k4st_rq = Signal()
        k4met_rq = Signal()
        k4met_v = Signal()  # a meta beat is latched in meta_hw
        # Payload pre-read registers cycle four banks by pair index
        # (w mod 4): the request gate keeps at most three un-composed
        # pairs in flight, so a bank's previous occupant has always
        # composed before its next response lands.
        k4pay_v = Signal(4)
        k4pay_store = Memory(width=self.data_width,
                             depth=4,
                             attrs={'ram_style': 'distributed'})
        m.submodules.k4pay_read = k4pay_read = k4pay_store.read_port(
            domain='comb')
        m.submodules.k4pay_write = k4pay_write = k4pay_store.write_port()
        # Pair-beat compose results (assigned inside the SG1 staging
        # block where the row assembly is visible).
        k4_even_res = Signal(8 * 32)
        k4_odd_res = Signal(8 * 32)
        # Beat write latches: payload / state / meta. The payload
        # latch loads at the pair's compose moment and cannot wait, so
        # the drain arbiter treats it as before (one deep, checked at
        # dispatch); state and meta latch-and-retry (their beat data
        # is stable until the next pre-read, which is gated on the
        # write retiring).
        k4w_pay_v = Signal()
        k4w_pay_addr = Signal(aw)
        k4w_pay_data = Signal(self.data_width)
        k4w_st_v = Signal()
        k4w_st_addr = Signal(aw)
        k4w_st_data = Signal(self.data_width)
        k4w_met_v = Signal()
        k4w_met_addr = Signal(aw)
        k4w_met_data = Signal(self.data_width)
        k4w_lock = Signal(2)  # 0 unlocked, 1 payload, 2 state, 3 meta
        # D3a deferred beat writes: a state/meta beat completes its
        # rows at a landing (or a risen completion) and its write
        # latch loads as soon as it drains; per-bank pending flags
        # hold the state pre-read of the bank until its beat retired.
        k4w_st_due = Signal()
        k4w_st_idx = Signal(4)
        k4w_st_pend = Signal(2)
        k4w_met_due = Signal()
        k4w_met_idx = Signal(3)

        # Phase membership, driven inside the FSM below.
        in_fill = Signal()
        in_gather = Signal()
        in_scale = Signal()
        in_quant = Signal()
        in_drain = Signal()
        in_sg1w = Signal()  # SG1 staging beat writes
        in_appw = (Signal() if app_on else Const(0))  # append patch drain
        in_drainload = Signal()  # row dispatch / pre-read hold
        in_k4row = Signal()  # K4 walk: dispatch / compare / quick issue
        # The non-quad K staging window: pre-read of the window's
        # payload and meta beats, a rebuilt row's block patches into
        # the store, and the store's write-back. A quad build has no
        # window, so its indicators never leave zero.
        in_kpre = Const(0)
        in_kpatch = Const(0)
        in_kdrain = Const(0)
        if nonquad:
            in_kpre = Signal()
            in_kpatch = Signal()
            in_kdrain = Signal()
        running = Signal()
        m.d.comb += running.eq(in_fill | in_gather | in_scale | in_quant | in_drain \
            | in_sg1w | in_appw | in_k4row | in_kpre | in_kpatch | in_kdrain)

        # ------------------------------------------------------------------
        # Fill: one outstanding TMEM read at a time; each returned
        # beat is absorbed into the buffer one 32-bit word per cycle.
        # A block-1 partial fills from word 32*32 up (the host LOADs
        # just the dirty block's slots re-based at the window image's
        # natural offset), so the read address tracks the same offset.
        # ------------------------------------------------------------------
        fill_word = Signal(range(VB_SLOTS * max_slot_words +
                                 1))  # words stored
        fill_total = Signal(range(VB_SLOTS * max_slot_words +
                                  1))  # words to store (live * 32)
        fill_wlo = Signal(range(VB_SLOTS * max_slot_words +
                                1))  # first buffer word of the fill
        fill_beat = Signal(range(VB_SLOTS * max_slot_words +
                                 1))  # beats requested
        fill_data = Signal(self.data_width)
        fill_have = Signal()  # a beat is being absorbed
        fill_next_data = Signal(self.data_width)  # a parked response
        fill_next_v = Signal()
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
        pre_cnt = Signal(4)  # next meta beat to pre-read (V: 0..3, K: 0)

        # One beat at a time through TMEM's single read slot, with the
        # next request issued two absorb groups before the current
        # beat finishes (D3a) and the response parked in a one-deep
        # hold when it lands mid-absorb, so the slot stays sustained:
        # five cycles per 64 B beat instead of a round trip per beat.
        # The pre-read shares the read port after the fill completes
        # (the phases are disjoint; a response routes by its pending
        # flag).
        # The beat cursor is re-based (block-1 fills start at beat 0),
        # so the request bound must add the fill's first word: without
        # it a block-1 fill keeps requesting past its extent and the
        # stray responses cross the next phase's reads (TMEM accepts
        # back-to-back requests; responses route by engine flags only).
        # D3a fill datapath: absorb min(4, bw) words per cycle into a
        # 128-bit group assembly register, writing each completed
        # 4-word group in one cycle (128 bits per cycle at bw >= 4).
        # The next request issues two absorb cycles before the current
        # beat finishes and the response parks in a one-deep hold when
        # it lands mid-absorb, so TMEM's single read slot stays
        # sustained instead of paying a round trip per beat. A group
        # may straddle two beats at bw < 4; the assembly register and
        # the park/hold handshake carry it across.
        fill_step = min(FILL_LANES, bw)
        fill_grp = Signal(32 * FILL_LANES)  # words staged, low words first
        fill_gcnt = Signal(range(FILL_LANES + 1))
        fill_near_end = Signal()
        m.d.comb += fill_near_end.eq(fill_have
                                     & (fill_wcnt + 2 * fill_step >= bw))
        more_fill = Signal()
        m.d.comb += more_fill.eq(~rd_pend & ~fill_next_v \
            & (~fill_have | fill_near_end) \
            & ((fill_wlo + fill_beat * bw) < fill_total))
        grp_next_value = fill_grp | (fill_data[:32 * fill_step] <<
                                     (32 * fill_gcnt))
        grp_next = Signal.like(grp_next_value)
        m.d.comb += grp_next.eq(grp_next_value)
        grp_done = Signal()
        m.d.comb += grp_done.eq(fill_have
                                & (fill_gcnt + fill_step == FILL_LANES))
        m.d.comb += [
            buf_w.addr.eq(fill_word[2:]),
            buf_w.data.eq(grp_next),
            buf_w.en.eq(grp_done),
        ]
        with m.If(fill_have):
            m.d.sync += [
                fill_grp.eq(Mux(grp_done, 0, grp_next)),
                fill_gcnt.eq(Mux(grp_done, 0, fill_gcnt + fill_step)),
                fill_data.eq(fill_data >> (32 * fill_step)),
                fill_word.eq(fill_word + FILL_LANES * grp_done),
                fill_wcnt.eq(fill_wcnt + fill_step),
            ]
            with m.If(fill_wcnt + fill_step >= bw):
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
        row = Signal(range(max_dims))
        blk = Signal(range(max_blocks))
        gi = Signal(6)  # requests issued, including the terminal count 32
        gather_valid = Signal()
        gather_index = Signal(5)
        gather_live = Signal()
        gather_par = Signal()  # K: the issued element's dim parity
        xmax16 = Signal(16)
        xnan = Signal()  # a live slot of this block is a NaN pattern
        xbuf0 = Array(Signal(16, name=f'xb0_{i}') for i in range(32))
        xbuf1 = Array(Signal(16, name=f'xb1_{i}') for i in range(32))
        # Q3c wide append: segments 2/3 of the 128-dim token row need
        # their own banks (the amax fold spans all four before any
        # quant stream drains a bank). Non-append modes keep the
        # two-bank alternation.
        banks = [xbuf0, xbuf1]
        if max_blocks > 2:
            xbuf2 = Array(Signal(16, name=f'xb2_{i}') for i in range(32))
            xbuf3 = Array(Signal(16, name=f'xb3_{i}') for i in range(32))
            banks += [xbuf2, xbuf3]
        # D3b-2: heal folds complete rows ahead of the scale/element
        # cursors. A bank stays owned until its last element issues;
        # its maximum is published only after the last read returns.
        # Wide K4 uses the append-only banks 2/3, otherwise idle here.
        heal_banks = banks[2:4] if max_blocks > 2 else banks[:2]
        hh_owned = Signal(2)
        hh_ready = Signal(2)
        hh_max = Array(Signal(16, name=f'hhmax{i}') for i in range(2))
        hh_row = Signal(7)
        hh_done = Signal()
        hh_busy = Signal()
        hh_iss = Signal(6)
        hh_valid = Signal()
        hh_index = Signal(5)
        hh_lane = Signal(2)
        hh_par = Signal()
        hh_mag = Signal(16)
        cur_bank = Signal()
        # A V append banks its gather segments by block index; every
        # other mode alternates the two banks per block/row. A K4 run
        # never gets here: it leaves FILL for K4ROW and visits neither
        # GATHER, SCALE nor QUANT, so the walk's block is not a selector.
        # (With it, banks 2 and 3 -- the heal and wide-append banks --
        # carried a gather decode they could never pass.)
        gbank_value = Mux(vapp, blk, cur_bank)
        gbank = Signal.like(gbank_value)
        m.d.comb += gbank.eq(gbank_value)
        qbank_value = Mux(vapp, blk, cur_bank)
        qbank = Signal.like(qbank_value)
        m.d.comb += qbank.eq(qbank_value)

        # Gather the next block while the current block quantizes.
        pgi = Signal(6)
        pgather_valid = Signal()
        pgather_index = Signal(5)
        pgather_live = Signal()
        pgather_par = Signal()
        pxmax16 = Signal(16)
        pxnan = Signal()
        prefetch_done = Signal()

        # The last output row: dim 63 (V) or slot 15 (K); a K append
        # builds exactly one row.
        # A non-quad K window run rebuilds 16 rows (the full
        # orientation), the two rows of the dirty pair (the partial) or
        # the appended token's row (the K append).
        _k_last = Mux(part_k, 1, VB_K_ROWS - 1) if self.n_outputs != 4 \
            else VB_K_ROWS - 1
        last_row_value = Mux(kapp, 0, Mux(krow, _k_last, dims - 1))
        last_row = Signal.like(last_row_value)
        m.d.comb += last_row.eq(last_row_value)
        last_blk_value = Mux(krow & wide, 3, 1)
        last_blk = Signal.like(last_blk_value)
        m.d.comb += last_blk.eq(last_blk_value)
        has_next = Signal()
        m.d.comb += has_next.eq((blk != last_blk) | (row != last_row))
        next_blk_value = Mux(blk != last_blk, blk + 1, 0)
        next_blk = Signal.like(next_blk_value)
        m.d.comb += next_blk.eq(next_blk_value)
        next_row_value = Mux(blk != last_blk, row, row + 1)
        next_row = Signal.like(next_row_value)
        m.d.comb += next_row.eq(next_row_value)

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
        lo_eff_value = Mux(part_vb1, 0, lo_live)
        lo_eff = Signal.like(lo_eff_value)
        m.d.comb += lo_eff.eq(lo_eff_value)
        hi_eff_value = Mux(part_vb0, 0, hi_live)
        hi_eff = Signal.like(hi_eff_value)
        m.d.comb += hi_eff.eq(hi_eff_value)
        # A K row-extent run at or above its live prefix: the row's
        # meta cells keep the pre-read (loaded) bytes instead of the
        # zeros a full run's dead tail writes.
        part_k_row_dead = Signal()
        m.d.comb += part_k_row_dead.eq(part_k & (row >= live))
        cur_live = Mux(krow, Mux(row < live, 32, 0),
                       Mux(blk == 0, lo_eff, hi_eff))
        # A K4 quick row quantizes exactly one element (the new slot's
        # column byte) regardless of the block's live count.
        # A V append never prefetches: its next gather segment is the
        # same row's next 32 dims under the same amax fold. A K append
        # keeps the K-mode next-block prefetch (the block transition
        # consumes pxmax16), with liveness whole-row as always. A K4
        # walk never prefetches either.
        nxt_live_value = Mux(
            vapp, 0,
            Mux(krow, Mux(next_row < live, 32, 0), Mux(blk == 0, hi_eff,
                                                       lo_eff)))
        nxt_live = Signal.like(nxt_live_value)
        m.d.comb += nxt_live.eq(nxt_live_value)

        # Pre-read issue (see the fill-side comment above): fires as
        # the row pass gathers the first row of a meta beat's sixteen.
        # A wide K row has four blocks: eight rows per metadata beat.
        # Partial K updates pre-read and retire only their original pair's
        # metadata beat, even though the local input is rebased to row zero.
        wide_k = Signal()
        m.d.comb += wide_k.eq(krow & wide)
        meta_group_value = Mux(wide_k, row >> 3, row >> 4)
        meta_group = Signal.like(meta_group_value)
        m.d.comb += meta_group.eq(meta_group_value)
        meta_boundary = Signal()
        m.d.comb += meta_boundary.eq(Mux(wide_k, row[:3] == 0, row[:4] == 0))
        meta_count_value = Mux(krow, Mux(wide, 2, 1), dims >> 4)
        meta_count = Signal.like(meta_count_value)
        m.d.comb += meta_count.eq(meta_count_value)
        pre_limit_value = Mux(part_k, 1, meta_count)
        pre_limit = Signal.like(pre_limit_value)
        m.d.comb += pre_limit.eq(pre_limit_value)
        pre_go = Signal()
        pre_addr = Signal(aw)
        if self.n_outputs == 4:
            m.d.comb += pre_go.eq((part != 0) & ~pre_rd & (pre_cnt < pre_limit) \
                & meta_boundary & (meta_group == pre_cnt) & in_gather)
            pre_index_value = Mux(part_k & wide_k, pair_base >> 3, pre_cnt)
            pre_index = Signal.like(pre_index_value)
            m.d.comb += pre_index.eq(pre_index_value)
            m.d.comb += pre_addr.eq(m_base + (pre_index << 6))
        else:
            # Generalized interleave: two due-driven pre-read walks
            # sharing the single outstanding beat -- the partial's
            # meta beats (into the alternating accumulators) and its
            # straddle-boundary payload beats (the retained neighbor
            # half). A walk fires once the row cursor is within two
            # rows of its op's first consumer: a row's
            # gather/scale/quant is tens of cycles, so the response
            # lands long before the scatter or compose that consumes
            # it (the DRAINLOAD pre-read hold below is the backstop).
            lanes = self.n_outputs
            _vrows_set = (64, max_dims) if max_dims != 64 else (64, )

            def _gl(g, vr):
                return lanes if (g + 1) * lanes <= vr else vr - g * lanes

            def _cell(r, j, vr):
                g, l = divmod(r, lanes)
                return g * 2 * lanes + j * _gl(g, vr) + l

            # (vr, beat, block) -> the first/last row scattering that
            # block's cell into that meta beat; block None is the full
            # run's last row over either block.
            _first = {}
            _lastrow = {}
            for _vr in _vrows_set:
                for _bm in range(_vr * 2 // 32):
                    for _b in (0, 1):
                        _rs = [
                            r for r in range(_vr)
                            if _cell(r, _b, _vr) >> 5 == _bm
                        ]
                        _first[(_vr, _bm, _b)] = min(_rs)
                        _lastrow[(_vr, _bm, _b)] = max(_rs)
                    _lastrow[(_vr, _bm, None)] = max(_lastrow[(_vr, _bm, 0)],
                                                     _lastrow[(_vr, _bm, 1)])
            _maxmb = max(_vr * 2 // 32 for _vr in _vrows_set)
            _groups_n = (64 + lanes - 1) // lanes
            _groups_w = ((max_dims + lanes - 1) //
                         lanes if max_dims != 64 else _groups_n)
            _tail_n = 64 - (_groups_n - 1) * lanes
            _tail_w = (max_dims -
                       (_groups_w - 1) * lanes if max_dims != 64 else _tail_n)

            def _fw(m, b):
                """The partial's first scatter row for meta beat ``m``
                (its block cells), muxed by the tile width."""
                if (64, m, b) in _first:
                    if max_dims != 64:
                        return Mux(wide, _first[(max_dims, m, b)],
                                   _first[(64, m, b)])
                    return _first[(64, m, b)]
                return _first[(max_dims, m, b)]

            gmet_cnt = Signal(range(_maxmb + 1))
            gbdy_cnt = Signal(range(max_dims // lanes + 1))
            pre_kind = Signal(2)  # latched at issue: 0 V meta beat, 1 V
            pre_acc = Signal()  # boundary beat, 2 K payload, 3 K meta
            pre_bpar = Signal()  # a boundary op's group parity
            mbeats_cur = (Mux(wide, _maxmb, 64 * 2 //
                              32) if max_dims != 64 else 64 * 2 // 32)

            def _bdy_lim(vr):
                """Groups whose lane count is odd -- the ones whose
                block boundary straddles a payload beat."""
                return sum(1 for g in range((vr + lanes - 1) // lanes)
                           if _gl(g, vr) % 2)

            bdy_lim = (Mux(wide, _bdy_lim(max_dims), _bdy_lim(64))
                       if max_dims != 64 else _bdy_lim(64))
            _fdue = [
                Mux(part_blk, _fw(m, 1), _fw(m, 0)) for m in range(_maxmb)
            ]
            gmet_due = part_vb & (gmet_cnt < mbeats_cur) \
                & (row + 2 >= Array(_fdue)[gmet_cnt])
            # The boundary beat's fire row follows the partial's
            # compose side: block 0 composes the straddle at the
            # group's LAST row (pre-read at the group's first row --
            # any earlier and the NEXT group's boundary would land
            # before this group's compose consumed the register),
            # block 1 composes it at the group's FIRST row (pre-read
            # two rows ahead).
            # Limit lookahead to a group so a pre-read cannot overwrite
            # the parity bank still needed by the current row.
            lookahead = min(2, lanes)
            block0_delay = min(2, lanes - 1)
            gbdy_due = (part_vb & (gbdy_cnt < bdy_lim)
                        & (row + lookahead >= gbdy_cnt * lanes +
                           Mux(part_blk, 0, block0_delay)))
            m.d.comb += pre_go.eq(part_vb & ~pre_rd & ~in_fill
                                  & (gmet_due | gbdy_due)
                                  & (in_gather | in_scale | in_quant
                                     | in_drainload | in_sg1w))
            # The boundary beat of group g (of lane count gl, odd) pairs
            # block 0's lane gl-1 with block 1's lane 0: chunk
            # g*2*lanes + gl - 1, an even staging index, so the beat's
            # byte offset is that chunk times 32.
            _gl_t = Mux(gbdy_cnt == Mux(wide, _groups_w, _groups_n) - 1,
                        Mux(wide, _tail_w, _tail_n), lanes)
            m.d.comb += pre_addr.eq(
                Mux(gmet_due, m_base + (gmet_cnt << 6),
                    r_base + ((gbdy_cnt * (2 * lanes) + _gl_t - 1) << 5)))

        # ---- non-quad K staging window: the pre-read walk ----
        # One outstanding beat at a time through the shared pre-read
        # flag: the window's payload beats 0..kpb-1 from r_base into
        # the store, then its meta beats 0..kmb-1 from m_base into the
        # register banks. The response routes by pre_kind (2 payload,
        # 3 meta) and the in-flight beat index latched at issue. Every
        # K mode of a non-quad build is a window run (kwin_mode): the
        # full 16-row orientation, the two-row partial and the K
        # append differ only in which rows it rebuilds.
        kwin_mode = Const(0)
        kpre_go = Const(0)
        kpre_addr = Const(0, aw)
        kd_pay = Const(0)
        kd_v = Const(0)
        kd_last = Const(0)
        if nonquad:
            kwin_mode = Signal()
            # krow is also set for a V append (its token row gathers in
            # the K direction); the window is the K modes' alone.
            m.d.comb += kwin_mode.eq(krow & ~vapp)
            kpre_pay = Signal()
            m.d.comb += kpre_pay.eq(kpp < kpb)
            kpre_go = Signal()
            m.d.comb += kpre_go.eq(in_kpre & ~pre_rd
                                   & (kpre_pay | (kpm < kmb)))
            kpre_addr = Signal(aw)
            m.d.comb += kpre_addr.eq(
                Mux(kpre_pay, r_base + (kpp << 6), m_base + (kpm << 6)))
            # The cursor names the next beat to stage. A one-entry output
            # register isolates the BRAM/meta mux from the shared TMEM port.
            kd_pending = Signal()
            kd_v = Signal()
            kd_last = Signal()
            kd_addr = Signal(aw)
            kd_data = Signal(self.data_width)
            kd_stage = Signal()
            kd_pop = Signal()
            kd_pay = Signal()
            m.d.comb += [
                kd_pay.eq(kdb < kpb),
                kd_pop.eq(in_kdrain & self.tmem_w.fire),
                kd_stage.eq(in_kdrain & (kdb < kpb + kmb)
                            & Mux(kd_pay, kdb_d == kdb, 1)
                            & (~kd_pending | kd_pop)),
                kd_v.eq(in_kdrain & kd_pending),
                kwin_r.addr.eq(kdb),
                kwin_r.en.eq(in_kdrain & kd_pay),
            ]
            with m.If(kwin_r.en):
                m.d.sync += kdb_d.eq(kdb)
            with m.Else():
                m.d.sync += kdb_d.eq(_kwin_depth + _kwin_mb_max)
            with m.If(kd_stage):
                m.d.sync += [
                    kd_pending.eq(1),
                    kd_last.eq(kdb == kpb + kmb - 1),
                    kdb.eq(kdb + 1),
                ]
            with m.Elif(kd_pop):
                m.d.sync += kd_pending.eq(0)

        # Append-mode pre-read: one outstanding beat at a time, the
        # patch drain's compose source. A V append's beat w < 32 (64)
        # is block app_blk's payload beat (group w>>1, pair w&1); the
        # remaining four beats are the F32 scale row at m_base. A K
        # append's beats 0..blocks-1 are the token's per-block payload
        # beats (lane pair kr>>1 & 1) and the last beat its meta beat.
        # K: the meta beat is beat index `blocks` (2 narrow, 4 wide).
        if app_on:
            app_kmeta = Signal()
            m.d.comb += app_kmeta.eq(
                kapp & (app_w == Mux(wide, Const(4, 7), Const(2, 7))))
            app_kmetabeat = Signal()
            m.d.comb += app_kmetabeat.eq(Mux(wide, kr[3], 0))
            # A K payload beat b sits at the token's own row-pair group and
            # lane: ((r>>2)*4*blocks + b*4 + (r&3)&~1) beats from r_base.
            app_kpay_value = r_base + Mux(wide, kr[2:4] << 9, kr[2:4] << 8) \
                + (app_w << 7) + (kr[1] << 6)
            app_kpay = Signal.like(app_kpay_value)
            m.d.comb += app_kpay.eq(app_kpay_value)
            # ---- non-quad V append walk (the generalized patch drain) ----
            # Block app_blk's cells sit at c(d) = g*2*lanes + app_blk*lanes
            # + l, so its payload is no longer a run of self-contained
            # beats: the drain visits the distinct beats holding those
            # cells in order, pre-reads each and patches the one or two of
            # its rows in it (an even cell is a beat's low half, the next
            # row's odd cell that beat's high half; a group's last even
            # cell lands alone under the other block's retained half).
            # ``arow``/``acell``/``alane`` walk row, cell and lane together
            # (+1 within a group, lanes+1 across one); ``abeat`` is the beat
            # being composed, ``asec``/``aph2`` the second row's delayed
            # patch, ``asrow`` the four-beat scale-row phase.
            app_srow_q = Mux(wide, app_w[6], app_w[5])
            app_raddr_value = Mux(
                kapp, Mux(app_kmeta, m_base + (app_kmetabeat << 6), app_kpay),
                Mux(
                    app_srow_q,
                    m_base + (Mux(wide, app_w[:6], app_w[:5]) << 6), r_base +
                    (app_w[1:7] << 8) + (app_w[0] << 6) + (app_blk << 7)))
            app_req_value = in_appw & ~app_rd & ~app_ph
            app_srow_value = app_srow_q
            if nonquad:
                _L = self.n_outputs
                arow = Signal(range(max_dims + 1))
                acell = Signal(range(max_dims * 2 + 1))
                alane = Signal(range(_L))
                abeat = Signal(7)
                asec = Signal()
                aph2 = Signal()
                asrow = Signal()
                # The scale-row phase keeps the quad's four app_w beats.
                app_req_value = in_appw & ~app_rd & ~app_ph & ~aph2
                app_srow_value = asrow
                app_raddr_value = Mux(asrow, m_base + (app_w[:2] << 6),
                                      r_base + ((acell >> 1) << 6))
            app_req = Signal()
            m.d.comb += app_req.eq(app_req_value)
            app_srow = Signal()
            m.d.comb += app_srow.eq(app_srow_value)
            app_raddr = Signal.like(app_raddr_value)
            m.d.comb += app_raddr.eq(app_raddr_value)
        else:
            app_req = Const(0)
            app_srow = Const(0)
            app_raddr = Const(0, aw)

        # The unit's single TMEM read port: the fill while filling,
        # then the meta pre-reads of a partial run, the append drain's
        # beat pre-reads, and the K4 walk's state/meta/payload beat
        # pre-reads (one outstanding K4 read at a time; TMEM itself
        # accepts one -- the class register routes the response).
        in_k4w = in_k4row

        # Every K4 request and completion stays in K4ROW. The state
        # request additionally waits for the bank's previous beat
        # write to retire, and the payload request stays within the
        # landing gate -- a leading response must never clobber a
        # bank whose pair has not composed.
        # Payload request gate: a leading response may never clobber an
        # un-composed bank -- at most three pairs beyond the landing
        # pair (k4_land_row[1:] is the last completed row's pair), four
        # banks cycling, so a bank's previous occupant has always
        # composed first.
        # ---- non-quad K4 positions ----
        # A wide tile row r sits at staging cell c(r) = g*2*lanes +
        # blk*gl + l ((g, l) = divmod(r, lanes), gl the group's lane
        # count), so the open block's payload slice is its beats in
        # image order, ranked: a full group gives each block
        # (lanes + 1) // 2 beats -- at an odd lane count one of them,
        # the straddle beat, is shared (five lanes: lanes 0-1, lanes
        # 2-3, then block 0's lane 4 over block 1's lane 0) -- and a
        # short tail group fewer. A row COMPOSES when its cell is a
        # beat's high half (the low half is the previous row's, held)
        # or when the cell above it is not this block's (an even cell:
        # it lands alone in the low half over the retained neighbour).
        # `_k4pos` derives a tile row's group, lane, beat rank, compose
        # role and meta beat.

        def _k4pos(r, tag):
            # Every field is a constant function of (app_blk, r), read
            # from a table. The arithmetic this replaces -- a constant
            # division, two products and a subtraction, in carry chains --
            # was the head of the unit's worst path: from k4_land_row
            # through the compose role and the beat bank to the meta
            # accumulator's write decode.
            lanes = self.n_outputs

            def _pos(blk, rr):
                g_, l_ = divmod(rr, lanes)
                tail_ = g_ == _k4groups
                gl_ = _k4tail if tail_ else lanes
                return (g_, l_, tail_, gl_, g_ * 2 * lanes + gl_ * blk + l_,
                        (_k4groups * _k4bpg if tail_ else g_ * _k4bpg) +
                        (((gl_ * blk + l_) >> 1) - ((gl_ * blk) >> 1)),
                        (l_ + 1) < gl_)

            entries = [
                _pos(blk, rr) for blk in (0, 1) for rr in range(1 << len(r))
            ]
            index = Cat(r, app_blk)

            def _table(width, field, name):
                sig = Signal(width, name=name)
                m.d.comb += sig.eq(
                    Array([Const(e[field], width) for e in entries])[index])
                return sig

            pos = {}
            pos['cell'] = cell = _table(
                len(Signal(range(max_dims * 2 * lanes))), 4, f'k4{tag}_cell')
            pos['l'] = l = _table(len(Signal(range(lanes))), 1, f'k4{tag}_l')
            pos['nxt'] = nxt = _table(1, 6, f'k4{tag}_nxt')
            pos['odd'] = odd = Signal(name=f'k4{tag}_odd')
            pos['comp'] = comp = Signal(name=f'k4{tag}_comp')
            m.d.comb += [odd.eq(cell[0]), comp.eq(cell[0] | ~nxt)]
            pos['rank'] = _table(len(Signal(range(K4_PAY_BEATS + 2))), 5,
                                 f'k4{tag}_rank')
            pos['mbeat'] = mbeat = Signal(3, name=f'k4{tag}_mbeat')
            m.d.comb += mbeat.eq(cell[5:8])
            return pos

        # The last tile row whose block cell falls in each meta beat
        # (32 cells): a beat's write arms at that row's completion,
        # whatever the block (the quad's 16-row multiple).
        _k4met_last = {}
        for _blk in (0, 1):
            for _m in range(max_dims * 2 // 32):
                _rows = [
                    r for r in range(max_dims)
                    if ((((r // self.n_outputs) * 2 * self.n_outputs) + _blk *
                         (self.n_outputs if
                          (r // self.n_outputs) < _k4groups else _k4tail) +
                         (r % self.n_outputs)) >> 5) == _m
                ]
                _k4met_last[(_blk, _m)] = max(_rows)

        # Composed beats since accept: the non-quad pre-read gate rides
        # this count instead of the landing row's pair index (a quad
        # build keeps the pair gate and never counts).
        k4_ncomp = Const(0)
        if nonquad:
            k4_ncomp = Signal(range(K4_PAY_BEATS + 1))
        k4pl = _k4pos(k4_land_row, 'l') if nonquad else None
        k4pd = _k4pos(row, 'd') if nonquad else None
        k4pay_gate_value = k4_land_row[1:] + 3
        if nonquad:
            # Three un-composed beats at most: the count rides the
            # completions instead of the landing row's pair index.
            k4pay_gate_value = k4_ncomp + 3
        k4pay_gate = Signal.like(k4pay_gate_value)
        m.d.comb += k4pay_gate.eq(k4pay_gate_value)
        k4st_rq_go = Signal()
        m.d.comb += k4st_rq_go.eq(in_k4w & k4st_rq & ~k4_rd_busy \
            & ~k4w_st_pend.bit_select(k4st_next[0], 1))
        k4met_rq_go = Signal()
        m.d.comb += k4met_rq_go.eq(in_k4w & k4met_rq & ~k4_rd_busy)
        # Decoupled dispatch may cross several pairs while one request
        # is pending. Track demand by the cursor, rather than collapsing
        # those events into a single request bit.
        k4pay_demand = Signal()
        k4pay_demand_value = ((k4pay_next < 64) & (k4pay_next <= (row >> 1)))
        if nonquad:
            # The dispatch row's own beat: every beat through it.
            k4pay_demand_value = ((k4pay_next < K4_PAY_BEATS)
                                  & (k4pay_next <= k4pd['rank']))
        m.d.comb += k4pay_demand.eq(k4pay_demand_value)
        k4pay_rq_go = Signal()
        m.d.comb += k4pay_rq_go.eq(in_k4w & k4pay_demand & ~k4_rd_busy \
            & (k4pay_next <= k4pay_gate))
        k4pay_addr_value = (r_base + (k4pay_next[1:7] << 8) +
                            (k4pay_next[0] << 6) + (app_blk << 7))
        if nonquad:
            # The block's slice: beat ``rank`` at r_base + rank beats.
            k4pay_addr_value = r_base + (k4pay_next << 6)
        rreq_go = ((in_fill & more_fill) | pre_go | app_req | k4st_rq_go
                   | k4met_rq_go | k4pay_rq_go)
        rreq_addr = Mux(
            k4pay_rq_go, k4pay_addr_value,
            Mux(
                k4met_rq_go, m_base + (k4met_next << 6),
                Mux(
                    k4st_rq_go,
                    m_base + (dims << 2) + (app_blk << 10) + (k4st_next << 6),
                    Mux(
                        app_req, app_raddr,
                        Mux(pre_go, pre_addr,
                            a_base + (fill_wlo << 2) + (fill_beat << lg))))))
        if nonquad:
            # The K window's pre-read owns the port outside the other
            # engines' phases; it still sits first in the address mux.
            rreq_go = rreq_go | kpre_go
            rreq_addr = Mux(kpre_go, kpre_addr, rreq_addr)
        m.d.comb += [
            self.tmem_rreq.valid.eq(rreq_go),
            self.tmem_rreq.bits.addr.eq(rreq_addr),
            self.tmem_rresp.ready.eq(running | (
                in_drainload if self.n_outputs != 4 else 0)),
        ]
        if nonquad:
            with m.If(kpre_go & self.tmem_rreq.fire):
                m.d.sync += [
                    pre_rd.eq(1),
                    pre_kind.eq(Mux(kpre_pay, 2, 3)),
                    kpre_idx.eq(Mux(kpre_pay, kpp, kpm)),
                ]
                with m.If(kpre_pay):
                    m.d.sync += kpp.eq(kpp + 1)
                with m.Else():
                    m.d.sync += kpm.eq(kpm + 1)
        with m.If(in_fill & more_fill & self.tmem_rreq.fire):
            m.d.sync += [rd_pend.eq(1), fill_beat.eq(fill_beat + 1)]
        with m.If(pre_go & self.tmem_rreq.fire):
            m.d.sync += pre_rd.eq(1)
            if self.n_outputs != 4:
                # Latch the winning walk's kind (the address mux's
                # priority: meta over boundary) and advance it; the
                # accumulator parity rides with a meta op.
                m.d.sync += [
                    pre_kind.eq(Mux(gmet_due, 0, 1)),
                    pre_acc.eq(gmet_cnt[0]),
                    pre_bpar.eq(gbdy_cnt[0]),
                ]
                with m.If(gmet_due):
                    m.d.sync += gmet_cnt.eq(gmet_cnt + 1)
                with m.Else():
                    m.d.sync += gbdy_cnt.eq(gbdy_cnt + 1)
        if app_on:
            with m.If(app_req & self.tmem_rreq.fire):
                m.d.sync += app_rd.eq(1)
        # The port's winner under the address mux's priority.
        win_met = Signal()
        m.d.comb += win_met.eq(~k4pay_rq_go & k4met_rq_go)
        win_st = Signal()
        m.d.comb += win_st.eq(~k4pay_rq_go & ~k4met_rq_go & k4st_rq_go)
        with m.If((k4st_rq_go | k4met_rq_go | k4pay_rq_go)
                  & ~rd_pend & ~pre_rd & ~app_rd & self.tmem_rreq.fire):
            # The class/beat bookkeeping MUST mirror the address mux's
            # priority (payload over meta over state) -- a simultaneous
            # state+meta request would otherwise label the response
            # with the wrong class.
            m.d.sync += [
                k4_rd_busy.eq(1),
                k4_rd_cls.eq(Mux(k4pay_rq_go, 2, Mux(k4met_rq_go, 1, 0))),
                k4_rd_beat.eq(
                    Mux(
                        k4pay_rq_go, Cat(k4pay_next[:4], Const(0, 4)),
                        Mux(k4met_rq_go, Cat(k4met_next, Const(0, 1)),
                            k4st_next))),
                # Only the port's WINNER clears its request flag --
                # the losers stay armed and retry (the rq_go terms are
                # demand signals, not grants).
                k4st_rq.eq(k4st_rq & ~win_st),
                k4met_rq.eq(k4met_rq & ~win_met),
            ]
        with m.If(rd_pend & self.tmem_rresp.fire):
            # D3a: a response landing mid-absorb parks in the hold;
            # the absorb's last group take-over loads it next cycle.
            with m.If(fill_have):
                m.d.sync += [
                    rd_pend.eq(0),
                    fill_next_v.eq(1),
                    fill_next_data.eq(self.tmem_rresp.bits.data),
                ]
            with m.Else():
                m.d.sync += [
                    rd_pend.eq(0),
                    fill_have.eq(1),
                    fill_data.eq(self.tmem_rresp.bits.data),
                    fill_wcnt.eq(0),
                ]
        with m.If(~fill_have & fill_next_v):
            m.d.sync += [
                fill_next_v.eq(0),
                fill_have.eq(1),
                fill_data.eq(fill_next_data),
                fill_wcnt.eq(0),
            ]

        # Buffer word j*32 + (d >> 1) is Cat(d[1:6], i, b); the
        # halfword is d & 1. Delay the index and live mask alongside
        # the RAM read; row/blk stay fixed until the last response lands.
        # K orientation: word row*32 + (blk*32+gi >> 1), halfword gi & 1.
        # D3a: buffer words are 4-lane 128-bit memory words -- the flat
        # word address maps to memory word >> 2, lane & 3, and the lane
        # rides the same one-cycle delay as the index.
        gather_go = Signal()
        m.d.comb += gather_go.eq(in_gather & (gi < cur_live))
        prefetch_go = Signal()
        m.d.comb += prefetch_go.eq(in_quant & has_next & (pgi < nxt_live))
        # The quant stream's issue/landing counters (declared early:
        # the K4 direct-read lookahead indexes the issue counter).
        q_iss = Signal(6)
        q_land = Signal(6)

        # D3a dispatch stream (the landing engine sits below the quant
        # pipeline, where its inputs are defined). Rows dispatch one
        # per cycle while their state beat is landed, an odd row's
        # payload beat has arrived, and -- D3b -- the dispatch bounds
        # hold (a decoupled run bounds the walker by FIFO space and a
        # free chain context for a rising row; heal additionally waits
        # for its folded bank). The condition itself
        # (k4_walk_go, before the rise test) drives the streaming
        # buffer address, so a cursor buffer-steal must block the
        # next cycle's dispatch (the walk re-presents its row).
        K4Q_CAP = 15  # in-flight elements (the state-bank
        #   protocol rides the request gating, not this cap).
        k4_stream_rd = Signal()
        m.d.comb += k4_stream_rd.eq(k4 & in_k4row & ~k4_heal)
        buf_word = Signal(VB_SLOTS * max_slot_words)
        buf_lane = buf_word[:2]
        gather_lane = Signal(2)
        pgather_lane = Signal(2)
        k4_lane = Signal(2)
        gather32 = buf_r.data.word_select(gather_lane, 32)
        pgather32 = buf_r.data.word_select(pgather_lane, 32)
        k4_word32 = buf_r.data.word_select(k4_lane, 32)
        with m.If(k4_stream_rd):
            m.d.sync += k4_lane.eq(buf_lane)

        # K4 compare state: the new value's pattern vs the retained
        # winner. State row d lives at flat halfword index
        # 32*bank + 4*(d & 7) with bank = (d >> 3) & 1 (= row[3]):
        # pattern at +0, the r bits at +2/+3. The three reads use a
        # linear 16-way selector over row[3:0] with constant indices:
        # dynamic Array reads with summed indices lower to nested full
        # cases (16 x 128 x 128 items, ~65k lines per use) and starve
        # both verilation and synthesis.
        k4_xh = Signal(16)
        m.d.comb += k4_xh.eq((k4_word32 >> (16 * row[0]))[:16])
        k4_mag_value = Mux(k4_heal, hh_max[row[0]], k4_xh & 0x7fff)
        k4_mag = Signal.like(k4_mag_value)
        m.d.comb += k4_mag.eq(k4_mag_value)
        k4_nan = Signal()
        m.d.comb += k4_nan.eq(k4_mag >= 0x7c01)
        k4_pat = Signal(16)
        for i in range(16):
            base = 32 * ((i >> 3) & 1) + 4 * (i & 7)
            with m.If(row[:4] == i):
                m.d.comb += k4_pat.eq(k4st[base])
        k4_rise = Signal()
        m.d.comb += k4_rise.eq((~k4_nan & (k4_mag > k4_pat)) | k4_heal)

        k4_st_ok = Signal()
        m.d.comb += k4_st_ok.eq(Mux(row[3], k4st_v[1], k4st_v[0]))
        ec_free_any = Cat(*[~a for a in ec_alloc]).any()
        k4_walk_go = Signal()
        # A composing row's dispatch waits for its beat's pre-read and
        # the free write latch (quad: every odd row composes its pair).
        _k4w_bank_ok = (~row[0] | (~k4w_pay_v
                                   & k4pay_v.bit_select(row[1:3], 1)))
        if nonquad:
            _k4w_bank_ok = (~k4pd['comp']
                            | (~k4w_pay_v
                               & k4pay_v.bit_select(k4pd['rank'][:2], 1)))
        m.d.comb += k4_walk_go.eq(k4 & in_k4row & k4_bufd \
            & ~k4_done_wait & ~ec_dispatch_done & k4_st_ok & ~ec_steal_d1 \
            & (~k4_heal | hh_ready.bit_select(row[0], 1)) \
            & _k4w_bank_ok \
            & (ec_fcnt != EC_FIFO) & (~k4_rise | ec_free_any))
        k4q_stream = Signal()
        m.d.comb += k4q_stream.eq(k4 & in_k4row)
        # The dispatch event itself (quick or risen), split here where
        # the rise test lives.
        k4_disp = k4_walk_go
        # Dispatch only enqueues; the element cursor owns every
        # quantizer issue and its matching landing tag.

        # --------------------------------------------------------------
        # D3b element cursor: issues every row's
        # elements strictly in row order -- quick rows their single
        # column byte straight from the dispatch FIFO, risen rows
        # their block-live elements buffer-direct (the walk's strided
        # addressing, driven by the cursor's counters) once the row's
        # chain context is done. Elements enter the same
        # f16w -> fmul_q1 -> fmul_q2 pipeline, so landings stay in
        # order and the D3a landing engine keeps its in-order
        # backbone. Buffer-port steals take cycles the walker is not
        # dispatching; the port data lags one cycle (a steal at t
        # presents its word at t+1, matching k4_lane's registration).
        # A risen row's LAST element additionally requires every
        # earlier row's completion to have fired, and no further
        # element issues until its own completion fires (ec_rfly):
        # the pipeline is then fully drained at the completion, so a
        # parked completion (its meta beat in flight, ec_pend) never
        # sees another landing arrive behind it.
        # --------------------------------------------------------------
        ec_active = Signal()
        m.d.comb += ec_active.eq(k4 & in_k4row)
        ec_head_risen = Signal()
        ec_head_xh = Signal(16)
        ec_head_ctx = Signal(4)
        m.d.comb += [
            ec_head_risen.eq(ec_f_risen[ec_fhead]),
            ec_head_xh.eq(ec_f_xh[ec_fhead]),
            ec_head_ctx.eq(ec_f_ctx[ec_fhead])
        ]
        # A risen row quantizes its block's live columns (the quick
        # row's single element is the FIFO's retained column byte).
        ec_live_value = Mux(app_blk, hi_live, lo_live)
        ec_live = Signal.like(ec_live_value)
        m.d.comb += ec_live.eq(ec_live_value)
        ec_ii_last = Signal()
        m.d.comb += ec_ii_last.eq(ec_ii == (ec_live - 1))
        ec_gates = Signal()
        m.d.comb += ec_gates.eq(ec_active & (ec_fcnt != 0) & ~ec_rfly & ~ec_pend \
            & (k4q_pend + ec_qcnt < K4Q_CAP) \
            & Mux(ec_head_risen,
                  ec_done[ec_head_ctx]
                  & (~ec_ii_last | (k4_land_row == ec_row)),
                  1))
        ec_issue_skid = Signal()
        m.d.comb += ec_issue_skid.eq(ec_gates & ec_head_risen & ec_xv)
        ec_issue_byp = Signal()
        m.d.comb += ec_issue_byp.eq(ec_gates & ec_head_risen & ~ec_xv
                                    & ec_steal_d1)
        ec_elem_go = Signal()
        m.d.comb += ec_elem_go.eq(ec_gates
                                  & (~ec_head_risen | k4_heal | ec_issue_skid
                                     | ec_issue_byp))
        ec_last_issue = Signal()
        m.d.comb += ec_last_issue.eq(ec_elem_go
                                     & (~ec_head_risen | ec_ii_last))
        ec_port_x = (k4_word32 >> (16 * ec_row[0]))[:16]
        ec_heal_x_value = Mux(ec_row[0], heal_banks[1][ec_ii[:5]],
                              heal_banks[0][ec_ii[:5]])
        ec_heal_x = Signal.like(ec_heal_x_value)
        m.d.comb += ec_heal_x.eq(ec_heal_x_value)
        ec_elem_x_value = Mux(
            k4_heal, ec_heal_x,
            Mux(ec_head_risen, Mux(ec_xv, ec_x, ec_port_x), ec_head_xh))
        ec_elem_x = Signal.like(ec_elem_x_value)
        m.d.comb += ec_elem_x.eq(ec_elem_x_value)
        # Steals: one outstanding read at a time (at most one element
        # in the port and one latched in the skid); a fresh steal may
        # follow an in-flight one only when its element issues from
        # the port this cycle (the bypass).
        ec_want = Signal()
        m.d.comb += ec_want.eq(ec_active & ~k4_heal & (ec_fcnt != 0) & ec_head_risen \
            & (ec_rqi != ec_live) & ~ec_xv \
            & (~ec_steal_d1 | ec_issue_byp))
        ec_rd = Signal()
        m.d.comb += ec_rd.eq(ec_want & ~k4_walk_go)
        ec_dword_value = (((app_blk << 5) + ec_rqi) << 6) + (ec_row >> 1)
        ec_dword = Signal.like(ec_dword_value)
        m.d.comb += ec_dword.eq(ec_dword_value)
        # The heal gather owns the buffer read port; quantization reads
        # its retained bank and therefore cannot contend with prefetch.
        hh_start = Signal()
        m.d.comb += hh_start.eq(ec_active & k4_heal & ~hh_done & ~hh_busy \
            & ~hh_owned.bit_select(hh_row[0], 1))
        hh_rd = Signal()
        m.d.comb += hh_rd.eq(ec_active & k4_heal & hh_busy
                             & (hh_iss != ec_live))
        hh_dword_value = (((app_blk << 5) + hh_iss) << 6) + (hh_row >> 1)
        hh_dword = Signal.like(hh_dword_value)
        m.d.comb += hh_dword.eq(hh_dword_value)
        m.d.sync += hh_valid.eq(hh_rd)
        with m.If(hh_start):
            m.d.sync += [
                hh_busy.eq(1),
                hh_iss.eq(0),
                hh_mag.eq(0),
                hh_owned.bit_select(hh_row[0], 1).eq(1)
            ]
        with m.If(hh_rd):
            m.d.sync += [
                hh_iss.eq(hh_iss + 1),
                hh_index.eq(hh_iss[:5]),
                hh_lane.eq(buf_lane),
                hh_par.eq(hh_row[0])
            ]
        hh_x = (buf_r.data.word_select(hh_lane, 32) >> (16 * hh_par))[:16]
        hh_xmag_value = hh_x & 0x7fff
        hh_xmag = Signal.like(hh_xmag_value)
        m.d.comb += hh_xmag.eq(hh_xmag_value)
        hh_winner_value = Mux((hh_xmag < 0x7c01) & (hh_xmag > hh_mag), hh_xmag,
                              hh_mag)
        hh_winner = Signal.like(hh_winner_value)
        m.d.comb += hh_winner.eq(hh_winner_value)
        with m.If(hh_valid):
            for b in range(2):
                with m.If(hh_row[0] == b):
                    m.d.sync += heal_banks[b][hh_index].eq(hh_x)
            m.d.sync += hh_mag.eq(hh_winner)
            with m.If(hh_index == ec_live - 1):
                m.d.sync += [
                    hh_busy.eq(0), hh_max[hh_row[0]].eq(hh_winner),
                    hh_ready.bit_select(hh_row[0], 1).eq(1)
                ]
                with m.If(hh_row == last_row):
                    m.d.sync += hh_done.eq(1)
                with m.Else():
                    m.d.sync += hh_row.eq(hh_row + 1)
        # The dispatched row's r bits: the state's stored r (quick) or
        # the chain context's fresh r (risen), riding the same
        # two-deep rpipe the heal walk's quick stream uses.
        ec_st_r = Signal(32)
        for i in range(16):
            ebase = 32 * ((i >> 3) & 1) + 4 * (i & 7)
            with m.If(ec_row[:4] == i):
                m.d.comb += ec_st_r.eq(Cat(k4st[ebase + 2], k4st[ebase + 3]))
        ec_r_sel_value = Mux(ec_head_risen, ec_y[ec_head_ctx], ec_st_r)
        ec_r_sel = Signal.like(ec_r_sel_value)
        m.d.comb += ec_r_sel.eq(ec_r_sel_value)

        # The walk streams the appended slot's dim words (one word
        # covers a dim pair; the port presents the NEXT row's word
        # while dispatching, else the current row's -- so a stalled or
        # warming dispatch always finds its word); the cursor's steals
        # present the next risen element's strided word.
        k4_srow_value = Mux(k4_walk_go & (row != last_row), (row + 1) >> 1,
                            row >> 1)
        k4_srow = Signal.like(k4_srow_value)
        m.d.comb += k4_srow.eq(k4_srow_value)
        m.d.comb += [
            buf_word.eq(
                Mux(
                    hh_rd, hh_dword,
                    Mux(
                        krow,
                        Mux(
                            prefetch_go, next_row * slot_words +
                            (next_blk << 4) + (pgi >> 1),
                            row * slot_words + (blk << 4) + (gi >> 1)),
                        Mux(
                            k4_stream_rd,
                            Mux(ec_rd, ec_dword, ((live - 1) << 6) + k4_srow),
                            Mux(prefetch_go,
                                ((next_blk << 5) + pgi) * slot_words +
                                (next_row >> 1),
                                ((blk << 5) + gi) * slot_words +
                                (row >> 1)))))),
            buf_r.addr.eq(buf_word[2:]),
            buf_r.en.eq(gather_go | prefetch_go | k4_stream_rd | hh_rd),
        ]

        # Cursor advance and steal handshake.
        m.d.sync += ec_steal_d1.eq(ec_rd)
        with m.If(ec_rd):
            m.d.sync += ec_rqi.eq(ec_rqi + 1)
        with m.If(ec_last_issue):
            with m.If(k4_heal):
                m.d.sync += hh_owned.bit_select(ec_row[0], 1).eq(0)
            m.d.sync += [
                ec_row.eq(ec_row + 1),
                ec_ii.eq(0),
                ec_rqi.eq(0),
                ec_xv.eq(0)
            ]
            with m.If(ec_head_risen):
                m.d.sync += ec_rfly.eq(1)
        with m.Elif(ec_elem_go & k4_heal):
            m.d.sync += ec_ii.eq(ec_ii + 1)
        with m.Elif(ec_issue_skid):
            m.d.sync += [ec_ii.eq(ec_ii + 1), ec_xv.eq(0)]
        with m.Elif(ec_issue_byp):
            m.d.sync += ec_ii.eq(ec_ii + 1)
        # The port's stolen word latches into the skid when its
        # element cannot issue this cycle (never conflicts with a
        # skid issue: a steal requires ~ec_xv, so at steal+1 the skid
        # is still empty unless the element bypasses to the issue).
        with m.If(ec_steal_d1 & ~ec_issue_byp):
            m.d.sync += [ec_x.eq(ec_port_x), ec_xv.eq(1)]

        # Dispatch FIFO: one entry per dispatched row (the walker's
        # k4_xh is the row's element halfword -- the walk presents the
        # row's word one dispatch ahead through the RAM's one-cycle
        # latency). A non-heal divert also allocates its chain
        # context here, latching the winner (the fold over the
        # extended prefix is exactly the new magnitude).
        ec_alloc_pick = Const(EC_CTXS, 4)
        for c in range(EC_CTXS):
            pick = Signal(4)
            m.d.comb += pick.eq(
                Mux((ec_alloc_pick == EC_CTXS)
                    & ~ec_alloc[c], c, ec_alloc_pick))
            ec_alloc_pick = pick
        ec_push = Signal()
        m.d.comb += ec_push.eq(k4_disp)
        ec_pop = ec_last_issue
        with m.If(ec_push):
            with m.If(k4_heal):
                m.d.sync += hh_ready.bit_select(row[0], 1).eq(0)
            with m.If(row == last_row):
                m.d.sync += ec_dispatch_done.eq(1)
            m.d.sync += [
                ec_f_xh[ec_ftail].eq(k4_xh), ec_f_risen[ec_ftail].eq(k4_rise),
                ec_f_ctx[ec_ftail].eq(Mux(k4_rise, ec_alloc_pick, 0)),
                ec_ftail.eq(ec_ftail + 1)
            ]
            with m.If(k4_rise):
                m.d.sync += [
                    ec_alloc[ec_alloc_pick].eq(1),
                    ec_done[ec_alloc_pick].eq(0), ec_st[ec_alloc_pick].eq(0),
                    ec_cnt[ec_alloc_pick].eq(0),
                    ec_xm16[ec_alloc_pick].eq(k4_mag)
                ]
        with m.If(ec_pop):
            m.d.sync += ec_fhead.eq(ec_fhead + 1)
        with m.If(ec_push & ~ec_pop):
            m.d.sync += ec_fcnt.eq(ec_fcnt + 1)
        with m.Elif(ec_pop & ~ec_push):
            m.d.sync += ec_fcnt.eq(ec_fcnt - 1)

        # The cursor's element issues replace the heal walk's
        # dispatch-time quick issues: the rpipe, the in-flight count
        # and the landing-tag pipe all follow ec_elem_go.
        ec_go_d1 = Signal()
        m.d.sync += ec_go_d1.eq(ec_elem_go)
        with m.If(ec_elem_go):
            m.d.sync += k4_rpipe0.eq(ec_r_sel)
        with m.If(ec_go_d1):
            m.d.sync += k4_rpipe1.eq(k4_rpipe0)
        with m.If(ec_elem_go):
            m.d.sync += [
                ec_tag_risen[0].eq(ec_head_risen),
                ec_tag_last[0].eq(ec_last_issue), ec_tag_ctx[0].eq(ec_head_ctx)
            ]
        for i in range(EC_TAGS - 1):
            m.d.sync += [
                ec_tag_risen[i + 1].eq(ec_tag_risen[i]),
                ec_tag_last[i + 1].eq(ec_tag_last[i]),
                ec_tag_ctx[i + 1].eq(ec_tag_ctx[i])
            ]

        # --------------------------------------------------------------
        # D3b chain engine: N contexts run today's exact scale op
        # sequence (widen -> classify -> seed -> 2xNewton -> sc ->
        # narrow) on the shared FP units under a fixed-priority grant
        # (at most one op per unit per cycle; the units are in-order
        # stall-free pipes, so each context captures at issue + L via
        # its countdown). The engine only runs in K4 runs,
        # where the legacy sequencer never issues.
        # --------------------------------------------------------------
        def ec_first(steps):
            """First allocated, not-done ctx sitting on one of `steps`
            with an idle countdown: (go, ctx)."""
            go = Const(0)
            sel = Const(EC_CTXS, 4)
            for c in range(EC_CTXS):
                step_match = ec_st[c] == steps[0]
                for s in steps[1:]:
                    step_match = step_match | (ec_st[c] == s)
                # Every candidate step needs the same eligibility gate.
                # Otherwise later Newton steps reissue while their result
                # is in flight, continually reloading the countdown.
                hit = ec_alloc[c] & ~ec_done[c] & (ec_cnt[c] == 0) \
                    & step_match
                next_go = Signal()
                next_sel = Signal(4)
                m.d.comb += [
                    next_go.eq(go | hit),
                    next_sel.eq(Mux(hit & (sel == EC_CTXS), c, sel))
                ]
                go, sel = next_go, next_sel
            return go, sel

        ec_widen_go, ec_widen_ctx = ec_first((EC_ST['widen'], ))
        ec_seed_go, ec_seed_ctx = ec_first((EC_ST['seed'], ))
        ec_mulm_go, ec_mulm_ctx = ec_first((EC_ST['it1m'], EC_ST['it2m']))
        ec_mulu_go, ec_mulu_ctx = ec_first((EC_ST['it1u'], EC_ST['it2u']))
        ec_mulsc_go, ec_mulsc_ctx = ec_first((EC_ST['sc'], ))
        ec_mul_go = Signal()
        m.d.comb += ec_mul_go.eq(ec_mulm_go | ec_mulu_go | ec_mulsc_go)
        ec_mul_ctx_value = Mux(ec_mulm_go, ec_mulm_ctx,
                               Mux(ec_mulu_go, ec_mulu_ctx, ec_mulsc_ctx))
        ec_mul_ctx = Signal.like(ec_mul_ctx_value)
        m.d.comb += ec_mul_ctx.eq(ec_mul_ctx_value)
        ec_mul_upd = Signal()
        m.d.comb += ec_mul_upd.eq(ec_mulu_go & ~ec_mulm_go)
        ec_mul_sc = Signal()
        m.d.comb += ec_mul_sc.eq(ec_mulsc_go & ~ec_mulm_go & ~ec_mulu_go)
        ec_mul_in1_value = Mux(ec_mul_upd, ec_y[ec_mul_ctx], ec_a[ec_mul_ctx])
        ec_mul_in1 = Signal.like(ec_mul_in1_value)
        m.d.comb += ec_mul_in1.eq(ec_mul_in1_value)
        ec_mul_in2_value = Mux(
            ec_mul_upd, ec_t[ec_mul_ctx],
            Mux(ec_mul_sc, Const(INV127_BITS, 32), ec_y[ec_mul_ctx]))
        ec_mul_in2 = Signal.like(ec_mul_in2_value)
        m.d.comb += ec_mul_in2.eq(ec_mul_in2_value)
        ec_add_go, ec_add_ctx = ec_first((EC_ST['it1s'], EC_ST['it2s']))
        ec_narrow_go, ec_narrow_ctx = ec_first((EC_ST['nrm'], ))
        m.d.comb += [
            f16ws.inp_valid.eq(ec_widen_go),
            f16ws.inp.eq(ec_xm16[ec_widen_ctx]),
        ]
        # Countdown and captures. Issue loads cnt <- L; the capture
        # at cnt == 1 (issue + L) advances the step. The classify
        # mirrors the legacy sequencer: heal may fold a zero amax,
        # which completes with r=d16=0; ordinary risens are nonzero.
        # An a >= 2^126 short-circuits r to zero and skips to the scale
        # product.
        ec_ws_e = f16ws.out[23:31]
        for c in range(EC_CTXS):
            grant_l = Mux(ec_st[c] == EC_ST['widen'], 2,
                          Mux(ec_st[c] == EC_ST['nrm'], 2, 3))
            granted = (ec_widen_go & (ec_widen_ctx == c)) \
                | (ec_seed_go & (ec_seed_ctx == c)) \
                | (ec_mul_go & (ec_mul_ctx == c)) \
                | (ec_add_go & (ec_add_ctx == c)) \
                | (ec_narrow_go & (ec_narrow_ctx == c))
            with m.If(ec_alloc[c] & ~ec_done[c]):
                with m.If(granted):
                    m.d.sync += ec_cnt[c].eq(grant_l)
                with m.Elif(ec_cnt[c] > 1):
                    m.d.sync += ec_cnt[c].eq(ec_cnt[c] - 1)
                with m.If(ec_cnt[c] == 1):
                    with m.If(ec_st[c] == EC_ST['widen']):
                        m.d.sync += ec_a[c].eq(f16ws.out)
                        with m.If(ec_ws_e == 0):
                            m.d.sync += [
                                ec_y[c].eq(0), ec_d16[c].eq(0), ec_a[c].eq(0),
                                ec_done[c].eq(1), ec_cnt[c].eq(0)
                            ]
                        with m.Elif(ec_ws_e >= 253):
                            m.d.sync += [
                                ec_y[c].eq(0), ec_st[c].eq(EC_ST['sc']),
                                ec_cnt[c].eq(0)
                            ]
                        with m.Else():
                            m.d.sync += [
                                ec_st[c].eq(EC_ST['seed']), ec_cnt[c].eq(0)
                            ]
                    with m.Elif(ec_st[c] == EC_ST['seed']):
                        m.d.sync += [
                            ec_y[c].eq(frec.out.bits.data),
                            ec_st[c].eq(EC_ST['it1m']), ec_cnt[c].eq(0)
                        ]
                    with m.Elif(ec_st[c] == EC_ST['it1m']):
                        m.d.sync += [
                            ec_t[c].eq(fmul_s.out.bits.data),
                            ec_st[c].eq(EC_ST['it1s']), ec_cnt[c].eq(0)
                        ]
                    with m.Elif(ec_st[c] == EC_ST['it1s']):
                        m.d.sync += [
                            ec_t[c].eq(fadd_s.out.bits.data),
                            ec_st[c].eq(EC_ST['it1u']), ec_cnt[c].eq(0)
                        ]
                    with m.Elif(ec_st[c] == EC_ST['it1u']):
                        m.d.sync += [
                            ec_y[c].eq(fmul_s.out.bits.data),
                            ec_st[c].eq(EC_ST['it2m']), ec_cnt[c].eq(0)
                        ]
                    with m.Elif(ec_st[c] == EC_ST['it2m']):
                        m.d.sync += [
                            ec_t[c].eq(fmul_s.out.bits.data),
                            ec_st[c].eq(EC_ST['it2s']), ec_cnt[c].eq(0)
                        ]
                    with m.Elif(ec_st[c] == EC_ST['it2s']):
                        m.d.sync += [
                            ec_t[c].eq(fadd_s.out.bits.data),
                            ec_st[c].eq(EC_ST['it2u']), ec_cnt[c].eq(0)
                        ]
                    with m.Elif(ec_st[c] == EC_ST['it2u']):
                        # The last Newton product is the quantizer's r.
                        m.d.sync += [
                            ec_y[c].eq(fmul_s.out.bits.data),
                            ec_st[c].eq(EC_ST['sc']), ec_cnt[c].eq(0)
                        ]
                    with m.Elif(ec_st[c] == EC_ST['sc']):
                        m.d.sync += [
                            ec_a[c].eq(fmul_s.out.bits.data),
                            ec_st[c].eq(EC_ST['nrm']), ec_cnt[c].eq(0)
                        ]
                    with m.Else():
                        m.d.sync += [
                            ec_d16[c].eq(f16n.out), ec_done[c].eq(1),
                            ec_cnt[c].eq(0)
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
                gather_lane.eq(buf_lane),
            ]
        with m.If(prefetch_go):
            m.d.sync += [
                pgi.eq(pgi + 1),
                pgather_index.eq(pgi[:5]),
                pgather_live.eq(
                    Mux(krow, next_row < live,
                        Cat(pgi[:5], next_blk) < live)),
                pgather_par.eq(pgi[0]),
                pgather_lane.eq(buf_lane),
            ]
        xh_now = Signal(16)
        mag_now = Signal(16)
        nan_now = Signal()
        m.d.comb += [
            xh_now.eq(
                Mux(gather_live, (gather32 >>
                                  (16 * Mux(krow, gather_par, row[0])))[:16],
                    0)),
            mag_now.eq(xh_now & 0x7fff),
            nan_now.eq(mag_now >= 0x7c01),
        ]
        with m.If(in_gather & gather_valid):
            for b in range(len(banks)):
                with m.If(gbank == b):
                    m.d.sync += banks[b][gather_index].eq(xh_now)
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
                    (pgather32 >>
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

        s_issue = Signal()
        m.d.comb += s_issue.eq(in_scale & ~s_wait & (s_step != 0)
                               & (s_step != 1))
        s_seed_go = Signal()
        m.d.comb += s_seed_go.eq(s_issue & (s_step == 2))
        s_it_go = Signal()
        m.d.comb += s_it_go.eq(s_issue & ((s_step == 4) | (s_step == 10)))
        s_sub_go = Signal()
        m.d.comb += s_sub_go.eq(s_issue & ((s_step == 6) | (s_step == 12)))
        s_upd_go = Signal()
        m.d.comb += s_upd_go.eq(s_issue & ((s_step == 8) | (s_step == 14)))
        s_sc_go = Signal()
        m.d.comb += s_sc_go.eq(s_issue & (s_step == 16))

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

        # The scale product's FP32->FP16 narrowing. The chain engine
        # shares the unit (never concurrently: the legacy sequencer
        # only issues inside SCALE, which decoupled runs never enter).
        m.d.comb += [
            f16n.inp_valid.eq((nrm_pend & ~nrm_iss) | ec_narrow_go),
            f16n.inp.eq(Mux(ec_narrow_go, ec_a[ec_narrow_ctx], sc)),
        ]
        with m.If(nrm_pend & ~nrm_iss):
            m.d.sync += nrm_iss.eq(1)
        with m.If(nrm_pend & nrm_iss & f16n.out_valid):
            m.d.sync += [
                d16.eq(f16n.out),
                nrm_pend.eq(0),
                scale_done.eq(1),
            ]

        # FP unit issue decode; the legacy contexts are mutually
        # exclusive, and the D3b chain engine's issues share the units
        # the same way (a chain issue only fires in a decoupled run,
        # where the legacy sequencer never issues).
        m.d.comb += [
            fmul_s.inp.valid.eq(s_it_go | s_upd_go | s_sc_go | ec_mul_go),
            fmul_s.inp.bits.fn.eq(FPUOperator.MUL),
            fmul_s.inp.bits.rm.eq(RoundingMode.RNE),
            fmul_s.inp.bits.in3.eq(0),
            fmul_s.inp.bits.in1.eq(
                Mux(ec_mul_go, ec_mul_in1,
                    Mux(s_sc_go, a_bits, Mux(s_upd_go, q_y, a_bits)))),
            fmul_s.inp.bits.in2.eq(
                Mux(
                    ec_mul_go, ec_mul_in2,
                    Mux(s_sc_go, Const(INV127_BITS, 32),
                        Mux(s_upd_go, q_t, q_y)))),
            fadd_s.inp.valid.eq(s_sub_go | ec_add_go),
            fadd_s.inp.bits.fn.eq(FPUOperator.ADD),
            fadd_s.inp.bits.fn_mod.eq(1),
            fadd_s.inp.bits.rm.eq(RoundingMode.RNE),
            fadd_s.inp.bits.in1.eq(Const(0x40000000, 32)),
            fadd_s.inp.bits.in2.eq(0),
            fadd_s.inp.bits.in3.eq(Mux(ec_add_go, ec_t[ec_add_ctx], q_t)),
            frec.inp.valid.eq(s_seed_go | ec_seed_go),
            frec.inp.bits.fn.eq(FPUOperator.REC),
            frec.inp.bits.fn_mod.eq(0),
            frec.inp.bits.rm.eq(RoundingMode.RNE),
            frec.inp.bits.in1.eq(Mux(ec_seed_go, ec_a[ec_seed_ctx], a_bits)),
            frec.inp.bits.in2.eq(0),
            frec.inp.bits.in3.eq(0),
        ]

        # ------------------------------------------------------------------
        # Quant stream: x -> widen -> R(x * r) -> R(t * 127) -> q8,
        # one element issued per cycle. The q8 byte writes the row in
        # issue order on the second multiplier's landing, so the row
        # byte cursor advances once per landed element.
        # ------------------------------------------------------------------
        wbyte = Signal(range(max_blocks * 34 +
                             1))  # next row byte to write, 0..67
        # The row assembly keeps 17 little-endian words; quant bytes
        # land strictly in order (word wbyte >> 2, byte wbyte & 3),
        # and each block's d16 pair is static: block 0 in the low
        # half of word 0, block 1 in the high half of word 8 (byte
        # 34). Dynamic whole-row bit selects synthesize as a full
        # crossbar, so the writes stay word-cased.
        rowwords = Array(Signal(32, name=f'rw{i}') for i in range(row_words))
        rowbuf_value = Cat(rowwords)
        rowbuf = Signal.like(rowbuf_value)
        m.d.comb += rowbuf.eq(rowbuf_value)

        # The amax widen and the element stream share Fp16Widen; the
        # scale phase completes before the quant phase starts. The
        # D3b cursor's element issues share it too (a cursor element
        # and a legacy issue never coincide: the cursor runs only in
        # K4 runs, which never enter the scale/quant states).
        amax_widen_go = Signal()
        m.d.comb += amax_widen_go.eq(in_scale & ~s_wait & (s_step == 0))
        quant_go = Signal()
        m.d.comb += quant_go.eq(in_quant & (q_iss != cur_live))
        quant_x = Signal(16)
        f16w_go = Signal()
        m.d.comb += f16w_go.eq(amax_widen_go | quant_go | ec_elem_go)
        with m.Switch(qbank):
            for b in range(len(banks)):
                with m.Case(b):
                    m.d.comb += quant_x.eq(banks[b][q_iss[:5]])
        m.d.comb += [
            f16w.inp_valid.eq(f16w_go),
            f16w.inp.eq(
                Mux(in_scale, xmax16, Mux(ec_elem_go, ec_elem_x, quant_x))),
        ]

        # The append's dedicated d16 widen: issued once at the scale
        # completion, latched two cycles later; the drain reads the
        # word long after (the whole quant walk runs between).
        if app_on:
            with m.If(f16wa.out_valid):
                m.d.sync += app_d32.eq(f16wa.out)

        # Bounded RNE int8 helper of the second product (the frozen
        # bit helper of troom.vector / q8_rne_bits in the model).
        # The result is registered once: the row-write switch below
        # references the register, so the helper's barrel shifters
        # are not replicated per case.
        p_bits = fmul_q2.out.bits.data
        qp_e = p_bits[23:31]
        qp_man_value = Cat(p_bits[:23], Const(1, 1))
        qp_man = Signal.like(qp_man_value)
        m.d.comb += qp_man.eq(qp_man_value)
        qp_sh = (Const(150, 9) - qp_e.as_signed())[:5].as_unsigned()
        qp_k = (qp_sh - Const(1, 5)).as_unsigned()
        qp_ip = (qp_man >> qp_sh)[:8]
        qp_guard = (qp_man >> qp_k)[0]
        qp_sticky = (qp_man << (Const(24, 6) - qp_k).as_unsigned())[:24].any()
        qp_rne_value = qp_ip[:8] + Mux(qp_guard & (qp_sticky | qp_ip[0]),
                                       Const(1, 8), Const(0, 8))
        qp_rne = Signal.like(qp_rne_value)
        m.d.comb += qp_rne.eq(qp_rne_value)
        qp_val_value = Mux(qp_e < 126, Const(0, 8),
                           Mux(qp_rne > 127, Const(127, 8), qp_rne[:8]))
        qp_val = Signal.like(qp_val_value)
        m.d.comb += qp_val.eq(qp_val_value)
        qp_sat = Signal()
        m.d.comb += qp_sat.eq(qp_e >= 134)
        q_byte_value = Mux(
            p_bits[31], Mux(qp_sat, Const(0x80, 8),
                            (-qp_val)[:8].as_unsigned()),
            Mux(qp_sat, Const(0x7f, 8), qp_val))
        q_byte = Signal.like(q_byte_value)
        m.d.comb += q_byte.eq(q_byte_value)
        q_byte_r = Signal(8)
        q_land_v = Signal()  # fmul_q2.out.valid delayed one cycle
        m.d.sync += [
            q_byte_r.eq(q_byte),
            q_land_v.eq(fmul_q2.out.valid),
        ]

        m.d.comb += [
            # The quick stream's elements flow while the walker state
            # holds them (the risen pass's elements ride in_quant).
            fmul_q1.inp.valid.eq(f16w.out_valid & (in_quant | k4q_stream)),
            fmul_q1.inp.bits.fn.eq(FPUOperator.MUL),
            fmul_q1.inp.bits.rm.eq(RoundingMode.RNE),
            fmul_q1.inp.bits.in3.eq(0),
            fmul_q1.inp.bits.in1.eq(f16w.out),
            # A K4 quick row multiplies by the state's stored r bits --
            # identical to the temporal rebuild's r because the row's
            # amax is unchanged. The r bits ride the widen pipeline
            # (the risen pass's stream reads the fresh scale's r_inv).
            fmul_q1.inp.bits.in2.eq(Mux(k4, k4_rpipe1, r_inv)),
            fmul_q2.inp.valid.eq(fmul_q1.out.valid
                                 & (in_quant | k4q_stream)),
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
        land_now = Signal()
        m.d.comb += land_now.eq(in_quant & q_land_v)
        with m.If(land_now):
            with m.Switch(wbyte[2:]):
                for k in range(row_words):
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
        # D3a pipelined K4 walk: the landing engine. It consumes the
        # quick stream's bytes in issue order ~11 cycles after
        # dispatch: even lanes park in the hold registers, odd lanes
        # compose the pair's payload beat and load the write latch --
        # strictly between the even lane's landing and the next pair's
        # even landing, so the held lane data can never be overwritten
        # first.
        # ------------------------------------------------------------------
        k4q_land = Signal()
        m.d.comb += k4q_land.eq(q_land_v & k4q_stream)

        # Both K4 prongs count every issued-but-not-landed element.
        with m.If(ec_elem_go & ~k4q_land):
            m.d.sync += k4q_pend.eq(k4q_pend + 1)
        with m.Elif(~ec_elem_go & k4q_land):
            m.d.sync += k4q_pend.eq(k4q_pend - 1)

        # ------------------------------------------------------------------
        # Tile drain: the padded row shifts out as whole beats. The
        # register ends with the row: above the first beat only the row's
        # own bytes are ever non-zero, because the padding loads zero and
        # every shift moves zero into the bits it vacates.
        # ------------------------------------------------------------------
        beats_per_row = self.row_pad // bb
        dbeat = Signal(range(beats_per_row + 1))
        drain_sr = Signal(max(self.data_width, 8 * VB_ROW_RAW))
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
        sg1_widx = Signal(range(max_blocks + 1))
        # Declared at function scope so the FSM's K4 states can reach
        # the accumulator under any beat width (K4 itself requires the
        # SG1 beat and completes BAD_SHAPE below it).
        meta_hw = Array(Signal(16, name=f'mhw{i}') for i in range(32))
        if self.beat_bytes == VB_SG1_BEAT:
            # Little-endian byte order: the row's LOW bytes are the
            # chunk's LOW bits, so the Cat runs from the chunk's byte
            # 0 (row byte 2) upward.
            chunks = [
                rowbuf[j * 34 * 8 + 16:j * 34 * 8 + 272]
                for j in range(max_blocks)
            ]
            holds = [Signal(256, name=f'hold_c{j}') for j in range(max_blocks)]
            # One halfword cell per (group, block, lane) of a 64-byte
            # meta beat (declared at function scope above); an Array
            # (not a dynamic bit-select on one wide signal, which
            # lowers to a latched comb read-modify-write) keeps the
            # writes latch-free.
            # Pre-read latch (partial runs): the loaded beat's 32
            # halfwords enter the accumulator here -- ABOVE the row
            # scatter below in elaboration order, so a same-cycle
            # scatter (a freshly quantized d16 of this very row) wins
            # the cell, exactly the compose a partial run needs.
            if self.n_outputs == 4:
                with m.If(pre_rd & self.tmem_rresp.fire):
                    m.d.sync += [
                        pre_rd.eq(0),
                        pre_cnt.eq(pre_cnt + 1),
                    ]
                    for i in range(32):
                        m.d.sync += meta_hw[i].eq(
                            self.tmem_rresp.bits.data[16 * i:16 * i + 16])
            else:
                # The generalized walks' response: a meta beat fills
                # its alternating accumulator wholesale; a boundary
                # beat contributes the retained half (the half the
                # partial's block does not rebuild -- block 0 owns the
                # beat's low chunk, block 1 the high one). The retained
                # halves live two banks deep by group parity: block 1's
                # compose for group g sits one row after group g-1's,
                # so a single register would be consumed between its
                # landing and its use.
                meta_hw2 = Array(
                    Signal(16, name=f'mhw2_{i}') for i in range(32))
                strret0 = Signal(256, name='strret0')
                strret1 = Signal(256, name='strret1')
                with m.If(pre_rd & self.tmem_rresp.fire):
                    m.d.sync += pre_rd.eq(0)
                    with m.If(pre_kind == 1):
                        with m.If(pre_bpar):
                            m.d.sync += [
                                strret1.eq(
                                    Mux(part_blk,
                                        self.tmem_rresp.bits.data[:256],
                                        self.tmem_rresp.bits.data[256:])),
                                strret_v[1].eq(1),
                            ]
                        with m.Else():
                            m.d.sync += [
                                strret0.eq(
                                    Mux(part_blk,
                                        self.tmem_rresp.bits.data[:256],
                                        self.tmem_rresp.bits.data[256:])),
                                strret_v[0].eq(1),
                            ]
                    # K window responses use the staging pipeline below.
                    with m.Elif(~pre_kind[1]):
                        with m.If(pre_acc):
                            for i in range(32):
                                m.d.sync += meta_hw2[i].eq(
                                    self.tmem_rresp.bits.data[16 * i:16 * i +
                                                              16])
                        with m.Else():
                            for i in range(32):
                                m.d.sync += meta_hw[i].eq(
                                    self.tmem_rresp.bits.data[16 * i:16 * i +
                                                              16])
                # ---- the K window store and its patches ----
                # A rebuilt row's cell for block j is staging cell
                # kbase + j*lanes, where kbase is the row's
                # (group, lane) position derived from the latched lane
                # offset: group (loff + row) // lanes, lane
                # (loff + row) % lanes. The payload cell is the store
                # half kcs (a whole 256-bit enable lane), the meta
                # cell the halfword kmhw + kcs of the window's banks.
                # The store's write port serves the pre-read response
                # (a whole beat) and the KPATCH state (one cell).
                kbase_n = Signal(range(_kwin_max_cells))
                kbase_w = Signal.like(kbase_n)
                with m.Switch(kloff):
                    for loff in range(_L):
                        with m.Case(loff):
                            with m.Switch(row[:4]):
                                for i in range(16):
                                    with m.Case(i):
                                        _g, _l = divmod(loff + i, _L)
                                        m.d.comb += [
                                            kbase_n.eq(_g * 2 * _L + _l),
                                            kbase_w.eq(_g * max_blocks * _L +
                                                       _l),
                                        ]
                                with m.Default():
                                    m.d.comb += [kbase_n.eq(0), kbase_w.eq(0)]
                    with m.Default():
                        m.d.comb += [kbase_n.eq(0), kbase_w.eq(0)]
                kcs_value = Mux(wide, kbase_w, kbase_n) + (kpj * _L)
                kcs = Signal.like(kcs_value)
                m.d.comb += kcs.eq(kcs_value)
                kp_chunk = Signal(8 * 32)
                kp_d16 = Signal(16)
                with m.Switch(kpj):
                    for j in range(max_blocks):
                        with m.Case(j):
                            _wi, _half = divmod(j * 34, 4)
                            m.d.comb += [
                                kp_chunk.eq(chunks[j]),
                                kp_d16.eq(rowwords[_wi][_half * 8:_half * 8 +
                                                        16]),
                            ]
                    with m.Default():
                        m.d.comb += [kp_chunk.eq(0), kp_d16.eq(0)]
                with m.If(pre_rd & self.tmem_rresp.fire & (pre_kind == 2)):
                    m.d.comb += [
                        kwin_w.addr.eq(kpre_idx),
                        kwin_w.data.eq(self.tmem_rresp.bits.data),
                        kwin_w.en.eq(3),
                    ]
                with m.Elif(in_kpatch):
                    m.d.comb += [
                        kwin_w.addr.eq(kcs[1:]),
                        kwin_w.data.eq(
                            Mux(kcs[0], kp_chunk << (self.data_width // 2),
                                kp_chunk)),
                        kwin_w.en.eq(1 << kcs[0]),
                    ]
                with m.Else():
                    m.d.comb += kwin_w.en.eq(0)
                kmi = Signal(7)
                m.d.comb += kmi.eq(kmhw + kcs)
                # Decode to per-bank write controls before the register
                # boundary. Full-beat preloads and halfword patches share
                # the registered data but use separate local strobes.
                meta_pre = pre_rd & self.tmem_rresp.fire & (pre_kind == 3)
                meta_beat_d = Signal(self.data_width)
                meta_patch_d = Signal(16)
                meta_word_d = Signal(5)
                with m.If(meta_pre):
                    m.d.sync += meta_beat_d.eq(self.tmem_rresp.bits.data)
                with m.If(in_kpatch):
                    m.d.sync += [
                        meta_patch_d.eq(kp_d16),
                        meta_word_d.eq(kmi[:5])
                    ]
                for b in range(_kwin_mb_max):
                    full_we = Signal(name=f'kwin_meta_full_we{b}',
                                     attrs={'keep': 'true'})
                    patch_we = Signal(name=f'kwin_meta_patch_we{b}',
                                      attrs={'keep': 'true'})
                    m.d.sync += [
                        full_we.eq(meta_pre & (kpre_idx == b)),
                        patch_we.eq(in_kpatch & (kmi[5:] == b)),
                    ]
                    with m.If(full_we):
                        for i in range(32):
                            m.d.sync += metab[b][i].eq(
                                meta_beat_d.word_select(i, 16))
                    with m.Elif(patch_we):
                        m.d.sync += metab[b][meta_word_d].eq(meta_patch_d)

            # Append-mode compose exists on a build that accepts appends
            # only (app_on).
            if app_on:
                # Append-mode compose (P2d-b): latch the pre-read beat the
                # same way, then scatter this run's two patch bytes (a
                # payload beat: the appended position of both lanes'
                # chunks) or the token d16 (the scale-row beat) into the
                # LATCHED response -- the compose must not race the
                # accumulator's old content, so the patched halfwords are
                # rebuilt from the response directly.
                if nonquad:
                    # The non-quad V append walk: the response cycle
                    # patches the beat's first row straight from the
                    # response; a second row of the same beat patches from
                    # the latched accumulator the cycle after (aph2), and
                    # only then does the beat's write arm (app_ph). The
                    # scale-row phase answers exactly like the quad.
                    _L = self.n_outputs
                    _agroups = max_dims // _L
                    _atail = max_dims - _agroups * _L

                    def _a_step():
                        """Advance the walk past one patched row: a cell
                        within the group, and across a group boundary
                        lanes + 1 cells -- less the tail group's missing
                        lanes when the walk is block 1's."""
                        m.d.sync += arow.eq(arow + 1)
                        with m.If(alane == _L - 1):
                            m.d.sync += alane.eq(0)
                            m.d.sync += acell.eq(
                                acell + _L + 1 -
                                Mux(arow + 1 == _agroups * _L, app_blk, 0) *
                                (_L - _atail))
                        with m.Else():
                            m.d.sync += [
                                alane.eq(alane + 1),
                                acell.eq(acell + 1)
                            ]

                    m.d.comb += asec.eq(~acell[0] & (alane + 1 < _L)
                                        & (arow + 1 < dims))
                    # The patch byte of tile row ``arow``: its dim's byte
                    # of the token row's chunk for that 32-dim block.
                    ap_ch = Signal(8 * 32)
                    with m.Switch(arow[5:7]):
                        for j in range(max_blocks):
                            with m.Case(j):
                                m.d.comb += ap_ch.eq(chunks[j])
                        with m.Default():
                            m.d.comb += ap_ch.eq(0)
                    ap_val = ap_ch.word_select(arow[:5], 8)
                    ap_hw = Cat(app_pos[1:5], acell[0])
                    ap_rh = self.tmem_rresp.bits.data.word_select(ap_hw, 16)
                    ap_lh = Signal(16)
                    m.d.comb += ap_lh.eq(meta_hw[ap_hw])
                    with m.If(app_rd & self.tmem_rresp.fire):
                        m.d.sync += [
                            app_rd.eq(0),
                            app_ph.eq(Mux(asrow, 1, ~asec)),
                            aph2.eq(~asrow & asec),
                            abeat.eq(acell >> 1),
                        ]
                        for i in range(32):
                            m.d.sync += meta_hw[i].eq(
                                self.tmem_rresp.bits.data[16 * i:16 * i + 16])
                        # The walk steps past the beat's first row.
                        with m.If(~asrow):
                            _a_step()
                    with m.If(app_rd & self.tmem_rresp.fire & ~asrow):
                        with m.If(app_pos[0]):
                            m.d.sync += meta_hw[ap_hw].eq(
                                Cat(ap_rh[:8], ap_val))
                        with m.Else():
                            m.d.sync += meta_hw[ap_hw].eq(
                                Cat(ap_val, ap_rh[8:]))
                    with m.If(aph2):
                        with m.If(app_pos[0]):
                            m.d.sync += meta_hw[ap_hw].eq(
                                Cat(ap_lh[:8], ap_val))
                        with m.Else():
                            m.d.sync += meta_hw[ap_hw].eq(
                                Cat(ap_val, ap_lh[8:]))
                        m.d.sync += [aph2.eq(0), app_ph.eq(1)]
                        _a_step()
                else:
                    with m.If(app_rd & self.tmem_rresp.fire):
                        m.d.sync += [app_rd.eq(0), app_ph.eq(1)]
                        for i in range(32):
                            m.d.sync += meta_hw[i].eq(
                                self.tmem_rresp.bits.data[16 * i:16 * i + 16])
            # K4 staging-read responses (single outstanding, routed by
            # the class register): a state beat latches into its
            # parity bank, a meta beat into meta_hw, a payload beat
            # into the pair-compose register.
            with m.If(k4_rd_busy & self.tmem_rresp.fire):
                m.d.sync += k4_rd_busy.eq(0)
                with m.If(k4_rd_cls == 0):
                    # Decode parity once and write the selected full beat.
                    # Constant halfword destinations avoid 32 indexed writes.
                    for parity in range(2):
                        with m.If(k4_rd_beat[0] == parity):
                            m.d.sync += Cat(
                                *k4st[32 * parity:32 * (parity + 1)]).eq(
                                    self.tmem_rresp.bits.data)
                    with m.If(k4_rd_beat[0]):
                        m.d.sync += k4st_v[1].eq(1)
                    with m.Else():
                        m.d.sync += k4st_v[0].eq(1)
                    # Beat 0's response arms beat 1's demand once
                    # (bank 1's first use has no predecessor write);
                    # every later beat arms at a group landing below.
                    with m.If(k4st_next == 0):
                        m.d.sync += k4st_rq.eq(1)
                    m.d.sync += k4st_next.eq(k4st_next + 1)
                with m.Elif(k4_rd_cls == 1):
                    for i in range(32):
                        m.d.sync += meta_hw[i].eq(
                            self.tmem_rresp.bits.data[16 * i:16 * i + 16])
                    m.d.sync += [k4met_v.eq(1), k4met_next.eq(k4met_next + 1)]
                with m.Else():
                    m.d.comb += [
                        k4pay_write.addr.eq(k4_rd_beat[:2]),
                        k4pay_write.data.eq(self.tmem_rresp.bits.data),
                        k4pay_write.en.eq(1),
                    ]
                    m.d.sync += [
                        k4pay_v.bit_select(k4_rd_beat[:2], 1).eq(1),
                        k4pay_next.eq(k4pay_next + 1)
                    ]
            if app_on:
                app_hw0 = app_pos[1:5]
                app_hw1 = Cat(app_pos[1:5], 1)  # (p >> 1) + 16
                app_rh0 = self.tmem_rresp.bits.data.word_select(app_hw0, 16)
                app_rh1 = self.tmem_rresp.bits.data.word_select(app_hw1, 16)
                # Beat w's lanes are dims 2w and 2w+1: chunk (dim >> 5),
                # byte 2*(w mod 16) (+1 for the odd lane). A K append's
                # beat w is block w's chunk outright.
                app_we = Cat(0, app_w[0:4])
                app_wo = Cat(1, app_w[0:4])
                app_ci = Mux(kapp, app_w[:2], app_w[4:6])
                app_ch = Signal(8 * 32)
                with m.Switch(app_ci):
                    for j in range(max_blocks):
                        with m.Case(j):
                            m.d.comb += app_ch.eq(chunks[j])
                app_be = app_ch.word_select(app_we, 8)
                app_bo = app_ch.word_select(app_wo, 8)
                if not nonquad:
                    with m.If(app_rd & self.tmem_rresp.fire & ~kapp
                              & ~app_srow):
                        # Byte p of the beat is the even lane's chunk, 32+p the
                        # odd lane's; the lane within the halfword is p & 1.
                        with m.If(app_pos[0]):
                            m.d.sync += meta_hw[app_hw0].eq(
                                Cat(app_rh0[:8], app_be))
                            m.d.sync += meta_hw[app_hw1].eq(
                                Cat(app_rh1[:8], app_bo))
                        with m.Else():
                            m.d.sync += meta_hw[app_hw0].eq(
                                Cat(app_be, app_rh0[8:]))
                            m.d.sync += meta_hw[app_hw1].eq(
                                Cat(app_bo, app_rh1[8:]))
                # Scale-row beats: patch WORD k-1 of the 256-B F32 row with
                # the widened token d16. The word spans halfword cells
                # 2*((k-1)&15) and +1 of beat pay_beats + ((k-1) >> 4);
                # every other cell keeps the latched response (the 1.0s the
                # host seeded and the previous appends' words). Cat is
                # LSB-first in this tree: the beat index is (blk << 1) |
                # p[4] and the cell base is (p & 15) << 1. A non-quad walk
                # numbers the scale-row beats 0..3 in their own phase.
                app_pay_beats = Mux(wide, Const(VB_APP_WIDE_PAY_BEATS, 7),
                                    Const(32, 7))
                app_sw0 = Cat(0, app_pos[0:4])  # 2 * ((k-1) & 15)
                app_sw_beat = Mux(Const(nonquad), Cat(app_pos[4], app_blk),
                                  app_pay_beats + Cat(app_pos[4], app_blk))
                with m.If(app_rd & self.tmem_rresp.fire & ~kapp & app_srow
                          & (app_w == app_sw_beat)):
                    m.d.sync += meta_hw[app_sw0].eq(app_d32[:16])
                    m.d.sync += meta_hw[app_sw0 + 1].eq(app_d32[16:])
                # K append payload beats: the token's whole 32-B chunk of
                # block w lands in its lane's sixteen halfword cells (a
                # chunk is half a beat); the neighbor lane's cells keep
                # the latched response.
                app_kc = Cat(Const(0, 4), kr[0])  # 16 * (token lane)
                with m.If(app_rd & self.tmem_rresp.fire & kapp & ~app_kmeta):
                    for j in range(16):
                        m.d.sync += meta_hw[app_kc + j].eq(
                            app_ch.word_select(j, 16))
                # K append meta beat: the four per-block d16 cells at
                # (r>>2)*8*blocks + 4*b + (r&3) within the group image's
                # meta halfwords (narrow: one beat; wide: beat r>>3).
                app_kcell = Mux(wide, kr[2] << 4, kr[2:4] << 3)
                with m.If(app_rd & self.tmem_rresp.fire & kapp & app_kmeta):
                    for j in range(max_blocks):
                        wi, half = divmod(j * 34, 4)
                        m.d.sync += meta_hw[app_kcell + 4 * j + kr[:2]].eq(
                            rowwords[wi][half * 8:half * 8 + 16])
            # This row's two halfword cells inside its 64-byte meta
            # beat: group g' = (row >> 2) & 3 contributes j0 at
            # 8*g' + lane and j1 at 8*g' + 4 + lane.
            if self.n_outputs == 4:
                meta_row = Signal(4)
                m.d.comb += meta_row.eq((row + Mux(part_k, pair_base, 0))
                                        & Mux(wide_k, 7, 15))
                meta_indices = [
                    (meta_row >> 2) * Mux(wide_k, 16, 8) + j * 4 + meta_row[:2]
                    for j in range(max_blocks)
                ]
                sg_blocks = Mux(wide_k, 4, 2)
                sg1_meta_now = Mux(part_k, row[:4].all(),
                                   Mux(wide_k, row[:3].all(), row[:4].all()))
                pay_ok = Mux(part_vb, sg1_widx == part_blk,
                             Mux(part_k, row <= live, 1))
                sg1_wr = in_sg1w & (((sg1_widx != sg_blocks) & pay_ok)
                                    | ((sg1_widx == sg_blocks) & sg1_meta_now))
                meta_out_index = Mux(part_k & wide_k, pair_base >> 3,
                                     meta_group)
                sg1_waddr = Mux(
                    sg1_widx == sg_blocks, m_base + (meta_out_index << 6),
                    r_base + (row >> 2) * Mux(wide_k, 512, 256) +
                    (row[1] << 6) + (sg1_widx << 7))
                # Keep ArrayProxy lowering outside the metadata selector: its
                # out-of-range default must not substitute index 3 for beat 4.
                sg1_payload = Signal(self.data_width)
                m.d.comb += sg1_payload.eq(
                    Cat(Array(holds)[sg1_widx],
                        Array(chunks)[sg1_widx]))
                sg1_wdata = Mux(sg1_widx == sg_blocks, Cat(*meta_hw),
                                sg1_payload)
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
                sg1_last = in_sg1w & ((
                    (sg1_widx == sg_blocks - 1) & ~sg1_meta_now
                    | (sg1_widx == sg_blocks)) & sg1_adv)
            else:
                # ---- the generalized interleave drain (V modes) ----
                # A row's staging roles follow from the parity of its
                # chunk indices c(j) = g*2*lanes + j*gl + l: an even c
                # is a beat's low half (held unless it is the group's
                # straddle tail), an odd c its high half (composed with
                # the held partner, or -- at lane 0 of block 1 -- held
                # across the whole group as that straddle's high half).
                # Every term below is an explicit combinational signal
                # (the file's rule for reused expressions): the role
                # tests feed the FSM's DRAINLOAD state, and a Python
                # Value alias would be re-expanded into each of the
                # ~1,500 per-signal processes that carry the state
                # switch -- the first five-lane elaboration measured
                # 68 MB of RTLIL against 6.8 MB for the quad unit.
                groups_rt = Signal(range(max_dims // lanes + 1))
                m.d.comb += groups_rt.eq(Mux(wide, _groups_w, _groups_n))
                in_tail = Signal()
                m.d.comb += in_tail.eq(sgrp == groups_rt - 1)
                gl = Signal(range(lanes + 1))
                m.d.comb += gl.eq(
                    Mux(in_tail, Mux(wide, _tail_w, _tail_n), lanes))
                hold0 = Signal()
                m.d.comb += hold0.eq(~slane[0] & (slane != gl - 1))
                hold1 = Signal()
                m.d.comb += hold1.eq(~(gl[0] ^ slane[0]) & (slane != gl - 1))
                strh1 = Signal()
                m.d.comb += strh1.eq((slane == 0) & gl[0])
                comp0 = Signal()
                m.d.comb += comp0.eq(slane[0])
                strc0 = Signal()
                m.d.comb += strc0.eq((slane == gl - 1) & gl[0])
                comp1 = Signal()
                m.d.comb += comp1.eq((gl[0] ^ slane[0]) & (slane != 0))
                cbase = Signal(range(max_dims // lanes * 2 * lanes + 1))
                m.d.comb += cbase.eq(sgrp * (2 * lanes))
                cell0 = Signal.like(cbase)
                m.d.comb += cell0.eq(cbase + slane)
                cell1 = Signal.like(cbase)
                m.d.comb += cell1.eq(cbase + gl + slane)
                strhead_c1 = Signal(256)
                mwr_sel = Signal()

                def _lf(m, mode):
                    """The last row scattering into meta beat ``m``;
                    ``mode`` None is either block (a full run), 0/1 the
                    partial's rebuilt block."""
                    if (64, m, mode) in _lastrow:
                        if max_dims != 64:
                            return Mux(wide, _lastrow[(max_dims, m, mode)],
                                       _lastrow[(64, m, mode)])
                        return _lastrow[(64, m, mode)]
                    return _lastrow[(max_dims, m, mode)]

                _lr = [
                    Mux(part_vb, Mux(part_blk, _lf(m, 1), _lf(m, 0)),
                        _lf(m, None)) for m in range(_maxmb)
                ]
                # The arm rows of the two beat walks, selected once.
                lrow0 = Signal(range(max_dims))
                lrow1 = Signal.like(lrow0)
                with m.Switch(mbeat0):
                    for _mbi in range(_maxmb):
                        with m.Case(_mbi):
                            m.d.comb += lrow0.eq(_lr[_mbi])
                    with m.Default():
                        m.d.comb += lrow0.eq(0)
                with m.Switch(mbeat1):
                    for _mbi in range(_maxmb):
                        with m.Case(_mbi):
                            m.d.comb += lrow1.eq(_lr[_mbi])
                    with m.Default():
                        m.d.comb += lrow1.eq(0)
                arm0 = Signal()
                m.d.comb += arm0.eq((mbeat0 < mbeats_cur) & (row == lrow0))
                arm1 = Signal()
                m.d.comb += arm1.eq((mbeat1 < mbeats_cur) & (row == lrow1))
                # The slot decode: slot 0 is block 0's entry (a pair
                # compose or the group's straddle beat), slot 1 block
                # 1's pair, and the first empty slot carries the armed
                # meta write.
                slot = sg1_widx
                entry_v = Mux(slot == 0, comp0 | strc0,
                              Mux(slot == 1, comp1, 0))
                entry_str = Mux(slot == 0, strc0, 0)
                gate_ok = Mux(
                    entry_str, 1,
                    Mux(slot == 0, ~part_vb | ~part_blk, ~part_vb | part_blk))
                # A partial's straddle compose waits for its retained
                # half's pre-read (fail-loud rather than corrupt).
                entry_stall = part_vb & entry_str \
                    & ~strret_v.bit_select(sgrp[0], 1)
                meta_slot = ~entry_v & mwr_v
                sg1_wr = in_sg1w & ((entry_v & gate_ok) | meta_slot) \
                    & ~entry_stall
                beat_idx = Mux(
                    entry_str, (cbase + gl - 1) >> 1,
                    Mux(slot == 0, (cbase + slane) >> 1,
                        (cbase + gl + slane) >> 1))
                sg1_waddr = Mux(meta_slot, m_base + (mwr_beat << 6),
                                r_base + (beat_idx << 6))
                pairdata = Mux(slot == 0, Cat(holds[0], chunks[0]),
                               Cat(holds[1], chunks[1]))
                strsel = Array([strret0, strret1])[sgrp[0]]
                strdata = Mux(
                    part_vb,
                    Mux(part_blk, Cat(strsel, strhead_c1),
                        Cat(chunks[0], strsel)), Cat(chunks[0], strhead_c1))
                sg1_wdata = Mux(meta_slot,
                                Mux(mwr_sel, Cat(*meta_hw2), Cat(*meta_hw)),
                                Mux(entry_str, strdata, pairdata))
                # The walk visits all three slots (block 0's entry at
                # 0, block 1's at 1, the armed meta at the first empty
                # slot): an entry slot steps on its write or its block
                # gate (a partial's other block skips), an empty slot
                # steps immediately, and the walk ends after slot 2.
                step = in_sg1w & ~entry_stall \
                    & (~sg1_wr | self.tmem_w.fire)
                with m.If(step):
                    m.d.sync += sg1_widx.eq(sg1_widx + 1)
                with m.If(sg1_wr & self.tmem_w.fire):
                    m.d.sync += self.beats.eq(self.beats + 1)
                    with m.If(meta_slot):
                        m.d.sync += mwr_v.eq(0)
                    with m.Elif(entry_str):
                        # The straddle compose consumed the retained
                        # half; the next boundary pre-read re-arms it.
                        m.d.sync += strret_v.bit_select(sgrp[0], 1).eq(0)
                # The last slot carries no entry (entry_v is 0 there),
                # so the general step rule evaluated there reduces to
                # this: the row ends once the armed meta write, if any,
                # fires. It is written out because it is the row pass's
                # advance condition -- it fans out to every row register
                # -- and must not carry the staging geometry that entry_v
                # and entry_stall derive from the lane position.
                sg1_last = in_sg1w & (sg1_widx == 2) \
                    & (~mwr_v | self.tmem_w.fire)
            if app_on:
                app_waddr = Mux(
                    kapp,
                    Mux(app_kmeta, m_base + (app_kmetabeat << 6), app_kpay),
                    Mux(
                        app_srow,
                        m_base + (Mux(wide, app_w[:6], app_w[:5]) << 6),
                        r_base + (app_w[1:7] << 8) + (app_w[0] << 6) +
                        (app_blk << 7)))
                if nonquad:
                    # The V append walk writes the composed payload beat
                    # (``abeat``, latched at its response) or a scale-row
                    # beat; the K append never reaches APPW here.
                    app_waddr = Mux(asrow, m_base + (app_w[:2] << 6),
                                    r_base + (abeat << 6))
                app_wdata = Cat(*meta_hw)[:self.data_width]
            else:
                app_waddr = Const(0, aw)
                app_wdata = Const(0, self.data_width)

            # ----------------------------------------------------------
            # K4 beat compose (rows 3): the pair beat's two 32-B chunk
            # lanes. A risen lane carries its fresh quant chunk (the
            # even lane's held at its row completion, the odd lane's
            # straight from the row assembly); a quick lane patches
            # byte app_pos of the pre-read chunk with its column byte.
            # The word-cased patch mirrors the row-assembly write style
            # (dynamic whole-row bit selects would lower to a full
            # crossbar per lane).
            def k4_patch32(src, pos, byte):
                out = Signal.like(src)
                with m.Switch(pos[2:5]):
                    for w in range(8):
                        with m.Case(w):
                            with m.Switch(pos[:2]):
                                for ln in range(4):
                                    with m.Case(ln):
                                        m.d.comb += out.eq(
                                            Cat(src[:32 * w + 8 * ln], byte,
                                                src[32 * w + 8 * ln + 8:]))
                return out

            # D3b: the cursor's landing tags (risen/last/ctx) rode the
            # 9-cycle element pipeline with their bytes. A risen row's
            # mid-row bytes assemble into one 32-B chunk register
            # (landings are in order, one row at a time), the last
            # byte patches in combinationally at the completing
            # landing, and a completion whose meta beat is still in
            # flight PARKS (ec_pend) -- the cursor stopped issuing
            # after the row's last element (ec_rfly), so the pipeline
            # is drained and no further landing can arrive behind the
            # parked row; the completion fires when the beat lands
            # (ec_exit).
            ec_land_risen = ec_tag_risen[EC_TAGS - 1]
            ec_land_last = ec_tag_last[EC_TAGS - 1]
            ec_land_ctx = ec_tag_ctx[EC_TAGS - 1]
            ec_rlast_land = k4q_land & ec_land_risen & ec_land_last
            ec_pay_ok_value = ~k4_land_row[0] \
                | (~k4w_pay_v & k4pay_v.bit_select(k4_land_row[1:3], 1))
            if nonquad:
                # The landing row composes on its role, not its parity,
                # and its beat's bank is the rank's low bits.
                ec_pay_ok_value = ~k4pl['comp'] \
                    | (~k4w_pay_v & k4pay_v.bit_select(k4pl['rank'][:2], 1))
                ec_pay_ok = Signal()
                m.d.comb += ec_pay_ok.eq(ec_pay_ok_value)
            else:
                ec_pay_ok = ec_pay_ok_value
            ec_park = ec_rlast_land & (~k4met_v | ~ec_pay_ok)
            ec_exit = ec_pend & k4met_v & ec_pay_ok
            ec_rcomp = (ec_rlast_land & ~ec_park) | ec_exit
            ec_quick_land = k4q_land & ~ec_land_risen
            ec_quick_byte = Signal(8)
            m.d.comb += ec_quick_byte.eq(
                Mux(ec_qcnt != 0, ec_qdata[ec_qhead], q_byte_r))
            ec_quick_go = ec_active & ((ec_qcnt != 0) | ec_quick_land) \
                & ec_pay_ok
            ec_qpush = ec_quick_land & ~((ec_qcnt == 0) & ec_quick_go)
            ec_qpop = (ec_qcnt != 0) & ec_quick_go
            with m.If(ec_qpush):
                m.d.sync += [
                    ec_qdata[ec_qtail].eq(q_byte_r),
                    ec_qtail.eq(ec_qtail + 1)
                ]
            with m.If(ec_qpop):
                m.d.sync += ec_qhead.eq(ec_qhead + 1)
            with m.If(ec_qpush & ~ec_qpop):
                m.d.sync += ec_qcnt.eq(ec_qcnt + 1)
            with m.Elif(ec_qpop & ~ec_qpush):
                m.d.sync += ec_qcnt.eq(ec_qcnt - 1)
            ec_complete = ec_quick_go | ec_rcomp
            ec_chunk_now = Mux(ec_exit, ec_chunk,
                               k4_patch32(chunk_assm, ec_land_cnt, q_byte_r))

            # D3a: the pair's pre-read beat lives in one of four banks
            # cycling by pair index, selected by the landing cursor.
            k4_comp_bank = Signal(2)
            k4_paysel = Signal(self.data_width)
            k4_comp_bank_value = k4_land_row[1:3]
            if nonquad:
                k4_comp_bank_value = k4pl['rank'][:2]
            m.d.comb += [
                k4_comp_bank.eq(k4_comp_bank_value),
                k4pay_read.addr.eq(k4_comp_bank),
                k4_paysel.eq(k4pay_read.data)
            ]
            if nonquad:
                # The composing row's beat: its own cell goes to the
                # half its cell index names (fresh chunk when risen,
                # else the pre-read half with its column byte patched);
                # the other half is the previous row's held cell (a
                # high cell with a run partner below) or the pre-read
                # neighbour's retained half (a straddle beat).
                k4_held = Signal(256)
                k4_own_src = Signal(256)
                k4_own = Signal(256)
                m.d.comb += [
                    k4_held.eq(
                        Mux(
                            k4_hold_risen, k4_hold_chunk,
                            k4_patch32(k4_paysel[:8 * 32], app_pos,
                                       k4_hold_byte))),
                    k4_own_src.eq(
                        Mux(k4pl['odd'], k4_paysel[8 * 32:],
                            k4_paysel[:8 * 32])),
                    k4_own.eq(
                        Mux(ec_rcomp, ec_chunk_now,
                            k4_patch32(k4_own_src, app_pos, ec_quick_byte))),
                    k4_even_res.eq(
                        Mux(k4pl['odd'],
                            Mux(k4pl['l'] != 0, k4_held, k4_paysel[:8 * 32]),
                            k4_own)),
                    k4_odd_res.eq(Mux(k4pl['odd'], k4_own,
                                      k4_paysel[8 * 32:])),
                ]
            else:
                m.d.comb += k4_even_res.eq(
                    Mux(k4_hold_risen, k4_hold_chunk,
                        k4_patch32(k4_paysel[:8 * 32], app_pos, k4_hold_byte)))
                m.d.comb += k4_odd_res.eq(
                    Mux(ec_rcomp & k4_land_row[0], ec_chunk_now,
                        k4_patch32(k4_paysel[8 * 32:], app_pos,
                                   ec_quick_byte)))
            # The K4 beat write arbiter: one write per cycle, payload
            # over state over meta, one-deep latches.
            k4w_any = k4w_pay_v | k4w_st_v | k4w_met_v
            k4w_sel_pay = (k4w_lock == 1) | ((k4w_lock == 0) & k4w_pay_v)
            k4w_sel_st = (k4w_lock == 2) \
                | ((k4w_lock == 0) & ~k4w_pay_v & k4w_st_v)
            k4w_go = in_k4w & k4w_any
            k4w_addr = Mux(k4w_sel_pay, k4w_pay_addr,
                           Mux(k4w_sel_st, k4w_st_addr, k4w_met_addr))
            k4w_data = Mux(k4w_sel_pay, k4w_pay_data,
                           Mux(k4w_sel_st, k4w_st_data, k4w_met_data))
            # A newly completed higher-priority class cannot replace a
            # beat already offered while ready is low.
            with m.If(k4w_go & ~self.tmem_w.ready & (k4w_lock == 0)):
                m.d.sync += k4w_lock.eq(
                    Mux(k4w_sel_pay, 1, Mux(k4w_sel_st, 2, 3)))
            with m.If(k4w_go & self.tmem_w.fire):
                m.d.sync += k4w_lock.eq(0)
                m.d.sync += self.beats.eq(self.beats + 1)
                with m.If(k4w_sel_pay):
                    m.d.sync += k4w_pay_v.eq(0)
                with m.Elif(k4w_sel_st):
                    # The bank's beat retired: its next pre-read may
                    # issue (the request gate watches the pend flag).
                    m.d.sync += [
                        k4w_st_v.eq(0),
                        k4w_st_pend.bit_select(k4w_st_addr[6], 1).eq(0)
                    ]
                with m.Else():
                    m.d.sync += k4w_met_v.eq(0)
                    # The accumulator is free: request the next meta
                    # beat (16 rows of lead).
                    with m.If(~k4met_next[3]):
                        m.d.sync += k4met_rq.eq(1)

            # ----------------------------------------------------------
            # D3a completion engine: quick landings and risen
            # completions advance the
            # landing cursor in issue order; even lanes park in the
            # hold registers, odd lanes compose the pair beat from its
            # bank and load the payload latch (the in-order landing
            # stream puts the compose strictly between the two
            # even-lane lifetimes), and 8/16-row boundaries arm the
            # deferred state/meta beat writes. k4_land_row holds the
            # COMPLETING row during the k4_comp cycle (it starts at 0
            # and advances once per completion), so every index below
            # uses it directly.
            # ----------------------------------------------------------
            k4_comp = ec_complete
            with m.If(k4q_land & ec_land_risen & ~ec_land_last):
                m.d.sync += [
                    chunk_assm.eq(k4_patch32(chunk_assm, ec_land_cnt,
                                             q_byte_r)),
                    ec_land_cnt.eq(ec_land_cnt + 1)
                ]
            # A non-composing row parks its data in the hold registers
            # (quad: the even row of a pair); a composing one loads the
            # payload write latch from its beat.
            _k4_hold_cond = ~k4_land_row[0]
            if nonquad:
                _k4_hold_cond = ~k4pl['comp']
            with m.If(ec_complete):
                with m.If(_k4_hold_cond):
                    with m.If(ec_rcomp):
                        m.d.sync += [
                            k4_hold_risen.eq(1),
                            k4_hold_chunk.eq(ec_chunk_now)
                        ]
                    with m.Else():
                        m.d.sync += [
                            k4_hold_risen.eq(0),
                            k4_hold_byte.eq(ec_quick_byte)
                        ]
                with m.Else():
                    _k4w_pay_addr_value = (r_base + ((k4_land_row >> 2) << 8) +
                                           (k4_land_row[1] << 6) +
                                           (app_blk << 7))
                    _k4w_bank = k4_land_row[1:3]
                    if nonquad:
                        _k4w_pay_addr_value = r_base + (k4pl['rank'] << 6)
                        _k4w_bank = k4pl['rank'][:2]
                    m.d.sync += [
                        k4w_pay_v.eq(1),
                        k4w_pay_addr.eq(_k4w_pay_addr_value),
                        k4w_pay_data.eq(Cat(k4_even_res, k4_odd_res)),
                        k4pay_v.bit_select(_k4w_bank, 1).eq(0),
                    ]
                    if nonquad:
                        m.d.sync += k4_ncomp.eq(k4_ncomp + 1)
            # The risen completion scatters using the landing cursor's
            # indices and the completing context's values, then frees
            # the context and re-opens the cursor.
            ec_cctx = Signal(4)
            ec_cmag = Signal(16)
            ec_cr = Signal(32)
            ec_cd16 = Signal(16)
            m.d.comb += [
                ec_cctx.eq(Mux(ec_exit, ec_pend_ctx, ec_land_ctx)),
                ec_cmag.eq(ec_xm16[ec_cctx]),
                ec_cr.eq(ec_y[ec_cctx]),
                ec_cd16.eq(ec_d16[ec_cctx])
            ]
            with m.If(ec_rcomp):
                # One selector, constant array indices. Independent
                # dynamic sums here multiply into nested legalization
                # switches (64 x 64 x 64 x 32) in the RTLIL backend.
                with m.Switch(k4_land_row[:4]):
                    for i in range(16):
                        base = 32 * (i >> 3) + 4 * (i & 7)
                        with m.Case(i):
                            m.d.sync += [
                                k4st[base].eq(ec_cmag),
                                k4st[base + 2].eq(ec_cr[:16]),
                                k4st[base + 3].eq(ec_cr[16:])
                            ]
                # The d16 lands in the meta beat accumulator at the
                # row's staging cell: (row>>2)*8 + blk*4 + (row&3) on
                # the quad, the five-lane cell index's low bits on a
                # non-quad build.
                if nonquad:
                    m.d.sync += meta_hw[k4pl['cell'][:5]].eq(ec_cd16)
                else:
                    with m.Switch(k4_land_row[:4]):
                        for i in range(16):
                            meta = ((i >> 2) << 3) | (i & 3)
                            with m.Case(i):
                                with m.If(app_blk):
                                    m.d.sync += meta_hw[meta + 4].eq(ec_cd16)
                                with m.Else():
                                    m.d.sync += meta_hw[meta].eq(ec_cd16)
                for c in range(EC_CTXS):
                    with m.If(ec_cctx == c):
                        m.d.sync += ec_alloc[c].eq(0)
                m.d.sync += [
                    ec_rfly.eq(0),
                    chunk_assm.eq(0),
                    ec_land_cnt.eq(0),
                ]
            with m.If(ec_park):
                m.d.sync += [
                    ec_pend.eq(1),
                    ec_pend_ctx.eq(ec_land_ctx),
                    ec_chunk.eq(ec_chunk_now)
                ]
            with m.If(ec_exit):
                m.d.sync += ec_pend.eq(0)
            with m.If(k4_comp):
                m.d.sync += [
                    self.rows.eq(self.rows + 1),
                    k4_land_row.eq(k4_land_row + 1)
                ]
                with m.If(k4_land_row[:3].all()):
                    m.d.sync += [
                        k4w_st_due.eq(1),
                        k4w_st_idx.eq(k4_land_row[3:7])
                    ]
                    # The boundary also arms the next-next state beat
                    # (same bank, two groups on): its pre-read now
                    # waits behind this due's latch and write retire,
                    # exactly the serialized order the bank needs.
                    with m.If(k4st_next != 16):
                        m.d.sync += k4st_rq.eq(1)
                _k4met_arm = k4_land_row[:4].all()
                _k4met_idx = k4_land_row[4:7]
                if nonquad:
                    # The beat's last row by table (the landing row's
                    # own meta beat names the entry).
                    k4met_last = Signal(range(max_dims + 1))
                    with m.Switch(Cat(k4pl['mbeat'], app_blk)):
                        for _blk in (0, 1):
                            for _m in range(max_dims * 2 // 32):
                                with m.Case(_m | (_blk << 3)):
                                    m.d.comb += k4met_last.eq(
                                        _k4met_last[(_blk, _m)])
                        with m.Default():
                            m.d.comb += k4met_last.eq(max_dims)
                    _k4met_arm = k4_land_row == k4met_last
                    _k4met_idx = k4pl['mbeat']
                with m.If(_k4met_arm):
                    m.d.sync += [k4w_met_due.eq(1), k4w_met_idx.eq(_k4met_idx)]
                with m.If(k4_land_row == last_row):
                    m.d.sync += k4_done_wait.eq(1)
            # Deferred state/meta beat writes: each latch loads as
            # soon as it drains; the beat data is stable until the
            # next pre-read, which is gated on the write retiring.
            with m.If(k4w_st_due & ~k4w_st_v):
                m.d.sync += [
                    k4w_st_v.eq(1),
                    k4w_st_due.eq(0),
                    k4w_st_addr.eq(m_base + (dims << 2) + (app_blk << 10) +
                                   (k4w_st_idx << 6)),
                    k4w_st_data.eq(
                        Mux(k4w_st_idx[0], Cat(*k4st[32:]), Cat(*k4st[:32]))),
                    k4w_st_pend.bit_select(k4w_st_idx[0], 1).eq(1),
                ]
            with m.If(k4w_met_due & ~k4w_met_v):
                m.d.sync += [
                    k4w_met_v.eq(1),
                    k4w_met_due.eq(0),
                    k4w_met_addr.eq(m_base + (k4w_met_idx << 6)),
                    k4w_met_data.eq(Cat(*meta_hw)),
                    k4met_v.eq(0),
                ]
        else:
            # Unreachable: an SG1 descriptor completes BAD_SHAPE at
            # accept below the deployed beat, so in_sg1w never asserts.
            sg1_wr = Const(0)
            sg1_waddr = Const(0, aw)
            sg1_wdata = Const(0, 8 * VB_SG1_BEAT)
            sg1_last = Const(0)
            app_waddr = Const(0, aw)
            app_wdata = Const(0, self.data_width)
            k4w_go = Const(0)
            k4w_addr = Const(0, aw)
            k4w_data = Const(0, self.data_width)
            k4_comp = Const(0)

        tw_valid = in_drain | (in_sg1w & sg1_wr) | (in_appw & app_ph) | k4w_go
        tw_addr = Mux(
            k4w_go, k4w_addr,
            Mux(
                in_appw, app_waddr,
                Mux(in_sg1w, sg1_waddr,
                    r_base + (row * self.row_pad) + (dbeat << lg))))
        tw_data = Mux(
            k4w_go, k4w_data,
            Mux(
                in_appw, app_wdata,
                Mux(in_sg1w, sg1_wdata[:self.data_width],
                    drain_sr[:self.data_width])))
        if nonquad:
            # The K window's write-back has the port to itself: the
            # store's payload beats at r_base, then the meta banks at
            # m_base.
            kd_mdata = Signal(self.data_width)
            with m.Switch(kdb - kpb):
                for b in range(_kwin_mb_max):
                    with m.Case(b):
                        m.d.comb += kd_mdata.eq(Cat(*metab[b]))
                with m.Default():
                    m.d.comb += kd_mdata.eq(0)
            with m.If(kd_stage):
                m.d.sync += [
                    kd_addr.eq(
                        Mux(kd_pay, r_base + (kdb << 6),
                            m_base + ((kdb - kpb) << 6))),
                    kd_data.eq(Mux(kd_pay, kwin_r.data, kd_mdata)),
                ]
            tw_valid = tw_valid | kd_v
            tw_addr = Mux(kd_v, kd_addr, tw_addr)
            tw_data = Mux(kd_v, kd_data, tw_data)
        m.d.comb += [
            self.tmem_w.valid.eq(tw_valid),
            self.tmem_w.bits.addr.eq(tw_addr),
            self.tmem_w.bits.data.eq(tw_data),
        ]

        # ------------------------------------------------------------------
        # Main FSM.
        # ------------------------------------------------------------------
        def start_block():
            m.d.sync += [gi.eq(0), xmax16.eq(0), xnan.eq(0)]

        def start_scale():
            m.d.sync += [s_step.eq(0), s_wait.eq(0), zero_blk.eq(0)]

        rowclr = Signal()  # the finished row's assembly clears next cycle

        def clear_row():
            for k in range(row_words):
                m.d.sync += rowwords[k].eq(0)

        def advance_row(last='DONE'):
            """Next-row transition shared by the drain paths: bump the
            row cursor, arm the row assembly's clear, and enter the next
            row's scale (prefetched) or gather. ``last`` is the state
            entered after the final row (a K window run still has its
            store to write back).

            The clear lands one cycle after this transition, from the
            ``rowclr`` register below: nothing writes the assembly in the
            first cycle of GATHER, SCALE, KDRAIN or DONE, and every drain
            path decides this transition in its last cycle from the
            staging geometry, so a clear issued here would put that
            geometry in front of all 17 words' enables."""
            with m.If(row == last_row):
                m.next = last
            with m.Else():
                m.d.sync += [
                    row.eq(row + 1),
                    blk.eq(0),
                    wbyte.eq(0),
                    cur_bank.eq(~cur_bank),
                ]
                if self.n_outputs != 4:
                    # The staging position rides the row cursor: the
                    # lane wraps at the group width, and the group ends
                    # with it.
                    with m.If(slane == self.n_outputs - 1):
                        m.d.sync += [slane.eq(0), sgrp.eq(sgrp + 1)]
                    with m.Else():
                        m.d.sync += slane.eq(slane + 1)
                m.d.sync += rowclr.eq(1)
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

        with m.If(rowclr):
            m.d.sync += [
                rowclr.eq(0), *[rowwords[k].eq(0) for k in range(row_words)]
            ]

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
                        wide.eq(offer_wide),
                        krow.eq(offer_krow | offer_va),
                        part.eq(offer_part),
                        live.eq(offer_live[:7]),
                        pair_base.eq((offer.k - 1) & 14),
                        *([
                            vapp.eq(offer_va),
                            kapp.eq(offer_ka),
                            kr.eq((offer.k - 1)[:4])
                        ] if app_on else []),
                        app_blk.eq(Mux(offer_k4, offer_k4_blk, offer_va1)),
                        app_pos.eq((offer.k - 1)[:5]),
                        *([app_w.eq(0),
                           app_ph.eq(0),
                           app_rd.eq(0)] if app_on else []),
                        fill_wlo.eq(
                            Mux(offer_vb1 | (offer_k4 & offer_k4_blk),
                                32 * (offer_slot_bytes >> 2), 0)),
                        fill_word.eq(
                            Mux(offer_vb1 | (offer_k4 & offer_k4_blk),
                                32 * (offer_slot_bytes >> 2), 0)),
                        fill_total.eq(
                            Mux(offer_app, offer_slot_bytes >> 2,
                                offer_live * (offer_slot_bytes >> 2)
                                ) if app_on else offer_live *
                            (offer_slot_bytes >> 2)),
                        fill_beat.eq(0),
                        fill_have.eq(0),
                        fill_next_v.eq(0),
                        fill_grp.eq(0),
                        fill_gcnt.eq(0),
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
                        # Generalized-interleave run state: the staging
                        # position, both accumulators' beat walks, the
                        # armed meta write, the pre-read walks and the
                        # retained straddle half all start empty.
                        *([
                            sgrp.eq(0),
                            slane.eq(0),
                            mbeat0.eq(0),
                            mbeat1.eq(1),
                            mwr_v.eq(0),
                            mwr_beat.eq(0),
                            strret_v.eq(0),
                            gmet_cnt.eq(0),
                            gbdy_cnt.eq(0),
                            pre_kind.eq(0),
                            pre_acc.eq(0),
                            pre_bpar.eq(0),
                            # The K window's run geometry and walks.
                            kloff.eq(offer_loff),
                            kpb.eq(kpb_o),
                            kmb.eq(kmb_o),
                            kmhw.eq(kmhw_o),
                            kpp.eq(0),
                            kpm.eq(0),
                            kpj.eq(0),
                            k4_ncomp.eq(0),
                            kdb.eq(0),
                            kd_pending.eq(0),
                            kdb_d.eq(_kwin_depth + _kwin_mb_max)
                        ] if self.n_outputs != 4 else []),
                        # K4 selective-append state. The walk STARTS in
                        # quick mode: the first rows may all be quick
                        # (a real prior state beats the new column),
                        # and the quick stream's pipeline gate needs
                        # the flag from the first dispatch.
                        k4.eq(offer_k4),
                        k4_heal.eq(Cat(offer.k, Const(0, 8))[VB_K4_HEAL]),
                        k4_bufd.eq(0),
                        k4_hold_risen.eq(0),
                        k4_hold_byte.eq(0),
                        k4q_pend.eq(0),
                        k4_land_row.eq(0),
                        k4_rpipe0.eq(0),
                        k4_rpipe1.eq(0),
                        # D3b decoupled risen pipeline: all K4 runs.
                        # Heal starts with two free prefetch banks.
                        # Contexts, dispatch FIFO,
                        # element cursor, landing tags and the risen
                        # completion all start empty.
                        ec_dispatch_done.eq(0),
                        hh_owned.eq(0),
                        hh_ready.eq(0),
                        hh_row.eq(0),
                        hh_done.eq(0),
                        hh_busy.eq(0),
                        hh_iss.eq(0),
                        hh_valid.eq(0),
                        hh_mag.eq(0),
                        *[ec_alloc[i].eq(0) for i in range(EC_CTXS)],
                        *[ec_st[i].eq(0) for i in range(EC_CTXS)],
                        *[ec_cnt[i].eq(0) for i in range(EC_CTXS)],
                        *[ec_done[i].eq(0) for i in range(EC_CTXS)],
                        ec_fhead.eq(0),
                        ec_ftail.eq(0),
                        ec_fcnt.eq(0),
                        ec_row.eq(0),
                        ec_ii.eq(0),
                        ec_rqi.eq(0),
                        ec_x.eq(0),
                        ec_xv.eq(0),
                        ec_steal_d1.eq(0),
                        ec_rfly.eq(0),
                        ec_pend.eq(0),
                        ec_pend_ctx.eq(0),
                        ec_chunk.eq(0),
                        chunk_assm.eq(0),
                        ec_land_cnt.eq(0),
                        ec_qhead.eq(0),
                        ec_qtail.eq(0),
                        ec_qcnt.eq(0),
                        k4st_v.eq(0),
                        k4_rd_busy.eq(0),
                        k4_rd_cls.eq(0),
                        k4_rd_beat.eq(0),
                        k4st_next.eq(0),
                        k4met_next.eq(0),
                        k4pay_next.eq(0),
                        # beat-0 pre-requests (fired from the first
                        # K4ROW cycle; the walk needs both before any
                        # patch).
                        k4st_rq.eq(offer_k4),
                        k4met_rq.eq(offer_k4),
                        k4met_v.eq(0),
                        k4pay_v.eq(0),
                        k4w_pay_v.eq(0),
                        k4w_st_v.eq(0),
                        k4w_met_v.eq(0),
                        k4w_lock.eq(0),
                        k4w_st_due.eq(0),
                        k4w_st_idx.eq(0),
                        k4w_st_pend.eq(0),
                        k4w_met_due.eq(0),
                        k4w_met_idx.eq(0),
                        k4_done_wait.eq(0),
                    ]
                    m.next = 'FILL'

            with m.State('FILL'):
                m.d.comb += in_fill.eq(1)
                with m.If((fill_word == fill_total) & ~fill_have
                          & ~rd_pend & ~fill_next_v):
                    m.d.sync += [row.eq(0), blk.eq(0), wbyte.eq(0)]
                    clear_row()
                    with m.If(k4):
                        # The K4 walk starts at the open block with the
                        # state/meta beat-0 pre-requests armed.
                        m.d.sync += blk.eq(app_blk)
                        m.next = 'K4ROW'
                    if self.n_outputs != 4:
                        # A K window run first pre-reads its staging
                        # window: the retained neighbours' cells the
                        # drain composes around its own rows.
                        with m.Elif(kwin_mode):
                            m.next = 'KPRE'
                    with m.Else():
                        start_block()
                        m.next = 'GATHER'

            if self.n_outputs != 4:
                with m.State('KPRE'):
                    # The K window pre-read: payload beats into the
                    # store, then meta beats into the banks (one
                    # outstanding at a time through the pre-read flag).
                    m.d.comb += in_kpre.eq(1)
                    with m.If((kpp == kpb) & (kpm == kmb) & ~pre_rd):
                        start_block()
                        m.next = 'GATHER'

                with m.State('KPATCH'):
                    # A rebuilt row lands its cells: one block per
                    # cycle, the chunk into the store half and the d16
                    # into the meta bank; then the next row (or, after
                    # the last, the write-back).
                    m.d.comb += in_kpatch.eq(1)
                    m.d.sync += kpj.eq(kpj + 1)
                    with m.If(kpj == Mux(wide, max_blocks - 1, 1)):
                        m.d.sync += [self.rows.eq(self.rows + 1), kpj.eq(0)]
                        advance_row('KDRAIN')

                with m.State('KDRAIN'):
                    # The store's payload beats, then the meta banks,
                    # back to the staging the host stores from. The
                    # last beat's write completes the run.
                    m.d.comb += in_kdrain.eq(1)
                    with m.If(kd_v & self.tmem_w.fire):
                        m.d.sync += self.beats.eq(self.beats + 1)
                        with m.If(kd_last):
                            m.next = 'DONE'

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
                    if app_on:
                        with m.If(vapp & (blk != last_blk)):
                            # The append's next gather segment: dims
                            # 32*(blk+1).. of the same slot continue the
                            # SAME amax fold (no xmax16/xnan reset, no
                            # scale yet) into the next bank.
                            m.d.sync += [
                                blk.eq(blk + 1),
                                gi.eq(0),
                                cur_bank.eq(~cur_bank),
                            ]
                            m.next = 'GATHER'
                        with m.Else():
                            # The last live slot folds this cycle; the
                            # scale sequencer arms on the next.
                            start_scale()
                            m.next = 'SCALE'
                    else:
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
                    # single scale at the last gather segment and
                    # writes EVERY block slot with the one per-token
                    # d16.
                    for j in range(max_blocks):
                        with m.If(blk == j):
                            wi, half = divmod(j * 34, 4)
                            m.d.sync += rowwords[wi][half * 8:half * 8 +
                                                     16].eq(d16)
                    if app_on:
                        with m.If(vapp):
                            for j in range(max_blocks):
                                wi, half = divmod(j * 34, 4)
                                m.d.sync += rowwords[wi][half * 8:half * 8 +
                                                         16].eq(d16)
                            # Widen the token d16 into its F32 row word
                            # now (the dedicated unit is idle in SCALE);
                            # the drain patches it beats later.
                            m.d.comb += [
                                f16wa.inp_valid.eq(1),
                                f16wa.inp.eq(d16)
                            ]
                            # The single scale ran at the last segment
                            # (the gather's final block); re-arm the quant
                            # walk as the block-0 pass so chunk 0
                            # quantizes dims 0..31 from bank 0 first, then
                            # the QUANT transition above walks the later
                            # banks' segments.
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
                        with m.If(blk != last_blk):
                            m.d.sync += [
                                blk.eq(blk + 1),
                                cur_bank.eq(~cur_bank)
                            ]
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

                    def quant_block0_done():
                        # A block-0 partial is done with this row
                        # once block 0 lands: block 1's extents
                        # stay loaded, so skip its zero-amax round
                        # entirely (and re-enter GATHER -- no
                        # prefetch is outstanding).
                        m.d.sync += drain_prefetched.eq(0)
                        m.next = 'DRAINLOAD'

                    def quant_next_block():
                        m.d.sync += [
                            blk.eq(blk + 1),
                            cur_bank.eq(~cur_bank),
                            xmax16.eq(pxmax16),
                            xnan.eq(pxnan),
                            # Realign the byte cursor: a capped
                            # quant stream stopped at 2 + cur_live,
                            # but the next block's bytes start at
                            # its fixed 34-byte slot boundary.
                            wbyte.eq((blk + 1) * 34),
                        ]
                        start_scale()
                        m.next = 'SCALE'

                    with m.If(blk != last_blk):
                        if app_on:
                            with m.If(vapp):
                                # The append's next quant segment: dims
                                # 32*(blk+1).. from the matching bank under
                                # the SAME scale (no second scale pass),
                                # their bytes at the fixed 34-byte chunk
                                # boundaries.
                                m.d.sync += [
                                    blk.eq(blk + 1),
                                    cur_bank.eq(~cur_bank),
                                    wbyte.eq((blk + 1) * 34 + 2),
                                    drain_prefetched.eq(0),
                                ]
                                m.next = 'QUANT'
                            with m.Elif(part_vb0):
                                quant_block0_done()
                            with m.Else():
                                quant_next_block()
                        else:
                            with m.If(part_vb0):
                                quant_block0_done()
                            with m.Else():
                                quant_next_block()
                    with m.Else():
                        # A partial run never prefetches the next row's
                        # selected block (nxt_live is 0 on the exit
                        # side), so its rows always re-enter GATHER; an
                        # append never leaves its one row either.
                        m.d.sync += drain_prefetched.eq((row != last_row)
                                                        & (part == 0) & ~vapp)
                        m.next = 'DRAINLOAD'

            def drainload_append():
                # Append-mode drain: the row assembly is the token
                # row; APPW pre-reads, patches and writes back the
                # V payload/scale-row beats or the K payload/meta
                # beats, all composed from the pre-read image.
                m.d.sync += [
                    self.rows.eq(self.rows + 1),
                    app_w.eq(0),
                    app_ph.eq(0),
                    app_rd.eq(0),
                ]
                if self.n_outputs != 4:
                    # The walk starts at the block's first cell
                    # (lane 0 of group 0).
                    m.d.sync += [
                        arow.eq(0),
                        alane.eq(0),
                        acell.eq(app_blk * self.n_outputs),
                        abeat.eq(0),
                        aph2.eq(0),
                        asrow.eq(0),
                    ]
                m.next = 'APPW'

            def drainload_legacy():
                m.d.sync += [
                    drain_sr.eq(rowbuf[:8 * VB_ROW_RAW]),
                    dbeat.eq(0),
                ]
                m.next = 'DRAIN'

            def drainload_v_row():
                """The row pass's exits, in their standing priority:
                the append drain (a build that accepts appends only),
                the legacy tile shift-out, the pre-read hold and the
                SG1 scatter."""
                if app_on:
                    with m.If(vapp | kapp):
                        drainload_append()
                    with m.Elif(~sg1):
                        drainload_legacy()
                else:
                    with m.If(~sg1):
                        drainload_legacy()
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
                        if self.n_outputs == 4:
                            # Partial gating: a V block-extent run writes
                            # only its block's cells; a K row-extent run
                            # writes only rows < live (dead rows keep the
                            # pre-read cells instead of today's zeros).
                            for j in range(max_blocks):
                                meta_go = (j < sg_blocks) & ~part_k_row_dead \
                                    & Mux(part_vb, part_blk == j, 1)
                                wi, half = divmod(j * 34, 4)
                                with m.If(meta_go):
                                    m.d.sync += meta_hw[meta_indices[j]].eq(
                                        rowwords[wi][half * 8:half * 8 + 16])
                            with m.If(~row[0]):
                                m.d.sync += [
                                    hold.eq(chunk)
                                    for hold, chunk in zip(holds, chunks)
                                ]
                                advance_row()
                            with m.Else():
                                m.d.sync += sg1_widx.eq(0)
                                m.next = 'SG1W'
                        else:
                            # The generalized interleave: scatter the
                            # row's d16 cells into the beat accumulator
                            # their staging index names (a row's cells
                            # can straddle a meta beat boundary -- the
                            # two accumulators alternate), load this
                            # row's held chunks, arm any completing
                            # meta beat, and enter SG1W when the row
                            # has entries or an armed write.
                            for j, cell in ((0, cell0), (1, cell1)):
                                meta_go = ~part_k_row_dead \
                                    & Mux(part_vb, part_blk == j, 1)
                                wi, half = divmod(j * 34, 4)
                                with m.If(meta_go):
                                    with m.If(cell[5]):
                                        m.d.sync += meta_hw2[cell[:5]].eq(
                                            rowwords[wi][half * 8:half * 8 +
                                                         16])
                                    with m.Else():
                                        m.d.sync += meta_hw[cell[:5]].eq(
                                            rowwords[wi][half * 8:half * 8 +
                                                         16])
                            with m.If(hold0):
                                m.d.sync += holds[0].eq(chunks[0])
                            with m.If(hold1):
                                m.d.sync += holds[1].eq(chunks[1])
                            with m.If(strh1):
                                m.d.sync += strhead_c1.eq(chunks[1])
                            with m.If(arm0):
                                m.d.sync += [
                                    mwr_v.eq(1),
                                    mwr_sel.eq(0),
                                    mwr_beat.eq(mbeat0),
                                    mbeat0.eq(mbeat0 + 2),
                                ]
                            with m.If(arm1):
                                m.d.sync += [
                                    mwr_v.eq(1),
                                    mwr_sel.eq(1),
                                    mwr_beat.eq(mbeat1),
                                    mbeat1.eq(mbeat1 + 2),
                                ]
                            m.d.sync += sg1_widx.eq(0)
                            with m.If(comp0 | strc0 | comp1 | arm0 | arm1):
                                m.next = 'SG1W'
                            with m.Else():
                                advance_row()

            with m.State('DRAINLOAD'):
                m.d.comb += in_drainload.eq(1)
                if self.n_outputs != 4:
                    # A K window row lands its cells in KPATCH (every K
                    # mode of a non-quad build is a window run, so the
                    # append is the only branch it displaces).
                    with m.If(kwin_mode):
                        m.d.sync += kpj.eq(0)
                        m.next = 'KPATCH'
                    with m.Else():
                        drainload_v_row()
                else:
                    drainload_v_row()

            with m.State('K4ROW'):
                # Both K4 prongs dispatch into the shared chain/element
                # pipeline. Heal waits for its folded prefetch bank;
                # ordinary appends test the new column against state.
                # State beat g+2 is requested only after group g lands
                # and its write retires, preserving the two-bank RMW
                # protocol even when dispatch runs ahead.
                m.d.comb += in_k4row.eq(1)
                with m.If(k4 & ~k4_bufd):
                    m.d.sync += k4_bufd.eq(1)
                with m.If(k4_disp & row[:3].all()):
                    with m.If(row[3]):
                        m.d.sync += k4st_v[1].eq(0)
                    with m.Else():
                        m.d.sync += k4st_v[0].eq(0)
                with m.If(k4_done_wait):
                    # The last row's completion armed this: every
                    # element landed and every context freed with it,
                    # so only the beat writes can remain.
                    with m.If(~k4w_pay_v & ~k4w_st_v & ~k4w_met_v
                              & ~k4w_st_due & ~k4w_met_due & ~k4_rd_busy
                              & ~k4st_rq & ~k4met_rq
                              & ~ec_rfly & ~ec_pend & ~ec_xv & ~ec_steal_d1
                              & (ec_fcnt == 0) & (ec_qcnt == 0)
                              & ~Cat(*[a for a in ec_alloc]).any()):
                        m.next = 'DONE'
                with m.Elif(k4_disp):
                    with m.If(row != last_row):
                        m.d.sync += row.eq(row + 1)

            with m.State('SG1W'):
                # SG1 staging writes: payload beat pair (widx 0, 1)
                # plus the meta beat on sixteenth rows (widx 2).
                m.d.comb += in_sg1w.eq(1)
                with m.If(sg1_last):
                    advance_row()

            if app_on:
                with m.State('APPW'):
                    # Append-mode patch drain: each beat was pre-read into
                    # the accumulator with its patch bytes (the widened d16
                    # word, a whole K chunk, or the K meta cells) scattered
                    # on the response; write the composed beat back, then
                    # advance to the next. A V append walks 36 beats at 64
                    # dims / 68 at 128 (payload then scale row); a K append
                    # walks one beat per block plus the meta beat.
                    m.d.comb += in_appw.eq(1)
                    app_last = Mux(
                        kapp, Mux(wide, Const(max_blocks, 7), Const(2, 7)),
                        Mux(wide, Const(VB_APP_WIDE_TOTAL_BEATS - 1, 7),
                            Const(35, 7)))
                    if nonquad:
                        # The V append walk: a payload beat's write either
                        # fetches the next beat of the block's cells or, once
                        # the last row landed, opens the four-beat scale-row
                        # phase (app_w 0..3); a second row of the same beat
                        # patches from the accumulator one cycle after the
                        # response, and arms the write after it (that walk
                        # lives beside the compose logic above).
                        with m.If(app_ph & self.tmem_w.fire):
                            m.d.sync += self.beats.eq(self.beats + 1)
                            with m.If(~asrow):
                                m.d.sync += app_ph.eq(0)
                                with m.If(arow == dims):
                                    m.d.sync += [asrow.eq(1), app_w.eq(0)]
                            with m.Else():
                                with m.If(app_w == 3):
                                    m.next = 'DONE'
                                with m.Else():
                                    m.d.sync += [
                                        app_w.eq(app_w + 1),
                                        app_ph.eq(0)
                                    ]
                    else:
                        with m.If(app_ph & self.tmem_w.fire):
                            m.d.sync += self.beats.eq(self.beats + 1)
                            with m.If(app_w == app_last):
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
        ec_chain_busy = Cat(
            *[ec_alloc[i] & ~ec_done[i] for i in range(EC_CTXS)]).any()
        # Exposed heal folding counts as gather; when it overlaps a
        # scale chain, scale owns the cycle. These are exclusive phase
        # counters, not additive busy counters for the parallel engines.
        ec_heal_gather = Signal()
        m.d.comb += ec_heal_gather.eq(in_k4row & k4_heal & (hh_busy | hh_valid) \
            & ~ec_chain_busy)
        with m.If(in_gather | prefetch_go | ec_heal_gather):
            m.d.sync += self.gather_cycles.eq(self.gather_cycles + 1)
        # Exclusive K4 phase partition: active chains own scale,
        # exposed heal folding owns gather, and the remaining dispatch,
        # element and completion work owns quant. Parallel work is never
        # added twice to the D1 active-cycle identity.
        with m.If(in_scale | (in_k4row & ec_chain_busy)):
            m.d.sync += self.scale_cycles.eq(self.scale_cycles + 1)
        with m.If(in_quant | (in_k4row & ~ec_chain_busy & ~ec_heal_gather)):
            m.d.sync += self.quant_cycles.eq(self.quant_cycles + 1)
        with m.If(in_drain | in_sg1w | in_appw | in_kpatch | in_kdrain):
            m.d.sync += self.drain_cycles.eq(self.drain_cycles + 1)
        with m.If(running):
            m.d.sync += self.run_cycles.eq(self.run_cycles + 1)
        # D1 riders (see the declaration block for the partition
        # identity). The wait subsets must stay subsets: fill waits
        # only in FILL, drain-side waits only across the drain states,
        # prefetch only inside QUANT. A K window run's staging pre-read
        # is dispatch-side (it holds the row pass like DRAINLOAD does).
        with m.If(in_drainload | in_kpre):
            m.d.sync += self.dispatch_cycles.eq(self.dispatch_cycles + 1)
        with m.If(in_fill & ~fill_have):
            m.d.sync += self.fill_wait_cycles.eq(self.fill_wait_cycles + 1)
        drain_wait = Signal()
        m.d.comb += drain_wait.eq((in_sg1w & sg1_wr & ~self.tmem_w.fire) \
            | (in_drain & ~self.tmem_w.fire) \
            | (in_appw & app_ph & ~self.tmem_w.fire) \
            | (in_appw & app_rd & ~self.tmem_rresp.fire) \
            | (in_appw & app_req & ~self.tmem_rreq.fire) \
            | (in_kdrain & kd_v & ~self.tmem_w.fire))
        with m.If(drain_wait):
            m.d.sync += self.drain_wait_cycles.eq(self.drain_wait_cycles + 1)
        with m.If(prefetch_go):
            m.d.sync += self.prefetch_cycles.eq(self.prefetch_cycles + 1)
        with m.If(self.active):
            m.d.sync += self.active_cycles.eq(self.active_cycles + 1)
        return m
