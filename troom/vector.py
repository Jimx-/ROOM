"""FP32 vector/reduction unit for the troom accelerator.

One engine serves both datapath opcodes: ``VECTOR`` streams an
elementwise op over TMEM regions and writes the padded result region,
``REDUCE`` folds one region into the single FP32 word of one
zero-padded beat. The op set covers the decode-path pieces the plans
name:

* ``VEC_ADD``/``VEC_SUB``/``VEC_MUL``/``VEC_SCALE`` -- residual
  addition, elementwise products, and scaling by a TMEM-resident
  scalar (the arithmetic of residual streams and RoPE rotations),
* ``RDC_SUM``/``RDC_SUMSQ`` -- sum and sum-of-squares reductions with
  the same ``isum_4`` partial placement and final fold as the matmul
  epilogue (RMSNorm's energy term),
* ``RDC_MAX`` -- RISC-V ``fmax`` fold for the softmax maximum,
* ``VEC_EXP2``/``VEC_RECIP``/``VEC_RSQRT`` -- unary transcendentals
  (SiLU/SwiGLU, softmax normalization, RMSNorm's inverse square
  root) as fixed sequences of rounded FP32 mul/add/sub over an
  ``FPURec`` table seed or a Taylor Horner evaluation, per the
  contract frozen in ``tests/troom/model.py``
  (``plans/2026-10-10-troom-vector-transcendentals.md``).
* ``VEC_QUANT`` -- device-side activation quantization: per 32-word
  segment, an fmax fold of ``|x|``, the scale pair
  ``sc = R(amax/127)`` and ``inv = R(127*recip(amax))`` (the frozen
  recip sequence), and RNE INT8 rounding of ``R(x*inv)``, emitted as
  the packed-quants region at ``o_base`` and the per-segment
  ``s_act``/sum meta region at ``b_base`` -- the image the
  ``QUANTIZE`` copy consumes, per the same frozen contract.
* ``VEC_F16`` -- FP32 -> FP16 narrowing: the frozen ``Fp32To16``
  sequence (RNE for every bit pattern including the FP16 subnormal
  range, NaNs canonicalized to a quiet NaN with the sign kept),
  packed two halfwords per word at ``o_base`` with a zero-padded
  final partial beat. The KV pool append (E1's native SET_ROWS)
  stores this image straight into the pool, byte-identical to the
  host's ``ggml_fp32_to_fp16`` conversion.

Numerical contract (``R`` is FP32 round-to-nearest-even, matching
``tests/troom/model.py`` bit for bit):

.. code-block:: text

    VEC_ADD   o[i] = R(a[i] + b[i])
    VEC_SUB   o[i] = R(a[i] - b[i])
    VEC_MUL   o[i] = R(a[i] * b[i or 0])   # signed zero products
    RDC_SUM   p[i mod 4] = R(p[i mod 4] + a[i]); result = fold
    RDC_SUMSQ p[i mod 4] = R(p[i mod 4] + R(a[i]*a[i])); result = fold
    RDC_MAX   m = a[0]; m = fmax(m, a[i])        # RISC-V fmax
    fold      R(R(p[0] + p[2]) + R(p[1] + p[3]))
    VEC_RECIP y = seed_rec(|x|); twice
              {t = R(|x|*y); t = R(2-t); y = R(y*t)}; sign of x
    VEC_RSQRT y = seed_rsqrt(x); h = R(0.5*x); twice
              {t = R(h*y); t = R(t*y); t = R(1.5-t); y = R(y*t)}
    VEC_EXP2  i = floor(x); f = R(x - float32(i)); p = C7;
              k = 6..0: p = R(R(p*f) + Ck); result = p * 2^i
              (Ck = fp32(ln2^k/k!); the scaling is an exponent add)
    VEC_SILU  sigmoid(x) = recip(exp2(x * -log2(e)) + 1), every
              rounding point the composition of the VEC_EXP2 and
              VEC_RECIP contracts above: m0 = R(a*-log2 e); exp2's
              classification and Horner on m0; w3 = R(w2 + 1.0);
              recip's classification of w3 and its seed/Newton
              sequence with the sign of w3 (in domain w3 >= 1, so the
              sign fold is the identity); o = R(a * w4). The -log2(e)
              scalar and the +1 are embedded constants -- the same
              bits the standalone chain's VEC_SCALE scalar beat and
              ones vector carried -- so the op is bit-identical to
              the six-command backend chain (SwiGLU) by construction.
    VEC_QUANT per 32-word segment: a = fmax fold of |x| from +0;
              sc = R(a * (1/127)); r = recip(a);
              q_i = q8(R(R(x_i * r) * 127)) (RNE INT8, saturating;
              the unscaled r keeps every intermediate <= ~127);
              quants -> o_base, (s_act, int16 sum) meta -> b_base
    VEC_SMEXP  m = fmax_i(a[i]) (RDC_MAX's fold, order-free);
              e[i] = VEC_EXP2(R(a[i] - m)) -> o_base;
              sum = RDC_SUM's isum_4 fold over e -> b_base beat

Every rounding point is a separate non-fused operation. NaN operands
and invalid operations produce the canonical NaN; signed zeros follow
the ROOM blocks' measured IEEE behavior (an exact cancellation under
RNE yields +0, a zero product keeps the product's sign).
``RDC_MAX`` is order independent (any NaN input is dropped, +0 beats
-0), so its serial fold needs no ``isum_4`` placement; the sum
reductions are order dependent and pin the exact ``isum_4`` order
above. The transcendental special values (zeros, subnormals, clamps,
infinities, NaNs) are classified before any arithmetic and bypass the
sequences, so no refinement ever sees an exceptional operand; the
seed is the ROOM ``FPURec`` block, whose native zero/infinity/NaN
results match the frozen table. Accuracy is ulp-bounded, not
correctly rounded; the model records the measured maxima (2/2/13 ulp
for recip/rsqrt/exp2 over the tested domains).

The arithmetic reuses the CPU floating-point blocks (``FPUFMA`` for
add/subtract/multiply, ``FPUComp`` for the maximum, ``FPURec`` for
the reciprocal/rsqrt seeds). They use valid protocol interfaces
without backpressure, so the unit is self-timed: elementwise results
flow through a credit-accounted FIFO into the beat packer, reductions
track partial-busy and in-flight counts, and every consumer is gated
on the producing unit's ``out.valid`` rather than a hardcoded
latency. The binary/reduction datapaths issue one element per cycle
when the read windows and credits allow; ``RDC_MAX``'s serial
dependency issues one comparison per unit latency; the
transcendentals run each element's fixed dependency sequence in one
of ``vec_unary_ctx`` independent contexts that share the FPU blocks
through per-unit arbitration (one add, one multiply, and one seed
issue per cycle across contexts per issue group), with per-unit
context tags routing every ``out.valid`` to its owning context and an
in-order retire buffer preserving element order. Contexts partition by
index modulo the configured unit count (1, 2, or experimental 4),
independently for adds and multiplies. The default two groups double
unary issue capacity over E2; four groups price additional arithmetic
throughput without changing the sequencer. Each
element's operation sequence, and therefore its numerical contract,
is unchanged by the interleaving and by the partition; with four
contexts the per-element cost is the op count (exp2's sixteen
dependent rounded ops) times the four-cycle capture loop, divided by
four -- roughly sixteen cycles per element instead of sixty-one.

``VEC_QUANT`` reuses both multiplier groups when ``vec_quant_pipe``
is enabled: consecutive elements flow through ``R(x*r)`` and then
``R(t*127)``, with a register between them. Its absolute-max fold
uses an unsigned magnitude comparison with NaNs ignored, exactly
equivalent to the original fmax fold from +0. One result-word credit
is reserved per four elements before either unstallable pipeline
starts. The segment's scale/reciprocal sequence and all rounding
points remain unchanged. Single-multiplier builds, or
``vec_quant_pipe=False``, keep the historical serial datapath.

Regions are read as consecutive whole beats through per-region byte
windows (any beat width works; ``data_width`` must be at least 32
bits), one outstanding TMEM read at a time, alternating between the A
and B windows. The output region must not overlap an input region the
operation reads -- A always, and B for the ops that read it (the
binary elementwise ops and ``VEC_SCALE``'s scalar beat); reductions
and the unary transcendentals never read B, so a B alias with their
output is accepted. Aliased inputs are allowed; in-place
output is deferred. A rejected descriptor latches nothing and leaves
the unit inert. A descriptor whose element count is
zero, whose output overlaps a read input, whose bases are not beat
aligned, or whose regions leave TMEM completes at accept with
``BAD_SHAPE``, ``BAD_ALIGN``, or ``BAD_RANGE`` without reading TMEM.
"""

from amaranth import *
from amaranth.hdl.rec import Direction, Record

from room.consts import RoundingMode
from room.fpu import FPUComp, FPFormat, FPUFMA, FPUOperator, FPURec

from troom.consts import (
    Status,
    VecOp,
    VEC_ELEMENTWISE,
    VEC_READS_B,
    VEC_REDUCTIONS,
)
from troom.convert import Fp32To16
from troom.types import HasTRoomParams

from roomsoc.interconnect.stream import Decoupled, Queue

# exp2 Taylor coefficients Ck = fp32(ln(2)**k / k!), k = 0..7,
# derived from the mathematical definition and frozen (with
# provenance) as EXP2_COEFFS in tests/troom/model.py; the bit-exact
# unit tests hold this table and the model to identical patterns.
EXP2_C_BITS = (
    0x3f800000,  # C0 = fp32(1)
    0x3f317218,  # C1 = fp32(ln 2)
    0x3e75fdf0,  # C2 = fp32(ln^2 2 / 2)
    0x3d635847,  # C3 = fp32(ln^3 2 / 6)
    0x3c1d955b,  # C4 = fp32(ln^4 2 / 24)
    0x3aaec3ff,  # C5 = fp32(ln^5 2 / 120)
    0x39218489,  # C6 = fp32(ln^6 2 / 720)
    0x377fe5fe,  # C7 = fp32(ln^7 2 / 5040)
)

# The VEC_QUANT scale pair, frozen in tests/troom/model.py as
# INV127_BITS/F127_BITS: sc = R(amax * (1/127)) and
# inv = R(127.0 * recip(amax)).
INV127_BITS = 0x3c010204  # fp32(1/127)
F127_BITS = 0x42fe0000  # fp32(127.0)

# The VEC_SILU embedded scalars: fp32(-log2(e)) -- bit-identical to
# the scalar beat the backend uploaded for the chain's VEC_SCALE, so
# the fused op reproduces the six-command chain's every rounding
# point -- and fp32(1.0) for the sigmoid's +1.
SILU_NLOG2E_BITS = 0xbfb8aa3b  # fp32(-log2(e))
SILU_ONE_BITS = 0x3f800000  # fp32(1.0)


class VectorDesc(HasTRoomParams, Record):
    """Vector/reduction descriptor: the sub-op, element count, regions.

    ``n`` FP32 elements are read at ``a_base`` (and ``b_base`` for the
    binary elementwise ops; ``VEC_SCALE`` reads only ``b_base``'s first
    word). Elementwise results fill the padded ``n * 4``-byte region at
    ``o_base`` with the final partial beat zero-padded; reductions write
    one zero-padded beat at ``o_base``.
    """

    def __init__(self, params, name=None, src_loc_at=0):
        HasTRoomParams.__init__(self, params)
        Record.__init__(self, [
            ('op', VecOp, Direction.FANOUT),
            ('n', self.lg_act_k + 1, Direction.FANOUT),
            ('a_base', self.tmem_addr_width, Direction.FANOUT),
            ('b_base', self.tmem_addr_width, Direction.FANOUT),
            ('o_base', self.tmem_addr_width, Direction.FANOUT),
        ],
                        name=name,
                        src_loc_at=src_loc_at + 1)


class VectorUnit(HasTRoomParams, Elaboratable):
    """Elementwise and reduction engine over FP32 TMEM regions.

    A descriptor is accepted in ``IDLE``; a rejected one completes at
    accept without side effects. A well-formed descriptor streams its
    regions and completes (``done``) when its final result beat
    commits to TMEM. The unit runs one command at a time.
    """

    def __init__(self, params):
        super().__init__(params)

        if self.data_width < 32:
            raise ValueError('vector data_width must be at least 32 bits')

        self.sink = Decoupled(VectorDesc, self.params)
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
        self.elements = Signal(48)
        self.write_beats = Signal(48)
        self.stall_cycles = Signal(48)
        # Per-unit FPU issue fires and the busy window they divide,
        # for issue-slot utilization attribution (M4). Timing-cold
        # observation only; nothing in the datapath reads them.
        self.active_cycles = Signal(48)
        self.add_issues = Signal(48)
        self.mul_issues = Signal(48)
        self.rec_issues = Signal(48)
        self.comp_issues = Signal(48)

    def elaborate(self, platform):
        m = Module()

        lg = self.lg_beat
        bb = self.beat_bytes
        aw = self.tmem_addr_width
        bw = self.data_width // 32  # FP32 words per beat

        # Widest expressible element count bounds every beat cursor.
        max_beats = (2 * self.act_max_k * 4 + bb - 1) // bb

        # Register operand selection before the FPU arithmetic stage.
        # Issue group 0 (even contexts) shares fadd/fmul with the
        # elementwise/reduction/quantization paths exactly as before;
        # the second pair (vec_*_units == 2) serves issue group 1
        # (odd contexts) through dedicated unary operand muxes. One
        # command runs at a time, so the groups never contend with the
        # elementwise paths, and each group is the proven E2-c
        # structure (one add slot, one multiply slot per cycle) -- the
        # fabric is two of them in parallel, parity partitioned.
        fadd = m.submodules.fadd = FPUFMA(32, FPFormat.S, latency=3)
        fmul = m.submodules.fmul = FPUFMA(32, FPFormat.S, latency=3)
        fcomp = m.submodules.fcomp = FPUComp(32, FPFormat.S, latency=2)
        frec = m.submodules.frec = FPURec(32, FPFormat.S, latency=3)
        # VEC_SMEXP's denominator fold runs on a dedicated adder: the
        # shared adders carry the Horner load of pass 2 (about 1.65
        # issues per cycle across the two groups at the exp2 floor),
        # so a shared accumulate would contend with context-tagged
        # issues (an untagged result would misroute the tag FIFOs) or
        # stall the mandatory in-order retire. The unit carries the
        # isum_4 accumulations and the final fold; nothing else uses
        # it, so its in-order pipeline needs no tags.
        fsum = m.submodules.fsum = FPUFMA(32, FPFormat.S, latency=3)
        fadd1 = fmul1 = None
        if self.vec_add_units >= 2:
            fadd1 = m.submodules.fadd1 = FPUFMA(32, FPFormat.S, latency=3)
        if self.vec_mul_units >= 2:
            fmul1 = m.submodules.fmul1 = FPUFMA(32, FPFormat.S, latency=3)
        # Optional extra issue groups preserve the same per-context sequence.
        # Group zero still shares the ordinary vector datapath.
        extra_add = []
        extra_mul = []
        for kind, count, units in (('add', self.vec_add_units, extra_add),
                                   ('mul', self.vec_mul_units, extra_mul)):
            for g in range(2, count):
                unit = FPUFMA(32, FPFormat.S, latency=3)
                setattr(m.submodules, f'f{kind}{g}', unit)
                units.append(unit)

        EXP2_C = Array(Const(c, 32) for c in EXP2_C_BITS)

        res_q = m.submodules.res_q = Queue(self.vec_result_q, Record,
                                           [('val', 32, Direction.FANOUT)])
        sq_q = m.submodules.sq_q = Queue(self.vec_sq_q, Record,
                                         [('val', 32, Direction.FANOUT)])

        #
        # Descriptor validation, on the offered combination. Op
        # classes are explicit membership predicates (never ordinal
        # ranges), so validation, dispatch, and the model agree as
        # the op set grows.
        #
        offer = self.sink.bits
        o_elem = Cat(offer.op == op for op in VEC_ELEMENTWISE).any()
        o_reduce = Cat(offer.op == op for op in VEC_REDUCTIONS).any()
        o_scale = offer.op == VecOp.VEC_SCALE
        o_quant = offer.op == VecOp.VEC_QUANT
        o_f16 = offer.op == VecOp.VEC_F16
        o_pswap = offer.op == VecOp.VEC_PSWAP
        o_smexp = offer.op == VecOp.VEC_SMEXP
        o_reads_b = Cat(offer.op == op for op in VEC_READS_B).any()
        op_bad = ~(o_elem | o_reduce)

        eb = offer.n << 2
        a_pad = ((eb + (bb - 1)) >> lg) << lg
        # VEC_QUANT writes two regions: nsegs*32 bytes of packed
        # quants at o_base, nsegs*8 bytes of meta at b_base.
        q_nsegs = (offer.n + 31) >> 5
        q_o_pad = ((q_nsegs * 32 + (bb - 1)) >> lg) << lg
        q_m_pad = ((q_nsegs * 8 + (bb - 1)) >> lg) << lg
        # VEC_F16 packs two halfwords per word: its output region is
        # the n*2-byte image (beat padded).
        f_o_pad = ((offer.n << 1) + (bb - 1) >> lg) << lg
        # VEC_SMEXP writes the n-word numerator image at o_base and
        # one denominator beat at b_base.
        b_pad = Mux(o_scale, bb, Mux(o_quant, q_m_pad, Mux(o_smexp, bb,
                                                           a_pad)))
        o_pad = Mux(o_reduce, bb,
                    Mux(o_quant, q_o_pad, Mux(o_f16, f_o_pad, a_pad)))
        # Amaranth addition extends by one bit, so no end address can
        # wrap before its range comparison.
        a_end = offer.a_base + a_pad
        b_end = offer.b_base + b_pad
        o_end = offer.o_base + o_pad

        overlap_a = (offer.o_base < a_end) & (offer.a_base < o_end)
        # Only operations that read the B region check it: the binary
        # elementwise ops and VEC_SCALE's scalar beat. Reductions and
        # the unary transcendentals never touch B, so a B alias with
        # the output must not reject them. VEC_QUANT writes its meta
        # there, so its B region is checked as a second output.
        overlap_b = o_reads_b & (offer.o_base < b_end) \
            & (offer.b_base < o_end)
        quant_overlap = o_quant \
            & ((offer.o_base < b_end) & (offer.b_base < o_end)
               | (offer.b_base < a_end) & (offer.a_base < b_end))
        # VEC_SMEXP's denominator beat is a second output read by
        # later commands, so all three of its regions stay disjoint
        # exactly like VEC_QUANT's.
        smexp_overlap = o_smexp \
            & ((offer.o_base < b_end) & (offer.b_base < o_end)
               | (offer.b_base < a_end) & (offer.a_base < b_end))
        shape_bad = op_bad | (offer.n == 0) | overlap_a | overlap_b \
            | quant_overlap | smexp_overlap | (o_pswap & offer.n[0])
        align_bad = offer.a_base[:lg].any() | offer.o_base[:lg].any() \
            | (o_reads_b & offer.b_base[:lg].any()) \
            | ((o_quant | o_smexp) & offer.b_base[:lg].any())
        range_bad = (a_end > self.tmem_bytes) | (o_end > self.tmem_bytes) \
            | (o_reads_b & (b_end > self.tmem_bytes)) \
            | (o_quant & (b_end > self.tmem_bytes)) \
            | (o_smexp & (b_end > self.tmem_bytes))
        bad = shape_bad | align_bad | range_bad

        q_qbeats = (q_nsegs * 32 + (bb - 1)) >> lg
        q_mbeats = (q_nsegs * 8 + (bb - 1)) >> lg
        nbeats_o = Mux(
            o_reduce, 1,
            Mux(
                o_quant, q_qbeats + q_mbeats,
                Mux(o_f16, ((offer.n << 1) + (bb - 1)) >> lg,
                    (eb + (bb - 1)) >> lg)))
        # The input regions always span ceil(n * 4 / beat) beats; only
        # the output region shrinks to one beat for a reduction.
        ibeats_o = (eb + (bb - 1)) >> lg

        #
        # Latched run state.
        #
        op = Signal(VecOp)
        n_l = Signal(self.lg_act_k + 1)
        a_base = Signal(aw)
        b_base = Signal(aw)
        o_base = Signal(aw)
        nbeats = Signal(range(max_beats + 1))
        ibeats = Signal(range(max_beats + 1))

        # Active-datapath selects, driven from the run FSM below. All
        # execution -- reads, arithmetic launches, and result writes --
        # is gated on an accepted command being in flight, so a
        # rejected descriptor (which latches nothing) and the terminal
        # state of a completed one stay inert in ``IDLE``.
        in_elem = Signal()
        in_rdc = Signal()
        running = in_elem | in_rdc

        # Explicit combinational signals keep shared decode expressions from
        # being expanded independently at every consumer during RTL lowering.
        op_add = Signal()
        m.d.comb += op_add.eq(op == VecOp.VEC_ADD)
        op_sub = Signal()
        m.d.comb += op_sub.eq(op == VecOp.VEC_SUB)
        op_mul = Signal()
        m.d.comb += op_mul.eq(op == VecOp.VEC_MUL)
        op_scale = Signal()
        m.d.comb += op_scale.eq(op == VecOp.VEC_SCALE)
        op_sum = Signal()
        m.d.comb += op_sum.eq(op == VecOp.RDC_SUM)
        op_max = Signal()
        m.d.comb += op_max.eq(op == VecOp.RDC_MAX)
        op_sumsq = Signal()
        m.d.comb += op_sumsq.eq(op == VecOp.RDC_SUMSQ)
        op_exp2 = Signal()
        m.d.comb += op_exp2.eq(op == VecOp.VEC_EXP2)
        op_recip = Signal()
        m.d.comb += op_recip.eq(op == VecOp.VEC_RECIP)
        op_rsqrt = Signal()
        m.d.comb += op_rsqrt.eq(op == VecOp.VEC_RSQRT)
        op_quant = Signal()
        m.d.comb += op_quant.eq(op == VecOp.VEC_QUANT)
        op_f16 = Signal()
        m.d.comb += op_f16.eq(op == VecOp.VEC_F16)
        op_pswap = Signal()
        m.d.comb += op_pswap.eq(op == VecOp.VEC_PSWAP)
        op_silu = Signal()
        m.d.comb += op_silu.eq(op == VecOp.VEC_SILU)
        op_smexp = Signal()
        m.d.comb += op_smexp.eq(op == VecOp.VEC_SMEXP)
        is_add_op = Signal()
        m.d.comb += is_add_op.eq(op_add | op_sub)
        is_mul_op = Signal()
        m.d.comb += is_mul_op.eq(op_mul | op_scale)
        binary_b = Signal()
        m.d.comb += binary_b.eq(op_add | op_sub | op_mul)
        is_unary = Signal()
        m.d.comb += is_unary.eq(op_exp2 | op_recip | op_rsqrt | op_silu
                                | op_smexp)

        # VEC_SMEXP phase membership (driven from the smexp FSM below,
        # like the quant sequencer's phase signals): pass 1 folds the
        # maximum over the A region, pass 2 re-reads it and streams
        # the shifted exp2 through the unary contexts. The context
        # machinery runs only in pass 2.
        sm_st_max = Signal()
        sm_st_exp = Signal()
        sm_m = Signal(32)  # the folded maximum (raw word)
        sm_any = Signal()  # a non-NaN element has been folded in
        # isum_4 accumulate index and per-partial in-flight bits,
        # referenced by the retire gate below and driven by the
        # accumulate datapath.
        sm_iss = Signal(self.lg_act_k + 2)
        sm_pbusy = Array(Signal(name=f'sm_pb{i}') for i in range(4))
        # The retire gate and the accumulate datapath both read the
        # in-flight bit of the partial the next accumulate targets.
        # Materialize the nested Array read once (the cls_x pattern):
        # every reference to a plain Python expression re-elaborates
        # the full tree, and the elaborated netlist duplicates it at
        # each fan-out instead of sharing -- the retire path's tree
        # fans out wide enough that the duplication is measurable
        # silicon (the 2026-11-04 board image wedged on it; see the
        # results README).
        sm_pb_sel = Signal()
        m.d.comb += sm_pb_sel.eq(sm_pbusy[sm_iss[:2]])

        #
        # Read side: per-region byte windows refilled from one
        # outstanding TMEM read, alternating between the windows.
        #
        awin = Signal(self.data_width)
        ahave = Signal(range(bb + 1))
        abeat = Signal(range(max_beats + 1))
        bwin = Signal(self.data_width)
        bhave = Signal(range(bb + 1))
        bbeat = Signal(range(max_beats + 1))
        bscalar = Signal(32)
        bscal_ok = Signal()
        a_turn = Signal()
        rd_pend = Signal()
        rd_b = Signal()

        # B is read only by the ops that use it (binary elementwise
        # and VEC_SCALE's scalar beat); reductions and the unary
        # transcendentals never request it.
        reads_b = binary_b | op_scale
        a_need = (ahave == 0) & (abeat < ibeats)
        b_need = reads_b & (bhave == 0) \
            & (bbeat < Mux(op_scale, 1, ibeats))
        rd_to_b = b_need & (~a_need | a_turn)

        m.d.comb += [
            self.tmem_rreq.valid.eq(running & (a_need | b_need) & ~rd_pend),
            self.tmem_rreq.bits.addr.eq(
                Mux(rd_to_b, b_base + (bbeat << lg), a_base + (abeat << lg))),
            self.tmem_rresp.ready.eq(1),
        ]
        with m.If(self.tmem_rreq.fire):
            m.d.sync += [rd_pend.eq(1), rd_b.eq(rd_to_b), a_turn.eq(~rd_to_b)]
        with m.If(rd_pend & self.tmem_rresp.fire):
            m.d.sync += rd_pend.eq(0)
            with m.If(rd_b):
                m.d.sync += [
                    bwin.eq(self.tmem_rresp.bits.data),
                    bhave.eq(bb),
                    bbeat.eq(bbeat + 1)
                ]
                # The scalar stays put for the whole command.
                with m.If(op_scale):
                    m.d.sync += [
                        bscalar.eq(self.tmem_rresp.bits.data[:32]),
                        bscal_ok.eq(1)
                    ]
            with m.Else():
                m.d.sync += [
                    awin.eq(self.tmem_rresp.bits.data),
                    ahave.eq(bb),
                    abeat.eq(abeat + 1)
                ]

        a_word = awin[:32]
        b_word = bwin[:32]
        b_operand = Mux(op_scale, bscalar, b_word)
        elem_a_ok = ahave >= 4
        elem_b_ok = Mux(op_scale, bscal_ok, bhave >= 4)
        elem_ok = elem_a_ok & elem_b_ok

        #
        # Run states shared by both datapaths. ``consumed`` is the
        # command-local element count for stall accounting; ``elements``
        # stays cumulative across commands as an observation counter.
        #
        issued = Signal(self.lg_act_k + 2)
        consumed = Signal(self.lg_act_k + 2)
        wcnt = Signal(range(2 * bw + 1))
        bidx = Signal(range(max_beats + 1))
        wdone = Signal(range(max_beats + 1))
        beat_buf = Signal(self.data_width)
        # VEC_QUANT beat-packer splits: quants words end at q_qwords
        # (the partial quant beat flushes there), total words at
        # q_total, and meta beats base at qnbeats.
        q_qwords = Signal(self.lg_act_k + 2)
        q_total = Signal(self.lg_act_k + 2)
        qnbeats = Signal(range(max_beats + 1))

        #
        # Elementwise datapath. One op per cycle into the adder or the
        # multiplier; results return in order through a FIFO whose free
        # space is reserved before issue (the FPU units cannot be
        # back-pressured). The packer shifts results LSB-first into
        # full beats and flushes a zero-padded final partial beat.
        #
        res_inflight = Signal(range(self.vec_result_q + 1))
        deq_count = Signal(self.lg_act_k + 2)

        # Free FIFO space is reserved before launch; the FPU units
        # cannot be back-pressured. One outstanding entry per launch
        # keeps the count bounded by the depth.
        room = res_q.count + res_inflight < self.vec_result_q
        elem_go = running & elem_ok & (issued < n_l) & room
        elem_add_go = elem_go & is_add_op
        elem_mul_go = elem_go & is_mul_op

        # Result words per beat for the running command: one FP32 word
        # for the elementwise packer, two FP16 halfwords for VEC_F16's
        # packed image (the packer threshold and shift below follow it).
        wwords = Mux(op_f16, 2 * bw, bw)

        flushing = wcnt == wwords
        quant_boundary = in_elem & op_quant & (deq_count == q_qwords) \
            & (wcnt != 0)
        final_partial = (deq_count == Mux(op_quant, q_total, n_l)) \
            & (wcnt != 0)
        take = in_elem & res_q.deq.valid & ~(flushing | quant_boundary)

        with m.If(take):
            m.d.sync += [
                beat_buf.eq(
                    beat_buf
                    |
                    (res_q.deq.bits.val << Mux(op_f16, 16 * wcnt, 32 * wcnt))),
                wcnt.eq(wcnt + 1),
                deq_count.eq(deq_count + 1),
            ]

        #
        # VEC_F16 narrow datapath (E1's native append). The frozen
        # Fp32To16 converter (bit for bit the model's
        # fp32_to_fp16_rne_bits) narrows one element per cycle from
        # the A window; the 16-bit results enter res_q zero-extended
        # and the packer above packs them LSB-first two per word. The
        # converter is a fixed two-cycle pipeline without
        # backpressure, so a res_q credit is reserved at issue exactly
        # as for the FPU units. B is never read (vec_op carries no
        # b_base contract for this op).
        #
        f16n = m.submodules.f16n = Fp32To16()
        f16_go = in_elem & op_f16 & elem_a_ok & (issued < n_l) & room
        m.d.comb += [
            f16n.inp_valid.eq(f16_go),
            f16n.inp.eq(a_word),
        ]

        #
        # VEC_PSWAP datapath (E3-c's RoPE pairing primitive). Pure
        # data movement with adjacent elements exchanged: one element
        # per cycle leaves the A window; the even element of each pair
        # is held one cycle so the odd element leads it into res_q,
        # and a trailing push retires the final held element after the
        # last read. No FPU, exact bit patterns (signs, NaNs
        # included); the credit a read reserves is the credit its
        # push consumes, so a push can never stall the FIFO (the
        # f16-path reservation argument, one cycle earlier).
        #
        pswap_hold = Signal(32)
        pswap_go = in_elem & op_pswap & elem_a_ok & (issued < n_l) & room
        with m.If(pswap_go & ~issued[0]):
            m.d.sync += pswap_hold.eq(a_word)
        pswap_mid_push = pswap_go & (issued != 0)
        pswap_last_push = in_elem & op_pswap & (issued == n_l) \
            & res_inflight.any()
        pswap_push = pswap_mid_push | pswap_last_push
        pswap_word = Mux(pswap_mid_push, Mux(issued[0], a_word, pswap_hold),
                         pswap_hold)

        #
        # Unary transcendental datapath. One serialized dependency
        # micro-sequencer per element: classify (bypassing the frozen
        # special values), then issue each arithmetic request of the
        # op's fixed sequence exactly once and capture it on the
        # owning unit's out.valid. An element is accepted only when
        # the sequencer is idle and a final-result FIFO credit is
        # free; the credit is reserved at accept and released when
        # the final word (or the bypass value) enters res_q, so
        # intermediates never do. One command runs at a time, so the
        # sequencer shares the elementwise add/multiply ports and the
        # FPURec seed block without contention.
        #
        # Interleaved unary-transcendental datapath. Each element runs
        # its fixed dependency sequence in its own context: classify
        # (bypassing the frozen special values), then issue each
        # arithmetic request exactly once and capture it on the owning
        # unit's out.valid. The FPU blocks are shared, so at most one
        # add, one multiply, and one seed issue per cycle across all
        # contexts; a context holds at most one outstanding request
        # (un_wait), so per-unit context tag FIFOs route every
        # out.valid to its owning context. Elements are accepted in
        # order into free contexts (bounded by res_q credits reserved
        # at accept) and retire through an in-order done buffer, so
        # final words enter the packer in element order exactly as the
        # serialized sequencer produced them. The per-element op
        # sequence is unchanged; only its wall-clock interleaving is.
        ctx_n = self.vec_unary_ctx
        ctx_w = max(1, (ctx_n - 1).bit_length())

        un_x = Array(Signal(32, name=f'un_x{i}') for i in range(ctx_n))
        un_acc = Array(Signal(32, name=f'un_acc{i}') for i in range(ctx_n))
        un_tmp = Array(Signal(32, name=f'un_tmp{i}') for i in range(ctx_n))
        un_f = Array(Signal(32, name=f'un_f{i}') for i in range(ctx_n))
        # Classify latches: exp2's floor helper i2f for the fraction
        # subtract and its two's-complement floor i for the final 2^i
        # scaling.
        un_i2f = Array(Signal(32, name=f'un_i2f{i}') for i in range(ctx_n))
        un_ci = Array(Signal(8, name=f'un_ci{i}') for i in range(ctx_n))
        un_busy = Array(Signal(name=f'un_busy{i}') for i in range(ctx_n))
        un_wait = Array(Signal(name=f'un_wait{i}') for i in range(ctx_n))
        un_arm = Array(Signal(name=f'un_arm{i}') for i in range(ctx_n))
        # VEC_SILU only: the exp2 classification of m0 = a*-log2(e)
        # bypassed the Horner evaluation, so the +1 add consumes the
        # bypass value directly instead of the 2^i-scaled polynomial.
        un_bp = Array(Signal(name=f'un_bp{i}') for i in range(ctx_n))
        un_step = Array(Signal(6, name=f'un_step{i}') for i in range(ctx_n))
        done_val = Array(Signal(name=f'un_done{i}') for i in range(ctx_n))
        done_word = Array(
            Signal(32, name=f'un_dword{i}') for i in range(ctx_n))

        # Accept-order ring: contexts were filled in element order, and
        # the drain walks the ring so final words reach res_q in order.
        ord_arr = Array(Signal(ctx_w, name=f'un_ord{i}') for i in range(ctx_n))
        ord_cnt = Signal(range(ctx_n + 1))
        ord_head = Signal(ctx_w)
        ord_tail = Signal(ctx_w)

        # Issue steps grouped by operation and destination. Capture
        # steps are derived from these same groups, one step later.
        rec_xy = (2, 8)
        rec_sub = (4, 10)
        rec_update = (6, 12)
        rs_half = (2, )
        rs_hy = (4, 12)
        rs_ty = (6, 14)
        rs_sub = (8, 16)
        rs_update = (10, 18)
        exp_mul = tuple(range(2, 27, 4))
        exp_add = tuple(range(4, 29, 4))
        # VEC_SILU composes the frozen VEC_EXP2 and VEC_RECIP
        # sequences around them, all rounding points identical to the
        # six-command chain:
        #   0  issue m0 = R(a * -log2 e)      -> un_tmp at 1
        #   2  classify m0 (exp2's table); on bypass latch
        #      un_bp/un_acc = exp2_val and jump to 32; else issue
        #      f = R(m0 - float32(i))          -> un_f at 3
        #   4+4j (j=0..6) Horner p*f           -> un_tmp
        #   6+4j          Horner C(6-j)+t      -> un_acc (last at 30)
        #   32 issue w3 = R(scale(p, i) + 1)   -> un_acc at 33
        #      (the 2^i scaling is the exponent-field add folded into
        #      this operand, exactly exp2's final; un_bp substitutes
        #      the exp2 bypass value for the scaled polynomial)
        #   34 classify w3 (recip's table); on bypass un_f = recip_val
        #      and jump to 50; else advance to 36
        #   36 issue seed_rec(|w3|)            -> un_f at 37
        #   38/44 issue t = R(|w3|*y)          -> un_tmp
        #   40/46 issue t = R(2.0 - t)         -> un_tmp
        #   42/48 issue y = R(y*t)             -> un_f
        #   50 issue R(a * (y ^ sign(w3)))     -> done at 51
        # (w3 = 1 + exp2(...) >= 1 or non-finite, so the sign fold is
        # the identity in domain; it is applied for contract fidelity.)
        silu_mul = (0, ) + tuple(range(4, 29, 4)) + (38, 42, 44, 48, 50)
        silu_add = (2, ) + tuple(range(6, 31, 4)) + (32, 40, 46)
        # VEC_SMEXP composes the frozen VEC_SUB and VEC_EXP2
        # sequences around them (exp2's schedule shifted +2, the
        # SILU precedent), all rounding points identical to the
        # five-op chain:
        #   0  issue m0 = R(a - max)               -> un_tmp at 1
        #   2  classify m0 (exp2's table); on bypass the element
        #      completes with the table value exactly as the chain's
        #      standalone EXP2 would; else (armed) issue
        #      f = R(m0 - float32(i))              -> un_f at 3
        #   4+4j (j=0..6) Horner p*f               -> un_tmp
        #   6+4j          Horner C(6-j)+t          -> un_acc (last at 30)
        #   31 final capture: the 2^i exponent-field add on the last
        #      Horner result (exp2's final), the element word e[i]
        sm_sub = (0, 2)
        sm_mul = tuple(range(4, 29, 4))
        sm_add = tuple(range(6, 31, 4))

        def step_in(steps, s):
            return Cat(s == t for t in steps).any()

        # One shared classification: at most one context classifies
        # per cycle (fixed priority), latching the per-context
        # results. A bypassed element completes through the done
        # buffer directly, exactly as the serialized sequencer's
        # bypass path did. VEC_SILU classifies twice per element --
        # the exp2 table on m0 (step 2, after the m0 product lands)
        # and the recip table on the intermediate w3 (step 34) --
        # so its classify steps are explicit; VEC_SMEXP classifies
        # once, on the shifted operand (step 2, after the a-m
        # subtract lands); the standing ops classify before arming,
        # exactly as before.
        cls_req = [
            un_busy[i] & ~done_val[i]
            & (op_silu & step_in((2, 34), un_step[i])
               | (op_smexp & (un_step[i] == 2))
               | (~op_silu & ~op_smexp & ~un_arm[i])) for i in range(ctx_n)
        ]
        cls_go = Cat(*cls_req).any()
        cls_sel = Signal(ctx_w)
        m.d.comb += cls_sel.eq(ctx_n - 1)
        for i in range(ctx_n - 1, -1, -1):
            with m.If(cls_req[i]):
                m.d.comb += cls_sel.eq(i)
        cls_s = Signal(6)
        m.d.comb += cls_s.eq(un_step[cls_sel])
        # The classify operand: the context's input for the standing
        # ops and SILU's/SMEXP's first (exp2) classify; the
        # intermediate for SILU's second (recip) classify.
        # Materialize the selected operand once before classification fans out.
        cls_x = Signal(32)
        m.d.comb += cls_x.eq(
            Mux(
                op_silu & (cls_s == 34), un_acc[cls_sel],
                Mux((op_silu | op_smexp) & (cls_s == 2), un_tmp[cls_sel],
                    un_x[cls_sel])))

        # Classification of the selected operand: the frozen
        # special-value table of tests/troom/model.py. exp2's clamps
        # are bit comparisons: x >= 128 is exponent field >= 134
        # (field 134 is [128, 256)), x < -126 is |x| > 126 (field >=
        # 134, or field 133 with the 24-bit significand above
        # 126 * 2^17).
        ux_sign = cls_x[31]
        ux_e = cls_x[23:31]
        ux_man_nz = Signal()
        m.d.comb += ux_man_nz.eq(cls_x[:23].any())
        ux_nan = Signal()
        m.d.comb += ux_nan.eq((ux_e == 255) & ux_man_nz)
        ux_inf = Signal()
        m.d.comb += ux_inf.eq((ux_e == 255) & ~ux_man_nz)
        ux_zero = Signal()
        m.d.comb += ux_zero.eq((ux_e == 0) & ~ux_man_nz)
        ux_sig = Cat(cls_x[:23], Const(1, 1))
        ux_ge128 = Signal()
        m.d.comb += ux_ge128.eq(~ux_sign & (ux_e >= 134))
        ux_lt_m126 = Signal()
        m.d.comb += ux_lt_m126.eq(ux_sign
                                  & ((ux_e >= 134)
                                     | ((ux_e == 133)
                                        & (ux_sig > Const(0xfc0000, 24)))))

        recip_bypass = Signal()
        m.d.comb += recip_bypass.eq(ux_nan | (ux_e == 0) | (ux_e >= 253))
        rsqrt_bypass = Signal()
        m.d.comb += rsqrt_bypass.eq(ux_nan | ux_sign | ux_zero | ux_inf \
            | (ux_e == 0))
        exp2_bypass = Signal()
        m.d.comb += exp2_bypass.eq(ux_nan | ux_ge128 | ux_lt_m126)
        # SILU's two classifications select their own tables: the
        # exp2 tests on m0 (step 2) and the recip tests on w3
        # (step 34).
        cls_bypass_sel = Signal()
        m.d.comb += cls_bypass_sel.eq(
            Mux(op_silu & (cls_s == 34), recip_bypass, exp2_bypass))
        un_bypass = Signal()
        m.d.comb += un_bypass.eq(
            Mux(
                op_recip, recip_bypass,
                Mux(op_rsqrt, rsqrt_bypass,
                    Mux(op_silu, cls_bypass_sel, exp2_bypass))))

        QNAN = Const(0x7fc00000, 32)
        PINF = Const(0x7f800000, 32)
        NINF = Const(0xff800000, 32)
        PZERO = Const(0, 32)
        recip_val = Mux(
            ux_nan, QNAN,
            Mux(ux_e == 0, Cat(PINF[:31], ux_sign), Cat(Const(0, 31),
                                                        ux_sign)))
        rsqrt_val = Mux(
            ux_nan, QNAN,
            Mux(ux_sign, Mux(ux_zero, NINF, QNAN), Mux(ux_inf, PZERO, PINF)))
        exp2_val = Mux(ux_nan, QNAN, Mux(ux_ge128, PINF, PZERO))
        cls_bypass_val = Mux(op_silu & (cls_s == 34), recip_val, exp2_val)
        un_bypass_val = Mux(
            op_recip, recip_val,
            Mux(op_rsqrt, rsqrt_val, Mux(op_silu, cls_bypass_val, exp2_val)))

        # exp2 range reduction, the bounded bit helper: in-sequence
        # inputs have field <= 133 (|x| < 128), so floor's magnitude
        # is the 24-bit significand shifted right by 150 - e and
        # float32(i) is exact for |i| <= 127. The helper also covers
        # the |x| < 1 split (floor 0 or -1).
        ux_small = ux_e < 127
        # Right shift by the (17..23) fraction span; the shifted-out
        # fraction lands in the top bits of the complement shift.
        ux_rsh = (Const(150, 9) - ux_e.as_signed())[:5].as_unsigned()
        ux_lsh = (ux_e.as_signed() - 126)[:3].as_unsigned()
        ux_mag = ux_sig >> ux_rsh
        ux_frac_nz = (ux_sig << ux_lsh)[:24].any()
        # The floor as an 8-bit two's-complement integer; every Mux
        # branch is explicitly unsigned(8) so the value never
        # silently widens into signed arithmetic.
        ux_i = Mux(
            ux_small, Mux(ux_sign & ~ux_zero, Const(0xff, 8), Const(0, 8)),
            Mux(ux_sign, (-ux_mag - ux_frac_nz)[:8].as_unsigned(), ux_mag[:8]))
        ux_iz = Mux(ux_i[7], (-ux_i)[:8], ux_i)
        # Leading-bit position of the magnitude (|i| <= 127); the
        # ascending chain lets the highest set bit win.
        ux_k = Signal(3)
        m.d.comb += ux_k.eq(0)
        for b in range(1, 7):
            with m.If(ux_iz[b]):
                m.d.comb += ux_k.eq(b)
        ux_i2f = Mux(
            ux_iz == 0, PZERO,
            Cat((ux_iz << (23 - ux_k).as_unsigned())[:23], (127 + ux_k)[:8],
                ux_sign))

        # The classify outcome: a bypassed element completes through
        # the done buffer directly (the serialized sequencer's bypass
        # path); otherwise the context arms and latches the exp2
        # helpers for its later steps. VEC_SILU never completes at a
        # classify: an m0 bypass skips exp2's Horner (the +1 add
        # consumes the bypass value) and a w3 bypass skips recip's
        # seed/Newton (the final product consumes it); the non-bypass
        # w3 classify just advances to the seed step.
        with m.If(cls_go):
            with m.If(un_bypass):
                with m.If(op_silu):
                    with m.If(cls_s == 2):
                        m.d.sync += [
                            un_bp[cls_sel].eq(1),
                            un_arm[cls_sel].eq(1),
                            un_acc[cls_sel].eq(un_bypass_val),
                            un_step[cls_sel].eq(32),
                        ]
                    with m.Else():
                        m.d.sync += [
                            un_f[cls_sel].eq(un_bypass_val),
                            un_step[cls_sel].eq(50),
                        ]
                with m.Else():
                    m.d.sync += [
                        done_word[cls_sel].eq(un_bypass_val),
                        done_val[cls_sel].eq(1),
                    ]
            with m.Else():
                m.d.sync += [
                    un_arm[cls_sel].eq(1),
                    un_i2f[cls_sel].eq(ux_i2f),
                    un_ci[cls_sel].eq(ux_i),
                ]
                with m.If(op_silu & (cls_s == 34)):
                    m.d.sync += un_step[cls_sel].eq(36)

        # Unit issue requests and arbiters. A context requests the
        # unit its next step needs (the op's fixed step map below),
        # at most one unit per cycle, and only once its previous
        # request has been captured. Fixed-priority selection cannot
        # starve: a winning context cannot request again until its
        # capture lands, so every pending request wins within a few
        # cycles.
        #   Step map: even steps issue, the following odd step awaits.
        #     VEC_RECIP: 0 classify (+seed on frec); 2/8 R(|x|*y);
        #       4/10 R(2.0-t); 6/12 R(y*t); 13 final, sign of x.
        #     VEC_RSQRT: 0 classify (+seed); 2 R(0.5*x) into the h
        #       register (un_f); per Newton iteration 4/12 R(h*y),
        #       6/14 R(t*y), 8/16 R(1.5-t), 10/18 R(y*t); 19 final.
        #       h keeps its own register: the iteration's temporary
        #       would clobber it.
        #     VEC_EXP2: 0 classify (+f = R(x - float32(i))); per
        #       Horner step j (2+4j mul, 4+4j add, j = 0..6, so
        #       coefficient C(6-j)); 29 final, scaled by 2^i.
        def un_add_step(s):
            return (op_exp2 & ((s == 0) | step_in(exp_add, s))) \
                | (op_recip & step_in(rec_sub, s)) \
                | (op_rsqrt & step_in(rs_sub, s)) \
                | (op_silu & step_in(silu_add, s)) \
                | (op_smexp & (step_in(sm_sub, s) | step_in(sm_add, s)))

        def un_mul_step(s):
            return (op_exp2 & step_in(exp_mul, s)) \
                | (op_recip & (step_in(rec_xy, s)
                               | step_in(rec_update, s))) \
                | (op_rsqrt & (step_in(rs_half, s) | step_in(rs_hy, s)
                               | step_in(rs_ty, s)
                               | step_in(rs_update, s))) \
                | (op_silu & step_in(silu_mul, s)) \
                | (op_smexp & step_in(sm_mul, s))

        add_req = [
            un_busy[i] & ~un_wait[i] & ~done_val[i]
            &
            ((un_arm[i] & un_add_step(un_step[i]))
             # VEC_SMEXP's m0 = R(a - max) precedes classification,
             # so it issues an unarmed add at step 0. SILU's
             # unarmed step 0 belongs exclusively to the multiplier.
             | (op_smexp & (un_step[i] == 0))) for i in range(ctx_n)
        ]
        mul_req = [
            un_busy[i] & ~un_wait[i] & ~done_val[i]
            &
            ((un_arm[i] & un_mul_step(un_step[i]))
             # VEC_SILU's m0 = a * -log2(e) precedes any
             # classification (the exp2 table reads m0), so it
             # issues at step 0 unarmed; no other op requests a
             # multiply at step 0.
             | (op_silu & (un_step[i] == 0))) for i in range(ctx_n)
        ]
        rec_req = [
            un_busy[i] & un_arm[i] & ~un_wait[i] & ~done_val[i]
            & (((un_step[i] == 0) & (op_recip | op_rsqrt))
               | (op_silu & (un_step[i] == 36))) for i in range(ctx_n)
        ]

        def pri_sel(reqs):
            sel = Signal(ctx_w)
            m.d.comb += sel.eq(ctx_n - 1)
            for i in range(ctx_n - 1, -1, -1):
                with m.If(reqs[i]):
                    m.d.comb += sel.eq(i)
            return sel, Cat(*reqs).any()

        # Issue groups: with a second adder/multiplier pair the
        # contexts partition by parity (even on the shared units,
        # odd on the second pair), each group arbitrating its own
        # units exactly as the single group did. With one unit per
        # class every context belongs to group 0 and the structure
        # degenerates to the standing machine.
        def pri_sel_idx(idxs, reqs):
            sel = Signal(ctx_w)
            m.d.comb += sel.eq(idxs[-1])
            for j in range(len(idxs) - 1, -1, -1):
                with m.If(reqs[j]):
                    m.d.comb += sel.eq(idxs[j])
            return sel, Cat(*reqs).any()

        add_idxs = [[i for i in range(ctx_n) if i % self.vec_add_units == g]
                    for g in range(self.vec_add_units)]
        mul_idxs = [[i for i in range(ctx_n) if i % self.vec_mul_units == g]
                    for g in range(self.vec_mul_units)]

        sel_add0, un_add_go0 = pri_sel_idx(add_idxs[0],
                                           [add_req[i] for i in add_idxs[0]])
        sel_mul0, un_mul_go0 = pri_sel_idx(mul_idxs[0],
                                           [mul_req[i] for i in mul_idxs[0]])
        sel_add1 = Signal(ctx_w)
        sel_mul1 = Signal(ctx_w)
        un_add_go1 = Signal()
        un_mul_go1 = Signal()
        m.d.comb += [
            un_add_go1.eq(0),
            un_mul_go1.eq(0),
            sel_add1.eq(add_idxs[0][-1]),
            sel_mul1.eq(mul_idxs[0][-1])
        ]
        if fadd1 is not None and add_idxs[1]:
            sel_add1, un_add_go1 = pri_sel_idx(
                add_idxs[1], [add_req[i] for i in add_idxs[1]])
        if fmul1 is not None and mul_idxs[1]:
            sel_mul1, un_mul_go1 = pri_sel_idx(
                mul_idxs[1], [mul_req[i] for i in mul_idxs[1]])
        sel_rec, un_rec_go = pri_sel(rec_req)

        extra_issues = []
        for kind, units, idxs, reqs in (('add', extra_add, add_idxs, add_req),
                                        ('mul', extra_mul, mul_idxs, mul_req)):
            for g, unit in enumerate(units, 2):
                sel, go = pri_sel_idx(idxs[g], [reqs[i] for i in idxs[g]])
                extra_issues.append((kind, g, unit, sel, go))
                with m.If(go):
                    m.d.sync += [
                        un_step[sel].eq(un_step[sel] + 1), un_wait[sel].eq(1)
                    ]

        for go, sel in ((un_add_go0, sel_add0), (un_add_go1, sel_add1),
                        (un_mul_go0, sel_mul0), (un_mul_go1, sel_mul1),
                        (un_rec_go, sel_rec)):
            with m.If(go):
                m.d.sync += [
                    un_step[sel].eq(un_step[sel] + 1), un_wait[sel].eq(1)
                ]

        # Per-unit context tag FIFOs: push the issuing context, pop on
        # the unit's out.valid. The FPU pipelines complete in order,
        # so the popped tag owns the completing result; a context
        # holds at most one outstanding request, so depth ctx_n
        # suffices.
        def tag_fifo(prefix, issue_go, issue_sel, out_valid, group=None):
            # Four-way issue has static ownership: group g only issues
            # contexts i % 4 == g. Store a bank-local tag and restore the
            # constant low bits at capture. Otherwise reset/unfilled tags
            # obscure this invariant from synthesis, leaving each result
            # bus connected to every context instead of its own bank.
            # At most one request per context is outstanding, so the
            # FIFO also only needs as many slots as its bank has contexts.
            bank_bits = 2 if group is not None else 0
            depth = len(range(group, ctx_n, 4)) if group is not None else ctx_n
            tag_w = max(1, ctx_w - bank_bits)
            issue_tag = issue_sel[bank_bits:] if ctx_w > bank_bits else Const(
                0, 1)
            tags = [
                Signal(tag_w, name=f'{prefix}_tag{i}') for i in range(depth)
            ]
            tarr = Array(tags)
            cnt = Signal(range(depth + 1), name=f'{prefix}_tagcnt')
            pop = out_valid & (cnt != 0)
            with m.If(pop):
                for i in range(depth - 1):
                    m.d.sync += tags[i].eq(tags[i + 1])
            with m.If(issue_go):
                with m.If(pop):
                    m.d.sync += tarr[cnt - 1].eq(issue_tag)
                with m.Else():
                    m.d.sync += tarr[cnt].eq(issue_tag)
            m.d.sync += cnt.eq(cnt + Mux(issue_go, 1, 0) - Mux(pop, 1, 0))
            owner = tags[0]
            if group is not None:
                owner = Cat(Const(group, bank_bits),
                            tags[0][:ctx_w - bank_bits])
            return owner, cnt

        add_owner, add_tcnt = tag_fifo(
            'unfadd',
            un_add_go0,
            sel_add0,
            fadd.out.valid,
            group=0 if self.vec_add_units == 4 else None)
        mul_owner, mul_tcnt = tag_fifo(
            'unfmul',
            un_mul_go0,
            sel_mul0,
            fmul.out.valid,
            group=0 if self.vec_mul_units == 4 else None)
        rec_owner, rec_tcnt = tag_fifo('unfrec', un_rec_go, sel_rec,
                                       frec.out.valid)

        # Capture decode for the popped owner: un_step[owner] names
        # the await step (issue step + 1), which identifies the
        # destination exactly as in the serialized sequencer: exp2's
        # Horner product p*f lands in un_tmp and the add's result
        # (the new p) in un_acc. The decode is identical for every
        # unit of a class (the operand order is the context's, not
        # the unit's), so one function serves both issue groups.
        def mul_capture(unit, owner, tcnt):
            mul_cap = unit.out.valid & (tcnt != 0)
            s_mul = un_step[owner]
            cap_mul_t = (op_recip & step_in((3, 9), s_mul)) \
                | (op_rsqrt & step_in((5, 13, 7, 15), s_mul)) \
                | (op_exp2 & step_in((3, 7, 11, 15, 19, 23, 27), s_mul)) \
                | (op_silu & step_in((1, 5, 9, 13, 17, 21, 25, 29,
                                      39, 45), s_mul)) \
                | (op_smexp & step_in((5, 9, 13, 17, 21, 25, 29), s_mul))
            cap_mul_h = op_rsqrt & (s_mul == 3)
            cap_mul_a = (op_recip & (s_mul == 7)) \
                | (op_rsqrt & (s_mul == 11))
            # SILU's Newton quotients land in the seed register.
            cap_mul_f = op_silu & step_in((43, 49), s_mul)
            fin_mul = (op_recip & (s_mul == 13)) \
                | (op_rsqrt & (s_mul == 19)) \
                | (op_silu & (s_mul == 51))

            with m.If(mul_cap):
                with m.If(fin_mul):
                    # Final word: recip applies the sign of x; rsqrt's
                    # last multiply is the result; SILU's last product
                    # a*sigmoid already carries the (identity) sign
                    # fold on its operand.
                    m.d.sync += [
                        done_word[owner].eq(
                            Mux(
                                op_recip, unit.out.bits.data
                                ^ Cat(Const(0, 31), un_x[owner][31]),
                                unit.out.bits.data)),
                        done_val[owner].eq(1),
                        un_wait[owner].eq(0),
                    ]
                with m.Else():
                    with m.If(cap_mul_t):
                        m.d.sync += un_tmp[owner].eq(unit.out.bits.data)
                    with m.If(cap_mul_h):
                        m.d.sync += un_f[owner].eq(unit.out.bits.data)
                    with m.If(cap_mul_a):
                        m.d.sync += un_acc[owner].eq(unit.out.bits.data)
                    with m.If(cap_mul_f):
                        m.d.sync += un_f[owner].eq(unit.out.bits.data)
                    m.d.sync += [
                        un_step[owner].eq(s_mul + 1), un_wait[owner].eq(0)
                    ]

        def add_capture(unit, owner, tcnt):
            add_cap = unit.out.valid & (tcnt != 0)
            s_add = un_step[owner]
            cap_add_t = (op_recip & step_in((5, 11), s_add)) \
                | (op_rsqrt & step_in((9, 17), s_add)) \
                | (op_silu & step_in((41, 47), s_add)) \
                | (op_smexp & (s_add == 1))
            cap_add_f = (op_exp2 & (s_add == 1)) | (op_silu & (s_add == 3)) \
                | (op_smexp & (s_add == 3))
            cap_add_a = (op_exp2 & step_in((5, 9, 13, 17, 21, 25), s_add)) \
                | (op_silu & step_in((7, 11, 15, 19, 23, 27, 31, 33),
                                     s_add)) \
                | (op_smexp & step_in((7, 11, 15, 19, 23, 27), s_add))
            fin_add = (op_exp2 & (s_add == 29)) \
                | (op_smexp & (s_add == 31))

            with m.If(add_cap):
                with m.If(fin_add):
                    # exp2 final scaling: add the latched floor i to the
                    # polynomial result's exponent field (p is in [1, 2],
                    # so its field is 127 or 128; the 9-bit sum spans
                    # 0..255 over the in-domain i).
                    exp2_pe = Cat(unit.out.bits.data[23:31],
                                  Const(0, 1)).as_signed()
                    exp2_sum = exp2_pe + un_ci[owner].as_signed()
                    m.d.sync += [
                        done_word[owner].eq(
                            Mux(
                                exp2_sum >= 255, PINF,
                                Mux(exp2_sum <= 0, PZERO,
                                    (unit.out.bits.data & 0x807fffff)
                                    | ((exp2_sum[:8]) << 23)))),
                        done_val[owner].eq(1),
                        un_wait[owner].eq(0),
                    ]
                with m.Else():
                    with m.If(cap_add_t):
                        m.d.sync += un_tmp[owner].eq(unit.out.bits.data)
                    with m.If(cap_add_f):
                        m.d.sync += un_f[owner].eq(unit.out.bits.data)
                    with m.If(cap_add_a):
                        m.d.sync += un_acc[owner].eq(unit.out.bits.data)
                    m.d.sync += [
                        un_step[owner].eq(s_add + 1), un_wait[owner].eq(0)
                    ]

        mul_capture(fmul, mul_owner, mul_tcnt)
        add_capture(fadd, add_owner, add_tcnt)
        if fmul1 is not None:
            mul_owner1, mul_tcnt1 = tag_fifo(
                'unfmul1',
                un_mul_go1,
                sel_mul1,
                fmul1.out.valid,
                group=1 if self.vec_mul_units == 4 else None)
            mul_capture(fmul1, mul_owner1, mul_tcnt1)
        if fadd1 is not None:
            add_owner1, add_tcnt1 = tag_fifo(
                'unfadd1',
                un_add_go1,
                sel_add1,
                fadd1.out.valid,
                group=1 if self.vec_add_units == 4 else None)
            add_capture(fadd1, add_owner1, add_tcnt1)

        for kind, g, unit, sel, go in extra_issues:
            owner, tcnt = tag_fifo(f'unf{kind}{g}',
                                   go,
                                   sel,
                                   unit.out.valid,
                                   group=g)
            if kind == 'add':
                add_capture(unit, owner, tcnt)
            else:
                mul_capture(unit, owner, tcnt)

        with m.If(frec.out.valid & (rec_tcnt != 0)):
            # RECIP/RSQRT start the seed at y in un_acc; SILU keeps
            # w3 (the recip argument) in un_acc for the Newton
            # products, so its seed lands in un_f.
            with m.If(op_silu):
                m.d.sync += un_f[rec_owner].eq(frec.out.bits.data)
            with m.Else():
                m.d.sync += un_acc[rec_owner].eq(frec.out.bits.data)
            m.d.sync += [
                un_step[rec_owner].eq(un_step[rec_owner] + 1),
                un_wait[rec_owner].eq(0)
            ]

        # Element accept: latch the operand into the lowest free
        # context and record the element in the retire ring. One
        # accept per cycle keeps the A-window shift simple; the FPU
        # issue floor is the throughput limiter, not the accept rate.
        # VEC_SMEXP accepts only in pass 2 (its pass 1 consumes the
        # region folding the maximum the pass-2 subtract reads).
        sel_free, have_free = pri_sel([~un_busy[i] for i in range(ctx_n)])
        un_go = in_elem & is_unary & elem_a_ok & (issued < n_l) & room \
            & have_free & (ord_cnt < ctx_n) & (~op_smexp | sm_st_exp)
        with m.If(un_go):
            m.d.sync += [
                un_busy[sel_free].eq(1),
                un_wait[sel_free].eq(0),
                un_arm[sel_free].eq(0),
                un_bp[sel_free].eq(0),
                un_step[sel_free].eq(0),
                un_x[sel_free].eq(a_word),
                ord_arr[ord_tail].eq(sel_free),
                ord_tail.eq(Mux(ord_tail == ctx_n - 1, 0, ord_tail + 1)),
            ]

        # In-order retire: the ring head's done word enters res_q
        # under the credit reserved at accept; the context and ring
        # slot free only when the word commits. The ring count moves
        # in one statement so an accept and a retire in the same
        # cycle compose. VEC_SMEXP's retire also waits for its isum_4
        # partial to be free -- an accumulate may not issue into a
        # partial whose previous accumulate has not captured (the
        # three-cycle pipeline vs the four-element isum_4 spacing), so
        # the in-order retire briefly waits and done words park in
        # their contexts exactly as under result-FIFO backpressure.
        # The retire handshake below fans out into the ring, the
        # packer and the SMEXP accumulate datapath, so -- like cls_x
        # -- every tree is materialized once here; plain Python
        # expressions are re-elaborated at each reference and the
        # nested Array reads (done_val[ord_arr[ord_head]],
        # sm_pbusy[sm_iss[:2]]) otherwise duplicate their full mux
        # trees per fan-out, which tripled the vector cell's netlist
        # and broke the physical image.
        ord_head_ctx = Signal(ctx_w)
        m.d.comb += ord_head_ctx.eq(ord_arr[ord_head])
        un_drain_go = Signal()
        m.d.comb += un_drain_go.eq(in_elem & is_unary & done_val[ord_head_ctx]
                                   & (~op_smexp | ~sm_pb_sel))
        un_drain_word = Signal(32)
        m.d.comb += un_drain_word.eq(done_word[ord_head_ctx])
        un_retire = Signal()
        m.d.comb += un_retire.eq(un_drain_go & res_q.enq.fire)
        m.d.sync += ord_cnt.eq(ord_cnt + Mux(un_go, 1, 0) -
                               Mux(un_retire, 1, 0))
        with m.If(un_retire):
            m.d.sync += [
                done_val[ord_head_ctx].eq(0),
                un_busy[ord_head_ctx].eq(0),
                ord_head.eq(Mux(ord_head == ctx_n - 1, 0, ord_head + 1)),
            ]

        #
        # VEC_SMEXP datapath. Pass 1 folds the region maximum at one
        # element per cycle: FP32 total order as a bit key (negative
        # patterns complemented, the sign bit otherwise forced set),
        # NaNs excluded -- bit-exact with the RISC-V fmax fold
        # (order-free, +0 beating -0, canonical NaN when nothing
        # survives) for every region. Pass 2's elementwise image and
        # isum_4 denominator ride the unary contexts and the retire
        # path above; the accumulations issue on the dedicated fsum
        # adder at retire, in element order, one per retiring word
        # (same-partial dependencies are four elements apart, so the
        # three-cycle latency never stalls the pipelined unit). Three
        # dependent fold adds then produce R(R(p0+p2)+R(p1+p3)) --
        # RDC_SUM's exact fold -- and the result beat lands at b_base.
        #
        sm_max_go = in_elem & op_smexp & sm_st_max & elem_a_ok \
            & (consumed < n_l)
        sm_nan = (a_word[23:31] == 255) & a_word[:23].any()
        sm_key_a = Mux(a_word[31], ~a_word, Cat(a_word[:31], Const(1, 1)))
        sm_key_m = Mux(sm_m[31], ~sm_m, Cat(sm_m[:31], Const(1, 1)))
        sm_wins = ~sm_nan & (~sm_any | (sm_key_a > sm_key_m))
        with m.If(sm_max_go & sm_wins):
            m.d.sync += [sm_m.eq(a_word), sm_any.eq(1)]

        # Denominator partials, in-flight tracking, and the fold
        # ladder (the reduction datapath's accumulate/fold pattern on
        # the dedicated unit).
        sm_p = [Signal(32, name=f'sm_p{i}') for i in range(4)]
        sm_p_arr = Array(sm_p)
        # Materialized once: the accumulate operand and the retire-gate
        # read share this nested Array read (the cls_x/un_retire
        # pattern; every Python reference re-elaborates the tree).
        sm_p_sel = Signal(32)
        m.d.comb += sm_p_sel.eq(sm_p_arr[sm_iss[:2]])
        sm_pend = Signal(range(6))
        sm_fold_v = [Signal(32, name=f'sm_fold_v{i}') for i in range(3)]
        sm_fold_arr = Array(sm_fold_v)
        sm_fold_iss = Signal(range(4))
        sm_fold_ret = Signal(range(4))
        sm_sum_wr = Signal()
        # The denominator beat's write request, driven by the phase
        # sequencer's SWRITE state and muxed onto the write port
        # behind the elementwise packer.
        sm_sum_we = Signal()

        # Both the capture one-hot below and the fold reads index by
        # the oldest in-flight partial; materialized for the same
        # sharing reason as sm_p_sel.
        sm_pidx = Signal(2)
        m.d.comb += sm_pidx.eq((sm_iss - sm_pend)[:2])
        sm_acc_go = Signal()
        sm_cap = Signal()
        sm_fold_run = Signal()
        m.d.comb += [
            sm_acc_go.eq(in_elem & op_smexp & sm_st_exp & un_retire),
            sm_cap.eq(fsum.out.valid & (sm_pend != 0)),
            sm_fold_run.eq(in_elem & op_smexp & sm_st_exp),
        ]
        sm_fold1_go = sm_fold_run & (sm_iss == n_l) & (sm_pend == 0) \
            & (sm_fold_iss == 0)
        sm_fold2_go = sm_fold_run & (sm_fold_iss == 1)
        sm_foldff_go = sm_fold_run & (sm_fold_iss == 2) \
            & (sm_fold_ret == 2)

        m.d.comb += [
            fsum.inp.valid.eq(sm_acc_go | sm_fold1_go | sm_fold2_go
                              | sm_foldff_go),
            fsum.inp.bits.fn.eq(FPUOperator.ADD),
            fsum.inp.bits.rm.eq(RoundingMode.RNE),
            fsum.inp.bits.fn_mod.eq(0),
            fsum.inp.bits.in2.eq(0),
            fsum.inp.bits.in1.eq(
                Mux(
                    sm_acc_go, sm_p_sel,
                    Mux(sm_fold_iss == 0, sm_p[0],
                        Mux(sm_fold_iss == 1, sm_p[1], sm_fold_v[0])))),
            fsum.inp.bits.in3.eq(
                Mux(
                    sm_acc_go, un_drain_word,
                    Mux(sm_fold_iss == 0, sm_p[2],
                        Mux(sm_fold_iss == 1, sm_p[3], sm_fold_v[1])))),
        ]
        with m.If(fsum.out.valid):
            with m.If(sm_pend != 0):
                # An accumulate completion retires the oldest
                # in-flight partial (fsum completes in order).
                for i in range(4):
                    with m.If(sm_pidx == i):
                        m.d.sync += [
                            sm_p[i].eq(fsum.out.bits.data), sm_pbusy[i].eq(0)
                        ]
            with m.Else():
                m.d.sync += [
                    sm_fold_arr[sm_fold_ret].eq(fsum.out.bits.data),
                    sm_fold_ret.eq(sm_fold_ret + 1),
                ]
        with m.If(sm_acc_go):
            m.d.sync += sm_iss.eq(sm_iss + 1)
            for i in range(4):
                with m.If(sm_iss[:2] == i):
                    m.d.sync += sm_pbusy[i].eq(1)
        # One statement: an issue and a capture can share a cycle (a
        # later element retiring while an earlier partial lands), and
        # separate statements would let the increment overwrite the
        # decrement -- the fold would then never see pend == 0.
        m.d.sync += sm_pend.eq(sm_pend + Mux(sm_acc_go, 1, 0) -
                               Mux(sm_cap, 1, 0))
        with m.If(sm_fold1_go | sm_fold2_go | sm_foldff_go):
            m.d.sync += sm_fold_iss.eq(sm_fold_iss + 1)

        # The phase sequencer rests between commands and starts with
        # the elementwise run state of an accepted descriptor.
        with m.FSM(name='smexp') as sfsm:
            with m.State('SIDLE'):
                with m.If(in_elem & op_smexp & ~sm_sum_wr):
                    m.next = 'SMAX'
            with m.State('SMAX'):
                m.d.comb += sm_st_max.eq(1)
                with m.If(consumed == n_l):
                    # No TMEM read can be outstanding once the last
                    # element consumed (its beat's response landed
                    # before the word could shift out), so the cursor
                    # rewind is safe. An all-NaN (or empty-survivor)
                    # region folds the canonical NaN, the serial
                    # fmax fold's result over such regions.
                    m.d.sync += [
                        sm_m.eq(Mux(sm_any, sm_m, QNAN)),
                        abeat.eq(0),
                        ahave.eq(0),
                        awin.eq(0),
                        consumed.eq(0),
                        issued.eq(0),
                    ]
                    m.next = 'SEXP'
            with m.State('SEXP'):
                m.d.comb += sm_st_exp.eq(1)
                with m.If(sm_fold_ret == 3):
                    m.next = 'SWRITE'
            with m.State('SWRITE'):
                # The denominator beat: only after every elementwise
                # beat has committed (the folds can complete while
                # the packer still drains res_q), so it never
                # interleaves the E image.
                m.d.comb += sm_sum_we.eq(in_elem & (wdone == nbeats)
                                         & ~(flushing | final_partial
                                             | quant_boundary))
                with m.If(self.tmem_w.fire & sm_sum_we):
                    m.d.sync += sm_sum_wr.eq(1)
                    m.next = 'SIDLE'

        # Unary FPU operand builders. Each unit works from one
        # per-context view (a single indexed read per register -- the
        # generated Verilog duplicates an inline Array read for every
        # reference, which multiplies the context muxes), with the
        # same step decode for both issue groups (the operand order
        # is the context's, not the unit's).
        class UniView:
            pass

        def unary_view(sel):
            v = UniView()
            v.s = Signal(6, name='un_views')
            v.x = Signal(32, name='un_viewx')
            v.acc = Signal(32, name='un_viewacc')
            v.tmp = Signal(32, name='un_viewtmp')
            v.f = Signal(32, name='un_viewf')
            v.i2f = Signal(32, name='un_viewi2f')
            v.ci = Signal(8, name='un_viewci')
            v.bp = Signal(name='un_viewbp')
            m.d.comb += [
                v.s.eq(un_step[sel]),
                v.x.eq(un_x[sel]),
                v.acc.eq(un_acc[sel]),
                v.tmp.eq(un_tmp[sel]),
                v.f.eq(un_f[sel]),
                v.i2f.eq(un_i2f[sel]),
                v.ci.eq(un_ci[sel]),
                v.bp.eq(un_bp[sel]),
            ]
            return v

        def unary_add_fn_mod(v):
            s = v.s
            return (op_recip | op_rsqrt) | (op_exp2 & (s == 0)) \
                | (op_silu & step_in((2, 40, 46), s)) \
                | (op_smexp & step_in(sm_sub, s))

        def unary_mul_in1(v):
            s = v.s
            return Mux(
                op_silu,
                Mux((s == 0) | (s == 50), v.x,
                    Mux(
                        step_in((38, 44), s), v.acc & 0x7fffffff,
                        Mux(step_in((42, 48), s), v.f,
                            Mux(s == 4, EXP2_C[7], v.acc)))),
                Mux(
                    op_smexp, Mux(s == sm_mul[0], EXP2_C[7], v.acc),
                    Mux(
                        op_exp2, Mux(s == exp_mul[0], EXP2_C[7], v.acc),
                        Mux(
                            op_recip & step_in(rec_xy, s), v.x & 0x7fffffff,
                            Mux(
                                op_rsqrt & (s == rs_half[0]),
                                Const(0x3f000000, 32),
                                Mux(
                                    op_rsqrt & step_in(rs_ty, s), v.tmp,
                                    Mux(op_rsqrt & step_in(rs_hy, s), v.f,
                                        v.acc)))))))

        def unary_mul_in2(v):
            s = v.s
            return Mux(
                op_silu,
                Mux(
                    s == 0, Const(SILU_NLOG2E_BITS, 32),
                    Mux(
                        s == 50, v.f ^ (v.acc & 0x80000000),
                        Mux(step_in((38, 44), s), v.f,
                            Mux(step_in((42, 48), s), v.tmp, v.f)))),
                Mux(
                    op_smexp, v.f,
                    Mux(
                        op_exp2, v.f,
                        Mux(
                            op_recip & step_in(rec_xy, s), v.acc,
                            Mux(
                                op_rsqrt & (s == rs_half[0]), v.x,
                                Mux(op_rsqrt & step_in(rs_hy + rs_ty, s),
                                    v.acc, v.tmp))))))

        def silu_w3_operand(v):
            # The +1 add's operand: the Horner result scaled by 2^i
            # (exp2's final exponent-field add, folded into this
            # operand), or the exp2 bypass value itself when the m0
            # classification bypassed the evaluation.
            p = v.acc
            pe = Cat(p[23:31], Const(0, 1)).as_signed()
            ssum = pe + v.ci.as_signed()
            scaled = Mux(
                ssum >= 255, PINF,
                Mux(ssum <= 0, PZERO, (p & 0x807fffff) | ((ssum[:8]) << 23)))
            return Mux(v.bp, p, scaled)

        def unary_add_in1(v):
            s = v.s
            return Mux(
                op_silu,
                Mux(
                    s == 2, v.tmp,
                    Mux(
                        s == 32, silu_w3_operand(v),
                        Mux(step_in((40, 46), s), Const(0x40000000, 32),
                            v.tmp))),
                Mux(
                    op_smexp,
                    # step 0 subtracts the folded maximum from the
                    # element; steps 2..30 are exp2's range subtract
                    # and Horner adds, all reading un_tmp.
                    Mux(s == 0, v.x, v.tmp),
                    Mux(
                        op_exp2 & (s == 0), v.x,
                        Mux(
                            op_exp2, v.tmp,
                            Mux(op_recip, Const(0x40000000, 32),
                                Const(0x3fc00000, 32))))))

        def unary_add_in3(v):
            s = v.s
            return Mux(
                op_silu,
                Mux(
                    s == 2, v.i2f,
                    Mux(
                        s == 32, Const(SILU_ONE_BITS, 32),
                        Mux(step_in((40, 46), s), v.tmp,
                            EXP2_C[((30 - s) >> 2)[:3]]))),
                Mux(
                    op_smexp,
                    Mux(s == 0, sm_m,
                        Mux(s == 2, v.i2f, EXP2_C[((30 - s) >> 2)[:3]])),
                    Mux(
                        op_exp2 & (s == 0), v.i2f,
                        Mux(op_exp2, EXP2_C[((exp_add[-1] - s) >> 2)[:3]],
                            v.tmp))))

        for kind, g, unit, sel, go in extra_issues:
            view = unary_view(sel)
            m.d.comb += [
                unit.inp.valid.eq(go),
                unit.inp.bits.rm.eq(RoundingMode.RNE)
            ]
            if kind == 'add':
                m.d.comb += [
                    unit.inp.bits.fn.eq(FPUOperator.ADD),
                    unit.inp.bits.fn_mod.eq(unary_add_fn_mod(view)),
                    unit.inp.bits.in1.eq(unary_add_in1(view)),
                    unit.inp.bits.in2.eq(0),
                    unit.inp.bits.in3.eq(unary_add_in3(view))
                ]
            else:
                m.d.comb += [
                    unit.inp.bits.fn.eq(FPUOperator.MUL),
                    unit.inp.bits.in1.eq(unary_mul_in1(view)),
                    unit.inp.bits.in2.eq(unary_mul_in2(view)),
                    unit.inp.bits.in3.eq(0)
                ]

        view_add0 = unary_view(sel_add0)
        view_mul0 = unary_view(sel_mul0)
        view_add1 = unary_view(sel_add1)
        view_mul1 = unary_view(sel_mul1)

        #
        # Activation quantization datapath (VEC_QUANT). One
        # segment context: fold amax over the 32 buffered
        # words (fmax of |x| from +0), derive sc and the unscaled
        # r ~ 1/amax through the shared multiplier and the FPURec
        # seed (the frozen recip sequence), then quantize the buffer
        # with the two-step product t = R(x*r), p = R(t*127) and the
        # bounded RNE int8 helper. Packed quant words and the meta
        # pairs flow through res_q like any elementwise result; a
        # credit is reserved per four-element group (and per drained
        # meta word) before the muls that feed it can be
        # backpressured.
        #
        # Reuse the multiplier groups as separately rounded stages.
        # SCALE retains its frozen shared-FPU/Newton sequence.
        q_pipeline = self.vec_quant_pipe and fmul1 is not None
        q_step = Signal(5)
        q_wait = Signal()
        q_valid = Signal(6)  # words of this segment read from A
        q_idx = Signal(6)  # amax/quant index within the segment
        q_ccnt = Signal(6)  # captured bytes this segment
        q_bcnt = Signal(2)
        q_bytes = Signal(32)
        q_sum = Signal(16)
        q_mstep = Signal(2)
        q_seg = Signal(7)  # segment index (meta buffer address)
        q_didx = Signal(self.lg_act_k + 2)  # meta drain index
        q_pe = Signal()  # element product step: 0 x*r, 1 *127
        q_pewait = Signal()
        q_pipe_valid = Signal()
        q_pipe_data = Signal(32)
        q_arm = Signal()  # a fresh command may start the sequencer
        qbuf = Array(Signal(32, name=f'qbuf{i}') for i in range(32))
        q_amax = Signal(32)
        q_maxbusy = Signal()
        q_y = Signal(32)  # the seed / Newton r (~1/amax)
        q_t = Signal(32)
        q_sc = Signal(32)
        # Per-segment (s_act, sum) pairs buffer: the output image is
        # dense quants then dense meta, so the pairs collected along
        # the way drain through res_q after the last segment.
        qmeta_mem = Memory(width=32, depth=2 * self.act_segs)
        m.submodules['qm_w'] = qm_w = qmeta_mem.write_port()
        m.submodules['qm_r'] = qm_r = qmeta_mem.read_port(domain='comb')

        # Phase membership, driven inside the sequencer's FSM below
        # like the run control's in_elem/in_rdc.
        q_st_amax = Signal()
        q_st_scale = Signal()
        q_st_quant = Signal()
        q_st_meta = Signal()
        q_st_drain = Signal()

        q_max_go = in_elem & op_quant & q_st_amax & elem_a_ok \
            & (q_idx < q_valid) & (Const(1) if q_pipeline else ~q_maxbusy)
        q_maxcap = in_elem & op_quant & q_st_amax & fcomp.out.valid

        # Scale classification of q_amax: zero or subnormal (every
        # |x| < 2^-126, or an all-NaN fold) zeroes the segment;
        # >= 2^126 keeps sc but forces inv = +0.
        qae = q_amax[23:31]
        q_zero = qae == 0
        q_big = qae >= 253
        q_cls = q_st_scale & ~q_wait & (q_step == 0)
        q_seed_go = q_cls & ~q_zero & ~q_big
        q_issue = q_st_scale & ~q_wait & (q_step != 0)
        q_it_go = q_issue & ((q_step == 2) | (q_step == 8))
        q_sub_go = q_issue & ((q_step == 4) | (q_step == 10))
        q_upd_go = q_issue & ((q_step == 6) | (q_step == 12))
        q_sc_go = q_issue & (q_step == 14)
        q_it_cap = q_wait & ((q_step == 3) | (q_step == 9))
        q_sub_cap = q_wait & ((q_step == 5) | (q_step == 11))
        q_upd_cap = q_wait & ((q_step == 7) | (q_step == 13))
        q_sc_cap = q_wait & (q_step == 15)

        # Element products run as a two-step dependency (t = R(x*r),
        # then p = R(t*127)): the unscaled multiplier r ~ 1/amax
        # never lets an intermediate exceed ~1, so the smallest
        # normal amax cannot overflow the way 127*recip(amax) would.
        q_grp_new = q_idx[:2] == 0
        if q_pipeline:
            q_m1_go = in_elem & op_quant & q_st_quant & (q_idx < 32) \
                & (q_grp_new & room | ~q_grp_new)
            q_cap1 = in_elem & op_quant & q_st_quant & fmul.out.valid
            q_m2_go = in_elem & op_quant & q_st_quant & q_pipe_valid
            q_cap2 = in_elem & op_quant & q_st_quant & fmul1.out.valid
            q_product = fmul1.out.bits.data
            m.d.sync += q_pipe_valid.eq(q_cap1)
            with m.If(q_cap1):
                m.d.sync += q_pipe_data.eq(fmul.out.bits.data)
        else:
            q_m1_go = in_elem & op_quant & q_st_quant & (q_idx < 32) \
                & ~q_pe & ~q_pewait & (q_grp_new & room | ~q_grp_new)
            q_m2_go = in_elem & op_quant & q_st_quant & q_pe & ~q_pewait
            q_cap1 = in_elem & op_quant & q_st_quant & ~q_pe & q_pewait \
                & fmul.out.valid
            q_cap2 = in_elem & op_quant & q_st_quant & q_pe & q_pewait \
                & fmul.out.valid
            q_product = fmul.out.bits.data
        q_word_done = q_cap2 & (q_bcnt == 3)
        q_drain_go = in_elem & op_quant & q_st_drain & room \
            & (q_didx < (q_total - q_qwords).as_unsigned())

        # Bounded RNE int8 rounding of the product, a bit helper in
        # the exp2 floor's style: |p| < 0.5 -> 0, |p| >= 128 or
        # exceptional -> saturate, exact halves to even.
        qp_e = q_product[23:31]
        qp_man = Cat(q_product[:23], Const(1, 1))
        qp_sh = (Const(150, 9) - qp_e.as_signed())[:5].as_unsigned()
        qp_k = (qp_sh - Const(1, 5)).as_unsigned()
        qp_ip = (qp_man >> qp_sh)[:8]
        qp_guard = (qp_man >> qp_k)[0]
        # Sticky: the significand bits below the guard, shifted up
        # and truncated (the exp2 helper's left-shift pattern).
        qp_sticky = (qp_man << (Const(24, 6) - qp_k).as_unsigned())[:24].any()
        qp_rne = qp_ip[:8] + Mux(qp_guard & (qp_sticky | qp_ip[0]), Const(
            1, 8), Const(0, 8))
        qp_val = Mux(qp_e < 126, Const(0, 8),
                     Mux(qp_rne > 127, Const(127, 8), qp_rne[:8]))
        qp_sat = qp_e >= 134
        q_byte = Mux(q_product[31],
                     Mux(qp_sat, Const(0x80, 8), (-qp_val)[:8].as_unsigned()),
                     Mux(qp_sat, Const(0x7f, 8), qp_val))
        q_next_bytes = (q_bytes >> 8) | (q_byte << 24)
        q_enq_valid = q_word_done | q_drain_go
        q_enq_word = Mux(q_word_done, q_next_bytes, qm_r.data)

        m.d.comb += [
            qm_w.addr.eq(Cat(q_mstep[0], q_seg)),
            qm_w.data.eq(Mux(q_mstep[0], Cat(q_sum[:16], Const(0, 16)), q_sc)),
            qm_w.en.eq(in_elem & op_quant & q_st_meta
                       & (q_mstep < 2)),
            qm_r.addr.eq(q_didx),
        ]

        # Segment launch latches: the valid word count, the amax
        # accumulator, and the pad slots (they quantize to byte 0
        # through the same multiplier).
        q_left = (n_l - consumed).as_unsigned()
        q_nvalid = Mux(q_left > 32, Const(32, 6), q_left[:6])

        def q_start_segment():
            for i in range(32):
                with m.If(~(Const(i, 6) < q_nvalid)):
                    m.d.sync += qbuf[i].eq(0)
            m.d.sync += [
                q_valid.eq(q_nvalid),
                q_idx.eq(0),
                q_ccnt.eq(0),
                q_bcnt.eq(0),
                q_sum.eq(0),
                q_amax.eq(0),
                q_maxbusy.eq(0),
            ]

        with m.If(q_max_go):
            m.d.sync += [
                qbuf[q_idx].eq(a_word),
                q_idx.eq(q_idx + 1),
            ]
            if q_pipeline:
                # Nonnegative IEEE magnitudes sort by unsigned bits.
                # fmax from +0 ignores NaNs and retains subnormals/inf.
                q_abs = Signal(31)
                q_nan = Signal()
                m.d.comb += [
                    q_abs.eq(a_word[:31]),
                    q_nan.eq((a_word[23:31] == 255) & a_word[:23].any()),
                ]
                with m.If(~q_nan & (q_abs > q_amax)):
                    m.d.sync += q_amax.eq(q_abs)
            else:
                m.d.sync += q_maxbusy.eq(1)
        if not q_pipeline:
            with m.If(q_maxcap):
                m.d.sync += [q_amax.eq(fcomp.out.bits.data), q_maxbusy.eq(0)]
        with m.If(q_cls):
            with m.If(q_zero):
                m.d.sync += [q_sc.eq(0), q_y.eq(0)]
            with m.Elif(q_big):
                m.d.sync += [q_y.eq(0), q_step.eq(14)]
            with m.Else():
                m.d.sync += [q_step.eq(1), q_wait.eq(1)]
        with m.Elif(q_issue):
            m.d.sync += [q_step.eq(q_step + 1), q_wait.eq(1)]
        with m.If(q_st_scale & q_wait):
            with m.If(frec.out.valid & (q_step == 1)):
                m.d.sync += q_y.eq(frec.out.bits.data)
            with m.If(fmul.out.valid & q_it_cap):
                m.d.sync += q_t.eq(fmul.out.bits.data)
            with m.If(fmul.out.valid & q_upd_cap):
                m.d.sync += q_y.eq(fmul.out.bits.data)
            with m.If(fmul.out.valid & q_sc_cap):
                m.d.sync += q_sc.eq(fmul.out.bits.data)
            with m.If(fadd.out.valid & q_sub_cap):
                m.d.sync += q_t.eq(fadd.out.bits.data)
            with m.If((frec.out.valid & (q_step == 1))
                      | (fmul.out.valid & (q_it_cap | q_upd_cap))
                      | (fadd.out.valid & q_sub_cap)):
                m.d.sync += [q_step.eq(q_step + 1), q_wait.eq(0)]
        if q_pipeline:
            with m.If(q_m1_go):
                m.d.sync += q_idx.eq(q_idx + 1)
        else:
            with m.If(q_m1_go):
                m.d.sync += q_pewait.eq(1)
            with m.If(q_cap1):
                m.d.sync += [
                    q_t.eq(fmul.out.bits.data),
                    q_pe.eq(1),
                    q_pewait.eq(0)
                ]
            with m.If(q_m2_go):
                m.d.sync += q_pewait.eq(1)
        with m.If(q_cap2):
            m.d.sync += [
                q_bytes.eq(q_next_bytes),
                q_bcnt.eq(q_bcnt + 1),
                q_ccnt.eq(q_ccnt + 1),
                q_sum.eq(q_sum + q_byte.as_signed()),
                q_pe.eq(0),
                q_pewait.eq(0),
            ]
            if not q_pipeline:
                m.d.sync += q_idx.eq(q_idx + 1)
        with m.If(in_elem & op_quant & q_st_meta & (q_mstep < 2)):
            m.d.sync += q_mstep.eq(q_mstep + 1)
        with m.If(q_drain_go):
            m.d.sync += q_didx.eq(q_didx + 1)

        # The sequencer rests in QIDLE between commands, armed by a
        # fresh descriptor: in_elem stays high while the packer
        # drains the last command's beats, and an unarmed QIDLE must
        # not re-read the exhausted A window.
        with m.FSM(name='quant') as qfsm:
            with m.State('QIDLE'):
                with m.If(in_elem & op_quant & q_arm):
                    q_start_segment()
                    m.d.sync += [q_arm.eq(0), q_seg.eq(0), q_didx.eq(0)]
                    m.next = 'AMAX'
            with m.State('AMAX'):
                m.d.comb += q_st_amax.eq(1)
                with m.If((q_idx == q_valid) & ~q_maxbusy):
                    m.d.sync += [q_step.eq(0), q_wait.eq(0)]
                    m.next = 'SCALE'
            with m.State('SCALE'):
                m.d.comb += q_st_scale.eq(1)
                with m.If(q_cls & q_zero):
                    m.d.sync += [
                        q_idx.eq(0),
                        q_ccnt.eq(0),
                        q_bcnt.eq(0),
                        q_pe.eq(0),
                        q_pewait.eq(0)
                    ]
                    m.next = 'QUANT'
                with m.If(fmul.out.valid & q_sc_cap):
                    m.d.sync += [
                        q_idx.eq(0),
                        q_ccnt.eq(0),
                        q_bcnt.eq(0),
                        q_pe.eq(0),
                        q_pewait.eq(0)
                    ]
                    m.next = 'QUANT'
            with m.State('QUANT'):
                m.d.comb += q_st_quant.eq(1)
                with m.If(q_ccnt == 32):
                    m.d.sync += q_mstep.eq(0)
                    m.next = 'META'
            with m.State('META'):
                m.d.comb += q_st_meta.eq(1)
                with m.If(q_mstep == 2):
                    m.d.sync += q_seg.eq(q_seg + 1)
                    with m.If(consumed == n_l):
                        m.next = 'DRAIN'
                    with m.Else():
                        q_start_segment()
                        m.next = 'AMAX'
            with m.State('DRAIN'):
                m.d.comb += q_st_drain.eq(1)
                # Leave only on the last word's accepted enqueue: the
                # index alone can outrun a backpressured FIFO.
                with m.If(q_drain_go
                          & ((q_didx + 1).as_unsigned() ==
                             (q_total - q_qwords).as_unsigned())):
                    m.next = 'QIDLE'

        m.d.comb += [
            res_q.enq.valid.eq(is_add_op & fadd.out.valid
                               | is_mul_op & fmul.out.valid
                               | is_unary & un_drain_go
                               | op_quant & q_enq_valid
                               | op_f16 & f16n.out_valid
                               | op_pswap & pswap_push),
            res_q.enq.bits.val.eq(
                Mux(
                    is_unary, un_drain_word,
                    Mux(
                        op_quant, q_enq_word,
                        Mux(
                            op_f16, Cat(f16n.out, Const(0, 16)),
                            Mux(
                                op_pswap, pswap_word,
                                Mux(is_add_op, fadd.out.bits.data,
                                    fmul.out.bits.data)))))),
            res_q.deq.ready.eq(take),
        ]

        #
        # Reduction datapath. SUM and SUMSQ accumulate into the four
        # ``isum_4`` partials; at most four adds are in flight, so the
        # completing partial is always the oldest. SUMSQ squares
        # through the multiplier into a credit-accounted staging FIFO.
        # MAX keeps one accumulator seeded from the first element. The
        # fold launches after the last accumulate retires.
        #
        part = [Signal(32, name=f'part{i}') for i in range(4)]
        part_arr = Array(part)
        add_iss = Signal(self.lg_act_k + 2)
        pend_cnt = Signal(range(5))
        mul_iss = Signal(self.lg_act_k + 2)
        sq_inflight = Signal(range(self.vec_sq_q + 1))

        sq_room = sq_q.count + sq_inflight < self.vec_sq_q
        sq_mul_go = running & op_sumsq & elem_a_ok & (mul_iss < n_l) \
            & sq_room
        sum_add_go = running & op_sum & elem_a_ok & (add_iss < n_l) \
            & (pend_cnt < 4)
        sumsq_add_go = running & op_sumsq & sq_q.deq.valid \
            & (add_iss < n_l) & (pend_cnt < 4)

        fold_v = [Signal(32, name=f'fold_v{i}') for i in range(3)]
        fold_arr = Array(fold_v)
        fold_iss = Signal(range(4))
        fold_ret = Signal(range(4))
        acc = Signal(32)
        acc_busy = Signal()

        acc_go = sum_add_go | sumsq_add_go
        # The fold sequence launches once every accumulate has retired
        # (pend_cnt == 0 leaves no accumulate result outstanding), and
        # only inside a running sum reduction -- without the state and
        # op gates the idle add_iss == n_l == 0 condition would launch
        # phantom folds whose stale results could corrupt the next
        # command's partials.
        fold_run = in_rdc & (op_sum | op_sumsq)
        fold1_go = fold_run & (add_iss == n_l) & (pend_cnt == 0) \
            & (fold_iss == 0)
        fold2_go = fold_run & (fold_iss == 1)
        foldff_go = fold_run & (fold_iss == 2) & (fold_ret == 2)

        max_iss = Signal(self.lg_act_k + 2)
        max_first = max_iss == 0
        max_go = running & op_max & elem_a_ok & (max_iss < n_l) \
            & (max_first | ~acc_busy)
        max_comp_go = max_go & ~max_first

        #
        # FPU unit inputs. The use contexts are mutually exclusive (one
        # command, one lifecycle phase), so a priority chain suffices.
        # The unary sequencer's contexts join the head of each chain.
        #
        m.d.comb += [
            fadd.inp.bits.fn.eq(FPUOperator.ADD),
            fadd.inp.bits.rm.eq(RoundingMode.RNE),
            fadd.inp.bits.in2.eq(0),
            fmul.inp.bits.fn.eq(FPUOperator.MUL),
            fmul.inp.bits.rm.eq(RoundingMode.RNE),
            fmul.inp.bits.in3.eq(0),
            fcomp.inp.bits.fn.eq(FPUOperator.MINMAX),
            fcomp.inp.bits.rm.eq(RoundingMode.RTZ),
        ]
        if fadd1 is not None:
            m.d.comb += [
                fadd1.inp.bits.fn.eq(FPUOperator.ADD),
                fadd1.inp.bits.rm.eq(RoundingMode.RNE),
                fadd1.inp.bits.in2.eq(0),
            ]
        if fmul1 is not None:
            m.d.comb += [
                fmul1.inp.bits.fn.eq(FPUOperator.MUL),
                fmul1.inp.bits.rm.eq(RoundingMode.RNE),
                fmul1.inp.bits.in3.eq(0),
            ]
        m.d.comb += fadd.inp.valid.eq(elem_add_go | acc_go | fold1_go
                                      | fold2_go | foldff_go
                                      | un_add_go0 | q_sub_go)
        if fadd1 is not None:
            m.d.comb += fadd1.inp.valid.eq(un_add_go1)
        # Unary add issues: the exp2 fraction subtract x - float32(i)
        # and the Newton corrections 2.0 - t / 1.5 - t subtract; the
        # exp2 Horner steps add; SILU's fraction subtract and Newton
        # corrections follow the same rule.
        m.d.comb += fadd.inp.bits.fn_mod.eq((elem_add_go & op_sub)
                                            | (un_add_go0
                                               & unary_add_fn_mod(view_add0))
                                            | q_sub_go)
        if fadd1 is not None:
            m.d.comb += fadd1.inp.bits.fn_mod.eq(un_add_go1
                                                 & unary_add_fn_mod(view_add1))
        with m.If(is_unary):
            # fadd: the winning group-0 context's fraction subtract or
            # Horner add, or the Newton correction.
            m.d.comb += [
                fadd.inp.bits.in1.eq(unary_add_in1(view_add0)),
                fadd.inp.bits.in3.eq(unary_add_in3(view_add0)),
            ]
            if fadd1 is not None:
                m.d.comb += [
                    fadd1.inp.bits.in1.eq(unary_add_in1(view_add1)),
                    fadd1.inp.bits.in3.eq(unary_add_in3(view_add1)),
                ]
        with m.Elif(q_sub_go):
            m.d.comb += [
                fadd.inp.bits.in1.eq(Const(0x40000000, 32)),
                fadd.inp.bits.in3.eq(q_t)
            ]
        with m.Elif(elem_add_go):
            m.d.comb += [
                fadd.inp.bits.in1.eq(a_word),
                fadd.inp.bits.in3.eq(b_operand)
            ]
        with m.Elif(acc_go):
            m.d.comb += [
                fadd.inp.bits.in1.eq(part_arr[add_iss[:2]]),
                fadd.inp.bits.in3.eq(Mux(op_sum, a_word, sq_q.deq.bits.val)),
            ]
        with m.Elif(fold1_go):
            m.d.comb += [
                fadd.inp.bits.in1.eq(part[0]),
                fadd.inp.bits.in3.eq(part[2])
            ]
        with m.Elif(fold2_go):
            m.d.comb += [
                fadd.inp.bits.in1.eq(part[1]),
                fadd.inp.bits.in3.eq(part[3])
            ]
        with m.Else():
            m.d.comb += [
                fadd.inp.bits.in1.eq(fold_v[0]),
                fadd.inp.bits.in3.eq(fold_v[1])
            ]

        m.d.comb += fmul.inp.valid.eq(elem_mul_go | sq_mul_go | un_mul_go0
                                      | q_it_go | q_upd_go | q_sc_go | q_m1_go
                                      | (Const(0) if q_pipeline else q_m2_go))
        if fmul1 is not None:
            m.d.comb += fmul1.inp.valid.eq(un_mul_go1 | (
                q_m2_go if q_pipeline else Const(0)))
        with m.If(is_unary):
            # fmul: the winning group-0 context's seed-refinement
            # products in the model's fixed operand order, and the
            # Horner multiplies p * f. rsqrt's h multiplies read the h
            # register (un_f).
            m.d.comb += [
                fmul.inp.bits.in1.eq(unary_mul_in1(view_mul0)),
                fmul.inp.bits.in2.eq(unary_mul_in2(view_mul0)),
            ]
            if fmul1 is not None:
                m.d.comb += [
                    fmul1.inp.bits.in1.eq(unary_mul_in1(view_mul1)),
                    fmul1.inp.bits.in2.eq(unary_mul_in2(view_mul1)),
                ]
        with m.Elif(op_quant):
            # The scale products t = amax*y, y = y*t, sc = amax*(1/127),
            # and the element products t = x*r then p = t*127.
            with m.If(q_st_quant):
                if q_pipeline:
                    m.d.comb += [
                        fmul.inp.bits.in1.eq(qbuf[q_idx]),
                        fmul.inp.bits.in2.eq(q_y),
                        fmul1.inp.bits.in1.eq(q_pipe_data),
                        fmul1.inp.bits.in2.eq(Const(F127_BITS, 32)),
                    ]
                else:
                    m.d.comb += [
                        fmul.inp.bits.in1.eq(Mux(q_pe, q_t, qbuf[q_idx])),
                        fmul.inp.bits.in2.eq(
                            Mux(q_pe, Const(F127_BITS, 32), q_y)),
                    ]
            with m.Else():
                m.d.comb += [
                    fmul.inp.bits.in1.eq(Mux(q_it_go | q_sc_go, q_amax, q_y)),
                    fmul.inp.bits.in2.eq(
                        Mux(
                            q_it_go, q_y,
                            Mux(q_sc_go, Const(INV127_BITS, 32),
                                Mux(q_upd_go, q_t, Const(F127_BITS, 32))))),
                ]
        with m.Elif(op_sumsq):
            m.d.comb += [
                fmul.inp.bits.in1.eq(a_word),
                fmul.inp.bits.in2.eq(a_word)
            ]
        with m.Else():
            m.d.comb += [
                fmul.inp.bits.in1.eq(a_word),
                fmul.inp.bits.in2.eq(b_operand)
            ]

        m.d.comb += [
            fcomp.inp.valid.eq(max_comp_go
                               | (Const(0) if q_pipeline else q_max_go)),
            fcomp.inp.bits.in1.eq(Mux(op_quant, q_amax, acc)),
            fcomp.inp.bits.in2.eq(Mux(op_quant, a_word & 0x7fffffff, a_word)),
        ]

        # The seed block: reciprocal reads |x|, rsqrt reads x; the
        # classify step has already excluded every input whose FPURec
        # estimate would leave the normal domain.
        m.d.comb += [
            frec.inp.valid.eq(un_rec_go | q_seed_go),
            frec.inp.bits.fn.eq(FPUOperator.REC),
            frec.inp.bits.fn_mod.eq(op_rsqrt),
            frec.inp.bits.rm.eq(RoundingMode.RNE),
            frec.inp.bits.in1.eq(
                Mux(
                    op_quant, q_amax,
                    Mux(
                        op_silu, un_acc[sel_rec] & 0x7fffffff,
                        Mux(op_rsqrt, un_x[sel_rec],
                            un_x[sel_rec] & 0x7fffffff)))),
            frec.inp.bits.in2.eq(0),
            frec.inp.bits.in3.eq(0),
        ]

        #
        # FPU unit outputs, captured by the owning context.
        #
        m.d.comb += [
            sq_q.enq.valid.eq(op_sumsq & fmul.out.valid),
            sq_q.enq.bits.val.eq(fmul.out.bits.data),
            sq_q.deq.ready.eq(sumsq_add_go),
        ]

        # A completing accumulate retires the oldest in-flight partial;
        # once every accumulate has retired, an adder result belongs to
        # the fold instead.
        acc_pidx = (add_iss - pend_cnt)[:2]
        acc_retire = (op_sum | op_sumsq) & fadd.out.valid & (pend_cnt != 0)
        fold_cap = fold_run & fadd.out.valid & (pend_cnt == 0)
        with m.If(acc_retire):
            m.d.sync += pend_cnt.eq(pend_cnt - 1)
            for i in range(4):
                with m.If(acc_pidx == i):
                    m.d.sync += part[i].eq(fadd.out.bits.data)

        with m.If(fold_cap):
            m.d.sync += [
                fold_arr[fold_ret].eq(fadd.out.bits.data),
                fold_ret.eq(fold_ret + 1),
            ]
        with m.If(fold1_go | fold2_go | foldff_go):
            m.d.sync += fold_iss.eq(fold_iss + 1)

        with m.If(op_max & fcomp.out.valid):
            m.d.sync += [acc.eq(fcomp.out.bits.data), acc_busy.eq(0)]

        #
        # Element consumption and run counters. Every context that
        # consumes an element advances the window(s) and counters; the
        # in-flight counters update in one statement each so a launch
        # and a retirement in the same cycle cannot override each
        # other.
        #
        consume_a = elem_add_go | elem_mul_go | sum_add_go | sq_mul_go \
            | max_go | un_go | q_max_go | f16_go | pswap_go | sm_max_go
        with m.If(consume_a):
            m.d.sync += [
                awin.eq(awin >> 32),
                ahave.eq(ahave - 4),
                consumed.eq(consumed + 1),
                self.elements.eq(self.elements + 1),
            ]
        with m.If(binary_b & (elem_add_go | elem_mul_go)):
            m.d.sync += [bwin.eq(bwin >> 32), bhave.eq(bhave - 4)]

        m.d.sync += issued.eq(
            issued + Mux(elem_add_go | elem_mul_go | un_go
                         | f16_go | pswap_go, 1, 0))
        m.d.sync += res_inflight.eq(
            res_inflight +
            Mux(elem_add_go | elem_mul_go
                | un_go | f16_go | pswap_go, 1, 0) +
            Mux(q_m1_go & q_grp_new, 1, 0) + Mux(q_drain_go, 1, 0) -
            Mux(res_q.enq.fire, 1, 0))
        m.d.sync += add_iss.eq(add_iss + Mux(acc_go, 1, 0))
        m.d.sync += pend_cnt.eq(pend_cnt + Mux(acc_go, 1, 0) -
                                Mux(acc_retire, 1, 0))
        m.d.sync += mul_iss.eq(mul_iss + Mux(sq_mul_go, 1, 0))
        m.d.sync += sq_inflight.eq(sq_inflight + Mux(sq_mul_go, 1, 0) -
                                   Mux(sq_q.enq.fire, 1, 0))
        with m.If(max_go):
            m.d.sync += max_iss.eq(max_iss + 1)
            with m.If(max_first):
                m.d.sync += acc.eq(a_word)
            with m.Else():
                m.d.sync += acc_busy.eq(1)

        #
        # Write side: the elementwise packer or the single reduction
        # result beat, selected by the active datapath.
        #
        rdc_val = Mux(op_max, acc, fold_v[2])
        rdc_ready = op_max & (max_iss == n_l) & ~acc_busy \
            | (op_sum | op_sumsq) & (fold_ret == 3)

        m.d.comb += [
            self.tmem_w.valid.eq(
                Mux(
                    in_rdc, rdc_ready,
                    in_elem & (flushing | final_partial | quant_boundary)
                    | sm_sum_we)),
            self.tmem_w.bits.addr.eq(
                Mux(
                    in_rdc, o_base,
                    Mux(
                        sm_sum_we, b_base,
                        Mux(op_quant & (bidx >= qnbeats),
                            b_base + ((bidx - qnbeats) << lg),
                            o_base + (bidx << lg))))),
            self.tmem_w.bits.data.eq(
                Mux(
                    in_rdc, Cat(rdc_val, Const(0, self.data_width - 32)),
                    Mux(sm_sum_we,
                        Cat(sm_fold_v[2], Const(0, self.data_width - 32)),
                        beat_buf))),
        ]
        elem_w_fire = in_elem & ~sm_sum_we & self.tmem_w.fire
        elem_last_beat = elem_w_fire & (wdone + 1 == nbeats)
        with m.If(elem_w_fire):
            m.d.sync += [
                wdone.eq(wdone + 1),
                bidx.eq(bidx + 1),
                wcnt.eq(0),
                beat_buf.eq(0),
            ]
        with m.If(self.tmem_w.fire):
            m.d.sync += self.write_beats.eq(self.write_beats + 1)

        advance = consume_a

        #
        # Run control.
        #
        with m.FSM() as fsm:
            with m.State('IDLE'):
                m.d.comb += [
                    self.sink.ready.eq(1),
                    self.done.eq(self.sink.fire & bad),
                    self.status.eq(
                        Mux(shape_bad, Status.BAD_SHAPE,
                            Mux(align_bad, Status.BAD_ALIGN,
                                Status.BAD_RANGE))),
                ]
                # A rejected descriptor latches nothing: the previous
                # command's terminal state (or the reset state) is
                # inert, so no read, launch, or write can follow a
                # rejection.
                with m.If(self.sink.fire & ~bad):
                    q_nsegs_l = (offer.n + 31) >> 5
                    m.d.sync += [
                        op.eq(offer.op),
                        n_l.eq(offer.n),
                        a_base.eq(offer.a_base),
                        b_base.eq(offer.b_base),
                        o_base.eq(offer.o_base),
                        nbeats.eq(nbeats_o),
                        ibeats.eq(ibeats_o),
                        q_qwords.eq(q_nsegs_l * 8),
                        q_total.eq(q_nsegs_l * 10),
                        qnbeats.eq((q_nsegs_l * 32 + (bb - 1)) >> lg),
                        issued.eq(0),
                        consumed.eq(0),
                        res_inflight.eq(0),
                        deq_count.eq(0),
                        wcnt.eq(0),
                        bidx.eq(0),
                        wdone.eq(0),
                        beat_buf.eq(0),
                        awin.eq(0),
                        ahave.eq(0),
                        abeat.eq(0),
                        bwin.eq(0),
                        bhave.eq(0),
                        bbeat.eq(0),
                        bscalar.eq(0),
                        bscal_ok.eq(0),
                        rd_pend.eq(0),
                        ord_cnt.eq(0),
                        ord_head.eq(0),
                        ord_tail.eq(0),
                        add_tcnt.eq(0),
                        mul_tcnt.eq(0),
                        rec_tcnt.eq(0),
                        q_step.eq(0),
                        q_wait.eq(0),
                        q_valid.eq(0),
                        q_idx.eq(0),
                        q_ccnt.eq(0),
                        q_bcnt.eq(0),
                        q_bytes.eq(0),
                        q_sum.eq(0),
                        q_mstep.eq(0),
                        q_seg.eq(0),
                        q_didx.eq(0),
                        q_pe.eq(0),
                        q_pewait.eq(0),
                        q_arm.eq(1),
                        q_pipe_valid.eq(0),
                        q_amax.eq(0),
                        q_maxbusy.eq(0),
                        q_y.eq(0),
                        q_t.eq(0),
                        q_sc.eq(0),
                        sm_m.eq(0),
                        sm_any.eq(0),
                        sm_iss.eq(0),
                        sm_pend.eq(0),
                        sm_fold_iss.eq(0),
                        sm_fold_ret.eq(0),
                        sm_sum_wr.eq(0),
                        add_iss.eq(0),
                        pend_cnt.eq(0),
                        mul_iss.eq(0),
                        sq_inflight.eq(0),
                        fold_iss.eq(0),
                        fold_ret.eq(0),
                        max_iss.eq(0),
                        acc_busy.eq(0),
                    ]
                    for i in range(4):
                        m.d.sync += part[i].eq(0)
                        m.d.sync += sm_p[i].eq(0)
                    # Unary contexts: control state resets; the data
                    # registers are dead state, re-latched at accept
                    # and classify before any use.
                    for i in range(ctx_n):
                        m.d.sync += [
                            un_busy[i].eq(0),
                            un_wait[i].eq(0),
                            un_arm[i].eq(0),
                            un_bp[i].eq(0),
                            un_step[i].eq(0),
                            done_val[i].eq(0),
                        ]
                    with m.If(o_reduce):
                        m.next = 'REDUCE'
                    with m.Else():
                        m.next = 'ELEM'

            with m.State('ELEM'):
                with m.If(elem_last_beat & ~op_smexp):
                    m.next = 'DONE'
                # VEC_SMEXP completes only after its denominator beat
                # lands (the folds can finish before or after the
                # last E beat; both orders are legal).
                with m.If(op_smexp & (wdone == nbeats) & sm_sum_wr):
                    m.next = 'DONE'

            with m.State('REDUCE'):
                with m.If(self.tmem_w.fire):
                    m.next = 'DONE'

            with m.State('DONE'):
                m.d.comb += [self.done.eq(1), self.status.eq(Status.OK)]
                m.next = 'IDLE'

        m.d.comb += [
            self.active.eq(~fsm.ongoing('IDLE')),
            in_elem.eq(fsm.ongoing('ELEM')),
            in_rdc.eq(fsm.ongoing('REDUCE')),
        ]
        with m.If(running):
            with m.If((consumed != n_l) & ~advance):
                m.d.sync += self.stall_cycles.eq(self.stall_cycles + 1)
            m.d.sync += self.active_cycles.eq(self.active_cycles + 1)
        m.d.sync += self.add_issues.eq(
            self.add_issues + Mux(fadd.inp.valid, 1, 0) +
            Mux(fadd1.inp.valid if fadd1 is not None else 0, 1, 0) +
            Mux(fsum.inp.valid, 1, 0) +
            sum(Mux(unit.inp.valid, 1, 0) for unit in extra_add))
        m.d.sync += self.mul_issues.eq(
            self.mul_issues + Mux(fmul.inp.valid, 1, 0) +
            Mux(fmul1.inp.valid if fmul1 is not None else 0, 1, 0) +
            sum(Mux(unit.inp.valid, 1, 0) for unit in extra_mul))
        m.d.sync += self.rec_issues.eq(self.rec_issues +
                                       Mux(frec.inp.valid, 1, 0))
        m.d.sync += self.comp_issues.eq(self.comp_issues +
                                        Mux(fcomp.inp.valid, 1, 0))

        return m
