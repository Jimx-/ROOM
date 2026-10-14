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
    VEC_QUANT per 32-word segment: a = fmax fold of |x| from +0;
              sc = R(a * (1/127)); r = recip(a);
              q_i = q8(R(R(x_i * r) * 127)) (RNE INT8, saturating;
              the unscaled r keeps every intermediate <= ~127);
              quants -> o_base, (s_act, int16 sum) meta -> b_base

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
transcendentals serialize elements through a dependency
micro-sequencer that issues each arithmetic request once and
captures it on ``out.valid`` (operation counts are not cycle counts:
exp2's fifteen dependent rounded ops occupy roughly sixty cycles per
element with the three-cycle blocks).

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

    def elaborate(self, platform):
        m = Module()

        lg = self.lg_beat
        bb = self.beat_bytes
        aw = self.tmem_addr_width
        bw = self.data_width // 32  # FP32 words per beat

        # Widest expressible element count bounds every beat cursor.
        max_beats = (2 * self.act_max_k * 4 + bb - 1) // bb

        # Register operand selection before the FPU arithmetic stage.
        fadd = m.submodules.fadd = FPUFMA(32, FPFormat.S, latency=3)
        fmul = m.submodules.fmul = FPUFMA(32, FPFormat.S, latency=3)
        fcomp = m.submodules.fcomp = FPUComp(32, FPFormat.S, latency=2)
        frec = m.submodules.frec = FPURec(32, FPFormat.S, latency=3)
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
        o_reads_b = Cat(offer.op == op for op in VEC_READS_B).any()
        op_bad = ~(o_elem | o_reduce)

        eb = offer.n << 2
        a_pad = ((eb + (bb - 1)) >> lg) << lg
        # VEC_QUANT writes two regions: nsegs*32 bytes of packed
        # quants at o_base, nsegs*8 bytes of meta at b_base.
        q_nsegs = (offer.n + 31) >> 5
        q_o_pad = ((q_nsegs * 32 + (bb - 1)) >> lg) << lg
        q_m_pad = ((q_nsegs * 8 + (bb - 1)) >> lg) << lg
        b_pad = Mux(o_scale, bb, Mux(o_quant, q_m_pad, a_pad))
        o_pad = Mux(o_reduce, bb, Mux(o_quant, q_o_pad, a_pad))
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
        shape_bad = op_bad | (offer.n == 0) | overlap_a | overlap_b \
            | quant_overlap
        align_bad = offer.a_base[:lg].any() | offer.o_base[:lg].any() \
            | (o_reads_b & offer.b_base[:lg].any()) \
            | (o_quant & offer.b_base[:lg].any())
        range_bad = (a_end > self.tmem_bytes) | (o_end > self.tmem_bytes) \
            | (o_reads_b & (b_end > self.tmem_bytes)) \
            | (o_quant & (b_end > self.tmem_bytes))
        bad = shape_bad | align_bad | range_bad

        q_qbeats = (q_nsegs * 32 + (bb - 1)) >> lg
        q_mbeats = (q_nsegs * 8 + (bb - 1)) >> lg
        nbeats_o = Mux(
            o_reduce, 1,
            Mux(o_quant, q_qbeats + q_mbeats, (eb + (bb - 1)) >> lg))
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

        op_add = op == VecOp.VEC_ADD
        op_sub = op == VecOp.VEC_SUB
        op_mul = op == VecOp.VEC_MUL
        op_scale = op == VecOp.VEC_SCALE
        op_sum = op == VecOp.RDC_SUM
        op_max = op == VecOp.RDC_MAX
        op_sumsq = op == VecOp.RDC_SUMSQ
        op_exp2 = op == VecOp.VEC_EXP2
        op_recip = op == VecOp.VEC_RECIP
        op_rsqrt = op == VecOp.VEC_RSQRT
        op_quant = op == VecOp.VEC_QUANT
        is_add_op = op_add | op_sub
        is_mul_op = op_mul | op_scale
        binary_b = op_add | op_sub | op_mul
        is_unary = op_exp2 | op_recip | op_rsqrt

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
        wcnt = Signal(range(bw + 1))
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

        flushing = wcnt == bw
        quant_boundary = in_elem & op_quant & (deq_count == q_qwords) \
            & (wcnt != 0)
        final_partial = (deq_count == Mux(op_quant, q_total, n_l)) \
            & (wcnt != 0)
        take = in_elem & res_q.deq.valid & ~(flushing | quant_boundary)

        with m.If(take):
            m.d.sync += [
                beat_buf.eq(beat_buf
                            | (res_q.deq.bits.val << (32 * wcnt))),
                wcnt.eq(wcnt + 1),
                deq_count.eq(deq_count + 1),
            ]

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
        un_x = Signal(32)  # original operand
        un_acc = Signal(32)  # seed / refinement y, Horner p
        un_tmp = Signal(32)  # iteration temporary t
        un_f = Signal(32)  # exp2 fraction
        un_busy = Signal()
        un_wait = Signal()  # request issued, awaiting out.valid
        un_step = Signal(5)

        def step_in(*steps):
            return Cat(un_step == s for s in steps).any()

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

        def capture_after(steps):
            return step_in(*(step + 1 for step in steps))

        # Classification of un_x: the frozen special-value table of
        # tests/troom/model.py. exp2's clamps are bit comparisons:
        # x >= 128 is exponent field >= 134 (field 134 is [128, 256)),
        # x < -126 is |x| > 126 (field >= 134, or field 133 with the
        # 24-bit significand above 126 * 2^17).
        ux_sign = un_x[31]
        ux_e = un_x[23:31]
        ux_man_nz = un_x[:23].any()
        ux_nan = (ux_e == 255) & ux_man_nz
        ux_inf = (ux_e == 255) & ~ux_man_nz
        ux_zero = (ux_e == 0) & ~ux_man_nz
        ux_sig = Cat(un_x[:23], Const(1, 1))
        ux_ge128 = ~ux_sign & (ux_e >= 134)
        ux_lt_m126 = ux_sign & ((ux_e >= 134)
                                | ((ux_e == 133)
                                   & (ux_sig > Const(0xfc0000, 24))))

        recip_bypass = ux_nan | (ux_e == 0) | (ux_e >= 253)
        rsqrt_bypass = ux_nan | ux_sign | ux_zero | ux_inf \
            | (ux_e == 0)
        exp2_bypass = ux_nan | ux_ge128 | ux_lt_m126
        un_bypass = Mux(op_recip, recip_bypass,
                        Mux(op_rsqrt, rsqrt_bypass, exp2_bypass))

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
        un_bypass_val = Mux(op_recip, recip_val,
                            Mux(op_rsqrt, rsqrt_val, exp2_val))

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

        # exp2 final scaling: add i to the polynomial result's
        # exponent field (p is in [1, 2], so its field is 127 or
        # 128; the 9-bit sum spans 0..255 over the in-domain i).
        exp2_pe = Cat(fadd.out.bits.data[23:31], Const(0, 1)).as_signed()
        exp2_sum = exp2_pe + ux_i.as_signed()
        un_exp2_word = Mux(
            exp2_sum >= 255, PINF,
            Mux(exp2_sum <= 0, PZERO, (fadd.out.bits.data & 0x807fffff)
                | ((exp2_sum[:8]) << 23)))

        # Step map: even steps issue, the following odd step awaits.
        #   VEC_RECIP: 0 classify (+seed); 2/8 R(|x|*y); 4/10
        #     R(2.0-t); 6/12 R(y*t); 13 final, sign of x applied.
        #   VEC_RSQRT: 0 classify (+seed); 2 R(0.5*x) into the h
        #     register (un_f); per Newton iteration 4/12 R(h*y),
        #     6/14 R(t*y), 8/16 R(1.5-t), 10/18 R(y*t); 19 final.
        #     h keeps its own register: the iteration's temporary
        #     would clobber it.
        #   VEC_EXP2: 0 classify (+f = R(x - float32(i))); per
        #     Horner step j (2+4j mul, 4+4j add, j = 0..6, so
        #     coefficient C(6-j)); 29 final, scaled by 2^i.
        un_go = in_elem & is_unary & ~un_busy & elem_a_ok \
            & (issued < n_l) & room
        un_cls = un_busy & (un_step == 0) & ~un_wait
        un_bypass_go = un_cls & un_bypass
        un_seed_go = un_cls & ~un_bypass & (op_recip | op_rsqrt)
        un_fsub_go = un_cls & ~un_bypass & op_exp2
        un_issue = un_busy & ~un_wait & (un_step != 0)

        recip_mul = op_recip & un_issue & step_in(*rec_xy, *rec_update)
        recip_sub = op_recip & un_issue & step_in(*rec_sub)
        rsqrt_mul = op_rsqrt & un_issue \
            & step_in(*rs_half, *rs_hy, *rs_ty, *rs_update)
        rsqrt_sub = op_rsqrt & un_issue & step_in(*rs_sub)
        exp2_mul = op_exp2 & un_issue \
            & step_in(*exp_mul)
        exp2_add = op_exp2 & un_issue \
            & step_in(*exp_add)
        un_mul_go = recip_mul | rsqrt_mul | exp2_mul
        un_add_go = recip_sub | rsqrt_sub | exp2_add | un_fsub_go

        # Await/capture decode per owning unit and destination.
        cap_seed = un_wait & (un_step == 1) & (op_recip | op_rsqrt)
        cap_mul_t = un_wait & (op_recip & capture_after(rec_xy)
                               | op_rsqrt & capture_after(rs_hy + rs_ty)
                               | op_exp2 & capture_after(exp_mul))
        cap_mul_h = un_wait & op_rsqrt & capture_after(rs_half)
        cap_mul_a = un_wait & (op_recip & capture_after(rec_update[:-1])
                               | op_rsqrt & capture_after(rs_update[:-1]))
        cap_add_t = un_wait & (op_recip & capture_after(rec_sub)
                               | op_rsqrt & capture_after(rs_sub))
        cap_add_f = un_wait & op_exp2 & (un_step == 1)
        cap_add_a = un_wait & op_exp2 \
            & capture_after(exp_add[:-1])
        un_fin_mul = un_wait & fmul.out.valid \
            & (op_recip & capture_after(rec_update[-1:])
               | op_rsqrt & capture_after(rs_update[-1:]))
        un_fin_add = un_wait & fadd.out.valid & op_exp2 \
            & capture_after(exp_add[-1:])

        # Final word: recip applies the sign of x, exp2 scales the
        # polynomial by 2^i; rsqrt's last multiply is the result.
        un_fin_word = Mux(op_recip,
                          fmul.out.bits.data ^ Cat(Const(0, 31), ux_sign),
                          Mux(op_rsqrt, fmul.out.bits.data, un_exp2_word))
        un_enq_valid = un_bypass_go | un_fin_mul | un_fin_add
        un_enq_word = Mux(un_bypass_go, un_bypass_val, un_fin_word)

        with m.If(un_go):
            m.d.sync += [
                un_busy.eq(1),
                un_step.eq(0),
                un_wait.eq(0),
                un_x.eq(a_word)
            ]
        with m.Elif(un_busy):
            with m.If(~un_wait):
                # An issue state advances to its await state; the
                # bypass path completes through res_q below instead.
                with m.If(~(un_cls & un_bypass)):
                    m.d.sync += [un_step.eq(un_step + 1), un_wait.eq(1)]
            with m.Else():
                # Awaiting: capture on the owning unit's valid. The
                # reserved credit guarantees the final enqueue fires
                # the same cycle, so a final capture ends the element
                # rather than advancing.
                with m.If(frec.out.valid & cap_seed):
                    m.d.sync += un_acc.eq(frec.out.bits.data)
                with m.If(fmul.out.valid & cap_mul_t):
                    m.d.sync += un_tmp.eq(fmul.out.bits.data)
                with m.If(fmul.out.valid & cap_mul_h):
                    m.d.sync += un_f.eq(fmul.out.bits.data)
                with m.If(fmul.out.valid & cap_mul_a):
                    m.d.sync += un_acc.eq(fmul.out.bits.data)
                with m.If(fadd.out.valid & cap_add_t):
                    m.d.sync += un_tmp.eq(fadd.out.bits.data)
                with m.If(fadd.out.valid & cap_add_f):
                    m.d.sync += un_f.eq(fadd.out.bits.data)
                with m.If(fadd.out.valid & cap_add_a):
                    m.d.sync += un_acc.eq(fadd.out.bits.data)
                with m.If(un_fin_mul | un_fin_add):
                    m.d.sync += un_wait.eq(0)
                with m.Elif((frec.out.valid & cap_seed)
                            | (fmul.out.valid
                               & (cap_mul_t | cap_mul_h | cap_mul_a))
                            | (fadd.out.valid
                               & (cap_add_t | cap_add_f | cap_add_a))):
                    m.d.sync += [un_step.eq(un_step + 1), un_wait.eq(0)]
            # The sequencer returns to idle once the final word (or
            # bypass value) commits to the result FIFO.
            with m.If(un_enq_valid & res_q.enq.fire):
                m.d.sync += un_busy.eq(0)

        #
        # Activation quantization datapath (VEC_QUANT). One
        # segment-serial sequencer: fold amax over the 32 buffered
        # words (FPUComp on |x| from +0), derive sc and the unscaled
        # r ~ 1/amax through the shared multiplier and the FPURec
        # seed (the frozen recip sequence), then quantize the buffer
        # with the two-step product t = R(x*r), p = R(t*127) and the
        # bounded RNE int8 helper. Packed quant words and the meta
        # pairs flow through res_q like any elementwise result; a
        # credit is reserved per four-element group (and per drained
        # meta word) before the muls that feed it can be
        # backpressured.
        #
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
            & (q_idx < q_valid) & ~q_maxbusy
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
        q_m1_go = in_elem & op_quant & q_st_quant & (q_idx < 32) \
            & ~q_pe & ~q_pewait & (q_grp_new & room | ~q_grp_new)
        q_m2_go = in_elem & op_quant & q_st_quant & q_pe & ~q_pewait
        q_cap1 = in_elem & op_quant & q_st_quant & ~q_pe & q_pewait \
            & fmul.out.valid
        q_cap2 = in_elem & op_quant & q_st_quant & q_pe & q_pewait \
            & fmul.out.valid
        q_word_done = q_cap2 & (q_bcnt == 3)
        q_drain_go = in_elem & op_quant & q_st_drain & room \
            & (q_didx < (q_total - q_qwords).as_unsigned())

        # Bounded RNE int8 rounding of the product, a bit helper in
        # the exp2 floor's style: |p| < 0.5 -> 0, |p| >= 128 or
        # exceptional -> saturate, exact halves to even.
        qp_e = fmul.out.bits.data[23:31]
        qp_man = Cat(fmul.out.bits.data[:23], Const(1, 1))
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
        q_byte = Mux(fmul.out.bits.data[31],
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
                q_maxbusy.eq(1),
            ]
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
                q_idx.eq(q_idx + 1),
            ]
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
                               | is_unary & un_enq_valid
                               | op_quant & q_enq_valid),
            res_q.enq.bits.val.eq(
                Mux(
                    is_unary, un_enq_word,
                    Mux(op_quant, q_enq_word,
                        Mux(is_add_op, fadd.out.bits.data,
                            fmul.out.bits.data)))),
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
        m.d.comb += fadd.inp.valid.eq(elem_add_go | acc_go | fold1_go
                                      | fold2_go | foldff_go
                                      | un_add_go | q_sub_go)
        m.d.comb += fadd.inp.bits.fn_mod.eq((elem_add_go & op_sub) | recip_sub
                                            | rsqrt_sub
                                            | un_fsub_go | q_sub_go)
        with m.If(is_unary):
            # fadd: the exp2 fraction subtract and Horner adds, or
            # the Newton corrections 2.0 - t / 1.5 - t.
            m.d.comb += [
                fadd.inp.bits.in1.eq(
                    Mux(
                        un_fsub_go, un_x,
                        Mux(recip_sub, Const(0x40000000, 32),
                            Mux(rsqrt_sub, Const(0x3fc00000, 32), un_tmp)))),
                fadd.inp.bits.in3.eq(
                    Mux(
                        un_fsub_go, ux_i2f,
                        Mux(op_exp2,
                            EXP2_C[((exp_add[-1] - un_step) >> 2)[:3]],
                            un_tmp))),
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

        m.d.comb += fmul.inp.valid.eq(elem_mul_go | sq_mul_go | un_mul_go
                                      | q_it_go | q_upd_go | q_sc_go | q_m1_go
                                      | q_m2_go)
        with m.If(is_unary):
            # fmul: the seed-refinement products in the model's fixed
            # operand order, and the Horner multiplies p * f. rsqrt's
            # h multiplies read the h register (un_f).
            m.d.comb += [
                fmul.inp.bits.in1.eq(un_acc),
                fmul.inp.bits.in2.eq(un_tmp)
            ]
            with m.If(op_exp2):
                m.d.comb += [
                    fmul.inp.bits.in1.eq(
                        Mux(un_step == exp_mul[0], EXP2_C[7], un_acc)),
                    fmul.inp.bits.in2.eq(un_f),
                ]
            with m.Elif(op_recip & step_in(*rec_xy)):
                m.d.comb += [
                    fmul.inp.bits.in1.eq(un_x & 0x7fffffff),
                    fmul.inp.bits.in2.eq(un_acc)
                ]
            with m.Elif(op_rsqrt):
                with m.If(step_in(*rs_half)):
                    m.d.comb += [
                        fmul.inp.bits.in1.eq(0x3f000000),
                        fmul.inp.bits.in2.eq(un_x)
                    ]
                with m.Elif(step_in(*rs_hy, *rs_ty)):
                    m.d.comb += [
                        fmul.inp.bits.in1.eq(Mux(step_in(*rs_hy), un_f,
                                                 un_tmp)),
                        fmul.inp.bits.in2.eq(un_acc),
                    ]
        with m.Elif(op_quant):
            # The scale products t = amax*y, y = y*t, sc = amax*(1/127),
            # and the element products t = x*r then p = t*127.
            with m.If(q_st_quant):
                m.d.comb += [
                    fmul.inp.bits.in1.eq(Mux(q_pe, q_t, qbuf[q_idx])),
                    fmul.inp.bits.in2.eq(Mux(q_pe, Const(F127_BITS, 32), q_y)),
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
            fcomp.inp.valid.eq(max_comp_go | q_max_go),
            fcomp.inp.bits.in1.eq(Mux(op_quant, q_amax, acc)),
            fcomp.inp.bits.in2.eq(Mux(op_quant, a_word & 0x7fffffff, a_word)),
        ]

        # The seed block: reciprocal reads |x|, rsqrt reads x; the
        # classify step has already excluded every input whose FPURec
        # estimate would leave the normal domain.
        m.d.comb += [
            frec.inp.valid.eq(un_seed_go | q_seed_go),
            frec.inp.bits.fn.eq(FPUOperator.REC),
            frec.inp.bits.fn_mod.eq(op_rsqrt),
            frec.inp.bits.rm.eq(RoundingMode.RNE),
            frec.inp.bits.in1.eq(
                Mux(op_quant, q_amax, Mux(op_rsqrt, un_x, un_x & 0x7fffffff))),
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
            | max_go | un_go | q_max_go
        with m.If(consume_a):
            m.d.sync += [
                awin.eq(awin >> 32),
                ahave.eq(ahave - 4),
                consumed.eq(consumed + 1),
                self.elements.eq(self.elements + 1),
            ]
        with m.If(binary_b & (elem_add_go | elem_mul_go)):
            m.d.sync += [bwin.eq(bwin >> 32), bhave.eq(bhave - 4)]

        m.d.sync += issued.eq(issued +
                              Mux(elem_add_go | elem_mul_go | un_go, 1, 0))
        m.d.sync += res_inflight.eq(res_inflight +
                                    Mux(elem_add_go | elem_mul_go
                                        | un_go, 1, 0) +
                                    Mux(q_m1_go & q_grp_new, 1, 0) +
                                    Mux(q_drain_go, 1, 0) -
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
                Mux(in_rdc, rdc_ready, in_elem & (flushing | final_partial
                                                  | quant_boundary))),
            self.tmem_w.bits.addr.eq(
                Mux(
                    in_rdc, o_base,
                    Mux(op_quant & (bidx >= qnbeats),
                        b_base + ((bidx - qnbeats) << lg),
                        o_base + (bidx << lg)))),
            self.tmem_w.bits.data.eq(
                Mux(in_rdc, Cat(rdc_val, Const(0, self.data_width - 32)),
                    beat_buf)),
        ]
        elem_w_fire = in_elem & self.tmem_w.fire
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
                        un_busy.eq(0),
                        un_wait.eq(0),
                        un_step.eq(0),
                        un_x.eq(0),
                        un_acc.eq(0),
                        un_tmp.eq(0),
                        un_f.eq(0),
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
                        q_amax.eq(0),
                        q_maxbusy.eq(0),
                        q_y.eq(0),
                        q_t.eq(0),
                        q_sc.eq(0),
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
                    with m.If(o_reduce):
                        m.next = 'REDUCE'
                    with m.Else():
                        m.next = 'ELEM'

            with m.State('ELEM'):
                with m.If(elem_last_beat):
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

        return m
