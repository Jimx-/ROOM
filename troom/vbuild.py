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


class VBuildDesc(HasTRoomParams, Record):
    """V-window build descriptor: window base, tile base, live slots."""

    def __init__(self, params, name=None, src_loc_at=0):
        HasTRoomParams.__init__(self, params)
        Record.__init__(self, [
            ('a_base', self.tmem_addr_width, Direction.FANOUT),
            ('r_base', self.tmem_addr_width, Direction.FANOUT),
            ('k', self.lg_act_k + 1, Direction.FANOUT),
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

        # Local window buffer: 64 slots x 32 words. One sync write
        # port (the fill) and one synchronous read port (the gather),
        # allowing block RAM inference. The phases are disjoint.
        buf = Memory(width=32, depth=VB_SLOTS * VB_SLOT_WORDS)
        buf_w = m.submodules.buf_w = buf.write_port()
        buf_r = m.submodules.buf_r = buf.read_port(transparent=False)

        # ------------------------------------------------------------------
        # Descriptor validation, on the offered combination.
        # ------------------------------------------------------------------
        offer = self.sink.bits
        a_end_o = offer.a_base + offer.k * VB_SLOT_BYTES
        r_end_o = offer.r_base + 64 * self.row_pad
        shape_bad = (offer.k == 0) | (offer.k > VB_SLOTS)
        align_bad = offer.a_base[:lg].any() | offer.r_base[:lg].any()
        range_bad = (a_end_o > self.tmem_bytes) \
            | (r_end_o > self.tmem_bytes)
        bad = shape_bad | align_bad | range_bad

        # Latched run state.
        a_base = Signal(aw)
        r_base = Signal(aw)
        live = Signal(7)

        # Phase membership, driven inside the FSM below.
        in_fill = Signal()
        in_gather = Signal()
        in_scale = Signal()
        in_quant = Signal()
        in_drain = Signal()
        running = in_fill | in_gather | in_scale | in_quant | in_drain

        # ------------------------------------------------------------------
        # Fill: one outstanding TMEM read at a time; each returned
        # beat is absorbed into the buffer one 32-bit word per cycle.
        # ------------------------------------------------------------------
        fill_word = Signal(12)  # words stored
        fill_total = Signal(12)  # words to store (live * 32)
        fill_beat = Signal(12)  # beats requested
        fill_data = Signal(self.data_width)
        fill_have = Signal()  # a beat is being absorbed
        fill_wcnt = Signal(range(bw + 1))
        rd_pend = Signal()

        # One beat at a time: the next request issues only after the
        # previous beat fully absorbed into the buffer, so a returned
        # beat can never clobber an absorption still in flight.
        more_fill = ~rd_pend & ~fill_have & (fill_beat * bw < fill_total)
        m.d.comb += [
            self.tmem_rreq.valid.eq(in_fill & more_fill),
            self.tmem_rreq.bits.addr.eq(a_base + (fill_beat << lg)),
            self.tmem_rresp.ready.eq(running),
        ]
        with m.If(in_fill & self.tmem_rreq.fire):
            m.d.sync += [rd_pend.eq(1), fill_beat.eq(fill_beat + 1)]
        with m.If(rd_pend & self.tmem_rresp.fire):
            m.d.sync += [
                rd_pend.eq(0),
                fill_have.eq(1),
                fill_data.eq(self.tmem_rresp.bits.data),
                fill_wcnt.eq(0),
            ]
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
        # Gather: one slot per cycle. The fold keeps the winning FP16
        # magnitude pattern; NaN patterns never win (>= 0x7c01), and
        # tail slots at or above `live` read as +0.
        # ------------------------------------------------------------------
        row = Signal(6)
        blk = Signal()
        gi = Signal(6)  # requests issued, including the terminal count 32
        gather_valid = Signal()
        gather_index = Signal(5)
        gather_live = Signal()
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
        pxmax16 = Signal(16)
        pxnan = Signal()
        prefetch_done = Signal()

        has_next = (blk == 0) | (row != 63)
        next_blk = Mux(blk == 0, 1, 0)
        next_row = Mux(blk == 0, row, row + 1)

        # Dead-tail early-out: the live prefix of block b is
        # clamp(live - 32*b, 0, 32). Slots at or above it quantize to
        # exactly zero bytes and fold +0 into the amax, so gathering
        # or streaming them is skippable work, never a numeric change.
        lo_live = Signal(6)  # live slots of block 0 (1..32)
        hi_live = Signal(6)  # live slots of block 1 (0..32)
        m.d.comb += [
            lo_live.eq(Mux(live > 32, 32, live)),
            hi_live.eq(Mux(live > 32, (live - 32)[:6], 0)),
        ]
        cur_live = Mux(blk == 0, lo_live, hi_live)
        nxt_live = Mux(blk == 0, hi_live, lo_live)

        # Buffer word j*32 + (d >> 1) is Cat(d[1:6], i, b); the
        # halfword is d & 1. Delay the index and live mask alongside
        # the RAM read; row/blk stay fixed until the last response lands.
        gather_go = in_gather & (gi < cur_live)
        prefetch_go = in_quant & has_next & (pgi < nxt_live)
        m.d.comb += [
            buf_r.addr.eq(
                Mux(prefetch_go, Cat(next_row[1:6], pgi[:5], next_blk),
                    Cat(row[1:6], gi[:5], blk))),
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
                gather_live.eq(Cat(gi[:5], blk) < live),
            ]
        with m.If(prefetch_go):
            m.d.sync += [
                pgi.eq(pgi + 1),
                pgather_index.eq(pgi[:5]),
                pgather_live.eq(Cat(pgi[:5], next_blk) < live),
            ]
        xh_now = Signal(16)
        mag_now = Signal(16)
        nan_now = Signal()
        m.d.comb += [
            xh_now.eq(Mux(gather_live, (buf_r.data >> (16 * row[0]))[:16], 0)),
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
                Mux(pgather_live, (buf_r.data >> (16 * next_row[0]))[:16], 0)),
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
        m.d.comb += [
            self.tmem_w.valid.eq(in_drain),
            self.tmem_w.bits.addr.eq(r_base + (row * self.row_pad) +
                                     (dbeat << lg)),
            self.tmem_w.bits.data.eq(drain_sr[:self.data_width]),
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
                        live.eq(offer.k[:7]),
                        fill_word.eq(0),
                        fill_total.eq(offer.k * VB_SLOT_WORDS),
                        fill_beat.eq(0),
                        fill_have.eq(0),
                        fill_wcnt.eq(0),
                        rd_pend.eq(0),
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
                with m.If((cur_live == 0)
                          | (gather_valid & (gather_index == cur_live - 1))):
                    # The last live slot folds this cycle (an empty
                    # block skips straight through); the scale
                    # sequencer arms on the next.
                    start_scale()
                    m.next = 'SCALE'

            with m.State('SCALE'):
                m.d.comb += in_scale.eq(1)
                with m.If(scale_done):
                    # The d16 pair is static per block: block 0 in
                    # word 0's low half (bytes 0-1), block 1 in word
                    # 8's high half (bytes 34-35).
                    with m.If(blk == 0):
                        m.d.sync += rowwords[0][:16].eq(d16)
                    with m.Else():
                        m.d.sync += rowwords[8][16:32].eq(d16)
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
                        m.d.sync += [
                            blk.eq(1),
                            cur_bank.eq(~cur_bank),
                            xmax16.eq(pxmax16),
                            xnan.eq(pxnan),
                            # Realign the byte cursor: a capped quant
                            # stream stopped at 2 + cur_live, but the
                            # next block's bytes start at its fixed
                            # 34-byte slot boundary.
                            wbyte.eq(34),
                        ]
                        start_scale()
                        m.next = 'SCALE'
                    with m.Else():
                        m.d.sync += drain_prefetched.eq(row != 63)
                        m.next = 'DRAINLOAD'

            with m.State('DRAINLOAD'):
                m.d.sync += [
                    drain_sr.eq(
                        Cat(rowbuf, Const(0,
                                          8 * self.row_pad - 8 * VB_ROW_RAW))),
                    dbeat.eq(0),
                ]
                m.next = 'DRAIN'

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
                        with m.If(row == 63):
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
        with m.If(in_drain):
            m.d.sync += self.drain_cycles.eq(self.drain_cycles + 1)
        with m.If(running):
            m.d.sync += self.run_cycles.eq(self.run_cycles + 1)
        return m
