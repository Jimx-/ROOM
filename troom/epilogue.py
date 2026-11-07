"""Per-lane FP32 epilogue for streaming matrix results.

Each lane owns conversion, scaling, accumulation and fold pipelines. There is
no shared arithmetic scheduler; independent lanes accept one record per cycle
when their row snapshots and result drain have capacity.

Per lane (all ``FPUFMA`` MUL/ADD semantics, rounding points and the
``isum_4`` fold order match the frozen ``mm_row_results`` contract):

    S,d (,mi*sum_a,dmin for Q4_K) -> i2f/f16w (2 cycles)
      -> m1 = R(S*d)      , vm1 = R(corr*dmin)     (mul, 3 cycles)
      -> u  = R(m1*s_act) , v   = R(vm1*s_act)     (mul, 3 cycles)
      -> t  = u | R(u - v)                          (sub, Q4_K only)
      -> bank[seg_k mod 4] = R(first_touch ? 0 : bank) + t   (acc)
      -> row_last: snapshot the four partials, fold
         R(R(p0+p2) + R(p1+p3)) -> result queue

Each unit serves exactly one producer and one consumer, so the lane is
a fixed free-running pipeline: a unit's registered output drives the
next unit's input one hop later and a context shift register
(``sh[l][k]``, pushed at cast issue, depth 14) carries the metadata to
the hops that need it -- offset 4 (the scale multiply), 7 (the second
multiplier landing: the Q4_K subtract issue and the non-Q4_K
accumulate issue), 10 (the subtract landing: the Q4_K accumulate issue
and the non-Q4_K accumulate landing) and 13 (the Q4_K accumulate
landing).  Bubbles flow between records; no stage ever stalls, so
there is nothing to schedule: records are accepted at sink rate and
the only flow control is row admission.

Row turnover needs no serialization here.  The shared design had to
hold a lane's next row behind ``row_busy`` until the final accumulate
issued because its units could reorder records; this pipeline is
strictly in order, which makes the bank hazards self-resolving: for
records r and r+1 the accumulate of r lands at least one cycle before
the accumulate of r+1 issues (constant unit latencies, >= 1 cycle
record spacing), so the row-end capture (at the final landing) always
reads the finished partials before the next row's first-touch landing
writes any bank, and the next row's segment-4 accumulate reads a bank
written by its own segment 0 exactly one cycle earlier.  Rows therefore
turn over with zero gap and admission gates only on row-context
capacity: a row-start record (``seg_k == 0``) is accepted while occupied
snapshot slots plus pending captures (``pend``, counted from the last
record's fire through its capture) leave a slot of the lane's
``epi_snap_bufs``-deep row queue -- every capture finds a free slot.
Short rows (one segment) throttle naturally to fold rate instead of
needing separate machinery.  (S4 short-row slice, 2026-11-02: the
historical two-buffer cap admitted at most one row past its
predecessor's capture, exposing the whole ~10-cycle capture/fold
round-trip on every row; at attention's ``nsegs == 2`` a row turns over
every two records and record intake throttled to ~4.9 cycles/record --
the FA epi_stalls/mm_credit class measured on the board.  The
row-context queue plus the pipelined fold scheduler hide that
round-trip behind the record stream; the per-row arithmetic and its
order are unchanged.)
"""

from amaranth import *
from amaranth.hdl.rec import Direction, Record
from amaranth.utils import log2_int

from room.consts import RoundingMode
from room.fpu import FPUFMA, FPFormat, FPUOperator

from troom.consts import Format, Status
from troom.convert import Fp16Widen, IntToFP32
from troom.matrix import MmSegment
from troom.types import HasTRoomParams

from roomsoc.interconnect.stream import Decoupled, Queue

# Context taps (offsets from the cast-issue cycle).  The push is
# registered, so ``sh[k]`` is visible one cycle after the cast: with
# the converters answering in 2 cycles and every ``FPUFMA`` in 3, the
# record's events land at cast+5/+8/+11/+14 and the taps sit one
# earlier.
_TAP_SCALE = 4  # u/v multiply issue reads s_act (event at +5)
_TAP_MULB = 7  # u/v landing: subtract issue / non-Q4_K acc issue (+8)
_TAP_SUB = 10  # subtract landing: Q4_K acc issue / non-Q4_K acc landing (+11)
_TAP_ACC = 13  # Q4_K accumulate landing (bank write / capture) (+14)
_SH_DEPTH = 14


def _shadow_layout(hp):
    return [
        ('v', 1, Direction.FANOUT),
        ('q4k', 1, Direction.FANOUT),
        ('s_act', 32, Direction.FANOUT),
        ('row', hp.rows_bits, Direction.FANOUT),
        ('seg_k', hp.lg_act_segs, Direction.FANOUT),
        ('row_last', 1, Direction.FANOUT),
    ]


class _Shadow(HasTRoomParams, Record):
    """One pipeline hop of a lane's record context."""

    def __init__(self, params, name=None, src_loc_at=0):
        HasTRoomParams.__init__(self, params)
        Record.__init__(self,
                        _shadow_layout(self),
                        name=name,
                        src_loc_at=src_loc_at + 1)


class Epilogue(HasTRoomParams, Elaboratable):
    """FP32 epilogue with per-lane dedicated pipelines (streaming path).

    ``start`` (with
    ``rows``/``r_base`` held valid) begins one matmul's drain when
    ``ready`` is asserted; ``done`` pulses once the final result beat
    commits to TMEM.  Row ``r``'s result lands at byte ``r_base + 4*r``.
    """

    def __init__(self, params):
        super().__init__(params)

        if self.data_width < 32:
            raise ValueError('epilogue data_width must be at least 32 bits')

        self.sink = Decoupled(MmSegment, self.params)
        self.tmem_w = Decoupled(
            Record, [('addr', self.tmem_addr_width, Direction.FANOUT),
                     ('data', self.data_width, Direction.FANOUT)])

        self.start = Signal()
        self.ready = Signal()
        self.rows = Signal(self.rows_bits)
        self.r_base = Signal(self.tmem_addr_width)

        self.done = Signal()
        self.status = Signal(Status)
        self.active = Signal()

        # Observation counters for record issue and result retirement.
        self.records = Signal(48)
        self.folds = Signal(48)
        self.write_beats = Signal(48)
        self.stall_cycles = Signal(48)
        self.active_cycles = Signal(48)
        self.cast_ops = Signal(48)
        self.mul_ops = Signal(48)
        self.add_ops = Signal(48)

    def elaborate(self, platform):
        m = Module()

        n = self.n_outputs
        lane_bits = max(log2_int(n), 1)
        bw = self.beat_bytes // 4
        lg_bw = log2_int(bw)

        def fpu(name):
            u = FPUFMA(32, FPFormat.S, latency=3)
            m.submodules[name] = u
            return u

        # Per-lane private banks.  Every unit is valid-only and never
        # stalls; its registered output is consumed combinationally by
        # exactly one downstream unit, so the whole lane advances one
        # hop per cycle with bubbles and no clock enables.
        i2f_a = []  # S -> fp32
        i2f_b = []  # Q4_K correction product -> fp32
        f16w_a = []  # d -> fp32
        f16w_b = []  # dmin -> fp32
        mul_a0 = []  # m1  = R(S * d)
        mul_a1 = []  # vm1 = R(corr * dmin)
        mul_b0 = []  # u   = R(m1 * s_act)
        mul_b1 = []  # v   = R(vm1 * s_act)
        add_sub = []  # t   = R(u - v), Q4_K only
        add_acc = []  # bank[idx] = R(bank' + t)
        add_fold = []  # snapshot fold
        for l in range(n):
            m.submodules[f'l{l}_i2fa'] = i2f = IntToFP32()
            m.submodules[f'l{l}_i2fb'] = i2fb = IntToFP32()
            m.submodules[f'l{l}_f16a'] = f16a = Fp16Widen()
            m.submodules[f'l{l}_f16b'] = f16b = Fp16Widen()
            i2f_a.append(i2f)
            i2f_b.append(i2fb)
            f16w_a.append(f16a)
            f16w_b.append(f16b)
            mul_a0.append(fpu(f'l{l}_ma0'))
            mul_a1.append(fpu(f'l{l}_ma1'))
            mul_b0.append(fpu(f'l{l}_mb0'))
            mul_b1.append(fpu(f'l{l}_mb1'))
            add_sub.append(fpu(f'l{l}_asub'))
            add_acc.append(fpu(f'l{l}_aacc'))
            add_fold.append(fpu(f'l{l}_afold'))

        res_q = []
        for l in range(n):
            q = Queue(self.mm_result_q, Record,
                      [('val', 32, Direction.FANOUT),
                       ('row', self.rows_bits, Direction.FANOUT)])
            res_q.append(q)
            m.submodules[f'epi_res_q{l}'] = q

        # Accumulator banks, fold snapshots and fold state per lane.
        bank = [[Signal(32, name=f'bank{l}{i}') for i in range(4)]
                for l in range(n)]
        # S4 short-row slice (2026-11-02): the fold side is a per-lane
        # circular ROW-CONTEXT queue of epi_snap_bufs slots. The
        # snapshot head is the next row to fold; captures append at
        # the tail; admission keeps (snapshots + pending captures)
        # below the queue depth so a capture always finds a free
        # slot. The single-context fold FSM (two snapshot buffers,
        # one row between its F1 and its final fold) exposed the whole
        # capture/fold round-trip on every row: at attention's
        # nsegs == 2 a row turns over every two records and record
        # intake throttled to ~4.9 cycles/record. Here F1/F2/FF of
        # consecutive rows pipeline on the lane's one fold adder
        # (3 issue slots per row, results in row order).
        E = self.epi_snap_bufs
        lg_e = log2_int(E)
        snap = [[[Signal(32, name=f'snap{b}{l}{i}') for i in range(4)]
                 for l in range(n)] for b in range(E)]
        snap_row = [[
            Signal(self.rows_bits, name=f'snap_row{b}{l}') for l in range(n)
        ] for b in range(E)]
        snap_hd = [Signal(lg_e, name=f'snap_hd{l}') for l in range(n)]
        snap_cnt = [
            Signal(range(E + 1), name=f'snap_cnt{l}') for l in range(n)
        ]
        # Rows whose last record has fired but whose capture has not
        # landed yet (bounded by the admission cap); a capture always
        # finds a free buffer.
        pend = [Signal(range(E + 1), name=f'pend{l}') for l in range(n)]
        # Fold contexts, one per in-flight fold, in row order:
        # fphase 1 = F1 issued (F2 due), 2 = F2 issued (halves
        # landing), 3 = both halves landed (final fold due),
        # 4 = final fold issued (landing retires the context). The
        # context queue is circular with the same depth as the
        # snapshot queue; the head context is the final-fold
        # candidate, so results enqueue in row order.
        ctx_hd = [Signal(lg_e, name=f'ctx_hd{l}') for l in range(n)]
        ctx_cnt = [Signal(range(E + 1), name=f'ctx_cnt{l}') for l in range(n)]
        fphase = [[Signal(3, name=f'fph{k}{l}') for k in range(E)]
                  for l in range(n)]
        fh1v = [[Signal(name=f'fh1v{k}{l}') for k in range(E)]
                for l in range(n)]
        fh1val = [[Signal(32, name=f'fh1val{k}{l}') for k in range(E)]
                  for l in range(n)]
        fh2v = [[Signal(name=f'fh2v{k}{l}') for k in range(E)]
                for l in range(n)]
        fh2val = [[Signal(32, name=f'fh2val{k}{l}') for k in range(E)]
                  for l in range(n)]
        frow = [[Signal(self.rows_bits, name=f'frow{k}{l}') for k in range(E)]
                for l in range(n)]
        # Result-queue reservations: the final fold checks room at
        # issue but enqueues at its landing three cycles later, so an
        # in-flight count keeps the reservation exact.
        enq_pend = [Signal(2, name=f'enq_pend{l}') for l in range(n)]
        # Landing tags delayed alongside the fold adder: which kind of
        # issue lands, and on which context slot.
        FC_NONE, FC_F1, FC_F2, FC_FF = 0, 1, 2, 3
        ftag = [[Signal(2 + lg_e, name=f'ftag{l}{k}') for k in range(3)]
                for l in range(n)]

        # Context shift chain: pushed at cast issue, tapped by offset.
        sh = [[
            _Shadow(self.params, name=f'sh{l}{k}') for k in range(_SH_DEPTH)
        ] for l in range(n)]

        #
        # Input record latch.  The whole record's casts issue on the
        # latch's first cycle (four converters per lane), so the latch
        # is always spent immediately and records can stream at one per
        # cycle.
        #
        in_rec = MmSegment(self.params)
        in_valid = Signal()
        spent = Signal()
        started = Signal()
        q4k = in_rec.fmt == Format.Q4_K
        lane_on = [in_rec.row_mask.bit_select(l, 1) for l in range(n)]
        m.d.comb += spent.eq(in_valid)

        # Q4_K's integer correction product: 6-bit unsigned minimum
        # times the activation segment sum, exact in signed 23 bits
        # (identical to the epilogue).
        cval = []
        for l in range(n):
            mi7 = Signal(signed(7), name=f'mi7_{l}')
            prod = Signal(signed(23), name=f'cprod{l}')
            m.d.comb += [
                mi7.eq(getattr(in_rec, f'mi{l}')),
                prod.eq(in_rec.sum_a * mi7),
            ]
            cval.append(prod)

        #
        # Row admission: row-context capacity only. ``occ`` counts
        # occupied snapshot slots plus rows pending capture; a
        # row-start record needs ``occ < epi_snap_bufs`` so the
        # invariant ``occ <= epi_snap_bufs`` holds and every capture
        # finds a free slot (its own pending entry guarantees the
        # queue is not full at the landing).
        #
        new_row_ready = Signal()
        occ_any = [Signal(range(E + 2), name=f'occ{l}') for l in range(n)]
        for l in range(n):
            m.d.comb += occ_any[l].eq(snap_cnt[l] + pend[l])
        row_block = Signal(n)
        for l in range(n):
            m.d.comb += row_block[l].eq(occ_any[l] >= E)
        m.d.comb += new_row_ready.eq(
            (self.sink.bits.seg_k != 0)
            | ~(row_block & self.sink.bits.row_mask).any())

        m.d.comb += self.sink.ready.eq(started & new_row_ready
                                       & (~in_valid | spent))
        with m.If(self.sink.fire):
            m.d.sync += [in_rec.eq(self.sink.bits), in_valid.eq(1)]
        with m.If(spent):
            m.d.sync += self.records.eq(self.records + 1)
            with m.If(~self.sink.fire):
                m.d.sync += in_valid.eq(0)

        #
        # Cast issues and the context push.  All four converters of a
        # lane issue together; the Q4_K pair only for Q4_K records.
        # Per-cycle op totals accumulate in Python (a second
        # ``.eq(sig + x)`` on the same signal would override, not add).
        #
        cast_terms = []
        for l in range(n):
            ca0 = in_valid & lane_on[l]
            ca1 = in_valid & lane_on[l] & q4k
            m.d.comb += [
                i2f_a[l].inp_valid.eq(ca0),
                i2f_a[l].inp.eq(getattr(in_rec, f'S{l}')),
                f16w_a[l].inp_valid.eq(ca0),
                f16w_a[l].inp.eq(getattr(in_rec, f'd{l}')),
                i2f_b[l].inp_valid.eq(ca1),
                i2f_b[l].inp.eq(Cat(cval[l], cval[l][-1].replicate(32 - 23))),
                f16w_b[l].inp_valid.eq(ca1),
                f16w_b[l].inp.eq(getattr(in_rec, f'dmin{l}')),
            ]
            cast_terms.append(ca0 + ca0 + ca1 + ca1)
            # Context pipeline advance and push.
            for k in range(_SH_DEPTH - 1):
                m.d.sync += sh[l][k + 1].eq(sh[l][k])
            m.d.sync += [
                sh[l][0].v.eq(in_valid & lane_on[l]),
                sh[l][0].q4k.eq(q4k),
                sh[l][0].s_act.eq(in_rec.s_act),
                sh[l][0].row.eq(getattr(in_rec, f'row{l}')),
                sh[l][0].seg_k.eq(in_rec.seg_k),
                sh[l][0].row_last.eq(in_rec.row_last),
            ]

        sh5 = [sh[l][_TAP_SCALE] for l in range(n)]
        sh8 = [sh[l][_TAP_MULB] for l in range(n)]
        sh11 = [sh[l][_TAP_SUB] for l in range(n)]
        sh14 = [sh[l][_TAP_ACC] for l in range(n)]

        #
        # Source products and scale applications.  Each multiplier's
        # operands are the previous stage's registered outputs; the
        # scale comes from the tap aligned with that hop.
        #
        mul_terms = []
        for l in range(n):
            m.d.comb += [
                mul_a0[l].inp.valid.eq(i2f_a[l].out_valid),
                mul_a0[l].inp.bits.fn.eq(FPUOperator.MUL),
                mul_a0[l].inp.bits.rm.eq(RoundingMode.RNE),
                mul_a0[l].inp.bits.fn_mod.eq(0),
                mul_a0[l].inp.bits.in3.eq(0),
                mul_a0[l].inp.bits.in1.eq(i2f_a[l].out),
                mul_a0[l].inp.bits.in2.eq(f16w_a[l].out),
                mul_a1[l].inp.valid.eq(i2f_b[l].out_valid),
                mul_a1[l].inp.bits.fn.eq(FPUOperator.MUL),
                mul_a1[l].inp.bits.rm.eq(RoundingMode.RNE),
                mul_a1[l].inp.bits.fn_mod.eq(0),
                mul_a1[l].inp.bits.in3.eq(0),
                mul_a1[l].inp.bits.in1.eq(i2f_b[l].out),
                mul_a1[l].inp.bits.in2.eq(f16w_b[l].out),
                mul_b0[l].inp.valid.eq(mul_a0[l].out.valid),
                mul_b0[l].inp.bits.fn.eq(FPUOperator.MUL),
                mul_b0[l].inp.bits.rm.eq(RoundingMode.RNE),
                mul_b0[l].inp.bits.fn_mod.eq(0),
                mul_b0[l].inp.bits.in3.eq(0),
                mul_b0[l].inp.bits.in1.eq(mul_a0[l].out.bits.data),
                mul_b0[l].inp.bits.in2.eq(sh5[l].s_act),
                mul_b1[l].inp.valid.eq(mul_a1[l].out.valid),
                mul_b1[l].inp.bits.fn.eq(FPUOperator.MUL),
                mul_b1[l].inp.bits.rm.eq(RoundingMode.RNE),
                mul_b1[l].inp.bits.fn_mod.eq(0),
                mul_b1[l].inp.bits.in3.eq(0),
                mul_b1[l].inp.bits.in1.eq(mul_a1[l].out.bits.data),
                mul_b1[l].inp.bits.in2.eq(sh5[l].s_act),
            ]
            mul_terms.append(i2f_a[l].out_valid + i2f_b[l].out_valid +
                             mul_a0[l].out.valid + mul_a1[l].out.valid)

        #
        # Q4_K correction subtract and the accumulate.  The accumulate
        # fires from the second multiplier's landing (non-Q4_K) or the
        # subtract's landing (Q4_K); the two can never collide because
        # one launch is one format.  First touch of a bank inside a row
        # adds literal zero, exactly as the shared epilogue (banks are
        # never cleared between rows: the next row's segment-4 read
        # follows its own segment-0 landing by construction).
        #
        add_terms = []
        sub_fire = [Signal(name=f'subfire{l}') for l in range(n)]
        acc_fire = [Signal(name=f'accfire{l}') for l in range(n)]
        acc_from_q4k = [Signal(name=f'accq{l}') for l in range(n)]
        for l in range(n):
            m.d.comb += [
                add_sub[l].inp.valid.eq(sub_fire[l]),
                add_sub[l].inp.bits.fn.eq(FPUOperator.ADD),
                add_sub[l].inp.bits.rm.eq(RoundingMode.RNE),
                add_sub[l].inp.bits.in2.eq(0),
                sub_fire[l].eq(mul_b0[l].out.valid & sh8[l].q4k),
            ]
            m.d.comb += [
                add_sub[l].inp.bits.in1.eq(mul_b0[l].out.bits.data),
                add_sub[l].inp.bits.in3.eq(mul_b1[l].out.bits.data),
                add_sub[l].inp.bits.fn_mod.eq(1),
            ]
            m.d.comb += [
                acc_from_q4k[l].eq(add_sub[l].out.valid),
                acc_fire[l].eq(acc_from_q4k[l]
                               | (mul_b0[l].out.valid & ~sh8[l].q4k)),
                add_acc[l].inp.valid.eq(acc_fire[l]),
                add_acc[l].inp.bits.fn.eq(FPUOperator.ADD),
                add_acc[l].inp.bits.rm.eq(RoundingMode.RNE),
                add_acc[l].inp.bits.fn_mod.eq(0),
                add_acc[l].inp.bits.in2.eq(0),
                add_acc[l].inp.bits.in3.eq(
                    Mux(acc_from_q4k[l], add_sub[l].out.bits.data,
                        mul_b0[l].out.bits.data)),
            ]
            add_terms.append(acc_fire[l] + sub_fire[l])
            # The issuing record's context: tap 8 for the non-Q4_K
            # fire, tap 11 for the Q4_K fire.
            idx_i = Mux(acc_from_q4k[l], sh11[l].seg_k[:2], sh8[l].seg_k[:2])
            first = Mux(acc_from_q4k[l], sh11[l].seg_k == sh11[l].seg_k[:2],
                        sh8[l].seg_k == sh8[l].seg_k[:2])
            m.d.comb += add_acc[l].inp.bits.in1.eq(
                Mux(first, 0,
                    Array(bank[l])[idx_i]))

        # Per-lane capture facts (used by the fold retire logic).
        cap_now = [Signal(name=f'capnow{l}') for l in range(n)]
        cap_buf = [Signal(lg_e, name=f'capbuf{l}') for l in range(n)]
        land_now = [Signal(name=f'landnow{l}') for l in range(n)]
        land_q4k = [Signal(name=f'landq{l}') for l in range(n)]
        for l in range(n):
            m.d.comb += [
                land_now[l].eq(add_acc[l].out.valid),
                # Invalid shadows retain the preceding launch's format.
                land_q4k[l].eq(sh14[l].v & sh14[l].q4k),
                # Captures append at the snapshot queue's tail; the
                # admission cap keeps the tail slot free.
                cap_buf[l].eq((snap_hd[l] + snap_cnt[l])[:lg_e]),
            ]

        # The fold reads the snapshot queue's head (the oldest row
        # whose capture has landed); the head advances when the F2
        # issue has consumed the slot's second half.
        rd_avail = [Signal(name=f'snap_rda{l}') for l in range(n)]
        rd_row = [
            Signal(self.rows_bits, name=f'snap_rdr{l}') for l in range(n)
        ]
        rd_snap = [[Signal(32, name=f'snap_rds{i}l{l}') for i in range(4)]
                   for l in range(n)]
        for l in range(n):
            m.d.comb += [
                rd_avail[l].eq(snap_cnt[l] != 0),
                rd_row[l].eq(
                    Array(snap_row[b][l] for b in range(E))[snap_hd[l]]),
            ]
            for i in range(4):
                m.d.comb += rd_snap[l][i].eq(
                    Array(snap[b][l][i] for b in range(E))[snap_hd[l]])

        # Fold launches per lane (used by the add_cnt accounting).
        # One adder, three issue kinds, strictly row-ordered effects:
        # the FINAL fold (FF) of the head context retires into the
        # result queue; the second half (F2) of the one slot that has
        # issued its first half releases that row's snapshot; the
        # first half (F1) of the next snapshot allocates the next
        # context. Priority FF > F2 > F1: FF unblocks the result queue
        # and frees contexts, F2 frees snapshot slots, and F1 is the
        # speculative one. Consecutive rows' folds pipeline at three
        # issue slots per row; each row's arithmetic is unchanged
        # (F1 = R(p0+p2), F2 = R(p1+p3), FF = R(F1+F2) from its own
        # snapshot, RNE at every step, landing order = issue order =
        # row order).
        fold_f1 = [Signal(name=f'ff1{l}') for l in range(n)]
        fold_f2 = [Signal(name=f'ff2{l}') for l in range(n)]
        fold_ff = [Signal(name=f'fff{l}') for l in range(n)]
        ff_land = [Signal(name=f'ffland{l}') for l in range(n)]
        f2_slot = [Signal(lg_e, name=f'f2slot{l}') for l in range(n)]
        f1_slot = [Signal(lg_e, name=f'f1slot{l}') for l in range(n)]

        for l in range(n):
            # The unique slot between its F1 and its F2 (F2 outranks
            # F1, so at most one exists).
            hit = Const(0)
            sel = Const(0, lg_e)
            for k in range(E):
                hit = hit | Mux(fphase[l][k] == 1, 1, 0)
                sel = Mux(fphase[l][k] == 1, k, sel)
            hd_phase = Array(fphase[l])[ctx_hd[l]]
            m.d.comb += [
                f2_slot[l].eq(sel),
                f1_slot[l].eq((ctx_hd[l] + ctx_cnt[l])[:lg_e]),
                fold_ff[l].eq((ctx_cnt[l] != 0) & (hd_phase == 3)
                              & ((res_q[l].count +
                                  enq_pend[l]) < self.mm_result_q)),
                fold_f2[l].eq(~fold_ff[l] & hit),
                fold_f1[l].eq(~fold_ff[l] & ~fold_f2[l] & rd_avail[l]
                              & (ctx_cnt[l] != E)),
                ff_land[l].eq(add_fold[l].out.valid
                              & (ftag[l][2][:2] == FC_FF)),
            ]
            ff_h1 = Array(fh1val[l][k] for k in range(E))[ctx_hd[l]]
            ff_h2 = Array(fh2val[l][k] for k in range(E))[ctx_hd[l]]
            m.d.comb += [
                add_fold[l].inp.valid.eq(fold_f1[l] | fold_f2[l]
                                         | fold_ff[l]),
                add_fold[l].inp.bits.fn.eq(FPUOperator.ADD),
                add_fold[l].inp.bits.rm.eq(RoundingMode.RNE),
                add_fold[l].inp.bits.fn_mod.eq(0),
                add_fold[l].inp.bits.in2.eq(0),
                add_fold[l].inp.bits.in1.eq(
                    Mux(fold_f1[l], rd_snap[l][0],
                        Mux(fold_f2[l], rd_snap[l][1], ff_h1))),
                add_fold[l].inp.bits.in3.eq(
                    Mux(fold_f1[l], rd_snap[l][2],
                        Mux(fold_f2[l], rd_snap[l][3], ff_h2))),
            ]
            add_terms.append(fold_f1[l] + fold_f2[l] + fold_ff[l])
            # Fold adder tag advance: which landing is F1/F2/FF, and
            # on which context slot.
            code = Mux(
                fold_ff[l], Const(FC_FF, 2),
                Mux(fold_f2[l], Const(FC_F2, 2),
                    Mux(fold_f1[l], Const(FC_F1, 2), Const(FC_NONE, 2))))
            slot_sel = Mux(fold_ff[l], ctx_hd[l],
                           Mux(fold_f2[l], f2_slot[l], f1_slot[l]))
            m.d.sync += [
                ftag[l][0].eq(Cat(code, slot_sel)),
                ftag[l][1].eq(ftag[l][0]),
                ftag[l][2].eq(ftag[l][1]),
            ]

        cast_cnt = Signal(range(4 * n + 1))
        mul_cnt = Signal(range(4 * n + 1))
        add_cnt = Signal(range(3 * n + 1))
        m.d.comb += [
            cast_cnt.eq(sum(cast_terms)),
            mul_cnt.eq(sum(mul_terms)),
            add_cnt.eq(sum(add_terms)),
        ]
        with m.If(cast_cnt.any()):
            m.d.sync += self.cast_ops.eq(self.cast_ops + cast_cnt)
        with m.If(mul_cnt.any()):
            m.d.sync += self.mul_ops.eq(self.mul_ops + mul_cnt)
        with m.If(add_cnt.any()):
            m.d.sync += self.add_ops.eq(self.add_ops + add_cnt)

        #
        # Accumulate landing: bank write, and the row-end capture.
        # The landing record sits at tap 14 (Q4_K) or tap 11; the
        # zero-fill rule (a partial above the row's last segment index
        # reads as zero) and the free-buffer choice are the epilogue's.
        #
        for l in range(n):
            # The landing record's fields: tap 14 (Q4_K) or tap 11.
            lrow_last = Mux(land_q4k[l], sh14[l].row_last, sh11[l].row_last)
            lrow = Mux(land_q4k[l], sh14[l].row, sh11[l].row)
            lseg_k = Mux(land_q4k[l], sh14[l].seg_k, sh11[l].seg_k)
            idx = Signal(2, name=f'lwidx{l}')
            m.d.comb += idx.eq(
                Mux(land_q4k[l], sh14[l].seg_k[:2], sh11[l].seg_k[:2]))
            m.d.comb += cap_now[l].eq(land_now[l] & lrow_last)
            with m.If(land_now[l]):
                for i in range(4):
                    with m.If(idx == i):
                        m.d.sync += bank[l][i].eq(add_acc[l].out.bits.data)
                with m.If(lrow_last):
                    for i in range(4):
                        word = Mux(idx == i, add_acc[l].out.bits.data,
                                   Mux(i > lseg_k, 0, bank[l][i]))
                        for b in range(E):
                            with m.If(cap_buf[l] == b):
                                m.d.sync += snap[b][l][i].eq(word)
                    for b in range(E):
                        with m.If(cap_buf[l] == b):
                            m.d.sync += snap_row[b][l].eq(lrow)
                    # Occupancy moves from ``pend`` to the snapshot
                    # queue; the single combined update below also
                    # folds the F2 release, so capture and release in
                    # one cycle cannot drop either term.

        # Pending-capture accounting: the fire-side increment and the
        # capture-side decrement MUST combine in one assignment -- two
        # separate ``.eq`` statements in the same cycle override (last
        # wins) and the counter drifts, over-admitting rows until a
        # capture clobbers an occupied snapshot (the shared epilogue's
        # "unified reservation updates" discipline).
        pend_inc = [Signal(name=f'pendinc{l}') for l in range(n)]
        for l in range(n):
            m.d.comb += pend_inc[l].eq(self.sink.fire
                                       & self.sink.bits.row_last
                                       & self.sink.bits.row_mask[l])
        for l in range(n):
            with m.If(pend_inc[l] | cap_now[l]):
                m.d.sync += pend[l].eq(pend[l] + pend_inc[l] - cap_now[l])

        # All lanes can capture in the same cycle: count, never
        # double-drive the shared counter.
        cap_cnt = Signal(range(n + 1))
        m.d.comb += cap_cnt.eq(sum(cap_now))
        with m.If(cap_cnt.any()):
            m.d.sync += self.folds.eq(self.folds + cap_cnt)

        #
        # Fold sequencing per lane: F1 allocates the next context and
        # latches its row, F2 releases the row's snapshot slot, and
        # the FF landing enqueues the row's result and retires the
        # context. Counter updates that can collide in one cycle are
        # single combined assignments (capture append vs F2 release;
        # F1 allocation vs FF-landing retirement; FF reservation vs FF
        # landing) -- two separate ``.eq`` statements would override.
        #
        for l in range(n):
            # Snapshot queue: captures append (cap_now), F2 issues
            # release (fold_f2).
            m.d.sync += snap_cnt[l].eq(
                Mux(cap_now[l] | fold_f2[l],
                    snap_cnt[l] + cap_now[l] - fold_f2[l], snap_cnt[l]))
            with m.If(fold_f2[l]):
                m.d.sync += snap_hd[l].eq((snap_hd[l] + 1)[:lg_e])
            # Context queue: F1 allocates (fold_f1), the FF landing
            # retires (ff_land).
            m.d.sync += ctx_cnt[l].eq(
                Mux(fold_f1[l] | ff_land[l],
                    ctx_cnt[l] + fold_f1[l] - ff_land[l], ctx_cnt[l]))
            with m.If(ff_land[l]):
                m.d.sync += ctx_hd[l].eq((ctx_hd[l] + 1)[:lg_e])
            # Result-queue reservations: FF issues reserve a slot,
            # FF landings release it.
            m.d.sync += enq_pend[l].eq(
                Mux(fold_ff[l] | ff_land[l],
                    enq_pend[l] + fold_ff[l] - ff_land[l], enq_pend[l]))

            with m.If(fold_f1[l]):
                for k in range(E):
                    with m.If(f1_slot[l] == k):
                        m.d.sync += [
                            fphase[l][k].eq(1),
                            fh1v[l][k].eq(0),
                            fh2v[l][k].eq(0),
                            frow[l][k].eq(rd_row[l]),
                        ]
            with m.If(fold_f2[l]):
                for k in range(E):
                    with m.If(f2_slot[l] == k):
                        m.d.sync += fphase[l][k].eq(2)
            # The final fold moves its context to the in-flight state
            # at ISSUE, so exactly one FF per context can exist (a
            # re-issue while the landing is pending would duplicate
            # the enqueue and underflow the context queue).
            with m.If(fold_ff[l]):
                for k in range(E):
                    with m.If(ctx_hd[l] == k):
                        m.d.sync += fphase[l][k].eq(4)

            with m.If(add_fold[l].out.valid):
                kind = ftag[l][2][:2]
                slot = ftag[l][2][2:]
                for k in range(E):
                    with m.If(slot == k):
                        with m.If(kind == FC_F1):
                            m.d.sync += [
                                fh1v[l][k].eq(1),
                                fh1val[l][k].eq(add_fold[l].out.bits.data),
                            ]
                        with m.If(kind == FC_F2):
                            m.d.sync += [
                                fh2v[l][k].eq(1),
                                fh2val[l][k].eq(add_fold[l].out.bits.data),
                            ]
                            # F1 always lands first (one in-order
                            # adder), so both halves are live now.
                            with m.If(fh1v[l][k]):
                                m.d.sync += fphase[l][k].eq(3)
                        with m.If(kind == FC_FF):
                            m.d.sync += [
                                fphase[l][k].eq(0),
                                fh1v[l][k].eq(0),
                                fh2v[l][k].eq(0),
                            ]
                            m.d.comb += [
                                res_q[l].enq.valid.eq(1),
                                res_q[l].enq.bits.val.eq(
                                    add_fold[l].out.bits.data),
                                res_q[l].enq.bits.row.eq(frow[l][k]),
                            ]

        #
        # Packer: results arrive in strict row order; shift them into
        # full-width beats and flush a partial final beat zero-padded
        # (the frozen row-fold order).
        #
        rows_l = Signal(self.rows_bits)
        r_base_l = Signal(self.tmem_addr_width)
        prow = Signal(self.rows_bits)
        wcnt = Signal(range(bw + 1))
        bidx = Signal(self.rows_bits + 1)
        beats_done = Signal(self.rows_bits + 1)
        beat_buf = Signal(self.data_width)
        total = (rows_l + (bw - 1)) >> lg_bw

        lane = prow[:lane_bits] if n > 1 else Const(0)
        take = Array(q.deq.valid for q in res_q)[lane]
        take &= Array(q.deq.bits.row for q in res_q)[lane] == prow
        take_val = Array(q.deq.bits.val for q in res_q)[lane]
        flushing = wcnt == bw
        final_partial = (prow == rows_l) & (wcnt != 0)
        take &= started & ~flushing & (prow != rows_l)

        beat_next = Mux(take, beat_buf | (take_val << (32 * wcnt)), beat_buf)
        m.d.comb += [
            self.tmem_w.valid.eq(flushing | final_partial),
            self.tmem_w.bits.addr.eq(r_base_l + (bidx << self.lg_beat)),
            self.tmem_w.bits.data.eq(beat_next),
        ]
        for l, q in enumerate(res_q):
            m.d.comb += q.deq.ready.eq(take & (lane == l))

        last_beat = self.tmem_w.fire & (beats_done + 1 == total)
        with m.If(self.tmem_w.fire):
            m.d.sync += [
                beats_done.eq(beats_done + 1),
                self.write_beats.eq(self.write_beats + 1),
                bidx.eq(bidx + 1),
                wcnt.eq(0),
                beat_buf.eq(0),
            ]
        with m.If(take):
            m.d.sync += [
                prow.eq(prow + 1),
                beat_buf.eq(beat_next),
                wcnt.eq(wcnt + 1)
            ]

        #
        # Run control (a lane is quiet when its
        # context chain, fold state, snapshots, pending captures and
        # result queue are all empty).
        #
        drained = ~started & ~in_valid & (wcnt == 0) & (prow == rows_l)
        for l in range(n):
            quiet = Const(1)
            for k in range(_SH_DEPTH):
                quiet = quiet & ~sh[l][k].v
            quiet = quiet & (snap_cnt[l] == 0) & (pend[l] == 0) \
                & (ctx_cnt[l] == 0) & (enq_pend[l] == 0) \
                & (res_q[l].count == 0)
            drained = drained & quiet

        m.d.comb += self.ready.eq(drained)
        begin = self.start & self.ready
        with m.If(begin):
            m.d.sync += [
                started.eq(1),
                rows_l.eq(self.rows),
                r_base_l.eq(self.r_base),
                prow.eq(0),
                wcnt.eq(0),
                bidx.eq(0),
                beats_done.eq(0),
                beat_buf.eq(0),
            ]
        with m.If(last_beat):
            m.d.sync += started.eq(0)

        m.d.comb += [
            self.done.eq(last_beat),
            self.status.eq(Status.OK),
            self.active.eq(started),
        ]
        with m.If(started):
            m.d.sync += self.active_cycles.eq(self.active_cycles + 1)
        with m.If(started & self.sink.valid & ~self.sink.fire):
            m.d.sync += self.stall_cycles.eq(self.stall_cycles + 1)

        # Pysim-only observation points:
        # plain Python references to existing signals, no statements
        # added; consumed by bring-up traces.
        self.dbg = dict(
            started=started,
            in_valid=in_valid,
            spent=spent,
            new_row_ready=new_row_ready,
            ca0=[in_valid & lane_on[l] for l in range(n)],
            i2fa_ov=[i2f_a[l].out_valid for l in range(n)],
            ma0_iv=[mul_a0[l].inp.valid for l in range(n)],
            ma0_ov=[mul_a0[l].out.valid for l in range(n)],
            mb0_iv=[mul_b0[l].inp.valid for l in range(n)],
            mb0_ov=[mul_b0[l].out.valid for l in range(n)],
            acc_fire=acc_fire,
            acc_ov=[add_acc[l].out.valid for l in range(n)],
            sh8v=[sh[l][_TAP_MULB].v for l in range(n)],
            sh11v=[sh[l][_TAP_SUB].v for l in range(n)],
            sh14v=[sh[l][_TAP_ACC].v for l in range(n)],
            enq_pend=enq_pend,
            fold_st=fphase,
            snap_cnt=snap_cnt,
            ctx_cnt=ctx_cnt,
            pend=pend,
            resq_count=[res_q[l].count for l in range(n)],
            prow=prow,
            wcnt=wcnt,
        )

        return m
