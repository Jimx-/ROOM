"""FP32 scale/correction/accumulate epilogue for the troom matrix unit.

Consumes the chain-end ``MmSegment`` stream (launch order, one record
per valid slot) and commits each output row's FP32 result into TMEM as
packed FP32 words in full beats, with the final beat's unused lanes
zeroed.

Per segment record and output lane the arithmetic is, with ``R`` the
FP32 round-to-nearest-even:

.. code-block:: text

    u = R(R(float(S) * d) * s_act)
    v = R(R(float(mi * sum_a) * dmin) * s_act)   # Q4_K only
    t = R(u - v) | u
    p[k mod 4] = R(p[k mod 4] + t)
    result = R(R(p[0] + p[2]) + R(p[1] + p[3]))

Every multiply and add is a separate rounding point; none are fused.
The float(S) and float(mi * sum_a) integer-to-float conversions are
exact (segment partials are bounded by 2^24), the FP16 d/dmin to FP32
conversions are exact, and the signed mi * sum_a product is exact in
signed 23 bits.

The FP32 operations reuse the CPU floating-point blocks: one
``FPUCastMulti`` performs the int32 and FP16 conversions, one
``FPUFMA`` serves every multiply, and one ``FPUFMA`` serves every
add/subtract. An operand-ready scheduler issues at most one op per
cycle per block and captures results ``latency`` cycles later through
per-unit destination tags, so the stages are elastic: a record costs a
handful of cycles (bounded by its conversion count) instead of one, and
the matrix unit's output-credit backpressure absorbs the difference.
Because the shared units have multi-cycle latency, every issue guard
must also cover results still in flight, not just staged values: the
u/v correction slots hold their ``u_wait``/``v_wait`` reservations from
issue until the subtract consumes them, the t slot holds ``t_busy``
until its value lands, the m1/vm1 source products hold ``m1_busy``/
``vm1_busy`` until they land, and each conversion destination is a
two-entry FIFO whose ``rsv`` count (occupied slots plus casts in
flight) is the cast issue gate, so two conversions in a row can never
race for one landing slot. Multiplies round as R(a*b + 0.0), which
canonicalizes a negative-zero product to +0; that never changes a row's
final bits because the four partials start at +0 and +0 plus (or
minus) a zero never yields a negative zero.

Row state: each lane keeps the four ``isum_4`` partials of its current
row in registers. The first segment reserves the lane's row context
until the row's final segment captures the partials into one of two
fold snapshots; a subsequent row may then be accepted (its own
row-start record additionally requires a free snapshot buffer), so the
previous row's fold overlaps the next row's segments. A fold snapshot
folds through the shared adder (p0+p2, p1+p3, then their sum) into a
per-lane result queue, one row in flight per lane at a time beyond the
snapshots; the ff issue gate reserves queue capacity and the global
``ff_busy`` serializes final folds, so the result enqueue at the ff
landing cannot fail. Each lane's t
stage admits one value at a time -- the u (or subtract) op waits until
the previous t was consumed -- so backpressure can never overrun a
staged value. Results leave in strict row order, so the packer shifts
them LSB-first into full TMEM beats.
"""

from enum import IntEnum

from amaranth import *
from amaranth.hdl.rec import Direction, Record
from amaranth.utils import log2_int

from room.consts import RoundingMode
from room.fpu import FPUFMA, FPUCastMulti, FPFormat, FPUOperator, IntFormat

from troom.consts import Format, Status
from troom.matrix import MmSegment
from troom.types import HasTRoomParams

from roomsoc.interconnect.stream import Decoupled, Queue


class Dest(IntEnum):
    """Destination tags for the per-unit in-flight op queues."""

    FS = 0
    D = 1
    FC = 2
    DM = 3
    M1 = 4
    U = 5
    VM1 = 6
    V = 7
    T = 8
    ACC = 9
    F1 = 10
    F2 = 11
    FF = 12


def _ctx_layout(hp):
    """Metadata carried beside every in-flight value."""
    return [
        ('s_act', 32, Direction.FANOUT),
        ('row', hp.rows_bits, Direction.FANOUT),
        ('seg_k', hp.lg_act_segs, Direction.FANOUT),
        ('row_last', 1, Direction.FANOUT),
        ('q4k', 1, Direction.FANOUT),
    ]


class Ctx(HasTRoomParams, Record):
    """Record context accompanying a staged value."""

    def __init__(self, params, name=None, src_loc_at=0):
        HasTRoomParams.__init__(self, params)
        Record.__init__(self,
                        _ctx_layout(self),
                        name=name,
                        src_loc_at=src_loc_at + 1)


class _Stage:
    """One staged value: payload, availability, and its context."""

    def __init__(self, hp, width, name):
        self.val = Signal(width, name=f'{name}_val')
        self.avail = Signal(name=f'{name}_avail')
        self.ctx = Ctx(hp.params, name=f'{name}_ctx')


class _CastStage:
    """Two-entry FIFO of one conversion result, in record order.

    M3b stage 2: with a single entry, a conversion occupied its
    destination from landing until the consuming multiply issued --
    its partner's landing latency plus the multiply's issue slot --
    capping the acceptance interval at six cycles for two-lane Q8_0
    whatever the cast issue order. Two entries let the next record's
    conversion land while the previous pair waits for its multiply.

    Ordering is structural: a record casts to a destination at most
    once, and only before it leaves the input latch (``spent``), so
    arrivals are in record order; the consuming multiply pops both of
    its operand stages in the same cycle, so the two FIFOs an ``m1``
    or ``vm1`` reads stay pair-aligned -- their heads always describe
    the same record. ``rsv`` counts occupied slots plus casts still in
    flight and is the cast issue gate, generalizing the old
    single-entry ``cast_pend`` reservation: at most two reservations
    exist per destination, and a landing therefore always finds at
    most one slot occupied.
    """

    def __init__(self, hp, name):
        self.e = [_Stage(hp, 32, f'{name}{i}') for i in range(2)]
        self.rsv = Signal(2, name=f'{name}_rsv')

    def any_avail(self):
        return self.e[0].avail | self.e[1].avail


class _Tag:
    """One stage of a unit's in-flight destination pipeline."""

    def __init__(self, hp, name):
        self.v = Signal(name=f'{name}_v')
        self.dest = Signal(Dest, name=f'{name}_dest')
        self.lane = Signal(max(log2_int(hp.n_outputs), 1), name=f'{name}_lane')
        self.ctx = Ctx(hp.params, name=f'{name}_ctx')


class Epilogue(HasTRoomParams, Elaboratable):
    """FP32 epilogue draining the matrix unit's segment stream.

    ``start`` (with ``rows``/``r_base`` held valid) begins one
    matmul's drain when ``ready`` is asserted. The caller must wait
    for ``ready`` before starting another matmul. ``done`` pulses
    once the final result beat commits to
    TMEM, which is the matmul's true completion point. ``r_base``
    must be beat aligned with room for every row's FP32 word; row
    ``r``'s result lands at byte ``r_base + 4*r``.
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

        # Observation counters.
        self.records = Signal(48)
        self.folds = Signal(48)
        self.write_beats = Signal(48)
        self.stall_cycles = Signal(48)
        # Cycle-breakdown window (M3b): cycles the epilogue is
        # running, and issues on its three shared FPU blocks, so unit
        # utilization is measurable against the active window. Cast
        # ops count int32/FP16 -> FP32 conversions, mul ops the u/v
        # product chain, add ops accumulations, corrections, and row
        # folds alike.
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

        # Include the FPU input register so scheduling/operand selection
        # does not share a cycle with floating-point alignment/arithmetic.
        cast = m.submodules.cast = FPUCastMulti(32, FPFormat.S, latency=3)
        fmul = m.submodules.fmul = FPUFMA(32, FPFormat.S, latency=3)
        fadd = m.submodules.fadd = FPUFMA(32, FPFormat.S, latency=3)

        res_q = []
        for l in range(n):
            q = Queue(self.mm_result_q, Record,
                      [('val', 32, Direction.FANOUT),
                       ('row', self.rows_bits, Direction.FANOUT)])
            res_q.append(q)
            m.submodules[f'epi_res_q{l}'] = q

        #
        # Per-lane staged values and row state. The four conversion
        # destinations are two-entry FIFOs (_CastStage); everything a
        # multiply or add produces stays single-entry.
        #
        fS = [_CastStage(self, f'fS{l}') for l in range(n)]
        dwt = [_CastStage(self, f'dw{l}') for l in range(n)]
        fC = [_CastStage(self, f'fC{l}') for l in range(n)]
        dmn = [_CastStage(self, f'dm{l}') for l in range(n)]
        m1 = [_Stage(self, 32, f'm1_{l}') for l in range(n)]
        vm1 = [_Stage(self, 32, f'vm1_{l}') for l in range(n)]
        uv = [_Stage(self, 32, f'u{l}') for l in range(n)]
        vv = [_Stage(self, 32, f'v{l}') for l in range(n)]
        tt = [_Stage(self, 32, f't{l}') for l in range(n)]
        f1 = [_Stage(self, 32, f'f1_{l}') for l in range(n)]
        f2 = [_Stage(self, 32, f'f2_{l}') for l in range(n)]

        bank = [[Signal(32, name=f'bank{l}{i}') for i in range(4)]
                for l in range(n)]
        # M3b stage 3: two fold snapshots per lane, so the next row's
        # records are accepted while the previous row's snapshot still
        # drains through the shared adder. A snapshot carries its own
        # context: by the time the fold halves launch, the lane's t
        # stage may already hold a later record. ``snap_rd`` names the
        # older unretired buffer the fold scheduler reads; a capture
        # takes the free buffer (acceptance of a row-start record
        # guarantees one). At most two snapshots per lane can be
        # unretired: the row-start gate requires a free buffer, and
        # each lane folds in order, so under result backpressure a
        # third row simply waits -- exactly the single-snapshot
        # behavior, one row further out.
        snap = [[[Signal(32, name=f'snap{b}{l}{i}') for i in range(4)]
                 for l in range(n)] for b in range(2)]
        snap_ctx = [[
            Ctx(self.params, name=f'snapctx{b}{l}') for l in range(n)
        ] for b in range(2)]
        snap_avail = [[Signal(name=f'snap_avail{b}{l}') for l in range(n)]
                      for b in range(2)]
        snap_done = [[Signal(2, name=f'snap_done{b}{l}') for l in range(n)]
                     for b in range(2)]
        snap_rd = [Signal(name=f'snap_rd{l}') for l in range(n)]

        #
        # Input record latch. The record is held until every conversion
        # it needs has issued; masked lanes need none.
        #
        in_rec = MmSegment(self.params, name='epi_in')
        in_valid = Signal()
        q4k = in_rec.fmt == Format.Q4_K
        lane_on = [in_rec.row_mask.bit_select(l, 1) for l in range(n)]
        cast_done = [[Signal(name=f'cd{k}{l}') for l in range(n)]
                     for k in range(4)]
        need = [[lane_on[l] & (Const(1) if k < 2 else q4k) for l in range(n)]
                for k in range(4)]

        # A conversion may only issue while its destination FIFO holds
        # fewer than two reservations (occupied slots plus casts still
        # in flight, ``_CastStage.rsv``): the cast unit captures
        # results unconditionally, so an over-issued destination could
        # land a value with no slot for it. Two reservations are the
        # capacity of the two-entry stage; at a landing at most one
        # slot is occupied (the gate ran before the issue), so a
        # landing never displaces an unconsumed value.
        casts_left = Const(0)
        pend_cnt = Const(0)
        for k in range(4):
            for l in range(n):
                pend_need = need[k][l] & ~cast_done[k][l]
                casts_left = casts_left | pend_need
                pend_cnt = pend_cnt + pend_need.bool()
        # M3b scheduling, stage 1: the record leaves the latch the
        # cycle its final required conversion ISSUES, not one cycle
        # later. When exactly one needed cast remains and the shared
        # unit picks it this cycle, the sink may replace the record in
        # that same cycle, and the next record's casts can begin the
        # following cycle -- removing the pop and re-accept cycles
        # from the minimum acceptance interval. The retiring record's
        # operands are read combinationally from the latch during its
        # issue cycle, so the end-of-cycle overwrite is safe; its
        # landing stays protected by cast_pend, and the incoming
        # record's cast_done reset must win over the issue's set (see
        # the cast scheduler). A deeper input FIFO would not remove
        # these cycles: the latch is each record's only operand home.
        spent = Signal(name='epi_spent')

        started = Signal()
        row_busy = Signal(n)
        # A row-start record (seg_k == 0) needs its lanes' row contexts
        # free AND a free snapshot buffer per masked lane: the row's
        # final accumulate will capture into it. row_busy retires at
        # that capture (the fold's queue capacity is reserved by the
        # ff issue gate and ff_busy serialization), so the next row
        # overlaps the previous row's fold instead of waiting for its
        # result to enqueue.
        row_block = Signal(n)
        for l in range(n):
            m.d.comb += row_block[l].eq(row_busy[l] | (snap_avail[0][l]
                                                       & snap_avail[1][l]))
        new_row_ready = (self.sink.bits.seg_k
                         != 0) | ~(row_block & self.sink.bits.row_mask).any()
        m.d.comb += self.sink.ready.eq(started & new_row_ready
                                       & (~in_valid | spent))
        with m.If(self.sink.fire):
            m.d.sync += [in_rec.eq(self.sink.bits), in_valid.eq(1)]
            with m.If(self.sink.bits.seg_k == 0):
                for l in range(n):
                    with m.If(self.sink.bits.row_mask[l]):
                        m.d.sync += row_busy[l].eq(1)
            for k in range(4):
                for l in range(n):
                    m.d.sync += cast_done[k][l].eq(0)
        with m.If(spent):
            m.d.sync += self.records.eq(self.records + 1)
            # Without a replacement the latch empties, as before.
            with m.If(~self.sink.fire):
                m.d.sync += in_valid.eq(0)

        # The Q4_K correction product mi * sum_a: 6-bit unsigned times
        # 16-bit signed, exact in signed 23 bits.
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
        # Packer: results arrive in strict row order; shift them into
        # full-width beats and flush a partial final beat zero-padded.
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
        # Run control.
        #
        any_avail = Const(0)
        for st in fS + dwt + fC + dmn:
            any_avail = any_avail | st.any_avail()
        for st in m1 + vm1 + uv + vv + tt + f1 + f2:
            any_avail = any_avail | st.avail
        drained = ~started & ~in_valid & ~any_avail & (wcnt == 0) \
            & (prow == rows_l)
        for l in range(n):
            drained = drained & ~snap_avail[0][l] & ~snap_avail[1][l] \
                & (res_q[l].count == 0)

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

        #
        # Scheduler scaffolding: per-unit in-flight tag pipelines and the
        # issued op's destination mux.
        #
        cast_tag = [_Tag(self, f'ctag{i}') for i in range(cast.latency)]
        mul_tag = [_Tag(self, f'mtag{i}') for i in range(fmul.latency)]
        add_tag = [_Tag(self, f'atag{i}') for i in range(fadd.latency)]

        def push_tag(tags, found, new_dest, new_lane, new_ctx):
            # These Valid-only FPUs never stall internally. Delay tags
            # alongside their operands, including bubbles, so adding an
            # input stage cannot overflow a two-entry metadata queue.
            for dst, src in zip(tags, tags[1:]):
                m.d.sync += dst.v.eq(src.v)
                with m.If(src.v):
                    m.d.sync += [
                        dst.dest.eq(src.dest),
                        dst.lane.eq(src.lane),
                        dst.ctx.eq(src.ctx)
                    ]
            m.d.sync += [
                tags[-1].v.eq(found),
            ]
            with m.If(found):
                m.d.sync += [
                    tags[-1].dest.eq(new_dest),
                    tags[-1].lane.eq(new_lane),
                    tags[-1].ctx.eq(new_ctx),
                ]

        def pick(ready, sel, found):
            """Priority selection: the first ready op wins."""
            for i in reversed(range(len(ready))):
                with m.If(ready[i]):
                    m.d.comb += [found.eq(1), sel.eq(i)]

        #
        # Shared conversion unit: int32 -> FP32 (S and Q4_K's mi*sum_a)
        # and NaN-boxed FP16 -> FP32 (d, dmin). Issue priority is
        # lane-major with the FP16 member of each consumed pair first
        # (d before S, dmin before the Q4_K correction): the pair's
        # integer conversion then completes last, so the consuming
        # multiply (m1/vm1) issues as soon as both stages hold values,
        # freeing the single-entry destinations earlier than the old
        # S-first order.
        #
        cast_ops_list = [
            op for l in range(n) for op in ((1, l), (0, l), (3, l), (2, l))
        ]
        # Indexed by the cast number k: 0=FS 1=D 2=FC 3=DM.
        cast_dest = [fS, dwt, fC, dmn]
        cast_ready = [
            in_valid & need[k][l] & ~cast_done[k][l]
            & (cast_dest[k][l].rsv != 2) for k, l in cast_ops_list
        ]
        cast_sel = Signal(range(len(cast_ops_list)))
        cast_found = Signal()
        pick(cast_ready, cast_sel, cast_found)

        # Stage 1 replacement condition: every needed cast issued in an
        # earlier cycle, or the one remaining pending cast is exactly
        # the op the unit picked this cycle (a ready op is always a
        # pending one).
        m.d.comb += spent.eq(in_valid & ((pend_cnt == 0)
                                         | (cast_found & (pend_cnt == 1))))

        cast_ctx = [Ctx(self.params, name=f'ccx{l}') for l in range(n)]
        for l in range(n):
            m.d.comb += [
                cast_ctx[l].s_act.eq(in_rec.s_act),
                cast_ctx[l].row.eq(getattr(in_rec, f'row{l}')),
                cast_ctx[l].seg_k.eq(in_rec.seg_k),
                cast_ctx[l].row_last.eq(in_rec.row_last),
                cast_ctx[l].q4k.eq(q4k),
            ]

        m.d.comb += [
            cast.inp.valid.eq(cast_found),
            cast.inp.bits.rm.eq(RoundingMode.RNE),
            cast.inp.bits.fn_mod.eq(0),
        ]
        for i, (k, l) in enumerate(cast_ops_list):
            with m.If(cast_sel == i):
                if k == 0:
                    m.d.comb += [
                        cast.inp.bits.fn.eq(FPUOperator.I2F),
                        cast.inp.bits.int_fmt.eq(IntFormat.INT32),
                        cast.inp.bits.dst_fmt.eq(FPFormat.S),
                        cast.inp.bits.in1.eq(getattr(in_rec, f'S{l}')),
                    ]
                elif k == 1:
                    m.d.comb += [
                        cast.inp.bits.fn.eq(FPUOperator.F2F),
                        cast.inp.bits.src_fmt.eq(FPFormat.H),
                        cast.inp.bits.dst_fmt.eq(FPFormat.S),
                        cast.inp.bits.in1.eq(
                            Cat(getattr(in_rec, f'd{l}'), Const(0xFFFF, 16))),
                    ]
                elif k == 2:
                    m.d.comb += [
                        cast.inp.bits.fn.eq(FPUOperator.I2F),
                        cast.inp.bits.int_fmt.eq(IntFormat.INT32),
                        cast.inp.bits.dst_fmt.eq(FPFormat.S),
                        cast.inp.bits.in1.eq(
                            Cat(cval[l], cval[l][-1].replicate(32 - 23))),
                    ]
                else:
                    m.d.comb += [
                        cast.inp.bits.fn.eq(FPUOperator.F2F),
                        cast.inp.bits.src_fmt.eq(FPFormat.H),
                        cast.inp.bits.dst_fmt.eq(FPFormat.S),
                        cast.inp.bits.in1.eq(
                            Cat(getattr(in_rec, f'dmin{l}'), Const(0xFFFF,
                                                                   16))),
                    ]
        with m.If(cast_found):
            m.d.sync += self.cast_ops.eq(self.cast_ops + 1)
            for i, (k, l) in enumerate(cast_ops_list):
                with m.If(cast_sel == i):
                    # On a same-cycle record replacement the fire
                    # block's cast_done reset describes the incoming
                    # record and must win over this set (assignment
                    # order alone does not decide it). The destination
                    # reservation is raised in the unified rsv update
                    # under Result routing.
                    m.d.sync += cast_done[k][l].eq(~self.sink.fire)

        c_dest = Signal(Dest, name='cdest')
        c_lane = Signal(lane_bits, name='clane')
        c_ctx = Ctx(self.params, name='cctx')
        m.d.comb += [
            c_dest.eq([Dest.FS, Dest.D, Dest.FC,
                       Dest.DM][cast_ops_list[0][0]]),
            c_lane.eq(cast_ops_list[0][1]),
            c_ctx.eq(cast_ctx[cast_ops_list[0][1]]),
        ]
        for i, (k, l) in enumerate(cast_ops_list):
            with m.If(cast_sel == i):
                m.d.comb += [
                    c_dest.eq([Dest.FS, Dest.D, Dest.FC, Dest.DM][k]),
                    c_lane.eq(l),
                    c_ctx.eq(cast_ctx[l])
                ]
        push_tag(cast_tag, cast_found, c_dest, c_lane, c_ctx)

        #
        # Shared multiply unit: the u and v product chains. Sources
        # first (m1/vm1), then the scale applications.
        #
        # t-slot occupancy: at most one t value staged or in flight per
        # lane, so a delayed accumulate can never be overrun by the
        # next record's t landing. The Q4_K correction operands carry
        # the same discipline: u and v hold their slots until the
        # correction subtract issues, so a later record cannot
        # overwrite a staged pair.
        t_busy = [Signal(name=f't_busy{l}') for l in range(n)]
        u_wait = [Signal(name=f'u_wait{l}') for l in range(n)]
        v_wait = [Signal(name=f'v_wait{l}') for l in range(n)]
        # The m1/vm1 products carry the same in-flight discipline:
        # with two-entry conversion FIFOs the source multiplies of
        # consecutive records can issue on back-to-back cycles, so a
        # landing would otherwise overwrite a staged (or still
        # flying) predecessor before its u/v multiply consumes it.
        # The stage gate sees only landed values; these reservations
        # cover the latency window, like cast_pend did for casts.
        m1_busy = [Signal(name=f'm1_busy{l}') for l in range(n)]
        vm1_busy = [Signal(name=f'vm1_busy{l}') for l in range(n)]
        # An m1/vm1 issue pops the head of both of its operand FIFOs
        # (same record, same cycle), keeping the pair alignment the
        # _CastStage ordering argument relies on.
        sd_pop = [Signal(name=f'sd_pop{l}') for l in range(n)]
        cd_pop = [Signal(name=f'cd_pop{l}') for l in range(n)]

        mul_ops = ([('m1', l) for l in range(n)] + [('vm1', l)
                                                    for l in range(n)] +
                   [('u', l) for l in range(n)] + [('v', l) for l in range(n)])
        mul_ready = []
        for what, l in mul_ops:
            if what == 'm1':
                mul_ready.append(fS[l].e[0].avail & dwt[l].e[0].avail
                                 & ~m1[l].avail & ~m1_busy[l])
            elif what == 'vm1':
                mul_ready.append(fC[l].e[0].avail & dmn[l].e[0].avail
                                 & ~vm1[l].avail & ~vm1_busy[l])
            elif what == 'u':
                mul_ready.append(m1[l].avail & ~tt[l].avail
                                 & ~t_busy[l] & ~u_wait[l])
            else:
                mul_ready.append(vm1[l].avail & ~v_wait[l])
        mul_sel = Signal(range(len(mul_ops)))
        mul_found = Signal()
        pick(mul_ready, mul_sel, mul_found)

        m.d.comb += [
            fmul.inp.valid.eq(mul_found),
            fmul.inp.bits.fn.eq(FPUOperator.MUL),
            fmul.inp.bits.rm.eq(RoundingMode.RNE),
            fmul.inp.bits.in3.eq(0),
        ]
        for i, (what, l) in enumerate(mul_ops):
            with m.If(mul_sel == i):
                if what == 'm1':
                    m.d.comb += [
                        fmul.inp.bits.in1.eq(fS[l].e[0].val),
                        fmul.inp.bits.in2.eq(dwt[l].e[0].val)
                    ]
                elif what == 'vm1':
                    m.d.comb += [
                        fmul.inp.bits.in1.eq(fC[l].e[0].val),
                        fmul.inp.bits.in2.eq(dmn[l].e[0].val)
                    ]
                elif what == 'u':
                    m.d.comb += [
                        fmul.inp.bits.in1.eq(m1[l].val),
                        fmul.inp.bits.in2.eq(m1[l].ctx.s_act)
                    ]
                else:
                    m.d.comb += [
                        fmul.inp.bits.in1.eq(vm1[l].val),
                        fmul.inp.bits.in2.eq(vm1[l].ctx.s_act)
                    ]
        # Pop pulses fire only on a real issue; mul_sel alone would
        # idle-match index 0 and pop continuously.
        for l in range(n):
            m.d.comb += sd_pop[l].eq(mul_found
                                     & (mul_sel == mul_ops.index(('m1', l))))
            m.d.comb += cd_pop[l].eq(mul_found
                                     & (mul_sel == mul_ops.index(('vm1', l))))
        with m.If(mul_found):
            m.d.sync += self.mul_ops.eq(self.mul_ops + 1)
            for i, (what, l) in enumerate(mul_ops):
                with m.If(mul_sel == i):
                    if what == 'm1':
                        m.d.sync += m1_busy[l].eq(1)
                    elif what == 'vm1':
                        m.d.sync += vm1_busy[l].eq(1)
                    elif what == 'u':
                        # m1/vm1's operand FIFOs free through the pop
                        # signals in the operand mux; the destination
                        # stage stays covered by its in-flight bit
                        # until the landing retires it.
                        m.d.sync += [m1[l].avail.eq(0), t_busy[l].eq(1)]
                        with m.If(m1[l].ctx.q4k):
                            m.d.sync += u_wait[l].eq(1)
                    else:
                        m.d.sync += vm1[l].avail.eq(0)
                        with m.If(vm1[l].ctx.q4k):
                            m.d.sync += v_wait[l].eq(1)

        m_dest = Signal(Dest, name='mdest')
        m_lane = Signal(lane_bits, name='mlane')
        m_ctx = Ctx(self.params, name='mctx')
        m_dmap = {'m1': Dest.M1, 'vm1': Dest.VM1, 'u': Dest.U, 'v': Dest.V}
        m_cmap = {
            'm1': lambda l: fS[l].e[0].ctx,
            'vm1': lambda l: fC[l].e[0].ctx,
            'u': lambda l: m1[l].ctx,
            'v': lambda l: vm1[l].ctx
        }
        m.d.comb += [
            m_dest.eq(m_dmap[mul_ops[0][0]]),
            m_lane.eq(mul_ops[0][1]),
            m_ctx.eq(m_cmap[mul_ops[0][0]](mul_ops[0][1]))
        ]
        for i, (what, l) in enumerate(mul_ops):
            with m.If(mul_sel == i):
                m.d.comb += [
                    m_dest.eq(m_dmap[what]),
                    m_lane.eq(l),
                    m_ctx.eq(m_cmap[what](l))
                ]
        push_tag(mul_tag, mul_found, m_dest, m_lane, m_ctx)

        #
        # Shared add/subtract unit: fold launches have top priority,
        # then final folds, accumulation, and Q4_K correction. The row
        # reservation retains fold state under result backpressure.
        #
        ff_busy = Signal()
        # Fold-scheduler view of the lane's oldest unretired snapshot
        # (per-lane muxes over the two buffers).
        rd_avail = [Signal(name=f'snap_rda{l}') for l in range(n)]
        rd_done = [[Signal(name=f'snap_rdd{b}{l}') for b in range(2)]
                   for l in range(n)]
        rd_snap = [[Signal(32, name=f'snap_rds{i}{l}') for i in range(4)]
                   for l in range(n)]
        rd_ctx = [Ctx(self.params, name=f'snap_rdctx{l}') for l in range(n)]
        for l in range(n):
            m.d.comb += [
                rd_avail[l].eq(
                    Mux(snap_rd[l], snap_avail[1][l], snap_avail[0][l])),
                rd_done[l][0].eq(
                    Mux(snap_rd[l], snap_done[1][l][0], snap_done[0][l][0])),
                rd_done[l][1].eq(
                    Mux(snap_rd[l], snap_done[1][l][1], snap_done[0][l][1])),
                rd_ctx[l].eq(Mux(snap_rd[l], snap_ctx[1][l], snap_ctx[0][l])),
            ]
            for i in range(4):
                m.d.comb += rd_snap[l][i].eq(
                    Mux(snap_rd[l], snap[1][l][i], snap[0][l][i]))
        add_ops = ([('f1', l) for l in range(n)] +
                   [('f2', l) for l in range(n)] + [('ff', l)
                                                    for l in range(n)] +
                   [('acc', l) for l in range(n)] + [('sub', l)
                                                     for l in range(n)])
        add_ready = []
        for what, l in add_ops:
            if what == 'f1':
                add_ready.append(rd_avail[l] & ~rd_done[l][0] & ~f1[l].avail)
            elif what == 'f2':
                add_ready.append(rd_avail[l] & ~rd_done[l][1] & ~f2[l].avail)
            elif what == 'acc':
                add_ready.append(tt[l].avail)
            elif what == 'sub':
                add_ready.append(uv[l].avail & vv[l].avail
                                 & uv[l].ctx.q4k & ~tt[l].avail
                                 & ~t_busy[l])
            else:
                add_ready.append(f1[l].avail & f2[l].avail
                                 & res_q[l].enq.ready & ~ff_busy)
        add_sel = Signal(range(len(add_ops)))
        add_found = Signal()
        pick(add_ready, add_sel, add_found)

        m.d.comb += [
            fadd.inp.valid.eq(add_found),
            fadd.inp.bits.fn.eq(FPUOperator.ADD),
            fadd.inp.bits.rm.eq(RoundingMode.RNE),
            fadd.inp.bits.fn_mod.eq(0),
            fadd.inp.bits.in2.eq(0),
        ]
        for i, (what, l) in enumerate(add_ops):
            with m.If(add_sel == i):
                if what == 'f1':
                    m.d.comb += [
                        fadd.inp.bits.in1.eq(rd_snap[l][0]),
                        fadd.inp.bits.in3.eq(rd_snap[l][2])
                    ]
                elif what == 'f2':
                    m.d.comb += [
                        fadd.inp.bits.in1.eq(rd_snap[l][1]),
                        fadd.inp.bits.in3.eq(rd_snap[l][3])
                    ]
                elif what == 'acc':
                    # A bank's first touch in a row is the segment
                    # whose index equals it (segments below 4 map to
                    # their own bank); later touches accumulate.
                    idx = tt[l].ctx.seg_k[:2]
                    m.d.comb += [
                        fadd.inp.bits.in1.eq(
                            Mux(tt[l].ctx.seg_k == idx, 0,
                                Array(bank[l])[idx])),
                        fadd.inp.bits.in3.eq(tt[l].val),
                    ]
                elif what == 'sub':
                    m.d.comb += [
                        fadd.inp.bits.fn_mod.eq(1),
                        fadd.inp.bits.in1.eq(uv[l].val),
                        fadd.inp.bits.in3.eq(vv[l].val),
                    ]
                else:
                    m.d.comb += [
                        fadd.inp.bits.in1.eq(f1[l].val),
                        fadd.inp.bits.in3.eq(f2[l].val)
                    ]
        with m.If(add_found):
            m.d.sync += self.add_ops.eq(self.add_ops + 1)
            for i, (what, l) in enumerate(add_ops):
                with m.If(add_sel == i):
                    if what == 'f1':
                        with m.If(snap_rd[l]):
                            m.d.sync += snap_done[1][l][0].eq(1)
                        with m.Else():
                            m.d.sync += snap_done[0][l][0].eq(1)
                    elif what == 'f2':
                        with m.If(snap_rd[l]):
                            m.d.sync += snap_done[1][l][1].eq(1)
                        with m.Else():
                            m.d.sync += snap_done[0][l][1].eq(1)
                    elif what == 'acc':
                        m.d.sync += tt[l].avail.eq(0)
                    elif what == 'sub':
                        m.d.sync += [
                            uv[l].avail.eq(0), vv[l].avail.eq(0),
                            t_busy[l].eq(1), u_wait[l].eq(0), v_wait[l].eq(0)
                        ]
                    else:
                        m.d.sync += [
                            f1[l].avail.eq(0), f2[l].avail.eq(0),
                            ff_busy.eq(1)
                        ]

        a_dest = Signal(Dest, name='adest')
        a_lane = Signal(lane_bits, name='alane')
        a_ctx = Ctx(self.params, name='actx')
        a_dmap = {
            'f1': Dest.F1,
            'f2': Dest.F2,
            'acc': Dest.ACC,
            'sub': Dest.T,
            'ff': Dest.FF
        }
        a_cmap = {
            'f1': lambda l: rd_ctx[l],
            'f2': lambda l: rd_ctx[l],
            'acc': lambda l: tt[l].ctx,
            'sub': lambda l: uv[l].ctx,
            'ff': lambda l: f1[l].ctx
        }
        m.d.comb += [
            a_dest.eq(a_dmap[add_ops[0][0]]),
            a_lane.eq(add_ops[0][1]),
            a_ctx.eq(a_cmap[add_ops[0][0]](add_ops[0][1]))
        ]
        for i, (what, l) in enumerate(add_ops):
            with m.If(add_sel == i):
                m.d.comb += [
                    a_dest.eq(a_dmap[what]),
                    a_lane.eq(l),
                    a_ctx.eq(a_cmap[what](l))
                ]
        push_tag(add_tag, add_found, a_dest, a_lane, a_ctx)

        #
        # Result routing: capture each unit's output through its
        # destination tag. All routing is by static lane comparisons;
        # dest mismatches never write.
        #
        # The four conversion destinations are two-entry FIFOs: a pop
        # (m1/vm1 issue, via sd_pop/cd_pop) shifts or clears the head,
        # a landing pushes behind it. A landing never sees both slots
        # occupied -- the rsv issue gate ran before the cast launched
        # -- so the push target is e1 exactly when e0 holds a value.
        cast_fire = cast.out.valid & cast_tag[0].v
        for dest, sts, pops in ((Dest.FS, fS, sd_pop), (Dest.D, dwt, sd_pop),
                                (Dest.FC, fC, cd_pop), (Dest.DM, dmn, cd_pop)):
            for l, st in enumerate(sts):
                land = cast_fire & (cast_tag[0].dest == dest) \
                    & (cast_tag[0].lane == l)
                e0, e1 = st.e
                with m.If(pops[l]):
                    with m.If(e1.avail):
                        # Full; no landing is possible this cycle.
                        m.d.sync += [
                            e0.val.eq(e1.val),
                            e0.avail.eq(1),
                            e0.ctx.eq(e1.ctx),
                            e1.avail.eq(0)
                        ]
                    with m.Else():
                        with m.If(land):
                            # The popped head is replaced by the value
                            # landing this cycle (it is the new head).
                            m.d.sync += [
                                e0.val.eq(cast.out.bits.data),
                                e0.avail.eq(1),
                                e0.ctx.eq(cast_tag[0].ctx)
                            ]
                        with m.Else():
                            m.d.sync += e0.avail.eq(0)
                with m.Else():
                    with m.If(land):
                        with m.If(e0.avail):
                            m.d.sync += [
                                e1.val.eq(cast.out.bits.data),
                                e1.avail.eq(1),
                                e1.ctx.eq(cast_tag[0].ctx)
                            ]
                        with m.Else():
                            m.d.sync += [
                                e0.val.eq(cast.out.bits.data),
                                e0.avail.eq(1),
                                e0.ctx.eq(cast_tag[0].ctx)
                            ]

        # Unified reservation update: a cast issue raises the
        # destination's count, an m1/vm1 pop retires it, and both in
        # the same cycle is a hold. Keeping this in one place makes
        # the same-cycle combination exact.
        for i, (k, l) in enumerate(cast_ops_list):
            st = cast_dest[k][l]
            inc = cast_found & (cast_sel == i)
            dec = (sd_pop[l] if k in (0, 1) else cd_pop[l])
            with m.If(inc & dec):
                pass
            with m.Elif(inc):
                m.d.sync += st.rsv.eq(st.rsv + 1)
            with m.Elif(dec):
                m.d.sync += st.rsv.eq(st.rsv - 1)

        plain = {Dest.M1: m1, Dest.VM1: vm1, Dest.F1: f1, Dest.F2: f2}
        for unit, tags in ((cast, cast_tag), (fmul, mul_tag), (fadd, add_tag)):
            fire = unit.out.valid & tags[0].v
            with m.If(fire):
                for d, sts in plain.items():
                    with m.If(tags[0].dest == d):
                        for l in range(n):
                            with m.If(tags[0].lane == l):
                                m.d.sync += [
                                    sts[l].val.eq(unit.out.bits.data),
                                    sts[l].avail.eq(1),
                                    sts[l].ctx.eq(tags[0].ctx),
                                ]

        # A landed source product retires its in-flight reservation.
        with m.If(fmul.out.valid & mul_tag[0].v):
            with m.If(mul_tag[0].dest == Dest.M1):
                for l in range(n):
                    with m.If(mul_tag[0].lane == l):
                        m.d.sync += m1_busy[l].eq(0)
            with m.If(mul_tag[0].dest == Dest.VM1):
                for l in range(n):
                    with m.If(mul_tag[0].lane == l):
                        m.d.sync += vm1_busy[l].eq(0)

        # The u product: held for the correction subtract on Q4_K, or
        # passed straight through as t otherwise (subtracting the +0
        # the v path produces would be equivalent, at one extra op).
        # Either way the t slot's in-flight reservation retires.
        with m.If(fmul.out.valid & mul_tag[0].v & (mul_tag[0].dest == Dest.U)):
            for l in range(n):
                with m.If(mul_tag[0].lane == l):
                    m.d.sync += t_busy[l].eq(0)
                    with m.If(mul_tag[0].ctx.q4k):
                        m.d.sync += [
                            uv[l].val.eq(fmul.out.bits.data),
                            uv[l].avail.eq(1), uv[l].ctx.eq(mul_tag[0].ctx)
                        ]
                    with m.Else():
                        m.d.sync += [
                            tt[l].val.eq(fmul.out.bits.data),
                            tt[l].avail.eq(1), tt[l].ctx.eq(mul_tag[0].ctx)
                        ]

        # The correction subtract result is the Q4_K t.
        with m.If(fadd.out.valid & add_tag[0].v & (add_tag[0].dest == Dest.T)):
            for l in range(n):
                with m.If(add_tag[0].lane == l):
                    m.d.sync += [
                        tt[l].val.eq(fadd.out.bits.data),
                        tt[l].avail.eq(1),
                        tt[l].ctx.eq(add_tag[0].ctx),
                        t_busy[l].eq(0),
                    ]

        # v always lands in its stage; it is only produced for Q4_K,
        # where it is exactly +0 whenever the correction vanishes.
        with m.If(fmul.out.valid & mul_tag[0].v & (mul_tag[0].dest == Dest.V)):
            for l in range(n):
                with m.If(mul_tag[0].lane == l):
                    m.d.sync += [
                        vv[l].val.eq(fmul.out.bits.data), vv[l].avail.eq(1),
                        vv[l].ctx.eq(mul_tag[0].ctx)
                    ]

        # Accumulation writes the addressed bank; a row's final segment
        # snapshots the four partials for the fold into the free
        # buffer. The snapshot write is ordered after the fold-launch
        # consumes above so a same cycle cannot lose either update.
        # The capture also retires the lane's row reservation: the
        # fold's queue capacity is reserved by the ff issue gate plus
        # the global ff_busy serialization, so the next row's records
        # may be accepted while this fold drains.
        with m.If(fadd.out.valid & add_tag[0].v
                  & (add_tag[0].dest == Dest.ACC)):
            idx = add_tag[0].ctx.seg_k[:2]
            for l in range(n):
                with m.If(add_tag[0].lane == l):
                    with m.If(idx == 0):
                        m.d.sync += bank[l][0].eq(fadd.out.bits.data)
                    with m.If(idx == 1):
                        m.d.sync += bank[l][1].eq(fadd.out.bits.data)
                    with m.If(idx == 2):
                        m.d.sync += bank[l][2].eq(fadd.out.bits.data)
                    with m.If(idx == 3):
                        m.d.sync += bank[l][3].eq(fadd.out.bits.data)
                    with m.If(add_tag[0].ctx.row_last):
                        # Capture target: the free buffer. Both full
                        # cannot happen -- row-start acceptance
                        # guarantees a free buffer per lane, and at
                        # most two rows per lane are open.
                        cap = Mux(snap_avail[0][l], 1, 0)
                        for i in range(4):
                            word = Mux(
                                idx == i, fadd.out.bits.data,
                                Mux(i > add_tag[0].ctx.seg_k, 0, bank[l][i]))
                            with m.If(cap == 0):
                                m.d.sync += snap[0][l][i].eq(word)
                            with m.Else():
                                m.d.sync += snap[1][l][i].eq(word)
                        m.d.sync += [
                            row_busy[l].eq(0),
                            self.folds.eq(self.folds + 1)
                        ]
                        for b in range(2):
                            with m.If(cap == b):
                                m.d.sync += [
                                    snap_avail[b][l].eq(1),
                                    snap_done[b][l].eq(0),
                                    snap_ctx[b][l].eq(add_tag[0].ctx)
                                ]
                                # With nothing pending the captured
                                # buffer becomes the fold to read.
                                with m.If(~rd_avail[l]):
                                    m.d.sync += snap_rd[l].eq(b)

        # Per-lane capture fact, for the retire logic below: a capture
        # landing in the same cycle as a retire makes the captured
        # buffer the next to read.
        cap_now = [Signal(name=f'cap_now{l}') for l in range(n)]
        cap_buf = [Signal(name=f'cap_buf{l}') for l in range(n)]
        for l in range(n):
            m.d.comb += [
                cap_now[l].eq(fadd.out.valid & add_tag[0].v
                              & (add_tag[0].dest == Dest.ACC)
                              & (add_tag[0].lane == l)
                              & add_tag[0].ctx.row_last),
                cap_buf[l].eq(Mux(snap_avail[0][l], 1, 0)),
            ]

        # The read snapshot retires once both fold halves have
        # launched; the read pointer moves to the other buffer when
        # that one holds the next pending fold (already captured
        # earlier, or captured this very cycle). Clearing the done
        # bits keeps a retired snapshot from killing the next capture
        # on an unrelated add issue the same cycle.
        with m.If(add_found):
            for l in range(n):
                with m.If(rd_avail[l] & (rd_done[l][0] & rd_done[l][1])):
                    other = ~snap_rd[l]
                    with m.If(snap_rd[l]):
                        m.d.sync += [
                            snap_avail[1][l].eq(0), snap_done[1][l].eq(0)
                        ]
                    with m.Else():
                        m.d.sync += [
                            snap_avail[0][l].eq(0), snap_done[0][l].eq(0)
                        ]
                    other_pending = Mux(other, snap_avail[1][l],
                                        snap_avail[0][l]) \
                        | (cap_now[l] & (cap_buf[l] == other))
                    with m.If(other_pending):
                        m.d.sync += snap_rd[l].eq(other)

        # A final fold enqueues its lane's result; ff_busy holds until
        # it lands, so at most one final fold occupies each queue. The
        # queue capacity was already checked at issue, and ff_busy
        # serializes final folds, so the enqueue cannot fail. row_busy
        # no longer clears here (M3b stage 3: it retired at the
        # snapshot capture; clearing again could open a third row and
        # overflow the two snapshot buffers).
        with m.If(fadd.out.valid & add_tag[0].v
                  & (add_tag[0].dest == Dest.FF)):
            m.d.sync += ff_busy.eq(0)
            for l in range(n):
                with m.If(add_tag[0].lane == l):
                    m.d.comb += [
                        res_q[l].enq.valid.eq(1),
                        res_q[l].enq.bits.val.eq(fadd.out.bits.data),
                        res_q[l].enq.bits.row.eq(add_tag[0].ctx.row),
                    ]

        return m
