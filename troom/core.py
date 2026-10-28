"""Streaming troom accelerator core.

DMA, StreamGemv, its per-lane FP32 epilogue, activation loading, vector/reduction
and V-window building share TMEM and the AXI master. SMATMUL streams SG1 weights
from DDR; SQUANTIZE loads the activation RAM. Retired MATMUL/QUANTIZE encodings
complete with BAD_OPCODE. Command issue is asynchronous with in-order completion,
level-held WAIT/SIGNAL events, and END draining all earlier commands.
"""

from amaranth import *
from amaranth.hdl.rec import Direction, Record

from troom.consts import (
    Format,
    Opcode,
    Source,
    Status,
    VecOp,
    VEC_ELEMENTWISE,
    VEC_REDUCTIONS,
)
from troom.types import (
    DMA_DESC_FIELDS,
    DmaDescriptor,
    HasTRoomParams,
    dma_desc_layout,
)

from troom.dma import DMA
from troom.epilogue import Epilogue
from troom.quantize import ActLoader
from troom.stream import StreamAxiReader, StreamGemv
from troom.tmem import TMEM
from troom.vector import VectorUnit
from troom.vbuild import VBuildUnit

from roomsoc.interconnect.axi import AXIInterface
from roomsoc.interconnect.axi.axi_full import AXIArbiter
from roomsoc.interconnect.stream import Decoupled, Queue


class Command(HasTRoomParams, Record):
    """Host-to-engine command record.

    ``evt`` selects the completion event for ``WAIT``/``SIGNAL`` and
    the asynchronous completion event set by engine commands when their
    work finishes (including error completions, so a following ``WAIT``
    cannot deadlock on a failed transfer). The 2D descriptor fields are
    carried flat (amaranth directional records cannot nest) and are
    meaningful for ``LOAD_2D``/``STORE_2D``; ``rows`` also serves as
    the matmul row count, while the matmul and activation-load fields
    name their own regions. ``VECTOR``/``REDUCE`` select their
    sub-operation in ``vec_op``, their element count in ``k``, and
    their regions in ``a_base`` (and ``b_base`` for the binary
    elementwise ops), writing results to ``r_base``.
    """

    def __init__(self, params, name=None, src_loc_at=0):
        HasTRoomParams.__init__(self, params)
        Record.__init__(
            self,
            [
                ('opcode', Opcode, Direction.FANOUT),
                ('evt', self.evt_bits, Direction.FANOUT),
                ('seq', self.seq_bits, Direction.FANOUT),
                ('fmt', Format, Direction.FANOUT),
                ('vec_op', VecOp, Direction.FANOUT),
                ('k', self.lg_act_k + 1, Direction.FANOUT),
                ('w_base', self.tmem_addr_width, Direction.FANOUT),
                ('w_stride', self.tmem_addr_width, Direction.FANOUT),
                ('r_base', self.tmem_addr_width, Direction.FANOUT),
                ('a_base', self.tmem_addr_width, Direction.FANOUT),
                ('m_base', self.tmem_addr_width, Direction.FANOUT),
            ] + [(name, width, Direction.FANOUT)
                 for name, width in dma_desc_layout(self)],
            name=name,
            src_loc_at=src_loc_at + 1)


class Completion(HasTRoomParams, Record):
    """Engine-to-host completion record."""

    def __init__(self, params, name=None, src_loc_at=0):
        HasTRoomParams.__init__(self, params)
        Record.__init__(self, [
            ('seq', self.seq_bits, Direction.FANOUT),
            ('status', Status, Direction.FANOUT),
        ],
                        name=name,
                        src_loc_at=src_loc_at + 1)


# Completion results are indexed by source; the engines complete
# their commands in acceptance order.
ENGINE_SOURCES = (Source.DMA, Source.ACT, Source.VEC, Source.VBUILD)


class Core(HasTRoomParams, Elaboratable):
    """troom engine: command sequencer, DMA, TMEM, and matrix path.

    ``cmd`` accepts commands in issue order; every command produces one
    completion record on ``resp`` in issue order. Engine commands are
    issued into asynchronous queues and complete only when their work
    finishes. ``SIGNAL`` sets an event flag, ``WAIT`` blocks until its
    event flag is set and then clears it, and ``END`` completes the
    sequence after draining all earlier commands, after which ``done``
    stays asserted and further commands are back-pressured until
    reset.

    ``mem_base``/``mem_size`` bound every DRAM address the DMA issues
    (see ``troom.dma``); descriptors whose 2D extent leaves the window
    complete with ``BAD_RANGE`` without bus traffic.
    """

    def __init__(self, params, *, mem_base=0, mem_size=None):
        super().__init__(params)

        self.mem_base = mem_base
        self.mem_size = mem_size

        self.cmd = Decoupled(Command, self.params)
        self.resp = Decoupled(Completion, self.params)

        self.bus = AXIInterface(data_width=self.data_width,
                                addr_width=self.addr_width,
                                id_width=self.id_width)

        self.stream = StreamGemv(self.params,
                                 mem_base=mem_base,
                                 mem_size=mem_size)
        self.sreader = StreamAxiReader(self.params)

        # Recovery stops new engine issue while already-issued DMA drains.
        self.halt = Signal()
        self.memory_idle = Signal()
        self.busy = Signal()
        self.done = Signal()

        # Observation counters; each attaches with the module that
        # generates it.
        self.cycle_count = Signal(48)
        self.cmds_done = Signal(32)
        self.dram_bytes = Signal(48)
        self.dma_stalls = Signal(48)
        self.tmem_conflicts = Signal(48)
        self.useful_macs = Signal(48)
        self.array_starve = Signal(48)
        # Cycle-breakdown window (M3b): the matrix unit's launch/drain
        # split with its output-credit stalls, and the FP32 epilogue's
        # activity, sink backpressure, record acceptance, and per-lane
        # FPU issue totals.
        self.mm_credit_stalls = Signal(48)
        self.mm_run_cycles = Signal(48)
        self.mm_drain_cycles = Signal(48)
        self.epi_records = Signal(48)
        self.epi_active_cycles = Signal(48)
        self.epi_stalls = Signal(48)
        self.epi_cast_ops = Signal(48)
        self.epi_mul_ops = Signal(48)
        self.epi_add_ops = Signal(48)

    def elaborate(self, platform):
        m = Module()

        # Current-command opcode, latched at FETCH.
        cur_opcode = Signal(Opcode)

        tmem = m.submodules.tmem = TMEM(self.params)
        dma_bus = AXIInterface(data_width=self.data_width,
                               addr_width=self.addr_width,
                               id_width=self.id_width)
        dma = m.submodules.dma = DMA(self.params,
                                     dma_bus,
                                     mem_base=self.mem_base,
                                     mem_size=self.mem_size)
        m.submodules.stream = self.stream
        m.submodules.sreader = self.sreader
        core_arb = m.submodules.core_arb = AXIArbiter(
            addr_width=self.addr_width,
            data_width=self.data_width,
            id_width=self.id_width)
        core_arb.add(dma_bus)
        core_arb.add(self.sreader.bus)
        m.d.comb += [
            core_arb.bus.connect(self.bus),
            self.stream.mem_rreq.connect(self.sreader.rreq),
            self.sreader.rresp.connect(self.stream.mem_rresp),
        ]
        epi = m.submodules.epi = Epilogue(self.params)
        actload = m.submodules.actload = ActLoader(self.params)
        vector = m.submodules.vector = VectorUnit(self.params)
        vbuild = m.submodules.vbuild = VBuildUnit(self.params)

        # Results retire only after the per-lane epilogue drains to TMEM.
        m.d.comb += [
            self.stream.segs.connect(epi.sink),
            epi.start.eq(self.stream.sink.fire & self.stream.start_ok),
        ]

        # Activation loading and streaming compute never overlap.
        m.d.comb += actload.act_wr.connect(self.stream.act_wr)

        cur_rows = Signal(self.rows_bits)
        cur_r_base = Signal(self.tmem_addr_width)
        m.d.comb += [epi.rows.eq(cur_rows), epi.r_base.eq(cur_r_base)]

        #
        # TMEM port arbitration. Reads: the DMA store reader, the
        # activation loader, the vector unit, and
        # the V-window builder share the single request/response
        # channel round-robin, with the owner tag routing the
        # response back. Writes: the DMA fill, the epilogue's result
        # drain, the vector unit's result beats, and the V-window
        # tile drain share the write channel.
        #
        rreqs = [
            dma.rreq, actload.tmem_rreq, vector.tmem_rreq, vbuild.tmem_rreq
        ]
        rresps = [
            dma.rresp, actload.tmem_rresp, vector.tmem_rresp, vbuild.tmem_rresp
        ]

        n_r = len(rreqs)
        r_valid = Array([r.valid for r in rreqs])
        r_addr = Array([r.bits.addr for r in rreqs])

        def rotate_reader(rr, q):
            """Rotate past n_r requesters without a width wrap."""
            v = rr + q
            return Mux(v >= n_r, v - n_r, v)

        r_rr = Signal(range(n_r))
        r_found = Signal()
        r_sel = Signal(range(n_r))
        for q in reversed(range(n_r)):
            idx = rotate_reader(r_rr, q)
            with m.If(r_valid[idx]):
                m.d.comb += [r_found.eq(1), r_sel.eq(idx)]

        r_owner = Signal(range(n_r))
        m.d.comb += [
            tmem.rreq.valid.eq(r_found),
            tmem.rreq.bits.addr.eq(r_addr[r_sel]),
        ]
        for i, r in enumerate(rreqs):
            m.d.comb += r.ready.eq(tmem.rreq.ready & r_found & (r_sel == i))
        with m.If(tmem.rreq.fire):
            m.d.sync += [r_owner.eq(r_sel), r_rr.eq(rotate_reader(r_sel, 1))]

        rresp_ready = Const(0)
        for i, r in enumerate(rresps):
            m.d.comb += [
                r.valid.eq(tmem.rresp.valid & (r_owner == i)),
                r.bits.data.eq(tmem.rresp.bits.data),
            ]
            rresp_ready = rresp_ready | ((r_owner == i) & r.ready)
        m.d.comb += tmem.rresp.ready.eq(rresp_ready)

        wreqs = [dma.tmem_w, epi.tmem_w, vector.tmem_w, vbuild.tmem_w]
        w_valid = Array([w.valid for w in wreqs])
        w_addr = Array([w.bits.addr for w in wreqs])
        w_data = Array([w.bits.data for w in wreqs])

        n_w = len(wreqs)

        def rot4w(rr, q):
            """Rotate past four writers without a width wrap."""
            v = rr + q
            return Mux(v >= n_w, v - n_w, v)

        w_rr = Signal(2)
        w_found = Signal()
        w_sel = Signal(2)
        for q in reversed(range(n_w)):
            idx = rot4w(w_rr, q)
            with m.If(w_valid[idx]):
                m.d.comb += [w_found.eq(1), w_sel.eq(idx)]
        m.d.comb += [
            tmem.write.valid.eq(w_found),
            tmem.write.bits.addr.eq(w_addr[w_sel]),
            tmem.write.bits.data.eq(w_data[w_sel]),
        ]
        for i, w in enumerate(wreqs):
            m.d.comb += w.ready.eq(tmem.write.ready & w_found & (w_sel == i))
        with m.If(tmem.write.fire):
            m.d.sync += w_rr.eq(rot4w(w_sel, 1))

        cmd_q = m.submodules.cmd_q = Queue(self.cmd_queue_depth,
                                           Command,
                                           self.params,
                                           flow=False)
        m.d.comb += self.cmd.connect(cmd_q.enq)

        evt_reg = Signal(self.n_events)

        cur_evt = Signal(self.evt_bits)
        cur_seq = Signal(self.seq_bits)
        cur_dma = DmaDescriptor(self.params)
        cur_fmt = Signal(Format)
        cur_vec_op = Signal(VecOp)
        cur_k = Signal(self.lg_act_k + 1)
        cur_a_base = Signal(self.tmem_addr_width)
        cur_m_base = Signal(self.tmem_addr_width)

        is_load = cur_opcode == Opcode.LOAD_2D
        is_store = cur_opcode == Opcode.STORE_2D
        is_dma = is_load | is_store
        is_wait = cur_opcode == Opcode.WAIT
        is_signal = cur_opcode == Opcode.SIGNAL
        is_end = cur_opcode == Opcode.END
        is_vector = (cur_opcode == Opcode.VECTOR) \
            | (cur_opcode == Opcode.REDUCE)
        is_vbuild = cur_opcode == Opcode.VBUILD
        is_smatmul = cur_opcode == Opcode.SMATMUL
        is_actload = cur_opcode == Opcode.SQUANTIZE

        # A vector command must name a sub-operation of its own class;
        # reserved sub-op encodings match neither set.
        vec_elem = Signal()
        vec_rdc = Signal()
        m.d.comb += [
            vec_elem.eq(Cat(cur_vec_op == op for op in VEC_ELEMENTWISE).any()),
            vec_rdc.eq(Cat(cur_vec_op == op for op in VEC_REDUCTIONS).any()),
        ]
        vec_cls_ok = Mux(cur_opcode == Opcode.VECTOR, vec_elem, vec_rdc)

        evt_hit = evt_reg.bit_select(cur_evt, 1)

        # The result region must be beat aligned with room for every
        # row's FP32 word; its padded end must stay inside TMEM.
        r_bytes = cur_rows << 2
        r_end = (cur_r_base +
                 ((r_bytes +
                   (self.beat_bytes - 1)) >> self.lg_beat << self.lg_beat))
        r_ok = (cur_r_base[:self.lg_beat] == 0) \
            & (r_end <= self.tmem_bytes)

        #
        # Engine result plumbing. Each engine completes its commands in
        # acceptance order; a done pulse pairs the engine's status with
        # the event of its oldest uncompleted accept and sets that
        # event at completion time.
        #
        n_slots = self.cmd_queue_depth

        sources = ENGINE_SOURCES + (Source.STREAM, )

        evt_qs = []
        res_qs = []
        for _ in sources:
            evt_q = Queue(n_slots, Record,
                          [('evt', self.evt_bits, Direction.FANOUT)])
            res_q = Queue(n_slots,
                          Record,
                          [('status', Shape.cast(Status), Direction.FANOUT),
                           ('evt', self.evt_bits, Direction.FANOUT)],
                          flow=False)
            evt_qs.append(evt_q)
            res_qs.append(res_q)
        (dma_evt_q, act_evt_q, vec_evt_q, vb_evt_q, sm_evt_q) = evt_qs
        (dma_res_q, act_res_q, vec_res_q, vb_res_q, sm_res_q) = res_qs
        qnames = ('dma_evt_q', 'act_evt_q', 'vec_evt_q', 'vb_evt_q',
                  'sm_evt_q', 'dma_res_q', 'act_res_q', 'vec_res_q',
                  'vb_res_q', 'sm_res_q')
        for name, q in zip(qnames, evt_qs + res_qs):
            setattr(m.submodules, name, q)

        sm_bus_err = Signal()
        with m.If(self.stream.sink.fire & self.stream.start_ok):
            m.d.sync += sm_bus_err.eq(0)
        with m.Elif(self.sreader.err):
            m.d.sync += sm_bus_err.eq(1)
        sm_bad = self.stream.done & (self.stream.status != Status.OK)
        sm_status = Mux(
            sm_bad, self.stream.status,
            Mux(sm_bus_err | self.sreader.err, Status.DMA_ERROR, epi.status))
        sm_done = sm_bad | epi.done

        m.d.comb += [
            dma_evt_q.enq.valid.eq(dma.sink.fire),
            dma_evt_q.enq.bits.evt.eq(cur_evt),
            act_evt_q.enq.valid.eq(actload.sink.fire),
            act_evt_q.enq.bits.evt.eq(cur_evt),
            vec_evt_q.enq.valid.eq(vector.sink.fire),
            vec_evt_q.enq.bits.evt.eq(cur_evt),
            vb_evt_q.enq.valid.eq(vbuild.sink.fire),
            vb_evt_q.enq.bits.evt.eq(cur_evt),
            dma_evt_q.deq.ready.eq(dma.done),
            act_evt_q.deq.ready.eq(actload.done),
            vec_evt_q.deq.ready.eq(vector.done),
            vb_evt_q.deq.ready.eq(vbuild.done),
            dma_res_q.enq.valid.eq(dma.done),
            dma_res_q.enq.bits.status.eq(dma.status),
            dma_res_q.enq.bits.evt.eq(dma_evt_q.deq.bits.evt),
            act_res_q.enq.valid.eq(actload.done),
            act_res_q.enq.bits.status.eq(actload.status),
            act_res_q.enq.bits.evt.eq(act_evt_q.deq.bits.evt),
            vec_res_q.enq.valid.eq(vector.done),
            vec_res_q.enq.bits.status.eq(vector.status),
            vec_res_q.enq.bits.evt.eq(vec_evt_q.deq.bits.evt),
            vb_res_q.enq.valid.eq(vbuild.done),
            vb_res_q.enq.bits.status.eq(vbuild.status),
            vb_res_q.enq.bits.evt.eq(vb_evt_q.deq.bits.evt),
        ]
        m.d.comb += [
            sm_evt_q.enq.valid.eq(self.stream.sink.fire),
            sm_evt_q.enq.bits.evt.eq(cur_evt),
            sm_evt_q.deq.ready.eq(sm_done),
            sm_res_q.enq.valid.eq(sm_done),
            sm_res_q.enq.bits.status.eq(sm_status),
            sm_res_q.enq.bits.evt.eq(sm_evt_q.deq.bits.evt),
        ]
        for q in res_qs:
            with m.If(q.enq.fire):
                m.d.sync += evt_reg.bit_select(q.enq.bits.evt, 1).eq(1)

        # Indexed by source value; the issue source never consults
        # these.
        res_by_source = dict(zip(sources, res_qs))
        res_valid_arr = Array(
            res_by_source[src].deq.valid if src in res_by_source else Const(0)
            for src in Source)
        res_status_arr = Array(res_by_source[src].deq.bits.status if src in
                               res_by_source else Const(0, Shape.cast(Status))
                               for src in Source)

        #
        # In-order completion tracker. Every issued command occupies
        # one slot until its completion record fires on ``resp``.
        # Slots resolved at issue carry their own status; an engine
        # slot resolves when its engine's oldest result is available.
        #
        n = n_slots
        ent_valid = Signal(n)
        ent_resolved = Signal(n)
        ent_is_end = Signal(n)
        ent_engine = [Signal(Source, name=f'ent_eng_{i}') for i in range(n)]
        ent_seq = [
            Signal(self.seq_bits, name=f'ent_seq_{i}') for i in range(n)
        ]
        ent_status = [Signal(Status, name=f'ent_status_{i}') for i in range(n)]

        seq_arr = Array(ent_seq)
        status_arr = Array(ent_status)
        eng_arr = Array(ent_engine)
        end_arr = Array(ent_is_end)

        head = Signal(range(n))
        tail = Signal(range(n))
        count = Signal(range(n + 1))

        def advance(ptr):
            return Mux(ptr == n - 1, 0, ptr + 1)

        push = Signal()
        push_resolved = Signal()
        push_is_end = Signal()
        push_engine = Signal(Source)
        push_status = Signal(Status)

        with m.If(push):
            m.d.sync += [
                ent_valid.bit_select(tail, 1).eq(1),
                ent_resolved.bit_select(tail, 1).eq(push_resolved),
                ent_is_end.bit_select(tail, 1).eq(push_is_end),
                tail.eq(advance(tail)),
            ]
            for i in range(n):
                with m.If(tail == i):
                    m.d.sync += [
                        ent_seq[i].eq(cur_seq),
                        ent_status[i].eq(push_status),
                        ent_engine[i].eq(push_engine),
                    ]

        head_engine = eng_arr[head]
        head_ready = Signal()
        head_status = Signal(Status)
        with m.If(head_engine == Source.ISSUE):
            m.d.comb += [
                head_ready.eq(ent_resolved.bit_select(head, 1)),
                head_status.eq(status_arr[head]),
            ]
        with m.Else():
            m.d.comb += [
                head_ready.eq(res_valid_arr[head_engine]),
                head_status.eq(res_status_arr[head_engine]),
            ]

        do_pop = self.resp.fire
        with m.If(do_pop):
            m.d.sync += [
                ent_valid.bit_select(head, 1).eq(0),
                head.eq(advance(head)),
                self.cmds_done.eq(self.cmds_done + 1),
            ]
            with m.If(end_arr[head]):
                m.d.sync += self.done.eq(1)

        with m.If(push & ~do_pop):
            m.d.sync += count.eq(count + 1)
        with m.Elif(do_pop & ~push):
            m.d.sync += count.eq(count - 1)

        m.d.comb += [
            self.resp.valid.eq((count != 0) & head_ready),
            self.resp.bits.seq.eq(seq_arr[head]),
            self.resp.bits.status.eq(head_status),
        ]
        # Popping an engine slot releases its engine's result.
        for e, res_q in zip(sources, res_qs):
            m.d.comb += res_q.deq.ready.eq(do_pop & (head_engine == e))

        #
        # Command issue. FETCH gates on tracker space; EXEC acts on
        # the latched command. WAIT blocks here until its event is
        # set; engine commands hold until their engine accepts.
        #
        with m.FSM() as fsm:
            with m.State('FETCH'):
                with m.If(~self.halt):
                    m.d.comb += cmd_q.deq.ready.eq(count != n)
                    with m.If(cmd_q.deq.fire):
                        deq = cmd_q.deq.bits
                        m.d.sync += [
                            cur_opcode.eq(deq.opcode),
                            cur_evt.eq(deq.evt),
                            cur_seq.eq(deq.seq),
                            cur_fmt.eq(deq.fmt),
                            cur_vec_op.eq(deq.vec_op),
                            cur_k.eq(deq.k),
                            cur_rows.eq(deq.rows),
                            cur_r_base.eq(deq.r_base),
                            cur_a_base.eq(deq.a_base),
                            cur_m_base.eq(deq.m_base),
                        ]
                        for name in DMA_DESC_FIELDS:
                            m.d.sync += getattr(cur_dma,
                                                name).eq(getattr(deq, name))
                        m.next = 'EXEC'

            with m.State('EXEC'):
                with m.If(~self.halt):
                    with m.If(is_signal):
                        m.d.comb += [
                            push.eq(1),
                            push_resolved.eq(1),
                            push_engine.eq(Source.ISSUE),
                            push_status.eq(Status.OK),
                        ]
                        m.d.sync += evt_reg.bit_select(cur_evt, 1).eq(1)
                        m.next = 'FETCH'

                    with m.Elif(is_wait):
                        with m.If(evt_hit):
                            m.d.comb += [
                                push.eq(1),
                                push_resolved.eq(1),
                                push_engine.eq(Source.ISSUE),
                                push_status.eq(Status.OK),
                            ]
                            m.d.sync += evt_reg.bit_select(cur_evt, 1).eq(0)
                            m.next = 'FETCH'

                    with m.Elif(is_end):
                        m.d.comb += [
                            push.eq(1),
                            push_resolved.eq(1),
                            push_is_end.eq(1),
                            push_engine.eq(Source.ISSUE),
                            push_status.eq(Status.OK),
                        ]
                        m.next = 'HALT'

                    with m.Elif(is_dma):
                        with m.If(dma.sink.fire):
                            m.d.comb += [
                                push.eq(1),
                                push_engine.eq(Source.DMA)
                            ]
                            m.next = 'FETCH'

                    with m.Elif(is_actload):
                        with m.If(actload.sink.fire):
                            m.d.comb += [
                                push.eq(1),
                                push_engine.eq(Source.ACT)
                            ]
                            m.next = 'FETCH'

                    with m.Elif(is_smatmul):
                        # Same issue-time result-region contract as
                        # MATMUL; shape/align/range rejection is the
                        # stream engine's own accept-time check and
                        # completes at its reject pulse.
                        with m.If(~r_ok):
                            m.d.comb += [
                                push.eq(1),
                                push_resolved.eq(1),
                                push_engine.eq(Source.ISSUE),
                                push_status.eq(Status.BAD_SHAPE),
                            ]
                            m.d.sync += evt_reg.bit_select(cur_evt, 1).eq(1)
                            m.next = 'FETCH'
                        with m.Elif(self.stream.sink.fire):
                            m.d.comb += [
                                push.eq(1),
                                push_engine.eq(Source.STREAM)
                            ]
                            m.next = 'FETCH'

                    with m.Elif(is_vector):
                        # A sub-operation from the other opcode class (or a
                        # reserved encoding) completes at issue without
                        # disturbing the engine.
                        with m.If(~vec_cls_ok):
                            m.d.comb += [
                                push.eq(1),
                                push_resolved.eq(1),
                                push_engine.eq(Source.ISSUE),
                                push_status.eq(Status.BAD_SHAPE),
                            ]
                            m.d.sync += evt_reg.bit_select(cur_evt, 1).eq(1)
                            m.next = 'FETCH'
                        with m.Elif(vector.sink.fire):
                            m.d.comb += [
                                push.eq(1),
                                push_engine.eq(Source.VEC)
                            ]
                            m.next = 'FETCH'

                    with m.Elif(is_vbuild):
                        with m.If(vbuild.sink.fire):
                            m.d.comb += [
                                push.eq(1),
                                push_engine.eq(Source.VBUILD)
                            ]
                            m.next = 'FETCH'

                    with m.Else():
                        m.d.comb += [
                            push.eq(1),
                            push_resolved.eq(1),
                            push_engine.eq(Source.ISSUE),
                            push_status.eq(Status.BAD_OPCODE),
                        ]
                        m.next = 'FETCH'

            with m.State('HALT'):
                pass

        # Hold offers until accepted, after the prior stream and result drain.
        act_offer_ok = ~self.stream.active
        m.d.comb += [
            dma.sink.valid.eq(fsm.ongoing('EXEC') & ~self.halt & is_dma),
            dma.sink.bits.store.eq(is_store),
            actload.sink.valid.eq(
                fsm.ongoing('EXEC') & ~self.halt & is_actload
                & act_offer_ok & epi.ready),
            actload.sink.bits.a_base.eq(cur_a_base),
            actload.sink.bits.m_base.eq(cur_m_base),
            actload.sink.bits.k.eq(cur_k),
            vector.sink.valid.eq(
                fsm.ongoing('EXEC') & ~self.halt & is_vector
                & vec_cls_ok),
            vector.sink.bits.op.eq(cur_vec_op),
            vector.sink.bits.n.eq(cur_k),
            vector.sink.bits.a_base.eq(cur_a_base),
            vector.sink.bits.b_base.eq(cur_m_base),
            vector.sink.bits.o_base.eq(cur_r_base),
            vbuild.sink.valid.eq(fsm.ongoing('EXEC') & ~self.halt & is_vbuild),
            vbuild.sink.bits.a_base.eq(cur_a_base),
            vbuild.sink.bits.r_base.eq(cur_r_base),
            vbuild.sink.bits.k.eq(cur_k),
        ]
        m.d.comb += [
            self.stream.sink.valid.eq(
                fsm.ongoing('EXEC') & ~self.halt & is_smatmul
                & r_ok & epi.ready & ~actload.active),
            self.stream.sink.bits.fmt.eq(cur_fmt),
            self.stream.sink.bits.k.eq(cur_k),
            self.stream.sink.bits.rows.eq(cur_rows),
            self.stream.sink.bits.p_base.eq(cur_dma.dram_addr),
            self.stream.sink.bits.m_base.eq(cur_dma.dram_stride),
        ]
        for name in DMA_DESC_FIELDS:
            m.d.comb += getattr(dma.sink.bits, name).eq(getattr(cur_dma, name))

        # Recovery quiescence must cover the reader: the stream unit
        # only reports done after its reader responses landed and its
        # queues flushed, so ~active implies no stream reads are in
        # flight on the public master.
        m.d.comb += self.memory_idle.eq(~dma.active & ~self.stream.active)
        m.d.comb += self.busy.eq(
            fsm.ongoing('EXEC') | (count != 0)
            | (fsm.ongoing('FETCH') & cmd_q.deq.valid)
            | dma.active | epi.active
            | actload.active | vector.active | vbuild.active
            | self.stream.active)

        m.d.sync += self.cycle_count.eq(self.cycle_count + 1)

        m.d.comb += [
            self.dram_bytes.eq(dma.dram_bytes),
            self.dma_stalls.eq(dma.stall_cycles),
            self.tmem_conflicts.eq(tmem.conflicts),
            self.useful_macs.eq(self.stream.useful_macs),
            self.array_starve.eq(self.stream.starve_cycles),
            self.mm_credit_stalls.eq(self.stream.credit_stalls),
            self.mm_run_cycles.eq(self.stream.run_cycles),
            self.mm_drain_cycles.eq(self.stream.drain_cycles),
            self.epi_records.eq(epi.records),
            self.epi_active_cycles.eq(epi.active_cycles),
            self.epi_stalls.eq(epi.stall_cycles),
            self.epi_cast_ops.eq(epi.cast_ops),
            self.epi_mul_ops.eq(epi.mul_ops),
            self.epi_add_ops.eq(epi.add_ops),
        ]

        return m
