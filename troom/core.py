"""Top-level integration for the troom tensor accelerator.

troom is a standalone, explicitly scheduled tensor accelerator. The
complete engine integrates DRAM-to-TMEM DMA, banked tensor memory, an
activation-stationary systolic array, and an FP32 vector/reduction unit
behind a descriptor-based command interface.

``Core`` provides the executable command contract, TMEM, the tensor
DMA, the matrix datapath, and the FP32 vector/reduction unit:

* a host command stream (``cmd``) and a completion record stream
  (``resp``) carrying per-command sequence numbers and status codes,
* ordered command issue with asynchronous engines: ``LOAD_2D``/
  ``STORE_2D`` descriptors stream through the DMA, ``QUANTIZE`` copies
  a host-quantized activation image from TMEM into the matrix unit's
  ACT RAM, ``MATMUL`` runs the matrix unit and its FP32 epilogue, and
  ``VECTOR``/``REDUCE`` run the vector unit's elementwise and
  reduction ops over TMEM regions, all while issue continues,
* an in-order completion tracker: every command produces exactly one
  completion record, and an engine command's record is emitted only
  when its work truly completes -- for a load, the final beat
  committed into TMEM; for a store, the final AXI B response; for a
  matmul, the last FP32 result beat committed to TMEM; for a vector
  command, its final result beat -- with errors propagated into its
  status,
* ``WAIT``/``SIGNAL`` completion events; engine commands set their
  event when they finish, including error completions, so a ``WAIT``
  cannot deadlock on a failed transfer,
* ``END`` sequence termination that drains all previously issued
  commands (a sticky ``done`` means every required write has
  completed),
* banked TMEM with one shared read path (round-robin across the DMA
  store reader, the weight feeder, the activation loader, and the
  vector unit, with tagged response routing) and one shared write
  path (round-robin across the DMA fill, the epilogue's result drain,
  and the vector unit's result beats),
* cycle, completion, DRAM-byte, DMA-stall, TMEM-conflict, useful-MAC,
  and array-starvation counters, plus the M3b cycle-breakdown window:
  matrix RUN/DRAIN cycles and output-credit stalls, and the FP32
  epilogue's active/stall cycles, accepted records, and shared FPU
  issue counts.

Events are level-held flags: ``SIGNAL`` sets one, ``WAIT`` retires once
its flag is set and then clears it. Because issue is ordered, a ``WAIT``
can only be satisfied by an earlier command or by an asynchronous
engine completion; a ``SIGNAL`` issued after a blocking ``WAIT`` is not
fetched until the ``WAIT`` retires, so it cannot unblock it.

Every datapath opcode is integrated: reserved opcode encodings still
complete with ``BAD_OPCODE``, a ``VECTOR``/``REDUCE`` command whose
``vec_op`` sub-operation belongs to the other opcode class is rejected
at issue with ``BAD_SHAPE``, and the engines reject malformed
descriptors themselves.
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
from troom.matrix import MatrixUnit
from troom.quantize import ActLoader
from troom.tmem import TMEM
from troom.vector import VectorUnit

from roomsoc.interconnect.axi import AXIInterface
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
ENGINE_SOURCES = (Source.DMA, Source.MATMUL, Source.ACT, Source.VEC)


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
        # activity, sink backpressure, record acceptance, and shared
        # FPU issue counts.
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

        tmem = m.submodules.tmem = TMEM(self.params)
        dma = m.submodules.dma = DMA(self.params,
                                     self.bus,
                                     mem_base=self.mem_base,
                                     mem_size=self.mem_size)
        matrix = m.submodules.matrix = MatrixUnit(self.params)
        epi = m.submodules.epi = Epilogue(self.params)
        actload = m.submodules.actload = ActLoader(self.params)
        vector = m.submodules.vector = VectorUnit(self.params)

        # The segment stream flows from the matrix unit's output queue
        # into the epilogue; the epilogue's acceptance paces the matrix
        # unit through its output-credit accounting. ACT uploads and
        # new matmuls wait until the previous result drain completes.
        m.d.comb += [
            matrix.segs.connect(epi.sink),
            actload.act_wr.connect(matrix.act_wr),
        ]

        cur_rows = Signal(self.rows_bits)
        cur_r_base = Signal(self.tmem_addr_width)
        m.d.comb += [epi.rows.eq(cur_rows), epi.r_base.eq(cur_r_base)]

        # A matmul's result drain starts when the matrix unit accepts a
        # well-formed descriptor; a rejected one never reaches the
        # epilogue.
        m.d.comb += epi.start.eq(matrix.sink.fire & matrix.start_ok)

        #
        # TMEM port arbitration. Reads: the DMA store reader, the
        # weight feeder, the activation loader, and the vector unit
        # share the single request/response channel round-robin, with
        # the owner tag routing the response back. Writes: the DMA
        # fill, the epilogue's result drain, and the vector unit's
        # result beats share the write channel.
        #
        rreqs = [
            dma.rreq, matrix.tmem_rreq, actload.tmem_rreq, vector.tmem_rreq
        ]
        rresps = [
            dma.rresp, matrix.tmem_rresp, actload.tmem_rresp, vector.tmem_rresp
        ]

        n_r = len(rreqs)
        r_valid = Array([r.valid for r in rreqs])
        r_addr = Array([r.bits.addr for r in rreqs])

        def rot4(rr, q):
            """Rotate past four requesters without a width wrap."""
            v = rr + q
            return Mux(v >= n_r, v - n_r, v)

        r_rr = Signal(2)
        r_found = Signal()
        r_sel = Signal(2)
        for q in reversed(range(n_r)):
            idx = rot4(r_rr, q)
            with m.If(r_valid[idx]):
                m.d.comb += [r_found.eq(1), r_sel.eq(idx)]

        r_owner = Signal(2)
        m.d.comb += [
            tmem.rreq.valid.eq(r_found),
            tmem.rreq.bits.addr.eq(r_addr[r_sel]),
        ]
        for i, r in enumerate(rreqs):
            m.d.comb += r.ready.eq(tmem.rreq.ready & r_found & (r_sel == i))
        with m.If(tmem.rreq.fire):
            m.d.sync += [r_owner.eq(r_sel), r_rr.eq(rot4(r_sel, 1))]

        rresp_ready = Const(0)
        for i, r in enumerate(rresps):
            m.d.comb += [
                r.valid.eq(tmem.rresp.valid & (r_owner == i)),
                r.bits.data.eq(tmem.rresp.bits.data),
            ]
            rresp_ready = rresp_ready | ((r_owner == i) & r.ready)
        m.d.comb += tmem.rresp.ready.eq(rresp_ready)

        wreqs = [dma.tmem_w, epi.tmem_w, vector.tmem_w]
        w_valid = Array([w.valid for w in wreqs])
        w_addr = Array([w.bits.addr for w in wreqs])
        w_data = Array([w.bits.data for w in wreqs])

        n_w = len(wreqs)

        def rot3(rr, q):
            """Rotate past three writers without a width wrap."""
            v = rr + q
            return Mux(v >= n_w, v - n_w, v)

        w_rr = Signal(2)
        w_found = Signal()
        w_sel = Signal(2)
        for q in reversed(range(n_w)):
            idx = rot3(w_rr, q)
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
            m.d.sync += w_rr.eq(rot3(w_sel, 1))

        cmd_q = m.submodules.cmd_q = Queue(self.cmd_queue_depth,
                                           Command,
                                           self.params,
                                           flow=False)
        m.d.comb += self.cmd.connect(cmd_q.enq)

        evt_reg = Signal(self.n_events)

        cur_opcode = Signal(Opcode)
        cur_evt = Signal(self.evt_bits)
        cur_seq = Signal(self.seq_bits)
        cur_dma = DmaDescriptor(self.params)
        cur_fmt = Signal(Format)
        cur_vec_op = Signal(VecOp)
        cur_k = Signal(self.lg_act_k + 1)
        cur_w_base = Signal(self.tmem_addr_width)
        cur_w_stride = Signal(self.tmem_addr_width)
        cur_a_base = Signal(self.tmem_addr_width)
        cur_m_base = Signal(self.tmem_addr_width)

        is_load = cur_opcode == Opcode.LOAD_2D
        is_store = cur_opcode == Opcode.STORE_2D
        is_dma = is_load | is_store
        is_wait = cur_opcode == Opcode.WAIT
        is_signal = cur_opcode == Opcode.SIGNAL
        is_end = cur_opcode == Opcode.END
        is_matmul = cur_opcode == Opcode.MATMUL
        is_actload = cur_opcode == Opcode.QUANTIZE
        is_vector = (cur_opcode == Opcode.VECTOR) \
            | (cur_opcode == Opcode.REDUCE)

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

        evt_qs = []
        res_qs = []
        for _ in ENGINE_SOURCES:
            evt_q = Queue(n_slots, Record,
                          [('evt', self.evt_bits, Direction.FANOUT)])
            res_q = Queue(n_slots,
                          Record,
                          [('status', Shape.cast(Status), Direction.FANOUT),
                           ('evt', self.evt_bits, Direction.FANOUT)],
                          flow=False)
            evt_qs.append(evt_q)
            res_qs.append(res_q)
        dma_evt_q, mm_evt_q, act_evt_q, vec_evt_q = evt_qs
        dma_res_q, mm_res_q, act_res_q, vec_res_q = res_qs
        for name, q in zip(('dma_evt_q', 'mm_evt_q', 'act_evt_q', 'vec_evt_q',
                            'dma_res_q', 'mm_res_q', 'act_res_q', 'vec_res_q'),
                           evt_qs + res_qs):
            setattr(m.submodules, name, q)

        mm_bad = matrix.done & (matrix.status == Status.BAD_SHAPE)
        mm_done = mm_bad | epi.done

        m.d.comb += [
            dma_evt_q.enq.valid.eq(dma.sink.fire),
            dma_evt_q.enq.bits.evt.eq(cur_evt),
            mm_evt_q.enq.valid.eq(matrix.sink.fire),
            mm_evt_q.enq.bits.evt.eq(cur_evt),
            act_evt_q.enq.valid.eq(actload.sink.fire),
            act_evt_q.enq.bits.evt.eq(cur_evt),
            vec_evt_q.enq.valid.eq(vector.sink.fire),
            vec_evt_q.enq.bits.evt.eq(cur_evt),
            dma_evt_q.deq.ready.eq(dma.done),
            mm_evt_q.deq.ready.eq(mm_done),
            act_evt_q.deq.ready.eq(actload.done),
            vec_evt_q.deq.ready.eq(vector.done),
            dma_res_q.enq.valid.eq(dma.done),
            dma_res_q.enq.bits.status.eq(dma.status),
            dma_res_q.enq.bits.evt.eq(dma_evt_q.deq.bits.evt),
            mm_res_q.enq.valid.eq(mm_done),
            mm_res_q.enq.bits.status.eq(
                Mux(mm_bad, Status.BAD_SHAPE, epi.status)),
            mm_res_q.enq.bits.evt.eq(mm_evt_q.deq.bits.evt),
            act_res_q.enq.valid.eq(actload.done),
            act_res_q.enq.bits.status.eq(actload.status),
            act_res_q.enq.bits.evt.eq(act_evt_q.deq.bits.evt),
            vec_res_q.enq.valid.eq(vector.done),
            vec_res_q.enq.bits.status.eq(vector.status),
            vec_res_q.enq.bits.evt.eq(vec_evt_q.deq.bits.evt),
        ]
        for q in res_qs:
            with m.If(q.enq.fire):
                m.d.sync += evt_reg.bit_select(q.enq.bits.evt, 1).eq(1)

        # Indexed by source value; the issue source never consults
        # these.
        res_valid_arr = Array([Const(0)] + [q.deq.valid for q in res_qs])
        res_status_arr = Array([Const(0, Shape.cast(Status))] +
                               [q.deq.bits.status for q in res_qs])

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
        for e, res_q in zip(ENGINE_SOURCES, res_qs):
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
                            cur_w_base.eq(deq.w_base),
                            cur_w_stride.eq(deq.w_stride),
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

                    with m.Elif(is_matmul):
                        # A result region the engine cannot express
                        # completes at issue without disturbing either
                        # engine.
                        with m.If(~r_ok):
                            m.d.comb += [
                                push.eq(1),
                                push_resolved.eq(1),
                                push_engine.eq(Source.ISSUE),
                                push_status.eq(Status.BAD_SHAPE),
                            ]
                            m.d.sync += evt_reg.bit_select(cur_evt, 1).eq(1)
                            m.next = 'FETCH'
                        with m.Elif(matrix.sink.fire):
                            m.d.comb += [
                                push.eq(1),
                                push_engine.eq(Source.MATMUL)
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

        # Engine descriptor offers, held until accepted.
        m.d.comb += [
            dma.sink.valid.eq(fsm.ongoing('EXEC') & ~self.halt & is_dma),
            dma.sink.bits.store.eq(is_store),
            matrix.sink.valid.eq(
                fsm.ongoing('EXEC') & ~self.halt & is_matmul
                & r_ok & epi.ready & ~actload.active),
            matrix.sink.bits.fmt.eq(cur_fmt),
            matrix.sink.bits.k.eq(cur_k),
            matrix.sink.bits.rows.eq(cur_rows),
            matrix.sink.bits.w_base.eq(cur_w_base),
            matrix.sink.bits.w_stride.eq(cur_w_stride),
            actload.sink.valid.eq(
                fsm.ongoing('EXEC') & ~self.halt & is_actload
                & ~matrix.active & epi.ready),
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
        ]
        for name in DMA_DESC_FIELDS:
            m.d.comb += getattr(dma.sink.bits, name).eq(getattr(cur_dma, name))

        m.d.comb += [
            self.memory_idle.eq(~dma.active),
            self.busy.eq(
                fsm.ongoing('EXEC') | (count != 0)
                | (fsm.ongoing('FETCH') & cmd_q.deq.valid)
                | dma.active | matrix.active | epi.active
                | actload.active | vector.active),
        ]

        m.d.sync += self.cycle_count.eq(self.cycle_count + 1)

        m.d.comb += [
            self.dram_bytes.eq(dma.dram_bytes),
            self.dma_stalls.eq(dma.stall_cycles),
            self.tmem_conflicts.eq(tmem.conflicts),
            self.useful_macs.eq(matrix.useful_macs),
            self.array_starve.eq(matrix.starve_cycles),
            self.mm_credit_stalls.eq(matrix.credit_stalls),
            self.mm_run_cycles.eq(matrix.run_cycles),
            self.mm_drain_cycles.eq(matrix.drain_cycles),
            self.epi_records.eq(epi.records),
            self.epi_active_cycles.eq(epi.active_cycles),
            self.epi_stalls.eq(epi.stall_cycles),
            self.epi_cast_ops.eq(epi.cast_ops),
            self.epi_mul_ops.eq(epi.mul_ops),
            self.epi_add_ops.eq(epi.add_ops),
        ]

        return m
