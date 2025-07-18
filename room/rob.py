from amaranth import *
from amaranth import tracer

from enum import IntEnum

from room.consts import *
from room.fu import ExecResp
from room.types import HasCoreParams, MicroOp
from room.branch import BranchUpdate
from room.exc import Cause
from room.utils import wrap_incr, wrap_decr, is_older

from roomsoc.interconnect.stream import Valid


class FlushType(IntEnum):
    NONE = 0
    EXCEPT = 1
    ERET = 2
    REFETCH = 3
    NEXT = 4


class CommitReq(HasCoreParams):

    def __init__(self, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.valids = Signal(self.core_width, name=f'{name}_valids')
        self.uops = [
            MicroOp(params, name=f'{name}_uop{i}')
            for i in range(self.core_width)
        ]

        self.fflags = Valid(Signal, 5)

        self.rollback = Signal(name=f'{name}_rollback')
        self.rollback_valids = Signal(self.core_width,
                                      name=f'{name}_rollback_valids')

    def eq(self, rhs):
        return [
            getattr(self, name).eq(getattr(rhs, name)) for name in (
                'valids',
                'fflags',
                'rollback',
                'rollback_valids',
            )
        ] + [luop.eq(ruop) for luop, ruop in zip(self.uops, rhs.uops)]


class CommitExceptionReq(HasCoreParams, Record):

    def __init__(self, params, name=None, src_loc_at=0):
        HasCoreParams.__init__(self, params)

        Record.__init__(self, [
            ('ftq_idx', range(self.ftq_size)),
            ('edge_inst', 1),
            ('pc_lsb', range(self.fetch_bytes)),
            ('is_rvc', 1),
            ('cause', self.xlen),
            ('badaddr', self.xlen),
            ('inst', 32),
            ('flush_type', FlushType),
        ],
                        name=name,
                        src_loc_at=src_loc_at + 1)


class Exception(HasCoreParams):

    def __init__(self, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.uop = MicroOp(params, name=f'{name}_uop')
        self.cause = Signal(self.xlen, name=f'{name}_cause')
        self.badaddr = Signal(self.core_max_addr_bits, name=f'{name}_badaddr')

    def eq(self, rhs):
        return [
            getattr(self, name).eq(getattr(rhs, name)) for name in (
                'uop',
                'cause',
                'badaddr',
            )
        ]


class ReorderBuffer(HasCoreParams, Elaboratable):

    def __init__(self, num_wakeup_ports, params):
        super().__init__(params)

        self.enq_uops = [
            MicroOp(params, name=f'enq_uop{i}') for i in range(self.core_width)
        ]
        self.enq_valids = Signal(self.core_width)
        self.enq_partial_stalls = Signal()

        self.exc_fetch_pc = Signal(self.vaddr_bits_extended)

        self.wb_resps = [
            Valid(ExecResp,
                  max(self.xlen, self.flen),
                  params,
                  name=f'wb_resp{i}') for i in range(num_wakeup_ports)
        ]

        self.lsu_clear_busy = [
            Valid(Signal,
                  range(self.core_width * self.num_rob_rows),
                  name=f'lsu_clear_busy{i}') for i in range(self.mem_width + 1)
        ]

        self.commit_req = CommitReq(params, name='commit_req')

        self.commit_exc = Valid(CommitExceptionReq, params)

        self.head_idx = Signal(range(self.core_width * self.num_rob_rows))
        self.pnr_idx = Signal(range(self.core_width * self.num_rob_rows))
        self.tail_idx = Signal(range(self.core_width * self.num_rob_rows))

        self.br_update = BranchUpdate(params)

        self.lsu_exc = Valid(Exception, params, name='lsu_exc')

        self.ready = Signal()
        self.empty = Signal()

        self.csr_stall = Signal()

        self.flush = Valid(CommitExceptionReq, params)

        self.commit_load_at_head = Signal()

    def elaborate(self, platform):
        m = Module()

        def get_bank(rob_idx):
            if self.core_width == 1:
                return 0
            else:
                return rob_idx[0:Shape.cast(range(self.core_width)).width]

        def get_row(rob_idx):
            if self.core_width == 1:
                return rob_idx
            else:
                return rob_idx >> Shape.cast(range(self.core_width)).width

        rob_head = Signal(range(self.num_rob_rows))
        rob_head_lsb = Signal(range(self.core_width))

        rob_pnr = Signal(range(self.num_rob_rows))
        rob_pnr_lsb = Signal(range(self.core_width))

        rob_tail = Signal(range(self.num_rob_rows))
        rob_tail_lsb = Signal(range(self.core_width))

        if self.core_width == 1:
            rob_head_idx = rob_head
            rob_pnr_idx = rob_pnr
            rob_tail_idx = rob_tail
        else:
            rob_head_idx = Cat(rob_head_lsb, rob_head)
            rob_pnr_idx = Cat(rob_pnr_lsb, rob_pnr)
            rob_tail_idx = Cat(rob_tail_lsb, rob_tail)

        maybe_full = Signal()
        full = Signal()
        empty = Signal()

        state_is_normal = Signal()
        state_is_wait_empty = Signal()
        state_is_rollback = Signal()

        commit_idx = Mux(state_is_rollback, rob_tail, rob_head)

        can_commit = Signal(self.core_width)
        can_throw_exception = Signal(self.core_width)
        will_commit = Signal(self.core_width)

        rob_head_valids = Signal(self.core_width)
        rob_head_busy = Signal(self.core_width)
        rob_head_uses_ldq = Signal(self.core_width)
        rob_pnr_unsafe = Signal(self.core_width)
        rob_tail_valids = Signal(self.core_width)

        for w in range(self.core_width):
            rob_valid = Array(
                Signal(name=f'rob_bank{w}_valid{i}')
                for i in range(self.num_rob_rows))
            rob_busy = Array(
                Signal(name=f'rob_bank{w}_busy{i}')
                for i in range(self.num_rob_rows))
            rob_unsafe = Array(
                Signal(name=f'rob_bank{w}_unsafe{i}')
                for i in range(self.num_rob_rows))
            rob_uop = Array(
                MicroOp(self.params, name=f'rob_bank{w}_uop{i}')
                for i in range(self.num_rob_rows))
            rob_exception = Array(
                Signal(name=f'rob_bank{w}_exception{i}')
                for i in range(self.num_rob_rows))

            with m.If(self.enq_valids[w]):
                m.d.sync += [
                    rob_valid[rob_tail].eq(1),
                    rob_busy[rob_tail].eq(~(self.enq_uops[w].is_fence
                                            | self.enq_uops[w].is_fencei)),
                    rob_unsafe[rob_tail].eq(self.enq_uops[w].is_unsafe()),
                    rob_uop[rob_tail].eq(self.enq_uops[w]),
                    rob_exception[rob_tail].eq(self.enq_uops[w].exception),
                ]

            for wb in self.wb_resps:
                uop = wb.bits.uop
                row_idx = get_row(uop.rob_idx)
                with m.If(wb.valid & (get_bank(uop.rob_idx) == w)):
                    m.d.sync += [
                        rob_busy[row_idx].eq(0),
                        rob_unsafe[row_idx].eq(0),
                    ]

            for clr in self.lsu_clear_busy:
                with m.If(clr.valid & (get_bank(clr.bits) == w)):
                    m.d.sync += [
                        rob_busy[get_row(clr.bits)].eq(0),
                        rob_unsafe[get_row(clr.bits)].eq(0),
                    ]

            with m.If(self.lsu_exc.valid
                      & (get_bank(self.lsu_exc.bits.uop.rob_idx) == w)):
                m.d.sync += [
                    rob_exception[get_row(
                        self.lsu_exc.bits.uop.rob_idx)].eq(1),
                    rob_uop[get_row(
                        self.lsu_exc.bits.uop.rob_idx)].exc_cause.eq(
                            self.lsu_exc.bits.cause),
                ]

            m.d.comb += [
                can_throw_exception[w].eq(rob_valid[rob_head]
                                          & rob_exception[rob_head]),
                can_commit[w].eq(rob_valid[rob_head]
                                 & ~rob_busy[rob_head] & ~self.csr_stall),
                self.commit_req.valids[w].eq(will_commit[w]),
                self.commit_req.uops[w].eq(rob_uop[commit_idx]),
            ]

            rollback_row = state_is_rollback & ~full
            m.d.comb += self.commit_req.rollback_valids[w].eq(
                rollback_row & rob_valid[commit_idx])

            with m.If(rollback_row):
                m.d.sync += [
                    rob_valid[commit_idx].eq(0),
                    rob_exception[commit_idx].eq(0),
                ]

            for i in range(self.num_rob_rows):
                with m.If(self.br_update.uop_killed(rob_uop[i])):
                    m.d.sync += rob_valid[i].eq(0)
                with m.Elif(rob_valid[i]):
                    m.d.sync += rob_uop[i].br_mask.eq(
                        self.br_update.get_new_br_mask(rob_uop[i].br_mask))

            with m.If(will_commit[w]):
                m.d.sync += rob_valid[rob_head].eq(0)

            m.d.comb += [
                rob_head_valids[w].eq(rob_valid[rob_head]),
                rob_head_busy[w].eq(rob_busy[rob_head]),
                rob_head_uses_ldq[w].eq(rob_uop[rob_head].uses_ldq),
                rob_pnr_unsafe[w].eq(rob_valid[rob_pnr]
                                     & (rob_unsafe[rob_pnr]
                                        | rob_exception[rob_pnr])),
                rob_tail_valids[w].eq(rob_valid[rob_tail]),
            ]

        exception_thrown = Signal()
        exception_thrown_d1 = Signal()
        exception_thrown_d2 = Signal()

        will_throw_exception = Signal()
        block_commit = (~state_is_normal & ~state_is_wait_empty
                        ) | exception_thrown_d1 | exception_thrown_d2
        block_exc = 0
        for w in range(self.core_width):
            next_throw_exception = Signal()

            m.d.comb += [
                next_throw_exception.eq((can_throw_exception[w] & ~block_commit
                                         & ~block_exc) | will_throw_exception),
                will_commit[w].eq(can_commit[w] & ~can_throw_exception[w]
                                  & ~block_commit)
            ]
            block_commit |= (rob_head_valids[w] &
                             (~can_commit[w] | can_throw_exception[w]))
            block_exc = will_commit[w]

            will_throw_exception = next_throw_exception

        #
        # Exception
        #

        m.d.comb += exception_thrown.eq(will_throw_exception)
        m.d.sync += [
            exception_thrown_d1.eq(exception_thrown),
            exception_thrown_d2.eq(exception_thrown_d1),
        ]

        m.d.comb += [
            self.commit_req.rollback.eq(state_is_rollback),
            self.commit_exc.valid.eq(exception_thrown & (
                self.commit_exc.bits.cause != Cause.MEM_ORDERING_FAULT)),
        ]

        commit_exc_uop = MicroOp(self.params, name='commit_exc_uop')
        for w in reversed(range(self.core_width)):
            with m.If(rob_head_valids[w]):
                m.d.comb += commit_exc_uop.eq(self.commit_req.uops[w])

        r_exc_valid = Signal()
        r_exc_uop = MicroOp(self.params)
        r_exc_badaddr = Signal(self.core_max_addr_bits)

        m.d.comb += [
            self.commit_exc.bits.ftq_idx.eq(commit_exc_uop.ftq_idx),
            self.commit_exc.bits.edge_inst.eq(commit_exc_uop.edge_inst),
            self.commit_exc.bits.pc_lsb.eq(commit_exc_uop.pc_lsb),
            self.commit_exc.bits.is_rvc.eq(commit_exc_uop.is_rvc),
            self.commit_exc.bits.cause.eq(commit_exc_uop.exc_cause),
            self.commit_exc.bits.inst.eq(commit_exc_uop.inst),
            self.commit_exc.bits.badaddr.eq(r_exc_badaddr.as_signed()),
        ]

        next_exc_uop = MicroOp(self.params)
        m.d.comb += next_exc_uop.eq(r_exc_uop)

        enq_exceptions = Signal(self.core_width)
        for w in range(self.core_width):
            m.d.comb += enq_exceptions[w].eq(self.enq_valids[w]
                                             & self.enq_uops[w].exception)

        with m.If(~(self.flush.valid | exception_thrown) & ~state_is_rollback):
            with m.If(self.lsu_exc.valid):
                with m.If(~r_exc_valid
                          | is_older(self.lsu_exc.bits.uop.rob_idx,
                                     r_exc_uop.rob_idx, rob_head_idx)):
                    m.d.sync += [
                        r_exc_valid.eq(1),
                        r_exc_badaddr.eq(self.lsu_exc.bits.badaddr),
                    ]
                    m.d.comb += [
                        next_exc_uop.eq(self.lsu_exc.bits.uop),
                        next_exc_uop.exc_cause.eq(self.lsu_exc.bits.cause),
                    ]

            with m.Elif(~r_exc_valid & enq_exceptions.any()):
                m.d.sync += [
                    r_exc_valid.eq(1),
                    r_exc_badaddr.eq(self.exc_fetch_pc + next_exc_uop.pc_lsb),
                ]

                for w in reversed(range(self.core_width)):
                    with m.If(enq_exceptions[w]):
                        m.d.comb += next_exc_uop.eq(self.enq_uops[w])

        m.d.sync += [
            r_exc_uop.eq(next_exc_uop),
            r_exc_uop.br_mask.eq(
                self.br_update.get_new_br_mask(next_exc_uop.br_mask)),
        ]
        with m.If(self.flush.valid | self.br_update.uop_killed(next_exc_uop)):
            m.d.sync += r_exc_valid.eq(0)

        flush_commit_mask = Signal(self.core_width)
        for w in range(self.core_width):
            m.d.comb += flush_commit_mask[w].eq(
                self.commit_req.valids[w]
                & self.commit_req.uops[w].flush_on_commit)

        flush_uop = MicroOp(self.params, name='flush_uop')
        m.d.comb += flush_uop.eq(commit_exc_uop)
        for w in reversed(range(self.core_width)):
            with m.If(flush_commit_mask[w]):
                m.d.comb += flush_uop.eq(self.commit_req.uops[w])

        flush_commit = flush_commit_mask != 0
        m.d.comb += [
            self.flush.valid.eq(exception_thrown | flush_commit),
            self.flush.bits.ftq_idx.eq(flush_uop.ftq_idx),
            self.flush.bits.edge_inst.eq(flush_uop.edge_inst),
            self.flush.bits.pc_lsb.eq(flush_uop.pc_lsb),
            self.flush.bits.is_rvc.eq(flush_uop.is_rvc),
        ]

        with m.If(self.flush.valid):
            with m.If(exception_thrown
                      & (self.commit_exc.bits.cause != Cause.MEM_ORDERING_FAULT
                         )):
                m.d.comb += self.flush.bits.flush_type.eq(FlushType.EXCEPT)
            with m.Elif(flush_commit & (flush_uop.opcode == UOpCode.ERET)):
                m.d.comb += self.flush.bits.flush_type.eq(FlushType.ERET)
            with m.Elif(exception_thrown):
                # Must be a memory ordering failure, just replay the memory load
                m.d.comb += self.flush.bits.flush_type.eq(FlushType.REFETCH)
            with m.Else():
                m.d.comb += self.flush.bits.flush_type.eq(FlushType.NEXT)

        #
        # FP update
        #

        fflags_valid = Signal(self.core_width)
        for w in range(self.core_width):
            m.d.comb += fflags_valid[w].eq(self.commit_req.valids[w]
                                           & self.commit_req.uops[w].fp_valid
                                           & ~self.commit_req.uops[w].uses_stq)
        m.d.comb += self.commit_req.fflags.valid.eq(fflags_valid.any())

        do_enq = Signal()
        do_deq = Signal()

        partial_row = Signal()
        with m.If(self.enq_valids != 0):
            m.d.sync += partial_row.eq(self.enq_partial_stalls)

        commit_row = (will_commit != 0) & (will_commit == rob_head_valids) & ~(
            partial_row & (rob_head == rob_tail) & ~maybe_full)

        with m.If(commit_row):
            m.d.sync += [
                rob_head.eq(wrap_incr(rob_head, self.num_rob_rows)),
                rob_head_lsb.eq(0),
            ]
            m.d.comb += do_deq.eq(1)

        with m.If(state_is_rollback & ((rob_tail != rob_head) | maybe_full)):
            m.d.sync += [
                rob_tail.eq(wrap_decr(rob_tail, self.num_rob_rows)),
                rob_tail_lsb.eq(self.core_width - 1)
            ]
            m.d.comb += do_deq.eq(1)
        with m.Elif(state_is_rollback
                    & ~((rob_tail != rob_head) | maybe_full)):
            m.d.sync += rob_tail_lsb.eq(rob_head_lsb)
        with m.Elif(self.br_update.br_res.mispredict):
            m.d.sync += [
                rob_tail.eq(
                    wrap_incr(get_row(self.br_update.br_res.uop.rob_idx),
                              self.num_rob_rows)),
                rob_tail_lsb.eq(0),
            ]
        with m.Elif((self.enq_valids != 0) & ~self.enq_partial_stalls):
            m.d.sync += [
                rob_tail.eq(wrap_incr(rob_tail, self.num_rob_rows)),
                rob_tail_lsb.eq(0),
            ]
            m.d.comb += do_enq.eq(1)
        with m.Elif(self.enq_valids != 0):
            for w in range(self.core_width - 1):
                with m.If(self.enq_valids[w]):
                    m.d.sync += rob_tail_lsb.eq(w + 1)

        m.d.sync += maybe_full.eq(~do_deq & (do_enq | maybe_full)
                                  | (self.br_update.mispredict_mask != 0))
        m.d.comb += [
            full.eq((rob_tail == rob_head) & maybe_full),
            empty.eq((rob_tail == rob_head) & (rob_head_valids == 0))
        ]

        #
        # Point-of-No-Return
        #

        pnr_maybe_at_tail = Signal()
        safe_to_inc_pnr = state_is_normal | state_is_wait_empty
        pnr_inc_row = ~rob_pnr_unsafe.any() & ((rob_pnr != rob_tail) |
                                               (full & ~pnr_maybe_at_tail))
        with m.If(empty & (self.enq_valids.any())):
            m.d.sync += rob_pnr.eq(rob_head)
            for w in reversed(range(self.core_width)):
                with m.If(self.enq_valids[w]):
                    m.d.sync += rob_pnr_lsb.eq(w)
        with m.Elif(safe_to_inc_pnr & pnr_inc_row):
            m.d.sync += [
                rob_pnr.eq(wrap_incr(rob_pnr, self.num_rob_rows)),
                rob_pnr_lsb.eq(0),
            ]
        with m.Elif(safe_to_inc_pnr
                    & ((rob_pnr != rob_tail) | (full & ~pnr_maybe_at_tail))):
            for w in reversed(range(self.core_width)):
                with m.If(rob_pnr_unsafe[w]):
                    m.d.sync += rob_pnr_lsb.eq(w)
        with m.Elif(safe_to_inc_pnr & ~full & ~empty):
            tail_valid_mask = Signal(self.core_width)
            for w in range(self.core_width):
                with m.If(rob_tail_valids[w]):
                    m.d.comb += tail_valid_mask.eq((1 << (w + 1)) - 1)
            for w in reversed(range(self.core_width)):
                with m.If(rob_pnr_unsafe[w] | ~rob_tail_valids[w]):
                    m.d.sync += rob_pnr_lsb.eq(w)
        with m.Elif(full | pnr_maybe_at_tail):
            m.d.sync += rob_pnr_lsb.eq(0)

        m.d.sync += pnr_maybe_at_tail.eq(~do_deq
                                         & (pnr_inc_row | pnr_maybe_at_tail))

        with m.FSM():
            with m.State('NORMAL'):
                m.d.comb += state_is_normal.eq(1)

                with m.If(exception_thrown_d2):
                    m.next = 'ROLLBACK'
                with m.Else():
                    for w in range(self.core_width):
                        with m.If(self.enq_valids[w]
                                  & self.enq_uops[w].clear_pipeline):
                            m.next = 'WAIT_EMPTY'

            with m.State('ROLLBACK'):
                m.d.comb += state_is_rollback.eq(1)

                with m.If(empty):
                    m.next = 'NORMAL'

            with m.State('WAIT_EMPTY'):
                m.d.comb += state_is_wait_empty.eq(1)

                with m.If(exception_thrown_d1):
                    m.next = 'ROLLBACK'
                with m.Elif(empty):
                    m.next = 'NORMAL'

        m.d.comb += [
            self.head_idx.eq(rob_head_idx),
            self.pnr_idx.eq(rob_pnr_idx),
            self.tail_idx.eq(rob_tail_idx),
            self.ready.eq(state_is_normal & ~full & ~r_exc_valid),
            self.empty.eq(empty),
        ]

        commit_head_uses_ldq = Signal()
        for w in reversed(range(self.core_width)):
            with m.If(rob_head_valids[w]):
                m.d.comb += commit_head_uses_ldq.eq(rob_head_uses_ldq[w])
        m.d.sync += self.commit_load_at_head.eq(commit_head_uses_ldq
                                                & (will_commit == 0))

        return m
