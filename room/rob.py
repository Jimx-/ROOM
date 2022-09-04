from amaranth import *

from room.alu import ExecResp
from room.types import MicroOp
from room.branch import BranchUpdate


def _incr(signal, modulo):
    if modulo == 2**len(signal):
        return signal + 1
    else:
        return Mux(signal == modulo - 1, 0, signal + 1)


class CommitReq:

    def __init__(self, params, name=None):
        core_width = params['core_width']

        self.valids = Signal(core_width,
                             name=(name is not None) and f'{name}_valids'
                             or None)
        self.uops = [
            MicroOp(params,
                    name=(name is not None) and f'{name}_uop{i}' or None)
            for i in range(core_width)
        ]

    def eq(self, rhs):
        return [self.valids.eq(rhs.valids)
                ] + [luop.eq(ruop) for luop, ruop in zip(self.uops, rhs.uops)]


class ReorderBuffer(Elaboratable):

    def __init__(self, num_wakeup_ports, params):
        self.params = params
        self.core_width = params['core_width']
        self.num_rob_rows = params['num_rob_rows']
        mem_width = params['mem_width']

        self.enq_uops = [
            MicroOp(params, name=f'enq_uop{i}') for i in range(self.core_width)
        ]
        self.enq_valids = Signal(self.core_width)
        self.enq_partial_stalls = Signal()

        self.wb_resps = [
            ExecResp(params, name=f'wb_resp{i}')
            for i in range(num_wakeup_ports)
        ]

        self.lsu_clear_busy_valids = Signal(mem_width)
        self.lsu_clear_busy_idx = [
            Signal(range(self.core_width * self.num_rob_rows),
                   name=f'lsu_clear_busy_idx{i}') for i in range(mem_width)
        ]

        self.commit_req = CommitReq(params, name='commit_req')

        self.head_idx = Signal(range(self.core_width * self.num_rob_rows))
        self.tail_idx = Signal(range(self.core_width * self.num_rob_rows))

        self.br_update = BranchUpdate(params)

        self.ready = Signal()
        self.empty = Signal()

        self.flush = Signal()

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

        rob_tail = Signal(range(self.num_rob_rows))
        rob_tail_lsb = Signal(range(self.core_width))

        if self.core_width == 1:
            rob_head_idx = rob_head
            rob_tail_idx = rob_tail
        else:
            rob_head_idx = Cat(rob_head_lsb, rob_head)
            rob_tail_idx = Cat(rob_tail_lsb, rob_tail)

        com_row = rob_head

        maybe_full = Signal()
        full = Signal()
        empty = Signal()
        state_is_normal = Signal()

        can_commit = Signal(self.core_width)
        will_commit = Signal(self.core_width)

        rob_head_valids = Signal(self.core_width)
        rob_head_busy = Signal(self.core_width)

        for w in range(self.core_width):
            rob_valid = Array(
                Signal(name=f'rob_bank{w}_valid{i}')
                for i in range(self.num_rob_rows))
            rob_busy = Array(
                Signal(name=f'rob_bank{w}_busy{i}')
                for i in range(self.num_rob_rows))
            rob_uop = Array(
                MicroOp(self.params, name=f'rob_bank{w}_uop{i}')
                for i in range(self.num_rob_rows))

            with m.If(self.enq_valids[w]):
                m.d.sync += [
                    rob_valid[rob_tail].eq(1),
                    rob_busy[rob_tail].eq(1),
                    rob_uop[rob_tail].eq(self.enq_uops[w]),
                ]

            for wb in self.wb_resps:
                uop = wb.uop
                row_idx = get_row(uop.rob_idx)
                with m.If(wb.valid & (get_bank(uop.rob_idx) == w)):
                    m.d.sync += rob_busy[row_idx].eq(0)

            for clr_val, clr_idx in zip(self.lsu_clear_busy_valids,
                                        self.lsu_clear_busy_idx):
                with m.If(clr_val & (get_bank(clr_idx) == w)):
                    m.d.sync += rob_busy[get_row(clr_idx)].eq(0)

            m.d.comb += [
                can_commit[w].eq(rob_valid[rob_head]
                                 & ~rob_busy[rob_head]),
                self.commit_req.valids[w].eq(will_commit[w]),
                self.commit_req.uops[w].eq(rob_uop[com_row]),
            ]

            for i in range(self.num_rob_rows):
                with m.If(self.br_update.uop_killed(rob_uop[i])):
                    m.d.sync += rob_valid[i].eq(0)
                with m.Elif(rob_valid[i]):
                    m.d.sync += rob_uop[i].br_mask.eq(
                        self.br_update.get_new_br_mask(rob_uop[i].br_mask))

            with m.If(will_commit[w]):
                m.d.sync += rob_valid[rob_head].eq(0)

            m.d.comb += rob_head_valids[w].eq(rob_valid[rob_head])
            m.d.comb += rob_head_busy[w].eq(rob_busy[rob_head])

        block_commit = 0
        for w in range(self.core_width):
            m.d.comb += will_commit[w].eq(can_commit[w] & ~block_commit)
            block_commit |= (rob_head_valids[w] & ~can_commit[w])

        do_enq = Signal()
        do_deq = Signal()

        partial_row = Signal()
        with m.If(self.enq_valids != 0):
            m.d.sync += partial_row.eq(self.enq_partial_stalls)

        commit_row = (will_commit != 0) & (will_commit == rob_head_valids) & ~(
            partial_row & (rob_head == rob_tail) & ~maybe_full)

        with m.If(commit_row):
            m.d.sync += [
                rob_head.eq(_incr(rob_head, self.num_rob_rows)),
                rob_head_lsb.eq(0),
            ]
            m.d.comb += do_deq.eq(1)

        with m.If(self.br_update.br_res.mispredict):
            m.d.sync += [
                rob_tail.eq(
                    _incr(get_row(self.br_update.br_res.uop.rob_idx),
                          self.num_rob_rows)),
                rob_tail_lsb.eq(0),
            ]
        with m.Elif((self.enq_valids != 0) & ~self.enq_partial_stalls):
            m.d.sync += [
                rob_tail.eq(_incr(rob_tail, self.num_rob_rows)),
                rob_tail_lsb.eq(0),
            ]
            m.d.comb += do_enq.eq(1)

        m.d.sync += maybe_full.eq(~do_deq & (do_enq | maybe_full)
                                  | (self.br_update.mispredict_mask != 0))
        m.d.comb += [
            full.eq((rob_tail == rob_head) & maybe_full),
            empty.eq((rob_tail == rob_head) & (rob_head_valids == 0))
        ]

        with m.FSM():
            with m.State('NORMAL'):
                m.d.comb += state_is_normal.eq(1)

                for w in range(self.core_width):
                    with m.If(self.enq_valids[w]
                              & self.enq_uops[w].clear_pipeline):
                        m.next = 'WAIT_EMPTY'

            with m.State('WAIT_EMPTY'):
                with m.If(empty):
                    m.next = 'NORMAL'

        m.d.comb += [
            self.head_idx.eq(rob_head_idx),
            self.tail_idx.eq(rob_tail_idx),
            self.ready.eq(state_is_normal & ~full),
            self.empty.eq(empty),
        ]

        return m
