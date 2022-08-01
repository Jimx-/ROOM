from amaranth import *
from amaranth.lib.coding import Encoder
from enum import IntEnum

from room.consts import *
from room.types import MicroOp


class IssueQueueWakeup(Record):

    def __init__(self, num_pregs, name=None):
        self.num_pregs = num_pregs

        super().__init__([('valid', 1), ('pdst', range(num_pregs)),
                          ('poisoned', 1)],
                         name=name)


class IssueSlot(Elaboratable):

    class State(IntEnum):
        INVALID = 0
        VALID_1 = 1
        VALID_2 = 2

    def __init__(self, num_wakeup_ports, params):
        self.params = params
        num_pregs = params['num_pregs']

        self.valid = Signal()

        self.in_uop = MicroOp(self.params)
        self.in_valid = Signal()

        self.out_uop = MicroOp(self.params)

        self.req = Signal()
        self.gnt = Signal()

        self.wakeup_ports = [
            IssueQueueWakeup(num_pregs, name=f'wakeup_port{i}')
            for i in range(num_wakeup_ports)
        ]

        self.kill = Signal()

    def elaborate(self, platform):
        m = Module()

        state = Signal(IssueSlot.State)
        next_state = Signal.like(state)

        m.d.comb += [
            next_state.eq(state),
            self.valid.eq(state != IssueSlot.State.INVALID),
        ]

        with m.If(self.kill):
            m.d.sync += state.eq(IssueSlot.State.INVALID)
        with m.Elif(self.in_valid):
            m.d.sync += state.eq(self.in_uop.issue_uops)
        with m.Else():
            m.d.sync += state.eq(next_state)

        slot_uop = MicroOp(self.params, name='slot_uop')
        next_uop = MicroOp(self.params, name='next_uop')
        m.d.comb += next_uop.eq(Mux(self.in_valid == 1, self.in_uop, slot_uop))

        p1 = Signal()
        p2 = Signal()

        with m.If(self.in_valid):
            m.d.sync += [
                slot_uop.eq(self.in_uop),
                p1.eq(~self.in_uop.prs1_busy),
                p2.eq(~self.in_uop.prs2_busy),
            ]

        for wu in self.wakeup_ports:
            with m.If(wu.valid & (wu.pdst == next_uop.prs1)):
                m.d.sync += p1.eq(1)
            with m.If(wu.valid & (wu.pdst == next_uop.prs2)):
                m.d.sync += p2.eq(1)

        with m.If(self.kill):
            m.d.comb += next_state.eq(IssueSlot.State.INVALID)
        with m.Elif(self.gnt & (state == IssueSlot.State.VALID_1)):
            m.d.comb += next_state.eq(IssueSlot.State.INVALID)

        with m.If(state == IssueSlot.State.VALID_1):
            m.d.comb += self.req.eq(p1 & p2 & ~self.kill)

        m.d.comb += [
            self.out_uop.eq(slot_uop),
            self.out_uop.issue_uops.eq(next_state),
        ]

        return m


class IssueUnit(Elaboratable):

    def __init__(self, issue_width, num_issue_slots, dispatch_width,
                 num_wakeup_ports, params):
        self.params = params
        self.issue_width = issue_width
        self.num_issue_slots = num_issue_slots
        self.dispatch_width = dispatch_width
        self.num_wakeup_ports = num_wakeup_ports
        num_pregs = params['num_pregs']

        self.dis_uops = [
            MicroOp(params, name=f'dis_uop{i}') for i in range(dispatch_width)
        ]
        self.dis_valids = Signal(dispatch_width)

        self.iss_uops = [
            MicroOp(params, name=f'iss_uop{i}') for i in range(issue_width)
        ]
        self.iss_valids = Signal(issue_width)

        self.fu_types = [
            Signal(FUType, name=f'fu_type{i}') for i in range(issue_width)
        ]

        self.wakeup_ports = [
            IssueQueueWakeup(num_pregs, name=f'wakeup_port{i}')
            for i in range(self.num_wakeup_ports)
        ]

        self.flush_pipeline = Signal()

        self.ready = Signal(dispatch_width)

    def elaborate(self, platform):
        m = Module()

        slots = []
        #
        # Issue slots
        #
        for i in range(self.num_issue_slots):
            slot = IssueSlot(self.num_wakeup_ports, self.params)
            setattr(m.submodules, f'issue_slot{i}', slot)
            slots.append(slot)

            m.d.comb += slot.kill.eq(self.flush_pipeline)

            for swu, wu in zip(slot.wakeup_ports, self.wakeup_ports):
                m.d.comb += swu.eq(wu)

        entry_wen_array = [
            Signal(self.dispatch_width, name=f'__entry_wen{i}')
            for i in range(self.num_issue_slots)
        ]

        allocated = Signal(self.dispatch_width)
        for slot, entry_wen in zip(slots, entry_wen_array):
            next_allocated = Signal(self.dispatch_width)
            can_allocate = ~slot.valid

            for w in range(self.dispatch_width):
                m.d.comb += [
                    entry_wen[w].eq(can_allocate & ~allocated[w]),
                    next_allocated[w].eq(can_allocate | allocated[w]),
                ]

                can_allocate &= allocated[w]

            allocated = next_allocated

        entry_wen = [
            Signal(self.dispatch_width, name=f'entry_wen{i}')
            for i in range(self.num_issue_slots)
        ]
        for wen_oh, tmp in zip(entry_wen, entry_wen_array):
            m.d.comb += wen_oh.eq(tmp & self.dis_valids)

        dis_uops_mux = Array(
            MicroOp(self.params) for _ in range(self.dispatch_width))
        for mux_uop, uop in zip(dis_uops_mux, self.dis_uops):
            m.d.comb += [
                mux_uop.eq(uop),
                mux_uop.issue_uops.eq(1),
            ]

        for slot, wen_oh in zip(slots, entry_wen):
            enc = Encoder(self.dispatch_width)
            m.submodules += enc

            m.d.comb += [
                enc.i.eq(wen_oh),
                slot.in_uop.eq(dis_uops_mux[enc.o]),
                slot.in_valid.eq(~enc.n),
            ]

        m.d.comb += self.ready.eq(allocated)

        req_valids = Signal(self.num_issue_slots)
        for v, slot in zip(req_valids, slots):
            m.d.comb += v.eq(slot.req)

        for iss, iss_valid, fu_types in zip(self.iss_uops, self.iss_valids,
                                            self.fu_types):

            port_issued = Signal()
            next_req_valids = Signal(self.num_issue_slots)

            for slot, v, next_v in zip(slots, req_valids, next_req_valids):
                can_allocate = (slot.out_uop.fu_type & fu_types) != 0
                next_port_issued = Signal()

                with m.If(v & ~port_issued & can_allocate):
                    m.d.comb += [
                        iss.eq(slot.out_uop),
                        iss_valid.eq(1),
                        slot.gnt.eq(1),
                    ]

                m.d.comb += [
                    next_port_issued.eq(port_issued | (v & can_allocate)),
                    next_v.eq(v & (~can_allocate | port_issued)),
                ]
                port_issued = next_port_issued

            req_valids = next_req_valids

        return m
