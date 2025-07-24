from amaranth import *
from amaranth.lib.coding import Encoder
from enum import IntEnum

from room.consts import *
from room.types import HasCoreParams, MicroOp
from room.branch import BranchUpdate
from room.utils import PopCount

from roomsoc.interconnect.stream import Valid


class IssueQueueWakeup(Record):

    def __init__(self, num_pregs, name=None, src_loc_at=0):
        self.num_pregs = num_pregs

        super().__init__([
            ('pdst', range(num_pregs)),
            ('poisoned', 1),
        ],
                         name=name,
                         src_loc_at=1 + src_loc_at)


class IssueSlot(HasCoreParams, Elaboratable):

    class State(IntEnum):
        INVALID = 0
        VALID_1 = 1
        VALID_2 = 2

    def __init__(self, num_wakeup_ports, params):
        super().__init__(params)

        self.valid = Signal()
        self.will_be_valid = Signal()

        self.in_uop = MicroOp(self.params)
        self.in_valid = Signal()

        self.out_uop = MicroOp(self.params)

        self.move_uop = MicroOp(self.params)

        self.req = Signal()
        self.gnt = Signal()

        self.wakeup_ports = [
            Valid(IssueQueueWakeup, self.num_pregs, name=f'wakeup_port{i}')
            for i in range(num_wakeup_ports)
        ]

        self.br_update = BranchUpdate(params)
        self.kill = Signal()
        self.clear = Signal()

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
        with m.Elif(self.clear):
            m.d.sync += state.eq(IssueSlot.State.INVALID)
        with m.Else():
            m.d.sync += state.eq(next_state)

        slot_uop = MicroOp(self.params, name='slot_uop')
        next_uop = MicroOp(self.params, name='next_uop')
        m.d.comb += next_uop.eq(Mux(self.in_valid == 1, self.in_uop, slot_uop))

        p1 = Signal()
        p2 = Signal()
        p3 = Signal()

        with m.If(self.in_valid):
            m.d.sync += [
                slot_uop.eq(self.in_uop),
                p1.eq(~self.in_uop.prs1_busy),
                p2.eq(~self.in_uop.prs2_busy),
                p3.eq(~self.in_uop.prs3_busy),
            ]

        for wu in self.wakeup_ports:
            with m.If(wu.valid & (wu.bits.pdst == next_uop.prs1)):
                m.d.sync += p1.eq(1)
            with m.If(wu.valid & (wu.bits.pdst == next_uop.prs2)):
                m.d.sync += p2.eq(1)
            with m.If(wu.valid & (wu.bits.pdst == next_uop.prs3)):
                m.d.sync += p3.eq(1)

        m.d.comb += [
            self.move_uop.eq(slot_uop),
            self.move_uop.issue_uops.eq(next_state),
            self.move_uop.prs1_busy.eq(~p1),
            self.move_uop.prs2_busy.eq(~p2),
            self.move_uop.prs3_busy.eq(~p3),
        ]

        with m.If(self.kill):
            m.d.comb += next_state.eq(IssueSlot.State.INVALID)
        with m.Elif(self.gnt & ((state == IssueSlot.State.VALID_1) | (
            (state == IssueSlot.State.VALID_2) & p1 & p2))):
            m.d.comb += next_state.eq(IssueSlot.State.INVALID)
        with m.Elif(self.gnt & (state == IssueSlot.State.VALID_2)):
            m.d.comb += next_state.eq(IssueSlot.State.VALID_1)
            with m.If(p1):
                m.d.sync += [
                    slot_uop.opcode.eq(UOpCode.STD),
                    slot_uop.lrs1_rtype.eq(RegisterType.X),
                ]
                m.d.comb += [
                    self.move_uop.opcode.eq(UOpCode.STD),
                    self.move_uop.lrs1_rtype.eq(RegisterType.X),
                ]
            with m.Else():
                m.d.sync += slot_uop.lrs2_rtype.eq(RegisterType.X)
                m.d.comb += self.move_uop.lrs2_rtype.eq(RegisterType.X)

        with m.If(self.br_update.uop_killed(slot_uop)):
            m.d.comb += next_state.eq(IssueSlot.State.INVALID)

        next_br_mask = self.br_update.get_new_br_mask(slot_uop.br_mask)
        with m.If(~self.in_valid):
            m.d.sync += slot_uop.br_mask.eq(next_br_mask)
        m.d.comb += self.move_uop.br_mask.eq(next_br_mask)

        with m.If(state == IssueSlot.State.VALID_1):
            m.d.comb += self.req.eq(p1 & p2 & p3 & ~self.kill)
        with m.Elif(state == IssueSlot.State.VALID_2):
            m.d.comb += self.req.eq((p1 | p2) & ~self.kill)

        may_vacate = self.gnt & ((state == IssueSlot.State.VALID_1) | (
            (state == IssueSlot.State.VALID_2) & p1 & p2))
        m.d.comb += [
            self.out_uop.eq(slot_uop),
            self.out_uop.issue_uops.eq(next_state),
            self.will_be_valid.eq(self.valid & ~may_vacate),
        ]

        with m.If(state == IssueSlot.State.VALID_2):
            with m.If(p1 & p2):
                pass
            with m.Elif(p1):
                m.d.comb += self.out_uop.lrs2_rtype.eq(RegisterType.X)
            with m.Elif(p2):
                m.d.comb += [
                    self.out_uop.opcode.eq(UOpCode.STD),
                    self.out_uop.lrs1_rtype.eq(RegisterType.X),
                ]

        return m


class IssueUnitBase(HasCoreParams, Elaboratable):

    def __init__(self, issue_width, num_issue_slots, dispatch_width,
                 num_wakeup_ports, iq_type, params):
        super().__init__(params)

        self.issue_width = issue_width
        self.num_issue_slots = num_issue_slots
        self.dispatch_width = dispatch_width
        self.num_wakeup_ports = num_wakeup_ports
        self.iq_type = iq_type

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
            Valid(IssueQueueWakeup, self.num_pregs, name=f'wakeup_port{i}')
            for i in range(self.num_wakeup_ports)
        ]

        self.br_update = BranchUpdate(params)
        self.flush_pipeline = Signal()

        self.ready = Signal(dispatch_width)

    def elaborate(self, platform):
        m = Module()

        self.slots = []
        #
        # Issue slots
        #
        for i in range(self.num_issue_slots):
            slot = IssueSlot(self.num_wakeup_ports, self.params)
            setattr(m.submodules, f'issue_slot{i}', slot)
            self.slots.append(slot)

            m.d.comb += [
                slot.kill.eq(self.flush_pipeline),
                slot.br_update.eq(self.br_update),
            ]

            for swu, wu in zip(slot.wakeup_ports, self.wakeup_ports):
                m.d.comb += swu.eq(wu)

        self.dis_uops_mux = Array(
            MicroOp(self.params) for _ in range(self.dispatch_width))
        for mux_uop, uop in zip(self.dis_uops_mux, self.dis_uops):
            m.d.comb += [
                mux_uop.eq(uop),
                mux_uop.issue_uops.eq(1),
            ]

            if self.iq_type == IssueQueueType.INT or self.iq_type == IssueQueueType.MEM:
                # For integer stores, issue two micro-ops (STA + STD)
                with m.If(((uop.opcode == UOpCode.STA)
                           & (uop.lrs2_rtype == RegisterType.FIX))
                          | (uop.opcode == UOpCode.AMO_AG)):
                    m.d.comb += mux_uop.issue_uops.eq(2)
                with m.Elif((uop.opcode == UOpCode.STA)
                            & (uop.lrs2_rtype != RegisterType.FIX)):
                    m.d.comb += [
                        mux_uop.lrs2_rtype.eq(RegisterType.X),
                        mux_uop.prs2_busy.eq(0),
                    ]

                m.d.comb += mux_uop.prs3_busy.eq(0)

            elif self.iq_type == IssueQueueType.FP:
                # For floating-point stores, address is from the integer unit
                with m.If(uop.opcode == UOpCode.STA):
                    m.d.comb += [
                        mux_uop.lrs1_rtype.eq(RegisterType.X),
                        mux_uop.prs1_busy.eq(0),
                    ]

        return m


class IssueUnitUnordered(IssueUnitBase):

    def __init__(self, issue_width, num_issue_slots, dispatch_width,
                 num_wakeup_ports, iq_type, params):
        super().__init__(issue_width, num_issue_slots, dispatch_width,
                         num_wakeup_ports, iq_type, params)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        entry_wen_array = [
            Signal(self.dispatch_width, name=f'__entry_wen{i}')
            for i in range(self.num_issue_slots)
        ]

        allocated = Signal(self.dispatch_width)
        for slot, entry_wen in zip(self.slots, entry_wen_array):
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
            for w in range(self.dispatch_width):
                m.d.comb += wen_oh[w].eq(tmp[w] & self.dis_valids[w]
                                         & ~self.dis_uops[w].exception
                                         & ~self.dis_uops[w].is_fence
                                         & ~self.dis_uops[w].is_fencei)

        for slot, wen_oh in zip(self.slots, entry_wen):
            enc = Encoder(self.dispatch_width)
            m.submodules += enc

            m.d.comb += [
                enc.i.eq(wen_oh),
                slot.in_uop.eq(self.dis_uops_mux[enc.o]),
                slot.in_valid.eq(~enc.n),
            ]

        m.d.comb += self.ready.eq(allocated)

        req_valids = Signal(self.num_issue_slots)
        for v, slot in zip(req_valids, self.slots):
            m.d.comb += v.eq(slot.req)

        for iss, iss_valid, fu_types in zip(self.iss_uops, self.iss_valids,
                                            self.fu_types):

            port_issued = Signal()
            next_req_valids = Signal(self.num_issue_slots)

            for slot, v, next_v in zip(self.slots, req_valids,
                                       next_req_valids):
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


class IssueUnitOrdered(IssueUnitBase):

    def __init__(self, issue_width, num_issue_slots, dispatch_width,
                 num_wakeup_ports, iq_type, params):
        super().__init__(issue_width, num_issue_slots, dispatch_width,
                         num_wakeup_ports, iq_type, params)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        max_shift = self.dispatch_width
        vacants = Cat(*[~s.valid for s in self.slots], ~self.dis_valids)
        shamts = [
            Signal(max_shift, name=f'shamt{i}')
            for i in range(self.num_issue_slots + self.dispatch_width)
        ]
        m.d.comb += shamts[0].eq(0)
        for w in range(1, len(shamts)):
            next = Signal(max_shift)
            m.d.comb += next.eq(shamts[w - 1])

            with m.If((shamts[w - 1] == 0) & vacants[w - 1]):
                m.d.comb += next.eq(1)
            with m.Elif(~shamts[w - 1][-1] & vacants[w - 1]):
                m.d.comb += next.eq(shamts[w - 1] << 1)

            m.d.comb += shamts[w].eq(next)

        will_be_valid = [s.will_be_valid for s in self.slots
                         ] + [(dv & ~d.exception & ~d.is_fence & ~d.is_fencei)
                              for dv, d in zip(self.dis_valids, self.dis_uops)]

        uops = [s.move_uop for s in self.slots] + list(self.dis_uops_mux)
        for i, slot in enumerate(self.slots):
            m.d.comb += slot.in_uop.eq(uops[i + 1])

            for j in range(1, max_shift + 1):
                with m.If(shamts[i + j][j - 1]):
                    m.d.comb += [
                        slot.in_valid.eq(will_be_valid[i + j]),
                        slot.in_uop.eq(uops[i + j]),
                    ]

            m.d.comb += slot.clear.eq(shamts[i].any())

        will_be_available = Cat(
            (~s.will_be_valid | s.clear) & ~s.in_valid for s in self.slots)
        popcnt_avail = PopCount(self.num_issue_slots)
        m.submodules += popcnt_avail
        m.d.comb += popcnt_avail.inp.eq(will_be_available)
        for w in range(self.dispatch_width):
            m.d.sync += self.ready[w].eq(popcnt_avail.out > w)

        req_valids = Signal(self.num_issue_slots)
        m.d.comb += req_valids.eq(Cat(s.valid for s in self.slots))

        port_issued = Const(0, self.issue_width)
        for slot in self.slots:
            next_port_issued = Signal(self.issue_width)
            uop_issued = Const(0, 1)

            for iss, iss_valid, fu_types, issued, next_issued in zip(
                    self.iss_uops, self.iss_valids, self.fu_types, port_issued,
                    next_port_issued):
                can_allocate = (slot.out_uop.fu_type & fu_types).any()

                with m.If(slot.valid & ~uop_issued & can_allocate & ~issued):
                    m.d.comb += [
                        slot.gnt.eq(slot.req),
                        iss_valid.eq(slot.req),
                        iss.eq(slot.out_uop),
                    ]

                next_uop_issued = Signal()
                m.d.comb += [
                    next_issued.eq((slot.valid & ~uop_issued
                                    & can_allocate) | issued),
                    next_uop_issued.eq((slot.valid & can_allocate & ~issued)
                                       | uop_issued),
                ]
                uop_issued = next_uop_issued

            port_issued = next_port_issued

        return m
