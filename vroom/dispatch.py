from amaranth import *

from vroom.types import HasVectorParams, VMicroOp

from roomsoc.interconnect.stream import Queue


class Dispatcher(HasVectorParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.expd_valid = Signal()
        self.expd_uop = VMicroOp(params)
        self.expd_ready = Signal()

        self.dis_valid = Signal()
        self.dis_uop = VMicroOp(params)
        self.dis_ready = Signal()

        self.sb_uop = VMicroOp(params)

    def elaborate(self, platform):
        m = Module()

        expd_fire = self.expd_valid & self.expd_ready
        dis_fire = self.dis_valid & self.dis_ready

        deq_uop = VMicroOp(self.params)
        q_out_buffer = VMicroOp(self.params)

        can_passthrough = Signal()

        queue = m.submodules.queue = Queue(self.issue_queue_depth,
                                           VMicroOp,
                                           self.params,
                                           flow=False)

        out_valid = Signal()

        do_enq = expd_fire
        do_deq = dis_fire

        going_empty = ~out_valid | (out_valid & do_deq & ~queue.count.any())
        with m.If(do_enq):
            m.d.comb += can_passthrough.eq(going_empty)

        m.d.comb += [
            queue.enq.bits.eq(self.expd_uop),
            queue.enq.valid.eq(do_enq & ~going_empty),
            queue.deq.ready.eq(do_deq),
            deq_uop.eq(queue.deq.bits),
            self.expd_ready.eq(queue.enq.ready),
        ]

        with m.If(do_enq & going_empty):
            m.d.sync += [
                q_out_buffer.eq(self.expd_uop),
                out_valid.eq(1),
            ]
        with m.Elif(do_deq):
            m.d.sync += [
                q_out_buffer.eq(queue.deq.bits),
                out_valid.eq(queue.deq.valid),
            ]

        dis_valid_n = Signal()
        with m.If(expd_fire & can_passthrough):
            m.d.comb += [
                dis_valid_n.eq(1),
                self.sb_uop.eq(self.expd_uop),
            ]
        with m.Else():
            m.d.comb += [
                dis_valid_n.eq(Mux(do_deq, queue.deq.valid, out_valid)),
                self.sb_uop.eq(q_out_buffer),
            ]

            with m.If(dis_fire):
                m.d.comb += self.sb_uop.eq(deq_uop),

        m.d.sync += [
            self.dis_valid.eq(dis_valid_n),
            self.dis_uop.eq(self.sb_uop),
        ]

        return m
