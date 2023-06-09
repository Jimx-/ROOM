from amaranth import *

from room.types import HasCoreParams, MicroOp

from roomsoc.interconnect.stream import Queue


class Dispatcher(HasCoreParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.dec_valid = Signal()
        self.dec_wid = Signal(range(self.n_warps))
        self.dec_uop = MicroOp(params)
        self.dec_ready = Signal()

        self.dis_valid = Signal()
        self.dis_wid = Signal(range(self.n_warps))
        self.dis_uop = MicroOp(params)
        self.dis_ready = Signal()

        self.sb_wid = Signal(range(self.n_warps))
        self.sb_uop = MicroOp(params)

    def elaborate(self, platform):
        m = Module()

        dec_fire = self.dec_valid & self.dec_ready
        dis_fire = self.dis_valid & self.dis_ready

        requests = Signal(self.n_warps)
        deq_uops = [
            MicroOp(self.params, name=f'deq_uop{i}')
            for i in range(self.n_warps)
        ]

        q_out_buffer = [
            MicroOp(self.params, name=f'q_out_buffer{i}')
            for i in range(self.n_warps)
        ]

        can_passthrough = Signal()

        for i in range(self.n_warps):
            queue = Queue(self.issue_queue_depth,
                          MicroOp,
                          self.params,
                          flow=False)
            setattr(m.submodules, f'queue{i}', queue)

            out_valid = Signal()

            writing = dec_fire & (self.dec_wid == i)
            reading = dis_fire & (self.dis_wid == i)

            going_empty = ~out_valid | (out_valid & reading
                                        & ~queue.count.any())
            with m.If(writing):
                m.d.comb += can_passthrough.eq(going_empty)

            m.d.comb += [
                queue.enq.bits.eq(self.dec_uop),
                queue.enq.valid.eq(writing & ~going_empty),
                queue.deq.ready.eq(reading),
                deq_uops[i].eq(queue.deq.bits),
            ]

            m.d.comb += requests[i].eq(Mux(reading, queue.deq.valid,
                                           out_valid))

            with m.If(self.dec_wid == i):
                m.d.comb += self.dec_ready.eq(queue.enq.ready)

            with m.If(writing & going_empty):
                m.d.sync += [
                    q_out_buffer[i].eq(self.dec_uop),
                    out_valid.eq(1),
                ]
            with m.Elif(reading):
                m.d.sync += [
                    q_out_buffer[i].eq(queue.deq.bits),
                    out_valid.eq(queue.deq.valid),
                ]

        dis_grant_n = Signal.like(self.dis_wid)
        dis_valid_n = Signal()
        m.d.comb += dis_grant_n.eq(self.dis_wid)
        with m.Switch(self.dis_wid):
            for i in range(self.n_warps):
                with m.Case(i):
                    m.d.comb += dis_valid_n.eq(requests[i])

                    for pred in reversed(range(i)):
                        with m.If(requests[pred]):
                            m.d.comb += [
                                dis_grant_n.eq(pred),
                                dis_valid_n.eq(1),
                            ]
                    for succ in reversed(range(i + 1, self.n_warps)):
                        with m.If(requests[succ]):
                            m.d.comb += [
                                dis_grant_n.eq(succ),
                                dis_valid_n.eq(1),
                            ]

        with m.If(dec_fire & can_passthrough):
            # Let incoming uop pass through
            m.d.comb += [
                dis_valid_n.eq(1),
                self.sb_wid.eq(self.dec_wid),
                self.sb_uop.eq(self.dec_uop),
            ]
        with m.Else():
            # Normal RR arbitration
            for i in range(self.n_warps):
                with m.If(dis_grant_n == i):

                    m.d.comb += [
                        self.sb_wid.eq(i),
                        self.sb_uop.eq(q_out_buffer[i]),
                    ]

                    with m.If(dis_fire & (self.dis_wid == i)):
                        m.d.comb += self.sb_uop.eq(deq_uops[i])

        m.d.sync += [
            self.dis_valid.eq(dis_valid_n),
            self.dis_wid.eq(self.sb_wid),
            self.dis_uop.eq(self.sb_uop),
        ]

        return m
