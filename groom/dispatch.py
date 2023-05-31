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

        requests = Signal(self.n_warps)
        deq_uops = [
            MicroOp(self.params, name=f'deq_uop{i}')
            for i in range(self.n_warps)
        ]

        for i in range(self.n_warps):
            queue = Queue(self.issue_queue_depth,
                          MicroOp,
                          self.params,
                          flow=True)
            setattr(m.submodules, f'queue{i}', queue)

            m.d.comb += [
                queue.enq.bits.eq(self.dec_uop),
                queue.enq.valid.eq(self.dec_valid & (self.dec_wid == i)),
                queue.deq.ready.eq(self.dis_valid & self.dis_ready
                                   & (self.dis_wid == i)),
                requests[i].eq(queue.deq.valid),
                deq_uops[i].eq(queue.deq.bits),
            ]

            with m.If(queue.deq.ready & (queue.count == 1) & ~queue.enq.valid):
                m.d.comb += requests[i].eq(0)

            with m.If(self.dec_wid == i):
                m.d.comb += self.dec_ready.eq(queue.enq.ready)

        dis_grant_n = Signal.like(self.dis_wid)
        m.d.comb += dis_grant_n.eq(self.dis_wid)
        with m.Switch(self.dis_wid):
            for i in range(self.n_warps):
                with m.Case(i):
                    for pred in reversed(range(i)):
                        with m.If(requests[pred]):
                            m.d.comb += dis_grant_n.eq(pred)
                    for succ in reversed(range(i + 1, self.n_warps)):
                        with m.If(requests[succ]):
                            m.d.comb += dis_grant_n.eq(succ)

        m.d.sync += self.dis_valid.eq(requests.any())
        for i in range(self.n_warps):
            with m.If(dis_grant_n == i):
                m.d.sync += [
                    self.dis_wid.eq(i),
                    self.dis_uop.eq(deq_uops[i]),
                ]

                m.d.comb += [
                    self.sb_wid.eq(i),
                    self.sb_uop.eq(deq_uops[i]),
                ]

        return m
