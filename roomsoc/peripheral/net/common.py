from amaranth import *
from amaranth.hdl.rec import Direction


def make_data_layout(data_width, user_width=1):
    return [
        ("data", data_width, Direction.FANOUT),
        ("keep", data_width // 8, Direction.FANOUT),
        ("user", user_width, Direction.FANOUT),
        ("last", 1, Direction.FANOUT),
    ]


def stream2queue(s, q):
    return [
        q.enq.bits.data.eq(s.bits.data),
        q.enq.bits.keep.eq(s.bits.keep),
        q.enq.bits.user.eq(s.bits.user),
        q.enq.bits.last.eq(s.bits.last),
        q.enq.valid.eq(s.valid),
        s.ready.eq(q.enq.ready),
    ]


def queue2stream(q, s):
    return [
        s.bits.data.eq(q.deq.bits.data),
        s.bits.keep.eq(q.deq.bits.keep),
        s.bits.user.eq(q.deq.bits.user),
        s.bits.last.eq(q.deq.bits.last),
        s.valid.eq(q.deq.valid),
        q.deq.ready.eq(s.ready),
    ]
