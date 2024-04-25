from amaranth import *
from amaranth import tracer
from amaranth.hdl.rec import *

from room.icache import ICache
from room.utils import Pipe

from roomsoc.interconnect import tilelink as tl
from roomsoc.interconnect.stream import Decoupled, Valid, Queue


def vec2_layout(w, dir=DIR_FANOUT):
    return [
        ('x', w, dir),
        ('y', w, dir),
    ]


def vec3_layout(w, dir=DIR_FANOUT):
    return [
        ('x', w, dir),
        ('y', w, dir),
        ('z', w, dir),
    ]


edge_layout = [
    ('a', 32, DIR_FANOUT),
    ('b', 32, DIR_FANOUT),
    ('c', 32, DIR_FANOUT),
]

tile_buffer_layout = [
    ('x', 15, DIR_FANOUT),
    ('y', 15, DIR_FANOUT),
    ('level', 4, DIR_FANOUT),
    ('count', 16, DIR_FANOUT),
]

prim_layout = [
    ('pid', 16, DIR_FANOUT),
    ('edge1', edge_layout),
    ('edge2', edge_layout),
    ('edge3', edge_layout),
]

tile_layout = [
    ('x', 15, DIR_FANOUT),
    ('y', 15, DIR_FANOUT),
    ('level', 4, DIR_FANOUT),
]


def fragment_layout(dir=DIR_FANOUT):
    return [
        ('pid', 16, dir),
        ('x', 15, dir),
        ('y', 15, dir),
        ('barycentric', vec3_layout(32, dir)),
        ('mask', 4, dir),
    ]


class Viewport(Record):

    def __init__(self, name=None, src_loc_at=0):
        super().__init__([
            ('x0', 16),
            ('y0', 16),
            ('width', 16),
            ('height', 16),
        ],
                         name=name,
                         src_loc_at=1 + src_loc_at)

    def inside(self, x, y):
        return (x >= self.x0) & (x <= (self.x0 + self.width)) & (
            y >= self.y0) & (y <= (self.y0 + self.height))


class Fragment(Record):

    def __init__(self, name=None, src_loc_at=0):
        super().__init__(fragment_layout(),
                         name=name,
                         src_loc_at=1 + src_loc_at)


class HasRasterParams:

    def __init__(self, params, *args, **kwargs):
        self.params = params

        self.n_threads = params['n_threads']

        self.scan_level = params.get('scan_level', 5)
        self.block_level = params.get('block_level', 2)

        self.quad_queue_depth = params.get('quad_queue_depth', 16)


class RasterRequest(HasRasterParams):

    def __init__(self, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.tmask = Signal(self.n_threads, name=f'{name}__tmask')

        self.resp = [
            Valid(Fragment, name=f'{name}__resp{t}')
            for t in range(self.n_threads)
        ]

    def connect(self, subord):
        return [subord.tmask.eq(self.tmask)
                ] + [(r.eq(rr) for r, rr in zip(self.resp, subord.resp))]


class FetchUnit(HasRasterParams, Elaboratable):

    class Request(Record):

        def __init__(self, name=None, src_loc_at=0):
            super().__init__([
                ('tile', tile_buffer_layout),
                ('prim', prim_layout),
            ],
                             name=name,
                             src_loc_at=1 + src_loc_at)

    def __init__(self, mem_bus, params):
        super().__init__(params)

        self.mem_bus = mem_bus

        self.tile_count = Signal(16)
        self.tilebuf_addr = Signal(32)
        self.prim_addr = Signal(32)
        self.prim_stride = Signal(16)

        self.req = Decoupled(FetchUnit.Request)

        self.done = Signal()

    def elaborate(self, platform):
        m = Module()

        cache = m.submodules.cache = ICache(self.mem_bus, self.params)

        tile_addr = Signal(32)

        beat = Signal(4)

        tile_index = Signal(16)
        cur_tile = Record(tile_buffer_layout)

        prim_index = Signal(16)
        cur_pid = Signal(16)
        cur_prim_addr = Signal(32)
        cur_prim = Record(prim_layout)
        m.d.comb += cur_prim.pid.eq(cur_pid)

        with m.FSM():
            with m.State('IDLE'):
                m.d.sync += [
                    tile_addr.eq(self.tilebuf_addr),
                    tile_index.eq(0),
                ]
                m.next = 'NEXT_TILE'

            with m.State('NEXT_TILE'):
                with m.If(tile_index == self.tile_count):
                    m.next = 'DONE'
                with m.Else():
                    m.next = 'READ_TILE'

            with m.State('READ_TILE'):
                m.d.comb += [
                    cache.req.valid.eq(1),
                    cache.req.bits.addr.eq(tile_addr),
                ]

                with m.If(cache.req.ready):
                    m.next = 'READ_TILE_1'

            with m.State('READ_TILE_1'):
                m.d.comb += cache.s1_paddr.eq(tile_addr)
                m.next = 'READ_TILE_2'

            with m.State('READ_TILE_2'):
                with m.If(cache.resp.valid):
                    m.d.sync += [
                        tile_addr.eq(tile_addr + 4),
                        beat.eq(beat + 1),
                    ]

                    with m.Switch(beat):
                        with m.Case(0):
                            m.d.sync += [
                                cur_tile.x.eq(cache.resp.bits.data[:15]),
                                cur_tile.y.eq(cache.resp.bits.data[16:31]),
                            ]

                        with m.Case(1):
                            m.d.sync += [
                                cur_tile.count.eq(cache.resp.bits.data[:16]),
                                cur_tile.level.eq(cache.resp.bits.data[16:20]),
                            ]

                    with m.If(beat == 1):
                        m.d.sync += [
                            tile_index.eq(tile_index + 1),
                            prim_index.eq(0),
                            beat.eq(0),
                        ]
                        m.next = 'NEXT_PRIM'
                    with m.Else():
                        m.next = 'READ_TILE'

                with m.Else():
                    m.next = 'READ_TILE'

            with m.State('NEXT_PRIM'):
                with m.If(prim_index == cur_tile.count):
                    m.next = 'NEXT_TILE'
                with m.Else():
                    m.next = 'READ_PID'

            with m.State('READ_PID'):
                m.d.comb += [
                    cache.req.valid.eq(1),
                    cache.req.bits.addr.eq(tile_addr),
                ]

                with m.If(cache.req.ready):
                    m.next = 'READ_PID_1'

            with m.State('READ_PID_1'):
                m.d.comb += [
                    cache.s1_paddr.eq(tile_addr),
                ]
                m.next = 'READ_PID_2'

            with m.State('READ_PID_2'):
                with m.If(cache.resp.valid):
                    m.d.sync += [
                        cur_pid.eq(cache.resp.bits.data),
                        tile_addr.eq(tile_addr + 4),
                    ]

                    m.next = 'CALC_PRIM_ADDR'

                with m.Else():
                    m.next = 'READ_PID'

            with m.State('CALC_PRIM_ADDR'):
                m.d.sync += cur_prim_addr.eq(self.prim_addr +
                                             (cur_pid * self.prim_stride))
                m.next = 'READ_PRIM'

            with m.State('READ_PRIM'):
                m.d.comb += [
                    cache.req.valid.eq(1),
                    cache.req.bits.addr.eq(cur_prim_addr + (beat << 2)),
                ]

                with m.If(cache.req.ready):
                    m.next = 'READ_PRIM_1'

            with m.State('READ_PRIM_1'):
                m.d.comb += [
                    cache.s1_paddr.eq(cur_prim_addr + (beat << 2)),
                ]
                m.next = 'READ_PRIM_2'

            with m.State('READ_PRIM_2'):
                with m.If(cache.resp.valid):
                    m.d.sync += beat.eq(beat + 1)

                    with m.Switch(beat):
                        for i in range(9):
                            with m.Case(i):
                                m.d.sync += getattr(
                                    getattr(cur_prim, f"edge{i//3+1}"),
                                    f"{chr(ord('a') +i%3)}").eq(
                                        cache.resp.bits.data)

                    with m.If(beat == 8):
                        m.d.sync += beat.eq(0)
                        m.next = 'REQUEST'
                    with m.Else():
                        m.next = 'READ_PRIM'

                with m.Else():
                    m.next = 'READ_PRIM'

            with m.State('REQUEST'):
                m.d.comb += [
                    self.req.valid.eq(1),
                    self.req.bits.tile.eq(cur_tile),
                    self.req.bits.prim.eq(cur_prim),
                ]

                with m.If(self.req.ready):
                    m.d.sync += prim_index.eq(prim_index + 1)
                    m.next = 'NEXT_PRIM'

            with m.State('DONE'):
                m.d.comb += self.done.eq(1)

        return m


class EdgeEvaluator(Elaboratable):

    class MAC(Elaboratable):

        def __init__(self):
            self.a = Signal(32)
            self.b = Signal(15)
            self.c = Signal(33)
            self.enable = Signal()

            self.out = Signal(33)

        def elaborate(self, platform):
            m = Module()

            with m.If(self.enable):
                m.d.sync += self.out.eq(self.a.as_signed() * self.b +
                                        self.c.as_signed())

            return m

    def __init__(self):
        self.valid = Signal()
        self.edge = Record(edge_layout)
        self.p = Record(vec2_layout(15))

        self.out = Decoupled(Signal, 32)

    def elaborate(self, platform):
        m = Module()

        s1_ready = Signal()
        s2_ready = Signal()
        s3_ready = Signal()

        s0_valid = Signal()
        s0_edge = Record(edge_layout)
        s0_p = Record(vec2_layout(15))
        s0_mac = m.submodules.s0_mac = EdgeEvaluator.MAC()
        m.d.comb += [
            s0_valid.eq(self.valid),
            s0_edge.eq(self.edge),
            s0_p.eq(self.p),
            s0_mac.a.eq(self.edge.a),
            s0_mac.b.eq(self.p.x),
        ]

        s1_valid = Signal()
        s1_edge = Record(edge_layout)
        s1_p = Record(vec2_layout(15))
        s1_mac = m.submodules.s1_mac = EdgeEvaluator.MAC()
        m.d.comb += [
            s1_ready.eq(~s1_valid | s2_ready),
            s1_mac.a.eq(s1_edge.b),
            s1_mac.b.eq(s1_p.y),
            s1_mac.c.eq(s0_mac.out),
            s0_mac.enable.eq(s1_ready),
        ]
        with m.If(s1_ready):
            m.d.sync += s1_valid.eq(s0_valid)
        with m.If(s0_valid & s1_ready):
            m.d.sync += [
                s1_edge.eq(s0_edge),
                s1_p.eq(s0_p),
            ]

        s2_valid = Signal()
        s2_edge = Record(edge_layout)
        s2_out = Signal(32)
        m.d.comb += [
            s2_ready.eq(~s2_valid | s3_ready),
            s2_out.eq(s1_mac.out.as_signed() + s2_edge.c),
            s1_mac.enable.eq(s2_ready),
        ]
        with m.If(s2_ready):
            m.d.sync += s2_valid.eq(s1_valid)
        with m.If(s1_valid & s2_ready):
            m.d.sync += s2_edge.eq(s1_edge)

        s3_valid = Signal()
        s3_out = Signal(32)
        m.d.comb += s3_ready.eq(~s3_valid | self.out.ready)
        with m.If(s3_ready):
            m.d.sync += s3_valid.eq(s2_valid)
        with m.If(s2_valid & s3_ready):
            m.d.sync += s3_out.eq(s2_out)

        m.d.comb += [
            self.out.valid.eq(s3_valid),
            self.out.bits.eq(s3_out),
        ]

        return m


class SampleEvaluator(Elaboratable):

    def __init__(self):
        self.valid = Signal()
        self.edge1 = Record(edge_layout)
        self.edge2 = Record(edge_layout)
        self.edge3 = Record(edge_layout)
        self.p = Record(vec2_layout(15))

        self.out = Decoupled(Record, vec3_layout(32))

    def elaborate(self, platform):
        m = Module()

        edge1_eval = m.submodules.edge1_eval = EdgeEvaluator()
        m.d.comb += [
            edge1_eval.valid.eq(self.valid),
            edge1_eval.edge.eq(self.edge1),
            edge1_eval.p.eq(self.p),
            edge1_eval.out.ready.eq(self.out.ready),
        ]

        edge2_eval = m.submodules.edge2_eval = EdgeEvaluator()
        m.d.comb += [
            edge2_eval.valid.eq(self.valid),
            edge2_eval.edge.eq(self.edge2),
            edge2_eval.p.eq(self.p),
            edge2_eval.out.ready.eq(self.out.ready),
        ]

        edge3_eval = m.submodules.edge3_eval = EdgeEvaluator()
        m.d.comb += [
            edge3_eval.valid.eq(self.valid),
            edge3_eval.edge.eq(self.edge3),
            edge3_eval.p.eq(self.p),
            edge3_eval.out.ready.eq(self.out.ready),
        ]

        m.d.comb += [
            self.out.valid.eq(edge1_eval.out.valid & edge2_eval.out.valid
                              & edge3_eval.out.valid),
            self.out.bits.x.eq(edge1_eval.out.bits),
            self.out.bits.y.eq(edge2_eval.out.bits),
            self.out.bits.z.eq(edge3_eval.out.bits),
        ]

        return m


class TileEvaluator(Elaboratable):

    def __init__(self):
        self.s0 = Record(vec3_layout(32))
        self.s1 = Record(vec3_layout(32))
        self.s2 = Record(vec3_layout(32))
        self.s3 = Record(vec3_layout(32))

        self.out = Signal()

    def elaborate(self, platform):
        m = Module()

        outcode = [Signal(3, name=f'outcode{w}') for w in range(4)]
        for i, c in enumerate(('x', 'y', 'z')):
            m.d.comb += [
                outcode[0][i].eq(getattr(self.s0, c)[-1]),
                outcode[1][i].eq(getattr(self.s1, c)[-1]),
                outcode[2][i].eq(getattr(self.s2, c)[-1]),
                outcode[3][i].eq(getattr(self.s3, c)[-1]),
            ]

        m.d.comb += self.out.eq(~(outcode[0] & outcode[1] & outcode[2]
                                  & outcode[3]).any())

        return m


class QuadEvaluator(HasRasterParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.viewport = Viewport()
        self.prim = Record(prim_layout)

        self.req = Decoupled(Record, tile_layout)

        self.quads = [Valid(Fragment, name=f'quads{i}') for i in range(4)]
        self.quad_ready = Signal()

        self.busy = Signal()

    def elaborate(self, platform):
        m = Module()

        s1_ready = Signal()
        s2_ready = Signal()
        s3_ready = Signal()
        s4_ready = Signal()

        s0_valid = Signal()
        s0_tile = Record(tile_layout)
        s0_prim = Record(prim_layout)
        m.d.comb += [
            s0_valid.eq(self.req.valid),
            s0_tile.eq(self.req.bits),
            s0_prim.eq(self.prim),
            self.req.ready.eq(s1_ready),
        ]

        # Generate 9 subsamples for the current quad
        s0_samples = [
            Record(vec2_layout(15), name=f's0_sample{i}') for i in range(9)
        ]
        for i, s in enumerate(s0_samples):
            m.d.comb += [
                s.x.eq(s0_tile.x + (i % 3) * 2),
                s.y.eq(s0_tile.y + (i // 3) * 2),
            ]

        sample_evals = [SampleEvaluator() for _ in range(9)]
        for i, (sample, ev) in enumerate(zip(s0_samples, sample_evals)):
            setattr(m.submodules, f'sample_eval{i}', ev)
            m.d.comb += [
                ev.valid.eq(s0_valid),
                ev.edge1.eq(s0_prim.edge1),
                ev.edge2.eq(s0_prim.edge2),
                ev.edge3.eq(s0_prim.edge3),
                ev.p.eq(sample),
                ev.out.ready.eq(s4_ready),
            ]

        s1_valid = Signal()
        s1_tile = Record(tile_layout)
        s1_prim = Record(prim_layout)
        s1_samples = [
            Record(vec2_layout(15), name=f's1_sample{i}') for i in range(9)
        ]
        m.d.comb += s1_ready.eq(~s1_valid | s2_ready)
        with m.If(s1_ready):
            m.d.sync += s1_valid.eq(s0_valid)
        with m.If(s0_valid & s1_ready):
            m.d.sync += [
                Cat(s1_tile, *s1_samples).eq(Cat(s0_tile, *s0_samples)),
                s1_prim.eq(s0_prim),
            ]

        s2_valid = Signal()
        s2_tile = Record(tile_layout)
        s2_prim = Record(prim_layout)
        s2_samples = [
            Record(vec2_layout(15), name=f's2_sample{i}') for i in range(9)
        ]
        m.d.comb += s2_ready.eq(~s2_valid | s3_ready)
        with m.If(s2_ready):
            m.d.sync += s2_valid.eq(s1_valid)
        with m.If(s1_valid & s2_ready):
            m.d.sync += [
                Cat(s2_tile, *s2_samples).eq(Cat(s1_tile, *s1_samples)),
                s2_prim.eq(s1_prim),
            ]

        s3_valid = Signal()
        s3_tile = Record(tile_layout)
        s3_prim = Record(prim_layout)
        s3_samples = [
            Record(vec2_layout(15), name=f's3_sample{i}') for i in range(9)
        ]
        s3_quads = [Valid(Fragment, name=f's3_quads{i}') for i in range(4)]
        m.d.comb += s3_ready.eq(~s3_valid | s4_ready)
        with m.If(s3_ready):
            m.d.sync += s3_valid.eq(s2_valid)
        with m.If(s2_valid & s3_ready):
            m.d.sync += [
                Cat(s3_tile, *s3_samples).eq(Cat(s2_tile, *s2_samples)),
                s3_prim.eq(s2_prim),
            ]

        corners = [
            (0, 1, 4, 3),
            (1, 2, 5, 4),
            (3, 4, 7, 6),
            (4, 5, 8, 7),
        ]
        starts = (0, 1, 3, 4)
        for i, (quad, corner,
                start) in enumerate(zip(s3_quads, corners, starts)):
            m.d.comb += [
                quad.bits.x.eq(s3_tile.x + 2 * (i & 1)),
                quad.bits.y.eq(s3_tile.y + 2 * (i >> 1)),
                quad.bits.barycentric.eq(sample_evals[start].out.bits),
                quad.bits.pid.eq(s3_prim.pid),
            ]

            tile_eval = TileEvaluator()
            setattr(m.submodules, f'tile_eval{i}', tile_eval)

            m.d.comb += [
                tile_eval.s0.eq(sample_evals[corner[0]].out.bits),
                tile_eval.s1.eq(sample_evals[corner[1]].out.bits),
                tile_eval.s2.eq(sample_evals[corner[2]].out.bits),
                tile_eval.s3.eq(sample_evals[corner[3]].out.bits),
                quad.valid.eq(
                    s3_valid & tile_eval.out
                    & self.viewport.inside(quad.bits.x, quad.bits.y)),
            ]

        s4_valid = Signal()
        s4_tile = Record(tile_layout)
        s4_prim = Record(prim_layout)
        s4_samples = [
            Record(vec2_layout(15), name=f's4_sample{i}') for i in range(9)
        ]
        s4_quads = [Valid(Fragment, name=f's4_quads{i}') for i in range(4)]
        s4_mask = [Signal(4, name=f's4_mask{i}') for i in range(4)]
        m.d.comb += s4_ready.eq(~s4_valid | self.quad_ready)
        with m.If(s4_ready):
            m.d.sync += s4_valid.eq(s3_valid)
        with m.If(s3_valid & s4_ready):
            m.d.sync += [
                Cat(s4_tile, *s4_samples).eq(Cat(s3_tile, *s3_samples)),
                s4_prim.eq(s3_prim),
            ]
            for s4_q, s3_q in zip(s4_quads, s3_quads):
                m.d.sync += s4_q.eq(s3_q)

        for i, quad in enumerate(s4_quads):

            for j in range(4):
                dx = j % 2
                dy = j // 2

                s = [Signal(signed(32)) for _ in range(3)]
                m.d.comb += [
                    s[0].eq(quad.bits.barycentric.x.as_signed() +
                            (s4_prim.edge1.a.as_signed() if dx == 1 else 0) +
                            (s4_prim.edge1.b.as_signed() if dy == 1 else 0)),
                    s[1].eq(quad.bits.barycentric.y.as_signed() +
                            (s4_prim.edge2.a.as_signed() if dx == 1 else 0) +
                            (s4_prim.edge2.b.as_signed() if dy == 1 else 0)),
                    s[2].eq(quad.bits.barycentric.z.as_signed() +
                            (s4_prim.edge3.a.as_signed() if dx == 1 else 0) +
                            (s4_prim.edge3.b.as_signed() if dy == 1 else 0)),
                    s4_mask[i][j].eq(~Cat(x < 0 for x in s).any()),
                ]

        for q, qq, mask in zip(self.quads, s4_quads, s4_mask):
            m.d.comb += [
                q.eq(qq),
                q.valid.eq(qq.valid & s4_valid & (mask != 0)),
                q.bits.mask.eq(mask),
            ]

        m.d.comb += self.busy.eq(s1_valid | s2_valid | s3_valid | s4_valid)

        return m


class QuadQueue(HasRasterParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.depth = self.quad_queue_depth

        self.w_quads = [
            Valid(Fragment, name=f'w_quads{i}')
            for i in range(4 * (self.block_level - 1)**2)
        ]
        self.w_en = Signal()
        self.w_rdy = Signal()

        self.r_quads = [
            Valid(Fragment, name=f'r_quads{i}') for i in range(self.n_threads)
        ]
        self.r_en = Signal()
        self.r_rdy = Signal()

        self.flush = Signal()

    def elaborate(self, platform):
        m = Module()

        num_entries = self.depth
        num_rows = self.depth // self.n_threads

        mem = Array(Fragment(name=f'mem{i}') for i in range(self.depth))

        head = Signal(num_rows, reset=1)
        tail = Signal(num_entries, reset=1)

        maybe_full = Signal()
        do_enq = Signal()
        do_deq = Signal()

        in_mask = Signal(len(self.w_quads))
        m.d.comb += in_mask.eq(Cat(q.valid & self.w_en for q in self.w_quads))

        wr_slots = [
            Signal(num_entries, name=f'wr_slot{i}')
            for i in range(len(self.w_quads))
        ]
        next_slot = tail
        for wr_slot, w_quad, mask in zip(wr_slots, self.w_quads, in_mask):
            m.d.comb += wr_slot.eq(next_slot)
            next_slot = Mux(
                mask,
                Cat(next_slot[num_entries - 1], next_slot[:num_entries - 1]),
                next_slot)

        for w, w_quad in enumerate(self.w_quads):
            for s in range(num_entries):
                with m.If(do_enq & in_mask[w] & wr_slots[w][s]):
                    m.d.sync += mem[s].eq(w_quad.bits)

        wr_mask = Const(0, num_entries)
        for w in range(1, len(self.w_quads)):
            wr_mask |= Cat(tail[num_entries - w:num_entries],
                           tail[:num_entries - w])

        might_hit_head = 0
        at_head = 0
        for r in range(num_rows):
            might_hit_head |= head[r] & wr_mask[r * self.n_threads]
            at_head |= head[r] & tail[r * self.n_threads]

        m.d.comb += [
            do_enq.eq(~(at_head & maybe_full | might_hit_head)),
            self.w_rdy.eq(do_enq),
        ]

        tail_collisions = Array(
            Signal(self.n_threads, name=f'tc{i}') for i in range(num_rows))
        for i in range(num_entries):
            m.d.comb += tail_collisions[i // self.n_threads][
                i % self.n_threads].eq(head[i // self.n_threads]
                                       & (~maybe_full
                                          | (i % self.n_threads != 0))
                                       & tail[i])
        slot_hit_tail = Signal(self.n_threads)
        for i in range(num_rows):
            with m.If(head[i]):
                m.d.comb += slot_hit_tail.eq(tail_collisions[i])
        will_hit_tail = slot_hit_tail != 0

        m.d.comb += do_deq.eq(self.r_en & ~will_hit_tail)

        hit_mask = slot_hit_tail
        for i in range(self.n_threads):
            hit_mask |= (hit_mask << 1)

        deq_valids = ~hit_mask[0:self.n_threads]

        for i in range(num_rows):
            with m.If(head[i]):
                for w in range(self.n_threads):
                    m.d.comb += [
                        self.r_quads[w].bits.eq(mem[i * self.n_threads + w]),
                        self.r_quads[w].valid.eq(deq_valids[w]),
                    ]
        m.d.comb += self.r_rdy.eq(deq_valids != 0)

        with m.If(do_enq):
            m.d.sync += tail.eq(next_slot)
            with m.If(in_mask.any()):
                m.d.sync += maybe_full.eq(1)

        with m.If(do_deq):
            m.d.sync += [
                head.eq(Cat(head[num_rows - 1], head[:num_rows - 1])),
                maybe_full.eq(0),
            ]

        with m.If(self.flush):
            m.d.sync += [
                head.eq(1),
                tail.eq(1),
                maybe_full.eq(0),
            ]

        return m


class BlockEvaluator(HasRasterParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.viewport = Viewport()
        self.prim = Record(prim_layout)

        self.req = Decoupled(Record, tile_layout)

        self.r_quads = [
            Valid(Fragment, name=f'r_quads{i}') for i in range(self.n_threads)
        ]
        self.r_en = Signal()
        self.r_rdy = Signal()

        self.busy = Signal()

    def elaborate(self, platform):
        m = Module()

        enq_quads = [
            Valid(Fragment, name=f'enq_quads{i}')
            for i in range(4 * (self.block_level - 1)**2)
        ]
        enq_ready = Signal()

        req_ready = 1
        busy = 0
        for i in range(self.block_level - 1):
            for j in range(self.block_level - 1):
                quad_eval = QuadEvaluator(self.params)
                setattr(m.submodules, f'quad_eval_{i}_{j}', quad_eval)

                m.d.comb += [
                    quad_eval.viewport.eq(self.viewport),
                    quad_eval.prim.eq(self.prim),
                    quad_eval.req.bits.x.eq(self.req.bits.x + 2 * i),
                    quad_eval.req.bits.y.eq(self.req.bits.y + 2 * j),
                    quad_eval.req.bits.level.eq(1),
                    quad_eval.req.valid.eq(self.req.valid),
                    quad_eval.quad_ready.eq(enq_ready),
                ]

                req_ready &= quad_eval.req.ready
                busy |= quad_eval.busy

                start = (i * (self.block_level - 1) + j) * 4
                for i in range(4):
                    m.d.comb += [
                        enq_quads[start + i].eq(quad_eval.quads[i]),
                    ]

        m.d.comb += [
            self.req.ready.eq(req_ready),
            self.busy.eq(busy),
        ]

        quad_queue = m.submodules.quad_queue = QuadQueue(self.params)
        for q, qq in zip(enq_quads, quad_queue.w_quads):
            m.d.comb += qq.eq(q)
        m.d.comb += [
            quad_queue.w_en.eq(Cat(q.valid for q in enq_quads).any()),
            enq_ready.eq(quad_queue.w_rdy),
        ]

        for q, qq in zip(self.r_quads, quad_queue.r_quads):
            m.d.comb += q.eq(qq)
        m.d.comb += [
            self.r_rdy.eq(quad_queue.r_rdy),
            quad_queue.r_en.eq(self.r_en),
        ]

        return m


class RasterSlice(HasRasterParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.viewport = Viewport()

        self.req = Decoupled(FetchUnit.Request)

        self.r_quads = [
            Valid(Fragment, name=f'r_quads{i}') for i in range(self.n_threads)
        ]
        self.r_en = Signal()
        self.r_rdy = Signal()

        self.done = Signal()

    def elaborate(self, platform):
        m = Module()

        busy = Signal()
        m.d.comb += self.req.ready.eq(~busy)

        req_prim = Record(prim_layout)
        with m.If(self.req.fire):
            m.d.sync += req_prim.eq(self.req.bits.prim)

        tile_queues = [
            Queue(2**((self.scan_level - self.block_level - 1) * 2 - 2),
                  Record,
                  tile_layout,
                  flow=False,
                  name=f'tile_queue{i}') for i in range(4)
        ]
        queue_request = Signal(4)
        queue_grant = Signal(range(4))
        for i in reversed(range(4)):
            setattr(m.submodules, f'tile_queue{i}', tile_queues[i])
            m.d.comb += queue_request[i].eq(tile_queues[i].deq.valid)

            with m.If(queue_request[i]):
                m.d.comb += queue_grant.eq(i)

        s0_valid = self.req.fire | queue_request.any()
        s0_tile = Record(tile_layout)

        with m.Switch(queue_grant):
            for i in range(4):
                with m.Case(i):
                    m.d.comb += [
                        s0_tile.eq(tile_queues[i].deq.bits),
                        tile_queues[i].deq.ready.eq(~self.req.fire),
                    ]
        with m.If(self.req.fire):
            m.d.comb += s0_tile.eq(self.req.bits.tile)

        s1_valid = Signal()
        s1_tile = Record(tile_layout)
        m.d.sync += [
            s1_valid.eq(s0_valid),
            s1_tile.eq(s0_tile),
        ]

        # Generate 9 subsamples for the current tile
        s1_samples = [
            Record(vec2_layout(15), name=f's1_sample{i}') for i in range(9)
        ]
        for i, s in enumerate(s1_samples):
            m.d.comb += [
                s.x.eq(s1_tile.x +
                       ((i % 3) << (s1_tile.level - 1).as_unsigned())),
                s.y.eq(s1_tile.y +
                       ((i // 3) << (s1_tile.level - 1).as_unsigned())),
            ]

        sample_evals = [SampleEvaluator() for _ in range(9)]
        for i, (sample, ev) in enumerate(zip(s1_samples, sample_evals)):
            setattr(m.submodules, f'sample_eval{i}', ev)
            m.d.comb += [
                ev.valid.eq(s1_valid),
                ev.edge1.eq(req_prim.edge1),
                ev.edge2.eq(req_prim.edge2),
                ev.edge3.eq(req_prim.edge3),
                ev.p.eq(sample),
                ev.out.ready.eq(1),
            ]

        s2_valid = Signal()
        m.d.sync += s2_valid.eq(s1_valid)
        s3_valid = Signal()
        m.d.sync += s3_valid.eq(s2_valid)

        s4_pipe_in = Cat(s1_tile, *s1_samples)
        s4_pipe = m.submodules.s4_pipe = Pipe(width=len(s4_pipe_in), depth=3)
        m.d.comb += [
            s4_pipe.in_valid.eq(s1_valid),
            s4_pipe.in_data.eq(s4_pipe_in),
        ]

        s4_valid = Signal()
        s4_tile = Record(tile_layout)
        s4_samples = [
            Record(vec2_layout(15), name=f's4_sample{i}') for i in range(9)
        ]
        m.d.comb += [
            s4_valid.eq(s4_pipe.out.valid),
            Cat(s4_tile, *s4_samples).eq(s4_pipe.out.bits),
        ]

        s4_subtiles = [
            Record(tile_layout, name=f's4_subtile{i}') for i in range(4)
        ]
        s4_subtile_valids = Signal(4)
        corners = [
            (0, 1, 4, 3),
            (1, 2, 5, 4),
            (3, 4, 7, 6),
            (4, 5, 8, 7),
        ]
        for i, (subtile, corner) in enumerate(zip(s4_subtiles, corners)):
            m.d.comb += [
                subtile.level.eq(s4_tile.level - 1),
                subtile.x.eq(s4_tile.x + ((i & 1) << subtile.level)),
                subtile.y.eq(s4_tile.y + ((i >> 1) << subtile.level)),
            ]

            tile_eval = TileEvaluator()
            setattr(m.submodules, f'tile_eval{i}', tile_eval)

            m.d.comb += [
                tile_eval.s0.eq(sample_evals[corner[0]].out.bits),
                tile_eval.s1.eq(sample_evals[corner[1]].out.bits),
                tile_eval.s2.eq(sample_evals[corner[2]].out.bits),
                tile_eval.s3.eq(sample_evals[corner[3]].out.bits),
                s4_subtile_valids[i].eq(
                    tile_eval.out
                    & self.viewport.inside(subtile.x, subtile.y)),
            ]

        block_queues = [
            Queue(2**((self.scan_level - self.block_level) * 2 - 2),
                  Record,
                  tile_layout,
                  flow=False,
                  name=f'tile_queue{i}') for i in range(4)
        ]
        block_request = Signal(4)
        block_grant = Signal(range(4))
        for i in reversed(range(4)):
            setattr(m.submodules, f'block_queue{i}', block_queues[i])
            m.d.comb += block_request[i].eq(block_queues[i].deq.valid)

            with m.If(block_request[i]):
                m.d.comb += block_grant.eq(i)

        for subtile, subtile_valid, tile_queue, block_queue in zip(
                s4_subtiles, s4_subtile_valids, tile_queues, block_queues):
            m.d.comb += [
                tile_queue.enq.valid.eq(s4_valid & subtile_valid
                                        & (subtile.level != self.block_level)),
                tile_queue.enq.bits.eq(subtile),
                block_queue.enq.valid.eq(s4_valid & subtile_valid
                                         &
                                         (subtile.level == self.block_level)),
                block_queue.enq.bits.eq(subtile),
            ]

        block_eval = m.submodules.block_eval = BlockEvaluator(self.params)
        m.d.comb += [
            block_eval.viewport.eq(self.viewport),
            block_eval.prim.eq(req_prim),
        ]
        with m.Switch(block_grant):
            for i in range(4):
                with m.Case(i):
                    m.d.comb += block_queues[i].deq.connect(block_eval.req)

        pipeline_busy = s1_valid | s2_valid | s3_valid | s4_valid
        m.d.comb += [
            busy.eq(pipeline_busy | queue_request.any()
                    | block_request.any()),
            self.done.eq(~(busy | block_eval.busy)),
        ]

        r_quads_mask = Signal(self.n_threads)
        with m.If(self.r_en & self.r_rdy & self.r_quads[-1].valid):
            m.d.sync += r_quads_mask.eq(0)
        with m.Else():
            m.d.sync += r_quads_mask.eq(r_quads_mask
                                        | Cat(q.valid & self.r_en & self.r_rdy
                                              for q in self.r_quads))

        for q, qq, mask in zip(self.r_quads, block_eval.r_quads, r_quads_mask):
            m.d.comb += [
                q.eq(qq),
                q.valid.eq(qq.valid & ~mask),
            ]
        m.d.comb += [
            self.r_rdy.eq(block_eval.r_rdy),
            block_eval.r_en.eq(self.r_en),
        ]

        return m


class RasterUnit(HasRasterParams, Elaboratable):

    def __init__(self, n_cores, params):
        super().__init__(params)

        self.tile_count = Signal(16)
        self.tilebuf_addr = Signal(32)
        self.prim_addr = Signal(32)
        self.prim_stride = Signal(16)

        self.mem_bus = tl.Interface(data_width=64, addr_width=32, size_width=3)

        self.req = [
            Decoupled(RasterRequest, params, name=f'req{i}')
            for i in range(n_cores)
        ]

    def elaborate(self, platform):
        m = Module()

        fetch_unit = m.submodules.fetch_unit = FetchUnit(
            self.mem_bus, self.params)
        m.d.comb += [
            fetch_unit.tile_count.eq(self.tile_count),
            fetch_unit.tilebuf_addr.eq(self.tilebuf_addr),
            fetch_unit.prim_addr.eq(self.prim_addr),
            fetch_unit.prim_stride.eq(self.prim_stride),
        ]

        raster_slice = m.submodules.raster_slice = RasterSlice(self.params)
        m.d.comb += fetch_unit.req.connect(raster_slice.req)

        m.d.comb += [
            raster_slice.viewport.width.eq(640),
            raster_slice.viewport.height.eq(480),
        ]

        request = Signal(len(self.req))
        m.d.comb += request.eq(Cat(r.valid for r in self.req))

        req_grant = Signal(range(len(self.req)))
        req_last_grant = Signal.like(req_grant)
        m.d.sync += req_last_grant.eq(req_grant)
        m.d.comb += req_grant.eq(req_last_grant)
        with m.Switch(req_last_grant):
            for i in range(len(request)):
                with m.Case(i):
                    for pred in reversed(range(i)):
                        with m.If(request[pred]):
                            m.d.comb += req_grant.eq(pred)
                    for succ in reversed(range(i + 1, len(request))):
                        with m.If(request[succ]):
                            m.d.comb += req_grant.eq(succ)

        quad_valids = Cat(r.valid for r in raster_slice.r_quads)

        with m.Switch(req_grant):
            for i, req in enumerate(self.req):
                with m.Case(i):
                    with m.If(req.valid & (
                        (raster_slice.r_rdy & (
                            (req.bits.tmask & quad_valids) == req.bits.tmask))
                            | (fetch_unit.done & raster_slice.done))):
                        m.d.comb += [
                            req.ready.eq(1),
                            raster_slice.r_en.eq(1),
                        ]

                        m.d.comb += [
                            rr.eq(r) for rr, r in zip(req.bits.resp,
                                                      raster_slice.r_quads)
                        ]

        return m
