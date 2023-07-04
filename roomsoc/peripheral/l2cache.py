from enum import IntEnum
from amaranth import *
from amaranth import tracer
from amaranth.hdl.rec import *
from amaranth.utils import bits_for, log2_int
from amaranth.lib.scheduler import RoundRobin
from amaranth.lib.fifo import SyncFIFO
from amaranth.hdl.ast import ValueCastable

from .peripheral import Peripheral

from roomsoc.interconnect import tilelink as tl
from roomsoc.interconnect.stream import Valid, Decoupled, Queue


class HasL2CacheParams:

    def __init__(self, params, *args, **kwargs):
        self.params = params

        self.capacity_kb = params['capacity_kb']
        self.n_ways = params['n_ways']
        self.block_bytes = params['block_bytes']
        self.lg_block_bytes = log2_int(self.block_bytes)
        self.n_sets = (self.capacity_kb << 10) // (self.block_bytes *
                                                   self.n_ways)
        self.beat_bytes = params.get('beat_bytes', 8)
        self.n_mshrs = params.get('n_mshrs', 4)
        self.secondary = self.n_mshrs

        self.offset_bits = log2_int(self.block_bytes)
        self.index_bits = log2_int(self.n_sets)
        self.untag_bits = self.offset_bits + self.index_bits
        self.tag_bits = 32 - self.untag_bits

        self.client_source_map = params['client_source_map']
        self.num_clients = len(self.client_source_map)
        self.client_bits = max(self.num_clients, 1)

        self.n_rel_lists = 2
        self.n_rel_entries = self.block_bytes // self.beat_bytes

        self.put_bits = bits_for(self.n_rel_lists - 1)

        in_bus_params = params['in_bus']
        self.in_source_id_width = in_bus_params['source_id_width']
        self.in_sink_id_width = in_bus_params['sink_id_width']
        self.in_size_width = in_bus_params['size_width']

        out_bus_params = params['out_bus']
        self.out_source_id_width = out_bus_params['source_id_width']
        self.out_sink_id_width = out_bus_params['sink_id_width']

    def parse_addr(self, addr):
        return addr[self.untag_bits:], addr[
            self.offset_bits:self.untag_bits], addr[:self.offset_bits]

    def make_addr(self, tag, set, offset):
        return Cat(offset, set, tag)

    def client_bit(self, source_id):
        return Cat((source_id >= self.client_source_map[c][0])
                   & (source_id <= self.client_source_map[c][1])
                   for c in range(self.num_clients))

    def client_source(self, m, bit):
        source = Signal(self.in_source_id_width)

        for c in range(self.num_clients):
            with m.If(bit[c]):
                m.d.comb += source.eq(self.client_source_map[c][0])

        return source

    def need_t(self, opcode, param):
        return ~opcode[2] | (((opcode == tl.ChannelAOpcode.AcquireBlock) |
                              (opcode == tl.ChannelAOpcode.AcquirePerm)) &
                             (param != tl.GrowParam.NtoB))

    def skip_probe_n(self, opcode):
        return (opcode == tl.ChannelAOpcode.AcquireBlock) | (
            opcode == tl.ChannelAOpcode.AcquirePerm) | (
                opcode == tl.ChannelAOpcode.Get)


class CacheState(IntEnum):
    INVALID = 0
    BRANCH = 1
    TRUNK = 2
    TIP = 3

    @staticmethod
    def on_shrink(m, state, param):
        resp_param = Signal(3)
        new_state = Signal(CacheState)

        with m.Switch(param):
            with m.Case(tl.CapParam.toT):
                with m.Switch(state):
                    with m.Case(CacheState.TIP):
                        m.d.comb += [
                            resp_param.eq(tl.ShrinkReportParam.TtoT),
                            new_state.eq(CacheState.TIP),
                        ]
                    with m.Case(CacheState.TRUNK):
                        m.d.comb += [
                            resp_param.eq(tl.ShrinkReportParam.TtoT),
                            new_state.eq(CacheState.TRUNK),
                        ]
                    with m.Case(CacheState.BRANCH):
                        m.d.comb += [
                            resp_param.eq(tl.ShrinkReportParam.BtoB),
                            new_state.eq(CacheState.BRANCH),
                        ]
                    with m.Case(CacheState.INVALID):
                        m.d.comb += [
                            resp_param.eq(tl.ShrinkReportParam.NtoN),
                            new_state.eq(CacheState.INVALID),
                        ]
            with m.Case(tl.CapParam.toB):
                with m.Switch(state):
                    with m.Case(CacheState.TIP):
                        m.d.comb += [
                            resp_param.eq(tl.ShrinkReportParam.TtoB),
                            new_state.eq(CacheState.BRANCH),
                        ]
                    with m.Case(CacheState.TRUNK):
                        m.d.comb += [
                            resp_param.eq(tl.ShrinkReportParam.TtoB),
                            new_state.eq(CacheState.BRANCH),
                        ]
                    with m.Case(CacheState.BRANCH):
                        m.d.comb += [
                            resp_param.eq(tl.ShrinkReportParam.BtoB),
                            new_state.eq(CacheState.BRANCH),
                        ]
                    with m.Case(CacheState.INVALID):
                        m.d.comb += [
                            resp_param.eq(tl.ShrinkReportParam.NtoN),
                            new_state.eq(CacheState.INVALID),
                        ]
            with m.Case(tl.CapParam.toN):
                with m.Switch(state):
                    with m.Case(CacheState.TIP):
                        m.d.comb += [
                            resp_param.eq(tl.ShrinkReportParam.TtoN),
                            new_state.eq(CacheState.INVALID),
                        ]
                    with m.Case(CacheState.TRUNK):
                        m.d.comb += [
                            resp_param.eq(tl.ShrinkReportParam.TtoN),
                            new_state.eq(CacheState.INVALID),
                        ]
                    with m.Case(CacheState.BRANCH):
                        m.d.comb += [
                            resp_param.eq(tl.ShrinkReportParam.BtoN),
                            new_state.eq(CacheState.INVALID),
                        ]
                    with m.Case(CacheState.INVALID):
                        m.d.comb += [
                            resp_param.eq(tl.ShrinkReportParam.NtoN),
                            new_state.eq(CacheState.INVALID),
                        ]

        return resp_param, new_state


class BankedStore(HasL2CacheParams, Elaboratable):

    class Port(HasL2CacheParams, Record):

        def __init__(self, params, write=False, name=None, src_loc_at=0):
            HasL2CacheParams.__init__(self, params)

            Record.__init__(self,
                            [('noop', 1, DIR_FANOUT),
                             ('way', range(self.n_ways), DIR_FANOUT),
                             ('set', self.index_bits, DIR_FANOUT),
                             ('beat', range(self.block_bytes //
                                            self.beat_bytes), DIR_FANOUT),
                             ('mask', self.beat_bytes, DIR_FANOUT),
                             ('data', self.beat_bytes * 8,
                              DIR_FANOUT if write else DIR_FANIN)],
                            name=name,
                            src_loc_at=1 + src_loc_at)

    def __init__(self, params):
        super().__init__(params=params)

        self.sinkc_port = Decoupled(BankedStore.Port, params, write=True)
        self.sinkd_port = Decoupled(BankedStore.Port, params, write=True)
        self.sourcec_port = Decoupled(BankedStore.Port, params, write=False)
        self.sourced_rport = Decoupled(BankedStore.Port, params, write=False)
        self.sourced_wport = Decoupled(BankedStore.Port, params, write=True)

    def elaborate(self, platform):
        m = Module()

        num_banks = self.n_ways
        num_beats = self.block_bytes // self.beat_bytes
        num_rows = self.n_sets * num_beats

        mem_banks = [
            Memory(width=self.beat_bytes * 8, depth=num_rows, name=f'mem{i}')
            for i in range(num_banks)
        ]

        class Request(HasL2CacheParams, Record):

            def __init__(self, params, name=None, src_loc_at=0):
                HasL2CacheParams.__init__(self, params)

                Record.__init__(self,
                                [('wen', 1), ('index', range(num_rows)),
                                 ('bank_sel', num_banks),
                                 ('bank_sum', num_banks),
                                 ('bank_en', num_banks),
                                 ('data', num_banks * self.beat_bytes * 8)],
                                name=name,
                                src_loc_at=1 + src_loc_at)

        def make_request(port, write, name=None):
            out = Request(self.params, name=name)

            ready = Array(~x for x in out.bank_sum)
            m.d.comb += port.ready.eq(ready[port.bits.way])

            m.d.comb += [
                out.wen.eq(write),
                out.index.eq(Cat(port.bits.beat, port.bits.set)),
            ]

            with m.If(port.valid):
                m.d.comb += out.bank_sel.eq(
                    Cat(w == port.bits.way for w in range(self.n_ways)))

            with m.If(~port.bits.noop):
                m.d.comb += out.bank_en.eq(out.bank_sel & Cat(ready))

            m.d.comb += out.data.eq(Cat([port.bits.data] * num_banks))
            return out

        sinkc_req = make_request(self.sinkc_port, True, 'sinkc_req')
        sinkd_req = make_request(self.sinkd_port, True, 'sinkd_req')
        sourcec_req = make_request(self.sourcec_port, False, 'sourcec_req')
        sourced_rreq = make_request(self.sourced_rport, False, 'sourced_rreq')
        sourced_wreq = make_request(self.sourced_wport, True, 'sourced_wreq')

        reqs = [sinkc_req, sourcec_req, sinkd_req, sourced_wreq, sourced_rreq]

        bank_sum = 0
        for r in reqs:
            m.d.comb += r.bank_sum.eq(bank_sum)
            bank_sum = r.bank_sel | bank_sum

        regout = [
            Signal(self.beat_bytes * 8, name=f'regout{i}')
            for i in range(num_banks)
        ]

        for i, bank in enumerate(mem_banks):
            rport = bank.read_port(transparent=False)
            setattr(m.submodules, f'rport{i}', rport)
            wport = bank.write_port()
            setattr(m.submodules, f'wport{i}', wport)

            bank_wen = Signal()

            for req in reversed(reqs):
                with m.If(req.bank_en[i]):
                    m.d.comb += [
                        wport.addr.eq(req.index),
                        rport.addr.eq(req.index),
                        wport.data.eq(
                            req.data[i * self.beat_bytes * 8:(i + 1) *
                                     self.beat_bytes * 8]),
                        wport.en.eq(req.wen),
                        bank_wen.eq(req.wen),
                    ]

            bank_en = Cat(req.bank_en[i] for req in reqs) != 0
            bank_ren = Signal()
            m.d.sync += bank_ren.eq(bank_en & ~bank_wen)

            with m.If(bank_ren):
                m.d.sync += regout[i].eq(rport.data)

        sourcec_regsel_d1 = Signal(num_banks)
        sourcec_regsel_d2 = Signal(num_banks)
        m.d.sync += [
            sourcec_regsel_d1.eq(sourcec_req.bank_en),
            sourcec_regsel_d2.eq(sourcec_regsel_d1),
        ]

        for i, bank_out in enumerate(regout):
            with m.If(sourcec_regsel_d2[i]):
                m.d.comb += self.sourcec_port.bits.data.eq(bank_out)

        sourced_regsel_d1 = Signal(num_banks)
        sourced_regsel_d2 = Signal(num_banks)
        m.d.sync += [
            sourced_regsel_d1.eq(sourced_rreq.bank_en),
            sourced_regsel_d2.eq(sourced_regsel_d1),
        ]

        for i, bank_out in enumerate(regout):
            with m.If(sourced_regsel_d2[i]):
                m.d.comb += self.sourced_rport.bits.data.eq(bank_out)

        return m


class BaseRequest(HasL2CacheParams, ValueCastable):

    def __init__(self, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)
        self.name = name

        self.prio = Signal(3, name=f'{name}_prio')
        self.control = Signal(name=f'{name}_control')
        self.opcode = Signal(3, name=f'{name}_opcode')
        self.param = Signal(3, name=f'{name}_param')
        self.size = Signal(self.in_size_width, name=f'{name}_size')
        self.source = Signal(self.in_source_id_width, name=f'{name}_source')
        self.tag = Signal(self.tag_bits, name=f'{name}_tag')
        self.offset = Signal(self.offset_bits, name=f'{name}_offset')
        self.put = Signal(self.put_bits, name=f'{name}_put')

    @ValueCastable.lowermethod
    def as_value(self):
        return Cat(self.prio, self.control, self.opcode, self.param, self.size,
                   self.source, self.tag, self.offset, self.put)

    def __len__(self):
        return len(Value.cast(self))

    def eq(self, rhs):
        return Value.cast(self).eq(Value.cast(rhs))


class FullRequest(BaseRequest):

    def __init__(self, params, name=None, src_loc_at=0):
        super().__init__(params=params, name=name, src_loc_at=src_loc_at + 1)

        self.set = Signal(self.index_bits, name=f'{self.name}_set')

    @ValueCastable.lowermethod
    def as_value(self):
        return Cat(super().as_value(), self.set)


class AllocateRequest(FullRequest):

    def __init__(self, params, name=None, src_loc_at=0):
        super().__init__(params=params, name=name, src_loc_at=src_loc_at + 1)

        self.repeat = Signal(name=f'{self.name}_repeat')

    @ValueCastable.lowermethod
    def as_value(self):
        return Cat(super().as_value(), self.repeat)


class Directory(HasL2CacheParams, Elaboratable):

    class Entry(HasL2CacheParams, Record):

        def __init__(self, params, name=None, src_loc_at=0):
            HasL2CacheParams.__init__(self, params)

            Record.__init__(self, [('dirty', 1), ('state', CacheState),
                                   ('clients', self.client_bits),
                                   ('tag', self.tag_bits)],
                            name=name,
                            src_loc_at=1 + src_loc_at)

    class WriteReq(HasL2CacheParams, Record):

        def __init__(self, params, name=None, src_loc_at=0):
            HasL2CacheParams.__init__(self, params)

            Record.__init__(self, [('set', self.index_bits),
                                   ('way', range(self.n_ways)), ('dirty', 1),
                                   ('state', CacheState),
                                   ('clients', self.client_bits),
                                   ('tag', self.tag_bits)],
                            name=name,
                            src_loc_at=1 + src_loc_at)

    class ReadReq(HasL2CacheParams, Record):

        def __init__(self, params, name=None, src_loc_at=0):
            HasL2CacheParams.__init__(self, params)

            Record.__init__(self, [('set', self.index_bits),
                                   ('tag', self.tag_bits)],
                            name=name,
                            src_loc_at=1 + src_loc_at)

    class Result(HasL2CacheParams, Record):

        def __init__(self, params, name=None, src_loc_at=0):
            HasL2CacheParams.__init__(self, params)

            Record.__init__(self, [('hit', 1), ('way', range(self.n_ways)),
                                   ('dirty', 1), ('state', CacheState),
                                   ('clients', self.client_bits),
                                   ('tag', self.tag_bits)],
                            name=name,
                            src_loc_at=1 + src_loc_at)

    def __init__(self, params):
        super().__init__(params=params)

        self.write = Decoupled(Directory.WriteReq, params)
        self.read = Valid(Directory.ReadReq, params)
        self.result = Valid(Directory.Result, params)
        self.ready = Signal()

    def elaborate(self, platform):
        m = Module()

        m.d.comb += self.ready.eq(1)

        dir_mem = Memory(width=self.n_ways * len(Directory.Entry(self.params)),
                         depth=self.n_sets)

        write_q = m.submodules.write_q = SyncFIFO(width=len(self.write.bits),
                                                  depth=1)
        write_req = Directory.WriteReq(self.params)
        write_entry = Directory.Entry(self.params)
        m.d.comb += [
            self.write.ready.eq(write_q.w_rdy),
            write_q.w_data.eq(self.write.bits),
            write_q.w_en.eq(self.write.valid),
            write_q.r_en.eq(~self.read.valid & write_q.r_rdy),
            write_req.eq(write_q.r_data),
            write_entry.dirty.eq(write_req.dirty),
            write_entry.state.eq(write_req.state),
            write_entry.clients.eq(write_req.clients),
            write_entry.tag.eq(write_req.tag),
        ]

        wen = write_q.r_rdy

        wport = m.submodules.wport = dir_mem.write_port(
            granularity=len(write_entry))
        with m.If(wen & ~self.read.valid):
            m.d.comb += [
                wport.addr.eq(write_req.set),
                wport.data.eq(Repl(write_entry, self.n_ways)),
                wport.en.eq(1 << write_req.way),
            ]

        ren1 = Signal()
        tag = Signal.like(self.read.bits.tag)
        set = Signal.like(self.read.bits.set)
        m.d.sync += ren1.eq(self.read.valid)
        with m.If(self.read.valid):
            m.d.sync += [
                tag.eq(self.read.bits.tag),
                set.eq(self.read.bits.set),
            ]

        rport = m.submodules.rport = dir_mem.read_port(transparent=False)
        m.d.comb += rport.addr.eq(self.read.bits.set)

        rways = Array(
            Directory.Entry(self.params, name=f'rways{i}')
            for i in range(self.n_ways))
        m.d.comb += Cat(rways).eq(rport.data)

        hits = Signal(self.n_ways)
        m.d.comb += hits.eq(
            Cat((w.tag == tag) & (w.state != CacheState.INVALID)
                for w in rways))
        hit = hits != 0

        hit_way = Signal(range(self.n_ways))
        for w in reversed(range(self.n_ways)):
            with m.If(hits[w]):
                m.d.comb += hit_way.eq(w)

        victim_way = Signal(range(self.n_ways))
        with m.If(ren1):
            m.d.sync += victim_way.eq(victim_way + 1)

        set_match = wen & (write_req.set == set)
        tag_match = write_req.tag == tag
        way_match = write_req.way == victim_way

        m.d.comb += [
            self.result.valid.eq(ren1),
            self.result.bits.hit.eq(hit | (set_match & tag_match & (
                write_req.state != CacheState.INVALID))),
            self.result.bits.way.eq(
                Mux(hit, hit_way,
                    Mux(set_match & tag_match, write_req.way, victim_way))),
        ]

        with m.If(hit):
            m.d.comb += [
                self.result.bits.dirty.eq(rways[hit_way].dirty),
                self.result.bits.state.eq(rways[hit_way].state),
                self.result.bits.clients.eq(rways[hit_way].clients),
                self.result.bits.tag.eq(rways[hit_way].tag),
            ]
        with m.Elif(set_match & (tag_match | way_match)):
            m.d.comb += [
                self.result.bits.dirty.eq(write_req.dirty),
                self.result.bits.state.eq(write_req.state),
                self.result.bits.clients.eq(write_req.clients),
                self.result.bits.tag.eq(write_req.tag),
            ]
        with m.Else():
            m.d.comb += [
                self.result.bits.dirty.eq(rways[victim_way].dirty),
                self.result.bits.state.eq(rways[victim_way].state),
                self.result.bits.clients.eq(rways[victim_way].clients),
                self.result.bits.tag.eq(rways[victim_way].tag),
            ]

        return m


class SourceA(HasL2CacheParams, Elaboratable):

    class Request(HasL2CacheParams, Record):

        def __init__(self, params, name=None, src_loc_at=0):
            HasL2CacheParams.__init__(self, params)

            Record.__init__(self, [
                ('set', self.index_bits),
                ('tag', self.tag_bits),
                ('param', 3),
                ('source', self.out_source_id_width),
                ('block', 1),
            ],
                            name=name,
                            src_loc_at=1 + src_loc_at)

    def __init__(self, params):
        super().__init__(params=params)

        self.a = Decoupled(tl.ChannelA,
                           addr_width=32,
                           data_width=self.beat_bytes * 8,
                           size_width=bits_for(self.lg_block_bytes),
                           source_id_width=self.out_source_id_width)

        self.req = Decoupled(SourceA.Request, params)

    def elaborate(self, platform):
        m = Module()

        a = tl.ChannelA(addr_width=32,
                        data_width=self.beat_bytes * 8,
                        size_width=bits_for(self.lg_block_bytes),
                        source_id_width=self.out_source_id_width)

        m.d.comb += [
            a.opcode.eq(
                Mux(self.req.bits.block, tl.ChannelAOpcode.AcquireBlock,
                    tl.ChannelAOpcode.AcquirePerm)),
            a.param.eq(self.req.bits.param),
            a.size.eq(self.offset_bits),
            a.source.eq(self.req.bits.source),
            a.address.eq(
                self.make_addr(self.req.bits.tag, self.req.bits.set,
                               Const(0, self.offset_bits))),
            a.mask.eq(~0),
        ]

        queue = m.submodules.queue = SyncFIFO(depth=1, width=len(a))
        m.d.comb += [
            queue.w_data.eq(a),
            queue.w_en.eq(self.req.valid),
            self.req.ready.eq(queue.w_rdy),
            self.a.bits.eq(queue.r_data),
            self.a.valid.eq(queue.r_rdy),
            queue.r_en.eq(self.a.ready),
        ]

        with m.If(self.req.valid):
            m.d.comb += self.a.valid.eq(1)
        with m.If(queue.w_rdy):
            m.d.comb += [
                self.a.bits.eq(a),
                queue.r_en.eq(0),
            ]
            with m.If(self.a.ready):
                m.d.comb += queue.w_en.eq(0)

        return m


class SourceB(HasL2CacheParams, Elaboratable):

    class Request(HasL2CacheParams, Record):

        def __init__(self, params, name=None, src_loc_at=0):
            HasL2CacheParams.__init__(self, params)

            Record.__init__(self, [
                ('set', self.index_bits),
                ('tag', self.tag_bits),
                ('param', 3),
                ('clients', self.client_bits),
            ],
                            name=name,
                            src_loc_at=1 + src_loc_at)

    def __init__(self, params):
        super().__init__(params=params)

        self.b = Decoupled(tl.ChannelB,
                           addr_width=32,
                           data_width=self.beat_bytes * 8,
                           size_width=bits_for(self.lg_block_bytes),
                           source_id_width=self.in_source_id_width)

        self.req = Decoupled(SourceB.Request, params)

    def elaborate(self, platform):
        m = Module()

        remain = Signal(self.client_bits)
        remain_set = Signal.like(remain)
        remain_clr = Signal.like(remain)

        busy = remain != 0
        todo = Mux(busy, remain, self.req.bits.clients)
        next = Signal.like(remain)

        for i in reversed(range(self.client_bits)):
            with m.If(todo[i]):
                m.d.comb += next.eq(1 << i)

        m.d.comb += self.req.ready.eq(~busy)
        with m.If(self.req.fire):
            m.d.comb += remain_set.eq(self.req.bits.clients)

        b = self.b

        m.d.comb += b.valid.eq(busy | self.req.valid)
        with m.If(b.fire):
            m.d.comb += remain_clr.eq(next)

        m.d.sync += remain.eq((remain | remain_set) & ~remain_clr)

        tag_reg = Signal.like(self.req.bits.tag)
        set_reg = Signal.like(self.req.bits.set)
        param_reg = Signal.like(self.req.bits.param)

        with m.If(self.req.fire):
            m.d.sync += [
                tag_reg.eq(self.req.bits.tag),
                set_reg.eq(self.req.bits.set),
                param_reg.eq(self.req.bits.param),
            ]

        m.d.comb += [
            b.bits.opcode.eq(tl.ChannelBOpcode.Probe),
            b.bits.param.eq(Mux(~busy, self.req.bits.param, param_reg)),
            b.bits.size.eq(self.offset_bits),
            b.bits.source.eq(self.client_source(m, next)),
            b.bits.address.eq(
                self.make_addr(Mux(~busy, self.req.bits.tag, tag_reg),
                               Mux(~busy, self.req.bits.set, set_reg),
                               Const(0, self.offset_bits))),
            b.bits.mask.eq(~0),
        ]

        return m


class SourceC(HasL2CacheParams, Elaboratable):

    class Request(HasL2CacheParams, Record):

        def __init__(self, params, name=None, src_loc_at=0):
            HasL2CacheParams.__init__(self, params)

            Record.__init__(self, [
                ('opcode', 3),
                ('param', 3),
                ('set', self.index_bits),
                ('tag', self.tag_bits),
                ('way', range(self.n_ways)),
                ('source', self.out_source_id_width),
                ('dirty', 1),
            ],
                            name=name,
                            src_loc_at=1 + src_loc_at)

    def __init__(self, params):
        super().__init__(params=params)

        self.c = Decoupled(tl.ChannelC,
                           addr_width=32,
                           data_width=self.beat_bytes * 8,
                           size_width=bits_for(self.lg_block_bytes),
                           source_id_width=self.out_source_id_width)

        self.req = Decoupled(SourceC.Request, params)

        self.port = Decoupled(BankedStore.Port, params, write=False)

    def elaborate(self, platform):
        m = Module()

        beats = self.block_bytes // self.beat_bytes
        queue = m.submodules.queue = SyncFIFO(depth=beats,
                                              width=len(self.c.bits))

        busy = Signal()

        req_reg = SourceC.Request(self.params)
        req = SourceC.Request(self.params)
        with m.If(self.req.valid & ~busy):
            m.d.sync += req_reg.eq(self.req.bits)
        with m.If(~busy):
            m.d.comb += req.eq(self.req.bits)
        with m.Else():
            m.d.comb += req.eq(req_reg)

        beat = Signal(range(beats))
        last = beat == Repl(1, len(beat))
        want_data = Signal()

        m.d.comb += self.req.ready.eq(~busy)

        m.d.comb += [
            self.port.valid.eq(want_data),
            self.port.bits.way.eq(req.way),
            self.port.bits.set.eq(req.set),
            self.port.bits.beat.eq(beat),
            self.port.bits.mask.eq(~0),
        ]

        with m.If(self.req.valid & self.req.bits.dirty):
            m.d.sync += [
                busy.eq(1),
                want_data.eq(1),
            ]
        with m.If(self.port.fire):
            with m.If(last):
                m.d.sync += want_data.eq(0)
            m.d.sync += beat.eq(beat + 1)

        s2_latch = Mux(want_data | (self.req.valid & self.req.bits.dirty),
                       self.port.fire, self.req.fire)
        s2_valid = Signal()
        s2_req = SourceC.Request(self.params)
        s2_beat = Signal.like(beat)
        s2_last = Signal()

        m.d.sync += s2_valid.eq(s2_latch)
        with m.If(s2_latch):
            m.d.sync += [
                s2_req.eq(req),
                s2_beat.eq(beat),
                s2_last.eq(last),
            ]

        s3_latch = s2_valid
        s3_valid = Signal()
        s3_req = SourceC.Request(self.params)
        s3_beat = Signal.like(beat)
        s3_last = Signal()

        m.d.sync += s3_valid.eq(s3_latch)
        with m.If(s3_latch):
            m.d.sync += [
                s3_req.eq(s2_req),
                s3_beat.eq(s2_beat),
                s3_last.eq(s2_last),
            ]

        queue_wdata = tl.ChannelC(addr_width=32,
                                  data_width=self.beat_bytes * 8,
                                  size_width=bits_for(self.lg_block_bytes),
                                  source_id_width=self.out_source_id_width)
        m.d.comb += [
            queue_wdata.opcode.eq(s3_req.opcode),
            queue_wdata.param.eq(s3_req.param),
            queue_wdata.size.eq(self.offset_bits),
            queue_wdata.source.eq(s3_req.source),
            queue_wdata.address.eq(
                self.make_addr(s3_req.tag, s3_req.set,
                               Const(0, self.offset_bits))),
            queue_wdata.data.eq(self.port.bits.data),
            queue.w_data.eq(queue_wdata),
            queue.w_en.eq(s3_valid),
        ]

        m.d.comb += [
            self.c.valid.eq(queue.r_rdy),
            self.c.bits.eq(queue.r_data),
            queue.r_en.eq(self.c.ready),
        ]

        _, _, done, _ = tl.Interface.count(m, self.c.bits, self.c.fire)
        with m.If(done):
            m.d.sync += busy.eq(0)

        return m


class SourceD(HasL2CacheParams, Elaboratable):

    class Request(FullRequest):

        def __init__(self, params, name=None, src_loc_at=0):
            super().__init__(params=params,
                             name=name,
                             src_loc_at=src_loc_at + 1)

            self.sink = Signal(self.in_sink_id_width, name=f'{self.name}_sink')
            self.way = Signal(self.n_ways, name=f'{self.name}_way')
            self.bad = Signal(name=f'{self.name}_bad')

        @ValueCastable.lowermethod
        def as_value(self):
            return Cat(super().as_value(), self.sink, self.way, self.bad)

    def __init__(self, params):
        super().__init__(params=params)

        self.req = Decoupled(SourceD.Request, params)

        self.d = Decoupled(tl.ChannelD,
                           data_width=self.beat_bytes * 8,
                           size_width=bits_for(self.lg_block_bytes),
                           source_id_width=self.in_source_id_width,
                           sink_id_width=self.in_sink_id_width)

        self.bs_rport = Decoupled(BankedStore.Port, params, write=False)
        self.bs_wport = Decoupled(BankedStore.Port, params, write=True)

        self.rel_pop = Decoupled(PutBufferPop, params)
        self.rel_entry = SinkC.PutBufferEntry(params)

    def elaborate(self, platform):
        m = Module()

        s1_valid = Signal()
        s2_valid = Signal()
        s3_valid = Signal()
        s2_ready = Signal()
        s3_ready = Signal()
        s4_ready = Signal()

        busy = Signal()

        s1_counter = Signal(range(self.block_bytes // self.beat_bytes))
        s1_block_r = Signal()
        s1_req = SourceD.Request(self.params)
        s1_req_reg = SourceD.Request(self.params)
        s1_grant = ((s1_req.opcode == tl.ChannelAOpcode.AcquireBlock) &
                    (s1_req.param == tl.GrowParam.BtoT)) | (
                        s1_req.opcode == tl.ChannelAOpcode.AcquirePerm)
        s1_need_r = s1_req.prio[0] & ~s1_grant
        s1_valid_r = (busy | self.req.valid) & s1_need_r & ~s1_block_r
        s1_need_pb = Mux(s1_req.prio[0], ~s1_req.opcode[2], s1_req.opcode[0])
        s1_single = Mux(s1_req.prio[0], s1_grant,
                        s1_req.opcode == tl.ChannelCOpcode.Release)
        s1_num_beats = Mux(s1_single, 1,
                           (1 << s1_req.size) >> log2_int(self.beat_bytes))
        s1_last = s1_counter == s1_num_beats - 1
        s1_first = s1_counter == 0

        with m.If(busy):
            m.d.comb += s1_req.eq(s1_req_reg)
        with m.Else():
            m.d.comb += s1_req.eq(self.req.bits)

        m.d.comb += [
            self.bs_rport.valid.eq(s1_valid_r),
            self.bs_rport.bits.way.eq(s1_req.way),
            self.bs_rport.bits.set.eq(s1_req.set),
            self.bs_rport.bits.beat.eq(s1_counter),
        ]

        bs_rport_fire_d1 = Signal()
        bs_rport_fire_d2 = Signal()
        m.d.sync += [
            bs_rport_fire_d1.eq(self.bs_rport.fire),
            bs_rport_fire_d2.eq(bs_rport_fire_d1),
        ]

        bs_rbuf = m.submodules.bs_rbuf = SyncFIFO(width=len(
            self.bs_rport.bits.data),
                                                  depth=3)
        bs_rbuf_wen = Signal()
        bs_rbuf_ren = Signal()
        m.d.comb += [
            bs_rbuf.w_data.eq(self.bs_rport.bits.data),
            bs_rbuf.w_en.eq(bs_rbuf_wen & (~bs_rbuf_ren | bs_rbuf.r_rdy)),
            bs_rbuf_wen.eq(bs_rport_fire_d2),
        ]

        with m.If(self.bs_rport.fire):
            m.d.sync += s1_block_r.eq(1)
        with m.If(self.req.valid & ~busy):
            m.d.sync += [
                busy.eq(1),
                s1_req_reg.eq(self.req.bits),
            ]
        with m.If(s1_valid & s2_ready):
            m.d.sync += [
                s1_counter.eq(s1_counter + 1),
                s1_block_r.eq(0),
            ]

            with m.If(s1_last):
                m.d.sync += [
                    s1_counter.eq(0),
                    busy.eq(0),
                ]

        m.d.comb += [
            s1_valid.eq((busy | self.req.valid)
                        & (~s1_valid_r | self.bs_rport.ready)),
            self.req.ready.eq(~busy),
        ]

        s2_latch = s1_valid & s2_ready
        s2_full = Signal()
        s2_valid_pb = Signal()
        s2_req = SourceD.Request(self.params)
        s2_need_r = Signal()
        s2_need_d = Signal()
        s2_need_pb = Signal()
        s2_last = Signal()
        s2_pb_data_reg = Signal(self.beat_bytes * 8)
        s2_pb_mask_reg = Signal(self.beat_bytes)
        s2_pb_corrupt_reg = Signal()
        s2_counter = Signal.like(s1_counter)

        s2_pb_data_raw = Mux(s2_req.prio[0], 0, self.rel_entry.data)
        s2_pb_mask_raw = Mux(s2_req.prio[0], 0, ~0)
        s2_pb_corrupt_raw = Mux(s2_req.prio[0], 0, self.rel_entry.corrupt)

        with m.If(s2_valid_pb):
            m.d.sync += [
                s2_pb_data_reg.eq(s2_pb_data_raw),
                s2_pb_mask_reg.eq(s2_pb_mask_raw),
                s2_pb_corrupt_reg.eq(s2_pb_corrupt_raw),
            ]

        s2_pb_data = Mux(s2_valid_pb, s2_pb_data_raw, s2_pb_data_reg)
        s2_pb_mask = Mux(s2_valid_pb, s2_pb_mask_raw, s2_pb_mask_reg)
        s2_pb_corrupt = Mux(s2_valid_pb, s2_pb_corrupt_raw, s2_pb_corrupt_reg)

        m.d.comb += [
            self.rel_pop.valid.eq(s2_valid_pb & ~s2_req.prio[0]),
            self.rel_pop.bits.index.eq(s2_req.put),
            self.rel_pop.bits.last.eq(s2_last),
        ]

        pb_ready = Mux(s2_req.prio[0], 0, self.rel_pop.ready)
        with m.If(pb_ready):
            m.d.sync += s2_valid_pb.eq(0)
        with m.If(s2_valid & s3_ready):
            m.d.sync += s2_full.eq(0)
        with m.If(s2_latch):
            m.d.sync += [
                s2_full.eq(1),
                s2_req.eq(s1_req),
                s2_last.eq(s1_last),
                s2_valid_pb.eq(s1_need_pb),
                s2_need_r.eq(s1_need_r),
                s2_need_d.eq(~s1_need_pb | s1_first),
                s2_need_pb.eq(s1_need_pb),
                s2_counter.eq(s1_counter),
            ]

        m.d.comb += [
            s2_valid.eq(s2_full & (~s2_valid_pb | pb_ready)),
            s2_ready.eq(~s2_full | (s3_ready & (~s2_valid_pb | pb_ready))),
        ]

        s3_latch = s2_valid & s3_ready
        s3_full = Signal()
        s3_valid_d = Signal()
        s3_req = SourceD.Request(self.params)
        s3_need_r = Signal()
        s3_need_pb = Signal()
        s3_rdata = Mux(~bs_rbuf.r_rdy, self.bs_rport.bits.data, bs_rbuf.r_data)
        s3_pb_data = Signal(self.beat_bytes * 8)
        s3_pb_mask = Signal(self.beat_bytes)
        s3_pb_corrupt = Signal()
        s3_counter = Signal.like(s2_counter)
        s3_acquire = (s3_req.opcode == tl.ChannelAOpcode.AcquireBlock) | (
            s3_req.opcode == tl.ChannelAOpcode.AcquirePerm)

        resp_opcode = Signal.like(self.d.bits.opcode)
        with m.Switch(s3_req.opcode):
            with m.Case(tl.ChannelAOpcode.PutFullData):
                m.d.comb += resp_opcode.eq(tl.ChannelDOpcode.AccessAck)
            with m.Case(tl.ChannelAOpcode.PutPartialData):
                m.d.comb += resp_opcode.eq(tl.ChannelDOpcode.AccessAck)
            with m.Case(tl.ChannelAOpcode.Get):
                m.d.comb += resp_opcode.eq(tl.ChannelDOpcode.AccessAckData)
            with m.Case(tl.ChannelAOpcode.AcquireBlock):
                m.d.comb += resp_opcode.eq(
                    Mux(s3_req.param == tl.GrowParam.BtoT,
                        tl.ChannelDOpcode.Grant, tl.ChannelDOpcode.GrantData))
            with m.Case(tl.ChannelAOpcode.AcquirePerm):
                m.d.comb += resp_opcode.eq(tl.ChannelDOpcode.Grant)

        d = self.d

        m.d.comb += [
            d.valid.eq(s3_valid_d),
            d.bits.opcode.eq(
                Mux(s3_req.prio[0], resp_opcode,
                    tl.ChannelDOpcode.ReleaseAck)),
            d.bits.param.eq(
                Mux(
                    s3_req.prio[0] & s3_acquire,
                    Mux(s3_req.param != tl.GrowParam.NtoB, tl.CapParam.toT,
                        tl.CapParam.toB), 0)),
            d.bits.size.eq(s3_req.size),
            d.bits.source.eq(s3_req.source),
            d.bits.sink.eq(s3_req.sink),
            d.bits.denied.eq(s3_req.bad),
            d.bits.data.eq(s3_rdata),
            d.bits.corrupt.eq(s3_req.bad & d.bits.opcode[0]),
        ]

        m.d.comb += [
            bs_rbuf_ren.eq(s3_valid & s4_ready & s3_need_r),
            bs_rbuf.r_en.eq(bs_rbuf_ren & bs_rbuf.r_rdy),
        ]

        with m.If(d.ready):
            m.d.sync += s3_valid_d.eq(0)
        with m.If(s3_valid & s4_ready):
            m.d.sync += s3_full.eq(0)
        with m.If(s3_latch):
            m.d.sync += [
                s3_full.eq(1),
                s3_valid_d.eq(s2_need_d),
                s3_req.eq(s2_req),
                s3_need_pb.eq(s2_need_pb),
                s3_pb_data.eq(s2_pb_data),
                s3_pb_mask.eq(s2_pb_mask),
                s3_pb_corrupt.eq(s2_pb_corrupt),
                s3_need_r.eq(s2_need_r),
                s3_counter.eq(s2_counter),
            ]

        m.d.comb += [
            s3_valid.eq(s3_full & (~s3_valid_d | d.ready)),
            s3_ready.eq(~s3_full | (s4_ready & (~s3_valid_d | d.ready))),
        ]

        s4_latch = s3_valid & s4_ready
        s4_full = Signal()
        s4_req = SourceD.Request(self.params)
        s4_need_wb = Signal()
        s4_pb_data = Signal(self.beat_bytes * 8)
        s4_pb_mask = Signal(self.beat_bytes)
        s4_pb_corrupt = Signal()
        s4_counter = Signal.like(s3_counter)

        m.d.comb += [
            self.bs_wport.valid.eq(s4_full & s4_need_wb),
            self.bs_wport.bits.way.eq(s4_req.way),
            self.bs_wport.bits.set.eq(s4_req.set),
            self.bs_wport.bits.beat.eq(s4_counter),
            self.bs_wport.bits.mask.eq(s4_pb_mask),
            self.bs_wport.bits.data.eq(s4_pb_data),
        ]

        with m.If(self.bs_wport.ready | ~s4_need_wb):
            m.d.sync += s4_full.eq(0)
        with m.If(s4_latch):
            m.d.sync += [
                s4_full.eq(1),
                s4_req.eq(s3_req),
                s4_need_wb.eq(s3_need_pb),
                s4_pb_data.eq(s3_pb_data),
                s4_pb_mask.eq(s3_pb_mask),
                s4_pb_corrupt.eq(s3_pb_corrupt),
                s4_counter.eq(s3_counter),
            ]

        m.d.comb += s4_ready.eq(~s4_full | self.bs_wport.ready | ~s4_need_wb)

        return m


class SourceE(HasL2CacheParams, Elaboratable):

    class Request(HasL2CacheParams, Record):

        def __init__(self, params, name=None, src_loc_at=0):
            HasL2CacheParams.__init__(self, params=params)

            Record.__init__(self, [
                ('sink', self.out_sink_id_width),
            ],
                            name=name,
                            src_loc_at=1 + src_loc_at)

    def __init__(self, params):
        super().__init__(params=params)

        self.req = Decoupled(SourceE.Request, params)

        self.e = Decoupled(tl.ChannelE)

    def elaborate(self, platform):
        m = Module()

        e = tl.ChannelE()

        m.d.comb += [
            e.sink.eq(self.req.bits.sink),
        ]

        queue = m.submodules.queue = SyncFIFO(depth=1, width=len(e))
        m.d.comb += [
            queue.w_data.eq(e),
            queue.w_en.eq(self.req.valid),
            self.req.ready.eq(queue.w_rdy),
            self.e.bits.eq(queue.r_data),
            self.e.valid.eq(queue.r_rdy),
            queue.r_en.eq(self.e.ready),
        ]

        with m.If(self.req.valid):
            m.d.comb += self.e.valid.eq(1)
        with m.If(queue.w_rdy):
            m.d.comb += [
                self.e.bits.eq(e),
                queue.r_en.eq(0),
            ]
            with m.If(self.e.ready):
                m.d.comb += queue.w_en.eq(0)

        return m


class SourceX(HasL2CacheParams, Elaboratable):

    class Request(Record):

        def __init__(self, name=None, src_loc_at=0):
            super().__init__([
                ('fail', 1),
            ],
                             name=name,
                             src_loc_at=1 + src_loc_at)

    def __init__(self, params):
        super().__init__(params=params)

        self.req = Decoupled(SourceX.Request)

    def elaborate(self, platform):
        m = Module()

        m.d.comb += self.req.ready.eq(1)

        return m


class PutBufferPop(HasL2CacheParams, Record):

    def __init__(self, params, name=None, src_loc_at=0):
        HasL2CacheParams.__init__(self, params=params)

        Record.__init__(self, [
            ('index', self.put_bits, Direction.FANOUT),
            ('last', 1, Direction.FANOUT),
        ],
                        name=name,
                        src_loc_at=1 + src_loc_at)


class SinkA(HasL2CacheParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params=params)

        self.a = Decoupled(tl.ChannelA,
                           addr_width=32,
                           data_width=self.beat_bytes * 8,
                           size_width=self.in_size_width,
                           source_id_width=self.in_source_id_width)

        self.req = Decoupled(FullRequest, params)

    def elaborate(self, platform):
        m = Module()

        a = self.a

        first, _, _, _ = tl.Interface.count(m, a.bits, a.fire)

        req_stall = first & ~self.req.ready

        m.d.comb += [
            a.ready.eq(~req_stall),
            self.req.valid.eq(a.valid),
        ]

        tag, set, offset = self.parse_addr(a.bits.address)

        m.d.comb += [
            self.req.bits.prio.eq(1),
            self.req.bits.opcode.eq(a.bits.opcode),
            self.req.bits.param.eq(a.bits.param),
            self.req.bits.size.eq(a.bits.size),
            self.req.bits.source.eq(a.bits.source),
            self.req.bits.offset.eq(offset),
            self.req.bits.set.eq(set),
            self.req.bits.tag.eq(tag),
        ]

        return m


class SinkB(HasL2CacheParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params=params)

        self.b = Decoupled(tl.ChannelB,
                           addr_width=32,
                           data_width=self.beat_bytes * 8,
                           size_width=self.in_size_width,
                           source_id_width=self.out_source_id_width)

        self.req = Decoupled(FullRequest, params)

    def elaborate(self, platform):
        m = Module()

        b = self.b

        m.d.comb += [
            b.ready.eq(self.req.ready),
            self.req.valid.eq(b.valid),
        ]

        tag, set, offset = self.parse_addr(b.bits.address)

        m.d.comb += [
            self.req.bits.prio.eq(2),
            self.req.bits.opcode.eq(b.bits.opcode),
            self.req.bits.param.eq(b.bits.param),
            self.req.bits.size.eq(b.bits.size),
            self.req.bits.source.eq(b.bits.source),
            self.req.bits.offset.eq(offset),
            self.req.bits.set.eq(set),
            self.req.bits.tag.eq(tag),
        ]

        return m


class SinkC(HasL2CacheParams, Elaboratable):

    class Response(HasL2CacheParams, Record):

        def __init__(self, params, name=None, src_loc_at=0):
            HasL2CacheParams.__init__(self, params=params)

            Record.__init__(self, [
                ('last', 1),
                ('set', range(self.n_sets)),
                ('tag', self.tag_bits),
                ('source', self.in_source_id_width),
                ('param', 3),
                ('data', 1),
            ],
                            name=name,
                            src_loc_at=1 + src_loc_at)

    class PutBufferEntry(HasL2CacheParams, Record):

        def __init__(self, params, name=None, src_loc_at=0):
            HasL2CacheParams.__init__(self, params=params)

            Record.__init__(self, [
                ('data', self.beat_bytes * 8),
                ('corrupt', 1),
            ],
                            name=name,
                            src_loc_at=1 + src_loc_at)

    def __init__(self, params):
        super().__init__(params=params)

        self.c = Decoupled(tl.ChannelC,
                           addr_width=32,
                           data_width=self.beat_bytes * 8,
                           size_width=self.in_size_width,
                           source_id_width=self.in_source_id_width)

        self.req = Decoupled(FullRequest, params)
        self.resp = Valid(SinkC.Response, params)

        self.set = Signal(range(self.n_sets))
        self.way = Signal(range(self.n_ways))

        self.port = Decoupled(BankedStore.Port, params, write=True)

        self.rel_pop = Decoupled(PutBufferPop, params)
        self.rel_entry = SinkC.PutBufferEntry(params)

    def elaborate(self, platform):
        m = Module()

        c = self.c

        tag, set, offset = self.parse_addr(c.bits.address)
        first, last, _, beat = tl.Interface.count(m, c.bits, c.fire)
        has_data = tl.Interface.has_data(c.bits)

        resp = (c.bits.opcode == tl.ChannelCOpcode.ProbeAck) | (
            c.bits.opcode == tl.ChannelCOpcode.ProbeAckData)
        resp_reg = Signal()
        is_resp = Mux(c.valid, resp, resp_reg)
        with m.If(c.valid):
            m.d.sync += resp_reg.eq(resp)

        set_reg = Signal.like(self.set)
        with m.If(c.valid):
            m.d.sync += set_reg.eq(set)
        m.d.comb += self.set.eq(Mux(c.valid, set, set_reg))

        m.d.sync += [
            self.port.valid.eq(is_resp & (~first | (c.valid & has_data))),
            self.port.bits.noop.eq(~c.valid),
            self.port.bits.way.eq(self.way),
            self.port.bits.set.eq(self.set),
            self.port.bits.beat.eq(beat),
            self.port.bits.mask.eq(~0),
            self.port.bits.data.eq(c.bits.data),
        ]

        m.d.comb += [
            self.resp.valid.eq(is_resp & c.valid & (first | last)),
            self.resp.bits.last.eq(last),
            self.resp.bits.set.eq(set),
            self.resp.bits.tag.eq(tag),
            self.resp.bits.source.eq(c.bits.source),
            self.resp.bits.param.eq(c.bits.param),
            self.resp.bits.data.eq(has_data),
        ]

        lists = Signal(self.n_rel_lists)
        lists_set = Signal.like(lists)
        lists_clr = Signal.like(lists)
        m.d.sync += lists.eq((lists | lists_set) & ~lists_clr)

        free_idx = Signal(range(self.n_rel_lists))
        free_idx_reg = Signal.like(free_idx)
        for i in reversed(range(self.n_rel_lists)):
            with m.If(~lists[i]):
                m.d.comb += free_idx.eq(i)
        with m.If(first):
            m.d.sync += free_idx_reg.eq(free_idx)

        put = Mux(first, free_idx, free_idx_reg)

        putbuffers = Array(
            SyncFIFO(width=len(self.rel_entry), depth=self.n_rel_entries)
            for _ in range(self.n_rel_lists))
        for i, pb in enumerate(putbuffers):
            setattr(m.submodules, f'putbuffer{i}', pb)

        putbuffer = putbuffers[put]

        req_stall = first & ~self.req.ready
        buf_stall = has_data & ~putbuffer.w_rdy
        list_stall = has_data & first & lists.all()

        m.d.comb += c.ready.eq(
            Mux(resp, 1, ~req_stall & ~buf_stall & ~list_stall))

        m.d.comb += [
            self.req.valid.eq(~is_resp & c.valid & first & ~buf_stall
                              & ~list_stall),
            putbuffer.w_en.eq(~is_resp & c.valid & has_data & ~req_stall
                              & ~list_stall),
        ]
        with m.If(~is_resp & c.valid & first & has_data & ~req_stall
                  & ~buf_stall):
            m.d.comb += lists_set.eq(1 << free_idx)

        m.d.comb += [
            self.req.bits.prio.eq(4),
            self.req.bits.opcode.eq(c.bits.opcode),
            self.req.bits.param.eq(c.bits.param),
            self.req.bits.size.eq(c.bits.size),
            self.req.bits.source.eq(c.bits.source),
            self.req.bits.set.eq(set),
            self.req.bits.tag.eq(tag),
            self.req.bits.offset.eq(offset),
            self.req.bits.put.eq(put),
        ]

        putbuffer_wdata = SinkC.PutBufferEntry(self.params)
        m.d.comb += [
            putbuffer_wdata.data.eq(c.bits.data),
            putbuffer_wdata.corrupt.eq(c.bits.corrupt),
            putbuffer.w_data.eq(putbuffer_wdata),
        ]

        m.d.comb += [
            self.rel_pop.ready.eq(putbuffers[self.rel_pop.bits.index].r_rdy),
            putbuffers[self.rel_pop.bits.index].r_en.eq(self.rel_pop.fire),
            self.rel_entry.eq(putbuffers[self.rel_pop.bits.index].r_data),
        ]

        with m.If(self.rel_pop.fire & self.rel_pop.bits.last):
            m.d.comb += lists_clr.eq(1 << self.rel_pop.bits.index)

        return m


class SinkD(HasL2CacheParams, Elaboratable):

    class Response(HasL2CacheParams, Record):

        def __init__(self, params, name=None, src_loc_at=0):
            HasL2CacheParams.__init__(self, params=params)

            Record.__init__(self, [
                ('last', 1),
                ('opcode', 3),
                ('param', 3),
                ('source', self.out_source_id_width),
                ('sink', 1),
                ('denied', 1),
            ],
                            name=name,
                            src_loc_at=1 + src_loc_at)

    def __init__(self, params):
        super().__init__(params=params)

        self.resp = Valid(SinkD.Response, params)

        self.d = Decoupled(tl.ChannelD,
                           data_width=self.beat_bytes * 8,
                           size_width=bits_for(self.lg_block_bytes),
                           source_id_width=self.out_source_id_width)

        self.port = Decoupled(BankedStore.Port, params, write=True)

        self.source = Signal(self.out_source_id_width)
        self.way = Signal(range(self.n_ways))
        self.set = Signal(self.index_bits)

    def elaborate(self, platform):
        m = Module()

        d = self.d

        source = Signal.like(self.source)
        with m.If(d.valid):
            m.d.sync += source.eq(d.bits.source)
        m.d.comb += self.source.eq(Mux(d.valid, d.bits.source, source))

        first, last, _, beat = tl.Interface.count(m, d.bits, d.fire)

        has_data = tl.Interface.has_data(d.bits)

        m.d.comb += [
            self.resp.valid.eq((first | last) & d.fire),
            d.ready.eq(self.port.ready),
            self.port.valid.eq(d.valid),
        ]

        m.d.comb += [
            self.resp.bits.last.eq(last),
            self.resp.bits.opcode.eq(d.bits.opcode),
            self.resp.bits.param.eq(d.bits.param),
            self.resp.bits.source.eq(d.bits.source),
            self.resp.bits.sink.eq(d.bits.sink),
            self.resp.bits.denied.eq(d.bits.denied),
        ]

        m.d.comb += [
            self.port.bits.noop.eq(~d.valid | ~has_data),
            self.port.bits.way.eq(self.way),
            self.port.bits.set.eq(self.set),
            self.port.bits.beat.eq(beat),
            self.port.bits.mask.eq(~0),
            self.port.bits.data.eq(d.bits.data),
        ]

        return m


class SinkE(HasL2CacheParams, Elaboratable):

    class Response(HasL2CacheParams, Record):

        def __init__(self, params, name=None, src_loc_at=0):
            HasL2CacheParams.__init__(self, params=params)

            Record.__init__(self, [
                ('sink', self.in_sink_id_width),
            ],
                            name=name,
                            src_loc_at=1 + src_loc_at)

    def __init__(self, params):
        super().__init__(params=params)

        self.resp = Valid(SinkE.Response, params)

        self.e = Decoupled(tl.ChannelE, sink_id_width=self.in_sink_id_width)

    def elaborate(self, platform):
        m = Module()

        e = self.e

        m.d.comb += [
            e.ready.eq(1),
            self.resp.valid.eq(e.valid),
            self.resp.bits.sink.eq(e.bits.sink),
        ]

        return m


class SinkX(HasL2CacheParams, Elaboratable):

    class Request(Record):

        def __init__(self, name=None, src_loc_at=0):
            super().__init__([
                ('address', 64, DIR_FANOUT),
            ],
                             name=name,
                             src_loc_at=1 + src_loc_at)

    def __init__(self, params):
        super().__init__(params=params)

        self.x = Decoupled(SinkX.Request)

        self.req = Decoupled(FullRequest, params)

    def elaborate(self, platform):
        m = Module()

        queue = m.submodules.queue = Queue(1, SinkX.Request, flow=False)
        m.d.comb += self.x.connect(queue.enq)
        x = queue.deq

        tag, set, _ = self.parse_addr(x.bits.address)

        m.d.comb += [
            x.ready.eq(self.req.ready),
            self.req.valid.eq(x.valid),
            self.req.bits.prio.eq(1),
            self.req.bits.control.eq(1),
            self.req.bits.size.eq(self.offset_bits),
            self.req.bits.set.eq(set),
            self.req.bits.tag.eq(tag),
        ]

        return m


class ScheduleRequest(HasL2CacheParams):

    def __init__(self, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)
        self.name = name

        self.a = Valid(SourceA.Request, params)
        self.b = Valid(SourceB.Request, params)
        self.c = Valid(SourceC.Request, params)
        self.d = Valid(SourceD.Request, params)
        self.e = Valid(SourceE.Request, params)
        self.x = Valid(SourceX.Request)
        self.dir = Valid(Directory.WriteReq, params)
        self.reload = Signal()

    def eq(self, rhs):
        return [
            self.a.eq(rhs.a),
            self.b.eq(rhs.b),
            self.c.eq(rhs.c),
            self.d.eq(rhs.d),
            self.e.eq(rhs.e),
            self.x.eq(rhs.x),
            self.dir.eq(rhs.dir),
            self.reload.eq(rhs.reload),
        ]


class MSHR(HasL2CacheParams, Elaboratable):

    class Status(HasL2CacheParams, Record):

        def __init__(self, params, name=None, src_loc_at=0):
            HasL2CacheParams.__init__(self, params)

            Record.__init__(self, [
                ('set', self.index_bits),
                ('tag', self.tag_bits),
                ('way', range(self.n_ways)),
                ('block_b', 1),
                ('nest_b', 1),
                ('block_c', 1),
                ('nest_c', 1),
            ],
                            name=name,
                            src_loc_at=1 + src_loc_at)

    def __init__(self, params):
        super().__init__(params=params)

        self.allocate = Valid(AllocateRequest, params)
        self.directory = Valid(Directory.Result, params)
        self.status = Valid(MSHR.Status, params)
        self.schedule = Decoupled(ScheduleRequest, params)
        self.sinkc = Valid(SinkC.Response, params)
        self.sinkd = Valid(SinkD.Response, params)
        self.sinke = Valid(SinkE.Response, params)

    def elaborate(self, platform):
        m = Module()

        request_valid = Signal()
        request = FullRequest(self.params)
        meta_valid = Signal()
        meta = Directory.Result(self.params)

        new_request = FullRequest(self.params)
        new_client_bit = Signal(self.client_bits)
        new_need_t = self.need_t(new_request.opcode, new_request.param)
        new_skip_probe = Mux(self.skip_probe_n(new_request.opcode),
                             new_client_bit, 0)
        m.d.comb += new_request.eq(request)
        with m.If(self.allocate.valid):
            m.d.comb += new_request.eq(self.allocate.bits)
        m.d.comb += new_client_bit.eq(self.client_bit(new_request.source))

        s_rprobe = Signal(reset=1)
        w_rprobeackfirst = Signal(reset=1)
        w_rprobeacklast = Signal(reset=1)
        s_release = Signal(reset=1)
        w_releaseack = Signal(reset=1)
        s_pprobe = Signal(reset=1)
        w_pprobeackfirst = Signal(reset=1)
        w_pprobeacklast = Signal(reset=1)
        w_pprobeack = Signal(reset=1)
        s_acquire = Signal(reset=1)
        w_grant = Signal(reset=1)
        w_grantfirst = Signal(reset=1)
        w_grantlast = Signal(reset=1)
        w_grantack = Signal(reset=1)
        s_execute = Signal(reset=1)
        s_grantack = Signal(reset=1)
        s_writeback = Signal(reset=1)
        s_probeack = Signal(reset=1)
        s_flush = Signal(reset=1)

        m.d.comb += [
            self.status.valid.eq(request_valid),
            self.status.bits.set.eq(request.set),
            self.status.bits.tag.eq(request.tag),
            self.status.bits.way.eq(meta.way),
            self.status.bits.block_b.eq(~meta_valid),
            self.status.bits.nest_b.eq(meta_valid
                                       & w_releaseack & w_rprobeacklast
                                       & w_pprobeacklast
                                       & ~w_grantfirst),
            self.status.bits.block_c.eq(~meta_valid),
            self.status.bits.nest_c.eq(meta_valid
                                       & (~w_rprobeackfirst | ~w_pprobeackfirst
                                          | ~w_grantfirst)),
        ]

        got_t = Signal()
        no_wait = w_rprobeacklast & w_releaseack & w_grantlast & w_pprobeacklast & w_grantack

        m.d.comb += [
            self.schedule.bits.a.valid.eq(~s_acquire & s_release
                                          & s_pprobe),
            self.schedule.bits.b.valid.eq(~s_rprobe | ~s_pprobe),
            self.schedule.bits.c.valid.eq((~s_release & w_rprobeackfirst)
                                          | (~s_probeack & w_pprobeackfirst)),
            self.schedule.bits.d.valid.eq(~s_execute & w_pprobeack & w_grant),
            self.schedule.bits.e.valid.eq(~s_grantack & w_grantfirst),
            self.schedule.bits.x.valid.eq(~s_flush & w_releaseack),
            self.schedule.bits.dir.valid.eq((~s_writeback & no_wait)
                                            | (~s_release & w_rprobeackfirst)),
            self.schedule.bits.reload.eq(no_wait),
            self.schedule.valid.eq(self.schedule.bits.a.valid
                                   | self.schedule.bits.b.valid
                                   | self.schedule.bits.c.valid
                                   | self.schedule.bits.d.valid
                                   | self.schedule.bits.e.valid
                                   | self.schedule.bits.x.valid
                                   | self.schedule.bits.dir.valid),
        ]

        with m.If(self.schedule.ready):
            m.d.sync += s_rprobe.eq(1)

            with m.If(w_rprobeackfirst):
                m.d.sync += [
                    s_release.eq(1),
                    s_pprobe.eq(1),
                ]
            with m.If(s_release & s_pprobe):
                m.d.sync += s_acquire.eq(1)
            with m.If(w_releaseack):
                m.d.sync += s_flush.eq(1)
            with m.If(w_pprobeackfirst):
                m.d.sync += s_probeack.eq(1)
            with m.If(w_grantfirst):
                m.d.sync += s_grantack.eq(1)
            with m.If(w_pprobeack & w_grant):
                m.d.sync += s_execute.eq(1)
            with m.If(no_wait):
                m.d.sync += [
                    s_writeback.eq(1),
                    request_valid.eq(0),
                    meta_valid.eq(0),
                ]

        meta_writeback = Directory.Result(self.params)
        m.d.comb += meta_writeback.eq(meta)

        req_need_t = self.need_t(request.opcode, request.param)
        req_acquire = (request.opcode == tl.ChannelAOpcode.AcquireBlock) | (
            request.opcode == tl.ChannelAOpcode.AcquirePerm)
        req_promote_t = req_acquire & Mux(
            meta.hit,
            (meta.clients == 0) & (meta.state == CacheState.TIP), got_t)

        req_client_bit = Signal(self.client_bits)
        m.d.comb += req_client_bit.eq(self.client_bit(request.source))

        honor_b_to_t = meta.hit & ((meta.clients & req_client_bit) != 0)

        report_param, probe_next_state = CacheState.on_shrink(
            m, meta.state, request.param)

        with m.If(request.prio[2]):
            m.d.comb += [
                meta_writeback.dirty.eq(meta.dirty | request.opcode[0]),
                meta_writeback.state.eq(
                    Mux((request.param != tl.ShrinkReportParam.TtoT) &
                        (meta.state == CacheState.TRUNK), CacheState.TIP,
                        meta.state)),
                meta_writeback.clients.eq(meta.clients & ~Mux(
                    (request.param == tl.ShrinkReportParam.BtoN) |
                    (request.param == tl.ShrinkReportParam.TtoN),
                    req_client_bit, 0)),
                meta_writeback.hit.eq(1),
            ]

        with m.Elif(request.prio[1]):
            m.d.comb += [
                meta_writeback.dirty.eq((request.param == tl.CapParam.toT)
                                        & meta.dirty),
                meta_writeback.state.eq(probe_next_state),
                meta_writeback.clients.eq(
                    Mux(request.param == tl.CapParam.toN, 0, meta.clients)),
                meta_writeback.hit.eq(1),
            ]

        with m.Elif(request.control):
            m.d.comb += [
                meta_writeback.dirty.eq(0),
                meta_writeback.state.eq(CacheState.INVALID),
                meta_writeback.clients.eq(0),
                meta_writeback.hit.eq(0),
            ]

        with m.Else():
            m.d.comb += [
                meta_writeback.dirty.eq((meta.hit & meta.dirty)
                                        | ~request.opcode[2]),
                meta_writeback.tag.eq(request.tag),
                meta_writeback.clients.eq(
                    Mux(req_acquire, self.client_bit(request.source), 0)),
                meta_writeback.hit.eq(1),
            ]

            with m.If(req_need_t):
                m.d.comb += meta_writeback.state.eq(
                    Mux(req_acquire, CacheState.TRUNK, CacheState.TIP))
            with m.Elif(~meta.hit):
                m.d.comb += meta_writeback.state.eq(
                    Mux(got_t,
                        Mux(req_acquire, CacheState.TRUNK, CacheState.TIP),
                        CacheState.BRANCH))
            with m.Else():
                with m.Switch(meta.state):
                    with m.Case(CacheState.INVALID):
                        m.d.comb += meta_writeback.state.eq(CacheState.BRANCH)
                    with m.Case(CacheState.BRANCH):
                        m.d.comb += meta_writeback.state.eq(CacheState.BRANCH)
                    with m.Case(CacheState.TRUNK):
                        m.d.comb += meta_writeback.state.eq(CacheState.TIP)
                    with m.Case(CacheState.TIP):
                        m.d.comb += meta_writeback.state.eq(
                            Mux((meta.clients == 0) & req_acquire,
                                CacheState.TRUNK, CacheState.TIP))

        excluded_clients = Mux(
            meta.hit & request.prio[0] & self.skip_probe_n(request.opcode),
            req_client_bit, 0)

        m.d.comb += [
            # Channel A
            self.schedule.bits.a.bits.tag.eq(request.tag),
            self.schedule.bits.a.bits.set.eq(request.set),
            self.schedule.bits.a.bits.param.eq(tl.GrowParam.NtoB),
            self.schedule.bits.a.bits.block.eq(1),
            self.schedule.bits.a.bits.source.eq(0),
            # Channel B
            self.schedule.bits.b.bits.param.eq(
                Mux(
                    ~s_rprobe, tl.CapParam.toN,
                    Mux(request.prio[1], request.param,
                        Mux(req_need_t, tl.CapParam.toN, tl.CapParam.toB)))),
            self.schedule.bits.b.bits.tag.eq(
                Mux(~s_rprobe, meta.tag, request.tag)),
            self.schedule.bits.b.bits.set.eq(request.set),
            self.schedule.bits.b.bits.clients.eq(meta.clients
                                                 & ~excluded_clients),
            # Channel C
            self.schedule.bits.c.bits.opcode.eq(
                Mux(
                    request.prio[1],
                    Mux(meta.dirty, tl.ChannelCOpcode.ProbeAckData,
                        tl.ChannelCOpcode.ProbeAck),
                    Mux(meta.dirty, tl.ChannelCOpcode.ReleaseData,
                        tl.ChannelCOpcode.Release))),
            self.schedule.bits.c.bits.param.eq(
                Mux(
                    request.prio[1], report_param,
                    Mux(meta.state == CacheState.BRANCH,
                        tl.ShrinkReportParam.BtoN,
                        tl.ShrinkReportParam.TtoN))),
            self.schedule.bits.c.bits.tag.eq(meta.tag),
            self.schedule.bits.c.bits.set.eq(request.set),
            self.schedule.bits.c.bits.way.eq(meta.way),
            self.schedule.bits.c.bits.dirty.eq(meta.dirty),
            # Channel D
            self.schedule.bits.d.bits.eq(request),
            self.schedule.bits.d.bits.param.eq(
                Mux(
                    ~req_acquire, request.param,
                    Mux(
                        request.param == tl.GrowParam.NtoB,
                        Mux(req_promote_t, tl.GrowParam.NtoT,
                            tl.GrowParam.NtoB),
                        Mux(
                            request.param == tl.GrowParam.BtoT,
                            Mux(honor_b_to_t, tl.GrowParam.BtoT,
                                tl.GrowParam.NtoT),
                            Mux(request.param == tl.GrowParam.NtoT,
                                tl.GrowParam.NtoT, request.param))))),
            self.schedule.bits.d.bits.way.eq(meta.way),
            # Directory writeback
            self.schedule.bits.dir.bits.set.eq(request.set),
            self.schedule.bits.dir.bits.way.eq(meta.way),
            self.schedule.bits.dir.bits.dirty.eq(
                Mux(~s_release, 0, meta_writeback.dirty)),
            self.schedule.bits.dir.bits.state.eq(
                Mux(~s_release, CacheState.INVALID, meta_writeback.state)),
            self.schedule.bits.dir.bits.clients.eq(
                Mux(~s_release, 0, meta_writeback.clients)),
            self.schedule.bits.dir.bits.tag.eq(
                Mux(~s_release, 0, meta_writeback.tag)),
        ]

        with m.If(self.sinkc.valid):
            last_probe = 1
            m.d.sync += [
                w_rprobeackfirst.eq(w_rprobeackfirst | last_probe),
                w_rprobeacklast.eq(w_rprobeacklast
                                   | (last_probe
                                      & self.sinkc.bits.last)),
                w_pprobeackfirst.eq(w_pprobeackfirst | last_probe),
                w_pprobeacklast.eq(w_pprobeacklast
                                   | (last_probe
                                      & self.sinkc.bits.last)),
                w_pprobeack.eq(w_pprobeack
                               | (last_probe & (self.sinkc.bits.last |
                                                (request.offset == 0)))),
            ]

            with m.If((meta.state != CacheState.INVALID)
                      & (self.sinkc.bits.tag == meta.tag)
                      & self.sinkc.bits.data):
                m.d.sync += meta.dirty.eq(1)

        with m.If(self.sinkd.valid):
            with m.If((self.sinkd.bits.opcode == tl.ChannelDOpcode.Grant)
                      |
                      (self.sinkd.bits.opcode == tl.ChannelDOpcode.GrantData)):
                m.d.sync += [
                    w_grantfirst.eq(1),
                    w_grantlast.eq(self.sinkd.bits.last),
                    w_grant.eq(self.sinkd.bits.last),
                    got_t.eq(self.sinkd.bits.param == tl.CapParam.toT),
                    self.schedule.bits.e.bits.sink.eq(self.sinkd.bits.sink),
                ]
            with m.Elif(
                    self.sinkd.bits.opcode == tl.ChannelDOpcode.ReleaseAck):
                m.d.sync += w_releaseack.eq(1)
        with m.If(self.sinke.valid):
            m.d.sync += w_grantack.eq(1)

        new_meta = Directory.Result(self.params)
        with m.If(self.allocate.valid & self.allocate.bits.repeat):
            m.d.comb += new_meta.eq(meta_writeback)
        with m.Else():
            m.d.comb += new_meta.eq(self.directory.bits)

        with m.If(self.allocate.valid):
            m.d.sync += [
                request_valid.eq(1),
                request.eq(self.allocate.bits),
            ]

        with m.If(self.directory.valid
                  | (self.allocate.valid & self.allocate.bits.repeat)):
            m.d.sync += [
                meta_valid.eq(1),
                meta.eq(new_meta),
                got_t.eq(0),
            ]

            m.d.sync += [
                s_rprobe.eq(1),
                w_rprobeackfirst.eq(1),
                w_rprobeacklast.eq(1),
                s_release.eq(1),
                w_releaseack.eq(1),
                s_pprobe.eq(1),
                w_pprobeackfirst.eq(1),
                w_pprobeacklast.eq(1),
                w_pprobeack.eq(1),
                s_acquire.eq(1),
                w_grant.eq(1),
                w_grantfirst.eq(1),
                w_grantlast.eq(1),
                w_grantack.eq(1),
                s_execute.eq(1),
                s_grantack.eq(1),
                s_writeback.eq(1),
                s_probeack.eq(1),
                s_flush.eq(1),
            ]

            with m.If(new_request.prio[2]):
                m.d.sync += s_execute.eq(0)

                with m.If(new_request.opcode[0] & ~new_meta.dirty):
                    m.d.sync += s_writeback.eq(0)

                with m.If((new_request.param == tl.ShrinkReportParam.TtoB)
                          & (new_meta.state == CacheState.TRUNK)):
                    m.d.sync += s_writeback.eq(0)

                with m.If(((new_request.param == tl.ShrinkReportParam.TtoN)
                           | (new_request.param == tl.ShrinkReportParam.BtoN))
                          & ((new_meta.clients & new_client_bit) != 0)):
                    m.d.sync += s_writeback.eq(0)

            with m.Elif(new_request.prio[1]):
                m.d.sync += s_probeack.eq(0)

                with m.If(new_meta.hit):
                    with m.If(((new_meta.state >= CacheState.TRUNK)
                               & (new_request.param != tl.CapParam.toT))
                              | ((new_meta.state == CacheState.BRANCH)
                                 & (new_request.param == tl.CapParam.toN))):
                        m.d.sync += s_writeback.eq(0)

                        with m.If(new_meta.clients != 0):
                            m.d.sync += [
                                s_pprobe.eq(0),
                                w_pprobeackfirst.eq(0),
                                w_pprobeacklast.eq(0),
                                w_pprobeack.eq(0),
                            ]

            with m.Elif(new_request.control):
                m.d.sync += s_flush.eq(0)

                with m.If(new_meta.hit):
                    m.d.sync += [
                        s_release.eq(0),
                        w_releaseack.eq(0),
                    ]

                    with m.If(new_meta.clients != 0):
                        m.d.sync += [
                            s_rprobe.eq(0),
                            w_rprobeackfirst.eq(0),
                            w_rprobeacklast.eq(0),
                        ]

            with m.Else():
                m.d.sync += s_execute.eq(0)

                with m.If(~new_meta.hit
                          & (new_meta.state != CacheState.INVALID)):
                    m.d.sync += [
                        s_release.eq(0),
                        w_releaseack.eq(0),
                    ]

                    with m.If(new_meta.clients != 0):
                        m.d.sync += [
                            s_rprobe.eq(0),
                            w_rprobeackfirst.eq(0),
                            w_rprobeacklast.eq(0),
                        ]
                with m.If(~new_meta.hit | (
                    (new_meta.state == CacheState.BRANCH) & new_need_t)):
                    m.d.sync += [
                        s_acquire.eq(0),
                        w_grantfirst.eq(0),
                        w_grantlast.eq(0),
                        w_grant.eq(0),
                        s_writeback.eq(0),
                        s_grantack.eq(0),
                    ]
                with m.If(new_meta.hit
                          & (new_need_t | (new_meta.state == CacheState.TRUNK))
                          & ((new_meta.clients & ~new_skip_probe) != 0)):
                    m.d.sync += [
                        s_pprobe.eq(0),
                        w_pprobeackfirst.eq(0),
                        w_pprobeacklast.eq(0),
                        w_pprobeack.eq(0),
                        s_writeback.eq(0),
                    ]
                with m.If(
                    (new_request.opcode == tl.ChannelAOpcode.AcquireBlock) | (
                        new_request.opcode == tl.ChannelAOpcode.AcquirePerm)):
                    m.d.sync += [
                        s_writeback.eq(0),
                        w_grantack.eq(0),
                    ]
                with m.If(~new_request.opcode[2] & new_meta.hit
                          & ~new_meta.dirty):
                    m.d.sync += s_writeback.eq(0)

        return m


class Scheduler(HasL2CacheParams, Elaboratable):

    def __init__(self, params, in_bus, out_bus):
        super().__init__(params=params)

        self.in_bus = in_bus
        self.out_bus = out_bus

        self.req = Decoupled(SinkX.Request)

    def elaborate(self, platform):
        m = Module()

        source_a = m.submodules.source_a = SourceA(self.params)
        source_b = m.submodules.source_b = SourceB(self.params)
        source_c = m.submodules.source_c = SourceC(self.params)
        source_d = m.submodules.source_d = SourceD(self.params)
        source_e = m.submodules.source_e = SourceE(self.params)
        source_x = m.submodules.source_x = SourceX(self.params)

        m.d.comb += [
            source_a.a.connect(self.out_bus.a),
            source_b.b.connect(self.in_bus.b),
            source_c.c.connect(self.out_bus.c),
            source_d.d.connect(self.in_bus.d),
            source_e.e.connect(self.out_bus.e),
        ]

        sink_a = m.submodules.sink_a = SinkA(self.params)
        sink_b = m.submodules.sink_b = SinkB(self.params)
        sink_c = m.submodules.sink_c = SinkC(self.params)
        sink_d = m.submodules.sink_d = SinkD(self.params)
        sink_e = m.submodules.sink_e = SinkE(self.params)
        sink_x = m.submodules.sink_x = SinkX(self.params)

        m.d.comb += [
            self.in_bus.a.connect(sink_a.a),
            self.out_bus.b.connect(sink_b.b),
            self.in_bus.c.connect(sink_c.c),
            self.out_bus.d.connect(sink_d.d),
            self.in_bus.e.connect(sink_e.e),
            self.req.connect(sink_x.x),
        ]

        directory = m.submodules.directory = Directory(self.params)
        banked_store = m.submodules.banked_store = BankedStore(self.params)
        mshrs = [MSHR(self.params) for _ in range(self.n_mshrs)]
        for i, mshr in enumerate(mshrs):
            setattr(m.submodules, f'mshr{i}', mshr)
        abc_mshrs, bc_mshr, c_mshr = mshrs[:-2], mshrs[-2], mshrs[-1]

        req_queues = Array(
            Queue(self.secondary, BaseRequest, self.params, flow=False)
            for _ in range(self.n_mshrs * 3))
        for i, queue in enumerate(req_queues):
            setattr(m.submodules, f'req_queue{i}', queue)

        mshr_stall_abc = Cat(
            (bc_mshr.status.valid
             & (m.status.bits.set == bc_mshr.status.bits.set))
            | (c_mshr.status.valid
               & (m.status.bits.set == c_mshr.status.bits.set))
            for m in abc_mshrs)
        mshr_stall_bc = (c_mshr.status.valid
                         & (bc_mshr.status.bits.set == c_mshr.status.bits.set))
        mshr_stall_c = Const(0, 1)
        mshr_stall = Signal(self.n_mshrs)
        m.d.comb += mshr_stall.eq(
            Cat(mshr_stall_abc, mshr_stall_bc, mshr_stall_c))

        mshr_request = Signal(self.n_mshrs)
        for i, mshr in enumerate(mshrs):
            m.d.comb += mshr_request[i].eq(mshr.schedule.valid
                                           & ~mshr_stall[i]
                                           & (source_a.req.ready
                                              | ~mshr.schedule.bits.a.valid)
                                           & (source_b.req.ready
                                              | ~mshr.schedule.bits.b.valid)
                                           & (source_c.req.ready
                                              | ~mshr.schedule.bits.c.valid)
                                           & (source_d.req.ready
                                              | ~mshr.schedule.bits.d.valid)
                                           & (source_e.req.ready
                                              | ~mshr.schedule.bits.e.valid)
                                           & (source_x.req.ready
                                              | ~mshr.schedule.bits.x.valid)
                                           & (directory.write.ready
                                              | ~mshr.schedule.bits.dir.valid))

            m.d.comb += [
                mshr.sinkc.valid.eq(sink_c.resp.valid & (
                    sink_c.resp.bits.set == mshr.status.bits.set)),
                mshr.sinkc.bits.eq(sink_c.resp.bits),
                mshr.sinkd.valid.eq(sink_d.resp.valid
                                    & (sink_d.resp.bits.source == i)),
                mshr.sinkd.bits.eq(sink_d.resp.bits),
                mshr.sinke.valid.eq(sink_e.resp.valid
                                    & (sink_e.resp.bits.sink == i)),
                mshr.sinke.bits.eq(sink_e.resp.bits),
            ]

        mshr_grant = Signal(range(self.n_mshrs))
        mshr_grant_mask = Signal(self.n_mshrs)
        mshr_last_grant = Signal.like(mshr_grant)
        m.d.sync += mshr_last_grant.eq(mshr_grant)
        m.d.comb += mshr_grant.eq(mshr_last_grant)
        with m.Switch(mshr_last_grant):
            for i in range(self.n_mshrs):
                with m.Case(i):
                    for pred in reversed(range(i)):
                        with m.If(mshr_request[pred]):
                            m.d.comb += mshr_grant.eq(pred)
                    for succ in reversed(range(i + 1, self.n_mshrs)):
                        with m.If(mshr_request[succ]):
                            m.d.comb += mshr_grant.eq(succ)

        with m.If(mshr_request.any()):
            m.d.comb += mshr_grant_mask.eq(1 << mshr_grant)

        schedule = ScheduleRequest(self.params)
        schedule_tag = Signal(self.tag_bits)
        schedule_set = Signal(self.index_bits)
        for i, mshr in enumerate(mshrs):
            m.d.comb += mshr.schedule.ready.eq(mshr_grant_mask[i])

            with m.If(mshr_grant_mask[i]):
                m.d.comb += [
                    schedule.eq(mshr.schedule.bits),
                    schedule_tag.eq(mshr.status.bits.tag),
                    schedule_set.eq(mshr.status.bits.set),
                ]

        m.d.comb += [
            schedule.a.bits.source.eq(mshr_grant),
            schedule.c.bits.source.eq(
                Mux(schedule.c.bits.opcode[1], mshr_grant, 0)),
            schedule.d.bits.sink.eq(mshr_grant),
        ]

        m.d.comb += [
            source_a.req.valid.eq(schedule.a.valid),
            source_a.req.bits.eq(schedule.a.bits),
            source_b.req.valid.eq(schedule.b.valid),
            source_b.req.bits.eq(schedule.b.bits),
            source_c.req.valid.eq(schedule.c.valid),
            source_c.req.bits.eq(schedule.c.bits),
            source_d.req.valid.eq(schedule.d.valid),
            source_d.req.bits.eq(schedule.d.bits),
            source_e.req.valid.eq(schedule.e.valid),
            source_e.req.bits.eq(schedule.e.bits),
            source_x.req.valid.eq(schedule.x.valid),
            source_x.req.bits.eq(schedule.x.bits),
            directory.write.valid.eq(schedule.dir.valid),
            directory.write.bits.eq(schedule.dir.bits),
        ]

        request = Decoupled(FullRequest, self.params)
        m.d.comb += request.valid.eq(directory.ready
                                     & (sink_a.req.valid | sink_b.req.valid
                                        | sink_c.req.valid
                                        | sink_x.req.valid))
        with m.If(sink_c.req.valid):
            m.d.comb += request.bits.eq(sink_c.req.bits)
        with m.Elif(sink_b.req.valid):
            m.d.comb += request.bits.eq(sink_b.req.bits)
        with m.Elif(sink_x.req.valid):
            m.d.comb += request.bits.eq(sink_x.req.bits)
        with m.Elif(sink_a.req.valid):
            m.d.comb += request.bits.eq(sink_a.req.bits)

        m.d.comb += [
            sink_c.req.ready.eq(directory.ready & request.ready),
            sink_b.req.ready.eq(directory.ready & request.ready
                                & ~sink_c.req.valid),
            sink_x.req.ready.eq(directory.ready & request.ready
                                & ~sink_c.req.valid & ~sink_b.req.valid),
            sink_a.req.ready.eq(directory.ready & request.ready
                                & ~sink_c.req.valid & ~sink_b.req.valid
                                & ~sink_x.req.valid),
        ]

        set_matches = Cat([
            m.status.valid & (m.status.bits.set == request.bits.set)
            for m in mshrs
        ])
        alloc_mshr = ~set_matches.any()
        block_b = Signal()
        nest_b = Signal()
        block_c = Signal()
        nest_c = Signal()
        prio_filter = Cat(Repl(1, self.n_mshrs - 2), ~request.bits.prio[0],
                          request.bits.prio[2])
        lower_matches = set_matches & prio_filter
        request_enq = lower_matches.any(
        ) & ~block_b & ~nest_b & ~block_c & ~nest_c
        for i, mshr in enumerate(mshrs):
            with m.If(set_matches[i] & mshr.status.bits.block_b
                      & request.bits.prio[1]):
                m.d.comb += block_b.eq(1)
            with m.If(set_matches[i] & mshr.status.bits.nest_b
                      & request.bits.prio[1]):
                m.d.comb += nest_b.eq(1)
            with m.If(set_matches[i] & mshr.status.bits.block_c
                      & request.bits.prio[2]):
                m.d.comb += block_c.eq(1)
            with m.If(set_matches[i] & mshr.status.bits.nest_c
                      & request.bits.prio[2]):
                m.d.comb += nest_c.eq(1)

        lower_matches1 = Signal.like(lower_matches)
        m.d.comb += lower_matches1.eq(
            Mux(
                lower_matches[self.n_mshrs - 1], 1 << (self.n_mshrs - 1),
                Mux(lower_matches[self.n_mshrs - 2], 1 << (self.n_mshrs - 2),
                    lower_matches)))

        queue_valids = Signal(self.n_mshrs * 3)
        for i, queue in enumerate(req_queues):
            m.d.comb += queue_valids[i].eq(queue.deq.valid
                                           & mshr_grant_mask[i % self.n_mshrs])

        deq_index = Signal(range(self.n_mshrs * 3))
        for i, mshr in enumerate(mshrs):
            a_deq = queue_valids[i]
            b_deq = queue_valids[self.n_mshrs + i]
            c_deq = queue_valids[self.n_mshrs * 2 + i]
            bypass_matches = lower_matches1[i] & Mux(
                c_deq | request.bits.prio[2], ~c_deq,
                Mux(b_deq | request.bits.prio[1], ~b_deq, ~a_deq))
            may_deq = a_deq | b_deq | c_deq
            may_bypass = request.valid & request_enq & bypass_matches
            will_reload = mshr.schedule.bits.reload & (may_deq | may_bypass)
            mshr_deq_index = Signal(range(self.n_mshrs * 3))

            with m.If(c_deq):
                m.d.comb += mshr_deq_index.eq(self.n_mshrs * 2 + i)
            with m.Elif(b_deq):
                m.d.comb += mshr_deq_index.eq(self.n_mshrs * 1 + i)
            with m.Else():
                m.d.comb += mshr_deq_index.eq(self.n_mshrs * 0 + i)

            with m.If(mshr_grant_mask[i]):
                m.d.comb += deq_index.eq(mshr_deq_index)

            m.d.comb += [
                mshr.allocate.bits.eq(
                    Mux(may_bypass, request.bits,
                        req_queues[mshr_deq_index].deq.bits)),
                mshr.allocate.bits.set.eq(mshr.status.bits.set),
                mshr.allocate.bits.repeat.eq(
                    mshr.allocate.bits.tag == mshr.status.bits.tag),
                mshr.allocate.valid.eq(mshr_grant_mask[i] & will_reload),
            ]

        a_deq = queue_valids[:self.n_mshrs].any()
        b_deq = queue_valids[self.n_mshrs:self.n_mshrs * 2].any()
        c_deq = queue_valids[self.n_mshrs * 2:].any()
        bypass_matches = (mshr_grant_mask & lower_matches1).any() & Mux(
            c_deq | request.bits.prio[2], ~c_deq,
            Mux(b_deq | request.bits.prio[1], ~b_deq, ~a_deq))
        may_deq = a_deq | b_deq | c_deq
        may_bypass = request.valid & request_enq & bypass_matches
        will_reload = schedule.reload & (may_deq | may_bypass)
        will_deq = schedule.reload & may_deq & ~may_bypass

        m.d.comb += req_queues[deq_index].deq.ready.eq(will_deq)

        deq_tag_mismatch = schedule_tag != req_queues[deq_index].deq.bits.tag
        mshr_uses_dir_no_bypass = schedule.reload & may_deq & deq_tag_mismatch
        mshr_uses_dir_deq = will_deq & deq_tag_mismatch
        mshr_uses_dir = will_reload & (schedule_tag != Mux(
            may_bypass, request.bits.tag, req_queues[deq_index].deq.bits.tag))

        mshr_free = Signal()
        req_queue_ready = Signal()
        will_bypass = schedule.reload & bypass_matches
        request_will_alloc = (
            alloc_mshr & ~mshr_uses_dir_no_bypass & mshr_free) | (
                nest_b & ~mshr_uses_dir_no_bypass & ~bc_mshr.status.valid
                & ~c_mshr.status.valid) | (nest_c & ~mshr_uses_dir_no_bypass
                                           & ~c_mshr.status.valid)
        alloc_uses_dir = request.valid & request_will_alloc

        m.d.comb += request.ready.eq(request_will_alloc
                                     | (request_enq
                                        & (will_bypass | req_queue_ready)))

        m.d.comb += [
            directory.read.valid.eq(alloc_uses_dir
                                    | mshr_uses_dir),
            directory.read.bits.set.eq(
                Mux(mshr_uses_dir_deq, schedule_set, request.bits.set)),
            directory.read.bits.tag.eq(
                Mux(mshr_uses_dir_deq, req_queues[deq_index].deq.bits.tag,
                    request.bits.tag)),
        ]

        for i in range(self.n_mshrs):
            with m.If(lower_matches1[i]):
                index = Mux(request.bits.prio[2], self.n_mshrs * 2,
                            Mux(request.bits.prio[1], self.n_mshrs * 1, 0)) + i

                m.d.comb += [
                    req_queues[index].enq.valid.eq(request.valid
                                                   & request_enq
                                                   & ~will_bypass),
                    req_queues[index].enq.bits.eq(request.bits),
                    req_queue_ready.eq(req_queues[index].enq.ready),
                ]

        mshr_write_index = Signal(range(self.n_mshrs))
        for i in reversed(range(self.n_mshrs)):
            with m.If(~mshrs[i].status.valid & prio_filter[i]):
                m.d.comb += [
                    mshr_write_index.eq(i),
                    mshr_free.eq(1),
                ]

        with m.If(request.valid & alloc_mshr & ~mshr_uses_dir_no_bypass):
            with m.Switch(mshr_write_index):
                for i, mshr in enumerate(mshrs):
                    with m.Case(i):
                        with m.If(~mshrs[i].status.valid):
                            m.d.comb += [
                                mshr.allocate.valid.eq(1),
                                mshr.allocate.bits.eq(request.bits),
                                mshr.allocate.bits.repeat.eq(0),
                            ]

        with m.If(request.valid & nest_b & ~bc_mshr.status.valid
                  & ~c_mshr.status.valid
                  & ~mshr_uses_dir_no_bypass):
            m.d.comb += [
                bc_mshr.allocate.valid.eq(1),
                bc_mshr.allocate.bits.eq(request.bits),
                bc_mshr.allocate.bits.repeat.eq(0),
            ]

        with m.If(request.valid & nest_c & ~c_mshr.status.valid
                  & ~mshr_uses_dir_no_bypass):
            m.d.comb += [
                c_mshr.allocate.valid.eq(1),
                c_mshr.allocate.bits.eq(request.bits),
                c_mshr.allocate.bits.repeat.eq(0),
            ]

        dir_target = Signal(range(self.n_mshrs))
        m.d.sync += dir_target.eq(
            Mux(
                mshr_uses_dir, mshr_grant,
                Mux(
                    alloc_uses_dir,
                    Mux(alloc_mshr, mshr_write_index,
                        Mux(nest_b, self.n_mshrs - 2, self.n_mshrs - 1)), 0)))

        for i, mshr in enumerate(mshrs):
            with m.If(dir_target == i):
                m.d.comb += mshr.directory.valid.eq(directory.result.valid)
            m.d.comb += mshr.directory.bits.eq(directory.result.bits)

        for i, mshr in enumerate(mshrs):
            with m.If(mshr.status.valid
                      & (mshr.status.bits.set == sink_c.set)):
                m.d.comb += sink_c.way.eq(mshr.status.bits.way)

            with m.If(sink_d.source == i):
                m.d.comb += [
                    sink_d.set.eq(mshr.status.bits.set),
                    sink_d.way.eq(mshr.status.bits.way),
                ]

        m.d.comb += [
            source_d.rel_pop.connect(sink_c.rel_pop),
            source_d.rel_entry.eq(sink_c.rel_entry),
        ]

        m.d.comb += [
            sink_c.port.connect(banked_store.sinkc_port),
            sink_d.port.connect(banked_store.sinkd_port),
            source_c.port.connect(banked_store.sourcec_port),
            source_d.bs_rport.connect(banked_store.sourced_rport),
            source_d.bs_wport.connect(banked_store.sourced_wport),
        ]

        return m


class L2Cache(HasL2CacheParams, Peripheral, Elaboratable):

    def __init__(self, params, *, name=None, **kwargs):
        Peripheral.__init__(self, name=name, src_loc_at=1)
        HasL2CacheParams.__init__(self, params)

        bank = self.csr_bank()
        self._banks = bank.csr(8, 'r')
        self._ways = bank.csr(8, 'r')
        self._lg_sets = bank.csr(8, 'r')
        self._lg_block_bytes = bank.csr(8, 'r')
        self._flush32 = bank.csr(32, 'rw')

        self._bridge = self.bridge(data_width=32, granularity=8, alignment=2)
        self.bus = self._bridge.bus

        self.in_bus = tl.Interface(data_width=self.beat_bytes * 8,
                                   addr_width=32,
                                   size_width=self.in_size_width,
                                   source_id_width=self.in_source_id_width,
                                   sink_id_width=self.in_sink_id_width,
                                   has_bce=True)

        self.out_bus = tl.Interface(data_width=self.beat_bytes * 8,
                                    addr_width=32,
                                    size_width=bits_for(self.lg_block_bytes),
                                    source_id_width=self.out_source_id_width,
                                    sink_id_width=self.out_sink_id_width,
                                    has_bce=True)

    def elaborate(self, platform):
        m = Module()
        m.submodules.bridge = self._bridge

        m.d.comb += [
            self._banks.r_data.eq(self.n_ways),
            self._ways.r_data.eq(self.n_ways),
            self._lg_sets.r_data.eq(log2_int(self.n_sets)),
            self._lg_block_bytes.r_data.eq(log2_int(self.block_bytes)),
        ]

        flush_in_valid = Signal()
        flush_in_address = Signal(64)
        flush_in_ready = Signal()

        with m.If(flush_in_ready):
            m.d.sync += flush_in_valid.eq(0)

        m.d.comb += self._flush32.r_data.eq(flush_in_valid)
        with m.If(self._flush32.w_stb & ~flush_in_valid):
            m.d.sync += [
                flush_in_valid.eq(1),
                flush_in_address.eq(self._flush32.w_data << 4),
            ]

        scheduler = m.submodules.scheduler = Scheduler(self.params,
                                                       self.in_bus,
                                                       self.out_bus)

        m.d.comb += [
            scheduler.req.valid.eq(flush_in_valid),
            scheduler.req.bits.address.eq(flush_in_address),
            flush_in_ready.eq(scheduler.req.ready),
        ]

        return m
