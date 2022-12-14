from amaranth import *
from amaranth import tracer
from amaranth.hdl.rec import *
from amaranth.utils import log2_int
from enum import IntEnum

from room.consts import *
from room.alu import AMODataGen
from room.types import HasCoreParams, MicroOp
from room.branch import BranchUpdate
from room.utils import Valid, Decoupled, Arbiter, BranchKillableFIFO

from roomsoc.interconnect import wishbone


class StoreGen(Elaboratable):

    def __init__(self, max_size):
        self.log_max_size = log2_int(max_size)

        self.typ = Signal(2)
        self.addr = Signal(self.log_max_size)
        self.data_in = Signal(max_size * 8)

        self.mask = Signal(max_size)
        self.data_out = Signal(max_size * 8)

    def elaborate(self, platform):
        m = Module()

        for i in range(self.log_max_size + 1):
            with m.If(self.typ == i):
                m.d.comb += self.data_out.eq(
                    Repl(self.data_in[:(8 << i)], 1 <<
                         (self.log_max_size - i)))

        mask = 1
        for i in range(self.log_max_size):
            upper = Mux(self.addr[i], mask, 0) | Mux(self.typ >= (i + 1),
                                                     (1 << (1 << i)) - 1, 0)
            lower = Mux(self.addr[i], 0, mask)
            mask = Cat(lower, upper)
        m.d.comb += self.mask.eq(mask)

        return m


class LoadGen(Elaboratable):

    def __init__(self, max_size):
        self.log_max_size = log2_int(max_size)

        self.typ = Signal(2)
        self.signed = Signal()
        self.addr = Signal(self.log_max_size)
        self.data_in = Signal(max_size * 8)

        self.data_out = Signal(max_size * 8)

    def elaborate(self, platform):
        m = Module()

        res = Signal.like(self.data_in)
        m.d.comb += res.eq(self.data_in)

        for i in range(self.log_max_size - 1, -1, -1):
            next_res = Signal.like(res)

            pos = 8 << i

            shifted = Signal(pos)
            m.d.comb += shifted.eq(
                Mux(self.addr[i], res[pos:pos * 2], res[:pos]))

            m.d.comb += next_res.eq(
                Cat(
                    shifted,
                    Mux(
                        self.typ == i,
                        Repl(self.signed & shifted[pos - 1],
                             (2**self.log_max_size) * 8 - pos), res[pos:])))

            res = next_res

        m.d.comb += self.data_out.eq(res)

        return m


class HasDCacheParams(HasCoreParams):

    def __init__(self, params, *args, **kwargs):
        super().__init__(params)

        dcache_params = params['dcache_params']
        self.n_sets = dcache_params['n_sets']
        self.n_ways = dcache_params['n_ways']
        self.block_bytes = dcache_params['block_bytes']

        self.block_off_bits = log2_int(self.block_bytes)
        self.index_bits = log2_int(self.n_sets)
        self.untag_bits = self.block_off_bits + self.index_bits
        self.tag_bits = 32 - self.untag_bits

        self.row_bits = dcache_params['row_bits']
        self.row_off_bits = log2_int(self.row_bits // 8)
        self.refill_cycles = (self.block_bytes * 8) // self.row_bits

        self.word_bits = self.xlen
        self.word_off_bits = log2_int(self.word_bits // 8)

        self.n_mshrs = dcache_params['n_mshrs']
        self.n_iomshrs = dcache_params['n_iomshrs']
        self.sdq_size = dcache_params['sdq_size']
        self.rpq_size = dcache_params['rpq_size']
        self.n_data_banks = dcache_params['n_data_banks']

    def addr_block_offset(self, addr):
        return addr[:self.block_off_bits]

    def addr_index(self, addr):
        return addr[self.block_off_bits:self.untag_bits]

    def addr_tag(self, addr):
        return addr[self.untag_bits:self.untag_bits + self.tag_bits]


class DCacheReqType(IntEnum):
    LSU = 1
    WRITEBACK = 2
    MSHR_META = 3
    REPLAY = 4


class DCacheReq(HasCoreParams):

    def __init__(self, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)
        self.name = name

        self.uop = MicroOp(params, name=f'{name}_uop')

        self.addr = Signal(self.vaddr_bits, name=f'{name}_addr')
        self.data = Signal(self.xlen, name=f'{name}_data')

    def eq(self, rhs):
        attrs = ['uop', 'addr', 'data']
        return [getattr(self, a).eq(getattr(rhs, a)) for a in attrs]


class DCacheResp(HasCoreParams):

    def __init__(self, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.uop = MicroOp(params, name=f'{name}_uop')

        self.data = Signal(self.xlen, name=f'{name}_data')

    def eq(self, rhs):
        attrs = ['uop', 'data']
        return [getattr(self, a).eq(getattr(rhs, a)) for a in attrs]


#
# Cache metadata
#


class CacheState(IntEnum):
    NOTHING = 0
    CLEAN = 1
    DIRTY = 3

    @staticmethod
    def is_valid(state):
        return state != CacheState.NOTHING

    @staticmethod
    def on_access(m, state, cmd):
        cmd_is_write = MemoryCommand.is_write(cmd)

        is_hit = Signal()
        next_state = Signal(CacheState)

        with m.Switch(state):
            with m.Case(CacheState.NOTHING):
                pass

            with m.Case(CacheState.CLEAN):
                m.d.comb += [
                    is_hit.eq(1),
                    next_state.eq(
                        Mux(cmd_is_write, CacheState.DIRTY, CacheState.CLEAN)),
                ]
            with m.Case(CacheState.DIRTY):
                m.d.comb += [
                    is_hit.eq(1),
                    next_state.eq(CacheState.DIRTY),
                ]

        return is_hit, next_state

    @staticmethod
    def on_sec_access(m, state, cmd_pri, cmd_sec):
        hit_pri, next_state_pri = CacheState.on_access(m, state, cmd_pri)
        hit_sec, next_state_sec = CacheState.on_access(m, state, cmd_sec)

        hit_again = hit_pri & hit_sec

        sec_is_write = MemoryCommand.is_write(cmd_sec)
        dirtier_state = Mux(sec_is_write, next_state_sec, next_state_pri)
        dirtier_cmd = Mux(sec_is_write, cmd_sec, cmd_pri)

        return hit_again, dirtier_state, dirtier_cmd

    @staticmethod
    def on_cache_control(m, state, cmd):
        is_dirty = Signal()

        m.d.comb += is_dirty.eq(state == CacheState.DIRTY)

        return is_dirty


class Metadata(HasDCacheParams, Record):

    def __init__(self, params, name=None, src_loc_at=0):
        HasDCacheParams.__init__(self, params)

        Record.__init__(self, [
            ('tag', self.tag_bits, DIR_FANOUT),
            ('state', CacheState, DIR_FANOUT),
        ],
                        name=name,
                        src_loc_at=1 + src_loc_at)


class MetaReadReq(HasDCacheParams, Record):

    def __init__(self, params, name=None, src_loc_at=0):
        HasDCacheParams.__init__(self, params)

        Record.__init__(self, [
            ('idx', self.index_bits, DIR_FANOUT),
            ('way_en', self.n_ways, DIR_FANOUT),
            ('tag', self.tag_bits, DIR_FANOUT),
        ],
                        name=name,
                        src_loc_at=1 + src_loc_at)


class MetaWriteReq(HasDCacheParams, Record):

    def __init__(self, params, name=None, src_loc_at=0):
        HasDCacheParams.__init__(self, params)

        Record.__init__(self, [
            ('idx', self.index_bits, DIR_FANOUT),
            ('way_en', self.n_ways, DIR_FANOUT),
            ('tag', self.tag_bits, DIR_FANOUT),
            ('state', CacheState, DIR_FANOUT),
        ],
                        name=name,
                        src_loc_at=1 + src_loc_at)


class MetadataArray(HasDCacheParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.read = Decoupled(MetaReadReq, params)
        self.write = Decoupled(MetaWriteReq, params)
        self.resp = [
            Metadata(params, name=f'resp{i}') for i in range(self.n_ways)
        ]

    def elaborate(self, platform):
        m = Module()

        meta_bits = len(self.resp[0])

        tag_mem = Memory(width=self.n_ways * meta_bits, depth=self.n_sets)

        tag_read = m.submodules.tag_read = tag_mem.read_port()
        m.d.comb += tag_read.addr.eq(self.read.bits.idx)

        for i in range(self.n_ways):
            m.d.comb += self.resp[i].eq(tag_read.data[i * meta_bits:(i + 1) *
                                                      meta_bits])

        w_data = Metadata(self.params)
        m.d.comb += [
            w_data.state.eq(self.write.bits.state),
            w_data.tag.eq(self.write.bits.tag),
        ]

        tag_write = m.submodules.tag_write = tag_mem.write_port(
            granularity=meta_bits)
        m.d.comb += [
            tag_write.addr.eq(self.write.bits.idx),
            tag_write.data.eq(Repl(w_data, self.n_ways)),
            tag_write.en.eq(self.write.bits.way_en
                            & Repl(self.write.valid, self.n_ways)),
        ]

        m.d.comb += [
            self.read.ready.eq(~self.write.valid),
            self.write.ready.eq(1),
        ]

        return m


#
# Cache data
#


class DataReadReq(HasDCacheParams, Record):

    def __init__(self, params, name=None, src_loc_at=0):
        HasDCacheParams.__init__(self, params)

        Record.__init__(self, [
            ('way_en', self.n_ways, DIR_FANOUT),
            ('addr', self.untag_bits, DIR_FANOUT),
        ],
                        name=name,
                        src_loc_at=1 + src_loc_at)


class DataWriteReq(HasDCacheParams, Record):

    def __init__(self, params, name=None, src_loc_at=0):
        HasDCacheParams.__init__(self, params)

        Record.__init__(self, [
            ('way_en', self.n_ways, DIR_FANOUT),
            ('addr', self.untag_bits, DIR_FANOUT),
            ('data', self.row_bits, DIR_FANOUT),
        ],
                        name=name,
                        src_loc_at=1 + src_loc_at)


class BaseDataArray(HasDCacheParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.read = [
            Valid(DataReadReq, self.params, name=f'read{i}')
            for i in range(self.mem_width)
        ]
        self.write = Valid(DataWriteReq, self.params)

        self.resp = [[
            Signal(self.row_bits, name=f'resp{i}_way{w}')
            for w in range(self.n_ways)
        ] for i in range(self.mem_width)]

        self.nack = Signal(self.mem_width)

    def elaborate(self, platform):
        m = Module()

        return m


class DuplicatedDataArray(BaseDataArray):

    def __init__(self, params):
        super().__init__(params)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        for i in range(self.mem_width):
            for w in range(self.n_ways):
                mem = Memory(width=self.row_bits,
                             depth=self.n_sets * self.refill_cycles)

                mem_read = mem.read_port()
                setattr(m.submodules, f'mem_read{i}_{w}', mem_read)

                m.d.comb += mem_read.addr.eq(
                    self.read[i].bits.addr >> self.row_off_bits)

                mem_write = mem.write_port()
                setattr(m.submodules, f'mem_write{i}_{w}', mem_write)

                m.d.comb += [
                    mem_write.addr.eq(
                        self.write.bits.addr >> self.row_off_bits),
                    mem_write.data.eq(self.write.bits.data),
                    mem_write.en.eq(self.write.bits.way_en[w]
                                    & self.write.valid),
                ]

                m.d.sync += self.resp[i][w].eq(mem_read.data)

        return m


#
# Writeback unit
#


class WritebackReq(HasDCacheParams, Record):

    def __init__(self, params, name=None, src_loc_at=0):
        HasDCacheParams.__init__(self, params)

        Record.__init__(self, [
            ('tag', self.tag_bits, DIR_FANOUT),
            ('idx', self.index_bits, DIR_FANOUT),
            ('way_en', self.n_ways, DIR_FANOUT),
        ],
                        name=name,
                        src_loc_at=1 + src_loc_at)


class WritebackUnit(HasDCacheParams, Elaboratable):

    def __init__(self, params, dbus):
        super().__init__(params)

        self.dbus = dbus

        self.req = Decoupled(WritebackReq, params)
        self.resp = Signal()

        self.idx = Valid(Signal, self.index_bits)

        self.meta_read = Decoupled(MetaReadReq, params)
        self.data_read = Decoupled(DataReadReq, params)
        self.data_resp = Signal(self.row_bits)

    def elaborate(self, platform):
        m = Module()

        req = WritebackReq(self.params)

        state_invalid = Signal()
        m.d.comb += [
            self.idx.valid.eq(~state_invalid),
            self.idx.bits.eq(req.idx),
        ]

        wb_buffer = Array(
            Signal(self.row_bits, name=f'wb_buffer{i}')
            for i in range(self.refill_cycles))
        wb_counter = Signal(range(self.refill_cycles))

        with m.FSM():
            with m.State('INVALID'):
                m.d.comb += [
                    state_invalid.eq(1),
                    self.req.ready.eq(1),
                ]

                with m.If(self.req.fire):
                    m.d.sync += [
                        req.eq(self.req.bits),
                        wb_counter.eq(0),
                    ]

                    m.next = 'FILL_BUFFER'

            with m.State('FILL_BUFFER'):
                m.d.comb += [
                    self.meta_read.valid.eq(1),
                    self.meta_read.bits.idx.eq(req.idx),
                    self.meta_read.bits.tag.eq(req.tag),
                    self.data_read.valid.eq(1),
                    self.data_read.bits.way_en.eq(req.way_en),
                    self.data_read.bits.addr.eq(
                        Cat(Repl(0, self.row_off_bits), wb_counter, req.idx)),
                ]

                with m.If(self.data_read.fire & self.meta_read.fire):
                    m.next = 'FILL_RESP_1'

            with m.State('FILL_RESP_1'):
                m.next = 'FILL_RESP_2'

            with m.State('FILL_RESP_2'):
                m.d.sync += wb_buffer[wb_counter].eq(self.data_resp)

                with m.If(wb_counter == self.refill_cycles - 1):
                    m.d.comb += self.resp.eq(1)
                    m.d.sync += wb_counter.eq(0)
                    m.next = 'WRITE_BACK'
                with m.Else():
                    m.d.sync += wb_counter.eq(wb_counter + 1)
                    m.next = 'FILL_BUFFER'

            with m.State('WRITE_BACK'):
                m.d.comb += [
                    self.dbus.adr.eq(Cat(wb_counter, req.idx, req.tag)),
                    self.dbus.cyc.eq(1),
                    self.dbus.stb.eq(1),
                    self.dbus.sel.eq(~0),
                    self.dbus.dat_w.eq(wb_buffer[wb_counter]),
                    self.dbus.we.eq(1),
                ]

                with m.If(self.dbus.ack):
                    m.d.sync += wb_counter.eq(wb_counter + 1)

                    with m.If(wb_counter == (self.refill_cycles - 1)):
                        m.next = 'INVALID'

        return m


#
# MSHR
#


class MSHRReq(HasDCacheParams, DCacheReq):

    def __init__(self, params, name=None, src_loc_at=0):
        super().__init__(params=params, name=name, src_loc_at=src_loc_at + 1)

        self.tag_match = Signal(name=f'{self.name}_tag_match')
        self.old_meta = Metadata(params, name=f'{self.name}_old_meta')
        self.way_en = Signal(self.n_ways, name=f'{self.name}_way_en')

        self.sdq_id = Signal(range(self.sdq_size), name=f'{self.name}_sdq_id')

    def eq(self, rhs):
        return super().eq(rhs) + [
            getattr(self, a).eq(getattr(rhs, a))
            for a in ['tag_match', 'old_meta', 'way_en', 'sdq_id']
        ]


class LineBufferReadReq(HasDCacheParams, Record):

    def __init__(self, params, name=None, src_loc_at=0):
        HasDCacheParams.__init__(self, params)

        Record.__init__(self, [
            ('id', Shape.cast(range(self.n_mshrs)).width, DIR_FANOUT),
            ('offset', self.refill_cycles, DIR_FANOUT),
        ],
                        name=name,
                        src_loc_at=1 + src_loc_at)

    def addr(self):
        return Cat(self.offset, self.id)


class LineBufferWriteReq(HasDCacheParams, Record):

    def __init__(self, params, name=None, src_loc_at=0):
        HasDCacheParams.__init__(self, params)

        Record.__init__(self, [
            ('id', Shape.cast(range(self.n_mshrs)).width, DIR_FANOUT),
            ('offset', self.refill_cycles, DIR_FANOUT),
            ('data', self.row_bits, DIR_FANOUT),
        ],
                        name=name,
                        src_loc_at=1 + src_loc_at)

    def addr(self):
        return Cat(self.offset, self.id)


class MSHR(HasDCacheParams, Elaboratable):

    def __init__(self, id, params, dbus):
        super().__init__(params)

        self.id = id

        self.dbus = dbus

        self.req = MSHRReq(params)
        self.req_pri_valid = Signal()
        self.req_pri_ready = Signal()
        self.req_sec_valid = Signal()
        self.req_sec_ready = Signal()

        self.idx = Valid(Signal, self.index_bits)
        self.way = Valid(Signal, self.n_ways)
        self.tag = Valid(Signal, self.tag_bits)

        self.meta_read = Decoupled(MetaReadReq, self.params)
        self.meta_resp = Valid(Metadata, self.params)
        self.meta_write = Decoupled(MetaWriteReq, self.params)

        self.refill = Decoupled(DataWriteReq, self.params)
        self.replay = Decoupled(MSHRReq, self.params)

        self.lb_read = Decoupled(LineBufferReadReq, self.params)
        self.lb_resp = Signal(self.dbus.data_width)
        self.lb_write = Decoupled(LineBufferWriteReq, self.params)

        self.wb_req = Decoupled(WritebackReq, self.params)
        self.wb_resp = Signal()

        self.resp = Decoupled(DCacheResp, params)

        self.br_update = BranchUpdate(params)
        self.exception = Signal()

    def elaborate(self, platform):
        m = Module()

        req = MSHRReq(self.params)
        req_idx = self.addr_index(req.addr)
        req_tag = self.addr_tag(req.addr)

        state_invalid = Signal()

        m.d.comb += [
            self.idx.valid.eq(~state_invalid),
            self.idx.bits.eq(req_idx),
            self.way.valid.eq(~state_invalid),
            self.way.bits.eq(req.way_en),
            self.tag.valid.eq(~state_invalid),
            self.tag.bits.eq(req_tag),
        ]

        rpq = m.submodules.rpq = BranchKillableFIFO(
            self.rpq_size,
            self.params,
            MSHRReq,
            self.params,
            flow=False,
            flush_fn=lambda req: req.uop.uses_ldq)

        m.d.comb += [
            rpq.br_update.eq(self.br_update),
            rpq.flush.eq(self.exception),
            rpq.w_en.eq((self.req_pri_valid & self.req_pri_ready)
                        | (self.req_sec_valid & self.req_sec_ready)),
            rpq.w_data.eq(self.req),
            rpq.w_br_mask.eq(self.req.uop.br_mask),
        ]

        refill_done = Signal()
        refill_counter = Signal(range(self.refill_cycles))

        lb_write_data = Signal(self.dbus.data_width)

        load_gen = m.submodules.load_gen = LoadGen(
            max_size=self.dbus.data_width // 8)

        commit_line = Signal()
        with m.If(refill_done):
            m.d.sync += commit_line.eq(0)

        new_state = Signal(CacheState)

        is_hit_again, dirtier_state, dirtier_cmd = CacheState.on_sec_access(
            m, new_state, req.uop.mem_cmd, self.req.uop.mem_cmd)
        block_sec_req = Signal()
        sec_ready = ~block_sec_req

        m.d.comb += self.req_sec_ready.eq(sec_ready & rpq.w_rdy)

        with m.If(self.req_sec_valid & self.req_sec_ready):
            m.d.sync += req.uop.mem_cmd.eq(dirtier_cmd)
            with m.If(is_hit_again):
                m.d.sync += new_state.eq(dirtier_state)

        with m.FSM():
            with m.State('INVALID'):
                m.d.comb += [
                    state_invalid.eq(1),
                    block_sec_req.eq(1),
                    self.req_pri_ready.eq(1),
                ]

                with m.If(self.req_pri_valid & self.req_pri_ready):
                    m.d.sync += req.eq(self.req)

                    with m.If(self.req.tag_match):
                        is_hit, next_state = CacheState.on_access(
                            m, self.req.old_meta.state, self.req.uop.mem_cmd)

                        with m.If(is_hit):
                            m.d.sync += new_state.eq(next_state)
                            m.next = 'DRAIN_REPLAY'

                    with m.Else():
                        m.d.sync += new_state.eq(CacheState.CLEAN)
                        m.next = 'REFILL'

            with m.State('REFILL'):
                m.d.comb += [
                    self.dbus.adr.eq(Cat(refill_counter, req_idx, req_tag)),
                    self.dbus.cyc.eq(1),
                    self.dbus.stb.eq(1),
                    self.dbus.sel.eq(~0),
                ]

                with m.If(self.dbus.ack):

                    m.d.comb += [
                        self.lb_write.valid.eq(1),
                        self.lb_write.bits.id.eq(self.id),
                        self.lb_write.bits.offset.eq(refill_counter),
                        self.lb_write.bits.data.eq(self.dbus.dat_r),
                    ]

                    with m.If(~self.lb_write.ready):
                        m.d.sync += lb_write_data.eq(self.dbus.dat_r)

                        m.next = 'WRITE_LB'

                    with m.Elif(refill_counter == (self.refill_cycles - 1)):
                        m.d.sync += refill_counter.eq(0)
                        m.d.comb += refill_done.eq(1)
                        m.next = 'DRAIN_LOAD'

                    with m.Else():
                        m.d.sync += refill_counter.eq(refill_counter + 1)

            with m.State('WRITE_LB'):
                m.d.comb += [
                    self.lb_write.valid.eq(1),
                    self.lb_write.bits.id.eq(self.id),
                    self.lb_write.bits.offset.eq(refill_counter),
                    self.lb_write.bits.data.eq(lb_write_data),
                ]

                with m.If(self.lb_write.ready):
                    with m.If(refill_counter == (self.refill_cycles - 1)):
                        m.d.sync += refill_counter.eq(0)
                        m.d.comb += refill_done.eq(1)
                        m.next = 'DRAIN_LOAD'

                    with m.Else():
                        m.d.sync += refill_counter.eq(refill_counter + 1)
                        m.next = 'REFILL'

            with m.State('DRAIN_LOAD'):
                drain_load = MemoryCommand.is_read(
                    rpq.r_data.uop.mem_cmd) & ~MemoryCommand.is_write(
                        rpq.r_data.uop.mem_cmd)

                rp_addr = Cat(self.addr_block_offset(rpq.r_data.addr), req_idx,
                              req_tag)

                m.d.comb += [
                    load_gen.typ.eq(rpq.r_data.uop.mem_size),
                    load_gen.signed.eq(rpq.r_data.uop.mem_signed),
                    load_gen.addr.eq(rp_addr),
                    load_gen.data_in.eq(self.lb_resp),
                ]

                m.d.comb += [
                    rpq.r_en.eq(self.resp.ready & self.lb_read.ready
                                & drain_load),
                    self.lb_read.valid.eq(rpq.r_rdy & drain_load),
                    self.lb_read.bits.id.eq(self.id),
                    self.lb_read.bits.offset.eq(
                        rpq.r_data.addr[self.row_off_bits:]),
                ]

                m.d.comb += [
                    self.resp.valid.eq(rpq.r_rdy & self.lb_read.fire
                                       & drain_load),
                    self.resp.bits.uop.eq(rpq.r_data.uop),
                    self.resp.bits.uop.br_mask.eq(rpq.r_br_mask),
                    self.resp.bits.data.eq(load_gen.data_out),
                ]

                with m.If(rpq.r_en & rpq.r_rdy):
                    m.d.sync += commit_line.eq(1)
                with m.Elif(rpq.empty & ~commit_line):
                    with m.If(~(rpq.w_en & rpq.w_rdy)):
                        m.next = 'MEM_FINISH'
                with m.Elif(rpq.empty | (rpq.r_rdy & ~drain_load)):
                    m.next = 'META_READ'

            with m.State('META_READ'):
                m.d.comb += [
                    self.meta_read.valid.eq(1),
                    self.meta_read.bits.idx.eq(req_idx),
                    self.meta_read.bits.tag.eq(req_tag),
                    self.meta_read.bits.way_en.eq(req.way_en),
                ]

                with m.If(self.meta_read.fire):
                    m.next = 'META_RESP_1'

            with m.State('META_RESP_1'):
                m.next = 'META_RESP_2'

            with m.State('META_RESP_2'):
                needs_wb = CacheState.on_cache_control(
                    m, self.meta_resp.bits.state, MemoryCommand.WRITE)

                with m.If(~self.meta_resp.valid):
                    m.next = 'META_READ'
                with m.Elif(needs_wb):
                    m.next = 'WB_REQ'
                with m.Else():
                    m.next = 'COMMIT_LINE'

            with m.State('WB_REQ'):
                m.d.comb += [
                    self.wb_req.valid.eq(1),
                    self.wb_req.bits.tag.eq(req.old_meta.tag),
                    self.wb_req.bits.idx.eq(req_idx),
                    self.wb_req.bits.way_en.eq(req.way_en),
                ]

                with m.If(self.wb_req.fire):
                    m.next = 'WB_RESP'

            with m.State('WB_RESP'):
                with m.If(self.wb_resp):
                    m.next = 'COMMIT_LINE'

            with m.State('COMMIT_LINE'):
                m.d.comb += [
                    self.lb_read.valid.eq(1),
                    self.lb_read.bits.id.eq(self.id),
                    self.lb_read.bits.offset.eq(refill_counter),
                    self.refill.valid.eq(self.lb_read.fire),
                    self.refill.bits.addr.eq(
                        Cat(Repl(0, self.row_off_bits), refill_counter,
                            req.addr[self.block_off_bits:])),
                    self.refill.bits.way_en.eq(req.way_en),
                    self.refill.bits.data.eq(self.lb_resp),
                ]

                with m.If(self.refill.fire):
                    m.d.sync += refill_counter.eq(refill_counter + 1)
                    with m.If(refill_counter == (self.refill_cycles - 1)):
                        m.d.sync += refill_counter.eq(0)
                        m.next = 'DRAIN_REPLAY'

            with m.State('DRAIN_REPLAY'):
                m.d.comb += [
                    self.replay.valid.eq(rpq.r_rdy),
                    self.replay.bits.eq(rpq.r_data),
                    self.replay.bits.uop.br_mask.eq(rpq.r_br_mask),
                    self.replay.bits.way_en.eq(req.way_en),
                    self.replay.bits.addr.eq(
                        Cat(self.addr_block_offset(rpq.r_data.addr), req_idx,
                            req_tag)),
                    rpq.r_en.eq(self.replay.ready),
                ]

                with m.If(self.replay.fire
                          & MemoryCommand.is_write(rpq.r_data.uop.mem_cmd)):
                    _, next_state = CacheState.on_access(
                        m, new_state, rpq.r_data.uop.mem_cmd)
                    m.d.sync += new_state.eq(next_state)

                with m.If(rpq.empty & ~rpq.w_en):
                    m.next = 'META_WRITE'

            with m.State('META_WRITE'):
                m.d.comb += [
                    block_sec_req.eq(1),
                    self.meta_write.valid.eq(1),
                    self.meta_write.bits.idx.eq(req_idx),
                    self.meta_write.bits.way_en.eq(req.way_en),
                    self.meta_write.bits.tag.eq(req_tag),
                    self.meta_write.bits.state.eq(new_state),
                ]

                with m.If(self.meta_write.fire):
                    m.next = 'MEM_FINISH'

            with m.State('MEM_FINISH'):
                m.d.comb += block_sec_req.eq(1)
                m.next = 'INVALID'

        return m


class IOMSHR(HasDCacheParams, Elaboratable):

    def __init__(self, id, params, dbus):
        super().__init__(params)

        self.id = id
        self.dbus = dbus

        self.req = Decoupled(DCacheReq, params)
        self.resp = Decoupled(DCacheResp, params)

    def elaborate(self, platform):
        m = Module()

        granularity_bits = log2_int(self.xlen // 8)

        req = DCacheReq(self.params)

        resp_data = Signal.like(self.resp.bits.data)

        load_gen = m.submodules.load_gen = LoadGen(max_size=self.xlen // 8)
        m.d.comb += [
            load_gen.typ.eq(req.uop.mem_size),
            load_gen.signed.eq(req.uop.mem_signed),
            load_gen.addr.eq(req.addr),
            load_gen.data_in.eq(resp_data),
        ]

        store_gen = m.submodules.store_gen = StoreGen(max_size=self.xlen // 8)
        m.d.comb += [
            store_gen.typ.eq(req.uop.mem_size),
            store_gen.addr.eq(req.addr),
            store_gen.data_in.eq(req.data),
        ]

        send_resp = req.uop.mem_cmd == MemoryCommand.READ

        with m.FSM():
            with m.State('IDLE'):
                m.d.comb += self.req.ready.eq(1)

                with m.If(self.req.fire):
                    m.d.sync += req.eq(self.req.bits)
                    m.next = 'MEM_ACCESS'

            with m.State('MEM_ACCESS'):
                m.d.comb += [
                    self.dbus.adr.eq(req.addr[granularity_bits:]),
                    self.dbus.stb.eq(1),
                    self.dbus.dat_w.eq(store_gen.data_out),
                    self.dbus.cyc.eq(1),
                    self.dbus.sel.eq(store_gen.mask),
                    self.dbus.we.eq(req.uop.mem_cmd == MemoryCommand.WRITE),
                ]

                with m.If(self.dbus.cyc & self.dbus.stb & self.dbus.ack):
                    with m.If(send_resp):
                        m.d.sync += resp_data.eq(self.dbus.dat_r)

                    m.next = 'RESP'

            with m.State('RESP'):
                m.d.comb += self.resp.valid.eq(send_resp)

                with m.If(~send_resp | self.resp.fire):
                    m.next = 'IDLE'

        m.d.comb += [
            self.resp.bits.uop.eq(req.uop),
            self.resp.bits.data.eq(load_gen.data_out),
        ]

        return m


class MSHRFile(HasDCacheParams, Elaboratable):

    def __init__(self, params, dbus):
        super().__init__(params)

        self.dbus = dbus

        self.req = [
            Decoupled(MSHRReq, params, name=f'req{i}')
            for i in range(self.mem_width)
        ]

        self.resp = Decoupled(DCacheResp, self.params)
        self.secondary_miss = Signal(self.mem_width)
        self.block_hit = Signal(self.mem_width)

        self.meta_read = Decoupled(MetaReadReq, self.params)
        self.meta_resp = Valid(Metadata, self.params)
        self.meta_write = Decoupled(MetaWriteReq, self.params)

        self.refill = Decoupled(DataWriteReq, self.params)
        self.replay = Decoupled(MSHRReq, self.params)

        self.wb_req = Decoupled(WritebackReq, self.params)
        self.wb_resp = Signal()

        self.br_update = BranchUpdate(params)
        self.exception = Signal()

    def elaborate(self, platform):
        m = Module()

        req_idx = Signal(range(self.mem_width))
        req = MSHRReq(self.params)
        req_valid = Signal()

        for w in reversed(range(self.mem_width)):
            with m.If(self.req[w].valid):
                m.d.comb += [
                    req_idx.eq(w),
                    req.eq(self.req[w].bits),
                    req_valid.eq(1),
                ]

        req_uncacheable = Signal()
        for origin, size in self.io_regions.items():
            with m.If((req.addr >= origin) & (req.addr < (origin + size))):
                m.d.comb += req_uncacheable.eq(1)

        #
        # Store data queue
        #

        sdq_valid = Signal(self.sdq_size)
        sdq_alloc_idx = Signal(range(self.sdq_size))
        sdq_ready = Signal()
        sdq = Memory(width=self.word_bits, depth=self.sdq_size)

        sdq_write = m.submodules.sdq_write = sdq.write_port()
        m.d.comb += [
            sdq_write.addr.eq(sdq_alloc_idx),
            sdq_write.data.eq(req.data),
            sdq_write.en.eq((Cat(self.req[w].fire
                                 for w in range(self.mem_width)) != 0)
                            & ~req_uncacheable
                            & MemoryCommand.is_write(req.uop.mem_cmd)),
        ]

        for i in reversed(range(self.sdq_size)):
            with m.If(~sdq_valid[i]):
                m.d.comb += [
                    sdq_alloc_idx.eq(i),
                    sdq_ready.eq(1),
                ]

        #
        # Line buffer
        #

        lb_read_arb = m.submodules.lb_read_arb = Arbiter(
            self.n_mshrs, LineBufferReadReq, self.params)
        lb_write_arb = m.submodules.lb_write_arb = Arbiter(
            self.n_mshrs, LineBufferWriteReq, self.params)

        lb_mem = Memory(width=self.dbus.data_width,
                        depth=self.n_mshrs * self.refill_cycles)

        lb_read = m.submodules.lb_read = lb_mem.read_port(domain='comb',
                                                          transparent=True)
        m.d.comb += [
            lb_read.addr.eq(lb_read_arb.out.bits.addr()),
            lb_read_arb.out.ready.eq(1),
        ]

        lb_write = m.submodules.lb_write = lb_mem.write_port()
        m.d.comb += [
            lb_write.addr.eq(lb_write_arb.out.bits.addr()),
            lb_write.data.eq(lb_write_arb.out.bits.data),
            lb_write.en.eq(lb_write_arb.out.fire),
            lb_write_arb.out.ready.eq(1),
        ]

        dbus_arbiter = m.submodules.dbus_arbiter = wishbone.Arbiter(
            data_width=self.dbus.data_width,
            addr_width=self.dbus.addr_width,
            granularity=self.dbus.granularity)

        meta_read_arb = m.submodules.meta_read_arb = Arbiter(
            self.n_mshrs, MetaReadReq, self.params)
        meta_write_arb = m.submodules.meta_write_arb = Arbiter(
            self.n_mshrs, MetaWriteReq, self.params)
        refill_arb = m.submodules.refill_arb = Arbiter(self.n_mshrs,
                                                       DataWriteReq,
                                                       self.params)
        replay_arb = m.submodules.replay_arb = Arbiter(self.n_mshrs, MSHRReq,
                                                       self.params)
        wb_arb = m.submodules.wb_arb = Arbiter(self.n_mshrs, WritebackReq,
                                               self.params)
        resp_arb = m.submodules.resp_arb = Arbiter(
            self.n_mshrs + self.n_iomshrs, DCacheResp, self.params)

        idx_matches = Array(
            Signal(self.n_mshrs, name=f'idx_matches{i}')
            for i in range(self.mem_width))
        tag_matches = [
            Signal(self.n_mshrs, name=f'tag_matches{i}')
            for i in range(self.mem_width)
        ]
        way_matches = [
            Signal(self.n_mshrs, name=f'way_matches{i}')
            for i in range(self.mem_width)
        ]

        idx_match = Array(
            Signal(name=f'idx_match{i}') for i in range(self.mem_width))
        tag_match = Array(
            Signal(name=f'tag_match{i}') for i in range(self.mem_width))
        way_match = Array(
            Signal(name=f'way_match{i}') for i in range(self.mem_width))
        for w in range(self.mem_width):
            for i in range(self.n_mshrs):
                with m.If(idx_matches[w][i]):
                    m.d.comb += [
                        idx_match[w].eq(1),
                        tag_match[w].eq(tag_matches[w][i]),
                        way_match[w].eq(way_matches[w][i]),
                    ]

        mshr_alloc_idx = Signal(range(self.n_mshrs))
        pri_valid = req_valid & sdq_ready & ~req_uncacheable & ~idx_match[
            req_idx]
        pri_ready = Signal()
        sec_ready = 0

        mshrs = []
        for i in range(self.n_mshrs):
            mshr_dbus = wishbone.Interface(addr_width=self.dbus.addr_width,
                                           data_width=self.dbus.data_width,
                                           granularity=self.dbus.granularity,
                                           name=f'mshr_dbus{i}')
            dbus_arbiter.add(mshr_dbus)

            mshr = MSHR(i, self.params, mshr_dbus)
            setattr(m.submodules, f'mshr{i}', mshr)

            for w in range(self.mem_width):
                m.d.comb += [
                    idx_matches[w][i].eq(mshr.idx.valid
                                         & (mshr.idx.bits == self.addr_index(
                                             self.req[w].bits.addr))),
                    tag_matches[w][i].eq(mshr.tag.valid
                                         & (mshr.tag.bits == self.addr_tag(
                                             self.req[w].bits.addr))),
                    way_matches[w][i].eq(
                        mshr.way.valid
                        & (mshr.way.bits == self.req[w].bits.way_en)),
                ]

            m.d.comb += [
                mshr.req.eq(req),
                mshr.req.sdq_id.eq(sdq_alloc_idx),
                mshr.req_pri_valid.eq(pri_valid & (i == mshr_alloc_idx)),
                mshr.req_sec_valid.eq(req_valid
                                      & sdq_ready & tag_match[req_idx]
                                      & idx_matches[req_idx][i]
                                      & ~req_uncacheable),
            ]
            with m.If(mshr_alloc_idx == i):
                m.d.comb += pri_ready.eq(mshr.req_pri_ready)

            sec_ready |= mshr.req_sec_valid & mshr.req_sec_ready

            m.d.comb += [
                mshr.br_update.eq(self.br_update),
                mshr.exception.eq(self.exception),
            ]

            m.d.comb += [
                mshr.lb_read.connect(lb_read_arb.inp[i]),
                mshr.lb_resp.eq(lb_read.data),
                mshr.lb_write.connect(lb_write_arb.inp[i]),
            ]

            m.d.comb += [
                mshr.meta_read.connect(meta_read_arb.inp[i]),
                mshr.meta_resp.eq(self.meta_resp),
                mshr.meta_write.connect(meta_write_arb.inp[i]),
                mshr.refill.connect(refill_arb.inp[i]),
                mshr.replay.connect(replay_arb.inp[i]),
                mshr.wb_req.connect(wb_arb.inp[i]),
                mshr.wb_resp.eq(self.wb_resp),
            ]

            m.d.comb += mshr.resp.connect(resp_arb.inp[i])

            mshrs.append(mshr)

        for i in reversed(range(self.n_mshrs)):
            with m.If(mshrs[i].req_pri_ready):
                m.d.comb += mshr_alloc_idx.eq(i)

        iomshr_alloc_arb = m.submodules.iomshr_alloc_arb = Arbiter(
            self.n_iomshrs, Signal)

        mmio_ready = 0

        io_mshrs = []
        for i in range(self.n_iomshrs):
            mshr_dbus = wishbone.Interface(addr_width=self.dbus.addr_width,
                                           data_width=self.dbus.data_width,
                                           granularity=self.dbus.granularity,
                                           name=f'iomshr_dbus{i}')
            dbus_arbiter.add(mshr_dbus)

            mshr = IOMSHR(self.n_mshrs + i, self.params, mshr_dbus)
            setattr(m.submodules, f'iomshr{i}', mshr)

            mmio_ready |= mshr.req.ready

            m.d.comb += [
                iomshr_alloc_arb.inp[i].valid.eq(mshr.req.ready),
                mshr.req.valid.eq(iomshr_alloc_arb.inp[i].ready),
                mshr.req.bits.uop.eq(req.uop),
                mshr.req.bits.addr.eq(req.addr),
                mshr.req.bits.data.eq(req.data),
            ]

            m.d.comb += mshr.resp.connect(resp_arb.inp[self.n_mshrs + i])

            io_mshrs.append(mshr)

        m.d.comb += iomshr_alloc_arb.out.ready.eq(req_valid & req_uncacheable)

        m.d.comb += dbus_arbiter.bus.connect(self.dbus)

        m.d.comb += [
            meta_read_arb.out.connect(self.meta_read),
            meta_write_arb.out.connect(self.meta_write),
            refill_arb.out.connect(self.refill),
            wb_arb.out.connect(self.wb_req),
        ]

        resp_q = m.submodules.resp_q = BranchKillableFIFO(
            4,
            self.params,
            DCacheResp,
            self.params,
            flow=False,
            flush_fn=lambda resp: resp.uop.uses_ldq)

        m.d.comb += [
            resp_q.br_update.eq(self.br_update),
            resp_q.flush.eq(self.exception),
            resp_q.w_data.eq(resp_arb.out.bits),
            resp_q.w_br_mask.eq(resp_arb.out.bits.uop.br_mask),
            resp_q.w_en.eq(resp_arb.out.valid),
            resp_arb.out.ready.eq(resp_q.w_rdy),
            self.resp.bits.eq(resp_q.r_data),
            self.resp.bits.uop.br_mask.eq(resp_q.r_br_mask),
            self.resp.valid.eq(resp_q.r_rdy),
            resp_q.r_en.eq(self.resp.ready),
        ]

        for w in range(self.mem_width):
            m.d.comb += [
                self.req[w].ready.eq((w == req_idx) & Mux(
                    req_uncacheable,
                    mmio_ready,
                    sdq_ready
                    & Mux(
                        idx_match[w],
                        tag_match[w]
                        & sec_ready,
                        pri_ready,
                    ),
                )),
                self.secondary_miss[w].eq(idx_match[w] & way_match[w]
                                          & ~tag_match[w]),
                self.block_hit[w].eq(idx_match[w] & tag_match[w]),
            ]

        sdq_read = m.submodules.sdq_read = sdq.read_port(domain='comb',
                                                         transparent=True)

        m.d.comb += [
            sdq_read.addr.eq(self.replay.bits.sdq_id),
            replay_arb.out.connect(self.replay),
            self.replay.bits.data.eq(sdq_read.data),
        ]

        sdq_alloc_mask = Signal(self.sdq_size)
        sdq_dealloc_mask = Signal(self.sdq_size)
        for i in range(self.sdq_size):
            with m.If(i == self.replay.bits.sdq_id):
                m.d.comb += sdq_dealloc_mask[i].eq(
                    self.replay.fire
                    & MemoryCommand.is_write(self.replay.bits.uop.mem_cmd))

            with m.If(i == sdq_alloc_idx):
                m.d.comb += sdq_alloc_mask[i].eq(sdq_write.en)

        m.d.sync += sdq_valid.eq(sdq_valid & ~sdq_dealloc_mask
                                 | sdq_alloc_mask)

        return m


#
# Non-blocking data cache
#


class DCache(HasDCacheParams, Elaboratable):

    def __init__(self, dbus, params):
        super().__init__(params)

        self.dbus = dbus

        self.req = [
            Decoupled(DCacheReq, params, name=f'req{i}')
            for i in range(self.mem_width)
        ]

        self.resp = [
            Valid(DCacheResp, self.params, name=f'resp{i}')
            for i in range(self.mem_width)
        ]

        self.nack = [
            Valid(DCacheReq, self.params, name=f'nack{i}')
            for i in range(self.mem_width)
        ]

        self.s1_kill = Signal(self.mem_width)

        self.br_update = BranchUpdate(params)
        self.exception = Signal()

    def elaborate(self, platform):
        m = Module()

        wb_dbus = wishbone.Interface(addr_width=self.dbus.addr_width,
                                     data_width=self.dbus.data_width,
                                     granularity=self.dbus.granularity,
                                     name='wb_dbus')
        wb = m.submodules.wb = WritebackUnit(self.params, wb_dbus)

        mshrs_dbus = wishbone.Interface(addr_width=self.dbus.addr_width,
                                        data_width=self.dbus.data_width,
                                        granularity=self.dbus.granularity,
                                        name='mshrs_dbus')
        mshrs = m.submodules.mshrs = MSHRFile(self.params, mshrs_dbus)

        m.d.comb += [
            mshrs.br_update.eq(self.br_update),
            mshrs.exception.eq(self.exception),
        ]

        dbus_arbiter = m.submodules.dbus_arbiter = wishbone.Arbiter(
            data_width=self.dbus.data_width,
            addr_width=self.dbus.addr_width,
            granularity=self.dbus.granularity)

        dbus_arbiter.add(wb_dbus)
        dbus_arbiter.add(mshrs_dbus)

        m.d.comb += dbus_arbiter.bus.connect(self.dbus)

        #
        # Tag & data access
        #

        # 0 - MSHR refill
        meta_write_arb = m.submodules.meta_write_arb = Arbiter(
            1, MetaWriteReq, self.params)

        meta_read_arb = []
        meta = []
        for w in range(self.mem_width):
            # 0 - MSHR replay, 1 - WB, 2 - MSHR meta read, 3 - LSU
            read_arb = Arbiter(4, MetaReadReq, self.params)
            setattr(m.submodules, f'meta_read_arb{w}', read_arb)

            array = MetadataArray(self.params)
            setattr(m.submodules, f'meta{w}', array)

            m.d.comb += [
                array.write.valid.eq(meta_write_arb.out.fire),
                array.write.bits.eq(meta_write_arb.out.bits),
                array.read.valid.eq(read_arb.out.fire),
                array.read.bits.eq(read_arb.out.bits),
                read_arb.out.ready.eq(array.read.ready),
            ]
            meta.append(array)
            meta_read_arb.append(read_arb)

        m.d.comb += meta_write_arb.out.ready.eq(
            Cat(m.write.ready for m in meta) != 0)

        data = DuplicatedDataArray(
            self.params) if self.n_data_banks == 1 else None
        m.submodules.data = data

        # 0 - LSU, 1 - MSHR refill
        data_write_arb = m.submodules.data_write_arb = Arbiter(
            2, DataWriteReq, self.params)

        data_read_arb = []
        for w in range(self.mem_width):
            # 0 - MSHR replay, 1 - WB, 2 - LSU
            read_arb = Arbiter(3, DataReadReq, self.params)
            setattr(m.submodules, f'data_read_arb{w}', read_arb)

            m.d.comb += [
                data.read[w].valid.eq(read_arb.out.fire),
                data.read[w].bits.eq(read_arb.out.bits),
                read_arb.out.ready.eq(1),
            ]
            data_read_arb.append(read_arb)

        m.d.comb += [
            data.write.eq(data_write_arb.out),
            data_write_arb.out.ready.eq(1),
        ]

        #
        # Incoming requests
        #

        req_ready = Cat(meta_read_arb[w].inp[3].ready
                        & data_read_arb[w].inp[2].ready
                        for w in range(self.mem_width)) == Repl(
                            1, self.mem_width)

        for w in range(self.mem_width):
            m.d.comb += [
                self.req[w].ready.eq(req_ready),
                meta_read_arb[w].inp[3].valid.eq(self.req[w].valid),
                meta_read_arb[w].inp[3].bits.idx.eq(
                    self.addr_index(self.req[w].bits.addr)),
                data_read_arb[w].inp[2].valid.eq(self.req[w].valid),
                data_read_arb[w].inp[2].bits.addr.eq(self.req[w].bits.addr),
                data_read_arb[w].inp[2].bits.way_en.eq(Repl(1, self.n_ways)),
            ]

        #
        # MSHR replay
        #

        mshr_replay_req = [
            DCacheReq(self.params, name=f'mshr_replay_req{i}')
            for i in range(self.mem_width)
        ]
        m.d.comb += [
            mshr_replay_req[0].uop.eq(mshrs.replay.bits.uop),
            mshr_replay_req[0].addr.eq(mshrs.replay.bits.addr),
            mshr_replay_req[0].data.eq(mshrs.replay.bits.data),
            mshrs.replay.ready.eq(meta_read_arb[0].inp[0].ready
                                  & data_read_arb[0].inp[0].ready),
            meta_read_arb[0].inp[0].valid.eq(mshrs.replay.valid),
            meta_read_arb[0].inp[0].bits.idx.eq(
                self.addr_index(mshrs.replay.bits.addr)),
            data_read_arb[0].inp[0].valid.eq(mshrs.replay.valid),
            data_read_arb[0].inp[0].bits.addr.eq(mshrs.replay.bits.addr),
            data_read_arb[0].inp[0].bits.way_en.eq(mshrs.replay.bits.way_en),
        ]

        #
        # MSHR metadata read
        #

        mshr_read_req = [
            DCacheReq(self.params, name=f'mshr_read_req{i}')
            for i in range(self.mem_width)
        ]
        m.d.comb += [
            mshr_read_req[0].addr.eq(
                Cat(Repl(0, self.block_off_bits), mshrs.meta_read.bits.idx,
                    mshrs.meta_read.bits.tag)),
            mshrs.meta_read.ready.eq(meta_read_arb[0].inp[2].ready),
            meta_read_arb[0].inp[2].valid.eq(mshrs.meta_read.valid),
            meta_read_arb[0].inp[2].bits.eq(mshrs.meta_read.bits),
        ]

        #
        # Writeback
        #

        wb_fire = wb.meta_read.fire & wb.data_read.fire
        wb_req = [
            DCacheReq(self.params, name=f'wb_req{i}')
            for i in range(self.mem_width)
        ]
        m.d.comb += [
            wb_req[0].addr.eq(
                Cat(wb.data_read.bits.addr, wb.meta_read.bits.tag)),
            meta_read_arb[0].inp[1].valid.eq(wb.meta_read.valid),
            meta_read_arb[0].inp[1].bits.eq(wb.meta_read.bits),
            data_read_arb[0].inp[1].valid.eq(wb.data_read.valid),
            data_read_arb[0].inp[1].bits.eq(wb.data_read.bits),
            wb.meta_read.ready.eq(meta_read_arb[0].inp[1].ready
                                  & data_read_arb[0].inp[1].ready),
            wb.data_read.ready.eq(meta_read_arb[0].inp[1].ready
                                  & data_read_arb[0].inp[1].ready),
        ]

        #
        # S0 - Request arbitration
        #

        s0_valid = Mux(
            Cat(r.fire for r in self.req) != 0, Cat(r.valid for r in self.req),
            Mux(wb_fire | mshrs.meta_read.fire | mshrs.replay.fire, 1, 0))
        s0_req = [
            DCacheReq(self.params, name=f's0_req{i}')
            for i in range(self.mem_width)
        ]
        s0_type = Signal(DCacheReqType)

        s0_need_resp = Mux(
            Cat(r.fire for r in self.req) != 0, s0_valid,
            Mux(
                mshrs.replay.fire
                & MemoryCommand.is_read(mshrs.replay.bits.uop.mem_cmd), 1, 0))

        for w in range(self.mem_width):
            with m.If(Cat(r.fire for r in self.req) != 0):
                m.d.comb += s0_req[w].eq(self.req[w].bits)
            with m.Elif(wb_fire):
                m.d.comb += s0_req[w].eq(wb_req[w])
            with m.Elif(mshrs.meta_read.fire):
                m.d.comb += s0_req[w].eq(mshr_read_req[w])
            with m.Else():
                m.d.comb += s0_req[w].eq(mshr_replay_req[w])

        m.d.comb += s0_type.eq(
            Mux(
                Cat(r.fire for r in self.req) != 0, DCacheReqType.LSU,
                Mux(
                    wb_fire, DCacheReqType.WRITEBACK,
                    Mux(mshrs.meta_read.fire, DCacheReqType.MSHR_META,
                        DCacheReqType.REPLAY))))

        #
        # S1 - Tag access
        #

        s1_req = [
            DCacheReq(self.params, name=f's1_req{i}')
            for i in range(self.mem_width)
        ]
        s1_addr = [
            Signal.like(s1_req[0].addr, name=f's1_addr{i}')
            for i in range(self.mem_width)
        ]
        s1_type = Signal.like(s0_type)
        s1_valid = Signal(self.mem_width)
        s2_store_failed = Signal()

        s1_need_resp = Signal.like(s0_need_resp)

        for w in range(self.mem_width):
            m.d.sync += [
                s1_req[w].eq(s0_req[w]),
                s1_req[w].uop.br_mask.eq(
                    self.br_update.get_new_br_mask(s0_req[w].uop.br_mask)),
                s1_valid[w].eq(s0_valid[w]
                               & ~self.br_update.uop_killed(s0_req[w].uop)
                               & ~(self.exception & s0_req[w].uop.uses_ldq)
                               & ~(s2_store_failed &
                                   (s0_type == DCacheReqType.LSU)
                                   & s0_req[w].uop.uses_stq)),
                s1_need_resp.eq(s0_need_resp),
            ]

            m.d.comb += s1_addr[w].eq(s1_req[w].addr)

        m.d.sync += s1_type.eq(s0_type)

        s1_mshr_meta_read_way_en = Signal(self.n_ways)
        s1_replay_way_en = Signal(self.n_ways)
        s1_wb_way_en = Signal(self.n_ways)
        m.d.sync += [
            s1_mshr_meta_read_way_en.eq(mshrs.meta_read.bits.way_en),
            s1_replay_way_en.eq(mshrs.replay.bits.way_en),
            s1_wb_way_en.eq(wb.data_read.bits.way_en),
        ]

        s1_tag_match_way = [
            Signal(self.n_ways, name=f's1_tag_match_way{i}')
            for i in range(self.mem_width)
        ]

        s1_wb_idx_matches = Signal(self.mem_width)

        for w in range(self.mem_width):
            with m.Switch(s1_type):
                with m.Case(DCacheReqType.MSHR_META):
                    m.d.comb += s1_tag_match_way[w].eq(
                        s1_mshr_meta_read_way_en)

                with m.Case(DCacheReqType.REPLAY):
                    m.d.comb += s1_tag_match_way[w].eq(s1_replay_way_en)

                with m.Case(DCacheReqType.WRITEBACK):
                    m.d.comb += s1_tag_match_way[w].eq(s1_wb_way_en)

                with m.Default():
                    m.d.comb += s1_tag_match_way[w].eq(
                        Cat((meta[w].resp[i].tag == self.addr_tag(s1_addr[w]))
                            & CacheState.is_valid(meta[w].resp[i].state)
                            for i in range(self.n_ways)))

            m.d.comb += s1_wb_idx_matches[w].eq(
                wb.idx.valid & (self.addr_index(s1_addr[w]) == wb.idx.bits))

        s1_replace_way = Signal(range(self.n_ways))
        s1_replace_way_en = Signal(self.n_ways)
        for i in range(self.n_ways):
            with m.If(s1_replace_way == i):
                m.d.comb += s1_replace_way_en[i].eq(1)

        #
        # S2 - Data access
        #

        s2_req = [
            DCacheReq(self.params, name=f's2_req{i}')
            for i in range(self.mem_width)
        ]
        s2_type = Signal.like(s1_type)
        s2_valid = Signal(self.mem_width)
        s2_tag_match_way = [
            Signal(self.n_ways, name=f's2_tag_match_way{i}')
            for i in range(self.mem_width)
        ]
        s2_tag_match = [s != 0 for s in s2_tag_match_way]
        s2_hit_state = [
            Signal(CacheState, name=f's2_hit_state{i}')
            for i in range(self.mem_width)
        ]
        s2_new_hit_state = [
            Signal(CacheState, name=f's2_new_hit_state{i}')
            for i in range(self.mem_width)
        ]
        s2_state_is_hit = Signal(self.mem_width)
        s2_hit = Signal(self.mem_width)
        s2_wb_idx_matches = Signal.like(s1_wb_idx_matches)

        s2_metadata = [[
            Metadata(self.params, name=f's2_metadata{i}_{w}')
            for w in range(self.n_ways)
        ] for i in range(self.mem_width)]

        s2_replace_way_en = Signal.like(s1_replace_way_en)
        s2_replace_meta = [
            Metadata(self.params, name=f's2_replace_meta{i}')
            for i in range(self.mem_width)
        ]

        s2_need_resp = Signal.like(s1_need_resp)

        for w in range(self.mem_width):
            m.d.sync += [
                s2_req[w].eq(s1_req[w]),
                s2_req[w].uop.br_mask.eq(
                    self.br_update.get_new_br_mask(s1_req[w].uop.br_mask)),
                s2_valid[w].eq(s1_valid[w]
                               & ~self.s1_kill[w]
                               & ~self.br_update.uop_killed(s1_req[w].uop)
                               & ~(self.exception & s1_req[w].uop.uses_ldq)
                               & ~(s2_store_failed &
                                   (s1_type == DCacheReqType.LSU)
                                   & s1_req[w].uop.uses_stq)),
                s2_tag_match_way[w].eq(s1_tag_match_way[w]),
            ]

            is_hit, next_state = CacheState.on_access(m, s2_hit_state[w],
                                                      s2_req[w].uop.mem_cmd)

            m.d.comb += [
                s2_state_is_hit[w].eq(is_hit),
                s2_new_hit_state[w].eq(next_state),
            ]

        m.d.sync += [
            s2_type.eq(s1_type),
            s2_replace_way_en.eq(s1_replace_way_en),
            s2_need_resp.eq(s1_need_resp),
            s2_wb_idx_matches.eq(s1_wb_idx_matches),
        ]

        m.d.comb += s2_hit.eq(
            Cat((s2_tag_match[w] & s2_state_is_hit[w]
                 & (s2_new_hit_state[w] == s2_hit_state[w]))
                | (s2_type == DCacheReqType.REPLAY)
                | (s2_type == DCacheReqType.WRITEBACK)
                for w in range(self.mem_width)))

        for i in range(self.mem_width):
            for w in range(self.n_ways):
                m.d.sync += s2_metadata[i][w].eq(meta[i].resp[w])

                with m.If(s2_tag_match_way[i][w]):
                    m.d.comb += s2_hit_state[i].eq(s2_metadata[i][w].state)

                with m.If(s2_replace_way_en[w]):
                    m.d.comb += s2_replace_meta[i].eq(s2_metadata[i][w])

        s2_data = [[
            Signal(self.row_bits, name=f's2_data{i}_way{w}')
            for w in range(self.n_ways)
        ] for i in range(self.mem_width)]

        for i in range(self.mem_width):
            for w in range(self.n_ways):
                m.d.comb += s2_data[i][w].eq(data.resp[i][w])

        s2_data_muxed = [
            Signal.like(s2_data[0][0], name=f's2_data_muxed{i}')
            for i in range(self.mem_width)
        ]
        for i in range(self.mem_width):
            for w in range(self.n_ways):
                with m.If(s2_tag_match_way[i][w]):
                    m.d.comb += s2_data_muxed[i].eq(s2_data[i][w])

        s2_nack_victim = Signal(self.mem_width)
        s2_nack_mshr = Signal(self.mem_width)
        s2_nack_data = Signal(self.mem_width)
        s2_nack_wb = Signal(self.mem_width)

        m.d.comb += [
            s2_nack_victim.eq(
                Cat(s2_valid[w] & s2_hit[w] & mshrs.secondary_miss[w]
                    for w in range(self.mem_width))),
            s2_nack_mshr.eq(
                Cat(s2_valid[w] & ~s2_hit[w] & ~mshrs.req[w].ready
                    for w in range(self.mem_width))),
            s2_nack_data.eq(data.nack),
            s2_nack_wb.eq(
                Cat(s2_valid[w] & ~s2_hit[w] & s2_wb_idx_matches[w]
                    for w in range(self.mem_width))),
        ]

        s2_nack = Signal(self.mem_width)
        m.d.comb += s2_nack.eq(
            Cat((s2_nack_mshr[w] | s2_nack_victim[w]
                 | s2_nack_data[w] | s2_nack_wb[w])
                & (s2_type != DCacheReqType.REPLAY)
                for w in range(self.mem_width)))

        s2_send_resp = Signal(self.mem_width)
        s2_send_nack = Signal(self.mem_width)

        for w in range(self.mem_width):
            m.d.comb += [
                s2_send_resp[w].eq(s2_need_resp[w] & ~s2_nack[w] & (
                    s2_hit[w]
                    | (mshrs.req[w].fire
                       & MemoryCommand.is_write(s2_req[w].uop.mem_cmd)
                       & ~MemoryCommand.is_read(s2_req[w].uop.mem_cmd)))),
                s2_send_nack[w].eq(s2_need_resp[w] & s2_nack[w]),
            ]

        # Kill younger stores if an older store failed
        m.d.comb += s2_store_failed.eq(s2_valid[0] & s2_nack[0]
                                       & s2_send_nack[0]
                                       & s2_req[0].uop.uses_stq)

        #
        # Cache miss
        #

        for w in range(self.mem_width):
            m.d.comb += [
                mshrs.req[w].valid.eq(
                    s2_valid[w] & ~s2_hit[w] & ~s2_nack_data[w]
                    & ~s2_nack_victim[w] & ~s2_nack_wb[w]
                    & (s2_type == DCacheReqType.LSU)
                    & ~self.br_update.uop_killed(s2_req[w].uop)
                    & ~(self.exception & s2_req[w].uop.uses_ldq)
                    & (MemoryCommand.is_read(s2_req[w].uop.mem_cmd)
                       | MemoryCommand.is_write(s2_req[w].uop.mem_cmd))),
                mshrs.req[w].bits.uop.eq(s2_req[w].uop),
                mshrs.req[w].bits.uop.br_mask.eq(
                    self.br_update.get_new_br_mask(s2_req[w].uop.br_mask)),
                mshrs.req[w].bits.addr.eq(s2_req[w].addr),
                mshrs.req[w].bits.data.eq(s2_req[w].data),
                mshrs.req[w].bits.tag_match.eq(s2_tag_match[w]),
                mshrs.req[w].bits.way_en.eq(
                    Mux(s2_tag_match[w], s2_tag_match_way[w],
                        s2_replace_way_en)),
                mshrs.req[w].bits.old_meta.tag.eq(s2_replace_meta[w].tag),
                mshrs.req[w].bits.old_meta.state.eq(
                    Mux(s2_tag_match[w], s2_hit_state[w],
                        s2_replace_meta[w].state)),
            ]

        m.d.comb += mshrs.meta_resp.valid.eq(1)
        for w in range(self.n_ways):
            with m.If(s2_tag_match_way[0][w]):
                m.d.comb += mshrs.meta_resp.bits.eq(s2_metadata[0][w])

        with m.If(Cat(r.fire for r in mshrs.req) != 0):
            m.d.sync += s1_replace_way.eq(s1_replace_way + 1)

        #
        # Refill
        #

        m.d.comb += [
            mshrs.meta_write.connect(meta_write_arb.inp[0]),
            mshrs.refill.connect(data_write_arb.inp[1]),
        ]

        #
        # Writeback
        #

        m.d.comb += [
            mshrs.wb_req.connect(wb.req),
            wb.data_resp.eq(s2_data_muxed[0]),
            mshrs.wb_resp.eq(wb.resp),
        ]

        #
        # Response selection
        #

        s2_data_word = [
            Signal(self.word_bits, name=f's2_data_word{i}')
            for i in range(self.mem_width)
        ]

        cache_resp = [
            Valid(DCacheResp, self.params, name=f'cache_resp{i}')
            for i in range(self.mem_width)
        ]

        for w in range(self.mem_width):
            load_gen = LoadGen(max_size=self.dbus.data_width // 8)
            m.submodules += load_gen

            m.d.comb += [
                load_gen.typ.eq(s2_req[w].uop.mem_size),
                load_gen.signed.eq(s2_req[w].uop.mem_signed),
                load_gen.addr.eq(s2_req[w].addr),
                load_gen.data_in.eq(s2_data_word[w]),
            ]

            m.d.comb += [
                cache_resp[w].valid.eq(s2_valid[w] & s2_send_resp[w]),
                cache_resp[w].bits.uop.eq(s2_req[w].uop),
                cache_resp[w].bits.data.eq(load_gen.data_out),
            ]

        resp = [
            Valid(DCacheResp, self.params, name=f'resp_mux{i}')
            for i in range(self.mem_width)
        ]

        mshr_resp_busy = 0
        for w in range(self.mem_width):
            m.d.comb += resp[w].eq(cache_resp[w])

            mshr_resp_valid = ~(cache_resp[w].valid | mshr_resp_busy)
            with m.If(mshr_resp_valid):
                m.d.comb += resp[w].eq(mshrs.resp)

            mshr_resp_busy |= mshr_resp_valid

        m.d.comb += mshrs.resp.ready.eq(
            Cat(r.valid for r in cache_resp) != Repl(1, self.mem_width))

        for w in range(self.mem_width):
            m.d.comb += [
                self.resp[w].valid.eq(
                    resp[w].valid
                    & ~self.br_update.uop_killed(resp[w].bits.uop)
                    & ~(self.exception & resp[w].bits.uop.uses_ldq)),
                self.resp[w].bits.eq(resp[w].bits),
                self.resp[w].bits.uop.br_mask.eq(
                    self.br_update.get_new_br_mask(resp[w].bits.uop.br_mask)),
            ]

            m.d.comb += [
                self.nack[w].valid.eq(
                    s2_valid[w] & s2_send_nack[w]
                    & ~self.br_update.uop_killed(s2_req[w].uop)
                    & ~(self.exception & s2_req[w].uop.uses_ldq)),
                self.nack[w].bits.eq(s2_req[w]),
                self.nack[w].bits.uop.br_mask.eq(
                    self.br_update.get_new_br_mask(s2_req[w].uop.br_mask)),
            ]

        #
        # Store hit
        #

        s3_req = DCacheReq(self.params, name=f's3_req')
        s3_valid = Signal()
        s3_way = Signal.like(s2_tag_match_way[0])

        amo_gen = m.submodules.amo_gen = AMODataGen(self.xlen)
        store_gen = m.submodules.store_gen = StoreGen(max_size=self.xlen // 8)

        m.d.comb += [
            store_gen.typ.eq(s2_req[0].uop.mem_size),
            store_gen.addr.eq(s2_req[0].addr),
            amo_gen.mask.eq(store_gen.mask),
            amo_gen.cmd.eq(s2_req[0].uop.mem_cmd),
            amo_gen.lhs.eq(s2_data_word[0]),
            amo_gen.rhs.eq(s2_req[0].data),
        ]

        m.d.sync += [
            s3_req.eq(s2_req[0]),
            s3_req.data.eq(amo_gen.out),
            s3_valid.eq(s2_valid[0] & s2_hit[0]
                        & MemoryCommand.is_write(s2_req[0].uop.mem_cmd)
                        & ~(s2_send_nack[0] & s2_nack[0])),
            s3_way.eq(s2_tag_match_way[0]),
        ]

        m.d.comb += [
            data_write_arb.inp[0].valid.eq(s3_valid),
            data_write_arb.inp[0].bits.addr.eq(s3_req.addr),
            data_write_arb.inp[0].bits.data.eq(s3_req.data),
            data_write_arb.inp[0].bits.way_en.eq(s3_way),
        ]

        #
        # Load bypassing
        #

        s4_req = DCacheReq(self.params, name=f's4_req')
        s4_valid = Signal()
        s5_req = DCacheReq(self.params, name=f's5_req')
        s5_valid = Signal()

        m.d.sync += [
            s4_req.eq(s3_req),
            s4_valid.eq(s3_valid),
            s5_req.eq(s4_req),
            s5_valid.eq(s4_valid),
        ]

        s3_bypass = Cat(s3_valid & (s2_req[w].addr[self.word_off_bits:] ==
                                    s3_req.addr[self.word_off_bits:])
                        for w in range(self.mem_width))
        s4_bypass = Cat(s4_valid & (s2_req[w].addr[self.word_off_bits:] ==
                                    s4_req.addr[self.word_off_bits:])
                        for w in range(self.mem_width))
        s5_bypass = Cat(s5_valid & (s2_req[w].addr[self.word_off_bits:] ==
                                    s5_req.addr[self.word_off_bits:])
                        for w in range(self.mem_width))

        for w in range(self.mem_width):
            m.d.comb += s2_data_word[w].eq(
                Mux(
                    s3_bypass[w], s3_req.data,
                    Mux(s4_bypass[w], s4_req.data,
                        Mux(s5_bypass[w], s5_req.data, s2_data_muxed[w]))))

        return m


class SimpleDCache(HasCoreParams, Elaboratable):

    def __init__(self, dbus, params):
        super().__init__(params)

        self.dbus = dbus

        self.req = Array(
            Decoupled(DCacheReq, params, name=f'req{i}')
            for i in range(self.mem_width))
        self.resp = Array(
            Valid(DCacheResp, params, name=f'resp{i}')
            for i in range(self.mem_width))

        self.nack = [
            Valid(DCacheReq, self.params, name=f'nack{i}')
            for i in range(self.mem_width)
        ]

        self.s1_kill = Array(
            Signal(name='s1_kill{i}') for i in range(self.mem_width))

        self.br_update = BranchUpdate(params)
        self.exception = Signal()

    def elaborate(self, platform):
        m = Module()

        granularity_bits = log2_int(self.xlen // 8)

        last_grant = Signal(range(self.mem_width))
        choice = Signal.like(last_grant)
        chosen = Signal.like(last_grant)

        for w in reversed(range(self.mem_width)):
            with m.If(self.req[w].valid):
                m.d.comb += choice.eq(w)
        for w in reversed(range(self.mem_width)):
            with m.If(self.req[w].valid & (w > last_grant)):
                m.d.comb += choice.eq(w)

        uop = MicroOp(self.params)

        req_addr = Signal(32)
        dat_w = Signal.like(self.dbus.dat_w)
        sel = Signal.like(self.dbus.sel)
        we = Signal.like(self.dbus.we)

        req_chosen = self.req[chosen]

        load_gen = m.submodules.load_gen = LoadGen(max_size=self.xlen // 8)
        m.d.comb += [
            load_gen.typ.eq(uop.mem_size),
            load_gen.signed.eq(uop.mem_signed),
            load_gen.addr.eq(req_addr),
            load_gen.data_in.eq(self.dbus.dat_r),
        ]

        store_gen = m.submodules.store_gen = StoreGen(max_size=self.xlen // 8)
        m.d.comb += [
            store_gen.typ.eq(req_chosen.bits.uop.mem_size),
            store_gen.addr.eq(req_chosen.bits.addr),
            store_gen.data_in.eq(req_chosen.bits.data),
        ]

        with m.FSM():
            with m.State('IDLE'):
                m.d.comb += chosen.eq(choice),

                with m.If(req_chosen.valid
                          & ~self.br_update.uop_killed(req_chosen.bits.uop)
                          & ~(self.exception & req_chosen.bits.uop.uses_ldq)):
                    m.d.comb += [
                        self.dbus.adr.eq(
                            req_chosen.bits.addr[granularity_bits:]),
                        self.dbus.stb.eq(1),
                        self.dbus.dat_w.eq(store_gen.data_out),
                        self.dbus.cyc.eq(1),
                        self.dbus.sel.eq(store_gen.mask),
                        self.dbus.we.eq(req_chosen.bits.uop.mem_cmd ==
                                        MemoryCommand.WRITE),
                        req_chosen.ready.eq(1),
                    ]

                    m.d.sync += [
                        uop.eq(req_chosen.bits.uop),
                        uop.br_mask.eq(
                            self.br_update.get_new_br_mask(
                                req_chosen.bits.uop.br_mask)),
                        req_addr.eq(req_chosen.bits.addr),
                        dat_w.eq(self.dbus.dat_w),
                        sel.eq(self.dbus.sel),
                        we.eq(self.dbus.we),
                        last_grant.eq(choice),
                    ]

                    m.next = 'WAIT_ACK'

            with m.State('WAIT_ACK'):
                req_killed = self.s1_kill[chosen] | self.br_update.uop_killed(
                    uop) | (self.exception & uop.uses_ldq)

                m.d.comb += [
                    chosen.eq(last_grant),
                    self.dbus.adr.eq(req_addr[granularity_bits:]),
                    self.dbus.stb.eq(self.dbus.cyc),
                    self.dbus.dat_w.eq(dat_w),
                    self.dbus.cyc.eq(~req_killed),
                    self.dbus.sel.eq(sel),
                    self.dbus.we.eq(we),
                ]

                m.d.sync += [
                    uop.br_mask.eq(self.br_update.get_new_br_mask(
                        uop.br_mask)),
                ]

                with m.If(req_killed):
                    m.next = 'IDLE'
                with m.Elif(self.dbus.cyc & self.dbus.stb & self.dbus.ack):
                    m.d.comb += [
                        self.resp[chosen].valid.eq(1),
                        self.resp[chosen].bits.data.eq(load_gen.data_out),
                        self.resp[chosen].bits.uop.eq(uop),
                        self.resp[chosen].bits.uop.br_mask.eq(
                            self.br_update.get_new_br_mask(uop.br_mask)),
                    ]

                    m.next = 'IDLE'

        return m
