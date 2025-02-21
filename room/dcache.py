from amaranth import *
from amaranth import tracer
from amaranth.hdl.rec import *
from amaranth.utils import log2_int, bits_for
from enum import IntEnum

from room.consts import *
from room.alu import AMODataGen
from room.types import HasCoreParams, MicroOp
from room.branch import BranchUpdate, BranchKillableFIFO
from room.utils import Arbiter
from room.mmu import PMAChecker

from roomsoc.interconnect.stream import Valid, Decoupled
from roomsoc.interconnect import tilelink as tl


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
        super().__init__(params, *args, **kwargs)

        dcache_params = params['dcache_params']
        self.n_sets = dcache_params['n_sets']
        self.n_ways = dcache_params['n_ways']
        self.n_banks = dcache_params['n_banks']
        self.block_bytes = dcache_params['block_bytes']
        self.lg_block_bytes = log2_int(self.block_bytes)

        self.block_off_bits = log2_int(self.block_bytes)
        self.index_bits = log2_int(self.n_sets)
        self.untag_bits = self.block_off_bits + self.index_bits
        self.tag_bits = 32 - self.untag_bits

        self.row_bits = dcache_params['row_bits']
        self.row_off_bits = log2_int(self.row_bits // 8)
        self.refill_cycles = (self.block_bytes * 8) // self.row_bits

        self.word_bits = self.xlen
        self.word_off_bits = log2_int(self.word_bits // 8)

        self.row_words = self.row_bits // self.word_bits

        self.n_mshrs = dcache_params['n_mshrs']
        self.n_iomshrs = dcache_params['n_iomshrs']
        self.sdq_size = dcache_params['sdq_size']
        self.rpq_size = dcache_params['rpq_size']

        self.source_id_bits = bits_for(self.n_mshrs + self.n_iomshrs + 1)

    def addr_block_offset(self, addr):
        return addr[:self.block_off_bits]

    def addr_index(self, addr):
        return addr[self.block_off_bits:self.untag_bits]

    def addr_tag(self, addr):
        return addr[self.untag_bits:self.untag_bits + self.tag_bits]

    def make_source(self, id):
        return Const(id, self.source_id_bits)

    def get_source_id(self, source):
        return source[:self.source_id_bits]


class DCacheReqType(IntEnum):
    LSU = 1
    WRITEBACK = 2
    MSHR_META = 3
    REPLAY = 4
    PROBE = 5


class DCacheReq(HasCoreParams):

    def __init__(self, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)
        self.name = name

        self.uop = MicroOp(params, name=f'{name}_uop')

        self.addr = Signal(self.core_max_addr_bits, name=f'{name}_addr')
        self.data = Signal(self.xlen, name=f'{name}_data')

        self.from_core = Signal(name=f'{name}_from_core')

    def eq(self, rhs):
        attrs = ['uop', 'addr', 'data', 'from_core']
        return [getattr(self, a).eq(getattr(rhs, a)) for a in attrs]


class DCacheResp(HasCoreParams):

    def __init__(self, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.uop = MicroOp(params, name=f'{name}_uop')

        self.data = Signal(self.xlen, name=f'{name}_data')

        self.from_core = Signal(name=f'{name}_from_core')

    def eq(self, rhs):
        attrs = ['uop', 'data', 'from_core']
        return [getattr(self, a).eq(getattr(rhs, a)) for a in attrs]


#
# Cache metadata
#


class CacheState(IntEnum):
    NOTHING = 0
    BRANCH = 1
    TRUNK = 2
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
                with m.If(cmd_is_write):
                    m.d.comb += next_state.eq(tl.GrowParam.NtoT)
                with m.Else():
                    m.d.comb += next_state.eq(tl.GrowParam.NtoB)

            with m.Case(CacheState.BRANCH):
                with m.If(cmd_is_write):
                    m.d.comb += next_state.eq(tl.GrowParam.BtoT)
                with m.Else():
                    m.d.comb += [
                        is_hit.eq(1),
                        next_state.eq(CacheState.BRANCH),
                    ]

            with m.Case(CacheState.TRUNK):
                m.d.comb += [
                    is_hit.eq(1),
                    next_state.eq(
                        Mux(cmd_is_write, CacheState.DIRTY, CacheState.TRUNK)),
                ]

            with m.Case(CacheState.DIRTY):
                m.d.comb += [
                    is_hit.eq(1),
                    next_state.eq(CacheState.DIRTY),
                ]

        return is_hit, next_state

    @staticmethod
    def on_grant(m, cmd, param):
        cmd_is_write = MemoryCommand.is_write(cmd)

        grant_state = Signal(CacheState, reset=CacheState.NOTHING)

        with m.Switch(param):
            with m.Case(tl.CapParam.toB):
                with m.If(~cmd_is_write):
                    m.d.comb += grant_state.eq(CacheState.BRANCH)

            with m.Case(tl.CapParam.toT):
                with m.If(cmd_is_write):
                    m.d.comb += grant_state.eq(CacheState.DIRTY)
                with m.Else():
                    m.d.comb += grant_state.eq(CacheState.TRUNK)

        return grant_state

    @staticmethod
    def on_sec_access(m, state, cmd_pri, cmd_sec):
        hit_pri, next_state_pri = CacheState.on_access(m, state, cmd_pri)
        hit_sec, next_state_sec = CacheState.on_access(m, state, cmd_sec)

        hit_again = hit_pri & hit_sec

        sec_is_write = MemoryCommand.is_write(cmd_sec)
        dirtier_state = Mux(sec_is_write, next_state_sec, next_state_pri)
        dirtier_cmd = Mux(sec_is_write, cmd_sec, cmd_pri)

        need_sec_acq = MemoryCommand.is_write(
            cmd_sec) & ~MemoryCommand.is_write(cmd_pri)

        return need_sec_acq, hit_again, dirtier_state, dirtier_cmd

    @staticmethod
    def on_shrink(m, state, param):
        is_dirty = Signal()
        resp_param = Signal(3)
        new_state = Signal(CacheState)

        m.d.comb += is_dirty.eq(state == CacheState.DIRTY)

        with m.Switch(param):
            with m.Case(tl.CapParam.toT):
                with m.Switch(state):
                    with m.Case(CacheState.DIRTY):
                        m.d.comb += [
                            resp_param.eq(tl.ShrinkReportParam.TtoT),
                            new_state.eq(CacheState.TRUNK),
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
                    with m.Case(CacheState.NOTHING):
                        m.d.comb += [
                            resp_param.eq(tl.ShrinkReportParam.NtoN),
                            new_state.eq(CacheState.NOTHING),
                        ]
            with m.Case(tl.CapParam.toB):
                with m.Switch(state):
                    with m.Case(CacheState.DIRTY):
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
                    with m.Case(CacheState.NOTHING):
                        m.d.comb += [
                            resp_param.eq(tl.ShrinkReportParam.NtoN),
                            new_state.eq(CacheState.NOTHING),
                        ]
            with m.Case(tl.CapParam.toN):
                with m.Switch(state):
                    with m.Case(CacheState.DIRTY):
                        m.d.comb += [
                            resp_param.eq(tl.ShrinkReportParam.TtoN),
                            new_state.eq(CacheState.NOTHING),
                        ]
                    with m.Case(CacheState.TRUNK):
                        m.d.comb += [
                            resp_param.eq(tl.ShrinkReportParam.TtoN),
                            new_state.eq(CacheState.NOTHING),
                        ]
                    with m.Case(CacheState.BRANCH):
                        m.d.comb += [
                            resp_param.eq(tl.ShrinkReportParam.BtoN),
                            new_state.eq(CacheState.NOTHING),
                        ]
                    with m.Case(CacheState.NOTHING):
                        m.d.comb += [
                            resp_param.eq(tl.ShrinkReportParam.NtoN),
                            new_state.eq(CacheState.NOTHING),
                        ]

        return is_dirty, resp_param, new_state

    @staticmethod
    def on_cache_control(m, state, cmd):
        return CacheState.on_shrink(m, state, tl.CapParam.toN)

    @staticmethod
    def on_probe(m, state, param):
        return CacheState.on_shrink(m, state, param)


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


class L1MetaReadReq(HasDCacheParams):

    def __init__(self, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.req = [
            MetaReadReq(self.params, name=f'{name}_req{i}')
            for i in range(self.mem_width)
        ]

    def eq(self, rhs):
        return [a.eq(b) for a, b in zip(self.req, rhs.req)]


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

        wipe_count = Signal(range(self.n_sets + 1))
        wipe_done = wipe_count == self.n_sets
        with m.If(~wipe_done):
            m.d.sync += wipe_count.eq(wipe_count + 1)

        meta_bits = len(self.resp[0])

        tag_mem = Memory(width=self.n_ways * meta_bits, depth=self.n_sets)

        tag_read = m.submodules.tag_read = tag_mem.read_port(transparent=False)
        m.d.comb += tag_read.addr.eq(self.read.bits.idx)

        for i in range(self.n_ways):
            m.d.comb += self.resp[i].eq(tag_read.data[i * meta_bits:(i + 1) *
                                                      meta_bits])

        w_data = Metadata(self.params)
        m.d.comb += [
            w_data.state.eq(self.write.bits.state),
            w_data.tag.eq(self.write.bits.tag),
        ]

        wen = ~wipe_done | self.write.valid
        tag_write = m.submodules.tag_write = tag_mem.write_port(
            granularity=meta_bits)
        with m.If(wen):
            m.d.comb += [
                tag_write.addr.eq(
                    Mux(wipe_done, self.write.bits.idx, wipe_count)),
                tag_write.data.eq(Mux(wipe_done, Repl(w_data, self.n_ways),
                                      0)),
                tag_write.en.eq(Mux(wipe_done, self.write.bits.way_en, ~0)),
            ]

        m.d.comb += [
            self.read.ready.eq(~wen),
            self.write.ready.eq(wipe_done),
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
            ('write_intent', 1, DIR_FANOUT),
        ],
                        name=name,
                        src_loc_at=1 + src_loc_at)


class DataWriteReq(HasDCacheParams, Record):

    def __init__(self, params, name=None, src_loc_at=0):
        HasDCacheParams.__init__(self, params)

        Record.__init__(self, [
            ('way_en', self.n_ways, DIR_FANOUT),
            ('wmask', self.row_words, DIR_FANOUT),
            ('addr', self.untag_bits, DIR_FANOUT),
            ('data', self.row_bits, DIR_FANOUT),
        ],
                        name=name,
                        src_loc_at=1 + src_loc_at)


class L1DataReadReq(HasDCacheParams):

    def __init__(self, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.req = [
            DataReadReq(self.params, name=f'{name}_req{i}')
            for i in range(self.mem_width)
        ]
        self.valid = Signal(self.mem_width, name=f'{name}_valid')

    def eq(self, rhs):
        return [a.eq(b) for a, b in zip(self.req, rhs.req)
                ] + [self.valid.eq(rhs.valid)]


class L1DataWriteReq(HasDCacheParams):

    def __init__(self, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.req = [
            DataWriteReq(self.params, name=f'{name}_req{i}')
            for i in range(self.mem_width)
        ]
        self.valid = Signal(self.mem_width, name=f'{name}_valid')

    def eq(self, rhs):
        return [a.eq(b) for a, b in zip(self.req, rhs.req)
                ] + [self.valid.eq(rhs.valid)]


class BaseDataArray(HasDCacheParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.read = [
            Valid(DataReadReq, self.params, name=f'read{i}')
            for i in range(self.mem_width)
        ]
        self.write = [
            Valid(DataWriteReq, self.params, name=f'write{i}')
            for i in range(self.mem_width)
        ]

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

                mem_read = mem.read_port(transparent=False)
                setattr(m.submodules, f'mem_read{i}_{w}', mem_read)

                m.d.comb += mem_read.addr.eq(
                    self.read[i].bits.addr >> self.row_off_bits)

                mem_write = mem.write_port(granularity=self.word_bits)
                setattr(m.submodules, f'mem_write{i}_{w}', mem_write)

                for j in reversed(range(self.mem_width)):
                    with m.If(self.write[j].valid):
                        m.d.comb += [
                            mem_write.addr.eq(
                                self.write[j].bits.addr >> self.row_off_bits),
                            mem_write.data.eq(self.write[j].bits.data),
                            mem_write.en.eq(
                                Repl(self.write[j].bits.way_en[w],
                                     self.row_words)
                                & self.write[j].bits.wmask),
                        ]

                m.d.sync += self.resp[i][w].eq(mem_read.data)

        return m


class BankedDataArray(BaseDataArray):

    def __init__(self, params):
        super().__init__(params)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        bank_size = self.n_sets * self.refill_cycles // self.n_banks

        bank_bits = log2_int(self.n_banks)
        bank_off_bits = self.row_off_bits
        bidx_bits = log2_int(bank_size)
        bidx_off_bits = bank_off_bits + bank_bits

        #
        # S0
        #

        s0_rbanks = [
            Signal(bank_bits, name=f's0_rbank{i}')
            for i in range(self.mem_width)
        ]
        s0_wbanks = [
            Signal(bank_bits, name=f's0_wbank{i}')
            for i in range(self.mem_width)
        ]
        s0_ridxs = [
            Signal(bidx_bits, name=f's0_ridx{i}')
            for i in range(self.mem_width)
        ]
        s0_widxs = [
            Signal(bidx_bits, name=f's0_widx{i}')
            for i in range(self.mem_width)
        ]
        s0_read_valids = Cat(r.valid for r in self.read)
        s0_write_intent = Cat(r.bits.write_intent for r in self.read)
        s0_bank_conflicts = Signal(self.mem_width)
        s0_do_read = s0_read_valids & ~s0_bank_conflicts
        s0_bank_read_gnts = [
            Signal(self.mem_width, name=f's0_bank_read_gnt{i}')
            for i in range(self.n_banks)
        ]

        for w in range(self.mem_width):
            m.d.comb += [
                s0_rbanks[w].eq(
                    (self.read[w].bits.addr >> bank_off_bits)[:bank_bits]),
                s0_ridxs[w].eq(
                    (self.read[w].bits.addr >> bidx_off_bits)[:bidx_bits]),
                s0_wbanks[w].eq(
                    (self.write[w].bits.addr >> bank_off_bits)[:bank_bits]),
                s0_widxs[w].eq(
                    (self.write[w].bits.addr >> bidx_off_bits)[:bidx_bits]),
            ]

            c = Const(0)
            for i in range(w):
                c |= self.read[i].valid & (s0_rbanks[i] == s0_rbanks[w])
            m.d.comb += s0_bank_conflicts[w].eq(c)

        for b in range(self.n_banks):
            for w in range(self.mem_width):
                with m.If((s0_rbanks[w] == b) & s0_do_read[w]):
                    m.d.comb += s0_bank_read_gnts[b][w].eq(1)

        #
        # S1
        #

        s1_rbanks = [
            Signal(bank_bits, name=f's1_rbank{i}')
            for i in range(self.mem_width)
        ]
        s1_ridxs = [
            Signal(bidx_bits, name=f's1_ridx{i}')
            for i in range(self.mem_width)
        ]
        s1_read_valids = Signal(self.mem_width)
        s1_write_intent = Signal(self.mem_width)
        s1_pipe_selection = [
            Signal(self.mem_width, name=f's1_pipe_selection{i}')
            for i in range(self.mem_width)
        ]
        s1_ridx_match = [
            Signal(self.mem_width, name=f's1_ridx_match{i}')
            for i in range(self.mem_width)
        ]
        s1_bank_selection = [
            Signal(range(self.n_banks), name=f's1_bank_selection{i}')
            for i in range(self.mem_width)
        ]
        s1_nacks = Signal(self.mem_width)

        m.d.sync += [
            s1_read_valids.eq(s0_read_valids),
            s1_write_intent.eq(s0_write_intent),
        ]
        for w in range(self.mem_width):
            m.d.sync += [
                s1_rbanks[w].eq(s0_rbanks[w]),
                s1_ridxs[w].eq(s0_ridxs[w]),
            ]

            m.d.comb += [
                s1_pipe_selection[w].eq(1 << w),
                s1_ridx_match[w][w].eq(1),
            ]
            for i in reversed(range(w)):
                with m.If(s1_read_valids[i] & (s1_rbanks[i] == s1_rbanks[w])):
                    m.d.comb += s1_pipe_selection[w].eq(1 << i)
                m.d.comb += s1_ridx_match[w][i].eq((s1_ridxs[i] == s1_ridxs[w])
                                                   & ~s1_write_intent[w])

            m.d.comb += s1_nacks[w].eq(s1_read_valids[w] & (
                (s1_pipe_selection[w] & ~s1_ridx_match[w]) != 0))

            for i in reversed(range(self.mem_width)):
                with m.If(s1_pipe_selection[w][i]):
                    m.d.comb += s1_bank_selection[w].eq(s1_rbanks[i])

        #
        # S2
        #

        s2_bank_selection = [
            Signal(range(self.n_banks), name=f's2_bank_selection{i}')
            for i in range(self.mem_width)
        ]
        s2_nacks = Signal(self.mem_width)

        m.d.sync += s2_nacks.eq(s1_nacks)
        for w in range(self.mem_width):
            m.d.sync += s2_bank_selection[w].eq(s1_bank_selection[w])

        for w in range(self.n_ways):
            s2_bank_reads = Array(
                Signal(self.row_bits, name=f's2_bank_read{w}_b{b}')
                for b in range(self.n_banks))

            for b in range(self.n_banks):
                mem = Memory(width=self.row_bits, depth=bank_size)

                mem_read = mem.read_port(transparent=False)
                setattr(m.submodules, f'mem_read{b}_{w}', mem_read)

                for i in range(self.mem_width):
                    with m.If(s0_bank_read_gnts[b][i]):
                        m.d.comb += mem_read.addr.eq(s0_ridxs[i])
                m.d.sync += s2_bank_reads[b].eq(mem_read.data)

                mem_write = mem.write_port(granularity=self.word_bits)
                setattr(m.submodules, f'mem_write{b}_{w}', mem_write)

                for j in reversed(range(self.mem_width)):
                    with m.If(self.write[j].valid
                              & self.write[j].bits.way_en[w]
                              & (s0_wbanks[j] == b)):
                        m.d.comb += [
                            mem_write.addr.eq(s0_widxs[j]),
                            mem_write.data.eq(self.write[j].bits.data),
                            mem_write.en.eq(self.write[j].bits.wmask),
                        ]

            for i in range(self.mem_width):
                m.d.comb += self.resp[i][w].eq(
                    s2_bank_reads[s2_bank_selection[i]])

        m.d.comb += self.nack.eq(s2_nacks)

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
            ('param', 3, DIR_FANOUT),
            ('voluntary', 1, DIR_FANOUT),
        ],
                        name=name,
                        src_loc_at=1 + src_loc_at)


class WritebackUnit(HasDCacheParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.mem_release = Decoupled(tl.ChannelC,
                                     addr_width=32,
                                     data_width=self.row_bits,
                                     size_width=bits_for(self.lg_block_bytes),
                                     source_id_width=self.source_id_bits)
        self.mem_grant = Signal()

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
        mem_acked = Signal()

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
                        mem_acked.eq(0),
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
                    self.mem_release.bits.opcode.eq(
                        Mux(req.voluntary, tl.ChannelCOpcode.ReleaseData,
                            tl.ChannelCOpcode.ProbeAckData)),
                    self.mem_release.bits.param.eq(req.param),
                    self.mem_release.bits.size.eq(self.lg_block_bytes),
                    self.mem_release.bits.source.eq(
                        self.make_source(self.n_mshrs + self.n_iomshrs)),
                    self.mem_release.bits.address.eq(
                        Cat(Const(0, self.block_off_bits), req.idx, req.tag)),
                    self.mem_release.bits.data.eq(wb_buffer[wb_counter]),
                    self.mem_release.valid.eq(1),
                ]

                with m.If(self.mem_grant):
                    m.d.sync += mem_acked.eq(1)

                with m.If(self.mem_release.fire):
                    m.d.sync += wb_counter.eq(wb_counter + 1)

                    with m.If(wb_counter == (self.refill_cycles - 1)):
                        with m.If(req.voluntary):
                            m.next = 'WAIT_ACK'
                        with m.Else():
                            m.next = 'INVALID'

            with m.State('WAIT_ACK'):
                with m.If(self.mem_grant | mem_acked):
                    m.next = 'INVALID'

        return m


#
# Probe Unit
#
class ProbeUnit(HasDCacheParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.req = Decoupled(tl.ChannelB,
                             addr_width=32,
                             data_width=self.row_bits,
                             size_width=bits_for(self.lg_block_bytes))

        self.mem_release = Decoupled(tl.ChannelC,
                                     addr_width=32,
                                     data_width=self.row_bits,
                                     size_width=bits_for(self.lg_block_bytes),
                                     source_id_width=self.source_id_bits)

        self.meta_read = Decoupled(MetaReadReq, params)
        self.meta_write = Decoupled(MetaWriteReq, params)
        self.meta_resp = Signal(CacheState)
        self.way_en = Signal(self.n_ways)
        self.wb_req = Decoupled(WritebackReq, params)

        self.wb_rdy = Signal()
        self.mshr_rdy = Signal()
        self.mshr_wb_rdy = Signal()

        self.state = Valid(Signal, 32)

    def elaborate(self, platform):
        m = Module()

        req = tl.ChannelB(addr_width=32,
                          data_width=self.row_bits,
                          size_width=bits_for(self.lg_block_bytes))
        req_idx = self.addr_index(req.address)
        req_tag = self.addr_tag(req.address)

        way_en = Signal.like(self.way_en)
        tag_matches = way_en != 0
        old_state = Signal(CacheState)
        is_dirty, report_param, new_state = CacheState.on_probe(
            m, old_state, req.param)

        m.d.comb += [
            self.state.valid.eq(1),
            self.state.bits.eq(req.address),
            self.mshr_wb_rdy.eq(1),
        ]

        m.d.comb += [
            self.meta_write.bits.way_en.eq(way_en),
            self.meta_write.bits.idx.eq(req_idx),
            self.meta_write.bits.tag.eq(req_tag),
            self.meta_write.bits.state.eq(new_state),
        ]

        with m.FSM():
            with m.State('IDLE'):
                m.d.comb += [
                    self.state.valid.eq(0),
                    self.req.ready.eq(1),
                ]

                with m.If(self.req.fire):
                    m.d.sync += req.eq(self.req.bits)
                    m.next = 'META_READ'

            with m.State('META_READ'):
                m.d.comb += [
                    self.meta_read.valid.eq(1),
                    self.meta_read.bits.idx.eq(req_idx),
                    self.meta_read.bits.tag.eq(req_tag),
                ]
                with m.If(self.meta_read.fire):
                    m.next = 'META_RESP'

            with m.State('META_RESP'):
                m.next = 'MSHR_REQ'

            with m.State('MSHR_REQ'):
                m.d.sync += [
                    old_state.eq(self.meta_resp),
                    way_en.eq(self.way_en),
                ]

                with m.If(self.mshr_rdy & self.wb_rdy):
                    m.next = 'MSHR_RESP'
                with m.Else():
                    m.next = 'META_READ'

            with m.State('MSHR_RESP'):
                with m.If(tag_matches & is_dirty):
                    m.next = 'WRITEBACK_REQ'
                with m.Else():
                    m.next = 'RELEASE'

            with m.State('WRITEBACK_REQ'):
                m.d.comb += self.mshr_wb_rdy.eq(0)

                m.d.comb += [
                    self.wb_req.valid.eq(1),
                    # self.wb_req.bits.source.eq(req.source),
                    self.wb_req.bits.idx.eq(req_idx),
                    self.wb_req.bits.tag.eq(req_tag),
                    self.wb_req.bits.param.eq(report_param),
                    self.wb_req.bits.way_en.eq(way_en),
                    self.wb_req.bits.voluntary.eq(0),
                ]

                with m.If(self.wb_req.fire):
                    m.next = 'WRITEBACK_RESP'

            with m.State('WRITEBACK_RESP'):
                m.d.comb += self.mshr_wb_rdy.eq(0)

                with m.If(self.wb_req.ready):
                    m.next = 'META_WRITE'

            with m.State('META_WRITE'):
                m.d.comb += [
                    self.mshr_wb_rdy.eq(0),
                    self.meta_write.valid.eq(1),
                ]

                with m.If(self.meta_write.fire):
                    m.next = 'META_WRITE_RESP'

            with m.State('META_WRITE_RESP'):
                m.d.comb += self.mshr_wb_rdy.eq(0)

                m.next = 'IDLE'

            with m.State('RELEASE'):
                m.d.comb += self.mshr_wb_rdy.eq(0)

                m.d.comb += [
                    self.mem_release.valid.eq(1),
                    self.mem_release.bits.opcode.eq(
                        tl.ChannelCOpcode.ProbeAck),
                    self.mem_release.bits.param.eq(report_param),
                    self.mem_release.bits.address.eq(req.address),
                    self.mem_release.bits.source.eq(req.source),
                ]

                with m.If(self.mem_release.fire):
                    with m.If(tag_matches):
                        m.next = 'META_WRITE'
                    with m.Else():
                        m.next = 'IDLE'

        return m


#
# MSHR
#


class MSHRReq(HasDCacheParams, DCacheReq):

    def __init__(self, params, name=None, src_loc_at=0):
        super().__init__(params=params, name=name, src_loc_at=src_loc_at + 2)

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

    def __init__(self, id, params, sink_id_width=1):
        super().__init__(params)

        self.id = id
        self.sink_id_width = sink_id_width

        self.mem_acquire = Decoupled(tl.ChannelA,
                                     addr_width=32,
                                     data_width=self.row_bits,
                                     size_width=bits_for(self.lg_block_bytes),
                                     source_id_width=self.source_id_bits)

        self.mem_grant = Decoupled(tl.ChannelD,
                                   data_width=self.row_bits,
                                   size_width=bits_for(self.lg_block_bytes),
                                   sink_id_width=sink_id_width)

        self.mem_finish = Decoupled(tl.ChannelE, sink_id_width=sink_id_width)

        self.req = MSHRReq(params)
        self.req_is_probe = Signal()
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
        self.lb_resp = Signal(len(self.mem_acquire.bits.data))
        self.lb_write = Decoupled(LineBufferWriteReq, self.params)

        self.wb_req = Decoupled(WritebackReq, self.params)
        self.wb_resp = Signal()

        self.resp = Decoupled(DCacheResp, params)

        self.br_update = BranchUpdate(params)
        self.exception = Signal()

        self.prober_state = Valid(Signal, 32)
        self.probe_rdy = Signal()

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

        load_gen = m.submodules.load_gen = LoadGen(max_size=self.xlen // 8)

        grantack = Valid(tl.ChannelE, sink_id_width=self.sink_id_width)
        grant_had_data = Signal()
        commit_line = Signal()

        new_state = Signal(CacheState)

        _, grow_param = CacheState.on_access(m, new_state, req.uop.mem_cmd)
        _, shrink_param, _ = CacheState.on_cache_control(
            m, req.old_meta.state, MemoryCommand.WRITE)
        grant_state = CacheState.on_grant(m, req.uop.mem_cmd,
                                          self.mem_grant.bits.param)

        need_sec_acq, is_hit_again, dirtier_state, dirtier_cmd = CacheState.on_sec_access(
            m, new_state, req.uop.mem_cmd, self.req.uop.mem_cmd)
        block_sec_req = Signal()
        sec_ready = ~need_sec_acq & ~self.req_is_probe & ~block_sec_req

        m.d.comb += self.req_sec_ready.eq(sec_ready & rpq.w_rdy)

        with m.If(self.req_sec_valid & self.req_sec_ready):
            m.d.sync += req.uop.mem_cmd.eq(dirtier_cmd)
            with m.If(is_hit_again):
                m.d.sync += new_state.eq(dirtier_state)

        probe_safe = Signal()
        meta_hazard = Signal(2)
        with m.If(meta_hazard != 0):
            m.d.sync += meta_hazard.eq(meta_hazard + 1)
        with m.If(self.meta_write.fire):
            m.d.sync += meta_hazard.eq(1)
        m.d.comb += self.probe_rdy.eq((meta_hazard == 0) & probe_safe)

        with m.FSM():
            with m.State('INVALID'):
                m.d.comb += [
                    state_invalid.eq(1),
                    block_sec_req.eq(1),
                    self.req_pri_ready.eq(1),
                    probe_safe.eq(1),
                ]
                m.d.sync += [
                    grant_had_data.eq(0),
                    grantack.valid.eq(0),
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
                            m.d.sync += new_state.eq(self.req.old_meta.state)
                            m.next = 'REFILL_REQ'

                    with m.Else():
                        m.d.sync += new_state.eq(CacheState.NOTHING)
                        m.next = 'REFILL_REQ'

            with m.State('REFILL_REQ'):
                m.d.comb += probe_safe.eq(1)

                m.d.comb += [
                    self.mem_acquire.bits.opcode.eq(
                        tl.ChannelAOpcode.AcquireBlock),
                    self.mem_acquire.bits.param.eq(grow_param),
                    self.mem_acquire.bits.size.eq(self.lg_block_bytes),
                    self.mem_acquire.bits.source.eq(self.make_source(self.id)),
                    self.mem_acquire.bits.address.eq(
                        Cat(Const(0, self.block_off_bits), req_idx, req_tag)),
                    self.mem_acquire.bits.mask.eq(~0),
                    self.mem_acquire.valid.eq(1),
                ]

                with m.If(self.mem_acquire.fire):
                    m.next = 'REFILL_RESP'

            with m.State('REFILL_RESP'):
                m.d.comb += probe_safe.eq(1)

                with m.If(tl.Interface.has_data(self.mem_grant.bits)):

                    m.d.comb += [
                        self.lb_write.valid.eq(self.mem_grant.valid),
                        self.lb_write.bits.id.eq(self.id),
                        self.lb_write.bits.offset.eq(refill_counter),
                        self.lb_write.bits.data.eq(self.mem_grant.bits.data),
                        self.mem_grant.ready.eq(self.lb_write.ready),
                    ]
                with m.Else():
                    m.d.comb += self.mem_grant.ready.eq(1)

                with m.If(self.mem_grant.fire):
                    m.d.sync += grant_had_data.eq(
                        tl.Interface.has_data(self.mem_grant.bits))

                    with m.If((refill_counter == (self.refill_cycles - 1))
                              | (self.mem_grant.bits.opcode ==
                                 tl.ChannelDOpcode.Grant)):
                        m.d.sync += [
                            grantack.valid.eq(
                                tl.Interface.is_request(self.mem_grant.bits)),
                            grantack.bits.sink.eq(self.mem_grant.bits.sink),
                            refill_counter.eq(0),
                            new_state.eq(grant_state),
                            commit_line.eq(0),
                        ]
                        m.d.comb += refill_done.eq(1)

                        with m.If(grant_had_data):
                            m.next = 'DRAIN_LOAD'
                        with m.Else():
                            m.next = 'DRAIN_REPLAY'

                    with m.Else():
                        m.d.sync += refill_counter.eq(refill_counter + 1)

            with m.State('DRAIN_LOAD'):
                m.d.comb += probe_safe.eq(1)

                drain_load = MemoryCommand.is_read(
                    rpq.r_data.uop.mem_cmd) & ~MemoryCommand.is_write(
                        rpq.r_data.uop.mem_cmd) & (rpq.r_data.uop.mem_cmd
                                                   != MemoryCommand.LR)

                rp_addr = Cat(self.addr_block_offset(rpq.r_data.addr), req_idx,
                              req_tag)
                word_idx = 0
                if self.row_words > 1:
                    word_idx = rp_addr[log2_int(self.word_bits //
                                                8):log2_int(self.word_bits *
                                                            self.row_words //
                                                            8)]

                m.d.comb += [
                    load_gen.typ.eq(rpq.r_data.uop.mem_size),
                    load_gen.signed.eq(rpq.r_data.uop.mem_signed),
                    load_gen.addr.eq(rp_addr),
                    load_gen.data_in.eq(self.lb_resp >> (
                        word_idx << log2_int(self.word_bits))),
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
                    self.resp.bits.from_core.eq(rpq.r_data.from_core),
                ]

                with m.If(rpq.r_en & rpq.r_rdy):
                    m.d.sync += commit_line.eq(1)
                with m.Elif(rpq.empty & ~commit_line):
                    with m.If(~(rpq.w_en & rpq.w_rdy)):
                        m.next = 'MEM_FINISH_1'
                with m.Elif(rpq.empty | (rpq.r_rdy & ~drain_load)):
                    m.next = 'META_READ'

            with m.State('META_READ'):
                m.d.comb += probe_safe.eq(grantack.valid)

                m.d.comb += [
                    self.meta_read.valid.
                    eq(~self.prober_state.valid | ~grantack.valid
                       | (self.addr_index(self.prober_state.bits) != req_idx)),
                    self.meta_read.bits.idx.eq(req_idx),
                    self.meta_read.bits.tag.eq(req_tag),
                    self.meta_read.bits.way_en.eq(req.way_en),
                ]

                with m.If(self.meta_read.fire):
                    m.next = 'META_RESP_1'

            with m.State('META_RESP_1'):
                m.next = 'META_RESP_2'

            with m.State('META_RESP_2'):
                needs_wb, _, _ = CacheState.on_cache_control(
                    m, self.meta_resp.bits.state, MemoryCommand.WRITE)

                with m.If(~self.meta_resp.valid):
                    m.next = 'META_READ'
                with m.Elif(needs_wb):
                    m.next = 'META_CLEAR'
                with m.Else():
                    m.next = 'COMMIT_LINE'

            with m.State('META_CLEAR'):
                m.d.comb += [
                    self.meta_write.valid.eq(1),
                    self.meta_write.bits.idx.eq(req_idx),
                    self.meta_write.bits.state.eq(CacheState.NOTHING),
                    self.meta_write.bits.tag.eq(req_tag),
                    self.meta_write.bits.way_en.eq(req.way_en),
                ]

                with m.If(self.meta_write.fire):
                    m.next = 'WB_REQ'

            with m.State('WB_REQ'):
                m.d.comb += [
                    self.wb_req.valid.eq(1),
                    self.wb_req.bits.tag.eq(req.old_meta.tag),
                    self.wb_req.bits.idx.eq(req_idx),
                    self.wb_req.bits.way_en.eq(req.way_en),
                    self.wb_req.bits.param.eq(shrink_param),
                    self.wb_req.bits.voluntary.eq(1),
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
                    self.refill.bits.wmask.eq(Repl(1, self.row_words)),
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
                    m.next = 'MEM_FINISH_1'

            with m.State('MEM_FINISH_1'):
                m.d.comb += [
                    block_sec_req.eq(1),
                    self.mem_finish.valid.eq(grantack.valid),
                    self.mem_finish.bits.eq(grantack.bits),
                ]

                with m.If(self.mem_finish.fire | ~grantack.valid):
                    m.d.sync += grantack.valid.eq(0)
                    m.next = 'MEM_FINISH_2'

            with m.State('MEM_FINISH_2'):
                m.d.comb += block_sec_req.eq(1)
                m.next = 'INVALID'

        return m


class IOMSHR(HasDCacheParams, Elaboratable):

    def __init__(self, id, params):
        super().__init__(params)

        self.id = id

        self.mem_access = Decoupled(tl.ChannelA,
                                    addr_width=32,
                                    data_width=self.row_bits,
                                    size_width=bits_for(self.lg_block_bytes),
                                    source_id_width=self.source_id_bits)

        self.mem_ack = Decoupled(tl.ChannelD,
                                 data_width=self.row_bits,
                                 size_width=bits_for(self.lg_block_bytes),
                                 source_id_width=self.source_id_bits)

        self.req = Decoupled(DCacheReq, params)
        self.resp = Decoupled(DCacheResp, params)

    def elaborate(self, platform):
        m = Module()

        req = DCacheReq(self.params)

        resp_data = Signal.like(self.mem_ack.bits.data)

        load_gen = m.submodules.load_gen = LoadGen(max_size=self.row_bits // 8)
        m.d.comb += [
            load_gen.typ.eq(req.uop.mem_size),
            load_gen.signed.eq(req.uop.mem_signed),
            load_gen.addr.eq(req.addr),
            load_gen.data_in.eq(resp_data),
        ]

        store_gen = m.submodules.store_gen = StoreGen(max_size=self.row_bits //
                                                      8)
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
                    self.mem_access.bits.opcode.eq(
                        Mux(req.uop.mem_cmd == MemoryCommand.WRITE,
                            tl.ChannelAOpcode.PutPartialData,
                            tl.ChannelAOpcode.Get)),
                    self.mem_access.bits.size.eq(req.uop.mem_size),
                    self.mem_access.bits.source.eq(self.make_source(self.id)),
                    self.mem_access.bits.address.eq(req.addr),
                    self.mem_access.bits.mask.eq(store_gen.mask),
                    self.mem_access.bits.data.eq(store_gen.data_out),
                    self.mem_access.valid.eq(1),
                ]

                with m.If(self.mem_access.fire):
                    m.next = 'MEM_ACK'

            with m.State('MEM_ACK'):
                with m.If(self.mem_ack.valid):
                    with m.If(send_resp):
                        m.d.sync += resp_data.eq(self.mem_ack.bits.data)

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

    def __init__(self, params, sink_id_width=1):
        super().__init__(params)

        self.sink_id_width = sink_id_width

        self.mem_acquire = Decoupled(tl.ChannelA,
                                     addr_width=32,
                                     data_width=self.row_bits,
                                     size_width=bits_for(self.lg_block_bytes),
                                     source_id_width=self.source_id_bits)

        self.mem_grant = Decoupled(tl.ChannelD,
                                   data_width=self.row_bits,
                                   size_width=bits_for(self.lg_block_bytes),
                                   sink_id_width=sink_id_width)

        self.mem_access = Decoupled(tl.ChannelA,
                                    addr_width=32,
                                    data_width=self.row_bits,
                                    size_width=bits_for(self.lg_block_bytes),
                                    source_id_width=self.source_id_bits)

        self.mem_ack = Decoupled(tl.ChannelD,
                                 data_width=self.row_bits,
                                 size_width=bits_for(self.lg_block_bytes),
                                 source_id_width=self.source_id_bits)

        self.mem_finish = Decoupled(tl.ChannelE, sink_id_width=sink_id_width)

        self.req = [
            Decoupled(MSHRReq, params, name=f'req{i}')
            for i in range(self.mem_width)
        ]
        self.req_is_probe = Signal(self.mem_width)

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

        self.prober_state = Valid(Signal, 32)
        self.probe_rdy = Signal()

    def elaborate(self, platform):
        m = Module()

        req_idx = Signal(range(self.mem_width))
        req = MSHRReq(self.params)
        req_valid = Signal()
        req_is_probe = self.req_is_probe[0]

        m.d.comb += self.probe_rdy.eq(1)

        for w in reversed(range(self.mem_width)):
            with m.If(self.req[w].valid):
                m.d.comb += [
                    req_idx.eq(w),
                    req.eq(self.req[w].bits),
                    req_valid.eq(1),
                ]

        req_uncacheable = Signal()
        pma = m.submodules.pma = PMAChecker(self.params)
        m.d.comb += [
            pma.paddr.eq(req.addr),
            req_uncacheable.eq(~pma.resp.cacheable),
        ]

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

        lb_mem = Memory(width=len(self.mem_acquire.bits.data),
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

        mem_acquire_arbiter = m.submodules.mem_acquire_arbiter = tl.Arbiter(
            tl.ChannelA,
            addr_width=32,
            data_width=self.row_bits,
            size_width=bits_for(self.lg_block_bytes),
            source_id_width=self.source_id_bits)

        mem_access_arbiter = m.submodules.mem_access_arbiter = tl.Arbiter(
            tl.ChannelA,
            addr_width=32,
            data_width=self.row_bits,
            size_width=bits_for(self.lg_block_bytes),
            source_id_width=self.source_id_bits)

        mem_finish_arbiter = m.submodules.mem_finish_arbiter = tl.Arbiter(
            tl.ChannelE, sink_id_width=self.sink_id_width)

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
            mshr = MSHR(i, self.params, sink_id_width=self.sink_id_width)
            setattr(m.submodules, f'mshr{i}', mshr)

            mem_acquire_arbiter.add(mshr.mem_acquire)
            mem_finish_arbiter.add(mshr.mem_finish)

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
                mshr.req_is_probe.eq(req_is_probe),
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

            with m.If(self.get_source_id(self.mem_grant.bits.source) == i):
                m.d.comb += self.mem_grant.connect(mshr.mem_grant)

            m.d.comb += mshr.resp.connect(resp_arb.inp[i])

            m.d.comb += mshr.prober_state.eq(self.prober_state)
            for w in range(self.mem_width):
                with m.If(~mshr.probe_rdy & idx_matches[w][i]
                          & self.req_is_probe[w]):
                    m.d.comb += self.probe_rdy.eq(0)

            mshrs.append(mshr)

        for i in reversed(range(self.n_mshrs)):
            with m.If(mshrs[i].req_pri_ready):
                m.d.comb += mshr_alloc_idx.eq(i)

        iomshr_alloc_arb = m.submodules.iomshr_alloc_arb = Arbiter(
            self.n_iomshrs, Signal)

        mmio_ready = 0

        io_mshrs = []
        for i in range(self.n_iomshrs):
            mshr_id = self.n_mshrs + i
            mshr = IOMSHR(mshr_id, self.params)
            setattr(m.submodules, f'iomshr{i}', mshr)

            mem_access_arbiter.add(mshr.mem_access)

            mmio_ready |= mshr.req.ready

            m.d.comb += [
                iomshr_alloc_arb.inp[i].valid.eq(mshr.req.ready),
                mshr.req.valid.eq(iomshr_alloc_arb.inp[i].ready),
                mshr.req.bits.uop.eq(req.uop),
                mshr.req.bits.addr.eq(req.addr),
                mshr.req.bits.data.eq(req.data),
            ]

            m.d.comb += [
                mshr.mem_ack.bits.eq(self.mem_ack.bits),
                mshr.mem_ack.valid.eq(
                    self.mem_ack.valid
                    &
                    (self.get_source_id(self.mem_ack.bits.source) == mshr_id)),
            ]
            with m.If(self.get_source_id(self.mem_ack.bits.source) == mshr_id):
                m.d.comb += self.mem_ack.ready.eq(1)

            m.d.comb += mshr.resp.connect(resp_arb.inp[self.n_mshrs + i])

            io_mshrs.append(mshr)

        m.d.comb += iomshr_alloc_arb.out.ready.eq(req_valid & req_uncacheable)

        m.d.comb += [
            mem_acquire_arbiter.bus.connect(self.mem_acquire),
            mem_access_arbiter.bus.connect(self.mem_access),
            mem_finish_arbiter.bus.connect(self.mem_finish),
        ]

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

    def __init__(self, dbus, dbus_mmio, params):
        super().__init__(params)

        if not dbus.has_bce:
            raise ValueError('Data bus must have BCE channels')

        self.dbus = dbus
        self.dbus_mmio = dbus_mmio

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
        self.s1_paddr = [
            Signal(self.paddr_bits, name=f's1_paddr{w}')
            for w in range(self.mem_width)
        ]

        self.br_update = BranchUpdate(params)
        self.exception = Signal()

    def elaborate(self, platform):
        m = Module()

        wb = m.submodules.wb = WritebackUnit(self.params)
        prober = m.submodules.prober = ProbeUnit(self.params)
        mshrs = m.submodules.mshrs = MSHRFile(self.params,
                                              self.dbus.sink_id_width)

        m.d.comb += [
            mshrs.br_update.eq(self.br_update),
            mshrs.exception.eq(self.exception),
        ]

        m.d.comb += [
            mshrs.mem_acquire.connect(self.dbus.a),
            mshrs.mem_access.connect(self.dbus_mmio.a),
        ]

        mem_release_arbiter = m.submodules.mem_release_arbiter = tl.Arbiter(
            tl.ChannelC,
            addr_width=32,
            data_width=self.row_bits,
            size_width=bits_for(self.lg_block_bytes),
            source_id_width=self.source_id_bits)
        mem_release_arbiter.add(wb.mem_release)
        mem_release_arbiter.add(prober.mem_release)
        m.d.comb += mem_release_arbiter.bus.connect(self.dbus.c)

        with m.If(
                self.get_source_id(self.dbus.d.bits.source) == self.n_mshrs +
                self.n_iomshrs):
            m.d.comb += [
                self.dbus.d.ready.eq(1),
                mshrs.mem_grant.valid.eq(0),
            ]
        with m.Else():
            m.d.comb += self.dbus.d.connect(mshrs.mem_grant)

        m.d.comb += self.dbus_mmio.d.connect(mshrs.mem_ack)

        m.d.comb += wb.mem_grant.eq(self.dbus.d.fire & (self.get_source_id(
            self.dbus.d.bits.source) == self.n_mshrs + self.n_iomshrs))

        m.d.comb += mshrs.mem_finish.connect(self.dbus.e)

        #
        # Tag & data access
        #

        # 0 - MSHR refill, 1 - Prober
        meta_write_arb = m.submodules.meta_write_arb = Arbiter(
            2, MetaWriteReq, self.params)
        # 0 - MSHR replay, 1 - Prober, 2 - WB, 3 - MSHR meta read, 4 - LSU
        meta_read_arb = m.submodules.meta_read_arb = Arbiter(
            5, L1MetaReadReq, self.params)

        meta = []
        for w in range(self.mem_width):
            array = MetadataArray(self.params)
            setattr(m.submodules, f'meta{w}', array)

            m.d.comb += [
                array.write.valid.eq(meta_write_arb.out.fire),
                array.write.bits.eq(meta_write_arb.out.bits),
                array.read.valid.eq(meta_read_arb.out.valid),
                array.read.bits.eq(meta_read_arb.out.bits.req[w]),
            ]
            meta.append(array)

        m.d.comb += [
            meta_read_arb.out.ready.eq(Cat(m.read.ready for m in meta) != 0),
            meta_write_arb.out.ready.eq(Cat(m.write.ready for m in meta) != 0),
        ]

        data = DuplicatedDataArray(
            self.params) if self.n_banks == 1 else BankedDataArray(self.params)
        m.submodules.data = data

        # 0 - LSU, 1 - MSHR refill
        data_write_arb = m.submodules.data_write_arb = Arbiter(
            2, L1DataWriteReq, self.params)

        # 0 - MSHR replay, 1 - WB, 2 - LSU
        data_read_arb = m.submodules.data_read_arb = Arbiter(
            3, L1DataReadReq, self.params)

        for w in range(self.mem_width):
            m.d.comb += [
                data.read[w].valid.eq(data_read_arb.out.bits.valid[w]
                                      & data_read_arb.out.valid),
                data.read[w].bits.eq(data_read_arb.out.bits.req[w]),
                data.write[w].valid.eq(data_write_arb.out.bits.valid[w]
                                       & data_write_arb.out.valid),
                data.write[w].bits.eq(data_write_arb.out.bits.req[w]),
            ]
        m.d.comb += [
            data_read_arb.out.ready.eq(1),
            data_write_arb.out.ready.eq(1),
        ]

        #
        # Incoming requests
        #

        req_ready = meta_read_arb.inp[4].ready & data_read_arb.inp[2].ready
        m.d.comb += [
            meta_read_arb.inp[4].valid.eq(Cat(r.valid for r in self.req) != 0),
            data_read_arb.inp[2].valid.eq(Cat(r.valid for r in self.req) != 0),
        ]
        for w in range(self.mem_width):
            m.d.comb += [
                self.req[w].ready.eq(req_ready),
                meta_read_arb.inp[4].bits.req[w].idx.eq(
                    self.addr_index(self.req[w].bits.addr)),
                data_read_arb.inp[2].bits.valid[w].eq(self.req[w].valid),
                data_read_arb.inp[2].bits.req[w].addr.eq(
                    self.req[w].bits.addr),
                data_read_arb.inp[2].bits.req[w].way_en.eq(Repl(
                    1, self.n_ways)),
                data_read_arb.inp[2].bits.req[w].write_intent.eq(
                    self.req[w].bits.uop.mem_cmd == MemoryCommand.WRITE),
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
            mshr_replay_req[0].from_core.eq(mshrs.replay.bits.from_core),
            mshrs.replay.ready.eq(meta_read_arb.inp[0].ready
                                  & data_read_arb.inp[0].ready),
            meta_read_arb.inp[0].valid.eq(mshrs.replay.valid),
            meta_read_arb.inp[0].bits.req[0].idx.eq(
                self.addr_index(mshrs.replay.bits.addr)),
            data_read_arb.inp[0].valid.eq(mshrs.replay.valid),
            data_read_arb.inp[0].bits.req[0].addr.eq(mshrs.replay.bits.addr),
            data_read_arb.inp[0].bits.req[0].way_en.eq(
                mshrs.replay.bits.way_en),
            data_read_arb.inp[0].bits.valid[0].eq(1),
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
            mshrs.meta_read.ready.eq(meta_read_arb.inp[3].ready),
            meta_read_arb.inp[3].valid.eq(mshrs.meta_read.valid),
            meta_read_arb.inp[3].bits.req[0].eq(mshrs.meta_read.bits),
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
            meta_read_arb.inp[2].valid.eq(wb.meta_read.valid),
            meta_read_arb.inp[2].bits.req[0].eq(wb.meta_read.bits),
            data_read_arb.inp[1].valid.eq(wb.data_read.valid),
            data_read_arb.inp[1].bits.req[0].eq(wb.data_read.bits),
            data_read_arb.inp[1].bits.valid[0].eq(1),
            wb.meta_read.ready.eq(meta_read_arb.inp[2].ready
                                  & data_read_arb.inp[1].ready),
            wb.data_read.ready.eq(meta_read_arb.inp[2].ready
                                  & data_read_arb.inp[1].ready),
        ]

        #
        # Prober
        #
        prober_fire = prober.meta_read.fire
        prober_req = [
            DCacheReq(self.params, name=f'prober_req{i}')
            for i in range(self.mem_width)
        ]
        m.d.comb += [
            prober_req[0].addr.eq(
                Cat(prober.meta_read.bits.idx, prober.meta_read.bits.tag) <<
                self.block_off_bits),
            meta_read_arb.inp[1].valid.eq(prober.meta_read.valid),
            meta_read_arb.inp[1].bits.req[0].eq(prober.meta_read.bits),
            prober.meta_read.ready.eq(meta_read_arb.inp[1].ready),
        ]

        #
        # S0 - Request arbitration
        #

        s0_valid = Mux(
            Cat(r.fire for r in self.req) != 0, Cat(r.valid for r in self.req),
            Mux(
                wb_fire | mshrs.meta_read.fire | mshrs.replay.fire
                | prober_fire, Const(1, self.mem_width), 0))
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
            with m.Elif(prober_fire):
                m.d.comb += s0_req[w].eq(prober_req[w])
            with m.Elif(mshrs.meta_read.fire):
                m.d.comb += s0_req[w].eq(mshr_read_req[w])
            with m.Else():
                m.d.comb += s0_req[w].eq(mshr_replay_req[w])

        m.d.comb += s0_type.eq(
            Mux(
                Cat(r.fire for r in self.req) != 0, DCacheReqType.LSU,
                Mux(
                    wb_fire, DCacheReqType.WRITEBACK,
                    Mux(
                        prober_fire, DCacheReqType.PROBE,
                        Mux(mshrs.meta_read.fire, DCacheReqType.MSHR_META,
                            DCacheReqType.REPLAY)))))

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
        s1_nack = Cat((self.addr_index(addr) == prober.meta_write.bits.idx)
                      & ~prober.req.ready for addr in s1_addr)
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

            m.d.comb += s1_addr[w].eq(
                Mux(self.use_vm & (s1_type == DCacheReqType.LSU),
                    self.s1_paddr[w], s1_req[w].addr))

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
        s2_nack = Signal(self.mem_width)
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
                s2_req[w].addr.eq(s1_addr[w]),
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
                 & (s2_new_hit_state[w] == s2_hit_state[w])
                 & ~mshrs.block_hit[w])
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

        lrsc_count = Signal(range(self.lrsc_cycles))
        lrsc_valid = lrsc_count > 3
        lrsc_addr = Signal.like(s2_req[0].addr)
        s2_lrsc_nack = Signal()
        s2_do_lr = (s2_req[0].uop.mem_cmd
                    == MemoryCommand.LR) & (~s2_lrsc_nack |
                                            (s2_type == DCacheReqType.REPLAY))
        s2_do_sc = (s2_req[0].uop.mem_cmd
                    == MemoryCommand.SC) & (~s2_lrsc_nack |
                                            (s2_type == DCacheReqType.REPLAY))
        s2_do_debug_lr = Signal()
        m.d.comb += s2_do_debug_lr.eq(
            s2_req[0].uop.mem_cmd == MemoryCommand.LR)
        s2_lrsc_addr_match = Cat(lrsc_valid
                                 & (s2_req[w].addr[self.block_off_bits:] ==
                                    lrsc_addr[self.block_off_bits:])
                                 for w in range(self.mem_width))
        s2_sc_fail = s2_do_sc & ~s2_lrsc_addr_match[0]

        m.d.sync += s2_lrsc_nack.eq(s1_nack[0])
        with m.If(lrsc_count > 0):
            m.d.sync += lrsc_count.eq(lrsc_count - 1)

        with m.If(s2_valid[0]
                  & (((s2_type == DCacheReqType.LSU) & s2_hit[0] & ~s2_nack[0])
                     | (s2_type == DCacheReqType.REPLAY))):
            with m.If(s2_do_lr):
                m.d.sync += [
                    lrsc_count.eq(self.lrsc_cycles - 1),
                    lrsc_addr.eq(s2_req[0].addr),
                ]
            with m.If(lrsc_count > 0):
                m.d.sync += lrsc_count.eq(0)

        for w in range(self.mem_width):
            with m.If(s2_valid[w] & (s2_type == DCacheReqType.LSU) & ~s2_hit[w]
                      & ~(s2_state_is_hit[w] & s2_tag_match[w])
                      & s2_lrsc_addr_match[w] & ~s2_nack[w]):
                m.d.sync += lrsc_count.eq(0)

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

        s2_nack_probe = Signal(self.mem_width)
        s2_nack_victim = Signal(self.mem_width)
        s2_nack_mshr = Signal(self.mem_width)
        s2_nack_data = Signal(self.mem_width)
        s2_nack_wb = Signal(self.mem_width)

        m.d.sync += s2_nack_probe.eq(s1_nack)
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

        m.d.comb += s2_nack.eq(
            Cat((s2_nack_mshr[w] | s2_nack_probe[w] | s2_nack_victim[w]
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

        if self.is_groom:
            m.d.comb += s2_store_failed.eq(0)
        else:
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
                    & ~s2_nack_victim[w] & ~s2_nack_wb[w] & ~s2_nack_probe[w]
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
                mshrs.req[w].bits.from_core.eq(s2_req[w].from_core),
                mshrs.req[w].bits.tag_match.eq(s2_tag_match[w]),
                mshrs.req[w].bits.way_en.eq(
                    Mux(s2_tag_match[w], s2_tag_match_way[w],
                        s2_replace_way_en)),
                mshrs.req[w].bits.old_meta.tag.eq(s2_replace_meta[w].tag),
                mshrs.req[w].bits.old_meta.state.eq(
                    Mux(s2_tag_match[w], s2_hit_state[w],
                        s2_replace_meta[w].state)),
                mshrs.req_is_probe[w].eq(s2_valid[w]
                                         & (s2_type == DCacheReqType.PROBE)),
            ]

        m.d.comb += mshrs.meta_resp.valid.eq(~s2_nack_probe[0]
                                             | prober.mshr_wb_rdy)
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
            data_write_arb.inp[1].valid.eq(mshrs.refill.valid),
            data_write_arb.inp[1].bits.req[0].eq(mshrs.refill.bits),
            data_write_arb.inp[1].bits.valid[0].eq(1),
            mshrs.refill.ready.eq(data_write_arb.inp[1].ready),
        ]

        #
        # Writeback
        #

        wb_arbiter = m.submodules.wb_arbiter = Arbiter(2, WritebackReq,
                                                       self.params)
        m.d.comb += wb_arbiter.out.connect(wb.req)

        m.d.comb += [
            mshrs.wb_req.connect(wb_arbiter.inp[1]),
            wb.data_resp.eq(s2_data_muxed[0]),
            mshrs.wb_resp.eq(wb.resp),
        ]

        #
        # Prober
        #

        m.d.comb += [
            prober.req.valid.eq(self.dbus.b.valid),
            self.dbus.b.ready.eq(prober.req.ready),
            prober.req.bits.address.eq(self.dbus.b.bits.address),
            prober.req.bits.param.eq(self.dbus.b.bits.param),
            prober.req.bits.source.eq(self.dbus.b.bits.source),
            prober.way_en.eq(s2_tag_match_way[0]),
            prober.meta_resp.eq(s2_hit_state[0]),
            prober.meta_write.connect(meta_write_arb.inp[1]),
            prober.wb_req.connect(wb_arbiter.inp[0]),
            prober.wb_rdy.eq(~wb.idx.valid
                             | (prober.meta_write.bits.idx != wb.idx.bits)),
            prober.mshr_rdy.eq(mshrs.probe_rdy),
            mshrs.prober_state.eq(prober.state),
        ]

        #
        # Response selection
        #

        s2_data_word_prebypass = [
            Signal(self.word_bits, name=f's2_data_word_prebypass{i}')
            for i in range(self.mem_width)
        ]

        for w in range(self.mem_width):
            word_idx = 0
            if self.row_words > 1:
                word_idx = s2_req[w].addr[log2_int(self.word_bits // 8
                                                   ):log2_int(self.word_bits *
                                                              self.row_words //
                                                              8)]

            m.d.comb += s2_data_word_prebypass[w].eq(
                s2_data_muxed[w] >> (word_idx << log2_int(self.word_bits)))

        s2_data_word = [
            Signal(self.word_bits, name=f's2_data_word{i}')
            for i in range(self.mem_width)
        ]

        cache_resp = [
            Valid(DCacheResp, self.params, name=f'cache_resp{i}')
            for i in range(self.mem_width)
        ]

        for w in range(self.mem_width):
            load_gen = LoadGen(max_size=self.xlen // 8)
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
                cache_resp[w].bits.data.eq(
                    Mux((w == 0) & s2_do_sc, 0, load_gen.data_out)
                    | s2_sc_fail),
                cache_resp[w].bits.from_core.eq(s2_req[w].from_core),
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

        s3_req = [
            DCacheReq(self.params, name=f's3_req{i}')
            for i in range(self.mem_width)
        ]
        s3_valid = Signal(self.mem_width)
        s3_way = [
            Signal.like(s2_tag_match_way[0], name=f's3_way{i}')
            for i in range(self.mem_width)
        ]

        for w in range(self.mem_width):
            amo_gen = AMODataGen(self.xlen)
            store_gen = StoreGen(max_size=self.xlen // 8)
            setattr(m.submodules, f'amo_gen{w}', amo_gen)
            setattr(m.submodules, f'store_gen{w}', store_gen)

            m.d.comb += [
                store_gen.typ.eq(s2_req[w].uop.mem_size),
                store_gen.addr.eq(s2_req[w].addr),
                amo_gen.mask.eq(store_gen.mask),
                amo_gen.cmd.eq(s2_req[w].uop.mem_cmd),
                amo_gen.lhs.eq(s2_data_word[w]),
                amo_gen.rhs.eq(s2_req[w].data),
            ]

            m.d.sync += [
                s3_req[w].eq(s2_req[w]),
                s3_req[w].data.eq(amo_gen.out),
                s3_valid[w].eq(s2_valid[w] & s2_hit[w]
                               & MemoryCommand.is_write(s2_req[w].uop.mem_cmd)
                               & ~s2_sc_fail
                               & ~(s2_send_nack[w] & s2_nack[w])),
                s3_way[w].eq(s2_tag_match_way[w]),
            ]

            word_idx = 0
            if self.row_words > 1:
                word_idx = s3_req[w].addr[log2_int(self.word_bits // 8
                                                   ):log2_int(self.word_bits *
                                                              self.row_words //
                                                              8)]

            m.d.comb += [
                data_write_arb.inp[0].bits.req[w].addr.eq(s3_req[w].addr),
                data_write_arb.inp[0].bits.req[w].data.eq(
                    Repl(s3_req[w].data, self.row_words)),
                data_write_arb.inp[0].bits.req[w].way_en.eq(s3_way[w]),
                data_write_arb.inp[0].bits.req[w].wmask.eq(1 << word_idx),
                data_write_arb.inp[0].bits.valid[w].eq(s3_valid[w]),
            ]
        m.d.comb += data_write_arb.inp[0].valid.eq(s3_valid != 0)

        #
        # Load bypassing
        #

        s4_req = [
            DCacheReq(self.params, name=f's4_req{i}')
            for i in range(self.mem_width)
        ]
        s4_valid = Signal.like(s3_valid)
        s5_req = [
            DCacheReq(self.params, name=f's5_req{i}')
            for i in range(self.mem_width)
        ]
        s5_valid = Signal.like(s4_valid)

        m.d.sync += [
            s4_valid.eq(s3_valid),
            s5_valid.eq(s4_valid),
        ]
        for w in range(self.mem_width):
            m.d.sync += [
                s4_req[w].eq(s3_req[w]),
                s5_req[w].eq(s4_req[w]),
            ]

            m.d.comb += s2_data_word[w].eq(s2_data_word_prebypass[w])

        for i in reversed(range(self.mem_width)):
            s3_bypass = Cat(s3_valid[i]
                            & (s2_req[w].addr[self.word_off_bits:] ==
                               s3_req[i].addr[self.word_off_bits:])
                            for w in range(self.mem_width))
            s4_bypass = Cat(s4_valid[i]
                            & (s2_req[w].addr[self.word_off_bits:] ==
                               s4_req[i].addr[self.word_off_bits:])
                            for w in range(self.mem_width))
            s5_bypass = Cat(s5_valid[i]
                            & (s2_req[w].addr[self.word_off_bits:] ==
                               s5_req[i].addr[self.word_off_bits:])
                            for w in range(self.mem_width))

            for w in range(self.mem_width):
                with m.If(s3_bypass[w]):
                    m.d.comb += s2_data_word[w].eq(s3_req[i].data)
                with m.Elif(s4_bypass[w]):
                    m.d.comb += s2_data_word[w].eq(s4_req[i].data)
                with m.Elif(s5_bypass[w]):
                    m.d.comb += s2_data_word[w].eq(s5_req[i].data)

        return m


class SimpleDCache(HasCoreParams, Elaboratable):

    def __init__(self, dbus, dbus_mmio, params):
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
