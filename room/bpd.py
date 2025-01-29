from amaranth import *
from amaranth import tracer
from amaranth.hdl.ast import ValueCastable
from amaranth.utils import log2_int
from amaranth.lib.coding import PriorityEncoder
import functools
import operator

from room.types import HasCoreParams
from room.branch import GlobalHistory

from roomsoc.interconnect.stream import Valid


class BranchPrediction(HasCoreParams, ValueCastable):

    def __init__(self, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.taken = Signal(name=f'{name}__taken')
        self.is_br = Signal(name=f'{name}__is_br')
        self.is_jal = Signal(name=f'{name}__is_jal')
        self.predicted_pc = Valid(Signal,
                                  self.vaddr_bits_extended,
                                  name=f'{name}__predicted_pc')

    @ValueCastable.lowermethod
    def as_value(self):
        return Cat(self.taken, self.is_br, self.is_jal, self.predicted_pc)

    def shape(self):
        return self.as_value().shape()

    def __len__(self):
        return len(Value.cast(self))

    def eq(self, rhs):
        return Value.cast(self).eq(Value.cast(rhs))


class BranchPredictions(HasCoreParams, ValueCastable):

    def __init__(self, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.pc = Signal(self.vaddr_bits_extended, name=f'{name}__pc')
        self.preds = [
            BranchPrediction(params, name=f'{name}__pred{w}')
            for w in range(self.fetch_width)
        ]

        self.meta = Signal(self.bpd_meta_length, name=f'{name}__meta')

    @ValueCastable.lowermethod
    def as_value(self):
        return Cat(self.pc, *self.preds, self.meta)

    def shape(self):
        return self.as_value().shape()

    def __len__(self):
        return len(Value.cast(self))

    def eq(self, rhs):
        return Value.cast(self).eq(Value.cast(rhs))


class BranchPredictionUpdate(HasCoreParams, Record):

    def __init__(self, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.is_mispredict_update = Signal(
            name=f'{name}__is_mispredict_update')
        self.is_repair_update = Signal(name=f'{name}__is_repair_update')

        self.pc = Signal(self.vaddr_bits_extended, name=f'{name}__pc')
        self.br_mask = Signal(self.fetch_width, name=f'{name}__br_mask')

        self.cfi_idx = Valid(Signal,
                             range(self.fetch_width),
                             name=f'{name}__cfi_idx')
        self.cfi_taken = Signal(name=f'{name}__cfi_taken')
        self.cfi_mispredicted = Signal(name=f'{name}__cfi_mispredicted')
        self.cfi_is_br = Signal(name=f'{name}__cfi_is_br')
        self.cfi_is_jal = Signal(name=f'{name}__cfi_is_jal')
        self.cfi_is_jalr = Signal(name=f'{name}__cfi_is_jalr')

        self.ghist = Signal(self.ghist_length, name=f'{name}__ghist')

        self.target = Signal(self.vaddr_bits_extended, name=f'{name}__target')

        self.meta = Signal(self.bpd_meta_length, name=f'{name}__meta')

    @property
    def is_commit_update(self):
        return ~self.is_mispredict_update & ~self.is_repair_update

    def eq(self, rhs):
        names = [
            'is_mispredict_update', 'is_repair_update', 'pc', 'br_mask',
            'cfi_idx', 'cfi_taken', 'cfi_mispredicted', 'cfi_is_br',
            'cfi_is_jal', 'cfi_is_jalr', 'ghist', 'target', 'meta'
        ]
        return [getattr(self, n).eq(getattr(rhs, n)) for n in names]


class BranchPredictionRequest(HasCoreParams):

    def __init__(self, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.pc = Signal(self.vaddr_bits_extended, name=f'{name}__pc')
        self.ghist = GlobalHistory(params, name=f'{name}__ghist')

    def eq(self, rhs):
        names = ['pc', 'ghist']
        return [getattr(self, n).eq(getattr(rhs, n)) for n in names]


class BranchPredictionResponse(HasCoreParams):

    def __init__(self, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.f1 = [
            BranchPrediction(params, name=f'{name}__f1_{w}')
            for w in range(self.fetch_width)
        ]
        self.f2 = [
            BranchPrediction(params, name=f'{name}__f2_{w}')
            for w in range(self.fetch_width)
        ]
        self.f3 = [
            BranchPrediction(params, name=f'{name}__f3_{w}')
            for w in range(self.fetch_width)
        ]

    def eq(self, rhs):
        names = ['f1', 'f2', 'f3']
        return [[a.eq(b) for a, b in zip(getattr(self, n), getattr(rhs, n))]
                for n in names]


class BaseBranchPredictorBank(HasCoreParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.f0_valid = Signal()
        self.f0_pc = Signal(self.vaddr_bits_extended)
        self.f0_mask = Signal(self.fetch_width)

        self.f1_ghist = Signal(self.ghist_length)

        self.resp_in = BranchPredictionResponse(params)
        self.resp = BranchPredictionResponse(params)

        self.f3_meta = None

        self.update = Valid(BranchPredictionUpdate, params)

    def elaborate(self, platform):
        m = Module()

        m.d.comb += self.resp.eq(self.resp_in)

        self.s0_idx = self.f0_pc >> log2_int(self.fetch_bytes)
        self.s1_idx = Signal.like(self.s0_idx)
        self.s2_idx = Signal.like(self.s0_idx)
        self.s3_idx = Signal.like(self.s0_idx)
        m.d.sync += [
            self.s1_idx.eq(self.s0_idx),
            self.s2_idx.eq(self.s1_idx),
            self.s3_idx.eq(self.s2_idx),
        ]

        self.s0_valid = self.f0_valid
        self.s1_valid = Signal()
        self.s2_valid = Signal()
        self.s3_valid = Signal()
        m.d.sync += [
            self.s1_valid.eq(self.s0_valid),
            self.s2_valid.eq(self.s1_valid),
            self.s3_valid.eq(self.s2_valid),
        ]

        self.s0_mask = self.f0_mask
        self.s1_mask = Signal.like(self.f0_mask)
        self.s2_mask = Signal.like(self.f0_mask)
        self.s3_mask = Signal.like(self.f0_mask)
        m.d.sync += [
            self.s1_mask.eq(self.s0_mask),
            self.s2_mask.eq(self.s1_mask),
            self.s3_mask.eq(self.s2_mask),
        ]

        self.s0_pc = self.f0_pc
        self.s1_pc = Signal.like(self.f0_pc)
        m.d.sync += self.s1_pc.eq(self.s0_pc)

        self.s0_update = self.update.bits
        self.s0_update_idx = self.update.bits.pc[log2_int(self.fetch_bytes):]
        self.s0_update_valid = self.update.valid

        self.s1_update = BranchPredictionUpdate(self.params)
        self.s1_update_idx = Signal.like(self.s0_update_idx)
        self.s1_update_valid = Signal()
        m.d.sync += [
            self.s1_update.eq(self.s0_update),
            self.s1_update_idx.eq(self.s0_update_idx),
            self.s1_update_valid.eq(self.s0_update_valid),
        ]

        return m


class HasTageParams(HasCoreParams):

    def __init__(self, params, *args, **kwargs):
        super().__init__(params, *args, **kwargs)

        tage_params = params['tage_params']

        self.table_params = tage_params['table_params']
        self.u_bit_period = tage_params['u_bit_period']


class TageBranchPredictorBank(BaseBranchPredictorBank, HasTageParams):

    class TableResp(Record):

        def __init__(self, name=None, src_loc_at=0):
            super().__init__([
                ('ctr', 3),
                ('u', 2),
            ],
                             name=name,
                             src_loc_at=src_loc_at + 1)

    class TableEntry(Record):

        def __init__(self, tag_size, name=None, src_loc_at=0):
            super().__init__([
                ('valid', 1),
                ('tag', tag_size),
                ('ctr', 3),
            ],
                             name=name,
                             src_loc_at=src_loc_at + 1)

    class TageTable(HasCoreParams, Elaboratable):

        def __init__(self, params, n_sets, tag_size, hist_length,
                     u_bit_period):
            super().__init__(params)

            self.n_sets = n_sets
            self.tag_size = tag_size
            self.hist_length = hist_length
            self.u_bit_period = u_bit_period

            self.f1_req_valid = Signal()
            self.f1_req_pc = Signal(self.vaddr_bits_extended)
            self.f1_req_ghist = Signal(self.ghist_length)

            self.f3_resp = [
                Valid(TageBranchPredictorBank.TableResp, name=f'f3_resp{w}')
                for w in range(self.fetch_width)
            ]

            self.update_mask = Signal(self.fetch_width)
            self.update_taken = Signal(self.fetch_width)
            self.update_alloc = Signal(self.fetch_width)
            self.update_old_ctr = [
                Signal(3, name=f'update_old_ctr{w}')
                for w in range(self.fetch_width)
            ]

            self.update_pc = Signal(self.vaddr_bits_extended)
            self.update_hist = Signal(self.ghist_length)

            self.update_u_mask = Signal(self.fetch_width)
            self.update_u = [
                Signal(2, name=f'update_u{w}') for w in range(self.fetch_width)
            ]

        def elaborate(self, platform):
            m = Module()

            do_reset = Signal(reset=1)
            reset_idx = Signal(range(self.n_sets))
            m.d.sync += reset_idx.eq(reset_idx + do_reset)
            with m.If(reset_idx == self.n_sets - 1):
                m.d.sync += do_reset.eq(0)

            def chunks(sig, n):
                for i in range(0, len(sig), n):
                    yield sig[i:i + n]

            s1_req_idx = self.f1_req_pc[log2_int(self.fetch_bytes):]
            idx_hist = functools.reduce(
                operator.__xor__,
                chunks(self.f1_req_ghist[:self.hist_length],
                       log2_int(self.n_sets)))
            tag_hist = functools.reduce(
                operator.__xor__,
                chunks(self.f1_req_ghist[:self.hist_length], self.tag_size))
            s1_hashed_idx = Signal(log2_int(self.n_sets))
            s1_tag = Signal(self.tag_size)
            m.d.comb += [
                s1_hashed_idx.eq(s1_req_idx ^ idx_hist),
                s1_tag.eq((s1_req_idx >> log2_int(self.n_sets)) ^ tag_hist),
            ]

            s2_tag = Signal.like(s1_tag)
            m.d.sync += s2_tag.eq(s1_tag)

            s2_tage_rdata = [
                TageBranchPredictorBank.TableEntry(self.tag_size,
                                                   name=f's2_tage_rdata{w}')
                for w in range(self.fetch_width)
            ]
            entry_size = len(s2_tage_rdata[0])
            hi_us = Memory(width=self.fetch_width, depth=self.n_sets)
            lo_us = Memory(width=self.fetch_width, depth=self.n_sets)
            table = Memory(width=self.fetch_width * entry_size,
                           depth=self.n_sets)

            hi_us_read = m.submodules.hi_us_read = hi_us.read_port(
                transparent=False)
            lo_us_read = m.submodules.lo_us_read = lo_us.read_port(
                transparent=False)
            table_read = m.submodules.table_read = table.read_port(
                transparent=False)

            m.d.comb += [
                hi_us_read.addr.eq(s1_hashed_idx),
                lo_us_read.addr.eq(s1_hashed_idx),
                table_read.addr.eq(s1_hashed_idx),
            ]
            for i, rdata in enumerate(s2_tage_rdata):
                m.d.comb += rdata.eq(table_read.data[i * entry_size:(i + 1) *
                                                     entry_size])

            s2_hius_rdata = hi_us_read.data
            s2_lous_rdata = lo_us_read.data
            s2_hits = Signal(self.fetch_width)
            m.d.comb += s2_hits.eq(
                Cat(e.valid & (e.tag == s2_tag) & ~do_reset
                    for e in s2_tage_rdata))

            for w in range(self.fetch_width):
                m.d.sync += [
                    self.f3_resp[w].valid.eq(s2_hits[w]),
                    self.f3_resp[w].bits.u.eq(
                        Cat(s2_lous_rdata[w], s2_hius_rdata[w])),
                    self.f3_resp[w].bits.ctr.eq(Cat(s2_tage_rdata[w].ctr)),
                ]

            clear_u_counter = Signal(
                log2_int(self.u_bit_period) + log2_int(self.n_sets) + 1)
            with m.If(do_reset):
                m.d.sync += clear_u_counter.eq(1)
            with m.Else():
                m.d.sync += clear_u_counter.eq(clear_u_counter + 1)

            do_clear_u = clear_u_counter[:log2_int(self.u_bit_period)] == 0
            do_clear_u_hi = do_clear_u & (clear_u_counter[-1] == 1)
            do_clear_u_lo = do_clear_u & (clear_u_counter[-1] == 0)
            clear_u_idx = Signal(self.n_sets)
            m.d.comb += clear_u_idx.eq(
                clear_u_counter[log2_int(self.u_bit_period):])

            update_req_idx = self.update_pc[log2_int(self.fetch_bytes):]
            update_idx_hist = functools.reduce(
                operator.__xor__,
                chunks(self.update_hist[:self.hist_length],
                       log2_int(self.n_sets)))
            update_tag_hist = functools.reduce(
                operator.__xor__,
                chunks(self.update_hist[:self.hist_length], self.tag_size))
            update_idx = Signal(log2_int(self.n_sets))
            update_tag = Signal(self.tag_size)
            m.d.comb += [
                update_idx.eq(update_req_idx ^ update_idx_hist),
                update_tag.eq((update_req_idx >> log2_int(self.n_sets))
                              ^ update_tag_hist),
            ]

            update_entry = [
                TageBranchPredictorBank.TableEntry(self.tag_size,
                                                   name=f'update_entry{w}')
                for w in range(self.fetch_width)
            ]
            table_write = m.submodules.table_write = table.write_port(
                granularity=entry_size)
            with m.If(do_reset):
                m.d.comb += [
                    table_write.addr.eq(reset_idx),
                    table_write.data.eq(0),
                    table_write.en.eq(~0),
                ]
            with m.Else():
                m.d.comb += [
                    table_write.addr.eq(update_idx),
                    table_write.data.eq(Cat(*update_entry)),
                    table_write.en.eq(self.update_mask),
                ]

            update_hi_wdata = Signal(self.fetch_width)
            hius_write = m.submodules.hius_write = hi_us.write_port(
                granularity=1)
            with m.If(do_reset | do_clear_u_hi):
                m.d.comb += [
                    hius_write.addr.eq(Mux(do_reset, reset_idx, clear_u_idx)),
                    hius_write.data.eq(0),
                    hius_write.en.eq(~0),
                ]
            with m.Else():
                m.d.comb += [
                    hius_write.addr.eq(update_idx),
                    hius_write.data.eq(update_hi_wdata),
                    hius_write.en.eq(self.update_u_mask),
                ]

            update_lo_wdata = Signal(self.fetch_width)
            lous_write = m.submodules.lous_write = lo_us.write_port(
                granularity=1)
            with m.If(do_reset | do_clear_u_lo):
                m.d.comb += [
                    lous_write.addr.eq(Mux(do_reset, reset_idx, clear_u_idx)),
                    lous_write.data.eq(0),
                    lous_write.en.eq(~0),
                ]
            with m.Else():
                m.d.comb += [
                    lous_write.addr.eq(update_idx),
                    lous_write.data.eq(update_lo_wdata),
                    lous_write.en.eq(self.update_u_mask),
                ]

            def inc_ctr(ctr, taken):
                return Mux(taken, Mux(ctr == 7, 7, ctr + 1),
                           Mux(ctr == 0, 0, ctr - 1))

            for w in range(self.fetch_width):
                m.d.comb += [
                    update_entry[w].valid.eq(1),
                    update_entry[w].tag.eq(update_tag),
                    update_entry[w].ctr.eq(
                        Mux(
                            self.update_alloc[w],
                            Mux(self.update_taken[w], 4, 3),
                            inc_ctr(self.update_old_ctr[w],
                                    self.update_taken[w]))),
                ]

                m.d.comb += [
                    update_hi_wdata[w].eq(self.update_u[w][1]),
                    update_lo_wdata[w].eq(self.update_u[w][0]),
                ]

            return m

    class Metadata(HasTageParams, ValueCastable):

        def __init__(self, params, name=None, src_loc_at=0):
            super().__init__(params)

            if name is None:
                name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

            n_tables = len(self.table_params)

            self.provider = [
                Valid(Signal, range(n_tables), name=f'{name}__provider{w}')
                for w in range(self.fetch_width)
            ]

            self.alt_differs = Signal(self.fetch_width,
                                      name=f'{name}__alt_differs')
            self.provider_u = [
                Signal(2, name=f'{name}__provider{w}_u')
                for w in range(self.fetch_width)
            ]
            self.provider_ctr = [
                Signal(3, name=f'{name}__provider{w}_ctr')
                for w in range(self.fetch_width)
            ]

            self.allocate = [
                Valid(Signal, range(n_tables), name=f'{name}__allocate{w}')
                for w in range(self.fetch_width)
            ]

        @ValueCastable.lowermethod
        def as_value(self):
            return Cat(*self.provider, self.alt_differs, *self.provider_u,
                       *self.provider_ctr, *self.allocate)

        def shape(self):
            return self.as_value().shape()

        def __len__(self):
            return len(Value.cast(self))

        def eq(self, rhs):
            return Value.cast(self).eq(Value.cast(rhs))

    def __init__(self, params):
        super().__init__(params)

        self.f3_meta = TageBranchPredictorBank.Metadata(params)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        tables = []
        for i, (n_sets, hist_length, tag_size) in enumerate(self.table_params):
            table = TageBranchPredictorBank.TageTable(
                params=self.params,
                n_sets=n_sets,
                tag_size=tag_size,
                hist_length=hist_length,
                u_bit_period=self.u_bit_period)
            setattr(m.submodules,
                    f'table{i}_n{n_sets}_t{tag_size}_l{hist_length}', table)
            tables.append(table)

            m.d.sync += [
                table.f1_req_valid.eq(self.f0_valid),
                table.f1_req_pc.eq(self.f0_pc),
            ]
            m.d.comb += table.f1_req_ghist.eq(self.f1_ghist)

        s1_update_meta = TageBranchPredictorBank.Metadata(self.params)
        s1_update_mispredict_mask = Signal(self.fetch_width)
        m.d.comb += s1_update_meta.eq(self.s1_update.meta)
        with m.If(self.s1_update.cfi_mispredicted):
            with m.Switch(self.s1_update.cfi_idx):
                for w in range(self.fetch_width):
                    with m.Case(w):
                        m.d.comb += s1_update_mispredict_mask[w].eq(1)

        s1_update_mask = [
            Signal(self.fetch_width, name=f's1_update_mask{i}')
            for i in range(len(tables))
        ]
        s1_update_u_mask = [
            Signal(self.fetch_width, name=f's1_update_u_mask{i}')
            for i in range(len(tables))
        ]

        s1_update_taken = [
            Signal(self.fetch_width, name=f's1_update_taken{i}')
            for i in range(len(tables))
        ]
        s1_update_old_ctr = [[
            Signal(3, name=f's1_update_old_ctr{i}_w{w}')
            for w in range(self.fetch_width)
        ] for i in range(len(tables))]
        s1_update_alloc = [
            Signal(self.fetch_width, name=f's1_update_alloc{i}')
            for i in range(len(tables))
        ]
        s1_update_u = [[
            Signal(2, name=f's1_update_u{i}_w{w}')
            for w in range(self.fetch_width)
        ] for i in range(len(tables))]

        for w in range(self.fetch_width):
            altpred = Signal(len(tables) + 1, name=f'altpred{w}')
            final_altpred = Signal(name=f'final_altpred{w}')
            m.d.comb += [
                altpred[0].eq(self.resp_in.f3[w].taken),
                final_altpred.eq(self.resp_in.f3[w].taken),
                self.resp.f3[w].taken.eq(self.resp_in.f3[w].taken),
            ]

            provided = Signal(len(tables), name=f'provided{w}')

            for i, table in enumerate(tables):
                hit = table.f3_resp[w].valid
                ctr = table.f3_resp[w].bits.ctr

                with m.If(hit):
                    m.d.comb += [
                        self.resp.f3[w].taken.eq(
                            Mux((ctr == 3) | (ctr == 4), altpred[i], ctr[2])),
                        final_altpred.eq(altpred[i]),
                    ]

                m.d.comb += [
                    provided[i].eq(hit),
                    altpred[i + 1].eq(Mux(hit, ctr[2], altpred[i])),
                ]

            m.d.comb += [
                self.f3_meta.provider[w].valid.eq(provided.any()),
                self.f3_meta.alt_differs[w].eq(
                    final_altpred != self.resp.f3[w].taken),
            ]
            for i, table in enumerate(tables):
                with m.If(provided[i]):
                    m.d.comb += [
                        self.f3_meta.provider[w].bits.eq(i),
                        self.f3_meta.provider_u[w].eq(table.f3_resp[w].bits.u),
                        self.f3_meta.provider_ctr[w].eq(
                            table.f3_resp[w].bits.ctr),
                    ]

            provider_mask = Signal(len(tables), name=f'provider_mask{w}')
            for i in range(len(tables)):
                with m.If(provided[i]):
                    m.d.comb += provider_mask.eq((1 << (i + 1)) - 1)

            allocatable_slots = Signal(len(tables),
                                       name=f'allocatable_slots{w}')
            m.d.comb += allocatable_slots.eq(
                Cat(~t.f3_resp[w].valid & (t.f3_resp[w].bits.u == 0)
                    for t in tables) & ~provider_mask)

            allocate_pe = PriorityEncoder(len(tables))
            m.submodules += allocate_pe
            m.d.comb += allocate_pe.i.eq(allocatable_slots)

            m.d.comb += [
                self.f3_meta.allocate[w].valid.eq(~allocate_pe.n),
                self.f3_meta.allocate[w].bits.eq(allocate_pe.o),
            ]

            update_taken = self.s1_update.cfi_idx.valid & (
                self.s1_update.cfi_idx.bits == w) & self.s1_update.cfi_taken
            with m.If(self.s1_update_valid & self.s1_update.is_commit_update
                      & self.s1_update.br_mask[w]):

                with m.If(s1_update_meta.provider[w].valid):
                    provider_u = s1_update_meta.provider_u[w]

                    with m.Switch(s1_update_meta.provider[w].bits):
                        for i in range(len(tables)):
                            with m.Case(i):
                                m.d.comb += [
                                    s1_update_mask[i][w].eq(1),
                                    s1_update_u_mask[i][w].eq(1),
                                    s1_update_u[i][w].eq(
                                        Mux(
                                            ~s1_update_meta.alt_differs[w],
                                            provider_u,
                                            Mux(
                                                s1_update_mispredict_mask[w],
                                                Mux(provider_u == 0, 0,
                                                    provider_u - 1),
                                                Mux(provider_u == 3, 3,
                                                    provider_u + 1)))),
                                    s1_update_taken[i][w].eq(update_taken),
                                    s1_update_old_ctr[i][w].eq(
                                        s1_update_meta.provider_ctr[w]),
                                    s1_update_alloc[i][w].eq(0),
                                ]

        with m.If(self.s1_update_valid & self.s1_update.is_commit_update
                  & self.s1_update.cfi_mispredicted
                  & self.s1_update.cfi_idx.valid):
            allocate = Valid(Signal, range(len(tables)))
            assert len(allocate) == len(s1_update_meta.allocate[0])

            with m.Switch(self.s1_update.cfi_idx.bits):
                for w in range(self.fetch_width):
                    with m.Case(w):
                        m.d.comb += allocate.eq(s1_update_meta.allocate[w])

                        with m.If(allocate.valid):
                            with m.Switch(allocate.bits):
                                for i in range(len(tables)):
                                    with m.Case(i):
                                        m.d.comb += [
                                            s1_update_mask[i][w].eq(1),
                                            s1_update_taken[i][w].eq(
                                                self.s1_update.cfi_taken),
                                            s1_update_alloc[i][w].eq(1),
                                            s1_update_u_mask[i][w].eq(1),
                                            s1_update_u[i][w].eq(0),
                                        ]

                        with m.Else():
                            decr_mask = Signal(len(tables))
                            with m.If(s1_update_meta.provider[w].valid):
                                with m.Switch(s1_update_meta.provider[w].bits):
                                    for i in range(len(tables)):
                                        with m.Case(i):
                                            m.d.comb += decr_mask.eq(
                                                (1 << (i + 1)) - 1)

                            for i in range(len(tables)):
                                with m.If(~decr_mask[i]):
                                    m.d.comb += [
                                        s1_update_u_mask[i][w].eq(1),
                                        s1_update_u[i][w].eq(0),
                                    ]

        for i, table in enumerate(tables):
            for w in range(self.fetch_width):
                m.d.sync += [
                    table.update_mask[w].eq(s1_update_mask[i][w]),
                    table.update_taken[w].eq(s1_update_taken[i][w]),
                    table.update_alloc[w].eq(s1_update_alloc[i][w]),
                    table.update_old_ctr[w].eq(s1_update_old_ctr[i][w]),
                    table.update_u_mask[w].eq(s1_update_u_mask[i][w]),
                    table.update_u[w].eq(s1_update_u[i][w]),
                ]

            m.d.sync += [
                table.update_pc.eq(self.s1_update.pc),
                table.update_hist.eq(self.s1_update.ghist),
            ]

        return m


class CompositeBranchPredictorBank(BaseBranchPredictorBank):

    def __init__(self, params):
        super().__init__(params)

        self.f3_meta = Signal(self.bpd_meta_length)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        component_cls = [
            ('tage', TageBranchPredictorBank),
        ]

        resp_in = self.resp_in

        meta_size = 0
        meta = []
        components = []
        for name, cls in component_cls:
            comp = cls(self.params)
            setattr(m.submodules, name, comp)
            components.append(comp)

            m.d.comb += [
                comp.f0_valid.eq(self.f0_valid),
                comp.f0_pc.eq(self.f0_pc),
                comp.f0_mask.eq(self.f0_mask),
                comp.f1_ghist.eq(self.f1_ghist),
                comp.resp_in.eq(resp_in),
            ]

            if comp.f3_meta is not None:
                meta_size += len(comp.f3_meta)
                meta.append(comp.f3_meta)

            resp_in = comp.resp

        if meta_size > self.bpd_meta_length:
            raise ValueError(
                f"Not enough bits ({self.bpd_meta_length}) to hold branch predictor metadata ({meta_size})"
            )

        m.d.comb += [
            self.f3_meta.eq(Cat(*meta)),
            self.resp.eq(resp_in),
        ]

        update_meta = []
        for comp in components:
            m.d.comb += comp.update.eq(self.update)

            if comp.f3_meta is not None:
                update_meta.append(comp.update.bits.meta[:len(comp.f3_meta)])

        m.d.comb += Cat(*update_meta).eq(self.update.bits.meta)

        return m


class NullBranchPredictorBank(BaseBranchPredictorBank):

    def __init__(self, params):
        super().__init__(params)

    def elaborate(self, platform):
        m = super().elaborate(platform)
        return m


class BranchPredictor(HasCoreParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.f0_req = Valid(BranchPredictionRequest, params)

        self.f1_resp = BranchPredictions(params)
        self.f2_resp = BranchPredictions(params)
        self.f3_resp = BranchPredictions(params)

        self.update = Valid(BranchPredictionUpdate, params)

    def elaborate(self, platform):
        m = Module()

        def fetch_mask(addr):
            off = (addr >> 1)[0:Shape.cast(range(self.fetch_width)).width]
            return (((1 << self.fetch_width) - 1) << off)[0:self.fetch_width]

        def fetch_align(addr):
            lsb_width = Shape.cast(range(self.fetch_bytes)).width
            return Cat(Const(0, lsb_width), addr[lsb_width:])

        bank_cls = CompositeBranchPredictorBank if self.use_bpd else NullBranchPredictorBank
        bank = m.submodules.bank = bank_cls(self.params)

        m.d.comb += [
            bank.f0_valid.eq(self.f0_req.valid),
            bank.f0_pc.eq(fetch_align(self.f0_req.bits.pc)),
            bank.f0_mask.eq(fetch_mask(self.f0_req.bits.pc)),
        ]

        m.d.sync += bank.f1_ghist.eq(self.f0_req.bits.ghist.history)

        for a, b in zip(self.f1_resp.preds, bank.resp.f1):
            m.d.comb += a.eq(b)
        for a, b in zip(self.f2_resp.preds, bank.resp.f2):
            m.d.comb += a.eq(b)
        for a, b in zip(self.f3_resp.preds, bank.resp.f3):
            m.d.comb += a.eq(b)
        m.d.comb += self.f3_resp.meta.eq(bank.f3_meta)

        m.d.sync += [
            self.f1_resp.pc.eq(self.f0_req.bits.pc),
            self.f2_resp.pc.eq(self.f1_resp.pc),
            self.f3_resp.pc.eq(self.f2_resp.pc),
        ]

        m.d.comb += bank.update.eq(self.update)

        return m
