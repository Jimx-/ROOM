from amaranth import *
from amaranth import tracer
from amaranth.utils import log2_int
from amaranth.hdl.ast import ValueCastable

from groom.fu import ExecResp
from room.consts import *
from room.types import HasCoreParams, MicroOp
from room.dcache import DCacheReq, DCacheResp
from room.lsu import LoadGen, StoreGen
from room.mmu import PMAChecker
from room.utils import sign_extend

from roomsoc.interconnect.stream import Valid, Decoupled


class LSUDebug(HasCoreParams, ValueCastable):

    def __init__(self, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.wid = Signal(range(self.n_warps), name=f'{name}__wid')
        self.uop_id = Signal(MicroOp.ID_WIDTH, name=f'{name}__uop_id')
        self.tmask = Signal(self.n_threads, name=f'{name}__tmask')
        self.opcode = Signal(UOpCode, name=f'{name}__opcode')

        self.addr = [
            Signal(self.xlen, name=f'{name}__addr{w}')
            for w in range(self.n_threads)
        ]
        self.data = [
            Signal(self.xlen, name=f'{name}__data{w}')
            for w in range(self.n_threads)
        ]

        self.data_valid = Signal(name=f'{name}__data_valid')
        self.mem_size = Signal(2, name=f'{name}__mem_size')

        self.lrs1 = Signal(range(32), name=f'{name}__lrs1')
        self.lrs2 = Signal(range(32), name=f'{name}__lrs2')

    @ValueCastable.lowermethod
    def as_value(self):
        return Cat(self.wid, self.uop_id, self.tmask, self.opcode, *self.addr,
                   *self.data, self.data_valid, self.mem_size, self.lrs1,
                   self.lrs2)

    def shape(self):
        return self.as_value().shape()

    def __len__(self):
        return len(Value.cast(self))

    def eq(self, rhs):
        return Value.cast(self).eq(Value.cast(rhs))


DMA_ID_WIDTH = 4


class SharedMemoryDMAReq(HasCoreParams, ValueCastable):

    def __init__(self,
                 params,
                 dma_id_width=DMA_ID_WIDTH,
                 name=None,
                 src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)
        self.name = name

        self.id = Signal(dma_id_width, name=f'{name}_id')
        self.offset = Signal(log2_int(self.smem_size) + 1,
                             name=f'{name}_offset')
        self.data = Signal(64, name=f'{name}_data')
        self.byte_enable = Signal(8, name=f'{name}_byte_enable')

    @ValueCastable.lowermethod
    def as_value(self):
        return Cat(self.id, self.offset, self.data, self.byte_enable)

    def shape(self):
        return self.as_value().shape()

    def __len__(self):
        return len(Value.cast(self))

    def eq(self, rhs):
        return Value.cast(self).eq(Value.cast(rhs))


class SharedMemoryDMACommit(HasCoreParams, ValueCastable):

    def __init__(self,
                 params,
                 dma_id_width=DMA_ID_WIDTH,
                 name=None,
                 src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)
        self.name = name

        self.id = Signal(dma_id_width, name=f'{name}_id')
        self.nbytes = Signal(range(9), name=f'{name}_nbytes')

    @ValueCastable.lowermethod
    def as_value(self):
        return Cat(self.id, self.nbytes)

    def shape(self):
        return self.as_value().shape()

    def __len__(self):
        return len(Value.cast(self))

    def eq(self, rhs):
        return Value.cast(self).eq(Value.cast(rhs))


class SharedMemory(HasCoreParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.req = [
            Decoupled(DCacheReq, params, name=f'req{i}')
            for i in range(self.n_threads)
        ]

        self.resp = [
            Valid(DCacheResp, self.params, name=f'resp{i}')
            for i in range(self.n_threads)
        ]

        self.nack = [
            Valid(DCacheReq, self.params, name=f'nack{i}')
            for i in range(self.n_threads)
        ]

        if self.use_async_copy:
            self.dma_req = Decoupled(SharedMemoryDMAReq,
                                     self.params,
                                     name='dma_req')
            self.dma_commit = Decoupled(SharedMemoryDMACommit,
                                        self.params,
                                        name='dma_commit')

    def elaborate(self, platform):
        m = Module()

        n_banks = self.smem_banks
        bank_size = self.smem_size // n_banks

        word_bytes = self.xlen // 8
        word_off_bits = log2_int(word_bytes)

        bank_bits = log2_int(n_banks)
        bank_off_bits = word_off_bits
        bidx_bits = log2_int(bank_size)
        bidx_off_bits = bank_off_bits + bank_bits

        use_dma = self.use_async_copy
        dma_bytes = 8
        dma_span = (dma_bytes - 2 + word_bytes) // word_bytes + 1
        dma_queue_depth = 4
        n_reqs = self.n_threads + (dma_span if use_dma else 0)

        for w in range(self.n_threads):
            m.d.comb += self.req[w].ready.eq(1)

        #
        # S0
        #

        s0_valids = [self.req[w].valid for w in range(self.n_threads)]
        s0_req = [
            DCacheReq(self.params, name=f's0_req{i}')
            for i in range(self.n_threads)
        ]
        s0_write_masks = [
            Signal(word_bytes, name=f's0_write_mask{i}')
            for i in range(self.n_threads)
        ]
        s0_banks = [
            Signal(bank_bits, name=f's0_bank{i}')
            for i in range(self.n_threads)
        ]
        s0_idxs = [
            Signal(bidx_bits, name=f's0_idx{i}') for i in range(self.n_threads)
        ]
        s0_bank_gnts = [
            Signal(n_reqs, name=f's0_bank_gnt{b}') for b in range(n_banks)
        ]

        for w in range(self.n_threads):
            store_gen = StoreGen(max_size=self.xlen // 8)
            setattr(m.submodules, f'store_gen{w}', store_gen)
            m.d.comb += [
                store_gen.typ.eq(self.req[w].bits.uop.mem_size),
                store_gen.addr.eq(self.req[w].bits.addr),
                store_gen.data_in.eq(self.req[w].bits.data),
            ]

            m.d.comb += [
                s0_req[w].eq(self.req[w].bits),
                s0_req[w].data.eq(store_gen.data_out),
                s0_write_masks[w].eq(store_gen.mask),
                s0_banks[w].eq(
                    (self.req[w].bits.addr >> bank_off_bits)[:bank_bits]),
                s0_idxs[w].eq(
                    (self.req[w].bits.addr >> bidx_off_bits)[:bidx_bits]),
            ]

        if use_dma:
            word_idx_bits = log2_int(self.smem_size // word_bytes)

            shamt = self.dma_req.bits.offset[:word_off_bits]
            shifted_data = Cat(
                self.dma_req.bits.data,
                Const(0, (dma_span - 1) * word_bytes * 8)) << (8 * shamt)
            shifted_mask = Cat(self.dma_req.bits.byte_enable,
                               Const(0, (dma_span - 1) * word_bytes)) << shamt

            load_valids = []
            load_banks = []
            load_idxs = []
            load_masks = []
            load_datas = []
            load_nbytes = Const(0)
            for j in range(dma_span):
                waddr = self.dma_req.bits.offset[word_off_bits:] + j
                mask = Mux(waddr[word_idx_bits:] == 0,
                           shifted_mask[j * word_bytes:(j + 1) * word_bytes],
                           Const(0, word_bytes))
                load_valids.append(mask != 0)
                load_banks.append(waddr[:bank_bits])
                load_idxs.append(waddr[bank_bits:bank_bits + bidx_bits])
                load_masks.append(mask)
                load_datas.append(shifted_data[j * self.xlen:(j + 1) *
                                               self.xlen])
                for byte in range(word_bytes):
                    load_nbytes = load_nbytes + mask[byte]

            frag_active = Signal()
            frag_id = Signal(DMA_ID_WIDTH)
            frag_nbytes = Signal(range(dma_bytes + 1))
            frag_pending = Signal(dma_span)
            frag_banks = [
                Signal(bank_bits, name=f'frag_bank{j}')
                for j in range(dma_span)
            ]
            frag_idxs = [
                Signal(bidx_bits, name=f'frag_idx{j}') for j in range(dma_span)
            ]
            frag_masks = [
                Signal(word_bytes, name=f'frag_mask{j}')
                for j in range(dma_span)
            ]
            frag_datas = [
                Signal(self.xlen, name=f'frag_data{j}')
                for j in range(dma_span)
            ]

            s1_commit_v = Signal()
            s1_commit_id = Signal(DMA_ID_WIDTH)
            s1_commit_nbytes = Signal(range(dma_bytes + 1))
            s2_commit_v = Signal()
            s2_commit_id = Signal(DMA_ID_WIDTH)
            s2_commit_nbytes = Signal(range(dma_bytes + 1))
            commit_q_ids = [
                Signal(DMA_ID_WIDTH, name=f'commit_q_id{i}')
                for i in range(dma_queue_depth)
            ]
            commit_q_nbytes = [
                Signal(range(dma_bytes + 1), name=f'commit_q_nbytes{i}')
                for i in range(dma_queue_depth)
            ]
            commit_q_head = Signal(range(dma_queue_depth))
            commit_q_count = Signal(range(dma_queue_depth + 1))

            may_complete = (commit_q_count + s2_commit_v + s1_commit_v
                            < dma_queue_depth)
            dma_grantable = Signal(dma_span)
            with m.If(may_complete):
                m.d.comb += dma_grantable.eq(frag_pending)
            with m.Else():
                m.d.comb += dma_grantable.eq(frag_pending & (frag_pending - 1))

            slot_valids = [
                Signal(name=f's0_dma_valid{j}') for j in range(dma_span)
            ]
            for j in range(dma_span):
                m.d.comb += slot_valids[j].eq(frag_active & dma_grantable[j])

            s0_valids += slot_valids
            s0_banks += frag_banks
            s0_idxs += frag_idxs

        s0_rot = Signal(range(n_reqs))
        s0_dists = [
            Signal(range(n_reqs), name=f's0_dist{i}') for i in range(n_reqs)
        ]
        for r in range(n_reqs):
            with m.If(s0_rot <= r):
                m.d.comb += s0_dists[r].eq(r - s0_rot)
            with m.Else():
                m.d.comb += s0_dists[r].eq(r + n_reqs - s0_rot)

        s0_wins = [Signal(name=f's0_wins{i}') for i in range(n_reqs)]
        for r in range(n_reqs):
            prior = Const(0)
            for q in range(n_reqs):
                if q != r:
                    prior = prior | (s0_valids[q]
                                     & (s0_banks[q] == s0_banks[r])
                                     & (s0_dists[q] < s0_dists[r]))
            m.d.comb += s0_wins[r].eq(s0_valids[r] & ~prior)

        for b in range(n_banks):
            for r in range(n_reqs):
                with m.If(s0_wins[r] & (s0_banks[r] == b)):
                    m.d.comb += s0_bank_gnts[b][r].eq(1)

        s0_nacks = Signal(self.n_threads)
        for w in range(self.n_threads):
            ride = Const(0)
            for q in range(self.n_threads):
                if q != w:
                    ride = ride | (s0_wins[q] & (s0_banks[q] == s0_banks[w])
                                   & (s0_idxs[q] == s0_idxs[w]))
            m.d.comb += s0_nacks[w].eq(s0_valids[w] & ~s0_wins[w] & ~ride)

        nack_terms = [s0_nacks[w] for w in range(self.n_threads)]
        if use_dma:
            nack_terms += [
                slot_valids[j] & ~s0_wins[self.n_threads + j]
                for j in range(dma_span)
            ]
        # Advance through every requester on contention. Advancing past the
        # highest bank winner lets an unrelated bank repeatedly skip losers.
        # A persistent loser now becomes first priority within n_reqs cycles.
        with m.If(Cat(*nack_terms).any()):
            m.d.sync += s0_rot.eq((s0_rot + 1) % n_reqs)

        #
        # S1
        #

        s1_valids = Signal(self.n_threads)
        s1_req = [
            DCacheReq(self.params, name=f's1_req{i}')
            for i in range(self.n_threads)
        ]
        s1_banks = [
            Signal(bank_bits, name=f's1_bank{i}')
            for i in range(self.n_threads)
        ]
        s1_nacks = Signal(self.mem_width)

        m.d.sync += [
            s1_valids.eq(Cat(*s0_valids[:self.n_threads])),
            s1_nacks.eq(s0_nacks),
        ]
        for w in range(self.n_threads):
            m.d.sync += [
                s1_req[w].eq(s0_req[w]),
                s1_banks[w].eq(s0_banks[w]),
            ]

        #
        # S2
        #

        s2_valids = Signal(self.n_threads)
        s2_req = [
            DCacheReq(self.params, name=f's2_req{i}')
            for i in range(self.n_threads)
        ]
        s2_bank_selection = [
            Signal(range(n_banks), name=f's2_bank_selection{i}')
            for i in range(self.n_threads)
        ]
        s2_nacks = Signal(self.n_threads)

        m.d.sync += [
            s2_valids.eq(s1_valids),
            s2_nacks.eq(s1_nacks),
        ]
        for w in range(self.n_threads):
            m.d.sync += [
                s2_req[w].eq(s1_req[w]),
                s2_bank_selection[w].eq(s1_banks[w]),
            ]

        s2_bank_reads = Array(
            Signal(self.xlen, name=f's2_bank_read{b}') for b in range(n_banks))

        for b in range(n_banks):
            mem = Memory(width=self.xlen, depth=bank_size)

            mem_read = mem.read_port(transparent=False)
            setattr(m.submodules, f'mem_read{b}', mem_read)

            for r in range(n_reqs):
                with m.If(s0_bank_gnts[b][r]):
                    m.d.comb += mem_read.addr.eq(s0_idxs[r])

            m.d.sync += s2_bank_reads[b].eq(mem_read.data)

            mem_write = mem.write_port(granularity=8)
            setattr(m.submodules, f'mem_write{b}', mem_write)

            for r in range(n_reqs):
                with m.If(s0_bank_gnts[b][r]):
                    m.d.comb += mem_write.addr.eq(s0_idxs[r])

            for i in range(self.n_threads):
                with m.If(s0_bank_gnts[b][i]):
                    for byte in range(word_bytes):
                        byte_data = s0_req[i].data.word_select(byte, 8)
                        byte_en = (MemoryCommand.is_write(
                            s0_req[i].uop.mem_cmd) & s0_write_masks[i][byte])

                        for q in range(self.n_threads):
                            if q != i:
                                coalesced_write = (s0_valids[q] &
                                                   (s0_banks[q] == b)
                                                   & (s0_idxs[q] == s0_idxs[i])
                                                   & MemoryCommand.is_write(
                                                       s0_req[q].uop.mem_cmd)
                                                   & s0_write_masks[q][byte])
                                byte_data = Mux(
                                    coalesced_write,
                                    s0_req[q].data.word_select(byte,
                                                               8), byte_data)
                                byte_en |= coalesced_write

                        m.d.comb += [
                            mem_write.data.word_select(byte, 8).eq(byte_data),
                            mem_write.en[byte].eq(byte_en),
                        ]

            if use_dma:
                for j in range(dma_span):
                    with m.If(s0_bank_gnts[b][self.n_threads + j]):
                        for byte in range(word_bytes):
                            m.d.comb += [
                                mem_write.data.word_select(byte, 8).eq(
                                    frag_datas[j].word_select(byte, 8)),
                                mem_write.en[byte].eq(frag_masks[j][byte]),
                            ]

        for w in range(self.n_threads):
            load_gen = LoadGen(max_size=self.xlen // 8)
            setattr(m.submodules, f'load_gen{w}', load_gen)
            m.d.comb += [
                load_gen.typ.eq(s2_req[w].uop.mem_size),
                load_gen.signed.eq(s2_req[w].uop.mem_signed),
                load_gen.addr.eq(s2_req[w].addr),
                load_gen.data_in.eq(s2_bank_reads[s2_bank_selection[w]]),
            ]

            m.d.comb += [
                self.resp[w].valid.eq(s2_valids[w] & ~s2_nacks[w]),
                self.resp[w].bits.uop.eq(s2_req[w].uop),
                self.resp[w].bits.data.eq(load_gen.data_out),
                self.nack[w].valid.eq(s2_valids[w] & s2_nacks[w]),
                self.nack[w].bits.uop.eq(s2_req[w].uop),
            ]

        if use_dma:
            slot_gnt = Cat(*[
                Cat(*[
                    s0_bank_gnts[b][self.n_threads + j] for b in range(n_banks)
                ]).any() for j in range(dma_span)
            ])
            # Empty masks (including fully out-of-bounds requests) still
            # consume a commit entry, even though they need no bank grant.
            frag_completing = (frag_active & may_complete
                               & ((frag_pending & ~slot_gnt) == 0))

            m.d.comb += self.dma_req.ready.eq(~frag_active | frag_completing)

            with m.If(self.dma_req.fire):
                m.d.sync += [
                    frag_active.eq(1),
                    frag_id.eq(self.dma_req.bits.id),
                    frag_nbytes.eq(load_nbytes),
                    frag_pending.eq(Cat(*load_valids)),
                ]
                for j in range(dma_span):
                    m.d.sync += [
                        frag_banks[j].eq(load_banks[j]),
                        frag_idxs[j].eq(load_idxs[j]),
                        frag_masks[j].eq(load_masks[j]),
                        frag_datas[j].eq(load_datas[j]),
                    ]
            with m.Else():
                m.d.sync += frag_pending.eq(frag_pending & ~slot_gnt)
                with m.If(frag_completing):
                    m.d.sync += frag_active.eq(0)

            m.d.sync += [
                s2_commit_v.eq(s1_commit_v),
                s2_commit_id.eq(s1_commit_id),
                s2_commit_nbytes.eq(s1_commit_nbytes),
                s1_commit_v.eq(frag_completing),
                s1_commit_id.eq(frag_id),
                s1_commit_nbytes.eq(frag_nbytes),
            ]

            commit_q_ids_arr = Array(commit_q_ids)
            commit_q_nbytes_arr = Array(commit_q_nbytes)
            commit_q_empty = commit_q_count == 0
            with m.If(commit_q_empty):
                m.d.comb += [
                    self.dma_commit.valid.eq(s2_commit_v),
                    self.dma_commit.bits.id.eq(s2_commit_id),
                    self.dma_commit.bits.nbytes.eq(s2_commit_nbytes),
                ]
            with m.Else():
                m.d.comb += [
                    self.dma_commit.valid.eq(1),
                    self.dma_commit.bits.id.eq(
                        commit_q_ids_arr[commit_q_head]),
                    self.dma_commit.bits.nbytes.eq(
                        commit_q_nbytes_arr[commit_q_head]),
                ]

            bypass_fire = commit_q_empty & self.dma_commit.fire
            dequeue = self.dma_commit.fire & ~commit_q_empty
            enqueue = s2_commit_v & ~bypass_fire
            commit_q_tail = (commit_q_head + commit_q_count) % dma_queue_depth

            with m.If(enqueue):
                with m.Switch(commit_q_tail):
                    for i in range(dma_queue_depth):
                        with m.Case(i):
                            m.d.sync += [
                                commit_q_ids[i].eq(s2_commit_id),
                                commit_q_nbytes[i].eq(s2_commit_nbytes),
                            ]
            m.d.sync += commit_q_count.eq(commit_q_count + enqueue - dequeue)
            with m.If(dequeue):
                m.d.sync += commit_q_head.eq(
                    (commit_q_head + 1) % dma_queue_depth)

        return m


class LSQEntry(HasCoreParams):

    def __init__(self, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.valid = Signal(name=f'{name}_valid')
        self.uop = MicroOp(params, name=f'{name}_uop')

        self.addr = [
            Signal(32, name=f'{name}_addr{i}') for i in range(self.n_threads)
        ]
        self.addr_valid = Signal(name=f'{name}_addr_valid')
        self.addr_uncacheable = Signal(self.n_threads,
                                       name=f'{name}_addr_uncacheable')
        self.addr_is_smem = Signal(self.n_threads, name=f'{name}_addr_is_smem')

        self.data = [
            Signal(self.xlen, name=f'{name}_data{i}')
            for i in range(self.n_threads)
        ]
        self.data_valid = Signal(name=f'{name}_data_valid')

        self.executed = Signal(self.n_threads, name=f'{name}_executed')
        self.succeeded = Signal(self.n_threads, name=f'{name}_succeeded')

    def eq(self, rhs):
        attrs = [
            'valid',
            'uop',
            'executed',
            'succeeded',
        ]
        return [getattr(self, a).eq(getattr(rhs, a)) for a in attrs
                ] + [a.eq(b) for a, b in zip(self.addr, rhs.addr)]


class LoadStoreUnit(HasCoreParams, Elaboratable):

    def __init__(self, params, sim_debug=False):
        super().__init__(params)

        self.sim_debug = sim_debug

        self.exec_req = Decoupled(ExecResp, self.xlen, params)

        self.exec_iresp = Decoupled(ExecResp, self.xlen, params)

        self.exec_fresp = Decoupled(ExecResp, self.xlen, params)

        self.fp_std = Decoupled(ExecResp, self.xlen, params)

        self.dcache_req = [
            Decoupled(DCacheReq, params, name=f'dcache_req{i}')
            for i in range(self.n_threads)
        ]

        self.dcache_resp = [
            Valid(DCacheResp, self.params, name=f'dcache_resp{i}')
            for i in range(self.n_threads)
        ]

        self.dcache_nack = [
            Valid(DCacheReq, self.params, name=f'dcache_nack{i}')
            for i in range(self.n_threads)
        ]

        self.warp_memory = Signal(self.n_warps)

        # LSQ entry allocated but still missing its store address (split
        # store whose STD half arrived first).
        self.warp_split_addr = Signal(self.n_warps)

        if self.use_smem and self.use_async_copy:
            self.dma_req = Decoupled(SharedMemoryDMAReq,
                                     self.params,
                                     name='dma_req')
            self.dma_commit = Decoupled(SharedMemoryDMACommit,
                                        self.params,
                                        name='dma_commit')

        if sim_debug:
            self.lsu_debug = Valid(LSUDebug, params)

    def elaborate(self, platform):
        m = Module()

        if self.use_smem:
            smem = m.submodules.smem = SharedMemory(self.params)

            if self.use_async_copy:
                m.d.comb += [
                    self.dma_req.connect(smem.dma_req),
                    smem.dma_commit.connect(self.dma_commit),
                ]

        lsq = Array(
            LSQEntry(self.params, name=f'lsq{i}') for i in range(self.n_warps))

        # Include requests allocating an LSQ entry on this edge so that an
        # idle-barrier bypass cannot race an older memory instruction.
        m.d.comb += self.warp_memory.eq(
            Cat(lsq[w].valid
                | (self.exec_req.fire & (self.exec_req.bits.wid == w))
                | (self.fp_std.fire & (self.fp_std.bits.wid == w))
                for w in range(self.n_warps)))

        m.d.comb += self.warp_split_addr.eq(
            Cat(lsq[w].valid & ~lsq[w].addr_valid
                for w in range(self.n_warps)))

        s0_tlb_uncacheable = Signal(self.n_threads)
        for w in range(self.n_threads):
            pma = PMAChecker(self.params)
            setattr(m.submodules, f'pma{w}', pma)

            m.d.comb += [
                pma.paddr.eq(self.exec_req.bits.addr[w]),
                s0_tlb_uncacheable[w].eq(~pma.resp.cacheable),
            ]

        s0_addr_is_smem = Signal(self.n_threads)
        if self.use_smem:
            for w in range(self.n_threads):
                with m.If((self.exec_req.bits.addr[w] >= self.smem_base)
                          & (self.exec_req.bits.addr[w] < (self.smem_base +
                                                           self.smem_size))):
                    m.d.comb += s0_addr_is_smem[w].eq(1)

        m.d.comb += self.exec_req.ready.eq(1)
        with m.If(self.exec_req.valid):
            m.d.comb += self.exec_req.ready.eq(
                ~lsq[self.exec_req.bits.wid].valid
                | (self.exec_req.bits.uop.is_sta
                   & ~self.exec_req.bits.uop.is_std
                   & ~lsq[self.exec_req.bits.wid].addr_valid))

            with m.If(self.exec_req.fire):
                m.d.sync += [
                    lsq[self.exec_req.bits.wid].valid.eq(1),
                    lsq[self.exec_req.bits.wid].uop.eq(self.exec_req.bits.uop),
                    lsq[self.exec_req.bits.wid].uop.lsq_wid.eq(
                        self.exec_req.bits.wid),
                    lsq[self.exec_req.bits.wid].executed.eq(0),
                    lsq[self.exec_req.bits.wid].succeeded.eq(0),
                    lsq[self.exec_req.bits.wid].addr_uncacheable.eq(
                        s0_tlb_uncacheable),
                    Cat(*lsq[self.exec_req.bits.wid].addr).eq(
                        Cat(*self.exec_req.bits.addr)),
                    lsq[self.exec_req.bits.wid].addr_valid.eq(1),
                ]

                if self.use_smem:
                    m.d.sync += lsq[self.exec_req.bits.wid].addr_is_smem.eq(
                        s0_addr_is_smem)

                with m.If(self.exec_req.bits.uop.is_std):
                    m.d.sync += [
                        Cat(*lsq[self.exec_req.bits.wid].data).eq(
                            Cat(*self.exec_req.bits.data)),
                        lsq[self.exec_req.bits.wid].data_valid.eq(1),
                    ]

        if self.sim_debug:
            m.d.comb += [
                self.lsu_debug.valid.eq(self.exec_req.fire),
                self.lsu_debug.bits.wid.eq(self.exec_req.bits.wid),
                self.lsu_debug.bits.uop_id.eq(self.exec_req.bits.uop.uop_id),
                self.lsu_debug.bits.tmask.eq(self.exec_req.bits.uop.tmask),
                self.lsu_debug.bits.opcode.eq(self.exec_req.bits.uop.opcode),
                self.lsu_debug.bits.data_valid.eq(
                    self.exec_req.bits.uop.is_std),
                self.lsu_debug.bits.mem_size.eq(
                    self.exec_req.bits.uop.mem_size),
                self.lsu_debug.bits.lrs1.eq(self.exec_req.bits.uop.lrs1),
                self.lsu_debug.bits.lrs2.eq(self.exec_req.bits.uop.lrs2),
            ]

            for w in range(self.n_threads):
                m.d.comb += [
                    self.lsu_debug.bits.addr[w].eq(
                        sign_extend(self.exec_req.bits.addr[w], self.xlen)),
                    self.lsu_debug.bits.data[w].eq(self.exec_req.bits.data[w]),
                ]

        m.d.comb += self.fp_std.ready.eq(1)
        with m.If(self.fp_std.valid):
            m.d.comb += self.fp_std.ready.eq(
                (lsq[self.fp_std.bits.wid].valid
                 & lsq[self.fp_std.bits.wid].uop.fp_valid
                 & lsq[self.fp_std.bits.wid].uop.uses_stq
                 & ~lsq[self.fp_std.bits.wid].data_valid)
                | (self.exec_req.fire & self.exec_req.bits.uop.fp_valid
                   & self.exec_req.bits.uop.uses_stq
                   & ~self.exec_req.bits.uop.is_std
                   & (self.exec_req.bits.wid == self.fp_std.bits.wid)))

            with m.If(self.fp_std.fire):
                m.d.sync += [
                    lsq[self.fp_std.bits.wid].valid.eq(1),
                    Cat(*lsq[self.fp_std.bits.wid].data).eq(
                        Cat(*self.fp_std.bits.data)),
                    lsq[self.fp_std.bits.wid].data_valid.eq(1),
                ]

        s0_block_req = Array(
            Signal(self.n_threads, name=f's0_block_req{i}')
            for i in range(self.n_warps))
        s1_block_req = Array(
            Signal(self.n_threads, name=f's1_block_req{i}')
            for i in range(self.n_warps))
        s2_block_req = Array(
            Signal(self.n_threads, name=f's2_block_req{i}')
            for i in range(self.n_warps))
        for a, b in zip(s1_block_req, s0_block_req):
            m.d.sync += a.eq(b)
        for a, b in zip(s2_block_req, s1_block_req):
            m.d.sync += a.eq(b)

        lsq_wakeup_valid = Signal(self.n_warps)
        for w in range(self.n_warps):
            block = ~(lsq[w].uop.tmask
                      & ~(s0_block_req[w] | s1_block_req[w])).any()
            m.d.comb += lsq_wakeup_valid[w].eq(lsq[w].valid & ~block)

        lsq_wakeup_idx = Signal(range(self.n_warps))
        lsq_wakeup_e = lsq[lsq_wakeup_idx]
        with m.Switch(lsq_wakeup_idx):
            for i in range(self.n_warps):
                with m.Case(i):
                    for pred in reversed(range(i)):
                        with m.If(lsq_wakeup_valid[pred]):
                            m.d.sync += lsq_wakeup_idx.eq(pred)
                    for succ in reversed(range(i + 1, self.n_warps)):
                        with m.If(lsq_wakeup_valid[succ]):
                            m.d.sync += lsq_wakeup_idx.eq(succ)

        can_fire_incoming = Signal()
        can_fire_wakeup = Signal()

        m.d.comb += [
            can_fire_incoming.eq(self.exec_req.fire
                                 & ~(self.exec_req.bits.uop.is_sta
                                     ^ self.exec_req.bits.uop.is_std)),
            can_fire_wakeup.eq(
                lsq_wakeup_e.valid
                & (lsq_wakeup_e.uop.tmask
                   & ~lsq_wakeup_e.executed
                   & ~lsq_wakeup_e.succeeded
                   & ~(s1_block_req[lsq_wakeup_idx]
                       | s2_block_req[lsq_wakeup_idx])).any()
                & (~lsq_wakeup_e.uop.uses_stq
                   | (lsq_wakeup_e.addr_valid & lsq_wakeup_e.data_valid))),
        ]

        s0_executing = Array(
            Signal(self.n_threads, name=f's0_executing{i}')
            for i in range(self.n_warps))
        s1_executing = Array(
            Signal(self.n_threads, name=f's1_executing{i}')
            for i in range(self.n_warps))
        s1_set_executed = Array(
            Signal(self.n_threads, name=f's1_set_executed({i})')
            for i in range(self.n_warps))
        for a, b in zip(s1_executing, s0_executing):
            m.d.sync += a.eq(b)
        for a, b in zip(s1_set_executed, s1_executing):
            m.d.comb += a.eq(b)

        will_fire_incoming = Signal()
        will_fire_wakeup = Signal()
        m.d.comb += [
            will_fire_incoming.eq(can_fire_incoming),
            will_fire_wakeup.eq(can_fire_wakeup & ~will_fire_incoming),
        ]

        #
        # Memory access
        #

        for w in range(self.n_threads):
            dmem_req = Decoupled(DCacheReq, self.params, name=f'dmem_req{w}')
            dmem_is_smem = Signal()

            store_data = Signal(self.xlen)
            store_gen = StoreGen(max_size=self.xlen // 8)
            setattr(m.submodules, f'store_gen{w}', store_gen)
            m.d.comb += [
                store_gen.typ.eq(dmem_req.bits.uop.mem_size),
                store_gen.addr.eq(0),
                store_gen.data_in.eq(store_data),
                dmem_req.bits.data.eq(store_gen.data_out),
            ]

            with m.If(will_fire_incoming):
                m.d.comb += [
                    dmem_req.valid.eq(self.exec_req.bits.uop.tmask[w]),
                    dmem_req.bits.uop.eq(self.exec_req.bits.uop),
                    dmem_req.bits.uop.lsq_wid.eq(self.exec_req.bits.wid),
                    dmem_req.bits.uop.lsq_tid.eq(w),
                    dmem_req.bits.addr.eq(self.exec_req.bits.addr[w]),
                    store_data.eq(self.exec_req.bits.data[w]),
                    dmem_is_smem.eq(s0_addr_is_smem[w]),
                    s0_executing[self.exec_req.bits.wid][w].eq(dmem_req.fire),
                    s0_block_req[self.exec_req.bits.wid][w].eq(dmem_req.fire),
                ]

            with m.Elif(will_fire_wakeup):
                m.d.comb += [
                    dmem_req.valid.eq(lsq_wakeup_e.uop.tmask[w]
                                      & ~lsq_wakeup_e.executed[w]
                                      & ~lsq_wakeup_e.succeeded[w]),
                    dmem_req.bits.uop.eq(lsq_wakeup_e.uop),
                    dmem_req.bits.uop.lsq_tid.eq(w),
                    dmem_req.bits.addr.eq(lsq_wakeup_e.addr[w]),
                    store_data.eq(lsq_wakeup_e.data[w]),
                    dmem_is_smem.eq(lsq_wakeup_e.addr_is_smem[w]),
                    s0_executing[lsq_wakeup_idx][w].eq(dmem_req.fire),
                    s0_block_req[lsq_wakeup_idx][w].eq(dmem_req.fire),
                ]

            m.d.comb += dmem_req.connect(self.dcache_req[w])
            if self.use_smem:
                with m.If(dmem_is_smem):
                    m.d.comb += [
                        dmem_req.connect(smem.req[w]),
                        self.dcache_req[w].valid.eq(0),
                    ]

        for i in range(self.n_warps):
            with m.If(s1_set_executed[i].any()):
                m.d.sync += lsq[i].executed.eq(lsq[i].executed
                                               | s1_set_executed[i])

        #
        # Writeback
        #

        for w in range(self.n_threads):
            with m.If(self.dcache_nack[w].valid):
                lsq_wid = self.dcache_nack[w].bits.uop.lsq_wid

                with m.Switch(self.dcache_nack[w].bits.uop.lsq_tid):
                    for t in range(self.n_threads):
                        with m.Case(t):
                            m.d.sync += lsq[lsq_wid].executed[t].eq(0)

            with m.If(self.dcache_resp[w].valid):
                lsq_wid = self.dcache_resp[w].bits.uop.lsq_wid

                with m.Switch(self.dcache_resp[w].bits.uop.lsq_tid):
                    for t in range(self.n_threads):
                        with m.Case(t):
                            m.d.sync += lsq[lsq_wid].succeeded[t].eq(1)
                            with m.If(lsq[lsq_wid].uop.is_load):
                                m.d.sync += lsq[lsq_wid].data[t].eq(
                                    self.dcache_resp[w].bits.data)

            if self.use_smem:
                with m.If(smem.nack[w].valid):
                    lsq_wid = smem.nack[w].bits.uop.lsq_wid

                    with m.Switch(smem.nack[w].bits.uop.lsq_tid):
                        for t in range(self.n_threads):
                            with m.Case(t):
                                m.d.sync += lsq[lsq_wid].executed[t].eq(0)

                with m.If(smem.resp[w].valid):
                    lsq_wid = smem.resp[w].bits.uop.lsq_wid

                    with m.Switch(smem.resp[w].bits.uop.lsq_tid):
                        for t in range(self.n_threads):
                            with m.Case(t):
                                m.d.sync += lsq[lsq_wid].succeeded[t].eq(1)
                                with m.If(lsq[lsq_wid].uop.is_load):
                                    m.d.sync += lsq[lsq_wid].data[t].eq(
                                        smem.resp[w].bits.data)

        #
        # Commit
        #
        for w in reversed(range(self.n_warps)):
            with m.If(lsq[w].valid & (lsq[w].uop.tmask == lsq[w].succeeded)):
                with m.If(lsq[w].uop.uses_ldq):
                    m.d.comb += [
                        self.exec_iresp.valid.eq(
                            lsq[w].uop.dst_rtype == RegisterType.FIX),
                        self.exec_iresp.bits.wid.eq(w),
                        self.exec_iresp.bits.uop.eq(lsq[w].uop),
                        Cat(*self.exec_iresp.bits.data).eq(Cat(*lsq[w].data)),
                        self.exec_fresp.valid.eq(
                            lsq[w].uop.dst_rtype == RegisterType.FLT),
                        self.exec_fresp.bits.wid.eq(w),
                        self.exec_fresp.bits.uop.eq(lsq[w].uop),
                        Cat(*self.exec_fresp.bits.data).eq(Cat(*lsq[w].data)),
                    ]

                with m.If(lsq[w].uop.uses_stq):
                    m.d.sync += [
                        lsq[w].valid.eq(0),
                        lsq[w].addr_valid.eq(0),
                        lsq[w].data_valid.eq(0),
                    ]

        with m.If(self.exec_iresp.fire):
            m.d.sync += [
                lsq[self.exec_iresp.bits.wid].valid.eq(0),
                lsq[self.exec_iresp.bits.wid].addr_valid.eq(0),
                lsq[self.exec_iresp.bits.wid].data_valid.eq(0),
            ]

        with m.If(self.exec_fresp.fire):
            m.d.sync += [
                lsq[self.exec_fresp.bits.wid].valid.eq(0),
                lsq[self.exec_fresp.bits.wid].addr_valid.eq(0),
                lsq[self.exec_fresp.bits.wid].data_valid.eq(0),
            ]

        return m
