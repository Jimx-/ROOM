from amaranth import *
from amaranth import tracer
from amaranth.utils import log2_int

from groom.fu import ExecResp

from room.consts import *
from room.types import HasCoreParams, MicroOp
from room.dcache import DCache, DCacheReq, DCacheResp
from room.lsu import LoadGen, StoreGen

from roomsoc.interconnect.stream import Valid, Decoupled


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

        for w in range(self.n_threads):
            m.d.comb += self.req[w].ready.eq(1)

        #
        # S0
        #

        s0_valids = Cat(req.valid for req in self.req)
        s0_req = [
            DCacheReq(self.params, name=f's0_req{i}')
            for i in range(self.n_threads)
        ]
        s0_banks = [
            Signal(bank_bits, name=f's0_bank{i}')
            for i in range(self.n_threads)
        ]
        s0_idxs = [
            Signal(bidx_bits, name=f's0_idx{i}') for i in range(self.n_threads)
        ]
        s0_bank_conflicts = Signal(self.n_threads)
        s0_bank_gnts = [
            Signal(self.n_threads, name=f's0_bank_gnt{i}')
            for i in range(n_banks)
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
                s0_banks[w].eq(
                    (self.req[w].bits.addr >> bank_off_bits)[:bank_bits]),
                s0_idxs[w].eq(
                    (self.req[w].bits.addr >> bidx_off_bits)[:bidx_bits]),
            ]

            c = Const(0)
            for i in range(w):
                c |= s0_valids[i] & (s0_banks[i] == s0_banks[w])
            m.d.comb += s0_bank_conflicts[w].eq(c)

        for b in range(n_banks):
            for w in range(self.n_threads):
                with m.If(s0_valids[w] & (s0_banks[w] == b)
                          & ~s0_bank_conflicts[w]):
                    m.d.comb += s0_bank_gnts[b][w].eq(1)

        #
        # S1
        #

        s1_valids = Signal.like(s0_valids)
        s1_req = [
            DCacheReq(self.params, name=f's1_req{i}')
            for i in range(self.n_threads)
        ]
        s1_banks = [
            Signal(bank_bits, name=f's1_bank{i}')
            for i in range(self.n_threads)
        ]
        s1_idxs = [
            Signal(bidx_bits, name=f's1_idx{i}') for i in range(self.n_threads)
        ]
        s1_pipe_selection = [
            Signal(self.n_threads, name=f's1_pipe_selection{i}')
            for i in range(self.n_threads)
        ]
        s1_idx_match = [
            Signal(self.n_threads, name=f's1_idx_match{i}')
            for i in range(self.n_threads)
        ]
        s1_bank_selection = [
            Signal(range(n_banks), name=f's1_bank_selection{i}')
            for i in range(self.n_threads)
        ]
        s1_nacks = Signal(self.mem_width)

        m.d.sync += s1_valids.eq(s0_valids)
        for w in range(self.n_threads):
            m.d.sync += [
                s1_req[w].eq(s0_req[w]),
                s1_banks[w].eq(s0_banks[w]),
                s1_idxs[w].eq(s0_idxs[w]),
            ]

            m.d.comb += [
                s1_pipe_selection[w].eq(1 << w),
                s1_idx_match[w][w].eq(1),
            ]
            for i in reversed(range(w)):
                with m.If(s1_valids[i] & (s1_banks[i] == s1_banks[w])):
                    m.d.comb += s1_pipe_selection[w].eq(1 << i)
                m.d.comb += s1_idx_match[w][i].eq(s1_idxs[i] == s1_idxs[w])

            m.d.comb += s1_nacks[w].eq(s1_valids[w] & (
                (s1_pipe_selection[w] & ~s1_idx_match[w]) != 0))

            for i in reversed(range(self.n_threads)):
                with m.If(s1_pipe_selection[w][i]):
                    m.d.comb += s1_bank_selection[w].eq(s1_banks[i])

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
                s2_bank_selection[w].eq(s1_bank_selection[w]),
            ]

        s2_bank_reads = Array(
            Signal(self.xlen, name=f's2_bank_read{b}') for b in range(n_banks))

        for b in range(n_banks):
            mem = Memory(width=self.xlen, depth=bank_size)

            mem_read = mem.read_port(transparent=False)
            setattr(m.submodules, f'mem_read{b}', mem_read)

            for i in range(self.n_threads):
                with m.If(s0_bank_gnts[b][i]):
                    m.d.comb += mem_read.addr.eq(s0_idxs[i])

            m.d.sync += s2_bank_reads[b].eq(mem_read.data)

            mem_write = mem.write_port()
            setattr(m.submodules, f'mem_write{b}', mem_write)

            for i in range(self.n_threads):
                with m.If(s0_bank_gnts[b][i]):
                    m.d.comb += [
                        mem_write.addr.eq(s0_idxs[i]),
                        mem_write.data.eq(s0_req[i].data),
                        mem_write.en.eq(
                            MemoryCommand.is_write(s0_req[i].uop.mem_cmd)),
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

    def __init__(self, dbus, dbus_mmio, params):
        super().__init__(params)

        self.cache_enable = Signal()

        self.dbus = dbus
        self.dbus_mmio = dbus_mmio

        self.exec_req = Decoupled(ExecResp, self.xlen, params)

        self.exec_iresp = Decoupled(ExecResp, self.xlen, params)

        self.exec_fresp = Decoupled(ExecResp, self.xlen, params)

        self.fp_std = Decoupled(ExecResp, self.xlen, params)

    def elaborate(self, platform):
        m = Module()

        m.domains += ClockDomain('dcache', local=True)
        m.d.comb += [
            ClockSignal('dcache').eq(ClockSignal()),
            ResetSignal('dcache').eq(~self.cache_enable),
        ]

        dcache = m.submodules.dcache = DomainRenamer('dcache')(DCache(
            self.dbus, self.dbus_mmio, self.params))
        if self.use_smem:
            smem = m.submodules.smem = SharedMemory(self.params)

        lsq = Array(
            LSQEntry(self.params, name=f'lsq{i}') for i in range(self.n_warps))

        s0_tlb_uncacheable = Signal(self.n_threads)
        for w in range(self.n_threads):
            for origin, size in self.io_regions.items():
                with m.If((self.exec_req.bits.addr[w] >= origin)
                          & (self.exec_req.bits.addr[w] < (origin + size))):
                    m.d.comb += s0_tlb_uncacheable[w].eq(1)

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

            m.d.comb += dmem_req.connect(dcache.req[w])
            if self.use_smem:
                with m.If(dmem_is_smem):
                    m.d.comb += [
                        dmem_req.connect(smem.req[w]),
                        dcache.req[w].valid.eq(0),
                    ]

        for i in range(self.n_warps):
            with m.If(s1_set_executed[i].any()):
                m.d.sync += lsq[i].executed.eq(lsq[i].executed
                                               | s1_set_executed[i])

        #
        # Writeback
        #

        for w in range(self.n_threads):
            with m.If(dcache.nack[w].valid):
                lsq_wid = dcache.nack[w].bits.uop.lsq_wid

                with m.Switch(dcache.nack[w].bits.uop.lsq_tid):
                    for t in range(self.n_threads):
                        with m.Case(t):
                            m.d.sync += lsq[lsq_wid].executed[t].eq(0)

            with m.If(dcache.resp[w].valid):
                lsq_wid = dcache.resp[w].bits.uop.lsq_wid

                with m.Switch(dcache.resp[w].bits.uop.lsq_tid):
                    for t in range(self.n_threads):
                        with m.Case(t):
                            m.d.sync += lsq[lsq_wid].succeeded[t].eq(1)
                            with m.If(lsq[lsq_wid].uop.is_load):
                                m.d.sync += lsq[lsq_wid].data[t].eq(
                                    dcache.resp[w].bits.data)

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
