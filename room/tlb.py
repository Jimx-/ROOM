from amaranth import *
from amaranth.hdl.rec import Direction
from amaranth.utils import log2_int

from room.consts import *
from room.types import HasCoreParams
from room.mmu import PTBR, PageTableWalker, PMAChecker
from room.exc import MStatus

from roomsoc.interconnect.stream import Decoupled, Valid


class SFenceReq(HasCoreParams, Record):

    def __init__(self, params, name=None, src_loc_at=0):
        HasCoreParams.__init__(self, params)

        Record.__init__(self, [
            ('rs1', 1, Direction.FANOUT),
            ('rs2', 1, Direction.FANOUT),
            ('vaddr', self.vaddr_bits, Direction.FANOUT),
        ],
                        name=name,
                        src_loc_at=1 + src_loc_at)


class TLBReq(HasCoreParams, Record):

    def __init__(self, log_max_size, params, name=None, src_loc_at=0):
        HasCoreParams.__init__(self, params)

        Record.__init__(self, [
            ('vaddr', self.vaddr_bits_extended, Direction.FANOUT),
            ('passthru', 1, Direction.FANOUT),
            ('size', range(log_max_size + 1), Direction.FANOUT),
            ('cmd', MemoryCommand, Direction.FANOUT),
        ],
                        name=name,
                        src_loc_at=1 + src_loc_at)


class TLBResp(HasCoreParams, Record):

    def __init__(self, params, name=None, src_loc_at=0):
        HasCoreParams.__init__(self, params)

        exc_layout = [
            ('ld', 1, Direction.FANOUT),
            ('st', 1, Direction.FANOUT),
            ('inst', 1, Direction.FANOUT),
        ]

        Record.__init__(self, [
            ('miss', 1, Direction.FANOUT),
            ('paddr', self.paddr_bits, Direction.FANOUT),
            ('pf', exc_layout),
            ('ae', exc_layout),
            ('ma', exc_layout),
            ('cacheable', 1, Direction.FANOUT),
        ],
                        name=name,
                        src_loc_at=1 + src_loc_at)


class TLB(HasCoreParams, Elaboratable):

    class Entry(HasCoreParams, Record):

        def __init__(self,
                     tag_bits,
                     params,
                     superpage=False,
                     name=None,
                     src_loc_at=0):
            HasCoreParams.__init__(self, params)

            self.superpage = superpage

            layout = [
                ('tag', tag_bits, Direction.FANOUT),
                ('ppn', self.ppn_bits, Direction.FANOUT),
                ('u', 1, Direction.FANOUT),
                ('g', 1, Direction.FANOUT),
                ('ae', 1, Direction.FANOUT),
                ('sr', 1, Direction.FANOUT),
                ('sw', 1, Direction.FANOUT),
                ('sx', 1, Direction.FANOUT),
                ('pr', 1, Direction.FANOUT),
                ('pw', 1, Direction.FANOUT),
                ('px', 1, Direction.FANOUT),
                ('c', 1, Direction.FANOUT),
            ]

            if superpage:
                layout += [
                    ('level', range(self.pg_levels)),
                ]

            Record.__init__(self, layout, name=name, src_loc_at=1 + src_loc_at)

    def __init__(self,
                 req_width,
                 params,
                 log_max_size,
                 n_sets,
                 n_ways,
                 n_banks,
                 n_superpage_entries=4):
        HasCoreParams.__init__(self, params)

        self.req_width = req_width
        self.log_max_size = log_max_size
        self.n_sets = n_sets
        self.n_ways = n_ways
        self.n_banks = n_banks
        self.n_superpage_entries = n_superpage_entries

        self.prv = Signal(PrivilegeMode)
        self.status = MStatus(self.xlen)
        self.ptbr = PTBR(self.xlen)

        self.req = [
            Decoupled(TLBReq, log_max_size, params, name=f'req{w}')
            for w in range(req_width)
        ]
        self.resp = [
            Valid(TLBResp, params, name=f'resp{w}') for w in range(req_width)
        ]
        self.sfence = Valid(SFenceReq, params, name=f'sfence')
        self.miss_ready = Signal()

        self.ptw_req = Decoupled(PageTableWalker.Request, params)
        self.ptw_resp = Valid(PageTableWalker.Response, params)

    def elaborate(self, platform):
        m = Module()

        bank_size = self.n_sets // self.n_banks
        bank_bits = log2_int(self.n_banks)
        bidx_bits = log2_int(bank_size)
        tag_bits = self.vpn_bits - bank_bits - bidx_bits
        tag_off_bits = bank_bits + bidx_bits
        row_bits = len(TLB.Entry(tag_bits, self.params))

        priv_s = self.prv[0]
        priv_use_vm = self.prv <= PrivilegeMode.S

        r_refill_tag = Signal(self.vpn_bits)
        r_replace_way = Signal(range(self.n_ways))
        r_superpage_replace_way = Signal(range(self.n_superpage_entries))
        refill_valid = Signal()
        refill_done = self.use_vm & self.ptw_resp.valid
        refill_invalidated = Signal()

        cacheable = Signal(self.req_width)
        prot_r = Signal(self.req_width)
        prot_w = Signal(self.req_width)
        prot_x = Signal(self.req_width)

        new_entry = TLB.Entry(tag_bits, self.params)
        m.d.comb += [
            new_entry.tag.eq((r_refill_tag >> tag_off_bits)[:tag_bits]),
            new_entry.ppn.eq(self.ptw_resp.bits.pte.ppn),
            new_entry.u.eq(self.ptw_resp.bits.pte.u),
            new_entry.g.eq(self.ptw_resp.bits.pte.g),
            new_entry.ae.eq(self.ptw_resp.bits.ae_leaf),
            new_entry.sr.eq(self.ptw_resp.bits.pte.sr()),
            new_entry.sw.eq(self.ptw_resp.bits.pte.sw()),
            new_entry.sx.eq(self.ptw_resp.bits.pte.sx()),
            new_entry.pr.eq(prot_r[0]),
            new_entry.pw.eq(prot_w[0]),
            new_entry.px.eq(prot_x[0]),
            new_entry.c.eq(cacheable[0]),
        ]

        superpage_entries = [
            TLB.Entry(self.vpn_bits,
                      self.params,
                      superpage=True,
                      name=f'superpage_entry{i}')
            for i in range(self.n_superpage_entries)
        ]
        superpage_valid = Signal(self.n_superpage_entries)
        with m.If(self.sfence.valid):
            m.d.sync += superpage_valid.eq(0)
        with m.Elif(refill_done & ~refill_invalidated
                    & (self.ptw_resp.bits.level < (self.pg_levels - 1))):
            with m.Switch(r_superpage_replace_way):
                for w in range(self.n_superpage_entries):
                    with m.Case(w):
                        m.d.sync += [
                            superpage_valid[w].eq(1),
                            superpage_entries[w].tag.eq(r_refill_tag),
                            superpage_entries[w].ppn.eq(new_entry.ppn),
                            superpage_entries[w].u.eq(new_entry.u),
                            superpage_entries[w].g.eq(new_entry.g),
                            superpage_entries[w].ae.eq(new_entry.ae),
                            superpage_entries[w].sr.eq(new_entry.sr),
                            superpage_entries[w].sw.eq(new_entry.sw),
                            superpage_entries[w].sx.eq(new_entry.sx),
                            superpage_entries[w].pr.eq(new_entry.pr),
                            superpage_entries[w].pw.eq(new_entry.pw),
                            superpage_entries[w].px.eq(new_entry.px),
                            superpage_entries[w].c.eq(new_entry.c),
                            superpage_entries[w].level.eq(
                                self.ptw_resp.bits.level),
                        ]

        #
        # S0
        #

        s0_valid = Signal(self.req_width)
        s0_vpn = [
            Signal(self.vpn_bits, name=f's0_vpn{w}')
            for w in range(self.req_width)
        ]
        for w in range(self.req_width):
            m.d.comb += [
                self.req[w].ready.eq(1),
                s0_valid[w].eq(self.req[w].fire),
                s0_vpn[w].eq(self.req[w].bits.vaddr[self.pg_offset_bits:self.
                                                    vaddr_bits]),
            ]

        s0_bank = [
            Signal(bank_bits, name=f's0_bank{w}')
            for w in range(self.req_width)
        ]
        s0_idx = [
            Signal(bidx_bits, name=f's0_idx{w}') for w in range(self.req_width)
        ]
        s0_bank_conflict = Signal(self.req_width)
        s0_do_read = s0_valid & ~s0_bank_conflict
        s0_bank_gnt = [
            Signal(self.req_width, name=f's0_bank_read_gnt{i}')
            for i in range(self.n_banks)
        ]
        s0_pipe_selection = [
            Signal(self.req_width, name=f's0_pipe_selection{w}')
            for w in range(self.req_width)
        ]
        s0_idx_match = [
            Signal(self.req_width, name=f's0_idx_match{w}')
            for w in range(self.req_width)
        ]
        s0_bank_selection = [
            Signal(range(self.n_banks), name=f's0_bank_selection{w}')
            for w in range(self.req_width)
        ]
        s0_nack = Signal(self.req_width)

        for w in range(self.req_width):
            m.d.comb += [
                s0_bank[w].eq(s0_vpn[w][:bank_bits]),
                s0_idx[w].eq((s0_vpn[w] >> bank_bits)[:bidx_bits]),
            ]

            c = Const(0)
            for i in range(w):
                c |= s0_valid[i] & (s0_bank[i] == s0_bank[w])
            m.d.comb += s0_bank_conflict[w].eq(c)

        for b in range(self.n_banks):
            for w in range(self.req_width):
                with m.If((s0_bank[w] == b) & s0_do_read[w]):
                    m.d.comb += s0_bank_gnt[b][w].eq(1)

        for w in range(self.req_width):
            m.d.comb += [
                s0_pipe_selection[w].eq(1 << w),
                s0_idx_match[w][w].eq(1),
            ]

            for i in reversed(range(w)):
                with m.If(s0_valid[i] & (s0_bank[i] == s0_bank[w])):
                    m.d.comb += s0_pipe_selection[w].eq(1 << i)
                m.d.comb += s0_idx_match[w][i].eq(s0_idx[i] == s0_idx[w])

            m.d.comb += s0_nack[w].eq(s0_valid[w] & (
                (s0_pipe_selection[w] & ~s0_idx_match[w]) != 0))

            for i in reversed(range(self.req_width)):
                with m.If(s0_pipe_selection[w][i]):
                    m.d.comb += s0_bank_selection[w].eq(s0_bank[i])

        #
        # Memory banks
        #

        s1_bank_selection = [
            Signal(range(self.n_banks), name=f's1_bank_selection{i}')
            for i in range(self.req_width)
        ]
        m.d.sync += Cat(*s1_bank_selection).eq(Cat(*s0_bank_selection))

        s1_data = [[
            TLB.Entry(tag_bits, self.params, name=f's1_data{i}_way{w}')
            for w in range(self.n_ways)
        ] for i in range(self.req_width)]
        s1_entry_valid = [
            Signal(self.n_ways, name=f's1_entry_valid{i}')
            for i in range(self.req_width)
        ]

        for w in range(self.n_ways):
            s1_bank_read = Array(
                Signal(row_bits, name=f's1_bank_read{w}_b{b}')
                for b in range(self.n_banks))
            s1_bank_valid = Array(
                Signal(name=f's1_bank_valid{w}_b{b}')
                for b in range(self.n_banks))

            for b in range(self.n_banks):
                mem = Memory(width=row_bits, depth=bank_size)
                entry_valid = Signal(bank_size, name=f'entry_valid{w}_b{b}')

                mem_read = mem.read_port(transparent=False)
                setattr(m.submodules, f'mem_read{b}_{w}', mem_read)

                for i in range(self.req_width):
                    with m.If(s0_bank_gnt[b][i]):
                        m.d.comb += mem_read.addr.eq(s0_idx[i])

                        with m.Switch(s0_idx[i]):
                            for s in range(bank_size):
                                with m.Case(s):
                                    m.d.sync += s1_bank_valid[b].eq(
                                        entry_valid[s])

                m.d.comb += s1_bank_read[b].eq(mem_read.data)

                mem_write = mem.write_port()
                setattr(m.submodules, f'mem_write{b}_{w}', mem_write)

                with m.If(refill_done & ~refill_invalidated
                          & (self.ptw_resp.bits.level == (self.pg_levels - 1))
                          & (r_refill_tag[:bank_bits] == b)
                          & (r_replace_way == w)):
                    refill_set = (r_refill_tag >> bank_bits)[:bidx_bits]
                    m.d.comb += [
                        mem_write.addr.eq(refill_set),
                        mem_write.data.eq(new_entry),
                        mem_write.en.eq(1),
                    ]

                    with m.Switch(refill_set):
                        for s in range(bank_size):
                            with m.Case(s):
                                m.d.sync += entry_valid[s].eq(1)

                with m.If(self.sfence.valid):
                    m.d.sync += entry_valid.eq(0)

            for i in range(self.req_width):
                m.d.comb += [
                    s1_data[i][w].eq(s1_bank_read[s1_bank_selection[i]]),
                    s1_entry_valid[i][w].eq(
                        s1_bank_valid[s1_bank_selection[i]]),
                ]

        #
        # S1
        #
        s1_valid = Signal(self.req_width)
        s1_req = [
            TLBReq(self.log_max_size, self.params, name=f's1_req{w}')
            for w in range(self.req_width)
        ]
        s1_vpn = [
            Signal(self.vpn_bits, name=f's1_vpn{w}')
            for w in range(self.req_width)
        ]
        s1_vm_enabled = Signal(self.req_width)
        s1_nack = Signal(self.req_width)
        s1_tag_match_way = [
            Signal(self.n_ways, name=f's1_tag_match_way{w}')
            for w in range(self.req_width)
        ]
        s1_superpage_hit = [
            Signal(self.n_superpage_entries, name=f's1_superpage_hit{w}')
            for w in range(self.req_width)
        ]
        s1_hits = [
            Signal(self.n_ways + self.n_superpage_entries, name=f's1_hits{w}')
            for w in range(self.req_width)
        ]
        s1_tlb_hit = Signal(self.req_width)
        s1_tlb_miss = Signal(self.req_width)

        for w in range(self.req_width):
            req_vpn = s1_req[w].vaddr[self.pg_offset_bits:self.vaddr_bits]

            for i in range(self.n_superpage_entries):
                tag_match = self.use_vm & superpage_valid[i]
                for l in range(self.pg_levels):
                    base = (self.pg_levels - l - 1) * self.pg_level_bits
                    tag_match &= (l > superpage_entries[i].level) | (
                        base >= self.vpn_bits) | (
                            ((superpage_entries[i].tag ^ req_vpn) >>
                             base)[:self.pg_level_bits] == 0)
                m.d.comb += s1_superpage_hit[w][i].eq(tag_match)

        m.d.sync += s1_nack.eq(s0_nack)
        for i in range(self.req_width):
            m.d.sync += [
                s1_valid[i].eq(s0_valid[i]),
                s1_req[i].eq(self.req[i].bits),
                s1_vpn[i].eq(s0_vpn[i]),
            ]

            m.d.comb += [
                s1_vm_enabled[i].eq(self.use_vm & priv_use_vm
                                    & self.ptbr.mode[-1]
                                    & ~s1_req[i].passthru),
                s1_tag_match_way[i].eq(
                    Cat((s1_data[i][w].tag ==
                         (s1_vpn[i] >> tag_off_bits)[:tag_bits])
                        & s1_entry_valid[i][w] & ~s1_nack[i]
                        for w in range(self.n_ways))),
                s1_hits[i].eq(Cat(s1_tag_match_way[i], s1_superpage_hit[i])),
                s1_tlb_hit[i].eq(s1_hits[i].any()),
                s1_tlb_miss[i].eq(s1_vm_enabled[i] & ~s1_tlb_hit[i]),
            ]

        #
        # Refill
        #

        refill_done_d1 = Signal()
        m.d.sync += refill_done_d1.eq(refill_done)

        for w in range(self.req_width):
            with m.If(s1_valid[w] & ~s1_nack[w] & s1_tlb_miss[w]
                      & ~refill_valid
                      & ~refill_done_d1):
                m.d.comb += [
                    self.ptw_req.valid.eq(1),
                    self.ptw_req.bits.vpn.eq(s1_vpn[w]),
                ]

                m.d.sync += r_refill_tag.eq(s1_vpn[w])

        with m.If(self.ptw_req.fire):
            m.d.sync += [
                refill_valid.eq(1),
                r_replace_way.eq(r_replace_way + 1),
                r_superpage_replace_way.eq(r_superpage_replace_way + 1),
            ]
        with m.Elif(refill_done):
            m.d.sync += refill_valid.eq(0)

        with m.If(self.sfence.valid):
            m.d.sync += refill_invalidated.eq(1)
        with m.If(~refill_valid):
            m.d.sync += refill_invalidated.eq(0)

        m.d.comb += self.miss_ready.eq(~refill_valid)

        #
        # PMA checks
        #

        mpu_ppn = [
            Mux(refill_done, self.ptw_resp.bits.pte.ppn,
                s1_req[w].vaddr[self.pg_offset_bits:])
            for w in range(self.req_width)
        ]
        mpu_paddr = [
            Cat(s1_req[w].vaddr[:self.pg_offset_bits], mpu_ppn[w])
            for w in range(self.req_width)
        ]

        for w in range(self.req_width):
            pma = PMAChecker(self.params)
            setattr(m.submodules, f'pma{w}', pma)

            m.d.comb += [
                pma.paddr.eq(mpu_paddr[w]),
                cacheable[w].eq(pma.resp.cacheable),
                prot_r[w].eq(pma.resp.r),
                prot_w[w].eq(pma.resp.w),
                prot_x[w].eq(pma.resp.x),
            ]

        #
        # Response
        #

        cmd_read = Signal(self.req_width)
        cmd_write = Signal(self.req_width)

        all_entries = [
            s1_data[w] + superpage_entries for w in range(self.req_width)
        ]
        ptw_ae_array = [
            Signal(len(all_entries[0]), name=f'ptw_ae{w}')
            for w in range(self.req_width)
        ]
        priv_rw_array = [
            Signal(len(all_entries[0]), name=f'priv_rw{w}')
            for w in range(self.req_width)
        ]
        priv_x_array = [
            Signal(len(all_entries[0]), name=f'priv_x{w}')
            for w in range(self.req_width)
        ]
        r_ok_array = [
            Signal(len(all_entries[0]), name=f'r_ok{w}')
            for w in range(self.req_width)
        ]
        w_ok_array = [
            Signal(len(all_entries[0]), name=f'w_ok{w}')
            for w in range(self.req_width)
        ]
        x_ok_array = [
            Signal(len(all_entries[0]), name=f'x_ok{w}')
            for w in range(self.req_width)
        ]
        pr_ok_array = [
            Signal(len(all_entries[0]), name=f'pr_ok{w}')
            for w in range(self.req_width)
        ]
        pw_ok_array = [
            Signal(len(all_entries[0]), name=f'pw_ok{w}')
            for w in range(self.req_width)
        ]
        px_ok_array = [
            Signal(len(all_entries[0]), name=f'px_ok{w}')
            for w in range(self.req_width)
        ]
        c_array = [
            Signal(len(all_entries[0]), name=f'c_array{w}')
            for w in range(self.req_width)
        ]

        pf_ld_array = [
            Signal(len(all_entries[0]), name=f'pf_ld{w}')
            for w in range(self.req_width)
        ]
        pf_st_array = [
            Signal(len(all_entries[0]), name=f'pf_st{w}')
            for w in range(self.req_width)
        ]
        pf_inst_array = [
            Signal(len(all_entries[0]), name=f'pf_inst{w}')
            for w in range(self.req_width)
        ]
        ae_ld_array = [
            Signal(len(all_entries[0]), name=f'ae_ld{w}')
            for w in range(self.req_width)
        ]
        ae_st_array = [
            Signal(len(all_entries[0]), name=f'ae_st{w}')
            for w in range(self.req_width)
        ]
        misaligned = Signal(self.req_width)

        for w in range(self.req_width):
            m.d.comb += [
                cmd_read[w].eq(MemoryCommand.is_read(s1_req[w].cmd)),
                cmd_write[w].eq(MemoryCommand.is_write(s1_req[w].cmd)),
            ]

            with m.Switch(s1_req[w].size):
                for i in range(self.log_max_size + 1):
                    with m.Case(i):
                        m.d.comb += misaligned[w].eq(
                            (s1_req[w].vaddr & ((1 << i) - 1)).any())

            for i, entry in enumerate(all_entries[w]):
                m.d.comb += [
                    ptw_ae_array[w][i].eq(entry.ae),
                    priv_rw_array[w][i].eq((
                        (~priv_s | self.status.sum) & entry.u)
                                           | (priv_s & ~entry.u)),
                    priv_x_array[w][i].eq(priv_s ^ entry.u),
                    #
                    r_ok_array[w][i].eq(priv_rw_array[w][i]
                                        & (entry.sr
                                           | (self.status.mxr & entry.sx))),
                    w_ok_array[w][i].eq(priv_rw_array[w][i] & entry.sw),
                    x_ok_array[w][i].eq(priv_x_array[w][i] & entry.sx),
                    #
                    pr_ok_array[w][i].eq(entry.pr & ~ptw_ae_array[w][i]),
                    pw_ok_array[w][i].eq(entry.pw & ~ptw_ae_array[w][i]),
                    px_ok_array[w][i].eq(entry.px & ~ptw_ae_array[w][i]),
                    c_array[w][i].eq(entry.c),
                    #
                    pf_ld_array[w]
                    [i].eq(cmd_read[w]
                           & ~(r_ok_array[w][i] | ptw_ae_array[w][i])),
                    pf_st_array[w][i].eq(
                        cmd_write[w]
                        & ~(w_ok_array[w][i] | ptw_ae_array[w][i])),
                    pf_inst_array[w][i].eq(~(x_ok_array[w][i]
                                             | ptw_ae_array[w][i])),
                    #
                    ae_ld_array[w][i].eq(cmd_read[w] & ~pr_ok_array[w][i]),
                    ae_st_array[w][i].eq(cmd_write[w] & ~pw_ok_array[w][i]),
                ]

        ppn = [
            Signal(self.ppn_bits, name=f'ppn{w}')
            for w in range(self.req_width)
        ]

        for w in range(self.req_width):
            for i, entry in enumerate(all_entries[w]):
                with m.If(s1_hits[w][i]):
                    if entry.superpage:
                        res = (entry.ppn >> self.pg_level_bits *
                               (self.pg_levels - 1))[:self.pg_level_bits]
                        for l in range(1, self.pg_levels):
                            base = (self.pg_levels - l -
                                    1) * self.pg_level_bits
                            res = Cat(
                                ((Mux(l > entry.level, s1_vpn[w], 0)
                                  | entry.ppn) >> base)[:self.pg_level_bits],
                                res)
                        m.d.comb += ppn[w].eq(res)
                    else:
                        m.d.comb += ppn[w].eq(entry.ppn)

        for w in range(self.req_width):
            m.d.comb += [
                self.resp[w].valid.eq(s1_valid[w]),
                self.resp[w].bits.miss.eq(refill_done | s1_tlb_miss[w]),
            ]

            with m.If(s1_vm_enabled[w]):
                m.d.comb += [
                    self.resp[w].bits.pf.ld.eq(
                        (pf_ld_array[w] & s1_hits[w]).any()),
                    self.resp[w].bits.pf.st.eq(
                        (pf_st_array[w] & s1_hits[w]).any()),
                    self.resp[w].bits.pf.inst.eq(
                        (pf_inst_array[w] & s1_hits[w]).any()),
                    #
                    self.resp[w].bits.ae.ld.eq(
                        (ae_ld_array[w] & s1_hits[w]).any()),
                    self.resp[w].bits.ae.st.eq(
                        (ae_st_array[w] & s1_hits[w]).any()),
                    self.resp[w].bits.ae.inst.eq(
                        (~px_ok_array[w] & s1_hits[w]).any()),
                    #
                    self.resp[w].bits.ma.ld.eq(cmd_read[w] & misaligned[w]),
                    self.resp[w].bits.ma.st.eq(cmd_write[w] & misaligned[w]),
                    #
                    self.resp[w].bits.paddr.eq(
                        Cat(s1_req[w].vaddr[:self.pg_offset_bits], ppn[w])),
                    self.resp[w].bits.cacheable.eq(
                        (c_array[w] & s1_hits[w]).any()),
                ]

            with m.Else():
                m.d.comb += [
                    self.resp[w].bits.paddr.eq(s1_req[w].vaddr),
                    self.resp[w].bits.ae.ld.eq(cmd_read[w] & ~prot_r[w]
                                               & ~refill_done),
                    self.resp[w].bits.ae.st.eq(cmd_write[w] & ~prot_w[w]
                                               & ~refill_done),
                    self.resp[w].bits.ae.inst.eq(~prot_x[w] & ~refill_done),
                    self.resp[w].bits.cacheable.eq(cacheable[w]),
                ]

        return m
