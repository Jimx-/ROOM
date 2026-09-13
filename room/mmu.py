from amaranth import *
from amaranth import tracer
from amaranth.hdl.rec import Direction
from amaranth.utils import log2_int
import riscvmodel.csrnames as csrnames

from room.consts import *
from room.csr import *
from room.types import HasCoreParams

from roomsoc.interconnect.stream import Decoupled, Valid


def xatp_layout(xlen):
    mode_bits, asid_bits = (4, 16) if xlen == 64 else (1, 9)
    return [
        ("ppn", xlen - mode_bits - asid_bits, CSRAccess.RW),
        ("asid", asid_bits, CSRAccess.RW),
        ("mode", mode_bits, CSRAccess.RW),
    ]


def hgatp_layout(xlen):
    if xlen == 64:
        return [
            ("ppn", 44, CSRAccess.RW),
            ("vmid", 14, CSRAccess.RW),
            ("_rsvd", 2, CSRAccess.RO),
            ("mode", 4, CSRAccess.RW),
        ]
    return [
        ("ppn", 22, CSRAccess.RW),
        ("vmid", 7, CSRAccess.RW),
        ("mode", 1, CSRAccess.RW),
    ]


class PTBR(CSRRecord):

    def __init__(self, xlen, name=None, src_loc_at=0):
        super().__init__(xatp_layout(xlen),
                         name=name,
                         src_loc_at=1 + src_loc_at)


class PTE(Record):

    _layout = [
        ('v', 1),
        ('r', 1),
        ('w', 1),
        ('x', 1),
        ('u', 1),
        ('g', 1),
        ('a', 1),
        ('d', 1),
        ('rsw', 2),
        ('ppn', 44),
        ('_rsvd', 10),
    ]

    def __init__(self, name=None, src_loc_at=0):
        super().__init__(self._layout, name=name, src_loc_at=1 + src_loc_at)

    def table(self):
        return self.v & ~self.r & ~self.w & ~self.x & ~self.u & ~self.a & ~self.d

    def leaf(self):
        return self.v & (self.r | (self.x & ~self.w)) & self.a

    def sr(self):
        return self.leaf() & self.r

    def sw(self):
        return self.leaf() & self.w & self.d

    def sx(self):
        return self.leaf() & self.x

    def ur(self):
        return self.sr() & self.u

    def uw(self):
        return self.sw() & self.u

    def ux(self):
        return self.sx() & self.u


class CoreMemRequest(HasCoreParams, Record):

    def __init__(self, params, name=None, src_loc_at=0):
        HasCoreParams.__init__(self, params)

        Record.__init__(self, [
            ('addr', self.vaddr_bits, Direction.FANOUT),
            ('cmd', MemoryCommand, Direction.FANOUT),
            ('size', 2, Direction.FANOUT),
            ('signed', 1, Direction.FANOUT),
            ('phys', 1, Direction.FANOUT),
            ('data', self.xlen, Direction.FANOUT),
            ('mask', self.xlen // 8, Direction.FANOUT),
        ],
                        name=name,
                        src_loc_at=1 + src_loc_at)


class CoreMemResponse(HasCoreParams, Record):

    def __init__(self, params, name=None, src_loc_at=0):
        HasCoreParams.__init__(self, params)

        Record.__init__(self, [
            ('has_data', 1, Direction.FANOUT),
            ('data', self.xlen, Direction.FANOUT),
        ],
                        name=name,
                        src_loc_at=1 + src_loc_at)


class PageTableWalker(HasCoreParams, Elaboratable, AutoCSR):

    class Request(HasCoreParams, Record):

        def __init__(self, params, name=None, src_loc_at=0):
            HasCoreParams.__init__(self, params)

            Record.__init__(
                self,
                [
                    # G-stage x4 translation has two more address bits than the
                    # corresponding VS-stage virtual address.
                    ('vpn', self.vpn_bits + 2, Direction.FANOUT),
                    ('vstage1', 1, Direction.FANOUT),
                    ('stage2', 1, Direction.FANOUT),
                ],
                name=name,
                src_loc_at=1 + src_loc_at)

    class Response(HasCoreParams):

        def __init__(self, params, name=None, src_loc_at=0):
            super().__init__(params)

            if name is None:
                name = tracer.get_var_name(depth=2 + src_loc_at, default=None)
            self.name = name

            self.pte = PTE(name=f'{name}_pte')
            self.level = Signal(range(self.pg_levels), name=f'{name}_level')

            self.ae_ptw = Signal(name=f'{name}_ae_ptw')
            self.ae_leaf = Signal(name=f'{name}_ae_leaf')
            self.pf = Signal(name=f'{name}_pf')
            self.gf = Signal(name=f'{name}_gf')
            self.gpa = Signal(self.vaddr_bits + 2, name=f'{name}_gpa')
            self.gpa_is_pte = Signal(name=f'{name}_gpa_is_pte')
            self.hr = Signal(name=f'{name}_hr')
            self.hw = Signal(name=f'{name}_hw')
            self.hx = Signal(name=f'{name}_hx')

        def eq(self, rhs):
            attrs = [
                'pte', 'level', 'ae_ptw', 'ae_leaf', 'pf', 'gf', 'gpa',
                'gpa_is_pte', 'hr', 'hw', 'hx'
            ]
            return [getattr(self, a).eq(getattr(rhs, a)) for a in attrs]

    def __init__(self, params):
        HasCoreParams.__init__(self, params)

        self.req = Decoupled(PageTableWalker.Request, params)
        self.resp = Valid(PageTableWalker.Response, params)

        self.satp = CSR(csrnames.satp, xatp_layout(self.xlen))
        if self.use_hypervisor:
            # Bare-only placeholders for the first HS/VS entry smoke. The
            # stage-aware PTW will give these distinct layouts and behavior.
            self.vsatp = CSR(0x280, xatp_layout(self.xlen))
            self.hgatp = CSR(0x680, hgatp_layout(self.xlen))

        self.mem_req = Decoupled(CoreMemRequest, params)
        self.mem_nack = Signal()
        self.mem_resp = Valid(CoreMemResponse, params)

    def elaborate(self, platform):
        m = Module()

        min_pg_levels = 2 if self.xlen == 32 else 3

        with m.If(self.satp.we):
            m.d.sync += [
                self.satp.r.mode.eq(self.satp.w.mode),
                self.satp.r.ppn.eq(self.satp.w.ppn[:self.ppn_bits]),
            ]

        if self.use_hypervisor:
            with m.If(self.vsatp.we):
                # This walker currently implements Bare and Sv39.
                with m.If((self.vsatp.w.mode == 0) | (self.vsatp.w.mode == 8)):
                    m.d.sync += [
                        self.vsatp.r.mode.eq(self.vsatp.w.mode),
                        self.vsatp.r.asid.eq(self.vsatp.w.asid),
                        self.vsatp.r.ppn.eq(self.vsatp.w.ppn[:self.ppn_bits]),
                    ]

            with m.If(self.hgatp.we):
                # Sv39x4 roots occupy four pages and therefore require a
                # 16-KiB-aligned PPN.  Misaligned writes are WARL-aligned.
                with m.If((self.hgatp.w.mode == 0) | (self.hgatp.w.mode == 8)):
                    m.d.sync += [
                        self.hgatp.r.mode.eq(self.hgatp.w.mode),
                        self.hgatp.r.vmid.eq(self.hgatp.w.vmid),
                        self.hgatp.r.ppn.eq(
                            Cat(Const(0, 2),
                                self.hgatp.w.ppn[2:self.ppn_bits])),
                    ]

        r_req = PageTableWalker.Request(self.params)
        r_pte = PTE()
        count = Signal(range(self.pg_levels))
        aux_pte = PTE()
        aux_count = Signal(range(self.pg_levels))
        vs_leaf = PTE()
        vs_level = Signal(range(self.pg_levels))
        gpa = Signal(self.vaddr_bits + 2)
        gpa_is_pte = Signal()
        r_hgatp_ppn = Signal(self.ppn_bits)
        translated_addr = Signal(self.paddr_bits)
        r_translated_addr = Signal(self.paddr_bits)

        resp_ae_ptw = Signal()
        resp_ae_leaf = Signal()
        resp_pf = Signal()
        resp_gf = Signal()
        resp_gpa = Signal(self.vaddr_bits + 2)
        resp_gpa_is_pte = Signal()
        resp_hr = Signal()
        resp_hw = Signal()
        resp_hx = Signal()
        resp_level = Signal(range(self.pg_levels))
        m.d.comb += [
            self.resp.bits.ae_ptw.eq(resp_ae_ptw),
            self.resp.bits.ae_leaf.eq(resp_ae_leaf),
            self.resp.bits.pf.eq(resp_pf),
            self.resp.bits.gf.eq(resp_gf),
            self.resp.bits.gpa.eq(resp_gpa),
            self.resp.bits.gpa_is_pte.eq(resp_gpa_is_pte),
            self.resp.bits.hr.eq(resp_hr),
            self.resp.bits.hw.eq(resp_hw),
            self.resp.bits.hx.eq(resp_hx),
            self.resp.bits.pte.eq(r_pte),
            self.resp.bits.level.eq(resp_level),
            resp_level.eq(count),
        ]
        if self.use_hypervisor:
            with m.If(r_req.stage2):
                m.d.comb += resp_level.eq(
                    Mux(r_req.vstage1,
                        Mux(vs_level > aux_count, vs_level, aux_count),
                        aux_count))
        m.d.sync += self.resp.valid.eq(0)

        mem_resp_valid = Signal()
        mem_resp_data = Signal.like(self.mem_resp.bits.data)
        m.d.sync += [
            mem_resp_valid.eq(self.mem_resp.valid),
            mem_resp_data.eq(self.mem_resp.bits.data),
        ]

        pte = PTE()
        invalid_paddr = Signal()
        pte_invalid = Signal()
        invalid_vs_addr = Signal()
        vs_superpage_misaligned = Signal()
        g_superpage_misaligned = Signal()
        m.d.comb += [
            pte.eq(mem_resp_data),
            invalid_paddr.eq((pte.ppn >> self.ppn_bits) != 0),
            pte_invalid.eq(~pte.v | (pte.w & ~pte.r) | (pte._rsvd != 0)),
            invalid_vs_addr.eq(invalid_paddr),
        ]
        if self.use_hypervisor:
            with m.If(r_req.stage2):
                m.d.comb += invalid_vs_addr.eq(
                    (pte.ppn >> (self.vpn_bits + 2)) != 0)
        for level in range(self.pg_levels - 1):
            low_bits = (self.pg_levels - level - 1) * self.pg_level_bits
            with m.If(count == level):
                m.d.comb += vs_superpage_misaligned.eq(
                    pte.ppn[:low_bits].any())
            with m.If(aux_count == level):
                m.d.comb += g_superpage_misaligned.eq(pte.ppn[:low_bits].any())

        vs_pte_addr = Signal(self.vaddr_bits + 2)
        vs_vpn_idx = Signal(self.pg_level_bits)
        g_pte_addr = Signal(self.paddr_bits)
        g_vpn_idx = Signal(self.pg_level_bits + 2)
        merged_vs_ppn = Signal(self.vpn_bits + 2)
        merged_g_ppn = Signal(self.ppn_bits)

        with m.Switch(count):
            for level in range(self.pg_levels):
                with m.Case(level):
                    m.d.comb += vs_vpn_idx.eq(r_req.vpn >> (
                        (self.pg_levels - level - 1) * self.pg_level_bits))

        with m.Switch(aux_count):
            for level in range(self.pg_levels):
                width = self.pg_level_bits + (2 if level == 0 else 0)
                with m.Case(level):
                    m.d.comb += g_vpn_idx.eq((gpa >> self.pg_offset_bits >>
                                              ((self.pg_levels - level - 1) *
                                               self.pg_level_bits))[:width])

        m.d.comb += [
            vs_pte_addr.eq((r_pte.ppn << self.pg_offset_bits)
                           | (vs_vpn_idx << log2_int(self.xlen // 8))),
            g_pte_addr.eq((aux_pte.ppn << self.pg_offset_bits)
                          | (g_vpn_idx << log2_int(self.xlen // 8))),
            merged_vs_ppn.eq(vs_leaf.ppn),
            merged_g_ppn.eq(pte.ppn[:self.ppn_bits]),
        ]
        for level in range(self.pg_levels - 1):
            low_bits = (self.pg_levels - level - 1) * self.pg_level_bits
            with m.If(vs_level == level):
                m.d.comb += merged_vs_ppn[:low_bits].eq(r_req.vpn[:low_bits])
            with m.If(aux_count == level):
                m.d.comb += merged_g_ppn[:low_bits].eq(
                    (gpa >> self.pg_offset_bits)[:low_bits])

        m.d.comb += translated_addr.eq((merged_g_ppn << self.pg_offset_bits)
                                       | gpa[:self.pg_offset_bits])

        def issue_read(addr):
            return [
                self.mem_req.valid.eq(1),
                self.mem_req.bits.addr.eq(addr),
                self.mem_req.bits.cmd.eq(MemoryCommand.READ),
                self.mem_req.bits.size.eq(log2_int(self.xlen // 8)),
                self.mem_req.bits.phys.eq(1),
            ]

        def finish_fault(guest, page_fault=True):
            return [
                self.resp.valid.eq(1),
                resp_pf.eq(page_fault & ~guest),
                resp_gf.eq(page_fault & guest),
                resp_gpa.eq(gpa),
                resp_gpa_is_pte.eq(gpa_is_pte),
            ]

        hgatp_ppn = self.hgatp.r.ppn if self.use_hypervisor else Const(
            0, self.ppn_bits)

        with m.FSM():
            with m.State('IDLE'):
                m.d.comb += self.req.ready.eq(1)

                with m.If(self.req.fire):
                    m.d.sync += [
                        r_req.eq(self.req.bits),
                        resp_ae_ptw.eq(0),
                        resp_ae_leaf.eq(0),
                        resp_pf.eq(0),
                        resp_gf.eq(0),
                        resp_gpa.eq(0),
                        resp_gpa_is_pte.eq(0),
                        resp_hr.eq(0),
                        resp_hw.eq(0),
                        resp_hx.eq(0),
                        r_hgatp_ppn.eq(hgatp_ppn),
                    ]
                    with m.If(self.req.bits.vstage1):
                        m.d.sync += [
                            r_pte.eq(0),
                            r_pte.ppn.eq(self.vsatp.r.ppn if self.
                                         use_hypervisor else self.satp.r.ppn),
                            count.eq(0),
                        ]
                        m.next = 'VS_ADDR'
                    with m.Elif(self.req.bits.stage2):
                        m.d.sync += [
                            gpa.eq(self.req.bits.vpn << self.pg_offset_bits),
                            gpa_is_pte.eq(0),
                            aux_pte.eq(0),
                            aux_pte.ppn.eq(hgatp_ppn),
                            aux_count.eq(0),
                        ]
                        m.next = 'G_REQ'
                    with m.Else():
                        m.d.sync += [
                            r_pte.eq(0),
                            r_pte.ppn.eq(self.satp.r.ppn),
                            count.eq(0),
                        ]
                        m.next = 'VS_MEM_REQ'

            with m.State('VS_ADDR'):
                with m.If(r_req.stage2):
                    m.d.sync += [
                        gpa.eq(vs_pte_addr),
                        gpa_is_pte.eq(1),
                        aux_pte.eq(0),
                        aux_pte.ppn.eq(r_hgatp_ppn),
                        aux_count.eq(0),
                    ]
                    m.next = 'G_REQ'
                with m.Else():
                    m.next = 'VS_MEM_REQ'

            with m.State('VS_MEM_REQ'):
                m.d.comb += issue_read(vs_pte_addr)
                with m.If(self.mem_req.fire):
                    m.next = 'VS_WAIT_1'

            with m.State('VS_WAIT_1'):
                m.next = 'VS_WAIT_2'
            with m.State('VS_WAIT_2'):
                m.next = 'VS_WAIT_3'
                with m.If(self.mem_nack):
                    m.next = 'VS_MEM_REQ'
            with m.State('VS_WAIT_3'):
                with m.If(mem_resp_valid):
                    with m.If(pte.table() & ~invalid_vs_addr
                              & (count != self.pg_levels - 1)):
                        m.d.sync += [r_pte.eq(pte), count.eq(count + 1)]
                        m.next = 'VS_ADDR'
                    with m.Elif(pte.leaf() & ~invalid_vs_addr & ~pte_invalid
                                & ~vs_superpage_misaligned):
                        m.d.sync += [vs_leaf.eq(pte), vs_level.eq(count)]
                        with m.If(r_req.stage2):
                            m.d.sync += [
                                gpa.eq((pte.ppn << self.pg_offset_bits)
                                       | (r_req.vpn << self.pg_offset_bits)),
                                gpa_is_pte.eq(0),
                                aux_pte.eq(0),
                                aux_pte.ppn.eq(r_hgatp_ppn),
                                aux_count.eq(0),
                            ]
                            m.next = 'G_FINAL_PREP'
                        with m.Else():
                            m.d.sync += [r_pte.eq(pte), self.resp.valid.eq(1)]
                            m.next = 'IDLE'
                    with m.Elif(invalid_vs_addr):
                        m.d.sync += [
                            resp_ae_ptw.eq(pte.table()
                                           & (count < self.pg_levels - 1)),
                            resp_ae_leaf.eq(pte.leaf()),
                            self.resp.valid.eq(1),
                        ]
                        m.next = 'IDLE'
                    with m.Else():
                        m.d.sync += finish_fault(False)
                        m.next = 'IDLE'

            with m.State('G_FINAL_PREP'):
                # Replace the low GPA page-number fields for a VS superpage.
                m.d.sync += gpa.eq((merged_vs_ppn << self.pg_offset_bits))
                m.next = 'G_REQ'

            with m.State('G_REQ'):
                m.d.comb += issue_read(g_pte_addr)
                with m.If(self.mem_req.fire):
                    m.next = 'G_WAIT_1'
            with m.State('G_WAIT_1'):
                m.next = 'G_WAIT_2'
            with m.State('G_WAIT_2'):
                m.next = 'G_WAIT_3'
                with m.If(self.mem_nack):
                    m.next = 'G_REQ'
            with m.State('G_WAIT_3'):
                with m.If(mem_resp_valid):
                    with m.If(pte.table() & ~invalid_paddr
                              & (aux_count != self.pg_levels - 1)):
                        m.d.sync += [
                            aux_pte.eq(pte),
                            aux_count.eq(aux_count + 1)
                        ]
                        m.next = 'G_REQ'
                    with m.Elif(pte.leaf() & ~invalid_paddr & ~pte_invalid
                                & ~g_superpage_misaligned
                                & (~gpa_is_pte | pte.r)):
                        with m.If(gpa_is_pte):
                            m.d.sync += r_translated_addr.eq(translated_addr)
                            m.next = 'VS_PTE_REQ'
                        with m.Else():
                            m.d.sync += [
                                r_pte.eq(pte),
                                r_pte.ppn.eq(merged_g_ppn),
                                # Keep VS permissions separate from G-stage
                                # permissions so the TLB can report the right
                                # page-fault class.
                                r_pte.r.eq(Mux(r_req.vstage1, vs_leaf.r, 1)),
                                r_pte.w.eq(Mux(r_req.vstage1, vs_leaf.w, 1)),
                                r_pte.x.eq(Mux(r_req.vstage1, vs_leaf.x, 1)),
                                r_pte.u.eq(Mux(r_req.vstage1, vs_leaf.u, 0)),
                                r_pte.g.eq(pte.g
                                           & Mux(r_req.vstage1, vs_leaf.g, 1)),
                                r_pte.a.eq(pte.a
                                           & Mux(r_req.vstage1, vs_leaf.a, 1)),
                                r_pte.d.eq(pte.d
                                           & Mux(r_req.vstage1, vs_leaf.d, 1)),
                                resp_hr.eq(pte.r),
                                resp_hw.eq(pte.w & pte.d),
                                resp_hx.eq(pte.x),
                                self.resp.valid.eq(1),
                            ]
                            m.next = 'IDLE'
                    with m.Else():
                        m.d.sync += finish_fault(True)
                        m.next = 'IDLE'

            with m.State('VS_PTE_REQ'):
                m.d.comb += issue_read(r_translated_addr)
                with m.If(self.mem_req.fire):
                    m.next = 'VS_WAIT_1'

        return m


class PMAChecker(HasCoreParams, Elaboratable):

    class Response(Record):

        def __init__(self, name=None, src_loc_at=0):
            super().__init__([
                ('cacheable', 1),
                ('r', 1),
                ('w', 1),
                ('x', 1),
            ],
                             name=name,
                             src_loc_at=1 + src_loc_at)

    def __init__(self, params):
        HasCoreParams.__init__(self, params)

        self.paddr = Signal(self.paddr_bits)

        self.resp = PMAChecker.Response()

    def elaborate(self, platform):
        m = Module()

        if self.pma_regions is None:
            m.d.comb += [
                self.resp.r.eq(1),
                self.resp.w.eq(1),
                self.resp.x.eq(1),
            ]

        else:
            for origin, size, mode, cacheable in self.pma_regions:
                with m.If((self.paddr >= origin)
                          & (self.paddr < origin + size)):
                    m.d.comb += [
                        self.resp.cacheable.eq(cacheable),
                        self.resp.r.eq(mode.count('r') > 0),
                        self.resp.w.eq(mode.count('w') > 0),
                        self.resp.x.eq(self.resp.r),
                    ]

        return m
