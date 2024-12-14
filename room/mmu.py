from amaranth import *
from amaranth import tracer
from amaranth.hdl.rec import Direction
from amaranth.utils import log2_int
import riscvmodel.csrnames as csrnames

from room.consts import *
from room.csr import *
from room.types import HasCoreParams
from room.lsu import LoadStoreUnit

from roomsoc.interconnect.stream import Decoupled, Valid


def xatp_layout(xlen):
    mode_bits, asid_bits = (4, 16) if xlen == 64 else (1, 9)
    return [
        ("ppn", xlen - mode_bits - asid_bits, CSRAccess.RW),
        ("asid", asid_bits, CSRAccess.RW),
        ("mode", mode_bits, CSRAccess.RW),
    ]


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


class PageTableWalker(HasCoreParams, Elaboratable, AutoCSR):

    class Request(HasCoreParams, Record):

        def __init__(self, params, name=None, src_loc_at=0):
            HasCoreParams.__init__(self, params)

            Record.__init__(self, [
                ('vpn', self.vpn_bits, Direction.FANOUT),
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

        def eq(self, rhs):
            attrs = ['pte', 'level', 'ae_ptw', 'ae_leaf', 'pf']
            return [getattr(self, a).eq(getattr(rhs, a)) for a in attrs]

    def __init__(self, params):
        HasCoreParams.__init__(self, params)

        self.req = Decoupled(PageTableWalker.Request, params)
        self.resp = Valid(PageTableWalker.Response, params)

        self.satp = CSR(csrnames.satp, xatp_layout(self.xlen))

        self.mem_req = Decoupled(LoadStoreUnit.CoreRequest, params)
        self.mem_nack = Signal()
        self.mem_resp = Valid(LoadStoreUnit.CoreResponse, params)

    def elaborate(self, platform):
        m = Module()

        min_pg_levels = 2 if self.xlen == 32 else 3

        r_req = PageTableWalker.Request(self.params)
        r_pte = PTE()
        count = Signal(range(self.pg_levels))

        resp_ae_ptw = Signal()
        resp_ae_leaf = Signal()
        resp_pf = Signal()
        m.d.comb += [
            self.resp.bits.ae_ptw.eq(resp_ae_ptw),
            self.resp.bits.ae_leaf.eq(resp_ae_leaf),
            self.resp.bits.pf.eq(resp_pf),
            self.resp.bits.pte.eq(r_pte),
            self.resp.bits.level.eq(count),
        ]
        m.d.sync += self.resp.valid.eq(0)

        mem_resp_valid = Signal()
        mem_resp_data = Signal.like(self.mem_resp.bits.data)
        m.d.sync += [
            mem_resp_valid.eq(self.mem_resp.valid),
            mem_resp_data.eq(self.mem_resp.bits.data),
        ]

        pte = PTE()
        invalid_paddr = Signal()
        go_next_level = Signal()
        m.d.comb += [
            pte.eq(mem_resp_data),
            invalid_paddr.eq((pte.ppn >> self.ppn_bits) != 0),
            go_next_level.eq(pte.table() & ~invalid_paddr
                             & (count != (self.pg_level_bits - 1))),
        ]

        vpn = r_req.vpn
        pte_addr = Signal.like(self.mem_req.bits.addr)
        if self.use_vm:
            vpn_idx = Signal(self.pg_level_bits)

            with m.Switch(count):
                for level in range(self.pg_levels):
                    with m.Case(level):
                        m.d.comb += vpn_idx.eq(vpn >> (self.pg_levels - level -
                                                       1) * self.pg_level_bits)

            m.d.comb += pte_addr.eq((r_pte.ppn << self.pg_offset_bits)
                                    | (vpn_idx << log2_int(self.xlen // 8)))

        with m.FSM():
            with m.State('IDLE'):
                m.d.comb += self.req.ready.eq(1)

                with m.If(self.req.fire):
                    initial_count = self.pg_levels - min_pg_levels - self.satp.r.mode[:
                                                                                      2]

                    m.d.sync += [
                        r_req.eq(self.req.bits),
                        r_pte.ppn.eq(self.satp.r.ppn),
                        count.eq(initial_count),
                        resp_ae_ptw.eq(0),
                        resp_ae_leaf.eq(0),
                        resp_pf.eq(0),
                    ]

                    m.next = 'REQUEST'

            with m.State('REQUEST'):
                m.d.comb += [
                    self.mem_req.valid.eq(1),
                    self.mem_req.bits.addr.eq(pte_addr),
                    self.mem_req.bits.cmd.eq(MemoryCommand.READ),
                    self.mem_req.bits.size.eq(log2_int(self.xlen // 8)),
                    self.mem_req.bits.phys.eq(1),
                ]

                with m.If(self.mem_req.fire):
                    m.next = 'WAIT_1'

            with m.State('WAIT_1'):
                m.next = 'WAIT_2'

            with m.State('WAIT_2'):
                m.next = 'WAIT_3'

                with m.If(self.mem_nack):
                    m.next = 'REQUEST'

            with m.State('WAIT_3'):
                with m.If(mem_resp_valid):
                    m.next = 'REQUEST'

                    with m.If(go_next_level):
                        m.d.sync += count.eq(count + 1)
                    with m.Else():
                        ae = pte.v & invalid_paddr
                        pf = pte.v & (pte._rsvd != 0)

                        with m.If(count == (self.pg_levels - 1)):
                            m.d.sync += self.resp.valid.eq(1)
                            m.next = 'IDLE'

                        m.d.sync += [
                            resp_ae_ptw.eq(ae
                                           & (count < (self.pg_levels - 1))
                                           & pte.table()),
                            resp_ae_leaf.eq(ae & pte.leaf()),
                            resp_pf.eq(pf),
                        ]

        with m.If(mem_resp_valid):
            m.d.sync += r_pte.eq(pte)

        return m
