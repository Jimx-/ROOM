from amaranth import *
from amaranth import tracer
from amaranth.hdl.rec import Direction
from amaranth.hdl.ast import ValueCastable
import riscvmodel.csrnames as csrnames

from room.consts import *
from room.types import HasCoreParams
from room.csr import *


class PMPConfig(Record):

    _layout = [
        ('r', 1, Direction.FANOUT),
        ('w', 1, Direction.FANOUT),
        ('x', 1, Direction.FANOUT),
        ('a', 2, Direction.FANOUT),
        ('_rsvd', 2, Direction.FANOUT),
        ('l', 1, Direction.FANOUT),
    ]

    def __init__(self, name=None, src_loc_at=0):
        super().__init__(self._layout, name=name, src_loc_at=1 + src_loc_at)


class PMPReg(HasCoreParams, ValueCastable):

    def __init__(self, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.cfg = PMPConfig(name=f'{name}__cfg')
        self.addr = Signal(self.paddr_bits - 2, name=f'{name}__addr')

    @ValueCastable.lowermethod
    def as_value(self):
        return Cat(self.cfg, self.addr)

    def shape(self):
        return self.as_value().shape()

    def __len__(self):
        return len(Value.cast(self))

    def eq(self, rhs):
        return Value.cast(self).eq(Value.cast(rhs))

    @property
    def is_tor(self):
        return self.cfg.a[0] & ~self.cfg.a[1]

    @property
    def mask(self):
        base = Cat(self.cfg.a[0],
                   self.addr) | ((self.pmp_granularity - 1) >> 2)
        return Cat(Const(3, 2), base & ~(base + 1))

    @property
    def masked_addr(self):
        return ~(~(self.addr << 2) | (self.pmp_granularity - 1))

    def pow2_match(self, addr, size, log_max_size):
        if log_max_size <= self.pmp_granularity:
            return ~((addr ^ self.masked_addr) & ~self.mask).any()

        lsb_mask = (self.mask | (1 << size))[:log_max_size]
        msb_match = ~((addr ^ self.masked_addr)
                      & ~self.mask)[log_max_size:].any()
        lsb_match = ~(
            (addr[:lsb_mask] ^ self.masked_addr[:lsb_mask]) & ~lsb_mask).any()
        return msb_match & lsb_match

    def bound_check(self, addr, lsb_mask, log_max_size):
        if log_max_size <= self.pmp_granularity:
            return addr < self.masked_addr
        else:
            msb_lt = addr[log_max_size:] < self.masked_addr[log_max_size:]
            msb_eq = ~(addr[log_max_size:]
                       ^ self.masked_addr[log_max_size:]).any()
            lsb_lt = (addr[:log_max_size]
                      | lsb_mask) < self.masked_addr[:log_max_size]
            return msb_lt | (msb_eq & lsb_lt)

    def lower_bound_match(self, addr, size, log_max_size):
        return ~self.bound_check(addr,
                                 (1 << size)[:log_max_size], log_max_size)

    def upper_bound_match(self, addr, log_max_size):
        return self.bound_check(addr, 0, log_max_size)

    def range_match(self, addr, size, log_max_size, prev):
        return prev.lower_bound_match(addr, size,
                                      log_max_size) & self.upper_bound_match(
                                          addr, log_max_size)

    def hit(self, addr, size, log_max_size, prev):
        return Mux(
            self.cfg.a[1], self.pow2_match(addr, size, log_max_size),
            self.cfg.a[0] & self.range_match(addr, size, log_max_size, prev))

    def aligned(self, addr, size, log_max_size, prev):
        lsb_mask = (1 << size)[:log_max_size]
        lower_straddle = ~(addr ^ prev.masked_addr)[log_max_size:].any() & (
            prev.masked_addr[:log_max_size] & ~addr[:log_max_size]).any()
        upper_straddle = ~(addr ^ self.masked_addr)[log_max_size:].any() & (
            self.masked_addr[:log_max_size] &
            (addr[:log_max_size] | lsb_mask)).any()
        pow2_aligned = ~(lsb_mask & ~self.mask[:log_max_size]).any()
        return Mux(self.cfg.a[1], pow2_aligned,
                   ~lower_straddle & ~upper_straddle)


class PMPCSRUnit(HasCoreParams, Elaboratable):

    def __init__(self, params):
        HasCoreParams.__init__(self, params)

        self.max_pmps = 16

        self.pmp = [PMPReg(params, name=f'pmp{i}') for i in range(self.n_pmps)]

        self.pmpcfg = [
            CSR(csrnames.pmpcfg0 + i, [('value', self.xlen, CSRAccess.RW)],
                name=f'pmpcfg{i}')
            for i in range(0, self.max_pmps // 4, self.xlen // 32)
        ]
        self.pmpaddr = [
            CSR(csrnames.pmpaddr0 + i, [('value', self.xlen, CSRAccess.RW)],
                name=f'pmpaddr{i}') for i in range(self.max_pmps)
        ]

    def iter_csrs(self):
        for r in self.pmpcfg:
            yield r
        for r in self.pmpaddr:
            yield r

    def elaborate(self, platform):
        m = Module()

        pmp_regs = [
            PMPReg(self.params, name=f'pmp_reg{i}') for i in range(self.n_pmps)
        ]

        pmpcfg_per_csr = self.xlen // len(pmp_regs[0].cfg)
        reg_groups = [
            pmp_regs[i:i + pmpcfg_per_csr]
            for i in range(0, len(pmp_regs), pmpcfg_per_csr)
        ]

        for pmpcfg, group in zip(self.pmpcfg, reg_groups):
            m.d.comb += pmpcfg.r.eq(Cat(r.cfg for r in group))
            with m.If(pmpcfg.we):
                new_cfg = [
                    PMPConfig(name=f'new_cfg{i}') for i in range(len(group))
                ]
                m.d.comb += Cat(new_cfg).eq(pmpcfg.w)

                for reg, cfg_w in zip(group, new_cfg):
                    with m.If(~reg.cfg.l):
                        m.d.sync += [
                            reg.cfg.eq(cfg_w),
                            reg.cfg.w.eq(cfg_w.r & cfg_w.w),
                        ]

        for pmpaddr, (pmp_reg, next_reg) in zip(
                self.pmpaddr, zip(pmp_regs, pmp_regs[1:] + [pmp_regs[-1]])):
            m.d.comb += pmpaddr.r.eq(pmp_reg.addr)
            with m.If(pmpaddr.we
                      & ~(pmp_reg.cfg.l | (next_reg.cfg.l & next_reg.is_tor))):
                m.d.sync += pmp_reg.addr.eq(pmpaddr.w)

        for pmp, reg in zip(self.pmp, pmp_regs):
            m.d.comb += pmp.eq(reg)

        return m


class PMPChecker(HasCoreParams, Elaboratable):

    def __init__(self, log_max_size, params):
        HasCoreParams.__init__(self, params)

        self.log_max_size = log_max_size

        self.prv = Signal(PrivilegeMode)
        self.paddr = Signal(self.paddr_bits)
        self.size = Signal(range(log_max_size))

        self.pmp = [PMPReg(params, name=f'pmp{i}') for i in range(self.n_pmps)]

        self.r = Signal()
        self.w = Signal()
        self.x = Signal()

    def elaborate(self, platform):
        m = Module()

        default = 1 if self.n_pmps == 0 else self.prv > PrivilegeMode.S
        m.d.comb += [
            self.r.eq(default),
            self.w.eq(default),
            self.x.eq(default),
        ]

        pmp0 = PMPReg(self.params)
        for pmp, prev in reversed(list(zip(self.pmp, [pmp0] + self.pmp))):
            ignore = default & ~pmp.cfg.l
            aligned = pmp.aligned(self.paddr, self.size, self.log_max_size,
                                  prev)

            with m.If(pmp.hit(self.paddr, self.size, self.log_max_size, prev)):
                m.d.comb += [
                    self.r.eq(aligned & (pmp.cfg.r | ignore)),
                    self.w.eq(aligned & (pmp.cfg.w | ignore)),
                    self.x.eq(aligned & (pmp.cfg.x | ignore)),
                ]

        return m
