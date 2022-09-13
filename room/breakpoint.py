from amaranth import *
import riscvmodel.csrnames as csrnames

from room.csr import *

mcontrol_layout = [
    ('load', 1),
    ('store', 1),
    ('execute', 1),
    ('u', 1),
    ('s', 1),
    ('zero0', 1),
    ('m', 1),
    ('match', 4),
    ('chain', 1),
    ('action', 4),
    ('sizelo', 2),
    ('timing', 1),
    ('select', 1),
    ('hit', 1),
    ('maskmax', 6),
]

tdata1_layout = [
    ('data', 27, CSRAccess.RW),
    ('dmode', 1, CSRAccess.RW),
    ('type', 4, CSRAccess.RO),
]

breakpoint_layout = [
    ('control', mcontrol_layout),
    ('dmode', 1),
    ('address', 32),
]


class Breakpoint(Record):

    def __init__(self, name=None, src_loc_at=0):
        super().__init__(breakpoint_layout,
                         name=name,
                         src_loc_at=1 + src_loc_at)

    def address_match(self, addr):
        return addr == self.address


class BreakpointUnit(Elaboratable, AutoCSR):

    def __init__(self, params):
        self.num_breakpoints = params['num_breakpoints']

        self.debug = Signal()

        self.bp = [
            Breakpoint(name=f'bp{i}') for i in range(self.num_breakpoints)
        ]

        self.tselect = CSR(csrnames.tselect, [('value', 32, CSRAccess.RW)])
        self.tdata1 = CSR(csrnames.tdata1, tdata1_layout)
        self.tdata2 = CSR(csrnames.tdata2, [('value', 32, CSRAccess.RW)])

    def elaborate(self, platform):
        m = Module()

        tselect = Signal(range(max(self.num_breakpoints, 1)))
        m.d.comb += self.tselect.r.eq(tselect)
        with m.If(self.tselect.we & (self.tselect.w < self.num_breakpoints)):
            m.d.sync += tselect.eq(self.tselect.w)

        m.d.comb += self.tdata1.r.type.eq(2)

        for i, bp in enumerate(self.bp):
            with m.If(tselect == i):
                m.d.comb += [
                    self.tdata1.r.dmode.eq(bp.dmode),
                    self.tdata1.r.data.eq(bp.control),
                    self.tdata2.r.eq(bp.address),
                ]

                with m.If((~bp.dmode | self.debug)):
                    with m.If(self.tdata2.we):
                        m.d.sync += bp.address.eq(self.tdata2.w)

                    with m.If(self.tdata1.we):
                        control = Record(mcontrol_layout)
                        m.d.comb += control.eq(self.tdata1.w.data)

                        dmode = self.tdata1.w.dmode & self.debug
                        m.d.sync += [
                            bp.dmode.eq(dmode),
                            bp.control.eq(control),
                            bp.control.action.eq(0),
                        ]

                        with m.If(dmode | (control.action > 1)):
                            m.d.sync += bp.control.action.eq(control.action)

        return m


class BreakpointMatcher(Elaboratable):

    def __init__(self, params):
        self.num_breakpoints = params['num_breakpoints']

        self.bp = [
            Breakpoint(name=f'bp{i}') for i in range(self.num_breakpoints)
        ]

        self.pc = Signal(32)

        self.exc_if = Signal()
        self.debug_if = Signal()

    def elaborate(self, platform):
        m = Module()

        for bp in self.bp:
            with m.If(bp.control.execute & bp.address_match(self.pc)):
                m.d.comb += [
                    self.exc_if.eq(bp.control.action == 0),
                    self.debug_if.eq(bp.control.action == 1),
                ]

        return m
