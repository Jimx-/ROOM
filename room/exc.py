from amaranth import *
from amaranth.lib.coding import PriorityEncoder
import riscvmodel.csrnames as csrnames
from enum import IntEnum

from room.csr import *


class CoreInterrupts(Record):

    def __init__(self, name=None):
        super().__init__([('debug', 1), ('mtip', 1), ('msip', 1), ('meip', 1),
                          ('seip', 1)],
                         name=name)


class Cause(IntEnum):
    FETCH_MISALIGNED = 0
    FETCH_ACCESS_FAULT = 1
    ILLEGAL_INSTRUCTION = 2
    BREAKPOINT = 3
    LOAD_MISALIGNED = 4
    LOAD_ACCESS_FAULT = 5
    STORE_MISALIGNED = 6
    STORE_ACCESS_FAULT = 7
    ECALL_FROM_U = 8
    ECALL_FROM_S = 9
    ECALL_FROM_M = 11
    FETCH_PAGE_FAULT = 12
    LOAD_PAGE_FAULT = 13
    STORE_PAGE_FAULT = 15
    # interrupts
    U_SOFTWARE_INTERRUPT = 0
    S_SOFTWARE_INTERRUPT = 1
    M_SOFTWARE_INTERRUPT = 3
    U_TIMER_INTERRUPT = 4
    S_TIMER_INTERRUPT = 5
    M_TIMER_INTERRUPT = 7
    U_EXTERNAL_INTERRUPT = 8
    S_EXTERNAL_INTERRUPT = 9
    M_EXTERNAL_INTERRUPT = 11
    DEBUG_INTERRUPT = 12


mstatus_layout = [
    ("uie", 1, CSRAccess.RO),  # User Interrupt Enable
    ("sie", 1, CSRAccess.RO),  # Supervisor Interrupt Enable
    ("zero0", 1, CSRAccess.RO),
    ("mie", 1, CSRAccess.RW),  # Machine Interrupt Enable
    ("upie", 1, CSRAccess.RO),  # User Previous Interrupt Enable
    ("spie", 1, CSRAccess.RO),  # Supervisor Previous Interrupt Enable
    ("zero1", 1, CSRAccess.RO),
    ("mpie", 1, CSRAccess.RW),  # Machine Previous Interrupt Enable
    ("spp", 1, CSRAccess.RO),  # Supervisor Previous Privilege
    ("zero2", 2, CSRAccess.RO),
    ("mpp", 2, CSRAccess.RW),  # Machine Previous Privilege
    ("fs", 2, CSRAccess.RO),  # FPU Status
    ("xs", 2, CSRAccess.RO),  # user-mode eXtensions Status
    ("mprv", 1, CSRAccess.RO),  # Modify PRiVilege
    ("sum", 1, CSRAccess.RO),  # Supervisor User Memory access
    ("mxr", 1, CSRAccess.RO),  # Make eXecutable Readable
    ("tvm", 1, CSRAccess.RO),  # Trap Virtual Memory
    ("tw", 1, CSRAccess.RO),  # Timeout Wait
    ("tsr", 1, CSRAccess.RO),  # Trap SRET
    ("zero3", 8, CSRAccess.RO),
    ("sd", 1, CSRAccess.RO),  # State Dirty (set if XS or FS are set to dirty)
]

mip_layout = [
    ("usip", 1, CSRAccess.RO),
    ("ssip", 1, CSRAccess.RO),
    ("zero0", 1, CSRAccess.RO),
    ("msip", 1, CSRAccess.RW),
    ("utip", 1, CSRAccess.RO),
    ("stip", 1, CSRAccess.RO),
    ("zero1", 1, CSRAccess.RO),
    ("mtip", 1, CSRAccess.RW),
    ("ueip", 1, CSRAccess.RO),
    ("seip", 1, CSRAccess.RO),
    ("zero2", 1, CSRAccess.RO),
    ("meip", 1, CSRAccess.RW),
    ("zero3", 20, CSRAccess.RO),
]

mcause_layout = [
    ("ecode", 31, CSRAccess.RW),
    ("interrupt", 1, CSRAccess.RW),
]


class ExceptionUnit(Elaboratable, AutoCSR):

    def __init__(self):
        self.interrupts = CoreInterrupts()

        self.interrupt = Signal()
        self.interrupt_cause = Signal(32)

        self.exception = Signal()
        self.cause = Signal(32)
        self.epc = Signal(32)

        self.mstatus = CSR(csrnames.mstatus, mstatus_layout)
        self.mip = CSR(csrnames.mip, mip_layout)
        self.mcause = CSR(csrnames.mcause, mcause_layout)

        self.dpc = CSR(csrnames.dpc, [('value', 32, CSRAccess.RW)])

    def elaborate(self, platform):
        m = Module()

        debug_mode = Signal()

        m.d.comb += [
            self.mip.r.meip.eq(self.interrupts.meip),
            self.mip.r.mtip.eq(self.interrupts.mtip),
            self.mip.r.msip.eq(self.interrupts.msip),
        ]

        interrupts = Signal(16)
        m.d.comb += [
            interrupts[Cause.DEBUG_INTERRUPT].eq(self.interrupts.debug)
        ]

        interrupt_pe = m.submodules.interrupt_pe = PriorityEncoder(16)
        m.d.comb += interrupt_pe.i.eq(interrupts)

        interrupt_cause = Record([l[:2] for l in mcause_layout])
        m.d.comb += [
            interrupt_cause.interrupt.eq(1),
            interrupt_cause.ecode.eq(interrupt_pe.o),
        ]

        m.d.comb += [
            self.interrupt.eq(~interrupt_pe.n & ~debug_mode),
            self.interrupt_cause.eq(interrupt_cause),
        ]

        cause = Record([l[:2] for l in mcause_layout])
        m.d.comb += cause.eq(self.cause)

        is_debug_int = cause.interrupt & (cause.ecode == Cause.DEBUG_INTERRUPT)
        trap_to_debug = is_debug_int

        with m.If(self.exception):
            with m.If(trap_to_debug):
                with m.If(~debug_mode):
                    m.d.sync += [
                        debug_mode.eq(1),
                        self.dpc.r.eq(self.epc),
                    ]

        return m
