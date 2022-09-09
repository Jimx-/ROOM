from amaranth import *
from amaranth.lib.coding import PriorityEncoder
import riscvmodel.insn as insn
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

dcsr_layout = [
    ("prv", 2, CSRAccess.RW),  # Privilege level before Debug Mode was entered
    ("step", 1,
     CSRAccess.RW),  # Execute a single instruction and re-enter Debug Mode
    ("nmip", 1, CSRAccess.RO),  # A non-maskable interrupt is pending
    ("mprven", 1, CSRAccess.RW),  # Use mstatus.mprv in Debug Mode
    ("zero0", 1, CSRAccess.RO),
    ("cause", 3, CSRAccess.RO),  # Explains why Debug Mode was entered
    ("stoptime", 1, CSRAccess.RW),  # Stop timer increment during Debug Mode
    ("stopcount", 1, CSRAccess.RW),  # Stop counter increment during Debug Mode
    ("stepie", 1, CSRAccess.RW),  # Enable interrupts during single stepping
    ("ebreaku", 1, CSRAccess.RW),  # EBREAKs in U-mode enter Debug Mode
    ("ebreaks", 1, CSRAccess.RW),  # EBREAKs in S-mode enter Debug Mode
    ("zero1", 1, CSRAccess.RO),
    ("ebreakm", 1, CSRAccess.RW),  # EBREAKs in M-mode enter Debug Mode
    ("zero2", 12, CSRAccess.RO),
    ("xdebugver", 4, CSRAccess.RO),  # External Debug specification version
]


class ExceptionUnit(Elaboratable, AutoCSR):

    def __init__(self):
        self.interrupts = CoreInterrupts()

        self.interrupt = Signal()
        self.interrupt_cause = Signal(32)
        self.exc_vector = Signal(32)

        self.debug_entry = Signal(32)

        self.system_insn = Signal()
        self.system_insn_imm = Signal(12)

        self.commit = Signal()
        self.exception = Signal()
        self.cause = Signal(32)
        self.epc = Signal(32)

        self.single_step = Signal()

        self.mstatus = CSR(csrnames.mstatus, mstatus_layout)
        self.mip = CSR(csrnames.mip, mip_layout)
        self.mcause = CSR(csrnames.mcause, mcause_layout)

        self.dcsr = CSR(csrnames.dcsr, dcsr_layout)
        self.dpc = CSR(csrnames.dpc, [('value', 32, CSRAccess.RW)])
        self.dscratch0 = CSR(csrnames.dscratch0, [('value', 32, CSRAccess.RW)])
        self.dscratch1 = CSR(csrnames.dscratch1, [('value', 32, CSRAccess.RW)])

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

        m.d.comb += [
            self.dcsr.r.xdebugver.eq(4),
            self.dcsr.r.prv.eq(3),
        ]
        with m.If(self.dcsr.we):
            m.d.sync += [
                self.dcsr.r.step.eq(self.dcsr.w.step),
                self.dcsr.r.ebreakm.eq(self.dcsr.w.ebreakm),
            ]
        with m.If(self.dpc.we):
            m.d.sync += self.dpc.r.eq(self.dpc.w)
        with m.If(self.dscratch0.we):
            m.d.sync += self.dscratch0.r.eq(self.dscratch0.w)
        with m.If(self.dscratch1.we):
            m.d.sync += self.dscratch1.r.eq(self.dscratch1.w)

        insn_break = Signal()
        insn_dret = Signal()
        insn_ret = insn_dret
        with m.If(self.system_insn):
            with m.Switch(self.system_insn_imm):
                with m.Case(insn.InstructionEBREAK.field_imm.value):
                    m.d.comb += insn_break.eq(1)
                with m.Case(0x7b2):  # DRET
                    m.d.comb += insn_dret.eq(1)

        cause = Record([l[:2] for l in mcause_layout])
        m.d.comb += cause.eq(self.cause)

        single_stepped = Signal()

        is_debug_int = cause.interrupt & (cause.ecode == Cause.DEBUG_INTERRUPT)
        is_debug_break = ~cause.interrupt & insn_break
        trap_to_debug = single_stepped | is_debug_int | is_debug_break | debug_mode

        m.d.comb += [
            self.exc_vector.eq(Mux(trap_to_debug, self.debug_entry, 0)),
            self.single_step.eq(self.dcsr.r.step & ~debug_mode),
        ]

        with m.If(~self.single_step):
            m.d.sync += single_stepped.eq(0)
        with m.Elif(self.commit | self.exception):
            m.d.sync += single_stepped.eq(1)

        exception = insn_break | self.exception

        with m.If(exception):
            with m.If(trap_to_debug):
                with m.If(~debug_mode):
                    m.d.sync += [
                        debug_mode.eq(1),
                        self.dcsr.r.cause.eq(
                            Mux(single_stepped, 4, Mux(is_debug_int, 3, 1))),
                        self.dpc.r.eq(self.epc),
                    ]

        with m.If(insn_ret):
            with m.If(insn_dret):
                m.d.sync += debug_mode.eq(0)
                m.d.comb += self.exc_vector.eq(self.dpc.r)

        return m
