from amaranth import *
from amaranth.lib.coding import PriorityEncoder
from amaranth.utils import log2_int
import riscvmodel.insn as insn
import riscvmodel.csrnames as csrnames
from enum import IntEnum

from room.csr import *
from room.types import HasCoreParams


class CoreInterrupts(Record):

    def __init__(self, name=None, src_loc_at=0):
        super().__init__([('debug', 1), ('mtip', 1), ('msip', 1), ('meip', 1),
                          ('seip', 1)],
                         name=name,
                         src_loc_at=src_loc_at + 1)


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
    DEBUG_TRIGGER = 16
    MEM_ORDERING_FAULT = 17
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


def mstatus_layout(xlen):
    layout = [
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
    ]

    if xlen == 32:
        layout = layout + [
            ("zero3", 8, CSRAccess.RO),
        ]
    else:
        layout = layout + [
            ("zero3", 9, CSRAccess.RO),
            ("uxl", 2, CSRAccess.RW),
            ("sxl", 2, CSRAccess.RW),
            ("zero4", xlen - 37, CSRAccess.RO),
        ]

    layout.append(
        ("sd", 1,
         CSRAccess.RO),  # State Dirty (set if XS or FS are set to dirty)
    )

    return layout


def mip_layout(xlen):
    return [
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
        ("zero3", xlen - 12, CSRAccess.RO),
    ]


def mcause_layout(xlen):
    return [
        ("ecode", xlen - 1, CSRAccess.RW),
        ("interrupt", 1, CSRAccess.RW),
    ]


def mtvec_layout(xlen):
    return [
        ("mode", 2, CSRAccess.RW),
        ("base", xlen - 2, CSRAccess.RW),
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


class MStatus(CSRRecord):

    def __init__(self, xlen, name=None, src_loc_at=0):
        super().__init__(mstatus_layout(xlen),
                         name=name,
                         src_loc_at=1 + src_loc_at)


class ExceptionUnit(HasCoreParams, Elaboratable, AutoCSR):

    def __init__(self, params):
        HasCoreParams.__init__(self, params)

        self.interrupts = CoreInterrupts()

        self.interrupt = Signal()
        self.interrupt_cause = Signal(self.xlen)
        self.exc_vector = Signal(32)

        self.debug_mode = Signal()
        self.debug_entry = Signal(32)

        self.system_insn = Signal()
        self.system_insn_imm = Signal(12)

        self.commit = Signal()
        self.exception = Signal()
        self.cause = Signal(self.xlen)
        self.epc = Signal(32)

        self.single_step = Signal()

        self.mstatus = CSR(csrnames.mstatus, mstatus_layout(self.xlen))
        self.mip = CSR(csrnames.mip, mip_layout(self.xlen))
        self.mcause = CSR(csrnames.mcause, mcause_layout(self.xlen))
        self.mepc = CSR(csrnames.mepc, [('value', self.xlen, CSRAccess.RW)])
        self.mtvec = CSR(csrnames.mtvec, mtvec_layout(self.xlen))
        self.mtval = CSR(csrnames.mtval, [('value', self.xlen, CSRAccess.RW)])

        self.dcsr = CSR(csrnames.dcsr, dcsr_layout)
        self.dpc = CSR(csrnames.dpc, [('value', self.xlen, CSRAccess.RW)])
        self.dscratch0 = CSR(csrnames.dscratch0,
                             [('value', self.xlen, CSRAccess.RW)])
        self.dscratch1 = CSR(csrnames.dscratch1,
                             [('value', self.xlen, CSRAccess.RW)])

    def elaborate(self, platform):
        m = Module()

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

        interrupt_cause = Record([l[:2] for l in mcause_layout(self.xlen)])
        m.d.comb += [
            interrupt_cause.interrupt.eq(1),
            interrupt_cause.ecode.eq(interrupt_pe.o),
        ]

        m.d.comb += [
            self.interrupt.eq(~interrupt_pe.n & ~self.debug_mode),
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
        insn_mret = Signal()
        insn_sret = Signal()
        insn_dret = Signal()
        insn_ret = insn_mret | insn_sret | insn_dret
        with m.If(self.system_insn):
            with m.Switch(self.system_insn_imm):
                with m.Case(insn.InstructionEBREAK.field_imm.value):
                    m.d.comb += insn_break.eq(1)
                with m.Case(insn.InstructionMRET.field_imm.value):
                    m.d.comb += insn_mret.eq(1)
                with m.Case(insn.InstructionSRET.field_imm.value):
                    m.d.comb += insn_sret.eq(1)
                with m.Case(0x7b2):  # DRET
                    m.d.comb += insn_dret.eq(1)

        cause = Record([l[:2] for l in mcause_layout(self.xlen)])
        m.d.comb += cause.eq(self.cause)

        single_stepped = Signal()

        is_debug_int = cause.interrupt & (cause.ecode == Cause.DEBUG_INTERRUPT)
        is_debug_trigger = ~cause.interrupt & (cause.ecode
                                               == Cause.DEBUG_TRIGGER)
        is_debug_break = ~cause.interrupt & insn_break
        trap_to_debug = single_stepped | is_debug_int | is_debug_trigger | is_debug_break | self.debug_mode

        with m.If(self.mtvec.we):
            m.d.sync += [
                self.mtvec.r.mode.eq(self.mtvec.w.mode & 1),
                self.mtvec.r.base.eq(self.mtvec.w.base),
            ]

        with m.If(self.mepc.we):
            m.d.sync += self.mepc.r.eq(self.mepc.w)

        with m.If(self.mcause.we):
            m.d.sync += [
                self.mcause.r.interrupt.eq(self.mcause.w.interrupt),
                self.mcause.r.ecode.eq(
                    self.mcause.w.ecode[:log2_int(self.xlen)]),
            ]

        tvec_csr = self.mtvec.r
        int_vector = Cat(0b00, cause.ecode, tvec_csr.base >>
                         (log2_int(self.xlen) + 2))
        vector_mode = tvec_csr.mode[0] & cause.interrupt
        trap_vector = Mux(vector_mode, int_vector, tvec_csr.base << 2)

        m.d.comb += [
            self.exc_vector.eq(
                Mux(trap_to_debug, self.debug_entry, trap_vector)),
            self.single_step.eq(self.dcsr.r.step & ~self.debug_mode),
        ]

        with m.If(~self.single_step):
            m.d.sync += single_stepped.eq(0)
        with m.Elif(self.commit | self.exception):
            m.d.sync += single_stepped.eq(1)

        exception = insn_break | self.exception

        with m.If(exception):
            with m.If(trap_to_debug):
                with m.If(~self.debug_mode):
                    m.d.sync += [
                        self.debug_mode.eq(1),
                        self.dcsr.r.cause.eq(
                            Mux(single_stepped, 4, Mux(is_debug_int, 3, 1))),
                        self.dpc.r.eq(self.epc),
                    ]

            with m.Else():
                m.d.sync += [
                    self.mepc.r.eq(self.epc),
                    self.mcause.r.eq(self.cause),
                    self.mstatus.r.mpie.eq(self.mstatus.r.mie),
                    self.mstatus.r.mie.eq(0),
                ]

        with m.If(insn_ret):
            with m.If(insn_dret):
                m.d.sync += self.debug_mode.eq(0)
                m.d.comb += self.exc_vector.eq(self.dpc.r)

            with m.Elif(insn_mret):
                m.d.sync += [
                    self.mstatus.r.mie.eq(self.mstatus.r.mpie),
                    self.mstatus.r.mpie.eq(1),
                ]
                m.d.comb += self.exc_vector.eq(self.mepc.r)

        return m
