from amaranth import *
from amaranth.utils import log2_int
import riscvmodel.insn as insn
import riscvmodel.csrnames as csrnames
from enum import IntEnum

from room.consts import PrivilegeMode
from room.csr import *
from room.types import HasCoreParams
from room.utils import sign_extend

from roomsoc.interconnect.stream import Valid


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
    ECALL_FROM_VS = 10
    ECALL_FROM_M = 11
    FETCH_PAGE_FAULT = 12
    LOAD_PAGE_FAULT = 13
    STORE_PAGE_FAULT = 15
    DEBUG_TRIGGER = 16
    MEM_ORDERING_FAULT = 17
    FETCH_GUEST_PAGE_FAULT = 20
    LOAD_GUEST_PAGE_FAULT = 21
    VIRTUAL_INSTRUCTION = 22
    STORE_GUEST_PAGE_FAULT = 23
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
        ("sie", 1, CSRAccess.RW),  # Supervisor Interrupt Enable
        ("zero0", 1, CSRAccess.RO),
        ("mie", 1, CSRAccess.RW),  # Machine Interrupt Enable
        ("upie", 1, CSRAccess.RO),  # User Previous Interrupt Enable
        ("spie", 1, CSRAccess.RW),  # Supervisor Previous Interrupt Enable
        ("ube", 1, CSRAccess.RO),
        ("mpie", 1, CSRAccess.RW),  # Machine Previous Interrupt Enable
        ("spp", 1, CSRAccess.RW),  # Supervisor Previous Privilege
        ("vs", 2, CSRAccess.RO),
        ("mpp", 2, CSRAccess.RW),  # Machine Previous Privilege
        ("fs", 2, CSRAccess.RW),  # FPU Status
        ("xs", 2, CSRAccess.RO),  # user-mode eXtensions Status
        ("mprv", 1, CSRAccess.RW),  # Modify PRiVilege
        ("sum", 1, CSRAccess.RW),  # Supervisor User Memory access
        ("mxr", 1, CSRAccess.RW),  # Make eXecutable Readable
        ("tvm", 1, CSRAccess.RW),  # Trap Virtual Memory
        ("tw", 1, CSRAccess.RW),  # Timeout Wait
        ("tsr", 1, CSRAccess.RW),  # Trap SRET
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
        ("ssip", 1, CSRAccess.RW),
        ("vssip", 1, CSRAccess.RW),
        ("msip", 1, CSRAccess.RW),
        ("utip", 1, CSRAccess.RO),
        ("stip", 1, CSRAccess.RW),
        ("vstip", 1, CSRAccess.RO),
        ("mtip", 1, CSRAccess.RW),
        ("ueip", 1, CSRAccess.RO),
        ("seip", 1, CSRAccess.RW),
        ("vseip", 1, CSRAccess.RO),
        ("meip", 1, CSRAccess.RW),
        ("zero3", xlen - 12, CSRAccess.RO),
    ]


def mie_layout(xlen):
    return [
        ("usie", 1, CSRAccess.RO),
        ("ssie", 1, CSRAccess.RW),
        ("vssie", 1, CSRAccess.RW),
        ("msie", 1, CSRAccess.RW),
        ("utie", 1, CSRAccess.RO),
        ("stie", 1, CSRAccess.RW),
        ("vstie", 1, CSRAccess.RW),
        ("mtie", 1, CSRAccess.RW),
        ("ueie", 1, CSRAccess.RO),
        ("seie", 1, CSRAccess.RW),
        ("vseie", 1, CSRAccess.RW),
        ("meie", 1, CSRAccess.RW),
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


def hstatus_layout(xlen):
    layout = [
        ("zero0", 5, CSRAccess.RO),
        ("vsbe", 1, CSRAccess.RO),
        ("gva", 1, CSRAccess.RO),
        ("spv", 1, CSRAccess.RW),
        ("spvp", 1, CSRAccess.RW),
        ("hu", 1, CSRAccess.RW),
        ("zero1", 2, CSRAccess.RO),
        ("vgein", 6, CSRAccess.RO),
        ("zero2", 2, CSRAccess.RO),
        ("vtvm", 1, CSRAccess.RW),
        ("vtw", 1, CSRAccess.RW),
        ("vtsr", 1, CSRAccess.RW),
        ("zero3", 9, CSRAccess.RO),
    ]

    if xlen == 64:
        layout += [
            ("vsxl", 2, CSRAccess.RO),
            ("zero4", 30, CSRAccess.RO),
        ]

    return layout


def h_interrupt_layout(xlen, *, pending=False, virtual=False):
    return [
        ("zero0", 2, CSRAccess.RO),
        ("vssip", 1, CSRAccess.RW),
        ("zero1", 3, CSRAccess.RO),
        ("vstip", 1, CSRAccess.RW if
         (virtual or not pending) else CSRAccess.RO),
        ("zero2", 3, CSRAccess.RO),
        ("vseip", 1, CSRAccess.RW if
         (virtual or not pending) else CSRAccess.RO),
        ("zero3", 1, CSRAccess.RO),
        ("sgeip", 1,
         CSRAccess.RW if not (pending or virtual) else CSRAccess.RO),
        ("zero4", xlen - 13, CSRAccess.RO),
    ]


def hie_layout(xlen):
    return [
        ("zero0", 2, CSRAccess.RO),
        ("vssie", 1, CSRAccess.RW),
        ("zero1", 3, CSRAccess.RO),
        ("vstie", 1, CSRAccess.RW),
        ("zero2", 3, CSRAccess.RO),
        ("vseie", 1, CSRAccess.RW),
        ("zero3", 1, CSRAccess.RO),
        ("sgeie", 1, CSRAccess.RO),
        ("zero4", xlen - 13, CSRAccess.RO),
    ]


def henvcfg_layout(xlen):
    return [
        ("fiom", 1, CSRAccess.RW),
        ("zero", xlen - 1, CSRAccess.RO),
    ]


def vsstatus_layout(xlen):
    layout = [
        ("uie", 1, CSRAccess.RO),
        ("sie", 1, CSRAccess.RW),
        ("zero0", 3, CSRAccess.RO),
        ("spie", 1, CSRAccess.RW),
        ("ube", 1, CSRAccess.RO),
        ("zero1", 1, CSRAccess.RO),
        ("spp", 1, CSRAccess.RW),
        ("vs", 2, CSRAccess.RO),
        ("zero2", 2, CSRAccess.RO),
        ("fs", 2, CSRAccess.RW),
        ("xs", 2, CSRAccess.RO),
        ("zero3", 1, CSRAccess.RO),
        ("sum", 1, CSRAccess.RW),
        ("mxr", 1, CSRAccess.RW),
        ("zero4", 12 if xlen == 64 else 11, CSRAccess.RO),
    ]
    if xlen == 64:
        layout += [
            ("uxl", 2, CSRAccess.RO),
            ("zero5", 29, CSRAccess.RO),
        ]
    layout.append(("sd", 1, CSRAccess.RO))
    return layout


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


class MIP(CSRRecord):

    def __init__(self, xlen, name=None, src_loc_at=0):
        super().__init__(mip_layout(xlen),
                         name=name,
                         src_loc_at=1 + src_loc_at)


class MCause(CSRRecord):

    def __init__(self, xlen, name=None, src_loc_at=0):
        super().__init__(mcause_layout(xlen),
                         name=name,
                         src_loc_at=1 + src_loc_at)


class MTVec(CSRRecord):

    def __init__(self, xlen, name=None, src_loc_at=0):
        super().__init__(mtvec_layout(xlen),
                         name=name,
                         src_loc_at=1 + src_loc_at)


class ExceptionUnit(HasCoreParams, Elaboratable, AutoCSR):

    def __init__(self, params):
        HasCoreParams.__init__(self, params)

        self.interrupts = CoreInterrupts()

        self.interrupt = Signal()
        self.interrupt_cause = Signal(self.xlen)
        self.exc_vector = Signal(self.vaddr_bits_extended)

        self.debug_mode = Signal()
        self.debug_entry = Signal(self.paddr_bits)
        self.debug_exception = Signal(self.paddr_bits)

        self.system_insn = Signal()
        self.system_insn_imm = Signal(12)

        self.commit = Signal()
        self.exception = Signal()
        self.cause = Signal(self.xlen)
        self.tval = Signal(self.xlen)
        self.epc = Signal(self.vaddr_bits_extended)
        self.prv = Signal(PrivilegeMode, reset=PrivilegeMode.M)
        self.v = Signal()
        self.dprv = Signal(PrivilegeMode)

        self.single_step = Signal()
        self.csr_stall = Signal()
        self.fcsr_flags = Valid(Signal, 5)

        self.mstatus = CSR(csrnames.mstatus, mstatus_layout(self.xlen))
        self.mip = CSR(csrnames.mip, mip_layout(self.xlen))
        self.mie = CSR(csrnames.mie, mie_layout(self.xlen))
        self.mcause = CSR(csrnames.mcause, mcause_layout(self.xlen))
        self.mepc = CSR(csrnames.mepc, [('value', self.xlen, CSRAccess.RW)])
        self.mtvec = CSR(csrnames.mtvec, mtvec_layout(self.xlen))
        self.mtval = CSR(csrnames.mtval, [('value', self.xlen, CSRAccess.RW)])
        if self.use_hypervisor:
            self.mtinst = CSR(0x34a, [('value', self.xlen, CSRAccess.RW)])
            self.mtval2 = CSR(0x34b, [('value', self.xlen, CSRAccess.RW)])
        self.mideleg = CSR(csrnames.mideleg,
                           [('value', self.xlen, CSRAccess.RW)])
        self.medeleg = CSR(csrnames.medeleg,
                           [('value', self.xlen, CSRAccess.RW)])

        self.sstatus = CSR(csrnames.sstatus, mstatus_layout(self.xlen))
        self.sip = CSR(csrnames.sip, mip_layout(self.xlen))
        self.sie = CSR(csrnames.sie, mie_layout(self.xlen))
        self.scause = CSR(csrnames.scause, mcause_layout(self.xlen))
        self.sepc = CSR(csrnames.sepc, [('value', self.xlen, CSRAccess.RW)])
        self.stvec = CSR(csrnames.stvec, mtvec_layout(self.xlen))
        self.stval = CSR(csrnames.stval, [('value', self.xlen, CSRAccess.RW)])

        if self.use_hypervisor:
            # riscvmodel's pre-ratification table assigns hstatus to 0x200;
            # the ratified H extension places it at 0x600.
            self.hstatus = CSR(0x600, hstatus_layout(self.xlen))
            self.hedeleg = CSR(0x602, [('value', self.xlen, CSRAccess.RW)])
            self.hideleg = CSR(0x603, [('value', self.xlen, CSRAccess.RW)])
            self.hie = CSR(0x604, hie_layout(self.xlen))
            self.htimedelta = CSR(0x605, [('value', self.xlen, CSRAccess.RW)])
            self.hcounteren = CSR(0x606, [('value', self.xlen, CSRAccess.RW)])
            # No guest-external interrupt inputs are implemented yet, so
            # GEILEN is zero and both guest-interrupt CSRs are hardwired zero.
            self.hgeie = CSR(0x607, [('value', self.xlen, CSRAccess.RO)])
            self.henvcfg = CSR(0x60a, henvcfg_layout(self.xlen))
            self.htval = CSR(0x643, [('value', self.xlen, CSRAccess.RW)])
            self.hip = CSR(0x644, h_interrupt_layout(self.xlen, pending=True))
            self.hvip = CSR(0x645, h_interrupt_layout(self.xlen, virtual=True))
            self.htinst = CSR(0x64a, [('value', self.xlen, CSRAccess.RW)])
            self.hgeip = CSR(0xe12, [('value', self.xlen, CSRAccess.RO)])

            self.vsstatus = CSR(0x200, vsstatus_layout(self.xlen))
            self.vsie = CSR(0x204, mie_layout(self.xlen))
            self.vstvec = CSR(0x205, mtvec_layout(self.xlen))
            self.vsscratch = CSR(0x240, [('value', self.xlen, CSRAccess.RW)])
            self.vsepc = CSR(0x241, [('value', self.xlen, CSRAccess.RW)])
            self.vscause = CSR(0x242, mcause_layout(self.xlen))
            self.vstval = CSR(0x243, [('value', self.xlen, CSRAccess.RW)])
            self.vsip = CSR(0x244, mip_layout(self.xlen))

        self.dcsr = CSR(csrnames.dcsr, dcsr_layout)
        self.dpc = CSR(csrnames.dpc, [('value', self.xlen, CSRAccess.RW)])
        self.dscratch0 = CSR(csrnames.dscratch0,
                             [('value', self.xlen, CSRAccess.RW)])
        self.dscratch1 = CSR(csrnames.dscratch1,
                             [('value', self.xlen, CSRAccess.RW)])

        self.fflags = CSR(csrnames.fflags, [('value', 5, CSRAccess.RW)])
        self.frm = CSR(csrnames.frm, [('value', 3, CSRAccess.RW)])
        self.fcsr = CSR(csrnames.fcsr, [('value', self.xlen, CSRAccess.RW)])

    def elaborate(self, platform):
        m = Module()

        self.mstatus.r.mpp.reset = PrivilegeMode.M
        if self.use_hypervisor:
            # The virtual supervisor interrupt classes are always delegated
            # out of M-mode when H is implemented.
            self.mideleg.r.value.reset = (1 << 2) | (1 << 6) | (1 << 10)

        m.d.comb += [
            self.mip.r.meip.eq(self.interrupts.meip),
            self.mip.r.mtip.eq(self.interrupts.mtip),
            self.mip.r.msip.eq(self.interrupts.msip),
        ]

        mip_r = MIP(self.xlen)
        m.d.comb += [
            mip_r.eq(self.mip.r),
            mip_r.seip.eq(self.mip.r.seip | self.interrupts.seip),
        ]
        if self.use_hypervisor:
            m.d.comb += [
                self.mip.r.vssip.eq(self.hvip.r.vssip),
                self.mip.r.vstip.eq(self.hvip.r.vstip),
                self.mip.r.vseip.eq(self.hvip.r.vseip),
            ]

        pending_interrupts = mip_r & self.mie.r
        d_interrupts = Signal(self.xlen)
        m_interrupts = Signal(self.xlen)
        s_interrupts = Signal(self.xlen)
        vs_interrupts = Signal(self.xlen)
        m.d.comb += d_interrupts[Cause.DEBUG_INTERRUPT].eq(
            self.interrupts.debug)

        with m.If((self.prv <= PrivilegeMode.S) | self.mstatus.r.mie):
            m.d.comb += m_interrupts.eq(pending_interrupts & ~self.mideleg.r)

        s_global_enable = ((self.prv < PrivilegeMode.S)
                           | ((self.prv == PrivilegeMode.S)
                              & self.mstatus.r.sie))
        if self.use_hypervisor:
            # HS is above every virtualized context. Interrupts delegated
            # onward to VS remain pending while V=0.
            with m.If(self.v | s_global_enable):
                m.d.comb += s_interrupts.eq(
                    pending_interrupts & self.mideleg.r
                    & Mux(self.v, ~self.hideleg.r.value, -1))
            with m.If(self.v & ((self.prv < PrivilegeMode.S)
                                | self.vsstatus.r.sie)):
                m.d.comb += vs_interrupts.eq(pending_interrupts
                                             & self.mideleg.r
                                             & self.hideleg.r.value)
        else:
            with m.If(s_global_enable):
                m.d.comb += s_interrupts.eq(pending_interrupts
                                            & self.mideleg.r)

        priority = [
            Cause.DEBUG_INTERRUPT,
            Cause.M_EXTERNAL_INTERRUPT,
            Cause.M_SOFTWARE_INTERRUPT,
            Cause.M_TIMER_INTERRUPT,
            Cause.S_EXTERNAL_INTERRUPT,
            Cause.S_SOFTWARE_INTERRUPT,
            Cause.S_TIMER_INTERRUPT,
            10,  # virtual supervisor external
            2,  # virtual supervisor software
            6,  # virtual supervisor timer
            Cause.U_EXTERNAL_INTERRUPT,
            Cause.U_SOFTWARE_INTERRUPT,
            Cause.U_TIMER_INTERRUPT,
        ]

        any_interrupt = Signal()
        which_interrupt = Signal(Cause)
        for mask in [vs_interrupts, s_interrupts, m_interrupts, d_interrupts]:
            for i in reversed(priority):
                if i < len(mask):
                    with m.If(mask[i]):
                        m.d.comb += [
                            any_interrupt.eq(1),
                            which_interrupt.eq(i),
                        ]

        interrupt_cause = MCause(self.xlen)
        m.d.comb += [
            interrupt_cause.interrupt.eq(1),
            interrupt_cause.ecode.eq(which_interrupt),
        ]

        m.d.comb += [
            self.interrupt.eq(any_interrupt & ~self.debug_mode),
            self.interrupt_cause.eq(interrupt_cause),
        ]

        m.d.comb += self.dcsr.r.xdebugver.eq(4)
        self.dcsr.r.prv.reset = PrivilegeMode.M
        with m.If(self.dcsr.we):
            m.d.sync += [
                self.dcsr.r.step.eq(self.dcsr.w.step),
                self.dcsr.r.ebreakm.eq(self.dcsr.w.ebreakm),
            ]
            if self.use_user:
                m.d.sync += self.dcsr.r.prv.eq(self.dcsr.w.prv)
        with m.If(self.dpc.we):
            m.d.sync += self.dpc.r.eq(self.dpc.w)
        with m.If(self.dscratch0.we):
            m.d.sync += self.dscratch0.r.eq(self.dscratch0.w)
        with m.If(self.dscratch1.we):
            m.d.sync += self.dscratch1.r.eq(self.dscratch1.w)

        insn_call = Signal()
        insn_break = Signal()
        insn_mret = Signal()
        insn_sret = Signal()
        insn_dret = Signal()
        insn_ret = insn_mret | insn_sret | insn_dret
        insn_wfi = Signal()
        with m.If(self.system_insn):
            with m.Switch(self.system_insn_imm):
                with m.Case(insn.InstructionECALL.field_imm.value):
                    m.d.comb += insn_call.eq(1)
                with m.Case(insn.InstructionEBREAK.field_imm.value):
                    m.d.comb += insn_break.eq(1)
                with m.Case(insn.InstructionMRET.field_imm.value):
                    m.d.comb += insn_mret.eq(1)
                with m.Case(insn.InstructionSRET.field_imm.value):
                    m.d.comb += insn_sret.eq(1)
                with m.Case(0x7b2):  # DRET
                    m.d.comb += insn_dret.eq(1)
                with m.Case(insn.InstructionWFI.field_imm.value):
                    m.d.comb += insn_wfi.eq(1)

        cause = MCause(self.xlen)
        ecall_cause = Signal(Cause)
        m.d.comb += ecall_cause.eq(Cause.ECALL_FROM_U + self.prv)
        if self.use_hypervisor:
            with m.If(self.v & (self.prv == PrivilegeMode.S)):
                m.d.comb += ecall_cause.eq(Cause.ECALL_FROM_VS)
        m.d.comb += cause.eq(
            Mux(insn_call, ecall_cause,
                Mux(insn_break, Cause.BREAKPOINT, self.cause)))

        single_stepped = Signal()

        is_debug_int = cause.interrupt & (cause.ecode == Cause.DEBUG_INTERRUPT)
        is_debug_trigger = ~cause.interrupt & (cause.ecode
                                               == Cause.DEBUG_TRIGGER)
        is_debug_break = ~cause.interrupt & insn_break & Cat(
            self.dcsr.r.ebreaku, self.dcsr.r.ebreaks, Const(0, 1),
            self.dcsr.r.ebreakm).bit_select(self.prv, 1)
        trap_to_debug = single_stepped | is_debug_int | is_debug_trigger | is_debug_break | self.debug_mode

        debug_vector = Mux(
            self.debug_mode,
            Mux(insn_break, self.debug_entry, self.debug_exception),
            self.debug_entry)

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

        with m.If(self.mtval.we):
            m.d.sync += self.mtval.r.eq(self.mtval.w)

        supported_interrupts = MIP(self.xlen)
        m.d.comb += [
            supported_interrupts.ssip.eq(self.use_supervisor),
            supported_interrupts.msip.eq(1),
            supported_interrupts.stip.eq(self.use_supervisor),
            supported_interrupts.mtip.eq(1),
            supported_interrupts.seip.eq(self.use_supervisor),
            supported_interrupts.meip.eq(1),
        ]

        delegatable_interrupts = MIP(self.xlen)
        m.d.comb += [
            delegatable_interrupts.ssip.eq(self.use_supervisor),
            delegatable_interrupts.stip.eq(self.use_supervisor),
            delegatable_interrupts.seip.eq(self.use_supervisor),
        ]
        virtual_interrupts = (1 << 2) | (1 << 6) | (1 << 10)
        if self.use_hypervisor:
            m.d.comb += [
                supported_interrupts.vssip.eq(1),
                supported_interrupts.vstip.eq(1),
                supported_interrupts.vseip.eq(1),
                delegatable_interrupts.vssip.eq(1),
                delegatable_interrupts.vstip.eq(1),
                delegatable_interrupts.vseip.eq(1),
            ]

        with m.If(self.mip.we):
            if self.use_supervisor:
                m.d.sync += [
                    self.mip.r.ssip.eq(self.mip.w.ssip),
                    self.mip.r.stip.eq(self.mip.w.stip),
                    self.mip.r.seip.eq(self.mip.w.seip),
                ]
            if self.use_hypervisor:
                m.d.sync += self.hvip.r.vssip.eq(self.mip.w.vssip)

        with m.If(self.mie.we):
            m.d.sync += self.mie.r.eq(self.mie.w & supported_interrupts)

        with m.If(self.mideleg.we):
            m.d.sync += self.mideleg.r.eq(
                (self.mideleg.w & delegatable_interrupts)
                | (virtual_interrupts if self.use_hypervisor else 0))

        delegatable_exceptions = sum([
            1 << e for e in (
                Cause.FETCH_MISALIGNED,
                Cause.FETCH_PAGE_FAULT,
                Cause.BREAKPOINT,
                Cause.LOAD_PAGE_FAULT,
                Cause.STORE_PAGE_FAULT,
                Cause.LOAD_MISALIGNED,
                Cause.STORE_MISALIGNED,
                Cause.ILLEGAL_INSTRUCTION,
                Cause.ECALL_FROM_U,
                *([Cause.ECALL_FROM_VS] if self.use_hypervisor else []),
            )
        ])

        with m.If(self.medeleg.we):
            m.d.sync += self.medeleg.r.eq(self.medeleg.w
                                          & delegatable_exceptions)

        delegate = Signal()
        with m.Switch(cause.ecode):
            for i in range(self.xlen):
                with m.Case(i):
                    m.d.comb += delegate.eq(self.use_supervisor
                                            & (self.prv <= PrivilegeMode.S)
                                            & Mux(cause.interrupt, self.mideleg
                                                  .r, self.medeleg.r)[i])

        delegate_vs = Signal()
        if self.use_hypervisor:
            with m.Switch(cause.ecode):
                for i in range(self.xlen):
                    with m.Case(i):
                        m.d.comb += delegate_vs.eq(delegate & self.v
                                                   & cause.interrupt
                                                   & self.hideleg.r.value[i])

        trap_cause = MCause(self.xlen)
        m.d.comb += trap_cause.eq(cause)
        if self.use_hypervisor:
            with m.If(delegate_vs):
                # hip/hie use positions 2/6/10; vscause and vstvec use the
                # corresponding supervisor positions 1/5/9.
                m.d.comb += trap_cause.ecode.eq(cause.ecode - 1)

        tvec_csr = MTVec(self.xlen)
        if self.use_hypervisor:
            m.d.comb += tvec_csr.eq(
                Mux(delegate_vs, self.vstvec.r,
                    Mux(delegate, self.stvec.r, self.mtvec.r)))
        else:
            m.d.comb += tvec_csr.eq(Mux(delegate, self.stvec.r, self.mtvec.r))
        int_vector = Cat(0b00, trap_cause.ecode, tvec_csr.base >>
                         (log2_int(self.xlen) + 2))
        vector_mode = tvec_csr.mode[0] & cause.interrupt
        trap_vector = Mux(vector_mode, int_vector, tvec_csr.base << 2)

        m.d.comb += [
            self.exc_vector.eq(Mux(trap_to_debug, debug_vector, trap_vector)),
            self.single_step.eq(self.dcsr.r.step & ~self.debug_mode),
        ]

        with m.If(~self.single_step):
            m.d.sync += single_stepped.eq(0)
        with m.Elif(self.commit | self.exception):
            m.d.sync += single_stepped.eq(1)

        epc_sext = sign_extend(self.epc, self.xlen)
        exception = insn_call | insn_break | self.exception
        tval = Mux(insn_break, epc_sext, self.tval)

        wfi_active = Signal()
        with m.If(insn_wfi & ~self.single_step & ~self.debug_mode):
            m.d.sync += wfi_active.eq(1)
        with m.If(pending_interrupts.any() | self.interrupts.debug
                  | exception):
            m.d.sync += wfi_active.eq(0)

        with m.If(exception):
            with m.If(trap_to_debug):
                with m.If(~self.debug_mode):
                    m.d.sync += [
                        self.debug_mode.eq(1),
                        self.dcsr.r.cause.eq(
                            Mux(single_stepped, 4, Mux(is_debug_int, 3, 1))),
                        self.dcsr.r.prv.eq(self.prv),
                        self.dpc.r.eq(epc_sext),
                        self.prv.eq(PrivilegeMode.M),
                    ]

            if self.use_hypervisor:
                with m.Elif(delegate_vs):
                    m.d.sync += [
                        self.vsepc.r.eq(epc_sext),
                        self.vscause.r.eq(trap_cause),
                        self.vstval.r.eq(tval),
                        self.vsstatus.r.spie.eq(self.vsstatus.r.sie),
                        self.vsstatus.r.spp.eq(self.prv),
                        self.vsstatus.r.sie.eq(0),
                        self.prv.eq(PrivilegeMode.S),
                    ]
            with m.Elif(delegate):
                m.d.sync += [
                    self.sepc.r.eq(epc_sext),
                    self.scause.r.eq(cause),
                    self.stval.r.eq(tval),
                    self.mstatus.r.spie.eq(self.mstatus.r.sie),
                    self.mstatus.r.spp.eq(self.prv),
                    self.mstatus.r.sie.eq(0),
                    self.prv.eq(PrivilegeMode.S),
                ]
                if self.use_hypervisor:
                    m.d.sync += [
                        self.hstatus.r.spv.eq(self.v),
                        self.hstatus.r.spvp.eq(self.prv == PrivilegeMode.S),
                        self.v.eq(0),
                    ]
            with m.Else():
                m.d.sync += [
                    self.mepc.r.eq(epc_sext),
                    self.mcause.r.eq(cause),
                    self.mtval.r.eq(tval),
                    self.mstatus.r.mpie.eq(self.mstatus.r.mie),
                    self.mstatus.r.mpp.eq(self.prv),
                    self.mstatus.r.mie.eq(0),
                    self.prv.eq(PrivilegeMode.M),
                ]
                if self.use_hypervisor:
                    m.d.sync += self.v.eq(0)

        with m.If(insn_ret):
            ret_prv = Signal.like(self.prv)

            with m.If(self.use_supervisor & insn_sret):
                if self.use_hypervisor:
                    with m.If(self.v):
                        m.d.sync += [
                            self.vsstatus.r.sie.eq(self.vsstatus.r.spie),
                            self.vsstatus.r.spie.eq(1),
                            self.vsstatus.r.spp.eq(PrivilegeMode.U),
                        ]
                        m.d.comb += [
                            ret_prv.eq(self.vsstatus.r.spp),
                            self.exc_vector.eq(self.vsepc.r),
                        ]
                    with m.Else():
                        m.d.sync += [
                            self.mstatus.r.sie.eq(self.mstatus.r.spie),
                            self.mstatus.r.spie.eq(1),
                            self.mstatus.r.spp.eq(PrivilegeMode.U),
                        ]
                        m.d.comb += [
                            ret_prv.eq(self.mstatus.r.spp),
                            self.exc_vector.eq(self.sepc.r),
                        ]
                        with m.If(self.hstatus.r.spv):
                            m.d.comb += ret_prv.eq(
                                Mux(self.hstatus.r.spvp, PrivilegeMode.S,
                                    PrivilegeMode.U))
                            m.d.sync += [
                                self.v.eq(1),
                                self.hstatus.r.spv.eq(0),
                            ]
                else:
                    m.d.sync += [
                        self.mstatus.r.sie.eq(self.mstatus.r.spie),
                        self.mstatus.r.spie.eq(1),
                        self.mstatus.r.spp.eq(PrivilegeMode.U),
                    ]
                    m.d.comb += [
                        ret_prv.eq(self.mstatus.r.spp),
                        self.exc_vector.eq(self.sepc.r),
                    ]

            with m.Elif(insn_dret):
                m.d.sync += self.debug_mode.eq(0)
                m.d.comb += [
                    ret_prv.eq(self.dcsr.r.prv),
                    self.exc_vector.eq(self.dpc.r),
                ]

            with m.Elif(insn_mret):
                m.d.sync += [
                    self.mstatus.r.mie.eq(self.mstatus.r.mpie),
                    self.mstatus.r.mpie.eq(1),
                    self.mstatus.r.mpp.eq(PrivilegeMode.U),
                ]
                m.d.comb += [
                    ret_prv.eq(self.mstatus.r.mpp),
                    self.exc_vector.eq(self.mepc.r),
                ]
                if self.use_hypervisor:
                    m.d.sync += self.v.eq(0)

            if self.use_user:
                with m.If(ret_prv <= PrivilegeMode.S):
                    m.d.sync += self.mstatus.r.mprv.eq(0)

            m.d.sync += self.prv.eq(ret_prv)

        set_fs_dirty = Signal()
        m.d.comb += set_fs_dirty.eq(self.fcsr_flags.valid)
        with m.If(set_fs_dirty):
            m.d.sync += self.mstatus.r.fs.eq(3)
        m.d.comb += self.fcsr.r.eq(Cat(self.fflags.r, self.frm.r))
        with m.If(self.fflags.we):
            m.d.comb += set_fs_dirty.eq(1)
            m.d.sync += self.fflags.r.eq(self.fflags.w)
        with m.If(self.frm.we):
            m.d.comb += set_fs_dirty.eq(1)
            m.d.sync += self.frm.r.eq(self.frm.w)
        with m.If(self.fcsr.we):
            m.d.comb += set_fs_dirty.eq(1)
            m.d.sync += Cat(self.fflags.r, self.frm.r).eq(self.fcsr.w)
        with m.If(self.fcsr_flags.valid):
            m.d.sync += self.fflags.r.eq(self.fflags.r | self.fcsr_flags.bits)

        m.d.comb += self.dprv.eq(
            Mux(self.mstatus.r.mprv & ~self.debug_mode, self.mstatus.r.mpp,
                self.prv))
        if self.use_hypervisor:
            if self.xlen == 64:
                m.d.comb += self.hstatus.r.vsxl.eq(log2_int(self.xlen) - 4)
            with m.If(self.hstatus.we):
                m.d.sync += [
                    self.hstatus.r.spv.eq(self.hstatus.w.spv),
                    self.hstatus.r.spvp.eq(self.hstatus.w.spvp),
                    self.hstatus.r.hu.eq(self.hstatus.w.hu),
                    self.hstatus.r.vtvm.eq(self.hstatus.w.vtvm),
                    self.hstatus.r.vtw.eq(self.hstatus.w.vtw),
                    self.hstatus.r.vtsr.eq(self.hstatus.w.vtsr),
                ]

            hedeleg_mask = sum(1 << cause for cause in (
                Cause.FETCH_MISALIGNED,
                Cause.FETCH_ACCESS_FAULT,
                Cause.ILLEGAL_INSTRUCTION,
                Cause.BREAKPOINT,
                Cause.LOAD_MISALIGNED,
                Cause.LOAD_ACCESS_FAULT,
                Cause.STORE_MISALIGNED,
                Cause.STORE_ACCESS_FAULT,
                Cause.ECALL_FROM_U,
                Cause.FETCH_PAGE_FAULT,
                Cause.LOAD_PAGE_FAULT,
                Cause.STORE_PAGE_FAULT,
            ))
            hideleg_mask = virtual_interrupts

            with m.If(self.mtinst.we):
                m.d.sync += self.mtinst.r.eq(self.mtinst.w)
            with m.If(self.mtval2.we):
                m.d.sync += self.mtval2.r.eq(self.mtval2.w)
            with m.If(self.hedeleg.we):
                m.d.sync += self.hedeleg.r.value.eq(self.hedeleg.w.value
                                                    & hedeleg_mask)
            with m.If(self.hideleg.we):
                m.d.sync += self.hideleg.r.value.eq(self.hideleg.w.value
                                                    & hideleg_mask)
            m.d.comb += self.hie.r.eq(self.mie.r & hideleg_mask)
            with m.If(self.hie.we):
                m.d.sync += self.mie.r.eq((self.mie.r & ~hideleg_mask)
                                          | (self.hie.w & hideleg_mask))
            with m.If(self.htimedelta.we):
                m.d.sync += self.htimedelta.r.eq(self.htimedelta.w)
            with m.If(self.hcounteren.we):
                m.d.sync += self.hcounteren.r.eq(self.hcounteren.w)
            with m.If(self.henvcfg.we):
                m.d.sync += self.henvcfg.r.fiom.eq(self.henvcfg.w.fiom)
            with m.If(self.htval.we):
                m.d.sync += self.htval.r.eq(self.htval.w)
            with m.If(self.htinst.we):
                m.d.sync += self.htinst.r.eq(self.htinst.w)

            m.d.comb += [
                self.hip.r.vssip.eq(self.hvip.r.vssip),
                self.hip.r.vstip.eq(self.hvip.r.vstip),
                self.hip.r.vseip.eq(self.hvip.r.vseip),
                self.vsip.r.ssip.eq(self.hvip.r.vssip
                                    & self.hideleg.r.value[2]),
                self.vsip.r.stip.eq(self.hvip.r.vstip
                                    & self.hideleg.r.value[6]),
                self.vsip.r.seip.eq(self.hvip.r.vseip
                                    & self.hideleg.r.value[10]),
                self.vsie.r.ssie.eq(self.mie.r.vssie
                                    & self.hideleg.r.value[2]),
                self.vsie.r.stie.eq(self.mie.r.vstie
                                    & self.hideleg.r.value[6]),
                self.vsie.r.seie.eq(self.mie.r.vseie
                                    & self.hideleg.r.value[10]),
            ]
            with m.If(self.hip.we):
                m.d.sync += self.hvip.r.vssip.eq(self.hip.w.vssip)
            with m.If(self.hvip.we):
                m.d.sync += [
                    self.hvip.r.vssip.eq(self.hvip.w.vssip),
                    self.hvip.r.vstip.eq(self.hvip.w.vstip),
                    self.hvip.r.vseip.eq(self.hvip.w.vseip),
                ]

            m.d.comb += [
                self.vsstatus.r.sd.eq(self.vsstatus.r.fs.all()
                                      | self.vsstatus.r.vs.all()
                                      | self.vsstatus.r.xs.all()),
            ]
            if self.xlen == 64:
                m.d.comb += self.vsstatus.r.uxl.eq(log2_int(self.xlen) - 4)
            with m.If(self.vsstatus.we):
                m.d.sync += [
                    self.vsstatus.r.sie.eq(self.vsstatus.w.sie),
                    self.vsstatus.r.spie.eq(self.vsstatus.w.spie),
                    self.vsstatus.r.spp.eq(self.vsstatus.w.spp),
                    self.vsstatus.r.fs.eq(self.vsstatus.w.fs),
                    self.vsstatus.r.sum.eq(self.vsstatus.w.sum),
                    self.vsstatus.r.mxr.eq(self.vsstatus.w.mxr),
                ]
            with m.If(self.vsie.we):
                m.d.sync += [
                    self.mie.r.vssie.eq(self.vsie.w.ssie
                                        & self.hideleg.r.value[2]),
                    self.mie.r.vstie.eq(self.vsie.w.stie
                                        & self.hideleg.r.value[6]),
                    self.mie.r.vseie.eq(self.vsie.w.seie
                                        & self.hideleg.r.value[10]),
                ]
            with m.If(self.vsip.we):
                with m.If(self.hideleg.r.value[2]):
                    m.d.sync += self.hvip.r.vssip.eq(self.vsip.w.ssip)
            with m.If(self.vstvec.we):
                m.d.sync += [
                    self.vstvec.r.mode.eq(self.vstvec.w.mode & 1),
                    self.vstvec.r.base.eq(self.vstvec.w.base),
                ]
            with m.If(self.vsscratch.we):
                m.d.sync += self.vsscratch.r.eq(self.vsscratch.w)
            with m.If(self.vsepc.we):
                m.d.sync += self.vsepc.r.eq(self.vsepc.w)
            with m.If(self.vscause.we):
                m.d.sync += [
                    self.vscause.r.interrupt.eq(self.vscause.w.interrupt),
                    self.vscause.r.ecode.eq(
                        self.vscause.w.ecode[:log2_int(self.xlen)]),
                ]
            with m.If(self.vstval.we):
                m.d.sync += self.vstval.r.eq(self.vstval.w)
        with m.If(self.mstatus.we):
            m.d.sync += [
                self.mstatus.r.mie.eq(self.mstatus.w.mie),
                self.mstatus.r.mpie.eq(self.mstatus.w.mpie),
            ]

            if self.use_user:
                m.d.sync += [
                    self.mstatus.r.mprv.eq(self.mstatus.w.mprv),
                    self.mstatus.r.mpp.eq(self.mstatus.w.mpp),
                ]

                if self.use_supervisor:
                    m.d.sync += [
                        self.mstatus.r.spp.eq(self.mstatus.w.spp),
                        self.mstatus.r.spie.eq(self.mstatus.w.spie),
                        self.mstatus.r.sie.eq(self.mstatus.w.sie),
                        self.mstatus.r.tw.eq(self.mstatus.w.tw),
                        self.mstatus.r.tsr.eq(self.mstatus.w.tsr),
                    ]

                if self.use_vm:
                    m.d.sync += [
                        self.mstatus.r.mxr.eq(self.mstatus.w.mxr),
                        self.mstatus.r.sum.eq(self.mstatus.w.sum),
                        self.mstatus.r.tvm.eq(self.mstatus.w.tvm),
                    ]

            if self.use_supervisor and self.use_fpu:
                m.d.sync += self.mstatus.r.fs.eq(self.mstatus.w.fs)

        m.d.comb += self.mstatus.r.sd.eq(self.mstatus.r.fs.all()
                                         | self.mstatus.r.vs.all()
                                         | self.mstatus.r.xs.all())
        if self.use_supervisor:
            m.d.comb += self.mstatus.r.sxl.eq(log2_int(self.xlen) - 4)
        if self.use_user:
            m.d.comb += self.mstatus.r.uxl.eq(log2_int(self.xlen) - 4)

        if self.use_supervisor:
            supervisor_interrupts = ((1 << Cause.S_SOFTWARE_INTERRUPT)
                                     | (1 << Cause.S_TIMER_INTERRUPT)
                                     | (1 << Cause.S_EXTERNAL_INTERRUPT))
            m.d.comb += [
                self.sstatus.r.sd.eq(self.mstatus.r.sd),
                self.sstatus.r.uxl.eq(self.mstatus.r.uxl),
                self.sstatus.r.mxr.eq(self.mstatus.r.mxr),
                self.sstatus.r.sum.eq(self.mstatus.r.sum),
                self.sstatus.r.xs.eq(self.mstatus.r.xs),
                self.sstatus.r.fs.eq(self.mstatus.r.fs),
                self.sstatus.r.vs.eq(self.mstatus.r.vs),
                self.sstatus.r.spp.eq(self.mstatus.r.spp),
                self.sstatus.r.spie.eq(self.mstatus.r.spie),
                self.sstatus.r.sie.eq(self.mstatus.r.sie),
            ]

            with m.If(self.sstatus.we):
                m.d.sync += [
                    self.mstatus.r.spp.eq(self.sstatus.w.spp),
                    self.mstatus.r.spie.eq(self.sstatus.w.spie),
                    self.mstatus.r.sie.eq(self.sstatus.w.sie),
                    self.mstatus.r.fs.eq(self.sstatus.w.fs),
                    self.mstatus.r.vs.eq(self.sstatus.w.vs),
                ]

                if self.use_vm:
                    m.d.sync += [
                        self.mstatus.r.mxr.eq(self.sstatus.w.mxr),
                        self.mstatus.r.sum.eq(self.sstatus.w.sum),
                    ]

            m.d.comb += [
                self.sip.r.eq(self.mip.r & self.mideleg.r
                              & supervisor_interrupts),
                self.sie.r.eq(self.mie.r & self.mideleg.r
                              & supervisor_interrupts),
            ]

            with m.If(self.sip.we):
                new_sip = MIP(self.xlen)
                m.d.comb += new_sip.eq((self.mip.r & ~supervisor_interrupts)
                                       | (self.sip.w & self.mideleg.r
                                          & supervisor_interrupts))
                m.d.sync += self.mip.r.ssip.eq(new_sip.ssip)

            with m.If(self.sie.we):
                m.d.sync += self.mie.r.eq((self.mie.r & ~supervisor_interrupts)
                                          | (self.sie.w & self.mideleg.r
                                             & supervisor_interrupts))

            with m.If(self.stvec.we):
                m.d.sync += [
                    self.stvec.r.mode.eq(self.stvec.w.mode & 1),
                    self.stvec.r.base.eq(self.stvec.w.base),
                ]

            with m.If(self.sepc.we):
                m.d.sync += self.sepc.r.eq(self.sepc.w)

            with m.If(self.scause.we):
                m.d.sync += [
                    self.scause.r.interrupt.eq(self.scause.w.interrupt),
                    self.scause.r.ecode.eq(
                        self.scause.w.ecode[:log2_int(self.xlen)]),
                ]

            with m.If(self.stval.we):
                m.d.sync += self.stval.r.eq(self.stval.w)

        m.d.comb += self.csr_stall.eq(wfi_active)

        return m
