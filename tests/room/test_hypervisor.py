from amaranth import Elaboratable, Module, Signal
from amaranth.sim import Simulator

from room.consts import CSRCommand, IssueQueueType, PrivilegeMode
from room.csr import CSRFile
from room.exc import Cause, ExceptionUnit
from room.mmu import PageTableWalker

CORE_PARAMS = dict(
    xlen=64,
    flen=64,
    use_fpu=False,
    fma_latency=0,
    vaddr_bits=39,
    paddr_bits=36,
    io_regions={},
    core_width=1,
    use_vm=True,
    use_user=True,
    use_supervisor=True,
    use_hypervisor=True,
    fetch_width=1,
    fetch_buffer_size=4,
    max_br_count=1,
    use_bpd=False,
    bpd_meta_length=1,
    n_ras_entries=1,
    num_int_pregs=32,
    num_fp_pregs=32,
    num_breakpoints=0,
    issue_params={
        IssueQueueType.INT: dict(issue_width=1),
        IssueQueueType.MEM: dict(issue_width=1),
    },
    num_rob_rows=4,
    ldq_size=4,
    stq_size=4,
    pg_levels=3,
    n_dtlb_sets=4,
    n_dtlb_ways=1,
)


class HypervisorCSRHarness(Elaboratable):

    def __init__(self):
        self.csr = CSRFile(CORE_PARAMS, width=CORE_PARAMS["xlen"])
        self.exc = ExceptionUnit(CORE_PARAMS)
        self.ptw = PageTableWalker(CORE_PARAMS)
        self.csr.add_csrs(self.exc.iter_csrs())
        self.csr.add_csrs(self.ptw.iter_csrs())
        self.port = self.csr.access_port()
        self.prv = Signal(PrivilegeMode, reset=PrivilegeMode.S)
        self.v = Signal()

    def elaborate(self, platform):
        m = Module()
        m.submodules.csr = self.csr
        m.submodules.exc = self.exc
        m.submodules.ptw = self.ptw
        m.d.comb += [
            self.csr.prv.eq(self.prv),
            self.csr.v.eq(self.v),
        ]
        return m


def test_hypervisor_csr_inventory():
    dut = HypervisorCSRHarness()
    exc = dut.exc
    ptw = dut.ptw

    expected = {
        "hstatus": 0x600,
        "hedeleg": 0x602,
        "hideleg": 0x603,
        "hie": 0x604,
        "htimedelta": 0x605,
        "hcounteren": 0x606,
        "hgeie": 0x607,
        "henvcfg": 0x60a,
        "htval": 0x643,
        "hip": 0x644,
        "hvip": 0x645,
        "htinst": 0x64a,
        "hgeip": 0xe12,
        "vsstatus": 0x200,
        "vsie": 0x204,
        "vstvec": 0x205,
        "vsscratch": 0x240,
        "vsepc": 0x241,
        "vscause": 0x242,
        "vstval": 0x243,
        "vsip": 0x244,
        "mtinst": 0x34a,
        "mtval2": 0x34b,
    }
    for name, address in expected.items():
        assert getattr(exc, name).addr == address

    assert ptw.vsatp.addr == 0x280
    assert ptw.hgatp.addr == 0x680
    Simulator(dut)


def test_supervisor_csrs_alias_vs_bank_when_virtualized():
    dut = HypervisorCSRHarness()

    def proc():
        port = dut.port

        def write(addr, value):
            yield port.addr.eq(addr)
            yield port.w_data.eq(value)
            yield port.cmd.eq(CSRCommand.W)
            yield
            yield port.cmd.eq(CSRCommand.X)
            yield

        def read(addr):
            yield port.addr.eq(addr)
            yield port.cmd.eq(CSRCommand.R)
            yield
            value = yield port.r_data
            yield port.cmd.eq(CSRCommand.X)
            yield
            return value

        yield dut.v.eq(0)
        yield
        yield from write(0x603, (1 << 2) | (1 << 6) | (1 << 10))
        yield dut.v.eq(1)
        yield

        vsstatus_value = ((1 << 1) | (1 << 5) | (1 << 8) | (1 << 18)
                          | (1 << 19))
        yield from write(0x100, vsstatus_value)
        yield from write(0x104, (1 << 1) | (1 << 5) | (1 << 9))
        yield from write(0x105, 0x8003)
        yield from write(0x140, 0x1111222233334444)
        yield from write(0x141, 0x80001234)
        yield from write(0x142, (1 << 63) | 0x2a)
        yield from write(0x143, 0x5555666677778888)
        yield from write(0x144, 1 << 1)
        vsatp_value = (8 << 60) | (0x1234 << 44) | 0xabcde
        yield from write(0x180, vsatp_value)

        assert (yield dut.exc.vsstatus.r.sie) == 1
        assert (yield dut.exc.vsstatus.r.spie) == 1
        assert (yield dut.exc.vsstatus.r.spp) == 1
        assert (yield dut.exc.vsstatus.r.sum) == 1
        assert (yield dut.exc.vsstatus.r.mxr) == 1
        assert (yield dut.exc.vsie.r.as_value()) == ((1 << 1) | (1 << 5)
                                                     | (1 << 9))
        assert (yield dut.exc.vstvec.r.as_value()) == 0x8001
        assert (yield dut.exc.vsscratch.r.value) == 0x1111222233334444
        assert (yield dut.exc.vsepc.r.value) == 0x80001234
        assert (yield dut.exc.vscause.r.interrupt) == 1
        assert (yield dut.exc.vscause.r.ecode) == 0x2a
        assert (yield dut.exc.vstval.r.value) == 0x5555666677778888
        assert (yield dut.exc.hvip.r.vssip) == 1
        assert (yield dut.ptw.vsatp.r.mode) == 8
        assert (yield dut.ptw.vsatp.r.asid) == 0x1234
        assert (yield dut.ptw.vsatp.r.ppn) == 0xabcde

        assert (yield from read(0x100)) & vsstatus_value == vsstatus_value
        assert (yield from read(0x104)) == ((1 << 1) | (1 << 5) | (1 << 9))
        assert (yield from read(0x105)) == 0x8001
        assert (yield from read(0x140)) == 0x1111222233334444
        assert (yield from read(0x141)) == 0x80001234
        assert (yield from read(0x142)) == ((1 << 63) | 0x2a)
        assert (yield from read(0x143)) == 0x5555666677778888
        assert (yield from read(0x144)) == 1 << 1
        assert (yield from read(0x180)) == vsatp_value

        # Leaving virtualization makes the same addresses refer to HS state.
        yield dut.v.eq(0)
        yield
        assert (yield from read(0x105)) == 0
        assert (yield from read(0x140)) == 0
        assert (yield from read(0x180)) == 0

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(proc)
    sim.run()


def test_hypervisor_csr_warl_masks_and_interrupt_views():
    dut = HypervisorCSRHarness()

    def proc():
        port = dut.port

        def write(addr, value):
            yield port.addr.eq(addr)
            yield port.w_data.eq(value)
            yield port.cmd.eq(CSRCommand.W)
            yield
            yield port.cmd.eq(CSRCommand.X)
            yield

        yield dut.v.eq(0)
        yield from write(0x602, (1 << 64) - 1)
        yield from write(0x603, (1 << 64) - 1)
        yield from write(0x604, (1 << 64) - 1)
        yield from write(0x645, (1 << 64) - 1)
        yield from write(0x60a, (1 << 64) - 1)
        yield from write(0x600, (1 << 64) - 1)

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
        hideleg_mask = (1 << 2) | (1 << 6) | (1 << 10)
        assert (yield dut.exc.hedeleg.r.value) == hedeleg_mask
        assert (yield dut.exc.hideleg.r.value) == hideleg_mask
        assert (yield dut.exc.hie.r.as_value()) == hideleg_mask
        assert (yield dut.exc.hvip.r.as_value()) == hideleg_mask
        assert (yield dut.exc.hip.r.as_value()) == hideleg_mask
        assert (yield dut.exc.vsip.r.as_value()) == ((1 << 1) | (1 << 5)
                                                     | (1 << 9))
        assert (yield dut.exc.hstatus.r.hu) == 1
        assert (yield dut.exc.hstatus.r.vtvm) == 1
        assert (yield dut.exc.hstatus.r.vtw) == 1
        assert (yield dut.exc.hstatus.r.vtsr) == 1
        assert (yield dut.exc.henvcfg.r.as_value()) == 1
        assert (yield dut.exc.hgeie.r.value) == 0
        assert (yield dut.exc.hgeip.r.value) == 0

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(proc)
    sim.run()


def test_virtual_interrupt_enable_and_pending_aliases():
    dut = HypervisorCSRHarness()

    def proc():
        port = dut.port

        def write(addr, value):
            yield port.addr.eq(addr)
            yield port.w_data.eq(value)
            yield port.cmd.eq(CSRCommand.W)
            yield
            yield port.cmd.eq(CSRCommand.X)
            yield

        def read(addr):
            yield port.addr.eq(addr)
            yield port.cmd.eq(CSRCommand.R)
            yield
            value = yield port.r_data
            yield port.cmd.eq(CSRCommand.X)
            yield
            return value

        virtual = (1 << 2) | (1 << 6) | (1 << 10)
        supervisor = (1 << 1) | (1 << 5) | (1 << 9)

        # HIE is an architectural view of MIE's virtual-interrupt bits.
        yield from write(0x304, virtual)
        assert (yield from read(0x604)) == virtual
        yield from write(0x604, 1 << 6)
        assert (yield from read(0x304)) & virtual == 1 << 6

        # VSIE/VSIP expose shifted aliases only when hideleg delegates them.
        yield from write(0x603, (1 << 2) | (1 << 10))
        yield dut.v.eq(1)
        yield
        yield from write(0x104, supervisor)
        assert (yield from read(0x104)) == ((1 << 1) | (1 << 9))
        yield dut.v.eq(0)
        yield
        assert (yield from read(0x604)) == ((1 << 2) | (1 << 10))

        yield from write(0x645, virtual)
        yield dut.v.eq(1)
        yield
        assert (yield from read(0x144)) == ((1 << 1) | (1 << 9))
        yield from write(0x144, 0)
        yield dut.v.eq(0)
        yield
        assert (yield from read(0x645)) == ((1 << 6) | (1 << 10))

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(proc)
    sim.run()


def test_injected_virtual_interrupt_traps_to_vs_and_sret_returns():
    dut = ExceptionUnit(CORE_PARAMS)

    def proc():
        virtual = (1 << 2) | (1 << 6) | (1 << 10)

        yield dut.mstatus.w.mpp.eq(PrivilegeMode.S)
        yield dut.mstatus.we.eq(1)
        yield
        yield dut.mstatus.we.eq(0)
        yield dut.system_insn_imm.eq(0x302)  # MRET to HS
        yield dut.system_insn.eq(1)
        yield
        yield dut.system_insn.eq(0)
        yield

        yield dut.hideleg.w.value.eq(virtual)
        yield dut.hideleg.we.eq(1)
        yield dut.hie.w.eq(virtual)
        yield dut.hie.we.eq(1)
        yield dut.hvip.w.eq(virtual)
        yield dut.hvip.we.eq(1)
        yield dut.vstvec.w.base.eq(0x80001000 >> 2)
        yield dut.vstvec.w.mode.eq(1)
        yield dut.vstvec.we.eq(1)
        yield dut.hstatus.w.spv.eq(1)
        yield dut.hstatus.w.spvp.eq(1)
        yield dut.hstatus.we.eq(1)
        yield
        yield dut.hideleg.we.eq(0)
        yield dut.hie.we.eq(0)
        yield dut.hvip.we.eq(0)
        yield dut.vstvec.we.eq(0)
        yield dut.hstatus.we.eq(0)
        yield dut.system_insn_imm.eq(0x102)  # SRET to VS
        yield dut.system_insn.eq(1)
        yield
        yield dut.system_insn.eq(0)
        yield

        assert (yield dut.v) == 1
        assert (yield dut.interrupt) == 0

        # At VS privilege the interrupt is gated by VSSTATUS.SIE.
        yield dut.vsstatus.w.sie.eq(1)
        yield dut.vsstatus.we.eq(1)
        yield
        yield dut.vsstatus.we.eq(0)
        yield
        assert (yield dut.interrupt) == 1
        # VSEI has the highest priority of the three injected classes.
        assert (yield dut.interrupt_cause) == ((1 << 63) | 10)

        yield dut.epc.eq(0x80000080)
        yield dut.cause.eq((1 << 63) | 10)
        yield dut.exception.eq(1)
        yield
        yield dut.exception.eq(0)
        yield

        assert (yield dut.v) == 1
        assert (yield dut.prv) == PrivilegeMode.S
        assert (yield dut.vsepc.r.value) == 0x80000080
        assert (yield dut.vscause.r.interrupt) == 1
        assert (yield dut.vscause.r.ecode) == Cause.S_EXTERNAL_INTERRUPT
        assert (yield dut.vsstatus.r.sie) == 0
        assert (yield dut.vsstatus.r.spie) == 1

        yield dut.system_insn_imm.eq(0x102)
        yield dut.system_insn.eq(1)
        yield
        assert (yield dut.exc_vector) == 0x80000080
        yield dut.system_insn.eq(0)
        yield
        assert (yield dut.v) == 1
        assert (yield dut.prv) == PrivilegeMode.S
        assert (yield dut.vsstatus.r.sie) == 1

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(proc)
    sim.run()


def test_undelegated_virtual_interrupt_traps_to_hs():
    dut = ExceptionUnit(CORE_PARAMS)

    def proc():
        yield dut.mstatus.w.mpp.eq(PrivilegeMode.S)
        yield dut.mstatus.we.eq(1)
        yield
        yield dut.mstatus.we.eq(0)
        yield dut.system_insn_imm.eq(0x302)
        yield dut.system_insn.eq(1)
        yield
        yield dut.system_insn.eq(0)
        yield

        yield dut.hie.w.vssie.eq(1)
        yield dut.hie.we.eq(1)
        yield dut.hvip.w.vssip.eq(1)
        yield dut.hvip.we.eq(1)
        yield dut.hstatus.w.spv.eq(1)
        yield dut.hstatus.w.spvp.eq(1)
        yield dut.hstatus.we.eq(1)
        yield
        yield dut.hie.we.eq(0)
        yield dut.hvip.we.eq(0)
        yield dut.hstatus.we.eq(0)
        yield dut.system_insn_imm.eq(0x102)
        yield dut.system_insn.eq(1)
        yield
        yield dut.system_insn.eq(0)
        yield

        # A virtual interrupt not selected by hideleg is taken by HS even
        # when VSSTATUS.SIE is clear, because HS is above VS.
        assert (yield dut.interrupt_cause) == ((1 << 63) | 2)
        yield dut.epc.eq(0x80000040)
        yield dut.cause.eq((1 << 63) | 2)
        yield dut.exception.eq(1)
        yield
        yield dut.exception.eq(0)
        yield
        assert (yield dut.v) == 0
        assert (yield dut.scause.r.interrupt) == 1
        assert (yield dut.scause.r.ecode) == 2
        assert (yield dut.hstatus.r.spv) == 1

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(proc)
    sim.run()


def test_hs_vs_ecall_round_trip():
    dut = ExceptionUnit(CORE_PARAMS)
    assert dut.hstatus.addr == 0x600

    def proc():
        yield dut.medeleg.w.value.eq(1 << Cause.ECALL_FROM_VS)
        yield dut.medeleg.we.eq(1)
        yield
        yield dut.medeleg.we.eq(0)
        yield

        yield dut.mstatus.w.mpp.eq(PrivilegeMode.S)
        yield dut.mstatus.we.eq(1)
        yield
        yield dut.mstatus.we.eq(0)
        yield

        yield dut.system_insn_imm.eq(0x302)  # MRET
        yield dut.system_insn.eq(1)
        yield
        yield dut.system_insn.eq(0)
        yield
        assert (yield dut.prv) == PrivilegeMode.S
        assert (yield dut.v) == 0

        yield dut.hstatus.w.spv.eq(1)
        yield dut.hstatus.w.spvp.eq(1)
        yield dut.hstatus.we.eq(1)
        yield
        yield dut.hstatus.we.eq(0)
        yield

        yield dut.system_insn_imm.eq(0x102)  # SRET
        yield dut.system_insn.eq(1)
        yield
        yield dut.system_insn.eq(0)
        yield
        assert (yield dut.prv) == PrivilegeMode.S
        assert (yield dut.v) == 1

        yield dut.epc.eq(0x80000084)
        yield dut.system_insn_imm.eq(0)  # ECALL
        yield dut.system_insn.eq(1)
        yield
        yield dut.system_insn.eq(0)
        yield

        assert (yield dut.prv) == PrivilegeMode.S
        assert (yield dut.v) == 0
        assert (yield dut.scause.r.ecode) == Cause.ECALL_FROM_VS
        assert (yield dut.sepc.r.value) == 0x80000084
        assert (yield dut.hstatus.r.spv) == 1
        assert (yield dut.hstatus.r.spvp) == 1

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(proc)
    sim.run()
