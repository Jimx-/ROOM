from amaranth import *
from amaranth.lib.fifo import AsyncFIFO
import riscvmodel.insn as insn
import riscvmodel.csrnames as csrnames

from roomsoc.interconnect import wishbone
from .jtag import JTAGInterface, JTAGTap, JTAGReg
from .peripheral import Peripheral

from enum import Enum, IntEnum

__all__ = ('DebugModule', )

_dtmcs_layout = [("version", 4), ("abits", 6), ("dmistat", 2), ("idle", 3),
                 ("zero0", 1), ("dmireset", 1), ("dmihardreset", 1),
                 ("zero1", 14)]

_dmi_layout = [
    ("op", 2),
    ("data", 32),
    ("addr", 7),
]

_dmi_resp_layout = [
    ('data', 32),
    ('resp', 2),
]


class Version(IntEnum):
    NONE = 0
    V011 = 1
    V013 = 2
    OTHER = 15


class Command(IntEnum):
    ACCESS_REG = 0
    QUICK_ACCESS = 1
    ACCESS_MEM = 2


class Error(Enum):
    NONE = 0
    BUSY = 1
    UNSUPPORTED = 2
    EXCEPTION = 3
    HALT_RESUME = 4


class RegMode(Enum):
    R = 0
    W = 1
    W1 = 2
    RW = 3
    RW1C = 4
    WARL = 5


class DmiOp(Enum):
    NOP = 0
    READ = 1
    WRITE = 2


class DebugReg(IntEnum):
    DATA0 = 0x04
    DMCONTROL = 0x10
    DMSTATUS = 0x11
    HARTINFO = 0x12
    HALTSUM1 = 0x13
    ABSTRACTCS = 0x16
    COMMAND = 0x17
    PROGBUF0 = 0x20
    HALTSUM2 = 0x34
    HALTSUM3 = 0x35
    SBCS = 0x38
    SBADDRESS0 = 0x39
    SBDATA0 = 0x3c
    HALTSUM0 = 0x40


dmstatus_layout = [("version", 4, RegMode.R, Version.V013),
                   ("confstrptrvalid", 1, RegMode.R, False),
                   ("hasresethaltreq", 1, RegMode.R, False),
                   ("authbusy", 1, RegMode.R, False),
                   ("authenticated", 1, RegMode.R, True),
                   ("anyhalted", 1, RegMode.R, False),
                   ("allhalted", 1, RegMode.R, False),
                   ("anyrunning", 1, RegMode.R, False),
                   ("allrunning", 1, RegMode.R, False),
                   ("anyunavail", 1, RegMode.R, False),
                   ("allunavail", 1, RegMode.R, False),
                   ("anynonexistent", 1, RegMode.R, False),
                   ("allnonexistent", 1, RegMode.R, False),
                   ("anyresumeack", 1, RegMode.R, False),
                   ("allresumeack", 1, RegMode.R, False),
                   ("anyhavereset", 1, RegMode.R, False),
                   ("allhavereset", 1, RegMode.R, False),
                   ("zero0", 2, RegMode.R, 0),
                   ("impebreak", 1, RegMode.R, False),
                   ("zero1", 9, RegMode.R, 0)]

dmcontrol_layout = [("dmactive", 1, RegMode.RW, False),
                    ("ndmreset", 1, RegMode.RW, False),
                    ("clrresethaltreq", 1, RegMode.W1, False),
                    ("setresethaltreq", 1, RegMode.W1, False),
                    ("zero0", 2, RegMode.R, 0),
                    ("hartselhi", 10, RegMode.R, 0),
                    ("hartsello", 10, RegMode.R, 0),
                    ("hasel", 1, RegMode.RW, False),
                    ("zero1", 1, RegMode.R, 0),
                    ("ackhavereset", 1, RegMode.W1, False),
                    ("hartreset", 1, RegMode.RW, False),
                    ("resumereq", 1, RegMode.W1, False),
                    ("haltreq", 1, RegMode.W, False)]

abstractcs_layout = [
    ("datacount", 4, RegMode.R, 1), ("zero0", 4, RegMode.R, 0),
    ("cmderr", 3, RegMode.RW1C, 0), ("zero1", 1, RegMode.R, 0),
    ("busy", 1, RegMode.R, False), ("zero2", 11, RegMode.R, 0),
    ("progbufsize", 5, RegMode.R, 0), ("zero3", 3, RegMode.R, 0)
]

cmd_access_reg_layout = [
    ("regno", 16),
    ("write", 1),
    ("transfer", 1),
    ("postexec", 1),
    ("aarpostincrement", 1),
    ("aarsize", 3),
    ("zero0", 1),
]

command_layout = [("control", 24, RegMode.W, 0),
                  ("cmdtype", 8, RegMode.W, Command.ACCESS_REG)]

sbcs_layout = [("sbaccess8", 1, RegMode.R, True),
               ("sbaccess16", 1, RegMode.R, True),
               ("sbaccess32", 1, RegMode.R, True),
               ("sbaccess64", 1, RegMode.R, False),
               ("sbaccess128", 1, RegMode.R, False),
               ("sbasize", 7, RegMode.R, 32), ("sberror", 3, RegMode.RW1C, 0),
               ("sbreadondata", 1, RegMode.RW, False),
               ("sbautoincrement", 1, RegMode.RW, False),
               ("sbaccess", 3, RegMode.RW, 2),
               ("sbreadonaddr", 1, RegMode.RW, False),
               ("sbbusy", 1, RegMode.R, False),
               ("sbbusyerror", 1, RegMode.RW1C, False),
               ("zero0", 6, RegMode.R, 0), ("sbversion", 3, RegMode.R, 1)]

flat_layout = [("value", 32, RegMode.RW, 0)]

reg_map = {
    DebugReg.DMSTATUS: dmstatus_layout,
    DebugReg.DMCONTROL: dmcontrol_layout,
    DebugReg.HARTINFO: flat_layout,
    DebugReg.ABSTRACTCS: abstractcs_layout,
    DebugReg.COMMAND: command_layout,
    DebugReg.SBCS: sbcs_layout,
    DebugReg.SBADDRESS0: flat_layout,
    DebugReg.SBDATA0: flat_layout,
    DebugReg.DATA0: flat_layout,
    DebugReg.HALTSUM0: flat_layout,
    DebugReg.HALTSUM1: flat_layout,
}


class DmiError(IntEnum):
    NONE = 0
    RESERVED = 1
    OP_FAILED = 2
    BUSY = 3


class DebugTransportModule(Elaboratable):

    def __init__(self):
        self.jtag = JTAGInterface()

        self.dmi_req = Record(_dmi_layout)
        self.dmi_req_valid = Signal()
        self.dmi_req_ready = Signal()

        self.dmi_resp = Record(_dmi_resp_layout)
        self.dmi_resp_valid = Signal()
        self.dmi_resp_ready = Signal()

    def elaborate(self, platform):
        m = Module()

        m.domains += ClockDomain('jtag')
        m.d.comb += ClockSignal('jtag').eq(self.jtag.tck)

        tap = m.submodules.tap = DomainRenamer('jtag')(JTAGTap({
            JTAGReg.IDCODE: [("value", 32)],
            JTAGReg.DTMCS:
            _dtmcs_layout,
            JTAGReg.DMI:
            _dmi_layout,
        }))

        dmi_error = Signal(DmiError)
        dmi_busy_error = Signal()

        dtmcs = Record(_dtmcs_layout)
        m.d.comb += [
            dtmcs.version.eq(1),
            dtmcs.abits.eq(7),
            dtmcs.dmistat.eq(dmi_error),
            dtmcs.idle.eq(7),
        ]

        m.d.comb += [
            tap.port.connect(self.jtag),
            tap.regs[JTAGReg.IDCODE].r.eq(0x913),
            tap.regs[JTAGReg.DTMCS].r.eq(dtmcs),
        ]

        dtmcs_reg = tap.regs[JTAGReg.DTMCS]
        dmi_reg = tap.regs[JTAGReg.DMI]

        req_fifo_params = dict(width=len(self.dmi_req),
                               depth=8,
                               r_domain='sync',
                               w_domain='jtag')
        resp_fifo_params = dict(width=len(self.dmi_resp),
                                depth=8,
                                r_domain='jtag',
                                w_domain='sync')

        if platform is None or getattr(
                platform, 'get_async_fifo', None) is None or not callable(
                    getattr(platform, 'get_async_fifo')):
            req_fifo = m.submodules.req_fifo = AsyncFIFO(**req_fifo_params)
            resp_fifo = m.submodules.resp_fifo = AsyncFIFO(**resp_fifo_params)
        else:
            req_fifo = m.submodules.req_fifo = platform.get_async_fifo(
                **req_fifo_params)
            resp_fifo = m.submodules.resp_fifo = platform.get_async_fifo(
                **resp_fifo_params)

        resp_data = Record(self.dmi_resp.layout)

        with m.FSM(domain='jtag'):
            with m.State('IDLE'):
                with m.If(dmi_reg.update & (dmi_error == DmiError.NONE)):
                    m.d.jtag += req_fifo.w_data.eq(dmi_reg.w)

                    with m.If(dmi_reg.w.op == DmiOp.READ):
                        m.next = 'READ'
                    with m.Elif(dmi_reg.w.op == DmiOp.WRITE):
                        m.next = 'WRITE'

            with m.State('READ'):
                with m.If(dmi_reg.update | dmi_reg.capture):
                    m.d.comb += dmi_busy_error.eq(1)

                m.d.comb += req_fifo.w_en.eq(1)

                with m.If(req_fifo.w_rdy):
                    m.next = 'READ_WAIT'

            with m.State('READ_WAIT'):
                with m.If(dmi_reg.update | dmi_reg.capture):
                    m.d.comb += dmi_busy_error.eq(1)

                with m.If(resp_fifo.r_rdy):
                    m.d.comb += resp_fifo.r_en.eq(1)
                    m.d.jtag += dmi_reg.r.data.eq(resp_data.data)

                    m.next = 'IDLE'

            with m.State('WRITE'):
                with m.If(dmi_reg.update):
                    m.d.comb += dmi_busy_error.eq(1)

                m.d.comb += req_fifo.w_en.eq(1)

                with m.If(req_fifo.w_rdy):
                    m.next = 'IDLE'

        with m.If(dmi_busy_error):
            m.d.jtag += dmi_error.eq(DmiError.BUSY)
        m.d.comb += dmi_reg.r.op.eq(
            Mux(dmi_error != DmiError.NONE, dmi_error,
                Mux(dmi_busy_error, DmiError.BUSY, DmiError.NONE)))

        with m.If(dtmcs_reg.update & dtmcs_reg.w.dmireset):
            m.d.jtag += dmi_error.eq(DmiError.NONE)

        m.d.comb += [
            self.dmi_req.eq(req_fifo.r_data),
            self.dmi_req_valid.eq(req_fifo.r_rdy),
            req_fifo.r_en.eq(self.dmi_req_ready),
            resp_fifo.w_data.eq(self.dmi_resp),
            resp_fifo.w_en.eq(self.dmi_resp_valid),
            self.dmi_resp_ready.eq(resp_fifo.w_rdy),
            resp_data.eq(resp_fifo.r_data),
        ]

        return m


class DebugRegisterFile(Elaboratable):

    def __init__(self):
        self.dmi_req = Record(_dmi_layout)
        self.dmi_req_valid = Signal()
        self.dmi_req_ready = Signal()

        self.dmi_resp = Record(_dmi_resp_layout)
        self.dmi_resp_valid = Signal()
        self.dmi_resp_ready = Signal()

        self.ports = dict()

    def reg_port(self, addr, name=None, src_loc_at=0):
        if addr not in reg_map:
            raise ValueError("Unknown register {:x}.".format(addr))
        if addr in self.ports:
            raise ValueError(
                "Register {:x} has already been allocated.".format(addr))
        layout = [f[:2] for f in reg_map[addr]]
        port = Record([("r", layout), ("w", layout), ("update", 1),
                       ("capture", 1)],
                      name=name,
                      src_loc_at=1 + src_loc_at)
        for name, _, _, reset in reg_map[addr]:
            getattr(port.r, name).reset = reset
            getattr(port.w, name).reset = reset
        self.ports[addr] = port
        return port

    def elaborate(self, platform):
        m = Module()

        def do_read(addr, port):
            rec = Record(port.w.layout)
            m.d.sync += self.dmi_resp.data.eq(rec)
            for name, _, mode, _ in reg_map[addr]:
                dst = getattr(rec, name)
                src = getattr(port.w, name)
                if mode in {RegMode.R, RegMode.RW, RegMode.RW1C}:
                    m.d.comb += dst.eq(src)
                else:
                    m.d.comb += dst.eq(Const(0))
            m.d.sync += port.capture.eq(1)

        def do_write(addr, port):
            rec = Record(port.r.layout)
            m.d.comb += rec.eq(self.dmi_req.data)
            for name, _, mode, _ in reg_map[addr]:
                dst = getattr(port.r, name)
                src = getattr(rec, name)
                if mode in {RegMode.W, RegMode.RW}:
                    m.d.sync += dst.eq(src)
                elif mode is RegMode.W1:
                    m.d.sync += dst.eq(getattr(port.w, name) | src)
                elif mode is RegMode.RW1C:
                    m.d.sync += dst.eq(getattr(port.w, name) & ~src)

            m.d.sync += port.update.eq(1)

        with m.If(self.dmi_req_valid):
            m.d.comb += self.dmi_req_ready.eq(1)

            with m.Switch(self.dmi_req.addr):
                for addr, port in self.ports.items():
                    with m.Case(addr):
                        with m.If(self.dmi_req.op == DmiOp.READ):
                            do_read(addr, port)
                        with m.Elif(self.dmi_req.op == DmiOp.WRITE):
                            do_write(addr, port)

                with m.Default():
                    with m.If(self.dmi_req.op == DmiOp.READ):
                        m.d.sync += self.dmi_resp.data.eq(0)

        m.d.sync += self.dmi_resp_valid.eq(self.dmi_req_valid
                                           & (self.dmi_req.op == DmiOp.READ))

        for port in self.ports.values():
            with m.If(port.update):
                m.d.sync += port.update.eq(0)
            with m.If(port.capture):
                m.d.sync += port.capture.eq(0)

        return m


class DebugModuleCSR(IntEnum):
    HALTED = 0x0
    GOING = 0x4
    RESUMING = 0x8

    TRAMPOLINE = 0x300
    ABSTRACT = 0x320
    DATA = 0x380

    FLAGS = 0x400

    ROM_BASE = 0x800


class DebugController(Elaboratable):

    class GenerateI(Elaboratable):

        def __init__(self, insn):
            self.insn = insn

            self.rd = Signal(5)
            self.rs1 = Signal(5)
            self.imm = Signal(12)
            self.inst = Signal(32)

        def elaborate(self, platform):
            m = Module()

            m.d.comb += [
                self.inst[:7].eq(self.insn.field_opcode.value),
                self.inst[7:12].eq(self.rd),
                self.inst[12:15].eq(self.insn.field_funct3.value),
                self.inst[15:20].eq(self.rs1),
                self.inst[20:].eq(self.imm),
            ]

            return m

    class GenerateS(Elaboratable):

        def __init__(self, insn):
            self.insn = insn

            self.rs1 = Signal(5)
            self.rs2 = Signal(5)
            self.imm = Signal(12)
            self.inst = Signal(32)

        def elaborate(self, platform):
            m = Module()

            m.d.comb += [
                self.inst[:7].eq(self.insn.field_opcode.value),
                self.inst[7:12].eq(self.imm[:5]),
                self.inst[12:15].eq(self.insn.field_funct3.value),
                self.inst[15:20].eq(self.rs1),
                self.inst[20:25].eq(self.rs2),
                self.inst[25:].eq(self.imm[5:]),
            ]

            return m

    class GenerateGPRAccess(Elaboratable):

        def __init__(self):
            self.cmd = Record(cmd_access_reg_layout)
            self.insts = Signal(32 * 4)

        def elaborate(self, platform):
            m = Module()

            base = Mux(self.cmd.regno[0], 8, 9)

            gen_ld = DebugController.GenerateI(insn.InstructionLW)
            m.submodules += gen_ld
            m.d.comb += [
                gen_ld.rd.eq(self.cmd.regno),
                gen_ld.rs1.eq(base),
                gen_ld.imm.eq((DebugModuleCSR.DATA - DebugModuleCSR.ROM_BASE)
                              & 0xfff),
            ]

            gen_st = DebugController.GenerateS(insn.InstructionSW)
            m.submodules += gen_st
            m.d.comb += [
                gen_st.rs1.eq(base),
                gen_st.rs2.eq(self.cmd.regno),
                gen_st.imm.eq((DebugModuleCSR.DATA - DebugModuleCSR.ROM_BASE)
                              & 0xfff),
            ]

            gen_csr = DebugController.GenerateI(insn.InstructionCSRRW)
            m.submodules += gen_csr
            m.d.comb += [
                gen_csr.rd.eq(base),
                gen_csr.rs1.eq(base),
                gen_csr.imm.eq(csrnames.dscratch1),
            ]

            nop = insn.InstructionADDI.field_opcode.value

            # Instruction 1: CSRRW s1,dscratch1,s1 or CSRRW s0,dscratch1,s0
            m.d.comb += self.insts[:32].eq(gen_csr.inst)

            # Instruction 2: LW regno,DebugModuleCSR.DATA(s{0,1}) or SW regno,DebugModuleCSR.DATA(s{0,1})
            m.d.comb += self.insts[32:64].eq(
                Mux(self.cmd.transfer,
                    Mux(self.cmd.write, gen_ld.inst, gen_st.inst), nop))

            # Instruction 3: CSRRW s1,dscratch1,s1 or CSRRW s0,dscratch1,s0
            m.d.comb += self.insts[64:96].eq(gen_csr.inst)

            # Instruction 4:
            m.d.comb += self.insts[96:].eq(0x6f)

            return m

    class GenerateCSRAccess(Elaboratable):

        def __init__(self):
            self.cmd = Record(cmd_access_reg_layout)
            self.insts = Signal(32 * 7)

        def elaborate(self, platform):
            m = Module()

            gen_csrr = DebugController.GenerateI(insn.InstructionCSRRS)
            m.submodules += gen_csrr
            m.d.comb += [
                gen_csrr.rd.eq(8),
                gen_csrr.imm.eq(self.cmd.regno),
            ]

            gen_csrw = DebugController.GenerateI(insn.InstructionCSRRW)
            m.submodules += gen_csrw
            m.d.comb += [
                gen_csrw.rs1.eq(8),
                gen_csrw.imm.eq(self.cmd.regno),
            ]

            gen_st = DebugController.GenerateS(insn.InstructionSW)
            m.submodules += gen_st
            m.d.comb += [
                gen_st.rs1.eq(9),
                gen_st.rs2.eq(8),
                gen_st.imm.eq((DebugModuleCSR.DATA - DebugModuleCSR.ROM_BASE)
                              & 0xfff),
            ]

            gen_ld = DebugController.GenerateI(insn.InstructionLW)
            m.submodules += gen_ld
            m.d.comb += [
                gen_ld.rd.eq(8),
                gen_ld.rs1.eq(9),
                gen_ld.imm.eq((DebugModuleCSR.DATA - DebugModuleCSR.ROM_BASE)
                              & 0xfff),
            ]

            gen_csr_s0 = DebugController.GenerateI(insn.InstructionCSRRW)
            m.submodules += gen_csr_s0
            m.d.comb += [
                gen_csr_s0.rd.eq(8),
                gen_csr_s0.rs1.eq(8),
                gen_csr_s0.imm.eq(csrnames.dscratch0),
            ]

            gen_csr_s1 = DebugController.GenerateI(insn.InstructionCSRRW)
            m.submodules += gen_csr_s1
            m.d.comb += [
                gen_csr_s1.rd.eq(9),
                gen_csr_s1.rs1.eq(9),
                gen_csr_s1.imm.eq(csrnames.dscratch1),
            ]

            nop = insn.InstructionADDI.field_opcode.value

            # Instrution 1: CSRRW s0,dscratch0,s0
            m.d.comb += self.insts[:32].eq(gen_csr_s0.inst)

            # Instrution 2: CSRRW s1,dscratch1,s1
            m.d.comb += self.insts[32:64].eq(gen_csr_s1.inst)

            # Instrution 3: CSRR s0,regno or LD s0,DebugModuleCSR.DATA(s1)
            m.d.comb += self.insts[64:96].eq(
                Mux(self.cmd.transfer,
                    Mux(self.cmd.write, gen_ld.inst, gen_csrr.inst), nop))

            # Instrution 4: SW s0,DebugModuleCSR.DATA(s1) or CSRW regno,s0
            m.d.comb += self.insts[96:128].eq(
                Mux(self.cmd.transfer,
                    Mux(self.cmd.write, gen_csrw.inst, gen_st.inst), nop))

            # Instrution 5: CSRRW s1,dscratch1,s1
            m.d.comb += self.insts[128:160].eq(gen_csr_s1.inst)

            # Instrution 6: CSRRW s0,dscratch0,s0
            m.d.comb += self.insts[160:192].eq(gen_csr_s0.inst)

            # Instrution 7:
            m.d.comb += self.insts[192:224].eq(0x6f)

            return m

    def __init__(self, debugrf, csr_bank):
        self.debug_int = Signal()

        self.dmstatus = debugrf.reg_port(DebugReg.DMSTATUS)
        self.dmcontrol = debugrf.reg_port(DebugReg.DMCONTROL)
        self.hartinfo = debugrf.reg_port(DebugReg.HARTINFO)
        self.abstractcs = debugrf.reg_port(DebugReg.ABSTRACTCS)
        self.command = debugrf.reg_port(DebugReg.COMMAND)
        self.data0 = debugrf.reg_port(DebugReg.DATA0)
        self.haltsum0 = debugrf.reg_port(DebugReg.HALTSUM0)

        self._halted = csr_bank.csr(32, 'w', addr=DebugModuleCSR.HALTED)
        self._going = csr_bank.csr(32, 'rw', addr=DebugModuleCSR.GOING)
        self._resuming = csr_bank.csr(32, 'r', addr=DebugModuleCSR.RESUMING)
        self._trampoline = csr_bank.csr(32,
                                        'r',
                                        addr=DebugModuleCSR.TRAMPOLINE)
        self._flags = csr_bank.csr(8, 'r', addr=DebugModuleCSR.FLAGS)

        self._abstract_rom = csr_bank.csr(32 * 7,
                                          'r',
                                          addr=DebugModuleCSR.ABSTRACT)
        self._shadowed_data = csr_bank.csr(32, 'rw', addr=DebugModuleCSR.DATA)

    def elaborate(self, platform):
        m = Module()

        hart_halted = Signal()

        with m.If(self.dmcontrol.update):
            m.d.sync += self.dmcontrol.w.dmactive.eq(self.dmcontrol.r.dmactive)

            with m.If(self.dmcontrol.w.dmactive):
                m.d.sync += [
                    self.dmcontrol.w.ndmreset.eq(self.dmcontrol.r.ndmreset),
                    self.dmcontrol.w.hartsello.eq(self.dmcontrol.r.hartsello),
                    self.dmcontrol.w.hartreset.eq(self.dmcontrol.r.hartreset),
                    self.dmcontrol.w.haltreq.eq(self.dmcontrol.r.haltreq),
                    self.dmcontrol.w.resumereq.eq(self.dmcontrol.r.resumereq),
                ]

        with m.If(~self.dmcontrol.w.dmactive):
            m.d.sync += [
                getattr(self.dmcontrol.w,
                        field).eq(getattr(self.dmcontrol.w, field).reset)
                for field in (
                    'ndmreset',
                    'hartselhi',
                    'hartsello',
                    'hasel',
                    'hartreset',
                    'resumereq',
                )
            ]

        with m.If(~self.dmcontrol.w.dmactive):
            m.d.sync += self.debug_int.eq(0)
        with m.Else():
            with m.If(self.dmcontrol.update):
                m.d.sync += self.debug_int.eq(self.dmcontrol.r.haltreq)

        with m.If(self.abstractcs.update):
            m.d.sync += self.abstractcs.w.cmderr.eq(self.abstractcs.r.cmderr)
        with m.If(~self.dmcontrol.w.dmactive):
            m.d.sync += self.abstractcs.w.cmderr.eq(0)

        abstract_data = Signal(32)
        m.d.comb += [
            self.data0.w.eq(abstract_data),
            self._shadowed_data.r_data.eq(abstract_data),
        ]

        with m.If(self.data0.update):
            m.d.sync += abstract_data.eq(self.data0.r)
        with m.Elif(self._shadowed_data.w_stb):
            m.d.sync += abstract_data.eq(self._shadowed_data.w_data)
        with m.If(~self.dmcontrol.w.dmactive):
            m.d.sync += abstract_data.eq(0)

        m.d.comb += [
            self.dmstatus.w.version.eq(Version.V013),
            self.dmstatus.w.authenticated.eq(1),
            self.dmstatus.w.allrunning.eq(~hart_halted),
            self.dmstatus.w.anyrunning.eq(~hart_halted),
            self.dmstatus.w.allhalted.eq(hart_halted),
            self.dmstatus.w.anyhalted.eq(hart_halted),
            self.haltsum0.w[0].eq(self.dmstatus.w.allhalted),
        ]

        with m.If(self._halted.w_stb):
            m.d.sync += hart_halted.eq(1)

        go_req = Signal()
        go_abstract = Signal()

        with m.If(~self.dmcontrol.w.dmactive):
            m.d.sync += go_req.eq(0)
        with m.Elif(go_abstract):
            m.d.sync += go_req.eq(1)
        with m.Elif(self._going.w_stb):
            m.d.sync += go_req.eq(0)

        m.d.comb += self._flags.r_data[0].eq(go_req)

        #
        # Abstract command instruction generation
        #

        cmd_access_reg = Record(cmd_access_reg_layout)
        m.d.comb += cmd_access_reg.eq(self.command.r.control)

        # JAL from DebugModuleCSR.TRAMPOLINE to DebugModuleCSR.ABSTRACT
        m.d.comb += self._trampoline.r_data.eq(
            insn.InstructionJAL.field_opcode.value
            | (((DebugModuleCSR.ABSTRACT -
                 DebugModuleCSR.TRAMPOLINE) >> 1) << 21))

        gen_gpr_access = DebugController.GenerateGPRAccess()
        m.submodules += gen_gpr_access
        m.d.comb += gen_gpr_access.cmd.eq(cmd_access_reg)

        gen_csr_access = DebugController.GenerateCSRAccess()
        m.submodules += gen_csr_access
        m.d.comb += gen_csr_access.cmd.eq(cmd_access_reg)

        with m.If(go_abstract):
            with m.If(cmd_access_reg.regno.matches("0000------------")):
                m.d.sync += self._abstract_rom.r_data.eq(gen_csr_access.insts)
            with m.Elif(cmd_access_reg.regno.matches("00010000000-----")):
                m.d.sync += self._abstract_rom.r_data.eq(gen_gpr_access.insts)

        # Abstract command FSM
        #

        with m.FSM():
            with m.State('WAIT'):
                with m.If(self.command.update):
                    m.d.sync += self.abstractcs.w.busy.eq(1)
                    m.next = 'CMD_START'

            with m.State('CMD_START'):
                with m.Switch(self.command.r.cmdtype):
                    with m.Case(Command.ACCESS_REG):
                        m.d.comb += go_abstract.eq(1)
                        m.next = 'CMD_EXEC'
                    with m.Case():
                        m.d.sync += self.abstractcs.w.cmderr.eq(
                            Error.UNSUPPORTED)
                        m.next = 'CMD_DONE'

            with m.State('CMD_EXEC'):
                pass

            with m.State('CMD_DONE'):
                m.d.sync += self.abstractcs.w.busy.eq(0)
                m.next = 'WAIT'

        return m


class BusError(IntEnum):
    NONE = 0
    TIMEOUT = 1
    BAD_ADDRESS = 2
    MISALIGNED = 3
    BAD_SIZE = 4
    OTHER = 7


class AccessSize(IntEnum):
    BYTE = 0
    HALF = 1
    WORD = 2


class SystemBusAccess(Elaboratable):

    def __init__(self, debugrf):
        self.bus = wishbone.Interface(addr_width=30,
                                      data_width=32,
                                      granularity=8)

        self.dbus_busy = Signal()

        self.sbcs = debugrf.reg_port(DebugReg.SBCS)
        self.sbaddress0 = debugrf.reg_port(DebugReg.SBADDRESS0)
        self.sbdata0 = debugrf.reg_port(DebugReg.SBDATA0)

    def elaborate(self, platform):
        m = Module()

        addr = self.sbaddress0.w.value
        size = self.sbcs.r.sbaccess

        width = Signal(6)
        m.d.comb += width.eq((1 << size) * 8)

        sberror = self.sbcs.w.sberror
        m.d.comb += self.dbus_busy.eq(self.sbcs.w.sbbusy)

        m.d.comb += [
            self.sbcs.w.sbaccess8.eq(1),
            self.sbcs.w.sbaccess16.eq(1),
            self.sbcs.w.sbaccess32.eq(1),
            self.sbcs.w.sbasize.eq(32),
            self.sbcs.w.sbversion.eq(1)
        ]

        with m.If(self.sbcs.update):
            m.d.sync += [
                self.sbcs.w.sbbusyerror.eq(self.sbcs.r.sbbusyerror),
                self.sbcs.w.sberror.eq(self.sbcs.r.sberror)
            ]

        we = Signal()
        re = Signal()

        with m.If(self.sbdata0.update):
            with m.If(self.sbcs.w.sbbusy):
                m.d.sync += self.sbcs.w.sbbusyerror.eq(1)
            with m.Else():
                m.d.sync += we.eq(~sberror.bool())

        with m.If(self.sbdata0.capture):
            with m.If(self.sbcs.w.sbbusy):
                m.d.sync += self.sbcs.w.sbbusyerror.eq(1)
            with m.Else():
                m.d.sync += re.eq(self.sbcs.r.sbreadondata & ~sberror.bool())

        with m.If(self.sbaddress0.update):
            with m.If(self.sbcs.w.sbbusy):
                m.d.sync += self.sbcs.w.sbbusyerror.eq(1)
            with m.Else():
                m.d.sync += [
                    re.eq(self.sbcs.r.sbreadonaddr & ~sberror.bool()),
                    self.sbaddress0.w.value.eq(self.sbaddress0.r.value)
                ]

        with m.FSM():
            with m.State("IDLE"):
                with m.If(we | re):
                    m.d.sync += we.eq(0), re.eq(0)
                    with m.If(size > AccessSize.WORD):
                        m.d.sync += sberror.eq(BusError.BAD_SIZE)
                    with m.Elif((addr & (1 << size) - 1) != 0):
                        m.d.sync += sberror.eq(BusError.MISALIGNED)
                    with m.Else():
                        m.d.sync += [
                            self.bus.cyc.eq(1),
                            self.bus.stb.eq(1),
                            self.bus.adr.eq(addr[2:]),
                            self.bus.we.eq(we),
                            self.bus.sel.eq((1 << (1 << size)) -
                                            1 << addr[:2]),
                            self.bus.dat_w.eq((self.sbdata0.r & (1 << width) -
                                               1) << addr[:2] * 8)
                        ]
                        m.next = "BUSY"

            with m.State("BUSY"):
                m.d.comb += self.sbcs.w.sbbusy.eq(1)
                with m.If(self.bus.ack):
                    m.d.sync += [
                        self.bus.cyc.eq(0),
                        self.bus.stb.eq(0),
                        self.bus.we.eq(0),
                    ]

                    with m.If(~self.bus.we):
                        m.d.sync += self.sbdata0.w.eq(
                            (self.bus.dat_r >> addr[:2] * 8)
                            & (1 << width) - 1)
                    with m.If(self.sbcs.r.sbautoincrement):
                        m.d.sync += addr.eq(addr + (1 << size))

                    m.next = "IDLE"

        return m


class DebugModule(Peripheral, Elaboratable):

    def __init__(self, rom_init, *, name=None):
        super().__init__(name=name)

        self.rom_init = rom_init

        self.jtag = JTAGInterface()
        self.debug_int = Signal()

        self.dbus = wishbone.Interface(addr_width=30,
                                       data_width=32,
                                       granularity=8)

        self._rom_bus = self.window(addr_width=Shape.cast(range(
            len(rom_init))).width,
                                    data_width=32,
                                    granularity=8,
                                    addr=DebugModuleCSR.ROM_BASE)

        bank = self.csr_bank()
        self._regfile = DebugRegisterFile()
        self._controller = DebugController(self._regfile, bank)

        self._bridge = self.bridge(data_width=32, granularity=8, alignment=2)
        self.bus = self._bridge.bus

    def elaborate(self, platform):
        m = Module()
        m.submodules.bridge = self._bridge

        m.submodules.rom = wishbone.SRAM(Memory(
            width=self._rom_bus.data_width,
            depth=2**self._rom_bus.addr_width,
            init=self.rom_init),
                                         read_only=True,
                                         bus=self._rom_bus)

        dtm = m.submodules.dtm = DebugTransportModule()
        regfile = m.submodules.regfile = self._regfile
        controller = m.submodules.controller = self._controller
        sba = m.submodules.sb_access = SystemBusAccess(regfile)

        m.d.comb += [
            dtm.jtag.connect(self.jtag),
            regfile.dmi_req.eq(dtm.dmi_req),
            regfile.dmi_req_valid.eq(dtm.dmi_req_valid),
            dtm.dmi_req_ready.eq(regfile.dmi_req_ready),
            dtm.dmi_resp.eq(regfile.dmi_resp),
            dtm.dmi_resp_valid.eq(regfile.dmi_resp_valid),
            regfile.dmi_resp_ready.eq(dtm.dmi_resp_ready),
        ]

        m.d.comb += self.debug_int.eq(controller.debug_int)

        m.d.comb += sba.bus.connect(self.dbus)

        return m
