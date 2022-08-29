from amaranth import *
from amaranth.hdl.rec import DIR_FANIN, DIR_FANOUT

from enum import Enum, IntEnum

__all__ = ('JTAGInterface', 'DebugUnit')

_jtag_layout = [
    ("tck", 1, DIR_FANIN),
    ("tdi", 1, DIR_FANIN),
    ("tdo", 1, DIR_FANOUT),
    ("tms", 1, DIR_FANIN),
    ("trst", 1, DIR_FANIN),
]


class JTAGInterface(Record):

    def __init__(self, name=None):
        super().__init__(_jtag_layout, name=name)


class JTAGReg(IntEnum):
    BYPASS = 0x00
    IDCODE = 0x01
    DTMCS = 0x10
    DMI = 0x11


_dtmcs_layout = [("version", 4), ("abits", 6), ("dmistat", 2), ("idle", 3),
                 ("zero0", 1), ("dmireset", 1), ("dmihardreset", 1),
                 ("zero1", 14)]

_dmi_layout = [
    ("op", 2),
    ("data", 32),
    ("addr", 7),
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


class DebugRegisterFile(Elaboratable):

    def __init__(self, dmi):
        self.dmi = dmi
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
            m.d.sync += self.dmi.r.data.eq(rec)
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
            m.d.comb += rec.eq(self.dmi.w.data)
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

        with m.If(self.dmi.update):
            with m.Switch(self.dmi.w.addr):
                for addr, port in self.ports.items():
                    with m.Case(addr):
                        with m.If(self.dmi.w.op == DmiOp.READ):
                            do_read(addr, port)
                        with m.Elif(self.dmi.w.op == DmiOp.WRITE):
                            do_write(addr, port)

        for port in self.ports.values():
            with m.If(port.update):
                m.d.sync += port.update.eq(0)
            with m.If(port.capture):
                m.d.sync += port.capture.eq(0)

        return m


class DebugController(Elaboratable):

    def __init__(self, debugrf):
        self.dmstatus = debugrf.reg_port(DebugReg.DMSTATUS)
        self.dmcontrol = debugrf.reg_port(DebugReg.DMCONTROL)
        self.hartinfo = debugrf.reg_port(DebugReg.HARTINFO)
        self.abstractcs = debugrf.reg_port(DebugReg.ABSTRACTCS)
        self.command = debugrf.reg_port(DebugReg.COMMAND)
        self.data0 = debugrf.reg_port(DebugReg.DATA0)
        self.haltsum0 = debugrf.reg_port(DebugReg.HALTSUM0)

    def elaborate(self, platform):
        m = Module()

        with m.If(self.dmcontrol.update):
            m.d.sync += self.dmcontrol.w.dmactive.eq(self.dmcontrol.r.dmactive)

            with m.If(self.dmcontrol.w.dmactive):
                m.d.sync += [
                    self.dmcontrol.w.ndmreset.eq(self.dmcontrol.r.ndmreset),
                    self.dmcontrol.w.hartsello.eq(self.dmcontrol.r.hartsello),
                    self.dmcontrol.w.hartreset.eq(self.dmcontrol.r.hartreset),
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

        m.d.comb += [
            self.dmstatus.w.version.eq(Version.V013),
            self.dmstatus.w.authenticated.eq(1),
            self.dmstatus.w.allrunning.eq(1),
            self.dmstatus.w.anyrunning.eq(1),
        ]

        return m


class DebugUnit(Elaboratable):

    def __init__(self):
        self.jtag = JTAGInterface()

    def elaborate(self, platform):
        m = Module()

        from jtagtap import JTAGTap
        tap = m.submodules.tap = JTAGTap({
            JTAGReg.IDCODE: [("value", 32)],
            JTAGReg.DTMCS: _dtmcs_layout,
            JTAGReg.DMI: _dmi_layout,
        })
        regfile = m.submodules.regfile = DebugRegisterFile(
            tap.regs[JTAGReg.DMI])
        m.submodules.controller = DebugController(regfile)

        m.d.comb += [
            tap.port.connect(self.jtag),
            tap.regs[JTAGReg.IDCODE].r.eq(0x913),
            tap.regs[JTAGReg.DTMCS].r.eq(0x71),
        ]

        return m
