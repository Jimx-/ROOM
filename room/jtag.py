from amaranth import *
from amaranth.hdl.rec import *
from amaranth.sim import Settle

from enum import IntEnum

__all__ = ('JTAGInterface', 'JTAGReg', 'JTAGTap')

_jtag_layout = [
    ("tck", 1, DIR_FANIN),
    ("tdi", 1, DIR_FANIN),
    ("tdo", 1, DIR_FANOUT),
    ("tms", 1, DIR_FANIN),
    ("trst", 1, DIR_FANIN),
]


class JTAGReg(IntEnum):
    BYPASS = 0x00
    IDCODE = 0x01
    DTMCS = 0x10
    DMI = 0x11


class JTAGInterface(Record):

    def __init__(self, name=None):
        super().__init__(_jtag_layout, name=name)

    def tick(self):
        yield
        yield Settle()
        b = (yield self.tdo)
        return b

    def write_tms(self, tms):
        yield self.tms.eq(tms)
        yield from self.tick()

    def write_bits(self, bits, n, tms_last):
        for i in range(n):
            yield self.tdi.eq(bits & 1)
            bits >>= 1
            if i == n - 1:
                yield self.tms.eq(tms_last)
            yield from self.tick()
        yield self.tms.eq(0)

    def set_ir(self, ir_length, opcode):
        yield from self.write_tms(1)
        yield from self.write_tms(1)
        yield from self.write_tms(0)
        yield from self.write_tms(0)
        yield from self.write_bits(opcode, ir_length, 1)
        yield from self.write_tms(1)
        yield from self.write_tms(0)

    def shift_dr(self):
        yield from self.write_tms(1)
        yield from self.write_tms(0)
        yield from self.write_tms(0)

    def rw_bits(self, wdata, n, tms_last):
        rdata = 0

        for i in range(n):
            yield self.tdi.eq(wdata & 1)
            wdata >>= 1
            if i == n - 1:
                yield self.tms.eq(tms_last)
            rdata |= (yield from self.tick()) << i

        yield self.tms.eq(0)
        return rdata

    def update_dr(self, exit_1_dr):
        if exit_1_dr:
            yield from self.write_tms(1)
        yield from self.write_tms(1)
        yield from self.write_tms(0)

    def get_idcode(self):
        yield from self.set_ir(5, JTAGReg.IDCODE)
        yield from self.shift_dr()
        idcode = yield from self.rw_bits(0, 32, 0)
        yield from self.update_dr(True)

        return idcode

    def write_dtmcs(self, data):
        yield from self.set_ir(5, JTAGReg.DTMCS)
        yield from self.shift_dr()
        yield from self.write_bits(data, 32, 1)
        yield from self.update_dr(False)

    def read_dtmcs(self):
        yield from self.set_ir(5, JTAGReg.DTMCS)
        yield from self.shift_dr()
        dtmcs = yield from self.rw_bits(0, 32, 1)
        yield from self.update_dr(False)

        return dtmcs

    def write_dmi(self, addr, data):
        write_data = ((addr & 0x7f) << 34) | ((data & 0xffffffff) << 2) | 2
        yield from self.set_ir(5, JTAGReg.DMI)
        yield from self.shift_dr()
        yield from self.write_bits(write_data, 41, 1)
        yield from self.update_dr(False)

    def read_dmi(self, addr, wait_cycles=10):
        write_data = ((addr & 0x7f) << 34) | 1
        print(''.join(list(reversed(bin(write_data)))))
        yield from self.set_ir(5, JTAGReg.DMI)
        yield from self.shift_dr()
        yield from self.write_bits(write_data, 41, 1)
        yield from self.update_dr(False)

        for _ in range(wait_cycles):
            yield from self.tick()

        write_data = ((addr & 0x7f) << 34)
        print(''.join(list(reversed(bin(write_data)))))
        yield from self.shift_dr()
        data_out = yield from self.rw_bits(write_data, 41, 1)
        yield from self.update_dr(False)
        return (data_out >> 2) & 0xffffffff


class _JTAGRegPortLayout(Layout):

    def __init__(self, fields):

        def fanout(fields):
            r = []
            for f in fields:
                if isinstance(f[1], (int, tuple)):
                    r.append((f[0], f[1], DIR_FANOUT))
                else:
                    r.append((f[0], fanout(f[1])))
            return r

        full_fields = [("r", fanout(fields)), ("w", fanout(fields)),
                       ("update", 1, DIR_FANOUT), ("capture", 1, DIR_FANOUT),
                       ("reset", 1, DIR_FANOUT)]
        super().__init__(full_fields)


class JTAGTap(Elaboratable):

    def __init__(self, reg_map, ir_width=5, ir_reset=0x01):
        self.port = JTAGInterface()
        self.regs = {
            a: Record(_JTAGRegPortLayout(f))
            for a, f in reg_map.items()
        }
        self.ir = Signal(ir_width, reset=ir_reset)
        self.dr = Signal(max(len(port.r) for port in self.regs.values()))

    def elaborate(self, platform):
        m = Module()

        tms = self.port.tms
        tdi = self.port.tdi

        with m.FSM():
            with m.State("TEST-LOGIC-RESET"):
                m.d.comb += (port.reset.eq(1) for port in self.regs.values())
                m.d.sync += self.ir.eq(self.ir.reset)
                with m.If(tms):
                    m.next = "TEST-LOGIC-RESET"
                with m.Else():
                    m.next = "RUN-TEST-IDLE"

            with m.State("RUN-TEST-IDLE"):
                with m.If(tms):
                    m.next = "SELECT-DR-SCAN"

            with m.State("SELECT-DR-SCAN"):
                with m.If(tms):
                    m.next = "SELECT-IR-SCAN"
                with m.Else():
                    m.next = "CAPTURE-DR"

            with m.State("CAPTURE-DR"):
                with m.Switch(self.ir):
                    for addr, port in self.regs.items():
                        with m.Case(addr):
                            m.d.sync += self.dr.eq(port.r)
                            m.d.comb += port.capture.eq(1)
                with m.If(tms):
                    m.next = "EXIT1-DR"
                with m.Else():
                    m.next = "SHIFT-DR"

            with m.State("SHIFT-DR"):
                m.d.sync += self.port.tdo.eq(self.dr[0])
                with m.Switch(self.ir):
                    for addr, port in self.regs.items():
                        with m.Case(addr):
                            m.d.sync += self.dr.eq(
                                Cat(self.dr[1:len(port.r)], tdi))
                with m.If(tms):
                    m.next = "EXIT1-DR"

            with m.State("EXIT1-DR"):
                m.d.sync += self.port.tdo.eq(0)
                with m.If(tms):
                    m.next = "UPDATE-DR"
                with m.Else():
                    m.next = "PAUSE-DR"

            with m.State("PAUSE-DR"):
                with m.If(tms):
                    m.next = "EXIT2-DR"

            with m.State("EXIT2-DR"):
                with m.If(tms):
                    m.next = "UPDATE-DR"
                with m.Else():
                    m.next = "SHIFT-DR"

            with m.State("UPDATE-DR"):
                with m.Switch(self.ir):
                    for addr, port in self.regs.items():
                        with m.Case(addr):
                            m.d.comb += [
                                port.w.eq(self.dr),
                                port.update.eq(1),
                            ]
                with m.If(tms):
                    m.next = "SELECT-DR-SCAN"
                with m.Else():
                    m.next = "RUN-TEST-IDLE"

            with m.State("SELECT-IR-SCAN"):
                with m.If(tms):
                    m.next = "TEST-LOGIC-RESET"
                with m.Else():
                    m.next = "CAPTURE-IR"

            with m.State("CAPTURE-IR"):
                with m.If(tms):
                    m.next = "EXIT1-IR"
                with m.Else():
                    m.next = "SHIFT-IR"

            with m.State("SHIFT-IR"):
                m.d.sync += self.port.tdo.eq(self.ir[0])
                m.d.sync += self.ir.eq(Cat(self.ir[1:], tdi))
                with m.If(tms):
                    m.next = "EXIT1-IR"

            with m.State("EXIT1-IR"):
                m.d.sync += self.port.tdo.eq(0)
                with m.If(tms):
                    m.next = "UPDATE-IR"
                with m.Else():
                    m.next = "PAUSE-IR"

            with m.State("PAUSE-IR"):
                with m.If(tms):
                    m.next = "EXIT2-IR"

            with m.State("EXIT2-IR"):
                with m.If(tms):
                    m.next = "UPDATE-IR"
                with m.Else():
                    m.next = "SHIFT-IR"

            with m.State("UPDATE-IR"):
                with m.If(tms):
                    m.next = "SELECT-DR-SCAN"
                with m.Else():
                    m.next = "RUN-TEST-IDLE"

        return m
