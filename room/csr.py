from enum import Enum
from collections import OrderedDict
import riscvmodel.csrnames as csrnames

from amaranth import *
from amaranth.utils import bits_for, log2_int

from room.consts import *
from room.types import HasCoreParams

__all__ = ["CSRAccess", "CSR", "AutoCSR", "CSRRecord", "CSRFile"]


class CSRAccess(Enum):
    RW = 0
    RO = 1


class CSR(Record):

    def __init__(self, addr, description, name=None, src_loc_at=0):
        fields = []
        mask = 0
        offset = 0
        for fname, shape, access in description:
            if isinstance(shape, int):
                shape = unsigned(shape)
            width = shape.width
            fields.append((fname, shape))
            if access is CSRAccess.RW:
                mask |= ((1 << width) - 1) << offset
            offset += width

        self.addr = addr
        self.mask = mask

        super().__init__([
            ("r", fields),
            ("w", fields),
            ("re", 1),
            ("we", 1),
        ],
                         name=name,
                         src_loc_at=1 + src_loc_at)


class AutoCSR:

    def iter_csrs(self):
        for v in vars(self).values():
            if isinstance(v, CSR):
                yield v
            elif hasattr(v, "iter_csrs"):
                yield from v.iter_csrs()


class CSRRecord(Record):

    def __init__(self, csr_layout, name=None, src_loc_at=0):
        fields = []
        for fname, shape, _ in csr_layout:
            if isinstance(shape, int):
                shape = unsigned(shape)
            fields.append((fname, shape))

        super().__init__(fields, name=name, src_loc_at=1 + src_loc_at)


def misa_layout(xlen):
    return [
        ("extensions", 26, CSRAccess.RW),
        ("zero", xlen - 28, CSRAccess.RO),
        ("mxl", 2, CSRAccess.RW),
    ]


class CSRFile(HasCoreParams, Elaboratable):

    def __init__(self, params, width=32, depth=2**12):
        super().__init__(params)

        self.width = width
        self.depth = depth
        self._csr_map = OrderedDict()
        self._ports = []

        self.mhartid = CSR(csrnames.mhartid,
                           [('value', self.width, CSRAccess.RO)])
        self.misa = CSR(csrnames.misa, misa_layout(self.width))
        self.mscratch = CSR(csrnames.mscratch,
                            [('value', self.width, CSRAccess.RW)])

        self.mcycle = CSR(csrnames.mcycle,
                          [('value', self.width, CSRAccess.RW)])

        self.add_csrs([self.mhartid, self.misa, self.mscratch])
        self.add_csrs([self.mcycle])

        if self.width == 32:
            self.mcycleh = CSR(csrnames.mcycleh,
                               [('value', self.width, CSRAccess.RW)])
            self.add_csrs([self.mcycleh])

    def add_csrs(self, csrs):
        for csr in csrs:
            if not isinstance(csr, CSR):
                raise TypeError("Object {!r} is not a CSR".format(csr))
            if csr.addr in self._csr_map:
                raise ValueError(
                    "CSR address 0x{:x} has already been allocated".format(
                        csr.addr))
            self._csr_map[csr.addr] = csr

    def access_port(self):
        port = Record([
            ("addr", bits_for(self.depth)),
            ("cmd", Shape.cast(CSRCommand).width),
            ("r_data", self.width),
            ("w_data", self.width),
        ],
                      name=f"port{len(self._ports)}")
        self._ports.append(port)
        return port

    def elaborate(self, platform):
        m = Module()

        isa_string = 'IMC' + ('F' if self.use_fpu and self.flen >= 32 else ''
                              ) + ('D' if self.use_fpu and self.flen >= 64 else
                                   '') + ('S' if self.use_supervisor else
                                          '') + ('U' if self.use_user else '')

        isa_ext = 0
        for c in isa_string:
            isa_ext |= 1 << (ord(c) - ord('A'))
        m.d.comb += [
            self.misa.r.extensions.eq(isa_ext),
            self.misa.r.mxl.eq(log2_int(self.xlen) - 4),
        ]

        with m.If(self.mscratch.we):
            m.d.sync += self.mscratch.r.eq(self.mscratch.w)

        cycles = Signal(64)
        m.d.sync += cycles.eq(cycles + 1)
        m.d.comb += self.mcycle.r.eq(cycles[:self.width])
        if self.width == 32:
            m.d.comb += self.mcycleh.r.eq(cycles[32:])

        for p in self._ports:
            with m.Switch(p.addr):
                w_data = (Mux(p.cmd[1], p.r_data, 0)
                          | p.w_data) & ~Mux(p.cmd[0] & p.cmd[1], p.w_data, 0)

                for addr, csr in self._csr_map.items():
                    with m.Case(addr):
                        m.d.comb += [
                            csr.re.eq(p.cmd[1]),
                            p.r_data.eq(csr.r),
                        ]

                        m.d.comb += csr.we.eq(p.cmd[2])
                        for i in range(min(self.width, len(csr.w))):
                            rw = (1 << i) & csr.mask
                            m.d.comb += csr.w[i].eq(
                                w_data[i] if rw else csr.r[i])

        return m
