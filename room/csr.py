from enum import Enum
from collections import OrderedDict
import riscvmodel.csrnames as csrnames

from amaranth import *
from amaranth.utils import bits_for

from room.consts import *

__all__ = ["CSRAccess", "CSR", "CSRFile"]


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
                shape = shape, False
            width, _ = shape
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


misa_layout = [
    ("extensions", 26, CSRAccess.RW),
    ("zero", 4, CSRAccess.RO),
    ("mxl", 2, CSRAccess.RW),
]

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


class CSRFile(Elaboratable):

    def __init__(self, width=32, depth=2**12):
        self.width = width
        self.depth = depth
        self._csr_map = OrderedDict()
        self._ports = []

        self.misa = CSR(csrnames.misa, misa_layout)
        self.mstatus = CSR(csrnames.mstatus, mstatus_layout)
        self.mscratch = CSR(csrnames.mscratch, [('value', 32, CSRAccess.RW)])

        self.add_csrs([self.misa, self.mstatus, self.mscratch])

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

        isa_string = 'IMC'
        isa_ext = 0
        for c in isa_string:
            isa_ext |= 1 << (ord(c) - ord('A'))
        m.d.comb += [
            self.misa.r.extensions.eq(isa_ext),
            self.misa.r.mxl.eq(1),
        ]

        with m.If(self.mscratch.we):
            m.d.sync += self.mscratch.r.eq(self.mscratch.w)

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
                        for i in range(self.width):
                            rw = (1 << i) & csr.mask
                            m.d.comb += csr.w[i].eq(
                                w_data[i] if rw else csr.r[i])

        return m
