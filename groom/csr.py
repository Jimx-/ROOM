from enum import Enum
from collections import OrderedDict
import riscvmodel.csrnames as csrnames

from amaranth import *
from amaranth import tracer
from amaranth.utils import bits_for, log2_int

import groom.csrnames as gpucsrnames

from room.consts import *
from room.types import HasCoreParams

__all__ = [
    "CSRAccess", "CSR", "ThreadLocalCSR", "BankedCSR", "AutoCSR", "CSRFile"
]


class CSRAccess(Enum):
    RW = 0
    RO = 1


def _get_fields_from_description(description):
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

    return fields, mask


class CSR(Record):

    def __init__(self,
                 addr,
                 description,
                 params=None,
                 name=None,
                 src_loc_at=0):
        fields, self.mask = _get_fields_from_description(description)
        self.addr = addr

        super().__init__([
            ("r", fields),
            ("w", fields),
            ("re", 1),
            ("we", 1),
        ],
                         name=name,
                         src_loc_at=1 + src_loc_at)


class ThreadLocalCSR(HasCoreParams):

    def __init__(self, addr, description, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        fields, self.mask = _get_fields_from_description(description)
        self.addr = addr

        self.r = [
            Record(fields, name=f'{name}__r{i}') for i in range(self.n_threads)
        ]
        self.w = [
            Record(fields, name=f'{name}__w{i}') for i in range(self.n_threads)
        ]
        self.re = Signal(self.n_threads, name=f'{name}__re')
        self.we = Signal(self.n_threads, name=f'{name}__we')


class BankedCSR(HasCoreParams):

    def __init__(self,
                 cls,
                 addr,
                 description,
                 params,
                 name=None,
                 src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.addr = addr

        self.warps = [
            cls(addr=addr,
                description=description,
                params=params,
                name=f'{name}{i}') for i in range(self.n_warps)
        ]


class AutoCSR:

    def iter_csrs(self):
        for v in vars(self).values():
            if isinstance(v, CSR) or isinstance(
                    v, ThreadLocalCSR) or isinstance(v, BankedCSR):
                yield v
            elif hasattr(v, "iter_csrs"):
                yield from v.iter_csrs()


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

        self.mhartid = BankedCSR(CSR, csrnames.mhartid,
                                 [('value', self.width, CSRAccess.RO)], params)
        self.misa = CSR(csrnames.misa, misa_layout(self.width))

        self.mcycle = CSR(csrnames.mcycle,
                          [('value', self.width, CSRAccess.RW)])

        self.wtid = ThreadLocalCSR(gpucsrnames.wtid,
                                   [('value', self.width, CSRAccess.RO)],
                                   params)
        self.ltid = BankedCSR(ThreadLocalCSR, gpucsrnames.ltid,
                              [('value', self.width, CSRAccess.RO)], params)
        self.lwid = BankedCSR(CSR, gpucsrnames.lwid,
                              [('value', self.width, CSRAccess.RO)], params)

        self.gtid = BankedCSR(ThreadLocalCSR, gpucsrnames.gtid,
                              [('value', self.width, CSRAccess.RO)], params)
        self.gcid = CSR(gpucsrnames.gcid,
                        [('value', self.width, CSRAccess.RO)])

        self.mnt = CSR(gpucsrnames.mnt, [('value', self.width, CSRAccess.RO)])
        self.mnw = CSR(gpucsrnames.mnw, [('value', self.width, CSRAccess.RO)])
        self.mnc = CSR(gpucsrnames.mnc, [('value', self.width, CSRAccess.RO)])

        self.add_csrs([self.mhartid, self.misa])
        self.add_csrs([self.mcycle])
        self.add_csrs([self.wtid, self.ltid, self.lwid, self.gtid, self.gcid])
        self.add_csrs([self.mnt, self.mnw, self.mnc])

        if self.width == 32:
            self.mcycleh = CSR(csrnames.mcycleh,
                               [('value', self.width, CSRAccess.RW)])
            self.add_csrs([self.mcycleh])

    def add_csrs(self, csrs):
        for csr in csrs:
            if not isinstance(csr, CSR) and not isinstance(
                    csr, ThreadLocalCSR) and not isinstance(csr, BankedCSR):
                raise TypeError("Object {!r} is not a CSR".format(csr))
            if csr.addr in self._csr_map:
                raise ValueError(
                    "CSR address 0x{:x} has already been allocated".format(
                        csr.addr))
            self._csr_map[csr.addr] = csr

    def access_port(self):
        port = Record([
            ("wid", bits_for(self.n_warps)),
            ("tmask", self.n_threads),
            ("addr", bits_for(self.depth)),
            ("cmd", Shape.cast(CSRCommand).width),
            ("r_data", self.width * self.n_threads),
            ("w_data", self.width),
        ],
                      name=f"port{len(self._ports)}")
        self._ports.append(port)
        return port

    def elaborate(self, platform):
        m = Module()

        isa_string = 'IMX' + ('F' if self.use_fpu and self.flen >= 32 else
                              '') + ('D' if self.use_fpu and self.flen >= 64
                                     else '') + ('B' if self.use_zba
                                                 or self.use_zbb else '')
        isa_ext = 0
        for c in isa_string:
            isa_ext |= 1 << (ord(c) - ord('A'))
        m.d.comb += [
            self.misa.r.extensions.eq(isa_ext),
            self.misa.r.mxl.eq(log2_int(self.xlen) - 4),
        ]

        cycles = Signal(64)
        m.d.sync += cycles.eq(cycles + 1)
        m.d.comb += self.mcycle.r.eq(cycles[:self.width])
        if self.width == 32:
            m.d.comb += self.mcycleh.r.eq(cycles[32:])

        for t in range(self.n_threads):
            m.d.comb += self.wtid.r[t].eq(t)

        for w in range(self.n_warps):
            m.d.comb += [
                self.lwid.warps[w].r.eq(w),
                self.mhartid.warps[w].r.eq(
                    (self.core_id << log2_int(self.n_warps)) + w),
            ]

            for t in range(self.n_threads):
                m.d.comb += [
                    self.ltid.warps[w].r[t].eq(w * self.n_threads + t),
                    self.gtid.warps[w].r[t].eq(
                        (self.core_id * self.n_warps + w) * self.n_threads +
                        t),
                ]

        m.d.comb += [
            self.gcid.r.eq(self.core_id),
            self.mnt.r.eq(self.n_threads),
            self.mnw.r.eq(self.n_warps),
            self.mnc.r.eq(self.n_cores),
        ]

        for p in self._ports:
            r_data = Signal(self.width * self.n_threads)

            with m.Switch(p.addr):
                w_data = (
                    Mux(p.cmd[1], r_data, 0)
                    | Repl(p.w_data, self.n_threads)) & ~Mux(
                        p.cmd[0] & p.cmd[1], Repl(p.w_data, self.n_threads),
                        Const(0, self.width * self.n_threads))

                def rw_csr(csr):
                    if isinstance(csr, CSR):
                        m.d.comb += csr.re.eq(p.cmd[1])
                        for t in range(self.n_threads):
                            m.d.comb += r_data[t * self.width:(t + 1) *
                                               self.width].eq(csr.r)

                        m.d.comb += csr.we.eq(p.cmd[2])
                        for i in range(min(self.width, len(csr.w))):
                            rw = (1 << i) & csr.mask
                            m.d.comb += csr.w[i].eq(
                                w_data[i] if rw else csr.r[i])

                    elif isinstance(csr, ThreadLocalCSR):
                        for t in range(self.n_threads):
                            m.d.comb += [
                                csr.re[t].eq(p.cmd[1] & p.tmask[t]),
                                r_data[t * self.width:(t + 1) * self.width].eq(
                                    csr.r[t]),
                            ]

                            m.d.comb += csr.we[t].eq(p.cmd[2] & p.tmask[t])
                            for i in range(min(self.width, len(csr.w[t]))):
                                rw = (1 << i) & csr.mask
                                m.d.comb += csr.w[t][i].eq(
                                    w_data[t * self.width +
                                           i] if rw else csr.r[w][i])

                for addr, csr in self._csr_map.items():
                    with m.Case(addr):
                        if isinstance(csr, BankedCSR):
                            with m.Switch(p.wid):
                                for w in range(self.n_warps):
                                    with m.Case(w):
                                        rw_csr(csr.warps[w])

                        else:
                            rw_csr(csr)

            m.d.comb += p.r_data.eq(r_data)

        return m
