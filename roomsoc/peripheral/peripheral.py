from amaranth import *
from amaranth import tracer
from amaranth.utils import log2_int

from amaranth_soc import wishbone, csr
from amaranth_soc.memory import MemoryMap

from roomsoc.interconnect.csr import WishboneCSRBridge


class Peripheral:

    def __init__(self, name=None, src_loc_at=1):
        self.name = name or tracer.get_var_name(depth=2 +
                                                src_loc_at).lstrip("_")

        self._csr_banks = []
        self._windows = []

        self._bus = None

    @property
    def bus(self):
        if self._bus is None:
            raise NotImplementedError(
                "Peripheral {!r} does not have a bus interface".format(self))
        return self._bus

    @bus.setter
    def bus(self, bus):
        if not isinstance(bus, wishbone.Interface):
            raise TypeError(
                "Bus interface must be an instance of wishbone.Interface, not {!r}"
                .format(bus))
        self._bus = bus

    def csr_bank(self, *, addr=None, alignment=None):
        bank = CSRBank(prefix=self.name)
        self._csr_banks.append((bank, addr, alignment))
        return bank

    def window(self,
               *,
               addr_width,
               data_width,
               granularity=None,
               alignment=0,
               addr=None,
               sparse=None):
        window = wishbone.Interface(addr_width=addr_width,
                                    data_width=data_width,
                                    granularity=granularity)
        granularity_bits = log2_int(data_width // window.granularity)
        window.memory_map = MemoryMap(addr_width=addr_width + granularity_bits,
                                      data_width=window.granularity,
                                      alignment=alignment)
        self._windows.append((window, addr, sparse))
        return window

    def bridge(self, *, data_width=8, granularity=None, alignment=0):
        return PeripheralBridge(self,
                                data_width=data_width,
                                granularity=granularity,
                                alignment=alignment)

    def iter_csr_banks(self):
        for bank, addr, alignment in self._csr_banks:
            yield bank, addr, alignment

    def iter_windows(self):
        for window, addr, sparse in self._windows:
            yield window, addr, sparse


class CSRBank:

    def __init__(self, *, prefix=''):
        self.prefix = prefix
        self.csr_regs = []

    def csr(self,
            width,
            access,
            *,
            addr=None,
            alignment=None,
            name=None,
            src_loc_at=0):
        name = name or tracer.get_var_name(depth=2 + src_loc_at).lstrip("_")

        elem_name = f'{self.prefix}_{name}'
        elem = csr.Element(width, access, name=elem_name)
        self.csr_regs.append((elem, addr, alignment))
        return elem

    def iter_csr_regs(self):
        for elem, addr, alignment in self.csr_regs:
            yield elem, addr, alignment


class PeripheralBridge(Elaboratable):

    def __init__(self, periph, *, data_width, granularity, alignment):
        self.bus_decoder = wishbone.Decoder(addr_width=1,
                                            data_width=data_width,
                                            granularity=granularity,
                                            alignment=alignment)

        self.csr_subs = []

        for bank, bank_addr, bank_alignment in periph.iter_csr_banks():
            if bank_alignment is None:
                bank_alignment = alignment
            csr_mux = csr.Multiplexer(addr_width=1,
                                      data_width=8,
                                      alignment=bank_alignment)
            for elem, elem_addr, elem_alignment in bank.iter_csr_regs():
                if elem_alignment is None:
                    elem_alignment = alignment
                csr_mux.add(elem,
                            addr=elem_addr,
                            alignment=elem_alignment,
                            extend=True)

            csr_bridge = WishboneCSRBridge(csr_mux.bus, data_width=data_width)
            self.bus_decoder.add(csr_bridge.wb_bus,
                                 addr=bank_addr,
                                 extend=True)
            self.csr_subs.append((csr_mux, csr_bridge))

        for window, window_addr, window_sparse in periph.iter_windows():
            self.bus_decoder.add(window,
                                 addr=window_addr,
                                 sparse=window_sparse,
                                 extend=True)

        self.bus = self.bus_decoder.bus

    def elaborate(self, platform):
        m = Module()

        for i, (csr_mux, csr_bridge) in enumerate(self.csr_subs):
            m.submodules["csr_mux_{}".format(i)] = csr_mux
            m.submodules["csr_bridge_{}".format(i)] = csr_bridge

        m.submodules.bus_decoder = self.bus_decoder

        return m
