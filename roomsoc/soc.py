from amaranth import *
from amaranth.utils import log2_int
from amaranth_soc.memory import MemoryMap

from roomsoc.peripheral import Peripheral
from roomsoc.interconnect import wishbone, axi


class BusHelper(Elaboratable):

    def __init__(self, standard='wishbone', data_width=32, addr_width=32):
        self.standard = standard
        self.data_width = data_width
        self.addr_width = addr_width

        self.masters = dict()
        self.slaves = dict()
        self.converters = dict()

        self.decoder = wishbone.Decoder(data_width=self.data_width,
                                        addr_width=self.get_addr_width(),
                                        granularity=8)

    def add_adapter(self, name, interface, direction='m2s'):

        def add_bus_standard_converter(interface, direction):
            main_bus_cls = {
                'wishbone': wishbone.Interface,
                'axi-lite': axi.AXILiteInterface,
            }[self.standard]

            if isinstance(interface, main_bus_cls):
                return interface

            adapted_interface = main_bus_cls(data_width=self.data_width,
                                             addr_width=self.get_addr_width(),
                                             granularity=8,
                                             name=f'{name}_bus_adapted')

            if direction == 'm2s':
                master, slave = interface, adapted_interface
            else:
                master, slave = adapted_interface, interface

            bridge_cls = {
                (axi.AXILiteInterface, wishbone.Interface):
                axi.AXILite2Wishbone,
            }[type(master), type(slave)]
            self.converters[f'{name}_bridge'] = bridge_cls(master, slave)

            return adapted_interface

        def add_data_width_converter(interface, direction):
            if interface.data_width == self.data_width:
                return interface
            else:
                adapted_interface = wishbone.Interface(
                    data_width=self.data_width,
                    addr_width=self.get_addr_width(),
                    granularity=8,
                    name=f'{name}_dw_adapted')
                if direction == 'm2s':
                    master, slave = interface, adapted_interface
                else:
                    master, slave = adapted_interface, interface

                self.converters[f'{name}_dw_converter'] = wishbone.Converter(
                    master=master, slave=slave)
                return adapted_interface

        adapted_interface = add_data_width_converter(interface, direction)
        adapted_interface = add_bus_standard_converter(adapted_interface,
                                                       direction)

        bus_names = {
            wishbone.Interface: "Wishbone",
            axi.AXILiteInterface: "AXI-Lite",
        }
        print(
            f'Bus {name} adapted from {bus_names[type(interface)]} {interface.data_width}-bit to {bus_names[type(adapted_interface)]} {self.data_width}-bit.'
        )

        return adapted_interface

    def add_master(self, name=None, master=None):
        if name is None:
            name = f'master{len(self.masters)}'
        master = self.add_adapter(name, master, 'm2s')
        self.masters[name] = master

        print(f'Add {name} as bus master.')

    def add_slave(self, name=None, slave=None, **kwargs):
        if name is None:
            name = f'slave{len(self.slaves)}'

        slave = self.add_adapter(name, slave, 's2m')
        self.slaves[name] = slave
        self.decoder.add(slave, **kwargs)

        print(f'Add {name} as bus slave.')

    def get_addr_width(self):
        return self.addr_width - Shape.cast(range(self.data_width // 8)).width

    def elaborate(self, platform):
        m = Module()

        m.submodules.decoder = self.decoder

        for k, v in self.converters.items():
            setattr(m.submodules, k, v)

        return m


class SoCController(Peripheral, Elaboratable):

    def __init__(self, name=None, with_reset=True, with_scratch=True):
        super().__init__(name=name)

        self.with_reset = with_reset
        self.with_scratch = with_scratch

        self.soc_reset = Signal()
        self.cpu_reset = Signal()

        bank = self.csr_bank()
        if with_reset:
            self._reset = bank.csr(2, 'w')

        if with_scratch:
            self._scratch = bank.csr(32, 'r')

        self._bridge = self.bridge(data_width=32, granularity=8, alignment=2)
        self.bus = self._bridge.bus

    def elaborate(self, platform):
        m = Module()
        m.submodules.bridge = self._bridge

        if self.with_reset:
            with m.If(self._reset.w_stb):
                m.d.sync += Cat(self.soc_reset,
                                self.cpu_reset).eq(self._reset.w_data)
            with m.Else():
                m.d.sync += self.soc_reset.eq(0)

        if self.with_scratch:
            m.d.comb += self._scratch.r_data.eq(0x12345678)

        return m


class SoC(Elaboratable):

    def __init__(self, bus_data_width=32, bus_addr_width=32):
        self.bus = BusHelper(data_width=bus_data_width,
                             addr_width=bus_addr_width)

        self.peripherals = dict()

    def add_cpu(self, cpu):
        self.cpu = cpu

        for n, cpu_bus in enumerate(self.cpu.periph_buses):
            self.bus.add_master(name=f'cpu_bus{n}', master=cpu_bus)

        if self.peripherals.get('ctrl', None) is not None:
            self.cpu = ResetInserter(self.peripherals['ctrl'].cpu_reset)(
                self.cpu)

    def add_ram(self, name, origin, size, init=[], mode='rw'):
        ram_bus = wishbone.Interface(data_width=self.bus.data_width,
                                     addr_width=log2_int(
                                         size // (self.bus.data_width >> 3)),
                                     granularity=8,
                                     name=f'{name}_bus')
        ram = wishbone.SRAM(Memory(width=self.bus.data_width,
                                   depth=size // (self.bus.data_width >> 3),
                                   init=init),
                            read_only=(mode == 'r'),
                            bus=ram_bus)

        ram_bus.memory_map = MemoryMap(data_width=8, addr_width=log2_int(size))

        self.bus.add_slave(name, ram_bus, addr=origin)

        self.peripherals[name] = ram

    def add_rom(self, name, origin, size, init=[], mode='r'):
        self.add_ram(name, origin, size, init, mode=mode)

    def add_peripheral(self, name, periph, *, as_submodule=True, **kwargs):
        bus = getattr(periph, 'bus')
        self.bus.add_slave(name, bus, **kwargs)

        if as_submodule:
            self.peripherals[name] = periph

        return periph

    def add_controller(self, name='ctrl', **kwargs):
        self.add_peripheral(name, SoCController(name=name, **kwargs))

    def elaborate(self, platform):
        m = Module()

        m.submodules.bus = self.bus

        m.submodules.cpu = self.cpu

        for k, v in self.peripherals.items():
            setattr(m.submodules, k, v)

        if len(self.bus.masters) and len(self.bus.slaves):
            if len(self.bus.masters) > 1:
                arbiter = m.submodules.bus_arbiter = wishbone.Arbiter(
                    data_width=self.bus.data_width,
                    addr_width=self.bus.get_addr_width(),
                    granularity=8)
                for master in self.bus.masters.values():
                    arbiter.add(master)
                master = arbiter.bus
            else:
                master = list(self.bus.masters.values())[0]

            slave = self.bus.decoder.bus

            m.d.comb += master.connect(slave)

        return m

    def resources(self):
        memory_map = self.bus.decoder.bus.memory_map

        for peripheral, (peripheral_start, _, _) in memory_map.windows():
            resources = peripheral.all_resources()

            for res in resources:

                size = res.end - res.start
                yield res.resource, peripheral_start + res.start, size
