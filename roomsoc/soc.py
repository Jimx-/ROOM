from amaranth import *
from amaranth.utils import log2_int
from amaranth_soc.memory import MemoryMap

from roomsoc.interconnect import wishbone


class SoCError(Exception):
    pass


class SoCRegion:

    def __init__(self, origin=None, size=None, mode='rw'):
        self.origin = origin
        self.size = size
        self.mode = mode


class BusHelper(Elaboratable):

    def __init__(self, data_width=32, addr_width=32):
        self.data_width = data_width
        self.addr_width = addr_width

        self.masters = dict()
        self.slaves = dict()
        self.regions = dict()
        self.converters = dict()

    def add_adapter(self, name, interface, direction='m2s'):

        def add_data_width_converter(interface, direction):
            if interface.data_width == self.data_width:
                return interface
            else:
                adapted_interface = wishbone.Interface(
                    data_width=self.data_width,
                    addr_width=self.get_addr_width(),
                    granularity=8,
                    name=f'{name}_adapted')
                if direction == 'm2s':
                    master, slave = interface, adapted_interface
                else:
                    master, slave = adapted_interface, interface

                self.converters[f'{name}_converter'] = wishbone.Converter(
                    master=master, slave=slave)
                return adapted_interface

        adapted_interface = add_data_width_converter(interface, direction)

        print(
            f'Bus {name} adapted from Wishbone {interface.data_width}-bit to Wishbone {self.data_width}-bit.'
        )

        return adapted_interface

    def add_region(self, name, region):
        self.regions[name] = region

    def add_master(self, name=None, master=None):
        if name is None:
            name = f'master{len(self.masters)}'
        master = self.add_adapter(name, master, 'm2s')
        self.masters[name] = master

        print(f'Add {name} as bus master.')

    def add_slave(self, name=None, slave=None, region=None):
        if name is None:
            name = f'slave{len(self.slaves)}'

        if region is None:
            region = self.regions.get(name, None)
            if region is None:
                raise SoCError()
        else:
            self.add_region(name, region)

        slave.memory_map = MemoryMap(data_width=8,
                                     addr_width=log2_int(region.size))
        slave = self.add_adapter(name, slave, 's2m')
        self.slaves[name] = slave

        print(f'Add {name} as bus slave.')

    def get_addr_width(self):
        return self.addr_width - Shape.cast(range(self.data_width // 8)).width

    def elaborate(self, platform):
        m = Module()

        for k, v in self.converters.items():
            setattr(m.submodules, k, v)

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

        self.bus.add_slave(name, ram_bus,
                           SoCRegion(origin=origin, size=size, mode=mode))

        self.peripherals[name] = ram

    def add_rom(self, name, origin, size, init=[], mode='r'):
        self.add_ram(name, origin, size, init, mode=mode)

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

            decoder = m.submodules.bus_decoder = wishbone.Decoder(
                data_width=self.bus.data_width,
                addr_width=self.bus.get_addr_width(),
                granularity=8)
            for name, slave in self.bus.slaves.items():
                region = self.bus.regions[name]
                decoder.add(slave, addr=region.origin)
            slave = decoder.bus

            m.d.comb += master.connect(slave)

        return m
