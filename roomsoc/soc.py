from amaranth import *
from amaranth.utils import log2_int
from amaranth_soc.memory import MemoryMap

from roomsoc.peripheral import Peripheral
from roomsoc.interconnect import wishbone, axi, ahb, apb, tilelink


class SoCRegion:

    def __init__(self, origin=None, size=None, mode='rw', cacheable=True):
        self.origin = origin
        self.size = size
        self.size_pow2 = 2**log2_int(size, False)
        self.mode = mode
        self.cacheable = cacheable

    def __str__(self):
        r = ''
        if self.origin is not None:
            r += f'Origin: 0x{self.origin:08x}, '
        if self.size is not None:
            r += f'Size: 0x{self.size:08x}, '
        r += f'Mode: {self.mode}, '
        r += f'Cacheable: {self.cacheable}'

        return r


class SoCIORegion(SoCRegion):
    pass


class BusHelper(Elaboratable):

    def __init__(self,
                 standard='wishbone',
                 data_width=32,
                 addr_width=32,
                 timeout=128):
        self.standard = standard
        self.data_width = data_width
        self.addr_width = addr_width
        self.timeout = timeout

        self.regions = dict()
        self.io_regions = dict()

        self.masters = dict()
        self.slaves = dict()
        self.converters = dict()

        self.bus_error = Signal()

    def check_regions_overlap(self, regions):
        i = 0
        while i < len(regions):
            n0 = list(regions.keys())[i]
            r0 = regions[n0]
            for n1 in list(regions.keys())[i + 1:]:
                r1 = regions[n1]
                if r0.origin >= (r1.origin + r1.size_pow2):
                    continue
                if r1.origin >= (r0.origin + r0.size_pow2):
                    continue
                return (n0, n1)
            i += 1
        return None

    def alloc_region(self, name, size, cacheable=True):
        if not cacheable:
            search_regions = self.io_regions
        else:
            search_regions = {
                'main': SoCRegion(origin=0, size=2**self.address_width - 1)
            }

        for _, search_region in search_regions.items():
            origin = search_region.origin
            while (origin + size) < (search_region.origin +
                                     search_region.size_pow2):
                if (origin % size):
                    origin += (origin - origin % size)
                    continue

                candidate = SoCRegion(origin=origin,
                                      size=size,
                                      cacheable=cacheable)
                overlap = False

                for _, allocated in self.regions.items():
                    if self.check_regions_overlap({
                            '0': allocated,
                            '1': candidate
                    }) is not None:
                        origin = max(allocated.origin + allocated.size,
                                     origin + size)
                        overlap = True
                        break
                if not overlap:
                    return candidate

        raise ValueError('Not enough address space to allocate region.')

    def add_region(self, name, region):
        if name in self.regions.keys() or name in self.io_regions.keys():
            raise ValueError(f'{name} already declared as a region')

        if isinstance(region, SoCIORegion):
            self.io_regions[name] = region
            overlap = self.check_regions_overlap(self.io_regions)
            if overlap is not None:
                raise ValueError(
                    f'IO region overlap between {overlap[0]} and {overlap[1]}')
        elif isinstance(region, SoCRegion):
            if region.origin is None:
                region = self.alloc_region(name, region.size, region.cacheable)
                self.regions[name] = region
            else:
                self.regions[name] = region

                overlap = self.check_regions_overlap(self.regions)
                if overlap is not None:
                    raise ValueError(
                        f'Region overlap between {overlap[0]} and {overlap[1]}'
                    )

    def add_adapter(self, name, interface, direction='m2s'):

        def add_bus_standard_converter(interface, direction):
            main_bus_cls = {
                'wishbone': wishbone.Interface,
                'axi-lite': axi.AXILiteInterface,
                'axi': axi.AXIInterface,
                'ahb': ahb.Interface,
                'tilelink': tilelink.Interface,
                'apb': apb.Interface,
            }[self.standard]

            if isinstance(interface, main_bus_cls):
                return interface

            if direction == 'm2s':
                master_cls, slave_cls = type(interface), main_bus_cls
            else:
                master_cls, slave_cls = main_bus_cls, type(interface)

            bridge_cls = {
                (axi.AXILiteInterface, wishbone.Interface):
                axi.AXILite2Wishbone,
                (axi.AXILiteInterface, axi.AXIInterface): axi.AXILite2AXI,
                (axi.AXIInterface, wishbone.Interface): axi.AXI2Wishbone,
                (ahb.Interface, wishbone.Interface): ahb.AHB2Wishbone,
                (apb.Interface, wishbone.Interface): apb.APB2Wishbone,
                (tilelink.Interface, wishbone.Interface):
                tilelink.TileLink2Wishbone,
                (tilelink.Interface, axi.AXIInterface): axi.TileLink2AXI,
                (axi.AXIInterface, tilelink.Interface): axi.AXI2Tilelink,
            }[master_cls, slave_cls]

            if hasattr(bridge_cls, 'get_adapted_interface'):
                adapted_interface = bridge_cls.get_adapted_interface(interface)
            else:
                main_bus_params = dict(data_width=self.data_width,
                                       name=f'{name}_bus_adapted')
                if self.standard == 'wishbone':
                    main_bus_params['granularity'] = 8

                if direction == 'm2s':
                    main_bus_params['addr_width'] = self.get_addr_width()
                else:
                    main_bus_params['addr_width'] = self.get_addr_width(
                        interface.addr_width +
                        log2_int(interface.data_width // 8))

                adapted_interface = main_bus_cls(**main_bus_params)

            if direction == 'm2s':
                master, slave = interface, adapted_interface
            else:
                master, slave = adapted_interface, interface
                master.memory_map = slave.memory_map

            self.converters[f'{name}_bridge'] = bridge_cls(master, slave)

            return adapted_interface

        def add_data_width_converter(interface, direction):
            if interface.data_width == self.data_width:
                return interface
            else:

                interface_cls = type(interface)
                converter_cls = {
                    wishbone.Interface: wishbone.Converter,
                    axi.AXILiteInterface: axi.AXILiteConverter,
                }[interface_cls]

                adapted_bus_params = dict(data_width=self.data_width,
                                          name=f'{name}_dw_adapted')
                if direction == 'm2s':
                    adapted_bus_params['addr_width'] = self.addr_width
                else:
                    adapted_bus_params[
                        'addr_width'] = interface.addr_width + log2_int(
                            interface.data_width // 8)

                if interface_cls is wishbone.Interface:
                    adapted_bus_params['granularity'] = 8
                    adapted_bus_params['addr_width'] -= log2_int(
                        self.data_width // 8)

                adapted_interface = interface_cls(**adapted_bus_params)

                if direction == 'm2s':
                    master, slave = interface, adapted_interface
                else:
                    master, slave = adapted_interface, interface
                    master.memory_map = slave.memory_map

                self.converters[f'{name}_dw_converter'] = converter_cls(
                    master=master, slave=slave)
                return adapted_interface

        adapted_interface = add_data_width_converter(interface, direction)
        adapted_interface = add_bus_standard_converter(adapted_interface,
                                                       direction)

        bus_names = {
            wishbone.Interface: "Wishbone",
            axi.AXILiteInterface: "AXI-Lite",
            axi.AXIInterface: "AXI",
            ahb.Interface: 'AHB',
            apb.Interface: 'APB',
            tilelink.Interface: 'TileLink',
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

    def add_slave(self, name=None, slave=None, region=None):
        if name is None and region is None:
            raise ValueError(
                'Either name or region should be provided for a bus slave.')

        if name is None:
            name = f'slave{len(self.slaves)}'

        if region is None:
            region = self.regions.get(name, None)
            if region is None:
                raise ValueError(f'Region {name} not found')
        else:
            self.add_region(name, region)
            region = self.regions[name]

        slave = self.add_adapter(name, slave, 's2m')
        self.slaves[name] = slave

        print(f'Add {name} as bus slave.')

    def get_addr_width(self, addr_width=None):
        if addr_width is None:
            addr_width = self.addr_width

        if self.standard == 'wishbone':
            return addr_width - Shape.cast(range(self.data_width // 8)).width
        return addr_width

    def elaborate(self, platform):
        m = Module()

        interconnect_p2p_cls = {
            'wishbone': wishbone.InterconnectP2P,
            'axi': axi.AXIInterconnectP2P,
        }[self.standard]
        interconnect_cls = {
            'wishbone': wishbone.InterconnectShared,
            'axi': axi.AXIInterconnectShared,
        }[self.standard]

        for k, v in self.converters.items():
            setattr(m.submodules, k, v)

        if len(self.masters) and len(self.slaves):
            if len(self.masters) == 1 and len(self.slaves) == 1:
                m.submodules.interconnect = interconnect_p2p_cls(
                    master=next(iter(self.masters.values())),
                    slave=next(iter(self.slaves.values())))
            else:
                interconnect = m.submodules.interconnect = interconnect_cls(
                    data_width=self.data_width,
                    addr_width=self.get_addr_width(),
                    masters=list(self.masters.values()),
                    slaves=[(self.regions[n], s)
                            for n, s in self.slaves.items()],
                    timeout_cycles=self.timeout)

                if hasattr(interconnect, 'timeout_error'):
                    m.d.comb += self.bus_error.eq(interconnect.timeout_error)

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

        self._bus_errors = bank.csr(32, 'r')
        self.bus_error = Signal()

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

        bus_errors = Signal(32)
        with m.If(self.bus_error):
            m.d.sync += bus_errors.eq(bus_errors + 1)
        m.d.comb += self._bus_errors.r_data.eq(bus_errors)

        return m


class SoC(Elaboratable):

    def __init__(self,
                 bus_standard='wishbone',
                 bus_data_width=32,
                 bus_addr_width=32,
                 bus_timeout=128):
        self.bus = BusHelper(standard=bus_standard,
                             data_width=bus_data_width,
                             addr_width=bus_addr_width,
                             timeout=bus_timeout)

        self.peripherals = dict()

    def add_cpu(self, cpu):
        self.cpu = cpu

        for n, (origin, size) in enumerate(self.cpu.io_regions.items()):
            self.bus.add_region(
                f'io{n}', SoCIORegion(origin=origin,
                                      size=size,
                                      cacheable=False))

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

        self.bus.add_slave(name, ram_bus,
                           SoCRegion(origin=origin, size=size, mode=mode))

        self.peripherals[name] = ram

    def add_rom(self, name, origin, size, init=[], mode='r'):
        self.add_ram(name, origin, size, init, mode=mode)

    def add_peripheral(self,
                       name,
                       periph,
                       *,
                       as_submodule=True,
                       origin=None,
                       cacheable=False):
        bus = getattr(periph, 'bus')
        self.bus.add_slave(
            name, bus,
            SoCRegion(origin=origin,
                      size=2**bus.memory_map.addr_width,
                      cacheable=cacheable))

        if as_submodule:
            self.peripherals[name] = periph

        return periph

    def add_controller(self, name='ctrl', **kwargs):
        self.ctrl = SoCController(name=name, **kwargs)
        self.add_peripheral(name, self.ctrl)

    def elaborate(self, platform):
        m = Module()

        m.submodules.bus = self.bus
        if hasattr(self, 'ctrl'):
            m.d.comb += self.ctrl.bus_error.eq(self.bus.bus_error)

        m.submodules.cpu = self.cpu

        for k, v in self.peripherals.items():
            setattr(m.submodules, k, v)

        return m

    def resources(self):
        for name, slave in self.bus.slaves.items():
            resources = slave.memory_map.all_resources()
            region = self.bus.regions[name]

            for res in resources:
                size = res.end - res.start
                yield res.resource, region.origin + res.start, size

    def generate_platform_header(self, macro_name='PLATFORM', file=None):

        def emit(s):
            print(s, file=file)

        emit('/* Auto-generated; do not edit. */')
        emit('\n')

        emit(f'#ifndef _{macro_name}_H_')
        emit(f'#define _{macro_name}_H_')
        emit('')

        emit('/*')
        emit(' * Peripherals')
        emit(' */')
        for resource, address, size in self.resources():
            name = resource.name

            emit(f'#define {name.upper()}_ADDRESS 0x{address:08x}UL')
            emit(f'#define {name.upper()}_SIZE {size}')
            emit('')

        emit('')
        emit('#endif')
