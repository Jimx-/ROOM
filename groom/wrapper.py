from amaranth import *

from groom.core import Core
from groom.ctrl import GroomController
from groom.raster import RasterUnit

from roomsoc.interconnect import tilelink as tl
from roomsoc.interconnect.stream import Decoupled
from roomsoc.peripheral.l2cache import L2Cache


class HasClusterParams:

    def __init__(self, params, *args, **kwargs):
        self.params = params

        self.num_cores = params['n_cores_per_cluster']
        self.num_clusters = params['n_clusters']
        self.core_bits = Shape.cast(range(self.num_cores)).width
        self.cluster_bits = Shape.cast(range(self.num_clusters)).width

        self.core_params = params['core_params']
        self.l2cache_params = params['l2cache_params']
        self.l3cache_params = params['l3cache_params']

        self.io_regions = params['io_regions']

        self.raster_params = params.get('raster_params', None)


class Cluster(HasClusterParams, Elaboratable):

    def __init__(self, cluster_id, params):
        super().__init__(params)

        self.cluster_id = cluster_id

        self.reset_vector = Signal(32)
        self.core_enable = Signal()
        self.cache_enable = Signal()
        self.raster_enable = Signal()

        self.busy = Signal()

        self.raster_tile_count = Signal(16)
        self.raster_tile_addr = Signal(32)
        self.raster_prim_addr = Signal(32)
        self.raster_prim_stride = Signal(16)

        self.dbus_mmio = tl.Interface(data_width=64,
                                      addr_width=32,
                                      size_width=3,
                                      source_id_width=5,
                                      name='dbus_mmio')

        self.l2cache_params['client_source_map'] = dict(
            (i, (self.make_source(True, i, Const(0, 8)),
                 self.make_source(True, i, Const(~0, 8))))
            for i in range(self.num_cores))
        l2cache = L2Cache(self.l2cache_params)
        self.l2cache = ResetInserter(~self.cache_enable)(l2cache)

        self.periph_buses = [self.dbus_mmio, self.l2cache.out_bus]

    @property
    def pma_regions(self):
        return self.params.get('pma_regions')

    @pma_regions.setter
    def pma_regions(self, value):
        self.params['pma_regions'] = value

    def make_source(self, is_dbus, core_id, source):
        return Cat(source[:-(1 + self.core_bits)],
                   Const(core_id, self.core_bits), Const(int(not is_dbus), 1))

    def unpack_source(self, source):
        return ~source[-1], source[-(1 + self.core_bits):-1], source[:-(
            1 + self.core_bits)]

    def elaborate(self, platform):
        m = Module()

        m.submodules.l2cache = self.l2cache

        cores = []

        a_arbiter = m.submodules.a_arbiter = tl.Arbiter(
            tl.ChannelA,
            data_width=64,
            addr_width=32,
            size_width=3,
            source_id_width=self.l2cache.in_source_id_width)
        mmio_a_arbiter = m.submodules.mmio_a_arbiter = tl.Arbiter(
            tl.ChannelA,
            data_width=64,
            addr_width=32,
            size_width=3,
            source_id_width=5)

        c_arbiter = m.submodules.c_arbiter = tl.Arbiter(
            tl.ChannelC,
            data_width=64,
            addr_width=32,
            size_width=3,
            source_id_width=self.l2cache.in_source_id_width)

        e_arbiter = m.submodules.e_arbiter = tl.Arbiter(
            tl.ChannelE, sink_id_width=self.l2cache.in_sink_id_width)

        for i in range(self.num_cores):
            core_params = self.core_params.copy()
            core_params['core_id'] = self.num_cores * self.cluster_id + i
            core_params['pma_regions'] = self.pma_regions

            m.domains += ClockDomain(f'core{i}', local=True)
            m.d.comb += [
                ClockSignal(f'core{i}').eq(ClockSignal()),
                ResetSignal(f'core{i}').eq(ResetSignal() | ~self.core_enable),
            ]

            core = DomainRenamer(f'core{i}')(Core(core_params))
            setattr(m.submodules, f'core{i}', core)
            cores.append(core)

            m.d.comb += [
                core.reset_vector.eq(self.reset_vector),
                core.cache_enable.eq(self.cache_enable),
            ]

            ibus_a = Decoupled(tl.ChannelA,
                               data_width=core.ibus.data_width,
                               addr_width=core.ibus.addr_width,
                               size_width=core.ibus.size_width,
                               source_id_width=core.ibus.source_id_width)
            dbus_a = Decoupled(tl.ChannelA,
                               data_width=core.dbus.data_width,
                               addr_width=core.dbus.addr_width,
                               size_width=core.dbus.size_width,
                               source_id_width=core.dbus.source_id_width)
            dbus_mmio_a = Decoupled(
                tl.ChannelA,
                data_width=core.dbus_mmio.data_width,
                addr_width=core.dbus_mmio.addr_width,
                size_width=core.dbus_mmio.size_width,
                source_id_width=core.dbus_mmio.source_id_width)
            m.d.comb += [
                core.ibus.a.connect(ibus_a),
                ibus_a.bits.source.eq(
                    self.make_source(is_dbus=False,
                                     core_id=i,
                                     source=core.ibus.a.bits.source)),
                core.dbus.a.connect(dbus_a),
                dbus_a.bits.source.eq(
                    self.make_source(is_dbus=True,
                                     core_id=i,
                                     source=core.dbus.a.bits.source)),
                core.dbus_mmio.a.connect(dbus_mmio_a),
                dbus_mmio_a.bits.source.eq(
                    self.make_source(is_dbus=True,
                                     core_id=i,
                                     source=core.dbus_mmio.a.bits.
                                     source[:self.dbus_mmio.source_id_width])),
            ]

            a_arbiter.add(ibus_a)
            a_arbiter.add(dbus_a)
            mmio_a_arbiter.add(dbus_mmio_a)

            dbus_c = Decoupled(tl.ChannelC,
                               data_width=core.dbus.data_width,
                               addr_width=core.dbus.addr_width,
                               size_width=core.dbus.size_width,
                               source_id_width=core.dbus.source_id_width)
            m.d.comb += [
                core.dbus.c.connect(dbus_c),
                dbus_c.bits.source.eq(
                    self.make_source(is_dbus=True,
                                     core_id=i,
                                     source=core.dbus.c.bits.source)),
            ]

            c_arbiter.add(dbus_c)

            e_arbiter.add(core.dbus.e)

        if self.raster_params is not None:
            m.domains += ClockDomain(f'raster', local=True)
            m.d.comb += [
                ClockSignal(f'raster').eq(ClockSignal()),
                ResetSignal(f'raster').eq(ResetSignal() | ~self.raster_enable),
            ]

            raster_unit = m.submodules.raster_unit = DomainRenamer('raster')(
                RasterUnit(self.num_cores, self.raster_params))
            m.d.comb += [
                raster_unit.tile_count.eq(self.raster_tile_count),
                raster_unit.tilebuf_addr.eq(self.raster_tile_addr),
                raster_unit.prim_addr.eq(self.raster_prim_addr),
                raster_unit.prim_stride.eq(self.raster_prim_stride),
            ]

            mem_bus_a = Decoupled(
                tl.ChannelA,
                data_width=raster_unit.mem_bus.data_width,
                addr_width=raster_unit.mem_bus.addr_width,
                size_width=raster_unit.mem_bus.size_width,
                source_id_width=self.l2cache.in_source_id_width)

            m.d.comb += [
                raster_unit.mem_bus.a.connect(mem_bus_a),
                mem_bus_a.bits.source.eq(~0),
            ]

            a_arbiter.add(mem_bus_a)

            for rr, core in zip(raster_unit.req, cores):
                m.d.comb += core.raster_req.connect(rr)

        m.d.comb += [
            a_arbiter.bus.connect(self.l2cache.in_bus.a),
            mmio_a_arbiter.bus.connect(self.dbus_mmio.a),
            c_arbiter.bus.connect(self.l2cache.in_bus.c),
            e_arbiter.bus.connect(self.l2cache.in_bus.e),
        ]

        _, b_core_id, b_source_id = self.unpack_source(
            self.l2cache.in_bus.b.bits.source)

        with m.Switch(b_core_id):
            for i, core in enumerate(cores):
                with m.Case(i):
                    m.d.comb += [
                        self.l2cache.in_bus.b.connect(core.dbus.b),
                        core.dbus.b.bits.source.eq(b_source_id),
                    ]

        with m.If(~self.l2cache.in_bus.d.bits.source.all()):
            d_is_dbus, d_core_id, d_source_id = self.unpack_source(
                self.l2cache.in_bus.d.bits.source)

            with m.Switch(d_core_id):
                for i, core in enumerate(cores):
                    with m.Case(i):
                        with m.If(d_is_dbus):
                            m.d.comb += [
                                self.l2cache.in_bus.d.connect(core.dbus.d),
                                core.dbus.d.bits.source.eq(d_source_id),
                            ]

                        with m.Else():
                            m.d.comb += [
                                self.l2cache.in_bus.d.connect(core.ibus.d),
                                core.ibus.d.bits.source.eq(d_source_id),
                            ]

        if self.raster_params is not None:
            with m.If(self.l2cache.in_bus.d.bits.source.all()):
                m.d.comb += self.l2cache.in_bus.d.connect(
                    raster_unit.mem_bus.d)

        _, mmio_d_core_id, mmio_d_source_id = self.unpack_source(
            self.dbus_mmio.d.bits.source)

        with m.Switch(mmio_d_core_id):
            for i, core in enumerate(cores):
                with m.Case(i):
                    m.d.comb += [
                        self.dbus_mmio.d.connect(core.dbus_mmio.d),
                        core.dbus_mmio.d.bits.source.eq(mmio_d_source_id),
                    ]

        m.d.comb += self.busy.eq(Cat(c.busy for c in cores).any())

        return m


class GroomWrapper(HasClusterParams, Elaboratable):

    def __init__(self, params, bus_master=None):
        super().__init__(params)
        self.bus_master = bus_master

        if bus_master is not None:
            self.cluster_bits = Shape.cast(range(self.num_clusters + 1)).width

        self.reset_vector = Signal(32)
        self.busy = Signal()

        self.ctrl = GroomController()

        self.dbus_mmio = tl.Interface(data_width=64,
                                      addr_width=32,
                                      size_width=3,
                                      source_id_width=5 + self.cluster_bits,
                                      name='dbus_mmio')

        self.l3cache_params['client_source_map'] = dict(
            (i, (self.make_source(i, Const(0, 8)),
                 self.make_source(i, Const(~0, 8))))
            for i in range(self.num_clusters))
        l3cache = L2Cache(self.l3cache_params)
        self.l3cache = ResetInserter(~self.ctrl.cache_enable)(l3cache)

        self.periph_buses = [self.dbus_mmio, self.l3cache.out_bus]

    @property
    def pma_regions(self):
        return self.params.get('pma_regions')

    @pma_regions.setter
    def pma_regions(self, value):
        self.params['pma_regions'] = value

    def make_source(self, cluster_id, source):
        return Cat(source[:-self.cluster_bits],
                   Const(cluster_id, self.cluster_bits))

    def unpack_source(self, source):
        return source[-self.cluster_bits:], source[:-self.cluster_bits]

    def elaborate(self, platform):
        m = Module()

        m.submodules.l3cache = self.l3cache
        m.submodules.ctrl = self.ctrl

        clusters = []

        a_arbiter = m.submodules.a_arbiter = tl.Arbiter(
            tl.ChannelA,
            data_width=64,
            addr_width=32,
            size_width=3,
            source_id_width=self.l3cache.in_source_id_width)
        mmio_a_arbiter = m.submodules.mmio_a_arbiter = tl.Arbiter(
            tl.ChannelA,
            data_width=64,
            addr_width=32,
            size_width=3,
            source_id_width=5 + self.cluster_bits)

        c_arbiter = m.submodules.c_arbiter = tl.Arbiter(
            tl.ChannelC,
            data_width=64,
            addr_width=32,
            size_width=3,
            source_id_width=self.l3cache.in_source_id_width)

        e_arbiter = m.submodules.e_arbiter = tl.Arbiter(
            tl.ChannelE, sink_id_width=self.l3cache.in_sink_id_width)

        for i in range(self.num_clusters):
            cluster_params = self.params.copy()

            cluster = Cluster(i, cluster_params)
            setattr(m.submodules, f'cluster{i}', cluster)
            clusters.append(cluster)

            m.d.comb += [
                cluster.reset_vector.eq(self.reset_vector),
                cluster.core_enable.eq(self.ctrl.core_enable),
                cluster.cache_enable.eq(self.ctrl.cache_enable),
                cluster.raster_enable.eq(self.ctrl.raster_enable),
                cluster.raster_tile_count.eq(self.ctrl.raster_tile_count),
                cluster.raster_tile_addr.eq(self.ctrl.raster_tile_addr),
                cluster.raster_prim_addr.eq(self.ctrl.raster_prim_addr),
                cluster.raster_prim_stride.eq(self.ctrl.raster_prim_stride),
            ]

            dbus_a = Decoupled(
                tl.ChannelA,
                data_width=cluster.l2cache.out_bus.data_width,
                addr_width=cluster.l2cache.out_bus.addr_width,
                size_width=cluster.l2cache.out_bus.size_width,
                source_id_width=cluster.l2cache.out_bus.source_id_width)
            dbus_mmio_a = Decoupled(
                tl.ChannelA,
                data_width=cluster.dbus_mmio.data_width,
                addr_width=cluster.dbus_mmio.addr_width,
                size_width=cluster.dbus_mmio.size_width,
                source_id_width=cluster.dbus_mmio.source_id_width +
                self.cluster_bits)
            m.d.comb += [
                cluster.l2cache.out_bus.a.connect(dbus_a),
                dbus_a.bits.source.eq(
                    self.make_source(
                        cluster_id=i,
                        source=cluster.l2cache.out_bus.a.bits.source)),
                cluster.dbus_mmio.a.connect(dbus_mmio_a),
                dbus_mmio_a.bits.source.eq(
                    Cat(cluster.dbus_mmio.a.bits.source,
                        Const(i, self.cluster_bits))),
            ]

            a_arbiter.add(dbus_a)
            mmio_a_arbiter.add(dbus_mmio_a)

            dbus_c = Decoupled(
                tl.ChannelC,
                data_width=cluster.l2cache.out_bus.data_width,
                addr_width=cluster.l2cache.out_bus.addr_width,
                size_width=cluster.l2cache.out_bus.size_width,
                source_id_width=cluster.l2cache.out_bus.source_id_width)
            m.d.comb += [
                cluster.l2cache.out_bus.c.connect(dbus_c),
                dbus_c.bits.source.eq(
                    self.make_source(
                        cluster_id=i,
                        source=cluster.l2cache.out_bus.c.bits.source)),
            ]

            c_arbiter.add(dbus_c)

            e_arbiter.add(cluster.l2cache.out_bus.e)

        if self.bus_master is not None:
            bus_master_a = Decoupled(
                tl.ChannelA,
                data_width=64,
                addr_width=32,
                size_width=3,
                source_id_width=self.l3cache.in_source_id_width)
            m.d.comb += [
                self.bus_master.a.connect(bus_master_a),
                bus_master_a.bits.source.eq(
                    self.make_source(
                        cluster_id=self.num_clusters,
                        source=Cat(
                            self.bus_master.a.bits.source,
                            Const(0,
                                  8 - len(self.bus_master.a.bits.source))))),
            ]
            a_arbiter.add(bus_master_a)

        m.d.comb += [
            a_arbiter.bus.connect(self.l3cache.in_bus.a),
            mmio_a_arbiter.bus.connect(self.dbus_mmio.a),
            c_arbiter.bus.connect(self.l3cache.in_bus.c),
            e_arbiter.bus.connect(self.l3cache.in_bus.e),
        ]

        b_cluster_id, b_source_id = self.unpack_source(
            self.l3cache.in_bus.b.bits.source)

        with m.Switch(b_cluster_id):
            for i, cluster in enumerate(clusters):
                with m.Case(i):
                    m.d.comb += [
                        self.l3cache.in_bus.b.connect(
                            cluster.l2cache.out_bus.b),
                        cluster.l2cache.out_bus.b.bits.source.eq(b_source_id),
                    ]

        d_cluster_id, d_source_id = self.unpack_source(
            self.l3cache.in_bus.d.bits.source)

        with m.Switch(d_cluster_id):
            for i, cluster in enumerate(clusters):
                with m.Case(i):
                    m.d.comb += [
                        self.l3cache.in_bus.d.connect(
                            cluster.l2cache.out_bus.d),
                        cluster.l2cache.out_bus.d.bits.source.eq(d_source_id),
                    ]

            if self.bus_master is not None:
                with m.Case(self.num_clusters):
                    m.d.comb += [
                        self.l3cache.in_bus.d.connect(self.bus_master.d),
                        self.bus_master.d.bits.source.eq(d_source_id),
                    ]

        d_mmio_cluster_id, d_mmio_source_id = self.unpack_source(
            self.dbus_mmio.d.bits.source)

        with m.Switch(d_mmio_cluster_id):
            for i, cluster in enumerate(clusters):
                with m.Case(i):
                    m.d.comb += [
                        self.dbus_mmio.d.connect(cluster.dbus_mmio.d),
                        cluster.dbus_mmio.d.bits.source.eq(d_mmio_source_id),
                    ]

        m.d.comb += [
            self.busy.eq(Cat(c.busy for c in clusters).any()),
            self.ctrl.core_busy.eq(self.busy),
        ]

        return m
