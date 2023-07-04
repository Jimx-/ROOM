from amaranth import *

from groom.core import Core

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


class Cluster(HasClusterParams, Elaboratable):

    def __init__(self, cluster_id, params):
        super().__init__(params)

        self.cluster_id = cluster_id

        self.reset_vector = Signal(32)

        self.dbus_mmio = tl.Interface(data_width=64,
                                      addr_width=32,
                                      size_width=3,
                                      source_id_width=8,
                                      name='dbus_mmio')

        self.l2cache_params['client_source_map'] = dict(
            (i, (self.make_source(True, i, Const(0, 8)),
                 self.make_source(True, i, Const(~0, 8))))
            for i in range(self.num_cores))
        self.l2cache = L2Cache(self.l2cache_params)

        self.periph_buses = [self.dbus_mmio, self.l2cache.out_bus]

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
            source_id_width=8)

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

            core = Core(core_params)
            setattr(m.submodules, f'core{i}', core)
            cores.append(core)

            m.d.comb += core.reset_vector.eq(self.reset_vector)

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
                                     source=core.dbus_mmio.a.bits.source)),
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

        _, mmio_d_core_id, mmio_d_source_id = self.unpack_source(
            self.dbus_mmio.d.bits.source)

        with m.Switch(mmio_d_core_id):
            for i, core in enumerate(cores):
                with m.Case(i):
                    m.d.comb += [
                        self.dbus_mmio.d.connect(core.dbus_mmio.d),
                        core.dbus_mmio.d.bits.source.eq(mmio_d_source_id),
                    ]

        return m


class GroomWrapper(HasClusterParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.reset_vector = Signal(32)

        self.dbus_mmio = tl.Interface(data_width=64,
                                      addr_width=32,
                                      size_width=3,
                                      source_id_width=8 + self.cluster_bits,
                                      name='dbus_mmio')

        self.l3cache_params['client_source_map'] = dict(
            (i, (self.make_source(i, Const(0, 8)),
                 self.make_source(i, Const(~0, 8))))
            for i in range(self.num_clusters))
        self.l3cache = L2Cache(self.l3cache_params)

        self.periph_buses = [self.dbus_mmio, self.l3cache.out_bus]

    def make_source(self, cluster_id, source):
        return Cat(source[:-self.cluster_bits],
                   Const(cluster_id, self.cluster_bits))

    def unpack_source(self, source):
        return source[-self.cluster_bits:], source[:-self.cluster_bits]

    def elaborate(self, platform):
        m = Module()

        m.submodules.l3cache = self.l3cache

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
            source_id_width=8 + self.cluster_bits)

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

            m.d.comb += cluster.reset_vector.eq(self.reset_vector)

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

        with m.Switch(self.dbus_mmio.d.bits.source[8:]):
            for i, cluster in enumerate(clusters):
                with m.Case(i):
                    m.d.comb += self.dbus_mmio.d.connect(cluster.dbus_mmio.d)

        return m
