from amaranth import *

from groom.if_stage import IFStage
from groom.id_stage import DecodeStage
from groom.dispatch import Dispatcher
from groom.issue import Scoreboard
from groom.regfile import RegisterFile, RegisterRead
from groom.ex_stage import ALUExecUnit
from groom.fu import ExecResp
from groom.csr import CSRFile
from groom.lsu import LoadStoreUnit
from groom.fp_pipeline import FPPipeline

from room.consts import *
from room.types import HasCoreParams
from room.utils import Arbiter

from roomsoc.interconnect import tilelink as tl
from roomsoc.interconnect.stream import Valid, Decoupled
from roomsoc.peripheral.l2cache import L2Cache


class Core(HasCoreParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.reset_vector = Signal(32)

        self.ibus = tl.Interface(data_width=64,
                                 addr_width=32,
                                 size_width=3,
                                 source_id_width=8,
                                 sink_id_width=4,
                                 name='ibus')

        self.dbus = tl.Interface(data_width=64,
                                 addr_width=32,
                                 size_width=3,
                                 source_id_width=8,
                                 sink_id_width=4,
                                 has_bce=True,
                                 name='dbus')

        self.dbus_mmio = tl.Interface(data_width=64,
                                      addr_width=32,
                                      size_width=3,
                                      source_id_width=8,
                                      name='dbus_mmio')

        self.periph_buses = [self.ibus, self.dbus, self.dbus_mmio]

    def elaborate(self, platform):
        m = Module()

        csr = m.submodules.csr = CSRFile(self.params, width=self.xlen)

        if self.use_fpu:
            fp_pipeline = m.submodules.fp_pipeline = FPPipeline(self.params)

        #
        # Instruction fetch
        #
        if_stage = m.submodules.if_stage = IFStage(self.ibus, self.params)
        csr.add_csrs(if_stage.iter_csrs())
        m.d.comb += if_stage.reset_vector.eq(self.reset_vector)

        #
        # Decoding
        #

        dec_stage = m.submodules.decode_stage = DecodeStage(self.params)
        m.d.comb += [
            if_stage.fetch_packet.connect(dec_stage.fetch_packet),
            if_stage.stall_req.eq(dec_stage.stall_req),
            if_stage.join_req.eq(dec_stage.join_req),
        ]

        #
        # Dispatcher
        #

        dispatcher = m.submodules.dispatcher = Dispatcher(self.params)
        m.d.comb += [
            dispatcher.dec_valid.eq(dec_stage.valid),
            dispatcher.dec_uop.eq(dec_stage.uop),
            dispatcher.dec_wid.eq(dec_stage.wid),
            dec_stage.ready.eq(dispatcher.dec_ready),
        ]

        #
        # Issue
        #

        scoreboard = m.submodules.scoreboard = Scoreboard(is_float=False,
                                                          params=self.params)
        m.d.comb += [
            scoreboard.dis_uop.eq(dispatcher.dis_uop),
            scoreboard.dis_wid.eq(dispatcher.dis_wid),
            scoreboard.sb_uop.eq(dispatcher.sb_uop),
            scoreboard.sb_wid.eq(dispatcher.sb_wid),
        ]

        fp_sb_ready = fp_pipeline.sb_ready if self.use_fpu else 1
        sb_ready = scoreboard.dis_ready & fp_sb_ready

        if self.use_fpu:
            m.d.comb += [
                fp_pipeline.dis_valid.eq(dispatcher.dis_valid),
                fp_pipeline.dis_uop.eq(dispatcher.dis_uop),
                fp_pipeline.dis_wid.eq(dispatcher.dis_wid),
                fp_pipeline.sb_uop.eq(dispatcher.sb_uop),
                fp_pipeline.sb_wid.eq(dispatcher.sb_wid),
            ]

        #
        # Register read
        #

        iregfile = m.submodules.iregfile = RegisterFile(
            rports=self.n_threads * 2,
            wports=self.n_threads,
            num_regs=32 * self.n_warps,
            data_width=self.xlen)

        iregread = m.submodules.iregread = RegisterRead(
            num_rports=self.n_threads * 2,
            rports_array=[2] * self.n_threads,
            reg_width=self.xlen,
            params=self.params)

        dis_is_fp = (dispatcher.dis_uop.iq_type & IssueQueueType.FP) != 0
        dis_is_int = (dispatcher.dis_uop.iq_type &
                      (IssueQueueType.INT | IssueQueueType.MEM)) != 0

        m.d.comb += [
            iregread.dis_valid.eq(dispatcher.dis_valid & dis_is_int
                                  & sb_ready),
            iregread.dis_uop.eq(dispatcher.dis_uop),
            iregread.dis_wid.eq(dispatcher.dis_wid),
        ]

        dis_ready = ~dis_is_int | iregread.dis_ready
        if self.use_fpu:
            dis_ready &= ~dis_is_fp | fp_pipeline.dis_ready

        m.d.comb += [
            scoreboard.dis_valid.eq(dispatcher.dis_valid & dis_ready),
            dispatcher.dis_ready.eq(sb_ready & dis_ready),
        ]

        if self.use_fpu:
            m.d.comb += [
                fp_pipeline.int_dis_ready.eq(iregread.dis_ready),
                fp_pipeline.int_sb_ready.eq(scoreboard.dis_ready),
            ]

        for irr_rp, rp in zip(iregread.read_ports, iregfile.read_ports):
            m.d.comb += irr_rp.connect(rp)

        #
        # Execute
        #

        exec_unit = m.submodules.exec_unit = ALUExecUnit(self.params,
                                                         has_ifpu=self.use_fpu)
        m.d.comb += [
            iregread.exec_req.connect(exec_unit.req),
            if_stage.br_res.eq(exec_unit.br_res),
            if_stage.warp_ctrl.eq(exec_unit.warp_ctrl),
        ]

        csr_write_data = Signal(self.xlen)
        for w in reversed(range(self.n_threads)):
            with m.If(exec_unit.iresp.bits.uop.tmask[w]):
                m.d.comb += csr_write_data.eq(exec_unit.iresp.bits.data[w])

        csr_port = csr.access_port()
        m.d.comb += [
            csr_port.wid.eq(exec_unit.iresp.bits.wid),
            csr_port.addr.eq(exec_unit.iresp.bits.uop.csr_addr),
            csr_port.cmd.eq(exec_unit.iresp.bits.uop.csr_cmd
                            & ~Mux(exec_unit.iresp.valid, 0, CSRCommand.I)),
            csr_port.w_data.eq(csr_write_data),
        ]

        if self.use_fpu:
            m.d.comb += exec_unit.mem_fresp.connect(fp_pipeline.from_int)

        #
        # Load/store unit
        #
        lsu = m.submodules.lsu = LoadStoreUnit(self.dbus, self.dbus_mmio,
                                               self.params)
        m.d.comb += exec_unit.lsu_req.connect(lsu.exec_req)

        if self.use_fpu:
            m.d.comb += [
                lsu.exec_fresp.connect(fp_pipeline.mem_wb_port),
                fp_pipeline.to_lsu.connect(lsu.fp_std),
            ]

        #
        #
        # Writeback
        #

        wb_arb = m.submodules.wb_arb = Arbiter(2 + self.use_fpu, ExecResp,
                                               self.xlen, self.params)
        wb_req = Valid(ExecResp, self.xlen, self.params)
        m.d.comb += [
            wb_arb.inp[0].bits.eq(exec_unit.iresp.bits),
            wb_arb.inp[0].valid.eq(exec_unit.iresp.valid),
            lsu.exec_iresp.connect(wb_arb.inp[1]),
            wb_req.eq(wb_arb.out),
            wb_arb.out.ready.eq(1),
        ]
        if self.use_fpu:
            m.d.comb += fp_pipeline.to_int.connect(wb_arb.inp[2])

        m.d.comb += [
            scoreboard.wakeup.valid.eq(
                wb_req.valid & wb_req.bits.uop.rf_wen()
                & (wb_req.bits.uop.dst_rtype == RegisterType.FIX)),
            scoreboard.wakeup.bits.wid.eq(wb_req.bits.wid),
            scoreboard.wakeup.bits.ldst.eq(wb_req.bits.uop.ldst),
        ]

        for i, wp in enumerate(iregfile.write_ports):
            m.d.comb += [
                wp.valid.eq(wb_req.valid & wb_req.bits.uop.rf_wen()
                            & (wb_req.bits.uop.dst_rtype == RegisterType.FIX)
                            & wb_req.bits.uop.tmask[i]),
                wp.bits.addr.eq(Cat(wb_req.bits.uop.ldst, wb_req.bits.wid)),
                wp.bits.data.eq(
                    Mux(wb_req.bits.uop.csr_cmd != CSRCommand.X,
                        csr_port.r_data[i * self.xlen:(i + 1) * self.xlen],
                        wb_req.bits.data[i])),
            ]

        return m


class HasClusterParams:

    def __init__(self, params, *args, **kwargs):
        self.params = params

        self.num_cores = params['num_cores']
        self.core_bits = Shape.cast(range(self.num_cores)).width

        self.core_params = params['core_params']
        self.l2cache_params = params['l2cache_params']

        self.io_regions = params['io_regions']


class Cluster(HasClusterParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

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

        cores = []

        a_arbiter = m.submodules.a_arbiter = tl.Arbiter(tl.ChannelA,
                                                        data_width=64,
                                                        addr_width=32,
                                                        size_width=3,
                                                        source_id_width=8)
        mmio_a_arbiter = m.submodules.mmio_a_arbiter = tl.Arbiter(
            tl.ChannelA,
            data_width=64,
            addr_width=32,
            size_width=3,
            source_id_width=8)

        c_arbiter = m.submodules.c_arbiter = tl.Arbiter(tl.ChannelC,
                                                        data_width=64,
                                                        addr_width=32,
                                                        size_width=3,
                                                        source_id_width=8)

        e_arbiter = m.submodules.e_arbiter = tl.Arbiter(tl.ChannelE,
                                                        sink_id_width=4)

        for i in range(self.num_cores):
            core_params = self.core_params.copy()
            core_params['core_id'] = i

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
