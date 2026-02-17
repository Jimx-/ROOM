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
from room.dcache import DCacheReq, DCacheResp
from groom.fp_pipeline import FPPipeline
from groom.raster import RasterRequest

from room.consts import *
from room.types import HasCoreParams
from room.utils import Arbiter

from roomsoc.interconnect import tilelink as tl
from roomsoc.interconnect.stream import Valid, Decoupled


class Core(HasCoreParams, Elaboratable):

    def __init__(self, params, *, ibus_source_width=8, ibus_sink_width=4):
        super().__init__(params)

        self.reset_vector = Signal(32)
        self.busy = Signal()

        self.dcache_req = [
            Decoupled(DCacheReq, params, name=f'dcache_req{i}')
            for i in range(self.n_threads)
        ]

        self.dcache_resp = [
            Valid(DCacheResp, self.params, name=f'dcache_resp{i}')
            for i in range(self.n_threads)
        ]

        self.dcache_nack = [
            Valid(DCacheReq, self.params, name=f'dcache_nack{i}')
            for i in range(self.n_threads)
        ]

        if self.use_raster:
            self.raster_req = Decoupled(RasterRequest, self.params)

        self.ibus = tl.Interface(data_width=64,
                                 addr_width=32,
                                 size_width=3,
                                 source_id_width=ibus_source_width,
                                 sink_id_width=ibus_sink_width,
                                 name='ibus')

        self.periph_buses = [self.ibus]

    def elaborate(self, platform):
        m = Module()

        csr = m.submodules.csr = CSRFile(self.params, width=self.xlen)

        if self.use_fpu:
            fp_pipeline = m.submodules.fp_pipeline = FPPipeline(self.params)
            csr.add_csrs(fp_pipeline.iter_csrs())

        #
        # Instruction fetch
        #
        if_stage = m.submodules.if_stage = IFStage(self.ibus, self.params)
        csr.add_csrs(if_stage.iter_csrs())
        m.d.comb += [
            if_stage.reset_vector.eq(self.reset_vector),
            self.busy.eq(if_stage.busy),
        ]

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
            iregread.dis_valid.eq(
                dispatcher.dis_valid & dis_is_int
                & ((~dis_is_fp | fp_pipeline.dis_ready) if self.use_fpu else 1)
                & sb_ready),
            iregread.dis_uop.eq(dispatcher.dis_uop),
            iregread.dis_wid.eq(dispatcher.dis_wid),
        ]

        dis_ready = ~dis_is_int | iregread.dis_ready
        if self.use_fpu:
            dis_ready &= ~dis_is_fp | fp_pipeline.dis_ready

        m.d.comb += [
            scoreboard.dis_valid.eq(dispatcher.dis_valid & dis_ready & (
                fp_pipeline.sb_ready if self.use_fpu else 1)),
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

        exec_unit = m.submodules.exec_unit = ALUExecUnit(
            self.params, has_ifpu=self.use_fpu, has_raster=self.use_raster)
        csr.add_csrs(exec_unit.iter_csrs())
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
            csr_port.tmask.eq(exec_unit.iresp.bits.uop.tmask),
            csr_port.addr.eq(exec_unit.iresp.bits.uop.csr_addr),
            csr_port.cmd.eq(exec_unit.iresp.bits.uop.csr_cmd
                            & ~Mux(exec_unit.iresp.valid, 0, CSRCommand.I)),
            csr_port.w_data.eq(csr_write_data),
        ]

        if self.use_fpu:
            m.d.comb += exec_unit.mem_fresp.connect(fp_pipeline.from_int)

        if self.use_raster:
            m.d.comb += exec_unit.raster_req.connect(self.raster_req)

        #
        # Load/store unit
        #
        lsu = m.submodules.lsu = LoadStoreUnit(self.params)
        m.d.comb += exec_unit.lsu_req.connect(lsu.exec_req)

        for dcache_req, lsu_dcache_req in zip(self.dcache_req, lsu.dcache_req):
            m.d.comb += lsu_dcache_req.connect(dcache_req)
        for dcache_resp, lsu_dcache_resp in zip(self.dcache_resp,
                                                lsu.dcache_resp):
            m.d.comb += lsu_dcache_resp.eq(dcache_resp)
        for dcache_nack, lsu_dcache_nack in zip(self.dcache_nack,
                                                lsu.dcache_nack):
            m.d.comb += lsu_dcache_nack.eq(dcache_nack)

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
