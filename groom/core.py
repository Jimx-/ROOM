from amaranth import *

from groom.if_stage import IFStage
from groom.id_stage import DecodeStage
from groom.dispatch import Dispatcher
from groom.issue import Scoreboard
from groom.regfile import RegisterFile, RegisterRead
from groom.ex_stage import ALUExecUnit
from groom.fu import ExecResp
from groom.csr import CSRFile

from room.consts import *
from room.types import HasCoreParams

from roomsoc.interconnect import tilelink as tl
from roomsoc.interconnect.stream import Valid


class Core(HasCoreParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.reset_vector = Signal(32)

        self.ibus = tl.Interface(data_width=64,
                                 addr_width=32,
                                 size_width=3,
                                 source_id_width=4,
                                 sink_id_width=4,
                                 name='ibus')

        self.periph_buses = [self.ibus]

    def elaborate(self, platform):
        m = Module()

        csr = m.submodules.csr = CSRFile(self.params, width=self.xlen)

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

        scoreboard = m.submodules.scoreboard = Scoreboard(self.params)
        m.d.comb += [
            scoreboard.dis_valid.eq(dispatcher.dis_valid),
            scoreboard.dis_uop.eq(dispatcher.dis_uop),
            scoreboard.dis_wid.eq(dispatcher.dis_wid),
            scoreboard.sb_uop.eq(dispatcher.sb_uop),
            scoreboard.sb_wid.eq(dispatcher.sb_wid),
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

        m.d.comb += [
            iregread.dis_valid.eq(dispatcher.dis_valid & scoreboard.dis_ready),
            iregread.dis_uop.eq(dispatcher.dis_uop),
            iregread.dis_wid.eq(dispatcher.dis_wid),
        ]

        m.d.comb += dispatcher.dis_ready.eq(scoreboard.dis_ready
                                            & iregread.dis_ready)

        for irr_rp, rp in zip(iregread.read_ports, iregfile.read_ports):
            m.d.comb += irr_rp.connect(rp)

        #
        # Execute
        #

        exec_unit = m.submodules.exec_unit = ALUExecUnit(self.params)
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

        #
        # Writeback
        #

        wb_req = Valid(ExecResp, self.xlen, self.params)
        with m.If(exec_unit.iresp.valid):
            m.d.comb += wb_req.eq(exec_unit.iresp)

        m.d.comb += [
            scoreboard.wakeup.valid.eq(
                wb_req.valid & wb_req.bits.uop.rf_wen()
                & wb_req.bits.uop.dst_rtype == RegisterType.FIX),
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
