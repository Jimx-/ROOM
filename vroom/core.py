from amaranth import *

from vroom.types import HasVectorParams
from vroom.if_stage import IFStage
from vroom.id_stage import DecodeStage, VOpExpander
from vroom.dispatch import Dispatcher

from room.branch import BranchUpdate
from room.fu import ExecReq, ExecResp
from room.csr import CSR, AutoCSR, CSRAccess
from room.utils import Decoupled


def vtype_layout(xlen):
    return [
        ("vlmul_mag", 2, CSRAccess.RO),
        ("vlmul_sign", 1, CSRAccess.RO),
        ("vsew", 3, CSRAccess.RO),
        ("vta", 1, CSRAccess.RO),
        ("vma", 1, CSRAccess.RO),
        ("_rsvd", xlen - 9, CSRAccess.RO),
        ("vill", 1, CSRAccess.RO),
    ]


class VectorUnit(HasVectorParams, AutoCSR, Elaboratable):

    def __init__(self, params, sim_debug=False):
        super().__init__(params)

        self.sim_debug = sim_debug

        self.req = Decoupled(ExecReq, self.xlen, params)
        self.resp = Decoupled(ExecResp, self.xlen, params)

        self.rob_head_idx = Signal(range(self.core_width * self.num_rob_rows))
        self.rob_pnr_idx = Signal(range(self.core_width * self.num_rob_rows))

        self.br_update = BranchUpdate(params)
        self.exception = Signal()

        self.vl = CSR(0xC20, [('value', self.xlen, CSRAccess.RO)])
        self.vtype = CSR(0xC21, vtype_layout(self.xlen))
        self.vlenb = CSR(0xC22, [('value', self.xlen, CSRAccess.RO)])

    def elaborate(self, platform):
        m = Module()

        #
        # Instruction fetch
        #

        if_stage = m.submodules.if_stage = IFStage(self.params)
        m.d.comb += [
            self.req.connect(if_stage.req),
            if_stage.resp.connect(self.resp),
            if_stage.rob_head_idx.eq(self.rob_head_idx),
            if_stage.rob_pnr_idx.eq(self.rob_pnr_idx),
            if_stage.br_update.eq(self.br_update),
            if_stage.exception.eq(self.exception),
        ]

        #
        # CSR
        #

        m.d.comb += [
            self.vtype.r.vlmul_mag.eq(if_stage.vtype.vlmul_mag),
            self.vtype.r.vlmul_sign.eq(if_stage.vtype.vlmul_sign),
            self.vtype.r.vsew.eq(if_stage.vtype.vsew),
            self.vtype.r.vta.eq(if_stage.vtype.vta),
            self.vtype.r.vma.eq(if_stage.vtype.vma),
            self.vtype.r.vill.eq(if_stage.vtype.vill),
            self.vl.r.eq(if_stage.vl),
            self.vlenb.r.eq(self.vlen // 8),
        ]

        #
        # Decoding
        #

        dec_stage = m.submodules.dec_stage = DecodeStage(self.params)
        m.d.comb += [
            dec_stage.fetch_packet.valid.eq(if_stage.fetch_packet.valid),
            dec_stage.fetch_packet.bits.eq(if_stage.fetch_packet.bits),
            if_stage.fetch_packet.ready.eq(dec_stage.fetch_packet.ready),
            dec_stage.vtype.eq(if_stage.vtype),
        ]

        expander = m.submodules.expander = VOpExpander(self.params)
        m.d.comb += [
            expander.dec_valid.eq(dec_stage.valid),
            expander.dec_uop.eq(dec_stage.uop),
            dec_stage.ready.eq(expander.dec_ready),
        ]

        #
        # Dispatch
        #

        dispatcher = m.submodules.dispatcher = Dispatcher(self.params)
        m.d.comb += [
            dispatcher.expd_valid.eq(expander.expd_valid),
            dispatcher.expd_uop.eq(expander.expd_uop),
            expander.expd_ready.eq(dispatcher.expd_ready),
        ]

        m.d.comb += dispatcher.dis_ready.eq(1)

        return m
