from amaranth import *

from vroom.types import HasVectorParams, VMicroOp
from vroom.fu import ExecReq

from groom.regfile import RegisterFile
from room.regfile import RFReadPort

from roomsoc.interconnect.stream import Decoupled, SkidBuffer


class RegReadDecoder(HasVectorParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.dis_uop = VMicroOp(params)
        self.dis_valid = Signal()

        self.rrd_uop = VMicroOp(params)
        self.rrd_valid = Signal()

    def elaborate(self, platform):
        m = Module()

        m.d.comb += [
            self.rrd_valid.eq(self.dis_valid),
            self.rrd_uop.eq(self.dis_uop),
        ]

        inuop = self.dis_uop
        uop = self.rrd_uop

        with m.If(inuop.is_ld | inuop.is_st):
            mop = inuop.funct6[:2]
            lumop = inuop.lrs2
            m.d.comb += [
                uop.unit_stride.eq(mop == 0),
                uop.mask.eq((mop == 0) & (lumop == 0b01011)),
                uop.strided.eq(mop == 2),
                uop.indexed.eq(mop[0]),
            ]

        return m


class RegisterRead(HasVectorParams, Elaboratable):

    def __init__(self, num_rports, reg_width, params):
        super().__init__(params)

        self.num_rports = num_rports
        self.data_width = reg_width

        self.dis_valid = Signal()
        self.dis_uop = VMicroOp(params)
        self.dis_ready = Signal()

        self.read_ports = [
            RFReadPort(5, self.data_width, name=f'read_port{i}')
            for i in range(num_rports)
        ]

        self.exec_req = Decoupled(ExecReq, self.params)

    def elaborate(self, platform):
        m = Module()

        rrd_uop = VMicroOp(self.params)
        rrd_valid = Signal()

        dec = m.submodules.decoder = RegReadDecoder(self.params)
        m.d.comb += [
            dec.dis_uop.eq(self.dis_uop),
            dec.dis_valid.eq(self.dis_valid),
            rrd_uop.eq(dec.rrd_uop),
            rrd_valid.eq(dec.rrd_valid),
        ]

        rrd_rs1_data = Signal(self.data_width)
        rrd_rs2_data = Signal(self.data_width)
        rrd_rs3_data = Signal(self.data_width)

        if self.num_rports > 0:
            m.d.comb += [self.read_ports[0].addr.eq(rrd_uop.lrs1)]
        if self.num_rports > 1:
            m.d.comb += [self.read_ports[1].addr.eq(rrd_uop.lrs2)]
        if self.num_rports > 2:
            m.d.comb += [self.read_ports[2].addr.eq(rrd_uop.ldst)]

        if self.num_rports > 0:
            m.d.comb += rrd_rs1_data.eq(self.read_ports[0].data)
        if self.num_rports > 1:
            m.d.comb += rrd_rs2_data.eq(self.read_ports[1].data)
        if self.num_rports > 2:
            m.d.comb += rrd_rs3_data.eq(self.read_ports[2].data)

        rrd_req = ExecReq(self.params)
        m.d.comb += rrd_req.uop.eq(rrd_uop)
        for l, r in zip(rrd_req.vs1_data, rrd_rs1_data):
            m.d.comb += l.eq(r)
        for l, r in zip(rrd_req.vs2_data, rrd_rs2_data):
            m.d.comb += l.eq(r)
        for l, r in zip(rrd_req.vs3_data, rrd_rs3_data):
            m.d.comb += l.eq(r)

        out_buf = m.submodules.out_buf = SkidBuffer(ExecReq, self.params)
        m.d.comb += [
            out_buf.enq.bits.eq(rrd_req),
            out_buf.enq.valid.eq(rrd_valid),
            self.dis_ready.eq(out_buf.enq.ready),
            out_buf.deq.connect(self.exec_req),
        ]

        return m
