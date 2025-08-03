from amaranth import *

from vroom.consts import *
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

        with m.Switch(inuop.opcode):
            with m.Case(VOpCode.VLE):
                mop = inuop.funct6[:2]
                lumop = inuop.lrs2
                m.d.comb += [
                    uop.unit_stride.eq(mop == 0),
                    uop.mask.eq((mop == 0) & (lumop == 0b01011)),
                    uop.strided.eq(mop == 2),
                    uop.indexed.eq(mop[0]),
                ]

            for opc, alu_op in (
                (VOpCode.VADD, VALUOperator.VADD),
                (VOpCode.VADDU, VALUOperator.VADDU),
                (VOpCode.VSUB, VALUOperator.VSUB),
                (VOpCode.VSUBU, VALUOperator.VSUBU),
                (VOpCode.VRSUB, VALUOperator.VRSUB),
                (VOpCode.VMAX, VALUOperator.VMAX),
                (VOpCode.VMAXU, VALUOperator.VMAXU),
                (VOpCode.VMIN, VALUOperator.VMIN),
                (VOpCode.VMINU, VALUOperator.VMINU),
                (VOpCode.VAND, VALUOperator.VAND),
                (VOpCode.VOR, VALUOperator.VOR),
                (VOpCode.VXOR, VALUOperator.VXOR),
                (VOpCode.VADC, VALUOperator.VADC),
                (VOpCode.VMADC, VALUOperator.VMADC),
                (VOpCode.VSBC, VALUOperator.VSBC),
                (VOpCode.VMSBC, VALUOperator.VMSBC),
                (VOpCode.VMSEQ, VALUOperator.VMSEQ),
                (VOpCode.VMSNE, VALUOperator.VMSNE),
                (VOpCode.VMSLTU, VALUOperator.VMSLTU),
                (VOpCode.VMSLT, VALUOperator.VMSLT),
                (VOpCode.VMSLEU, VALUOperator.VMSLEU),
                (VOpCode.VMSLE, VALUOperator.VMSLE),
                (VOpCode.VMSGTU, VALUOperator.VMSGTU),
                (VOpCode.VMSGT, VALUOperator.VMSGT),
                (VOpCode.VSLL, VALUOperator.VSL),
                (VOpCode.VSRL, VALUOperator.VSR),
                (VOpCode.VSRA, VALUOperator.VSRA),
                (VOpCode.VNSRL, VALUOperator.VSR),
                (VOpCode.VNSRA, VALUOperator.VSRA),
                (VOpCode.VMVSX, VALUOperator.VMVSX),
                (VOpCode.VMVXS, VALUOperator.VMVXS),
                (VOpCode.VMERGE, VALUOperator.VMERGE),
            ):
                with m.Case(opc):
                    m.d.comb += uop.alu_fn.eq(alu_op)

                    with m.Switch(inuop.funct3):
                        with m.Case(0b000):  # OPIVV
                            m.d.comb += uop.opa_sel.eq(VOpA.VS1)

                        with m.Case(0b010):  # OPMVV
                            m.d.comb += uop.opa_sel.eq(VOpA.VS1)

                        with m.Case(0b011):  # OPIVI
                            m.d.comb += uop.opa_sel.eq(VOpA.IMM)

                        with m.Case(0b100):  # OPIVX
                            m.d.comb += uop.opa_sel.eq(VOpA.SCALAR)

                        with m.Case(0b110):  # OPMVX
                            m.d.comb += uop.opa_sel.eq(VOpA.SCALAR)

            for opc, alu_op in (
                (VOpCode.VSEXT, VALUOperator.VSEXT),
                (VOpCode.VZEXT, VALUOperator.VZEXT),
            ):
                with m.Case(opc):
                    m.d.comb += [
                        uop.alu_fn.eq(alu_op),
                        uop.opa_sel.eq(VOpA.IMM),
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
            m.d.comb += self.read_ports[0].addr.eq(rrd_uop.lrs1)
        if self.num_rports > 1:
            m.d.comb += self.read_ports[1].addr.eq(rrd_uop.lrs2)
        if self.num_rports > 2:
            m.d.comb += self.read_ports[2].addr.eq(rrd_uop.ldst)

        if self.num_rports > 0:
            m.d.comb += rrd_rs1_data.eq(self.read_ports[0].data)
        if self.num_rports > 1:
            m.d.comb += rrd_rs2_data.eq(self.read_ports[1].data)
        if self.num_rports > 2:
            m.d.comb += rrd_rs3_data.eq(self.read_ports[2].data)

        rrd_req = ExecReq(self.params)
        m.d.comb += [
            rrd_req.uop.eq(rrd_uop),
            rrd_req.vs1_data.eq(rrd_rs1_data),
            rrd_req.vs2_data.eq(rrd_rs2_data),
            rrd_req.vs3_data.eq(rrd_rs3_data),
        ]

        out_buf = m.submodules.out_buf = SkidBuffer(ExecReq, self.params)
        m.d.comb += [
            out_buf.enq.bits.eq(rrd_req),
            out_buf.enq.valid.eq(rrd_valid),
            self.dis_ready.eq(out_buf.enq.ready),
            out_buf.deq.connect(self.exec_req),
        ]

        return m
