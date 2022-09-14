from amaranth import *
from amaranth.hdl.rec import DIR_FANIN, DIR_FANOUT

from room.consts import *
from room.types import MicroOp
from room.alu import ExecReq
from room.branch import BranchUpdate


class RFReadPort(Record):

    def __init__(self, addr_width, data_width, name=None):
        self.addr_width = addr_width
        self.data_width = data_width

        super().__init__([('addr', addr_width, DIR_FANOUT),
                          ('data', data_width, DIR_FANIN)],
                         name=name)


class RFWritePort(Record):

    def __init__(self, addr_width, data_width, name=None):
        self.addr_width = addr_width
        self.data_width = data_width

        super().__init__([('addr', addr_width, DIR_FANIN),
                          ('data', data_width, DIR_FANIN),
                          ('valid', 1, DIR_FANIN)],
                         name=name)


class RegisterFile(Elaboratable):

    def __init__(self, rports, wports, num_regs=32, data_width=32):
        self.num_regs = num_regs
        addr_width = Shape.cast(range(num_regs)).width
        self.data_width = data_width

        self.read_ports = [
            RFReadPort(addr_width, self.data_width, name=f'read_port{i}')
            for i in range(rports)
        ]

        self.write_ports = [
            RFWritePort(addr_width, self.data_width, name=f'write_port{i}')
            for i in range(wports)
        ]

    def elaborate(self, platform):
        m = Module()

        mem = Memory(width=self.data_width, depth=self.num_regs)

        mem_rports = [mem.read_port() for _ in self.read_ports]
        for i, port in enumerate(mem_rports):
            setattr(m.submodules, f'mem_read_port{i}', port)

        for rp, mrp in zip(self.read_ports, mem_rports):
            m.d.comb += [
                mrp.addr.eq(rp.addr),
                rp.data.eq(mrp.data),
            ]

        mem_wports = [mem.write_port() for _ in self.write_ports]
        for i, port in enumerate(mem_wports):
            setattr(m.submodules, f'mem_write_port{i}', port)

        for wp, mwp in zip(self.write_ports, mem_wports):
            m.d.comb += [
                mwp.addr.eq(wp.addr),
                mwp.data.eq(wp.data),
                mwp.en.eq(wp.valid),
            ]

        return m


class RegReadDecoder(Elaboratable):

    def __init__(self, params):
        self.iss_uop = MicroOp(params)
        self.iss_valid = Signal()

        self.rrd_uop = MicroOp(params)
        self.rrd_valid = Signal()

    def elaborate(self, platform):
        m = Module()

        m.d.comb += [
            self.rrd_valid.eq(self.iss_valid),
            self.rrd_uop.eq(self.iss_uop),
        ]

        F = lambda f: self.rrd_uop.alu_fn.eq(f)

        BT = lambda typ: self.rrd_uop.br_type.eq(typ)

        OPA_RS1 = self.rrd_uop.opa_sel.eq(OpA.RS1)
        OPA_PC = self.rrd_uop.opa_sel.eq(OpA.PC)
        OPA_ZERO = self.rrd_uop.opa_sel.eq(OpA.ZERO)

        OPB_RS2 = self.rrd_uop.opb_sel.eq(OpB.RS2)
        OPB_IMM = self.rrd_uop.opb_sel.eq(OpB.IMM)
        OPB_NEXT = self.rrd_uop.opb_sel.eq(OpB.NEXT)
        OPB_ZERO = self.rrd_uop.opb_sel.eq(OpB.ZERO)
        OPB_IMMC = self.rrd_uop.opb_sel.eq(OpB.IMMC)

        IMM_I = self.rrd_uop.imm_sel.eq(ImmSel.I)
        IMM_J = self.rrd_uop.imm_sel.eq(ImmSel.J)
        IMM_B = self.rrd_uop.imm_sel.eq(ImmSel.B)
        IMM_U = self.rrd_uop.imm_sel.eq(ImmSel.U)

        with m.Switch(self.iss_uop.opcode):
            with m.Case(UOpCode.LUI):
                m.d.comb += [
                    F(ALUOperator.ADD),
                    OPA_ZERO,
                    OPB_IMM,
                    IMM_U,
                ]

            for uopc, alu_op in (
                (UOpCode.ADDI, ALUOperator.ADD),
                (UOpCode.ANDI, ALUOperator.AND),
                (UOpCode.ORI, ALUOperator.OR),
                (UOpCode.XORI, ALUOperator.XOR),
                (UOpCode.SLTI, ALUOperator.SLT),
                (UOpCode.SLTIU, ALUOperator.SLTU),
                (UOpCode.SLLI, ALUOperator.SL),
                (UOpCode.SRAI, ALUOperator.SRA),
                (UOpCode.SRLI, ALUOperator.SR),
            ):
                with m.Case(uopc):
                    m.d.comb += [
                        F(alu_op),
                        OPA_RS1,
                        OPB_IMM,
                        IMM_I,
                    ]

            for uopc, alu_op in (
                (UOpCode.ADD, ALUOperator.ADD),
                (UOpCode.SUB, ALUOperator.SUB),
                (UOpCode.AND, ALUOperator.AND),
                (UOpCode.OR, ALUOperator.OR),
                (UOpCode.XOR, ALUOperator.XOR),
                (UOpCode.SLT, ALUOperator.SLT),
                (UOpCode.SLTU, ALUOperator.SLTU),
                (UOpCode.SLL, ALUOperator.SL),
                (UOpCode.SRA, ALUOperator.SRA),
                (UOpCode.SRL, ALUOperator.SR),
            ):
                with m.Case(uopc):
                    m.d.comb += [
                        F(alu_op),
                        OPA_RS1,
                        OPB_RS2,
                    ]

            for uopc, alu_op in (
                (UOpCode.MUL, ALUOperator.MUL),
                (UOpCode.MULH, ALUOperator.MULH),
                (UOpCode.MULHU, ALUOperator.MULHU),
                (UOpCode.MULHSU, ALUOperator.MULHSU),
            ):
                with m.Case(uopc):
                    m.d.comb += [
                        F(alu_op),
                        OPA_RS1,
                        OPB_RS2,
                    ]

            for uopc, alu_op in (
                (UOpCode.DIV, ALUOperator.DIV),
                (UOpCode.DIVU, ALUOperator.DIVU),
                (UOpCode.REM, ALUOperator.REM),
                (UOpCode.REMU, ALUOperator.REMU),
            ):
                with m.Case(uopc):
                    m.d.comb += [
                        F(alu_op),
                        OPA_RS1,
                        OPB_RS2,
                    ]

            with m.Case(UOpCode.JAL):
                m.d.comb += [
                    F(ALUOperator.ADD),
                    BT(BranchType.J),
                    OPA_PC,
                    OPB_NEXT,
                    IMM_J,
                ]

            with m.Case(UOpCode.JALR):
                m.d.comb += [
                    F(ALUOperator.ADD),
                    BT(BranchType.JR),
                    OPA_PC,
                    OPB_NEXT,
                    IMM_I,
                ]

            with m.Case(UOpCode.AUIPC):
                m.d.comb += [
                    F(ALUOperator.ADD),
                    OPA_PC,
                    OPB_IMM,
                    IMM_U,
                ]

            with m.Case(UOpCode.BEQ):
                m.d.comb += [
                    F(ALUOperator.SUB),
                    BT(BranchType.EQ),
                    IMM_B,
                ]

            with m.Case(UOpCode.BNE):
                m.d.comb += [
                    F(ALUOperator.SUB),
                    BT(BranchType.NE),
                    IMM_B,
                ]

            with m.Case(UOpCode.BGE):
                m.d.comb += [
                    F(ALUOperator.SLT),
                    BT(BranchType.GE),
                    IMM_B,
                ]

            with m.Case(UOpCode.BGEU):
                m.d.comb += [
                    F(ALUOperator.SLTU),
                    BT(BranchType.GEU),
                    IMM_B,
                ]

            with m.Case(UOpCode.BLT):
                m.d.comb += [
                    F(ALUOperator.SLT),
                    BT(BranchType.LT),
                    IMM_B,
                ]

            with m.Case(UOpCode.BLTU):
                m.d.comb += [
                    F(ALUOperator.SLTU),
                    BT(BranchType.LTU),
                    IMM_B,
                ]

            with m.Case(UOpCode.CSRRW, UOpCode.CSRRS, UOpCode.CSRRC):
                m.d.comb += [
                    F(ALUOperator.ADD),
                    OPA_RS1,
                    OPB_ZERO,
                    IMM_I,
                ]

            with m.Case(UOpCode.CSRRWI, UOpCode.CSRRSI, UOpCode.CSRRCI):
                m.d.comb += [
                    F(ALUOperator.ADD),
                    OPA_ZERO,
                    OPB_IMMC,
                    IMM_I,
                ]

            with m.Case(UOpCode.ERET):
                m.d.comb += [
                    F(ALUOperator.ADD),
                    OPA_ZERO,
                    OPB_IMMC,
                    IMM_I,
                ]

        m.d.comb += [
            self.rrd_uop.is_load.eq(self.iss_uop.opcode == UOpCode.LD),
            self.rrd_uop.is_sta.eq(self.iss_uop.opcode == UOpCode.STA),
            self.rrd_uop.is_std.eq((self.iss_uop.opcode == UOpCode.STD) | (
                (self.iss_uop.opcode == UOpCode.STA)
                & (self.iss_uop.lrs2_rtype == RegisterType.FIX))),
        ]

        return m


class RegisterRead(Elaboratable):

    def __init__(self, issue_width, num_rports, rports_array, reg_width,
                 params):
        num_regs = params['num_pregs']
        self.issue_width = issue_width
        self.params = params
        self.rports_array = rports_array
        self.data_width = reg_width

        self.iss_uops = [
            MicroOp(params, name=f'iss_uop{i}') for i in range(issue_width)
        ]
        self.iss_valids = Signal(issue_width)

        self.read_ports = [
            RFReadPort(Shape.cast(range(num_regs)).width,
                       self.data_width,
                       name=f'read_port{i}') for i in range(num_rports)
        ]

        self.exec_reqs = [
            ExecReq(self.params, name=f'exec_req{i}')
            for i in range(self.issue_width)
        ]

        self.br_update = BranchUpdate(params)

        self.kill = Signal()

    def elaborate(self, platform):
        m = Module()

        rrd_uops = [
            MicroOp(self.params, name=f'rrd_uop{i}')
            for i in range(self.issue_width)
        ]
        rrd_valids = Signal(self.issue_width)

        for rrd_uop, iss_uop, rrd_v, iss_v in zip(rrd_uops, self.iss_uops,
                                                  rrd_valids, self.iss_valids):
            dec = RegReadDecoder(self.params)
            m.submodules += dec
            m.d.comb += [
                dec.iss_uop.eq(iss_uop),
                dec.iss_valid.eq(iss_v),
            ]

            m.d.sync += [
                rrd_uop.eq(dec.rrd_uop),
                rrd_v.eq(dec.rrd_valid
                         & ~self.br_update.uop_killed(dec.rrd_uop)),
            ]

        rrd_rs1_data = [
            Signal(self.data_width, name=f'rrd{i}_rs1_data')
            for i in range(self.issue_width)
        ]
        rrd_rs2_data = [
            Signal(self.data_width, name=f'rrd{i}_rs2_data')
            for i in range(self.issue_width)
        ]

        idx = 0
        for iss_uop, rrd_uop, nrps, rs1_data, rs2_data in zip(
                self.iss_uops, rrd_uops, self.rports_array, rrd_rs1_data,
                rrd_rs2_data):
            if nrps > 0:
                m.d.comb += [self.read_ports[idx].addr.eq(iss_uop.prs1)]
            if nrps > 1:
                m.d.comb += [self.read_ports[idx + 1].addr.eq(iss_uop.prs2)]

            if nrps > 0:
                m.d.comb += rs1_data.eq(
                    Mux(rrd_uop.prs1 == 0, 0, self.read_ports[idx].data))
            if nrps > 1:
                m.d.comb += rs2_data.eq(
                    Mux(rrd_uop.prs2 == 0, 0, self.read_ports[idx + 1].data))

            idx += nrps

        for nrps, req, rs1_data, rs2_data in zip(self.rports_array,
                                                 self.exec_reqs, rrd_rs1_data,
                                                 rrd_rs2_data):
            if nrps > 0:
                m.d.sync += req.rs1_data.eq(rs1_data)
            if nrps > 1:
                m.d.sync += req.rs2_data.eq(rs2_data)

        for req, rrd_uop, rrd_v in zip(self.exec_reqs, rrd_uops, rrd_valids):
            rrd_killed = self.kill | self.br_update.uop_killed(rrd_uop)

            m.d.sync += [
                req.uop.eq(rrd_uop),
                req.uop.br_mask.eq(
                    self.br_update.get_new_br_mask(rrd_uop.br_mask)),
                req.valid.eq(rrd_v & ~rrd_killed),
            ]

        return m
