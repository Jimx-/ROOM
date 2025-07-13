from amaranth import *
from amaranth.hdl.rec import DIR_FANIN, DIR_FANOUT

from room.consts import *
from room.types import HasCoreParams, MicroOp
from room.fu import ExecReq, ExecResp
from room.branch import BranchUpdate

from roomsoc.interconnect.stream import Decoupled, Valid


class WritebackDebug(HasCoreParams, Record):

    def __init__(self, params, name=None, src_loc_at=0):
        HasCoreParams.__init__(self, params)

        Record.__init__(self, [
            ('uop_id', MicroOp.ID_WIDTH),
            ('pdst', range(self.num_pregs)),
            ('data', self.xlen),
        ],
                        name=name,
                        src_loc_at=1 + src_loc_at)


class RFReadPort(Record):

    def __init__(self, addr_width, data_width, name=None, src_loc_at=0):
        self.addr_width = addr_width
        self.data_width = data_width

        super().__init__([('addr', addr_width, DIR_FANOUT),
                          ('data', data_width, DIR_FANIN)],
                         name=name,
                         src_loc_at=1 + src_loc_at)


class RFWritePort(Record):

    def __init__(self, addr_width, data_width, name=None, src_loc_at=0):
        self.addr_width = addr_width
        self.data_width = data_width

        super().__init__([
            ('addr', addr_width, DIR_FANIN),
            ('data', data_width, DIR_FANIN),
        ],
                         name=name,
                         src_loc_at=1 + src_loc_at)


class RegisterFile(Elaboratable):

    def __init__(self,
                 rports,
                 wports,
                 num_regs=32,
                 data_width=32,
                 bypassable_mask=None):
        self.num_regs = num_regs
        addr_width = Shape.cast(range(num_regs)).width
        self.data_width = data_width
        self.bypassable_mask = bypassable_mask

        self.read_ports = [
            RFReadPort(addr_width, self.data_width, name=f'read_port{i}')
            for i in range(rports)
        ]

        self.write_ports = [
            Decoupled(RFWritePort,
                      addr_width,
                      self.data_width,
                      name=f'write_port{i}') for i in range(wports)
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
                mwp.addr.eq(wp.bits.addr),
                mwp.data.eq(wp.bits.data),
                mwp.en.eq(wp.valid),
                wp.ready.eq(1),
            ]

        assert self.bypassable_mask is None or len(
            self.bypassable_mask) == len(self.write_ports)

        if self.bypassable_mask is not None and any(self.bypassable_mask):
            for rp in self.read_ports:
                raddr = Signal.like(rp.addr)
                m.d.sync += raddr.eq(rp.addr)

                for wp, b in zip(self.write_ports, self.bypassable_mask):
                    if b:
                        with m.If(wp.valid & (wp.bits.addr == raddr)):
                            m.d.comb += rp.data.eq(wp.bits.data)

        return m


class RegReadDecoder(HasCoreParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

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
        OPA_RS1SHL = self.rrd_uop.opa_sel.eq(OpA.RS1SHL)

        OPB_RS2 = self.rrd_uop.opb_sel.eq(OpB.RS2)
        OPB_IMM = self.rrd_uop.opb_sel.eq(OpB.IMM)
        OPB_NEXT = self.rrd_uop.opb_sel.eq(OpB.NEXT)
        OPB_ZERO = self.rrd_uop.opb_sel.eq(OpB.ZERO)
        OPB_IMMC = self.rrd_uop.opb_sel.eq(OpB.IMMC)
        OPB_RS2OH = self.rrd_uop.opb_sel.eq(OpB.RS2OH)
        OPB_IMMOH = self.rrd_uop.opb_sel.eq(OpB.IMMOH)

        IMM_I = self.rrd_uop.imm_sel.eq(ImmSel.I)
        IMM_J = self.rrd_uop.imm_sel.eq(ImmSel.J)
        IMM_B = self.rrd_uop.imm_sel.eq(ImmSel.B)
        IMM_U = self.rrd_uop.imm_sel.eq(ImmSel.U)
        IMM_V = self.rrd_uop.imm_sel.eq(ImmSel.V)

        DW_32 = self.rrd_uop.alu_dw.eq(ALUWidth.DW_32)

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
                (UOpCode.ADDIW, ALUOperator.ADD),
                (UOpCode.SLLIW, ALUOperator.SL),
                (UOpCode.SRAIW, ALUOperator.SRA),
                (UOpCode.SRLIW, ALUOperator.SR),
            ):
                with m.Case(uopc):
                    m.d.comb += [
                        F(alu_op),
                        OPA_RS1,
                        OPB_IMM,
                        IMM_I,
                        DW_32,
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
                (UOpCode.ADDW, ALUOperator.ADD),
                (UOpCode.SUBW, ALUOperator.SUB),
                (UOpCode.SLLW, ALUOperator.SL),
                (UOpCode.SRAW, ALUOperator.SRA),
                (UOpCode.SRLW, ALUOperator.SR),
            ):
                with m.Case(uopc):
                    m.d.comb += [
                        F(alu_op),
                        OPA_RS1,
                        OPB_RS2,
                        DW_32,
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

            with m.Case(UOpCode.MULW):
                m.d.comb += [
                    F(ALUOperator.MUL),
                    OPA_RS1,
                    OPB_RS2,
                    DW_32,
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

            for uopc, alu_op in (
                (UOpCode.DIVW, ALUOperator.DIV),
                (UOpCode.DIVUW, ALUOperator.DIVU),
                (UOpCode.REMW, ALUOperator.REM),
                (UOpCode.REMUW, ALUOperator.REMU),
            ):
                with m.Case(uopc):
                    m.d.comb += [
                        F(alu_op),
                        OPA_RS1,
                        OPB_RS2,
                        DW_32,
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

            with m.Case(UOpCode.ERET, UOpCode.WFI):
                m.d.comb += [
                    F(ALUOperator.ADD),
                    OPA_ZERO,
                    OPB_IMMC,
                    IMM_I,
                ]

            if self.use_zba:
                for uopc, alu_op, shamt, is_uw in (
                    (UOpCode.ADD_UW, ALUOperator.ADD, 0, 1),
                    (UOpCode.SH1ADD, ALUOperator.ADD, 1, 0),
                    (UOpCode.SH1ADD_UW, ALUOperator.ADD, 1, 1),
                    (UOpCode.SH2ADD, ALUOperator.ADD, 2, 0),
                    (UOpCode.SH2ADD_UW, ALUOperator.ADD, 2, 1),
                    (UOpCode.SH3ADD, ALUOperator.ADD, 3, 0),
                    (UOpCode.SH3ADD_UW, ALUOperator.ADD, 3, 1),
                ):
                    with m.Case(uopc):
                        m.d.comb += [
                            F(alu_op),
                            OPA_RS1SHL,
                            OPB_RS2,
                            self.rrd_uop.opa_shamt.eq(shamt),
                            self.rrd_uop.opa_is_uw.eq(is_uw),
                        ]

                with m.Case(UOpCode.SLLI_UW):
                    m.d.comb += [
                        F(ALUOperator.SL),
                        OPA_RS1SHL,
                        OPB_IMM,
                        IMM_I,
                        self.rrd_uop.opa_is_uw.eq(1),
                    ]

            if self.use_zbb:
                for uopc, alu_op in (
                    (UOpCode.ANDN, ALUOperator.ANDN),
                    (UOpCode.ORN, ALUOperator.ORN),
                    (UOpCode.XNOR, ALUOperator.XNOR),
                    (UOpCode.MAX, ALUOperator.MAX),
                    (UOpCode.MIN, ALUOperator.MIN),
                    (UOpCode.MAXU, ALUOperator.MAXU),
                    (UOpCode.MINU, ALUOperator.MINU),
                    (UOpCode.ROL, ALUOperator.ROL),
                    (UOpCode.ROR, ALUOperator.ROR),
                ):
                    with m.Case(uopc):
                        m.d.comb += [
                            F(alu_op),
                            OPA_RS1,
                            OPB_RS2,
                        ]

                for uopc, alu_op in (
                    (UOpCode.ROLW, ALUOperator.ROL),
                    (UOpCode.RORW, ALUOperator.ROR),
                ):
                    with m.Case(uopc):
                        m.d.comb += [
                            F(alu_op),
                            OPA_RS1,
                            OPB_RS2,
                            DW_32,
                        ]

                for uopc, alu_op in (
                    (UOpCode.UNARY, ALUOperator.UNARY),
                    (UOpCode.RORI, ALUOperator.ROR),
                ):
                    with m.Case(uopc):
                        m.d.comb += [
                            F(alu_op),
                            OPA_RS1,
                            OPB_IMM,
                            IMM_I,
                        ]

                for uopc, alu_op in (
                    (UOpCode.UNARYW, ALUOperator.UNARY),
                    (UOpCode.RORIW, ALUOperator.ROR),
                ):
                    with m.Case(uopc):
                        m.d.comb += [
                            F(alu_op),
                            OPA_RS1,
                            OPB_IMM,
                            IMM_I,
                            DW_32,
                        ]

            if self.use_zbs:
                for uopc, alu_op in (
                    (UOpCode.BCLR, ALUOperator.ANDN),
                    (UOpCode.BINV, ALUOperator.XOR),
                    (UOpCode.BSET, ALUOperator.OR),
                ):
                    with m.Case(uopc):
                        m.d.comb += [
                            F(alu_op),
                            OPA_RS1,
                            OPB_RS2OH,
                        ]

                for uopc, alu_op in (
                    (UOpCode.BCLRI, ALUOperator.ANDN),
                    (UOpCode.BINVI, ALUOperator.XOR),
                    (UOpCode.BSETI, ALUOperator.OR),
                ):
                    with m.Case(uopc):
                        m.d.comb += [
                            F(alu_op),
                            OPA_RS1,
                            OPB_IMMOH,
                            IMM_I,
                        ]

                with m.Case(UOpCode.BEXT):
                    m.d.comb += [
                        F(ALUOperator.BEXT),
                        OPA_RS1,
                        OPB_RS2,
                    ]

                with m.Case(UOpCode.BEXTI):
                    m.d.comb += [
                        F(ALUOperator.BEXT),
                        OPA_RS1,
                        OPB_IMM,
                        IMM_I,
                    ]

            if self.use_zicond:
                for uopc, alu_op in (
                    (UOpCode.CZERO_EQZ, ALUOperator.CZEQZ),
                    (UOpCode.CZERO_NEZ, ALUOperator.CZNEZ),
                ):
                    with m.Case(uopc):
                        m.d.comb += [
                            F(alu_op),
                            OPA_RS1,
                            OPB_RS2,
                        ]

            if self.use_vector:
                with m.Case(UOpCode.VSETVL):
                    m.d.comb += [
                        OPA_RS1,
                        OPB_RS2,
                    ]

                with m.Case(UOpCode.VSETVLI):
                    m.d.comb += [
                        OPA_RS1,
                        OPB_IMM,
                        IMM_V,
                    ]

                with m.Case(UOpCode.VSETIVLI):
                    m.d.comb += [
                        OPB_IMM,
                        IMM_V,
                    ]

        m.d.comb += [
            self.rrd_uop.is_load.eq(self.iss_uop.opcode == UOpCode.LD),
            self.rrd_uop.is_sta.eq((self.iss_uop.opcode == UOpCode.STA)
                                   | (self.iss_uop.opcode == UOpCode.AMO_AG)),
            self.rrd_uop.is_std.eq((self.iss_uop.opcode == UOpCode.STD) | (
                self.rrd_uop.is_sta
                & (self.iss_uop.lrs2_rtype == RegisterType.FIX))),
        ]

        with m.If((self.rrd_uop.opcode == UOpCode.AMO_AG)
                  | ((self.rrd_uop.opcode == UOpCode.LD)
                     & (self.rrd_uop.mem_cmd == MemoryCommand.LR))):
            m.d.comb += self.rrd_uop.imm_packed.eq(0)

        if hasattr(self, 'num_pregs'):
            with m.If(((self.iss_uop.csr_cmd == CSRCommand.S)
                       | (self.iss_uop.csr_cmd == CSRCommand.C))
                      & (self.iss_uop.prs1 == 0)):
                m.d.comb += self.rrd_uop.csr_cmd.eq(CSRCommand.R)
        else:
            with m.If(((self.iss_uop.csr_cmd == CSRCommand.S)
                       | (self.iss_uop.csr_cmd == CSRCommand.C))
                      & (self.iss_uop.lrs1 == 0)):
                m.d.comb += self.rrd_uop.csr_cmd.eq(CSRCommand.R)

        return m


class RegisterRead(HasCoreParams, Elaboratable):

    def __init__(self, issue_width, num_rports, rports_array, num_bypass_ports,
                 reg_width, params):
        super().__init__(params)

        self.issue_width = issue_width
        self.rports_array = rports_array
        self.data_width = reg_width

        self.iss_uops = [
            MicroOp(params, name=f'iss_uop{i}') for i in range(issue_width)
        ]
        self.iss_valids = Signal(issue_width)

        self.read_ports = [
            RFReadPort(Shape.cast(range(self.num_pregs)).width,
                       self.data_width,
                       name=f'read_port{i}') for i in range(num_rports)
        ]

        self.exec_reqs = [
            Decoupled(ExecReq, reg_width, self.params, name=f'exec_req{i}')
            for i in range(self.issue_width)
        ]

        self.bypass = [
            Valid(ExecResp, reg_width, self.params, name=f'bypass{i}')
            for i in range(num_bypass_ports)
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
                rrd_uop.br_mask.eq(
                    self.br_update.get_new_br_mask(dec.rrd_uop.br_mask)),
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
        rrd_rs3_data = [
            Signal(self.data_width, name=f'rrd{i}_rs3_data')
            for i in range(self.issue_width)
        ]

        idx = 0
        for iss_uop, rrd_uop, nrps, rs1_data, rs2_data, rs3_data in zip(
                self.iss_uops, rrd_uops, self.rports_array, rrd_rs1_data,
                rrd_rs2_data, rrd_rs3_data):
            if nrps > 0:
                m.d.comb += [self.read_ports[idx].addr.eq(iss_uop.prs1)]
            if nrps > 1:
                m.d.comb += [self.read_ports[idx + 1].addr.eq(iss_uop.prs2)]
            if nrps > 2:
                m.d.comb += [self.read_ports[idx + 2].addr.eq(iss_uop.prs3)]

            if nrps > 0:
                m.d.comb += rs1_data.eq(
                    Mux(rrd_uop.prs1 == 0, 0, self.read_ports[idx].data))
            if nrps > 1:
                m.d.comb += rs2_data.eq(
                    Mux(rrd_uop.prs2 == 0, 0, self.read_ports[idx + 1].data))
            if nrps > 2:
                m.d.comb += rs3_data.eq(
                    Mux(rrd_uop.prs3 == 0, 0, self.read_ports[idx + 2].data))

            idx += nrps

        bypass_rs1_data = [
            Signal(self.data_width, name=f'bypass_rs1_data{i}')
            for i in range(self.issue_width)
        ]
        bypass_rs2_data = [
            Signal(self.data_width, name=f'bypass_rs2_data{i}')
            for i in range(self.issue_width)
        ]

        for rrd_uop, bypass_data, rs1_data in zip(rrd_uops, bypass_rs1_data,
                                                  rrd_rs1_data):
            m.d.comb += bypass_data.eq(rs1_data)
            for byp in self.bypass:
                with m.If(byp.valid & (rrd_uop.prs1 == byp.bits.uop.pdst)
                          & (rrd_uop.prs1 != 0)
                          & (byp.bits.uop.dst_rtype == RegisterType.FIX)
                          & (rrd_uop.lrs1_rtype == RegisterType.FIX)):
                    m.d.comb += bypass_data.eq(byp.bits.data)

        for rrd_uop, bypass_data, rs2_data in zip(rrd_uops, bypass_rs2_data,
                                                  rrd_rs2_data):
            m.d.comb += bypass_data.eq(rs2_data)
            for byp in self.bypass:
                with m.If(byp.valid & (rrd_uop.prs2 == byp.bits.uop.pdst)
                          & (rrd_uop.prs2 != 0)
                          & (byp.bits.uop.dst_rtype == RegisterType.FIX)
                          & (rrd_uop.lrs2_rtype == RegisterType.FIX)):
                    m.d.comb += bypass_data.eq(byp.bits.data)

        for nrps, req, rs1_data, rs2_data, rs3_data in zip(
                self.rports_array, self.exec_reqs, bypass_rs1_data,
                bypass_rs2_data, rrd_rs3_data):
            if nrps > 0:
                m.d.sync += req.bits.rs1_data.eq(rs1_data)
            if nrps > 1:
                m.d.sync += req.bits.rs2_data.eq(rs2_data)
            if nrps > 2:
                m.d.sync += req.bits.rs3_data.eq(rs3_data)

        for req, rrd_uop, rrd_v in zip(self.exec_reqs, rrd_uops, rrd_valids):
            rrd_killed = self.kill | self.br_update.uop_killed(rrd_uop)

            m.d.sync += [
                req.bits.uop.eq(rrd_uop),
                req.bits.uop.br_mask.eq(
                    self.br_update.get_new_br_mask(rrd_uop.br_mask)),
                req.valid.eq(rrd_v & ~rrd_killed),
            ]

        return m
