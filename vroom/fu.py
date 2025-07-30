from amaranth import *
from amaranth import tracer
from amaranth.hdl.ast import ValueCastable

from vroom.consts import *
from vroom.types import HasVectorParams, VMicroOp
from vroom.alu import VALU
from vroom.utils import TailGen

from room.utils import Decoupled, sign_extend


class ExecReq(HasVectorParams, ValueCastable):

    def __init__(self, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.uop = VMicroOp(params, name=f'{name}__uop')

        self.vs1_data = Signal(self.vlen, name=f'{name}__vs1_data')
        self.vs2_data = Signal(self.vlen, name=f'{name}__vs2_data')
        self.vs3_data = Signal(self.vlen, name=f'{name}__vs3_data')
        self.mask = Signal(self.vlen, name=f'{name}__mask')

        self.rs1_data = Signal(self.xlen, name=f'{name}__rs1_data')
        self.rs2_data = Signal(self.xlen, name=f'{name}__rs2_data')

    @ValueCastable.lowermethod
    def as_value(self):
        return Cat(self.uop, self.vs1_data, self.vs2_data, self.vs3_data,
                   self.mask, self.rs1_data, self.rs2_data)

    def shape(self):
        return self.as_value().shape()

    def __len__(self):
        return len(Value.cast(self))

    def eq(self, rhs):
        return Value.cast(self).eq(Value.cast(rhs))


class ExecResp(HasVectorParams, ValueCastable):

    def __init__(self, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.uop = VMicroOp(params, name=f'{name}__uop')

        self.base_addr = Signal(self.xlen, name=f'{name}__base_addr')
        self.stride = Signal(self.xlen, name=f'{name}__stride')
        self.old_vd = Signal(self.vlen, name=f'{name}__old_vd')

        self.vd_data = Signal(self.vlen, name=f'{name}__vd_data')
        self.rd_data = Signal(self.xlen, name=f'{name}__rd_data')

    @ValueCastable.lowermethod
    def as_value(self):
        return Cat(self.uop, self.base_addr, self.stride, self.old_vd,
                   self.vd_data, self.rd_data)

    def shape(self):
        return self.as_value().shape()

    def __len__(self):
        return len(Value.cast(self))

    def eq(self, rhs):
        return Value.cast(self).eq(Value.cast(rhs))


class ExecLaneReq(HasVectorParams, ValueCastable):

    def __init__(self, data_width, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.data_width = data_width

        self.uop = VMicroOp(params, name=f'{name}__uop')

        self.opa_data = Signal(data_width, name=f'{name}__opa_data')
        self.opb_data = Signal(data_width, name=f'{name}__opb_data')
        self.old_vd = Signal(data_width, name=f'{name}__old_vd')

        self.tail = Signal(data_width // 8, name=f'{name}__tail')

    @ValueCastable.lowermethod
    def as_value(self):
        return Cat(self.uop, self.opa_data, self.opb_data, self.old_vd,
                   self.tail)

    def shape(self):
        return self.as_value().shape()

    def __len__(self):
        return len(Value.cast(self))

    def eq(self, rhs):
        return Value.cast(self).eq(Value.cast(rhs))


class ExecLaneResp(HasVectorParams, ValueCastable):

    def __init__(self, data_width, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.data_width = data_width

        self.uop = VMicroOp(params, name=f'{name}__uop')

        self.vd_data = Signal(self.data_width, name=f'{name}__vd_data')

    @ValueCastable.lowermethod
    def as_value(self):
        return Cat(self.uop, self.vd_data)

    def shape(self):
        return self.as_value().shape()

    def __len__(self):
        return len(Value.cast(self))

    def eq(self, rhs):
        return Value.cast(self).eq(Value.cast(rhs))


class PipelinedFunctionalUnitBase(Elaboratable):

    def __init__(self, num_stages):
        self.num_stages = num_stages

    def elaborate(self, platform):
        m = Module()

        m.d.comb += self.req.ready.eq(1)

        if self.num_stages > 0:
            self.valids = Signal(self.num_stages)
            self.uops = [
                VMicroOp(self.params, name=f's{i}_uop')
                for i in range(self.num_stages)
            ]

            m.d.sync += [
                self.valids[0].eq(self.req.valid),
                self.uops[0].eq(self.req.bits.uop),
            ]

            for i in range(1, self.num_stages):
                m.d.sync += [
                    self.valids[i].eq(self.valids[i - 1]),
                    self.uops[i].eq(self.uops[i - 1]),
                ]

            m.d.comb += [
                self.resp.valid.eq(self.valids[self.num_stages - 1]),
                self.resp.bits.uop.eq(self.uops[self.num_stages - 1]),
            ]
        else:
            m.d.comb += [
                self.resp.valid.eq(self.req.valid),
                self.resp.bits.uop.eq(self.req.bits.uop),
            ]

        return m


class LaneFunctionalUnit(HasVectorParams, Elaboratable):

    def __init__(self, data_width, params):
        super().__init__(params)

        self.req = Decoupled(ExecLaneReq, data_width, params)
        self.resp = Decoupled(ExecLaneResp, data_width, params)


class PipelinedLaneFunctionalUnit(LaneFunctionalUnit,
                                  PipelinedFunctionalUnitBase):

    def __init__(self, data_width, num_stages, params):
        LaneFunctionalUnit.__init__(self, data_width, params)
        PipelinedFunctionalUnitBase.__init__(self, num_stages)


class FunctionalUnit(HasVectorParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.req = Decoupled(ExecReq, params)
        self.resp = Decoupled(ExecResp, params)


class PipelinedFunctionalUnit(FunctionalUnit, PipelinedFunctionalUnitBase):

    def __init__(self, num_stages, params):
        FunctionalUnit.__init__(self, params)
        PipelinedFunctionalUnitBase.__init__(self, num_stages)


class PerLaneFunctionalUnit(FunctionalUnit):

    def __init__(self, params):
        super().__init__(params)

    def elaborate(self, platform):
        m = Module()

        uop = self.req.bits.uop
        sew = uop.vsew

        self.lane_reqs = [
            Decoupled(ExecLaneReq,
                      self.lane_width,
                      self.params,
                      name=f'lane_req{i}') for i in range(self.n_lanes)
        ]
        self.lane_resps = [
            Decoupled(ExecLaneResp,
                      self.lane_width,
                      self.params,
                      name=f'lane_resp{i}') for i in range(self.n_lanes)
        ]

        imm = sign_extend(uop.lrs1, self.lane_width)

        for w, lane_req in enumerate(self.lane_reqs):
            vs1_data = Signal(self.lane_width, name=f'vs1_data{w}')
            with m.If(uop.widen | uop.widen2):
                m.d.comb += vs1_data.eq(
                    Cat(self.req.bits.vs1_data[i * 32:(i + 1) * 32]
                        for i in range(w, self.vlen // 32, self.n_lanes)))
            with m.Else():
                m.d.comb += vs1_data.eq(
                    self.req.bits.vs1_data[w * self.lane_width:(w + 1) *
                                           self.lane_width])

            vs2_data = Signal(self.lane_width, name=f'vs2_data{w}')
            vs2_f2 = Cat(self.req.bits.vs2_data[i * 32:(i + 1) * 32]
                         for i in range(w, self.vlen // 32, self.n_lanes))
            vs2_f4 = Cat(self.req.bits.vs2_data[i * 16:(i + 1) * 16]
                         for i in range(w, self.vlen // 16, self.n_lanes))
            vs2_f8 = Cat(self.req.bits.vs2_data[i * 8:(i + 1) * 8]
                         for i in range(w, self.vlen // 8, self.n_lanes))
            with m.If(uop.widen | (VALUOperator.is_ext(uop.alu_fn)
                                   & (uop.lrs1[1:3] == 3))):
                m.d.comb += vs2_data.eq(vs2_f2)
            with m.Elif(
                    VALUOperator.is_ext(uop.alu_fn)
                    & (uop.lrs1[1:3] == 2)):
                m.d.comb += vs2_data.eq(vs2_f4)
            with m.Elif(
                    VALUOperator.is_ext(uop.alu_fn)
                    & (uop.lrs1[1:3] == 1)):
                m.d.comb += vs2_data.eq(vs2_f8)
            with m.Else():
                m.d.comb += vs2_data.eq(
                    self.req.bits.vs2_data[w * self.lane_width:(w + 1) *
                                           self.lane_width])

            opa_data = Signal(self.lane_width, name=f'opa_data{w}')
            opb_data = Signal(self.lane_width, name=f'opb_data{w}')
            old_vd = Signal(self.lane_width, name=f'old_vd{w}')

            with m.Switch(uop.opa_sel):
                with m.Case(VOpA.VS1):
                    m.d.comb += opa_data.eq(vs1_data)
                with m.Case(VOpA.IMM):
                    with m.Switch(sew):
                        for i in range(4):
                            with m.Case(i):
                                n = 1 << (i + 3)
                                m.d.comb += opa_data.eq(imm[:n].replicate(
                                    self.lane_width // n))

                with m.Case(VOpA.SCALAR):
                    with m.Switch(sew):
                        for i in range(4):
                            with m.Case(i):
                                n = 1 << (i + 3)
                                m.d.comb += opa_data.eq(
                                    self.req.bits.rs1_data[:n].replicate(
                                        self.lane_width // n))

            m.d.comb += [
                opb_data.eq(vs2_data),
                old_vd.eq(self.req.bits.vs3_data[w * self.lane_width:(w + 1) *
                                                 self.lane_width]),
            ]

            m.d.comb += [
                lane_req.valid.eq(self.req.valid),
                lane_req.bits.uop.eq(self.req.bits.uop),
                lane_req.bits.opa_data.eq(opa_data),
                lane_req.bits.opb_data.eq(opb_data),
                lane_req.bits.old_vd.eq(old_vd),
            ]
        m.d.comb += self.req.ready.eq(self.lane_reqs[0].ready)

        tail_gen = m.submodules.tail_gen = TailGen(self.params)
        m.d.comb += [
            tail_gen.vl.eq(self.req.bits.uop.vl),
            tail_gen.uop_idx.eq(self.req.bits.uop.expd_idx),
            tail_gen.eew.eq(self.req.bits.uop.dest_eew()),
        ]
        for w, lane_req in enumerate(self.lane_reqs):
            with m.Switch(self.req.bits.uop.dest_eew()):
                for i in range(4):
                    with m.Case(i):
                        tail_width = self.lane_width // (8 << i)
                        m.d.comb += lane_req.bits.tail.eq(
                            tail_gen.tail[w * tail_width:(w + 1) * tail_width])

        m.d.comb += [
            self.resp.valid.eq(self.lane_resps[0].valid),
            self.resp.bits.uop.eq(self.lane_resps[0].bits.uop),
        ]

        vd_cmp_out = Signal(self.vlen)
        with m.Switch(self.resp.bits.uop.vsew):
            for w in range(4):
                with m.Case(w):
                    sew = 1 << (w + 3)
                    for i in range(self.vlen):
                        if i >= self.vlen // (1 << w):
                            m.d.comb += vd_cmp_out[i].eq(1)
                        else:
                            lane_idx = (i % (self.vlen // sew)) // (
                                self.lane_width // sew)
                            lmul_idx = i // (self.vlen // sew)
                            offset = (i % (self.vlen // sew)
                                      ) - lane_idx * (self.lane_width // sew)
                            m.d.comb += vd_cmp_out[i].eq(
                                self.lane_resps[lane_idx].bits.vd_data[
                                    lmul_idx * 8 + offset])

        for w, lane_resp in enumerate(self.lane_resps):
            m.d.comb += self.resp.bits.vd_data[w * self.lane_width:(
                w + 1) * self.lane_width].eq(
                    Mux(
                        self.resp.bits.uop.narrow_to_1,
                        vd_cmp_out[w * self.lane_width:(w + 1) *
                                   self.lane_width], lane_resp.bits.vd_data))

        return m


class VALULane(PipelinedLaneFunctionalUnit):

    def __init__(self, data_width, params, num_stages=1):
        super().__init__(data_width, num_stages, params)

        self.data_width = data_width

    def elaborate(self, platform):
        m = super().elaborate(platform)

        uop = self.req.bits.uop

        alu = m.submodules.alu = VALU(self.data_width)
        m.d.comb += [
            alu.fn.eq(uop.alu_fn),
            alu.sew.eq(uop.vsew),
            alu.eew_vd.eq(uop.dest_eew()),
            alu.uop_idx.eq(uop.expd_idx),
            alu.in1.eq(self.req.bits.opa_data),
            alu.in2.eq(self.req.bits.opb_data),
            alu.in3.eq(self.req.bits.old_vd),
            alu.vmask.eq(uop.mask),
            alu.vm.eq(uop.vm),
            alu.ma.eq(uop.vma),
            alu.widen.eq(uop.widen),
            alu.widen2.eq(uop.widen2),
            alu.narrow_to_1.eq(uop.narrow_to_1),
        ]

        tail_mask = Signal(self.data_width)
        with m.If(uop.narrow_to_1):
            m.d.comb += tail_mask.eq(self.req.bits.tail)
        with m.Else():
            with m.Switch(uop.dest_eew()):
                for i in range(4):
                    with m.Case(i):
                        m.d.comb += tail_mask.eq(
                            Cat(
                                x.replicate(1 << i)
                                for x in self.req.bits.tail))

        mask_keep = Signal(self.data_width)
        mask_off_data = Signal(self.data_width)
        mask_keep_cmp = Signal(self.data_width // 8)
        mask_off_cmp = Signal(self.data_width // 8)
        for i in range(self.data_width // 8):
            with m.If(tail_mask[i]):
                m.d.comb += [
                    mask_off_data[i * 8:(i + 1) * 8].eq(
                        Mux(uop.vta, Const(~0, 8),
                            self.req.bits.old_vd[i * 8:(i + 1) * 8])),
                    mask_off_cmp[i].eq(Mux(uop.vta, 1,
                                           self.req.bits.old_vd[i])),
                ]
            with m.Else():
                m.d.comb += [
                    mask_keep[i * 8:(i + 1) * 8].eq(~0),
                    mask_keep_cmp[i].eq(1),
                ]

        cmp_out_masked = (alu.cmp_out & mask_keep_cmp) | mask_off_cmp

        s1_alu_out = Signal.like(alu.out)
        s1_mask_keep = Signal.like(mask_keep)
        s1_mask_off_data = Signal.like(mask_off_data)
        m.d.sync += [
            s1_alu_out.eq(alu.out),
            s1_mask_keep.eq(mask_keep),
            s1_mask_off_data.eq(mask_off_data),
        ]

        s1_cmp_out = [
            Signal(self.data_width // 8, name=f's1_cmp_out{i}')
            for i in range(8)
        ]
        with m.If(self.req.valid):
            with m.Switch(uop.expd_idx):
                for i in range(1, 8):
                    with m.Case(i):
                        m.d.sync += s1_cmp_out[i].eq(cmp_out_masked)

                with m.Default():
                    m.d.sync += [
                        Cat(s1_cmp_out).eq(~0),
                        s1_cmp_out[0].eq(cmp_out_masked),
                    ]

        s1_alu_out_masked = (s1_alu_out & s1_mask_keep) | s1_mask_off_data
        m.d.comb += self.resp.bits.vd_data.eq(
            Mux(self.uops[0].narrow_to_1, Cat(s1_cmp_out), s1_alu_out_masked))

        return m


class VALUUnit(PerLaneFunctionalUnit):

    def __init__(self, params, num_stages=1):
        super().__init__(params)

        self.num_stages = num_stages

    def elaborate(self, platform):
        m = super().elaborate(platform)

        for w in range(self.n_lanes):
            lane = VALULane(self.lane_width, self.params, self.num_stages)
            setattr(m.submodules, f'lane{w}', lane)
            m.d.comb += [
                self.lane_reqs[w].connect(lane.req),
                lane.resp.connect(self.lane_resps[w]),
            ]

        return m


class AddrGenUnit(PipelinedFunctionalUnit):

    def __init__(self, params):
        super().__init__(0, params)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        m.d.comb += [
            self.resp.bits.base_addr.eq(self.req.bits.rs1_data),
            self.resp.bits.stride.eq(self.req.bits.rs2_data),
            self.resp.bits.old_vd.eq(self.req.bits.vs3_data),
        ]

        return m
