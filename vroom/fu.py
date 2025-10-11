from amaranth import *
from amaranth import tracer
from amaranth.hdl.ast import ValueCastable
from amaranth.utils import log2_int
import functools
import operator

from vroom.consts import *
from vroom.types import HasVectorParams, VMicroOp
from vroom.alu import VALU, VFixPointALU, VMultiplier, VIntDiv, CSA3to2, ReductionSlice, Compare2to1, Compare3to1, VMask
from vroom.fpu import VFPUFMA, VFPUComp, VFPUDivSqrt
from vroom.utils import TailGen, vlmul_to_lmul

from room.consts import RegisterType, RoundingMode
from room.fpu import FPUOperator, FPFormat, FPUCastMulti
from room.utils import Decoupled, Pipe, sign_extend, bit_extend


def get_byte_mask(m, mask_out, mask, dest_eew):
    with m.Switch(dest_eew):
        for i in range(4):
            with m.Case(i):
                m.d.comb += mask_out.eq(Cat(x.replicate(1 << i) for x in mask))


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

    @ValueCastable.lowermethod
    def as_value(self):
        return Cat(self.uop, self.base_addr, self.stride, self.old_vd,
                   self.vd_data)

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
        self.mask = Signal(data_width // 8, name=f'{name}__mask')

    @ValueCastable.lowermethod
    def as_value(self):
        return Cat(self.uop, self.opa_data, self.opb_data, self.old_vd,
                   self.tail, self.mask)

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

    def __init__(self, num_stages, is_redu=False):
        self.num_stages = num_stages
        self.is_redu = is_redu

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


class IterativeFunctionalUnitBase(Elaboratable):

    def elaborate(self, platform):
        m = Module()

        uop = VMicroOp(self.params)

        with m.If(self.req.fire):
            m.d.sync += uop.eq(self.req.bits.uop)

        m.d.comb += self.resp.bits.uop.eq(uop)

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


class IterativeLaneFunctionalUnit(LaneFunctionalUnit,
                                  IterativeFunctionalUnitBase):

    def __init__(self, data_width, params):
        LaneFunctionalUnit.__init__(self, data_width, params)


class FunctionalUnit(HasVectorParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.req = Decoupled(ExecReq, params)
        self.resp = Decoupled(ExecResp, params)


class PipelinedFunctionalUnit(FunctionalUnit, PipelinedFunctionalUnitBase):

    def __init__(self, num_stages, params):
        FunctionalUnit.__init__(self, params)
        PipelinedFunctionalUnitBase.__init__(self, num_stages)


class IterativeFunctionalUnit(FunctionalUnit, IterativeFunctionalUnitBase):

    def __init__(self, params):
        FunctionalUnit.__init__(self, params)
        IterativeFunctionalUnitBase.__init__(self)


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

        #
        # Input data arrangement
        #

        imm = sign_extend(uop.lrs1, self.lane_width)

        for w, lane_req in enumerate(self.lane_reqs):
            vs1_data = Signal(self.lane_width, name=f'vs1_data{w}')
            with m.If(uop.widen | uop.widen2 | uop.narrow):
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

                with m.Case(VOpA.VS1_HALF):
                    m.d.comb += opa_data.eq(
                        Cat(
                            self.req.bits.vs1_data[w * self.lane_width //
                                                   2:(w + 1) *
                                                   self.lane_width // 2],
                            self.req.bits.vs1_data[(w + 2) * self.lane_width //
                                                   2:(w + 3) *
                                                   self.lane_width // 2]))
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

            with m.Switch(uop.opb_sel):
                with m.Case(VOpB.VS2):
                    m.d.comb += opb_data.eq(vs2_data)

                with m.Case(VOpB.VS2_HALF):
                    m.d.comb += opb_data.eq(
                        Cat(
                            self.req.bits.vs2_data[w * self.lane_width //
                                                   2:(w + 1) *
                                                   self.lane_width // 2],
                            self.req.bits.vs2_data[(w + 2) * self.lane_width //
                                                   2:(w + 3) *
                                                   self.lane_width // 2]))

                with m.Case(VOpB.OLD_VD):
                    m.d.comb += opb_data.eq(
                        self.req.bits.vs3_data[w * self.lane_width:(w + 1) *
                                               self.lane_width])

            with m.Switch(uop.opc_sel):
                with m.Case(VOpC.OLD_VD):
                    m.d.comb += old_vd.eq(
                        self.req.bits.vs3_data[w * self.lane_width:(w + 1) *
                                               self.lane_width])

                with m.Case(VOpC.VS2):
                    m.d.comb += old_vd.eq(vs2_data)

            m.d.comb += [
                lane_req.valid.eq(
                    Cat(self.req.valid & req.ready
                        for i, req in enumerate(self.lane_reqs)
                        if i != w).all()),
                lane_req.bits.uop.eq(self.req.bits.uop),
                lane_req.bits.opa_data.eq(opa_data),
                lane_req.bits.opb_data.eq(opb_data),
                lane_req.bits.old_vd.eq(old_vd),
            ]
        m.d.comb += self.req.ready.eq(
            Cat(req.ready for req in self.lane_reqs).all())

        #
        # Tail distribution
        #

        tail_gen = m.submodules.tail_gen = TailGen(self.params)
        m.d.comb += [
            tail_gen.vl.eq(self.req.bits.uop.vl),
            tail_gen.uop_idx.eq(self.req.bits.uop.expd_idx),
            tail_gen.eew.eq(self.req.bits.uop.dest_eew()),
            tail_gen.narrow.eq(self.req.bits.uop.narrow),
        ]
        for w, lane_req in enumerate(self.lane_reqs):
            lane_tail = Signal(self.lane_width // 8, name=f'lane_tail{w}')
            lane_tail_narrow = Signal(self.lane_width // 8,
                                      name=f'lane_tail_narrow{w}')

            with m.Switch(self.req.bits.uop.dest_eew()):
                for i in range(4):
                    with m.Case(i):
                        tail_width = self.lane_width // (8 << i)
                        m.d.comb += lane_tail.eq(
                            tail_gen.tail[w * tail_width:(w + 1) * tail_width])

            with m.Switch(self.req.bits.uop.dest_eew()):
                for i in range(3):
                    with m.Case(i):
                        tail_width = self.lane_width // (16 << i)
                        m.d.comb += lane_tail_narrow.eq(
                            Cat(
                                tail_gen.tail[w * tail_width:(w + 1) *
                                              tail_width],
                                tail_gen.tail[(w + self.n_lanes) *
                                              tail_width:(w + self.n_lanes +
                                                          1) * tail_width]))

            m.d.comb += lane_req.bits.tail.eq(
                Mux(self.req.bits.uop.narrow, lane_tail_narrow, lane_tail))

        #
        # Mask distribution
        #

        mask = Signal(self.vlen_bytes)
        with m.Switch(self.req.bits.uop.expd_idx >> self.req.bits.uop.narrow):
            for i in range(8):
                with m.Case(i):
                    with m.Switch(self.req.bits.uop.dest_eew()):
                        for w in range(4):
                            with m.Case(w):
                                n = self.vlen_bytes // (1 << w)
                                m.d.comb += mask.eq(
                                    self.req.bits.mask[i * n:(i + 1) * n])

        for w, lane_req in enumerate(self.lane_reqs):
            lane_mask = Signal(self.lane_width // 8, name=f'lane_mask{w}')
            lane_mask_narrow = Signal(self.lane_width // 8,
                                      name=f'lane_mask_narrow{w}')

            with m.Switch(self.req.bits.uop.dest_eew()):
                for i in range(4):
                    with m.Case(i):
                        mask_width = self.lane_width // (8 << i)
                        m.d.comb += lane_mask.eq(mask[w * mask_width:(w + 1) *
                                                      mask_width])

            with m.Switch(self.req.bits.uop.dest_eew()):
                for i in range(3):
                    with m.Case(i):
                        mask_width = self.lane_width // (16 << i)
                        m.d.comb += lane_mask_narrow.eq(
                            Cat(
                                mask[w * mask_width:(w + 1) * mask_width],
                                mask[(w + self.n_lanes) *
                                     mask_width:(w + self.n_lanes + 1) *
                                     mask_width]))

            m.d.comb += lane_req.bits.mask.eq(
                Mux(self.req.bits.uop.narrow, lane_mask_narrow, lane_mask))

        m.d.comb += [
            self.resp.valid.eq(
                Cat(resp.valid for resp in self.lane_resps).all()),
            self.resp.bits.uop.eq(self.lane_resps[0].bits.uop),
        ]

        for w, lane_resp in enumerate(self.lane_resps):
            m.d.comb += lane_resp.ready.eq(
                Cat(resp.valid & self.resp.ready
                    for i, resp in enumerate(self.lane_resps) if i != w))

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
            lane_vd_data = Signal(self.lane_width, name=f'lane_vd_data{w}')
            with m.If(self.resp.bits.uop.narrow):
                if w < self.n_lanes // 2:
                    m.d.comb += lane_vd_data.eq(
                        Cat(
                            self.lane_resps[2 *
                                            w].bits.vd_data[:self.lane_width //
                                                            2],
                            self.lane_resps[2 * w +
                                            1].bits.vd_data[:self.lane_width //
                                                            2]))
                else:
                    m.d.comb += lane_vd_data.eq(
                        Cat(
                            self.lane_resps[2 * w - self.n_lanes].bits.
                            vd_data[self.lane_width // 2:],
                            self.lane_resps[2 * w + 1 - self.n_lanes].bits.
                            vd_data[self.lane_width // 2:]))
            with m.Else():
                m.d.comb += lane_vd_data.eq(lane_resp.bits.vd_data)

            m.d.comb += self.resp.bits.vd_data[w * self.lane_width:(
                w + 1) * self.lane_width].eq(
                    Mux(
                        self.resp.bits.uop.narrow_to_1,
                        vd_cmp_out[w * self.lane_width:(w + 1) *
                                   self.lane_width], lane_vd_data))

        return m


class MaskDataGen(HasVectorParams, Elaboratable):

    def __init__(self, data_width, params):
        super().__init__(params)
        self.data_width = data_width

        self.mask = Signal(data_width // 8)
        self.tail = Signal(data_width // 8)
        self.old_vd = Signal(data_width)
        self.uop = VMicroOp(params)

        self.mask_keep = Signal(data_width)
        self.mask_off = Signal(data_width)

        self.mask_keep_cmp = Signal(data_width // 8)
        self.mask_off_cmp = Signal(data_width // 8)

    def elaborate(self, platform):
        m = Module()

        for i in range(self.data_width // 8):
            with m.If(self.tail[i]):
                with m.If(self.uop.vta | self.uop.narrow_to_1):
                    m.d.comb += [
                        self.mask_off[i * 8:(i + 1) * 8].eq(~0),
                        self.mask_off_cmp[i].eq(1),
                    ]
                with m.Else():
                    m.d.comb += [
                        self.mask_off[i * 8:(i + 1) * 8].eq(
                            self.old_vd[i * 8:(i + 1) * 8]),
                        self.mask_off_cmp[i].eq(self.old_vd[i]),
                    ]

            with m.Elif(
                    VALUOperator.is_add_with_carry(self.uop.alu_fn)
                    | VALUOperator.is_vmerge(self.uop.alu_fn)):
                m.d.comb += [
                    self.mask_keep[i * 8:(i + 1) * 8].eq(~0),
                    self.mask_keep_cmp[i].eq(1),
                ]

            with m.Elif(~self.uop.vm & ~self.mask[i]):
                with m.If(self.uop.vma):
                    m.d.comb += [
                        self.mask_off[i * 8:(i + 1) * 8].eq(~0),
                        self.mask_off_cmp[i].eq(1),
                    ]
                with m.Else():
                    m.d.comb += [
                        self.mask_off[i * 8:(i + 1) * 8].eq(
                            self.old_vd[i * 8:(i + 1) * 8]),
                        self.mask_off_cmp[i].eq(self.old_vd[i]),
                    ]

            with m.Else():
                m.d.comb += [
                    self.mask_keep[i * 8:(i + 1) * 8].eq(~0),
                    self.mask_keep_cmp[i].eq(1),
                ]

        return m


class VALULane(PipelinedLaneFunctionalUnit):

    def __init__(self, data_width, params, num_stages=2):
        super().__init__(data_width, num_stages, params)

        self.data_width = data_width

    def elaborate(self, platform):
        m = super().elaborate(platform)

        uop = self.req.bits.uop
        is_fixp = VALUOperator.is_fixp(uop.alu_fn)

        alu = m.submodules.alu = VALU(self.data_width)
        m.d.comb += [
            alu.fn.eq(uop.alu_fn),
            alu.sew.eq(uop.vsew),
            alu.eew_vd.eq(uop.dest_eew()),
            alu.uop_idx.eq(uop.expd_idx),
            alu.in1.eq(self.req.bits.opa_data),
            alu.in2.eq(self.req.bits.opb_data),
            alu.in3.eq(self.req.bits.old_vd),
            alu.vmask.eq(self.req.bits.mask),
            alu.vm.eq(uop.vm),
            alu.ma.eq(uop.vma),
            alu.vi.eq(uop.opa_sel == VOpA.IMM),
            alu.widen.eq(uop.widen),
            alu.widen2.eq(uop.widen2),
            alu.narrow.eq(uop.narrow),
            alu.narrow_to_1.eq(uop.narrow_to_1),
        ]

        tail_mask = Signal(self.data_width)
        byte_mask = Signal(self.data_width)
        get_byte_mask(m, tail_mask, self.req.bits.tail, uop.dest_eew())
        get_byte_mask(m, byte_mask, self.req.bits.mask, uop.dest_eew())
        with m.If(uop.narrow_to_1):
            m.d.comb += [
                tail_mask.eq(self.req.bits.tail),
                byte_mask.eq(self.req.bits.mask),
            ]

        mask_gen = m.submodules.mask_gen = MaskDataGen(self.data_width,
                                                       self.params)
        m.d.comb += [
            mask_gen.mask.eq(byte_mask),
            mask_gen.tail.eq(tail_mask),
            mask_gen.old_vd.eq(self.req.bits.old_vd),
            mask_gen.uop.eq(uop),
        ]

        cmp_out_masked = (alu.cmp_out
                          & mask_gen.mask_keep_cmp) | mask_gen.mask_off_cmp

        s1_uop = self.uops[0]
        s1_alu_out = Signal.like(alu.out)
        s1_is_fixp = Signal()
        s1_mask_keep = Signal.like(mask_gen.mask_keep)
        s1_mask_off_data = Signal.like(mask_gen.mask_off)
        m.d.sync += [
            s1_alu_out.eq(alu.out),
            s1_is_fixp.eq(is_fixp),
            s1_mask_keep.eq(mask_gen.mask_keep),
            s1_mask_off_data.eq(mask_gen.mask_off),
        ]

        s1_cmp_out = [
            Signal(self.data_width // 8, name=f's1_cmp_out{i}')
            for i in range(8)
        ]
        s1_narrow_out = Signal(self.data_width)
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

            m.d.sync += s1_narrow_out.eq(
                Mux(uop.expd_idx[0],
                    Cat(s1_narrow_out[:self.data_width // 2], alu.narrow_out),
                    alu.narrow_out))

        s1_alu_out_masked = (Mux(s1_uop.narrow, s1_narrow_out, s1_alu_out)
                             & s1_mask_keep) | s1_mask_off_data
        s1_int_out = Mux(
            s1_uop.dst_rtype != RegisterType.VEC, s1_alu_out,
            Mux(s1_uop.narrow_to_1, Cat(s1_cmp_out), s1_alu_out_masked))

        s1_adder_out = Signal.like(alu.adder_out)
        s1_adder_cout = Signal.like(alu.adder_cout)
        s1_alu_in1h = Signal.like(alu.in1h)
        s1_alu_in2h = Signal.like(alu.in2h)
        s1_shift_out = Signal.like(alu.shift_out)
        s1_round_high = Signal.like(alu.round_high)
        s1_round_tail = Signal.like(alu.round_tail)
        m.d.sync += [
            s1_adder_out.eq(alu.adder_out),
            s1_adder_cout.eq(alu.adder_cout),
            s1_alu_in1h.eq(alu.in1h),
            s1_alu_in2h.eq(alu.in2h),
            s1_shift_out.eq(alu.shift_out),
            s1_round_high.eq(alu.round_high),
            s1_round_tail.eq(alu.round_tail),
        ]

        fixp = m.submodules.fixp = VFixPointALU(self.data_width)
        m.d.comb += [
            fixp.fn.eq(s1_uop.alu_fn),
            fixp.sew.eq(s1_uop.vsew),
            fixp.vxrm.eq(s1_uop.vxrm),
            fixp.alu_out.eq(s1_adder_out),
            fixp.alu_cout.eq(s1_adder_cout),
            fixp.in1h.eq(s1_alu_in1h),
            fixp.in2h.eq(s1_alu_in2h),
            fixp.alu_shift_out.eq(s1_shift_out),
            fixp.round_high.eq(s1_round_high),
            fixp.round_tail.eq(s1_round_tail),
        ]

        s2_uop = self.uops[1]
        s2_int_out = Signal(self.data_width)
        s2_fixp_out = Signal(self.data_width)
        s2_is_fixp = Signal()
        s2_mask_keep = Signal.like(s1_mask_keep)
        s2_mask_off_data = Signal.like(s1_mask_off_data)
        s2_fixp_narrow_out = Signal(self.data_width)
        m.d.sync += [
            s2_int_out.eq(s1_int_out),
            s2_fixp_out.eq(fixp.out),
            s2_is_fixp.eq(s1_is_fixp),
            s2_mask_keep.eq(s1_mask_keep),
            s2_mask_off_data.eq(s1_mask_off_data),
            s2_fixp_narrow_out.eq(
                Mux(
                    s1_uop.expd_idx[0],
                    Cat(s2_fixp_narrow_out[:self.data_width // 2],
                        fixp.narrow_out), fixp.narrow_out)),
        ]

        s2_fixp_out_masked = (
            Mux(s2_uop.narrow, s2_fixp_narrow_out, s2_fixp_out)
            & s2_mask_keep) | s2_mask_off_data

        m.d.comb += self.resp.bits.vd_data.eq(
            Mux(s2_is_fixp, s2_fixp_out_masked, s2_int_out))

        return m


class VALUUnit(PerLaneFunctionalUnit):

    def __init__(self, params, num_stages=2):
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


class VMultiplierLane(PipelinedLaneFunctionalUnit):

    def __init__(self, data_width, params, num_stages):
        super().__init__(data_width, num_stages, params)

        self.data_width = data_width

    def elaborate(self, platform):
        m = super().elaborate(platform)

        uop = self.req.bits.uop

        mul = m.submodules.mul = VMultiplier(self.data_width, self.num_stages)
        m.d.comb += [
            mul.valid.eq(self.req.valid),
            mul.fn.eq(uop.alu_fn),
            mul.sew.eq(uop.vsew),
            mul.vxrm.eq(uop.vxrm),
            mul.uop_idx.eq(uop.expd_idx),
            mul.in1.eq(self.req.bits.opa_data),
            mul.in2.eq(self.req.bits.opb_data),
            mul.in3.eq(self.req.bits.old_vd),
            mul.widen.eq(uop.widen),
        ]

        pipe_in = Cat(self.req.bits.old_vd, self.req.bits.tail,
                      self.req.bits.mask)
        out_old_vd = Signal.like(self.req.bits.old_vd)
        out_tail = Signal.like(self.req.bits.tail)
        out_mask = Signal.like(self.req.bits.mask)
        out_pipe = m.submodules.out_pipe = Pipe(width=len(pipe_in),
                                                depth=self.num_stages)
        m.d.comb += [
            out_pipe.in_valid.eq(self.req.valid),
            out_pipe.in_data.eq(pipe_in),
            Cat(out_old_vd, out_tail, out_mask).eq(out_pipe.out.bits),
        ]

        tail_mask = Signal(self.data_width)
        byte_mask = Signal(self.data_width)
        get_byte_mask(m, tail_mask, out_tail, self.resp.bits.uop.dest_eew())
        get_byte_mask(m, byte_mask, out_mask, self.resp.bits.uop.dest_eew())

        mask_gen = m.submodules.mask_gen = MaskDataGen(self.data_width,
                                                       self.params)
        m.d.comb += [
            mask_gen.mask.eq(byte_mask),
            mask_gen.tail.eq(tail_mask),
            mask_gen.old_vd.eq(out_old_vd),
            mask_gen.uop.eq(self.resp.bits.uop),
        ]

        m.d.comb += self.resp.bits.vd_data.eq((mul.resp_data
                                               & mask_gen.mask_keep)
                                              | mask_gen.mask_off)

        return m


class VMultiplierUnit(PerLaneFunctionalUnit):

    def __init__(self, latency, params):
        super().__init__(params)

        self.latency = latency

    def elaborate(self, platform):
        m = super().elaborate(platform)

        for w in range(self.n_lanes):
            lane = VMultiplierLane(self.lane_width, self.params, self.latency)
            setattr(m.submodules, f'lane{w}', lane)
            m.d.comb += [
                self.lane_reqs[w].connect(lane.req),
                lane.resp.connect(self.lane_resps[w]),
            ]

        return m


class VDivLane(IterativeLaneFunctionalUnit):

    def __init__(self, data_width, params):
        super().__init__(data_width, params)

        self.data_width = data_width

    def elaborate(self, platform):
        m = super().elaborate(platform)

        uop = self.req.bits.uop

        div = m.submodules.div = VIntDiv(self.data_width)
        m.d.comb += [
            div.valid.eq(self.req.valid),
            self.req.ready.eq(div.ready),
            div.fn.eq(uop.alu_fn),
            div.sew.eq(uop.vsew),
            div.in1.eq(self.req.bits.opa_data),
            div.in2.eq(self.req.bits.opb_data),
            self.resp.valid.eq(div.resp.valid),
            div.resp.ready.eq(self.resp.ready),
        ]

        old_vd = Signal.like(self.req.bits.old_vd)
        tail = Signal.like(self.req.bits.tail)
        mask = Signal.like(self.req.bits.mask)
        with m.If(self.req.fire):
            m.d.sync += [
                old_vd.eq(self.req.bits.old_vd),
                tail.eq(self.req.bits.tail),
                mask.eq(self.req.bits.mask),
            ]

        tail_mask = Signal(self.data_width)
        byte_mask = Signal(self.data_width)
        get_byte_mask(m, tail_mask, tail, self.resp.bits.uop.dest_eew())
        get_byte_mask(m, byte_mask, mask, self.resp.bits.uop.dest_eew())

        mask_gen = m.submodules.mask_gen = MaskDataGen(self.data_width,
                                                       self.params)
        m.d.comb += [
            mask_gen.mask.eq(byte_mask),
            mask_gen.tail.eq(tail_mask),
            mask_gen.old_vd.eq(old_vd),
            mask_gen.uop.eq(self.resp.bits.uop),
        ]

        m.d.comb += self.resp.bits.vd_data.eq((div.resp.bits
                                               & mask_gen.mask_keep)
                                              | mask_gen.mask_off)

        return m


class VDivUnit(PerLaneFunctionalUnit):

    def __init__(self, params):
        super().__init__(params)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        for w in range(self.n_lanes):
            lane = VDivLane(self.lane_width, self.params)
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


class VReductionUnit(PipelinedFunctionalUnit):

    def __init__(self, params):
        super().__init__(3, params)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        vd_reg = Signal(self.vlen)

        s0_vsew = self.req.bits.uop.vsew
        s0_vm = self.req.bits.uop.vm
        s0_vmask = self.req.bits.mask
        s0_vl = self.req.bits.uop.vl
        s0_opc = self.req.bits.uop.opcode
        s0_uop_idx = self.req.bits.uop.expd_idx
        s0_widen = self.req.bits.uop.widen

        s0_vmask_vl = s0_vmask & (Const(~0, self.vlen) >>
                                  (self.vlen - s0_vl).as_unsigned())
        s0_vmask_uop = Signal(self.vlen_bytes)
        with m.Switch(s0_vsew):
            for w in range(4):
                with m.Case(w):
                    n = 1 << (3 + w)
                    stride = self.vlen // n

                    with m.Switch(s0_uop_idx):
                        for i in range(8):
                            with m.Case(i):
                                m.d.comb += s0_vmask_uop.eq(
                                    s0_vmask_vl[i * stride:(i + 1) * stride])

        vl_rem = Signal(self.vl_bits)
        with m.If(s0_vl > (
            (s0_uop_idx << log2_int(self.vlen_bytes)) >> s0_vsew)):
            m.d.comb += vl_rem.eq(s0_vl - (
                (s0_uop_idx << log2_int(self.vlen_bytes)) >> s0_vsew))

        vs2_masked_data = Signal(self.vlen)
        with m.Switch(s0_vsew):
            for w in range(4):
                with m.Case(w):
                    n = 1 << (3 + w)
                    for i in range(self.vlen // n):
                        with m.If((~s0_vm & ~s0_vmask_uop[i]) | (i >= vl_rem)):
                            with m.Switch(s0_opc):
                                with m.Case(VOpCode.VREDMAX):
                                    m.d.comb += vs2_masked_data[i * n:(i + 1) *
                                                                n].eq(1 << (n -
                                                                            1))
                                with m.Case(VOpCode.VREDMIN):
                                    m.d.comb += vs2_masked_data[i * n:(
                                        i + 1) * n].eq((1 << (n - 1)) - 1)
                                with m.Case(VOpCode.VREDMINU, VOpCode.VREDAND):
                                    m.d.comb += vs2_masked_data[i * n:(i + 1) *
                                                                n].eq(~0)

                        with m.Else():
                            m.d.comb += vs2_masked_data[i * n:(i + 1) * n].eq(
                                self.req.bits.vs2_data[i * n:(i + 1) * n])

        vs2_masked_data_widen = Signal(self.vlen * 2)
        m.d.comb += vs2_masked_data_widen.eq(vs2_masked_data)
        with m.If(s0_widen):
            with m.Switch(s0_vsew):
                for w in range(3):
                    with m.Case(w):
                        n = 1 << (3 + w)
                        for i in range(self.vlen // n):
                            m.d.comb += vs2_masked_data_widen[i * 2 * n:(
                                i + 1) * 2 * n].eq(
                                    bit_extend(
                                        vs2_masked_data[i * n:(i + 1) * n],
                                        2 * n, s0_opc[0]))

        vs1_masked_data = Signal(self.elen)
        with m.Switch(s0_vsew):
            for w in range(4):
                with m.Case(w):
                    n = 1 << (3 + w)
                    m.d.comb += vs1_masked_data.eq(
                        Cat(self.req.bits.vs1_data[:n],
                            (s0_opc == VOpCode.VREDAND).replicate(self.elen -
                                                                  n)))

        s1_vs2_data_widen = Signal.like(vs2_masked_data_widen)
        s1_vs1_data = Signal(self.elen)
        s1_old_vd = Signal(self.vlen)
        with m.If(self.req.fire):
            m.d.sync += [
                s1_vs2_data_widen.eq(vs2_masked_data_widen),
                s1_vs1_data.eq(vs1_masked_data),
                s1_old_vd.eq(self.req.bits.vs3_data),
            ]

        slices = []
        slice_width = 128
        n_slices = self.vlen // slice_width
        slice_vd = Signal(self.vlen)
        for i in range(n_slices):
            slice = ReductionSlice(slice_width)
            setattr(m.submodules, f'slice{i}', slice)
            m.d.comb += [
                slice.valid.eq(self.valids[0]),
                slice.opcode.eq(self.uops[0].opcode),
                slice.widen.eq(self.uops[0].widen),
                slice.sew.eq(self.uops[0].vsew),
                slice.in_data.eq(
                    Mux(
                        self.uops[0].widen,
                        s1_vs2_data_widen[i * 2 * slice_width:(i + 1) * 2 *
                                          slice_width],
                        Cat(
                            s1_vs2_data_widen[i * slice_width:(i + 1) *
                                              slice_width],
                            Const(0, slice_width)))),
                slice_vd[i * slice_width:(i + 1) * slice_width].eq(
                    slice.resp_data),
            ]

            slices.append(slice)

        s2_vs1_data = Signal(self.elen)
        s2_old_vd = Signal(self.vlen)
        with m.If(self.valids[0]):
            m.d.sync += [
                s2_vs1_data.eq(s1_vs1_data),
                s2_old_vd.eq(s1_old_vd),
            ]

        s2_vd_masked_data = Signal(self.elen)
        with m.Switch(self.uops[1].vsew):
            for w in range(4):
                with m.Case(w):
                    n = 1 << (3 + w)
                    m.d.comb += s2_vd_masked_data.eq(
                        Cat(vd_reg[:n], (self.uops[1].opcode == VOpCode.VREDAND
                                         ).replicate(self.elen - n)))

        s2_vs1_vd = Signal(self.elen)
        with m.If(self.uops[1].expd_idx.any()):
            m.d.comb += s2_vs1_vd.eq(s2_vd_masked_data)
        with m.Else():
            m.d.comb += s2_vs1_vd.eq(s2_vs1_data)

        #
        # VREDAND, VREDOR, VREDXOR
        #

        logic_out = Signal(self.vlen)
        logic_operands = [
            slice_vd[i * 64:(i + 1) * 64] for i in range(len(slice_vd) // 64)
        ]
        logic_operands.append(s2_vs1_vd)
        with m.Switch(self.uops[1].opcode):
            with m.Case(VOpCode.VREDAND):
                m.d.comb += logic_out.eq(
                    functools.reduce(operator.and_, logic_operands))
            with m.Case(VOpCode.VREDOR):
                m.d.comb += logic_out.eq(
                    functools.reduce(operator.or_, logic_operands))
            with m.Case(VOpCode.VREDXOR):
                m.d.comb += logic_out.eq(
                    functools.reduce(operator.xor, logic_operands))

        #
        # VREDSUM
        #

        adder_out = [Signal(self.vlen, name=f'adder_out{w}') for w in range(4)]
        for w in range(4):
            n = 1 << (3 + w)
            addens = []
            for i in range(n_slices):
                addens.append(slice_vd[i * slice_width:i * slice_width + n])
                addens.append(slice_vd[i * slice_width + n:i * slice_width +
                                       2 * n])
            addens.append(s2_vs1_vd)

            while len(addens) > 2:
                groups = [addens[i:i + 3] for i in range(0, len(addens), 3)]
                rem = []
                if len(groups[-1]) < 3:
                    groups, rem = groups[:-1], groups[-1]

                cout = [Signal(n) for _ in range(len(groups))]
                sum = [Signal(n) for _ in range(len(groups))]
                for group, co, s in zip(groups, cout, sum):
                    a, b, c = group
                    csa = CSA3to2(n)
                    m.submodules += csa

                    m.d.comb += [
                        csa.a.eq(a),
                        csa.b.eq(b),
                        csa.c.eq(c),
                        co.eq(csa.cout),
                        s.eq(csa.sum),
                    ]

                addens = sum + cout + rem

            assert len(addens) == 2
            m.d.comb += adder_out[w].eq(addens[0] + addens[1])

        #
        # VREDMINU, VREDMIN, VREDMAXU, VREDMAX
        #

        s2_is_max = (self.uops[1].opcode == VOpCode.VREDMAXU) | (
            self.uops[1].opcode == VOpCode.VREDMAX)
        s2_unsigned = (self.uops[1].opcode == VOpCode.VREDMAXU) | (
            self.uops[1].opcode == VOpCode.VREDMINU)
        minmax = [Signal(self.vlen, name=f'minmax{w}') for w in range(4)]
        for w in range(4):
            n = 1 << (3 + w)
            comparands = []
            for i in range(n_slices):
                comparands.append(slice_vd[i * slice_width:i * slice_width +
                                           n])
            comparands.append(s2_vs1_vd[:n])

            while len(comparands) > 1:
                new_comparands = []

                if len(comparands) == 2:
                    comp2 = Compare2to1(width=n)
                    m.submodules += comp2
                    m.d.comb += [
                        comp2.a.eq(comparands[0]),
                        comp2.b.eq(comparands[1]),
                        comp2.max.eq(s2_is_max),
                        comp2.unsigned.eq(s2_unsigned),
                    ]

                    new_comparands.append(comp2.out)

                else:
                    groups = [
                        comparands[i:i + 3]
                        for i in range(0, len(comparands), 3)
                    ]
                    rem = []
                    if len(groups[-1]) < 3:
                        groups, rem = groups[:-1], groups[-1]

                    for a, b, c in groups:
                        comp3 = Compare3to1(width=n)
                        m.submodules += comp3
                        m.d.comb += [
                            comp3.a.eq(a),
                            comp3.b.eq(b),
                            comp3.c.eq(c),
                            comp3.max.eq(s2_is_max),
                            comp3.unsigned.eq(s2_unsigned),
                        ]
                        new_comparands.append(comp3.out)

                    new_comparands.extend(rem)

                comparands = new_comparands

            assert len(comparands) == 1
            m.d.comb += minmax[w].eq(comparands[0])

        s3_old_vd = Signal(self.vlen)
        with m.If(self.valids[1]):
            m.d.sync += s3_old_vd.eq(s2_old_vd)

        with m.If(self.valids[1]):
            with m.Switch(self.uops[1].opcode):
                with m.Case(VOpCode.VREDAND, VOpCode.VREDOR, VOpCode.VREDXOR):
                    m.d.sync += vd_reg.eq(logic_out)

                with m.Case(VOpCode.VREDMINU, VOpCode.VREDMIN,
                            VOpCode.VREDMAXU, VOpCode.VREDMAX):
                    with m.Switch(self.uops[1].vsew):
                        for w in range(4):
                            with m.Case(w):
                                m.d.sync += vd_reg.eq(minmax[w])

                with m.Default():
                    with m.Switch(self.uops[1].vsew + self.uops[1].widen):
                        for w in range(4):
                            with m.Case(w):
                                m.d.sync += vd_reg.eq(adder_out[w])

        vd_mask = Signal(self.vlen)
        vd_masked_data = Signal(self.vlen)
        with m.Switch(self.uops[2].vsew + self.uops[2].widen):
            for w in range(4):
                with m.Case(w):
                    n = 1 << (3 + w)
                    m.d.comb += vd_mask.eq((1 << n) - 1)

        with m.If(self.uops[2].vta):
            m.d.comb += vd_masked_data.eq(vd_reg | ~vd_mask)
        with m.Else():
            m.d.comb += vd_masked_data.eq((vd_reg & vd_mask)
                                          | (s3_old_vd & ~vd_mask))

        m.d.comb += self.resp.bits.vd_data.eq(vd_masked_data)

        return m


class VMaskUnit(IterativeFunctionalUnit):

    def __init__(self, params):
        super().__init__(params)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        is_vcpop = self.req.bits.uop.opcode == VOpCode.VCPOP
        cpop_busy = Signal()
        m.d.comb += self.req.ready.eq(~cpop_busy)

        lmul_reg = Signal(range(8))
        cpop_uop_idx = Signal(range(8))
        cpop_done = (self.req.fire & is_vcpop &
                     (self.req.bits.uop.vlmul_sign
                      | ~self.req.bits.uop.vlmul_mag.any())) | (
                          cpop_busy & (cpop_uop_idx == lmul_reg))

        with m.If(self.req.fire & is_vcpop & ~self.req.bits.uop.vlmul_sign
                  & self.req.bits.uop.vlmul_mag.any()):
            m.d.sync += [
                cpop_busy.eq(1),
                lmul_reg.eq(
                    vlmul_to_lmul(self.req.bits.uop.vlmul_sign,
                                  self.req.bits.uop.vlmul_mag) - 1),
                cpop_uop_idx.eq(1),
            ]
        with m.Elif(cpop_done):
            m.d.sync += cpop_busy.eq(0)

        with m.If(cpop_busy):
            m.d.sync += cpop_uop_idx.eq(cpop_uop_idx + 1)

        vs2_reg = Signal(self.vlen)
        old_vd_reg = Signal(self.vlen)
        mask_reg = Signal(self.vlen)
        with m.If(self.req.fire):
            m.d.sync += [
                vs2_reg.eq(self.req.bits.vs2_data),
                old_vd_reg.eq(self.req.bits.vs3_data),
                mask_reg.eq(self.req.bits.mask),
            ]

        vl_mask = Signal(self.vlen)
        vl_mask_reg = Signal.like(vl_mask)
        tail_mask = Signal(self.vlen)
        m.d.comb += [
            vl_mask.eq((1 << self.req.bits.uop.vl) - 1),
            tail_mask.eq(~vl_mask),
        ]
        with m.If(self.req.fire):
            m.d.sync += vl_mask_reg.eq(vl_mask)

        vid_mask = Signal(self.vlen_bytes)
        with m.Switch(self.req.bits.uop.expd_idx):
            for i in range(8):
                with m.Case(i):
                    with m.Switch(self.req.bits.uop.dest_eew()):
                        for w in range(4):
                            with m.Case(w):
                                n = self.vlen_bytes // (1 << w)
                                m.d.comb += vid_mask.eq(
                                    self.req.bits.mask[i * n:(i + 1) * n])

        vid_tail = Signal(self.vlen_bytes)
        tail_gen = m.submodules.tail_gen = TailGen(self.params)
        m.d.comb += [
            tail_gen.vl.eq(self.req.bits.uop.vl),
            tail_gen.uop_idx.eq(self.req.bits.uop.expd_idx),
            tail_gen.eew.eq(self.req.bits.uop.dest_eew()),
            tail_gen.narrow.eq(self.req.bits.uop.narrow),
            vid_tail.eq(tail_gen.tail),
        ]

        vid_mask_reg = Signal.like(vid_mask)
        vid_tail_reg = Signal.like(vid_tail)
        m.d.sync += [
            vid_mask_reg.eq(vid_mask),
            vid_tail_reg.eq(vid_tail),
        ]

        vmask = m.submodules.vmask = VMask(self.vlen)
        m.d.comb += [
            vmask.valid.eq(self.req.valid | cpop_busy),
            vmask.sew.eq(
                Mux(cpop_busy, self.resp.bits.uop.vsew,
                    self.req.bits.uop.vsew)),
            vmask.uop_idx.eq(
                Mux(cpop_busy, cpop_uop_idx, self.req.bits.uop.expd_idx)),
            vmask.cpop_done.eq(cpop_done),
            vmask.vm.eq(
                Mux(cpop_busy, self.resp.bits.uop.vm, self.req.bits.uop.vm)),
            vmask.opcode.eq(
                Mux(cpop_busy, self.resp.bits.uop.opcode,
                    self.req.bits.uop.opcode)),
            vmask.in1.eq(self.req.bits.vs1_data),
            vmask.in2.eq(Mux(cpop_busy, vs2_reg, self.req.bits.vs2_data)),
            vmask.tail.eq(tail_mask),
            vmask.vmask.eq(Mux(cpop_busy, mask_reg, self.req.bits.mask)),
        ]

        vid_tail_mask = Signal(self.vlen)
        vid_byte_mask = Signal(self.vlen)
        get_byte_mask(m, vid_tail_mask, vid_tail_reg,
                      self.resp.bits.uop.dest_eew())
        get_byte_mask(m, vid_byte_mask, vid_mask_reg,
                      self.resp.bits.uop.dest_eew())

        mask_gen = m.submodules.mask_gen = MaskDataGen(self.vlen, self.params)
        m.d.comb += [
            mask_gen.mask.eq(vid_byte_mask),
            mask_gen.tail.eq(vid_tail_mask),
            mask_gen.old_vd.eq(old_vd_reg),
            mask_gen.uop.eq(self.resp.bits.uop),
        ]

        vd_out = Signal(self.vlen)
        with m.If((self.resp.bits.uop.opcode >= VOpCode.VMANDNOT)
                  & (self.resp.bits.uop.opcode <= VOpCode.VMXNOR)):
            m.d.comb += vd_out.eq((vmask.resp_data.bits & vl_mask_reg)
                                  | ~vl_mask_reg)
        with m.Elif((self.resp.bits.uop.opcode == VOpCode.VIOTA)
                    | (self.resp.bits.uop.opcode == VOpCode.VID)):
            m.d.comb += vd_out.eq((vmask.resp_data.bits & mask_gen.mask_keep)
                                  | mask_gen.mask_off)
        with m.Else():
            m.d.comb += vd_out.eq(vmask.resp_data.bits)

        m.d.comb += [
            self.resp.valid.eq(vmask.resp_data.valid),
            self.resp.bits.vd_data.eq(vd_out),
        ]

        return m


class VFPULane(PipelinedLaneFunctionalUnit):

    def __init__(self, data_width, params, num_stages):
        super().__init__(data_width, num_stages, params)

        self.data_width = data_width

    def elaborate(self, platform):
        m = super().elaborate(platform)

        uop = self.req.bits.uop

        fma_en = Signal()
        cast_en = Signal()
        cmp_en = Signal()

        fma_op = Signal(FPUOperator)
        fma_op_mod = Signal()
        fma_fmt = Mux(uop.fp_single & ~(uop.widen | uop.widen2), FPFormat.S,
                      FPFormat.D)

        fmt_in = Signal(FPFormat)
        fmt_out = Signal(FPFormat)
        fmt_int = Signal(2)

        neg_vs1 = Signal()
        neg_vs2 = Signal()
        swap32 = Signal()

        fp_rm = Signal(RoundingMode)

        with m.Switch(uop.opcode):
            with m.Case(VOpCode.VFADD):
                m.d.comb += [
                    fma_en.eq(1),
                    fma_op.eq(FPUOperator.ADD),
                    fmt_in.eq(fma_fmt),
                    fmt_out.eq(fma_fmt),
                    swap32.eq(1),
                ]

            with m.Case(VOpCode.VFSUB):
                m.d.comb += [
                    fma_en.eq(1),
                    fma_op.eq(FPUOperator.ADD),
                    fmt_in.eq(fma_fmt),
                    fmt_out.eq(fma_fmt),
                    swap32.eq(1),
                    neg_vs1.eq(1),
                ]

            with m.Case(VOpCode.VFRSUB):
                m.d.comb += [
                    fma_en.eq(1),
                    fma_op.eq(FPUOperator.ADD),
                    fma_op_mod.eq(1),
                    fmt_in.eq(fma_fmt),
                    fmt_out.eq(fma_fmt),
                    swap32.eq(1),
                ]

            with m.Case(VOpCode.VFMUL):
                m.d.comb += [
                    fma_en.eq(1),
                    fma_op.eq(FPUOperator.MUL),
                    fmt_in.eq(fma_fmt),
                    fmt_out.eq(fma_fmt),
                ]

            with m.Case(VOpCode.VFMACC, VOpCode.VFMADD):
                m.d.comb += [
                    fma_en.eq(1),
                    fma_op.eq(FPUOperator.FMADD),
                    fmt_in.eq(fma_fmt),
                    fmt_out.eq(fma_fmt),
                    swap32.eq(uop.opcode == VOpCode.VFMADD),
                ]

            with m.Case(VOpCode.VFMSAC, VOpCode.VFMSUB):
                m.d.comb += [
                    fma_en.eq(1),
                    fma_op.eq(FPUOperator.FMADD),
                    fma_op_mod.eq(1),
                    fmt_in.eq(fma_fmt),
                    fmt_out.eq(fma_fmt),
                    swap32.eq(uop.opcode == VOpCode.VFMSUB),
                ]

            with m.Case(VOpCode.VFNMSAC, VOpCode.VFNMSUB):
                m.d.comb += [
                    fma_en.eq(1),
                    fma_op.eq(FPUOperator.FNMSUB),
                    fmt_in.eq(fma_fmt),
                    fmt_out.eq(fma_fmt),
                    swap32.eq(uop.opcode == VOpCode.VFNMSUB),
                ]

            with m.Case(VOpCode.VFNMACC, VOpCode.VFNMADD):
                m.d.comb += [
                    fma_en.eq(1),
                    fma_op.eq(FPUOperator.FNMSUB),
                    fma_op_mod.eq(1),
                    fmt_in.eq(fma_fmt),
                    fmt_out.eq(fma_fmt),
                    swap32.eq(uop.opcode == VOpCode.VFNMADD),
                ]

            with m.Case(VOpCode.VFSGNJ, VOpCode.VFSGNJN, VOpCode.VFSGNJX):
                m.d.comb += [
                    cmp_en.eq(1),
                    fma_op.eq(FPUOperator.SGNJ),
                    fp_rm.eq(
                        Mux(
                            uop.opcode == VOpCode.VFSGNJN, RoundingMode.RTZ,
                            Mux(uop.opcode == VOpCode.VFSGNJX,
                                RoundingMode.RDN, RoundingMode.RNE))),
                    fmt_in.eq(Mux(uop.fp_single, FPFormat.S, FPFormat.D)),
                    fmt_out.eq(Mux(uop.fp_single, FPFormat.S, FPFormat.D)),
                ]

            with m.Case(VOpCode.VFMIN, VOpCode.VFMAX):
                m.d.comb += [
                    cmp_en.eq(1),
                    fma_op.eq(FPUOperator.MINMAX),
                    fp_rm.eq(
                        Mux(uop.opcode == VOpCode.VFMIN, RoundingMode.RNE,
                            RoundingMode.RTZ)),
                    fmt_in.eq(Mux(uop.fp_single, FPFormat.S, FPFormat.D)),
                    fmt_out.eq(Mux(uop.fp_single, FPFormat.S, FPFormat.D)),
                ]

            with m.Case(VOpCode.VMFEQ, VOpCode.VMFNE):
                m.d.comb += [
                    cmp_en.eq(1),
                    fma_op.eq(FPUOperator.CMP),
                    fma_op_mod.eq(uop.opcode == VOpCode.VMFNE),
                    fp_rm.eq(RoundingMode.RDN),
                    fmt_in.eq(Mux(uop.fp_single, FPFormat.S, FPFormat.D)),
                ]

            with m.Case(VOpCode.VMFLT, VOpCode.VMFGE):
                m.d.comb += [
                    cmp_en.eq(1),
                    fma_op.eq(FPUOperator.CMP),
                    fma_op_mod.eq(uop.opcode == VOpCode.VMFLT),
                    fp_rm.eq(RoundingMode.RNE),
                    fmt_in.eq(Mux(uop.fp_single, FPFormat.S, FPFormat.D)),
                ]

            with m.Case(VOpCode.VMFLE, VOpCode.VMFGT):
                m.d.comb += [
                    cmp_en.eq(1),
                    fma_op.eq(FPUOperator.CMP),
                    fma_op_mod.eq(uop.opcode == VOpCode.VMFLE),
                    fp_rm.eq(RoundingMode.RTZ),
                    fmt_in.eq(Mux(uop.fp_single, FPFormat.S, FPFormat.D)),
                ]

        vs1_data_inv = Signal(self.data_width)
        vs2_data_inv = Signal(self.data_width)
        with m.Switch(uop.vsew):
            for w in range(4):
                with m.Case(w):
                    n = 1 << (3 + w)
                    for i in range(self.data_width // n):
                        m.d.comb += [
                            vs1_data_inv[i * n:(i + 1) * n].eq(
                                Cat(
                                    self.req.bits.opa_data[i * n:(i + 1) * n -
                                                           1],
                                    self.req.bits.opa_data[(i + 1) * n - 1]
                                    ^ neg_vs1)),
                            vs2_data_inv[i * n:(i + 1) * n].eq(
                                Cat(
                                    self.req.bits.opb_data[i * n:(i + 1) * n -
                                                           1],
                                    self.req.bits.opb_data[(i + 1) * n - 1]
                                    ^ neg_vs2)),
                        ]

        s1_vs1_data_widen = Signal(self.data_width)
        s1_vs2_data_widen = Signal(self.data_width)
        for i in range(self.data_width // 64):
            fcvt_widen_vs1 = FPUCastMulti(latency=1)
            fcvt_widen_vs2 = FPUCastMulti(latency=1)
            m.submodules += fcvt_widen_vs1
            m.submodules += fcvt_widen_vs2

            with m.Switch(uop.expd_idx[0]):
                for w in range(2):
                    with m.Case(w):
                        m.d.comb += [
                            fcvt_widen_vs1.inp.valid.eq(uop.widen
                                                        | uop.widen2),
                            fcvt_widen_vs1.inp.bits.in1.eq(
                                vs1_data_inv[w * (self.data_width // 2) +
                                             i * 32:w *
                                             (self.data_width // 2) +
                                             (i + 1) * 32]),
                            fcvt_widen_vs1.inp.bits.src_fmt.eq(FPFormat.S),
                            fcvt_widen_vs1.inp.bits.dst_fmt.eq(FPFormat.D),
                            fcvt_widen_vs2.inp.valid.eq(uop.widen),
                            fcvt_widen_vs2.inp.bits.in1.eq(
                                vs2_data_inv[w * (self.data_width // 2) +
                                             i * 32:w *
                                             (self.data_width // 2) +
                                             (i + 1) * 32]),
                            fcvt_widen_vs2.inp.bits.src_fmt.eq(FPFormat.S),
                            fcvt_widen_vs2.inp.bits.dst_fmt.eq(FPFormat.D),
                        ]

            m.d.comb += [
                s1_vs1_data_widen[i * 64:(i + 1) * 64].eq(
                    fcvt_widen_vs1.out.bits),
                s1_vs2_data_widen[i * 64:(i + 1) * 64].eq(
                    fcvt_widen_vs2.out.bits),
            ]

        s1_valid = Signal()
        s1_fma_en = Signal()
        s1_cast_en = Signal()
        s1_cmp_en = Signal()
        s1_fma_op = Signal(FPUOperator)
        s1_fma_op_mod = Signal()
        s1_fmt_in = Signal(FPFormat)
        s1_fmt_out = Signal(FPFormat)
        s1_fmt_int = Signal(2)
        s1_swap32 = Signal()
        s1_fp_rm = Signal(RoundingMode)
        s1_widen = Signal()
        s1_widen2 = Signal()
        m.d.sync += [
            s1_valid.eq(self.req.valid),
            s1_fma_en.eq(fma_en),
            s1_cast_en.eq(cast_en),
            s1_cmp_en.eq(cmp_en),
            s1_fma_op.eq(fma_op),
            s1_fma_op_mod.eq(fma_op_mod),
            s1_fmt_in.eq(fmt_in),
            s1_fmt_out.eq(fmt_out),
            s1_fmt_int.eq(fmt_int),
            s1_swap32.eq(swap32),
            s1_fp_rm.eq(fp_rm),
            s1_widen.eq(uop.widen),
            s1_widen2.eq(uop.widen2),
        ]

        s1_vs1_data_inv = Signal(self.data_width)
        s1_vs2_data_inv = Signal(self.data_width)
        s1_vd_data = Signal(self.data_width)
        m.d.sync += [
            s1_vs1_data_inv.eq(vs1_data_inv),
            s1_vs2_data_inv.eq(vs2_data_inv),
            s1_vd_data.eq(self.req.bits.old_vd),
        ]

        s1_vs1_data = Mux(s1_widen | s1_widen2, s1_vs1_data_widen,
                          s1_vs1_data_inv)
        s1_vs2_data = Mux(s1_widen, s1_vs2_data_widen, s1_vs2_data_inv)

        def set_fu_input(inp):
            m.d.comb += [
                inp.bits.in1.eq(s1_vs1_data),
                inp.bits.in2.eq(s1_vs2_data),
                inp.bits.in3.eq(s1_vd_data),
                inp.bits.fn.eq(s1_fma_op),
                inp.bits.fn_mod.eq(s1_fma_op_mod),
                inp.bits.rm.eq(s1_fp_rm),
                inp.bits.src_fmt.eq(s1_fmt_in),
                inp.bits.dst_fmt.eq(s1_fmt_out),
                inp.bits.int_fmt.eq(s1_fmt_int),
            ]

            with m.If(s1_swap32):
                m.d.comb += [
                    inp.bits.in3.eq(s1_vs2_data),
                    inp.bits.in2.eq(s1_vd_data),
                ]

        fma = m.submodules.fma = VFPUFMA(self.data_width, self.num_stages - 1)
        set_fu_input(fma.inp)
        m.d.comb += fma.inp.valid.eq(s1_valid & s1_fma_en)

        fcmp = m.submodules.fcmp = VFPUComp(self.data_width,
                                            self.num_stages - 1)
        set_fu_input(fcmp.inp)
        m.d.comb += fcmp.inp.valid.eq(s1_valid & s1_cmp_en)

        pipe_in = Cat(self.req.bits.old_vd, self.req.bits.tail,
                      self.req.bits.mask)
        out_old_vd = Signal.like(self.req.bits.old_vd)
        out_tail = Signal.like(self.req.bits.tail)
        out_mask = Signal.like(self.req.bits.mask)
        out_pipe = m.submodules.out_pipe = Pipe(width=len(pipe_in),
                                                depth=self.num_stages)
        m.d.comb += [
            out_pipe.in_valid.eq(self.req.valid),
            out_pipe.in_data.eq(pipe_in),
            Cat(out_old_vd, out_tail, out_mask).eq(out_pipe.out.bits),
        ]

        tail_mask = Signal(self.data_width)
        byte_mask = Signal(self.data_width)
        get_byte_mask(m, tail_mask, out_tail, self.resp.bits.uop.dest_eew())
        get_byte_mask(m, byte_mask, out_mask, self.resp.bits.uop.dest_eew())

        mask_gen = m.submodules.mask_gen = MaskDataGen(self.data_width,
                                                       self.params)
        m.d.comb += [
            mask_gen.mask.eq(byte_mask),
            mask_gen.tail.eq(tail_mask),
            mask_gen.old_vd.eq(out_old_vd),
            mask_gen.uop.eq(self.resp.bits.uop),
        ]

        cmp_out_masked = (fcmp.out.bits.data
                          & mask_gen.mask_keep_cmp) | mask_gen.mask_off_cmp

        cmp_out = [
            Signal(self.data_width // 8, name=f'cmp_out{i}') for i in range(8)
        ]
        cmp_out_next = [
            Signal(self.data_width // 8, name=f'cmp_out_next{i}')
            for i in range(8)
        ]
        m.d.comb += Cat(cmp_out_next).eq(Cat(cmp_out))
        with m.If(fcmp.out.valid):
            with m.Switch(self.resp.bits.uop.expd_idx):
                for i in range(1, 8):
                    with m.Case(i):
                        m.d.comb += cmp_out_next[i].eq(cmp_out_masked)

                with m.Default():
                    m.d.comb += [
                        Cat(cmp_out_next).eq(~0),
                        cmp_out_next[0].eq(cmp_out_masked),
                    ]

            m.d.sync += Cat(cmp_out).eq(Cat(cmp_out_next))

        m.d.comb += self.resp.bits.vd_data.eq(
            Mux(
                self.resp.bits.uop.narrow_to_1, Cat(cmp_out_next),
                Mux(fcmp.out.valid, fcmp.out.bits.data, fma.out.bits.data)
                & mask_gen.mask_keep)
            | mask_gen.mask_off)

        return m


class VFPUUnit(PerLaneFunctionalUnit):

    def __init__(self, params):
        super().__init__(params)

        self.num_stages = params['fma_latency'] + 1

    def elaborate(self, platform):
        m = super().elaborate(platform)

        for w in range(self.n_lanes):
            lane = VFPULane(self.lane_width, self.params, self.num_stages)
            setattr(m.submodules, f'lane{w}', lane)
            m.d.comb += [
                self.lane_reqs[w].connect(lane.req),
                lane.resp.connect(self.lane_resps[w]),
            ]

        return m


class VFDivLane(IterativeLaneFunctionalUnit):

    def __init__(self, data_width, params):
        super().__init__(data_width, params)

        self.data_width = data_width

    def elaborate(self, platform):
        m = super().elaborate(platform)

        uop = self.req.bits.uop

        fdiv = m.submodules.fdiv = VFPUDivSqrt(self.data_width)
        m.d.comb += [
            fdiv.a.eq(self.req.bits.opb_data),
            fdiv.b.eq(self.req.bits.opa_data),
            fdiv.is_sqrt.eq(uop.opcode == VOpCode.VFSQRT),
            fdiv.fmt.eq(Mux(uop.fp_single, FPFormat.S, FPFormat.D)),
            fdiv.in_valid.eq(self.req.valid),
            self.req.ready.eq(fdiv.in_ready),
            self.resp.valid.eq(fdiv.out.valid),
            fdiv.out.ready.eq(self.resp.ready),
        ]

        with m.If(uop.opcode == VOpCode.VFRDIV):
            m.d.comb += [
                fdiv.a.eq(self.req.bits.opa_data),
                fdiv.b.eq(self.req.bits.opb_data),
            ]

        old_vd = Signal.like(self.req.bits.old_vd)
        tail = Signal.like(self.req.bits.tail)
        mask = Signal.like(self.req.bits.mask)
        with m.If(self.req.fire):
            m.d.sync += [
                old_vd.eq(self.req.bits.old_vd),
                tail.eq(self.req.bits.tail),
                mask.eq(self.req.bits.mask),
            ]

        tail_mask = Signal(self.data_width)
        byte_mask = Signal(self.data_width)
        get_byte_mask(m, tail_mask, tail, self.resp.bits.uop.dest_eew())
        get_byte_mask(m, byte_mask, mask, self.resp.bits.uop.dest_eew())

        mask_gen = m.submodules.mask_gen = MaskDataGen(self.data_width,
                                                       self.params)
        m.d.comb += [
            mask_gen.mask.eq(byte_mask),
            mask_gen.tail.eq(tail_mask),
            mask_gen.old_vd.eq(old_vd),
            mask_gen.uop.eq(self.resp.bits.uop),
        ]

        m.d.comb += self.resp.bits.vd_data.eq((fdiv.out.bits
                                               & mask_gen.mask_keep)
                                              | mask_gen.mask_off)

        return m


class VFDivUnit(PerLaneFunctionalUnit):

    def __init__(self, params):
        super().__init__(params)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        for w in range(self.n_lanes):
            lane = VFDivLane(self.lane_width, self.params)
            setattr(m.submodules, f'lane{w}', lane)
            m.d.comb += [
                self.lane_reqs[w].connect(lane.req),
                lane.resp.connect(self.lane_resps[w]),
            ]

        return m
