from amaranth import *
from amaranth import tracer
from amaranth.hdl.ast import ValueCastable

from vroom.consts import *
from vroom.types import HasVectorParams, VMicroOp
from vroom.alu import VALU, VMultiplier, VIntDiv
from vroom.utils import TailGen

from room.consts import RegisterType
from room.utils import Decoupled, sign_extend


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
                                n = 1 << w
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

        s1_alu_out = Signal.like(alu.out)
        s1_mask_keep = Signal.like(mask_gen.mask_keep)
        s1_mask_off_data = Signal.like(mask_gen.mask_off)
        m.d.sync += [
            s1_alu_out.eq(alu.out),
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

        s1_alu_out_masked = (Mux(self.uops[0].narrow, s1_narrow_out,
                                 s1_alu_out) & s1_mask_keep) | s1_mask_off_data
        m.d.comb += self.resp.bits.vd_data.eq(
            Mux(
                self.uops[0].dst_rtype != RegisterType.VEC, s1_alu_out,
                Mux(self.uops[0].narrow_to_1, Cat(s1_cmp_out),
                    s1_alu_out_masked)))

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
            mul.uop_idx.eq(uop.expd_idx),
            mul.in1.eq(self.req.bits.opa_data),
            mul.in2.eq(self.req.bits.opb_data),
            mul.in3.eq(self.req.bits.old_vd),
            mul.widen.eq(uop.widen),
        ]

        s1_old_vd = Signal.like(self.req.bits.old_vd)
        s1_tail = Signal.like(self.req.bits.tail)
        s1_mask = Signal.like(self.req.bits.mask)
        with m.If(self.req.valid):
            m.d.sync += [
                s1_old_vd.eq(self.req.bits.old_vd),
                s1_tail.eq(self.req.bits.tail),
                s1_mask.eq(self.req.bits.mask),
            ]

        s2_old_vd = Signal.like(s1_old_vd)
        s2_tail = Signal.like(s1_tail)
        s2_mask = Signal.like(s1_mask)
        with m.If(self.valids[0]):
            m.d.sync += [
                s2_old_vd.eq(s1_old_vd),
                s2_tail.eq(s1_tail),
                s2_mask.eq(s1_mask),
            ]

        tail_mask = Signal(self.data_width)
        byte_mask = Signal(self.data_width)
        get_byte_mask(m, tail_mask, s2_tail, self.uops[1].dest_eew())
        get_byte_mask(m, byte_mask, s2_mask, self.uops[1].dest_eew())

        mask_gen = m.submodules.mask_gen = MaskDataGen(self.data_width,
                                                       self.params)
        m.d.comb += [
            mask_gen.mask.eq(byte_mask),
            mask_gen.tail.eq(tail_mask),
            mask_gen.old_vd.eq(s2_old_vd),
            mask_gen.uop.eq(self.uops[1]),
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
