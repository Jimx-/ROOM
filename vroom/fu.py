from amaranth import *
from amaranth import tracer
from amaranth.hdl.ast import ValueCastable

from vroom.types import HasVectorParams, VMicroOp

from room.utils import Decoupled


class ExecReq(HasVectorParams, ValueCastable):

    def __init__(self, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.uop = VMicroOp(params, name=f'{name}__uop')

        self.vs1_data = Signal(self.vlen, name=f'{name}__vs1_data')
        self.vs2_data = Signal(self.vlen, name=f'{name}__vs2_data')
        self.vs3_data = Signal(self.vlen, name=f'{name}__vs3_data')

        self.rs1_data = Signal(self.xlen, name=f'{name}__rs1_data')
        self.rs2_data = Signal(self.xlen, name=f'{name}__rs2_data')

    @ValueCastable.lowermethod
    def as_value(self):
        return Cat(self.uop, self.vs1_data, self.vs2_data, self.vs3_data,
                   self.rs1_data, self.rs2_data)

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

        self.vd_data = Signal(self.vlen, name=f'{name}__vd_data')
        self.rd_data = Signal(self.xlen, name=f'{name}__rd_data')

    @ValueCastable.lowermethod
    def as_value(self):
        return Cat(self.uop, self.base_addr, self.stride, self.vd_data,
                   self.rd_data)

    def shape(self):
        return self.as_value().shape()

    def __len__(self):
        return len(Value.cast(self))

    def eq(self, rhs):
        return Value.cast(self).eq(Value.cast(rhs))


class FunctionalUnit(HasVectorParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.req = Decoupled(ExecReq, params)
        self.resp = Decoupled(ExecResp, params)


class PipelinedFunctionalUnit(FunctionalUnit):

    def __init__(self, num_stages, params):
        self.params = params
        self.num_stages = num_stages

        super().__init__(params)

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


class AddrGenUnit(PipelinedFunctionalUnit):

    def __init__(self, params):
        super().__init__(0, params)

    def elaborate(self, platform):
        m = super().elaborate(platform)

        m.d.comb += [
            self.resp.bits.base_addr.eq(self.req.bits.rs1_data),
            self.resp.bits.stride.eq(self.req.bits.rs2_data),
        ]

        return m
