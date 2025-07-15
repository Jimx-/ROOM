from amaranth import *
from amaranth import tracer
from amaranth.hdl.ast import ValueCastable

from vroom.types import HasVectorParams, VMicroOp


class ExecReq(HasVectorParams, ValueCastable):

    def __init__(self, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.uop = VMicroOp(params, name=f'{name}_uop')

        self.vs1_data = Signal(self.vlen)
        self.vs2_data = Signal(self.vlen)
        self.vs3_data = Signal(self.vlen)

        self.rs1_data = Signal(self.xlen)
        self.rs2_data = Signal(self.xlen)

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

        self.uop = VMicroOp(params, name=f'{name}_uop')

        self.vd_data = Signal(self.vlen)

        self.rd_data = Signal(self.xlen)

    @ValueCastable.lowermethod
    def as_value(self):
        return Cat(self.uop, self.vd_data, self.rd_data)

    def shape(self):
        return self.as_value().shape()

    def __len__(self):
        return len(Value.cast(self))

    def eq(self, rhs):
        return Value.cast(self).eq(Value.cast(rhs))
