from amaranth import *
from amaranth.utils import log2_int

from room.consts import RegisterType
from room.types import HasCoreParams


class HasVectorParams(HasCoreParams):

    def __init__(self, params, *args, **kwargs):
        super().__init__(params, *args, **kwargs)

        vector_params = params['vector_params']
        self.fetch_buffer_size = self.ftq_size = vector_params[
            'fetch_buffer_size']

    @property
    def max_vlmax(self):
        return self.vlen

    @property
    def min_vlmax(self):
        return max(((self.max_vlmax // self.elen) >> 3), 1)

    @property
    def vl_bits(self):
        return log2_int(self.max_vlmax) + 1

    @property
    def max_vsew(self):
        return log2_int(self.elen // 8)


class VMicroOp(HasVectorParams, Record):

    def __init__(self, params, name=None, src_loc_at=0):
        HasVectorParams.__init__(self, params)

        Record.__init__(self, [
            ('inst', 32),
            ('ftq_idx', range(self.ftq_size)),
            ('scalar_data', self.xlen),
            ('ldst', 5),
            ('lrs1', 5),
            ('lrs2', 5),
            ('lrs3', 5),
            ('ldst_valid', 1),
            ('dst_rtype', RegisterType),
            ('lrs1_rtype', RegisterType),
            ('lrs2_rtype', RegisterType),
        ],
                        name=name,
                        src_loc_at=1 + src_loc_at)


class VType(HasVectorParams, Record):

    def __init__(self, params, name=None, src_loc_at=0):
        HasVectorParams.__init__(self, params)

        Record.__init__(self, [
            ('vlmul_mag', 2),
            ('vlmul_sign', 1),
            ('vsew', 3),
            ('vta', 1),
            ('vma', 1),
            ('vill', 1),
        ],
                        name=name,
                        src_loc_at=1 + src_loc_at)

    def lmul_ok(self):
        return ~self.vlmul_sign | (
            (self.vlmul_mag != 0) &
            (~self.vlmul_mag < (self.max_vsew - self.vsew)))

    def vlmax(self):
        return (self.max_vlmax >>
                (self.vsew + Cat(~self.vlmul_mag, self.vlmul_sign))
                ) & ~(self.min_vlmax - 1)
