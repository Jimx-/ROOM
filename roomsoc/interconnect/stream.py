from amaranth import *
from amaranth import tracer


class Valid:

    def __init__(self, cls, *args, name=None, src_loc_at=0, **kwargs):
        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.bits = cls(name=f'{name}_bits', *args, **kwargs)
        self.valid = Signal(name=f'{name}_valid')

    @property
    def fire(self):
        return self.valid

    def eq(self, rhs):
        return [
            self.bits.eq(rhs.bits),
            self.valid.eq(rhs.valid),
        ]


class Decoupled:

    def __init__(self, cls, *args, name=None, src_loc_at=0, **kwargs):
        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.bits = cls(name=f'{name}_bits', *args, **kwargs)
        self.valid = Signal(name=f'{name}_valid')
        self.ready = Signal(name=f'{name}_ready')

    @property
    def fire(self):
        return self.valid & self.ready

    def connect(self, subord):
        stmts = []

        if getattr(self.bits, 'connect', None) is not None:
            stmts += self.bits.connect(subord.bits)
        else:
            stmts += [subord.bits.eq(self.bits)]

        stmts.append(subord.valid.eq(self.valid))
        stmts.append(self.ready.eq(subord.ready))

        return stmts
