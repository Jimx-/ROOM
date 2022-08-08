from amaranth import *

from room.consts import *
from room.types import MicroOp


class BranchResolution:

    def __init__(self, params, name=None):
        name = (name is not None) and f'{name}_' or ''

        self.valid = Signal(name=f'{name}valid')
        self.uop = MicroOp(params, name=f'{name}uop')
        self.mispredict = Signal(name=f'{name}mispredict')
        self.cfi_type = Signal(CFIType, name=f'{name}cfi_type')
        self.taken = Signal(name=f'{name}taken')
        self.pc_sel = Signal(PCSel, name=f'{name}pc_sel')
        self.target_offset = Signal(signed(32), name=f'{name}target_offset')

    def eq(self, rhs):
        names = [
            'valid',
            'uop',
            'mispredict',
            'cfi_type',
            'taken',
            'pc_sel',
            'target_offset',
        ]
        return [getattr(self, n).eq(getattr(rhs, n)) for n in names]


class BranchUpdate:

    def __init__(self, params, name=None):
        max_br_count = params['max_br_count']
        name = (name is not None) and f'{name}_' or ''

        self.resolve_mask = Signal(max_br_count, name=f'{name}resolve_mask')
        self.mispredict_mask = Signal(max_br_count,
                                      name=f'{name}mispredict_mask')

        ###

        self.br_res = BranchResolution(params, name=f'{name}resolution')

    def eq(self, rhs):
        names = ['resolve_mask', 'mispredict_mask', 'br_res']
        return [getattr(self, n).eq(getattr(rhs, n)) for n in names]

    def get_new_br_mask(self, mask):
        return mask & ~self.resolve_mask

    def uop_killed(self, uop):
        return (self.mispredict_mask & uop.br_mask) != 0


class BranchMaskAllocator(Elaboratable):

    def __init__(self, params):
        self.core_width = params['core_width']
        self.max_br_count = params['max_br_count']

        self.is_branch = Signal(self.core_width)
        self.reqs = Signal(self.core_width)

        self.br_tag = [
            Signal(range(self.max_br_count), name=f'br_tag{i}')
            for i in range(self.core_width)
        ]

        self.br_mask = [
            Signal(self.max_br_count, name=f'br_tag{i}')
            for i in range(self.core_width)
        ]

        self.full = Signal(self.core_width)

        self.br_update = BranchUpdate(params)

        self.flush = Signal()

    def elaborate(self, platform):
        m = Module()

        branch_mask = Signal(self.max_br_count)

        allocate_mask = branch_mask
        tag_masks = [
            Signal(self.max_br_count, name=f'tag_mask{i}')
            for i in range(self.core_width)
        ]

        for br_tag, mask, full, is_br in zip(self.br_tag, tag_masks, self.full,
                                             self.is_branch):
            new_tag = Signal.like(br_tag)

            for t in reversed(range(self.max_br_count)):
                with m.If(~allocate_mask[t]):
                    m.d.comb += [
                        new_tag.eq(t),
                        mask.eq(1 << t),
                    ]

            m.d.comb += [
                full.eq((allocate_mask == Repl(1, self.max_br_count)) & is_br),
                br_tag.eq(new_tag),
            ]

            allocate_mask = Mux(is_br, mask | allocate_mask, allocate_mask)

        cur_mask = branch_mask
        for br_mask, req, tag_mask in zip(self.br_mask, self.reqs, tag_masks):
            m.d.comb += br_mask.eq(self.br_update.get_new_br_mask(cur_mask))
            cur_mask = Mux(req, tag_mask | cur_mask, cur_mask)

        with m.If(self.flush):
            m.d.sync += branch_mask.eq(0)
        with m.Else():
            with m.If(self.br_update.br_res.mispredict):
                m.d.sync += branch_mask.eq(
                    self.br_update.get_new_br_mask(cur_mask)
                    & self.br_update.br_res.uop.br_mask)
            with m.Else():
                m.d.sync += branch_mask.eq(
                    self.br_update.get_new_br_mask(cur_mask))

        return m
