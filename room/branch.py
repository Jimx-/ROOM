from amaranth import *
from amaranth import tracer

from room.consts import *
from room.types import HasCoreParams, MicroOp
from room.utils import wrap_incr, wrap_decr


class GlobalHistory(HasCoreParams, Record):

    def __init__(self, params, name=None, src_loc_at=0):
        HasCoreParams.__init__(self, params)

        Record.__init__(self, [
            ('history', self.ghist_length),
            ('br_not_taken', 1),
            ('ras_idx', range(self.n_ras_entries)),
        ],
                        name=name,
                        src_loc_at=1 + src_loc_at)

    def update(self, m, branches, cfi_taken, cfi_is_br, cfi_idx, cfi_valid,
               cfi_is_call, cfi_is_ret, new_ghist):

        cfi_mask = Signal(self.fetch_width)
        m.d.comb += cfi_mask.eq(~0)
        with m.If(cfi_valid):
            with m.Switch(cfi_idx):
                for w in range(self.fetch_width):
                    with m.Case(w):
                        with m.If(cfi_taken & cfi_is_br):
                            m.d.comb += cfi_mask.eq((1 << w) - 1)
                        with m.Else():
                            m.d.comb += cfi_mask.eq((1 << (w + 1)) - 1)

        not_taken_branches = branches & cfi_mask
        m.d.comb += [
            new_ghist.br_not_taken.eq(0),
            new_ghist.history.eq(
                Mux(
                    cfi_valid & cfi_is_br & cfi_taken, (self.history << 1) | 1,
                    Mux(not_taken_branches.any() | self.br_not_taken,
                        self.history << 1, self.history))),
        ]

        m.d.comb += new_ghist.ras_idx.eq(
            Mux(
                cfi_valid & cfi_is_call,
                wrap_incr(self.ras_idx, self.n_ras_entries),
                Mux(cfi_valid & cfi_is_ret,
                    wrap_decr(self.ras_idx, self.n_ras_entries), 0)))


class FTQEntry(HasCoreParams, Record):

    def __init__(self, params, name=None, src_loc_at=0):
        HasCoreParams.__init__(self, params)

        Record.__init__(self, [
            ('cfi_valid', 1),
            ('cfi_idx', range(self.fetch_width)),
            ('cfi_taken', 1),
            ('cfi_mispredicted', 1),
            ('cfi_type', CFIType),
            ('br_mask', self.fetch_width),
            ('cfi_is_call', 1),
            ('cfi_is_ret', 1),
            ('cfi_npc_plus4', 1),
        ],
                        name=name,
                        src_loc_at=1 + src_loc_at)


class GetPCResp(HasCoreParams):

    def __init__(self, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.entry = FTQEntry(params, name=f'{name}__entry')
        self.ghist = GlobalHistory(params, name=f'{name}__ghist')

        self.pc = Signal(self.vaddr_bits_extended, name=f'{name}__pc')
        self.commit_pc = Signal(self.vaddr_bits_extended,
                                name=f'{name}__commit_pc')
        self.next_pc = Signal(self.vaddr_bits_extended,
                              name=f'{name}__next_pc')
        self.next_valid = Signal()

    def eq(self, rhs):
        names = [
            'entry',
            'ghist',
            'pc',
            'commit_pc',
            'next_pc',
            'next_valid',
        ]
        return [getattr(self, n).eq(getattr(rhs, n)) for n in names]


class BranchResolution(HasCoreParams):

    def __init__(self, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.valid = Signal(name=f'{name}_valid')
        self.uop = MicroOp(params, name=f'{name}_uop')
        self.mispredict = Signal(name=f'{name}_mispredict')
        self.cfi_type = Signal(CFIType, name=f'{name}_cfi_type')
        self.taken = Signal(name=f'{name}_taken')
        self.pc_sel = Signal(PCSel, name=f'{name}_pc_sel')
        self.target_offset = Signal(signed(32), name=f'{name}_target_offset')
        self.jalr_target = Signal(self.vaddr_bits_extended,
                                  name=f'{name}_jalr_target')

    def eq(self, rhs):
        names = [
            'valid',
            'uop',
            'mispredict',
            'cfi_type',
            'taken',
            'pc_sel',
            'target_offset',
            'jalr_target',
        ]
        return [getattr(self, n).eq(getattr(rhs, n)) for n in names]


class BranchUpdate(HasCoreParams):

    def __init__(self, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.resolve_mask = Signal(self.max_br_count,
                                   name=f'{name}_resolve_mask')
        self.mispredict_mask = Signal(self.max_br_count,
                                      name=f'{name}_mispredict_mask')

        ###

        self.br_res = BranchResolution(params, name=f'{name}_resolution')

    def eq(self, rhs):
        names = ['resolve_mask', 'mispredict_mask', 'br_res']
        return [getattr(self, n).eq(getattr(rhs, n)) for n in names]

    def get_new_br_mask(self, mask):
        return mask & ~self.resolve_mask

    def br_mask_killed(self, br_mask):
        return (self.mispredict_mask & br_mask) != 0

    def uop_killed(self, uop):
        return self.br_mask_killed(uop.br_mask)


class BranchMaskAllocator(HasCoreParams, Elaboratable):

    def __init__(self, params):
        super().__init__(params)

        self.is_branch = Signal(self.core_width)
        self.reqs = Signal(self.core_width)

        self.br_tag = [
            Signal(range(self.max_br_count), name=f'br_tag{i}')
            for i in range(self.core_width)
        ]

        self.br_mask = [
            Signal(self.max_br_count, name=f'br_mask{i}')
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


class BranchKillableFIFO(HasCoreParams, Elaboratable):

    def __init__(self,
                 entries,
                 params,
                 cls,
                 *args,
                 flow=True,
                 flush_fn=None,
                 **kwargs):
        super().__init__(params)

        self.entries = entries
        self.flow = flow

        if flush_fn is None:
            flush_fn = lambda _: 1
        self.flush_fn = flush_fn

        self.cls = cls
        self.args = args
        self.kwargs = kwargs

        self.w_data = cls(*args, **kwargs)
        self.w_br_mask = Signal(self.max_br_count)
        self.w_en = Signal()
        self.w_rdy = Signal()

        self.r_data = cls(*args, **kwargs)
        self.r_br_mask = Signal(self.max_br_count)
        self.r_en = Signal()
        self.r_rdy = Signal()

        self.empty = Signal()

        self.br_update = BranchUpdate(params)
        self.flush = Signal()

    def elaborate(self, platform):
        m = Module()

        mem = [
            self.cls(*self.args, **self.kwargs, name=f'mem{i}')
            for i in range(self.entries)
        ]
        valids = Array(Signal(name=f'valid{i}') for i in range(self.entries))
        br_masks = Array(
            Signal.like(self.w_br_mask, name=f'br_mask{i}')
            for i in range(self.entries))

        w_ptr = Signal(range(self.entries))
        r_ptr = Signal(range(self.entries))
        maybe_full = Signal()

        m.d.comb += self.empty.eq((r_ptr == w_ptr) & ~maybe_full)
        full = (r_ptr == w_ptr) & maybe_full

        do_enq = Signal()
        do_deq = Signal()

        m.d.comb += [
            do_enq.eq(self.w_en & self.w_rdy),
            do_deq.eq((self.r_en | ~valids[r_ptr])
                      & ~self.empty),
        ]

        for i in range(self.entries):
            m.d.sync += valids[i].eq(
                valids[i] & ~self.br_update.br_mask_killed(br_masks[i])
                & ~(self.flush & self.flush_fn(mem[i])))

            with m.If(valids[i]):
                m.d.sync += br_masks[i].eq(
                    self.br_update.get_new_br_mask(br_masks[i]))

        with m.If(do_enq):
            m.d.sync += [
                br_masks[w_ptr].eq(
                    self.br_update.get_new_br_mask(self.w_br_mask)),
                valids[w_ptr].eq(1),
                w_ptr.eq(wrap_incr(w_ptr, self.entries)),
            ]

            for i in range(self.entries):
                with m.If(w_ptr == i):
                    m.d.sync += mem[i].eq(self.w_data)

        with m.If(do_deq):
            m.d.sync += [
                valids[r_ptr].eq(0),
                r_ptr.eq(wrap_incr(r_ptr, self.entries)),
            ]

        with m.If(do_enq ^ do_deq):
            m.d.sync += maybe_full.eq(do_enq)

        m.d.comb += self.w_rdy.eq(~full)

        flush_r_ptr = Signal()
        for i in range(self.entries):
            with m.If(r_ptr == i):
                m.d.comb += flush_r_ptr.eq(self.flush_fn(mem[i]))

        m.d.comb += [
            self.r_rdy.eq(~self.empty & valids[r_ptr]
                          & ~self.br_update.br_mask_killed(br_masks[r_ptr])
                          & ~(self.flush & flush_r_ptr)),
            self.r_br_mask.eq(self.br_update.get_new_br_mask(br_masks[r_ptr])),
        ]

        for i in range(self.entries):
            with m.If(r_ptr == i):
                m.d.comb += self.r_data.eq(mem[i])

        if self.flow:
            with m.If(self.empty):
                m.d.comb += [
                    self.r_rdy.eq(self.w_en),
                    self.r_data.eq(self.w_data),
                    self.r_br_mask.eq(
                        self.br_update.get_new_br_mask(self.w_br_mask)),
                    do_deq.eq(0),
                ]

                with m.If(self.r_en):
                    m.d.comb += do_enq.eq(0)

        return m
