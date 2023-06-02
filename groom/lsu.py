from amaranth import *
from amaranth import tracer

from groom.fu import ExecResp

from room.types import HasCoreParams, MicroOp
from room.dcache import DCache

from roomsoc.interconnect.stream import Valid, Decoupled


class LSQEntry(HasCoreParams):

    def __init__(self, params, name=None, src_loc_at=0):
        super().__init__(params)

        if name is None:
            name = tracer.get_var_name(depth=2 + src_loc_at, default=None)

        self.valid = Signal(name=f'{name}_valid')
        self.uop = MicroOp(params, name=f'{name}_uop')

        self.addr = [
            Signal(self.vaddr_bits_extended, name=f'{name}_addr{i}')
            for i in range(self.n_threads)
        ]
        self.addr_uncacheable = Signal(self.n_threads,
                                       name=f'{name}_addr_uncacheable')

        self.data = [
            Signal(self.xlen, name=f'{name}_data{i}')
            for i in range(self.n_threads)
        ]

        self.executed = Signal(self.n_threads, name=f'{name}_executed')
        self.succeeded = Signal(self.n_threads, name=f'{name}_succeeded')

    def eq(self, rhs):
        attrs = [
            'valid',
            'uop',
            'executed',
            'succeeded',
        ]
        return [getattr(self, a).eq(getattr(rhs, a)) for a in attrs
                ] + [a.eq(b) for a, b in zip(self.addr, rhs.addr)]


class LoadStoreUnit(HasCoreParams, Elaboratable):

    def __init__(self, dbus, dbus_mmio, params):
        super().__init__(params)

        self.dbus = dbus
        self.dbus_mmio = dbus_mmio

        self.exec_req = Decoupled(ExecResp, self.xlen, params)

        self.exec_iresp = Decoupled(ExecResp, self.xlen, params)

    def elaborate(self, platform):
        m = Module()

        dcache = m.submodules.dcache = DCache(self.dbus, self.dbus_mmio,
                                              self.params)

        lsq = Array(
            LSQEntry(self.params, name=f'lsq{i}') for i in range(self.n_warps))

        s0_tlb_uncacheable = Signal(self.n_threads)
        for w in range(self.n_threads):
            for origin, size in self.io_regions.items():
                with m.If((self.exec_req.bits.addr[w] >= origin)
                          & (self.exec_req.bits.addr[w] < (origin + size))):
                    m.d.comb += s0_tlb_uncacheable[w].eq(1)

        m.d.comb += self.exec_req.ready.eq(1)
        with m.If(self.exec_req.valid):
            m.d.comb += self.exec_req.ready.eq(
                ~lsq[self.exec_req.bits.wid].valid)

            with m.If(~lsq[self.exec_req.bits.wid].valid):
                m.d.sync += [
                    lsq[self.exec_req.bits.wid].valid.eq(1),
                    lsq[self.exec_req.bits.wid].uop.eq(self.exec_req.bits.uop),
                    lsq[self.exec_req.bits.wid].uop.lsq_wid.eq(
                        self.exec_req.bits.wid),
                    lsq[self.exec_req.bits.wid].executed.eq(0),
                    lsq[self.exec_req.bits.wid].succeeded.eq(0),
                    lsq[self.exec_req.bits.wid].addr_uncacheable.eq(
                        s0_tlb_uncacheable),
                ]

                for w in range(self.n_threads):
                    m.d.sync += lsq[self.exec_req.bits.wid].addr[w].eq(
                        self.exec_req.bits.addr[w])

                    with m.If(self.exec_req.bits.uop.is_sta):
                        m.d.sync += lsq[self.exec_req.bits.wid].data[w].eq(
                            self.exec_req.bits.data[w])

        s0_block_req = Array(
            Signal(self.n_threads, name=f's0_block_req{i}')
            for i in range(self.n_warps))
        s1_block_req = Array(
            Signal(self.n_threads, name=f's1_block_req{i}')
            for i in range(self.n_warps))
        s2_block_req = Array(
            Signal(self.n_threads, name=f's2_block_req{i}')
            for i in range(self.n_warps))
        for a, b in zip(s1_block_req, s0_block_req):
            m.d.sync += a.eq(b)
        for a, b in zip(s2_block_req, s1_block_req):
            m.d.sync += a.eq(b)

        lsq_wakeup_valid = Signal(self.n_warps)
        for w in range(self.n_warps):
            block = ~(lsq[w].uop.tmask
                      & ~(s0_block_req[w] | s1_block_req[w])).any()
            m.d.comb += lsq_wakeup_valid[w].eq(lsq[w].valid & ~block)

        lsq_wakeup_idx = Signal(range(self.n_warps))
        lsq_wakeup_e = lsq[lsq_wakeup_idx]
        with m.Switch(lsq_wakeup_valid):
            for i in range(self.n_warps):
                with m.Case(i):
                    for pred in reversed(range(i)):
                        with m.If(lsq_wakeup_valid[pred]):
                            m.d.sync += lsq_wakeup_idx.eq(pred)
                    for succ in reversed(range(i + 1, self.n_warps)):
                        with m.If(lsq_wakeup_valid[succ]):
                            m.d.sync += lsq_wakeup_idx.eq(succ)

        can_fire_incoming = Signal()
        can_fire_wakeup = Signal()

        m.d.comb += [
            can_fire_incoming.eq(self.exec_req.fire),
            can_fire_wakeup.eq(lsq_wakeup_e.valid
                               & (lsq_wakeup_e.uop.tmask
                                  & ~lsq_wakeup_e.executed
                                  & ~lsq_wakeup_e.succeeded
                                  & ~(s1_block_req[lsq_wakeup_idx]
                                      | s2_block_req[lsq_wakeup_idx])).any()),
        ]

        s0_executing = Array(
            Signal(self.n_threads, name=f's0_executing{i}')
            for i in range(self.n_warps))
        s1_executing = Array(
            Signal(self.n_threads, name=f's1_executing{i}')
            for i in range(self.n_warps))
        s1_set_executed = Array(
            Signal(self.n_threads, name=f's1_set_executed({i})')
            for i in range(self.n_warps))
        for a, b in zip(s1_executing, s0_executing):
            m.d.sync += a.eq(b)
        for a, b in zip(s1_set_executed, s1_executing):
            m.d.comb += a.eq(b)

        will_fire_incoming = Signal()
        will_fire_wakeup = Signal()
        m.d.comb += [
            will_fire_incoming.eq(can_fire_incoming),
            will_fire_wakeup.eq(can_fire_wakeup & ~will_fire_incoming),
        ]

        #
        # Memory access
        #

        for w in range(self.n_threads):
            dmem_req = dcache.req[w]

            with m.If(will_fire_incoming):
                m.d.comb += [
                    dmem_req.valid.eq(self.exec_req.bits.uop.tmask[w]),
                    dmem_req.bits.uop.eq(self.exec_req.bits.uop),
                    dmem_req.bits.uop.lsq_tid.eq(w),
                    dmem_req.bits.addr.eq(self.exec_req.bits.addr[w]),
                    dmem_req.bits.data.eq(self.exec_req.bits.data[w]),
                    s0_executing[self.exec_req.bits.wid][w].eq(dmem_req.fire),
                    s0_block_req[self.exec_req.bits.wid][w].eq(dmem_req.fire),
                ]

            with m.Elif(will_fire_wakeup):
                m.d.comb += [
                    dmem_req.valid.eq(lsq_wakeup_e.uop.tmask[w]
                                      & ~lsq_wakeup_e.executed[w]
                                      & ~lsq_wakeup_e.succeeded[w]),
                    dmem_req.bits.uop.eq(lsq_wakeup_e.uop),
                    dmem_req.bits.uop.lsq_tid.eq(w),
                    dmem_req.bits.addr.eq(lsq_wakeup_e.addr[w]),
                    dmem_req.bits.data.eq(lsq_wakeup_e.data[w]),
                    s0_executing[lsq_wakeup_idx][w].eq(dmem_req.fire),
                    s0_block_req[lsq_wakeup_idx][w].eq(dmem_req.fire),
                ]

        for i in range(self.n_warps):
            with m.If(s1_set_executed[i].any()):
                m.d.sync += lsq[i].executed.eq(lsq[i].executed
                                               | s1_set_executed[i])

        #
        # Writeback
        #

        for w in range(self.n_threads):
            with m.If(dcache.nack[w].valid):
                lsq_wid = dcache.nack[w].bits.uop.lsq_wid

                with m.Switch(dcache.nack[w].bits.uop.lsq_tid):
                    for t in range(self.n_threads):
                        with m.Case(t):
                            m.d.sync += lsq[lsq_wid].executed[t].eq(0)

            with m.If(dcache.resp[w].valid):
                lsq_wid = dcache.resp[w].bits.uop.lsq_wid

                with m.Switch(dcache.resp[w].bits.uop.lsq_tid):
                    for t in range(self.n_threads):
                        with m.Case(t):
                            m.d.sync += lsq[lsq_wid].succeeded[t].eq(1)
                            with m.If(lsq[lsq_wid].uop.is_load):
                                m.d.sync += lsq[lsq_wid].data[t].eq(
                                    dcache.resp[w].bits.data)

        #
        # Commit
        #
        for w in reversed(range(self.n_warps)):
            with m.If(lsq[w].valid & (lsq[w].uop.tmask == lsq[w].succeeded)):
                m.d.comb += [
                    self.exec_iresp.valid.eq(1),
                    self.exec_iresp.bits.wid.eq(w),
                    self.exec_iresp.bits.uop.eq(lsq[w].uop),
                    Cat(*self.exec_iresp.bits.data).eq(Cat(*lsq[w].data)),
                ]

        with m.If(self.exec_iresp.fire):
            m.d.sync += lsq[self.exec_iresp.bits.wid].valid.eq(0)

        return m
