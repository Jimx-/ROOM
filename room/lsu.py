from amaranth import *
from amaranth.utils import log2_int

from room.consts import *
from room.types import MicroOp
from room.alu import ExecResp
from room.branch import BranchUpdate
from room.rob import CommitReq

from roomsoc.interconnect import wishbone


class RRPriorityEncoder(Elaboratable):

    def __init__(self, width, is_head=True):
        self.width = width
        self.is_head = is_head

        self.i = Signal(width)
        self.head_or_tail = Signal(range(width))
        self.o = Signal(range(width))
        self.n = Signal()

    def elaborate(self, platform):
        m = Module()

        if self.is_head:
            for j in reversed(range(self.width)):
                with m.If(self.i[j]):
                    m.d.comb += self.o.eq(j)

            for j in reversed(range(self.width)):
                with m.If(self.i[j] & (j >= self.head_or_tail)):
                    m.d.comb += self.o.eq(j)
        else:
            for j in range(self.width):
                with m.If(self.i[j]):
                    m.d.comb += self.o.eq(j)

            for j in range(self.width):
                with m.If(self.i[j] & (j < self.head_or_tail)):
                    m.d.comb += self.o.eq(j)

        m.d.comb += self.n.eq(self.i == 0)
        return m


class DCacheReq:

    def __init__(self, params, name=None):
        name = (name is not None) and f'{name}_' or ''

        self.valid = Signal(name=f'{name}valid')
        self.uop = MicroOp(params, name=f'{name}uop')
        self.kill = Signal(name=f'{name}kill')

        self.addr = Signal(32, name=f'{name}addr')
        self.data = Signal(32, name=f'{name}data')

        self.ready = Signal(name=f'{name}ready')


class DCacheResp:

    def __init__(self, params, name=None):
        name = (name is not None) and f'{name}_' or ''

        self.valid = Signal(name=f'{name}valid')
        self.uop = MicroOp(params, name=f'{name}uop')
        self.data = Signal(32, name=f'{name}data')


class StoreGen(Elaboratable):

    def __init__(self, max_size):
        self.log_max_size = log2_int(max_size)

        self.typ = Signal(2)
        self.addr = Signal(max_size)
        self.data_in = Signal(max_size * 8)

        self.mask = Signal(max_size)
        self.data_out = Signal(max_size * 8)

    def elaborate(self, platform):
        m = Module()

        for i in range(self.log_max_size + 1):
            with m.If(self.typ == i):
                m.d.comb += self.data_out.eq(
                    Repl(self.data_in[:(8 << i)], 1 <<
                         (self.log_max_size - i)))

        mask = 1
        for i in range(self.log_max_size):
            upper = Mux(self.addr[i], mask, 0) | Mux(self.typ >= (i + 1),
                                                     (1 << (1 << i)) - 1, 0)
            lower = Mux(self.addr[i], 0, mask)
            mask = Cat(lower, upper)
        m.d.comb += self.mask.eq(mask)

        return m


class LoadGen(Elaboratable):

    def __init__(self, max_size):
        self.log_max_size = log2_int(max_size)

        self.typ = Signal(2)
        self.signed = Signal()
        self.addr = Signal(max_size)
        self.data_in = Signal(max_size * 8)

        self.data_out = Signal(max_size * 8)

    def elaborate(self, platform):
        m = Module()

        res = self.data_in
        for i in range(self.log_max_size - 1, -1, -1):
            pos = 8 << i
            shifted = Mux(self.addr[i], res[pos:pos * 2], res[:pos])
            res = Cat(
                shifted,
                Mux(self.typ == i,
                    Repl(self.signed & shifted[pos - 1], 32 - pos), res[pos:]))
        m.d.comb += self.data_out.eq(res)

        return m


class DataCache(Elaboratable):

    def __init__(self, params):
        self.mem_width = params['mem_width']
        self.log_max_size = 2  # 4 bytes
        self.params = params

        self.dbus = wishbone.Interface(data_width=32,
                                       addr_width=30,
                                       granularity=8)

        self.reqs = Array(
            DCacheReq(params, name=f'req{i}') for i in range(self.mem_width))
        self.resps = Array(
            DCacheResp(params, name=f'resp{i}') for i in range(self.mem_width))

        self.br_update = BranchUpdate(params)
        self.exception = Signal()

    def elaborate(self, platform):
        m = Module()

        last_grant = Signal(range(self.mem_width))
        choice = Signal.like(last_grant)
        chosen = Signal.like(last_grant)

        for w in reversed(range(self.mem_width)):
            with m.If(self.reqs[w].valid):
                m.d.comb += choice.eq(w)
        for w in reversed(range(self.mem_width)):
            with m.If(self.reqs[w].valid & (w > last_grant)):
                m.d.comb += choice.eq(w)

        uop = MicroOp(self.params)

        req_addr = Signal(32)
        dat_w = Signal.like(self.dbus.dat_w)
        sel = Signal.like(self.dbus.sel)
        we = Signal.like(self.dbus.we)

        req_chosen = self.reqs[chosen]

        load_gen = m.submodules.load_gen = LoadGen(max_size=4)
        m.d.comb += [
            load_gen.typ.eq(uop.mem_size),
            load_gen.signed.eq(uop.mem_signed),
            load_gen.addr.eq(req_addr),
            load_gen.data_in.eq(self.dbus.dat_r),
        ]

        store_gen = m.submodules.store_gen = StoreGen(max_size=4)
        m.d.comb += [
            store_gen.typ.eq(req_chosen.uop.mem_size),
            store_gen.addr.eq(req_chosen.addr),
            store_gen.data_in.eq(req_chosen.data),
        ]

        with m.FSM():
            with m.State('IDLE'):
                m.d.comb += chosen.eq(choice),

                with m.If(req_chosen.valid
                          & ~self.br_update.uop_killed(req_chosen.uop)
                          & ~(self.exception & req_chosen.uop.uses_ldq)):
                    m.d.comb += [
                        self.dbus.adr.eq(req_chosen.addr[2:]),
                        self.dbus.stb.eq(1),
                        self.dbus.dat_w.eq(store_gen.data_out),
                        self.dbus.cyc.eq(1),
                        self.dbus.sel.eq(Mux(self.dbus.we, store_gen.mask,
                                             ~0)),
                        self.dbus.we.eq(
                            req_chosen.uop.mem_cmd == MemoryCommand.WRITE),
                        req_chosen.ready.eq(1),
                    ]

                    m.d.sync += [
                        uop.eq(req_chosen.uop),
                        uop.br_mask.eq(
                            self.br_update.get_new_br_mask(
                                req_chosen.uop.br_mask)),
                        req_addr.eq(req_chosen.addr),
                        dat_w.eq(self.dbus.dat_w),
                        sel.eq(self.dbus.sel),
                        we.eq(self.dbus.we),
                        last_grant.eq(choice),
                    ]

                    m.next = 'WAIT_ACK'

            with m.State('WAIT_ACK'):
                m.d.comb += [
                    chosen.eq(last_grant),
                    self.dbus.adr.eq(req_addr[2:]),
                    self.dbus.stb.eq(self.dbus.cyc),
                    self.dbus.dat_w.eq(dat_w),
                    self.dbus.cyc.eq(~req_chosen.kill),
                    self.dbus.sel.eq(sel),
                    self.dbus.we.eq(we),
                ]

                m.d.sync += [
                    uop.br_mask.eq(self.br_update.get_new_br_mask(
                        uop.br_mask)),
                ]

                with m.If(req_chosen.kill | self.br_update.uop_killed(uop)
                          | (self.exception & uop.uses_ldq)):
                    m.next = 'IDLE'
                with m.Elif(self.dbus.cyc & self.dbus.stb & self.dbus.ack):
                    m.d.comb += [
                        self.resps[chosen].valid.eq(1),
                        self.resps[chosen].data.eq(load_gen.data_out),
                        self.resps[chosen].uop.eq(uop),
                        self.resps[chosen].uop.br_mask.eq(
                            self.br_update.get_new_br_mask(uop.br_mask)),
                    ]

                    m.next = 'IDLE'

        return m


class LDQEntry:

    def __init__(self, params, name=None):
        name = (name is not None) and f'{name}_' or ''

        self.valid = Signal(name=f'{name}valid')
        self.uop = MicroOp(params, name=f'{name}uop')

        self.addr = Signal(32, name=f'{name}addr')
        self.addr_valid = Signal(name=f'{name}addr_valid')

        self.executed = Signal(name=f'{name}executed')
        self.succeeded = Signal(name=f'{name}succeeded')

        self.st_dep_mask = Signal(params['stq_size'],
                                  name=f'{name}st_dep_mask')

    def eq(self, rhs):
        attrs = [
            'valid',
            'uop',
            'addr',
            'addr_valid',
            'executed',
            'succeeded',
            'st_dep_mask',
        ]
        return [getattr(self, a).eq(getattr(rhs, a)) for a in attrs]


class STQEntry:

    def __init__(self, params, name=None):
        name = (name is not None) and f'{name}_' or ''

        self.valid = Signal(name=f'{name}valid')
        self.uop = MicroOp(params, name=f'{name}uop')

        self.addr = Signal(32, name=f'{name}addr')
        self.addr_valid = Signal(name=f'{name}addr_valid')
        self.data = Signal(32, name=f'{name}data')
        self.data_valid = Signal(name=f'{name}data_valid')

        self.committed = Signal(name=f'{name}committed')
        self.succeeded = Signal(name=f'{name}succeeded')

    def eq(self, rhs):
        attrs = [
            'valid',
            'uop',
            'addr',
            'addr_valid',
            'data',
            'data_valid',
            'committed',
            'succeeded',
        ]
        return [getattr(self, a).eq(getattr(rhs, a)) for a in attrs]


def _incr(signal, modulo):
    if modulo == 2**len(signal):
        return (signal + 1)[:len(signal)]
    else:
        return Mux(signal == modulo - 1, 0, signal + 1)


def gen_byte_mask(addr, size):
    return Mux(
        size == 0, 1 << addr[:3],
        Mux(size == 1, 3 << (addr[1:3] << 1),
            Mux(size == 2, Mux(addr[2], 0xf0, 0xf), Mux(size == 3, 0xff, 0))))


class LoadStoreUnit(Elaboratable):

    def __init__(self, dbus, params):
        self.core_width = params['core_width']
        self.ldq_size = params['ldq_size']
        self.stq_size = params['stq_size']
        self.mem_width = params['mem_width']
        self.params = params

        self.dbus = dbus

        self.dis_uops = [
            MicroOp(params, name=f'dis_uop{i}') for i in range(self.core_width)
        ]
        self.dis_valids = Signal(self.core_width)

        self.ldq_full = Signal(self.core_width)
        self.stq_full = Signal(self.core_width)

        self.dis_ldq_idx = [
            Signal(range(self.ldq_size), name=f'dis_ldq_idx{i}')
            for i in range(self.core_width)
        ]
        self.dis_stq_idx = [
            Signal(range(self.stq_size), name=f'dis_stq_idx{i}')
            for i in range(self.core_width)
        ]

        self.exec_reqs = [
            ExecResp(self.params, name=f'exec_req{i}')
            for i in range(self.mem_width)
        ]

        self.exec_iresps = [
            ExecResp(self.params, name=f'exec_iresp{i}')
            for i in range(self.mem_width)
        ]

        self.clear_busy_valids = Signal(self.mem_width)
        self.clear_busy_idx = [
            Signal(range(self.core_width * params['num_rob_rows']),
                   name=f'clear_busy_idx{i}') for i in range(self.mem_width)
        ]

        self.commit = CommitReq(params)
        self.exception = Signal()

        self.br_update = BranchUpdate(params)

    def elaborate(self, platform):
        m = Module()

        dcache = m.submodules.dcache = DataCache(self.params)
        m.d.comb += [
            dcache.dbus.connect(self.dbus),
            dcache.br_update.eq(self.br_update),
        ]

        ldq = Array(
            LDQEntry(self.params, name=f'ldq{i}')
            for i in range(self.ldq_size))
        stq = Array(
            STQEntry(self.params, name=f'stq{i}')
            for i in range(self.stq_size))

        ldq_head = Signal(range(self.ldq_size))
        ldq_tail = Signal.like(ldq_head)
        stq_head = Signal(range(self.stq_size))
        stq_tail = Signal.like(stq_head)
        stq_commit_head = Signal.like(stq_head)
        stq_execute_head = Signal.like(stq_head)

        clear_store = Signal()
        live_store_mask = Signal(self.stq_size)
        next_live_store_mask = Mux(clear_store,
                                   live_store_mask & ~(1 << stq_head),
                                   live_store_mask)

        with m.If(clear_store):
            for i in range(self.ldq_size):
                m.d.sync += ldq[i].st_dep_mask.eq(ldq[i].st_dep_mask
                                                  & ~(1 << stq_head))

        ldq_w_idx = ldq_tail
        stq_w_idx = stq_tail

        for w in range(self.core_width):
            m.d.comb += [
                self.ldq_full[w].eq(
                    _incr(ldq_w_idx, self.ldq_size) == ldq_head),
                self.stq_full[w].eq(
                    _incr(stq_w_idx, self.stq_size) == stq_head),
                self.dis_ldq_idx[w].eq(ldq_w_idx),
                self.dis_stq_idx[w].eq(stq_w_idx),
            ]

            dis_w_ldq = self.dis_valids[w] & self.dis_uops[w].uses_ldq
            dis_w_stq = self.dis_valids[w] & self.dis_uops[w].uses_stq

            with m.If(dis_w_ldq):
                m.d.sync += [
                    ldq[self.dis_ldq_idx[w]].valid.eq(1),
                    ldq[self.dis_ldq_idx[w]].uop.eq(self.dis_uops[w]),
                    ldq[self.dis_ldq_idx[w]].addr_valid.eq(0),
                    ldq[self.dis_ldq_idx[w]].executed.eq(0),
                    ldq[self.dis_ldq_idx[w]].succeeded.eq(0),
                    ldq[self.dis_ldq_idx[w]].st_dep_mask.eq(
                        next_live_store_mask),
                ]
            with m.Elif(dis_w_stq):
                m.d.sync += [
                    stq[self.dis_stq_idx[w]].valid.eq(1),
                    stq[self.dis_stq_idx[w]].uop.eq(self.dis_uops[w]),
                    stq[self.dis_stq_idx[w]].addr_valid.eq(0),
                    stq[self.dis_stq_idx[w]].data_valid.eq(0),
                    stq[self.dis_stq_idx[w]].committed.eq(0),
                    stq[self.dis_stq_idx[w]].succeeded.eq(0),
                ]

            ldq_w_idx = Mux(dis_w_ldq, _incr(ldq_w_idx, self.ldq_size),
                            ldq_w_idx)

            next_live_store_mask = Mux(dis_w_stq,
                                       next_live_store_mask | (1 << stq_w_idx),
                                       next_live_store_mask)
            stq_w_idx = Mux(dis_w_stq, _incr(stq_w_idx, self.stq_size),
                            stq_w_idx)

        m.d.sync += [
            ldq_tail.eq(ldq_w_idx),
            stq_tail.eq(stq_w_idx),
        ]

        ldq_retry_enc = RRPriorityEncoder(self.ldq_size)
        m.submodules += ldq_retry_enc
        for i in range(self.ldq_size):
            m.d.comb += ldq_retry_enc.i[i].eq(ldq[i].addr_valid
                                              & ~ldq[i].executed
                                              & ~ldq[i].succeeded)
        m.d.comb += ldq_retry_enc.head_or_tail.eq(ldq_head)
        ldq_retry_idx = ldq_retry_enc.o
        ldq_retry_e = ldq[ldq_retry_idx]
        s1_ldq_retry_idx = Signal(range(self.ldq_size))
        m.d.sync += s1_ldq_retry_idx.eq(ldq_retry_idx)

        stq_commit_e = stq[stq_execute_head]

        can_fire_load_incoming = Signal(self.mem_width)
        can_fire_sta_incoming = Signal(self.mem_width)
        can_fire_std_incoming = Signal(self.mem_width)
        can_fire_stad_incoming = Signal(self.mem_width)
        can_fire_load_retry = Signal(self.mem_width)
        can_fire_store_commit = Signal(self.mem_width)

        s0_executing_loads = Array(
            Signal(name=f's0_executing{i}') for i in range(self.ldq_size))
        s1_executing_loads = Array(
            Signal(name=f's1_executing{i}') for i in range(self.ldq_size))
        s1_set_executed = Array(
            Signal(name=f's1_set_executed({i})') for i in range(self.ldq_size))
        for a, b in zip(s1_executing_loads, s0_executing_loads):
            m.d.sync += a.eq(b)
        for a, b in zip(s1_set_executed, s1_executing_loads):
            m.d.comb += a.eq(b)

        for w in range(self.mem_width):
            m.d.comb += [
                can_fire_load_incoming[w].eq(self.exec_reqs[w].valid
                                             & self.exec_reqs[w].uop.is_load),
                can_fire_sta_incoming[w].eq(self.exec_reqs[w].valid
                                            & self.exec_reqs[w].uop.is_sta
                                            & ~self.exec_reqs[w].uop.is_std),
                can_fire_std_incoming[w].eq(self.exec_reqs[w].valid
                                            & self.exec_reqs[w].uop.is_std
                                            & ~self.exec_reqs[w].uop.is_sta),
                can_fire_stad_incoming[w].eq(self.exec_reqs[w].valid
                                             & self.exec_reqs[w].uop.is_sta
                                             & self.exec_reqs[w].uop.is_std),
                can_fire_load_retry[w].eq(ldq_retry_e.valid
                                          & ldq_retry_e.addr_valid
                                          & ~ldq_retry_e.executed
                                          & ~ldq_retry_e.succeeded
                                          & ~s1_executing_loads[ldq_retry_idx]
                                          & (w == 0)
                                          & (ldq_retry_e.st_dep_mask == 0)),
                can_fire_store_commit[w].eq(stq_commit_e.valid
                                            & stq_commit_e.committed
                                            & (w == self.mem_width - 1)),
            ]

        will_fire_load_incoming = Signal(self.mem_width)
        will_fire_sta_incoming = Signal(self.mem_width)
        will_fire_std_incoming = Signal(self.mem_width)
        will_fire_stad_incoming = Signal(self.mem_width)
        will_fire_load_retry = Signal(self.mem_width)
        will_fire_store_commit = Signal(self.mem_width)

        for w in range(self.mem_width):
            dc_avail = 1
            cam_avail = 1

            def sched(can_fire, uses_dc, uses_cam, dc_avail, cam_avail):
                will_fire = can_fire & ~(uses_dc & ~dc_avail) & ~(uses_cam
                                                                  & ~cam_avail)
                dc_avail &= ~(will_fire & uses_dc)
                cam_avail &= ~(will_fire & uses_dc)
                return will_fire, dc_avail, cam_avail

            will_fire_load_incoming_, dc_avail, cam_avail = sched(
                can_fire_load_incoming[w], 1, 1, dc_avail, cam_avail)
            will_fire_stad_incoming_, dc_avail, cam_avail = sched(
                can_fire_stad_incoming[w], 0, 1, dc_avail, cam_avail)
            will_fire_sta_incoming_, dc_avail, cam_avail = sched(
                can_fire_sta_incoming[w], 0, 1, dc_avail, cam_avail)
            will_fire_std_incoming_, dc_avail, cam_avail = sched(
                can_fire_std_incoming[w], 0, 0, dc_avail, cam_avail)
            will_fire_load_retry_, dc_avail, cam_avail = sched(
                can_fire_load_retry[w], 1, 1, dc_avail, cam_avail)
            will_fire_store_commit_, _, _ = sched(can_fire_store_commit[w], 1,
                                                  0, dc_avail, cam_avail)

            m.d.comb += [
                will_fire_load_incoming[w].eq(will_fire_load_incoming_),
                will_fire_stad_incoming[w].eq(will_fire_stad_incoming_),
                will_fire_sta_incoming[w].eq(will_fire_sta_incoming_),
                will_fire_std_incoming[w].eq(will_fire_std_incoming_),
                will_fire_load_retry[w].eq(will_fire_load_retry_),
                will_fire_store_commit[w].eq(will_fire_store_commit_),
            ]

        for w in range(self.mem_width):
            with m.If(will_fire_load_incoming[w]):
                ldq_idx = self.exec_reqs[w].uop.ldq_idx
                m.d.sync += [
                    ldq[ldq_idx].addr.eq(self.exec_reqs[w].addr),
                    ldq[ldq_idx].addr_valid.eq(1),
                    ldq[ldq_idx].uop.pdst.eq(self.exec_reqs[w].uop.pdst),
                ]

            with m.If(will_fire_sta_incoming[w] | will_fire_stad_incoming[w]):
                stq_idx = self.exec_reqs[w].uop.stq_idx
                m.d.sync += [
                    stq[stq_idx].addr.eq(self.exec_reqs[w].addr),
                    stq[stq_idx].addr_valid.eq(1),
                    stq[stq_idx].uop.pdst.eq(self.exec_reqs[w].uop.pdst),
                ]

            with m.If(will_fire_std_incoming[w] | will_fire_stad_incoming[w]):
                stq_idx = self.exec_reqs[w].uop.stq_idx
                m.d.sync += [
                    stq[stq_idx].data.eq(self.exec_reqs[w].data),
                    stq[stq_idx].data_valid.eq(1),
                ]

        #
        # Memory access
        #

        m.d.comb += dcache.exception.eq(self.exception)

        for w in range(self.mem_width):
            dmem_req = dcache.reqs[w]

            with m.If(will_fire_load_incoming[w]):
                m.d.comb += [
                    dmem_req.valid.eq(1),
                    dmem_req.uop.eq(self.exec_reqs[w].uop),
                    dmem_req.addr.eq(self.exec_reqs[w].addr),
                    s0_executing_loads[self.exec_reqs[w].uop.ldq_idx].eq(
                        dmem_req.ready),
                ]
            with m.Elif(will_fire_load_retry[w]):
                m.d.comb += [
                    dmem_req.valid.eq(1),
                    dmem_req.uop.eq(ldq_retry_e.uop),
                    dmem_req.addr.eq(ldq_retry_e.addr),
                    s0_executing_loads[ldq_retry_idx].eq(dmem_req.ready),
                ]
            with m.Elif(will_fire_store_commit[w]):
                m.d.comb += [
                    dmem_req.valid.eq(1),
                    dmem_req.uop.eq(stq_commit_e.uop),
                    dmem_req.addr.eq(stq_commit_e.addr),
                    dmem_req.data.eq(stq_commit_e.data),
                ]

                m.d.sync += [
                    stq_execute_head.eq(
                        Mux(dmem_req.ready,
                            _incr(stq_execute_head, self.stq_size),
                            stq_execute_head)),
                    stq[stq_execute_head].succeeded.eq(0),
                ]

        for i in range(self.ldq_size):
            with m.If(s1_set_executed[i]):
                m.d.sync += ldq[i].executed.eq(1)

        dmem_req_fired = Signal(self.mem_width)

        fired_load_incoming = Signal(self.mem_width)
        fired_load_retry = Signal(self.mem_width)
        fired_sta_incoming = Signal(self.mem_width)
        fired_std_incoming = Signal(self.mem_width)
        fired_stad_incoming = Signal(self.mem_width)

        fired_incoming_uop = [
            MicroOp(self.params, name=f'fired_incoming_uop{i}')
            for i in range(self.mem_width)
        ]

        fired_ldq_incoming_e = [
            LDQEntry(self.params, name=f'fired_ldq_incoming_e{i}')
            for i in range(self.mem_width)
        ]
        fired_ldq_retry_e = [
            LDQEntry(self.params, name=f'fired_ldq_retry_e{i}')
            for i in range(self.mem_width)
        ]

        fired_stq_incoming_e = [
            STQEntry(self.params, name=f'fired_stq_incoming_e{i}')
            for i in range(self.mem_width)
        ]

        fired_ldq_e = [
            LDQEntry(self.params, name=f'fired_ldq_e{i}')
            for i in range(self.mem_width)
        ]

        fired_mem_addr = [
            Signal(32, name=f'fired_mem_addr{i}')
            for i in range(self.mem_width)
        ]

        for w in range(self.mem_width):
            req_killed = self.br_update.uop_killed(self.exec_reqs[w].uop)

            with m.If(fired_load_incoming[w]):
                m.d.comb += fired_ldq_e[w].eq(fired_ldq_incoming_e[w])
            with m.Elif(fired_load_retry[w]):
                m.d.comb += fired_ldq_e[w].eq(fired_ldq_retry_e[w])

            m.d.sync += [
                dmem_req_fired[w].eq(dcache.reqs[w].valid
                                     & dcache.reqs[w].ready),
                fired_incoming_uop[w].eq(self.exec_reqs[w].uop),
                fired_incoming_uop[w].br_mask.eq(
                    self.br_update.get_new_br_mask(
                        self.exec_reqs[w].uop.br_mask)),
                fired_load_incoming[w].eq(will_fire_load_incoming[w]
                                          & ~req_killed),
                fired_load_retry[w].eq(will_fire_load_retry[w] & ~req_killed),
                fired_sta_incoming[w].eq(will_fire_sta_incoming[w]
                                         & ~req_killed),
                fired_std_incoming[w].eq(will_fire_std_incoming[w]
                                         & ~req_killed),
                fired_stad_incoming[w].eq(will_fire_stad_incoming[w]
                                          & ~req_killed),
                fired_ldq_incoming_e[w].eq(ldq[self.exec_reqs[w].uop.ldq_idx]),
                fired_ldq_incoming_e[w].uop.br_mask.eq(
                    self.br_update.get_new_br_mask(
                        ldq[self.exec_reqs[w].uop.ldq_idx].uop.br_mask)),
                fired_ldq_retry_e[w].eq(ldq_retry_e),
                fired_ldq_retry_e[w].uop.br_mask.eq(
                    self.br_update.get_new_br_mask(ldq_retry_e.uop.br_mask)),
                fired_stq_incoming_e[w].eq(stq[self.exec_reqs[w].uop.stq_idx]),
                fired_stq_incoming_e[w].uop.br_mask.eq(
                    self.br_update.get_new_br_mask(
                        stq[self.exec_reqs[w].uop.stq_idx].uop.br_mask)),
                fired_mem_addr[w].eq(dcache.reqs[w].addr),
            ]

        #
        # Clear ROB busy
        #

        clr_bsy_valids = Signal(self.mem_width)
        clr_bsy_rob_idx = [
            Signal.like(self.clear_busy_idx[0], name=f'clr_bsy_rob_idx{i}')
            for i in range(self.mem_width)
        ]
        clr_bsy_br_mask = [
            Signal.like(self.br_update.resolve_mask,
                        name=f'clr_bsy_br_mask{i}')
            for i in range(self.mem_width)
        ]

        for w in range(self.mem_width):
            m.d.sync += [
                clr_bsy_valids[w].eq(0),
                clr_bsy_rob_idx[w].eq(0),
                clr_bsy_br_mask[w].eq(0),
            ]

            with m.If(fired_stad_incoming[w]):
                m.d.sync += [
                    clr_bsy_valids[w].eq(fired_stq_incoming_e[w].valid
                                         & ~self.br_update.uop_killed(
                                             fired_stq_incoming_e[w].uop)),
                    clr_bsy_rob_idx[w].eq(fired_stq_incoming_e[w].uop.rob_idx),
                    clr_bsy_br_mask[w].eq(
                        self.br_update.get_new_br_mask(
                            fired_stq_incoming_e[w].uop.br_mask)),
                ]
            with m.Elif(fired_sta_incoming[w]):
                m.d.sync += [
                    clr_bsy_valids[w].eq(fired_stq_incoming_e[w].valid
                                         & fired_stq_incoming_e[w].data_valid
                                         & ~self.br_update.uop_killed(
                                             fired_stq_incoming_e[w].uop)),
                    clr_bsy_rob_idx[w].eq(fired_stq_incoming_e[w].uop.rob_idx),
                    clr_bsy_br_mask[w].eq(
                        self.br_update.get_new_br_mask(
                            fired_stq_incoming_e[w].uop.br_mask)),
                ]
            with m.Elif(fired_std_incoming[w]):
                m.d.sync += [
                    clr_bsy_valids[w].eq(fired_stq_incoming_e[w].valid
                                         & fired_stq_incoming_e[w].addr_valid
                                         & ~self.br_update.uop_killed(
                                             fired_stq_incoming_e[w].uop)),
                    clr_bsy_rob_idx[w].eq(fired_stq_incoming_e[w].uop.rob_idx),
                    clr_bsy_br_mask[w].eq(
                        self.br_update.get_new_br_mask(
                            fired_stq_incoming_e[w].uop.br_mask)),
                ]

            m.d.comb += [
                self.clear_busy_valids[w].eq(
                    clr_bsy_valids[w]
                    & ~self.br_update.br_mask_killed(clr_bsy_br_mask[w])),
                self.clear_busy_idx[w].eq(clr_bsy_rob_idx[w]),
            ]

        #
        # Memory ordering
        #

        do_ld_search = Cat((fired_load_incoming[w] | fired_load_retry[w])
                           for w in range(self.mem_width))

        cam_addr = [
            Signal(32, name=f'cam_addr{i}') for i in range(self.mem_width)
        ]
        cam_uop = [
            MicroOp(self.params, name=f'cam_uop{i}')
            for i in range(self.mem_width)
        ]
        cam_mask = [
            Signal(8, name=f'cam_mask{i}') for i in range(self.mem_width)
        ]

        cam_ldq_idx = [
            Signal(range(self.ldq_size), name=f'cam_ldq_idx{i}')
            for i in range(self.mem_width)
        ]

        for w in range(self.mem_width):
            m.d.comb += [
                cam_addr[w].eq(fired_mem_addr[w]),
                cam_uop[w].eq(Mux(do_ld_search[w], fired_ldq_e[w].uop, 0)),
                cam_mask[w].eq(gen_byte_mask(cam_addr[w],
                                             cam_uop[w].mem_size)),
                cam_ldq_idx[w].eq(
                    Mux(fired_load_incoming[w], fired_incoming_uop[w].ldq_idx,
                        Mux(fired_load_retry[w], s1_ldq_retry_idx, 0))),
            ]

        ldst_addr_matches = [
            Signal(self.stq_size, name=f'ldst_addr_matches{i}')
            for i in range(self.mem_width)
        ]
        ldst_forward_matches = [
            Signal(self.stq_size, name=f'ldst_forward_matches{i}')
            for i in range(self.mem_width)
        ]

        for i in range(self.stq_size):
            addr_matches = Cat(
                (stq[i].addr_valid & (stq[i].addr[3:] == cam_addr[w][3:]))
                for w in range(self.mem_width))

            write_mask = gen_byte_mask(stq[i].addr, stq[i].uop.mem_size)

            for w in range(self.mem_width):
                with m.If(do_ld_search[w] & stq[i].valid
                          & fired_ldq_e[w].st_dep_mask[i]):
                    with m.If(((cam_mask[w] & write_mask) == cam_mask[w])
                              & addr_matches[w]):
                        m.d.comb += [
                            ldst_addr_matches[w][i].eq(1),
                            ldst_forward_matches[w][i].eq(1),
                            dcache.reqs[w].kill.eq(dmem_req_fired[w]),
                            s1_set_executed[cam_ldq_idx[w]].eq(0),
                        ]
                    with m.Elif(((cam_mask[w] & write_mask) != 0)
                                & addr_matches[w]):
                        m.d.comb += [
                            ldst_addr_matches[w][i].eq(1),
                            dcache.reqs[w].kill.eq(dmem_req_fired[w]),
                            s1_set_executed[cam_ldq_idx[w]].eq(0),
                        ]

        mem_forward_valid = Signal(self.mem_width)
        mem_forward_stq_idx = [
            Signal(range(self.stq_size), name=f'mem_forward_stq_idx{i}')
            for i in range(self.mem_width)
        ]

        wb_forward_valid = Signal(self.mem_width)
        wb_forward_ldq_idx = [
            Signal(range(self.ldq_size), name=f'wb_forward_ldq_idx{i}')
            for i in range(self.mem_width)
        ]
        wb_forward_stq_idx = [
            Signal(range(self.stq_size), name=f'wb_forward_stq_idx{i}')
            for i in range(self.mem_width)
        ]

        for w in range(self.mem_width):
            enc = RRPriorityEncoder(self.stq_size, is_head=False)
            m.submodules += enc
            m.d.comb += [
                enc.i.eq(ldst_addr_matches[w]),
                enc.head_or_tail.eq(cam_uop[w].stq_idx),
                mem_forward_stq_idx[w].eq(enc.o),
                mem_forward_valid[w].eq(
                    ((ldst_forward_matches[w] &
                      (1 << mem_forward_stq_idx[w])) != 0)
                    & ~self.br_update.uop_killed(cam_uop[w])),
            ]

            m.d.sync += [
                wb_forward_valid[w].eq(mem_forward_valid[w]),
                wb_forward_ldq_idx[w].eq(cam_ldq_idx[w]),
                wb_forward_stq_idx[w].eq(mem_forward_stq_idx[w]),
            ]

        #
        # Writeback
        #

        dmem_resp_fire = Signal(self.mem_width)

        for w in range(self.mem_width):
            with m.If(dcache.resps[w].valid):
                with m.If(dcache.resps[w].uop.uses_ldq):
                    ldq_idx = dcache.resps[w].uop.ldq_idx
                    iresp = self.exec_iresps[w]

                    m.d.comb += [
                        iresp.uop.eq(ldq[ldq_idx].uop),
                        iresp.valid.eq(
                            ldq[ldq_idx].uop.dst_rtype == RegisterType.FIX),
                        iresp.data.eq(dcache.resps[w].data),
                        dmem_resp_fire[w].eq(1),
                    ]

                    m.d.sync += ldq[ldq_idx].succeeded.eq(1)
                with m.If(dcache.resps[w].uop.uses_stq):
                    stq_idx = dcache.resps[w].uop.stq_idx
                    m.d.comb += dmem_resp_fire[w].eq(1)
                    m.d.sync += stq[stq_idx].succeeded.eq(1)

            with m.If(~dmem_resp_fire[w] & wb_forward_valid[w]):
                ldq_e = ldq[wb_forward_ldq_idx[w]]
                stq_e = stq[wb_forward_stq_idx[w]]

                data_valid = stq_e.data_valid
                live = ~self.br_update.uop_killed(ldq_e.uop)

                load_gen = LoadGen(max_size=4)
                store_gen = StoreGen(max_size=4)
                m.submodules += [load_gen, store_gen]

                m.d.comb += [
                    store_gen.typ.eq(stq_e.uop.mem_size),
                    store_gen.addr.eq(stq_e.addr),
                    store_gen.data_in.eq(stq_e.data),
                    load_gen.typ.eq(ldq_e.uop.mem_size),
                    load_gen.signed.eq(ldq_e.uop.mem_signed),
                    load_gen.addr.eq(ldq_e.addr),
                    load_gen.data_in.eq(store_gen.data_out),
                ]

                m.d.comb += [
                    iresp.uop.eq(ldq_e.uop),
                    iresp.valid.eq(ldq_e.uop.dst_rtype == RegisterType.FIX
                                   & data_valid & live),
                    iresp.data.eq(load_gen.data_out),
                ]

                with m.If(data_valid & live):
                    m.d.sync += ldq_e.succeeded.eq(1)

        #
        # Branch mispredict
        #

        st_brkilled_mask = Signal(self.stq_size)

        for i in range(self.stq_size):
            with m.If(stq[i].valid):
                m.d.sync += stq[i].uop.br_mask.eq(
                    self.br_update.get_new_br_mask(stq[i].uop.br_mask))

                with m.If(self.br_update.uop_killed(stq[i].uop)):
                    m.d.sync += [
                        stq[i].valid.eq(0),
                        stq[i].addr_valid.eq(0),
                        stq[i].data_valid.eq(0),
                    ]
                    m.d.comb += st_brkilled_mask[i].eq(1)

        for i in range(self.ldq_size):
            with m.If(ldq[i].valid):
                m.d.sync += ldq[i].uop.br_mask.eq(
                    self.br_update.get_new_br_mask(ldq[i].uop.br_mask))

                with m.If(self.br_update.uop_killed(ldq[i].uop)):
                    m.d.sync += [
                        ldq[i].valid.eq(0),
                        ldq[i].addr_valid.eq(0),
                    ]

        with m.If(self.br_update.br_res.mispredict):
            m.d.sync += [
                stq_tail.eq(self.br_update.br_res.uop.stq_idx),
                ldq_tail.eq(self.br_update.br_res.uop.ldq_idx),
            ]

        #
        # Commit
        #

        next_ldq_head = ldq_head
        next_stq_commit_head = stq_commit_head

        for w in range(self.core_width):
            commit_store = self.commit.valids[w] & self.commit.uops[w].uses_stq
            commit_load = self.commit.valids[w] & self.commit.uops[w].uses_ldq
            idx = Mux(commit_store, next_stq_commit_head, next_ldq_head)

            with m.If(commit_store):
                m.d.sync += stq[idx].committed.eq(1)
            with m.Elif(commit_load):
                m.d.sync += [
                    ldq[idx].valid.eq(0),
                    ldq[idx].addr_valid.eq(0),
                    ldq[idx].executed.eq(0),
                    ldq[idx].succeeded.eq(0),
                ]

            next_stq_commit_head = Mux(
                commit_store, _incr(next_stq_commit_head, self.stq_size),
                next_stq_commit_head)
            next_ldq_head = Mux(commit_load, _incr(next_ldq_head,
                                                   self.ldq_size),
                                next_ldq_head)

        m.d.sync += [
            stq_commit_head.eq(next_stq_commit_head),
            ldq_head.eq(next_ldq_head),
        ]

        with m.If(stq[stq_head].valid & stq[stq_head].committed):
            m.d.comb += clear_store.eq(stq[stq_head].succeeded)

        with m.If(clear_store):
            m.d.sync += [
                stq[stq_head].valid.eq(0),
                stq[stq_head].addr_valid.eq(0),
                stq[stq_head].data_valid.eq(0),
                stq[stq_head].committed.eq(0),
                stq[stq_head].succeeded.eq(0),
                stq_head.eq(_incr(stq_head, self.stq_size)),
            ]

        #
        # Exception
        #

        st_exc_killed_mask = Signal(self.stq_size)

        with m.If(self.exception):
            m.d.sync += [
                ldq_head.eq(0),
                ldq_tail.eq(0),
            ]

            for i in range(self.ldq_size):
                m.d.sync += [
                    ldq[i].valid.eq(0),
                    ldq[i].addr_valid.eq(0),
                    ldq[i].executed.eq(0),
                ]

            m.d.sync += stq_tail.eq(stq_commit_head)
            for i in range(self.stq_size):
                with m.If(~stq[i].committed & ~stq[i].succeeded):
                    m.d.sync += [
                        stq[i].valid.eq(0),
                        stq[i].addr_valid.eq(0),
                        stq[i].data_valid.eq(0),
                    ]

                    m.d.comb += st_exc_killed_mask[i].eq(1)

        m.d.sync += live_store_mask.eq(next_live_store_mask
                                       & ~st_brkilled_mask
                                       & ~st_exc_killed_mask)

        return m