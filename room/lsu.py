from amaranth import *

from room.consts import *
from room.types import MicroOp
from room.alu import ExecResp
from room.branch import BranchUpdate
from room.rob import CommitReq

from roomsoc.interconnect import wishbone


class RRPriorityEncoder(Elaboratable):

    def __init__(self, width):
        self.width = width

        self.i = Signal(width)
        self.head = Signal(range(width))
        self.o = Signal(range(width))
        self.n = Signal()

    def elaborate(self, platform):
        m = Module()
        for j in reversed(range(self.width)):
            with m.If(self.i[j] & (j < self.head)):
                m.d.comb += self.o.eq(j)
        for j in reversed(range(self.width)):
            with m.If(self.i[j] & (j >= self.head)):
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


class DataCache(Elaboratable):

    def __init__(self, params):
        self.mem_width = params['mem_width']
        self.params = params

        self.dbus = wishbone.Interface(data_width=32, adr_width=32)

        self.reqs = Array(
            DCacheReq(params, name=f'req{i}') for i in range(self.mem_width))
        self.resps = Array(
            DCacheResp(params, name=f'resp{i}') for i in range(self.mem_width))

        self.br_update = BranchUpdate(params)

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

        adr = Signal.like(self.dbus.adr)
        dat_w = Signal.like(self.dbus.dat_w)
        sel = Signal.like(self.dbus.sel)
        we = Signal.like(self.dbus.we)

        req_chosen = self.reqs[chosen]

        with m.FSM():
            with m.State('IDLE'):
                m.d.comb += chosen.eq(choice),

                with m.If(req_chosen.valid
                          & ~self.br_update.uop_killed(req_chosen.uop)):
                    m.d.comb += [
                        self.dbus.adr.eq(req_chosen.addr),
                        self.dbus.stb.eq(1),
                        self.dbus.dat_w.eq(req_chosen.data),
                        self.dbus.cyc.eq(1),
                        self.dbus.sel.eq(~0),
                        self.dbus.we.eq(self.reqs[choice].uop.mem_cmd ==
                                        MemoryCommand.WRITE),
                        req_chosen.ready.eq(1),
                    ]

                    m.d.sync += [
                        uop.eq(req_chosen.uop),
                        uop.br_mask.eq(
                            self.br_update.get_new_br_mask(
                                req_chosen.uop.br_mask)),
                        adr.eq(self.dbus.adr),
                        dat_w.eq(self.dbus.dat_w),
                        sel.eq(self.dbus.sel),
                        we.eq(self.dbus.we),
                        last_grant.eq(choice),
                    ]

                    m.next = 'WAIT_ACK'

            with m.State('WAIT_ACK'):
                m.d.comb += [
                    chosen.eq(last_grant),
                    self.dbus.adr.eq(adr),
                    self.dbus.stb.eq(1),
                    self.dbus.dat_w.eq(dat_w),
                    self.dbus.cyc.eq(1),
                    self.dbus.sel.eq(sel),
                    self.dbus.we.eq(we),
                ]

                m.d.sync += [
                    uop.br_mask.eq(self.br_update.get_new_br_mask(
                        uop.br_mask)),
                ]

                with m.If(req_chosen.kill | self.br_update.uop_killed(uop)):
                    m.next = 'IDLE'
                with m.Elif(self.dbus.ack):
                    m.d.comb += [
                        self.resps[chosen].valid.eq(1),
                        self.resps[chosen].data.eq(self.dbus.dat_r),
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
        return signal + 1
    else:
        return Mux(signal == modulo - 1, 0, signal + 1)


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

        ldq_w_idx = ldq_tail
        stq_w_idx = stq_tail

        clear_store = Signal()

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
                    ldq[ldq_w_idx].valid.eq(1),
                    ldq[ldq_w_idx].uop.eq(self.dis_uops[w]),
                    ldq[ldq_w_idx].addr_valid.eq(0),
                    ldq[ldq_w_idx].executed.eq(0),
                    ldq[ldq_w_idx].succeeded.eq(0),
                ]
            with m.Elif(dis_w_stq):
                m.d.sync += [
                    stq[stq_w_idx].valid.eq(1),
                    stq[stq_w_idx].uop.eq(self.dis_uops[w]),
                    stq[stq_w_idx].addr_valid.eq(0),
                    stq[stq_w_idx].data_valid.eq(0),
                    stq[stq_w_idx].committed.eq(0),
                    stq[stq_w_idx].succeeded.eq(0),
                ]

            ldq_w_idx = Mux(dis_w_ldq, _incr(ldq_w_idx, self.ldq_size),
                            ldq_w_idx)
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
                                              & ~ldq[i].executed)
        m.d.comb += ldq_retry_enc.head.eq(ldq_head)
        ldq_retry_idx = ldq_retry_enc.o
        ldq_retry_e = ldq[ldq_retry_idx]

        stq_commit_e = stq[stq_execute_head]

        can_fire_load_incoming = Signal(self.core_width)
        can_fire_sta_incoming = Signal(self.core_width)
        can_fire_std_incoming = Signal(self.core_width)
        can_fire_stad_incoming = Signal(self.core_width)
        can_fire_load_retry = Signal(self.core_width)
        can_fire_store_commit = Signal(self.core_width)

        s0_executing_loads = Array(
            Signal(name=f's0_executing{i}') for i in range(self.ldq_size))
        s1_executing_loads = Array(
            Signal(name=f's1_executing{i}') for i in range(self.ldq_size))
        s1_set_executed = Signal(self.ldq_size)
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
                                          & (w == 0)),
                can_fire_store_commit[w].eq(stq_commit_e.valid
                                            & stq_commit_e.committed
                                            & (w == self.mem_width - 1)),
            ]

        for w in range(self.mem_width):
            with m.If(can_fire_load_incoming[w]):
                ldq_idx = self.exec_reqs[w].uop.ldq_idx
                m.d.sync += [
                    ldq[ldq_idx].addr.eq(self.exec_reqs[w].addr),
                    ldq[ldq_idx].addr_valid.eq(1),
                    ldq[ldq_idx].uop.pdst.eq(self.exec_reqs[w].uop.pdst),
                ]

            with m.If(can_fire_sta_incoming[w] | can_fire_stad_incoming[w]):
                stq_idx = self.exec_reqs[w].uop.stq_idx
                m.d.sync += [
                    stq[stq_idx].addr.eq(self.exec_reqs[w].addr),
                    stq[stq_idx].addr_valid.eq(1),
                    stq[stq_idx].uop.pdst.eq(self.exec_reqs[w].uop.pdst),
                ]

            with m.If(can_fire_std_incoming[w] | can_fire_stad_incoming[w]):
                stq_idx = self.exec_reqs[w].uop.stq_idx
                m.d.sync += [
                    stq[stq_idx].data.eq(self.exec_reqs[w].data),
                    stq[stq_idx].data_valid.eq(1),
                ]

        #
        # Memory access
        #

        for w in range(self.mem_width):
            dmem_req = dcache.reqs[w]

            with m.If(can_fire_load_retry[w]):
                m.d.comb += [
                    dmem_req.valid.eq(1),
                    dmem_req.uop.eq(ldq_retry_e.uop),
                    dmem_req.addr.eq(ldq_retry_e.addr),
                    s0_executing_loads[ldq_retry_idx].eq(dmem_req.ready),
                ]
            with m.Elif(can_fire_store_commit[w]):
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

        fired_sta_incoming = Signal(self.mem_width)
        fired_std_incoming = Signal(self.mem_width)
        fired_stad_incoming = Signal(self.mem_width)

        fired_stq_incoming_e = [
            STQEntry(self.params, name=f'fired_stq_incoming_e{i}')
            for i in range(self.mem_width)
        ]

        for w in range(self.mem_width):
            req_killed = self.br_update.uop_killed(self.exec_reqs[w].uop)

            m.d.sync += [
                fired_sta_incoming[w].eq(can_fire_sta_incoming[w]
                                         & ~req_killed),
                fired_std_incoming[w].eq(can_fire_std_incoming[w]
                                         & ~req_killed),
                fired_stad_incoming[w].eq(can_fire_stad_incoming[w]
                                          & ~req_killed),
                fired_stq_incoming_e[w].eq(stq[self.exec_reqs[w].uop.stq_idx]),
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
        # Writeback
        #

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
                    ]

                    m.d.sync += ldq[ldq_idx].succeeded.eq(1)
                with m.If(dcache.resps[w].uop.uses_stq):
                    stq_idx = dcache.resps[w].uop.stq_idx
                    m.d.sync += stq[stq_idx].succeeded.eq(1)

        #
        # Branch mispredict
        #

        for i in range(self.stq_size):
            with m.If(stq[i].valid):
                m.d.sync += stq[i].uop.br_mask.eq(
                    self.br_update.get_new_br_mask(stq[i].uop.br_mask))

                with m.If(self.br_update.uop_killed(stq[i].uop)):
                    m.d.sync += stq[i].valid.eq(0)

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

        return m
