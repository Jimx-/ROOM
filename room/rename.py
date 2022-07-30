from amaranth import *
from amaranth.lib.coding import PriorityEncoder, Decoder

from room.consts import *
from room.types import MicroOp
from room.rob import CommitReq
from room.issue import IssueQueueWakeup


class MapReq(Record):

    def __init__(self, num_lregs, name=None):
        super().__init__([
            ('lrs1', range(num_lregs)),
            ('lrs2', range(num_lregs)),
            ('lrs3', range(num_lregs)),
            ('ldst', range(num_lregs)),
        ],
                         name=name)


class MapResp(Record):

    def __init__(self, num_pregs, name=None):
        super().__init__([
            ('prs1', range(num_pregs)),
            ('prs2', range(num_pregs)),
            ('prs3', range(num_pregs)),
            ('stale_pdst', range(num_pregs)),
        ],
                         name=name)


class RemapReq(Record):

    def __init__(self, num_lregs, num_pregs, name=None):
        super().__init__([('ldst', range(num_lregs)),
                          ('pdst', range(num_pregs)), ('valid', 1)],
                         name=name)


class MapTable(Elaboratable):

    def __init__(self, core_width, num_lregs, num_pregs):
        self.core_width = core_width
        self.num_lregs = num_lregs
        self.num_pregs = num_pregs

        self.map_reqs = [
            MapReq(num_lregs, name=f'map_req{i}') for i in range(core_width)
        ]
        self.map_resps = [
            MapResp(num_pregs, name=f'map_resp{i}') for i in range(core_width)
        ]
        self.remap_reqs = [
            RemapReq(num_lregs, num_pregs, name=f'remap_req{i}')
            for i in range(core_width)
        ]

    def elaborate(self, platform):
        m = Module()

        map_table = Array(
            Signal(range(self.num_pregs), name=f'map_table_row{i}')
            for i in range(self.num_lregs))

        remap_table = [[
            Signal(range(self.num_pregs), name=f'remap_table_row{i}_col{j}')
            for j in range(self.num_lregs)
        ] for i in range(self.core_width + 1)]

        for ldst in range(self.num_lregs):
            m.d.comb += remap_table[0][ldst].eq(map_table[ldst])

            if ldst == 0:
                for pl in range(self.core_width):
                    m.d.comb += remap_table[pl + 1][ldst].eq(0)
            else:
                for pl in range(self.core_width):
                    m.d.comb += remap_table[pl + 1][ldst].eq(
                        Mux(
                            self.remap_reqs[pl].valid
                            & (self.remap_reqs[pl].ldst == ldst),
                            self.remap_reqs[pl].pdst, remap_table[pl][ldst]))

        for i in range(self.num_lregs):
            m.d.sync += map_table[i].eq(remap_table[self.core_width][i])

        for req, resp in zip(self.map_reqs, self.map_resps):
            m.d.comb += [
                resp.prs1.eq(map_table[req.lrs1]),
                resp.prs2.eq(map_table[req.lrs2]),
                resp.prs3.eq(map_table[req.lrs3]),
                resp.stale_pdst.eq(map_table[req.ldst]),
            ]

        return m


class Freelist(Elaboratable):

    def __init__(self, core_width, num_pregs):
        self.core_width = core_width
        self.num_pregs = num_pregs

        self.reqs = Signal(core_width)
        self.alloc_pregs = [
            Signal(range(num_pregs), name=f'alloc_preg{i}')
            for i in range(core_width)
        ]
        self.alloc_valids = [Signal() for _ in range(core_width)]

        self.dealloc_pregs = [
            Signal(range(num_pregs), name=f'dealloc_preg{i}')
            for i in range(core_width)
        ]
        self.dealloc_valids = [Signal() for _ in range(core_width)]

    def elaborate(self, platform):
        m = Module()

        free_list = Signal(self.num_pregs, reset=~1)

        sels = [Signal(self.num_pregs) for _ in range(self.core_width)]
        sel_fire = [Signal() for _ in range(self.core_width)]

        sm = 0
        for s, f in zip(sels, sel_fire):
            sm |= Mux(f, s, 0)
        sel_mask = Signal(self.num_pregs)
        m.d.comb += sel_mask.eq(sm)

        masks = [Signal(self.num_pregs) for _ in range(self.core_width)]
        m.d.comb += masks[0].eq(free_list)

        for i in range(self.core_width):
            enc = PriorityEncoder(self.num_pregs)
            dec = Decoder(self.num_pregs)
            m.submodules += [enc, dec]

            m.d.comb += [
                enc.i.eq(masks[i]),
                dec.i.eq(enc.o),
                dec.n.eq(enc.n),
                sels[i].eq(dec.o),
                self.alloc_pregs[i].eq(enc.o),
            ]

            if i != self.core_width - 1:
                m.d.comb += masks[i + 1].eq(masks[i] & ~sels[i])

            valid = Signal()
            m.d.sync += [valid.eq((valid & ~self.reqs[i]) | (sels[i] != 0))]

            m.d.comb += [
                sel_fire[i].eq((~valid | self.reqs[i]) & (sels[i] != 0)),
                self.alloc_valids[i].eq(valid),
            ]

        dealloc_mask = 0
        for preg, v in zip(self.dealloc_pregs, self.dealloc_valids):
            dec = Decoder(self.num_pregs)
            m.submodules += dec

            m.d.comb += dec.i.eq(preg)

            dealloc_mask |= Mux(v, dec.o, 0)

        m.d.sync += [
            free_list.eq((free_list & ~sel_mask | dealloc_mask)
                         & ~Const(1, self.num_pregs)),
        ]

        return m


class BusyResp(Record):

    def __init__(self, name=None):
        super().__init__([
            ('prs1_busy', 1),
            ('prs2_busy', 1),
            ('prs3_busy', 1),
        ],
                         name=name)


class BusyTable(Elaboratable):

    def __init__(self, num_wakeup_ports, params):
        self.core_width = params['core_width']
        self.num_pregs = params['num_pregs']

        self.ren_uops = [
            MicroOp(params, name=f'busy_uop{i}')
            for i in range(self.core_width)
        ]

        self.busy_resps = [
            BusyResp(name=f'busy_resp{i}') for i in range(self.core_width)
        ]

        self.busy_reqs = Signal(self.core_width)

        self.wb_valids = Signal(num_wakeup_ports)
        self.wb_pdst = [
            Signal(range(self.num_pregs), name=f'wb_pdst{i}')
            for i in range(num_wakeup_ports)
        ]

    def elaborate(self, platform):
        m = Module()

        busy_table = Array(
            Signal(name=f'busy_table{i}') for i in range(self.num_pregs))

        wb_mask = 0
        for preg, v in zip(self.wb_pdst, self.wb_valids):
            dec = Decoder(self.num_pregs)
            m.submodules += dec

            m.d.comb += dec.i.eq(preg)

            wb_mask |= Mux(v, dec.o, 0)

        busy_table_next = 0
        for uop, req in zip(self.ren_uops, self.busy_reqs):
            dec = Decoder(self.num_pregs)
            m.submodules += dec

            m.d.comb += dec.i.eq(uop.pdst)

            busy_table_next = busy_table_next | Mux(req, dec.o, 0)

        for i in range(self.num_pregs):
            m.d.sync += busy_table[i].eq(busy_table[i] & ~wb_mask[i]
                                         | busy_table_next[i])

        for resp, uop in zip(self.busy_resps, self.ren_uops):
            m.d.comb += [
                resp.prs1_busy.eq(busy_table[uop.prs1]),
                resp.prs2_busy.eq(busy_table[uop.prs2]),
                resp.prs3_busy.eq(busy_table[uop.prs3]),
            ]

        return m


class RenameStage(Elaboratable):

    def __init__(self, num_wakeup_ports, params):
        self.params = params
        self.core_width = params['core_width']
        self.num_pregs = params['num_pregs']
        self.num_wakeup_ports = num_wakeup_ports

        self.kill = Signal()

        self.dec_fire = Signal(self.core_width)
        self.dec_uops = [
            MicroOp(params, name=f'dec_uop{i}') for i in range(self.core_width)
        ]

        self.dis_fire = Signal(self.core_width)
        self.dis_ready = Signal()

        self.ren2_uops = [
            MicroOp(params, name=f'ren2_uop{i}')
            for i in range(self.core_width)
        ]
        self.ren2_mask = Signal(self.core_width)

        self.wakeup_ports = [
            IssueQueueWakeup(self.num_pregs, name=f'wakeup_port{i}')
            for i in range(self.num_wakeup_ports)
        ]

        self.commit = CommitReq(self.params)

        self.stalls = Signal(self.core_width)

    def bypass_preg_alloc(self, m, i, uop, older_uops, alloc_reqs):
        bypass_uop = MicroOp(self.params, name=f'bypass_uop{i}')
        m.d.comb += bypass_uop.eq(uop)
        w = len(older_uops)

        bypass_rs1 = [Signal(range(self.num_pregs)) for _ in range(w + 1)]
        bypass_rs2 = [Signal(range(self.num_pregs)) for _ in range(w + 1)]
        bypass_dst = [Signal(range(self.num_pregs)) for _ in range(w + 1)]

        m.d.comb += [
            bypass_rs1[0].eq(0), bypass_rs2[0].eq(0), bypass_dst[0].eq(0)
        ]

        for i in range(w):
            m.d.comb += [
                bypass_rs1[i + 1].eq(
                    Mux((uop.lrs1 == older_uops[i].ldst) & alloc_reqs[i],
                        older_uops[i].pdst, bypass_rs1[i])),
                bypass_rs2[i + 1].eq(
                    Mux((uop.lrs2 == older_uops[i].ldst) & alloc_reqs[i],
                        older_uops[i].pdst, bypass_rs2[i])),
                bypass_dst[i + 1].eq(
                    Mux((uop.ldst == older_uops[i].ldst) & alloc_reqs[i],
                        older_uops[i].pdst, bypass_dst[i]))
            ]

        m.d.comb += [
            bypass_uop.prs1.eq(Mux(bypass_rs1[w] != 0, bypass_rs1[w],
                                   uop.prs1)),
            bypass_uop.prs2.eq(Mux(bypass_rs2[w] != 0, bypass_rs2[w],
                                   uop.prs2)),
            bypass_uop.stale_pdst.eq(
                Mux(bypass_dst[w] != 0, bypass_dst[w], uop.stale_pdst)),
            bypass_uop.prs1_busy.eq(Mux(bypass_rs1[w] != 0, 1, uop.prs1_busy)),
            bypass_uop.prs2_busy.eq(Mux(bypass_rs2[w] != 0, 1, uop.prs2_busy)),
        ]

        return bypass_uop

    def elaborate(self, platform):
        m = Module()

        ren1_fire = Signal(self.core_width)
        ren1_uops = [
            MicroOp(self.params, name=f'_ren1_uop{i}')
            for i in range(self.core_width)
        ]

        ren2_fire = Signal(self.core_width)
        ren2_ready = Signal()
        ren2_uops = [
            MicroOp(self.params, name=f'_ren2_uop{i}')
            for i in range(self.core_width)
        ]
        ren2_valids = Signal(self.core_width)
        ren2_alloc_reqs = Signal(self.core_width)
        m.d.comb += [
            ren2_fire.eq(self.dis_fire),
            ren2_ready.eq(self.dis_ready)
        ]

        for ren1_uop, dec_uop in zip(ren1_uops, self.dec_uops):
            m.d.comb += ren1_uop.eq(dec_uop)
            m.d.comb += ren1_uop.lrs1.eq(dec_uop.lrs1)
        m.d.comb += ren1_fire.eq(self.dec_fire)

        for i, (r2_fire, ren2_uop, ren2_valid, r1_fire, ren1_uop) in enumerate(
                zip(ren2_fire, ren2_uops, ren2_valids, ren1_fire, ren1_uops)):
            uop = MicroOp(self.params, f'_ren2_uop_r{i}')
            uop_next = MicroOp(self.params, '_ren2_uop_next{i}')

            with m.If(ResetSignal()):
                m.d.comb += uop_next.eq(0)
            with m.Elif(self.kill):
                m.d.sync += ren2_valid.eq(0)
            with m.Elif(ren2_ready):
                m.d.sync += ren2_valid.eq(r1_fire)
                m.d.comb += uop_next.eq(ren1_uop)
            with m.Else():
                m.d.sync += ren2_valid.eq(ren2_valid & ~r2_fire)
                m.d.comb += uop_next.eq(uop)

            # If REN1 and REN2 fire at the same cycle, REN1 will read stale mapping
            # invalidated by REN2.
            m.d.sync += uop.eq(
                self.bypass_preg_alloc(m, i, uop_next, ren2_uops,
                                       ren2_alloc_reqs))

            m.d.comb += ren2_uop.eq(uop)

        m.d.comb += self.ren2_mask.eq(ren2_valids)

        map_table = m.submodules.map_table = MapTable(self.core_width, 32,
                                                      self.num_pregs)
        freelist = m.submodules.freelist = Freelist(self.core_width,
                                                    self.num_pregs)
        busy_table = m.submodules.busy_table = BusyTable(
            self.num_wakeup_ports, self.params)

        commit_valids = Signal(self.core_width)

        for v, cuop, cv in zip(commit_valids, self.commit.uops,
                               self.commit.valids):
            m.d.comb += v.eq(cuop.ldst_valid
                             & (cuop.dst_rtype == RegisterType.FIX) & cv)

        #
        # Map table
        #

        for map_req, ren1_uop, map_resp in zip(map_table.map_reqs, ren1_uops,
                                               map_table.map_resps):
            m.d.comb += [
                map_req.lrs1.eq(ren1_uop.lrs1),
                map_req.lrs2.eq(ren1_uop.lrs2),
                map_req.lrs3.eq(ren1_uop.lrs3),
                map_req.ldst.eq(ren1_uop.ldst),
                #
                ren1_uop.prs1.eq(map_resp.prs1),
                ren1_uop.prs2.eq(map_resp.prs2),
                ren1_uop.prs3.eq(map_resp.prs3),
                ren1_uop.stale_pdst.eq(map_resp.stale_pdst),
            ]

        for rem_req, ren2_uop, alloc_req in zip(map_table.remap_reqs,
                                                ren2_uops, ren2_alloc_reqs):
            m.d.comb += [
                rem_req.ldst.eq(ren2_uop.ldst),
                rem_req.pdst.eq(ren2_uop.pdst),
                rem_req.valid.eq(alloc_req),
            ]

        #
        # Free list
        #

        for uop, fire, req in zip(ren2_uops, ren2_fire, ren2_alloc_reqs):
            m.d.comb += req.eq(uop.ldst_valid
                               & (uop.dst_rtype == RegisterType.FIX) & fire)

        for uop, preg in zip(ren2_uops, freelist.alloc_pregs):
            m.d.comb += uop.pdst.eq(Mux(uop.ldst != 0, preg, 0))

        for preg, uop, v, cv in zip(freelist.dealloc_pregs, self.commit.uops,
                                    freelist.dealloc_valids, commit_valids):
            m.d.comb += [
                preg.eq(uop.stale_pdst),
                v.eq(cv),
            ]

        m.d.comb += freelist.reqs.eq(ren2_alloc_reqs)

        #
        # Busy table
        #

        for busy_uop, ren_uop, busy_resp in zip(busy_table.ren_uops, ren2_uops,
                                                busy_table.busy_resps):
            m.d.comb += [
                busy_uop.eq(ren_uop),
                ren_uop.prs1_busy.eq((ren_uop.lrs1_rtype == RegisterType.FIX)
                                     & busy_resp.prs1_busy),
                ren_uop.prs2_busy.eq((ren_uop.lrs2_rtype == RegisterType.FIX)
                                     & busy_resp.prs2_busy),
            ]
        for busy_req, fl_req in zip(busy_table.busy_reqs, ren2_alloc_reqs):
            m.d.comb += busy_req.eq(fl_req)

        for wb_v, wb_pdst, wu in zip(busy_table.wb_valids, busy_table.wb_pdst,
                                     self.wakeup_ports):
            m.d.comb += [wb_v.eq(wu.valid), wb_pdst.eq(wu.pdst)]

        for i in range(self.core_width):
            if i == 0:
                bypass_uop = ren2_uops[i]
            else:
                bypass_uop = self.bypass_preg_alloc(m, i, ren2_uops[i],
                                                    ren2_uops[:i],
                                                    ren2_alloc_reqs[:i])

            m.d.comb += [
                self.ren2_uops[i].eq(bypass_uop),
                self.stalls.eq((ren2_uops[i].dst_rtype == RegisterType.FIX)
                               & ~freelist.alloc_valids[i]),
            ]

        return m
