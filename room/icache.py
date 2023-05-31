from amaranth import *
from amaranth.utils import log2_int

from room.types import HasCoreParams

from roomsoc.interconnect.stream import Valid, Decoupled


class HasICacheParams(HasCoreParams):

    def __init__(self, params):
        super().__init__(params)

        icache_params = params['icache_params']
        self.n_sets = icache_params['n_sets']
        self.n_ways = icache_params['n_ways']
        self.block_bytes = icache_params['block_bytes']
        self.lg_block_bytes = log2_int(self.block_bytes)


class ICacheReq(HasCoreParams, Record):

    def __init__(self, params, name=None, src_loc_at=0):
        HasCoreParams.__init__(self, params)

        Record.__init__(self, [
            ('addr', self.vaddr_bits_extended),
        ],
                        name=name,
                        src_loc_at=1 + src_loc_at)


class ICacheResp(HasCoreParams, Record):

    def __init__(self, params, name=None, src_loc_at=0):
        HasCoreParams.__init__(self, params)

        Record.__init__(self, [
            ('data', self.fetch_bytes * 8),
        ],
                        name=name,
                        src_loc_at=1 + src_loc_at)


class ICache(HasICacheParams, Elaboratable):

    def __init__(self, ibus, params):
        super().__init__(params)

        self.ibus = ibus

        self.req = Decoupled(ICacheReq, params)
        self.s1_paddr = Signal(32)
        self.s1_kill = Signal()
        self.s2_kill = Signal()

        self.resp = Valid(ICacheResp, params)

        self.invalidate = Signal()

    def elaborate(self, platform):
        m = Module()

        block_off_bits = log2_int(self.block_bytes)
        index_bits = log2_int(self.n_sets)
        untag_bits = block_off_bits + index_bits
        tag_bits = 32 - untag_bits

        word_bits = self.fetch_bytes * 8

        row_bits = self.ibus.data_width
        refill_cycles = (self.block_bytes * 8) // row_bits

        row_words = row_bits // word_bits

        s0_valid = self.req.fire
        s0_vaddr = self.req.bits.addr

        s1_valid = Signal()
        s1_tag_hit = Signal(self.n_ways)
        s1_hit = s1_tag_hit != 0
        s2_valid = Signal()
        s2_paddr = Signal.like(self.s1_paddr)
        s2_hit = Signal()
        s2_miss = s2_valid & ~s2_hit

        m.d.sync += [
            s1_valid.eq(s0_valid),
            s2_valid.eq(s1_valid & ~self.s1_kill),
            s2_paddr.eq(self.s1_paddr),
            s2_hit.eq(s1_hit),
        ]

        refill_way = Signal(range(self.n_ways))
        invalidated = Signal()
        refill_valid = Signal()
        refill_paddr = Signal.like(self.s1_paddr)
        refill_index = refill_paddr[block_off_bits:untag_bits]
        refill_tag = refill_paddr[untag_bits:untag_bits + tag_bits]
        refill_done = Signal()
        refill_counter = Signal(range(refill_cycles))
        refill_one_beat = self.ibus.d.fire & self.ibus.has_data(
            self.ibus.d.bits)
        with m.If(s1_valid & ~(s2_miss | refill_valid)):
            m.d.sync += refill_paddr.eq(self.s1_paddr)

        m.d.comb += self.req.ready.eq(~refill_one_beat)

        #
        # Tag array
        #

        tag_mem = Memory(width=self.n_ways * tag_bits, depth=self.n_sets)

        tag_rdata = [
            Signal(tag_bits, name=f'tag_rdata{i}') for i in range(self.n_ways)
        ]
        tag_read = m.submodules.tag_read = tag_mem.read_port()
        m.d.comb += tag_read.addr.eq(s0_vaddr[block_off_bits:untag_bits])
        for i in range(self.n_ways):
            m.d.comb += tag_rdata[i].eq(tag_read.data[i * tag_bits:(i + 1) *
                                                      tag_bits])

        tag_write = m.submodules.tag_write = tag_mem.write_port(
            granularity=tag_bits)
        m.d.comb += [
            tag_write.addr.eq(refill_index),
            tag_write.data.eq(Repl(refill_tag, self.n_ways)),
            tag_write.en.eq(
                Cat((i == refill_way) & refill_done
                    for i in range(self.n_ways))),
        ]

        slot_valids = Array(
            Signal(name=f'slot_valid{i}')
            for i in range(self.n_sets * self.n_ways))

        with m.If(refill_one_beat):
            m.d.sync += slot_valids[Cat(
                refill_index, refill_way)].eq(refill_done & ~invalidated)

        with m.If(self.invalidate):
            m.d.sync += [
                Cat(*slot_valids).eq(0),
                invalidated.eq(1),
            ]

        with m.If(~refill_valid):
            m.d.sync += invalidated.eq(0)

        s1_tags = [
            Signal(tag_bits, name=f's1_tag{i}') for i in range(self.n_ways)
        ]
        for i in range(self.n_ways):
            s1_idx = self.s1_paddr[block_off_bits:untag_bits]
            s1_tag = self.s1_paddr[untag_bits:untag_bits + tag_bits]
            m.d.comb += s1_tags[i].eq(s1_tag)
            slot_valid = slot_valids[Cat(s1_idx, Const(i,
                                                       log2_int(self.n_ways)))]
            m.d.comb += s1_tag_hit[i].eq(slot_valid & (s1_tag == tag_rdata[i]))

        #
        # Data memory
        #

        data_ways = [
            Memory(width=row_bits,
                   depth=self.n_sets * refill_cycles,
                   name=f'data_mem{i}') for i in range(self.n_ways)
        ]

        s2_dout = [
            Signal(row_bits, name=f's2_dout{i}') for i in range(self.n_ways)
        ]

        for i in range(self.n_ways):
            rport = data_ways[i].read_port()
            wport = data_ways[i].write_port()

            m.submodules += [rport, wport]

            row_addr = s0_vaddr[block_off_bits -
                                log2_int(refill_cycles):untag_bits]

            m.d.sync += rport.addr.eq(row_addr)
            m.d.comb += s2_dout[i].eq(rport.data)

            m.d.comb += [
                wport.addr.eq((refill_index << len(refill_counter))
                              | refill_counter),
                wport.data.eq(self.ibus.d.bits.data),
                wport.en.eq((i == refill_way) & refill_one_beat
                            & ~invalidated),
            ]

        s2_tag_hit = Signal.like(s1_tag_hit)
        m.d.sync += s2_tag_hit.eq(s1_tag_hit)

        s2_data = Signal.like(self.resp.bits.data)
        s2_word_idx = Signal(range(row_words))

        if row_words > 1:
            m.d.comb += s2_word_idx.eq(
                s2_paddr[log2_int(self.fetch_bytes):log2_int(self.fetch_bytes *
                                                             row_words)])

        for i in range(self.n_ways):
            with m.If(s2_tag_hit[i]):
                m.d.comb += s2_data.eq(s2_dout[i] >>
                                       (s2_word_idx << log2_int(word_bits)))

        m.d.comb += [
            self.resp.bits.data.eq(s2_data),
            self.resp.valid.eq(s2_valid & s2_hit),
        ]

        #
        # Refill logic
        #

        with m.FSM():
            with m.State('IDLE'):
                m.d.sync += refill_counter.eq(0)

                with m.If(s2_miss & ~self.s2_kill):
                    m.d.comb += [
                        self.ibus.tilelink_get(address=Cat(
                            Const(0, block_off_bits),
                            refill_paddr[block_off_bits:]),
                                               size=self.lg_block_bytes,
                                               mask=~0,
                                               source=~0),
                        self.ibus.a.valid.eq(1),
                    ]

                    with m.If(self.ibus.a.fire):
                        m.d.sync += refill_way.eq(refill_way + 1)
                        m.next = 'REFILL'

            with m.State('REFILL'):
                m.d.comb += [
                    refill_valid.eq(1),
                    self.ibus.d.ready.eq(1),
                ]

                with m.If(refill_one_beat):
                    m.d.sync += refill_counter.eq(refill_counter + 1)

                    with m.If(refill_counter == (refill_cycles - 1)):
                        m.d.comb += refill_done.eq(1)
                        m.next = 'IDLE'

        return m
