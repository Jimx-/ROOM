"""Systolic matrix datapath for the troom accelerator.

This module implements the reviewed activation-stationary matrix unit
from the troom plan: an ACT RAM holding one host-quantized activation
row, a TMEM feeder with per-pipeline row cursors and ping/pong block
buffers that decodes packed GGUF weights (``troom.unpack``) into
logical-K segments, the skewed INT8 MAC chains, and the integer
chain-end cells that produce signed INT32 segment partials. The FP32
scale/correction/accumulation epilogue is a separate later module; its
input is this unit's ``segs`` record stream.

Geometry (per the plan): ``seg_len`` reduction elements per slot, two
independent 16-position DSP chains per output pipeline, one product per
DSP. Pipeline ``p`` consumes output row ``p + n_outputs * pair``'s
weights; both pipelines advance through K together, one logical segment
per slot. A slot launches only when every unmasked pipeline's decoded
segment, the ACT segment, and output-queue credit are ready; a missing
segment inserts an invalid slot that advances neither the logical K
index nor any accumulator. Odd output counts mask the unused lane of
the final pair, which then never requests beats beyond its tile.

Cycle table for a slot launched at cycle ``s`` (both portable simulation
and KU040 DSP48E2 use MREG=1, PREG=1 and the same ``L_PE=2``):

.. code-block:: text

    s          slot operands presented to the skew inputs
    s + pos    position pos's weight/activation pair (delayed pos)
    s + pos+1  position pos's registered product M
    s + pos+2  position pos's registered P (PCIN + M; P = M at pos 0)
    s + 17     both chain ends visible (L_PE + CL - 1 = 17): integer
               chain-end combine against metadata delay stage 16
    s + 18     segment record registered into the output stream

With ``L_PE = 2`` and chain length ``CL = seg_len / 2 = 16``, a
segment's chain-end value covers ``sum(w_i * a_i)`` over its 32
elements exactly in signed INT32 by the plan's bounds (Q8_0 524,288;
Q4_K 3,870,720; Q6_K 16,777,216; Q5_0 65,536). The chain-end cells
apply the integer sub-scale combine per format before the record
leaves this module:

* Q5_0/Q8_0: ``S = P_0 + P_1``
* Q4_K:      ``S = sc * (P_0 + P_1)``           (sc unsigned 6-bit)
* Q6_K:      ``S = sc_0 * P_0 + sc_1 * P_1``    (sc signed 8-bit)

The metadata the epilogue still needs (weight ``d``/``dmin``/``mi``,
activation ``s_act``/``sum_a``, row, segment index, row-last marker)
travels a delay line matched to the chains, so records come out in
launch order, one per valid slot.
"""

from amaranth import *
from amaranth.hdl.rec import Direction, Record
from amaranth.utils import log2_int

from troom.consts import Format, MM_FORMATS, Status
from troom.types import HasTRoomParams
from troom.unpack import (
    BLOCK_BYTES,
    BLOCK_SEGS,
    MAX_BLOCK_BYTES,
    decode_segment,
)

from roomsoc.interconnect.stream import Decoupled, Queue


class IntMac(Elaboratable):
    """Portable two-stage signed multiply/cascade-add cell.

    At each sync edge M <- a*b and P <- cascade_in + old M (modulo 2**48).
    The caller skews operands and carries validity alongside the free-running
    chain. Both registers synchronously clear with the sync domain reset.
    ``result`` drives fabric; ``cascade_out`` only connects to another
    IntMac's ``cascade_in``. They carry the same registered value.
    ``first=True`` ignores cascade_in and computes P <- old M, allowing
    a DSP chain to start at any site without selecting a tied-off PCIN.
    """

    product_latency = 1
    output_latency = 2

    def __init__(self, *, a_width=8, b_width=8, first=False):
        if a_width < 1 or b_width < 1 or a_width + b_width > 48:
            raise ValueError('signed product must fit in 48 bits')
        self.a = Signal(signed(a_width))
        self.first = first
        self.b = Signal(signed(b_width))
        self.cascade_in = Signal(signed(48))
        self.cascade_out = Signal(signed(48))
        self.result = Signal(signed(48))

    def elaborate(self, platform):
        factory = getattr(platform, 'get_int_mac', None)
        if factory is not None:
            return factory(self)

        m = Module()
        product = Signal(signed(len(self.a) + len(self.b)))
        m.d.sync += [
            product.eq(self.a * self.b),
            self.result.eq((0 if self.first else self.cascade_in) + product),
        ]
        m.d.comb += self.cascade_out.eq(self.result)
        return m


# Latency from a slot's arrival at chain position zero's input to its
# registered P: one multiply register, one accumulate register.
L_PE = IntMac.output_latency


def _s32(v):
    """Truncate a signed value to its low 32 bits, preserving sign.

    Exact for every value this unit produces; the plan's per-format
    bounds keep all segment partials inside INT32.
    """
    if len(v) < 32:
        return Cat(v, v[-1].replicate(32 - len(v))).as_signed()
    return v[:32].as_signed()


class ActWord(HasTRoomParams, Record):
    """One activation segment in ACT RAM.

    ``quants`` holds ``seg_len`` signed INT8 activation quants in
    logical K order, ``s_act`` is the activation scale the FP32 epilogue
    applies (an exact FP32; Q8_0-style FP16 block scales convert
    exactly), and ``sum_a`` is the signed sum of the segment's quants
    for the Q4_K minimum-term correction. Q8_K-style activations
    replicate their per-256 scale across the super-block's eight
    segments and carry the per-32 sum formed from the per-16 sums.
    """

    def __init__(self, params, name=None, src_loc_at=0):
        HasTRoomParams.__init__(self, params)
        Record.__init__(self, [
            ('quants', self.seg_len * 8, Direction.FANOUT),
            ('s_act', 32, Direction.FANOUT),
            ('sum_a', signed(16), Direction.FANOUT),
        ],
                        name=name,
                        src_loc_at=src_loc_at + 1)


class WSegWord(HasTRoomParams, Record):
    """One decoded weight segment for a single output pipeline.

    ``w`` holds ``seg_len`` PE weights in logical K order after integer
    unpack (centered for Q5_0/Q6_K, zero-extended for Q4_K, the signed
    byte for Q8_0). ``d``/``dmin`` are GGUF FP16 bit patterns, ``mi`` is
    the unsigned 6-bit Q4_K minimum, and ``sc0``/``sc1`` are the two
    chains' signed sub-scales (Q4_K's single scale rides zero-extended
    in ``sc0``). Fields a format does not use are zero. ``row`` is the
    output row this segment belongs to.
    """

    def __init__(self, params, name=None, src_loc_at=0):
        HasTRoomParams.__init__(self, params)
        Record.__init__(self, [
            ('w', self.seg_len * 8, Direction.FANOUT),
            ('d', 16, Direction.FANOUT),
            ('dmin', 16, Direction.FANOUT),
            ('mi', 6, Direction.FANOUT),
            ('sc0', signed(8), Direction.FANOUT),
            ('sc1', signed(8), Direction.FANOUT),
            ('row', self.rows_bits, Direction.FANOUT),
        ],
                        name=name,
                        src_loc_at=src_loc_at + 1)


def _slot_layout(hp):
    """Launch-slot metadata: everything the chains' outputs must be
    matched with, including the sub-scales the chain-end combine
    consumes and the validity bit invalid slots never assert."""
    layout = [
        ('valid', 1, Direction.FANOUT),
        ('fmt', Format, Direction.FANOUT),
        ('row_mask', hp.n_outputs, Direction.FANOUT),
        ('s_act', 32, Direction.FANOUT),
        ('sum_a', signed(16), Direction.FANOUT),
        ('seg_k', hp.lg_act_segs, Direction.FANOUT),
        ('row_last', 1, Direction.FANOUT),
    ]
    for p in range(hp.n_outputs):
        layout += [
            (f'row{p}', hp.rows_bits, Direction.FANOUT),
            (f'd{p}', 16, Direction.FANOUT),
            (f'dmin{p}', 16, Direction.FANOUT),
            (f'mi{p}', 6, Direction.FANOUT),
            (f'sc{p}a', signed(8), Direction.FANOUT),
            (f'sc{p}b', signed(8), Direction.FANOUT),
        ]
    return layout


class SlotMeta(HasTRoomParams, Record):
    """Metadata for one launched slot, skewed alongside the MAC chains."""

    def __init__(self, params, name=None, src_loc_at=0):
        HasTRoomParams.__init__(self, params)
        Record.__init__(self,
                        _slot_layout(self),
                        name=name,
                        src_loc_at=src_loc_at + 1)


def _segment_layout(hp):
    """Chain-end segment record layout: the FP32 epilogue's input."""
    layout = [
        ('fmt', Format, Direction.FANOUT),
        ('row_mask', hp.n_outputs, Direction.FANOUT),
        ('s_act', 32, Direction.FANOUT),
        ('sum_a', signed(16), Direction.FANOUT),
        ('seg_k', hp.lg_act_segs, Direction.FANOUT),
        ('row_last', 1, Direction.FANOUT),
    ]
    for p in range(hp.n_outputs):
        layout += [
            (f'S{p}', signed(32), Direction.FANOUT),
            (f'row{p}', hp.rows_bits, Direction.FANOUT),
            (f'd{p}', 16, Direction.FANOUT),
            (f'dmin{p}', 16, Direction.FANOUT),
            (f'mi{p}', 6, Direction.FANOUT),
        ]
    return layout


class MmSegment(HasTRoomParams, Record):
    """Chain-end segment record: the matrix unit's output stream.

    Records are produced in launch order, one per valid slot; invalid
    slots update nothing and produce no record. ``S`` is the signed
    INT32 segment partial after the integer chain-end combine.
    ``seg_k`` is the segment's index within its row -- the FP32
    epilogue's ``k mod 4`` accumulation order -- and ``row_last`` marks
    the row's final segment so the epilogue can fold its four partials.
    """

    def __init__(self, params, name=None, src_loc_at=0):
        HasTRoomParams.__init__(self, params)
        Record.__init__(self,
                        _segment_layout(self),
                        name=name,
                        src_loc_at=src_loc_at + 1)


class MatmulDesc(HasTRoomParams, Record):
    """MATMUL control descriptor for the matrix unit.

    ``k`` is the reduction length common to all rows, ``rows`` the
    output row count (odd counts leave the final lane masked), and
    ``w_base``/``w_stride`` name the TMEM tile: row ``r`` occupies
    ``row_bytes(fmt, k)`` packed GGUF bytes at ``w_base + r *
    w_stride``, padded to the bus beat by the device format.
    """

    def __init__(self, params, name=None, src_loc_at=0):
        HasTRoomParams.__init__(self, params)
        Record.__init__(self, [
            ('fmt', Format, Direction.FANOUT),
            ('k', self.lg_act_k + 1, Direction.FANOUT),
            ('rows', self.rows_bits, Direction.FANOUT),
            ('w_base', self.tmem_addr_width, Direction.FANOUT),
            ('w_stride', self.tmem_addr_width, Direction.FANOUT),
        ],
                        name=name,
                        src_loc_at=src_loc_at + 1)


class ActRAM(HasTRoomParams, Elaboratable):
    """Activation segment storage pinned by the matrix unit.

    One ``seg_len``-quant word plus a 48-bit metadata word (FP32
    ``s_act``, signed INT16 ``sum_a``) per segment. ``QUANTIZE`` (or,
    for host quantization, the uploader) owns the write side; while
    ``pin`` is asserted -- from MATMUL accept to drain -- writes are
    back-pressured and the contents pinned, so reads in flight can
    never observe a torn generation. The read side is private to the
    matrix unit: a transparent synchronous port addressed by segment
    index, whose one-cycle latency the ACT read-ahead accounts for.
    """

    def __init__(self, params):
        super().__init__(params)

        self.wr = Decoupled(Record, [
            ('seg', self.lg_act_segs, Direction.FANOUT),
            ('quants', self.seg_len * 8, Direction.FANOUT),
            ('s_act', 32, Direction.FANOUT),
            ('sum_a', signed(16), Direction.FANOUT),
        ])
        self.pin = Signal()

        self.rd_seg = Signal(self.lg_act_segs)
        self.rd_quants = Signal(self.seg_len * 8)
        self.rd_s_act = Signal(32)
        self.rd_sum_a = Signal(signed(16))

    def elaborate(self, platform):
        m = Module()

        quants = Memory(width=self.seg_len * 8,
                        depth=self.act_segs,
                        name='act_quants')
        meta = Memory(width=48, depth=self.act_segs, name='act_meta')

        qw = m.submodules.act_qw = quants.write_port()
        mw = m.submodules.act_mw = meta.write_port()
        qr = m.submodules.act_qr = quants.read_port(domain='sync',
                                                    transparent=True)
        mr = m.submodules.act_mr = meta.read_port(domain='sync',
                                                  transparent=True)

        m.d.comb += [
            self.wr.ready.eq(~self.pin),
            qw.addr.eq(self.wr.bits.seg),
            qw.data.eq(self.wr.bits.quants),
            qw.en.eq(self.wr.fire),
            mw.addr.eq(self.wr.bits.seg),
            mw.data.eq(
                Cat(self.wr.bits.s_act, self.wr.bits.sum_a.as_unsigned())),
            mw.en.eq(self.wr.fire),
            qr.addr.eq(self.rd_seg),
            mr.addr.eq(self.rd_seg),
            self.rd_quants.eq(qr.data),
            self.rd_s_act.eq(mr.data[:32]),
            self.rd_sum_a.eq(mr.data[32:]),
        ]

        return m


class WeightSupply(HasTRoomParams, Elaboratable):
    """TMEM-to-pipeline weight feeder for the matrix unit.

    One row cursor per output pipeline walks the tile: pipeline ``p``
    fills and decodes rows ``p, p + n_outputs, ...`` so the lanes of a
    row pair stay in lockstep at the launch join. A round-robin arbiter
    shares the single-outstanding TMEM read port, routing each fetched
    beat's valid bytes (device rows pad to the beat; padding is never
    taken) into the owning pipeline's ping/pong block buffers. Because
    valid row bytes are an exact whole number of blocks, the filled
    byte stream is one contiguous sequence of blocks across the whole
    tile; each buffer alternates per global block index, and fill
    pauses whenever it would run two blocks ahead of decode, so a
    buffer being decoded is never overwritten. A beat may span several
    blocks (Q5_0's 22-byte blocks against wide beats); per-lane
    cursors advance the buffer offset and parity per block-sized chunk.

    Per-pipeline decoders (``troom.unpack``) decode one whole segment
    per cycle from the buffer the global decode cursor selects and push
    ``WSegWord`` records into per-pipeline FIFOs. Q4_K/Q6_K
    super-block gathering is exactly the ping/pong block buffer;
    nothing decodes until a block is complete.
    """

    def __init__(self, params):
        super().__init__(params)

        self.tmem_rreq = Decoupled(
            Record, [('addr', self.tmem_addr_width, Direction.FANOUT)])
        self.tmem_rresp = Decoupled(
            Record, [('data', self.data_width, Direction.FANOUT)])

        self.seg_q = [
            # Register the decode/MAC boundary even when the FIFO is empty.
            Queue(self.mm_seg_fifo, WSegWord, self.params, flow=False)
            for _ in range(self.n_outputs)
        ]
        self.segs = [q.deq for q in self.seg_q]

        # Held configuration and the one-cycle start pulse from the
        # control FSM. ``row_bytes`` and ``blocks_per_row`` are the
        # format-derived tile geometry.
        self.run = Signal()
        self.start = Signal()
        self.fmt = Signal(Format)
        self.rows = Signal(self.rows_bits)
        self.w_base = Signal(self.tmem_addr_width)
        self.w_stride = Signal(self.tmem_addr_width)
        self.row_bytes = Signal(self.row_bytes_bits)
        self.blocks_per_row = Signal(self.row_bytes_bits)

        self.beats = Signal(48)

        # Per-pipeline fill/decode cursors, kept as attributes for
        # waveform and testbench observation. The row cursors are one
        # bit wider than ``rows_bits``: advancing past a tile's last
        # row (a maximum-row-count tile reaches 2**rows_bits) must not
        # wrap back into the row set, or the feeder would re-read that
        # row and queue its segments for the next command.
        self.f_row = []
        self.f_beat = []
        self.f_cnt = []
        self.f_off = []
        self.f_blks = []
        self.g_dec = []
        self.seg_j = []
        self.d_row = []
        self.d_blk = []
        for _ in range(self.n_outputs):
            self.f_row.append(Signal(self.rows_bits + 1))
            self.f_beat.append(Signal(self.row_bytes_bits))
            self.f_cnt.append(Signal(self.row_bytes_bits))
            self.f_off.append(Signal(8))
            self.f_blks.append(Signal(32))
            self.g_dec.append(Signal(32))
            self.seg_j.append(Signal(3))
            self.d_row.append(Signal(self.rows_bits + 1))
            self.d_blk.append(Signal(self.row_bytes_bits))

    def elaborate(self, platform):
        m = Module()

        n = self.n_outputs
        lg = self.lg_beat
        lgn = log2_int(n)
        buf_depth = MAX_BLOCK_BYTES

        # Only the configured formats get decoders; ``fmt_sel`` maps
        # the descriptor's format onto the subset (the unit rejects
        # unconfigured formats at accept).
        sub = self.mm_formats
        pos = Array(
            [self.fmt_pos.get(Format(f), 0) for f in range(len(MM_FORMATS))])
        fmt_sel = pos[self.fmt]
        bb = Array([BLOCK_BYTES[f] for f in sub])[fmt_sel]
        bs = Array([BLOCK_SEGS[f] for f in sub])[fmt_sel]

        for p in range(n):
            m.submodules[f'seg_q{p}'] = self.seg_q[p]

        #
        # Per-pipeline fill and decode state.
        #
        # Block buffers as single wide registers: a beat commits with
        # one shifted OR (a dynamic shift is one operation in
        # simulation), and the decoders slice static byte positions
        # out of the selected buffer.
        ping = [Signal(buf_depth * 8, name=f'ping{p}') for p in range(n)]
        pong = [Signal(buf_depth * 8, name=f'pong{p}') for p in range(n)]
        beat_data = [
            Signal(self.beat_bytes * 8, name=f'beat_data{p}') for p in range(n)
        ]
        beat_left = [
            Signal(range(self.beat_bytes + 1), name=f'beat_left{p}')
            for p in range(n)
        ]

        f_row = self.f_row
        f_beat = self.f_beat
        f_cnt = self.f_cnt
        f_off = self.f_off
        f_blks = self.f_blks
        g_dec = self.g_dec
        seg_j = self.seg_j
        d_row = self.d_row
        d_blk = self.d_blk

        # Fill works while the cursor is inside the tile's row set, and
        # pauses a pipeline whose fill would run two blocks ahead of
        # its decode so ping/pong never collide. A pipeline with a
        # saved beat tail takes no new beats until that tail is consumed.
        fill_work = [f_row[p] < self.rows for p in range(n)]
        gate_ok = [(f_blks[p] - g_dec[p]) < 2 for p in range(n)]
        req = [
            fill_work[p] & gate_ok[p] & (beat_left[p] == 0) for p in range(n)
        ]

        #
        # Read-port arbiter: one outstanding beat at a time, round
        # robin, owner tracked for response routing.
        #
        pend = Signal()
        owner = Signal(range(max(n, 2)))
        rr = Signal(range(max(n, 2)))

        req_a = Array(req)
        found = Signal()
        sel_p = Signal(range(max(n, 2)))
        # Later assignments win, so give the first round-robin candidate
        # highest priority without feeding found back into its own logic.
        for q in reversed(range(n)):
            idx = (rr + q)[:lgn] if lgn else Const(0, 1)
            with m.If(req_a[idx]):
                m.d.comb += [found.eq(1), sel_p.eq(idx)]

        # Incremental addresses keep the row multiply and lane selection
        # out of the request -> TMEM BRAM enable/address path. Each lane
        # walks every n-th row, and advances a beat only on its response.
        row_addr = [
            Signal(self.tmem_addr_width, name=f'row_addr{p}') for p in range(n)
        ]
        beat_addr = [
            Signal(self.tmem_addr_width, name=f'beat_addr{p}')
            for p in range(n)
        ]

        m.d.comb += [
            # Only an active command may consume shared TMEM read
            # service; ``run`` is low in IDLE, so a rejected
            # descriptor's stale cursors request nothing.
            self.tmem_rreq.valid.eq(found & ~pend & self.run),
            self.tmem_rreq.bits.addr.eq(Array(beat_addr)[sel_p]),
        ]
        with m.If(self.tmem_rreq.fire):
            m.d.sync += [
                pend.eq(1),
                owner.eq(sel_p),
                rr.eq((sel_p + 1)[:lgn] if lgn else Const(0, 1)),
            ]

        m.d.comb += self.tmem_rresp.ready.eq(1)
        with m.If(pend & self.tmem_rresp.fire):
            m.d.sync += [pend.eq(0), self.beats.eq(self.beats + 1)]

        for p in range(n):
            #
            # Beat consumption. Valid row bytes are a prefix of the
            # beat (device rows pad at the end), so lane k lands at
            # block offset f_off + k. A beat that fits the current
            # block commits in one shifted-OR write; a beat crossing a
            # block boundary commits the fitting part and copies the
            # rest one block-sized chunk per cycle, which stays
            # correct for any beat/block size ratio. Because valid row
            # bytes are a whole number of blocks, f_off runs
            # continuously across rows: the tile's blocks are one
            # contiguous stream and buffer parity alternates per global
            # block index.
            #
            base = f_beat[p] << lg
            take_all = (base + self.beat_bytes) <= self.row_bytes
            # The remainder is bounded by one beat; keep it narrow so
            # downstream shifts stay small.
            m_cnt = Mux(take_all, self.beat_bytes,
                        (self.row_bytes - base)[:self.lg_beat + 1])
            data = self.tmem_rresp.bits.data
            # A variable (1 << bit_count) - 1 becomes a very wide
            # subtractor in FPGA synthesis, followed by the buffer
            # barrel shifter. Decode the byte enables directly instead:
            # each comparator is only as wide as the byte count.
            taken = Signal(self.data_width)
            m.d.comb += taken.eq(
                Cat(*(Mux(m_cnt > i, data.word_select(i, 8), 0)
                      for i in range(self.beat_bytes))))

            hit = pend & self.tmem_rresp.fire & (owner == p) \
                & (beat_left[p] == 0)
            fast = (f_off[p] + m_cnt) <= bb
            split = (bb - f_off[p])[:8]
            fitting = Signal(self.data_width)
            m.d.comb += fitting.eq(
                Cat(*(Mux(split > i, taken.word_select(i, 8), 0)
                      for i in range(self.beat_bytes))))

            def commit(par, word):
                with m.If(par):
                    m.d.sync += pong[p].eq(
                        Mux(f_off[p] == 0, 0, pong[p]) | word)
                with m.Else():
                    m.d.sync += ping[p].eq(
                        Mux(f_off[p] == 0, 0, ping[p]) | word)

            # Every saved chunk starts at offset zero: a full block
            # leaves another aligned tail, and a partial block consumes
            # the entire tail. Use constant per-format shifts rather
            # than another variable shifter on the buffer write path.
            tail_count = Signal(range(self.beat_bytes + 1))
            m.d.comb += tail_count.eq(Mux(beat_left[p] < bb, beat_left[p], bb))
            tail_word = Cat(
                *(Mux(tail_count > i, beat_data[p].word_select(i, 8), 0)
                  for i in range(self.beat_bytes)))
            tail_next = Array(
                [beat_data[p] >> (8 * BLOCK_BYTES[f]) for f in sub])[fmt_sel]

            with m.If(hit):
                with m.If(fast):
                    commit(f_blks[p][0], taken << (8 * f_off[p]))
                    with m.If(f_off[p] + m_cnt == bb):
                        m.d.sync += [
                            f_off[p].eq(0), f_blks[p].eq(f_blks[p] + 1)
                        ]
                    with m.Else():
                        m.d.sync += f_off[p].eq(f_off[p] + m_cnt)
                with m.Else():
                    # The fitting part completes this block; the tail
                    # starts in the next buffer at offset 0.
                    commit(f_blks[p][0], fitting << (8 * f_off[p]))
                    m.d.sync += [
                        f_off[p].eq(0),
                        f_blks[p].eq(f_blks[p] + 1),
                        beat_data[p].eq(data >> (8 * split)),
                        beat_left[p].eq(m_cnt - split),
                    ]
                m.d.sync += [
                    f_beat[p].eq(f_beat[p] + 1),
                    f_cnt[p].eq(f_cnt[p] + m_cnt),
                    beat_addr[p].eq(beat_addr[p] + self.beat_bytes),
                ]
                with m.If(f_cnt[p] + m_cnt == self.row_bytes):
                    m.d.sync += [
                        f_row[p].eq(f_row[p] + n),
                        f_beat[p].eq(0),
                        f_cnt[p].eq(0),
                        row_addr[p].eq(row_addr[p] + n * self.w_stride),
                        beat_addr[p].eq(row_addr[p] + n * self.w_stride),
                    ]
            # Completing a block can leave both buffers occupied.
            # The saved beat tail must wait for decode to release the
            # next buffer, just like a new read request would.
            with m.Elif((beat_left[p] != 0) & gate_ok[p]):
                commit(f_blks[p][0], tail_word)
                m.d.sync += [
                    beat_data[p].eq(tail_next),
                    beat_left[p].eq(beat_left[p] - tail_count),
                ]
                with m.If(tail_count == bb):
                    m.d.sync += [f_off[p].eq(0), f_blks[p].eq(f_blks[p] + 1)]
                with m.Else():
                    m.d.sync += f_off[p].eq(tail_count)

            #
            # Decode: one logical segment per cycle from the completed
            # block the global decode cursor selects.
            #
            bufw = Mux(g_dec[p][0], pong[p], ping[p])
            bufm = [bufw[8 * i:8 * i + 8] for i in range(buf_depth)]
            dec_w = []
            dec_d = []
            dec_dmin = []
            dec_mi = []
            dec_sc0 = []
            dec_sc1 = []
            for f in sub:
                ws, ds, dmins, mis, sc0s, sc1s = [], [], [], [], [], []
                for j in range(BLOCK_SEGS[f]):
                    w, meta = decode_segment(f, bufm, j)
                    ws.append(tuple(w))
                    ds.append(meta['d'])
                    dmins.append(meta['dmin'])
                    mis.append(meta['mi'])
                    sc0s.append(meta['sc0'])
                    sc1s.append(meta['sc1'])
                dec_w.append(Array(ws))
                dec_d.append(Array(ds))
                dec_dmin.append(Array(dmins))
                dec_mi.append(Array(mis))
                dec_sc0.append(Array(sc0s))
                dec_sc1.append(Array(sc1s))
            dec_w = Array(dec_w)[fmt_sel]
            dec_d = Array(dec_d)[fmt_sel]
            dec_dmin = Array(dec_dmin)[fmt_sel]
            dec_mi = Array(dec_mi)[fmt_sel]
            dec_sc0 = Array(dec_sc0)[fmt_sel]
            dec_sc1 = Array(dec_sc1)[fmt_sel]

            dec_en = self.run & (g_dec[p] < f_blks[p]) \
                & self.seg_q[p].enq.ready

            m.d.comb += [
                self.seg_q[p].enq.valid.eq(dec_en),
                self.seg_q[p].enq.bits.w.eq(Cat(*dec_w[seg_j[p]])),
                self.seg_q[p].enq.bits.d.eq(dec_d[seg_j[p]]),
                self.seg_q[p].enq.bits.dmin.eq(dec_dmin[seg_j[p]]),
                self.seg_q[p].enq.bits.mi.eq(dec_mi[seg_j[p]]),
                self.seg_q[p].enq.bits.sc0.eq(dec_sc0[seg_j[p]]),
                self.seg_q[p].enq.bits.sc1.eq(dec_sc1[seg_j[p]]),
                self.seg_q[p].enq.bits.row.eq(d_row[p][:self.rows_bits]),
            ]

            with m.If(self.seg_q[p].enq.fire):
                with m.If(seg_j[p] + 1 == bs):
                    m.d.sync += [seg_j[p].eq(0), g_dec[p].eq(g_dec[p] + 1)]
                    with m.If(d_blk[p] + 1 == self.blocks_per_row):
                        m.d.sync += [
                            d_blk[p].eq(0),
                            d_row[p].eq(d_row[p] + n),
                        ]
                    with m.Else():
                        m.d.sync += d_blk[p].eq(d_blk[p] + 1)
                with m.Else():
                    m.d.sync += seg_j[p].eq(seg_j[p] + 1)

        #
        # Start: reset every cursor for a freshly accepted tile.
        #
        with m.If(self.start):
            for p in range(n):
                m.d.sync += [
                    f_row[p].eq(p),
                    f_beat[p].eq(0),
                    f_cnt[p].eq(0),
                    f_off[p].eq(0),
                    f_blks[p].eq(0),
                    g_dec[p].eq(0),
                    seg_j[p].eq(0),
                    d_row[p].eq(p),
                    d_blk[p].eq(0),
                    beat_left[p].eq(0),
                    row_addr[p].eq(self.w_base + p * self.w_stride),
                    beat_addr[p].eq(self.w_base + p * self.w_stride),
                ]
            m.d.sync += [pend.eq(0), rr.eq(0)]

        return m


class MacArray(HasTRoomParams, Elaboratable):
    """Skewed MAC chains and integer chain-end cells.

    The array free-runs: every cycle it consumes one slot's operands
    (weight words per pipeline, the shared activation word, and the
    slot's metadata) whether the slot is valid or not. Element ``i`` of
    a segment reaches its chain position delayed ``i % CL`` cycles,
    where position ``pos`` of chain ``c`` multiplies element
    ``CL * c + pos``; the activation skew is shared between pipelines
    and the weight skew is per pipeline. Both chains of a pipeline
    launch together and have identical latency, so their ends meet the
    slot's metadata (delayed ``L_PE + CL - 1`` cycles) at the integer
    combine. Invalid slots ripple through as zeroes with ``valid = 0``
    metadata and never produce a record.
    """

    def __init__(self, params):
        super().__init__(params)

        self.cl = self.seg_len // 2
        self.l_pe = L_PE

        self.w_in = [
            Signal(self.seg_len * 8, name=f'w_in{p}')
            for p in range(self.n_outputs)
        ]
        self.a_in = Signal(self.seg_len * 8)
        self.meta_in = SlotMeta(self.params)

        self.segs = Decoupled(MmSegment, self.params)

    def elaborate(self, platform):
        m = Module()

        cl = self.cl

        def skew(val, depth, name):
            """Delay ``val`` by ``depth`` cycles (depth 0: a wire)."""
            if depth == 0:
                return val
            regs = [
                Signal.like(val, name=f'{name}{depth}_{i}')
                for i in range(depth)
            ]
            m.d.sync += regs[0].eq(val)
            for i in range(1, depth):
                m.d.sync += regs[i].eq(regs[i - 1])
            return regs[-1]

        # Shared activation skew: element pairs (pos, pos + CL) ride
        # one delay line per position.
        a_pairs = []
        for pos in range(cl):
            in_pair = Cat(self.a_in[8 * pos:8 * pos + 8],
                          self.a_in[8 * (pos + cl):8 * (pos + cl) + 8])
            a_pairs.append(skew(in_pair, pos, 'a_skew'))

        chain_end = [[None, None] for _ in range(self.n_outputs)]
        for p in range(self.n_outputs):
            w_pairs = []
            for pos in range(cl):
                in_pair = Cat(self.w_in[p][8 * pos:8 * pos + 8],
                              self.w_in[p][8 * (pos + cl):8 * (pos + cl) + 8])
                w_pairs.append(skew(in_pair, pos, f'w{p}_skew'))

            for c in (0, 1):
                pregs = []
                for pos in range(cl):
                    wb = w_pairs[pos][8 * c:8 * c + 8].as_signed()
                    ab = a_pairs[pos][8 * c:8 * c + 8].as_signed()
                    pe = IntMac(first=(pos == 0))
                    m.submodules[f'pe{p}_{c}_{pos}'] = pe
                    m.d.comb += [
                        pe.a.eq(wb),
                        pe.b.eq(ab),
                        pe.cascade_in.eq(0 if pos == 0 else pregs[-1]),
                    ]
                    pregs.append(pe.cascade_out)
                chain_end[p][c] = pe.result[:24].as_signed()

        # Metadata delay matched to the chains.
        depth = self.l_pe + cl - 1
        stages = [
            SlotMeta(self.params, name=f'meta_d{i}') for i in range(depth)
        ]
        m.d.sync += stages[0].eq(self.meta_in)
        for i in range(1, depth):
            m.d.sync += stages[i].eq(stages[i - 1])
        meta = stages[-1]

        # Integer chain-end combine, per format.
        rec = MmSegment(self.params)
        rec_valid = Signal()
        for p in range(self.n_outputs):
            p0 = chain_end[p][0]
            p1 = chain_end[p][1]
            sca = getattr(meta, f'sc{p}a').as_signed()
            scb = getattr(meta, f'sc{p}b').as_signed()
            segment_sum = Mux(
                (meta.fmt == Format.Q5_0) | (meta.fmt == Format.Q8_0),
                _s32(p0 + p1),
                Mux(meta.fmt == Format.Q4_K, _s32(sca * (p0 + p1)),
                    _s32(sca * p0 + scb * p1)))
            m.d.sync += getattr(rec, f'S{p}').eq(segment_sum)
            for f in ('row', 'd', 'dmin', 'mi'):
                m.d.sync += getattr(rec,
                                    f'{f}{p}').eq(getattr(meta, f'{f}{p}'))
        for f in ('fmt', 'row_mask', 's_act', 'sum_a', 'seg_k', 'row_last'):
            m.d.sync += getattr(rec, f).eq(getattr(meta, f))
        m.d.sync += rec_valid.eq(meta.valid)

        m.d.comb += [
            self.segs.valid.eq(rec_valid),
            self.segs.bits.eq(rec),
        ]

        return m


class MatrixUnit(HasTRoomParams, Elaboratable):
    """Activation-stationary matrix unit.

    ``sink`` accepts one MATMUL descriptor at a time; V1 drains a
    matmul completely before accepting another or unpinning ACT RAM.
    ``segs`` emits the chain-end records, in launch order, to the FP32
    epilogue. A descriptor whose shape the formats cannot express
    completes immediately with ``BAD_SHAPE``: a reduction length that
    is not a whole number of blocks (K-quants additionally require K
    divisible by 256), K beyond ``act_max_k``, no rows, or a stride or
    base that is not beat aligned, a stride smaller than the packed
    row, or a tile running off the end of TMEM.

    Launch gating: a valid slot needs every unmasked pipeline's decoded
    segment, the ACT segment, and free output credit. The credit
    counter tracks every launched record until it is dequeued, so the
    output queue can never overflow under downstream backpressure,
    however long.
    """

    def __init__(self, params):
        super().__init__(params)

        self.sink = Decoupled(MatmulDesc, self.params)
        self.segs = Decoupled(MmSegment, self.params)

        self.actram = ActRAM(self.params)
        self.supply = WeightSupply(self.params)
        self.mac = MacArray(self.params)

        # The unit's private TMEM read port; sharing it with DMA stores
        # is the later arbiter milestone.
        self.tmem_rreq = self.supply.tmem_rreq
        self.tmem_rresp = self.supply.tmem_rresp
        self.act_wr = self.actram.wr

        self.done = Signal()
        self.status = Signal(Status)
        self.active = Signal()
        # Pulses for one cycle when a descriptor passes accept-time
        # validation; the engine starts the result drain alongside.
        self.start_ok = Signal()

        # Observation counters from the plan's scheduling section:
        # valid slots versus inserted bubbles, and why.
        self.slots = Signal(48)
        self.bubbles = Signal(48)
        self.starve_cycles = Signal(48)
        self.credit_stalls = Signal(48)
        self.useful_macs = Signal(48)
        self.feed_beats = Signal(48)
        # Cycle-breakdown window (M3b): cycles spent launching slots
        # versus waiting for the launched records to drain through the
        # epilogue. Bubbles inside RUN split into starve + credit.
        self.run_cycles = Signal(48)
        self.drain_cycles = Signal(48)

    def elaborate(self, platform):
        m = Module()

        m.submodules.actram = self.actram
        m.submodules.supply = self.supply
        m.submodules.mac = self.mac

        out_q = m.submodules.out_q = Queue(self.mm_out_depth, MmSegment,
                                           self.params)
        m.d.comb += [
            self.mac.segs.connect(out_q.enq),
            out_q.deq.connect(self.segs),
        ]

        n = self.n_outputs
        lgn = log2_int(n)

        cur_fmt = Signal(Format)
        cur_k = Signal(self.lg_act_k + 1)
        cur_rows = Signal(self.rows_bits)
        cur_base = Signal(self.tmem_addr_width)
        cur_stride = Signal(self.tmem_addr_width)

        # Format-derived geometry for the accepted shape.
        bb = Array([BLOCK_BYTES[Format(f)] for f in range(len(MM_FORMATS))])
        ksh = Array([Const(5), Const(5), Const(8), Const(8)])
        blocks_per_row = cur_k >> ksh[cur_fmt]
        row_bytes = (blocks_per_row * bb[cur_fmt])[:self.row_bytes_bits]
        nsegs = cur_k >> 5
        pair_row = Cat(cur_rows, Const(0, lgn)) if lgn else cur_rows
        pairs = ((pair_row + n - 1) >> lgn)[:self.rows_bits]

        #
        # Accept-time validation against the offered descriptor.
        #
        offer = self.sink.bits
        off_blocks = offer.k >> ksh[offer.fmt]
        off_row_bytes = off_blocks * bb[offer.fmt]
        is_kquant = (offer.fmt == Format.Q4_K) | (offer.fmt == Format.Q6_K)
        off_last_row = (offer.rows - 1)[:self.rows_bits]
        tile_end = (offer.w_base + off_last_row * offer.w_stride +
                    off_row_bytes)
        fmt_enabled = Array([
            Const(0 if Format(f) not in self.fmt_pos else 1, 1)
            for f in range(len(MM_FORMATS))
        ])[offer.fmt]

        shape_ok = (fmt_enabled
                    & (offer.k >= self.seg_len)
                    & (offer.k <= self.act_max_k)
                    & (offer.k[:5] == 0)
                    & ~(is_kquant & offer.k[:8].any())
                    & (offer.rows != 0)
                    & (offer.w_stride[:self.lg_beat] == 0)
                    & (offer.w_base[:self.lg_beat] == 0)
                    & (offer.w_stride >= off_row_bytes)
                    & (tile_end <= self.tmem_bytes))

        pair = Signal(self.rows_bits)
        seg = Signal(self.lg_act_segs)
        rd_idx = Signal(self.lg_act_segs + 1)
        rd_issue = Signal()
        rd_pending = Signal()
        m.d.sync += rd_pending.eq(rd_issue)
        act_cnt = Signal(range(self.mm_act_fifo + 1))
        act_e = [
            ActWord(self.params, name=f'act_e{i}')
            for i in range(self.mm_act_fifo)
        ]
        resv = Signal(range(self.mm_out_depth + 1))

        start = Signal()
        rd_word = ActWord(self.params, name='act_rd_word')
        m.d.comb += [
            rd_word.quants.eq(self.actram.rd_quants),
            rd_word.s_act.eq(self.actram.rd_s_act),
            rd_word.sum_a.eq(self.actram.rd_sum_a),
        ]

        with m.FSM() as fsm:
            with m.State('IDLE'):
                m.d.comb += self.sink.ready.eq(1)
                # A rejected descriptor latches nothing: the supply's
                # configuration keeps the previous (completed or reset)
                # tile, whose cursors are inert, and no read can issue
                # for a command that never started.
                with m.If(self.sink.fire & shape_ok):
                    m.d.sync += [
                        cur_fmt.eq(offer.fmt),
                        cur_k.eq(offer.k),
                        cur_rows.eq(offer.rows),
                        cur_base.eq(offer.w_base),
                        cur_stride.eq(offer.w_stride),
                    ]
                    m.d.comb += start.eq(1)
                    m.d.sync += [
                        pair.eq(0),
                        seg.eq(0),
                        rd_idx.eq(0),
                        act_cnt.eq(0),
                        resv.eq(0),
                    ]
                    m.next = 'RUN'
                with m.Elif(self.sink.fire):
                    m.d.comb += [
                        self.done.eq(1),
                        self.status.eq(Status.BAD_SHAPE),
                    ]

            with m.State('RUN'):
                #
                # Launch join: one slot per cycle, valid only when
                # every unmasked pipeline, the ACT segment, and the
                # output credit agree. Otherwise the free-running
                # array takes an invalid slot.
                #
                rows_w = pair_row
                unmasked = [(((pair << lgn) + p) < rows_w) for p in range(n)]
                ready = [
                    ~unmasked[p] | self.supply.segs[p].valid for p in range(n)
                ]
                act_ok = act_cnt != 0
                operands_ok = act_ok & Cat(ready).all()
                credit_ok = resv < self.mm_out_depth
                seg_fire = operands_ok & credit_ok

                head = act_e[0]
                meta = self.mac.meta_in
                m.d.comb += [
                    meta.valid.eq(seg_fire),
                    meta.fmt.eq(cur_fmt),
                    meta.row_mask.eq(Cat(unmasked)),
                    meta.s_act.eq(Mux(seg_fire, head.s_act, 0)),
                    meta.sum_a.eq(Mux(seg_fire, head.sum_a, 0)),
                    meta.seg_k.eq(seg),
                    meta.row_last.eq(seg + 1 == nsegs),
                    self.mac.a_in.eq(Mux(seg_fire, head.quants, 0)),
                ]
                for p in range(n):
                    feed_p = seg_fire & unmasked[p]
                    whead = self.supply.segs[p].bits
                    lane_row = ((pair << lgn) + p)[:self.rows_bits]
                    m.d.comb += [
                        self.mac.w_in[p].eq(Mux(feed_p, whead.w, 0)),
                        getattr(meta, f'row{p}').eq(
                            Mux(unmasked[p], whead.row, lane_row)),
                        getattr(meta, f'd{p}').eq(Mux(feed_p, whead.d, 0)),
                        getattr(meta,
                                f'dmin{p}').eq(Mux(feed_p, whead.dmin, 0)),
                        getattr(meta, f'mi{p}').eq(Mux(feed_p, whead.mi, 0)),
                        getattr(meta, f'sc{p}a').eq(Mux(feed_p, whead.sc0, 0)),
                        getattr(meta, f'sc{p}b').eq(Mux(feed_p, whead.sc1, 0)),
                        self.supply.seg_q[p].deq.ready.eq(feed_p),
                    ]

                #
                # ACT read-ahead: reserve space for the synchronous
                # RAM response before issuing each read. The pair cursor
                # rewinds the index at each row-pair boundary, by which point the
                # previous pair's segments were all consumed.
                #
                m.d.comb += rd_issue.eq((rd_idx != nsegs)
                                        & (act_cnt +
                                           rd_pending < self.mm_act_fifo))
                push = rd_pending
                m.d.comb += self.actram.rd_seg.eq(rd_idx[:self.lg_act_segs])
                with m.If(rd_issue):
                    m.d.sync += rd_idx.eq(rd_idx + 1)
                with m.If(seg_fire & ~push):
                    m.d.sync += act_cnt.eq(act_cnt - 1)
                with m.Elif(push & ~seg_fire):
                    m.d.sync += act_cnt.eq(act_cnt + 1)
                # Shift consumed entries out before appending, so a
                # simultaneous push lands in the freed slot.
                with m.If(seg_fire):
                    for i in range(self.mm_act_fifo - 1):
                        m.d.sync += act_e[i].eq(act_e[i + 1])
                with m.If(push):
                    tgt = act_cnt - seg_fire
                    for i in range(self.mm_act_fifo):
                        with m.If(tgt == i):
                            m.d.sync += act_e[i].eq(rd_word)

                # Credit: reserve one output-queue slot per launched
                # record until it is dequeued downstream.
                with m.If(seg_fire & ~self.segs.fire):
                    m.d.sync += resv.eq(resv + 1)
                with m.Elif(self.segs.fire & ~seg_fire):
                    m.d.sync += resv.eq(resv - 1)

                # Slot accounting and the logical K / row-pair cursors.
                n_active = sum(unmasked)
                m.d.sync += self.run_cycles.eq(self.run_cycles + 1)
                with m.If(seg_fire):
                    m.d.sync += [
                        self.slots.eq(self.slots + 1),
                        self.useful_macs.eq(self.useful_macs +
                                            self.seg_len * n_active),
                    ]
                with m.Else():
                    m.d.sync += self.bubbles.eq(self.bubbles + 1)
                    with m.If(~operands_ok):
                        m.d.sync += self.starve_cycles.eq(self.starve_cycles +
                                                          1)
                    with m.Elif(~credit_ok):
                        m.d.sync += self.credit_stalls.eq(self.credit_stalls +
                                                          1)

                with m.If(seg_fire):
                    with m.If(seg + 1 == nsegs):
                        m.d.sync += seg.eq(0)
                        with m.If(pair + 1 == pairs):
                            m.next = 'DRAIN'
                        with m.Else():
                            m.d.sync += [pair.eq(pair + 1), rd_idx.eq(0)]
                    with m.Else():
                        m.d.sync += seg.eq(seg + 1)

            with m.State('DRAIN'):
                # A matmul completes only when its last record has been
                # dequeued, not merely launched.
                m.d.sync += self.drain_cycles.eq(self.drain_cycles + 1)
                with m.If(self.segs.fire):
                    m.d.sync += resv.eq(resv - 1)
                with m.If(resv == 0):
                    m.d.comb += [
                        self.done.eq(1),
                        self.status.eq(Status.OK),
                    ]
                    m.next = 'IDLE'

        m.d.comb += [
            self.supply.run.eq(~fsm.ongoing('IDLE')),
            self.supply.start.eq(start),
            self.start_ok.eq(start),
            self.supply.fmt.eq(cur_fmt),
            self.supply.rows.eq(cur_rows),
            # Address cursors initialize on the accepting edge, before
            # the held configuration registers acquire the new tile.
            self.supply.w_base.eq(Mux(start, offer.w_base, cur_base)),
            self.supply.w_stride.eq(Mux(start, offer.w_stride, cur_stride)),
            self.supply.row_bytes.eq(row_bytes),
            self.supply.blocks_per_row.eq(blocks_per_row),
            self.actram.pin.eq(~fsm.ongoing('IDLE')),
            self.active.eq(~fsm.ongoing('IDLE')),
            self.feed_beats.eq(self.supply.beats),
        ]

        return m
