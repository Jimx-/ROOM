"""Shared matrix records, activation RAM and systolic integer MAC arithmetic.

StreamGemv owns scheduling and DDR weight supply. These building blocks have
no legacy TMEM weight reader or matrix command executor.
"""

from amaranth import *
from amaranth.hdl.rec import Direction, Record
from amaranth.utils import log2_int

from troom.consts import Format
from troom.types import HasTRoomParams

from roomsoc.interconnect.stream import Decoupled


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
    P2a banked mode (``act_banks == 2``, 2026-11-03): two banks with
    a write-bank / read-bank ownership pair. Writes always target
    ``wr_bank`` and are always accepted; ``commit`` (the stream
    unit's accept pulse) latches ``rd_bank := wr_bank`` and flips
    ``wr_bank`` -- but only when a fill landed since the previous
    commit, preserving the historical persistence contract (a
    back-to-back SMATMUL pair with no SQUANTIZE between them reads
    the same activation, the shared-x follower discipline). A loader
    positioned before a stream command in the in-order issue stream
    fills the bank that command will read, while a loader positioned
    after it fills the bank the *next* command will read -- the two
    never collide. ``pin`` is ignored (each bank's only reader is
    the committed stream command, which by the single-context accept
    discipline has finished before the next commit). The commit must
    arrive with the loader quiescent; Core keeps ``~actload.active``
    on the stream accept so an illegal list stalls instead of
    tearing a bank.
    """

    def __init__(self, params):
        super().__init__(params)

        self.banks = self.act_banks
        self.wr = Decoupled(Record, [
            ('seg', self.lg_act_segs, Direction.FANOUT),
            ('quants', self.seg_len * 8, Direction.FANOUT),
            ('s_act', 32, Direction.FANOUT),
            ('sum_a', signed(16), Direction.FANOUT),
        ])
        self.pin = Signal()
        # P2a: pulse to latch the read bank and rotate the write bank
        # (banked mode only).
        self.commit = Signal()

        self.rd_seg = Signal(self.lg_act_segs)
        self.rd_quants = Signal(self.seg_len * 8)
        self.rd_s_act = Signal(32)
        self.rd_sum_a = Signal(signed(16))

    def elaborate(self, platform):
        m = Module()

        quants = [
            Memory(width=self.seg_len * 8,
                   depth=self.act_segs,
                   name='act_quants' if self.banks == 1 else f'act_quants{b}')
            for b in range(self.banks)
        ]
        meta = [
            Memory(width=48,
                   depth=self.act_segs,
                   name='act_meta' if self.banks == 1 else f'act_meta{b}')
            for b in range(self.banks)
        ]

        for b in range(self.banks):
            m.submodules[f'act_qw{b}'] = quants[b].write_port()
            m.submodules[f'act_mw{b}'] = meta[b].write_port()
            m.submodules[f'act_qr{b}'] = quants[b].read_port(domain='sync',
                                                             transparent=True)
            m.submodules[f'act_mr{b}'] = meta[b].read_port(domain='sync',
                                                           transparent=True)
        qw = [getattr(m.submodules, f'act_qw{b}') for b in range(self.banks)]
        mw = [getattr(m.submodules, f'act_mw{b}') for b in range(self.banks)]
        qr = [getattr(m.submodules, f'act_qr{b}') for b in range(self.banks)]
        mr = [getattr(m.submodules, f'act_mr{b}') for b in range(self.banks)]

        if self.banks == 1:
            wr_bank = Const(0)
            rd_bank = Const(0)
            wr_ready = ~self.pin
        else:
            wr_bank = Signal()
            rd_bank = Signal()
            # Persistence: the historical single RAM served every
            # stream command from the latest written content -- a
            # back-to-back SMATMUL pair with no SQUANTIZE between
            # them (the backend's shared-x follower nodes) reads the
            # same activation twice. The commit therefore rotates
            # banks only when a fill landed since the last commit;
            # an unloaded commit is a no-op and the next SQUANTIZE
            # still targets the bank no reader owns.
            loaded = Signal()
            with m.If(self.wr.fire):
                m.d.sync += loaded.eq(1)
            with m.If(self.commit & loaded):
                m.d.sync += [
                    rd_bank.eq(wr_bank),
                    wr_bank.eq(~wr_bank),
                    loaded.eq(0),
                ]
            wr_ready = Const(1)

        rd_q = [
            Signal.like(qr[b].data, name=f'act_rd_q{b}')
            for b in range(self.banks)
        ]
        rd_m = [Signal(48, name=f'act_rd_m{b}') for b in range(self.banks)]

        for b in range(self.banks):
            m.d.comb += [
                qw[b].addr.eq(self.wr.bits.seg),
                qw[b].data.eq(self.wr.bits.quants),
                qw[b].en.eq(self.wr.fire & (wr_bank == b)),
                mw[b].addr.eq(self.wr.bits.seg),
                mw[b].data.eq(
                    Cat(self.wr.bits.s_act, self.wr.bits.sum_a.as_unsigned())),
                mw[b].en.eq(self.wr.fire & (wr_bank == b)),
                qr[b].addr.eq(self.rd_seg),
                mr[b].addr.eq(self.rd_seg),
                rd_q[b].eq(qr[b].data),
                rd_m[b].eq(mr[b].data),
            ]

        rd_quants = rd_q[0]
        rd_meta = rd_m[0]
        for b in range(1, self.banks):
            rd_quants = Mux(rd_bank == b, rd_q[b], rd_quants)
            rd_meta = Mux(rd_bank == b, rd_m[b], rd_meta)

        m.d.comb += [
            self.wr.ready.eq(wr_ready),
            self.rd_quants.eq(rd_quants),
            self.rd_s_act.eq(rd_meta[:32]),
            self.rd_sum_a.eq(rd_meta[32:]),
        ]

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
