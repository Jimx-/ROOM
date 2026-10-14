"""Activation load path for the troom matrix unit.

The initial contract quantizes activations on the host: the host
writes a quantized activation image into TMEM through the DMA, and a
``QUANTIZE`` command copies it into the matrix unit's ACT RAM. This
module is that copy engine. Device-side quantization from FP32
activations exists as the ``VEC_QUANT`` vector op (see
``troom/vector.py``), which writes exactly this image; a future
extension may fold the copy in.

The TMEM image is two dense, beat-aligned regions:

* the quants region at ``a_base``: ``k / 32`` segments of 32 signed
  INT8 quants each, ``nsegs * 32`` bytes total, and
* the meta region at ``m_base``: ``nsegs`` entries of 8 bytes: the
  segment's FP32 ``s_act`` scale, the signed INT16 ``sum_a``, and two
  pad bytes.

Regions are read as consecutive whole beats into per-region byte
windows, so any beat width works: a 64-bit beat covers a quarter of a
segment's quants, a 512-bit beat covers two segments' quants and eight
meta entries. Each collected pair is written into ACT RAM through its
upload port, which the matrix unit back-pressures while a matmul has
the contents pinned.
"""

from amaranth import *
from amaranth.hdl.rec import Direction, Record

from troom.consts import Status
from troom.types import HasTRoomParams

from roomsoc.interconnect.stream import Decoupled


class ActLoadDesc(HasTRoomParams, Record):
    """Activation load descriptor: the two region bases and ``k``."""

    def __init__(self, params, name=None, src_loc_at=0):
        HasTRoomParams.__init__(self, params)
        Record.__init__(self, [
            ('a_base', self.tmem_addr_width, Direction.FANOUT),
            ('m_base', self.tmem_addr_width, Direction.FANOUT),
            ('k', self.lg_act_k + 1, Direction.FANOUT),
        ],
                        name=name,
                        src_loc_at=src_loc_at + 1)


class ActLoader(HasTRoomParams, Elaboratable):
    """TMEM-to-ACT-RAM copy engine for host-quantized activations.

    A descriptor whose ``k`` is not a whole number of segments or
    exceeds ``act_max_k``, whose region bases are not beat aligned, or
    whose regions leave TMEM completes immediately with ``BAD_SHAPE``,
    ``BAD_ALIGN``, or ``BAD_RANGE`` without reading TMEM.
    """

    def __init__(self, params):
        super().__init__(params)

        self.sink = Decoupled(ActLoadDesc, self.params)
        self.tmem_rreq = Decoupled(
            Record, [('addr', self.tmem_addr_width, Direction.FANOUT)])
        self.tmem_rresp = Decoupled(
            Record, [('data', self.data_width, Direction.FANOUT)])
        self.act_wr = Decoupled(Record, [
            ('seg', self.lg_act_segs, Direction.FANOUT),
            ('quants', self.seg_len * 8, Direction.FANOUT),
            ('s_act', 32, Direction.FANOUT),
            ('sum_a', signed(16), Direction.FANOUT),
        ])

        self.done = Signal()
        self.status = Signal(Status)
        self.active = Signal()

        self.segs = Signal(48)
        self.beats = Signal(48)

    def elaborate(self, platform):
        m = Module()

        lg = self.lg_beat
        bb = self.beat_bytes
        aw = self.tmem_addr_width

        offer = self.sink.bits
        nsegs_o = offer.k[5:self.lg_act_k + 1]
        q_bytes = nsegs_o << 5
        m_bytes = nsegs_o << 3
        # Preserve the full expression width: act_max_k can exceed
        # TMEM capacity, so truncating the end could hide overflow.
        q_end = (offer.a_base + (((q_bytes + (bb - 1)) >> lg) << lg))
        m_end = (offer.m_base + (((m_bytes + (bb - 1)) >> lg) << lg))

        shape_bad = ((offer.k < self.seg_len)
                     | (offer.k > self.act_max_k)
                     | offer.k[:5].any())
        align_bad = offer.a_base[:lg].any() | offer.m_base[:lg].any()
        range_bad = (q_end > self.tmem_bytes) | (m_end > self.tmem_bytes)
        bad = shape_bad | align_bad | range_bad

        a_base = Signal(aw)
        m_base = Signal(aw)
        nsegs = Signal(self.lg_act_segs + 1)

        # Per-region byte windows and beat cursors. A window is
        # refilled from the next unread beat of its region whenever it
        # empties; cursors advance monotonically across segments, and
        # a window keeps its leftover bytes across a segment's meta
        # phase (a wide beat may hold several segments' quants).
        qwin = Signal(self.data_width)
        qhave = Signal(range(bb + 1))
        qbeat = Signal(32)
        mwin = Signal(self.data_width)
        mhave = Signal(range(bb + 1))
        mbeat = Signal(32)

        qacc = Signal(self.seg_len * 8)
        qcnt = Signal(range(self.seg_len + 1))
        macc = Signal(64)
        mcnt = Signal(range(9))

        seg = Signal(self.lg_act_segs + 1)
        meta = Signal()
        rd_pend = Signal()
        rd_meta = Signal()

        m.d.comb += self.act_wr.bits.seg.eq(seg[:self.lg_act_segs])

        with m.FSM() as fsm:
            with m.State('IDLE'):
                m.d.comb += [
                    self.sink.ready.eq(1),
                    self.done.eq(self.sink.fire & bad),
                    self.status.eq(
                        Mux(shape_bad, Status.BAD_SHAPE,
                            Mux(align_bad, Status.BAD_ALIGN,
                                Status.BAD_RANGE))),
                ]
                with m.If(self.sink.fire):
                    m.d.sync += [
                        a_base.eq(offer.a_base),
                        m_base.eq(offer.m_base),
                        nsegs.eq(nsegs_o),
                        qbeat.eq(0),
                        mbeat.eq(0),
                        qhave.eq(0),
                        mhave.eq(0),
                        qcnt.eq(0),
                        mcnt.eq(0),
                        seg.eq(0),
                        meta.eq(0),
                        rd_pend.eq(0),
                    ]
                    with m.If(~bad):
                        m.next = 'STREAM'

            with m.State('STREAM'):
                m.d.comb += [
                    self.act_wr.bits.quants.eq(qacc),
                    self.act_wr.bits.s_act.eq(macc[:32]),
                    self.act_wr.bits.sum_a.eq(macc[32:48].as_signed()),
                ]

                # Beat refill: one outstanding read for the region the
                # segment phase needs, routed back through rd_meta.
                q_need = ~meta & (qcnt != self.seg_len)
                m_need = meta & (mcnt != 8)
                m.d.comb += [
                    self.tmem_rreq.valid.eq((q_need & (qhave == 0)
                                             | m_need & (mhave == 0))
                                            & ~rd_pend),
                    self.tmem_rreq.bits.addr.eq(
                        Mux(meta, m_base + (mbeat << lg),
                            a_base + (qbeat << lg))),
                    self.tmem_rresp.ready.eq(1),
                ]
                with m.If(self.tmem_rreq.fire):
                    m.d.sync += [rd_pend.eq(1), rd_meta.eq(meta)]
                with m.If(rd_pend & self.tmem_rresp.fire):
                    m.d.sync += rd_pend.eq(0)
                    with m.If(rd_meta):
                        m.d.sync += [
                            mwin.eq(self.tmem_rresp.bits.data),
                            mhave.eq(bb),
                            mbeat.eq(mbeat + 1)
                        ]
                    with m.Else():
                        m.d.sync += [
                            qwin.eq(self.tmem_rresp.bits.data),
                            qhave.eq(bb),
                            qbeat.eq(qbeat + 1)
                        ]

                with m.If(~meta):
                    # One quant byte per cycle, low byte of the window
                    # first, placed at its segment offset. Explicit
                    # cases keep the bit_select index constant and in
                    # range: the counter shape reaches one past the
                    # final byte, and a dynamic index there generates
                    # out-of-range write temporaries in RTL.
                    with m.If(qcnt == self.seg_len):
                        m.d.sync += meta.eq(1)
                    with m.Elif(qhave != 0):
                        with m.Switch(qcnt):
                            for byte in range(self.seg_len):
                                with m.Case(byte):
                                    m.d.sync += qacc.bit_select(8 * byte,
                                                                8).eq(qwin[:8])
                        m.d.sync += [
                            qcnt.eq(qcnt + 1),
                            qwin.eq(qwin >> 8),
                            qhave.eq(qhave - 1),
                        ]
                with m.Else():
                    with m.If(mcnt == 8):
                        m.d.comb += self.act_wr.valid.eq(1)
                        with m.If(self.act_wr.fire):
                            m.d.sync += [
                                seg.eq(seg + 1),
                                self.segs.eq(self.segs + 1),
                                qcnt.eq(0),
                                mcnt.eq(0),
                                qacc.eq(0),
                                macc.eq(0),
                                meta.eq(0),
                            ]
                            with m.If(seg + 1 == nsegs):
                                m.next = 'DONE'
                    with m.Elif(mhave != 0):
                        # Same constant-index cases for the 8-byte
                        # meta entry; mcnt's shape reaches 8, one past
                        # the final byte of macc.
                        with m.Switch(mcnt):
                            for byte in range(8):
                                with m.Case(byte):
                                    m.d.sync += macc.bit_select(8 * byte,
                                                                8).eq(mwin[:8])
                        m.d.sync += [
                            mcnt.eq(mcnt + 1),
                            mwin.eq(mwin >> 8),
                            mhave.eq(mhave - 1),
                        ]

            with m.State('DONE'):
                m.d.comb += [
                    self.done.eq(1),
                    self.status.eq(Status.OK),
                ]
                m.next = 'IDLE'

        m.d.comb += self.active.eq(~fsm.ongoing('IDLE'))
        with m.If(self.tmem_rresp.fire):
            m.d.sync += self.beats.eq(self.beats + 1)

        return m
