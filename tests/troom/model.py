"""Executable reference model for the troom command contract.

Implements the semantics the engine promises so RTL runs can be
compared against an independent executable specification:

* ordered issue with one completion record per command, in issue order,
* ``SIGNAL`` sets an event, ``WAIT`` retires on a set event and clears
  it, ``END`` completes after draining earlier commands,
* 2D DMA descriptor semantics: beat alignment of every field, TMEM
  range checking row by row, and byte-exact data movement. Beats move
  before errors are observed, as in the RTL: a bus fault fails the
  response rather than the data stream, and a range error stops after
  the in-range prefix,
* status precedence ``BAD_ALIGN`` > ``DMA_ERROR`` > ``BAD_RANGE`` >
  ``OK``,
* the reviewed MATMUL numerical contract: per-format GGUF unpack
  algebra, the integer chain-end combines with their exactness bounds,
  segment/epilogue ordering (``isum_4``), and the ordered FP32
  epilogue with round-to-nearest-even at every step,
* the VECTOR/REDUCE numerical contract: bit-exact FP32
  add/subtract/multiply (double-rounding safe through Python's
  binary64), canonical-NaN policy, the ``isum_4`` reduction placement
  with its paired fold, and the RISC-V ``fmax`` maximum, with the same
  descriptor validation precedence as the RTL,
* the unary transcendental contract (``VEC_EXP2``/``VEC_RECIP``/
  ``VEC_RSQRT``): fixed rounded-op sequences over bit-exact
  ``FPURec`` table seeds, a frozen special-value/clamp table, and
  ulp-bounded accuracy against a binary64 reference.

The DMA part of the model executes commands synchronously. The RTL
issues them asynchronously but completes them in acceptance order, so
completion sequences and final memory states agree as long as events
are not reused by concurrently in-flight commands, which the model
rejects.

The weight pack/unpack below is written from llama.cpp's
``ggml-common.h`` layouts and ``ggml-quants.c`` reference decoders,
independently of ``troom/unpack.py``, so RTL and model agreement
exercises two separate readings of the formats.
"""

import math
import struct

from room.fpu import FPURec

from troom.consts import (
    Format,
    Opcode,
    Status,
    VecOp,
    VEC_ELEMENTWISE,
    VEC_READS_B,
    VEC_REDUCTIONS,
)
from troom.types import HasTRoomParams


def dma_cmd(opcode,
            seq,
            evt=0,
            dram_addr=0,
            tmem_addr=0,
            row_bytes=0,
            rows=0,
            dram_stride=0,
            tmem_stride=0,
            force_error=False):
    """Build a LOAD_2D/STORE_2D command dictionary for the model."""
    return dict(opcode=opcode,
                seq=seq,
                evt=evt,
                dram_addr=dram_addr,
                tmem_addr=tmem_addr,
                row_bytes=row_bytes,
                rows=rows,
                dram_stride=dram_stride,
                tmem_stride=tmem_stride,
                force_error=force_error)


def simple_cmd(opcode, seq, evt=0):
    return dict(opcode=opcode, seq=seq, evt=evt)


class TRoomModel:
    """Reference model over bytearrays for DRAM and TMEM."""

    def __init__(self, params, dram, tmem):
        self.hp = HasTRoomParams(params)
        self.dram = dram
        self.tmem = tmem
        self.events = [False] * self.hp.n_events

    def run(self, cmds):
        """Execute commands, returning ``[(seq, Status), ...]``."""
        completions = []
        for cmd in cmds:
            opcode = cmd['opcode']
            if opcode is Opcode.SIGNAL:
                self.events[cmd['evt']] = True
                completions.append((cmd['seq'], Status.OK))
            elif opcode is Opcode.WAIT:
                if not self.events[cmd['evt']]:
                    raise AssertionError(
                        'model WAIT on a clear event would deadlock the '
                        'RTL; tests must only pair WAIT with events set '
                        'by earlier commands')
                self.events[cmd['evt']] = False
                completions.append((cmd['seq'], Status.OK))
            elif opcode in (Opcode.LOAD_2D, Opcode.STORE_2D):
                status = self._dma(cmd)
                self.events[cmd['evt']] = True
                completions.append((cmd['seq'], status))
            elif opcode in (Opcode.VECTOR, Opcode.REDUCE):
                status = self._vector(cmd)
                self.events[cmd['evt']] = True
                completions.append((cmd['seq'], status))
            elif opcode is Opcode.END:
                completions.append((cmd['seq'], Status.OK))
                break
            else:
                completions.append((cmd['seq'], Status.UNIMPLEMENTED))
        return completions

    def _dma(self, cmd):
        hp = self.hp
        if (cmd['row_bytes'] == 0 or cmd['rows'] == 0
                or cmd['dram_addr'] % hp.beat_bytes
                or cmd['tmem_addr'] % hp.beat_bytes
                or cmd['row_bytes'] % hp.beat_bytes
                or cmd['dram_stride'] % hp.beat_bytes
                or cmd['tmem_stride'] % hp.beat_bytes):
            return Status.BAD_ALIGN

        # Like the RTL, beats move before an error is observed: a bus
        # fault fails the response, not the data stream, and a range
        # error stops after the in-range prefix.
        store = cmd['opcode'] is Opcode.STORE_2D
        row_bytes = cmd['row_bytes']
        range_error = False
        for row in range(cmd['rows']):
            toff = cmd['tmem_addr'] + row * cmd['tmem_stride']
            if toff + row_bytes > hp.tmem_bytes:
                range_error = True
                break
            doff = cmd['dram_addr'] + row * cmd['dram_stride']
            if store:
                self.dram[doff:doff + row_bytes] = self.tmem[toff:toff +
                                                             row_bytes]
            else:
                self.tmem[toff:toff + row_bytes] = self.dram[doff:doff +
                                                             row_bytes]
        if cmd['force_error']:
            return Status.DMA_ERROR
        if range_error:
            return Status.BAD_RANGE
        return Status.OK

    def _vector(self, cmd):
        """Execute one VECTOR/REDUCE command against TMEM.

        Validation mirrors the RTL exactly: the opcode must name a
        sub-operation of its own class (checked by ``Core`` at issue
        there), then the unit checks shape (non-zero count, no output
        overlap with a read input -- B is unread by the reductions
        and the unary transcendentals), alignment, and range, in
        that precedence.
        """
        hp = self.hp
        bb = hp.beat_bytes
        opcode = cmd['opcode']
        raw_op = cmd['vec_op']

        # Mirrors Core's class decode: the opcode must name a
        # sub-operation of its own class; reserved encodings (and any
        # non-member value) match neither set.
        elem_ints = tuple(int(o) for o in VEC_ELEMENTWISE)
        rdc_ints = tuple(int(o) for o in VEC_REDUCTIONS)
        if opcode is Opcode.VECTOR:
            cls_ok = raw_op in elem_ints
        else:
            cls_ok = raw_op in rdc_ints
        if not cls_ok:
            return Status.BAD_SHAPE
        op = VecOp(raw_op)

        n = cmd['n']

        def pad(b):
            return -(-b // bb) * bb

        # Only operations that read the B region check it: the binary
        # elementwise ops and VEC_SCALE's scalar beat. The unary
        # transcendentals and the reductions ignore ``b_base``
        # entirely; VEC_QUANT instead writes its meta there.
        reads_b = op in VEC_READS_B
        quant = op is VecOp.VEC_QUANT

        a_bytes = pad(4 * n)
        b_bytes = bb if op is VecOp.VEC_SCALE else a_bytes
        o_bytes = bb if opcode is Opcode.REDUCE else a_bytes
        if quant:
            nsegs = -(-n // 32)
            o_bytes = pad(32 * nsegs)
            b_bytes = pad(8 * nsegs)

        def overlap(x_base, x_bytes, y_base, y_bytes):
            return (x_base < y_base + y_bytes and y_base < x_base + x_bytes)

        shape_bad = (n == 0) \
            or overlap(cmd['o_base'], o_bytes, cmd['a_base'], a_bytes) \
            or (reads_b and overlap(cmd['o_base'], o_bytes,
                                    cmd['b_base'], b_bytes)) \
            or (quant and (overlap(cmd['o_base'], o_bytes,
                                   cmd['b_base'], b_bytes)
                           or overlap(cmd['b_base'], b_bytes,
                                      cmd['a_base'], a_bytes)))
        align_bad = (cmd['a_base'] % bb or cmd['o_base'] % bb
                     or (reads_b and cmd['b_base'] % bb)
                     or (quant and cmd['b_base'] % bb))
        range_bad = (cmd['a_base'] + a_bytes > hp.tmem_bytes
                     or cmd['o_base'] + o_bytes > hp.tmem_bytes
                     or (reads_b and cmd['b_base'] + b_bytes > hp.tmem_bytes)
                     or (quant and cmd['b_base'] + b_bytes > hp.tmem_bytes))
        if shape_bad:
            return Status.BAD_SHAPE
        if align_bad:
            return Status.BAD_ALIGN
        if range_bad:
            return Status.BAD_RANGE

        def word(base, i):
            off = base + 4 * i
            return int.from_bytes(self.tmem[off:off + 4], 'little')

        if quant:
            qblob, mblob = vec_quant_regions(
                [word(cmd['a_base'], i) for i in range(n)], bb)
            self.tmem[cmd['o_base']:cmd['o_base'] + o_bytes] = qblob
            self.tmem[cmd['b_base']:cmd['b_base'] + b_bytes] = mblob
            return Status.OK

        if opcode is Opcode.VECTOR:
            a = [word(cmd['a_base'], i) for i in range(n)]
            b = [
                word(cmd['b_base'], 0 if op is VecOp.VEC_SCALE else i)
                for i in range(n)
            ] if reads_b else [0] * n
            out = [vec_word(op, a[i], b[i]) for i in range(n)]
            blob = b''.join(w.to_bytes(4, 'little') for w in out)
            self.tmem[cmd['o_base']:cmd['o_base'] + o_bytes] = \
                blob + bytes(o_bytes - len(blob))
        else:
            a = [word(cmd['a_base'], i) for i in range(n)]
            result = rdc_result(op, a)
            blob = result.to_bytes(4, 'little')
            self.tmem[cmd['o_base']:cmd['o_base'] + o_bytes] = \
                blob + bytes(o_bytes - len(blob))
        return Status.OK


# ---------------------------------------------------------------------------
# MATMUL numerical contract
# ---------------------------------------------------------------------------

SEG_LEN = 32
QK_SUPER = 256

BLOCK_BYTES = {
    Format.Q5_0: 22,
    Format.Q8_0: 34,
    Format.Q4_K: 144,
    Format.Q6_K: 210,
}

BLOCK_SEGS = {
    Format.Q5_0: 1,
    Format.Q8_0: 1,
    Format.Q4_K: QK_SUPER // SEG_LEN,
    Format.Q6_K: QK_SUPER // SEG_LEN,
}

# Largest segment partial each format can produce, from the plan's
# bounds proof: |w| * |a| * 32 products, then the sub-scale combine.
#   Q8_0: 32 * 128 * 128                      = 524,288
#   Q5_0: 32 *  16 * 128                      =  65,536
#   Q4_K: 63 * (32 * 15 * 128)                = 3,870,720
#   Q6_K: 128 * 2 * (16 * 32 * 128)           = 16,777,216
# All are <= 2^24, so INT32 -> FP32 conversion is exact and the sums
# never overflow a signed INT32 chain-end combine.
SEGMENT_BOUND = {
    Format.Q5_0: 32 * 16 * 128,
    Format.Q8_0: 32 * 128 * 128,
    Format.Q4_K: 63 * 32 * 15 * 128,
    Format.Q6_K: 128 * 2 * 16 * 32 * 128,
}


def f32(x):
    """Round to the nearest FP32 value (round-to-nearest-even)."""
    return struct.unpack('<f', struct.pack('<f', x))[0]


def f32_bits(x):
    return struct.unpack('<I', struct.pack('<f', x))[0]


def bits_to_f32(bits):
    return struct.unpack('<f', struct.pack('<I', bits))[0]


def fp16_to_f32(h):
    """Exact FP16 bit pattern -> Python float."""
    return struct.unpack('<e', struct.pack('<H', h))[0]


def f32_to_fp16_bits(x):
    return struct.unpack('<H', struct.pack('<e', f32(x)))[0]


def _sgn8(v):
    return v - 256 if v >= 128 else v


def _round_half_away(x):
    return math.floor(x + 0.5) if x >= 0 else math.ceil(x - 0.5)


def _pack_k4_scales(sc, mi):
    """Pack 8 unsigned 6-bit (sc, mi) pairs into Q4_K's 12 bytes.

    Inverse of the ``get_scale_min_k4`` decoder: pairs 0..3 take the
    low 6 bits of bytes j and j+4; pairs 4..7 scatter their high bits
    into the top 2 bits of bytes j-4, j, j+4 and the halves of j+4+4.
    """
    q = bytearray(12)
    for j in range(4):
        q[j] = (sc[j] & 63) | ((sc[j + 4] >> 4) << 6)
        q[j + 4] = (mi[j] & 63) | ((mi[j + 4] >> 4) << 6)
        q[j + 8] = (sc[j + 4] & 0xF) | ((mi[j + 4] & 0xF) << 4)
    return bytes(q)


def _unpack_k4_scales(q):
    sc = []
    mi = []
    for j in range(8):
        if j < 4:
            sc.append(q[j] & 63)
            mi.append(q[j + 4] & 63)
        else:
            sc.append((q[j + 4] & 0xF) | ((q[j - 4] >> 6) << 4))
            mi.append((q[j + 4] >> 4) | ((q[j] >> 6) << 4))
    return sc, mi


def pack_row(fmt, segs):
    """Pack per-segment logical fields into device-format row bytes.

    Each segment dict carries ``w`` (32 weights in the format's decoded
    integer range) plus its scale fields: ``d``/``dmin`` as FP16 bit
    patterns, Q4_K's ``mi`` and per-segment scale ``sc0``, Q6_K's two
    signed chain scales ``sc0``/``sc1``. K-quant blocks take 8 segments
    at a time sharing one ``d`` (and ``dmin``).
    """
    out = bytearray()
    if fmt is Format.Q5_0:
        for s in segs:
            qs = bytearray(16)
            qh = 0
            for i in range(SEG_LEN):
                code = s['w'][i] + 16
                assert 0 <= code <= 31
                if i < 16:
                    qs[i] |= code & 0xF
                else:
                    qs[i - 16] |= (code & 0xF) << 4
                qh |= ((code >> 4) & 1) << i
            out += s['d'].to_bytes(2, 'little')
            out += qh.to_bytes(4, 'little')
            out += bytes(qs)
    elif fmt is Format.Q8_0:
        for s in segs:
            out += s['d'].to_bytes(2, 'little')
            out += bytes(x & 0xFF for x in s['w'])
    elif fmt is Format.Q4_K:
        for b in range(len(segs) // 8):
            blk = segs[8 * b:8 * b + 8]
            out += blk[0]['d'].to_bytes(2, 'little')
            out += blk[0]['dmin'].to_bytes(2, 'little')
            out += _pack_k4_scales([s['sc0'] for s in blk],
                                   [s['mi'] for s in blk])
            qs = bytearray(128)
            for j in range(8):
                for l in range(SEG_LEN):
                    qs[32 * (j // 2) + l] |= (blk[j]['w'][l] & 0xF) \
                        << (4 if j % 2 else 0)
            out += bytes(qs)
    elif fmt is Format.Q6_K:
        for b in range(len(segs) // 8):
            blk = segs[8 * b:8 * b + 8]
            w256 = [x for s in blk for x in s['w']]
            ql = bytearray(128)
            qh = bytearray(64)
            for h in range(2):
                for l in range(32):
                    codes = [
                        w256[128 * h + off + l] + 32 for off in (0, 32, 64, 96)
                    ]
                    for c in codes:
                        assert 0 <= c <= 63
                    ql[64 * h + l] |= codes[0] & 0xF
                    ql[64 * h + l + 32] |= codes[1] & 0xF
                    ql[64 * h + l] |= (codes[2] & 0xF) << 4
                    ql[64 * h + l + 32] |= (codes[3] & 0xF) << 4
                    qh[32 * h + l] = (((codes[0] >> 4) & 3)
                                      | (((codes[1] >> 4) & 3) << 2)
                                      | (((codes[2] >> 4) & 3) << 4)
                                      | (((codes[3] >> 4) & 3) << 6))
            out += bytes(ql) + bytes(qh)
            out += bytes(s & 0xFF for s2 in blk
                         for s in (s2['sc0'], s2['sc1']))
            out += blk[0]['d'].to_bytes(2, 'little')
    else:
        raise ValueError(f'unsupported format {fmt}')
    return bytes(out)


def unpack_row(fmt, data):
    """Unpack device-format row bytes into per-segment logical fields.

    The mirror of ``pack_row``, written straight from the GGUF
    reference decoders: Q4_K's nibble pairs interleave two segments per
    byte, Q6_K splits every element across a ql nibble and a 2-bit qh
    plane, and the scale planes follow the layouts above.
    """
    BB = BLOCK_BYTES[fmt]
    assert len(data) % BB == 0
    segs = []
    for b in range(len(data) // BB):
        blk = data[b * BB:(b + 1) * BB]
        if fmt is Format.Q5_0:
            d = int.from_bytes(blk[0:2], 'little')
            qh = int.from_bytes(blk[2:6], 'little')
            qs = blk[6:22]
            w = []
            for i in range(SEG_LEN):
                nib = (qs[i] & 0xF) if i < 16 else (qs[i - 16] >> 4)
                w.append((nib | (((qh >> i) & 1) << 4)) - 16)
            segs.append(dict(w=w, d=d, dmin=0, mi=0, sc0=0, sc1=0))
        elif fmt is Format.Q8_0:
            d = int.from_bytes(blk[0:2], 'little')
            w = [_sgn8(v) for v in blk[2:34]]
            segs.append(dict(w=w, d=d, dmin=0, mi=0, sc0=0, sc1=0))
        elif fmt is Format.Q4_K:
            d = int.from_bytes(blk[0:2], 'little')
            dmin = int.from_bytes(blk[2:4], 'little')
            sc, mi = _unpack_k4_scales(blk[4:16])
            qs = blk[16:144]
            for j in range(8):
                w = []
                for l in range(SEG_LEN):
                    byte = qs[32 * (j // 2) + l]
                    w.append((byte >> 4) if j % 2 else (byte & 0xF))
                segs.append(
                    dict(w=w, d=d, dmin=dmin, mi=mi[j], sc0=sc[j], sc1=0))
        elif fmt is Format.Q6_K:
            ql = blk[0:128]
            qh = blk[128:192]
            sc8 = blk[192:208]
            d = int.from_bytes(blk[208:210], 'little')
            for j in range(8):
                h, q = divmod(j, 4)
                w = []
                for l in range(SEG_LEN):
                    base = 64 * h + l + (32 if q % 2 else 0)
                    lo = (ql[base] >> 4) if q >= 2 else (ql[base] & 0xF)
                    hb = (qh[32 * h + l] >> (2 * q)) & 3
                    w.append((lo | (hb << 4)) - 32)
                segs.append(
                    dict(w=w,
                         d=d,
                         dmin=0,
                         mi=0,
                         sc0=_sgn8(sc8[8 * h + 2 * q]),
                         sc1=_sgn8(sc8[8 * h + 2 * q + 1])))
        else:
            raise ValueError(f'unsupported format {fmt}')
    return segs


def seg_combine(fmt, wseg, quants):
    """Integer chain-end combine: the two 16-product chain partials and
    the format's sub-scale multiplication, exact in signed INT32."""
    P0 = sum(wseg['w'][i] * quants[i] for i in range(16))
    P1 = sum(wseg['w'][i] * quants[i] for i in range(16, SEG_LEN))
    if fmt in (Format.Q5_0, Format.Q8_0):
        S = P0 + P1
    elif fmt is Format.Q4_K:
        S = wseg['sc0'] * (P0 + P1)
    else:
        S = wseg['sc0'] * P0 + wseg['sc1'] * P1
    assert abs(S) <= SEGMENT_BOUND[fmt], (fmt, S)
    return S


def quantize_act_q8_0(x):
    """Q8_0-style activation block: FP16 scale over 32, INT8 quants."""
    amax = max(abs(v) for v in x)
    if amax == 0:
        quants = [0] * SEG_LEN
    else:
        d = f32(amax / 127.0)
        quants = [max(-128, min(127, _round_half_away(v / d))) for v in x]
        quants = [float(q) for q in quants]
    d16 = f32_to_fp16_bits(f32(amax / 127.0)) if amax else 0
    return dict(quants=[int(q) for q in quants],
                s_act=f32_bits(fp16_to_f32(d16)),
                sum_a=sum(int(q) for q in quants))


def quantize_act_q8_k(x):
    """Q8_K-style activation block: FP32 scale over 256, INT8 quants.

    The per-32 signed sum the Q4_K correction needs would come from the
    per-16 sums; this host-side builder simply sums the quants, which
    is the same integer.
    """
    amax = max(abs(v) for v in x)
    if amax == 0:
        quants = [0] * QK_SUPER
        d = 0.0
    else:
        d = f32(amax / 127.0)
        quants = [max(-128, min(127, _round_half_away(v / d))) for v in x]
    segs = []
    for j in range(QK_SUPER // SEG_LEN):
        q32 = [int(q) for q in quants[32 * j:32 * j + 32]]
        segs.append(dict(quants=q32, s_act=f32_bits(d), sum_a=sum(q32)))
    return segs


def build_act(fmt, vec):
    """Quantize a float vector into ACT segments for a weight format.

    Q5_0/Q8_0 weights pair with Q8_0-style activation blocks; K-quants
    pair with Q8_K-style blocks, whose per-256 scale replicates across
    the super-block's eight segments.
    """
    if fmt in (Format.Q4_K, Format.Q6_K):
        assert len(vec) % QK_SUPER == 0
        out = []
        for i in range(len(vec) // QK_SUPER):
            out += quantize_act_q8_k(vec[QK_SUPER * i:QK_SUPER * (i + 1)])
        return out
    assert len(vec) % SEG_LEN == 0
    return [
        quantize_act_q8_0(vec[32 * j:32 * j + 32])
        for j in range(len(vec) // SEG_LEN)
    ]


def row_bytes(fmt, k):
    nblocks = k // (QK_SUPER if fmt in (Format.Q4_K, Format.Q6_K) else SEG_LEN)
    return nblocks * BLOCK_BYTES[fmt]


def mm_segments(fmt, k, rows, weight_rows, act, n_outputs=2):
    """Reference chain-end records, in launch order.

    ``weight_rows`` is the list of ``rows`` packed device-format rows;
    ``act`` is ``build_act`` output (or any per-segment quants/scale/
    sum image). The returned records match the matrix unit's output
    stream field for field, including the row-pair order and the masked
    lane of a final odd row.
    """
    nsegs = k // SEG_LEN
    pairs = (rows + n_outputs - 1) // n_outputs
    seg_rows = [unpack_row(fmt, weight_rows[r]) for r in range(rows)]

    recs = []
    for pair in range(pairs):
        for j in range(nsegs):
            rec = dict(fmt=fmt,
                       seg_k=j,
                       row_last=(j == nsegs - 1),
                       s_act=act[j]['s_act'],
                       sum_a=act[j]['sum_a'],
                       row_mask=0)
            for p in range(n_outputs):
                row = n_outputs * pair + p
                if row >= rows:
                    continue
                wseg = seg_rows[row][j]
                rec[f'S{p}'] = seg_combine(fmt, wseg, act[j]['quants'])
                rec[f'row{p}'] = row
                rec[f'd{p}'] = wseg['d']
                rec[f'dmin{p}'] = wseg['dmin']
                rec[f'mi{p}'] = wseg['mi']
                rec['row_mask'] |= 1 << p
            recs.append(rec)
    return recs


def mm_row_results(records):
    """Ordered FP32 epilogue over the reference records, per output row.

    For segment k of a row:

    .. code-block:: text

        u_k = R(R(float(S_k) * d) * s_act)
        v_k = R(R(float(mi_k * sum_a_k) * dmin) * s_act)   # Q4_K only
        t_k = R(u_k - v_k) | u_k
        p[k mod 4] = R(p[k mod 4] + t_k)
        result = R(R(p[0] + p[2]) + R(p[1] + p[3]))

    No multiply-adds are fused; every rounding point is explicit, so a
    later epilogue module can match this bit for bit.
    """
    per_row = {}
    for rec in records:
        for p in range(rec['row_mask'].bit_length()):
            if not (rec['row_mask'] >> p) & 1:
                continue
            per_row.setdefault(rec[f'row{p}'], []).append((rec, p))

    results = {}
    for row, items in per_row.items():
        acc = [0.0, 0.0, 0.0, 0.0]
        for k, (rec, p) in enumerate(items):
            s_act = bits_to_f32(rec['s_act'])
            d = fp16_to_f32(rec[f'd{p}'])
            u = f32(f32(float(rec[f'S{p}']) * d) * s_act)
            if rec['fmt'] is Format.Q4_K:
                dmin = fp16_to_f32(rec[f'dmin{p}'])
                v = f32(
                    f32(float(rec[f'mi{p}'] * rec['sum_a']) * dmin) * s_act)
                t = f32(u - v)
            else:
                t = u
            acc[k % 4] = f32(acc[k % 4] + t)
        results[row] = f32(f32(acc[0] + acc[2]) + f32(acc[1] + acc[3]))
    return results


# ---------------------------------------------------------------------------
# VECTOR/REDUCE numerical contract
# ---------------------------------------------------------------------------
#
# Bit-exact FP32 primitives mirroring the ROOM FPU blocks the RTL
# instantiates. NaN operands and invalid operations produce the
# canonical NaN (FType.default_nan()); signed-zero behavior matches
# the blocks as measured: zero sums follow IEEE 754 (an exact
# cancellation under RNE yields +0) and zero products keep the
# product's sign. Add/subtract/multiply of finite operands round
# through binary64 without double-rounding hazards: a binary64 holds
# 53 significand bits and 53 >= 2*24 + 2 (Figueroa's bound), and a
# product of two binary32 values is exact in binary64.

FP32_NAN_BITS = 0x7fc00000


def _f32_pack(x):
    """Round-to-nearest-even binary64 -> FP32 bits, overflow to inf."""
    try:
        return struct.unpack('<I', struct.pack('<f', x))[0]
    except OverflowError:
        return 0xff800000 if x < 0 else 0x7f800000


def _fp32_is_nan(b):
    return (b & 0x7f800000) == 0x7f800000 and (b & 0x007fffff) != 0


def _fp32_is_inf(b):
    return (b & 0x7fffffff) == 0x7f800000


def _fp32_is_zero(b):
    return (b & 0x7fffffff) == 0


def _fp32_is_subnormal_bits(b):
    return (b & 0x7f800000) == 0 and (b & 0x007fffff) != 0


def fp32_add_bits(a, b):
    """R(a + b): RNE, canonical NaN on NaN or inf - inf."""
    if _fp32_is_nan(a) or _fp32_is_nan(b):
        return FP32_NAN_BITS
    if _fp32_is_inf(a) and _fp32_is_inf(b) \
            and ((a ^ b) & 0x80000000):
        return FP32_NAN_BITS
    if _fp32_is_inf(a):
        return a
    if _fp32_is_inf(b):
        return b
    return _f32_pack(bits_to_f32(a) + bits_to_f32(b))


def fp32_mul_bits(a, b):
    """R(a * b): RNE with the product's sign (IEEE multiply).

    The RTL rounds the exact FMA ``a * b + 0.0``; the +0.0 addend is
    fully shifted out of the alignment (it contributes no sticky), so
    a zero product keeps its product sign and nothing else changes.
    """
    if _fp32_is_nan(a) or _fp32_is_nan(b):
        return FP32_NAN_BITS
    if (_fp32_is_zero(a) and _fp32_is_inf(b)) \
            or (_fp32_is_inf(a) and _fp32_is_zero(b)):
        return FP32_NAN_BITS
    if _fp32_is_inf(a) or _fp32_is_inf(b):
        return ((a ^ b) & 0x80000000) | 0x7f800000
    return _f32_pack(bits_to_f32(a) * bits_to_f32(b))


def fp32_max_bits(a, b):
    """RISC-V fmax: the non-NaN operand wins, +0 beats -0."""
    if _fp32_is_nan(a):
        return FP32_NAN_BITS if _fp32_is_nan(b) else b
    if _fp32_is_nan(b):
        return a
    fa, fb = bits_to_f32(a), bits_to_f32(b)
    if fa == fb:
        # Equal values are bit-identical except +0/-0, where the
        # bitwise and selects the positive zero.
        return a & b
    return a if fa > fb else b


def fp32_neg_bits(b):
    return b ^ 0x80000000


# ---------------------------------------------------------------------------
# Unary transcendental contract (VEC_EXP2 / VEC_RECIP / VEC_RSQRT)
# ---------------------------------------------------------------------------
#
# Frozen per ``plans/2026-10-10-troom-vector-transcendentals.md``. Each
# op is a fixed sequence of the rounded primitives above, executed by
# the ROOM blocks the RTL instantiates (``FPURec`` seed for reciprocal
# and rsqrt; ``FPUFMA`` multiply and add/subtract for every refinement
# and polynomial step), so the RTL matches this model bit for bit.
# Accuracy is bounded in ulps, not correct rounding; the measured
# maxima over the tested domains (every exponent with dense mantissa
# samples, table-index boundaries, powers of two, and clamp
# neighbors) are 2 ulp for recip, 2 ulp for rsqrt, and 13 ulp for
# exp2. Sampling is not a proof of a global bound.
#
# Special cases are classified before any arithmetic, so exceptional
# inputs never enter a Newton refinement (where ``0 * inf`` could
# manufacture spurious NaNs). Subnormal policy: inputs and outputs
# are NOT flushed (the plain primitives preserve subnormals); the
# only intentional departures from the real function are the frozen
# clamps below, chosen so every intermediate and every sequenced
# result stays in the normal domain:
#
# * ``VEC_RECIP``: subnormal input -> signed infinity (|1/x| >
#   2^126; finite only for |x| > 2^-128), |x| >= 2^126 -> signed
#   zero (|1/x| <= 2^-126 would be subnormal or the smallest
#   normal);
# * ``VEC_RSQRT``: negative input -> canonical NaN, -0 -> -inf, +0
#   -> +inf, +inf -> +0 (IEEE sqrt-class policy, matching FPURec's
#   own special values); positive subnormal -> +inf (true value is
#   the finite normal 2^63.5..2^75, but the frozen sequence's
#   ``h = R(0.5*x)`` loses the leading bit for x < 2^-148);
# * ``VEC_EXP2``: x >= 128 -> +inf, x < -126 -> +0 (subnormal and
#   underflowing results flush), NaN -> canonical NaN, +/-inf ride
#   the clamps (+inf -> +inf, -inf -> +0).
#
# Seeds are bit-exact models of ``room.fpu.FPURec`` restricted to the
# positive normal inputs the sequencer feeds it (the reciprocal path
# additionally excludes exponent fields >= 253, where FPURec builds
# subnormal estimates the contract replaces with the clamp above).
# The tables are imported from the RTL definition rather than
# transcribed, so model and RTL cannot drift; the bit-exact unit
# tests drive the same inputs through the RTL block.

FP32_INF_BITS = 0x7f800000


def fp32_rec_seed_bits(a):
    """FPURec reciprocal estimate of a positive normal, bit for bit.

    Table indexed by the top 7 mantissa bits (the FPURec address is
    ``norm_mantissa[-7:]``, Amaranth's top-bit slice); estimate
    exponent field ``2*127 - 1 - e``; table entry in the top 7
    mantissa bits (``Cat(zeros, table)``).
    """
    e = (a >> 23) & 0xff
    assert 1 <= e <= 252
    idx = (a >> 16) & 0x7f
    return ((253 - e) << 23) | (FPURec._REC_TABLE[idx] << 16)


def fp32_rsqrt_seed_bits(x):
    """FPURec rsqrt estimate of a positive normal, bit for bit.

    Table indexed by the top 6 mantissa bits and the exponent LSB;
    estimate exponent field ``(3*127 - 1 - e) >> 1``.
    """
    e = (x >> 23) & 0xff
    man = x & 0x7fffff
    assert 1 <= e <= 254
    idx = ((man >> 17) & 0x3f) | ((e & 1) << 6)
    return (((3 * 127 - 1 - e) >> 1) << 23) \
        | (FPURec._RSQRT_TABLE[idx] << 16)


# exp2 Taylor coefficients Ck = fp32(ln(2)**k / k!), k = 0..7,
# derived from the binary64 math.log(2) (provenance: mathematical
# definition, matching the openTPU reference's degree-7 Taylor of
# 2**f over f in [0, 1)). The RTL (troom/vector.py) hardcodes these
# same bit patterns.
EXP2_COEFFS = tuple(
    f32_bits(math.log(2)**k / math.factorial(k)) for k in range(8))


def vec_recip_bits(x):
    """Roughly 1/x: FPURec seed plus two Newton iterations on |x|.

    .. code-block:: text

        a = |x|;  y = seed_rec(a)
        repeat twice:
            t = R(a * y);  t = R(2.0 - t);  y = R(y * t)
        result = y with the sign of x
    """
    if _fp32_is_nan(x):
        return FP32_NAN_BITS
    e = (x >> 23) & 0xff
    if e == 0:  # +-0 and subnormals -> signed infinity
        return (x & 0x80000000) | FP32_INF_BITS
    if e >= 253:  # +-inf and |x| >= 2^126 -> signed zero
        return x & 0x80000000
    a = x & 0x7fffffff
    y = fp32_rec_seed_bits(a)
    for _ in range(2):
        t = fp32_mul_bits(a, y)
        t = fp32_add_bits(0x40000000, fp32_neg_bits(t))
        y = fp32_mul_bits(y, t)
    return y | (x & 0x80000000)


def vec_rsqrt_bits(x):
    """Roughly 1/sqrt(x): FPURec seed plus two Newton iterations.

    .. code-block:: text

        y = seed_rsqrt(x);  h = R(0.5 * x)
        repeat twice:
            t = R(h * y);  t = R(t * y);  t = R(1.5 - t);  y = R(y * t)

    The multiplication order is fixed (never ``y*y`` first): for
    tiny positive x that would overflow even though the correction
    is finite.
    """
    if _fp32_is_nan(x):
        return FP32_NAN_BITS
    if x & 0x80000000:
        return 0xff800000 if _fp32_is_zero(x) else FP32_NAN_BITS
    if _fp32_is_zero(x) or _fp32_is_subnormal_bits(x):
        return FP32_INF_BITS
    if _fp32_is_inf(x):
        return 0
    y = fp32_rsqrt_seed_bits(x)
    h = fp32_mul_bits(0x3f000000, x)
    for _ in range(2):
        t = fp32_mul_bits(h, y)
        t = fp32_mul_bits(t, y)
        t = fp32_add_bits(0x3fc00000, fp32_neg_bits(t))
        y = fp32_mul_bits(y, t)
    return y


def _exp2_floor_i(x):
    """(floor(x), float32(floor(x))) for finite -126 <= x < 128.

    Mirrors the RTL's bounded bit helper: for |x| >= 1 the integer
    part is the 24-bit significand shifted right by ``150 - e``
    (exact for every FP32 with e <= 133, i.e. |x| < 128), with the
    negative side rounding down (magnitude plus any lost fraction
    bit); for |x| < 1 the floor is 0 or -1. float32(i) is exact for
    |i| <= 127.
    """
    sign = x >> 31
    e = (x >> 23) & 0xff
    man = x & 0x7fffff
    if e < 127:
        if sign and (e != 0 or man != 0):
            return -1, 0xbf800000
        return 0, 0x00000000
    sig = man | 0x800000
    shift = 150 - e
    mag = sig >> shift
    frac_nz = (sig & ((1 << shift) - 1)) != 0
    if sign:
        i = -(mag + (1 if frac_nz else 0))
    else:
        i = mag
    return i, f32_bits(float(i))


def _scale_pow2_bits(p, i):
    """Scale normal p by 2**i by adding i to the exponent field.

    Overflow (field >= 255) gives +inf; a nonpositive field gives
    +0 (the frozen flush for underflowing exp2 results; unreachable
    for in-domain inputs, where p >= 1.0 and i >= -126).
    """
    e = ((p >> 23) & 0xff) + i
    if e >= 255:
        return FP32_INF_BITS
    if e <= 0:
        return 0
    return (p & 0x807fffff) | (e << 23)


def vec_exp2_bits(x):
    """Roughly 2**x: bounded range reduction plus degree-7 Horner.

    .. code-block:: text

        i = floor(x);  f = R(x - float32(i))
        p = C7
        for k = 6 down to 0:
            p = R(R(p * f) + Ck)
        result = scale_by_power_of_two(p, i)

    Integer inputs are exact (f = +0 collapses the polynomial to
    C0 = 1.0 and the scaling is an exponent-field add).
    """
    if _fp32_is_nan(x):
        return FP32_NAN_BITS
    xf = bits_to_f32(x)
    if xf >= 128.0:  # includes +inf
        return FP32_INF_BITS
    if xf < -126.0:  # includes -inf
        return 0
    i, i2f = _exp2_floor_i(x)
    f = fp32_add_bits(x, fp32_neg_bits(i2f))
    p = EXP2_COEFFS[7]
    for k in range(6, -1, -1):
        p = fp32_add_bits(fp32_mul_bits(p, f), EXP2_COEFFS[k])
    return _scale_pow2_bits(p, i)


# ---------------------------------------------------------------------------
# Device-side activation quantization contract (VEC_QUANT)
# ---------------------------------------------------------------------------
#
# Frozen per the attention plan (the openTPU otpu_quant.sv reference,
# re-expressed over the TROOM arithmetic blocks): per 32-word segment,
#
#     a  = fold of RISC-V fmax over |x| from +0   (NaN never wins)
#     sc = R(a * fp32(1/127))                      (the dequant scale)
#     r  = recip(a)                                (the frozen
#                                                  recip sequence)
#     q_i = q8(R(R(x_i * r) * 127.0))              (RNE int8,
#                                                  saturating)
#
# The element product runs in two rounded steps through the unscaled
# r ~ 1/a: |R(x*r)| <= ~1 for every element, so no intermediate can
# overflow -- 127*recip(a) itself overflows to +inf for a near the
# smallest normal, which is why the scale is folded into the second
# step. Clamps, classified before any arithmetic like the
# transcendental table: a == 0 or subnormal (every |x| < 2^-126, or
# all NaN) zeroes the segment; a >= 2^126 (including +inf, which an
# NaN-free fold can produce) keeps sc = R(a/127) but forces r = +0,
# so the quants are zero. The scale pair is two independent rounded
# products of one recip estimate (2 ulp), so dequant error stays far
# below the INT8 grid; accuracy is ulp-bounded, not correctly rounded.
#
# The output is the activation image the QUANTIZE copy consumes:
# dense INT8 quants (little-endian words, byte i = element i) padded
# to a beat at ``r_base``, then per-segment meta (s_act word, int16
# quant sum, two pad bytes) padded to a beat at ``b_base``. A final
# partial segment zero-pads its buffer slots, so its pad bytes are
# q8(R(R(+0 * r) * 127)) = 0 and change neither amax nor the sum.

INV127_BITS = f32_bits(1.0 / 127.0)
F127_BITS = f32_bits(127.0)


def q8_rne_bits(p):
    """INT8 quantization of an FP32 word: RNE on the integer grid with
    saturation, the bounded bit helper the RTL instantiates.

    |p| < 0.5 rounds to 0; |p| >= 128 saturates (+127 / -128); exact
    halves round to even. NaN and infinity ride the saturation by
    their sign bit.
    """
    e = (p >> 23) & 0xff
    man = (p & 0x7fffff) | 0x800000
    if e < 126:
        return 0
    if e >= 134:
        return 0x80 if (p >> 31) else 0x7f
    sh = 150 - e
    ip = man >> sh
    g = (man >> (sh - 1)) & 1
    st = (man & ((1 << (sh - 1)) - 1)) != 0
    q = ip + (1 if g and (st or ip & 1) else 0)
    if q > 127:
        q = 127
    return (256 - q) & 0xff if (p >> 31) else q


def vec_quant_segments(words):
    """Per-segment activation image of VEC_QUANT.

    Returns ``build_act``-shaped dicts (quants, s_act, sum_a) in
    segment order, mirroring the RTL bit for bit; quants are signed
    like ``quantize_act_q8_0``'s.
    """
    segs = []
    for s0 in range(0, len(words), 32):
        seg = list(words[s0:s0 + 32])
        seg += [0] * (32 - len(seg))  # the pad slots the RTL zeroes
        a = 0
        for w in seg:
            a = fp32_max_bits(a, w & 0x7fffffff)
        e = (a >> 23) & 0xff
        if e == 0:
            sc, r = 0, 0
        elif e >= 253:
            sc, r = fp32_mul_bits(a, INV127_BITS), 0
        else:
            y = fp32_rec_seed_bits(a)
            for _ in range(2):
                t = fp32_mul_bits(a, y)
                t = fp32_add_bits(0x40000000, fp32_neg_bits(t))
                y = fp32_mul_bits(y, t)
            sc = fp32_mul_bits(a, INV127_BITS)
            r = y
        qb = [
            q8_rne_bits(fp32_mul_bits(fp32_mul_bits(w, r), F127_BITS))
            for w in seg
        ]
        quants = [q - 256 if q > 127 else q for q in qb]
        segs.append(dict(quants=quants, s_act=sc, sum_a=sum(quants)))
    return segs


def vec_quant_regions(words, beat_bytes):
    """The two padded TMEM regions VEC_QUANT writes: quants, then meta."""
    segs = vec_quant_segments(words)

    def pad(b):
        return b + bytes((-len(b)) % beat_bytes)

    qblob = pad(bytes(q & 0xff for s in segs for q in s['quants']))
    mblob = pad(
        b''.join(s['s_act'].to_bytes(4, 'little') +
                 (s['sum_a'] & 0xffff).to_bytes(2, 'little') + b'\x00\x00'
                 for s in segs))
    return qblob, mblob


def vec_word(op, a, b):
    """One elementwise result word from FP32 operand bit patterns."""
    if op is VecOp.VEC_ADD:
        return fp32_add_bits(a, b)
    if op is VecOp.VEC_SUB:
        return fp32_add_bits(a, fp32_neg_bits(b))
    if op is VecOp.VEC_EXP2:
        return vec_exp2_bits(a)
    if op is VecOp.VEC_RECIP:
        return vec_recip_bits(a)
    if op is VecOp.VEC_RSQRT:
        return vec_rsqrt_bits(a)
    assert op in (VecOp.VEC_MUL, VecOp.VEC_SCALE), op
    return fp32_mul_bits(a, b)


def rdc_result(op, words):
    """One reduction result from the region's FP32 word patterns.

    The sum reductions pin the ``isum_4`` placement and fold order the
    RTL implements (identical to the matmul epilogue); the maximum
    seeds from the first element, which makes its serial fold exactly
    the model's fold.
    """
    if op is VecOp.RDC_MAX:
        m = words[0]
        for w in words[1:]:
            m = fp32_max_bits(m, w)
        return m
    assert op in (VecOp.RDC_SUM, VecOp.RDC_SUMSQ), op
    p = [0, 0, 0, 0]
    for i, w in enumerate(words):
        x = fp32_mul_bits(w, w) if op is VecOp.RDC_SUMSQ else w
        p[i % 4] = fp32_add_bits(p[i % 4], x)
    return fp32_add_bits(fp32_add_bits(p[0], p[2]), fp32_add_bits(p[1], p[3]))
