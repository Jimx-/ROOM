"""Packed GGUF weight decode for the troom matrix unit.

Weights stay packed in device format v1: GGUF blocks byte-packed within
beat-padded TMEM rows. This module decodes one packed block into the
matrix unit's logical order -- reduction segments of 32 elements -- so
the systolic chains multiply against the activation segment in K order,
not the packed physical order.

Block layouts follow llama.cpp's ``ggml-common.h`` (verified against the
local checkout's ``ggml-quants.c`` reference decoders):

* Q5_0 (22 B): ``d`` fp16, ``qh`` uint32, ``qs`` 16 nibble bytes.
  Byte ``i``'s low nibble is element ``i`` and its high nibble is
  element ``i + 16``; element ``i``'s fifth bit is qh bit ``i``, exactly
  as in ``dequantize_row_q5_0`` (the reference reads the upper half's
  bit as ``(qh >> (i - 4)) & 0x10``, which selects qh bit ``i``).
* Q8_0 (34 B): ``d`` fp16 and 32 signed bytes in logical order.
* Q4_K (144 B): ``d``/``dmin`` fp16, 12 scale bytes, 128 nibble bytes.
  Each 32-byte qs chunk covers a 64-element group: low nibbles are one
  32-element segment, high nibbles the next -- the physical order is
  not the segment order. Scales decode through the 6-bit
  ``get_scale_min_k4`` packing into one unsigned (sc, mi) pair per
  32 elements. Weights are the nonnegative nibble ``q`` in 0..15; the
  representation is ``d * sc * q - dmin * mi``, so no centering happens
  here (the plan's Q4_K algebra applies the minimum term as an explicit
  correction instead).
* Q6_K (210 B): ``ql`` 128 low-nibble bytes, ``qh`` 64 two-bit bytes,
  16 signed per-16 scales, ``d`` fp16. Element decode follows the
  ``dequantize_row_q6_K`` pattern: within each 128-element half, byte
  ``ql[l]`` holds elements ``l`` and ``l + 64`` (low/high nibble) and
  ``ql[l+32]`` holds elements ``l + 32`` and ``l + 96``; ``qh[l]``
  carries the two high bits for each of the four. Weights are the
  6-bit code minus 32.

All decode helpers are pure functions returning Amaranth values, so the
feeder muxes them by runtime format and segment index.
"""

from amaranth import *

from troom.consts import Format

# Reduction segment handled per slot (Q5_0/Q8_0 block size and
# K-quant sub-block size).
SEG_LEN = 32

# K-quant super-block size.
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

MAX_BLOCK_BYTES = max(BLOCK_BYTES.values())

# Valid (unpadded) row bytes for a reduction length k.
ROW_BYTES = {
    Format.Q5_0: lambda k: (k // SEG_LEN) * BLOCK_BYTES[Format.Q5_0],
    Format.Q8_0: lambda k: (k // SEG_LEN) * BLOCK_BYTES[Format.Q8_0],
    Format.Q4_K: lambda k: (k // QK_SUPER) * BLOCK_BYTES[Format.Q4_K],
    Format.Q6_K: lambda k: (k // QK_SUPER) * BLOCK_BYTES[Format.Q6_K],
}


def row_bytes(fmt, k):
    """Packed GGUF bytes a row of ``k`` elements occupies."""
    return ROW_BYTES[fmt](k)


def block_segs(fmt):
    """Segments (activation-stationary slots) per packed block."""
    return BLOCK_SEGS[fmt]


def _w8(v):
    """Normalize one weight to its 8-bit two's-complement pattern.

    Decode expressions come out at their natural width (a Q4_K nibble
    is 4 bits, a centered Q5_0 code 6 bits); the feeder packs a
    segment's weights into one ``seg_len * 8`` word, so every element
    is sign- or zero-extended to exactly 8 bits first.
    """
    v = Value.cast(v)
    n = len(v)
    if not v.shape().signed:
        return Cat(v, Const(0, 8 - n))
    u = v.as_unsigned()
    return Cat(u, u[n - 1:n].replicate(8 - n))


def _k4_scales(buf):
    """Decode Q4_K's 12 scale bytes into 8 unsigned (sc, mi) pairs.

    Follows ``get_scale_min_k4``: pairs 0..3 take the low 6 bits of
    bytes j and j+4; pairs 4..7 gather their high bits from the top 2
    bits of neighboring bytes.
    """
    sc = []
    mi = []
    for j in range(8):
        if j < 4:
            sc.append(buf[j][:6])
            mi.append(buf[j + 4][:6])
        else:
            sc.append(Cat(buf[j + 4][:4], buf[j - 4][6:8]))
            mi.append(Cat(buf[j + 4][4:8], buf[j][6:8]))
    return sc, mi


def decode_segment(fmt, buf, j):
    """Decode segment ``j`` (a Python int) of one packed block.

    ``buf`` indexes the block's bytes as 8-bit values in GGUF order.
    Returns ``(w, meta)`` where ``w`` is the segment's 32 weights as
    signed values in logical K order and ``meta`` maps scale names to
    values: ``d`` and ``dmin`` are raw FP16 bit patterns, ``mi`` is
    Q4_K's unsigned 6-bit minimum scale, and ``sc0``/``sc1`` are the
    two chains' signed sub-scales (Q4_K repeats its single unsigned
    scale in ``sc0``). Unused fields are zero.
    """
    if fmt is Format.Q5_0:
        assert j == 0
        d = Cat(buf[0], buf[1])
        qh = Cat(buf[2], buf[3], buf[4], buf[5])
        w = []
        for i in range(SEG_LEN):
            # Byte i's low nibble is element i; its high nibble is
            # element i + 16. The fifth bit of element i is qh bit i
            # (for i >= 16 the reference shifts it down from bit
            # i - 4 after combining with the nibble).
            if i < 16:
                nib = buf[6 + i][:4]
            else:
                nib = buf[6 + (i - 16)][4:8]
            bit = qh[i:i + 1]
            w.append(Cat(nib, bit).as_unsigned() - 16)
        return [_w8(x) for x in w], dict(d=d,
                                         dmin=Const(0, 16),
                                         mi=Const(0, 6),
                                         sc0=Const(0, 8),
                                         sc1=Const(0, 8))

    if fmt is Format.Q8_0:
        assert j == 0
        d = Cat(buf[0], buf[1])
        w = [buf[2 + i].as_signed() for i in range(SEG_LEN)]
        return [_w8(x) for x in w], dict(d=d,
                                         dmin=Const(0, 16),
                                         mi=Const(0, 6),
                                         sc0=Const(0, 8),
                                         sc1=Const(0, 8))

    if fmt is Format.Q4_K:
        g, hi = j // 2, j % 2
        d = Cat(buf[0], buf[1])
        dmin = Cat(buf[2], buf[3])
        sc, mi = _k4_scales(buf[4:16])
        w = []
        for l in range(SEG_LEN):
            byte = buf[16 + 32 * g + l]
            nib = byte[4:8] if hi else byte[:4]
            w.append(nib.as_unsigned())
        sc4 = Cat(sc[j], Const(0, 1)).as_signed()
        return [_w8(x) for x in w], dict(d=d,
                                         dmin=dmin,
                                         mi=mi[j],
                                         sc0=sc4,
                                         sc1=Const(0, 8))

    if fmt is Format.Q6_K:
        h, q = j // 4, j % 4
        d = Cat(buf[208], buf[209])
        w = []
        for l in range(SEG_LEN):
            ql_idx = 64 * h + l + (32 if q % 2 else 0)
            qlb = buf[ql_idx]
            nib = qlb[4:8] if q >= 2 else qlb[:4]
            qhb = buf[128 + 32 * h + l]
            hi2 = qhb[2 * q:2 * q + 2]
            w.append(Cat(nib, hi2).as_unsigned() - 32)
        sc0 = buf[192 + 8 * h + 2 * q].as_signed()
        sc1 = buf[192 + 8 * h + 2 * q + 1].as_signed()
        return [_w8(x) for x in w], dict(d=d,
                                         dmin=Const(0, 16),
                                         mi=Const(0, 6),
                                         sc0=sc0,
                                         sc1=sc1)

    raise ValueError(f'unsupported format {fmt}')
