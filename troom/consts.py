"""Constant definitions for the troom tensor accelerator."""

from enum import IntEnum


class Opcode(IntEnum):
    """Descriptor opcodes with stable host ABI numbers.

    SMATMUL carries SG1 payload/meta bases in dram_addr/dram_stride;
    SQUANTIZE loads the streaming activation RAM. MATMUL and QUANTIZE
    are retired encodings and complete with BAD_OPCODE.
    """
    LOAD_2D = 0
    STORE_2D = 1
    QUANTIZE = 2
    MATMUL = 3
    VECTOR = 4
    REDUCE = 5
    WAIT = 6
    SIGNAL = 7
    END = 8
    VBUILD = 9
    SMATMUL = 10
    SQUANTIZE = 11


DATAPATH_OPCODES = (
    Opcode.LOAD_2D,
    Opcode.STORE_2D,
    Opcode.SQUANTIZE,
    Opcode.SMATMUL,
    Opcode.VECTOR,
    Opcode.REDUCE,
    Opcode.VBUILD,
)

DMA_OPCODES = (
    Opcode.LOAD_2D,
    Opcode.STORE_2D,
)


class Format(IntEnum):
    """Weight formats handled by the matrix unit.

    The layout of each format's packed blocks follows the GGUF
    definitions in llama.cpp's ``ggml-common.h``; see ``troom.unpack``
    for the byte-level decode.
    """

    Q5_0 = 0
    Q8_0 = 1
    Q4_K = 2
    Q6_K = 3


MM_FORMATS = (
    Format.Q5_0,
    Format.Q8_0,
    Format.Q4_K,
    Format.Q6_K,
)


class VecOp(IntEnum):
    """Sub-operations selected by the ``VECTOR``/``REDUCE`` opcodes.

    ``VECTOR`` takes the elementwise ops over FP32 words in TMEM (the
    ``b`` operand of ``VEC_SCALE`` is the single scalar at ``b_base``;
    the unary transcendentals never read ``b``); ``REDUCE`` takes the
    whole-region reductions, whose FP32 result lands in the low word
    of one zero-padded beat at ``o_base``.

    The transcendentals (see
    ``plans/2026-10-10-troom-vector-transcendentals.md``) are fixed
    sequences of rounded FP32 mul/add/sub over an ``FPURec`` seed
    (reciprocal and rsqrt) or a degree-7 Taylor Horner evaluation
    (exp2); the sequence, constants, special-case table, and accuracy
    measurements are frozen in ``tests/troom/model.py``, and the RTL
    must match it bit for bit.

    ``VEC_QUANT`` quantizes an FP32 A region into the activation
    image the ``QUANTIZE`` copy consumes: packed INT8 quants at
    ``r_base`` and per-segment ``s_act``/sum meta at ``b_base`` (an
    output for this op; B is never read). The sequence, scale pair,
    and clamps are frozen in ``tests/troom/model.py`` the same way.

    ``VEC_F16`` narrows the FP32 A region to packed FP16 halfwords
    at ``o_base`` (two elements per 32-bit word, zero-padded final
    partial beat; B is never read). The conversion is the frozen
    ``Fp32To16`` sequence (troom.convert), bit for bit the model's
    ``fp32_to_fp16_rne_bits`` -- RNE for every bit pattern including
    the FP16 subnormal range, NaNs canonicalized to a quiet NaN with
    the sign kept -- which ggml's ``ggml_compute_fp32_to_fp16``
    matches bit for bit, so a device-converted KV pool row is
    byte-identical to the host conversion (E1's native append).

    ``VEC_PSWAP`` copies the FP32 A region to ``o_base`` with
    adjacent element pairs exchanged: ``o[2j] = a[2j+1]``,
    ``o[2j+1] = a[2j]`` (n must be even; zero-padded final partial
    beat; B is never read). Pure data movement -- no FPU, exact bit
    patterns, signs and NaNs included. E3-c's device RoPE composes
    it with VEC_MUL/VEC_ADD to express the interleaved (NORMAL) pair
    rotation; the DMA engine's whole-beat rows cannot deinterleave
    sub-beat strides, so the pairing has to come from the vector
    unit.

    ``VEC_SILU`` fuses the backend's SwiGLU/SILU sigmoid chain into
    one unary op: ``o[i] = R(a[i] * sigmoid(a[i]))`` where
    ``sigmoid(x) = recip(exp2(x * -log2(e)) + 1)``, with the
    ``-log2(e)`` scalar and the ``+1`` embedded as constants (the
    standalone chain uploaded them as TMEM beats). Every rounding
    point is the composed sequence of the frozen VEC_EXP2/VEC_RECIP
    contracts -- ``m0 = R(a*-log2 e)``; exp2's classification, range
    reduction and Horner on ``m0``; ``w3 = R(w2 + 1)``; recip's
    classification of ``w3`` and its seed/Newton sequence with the
    sign of ``w3``; ``R(a * w4)`` -- so the op is bit-identical to
    the six-command chain by construction (the S0 sizing record:
    results/troom-silu-sizing-20261030). B is never read.

    ``VEC_SMEXP`` fuses the softmax body the FA chain emits between
    the mask add and the reciprocal (P2b-ii base slice,
    plans/2026-11-02-troom-pipelined-batch-decode.md): one op over
    the masked-score region at ``a_base`` produces both softmax
    products -- ``e[i] = VEC_EXP2(R(a[i] - m))`` packed at
    ``o_base`` (the standard elementwise image) and the denominator
    ``sum`` in the first word of one zero-padded beat at ``b_base``
    (an output for this op; B is never read). The maximum uses the
    ``RDC_MAX`` fold semantics (RISC-V fmax, order-free: NaNs dropped,
    +0 beating -0, canonical NaN when nothing survives); the subtract
    is VEC_SUB's single rounded operation (the chain's
    SCALE-broadcast of ``m`` is exact and disappears); the exponential
    is the frozen VEC_EXP2 sequence on that operand; the sum keeps
    RDC_SUM's exact ``isum_4`` placement and final fold over the
    result words in element order. Every rounding point is the
    composed five-op chain (RDC_MAX/VEC_SCALE/VEC_SUB/VEC_EXP2/
    RDC_SUM), unchanged. The op reads ``a`` twice (a max fold pass,
    then the streaming exp2 pass); the descriptor's three regions
    must be pairwise disjoint like VEC_QUANT's.
    """

    VEC_ADD = 0
    VEC_SUB = 1
    VEC_MUL = 2
    VEC_SCALE = 3
    RDC_SUM = 4
    RDC_MAX = 5
    RDC_SUMSQ = 6
    VEC_EXP2 = 7
    VEC_RECIP = 8
    VEC_RSQRT = 9
    VEC_QUANT = 10
    VEC_F16 = 11
    VEC_PSWAP = 12
    VEC_SILU = 13
    VEC_SMEXP = 14


VEC_ELEMENTWISE = (
    VecOp.VEC_ADD,
    VecOp.VEC_SUB,
    VecOp.VEC_MUL,
    VecOp.VEC_SCALE,
    VecOp.VEC_EXP2,
    VecOp.VEC_RECIP,
    VecOp.VEC_RSQRT,
    VecOp.VEC_QUANT,
    VecOp.VEC_F16,
    VecOp.VEC_PSWAP,
    VecOp.VEC_SILU,
    VecOp.VEC_SMEXP,
)

VEC_REDUCTIONS = (
    VecOp.RDC_SUM,
    VecOp.RDC_MAX,
    VecOp.RDC_SUMSQ,
)

# Unary transcendentals: read only the A region; the descriptor's
# ``b_base`` is ignored entirely (no read, alignment, range, or
# overlap check), as with the reductions.
VEC_UNARY = (
    VecOp.VEC_EXP2,
    VecOp.VEC_RECIP,
    VecOp.VEC_RSQRT,
    VecOp.VEC_SILU,
)

# Operations that read the B region: the binary elementwise ops and
# ``VEC_SCALE``'s scalar beat. Kept as an explicit predicate set
# (rather than ordinal ranges) so validation, dispatch, and the
# model agree as the op set grows.
VEC_READS_B = (
    VecOp.VEC_ADD,
    VecOp.VEC_SUB,
    VecOp.VEC_MUL,
    VecOp.VEC_SCALE,
)


class Status(IntEnum):
    """Completion record status codes."""

    OK = 0
    UNIMPLEMENTED = 1
    BAD_OPCODE = 2
    BAD_ALIGN = 3
    BAD_RANGE = 4
    DMA_ERROR = 5
    BAD_SHAPE = 6


class Source(IntEnum):
    """Completion sources for the engine's command tracker.

    A command either resolves at issue (events, reserved opcodes,
    errors) or is completed by one of the engines, each of which
    finishes its commands in acceptance order.
    """

    ISSUE = 0
    DMA = 1
    MATMUL = 2
    ACT = 3
    VEC = 4
    VBUILD = 5
    STREAM = 6
