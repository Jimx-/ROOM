"""Constant definitions for the troom tensor accelerator."""

from enum import IntEnum


class Opcode(IntEnum):
    """Descriptor opcodes from the troom command set."""

    LOAD_2D = 0
    STORE_2D = 1
    QUANTIZE = 2
    MATMUL = 3
    VECTOR = 4
    REDUCE = 5
    WAIT = 6
    SIGNAL = 7
    END = 8


DATAPATH_OPCODES = (
    Opcode.LOAD_2D,
    Opcode.STORE_2D,
    Opcode.QUANTIZE,
    Opcode.MATMUL,
    Opcode.VECTOR,
    Opcode.REDUCE,
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


VEC_ELEMENTWISE = (
    VecOp.VEC_ADD,
    VecOp.VEC_SUB,
    VecOp.VEC_MUL,
    VecOp.VEC_SCALE,
    VecOp.VEC_EXP2,
    VecOp.VEC_RECIP,
    VecOp.VEC_RSQRT,
    VecOp.VEC_QUANT,
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
