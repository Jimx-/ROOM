"""Frozen host command interface ABI for the troom FPGA wrapper.

Single definition point for the register map, the format-0 command
descriptor serialization, the completion record layout, the status and
wrapper-error encodings, and the launch identifier protocol. The launch
controller (``troom.ctrl``), the command fetcher (``troom.fetch``), the
completion writer (``troom.completion``), and pysim tests take their
values from this module. The FPGA host driver must mirror these
constants; see
``plans/2026-10-09-troom-fpga-host-interface.md``.

Register offsets are bytes relative to the TROOM MMIO base. Registers
use aligned 32-bit accesses; 64-bit registers occupy two aligned words
and must be read low word first (ascending) for a coherent snapshot.
The checked AXI-Lite bridge merges byte strobes for staging words.
``MAX_CYCLES`` commits on a high-word write; its low word is staged
independently. ``CONTROL`` actions live in the top byte, so writes
excluding lane 3 cannot trigger an action. Unaligned, unmapped, and
wrong-direction accesses return SLVERR without side effects.
"""

from enum import IntEnum

from troom.consts import Format, Opcode, VecOp

# Identification and interface version.
TROOM_ID = 0x54524F4D
TROOM_ABI_VERSION = 1

# ---------------------------------------------------------------------
# Register offsets (bytes from the TROOM MMIO base)
# ---------------------------------------------------------------------

REG_ID = 0x00  # r    identification
REG_ABI_VERSION = 0x04  # r    register/descriptor ABI version
REG_CAPS = 0x08  # r    interface capabilities
REG_STATUS = 0x0c  # r    busy/done/error/quiescent/rejected
REG_CONTROL = 0x10  # w    write-one actions (top byte)
REG_SUBMIT_ID = 0x14  # rw   host-provided launch identifier
REG_CMD_BASE_LO = 0x18  # rw   command-buffer device address, low word
REG_CMD_BASE_HI = 0x1c  # rw   command-buffer device address, high word
REG_CMD_COUNT = 0x20  # rw   number of command descriptors
REG_CMD_FORMAT = 0x24  # rw   descriptor format selector
REG_COMP_BASE_LO = 0x28  # rw   completion-buffer device address, low word
REG_COMP_BASE_HI = 0x2c  # rw   completion-buffer device address, high word
REG_COMP_CAPACITY = 0x30  # rw   completion capacity in records
REG_COMPLETED_COUNT = 0x34  # r   records committed through AXI B responses
REG_FINISHED_ID = 0x38  # r    most recently terminated launch
REG_ERROR_CODE = 0x3c  # r    first run error, sticky
REG_ERROR_INDEX = 0x40  # r    command index, or ERROR_INDEX_NONE
REG_ACCEPTED_ID = 0x44  # r    most recently accepted launch
REG_MAX_CYCLES = 0x48  # rw64 run watchdog; zero disables it

# Capabilities and counters. 0x50..0x7f are reserved for future
# standard registers; capability space starts at 0x80.
REG_MEM_BASE = 0x80  # r64  device memory window base address
REG_MEM_SIZE = 0x88  # r64  device memory window size in bytes
REG_BEAT_BYTES = 0x90  # r    memory bus beat size in bytes
REG_TMEM_BYTES = 0x94  # r    tensor memory capacity in bytes
REG_CMD_ALIGN = 0x98  # r    required command-buffer base alignment
REG_COMP_ALIGN = 0x9c  # r    required completion-buffer base alignment
REG_CYCLES = 0xa0  # r64  free-running cycle counter

# Engine observation-counter window, one snapshot register per
# launch. Each holds the compute core's counter value at launch
# termination (a fault-terminated launch freezes the values from its
# first fault cycle), so the host can read them any time before the
# following launch terminates; the per-run core re-arm resets the live
# counters in between. All are read-only 64-bit, values zero-extended
# from 48-bit (32-bit for commands done) counters. Units:
# - commands done: descriptors retired by the core sequencer;
# - run cycles: core cycles from engine re-arm to termination,
#   including idle and event waits;
# - dram bytes: beats accepted on the core's tensor-DMA master in both
#   directions, counted at whole-beat granularity, so row tails round
#   up; command fetch and completion-record traffic are not included;
# - dma stalls: cycles an active DMA descriptor transferred no beat
#   (bus latency and backpressure alike);
# - tmem conflicts: tensor-memory reads delayed by a same-bank write;
# - useful macs: multiply-accumulates retired by the array;
# - array starve: array cycles lost waiting for operand supply.
# Advertised by CAPS_ENGINE_COUNTERS; images without the bit must
# report zeros here, not fabricate values.
REG_CMDS_DONE = 0xa8  # r64  snapshot: commands retired
REG_RUN_CYCLES = 0xb0  # r64  snapshot: cycles under this launch
REG_DRAM_BYTES = 0xb8  # r64  snapshot: tensor-DMA beats in bytes
REG_DMA_STALLS = 0xc0  # r64  snapshot: DMA no-progress cycles
REG_TMEM_CONFLICTS = 0xc8  # r64  snapshot: TMEM read-vs-write delays
REG_USEFUL_MACS = 0xd0  # r64  snapshot: retired multiply-accumulates
REG_ARRAY_STARVE = 0xd8  # r64  snapshot: array operand-starve cycles

# M3b cycle-breakdown window, one snapshot register per launch with the
# same freeze/reset semantics as the window above. Units:
# - mm credit stalls: matrix launch-join cycles lost with every operand
#   ready but no output-queue credit, i.e. sustained downstream
#   (epilogue) backpressure on the segment-record stream;
# - mm run cycles: cycles the matrix unit spent launching slots (valid
#   and starved alike, one matmul at a time);
# - mm drain cycles: cycles waiting for the last launched records to be
#   dequeued after the final slot;
# - epi records: segment records accepted and fully issued by the
#   epilogue;
# - epi active cycles: cycles the epilogue is running a matmul's drain,
#   from start to the final result beat committing to TMEM;
# - epi stalls: cycles a record waited at the epilogue sink without
#   acceptance while the drain was running;
# - epi cast/mul/add ops: issues on the shared conversion, multiply,
#   and add/subtract blocks (utilization numerators against the active
#   window; adds count accumulations, Q4_K corrections, and row folds
#   alike). All zero-extended from 48-bit counters. Advertised by
#   CAPS_MM_EPI_COUNTERS; images without the bit must report zeros
#   here, not fabricate values.
REG_MM_CREDIT_STALLS = 0xe0  # r64  snapshot: matrix output-credit stalls
REG_MM_RUN_CYCLES = 0xe8  # r64  snapshot: matrix RUN-state cycles
REG_MM_DRAIN_CYCLES = 0xf0  # r64  snapshot: matrix DRAIN-state cycles
REG_EPI_RECORDS = 0xf8  # r64  snapshot: epilogue records accepted
REG_EPI_ACTIVE_CYCLES = 0x100  # r64 snapshot: epilogue active cycles
REG_EPI_STALLS = 0x108  # r64  snapshot: epilogue sink stalls
REG_EPI_CAST_OPS = 0x110  # r64  snapshot: epilogue conversion issues
REG_EPI_MUL_OPS = 0x118  # r64  snapshot: epilogue multiply issues
REG_EPI_ADD_OPS = 0x120  # r64  snapshot: epilogue add issues

# ---------------------------------------------------------------------
# STATUS register bits
# ---------------------------------------------------------------------

STATUS_BUSY = 0  # a launch is active
STATUS_DONE = 1  # terminal: batch completed successfully
STATUS_ERROR = 2  # terminal or latched run error
STATUS_QUIESCENT = 3  # wrapper has no outstanding work or traffic
STATUS_REJECTED = 4  # the most recent START was rejected

# ---------------------------------------------------------------------
# CONTROL register bits
# ---------------------------------------------------------------------

# Write-one actions. The register commits when the CSR machinery
# completes it -- that is, when a write covers the register's last
# byte lane (offset + 3) -- and the action bits live in that byte, so
# a committed write always carries freshly written action bits and a
# partial write excluding lane 3 cannot trigger an action.
CONTROL_START = 24  # launch with the staged configuration
CONTROL_CLEAR = 25  # clear sticky terminal status (busy: ignored)

# ---------------------------------------------------------------------
# CAPS register bits
# ---------------------------------------------------------------------

CAPS_FMT0 = 0  # format-0 command descriptors supported
CAPS_WATCHDOG = 1  # MAX_CYCLES hardware watchdog implemented
CAPS_ENGINE_COUNTERS = 2  # observation-counter window implemented
CAPS_MM_EPI_COUNTERS = 3  # matrix/epilogue cycle-breakdown window


class WrapperError(IntEnum):
    """Wrapper-level error codes reported through ``ERROR_CODE``.

    These are distinct from the per-command :class:`troom.consts.Status`
    codes carried in completion records. Command failures surface as
    ``CMD_FAILED`` with the failing command's index in ``ERROR_INDEX``;
    fetch/decode/write failures name the descriptor where possible, and
    non-command errors keep ``ERROR_INDEX`` at ``ERROR_INDEX_NONE``.
    """

    NONE = 0
    BAD_FORMAT = 1  # unsupported CMD_FORMAT
    BAD_COUNT = 2  # zero command count
    BAD_ALIGN = 3  # buffer base misaligned
    BAD_RANGE = 4  # buffer range outside device memory
    BAD_CAPACITY = 5  # completion capacity below command count
    ADDR_OVERFLOW = 6  # nonzero high address word
    BUF_OVERLAP = 7  # command and completion buffers overlap
    FETCH_ERROR = 8  # command fetch read returned an error
    DECODE_ERROR = 9  # descriptor decode or END placement fault
    COMP_ERROR = 10  # completion write B response error
    CMD_FAILED = 11  # a command completed with error status
    WATCHDOG = 12  # MAX_CYCLES watchdog expired


ERROR_INDEX_NONE = 0xffffffff

# ---------------------------------------------------------------------
# Format-0 command descriptors
# ---------------------------------------------------------------------

CMD_FORMAT_0 = 0

# Public command field order; word ``i`` of a descriptor carries
# ``CMD_FIELDS[i]`` serialized as a little-endian 32-bit integer.
CMD_FIELDS = (
    'opcode',
    'evt',
    'seq',
    'fmt',
    'vec_op',
    'k',
    'w_base',
    'w_stride',
    'r_base',
    'a_base',
    'm_base',
    'dram_addr',
    'tmem_addr',
    'row_bytes',
    'rows',
    'dram_stride',
    'tmem_stride',
)

CMD_DESC_WORDS = len(CMD_FIELDS)
CMD_DESC_BYTES = 4 * CMD_DESC_WORDS

# Completion records: two little-endian words, {seq, status}.
COMP_RECORD_WORDS = 2
COMP_RECORD_BYTES = 4 * COMP_RECORD_WORDS


def _enum_width(enum):
    """Unsigned bit width of an ``IntEnum`` used as an Amaranth shape."""
    return max(int(v) for v in enum).bit_length()


def command_field_shapes(hp):
    """Format-0 field bit widths for a validated parameter set.

    Returns ``[(name, bits), ...]`` in word order. Host words must be
    representable in these widths; the fetcher rejects a descriptor
    whose words carry bits above them.
    """
    return [
        ('opcode', _enum_width(Opcode)),
        ('evt', hp.evt_bits),
        ('seq', hp.seq_bits),
        ('fmt', _enum_width(Format)),
        ('vec_op', _enum_width(VecOp)),
        ('k', hp.lg_act_k + 1),
        ('w_base', hp.tmem_addr_width),
        ('w_stride', hp.tmem_addr_width),
        ('r_base', hp.tmem_addr_width),
        ('a_base', hp.tmem_addr_width),
        ('m_base', hp.tmem_addr_width),
        ('dram_addr', hp.addr_width),
        ('tmem_addr', hp.tmem_addr_width),
        ('row_bytes', hp.row_bytes_bits),
        ('rows', hp.rows_bits),
        ('dram_stride', hp.addr_width),
        ('tmem_stride', hp.tmem_addr_width),
    ]


def pack_command(cmd, shapes=None):
    """Serialize one command into ``CMD_DESC_WORDS`` 32-bit integers.

    ``cmd`` maps field names to integers. With ``shapes`` (from
    :func:`command_field_shapes`) each value is checked to be
    representable in its field, mirroring the fetcher's RTL check.
    """
    if shapes is None:
        shapes = [(name, 32) for name in CMD_FIELDS]
    words = []
    for name, bits in shapes:
        if name not in cmd:
            raise KeyError(f'command field {name!r} missing')
        value = int(cmd[name])
        if value >> bits:
            raise ValueError(
                f'command field {name}={value:#x} does not fit {bits} bits')
        words.append(value)
    return words


def unpack_command(words, shapes=None):
    """Deserialize ``CMD_DESC_WORDS`` integers into a field dictionary."""
    if len(words) != CMD_DESC_WORDS:
        raise ValueError(f'expected {CMD_DESC_WORDS} words, got {len(words)}')
    if shapes is None:
        shapes = [(name, 32) for name in CMD_FIELDS]
    return {name: word for (name, _), word in zip(shapes, words)}


def pack_buffer(cmds, shapes=None):
    """Serialize a whole command buffer as little-endian bytes."""
    data = bytearray()
    for cmd in cmds:
        for word in pack_command(cmd, shapes):
            data += word.to_bytes(4, 'little')
    return bytes(data)


def fetch_length(count, beat_bytes):
    """Bytes the fetcher reads for ``count`` descriptors on ``beat_bytes``.

    Whole beats only: the tail padding past ``68 * count`` bytes must
    stay inside the host's allocation.
    """
    total = CMD_DESC_BYTES * count
    return (total + beat_bytes - 1) // beat_bytes * beat_bytes


def pack_completion(seq, status):
    """Serialize one completion record as little-endian bytes."""
    return int(seq).to_bytes(4, 'little') + int(status).to_bytes(4, 'little')
