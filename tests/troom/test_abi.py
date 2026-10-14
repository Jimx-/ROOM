"""Tests for the frozen host command interface ABI in ``troom.abi``."""

import pytest

from troom.abi import (
    CMD_DESC_BYTES,
    CMD_DESC_WORDS,
    CMD_FIELDS,
    COMP_RECORD_BYTES,
    ERROR_INDEX_NONE,
    TROOM_ABI_VERSION,
    TROOM_ID,
    WrapperError,
    command_field_shapes,
    fetch_length,
    pack_buffer,
    pack_command,
    pack_completion,
    unpack_command,
)
from troom.core import Command
from troom.types import HasTRoomParams

PARAMS = dict(
    addr_width=32,
    data_width=64,
    id_width=4,
    seg_len=32,
    n_outputs=2,
    n_tokens=1,
    tmem_bytes=16 * 1024,
    n_tmem_banks=4,
    cmd_queue_depth=4,
    n_events=8,
    seq_bits=8,
)


def shapes():
    return command_field_shapes(HasTRoomParams(PARAMS))


def sample_command():
    return dict(
        opcode=3,  # MATMUL
        evt=5,
        seq=0x42,
        fmt=1,  # Q8_0
        vec_op=2,
        k=64,
        w_base=0x100,
        w_stride=0x40,
        r_base=0x800,
        a_base=0x900,
        m_base=0xa00,
        dram_addr=0x10000,
        tmem_addr=0x400,
        row_bytes=64,
        rows=4,
        dram_stride=128,
        tmem_stride=64,
    )


def test_descriptor_geometry_is_frozen():
    assert CMD_DESC_WORDS == 17
    assert CMD_DESC_BYTES == 68
    assert len(CMD_FIELDS) == 17
    assert COMP_RECORD_BYTES == 8
    assert TROOM_ID == 0x54524F4D
    assert TROOM_ABI_VERSION == 1
    assert ERROR_INDEX_NONE == 0xffffffff


def test_pack_unpack_roundtrip():
    cmd = sample_command()
    words = pack_command(cmd, shapes())
    assert len(words) == CMD_DESC_WORDS
    assert unpack_command(words, shapes()) == cmd


def test_pack_buffer_is_little_endian_word_stream():
    cmd = sample_command()
    data = pack_buffer([cmd], shapes())
    assert len(data) == CMD_DESC_BYTES
    for i, word in enumerate(pack_command(cmd, shapes())):
        assert data[4 * i:4 * i + 4] == word.to_bytes(4, 'little')


def test_pack_command_rejects_unrepresentable_fields():
    with pytest.raises(ValueError):
        pack_command(dict(sample_command(), seq=1 << 8), shapes())
    with pytest.raises(ValueError):
        pack_command(dict(sample_command(), opcode=1 << 8), shapes())
    with pytest.raises(KeyError):
        pack_command({'opcode': 0}, shapes())


def test_field_shapes_match_command_record():
    cmd = Command(PARAMS)
    for name, bits in shapes():
        assert len(getattr(cmd, name)) == bits, name


def test_fetch_length_pads_to_whole_beats():
    assert fetch_length(1, 8) == 72  # 68 rounded to 9 beats
    assert fetch_length(3, 8) == 208  # 204 rounded to 26 beats
    assert fetch_length(1, 16) == 80
    assert fetch_length(2, 4) == 136  # already beat aligned


def test_pack_completion_layout():
    assert pack_completion(7, 3) == b'\x07\x00\x00\x00\x03\x00\x00\x00'


def test_error_code_encoding_is_frozen():
    assert int(WrapperError.NONE) == 0
    assert int(WrapperError.WATCHDOG) == 12
    values = [int(e) for e in WrapperError]
    assert len(set(values)) == len(values)  # dense, no aliases
    assert values == list(range(len(values)))
    # The sentinel index is distinguishable from any command index in a
    # 32-bit command count.
    assert ERROR_INDEX_NONE > 0xffffffff - 1
