import pytest
from amaranth.sim import Simulator

from roomsoc.peripheral.net.crc import (CrcCalculate, CrcExtract, CrcInsert,
                                       _crc_table)


def _crc_ref(data, width, polynomial, initial_value, final_xor):
    crc = initial_value
    for byte in data:
        crc ^= byte
        for _ in range(8):
            crc = ((crc >> 1) ^ polynomial
                   if crc & 1 else crc >> 1)
    return crc ^ final_xor


def _beats(payload, bytes_per_beat, user=0):
    beats = []
    for offset in range(0, len(payload), bytes_per_beat):
        chunk = payload[offset:offset + bytes_per_beat]
        beats.append({
            "data": sum(byte << (8 * i) for i, byte in enumerate(chunk)),
            "keep": (1 << len(chunk)) - 1,
            "last": offset + len(chunk) == len(payload),
            "user": user,
        })
    return beats


def _append_kept_bytes(buffer, data, keep, byte_count):
    for lane in range(byte_count):
        if keep & (1 << lane):
            buffer.append((data >> (8 * lane)) & 0xff)


def test_crc_table_matches_bitwise_step_for_every_byte():
    for width, polynomial in ((16, 0xa001), (32, 0xedb88320)):
        table = _crc_table(width, polynomial)
        assert len(table) == 256
        for byte, actual in enumerate(table):
            expected = byte
            for _ in range(8):
                expected = ((expected >> 1) ^ polynomial
                            if expected & 1 else expected >> 1)
            assert actual == expected


@pytest.mark.parametrize("data_width,width,polynomial,initial_value,final_xor", [
    (64, 32, 0xedb88320, 0xffffffff, 0xffffffff),
    (24, 16, 0xa001, 0x0000, 0x0000),
])
def test_crc_calculate_generic_parameters(data_width, width, polynomial,
                                          initial_value, final_xor):
    payload = b"123456789"
    bytes_per_beat = data_width // 8
    expected = _crc_ref(payload, width, polynomial, initial_value, final_xor)
    dut = CrcCalculate(data_width=data_width,
                       crc_width=width,
                       polynomial=polynomial,
                       initial_value=initial_value,
                       final_xor=final_xor)

    def process():
        yield dut.data_out.ready.eq(1)
        yield dut.crc.ready.eq(0)

        for offset in range(0, len(payload), bytes_per_beat):
            beat = payload[offset:offset + bytes_per_beat]
            value = sum(byte << (8 * i) for i, byte in enumerate(beat))
            yield dut.data_in.bits.data.eq(value)
            yield dut.data_in.bits.keep.eq((1 << len(beat)) - 1)
            yield dut.data_in.bits.last.eq(offset + len(beat) == len(payload))
            yield dut.data_in.valid.eq(1)

            while True:
                yield
                if (yield dut.data_in.ready):
                    break

        yield dut.data_in.valid.eq(0)

        while not (yield dut.crc.valid):
            yield
        assert (yield dut.crc.bits) == expected

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(process)
    sim.run()


def test_crc_calculate_packet_boundaries_keep_and_backpressure():
    packets = [bytes(range(length)) for length in range(1, 18)]
    expected_beats = [
        beat
        for packet_id, packet in enumerate(packets)
        for beat in _beats(packet, 8, user=packet_id)
    ]
    expected_crcs = [
        _crc_ref(packet, 32, 0xedb88320, 0xffffffff, 0xffffffff)
        for packet in packets
    ]
    dut = CrcCalculate(data_width=64, user_width=8)
    output_beats = []
    output_crcs = []

    def tx_process():
        for beat in expected_beats:
            yield dut.data_in.bits.data.eq(beat["data"])
            yield dut.data_in.bits.keep.eq(beat["keep"])
            yield dut.data_in.bits.last.eq(beat["last"])
            yield dut.data_in.bits.user.eq(beat["user"])
            yield dut.data_in.valid.eq(1)
            yield
            while not (yield dut.data_in.ready):
                yield
        yield dut.data_in.valid.eq(0)

    def rx_process():
        for cycle in range(300):
            yield dut.data_out.ready.eq(cycle % 4 != 0)
            if ((yield dut.data_out.valid)
                    and (yield dut.data_out.ready)):
                output_beats.append({
                    "data": (yield dut.data_out.bits.data),
                    "keep": (yield dut.data_out.bits.keep),
                    "last": bool((yield dut.data_out.bits.last)),
                    "user": (yield dut.data_out.bits.user),
                })
            yield

    def crc_process():
        for cycle in range(300):
            yield dut.crc.ready.eq(cycle % 5 not in (0, 1))
            if (yield dut.crc.valid) and (yield dut.crc.ready):
                output_crcs.append((yield dut.crc.bits))
            yield

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(tx_process)
    sim.add_sync_process(rx_process)
    sim.add_sync_process(crc_process)
    sim.run()

    assert output_beats == expected_beats
    assert output_crcs == expected_crcs


def test_crc_calculate_skips_sparse_keep_lanes():
    data = bytes(range(8))
    keep = 0b10101101
    selected = bytes(byte for lane, byte in enumerate(data)
                     if keep & (1 << lane))
    expected = _crc_ref(selected, 32, 0xedb88320, 0xffffffff, 0xffffffff)
    dut = CrcCalculate(data_width=64)

    def process():
        yield dut.data_out.ready.eq(1)
        yield dut.crc.ready.eq(0)
        yield dut.data_in.bits.data.eq(int.from_bytes(data, "little"))
        yield dut.data_in.bits.keep.eq(keep)
        yield dut.data_in.bits.last.eq(1)
        yield dut.data_in.valid.eq(1)
        yield
        while not (yield dut.data_in.ready):
            yield
        yield dut.data_in.valid.eq(0)
        while not (yield dut.crc.valid):
            yield
        assert (yield dut.crc.bits) == expected

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(process)
    sim.run()


@pytest.mark.parametrize("payload", [b"abcd", b"abcdefgh"])
def test_crc_insert_appends_crc_in_place_or_as_new_beat(payload):
    dut = CrcInsert(data_width=64, user_width=8)
    input_beats = _beats(payload, 8, user=0x5a)
    output = bytearray()
    output_last = []
    output_users = []

    def tx_process():
        for beat in input_beats:
            yield dut.data_in.bits.data.eq(beat["data"])
            yield dut.data_in.bits.keep.eq(beat["keep"])
            yield dut.data_in.bits.last.eq(beat["last"])
            yield dut.data_in.bits.user.eq(beat["user"])
            yield dut.data_in.valid.eq(1)
            yield
            while not (yield dut.data_in.ready):
                yield
        yield dut.data_in.valid.eq(0)

    def rx_process():
        yield dut.data_out.ready.eq(1)
        for _ in range(30):
            if (yield dut.data_out.valid):
                _append_kept_bytes(output,
                                   (yield dut.data_out.bits.data),
                                   (yield dut.data_out.bits.keep), 8)
                output_last.append(bool((yield dut.data_out.bits.last)))
                output_users.append((yield dut.data_out.bits.user))
            yield

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(tx_process)
    sim.add_sync_process(rx_process)
    sim.run()

    expected_crc = _crc_ref(payload, 32, 0xedb88320, 0xffffffff, 0xffffffff)
    assert output == payload + expected_crc.to_bytes(4, "little")
    assert output_last[-1] is True
    assert not any(output_last[:-1])
    assert output_users[0] == 0x5a


def test_crc_extract_removes_crc_and_reports_it():
    payload = b"abcd"
    expected_crc = _crc_ref(payload, 32, 0xedb88320, 0xffffffff, 0xffffffff)
    frame = payload + expected_crc.to_bytes(4, "little")
    dut = CrcExtract(data_width=64)
    output = bytearray()
    extracted = []

    def tx_process():
        beat = _beats(frame, 8)[0]
        yield dut.data_in.bits.data.eq(beat["data"])
        yield dut.data_in.bits.keep.eq(beat["keep"])
        yield dut.data_in.bits.last.eq(1)
        yield dut.data_in.valid.eq(1)
        yield
        while not (yield dut.data_in.ready):
            yield
        yield dut.data_in.valid.eq(0)

    def rx_process():
        yield dut.data_out.ready.eq(1)
        yield dut.crc.ready.eq(1)
        for _ in range(20):
            if (yield dut.data_out.valid):
                _append_kept_bytes(output,
                                   (yield dut.data_out.bits.data),
                                   (yield dut.data_out.bits.keep), 8)
                assert (yield dut.data_out.bits.last)
            if (yield dut.crc.valid):
                extracted.append((yield dut.crc.bits))
            yield

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(tx_process)
    sim.add_sync_process(rx_process)
    sim.run()

    assert output == payload
    assert extracted == [expected_crc]
