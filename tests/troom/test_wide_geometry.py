"""Qwen geometry gates against independently packed frozen quantizer bytes."""
import random

import pytest
from amaranth import Module
from amaranth.sim import Simulator

from troom.abi import (CAPS_ACT_K4096, CAPS_VBUILD_HD128, CAPS_VBUILD_VAPP,
                       REG_CAPS, VBUILD_ROWS_HD128)
from troom.consts import Status
from troom.matrix import ActRAM
from troom.types import HasTRoomParams
from tests.troom.model import vbuild_quant_row
from tests.troom.test_vbuild import PARAMS, run_build
from tests.troom.test_stream import check_records, params_for, run_stream
from tests.troom.test_ctrl import CtrlTop, cread, run_ctrl
from troom.consts import VecOp
from tests.troom.test_vector import run_unit, words_to_bytes
from tests.troom.test_vector_unary import un_word
from tests.troom.model import f32_bits


def pack_rows(rows):
    blocks = len(rows[0]) // 34
    pay = bytearray(len(rows) * blocks * 32)
    meta = bytearray(len(rows) * blocks * 2)
    for r, raw in enumerate(rows):
        for b in range(blocks):
            q = ((r // 4) * blocks * 4 + b * 4 + r % 4) * 32
            d = ((r // 4) * blocks * 4 + b * 4 + r % 4) * 2
            pay[q:q + 32] = raw[b * 34 + 2:b * 34 + 34]
            meta[d:d + 2] = raw[b * 34:b * 34 + 2]
    return bytes(pay), bytes(meta)


def test_wide_geometry_capabilities():
    top = CtrlTop(
        dict(PARAMS,
             act_max_k=4096,
             vbuild_max_head_dim=128,
             vbuild_native_append=False))

    def run():
        caps = yield from cread(top.axil, REG_CAPS)
        assert caps & (1 << CAPS_ACT_K4096)
        assert caps & (1 << CAPS_VBUILD_HD128)
        assert not caps & (1 << CAPS_VBUILD_VAPP)

    run_ctrl(top, run)


def test_silu_3072():
    params = dict(PARAMS,
                  data_width=512,
                  act_max_k=4096,
                  vec_unary_ctx=32,
                  vec_result_q=32,
                  vec_add_units=4,
                  vec_mul_units=4)
    words = [f32_bits((i % 127 - 63) / 8) for i in range(3072)]
    writes, result = run_unit(params,
                              dict(op=int(VecOp.VEC_SILU),
                                   n=3072,
                                   o_base=0x5000),
                              words_to_bytes(words),
                              deadline=300000)
    assert result['status'] == Status.OK
    actual = b''.join(word.to_bytes(64, 'little') for addr, word in writes)
    expected = words_to_bytes([un_word(VecOp.VEC_SILU, x) for x in words])
    assert actual == expected


def test_legacy_k_on_wide_builder():
    rng = random.Random(64)
    data = [rng.randrange(0x3000, 0x4400) for _ in range(16 * 64)]
    ref_pay, ref_meta = pack_rows(
        [vbuild_quant_row(data[r * 64:(r + 1) * 64]) for r in range(16)])
    params = dict(PARAMS,
                  data_width=512,
                  act_max_k=4096,
                  vbuild_max_head_dim=128,
                  vbuild_native_append=False)
    rb, mb = 0x5000, 0x7800
    beats, result = run_build(params, data, 16, r_base=rb, m_base=mb, rows=16)
    assert result['status'] == Status.OK
    actual = {addr: word.to_bytes(64, 'little') for addr, word in beats}
    assert b''.join(actual[rb + off]
                    for off in range(0, len(ref_pay), 64)) == ref_pay
    assert actual[mb] == ref_meta


@pytest.mark.parametrize('krow,live', [(True, 13), (True, 0), (False, 33)])
@pytest.mark.parametrize('special', [False, True])
def test_wide_kv_bytes(krow, live, special):
    rng = random.Random(128 + live)
    slots = 16 if krow else 64
    data = [(rng.choice(
        [0, 0x8000, 0x7e01, 0x7c00, 0xfc00,
         rng.randrange(0x3000, 0x4600)]) if special else
             (rng.randrange(0x3000, 0x4600)
              | rng.choice([0, 0x8000]))) if slot < live else 0
            for slot in range(slots) for d in range(128)]
    if krow:
        rows = [
            vbuild_quant_row(data[r * 128:r * 128 + 64]) +
            vbuild_quant_row(data[r * 128 + 64:(r + 1) * 128])
            for r in range(16)
        ]
    else:
        rows = [
            vbuild_quant_row([data[j * 128 + d] for j in range(64)])
            for d in range(128)
        ]
    ref_pay, ref_meta = pack_rows(rows)
    params = dict(PARAMS,
                  data_width=512,
                  act_max_k=4096,
                  vbuild_max_head_dim=128,
                  vbuild_native_append=False)
    rb, mb = 0x5000, 0x7800
    beats, result = run_build(params,
                              data,
                              live,
                              r_base=rb,
                              m_base=mb,
                              rows=VBUILD_ROWS_HD128 | (16 if krow else 0),
                              deadline=800000)
    assert result['status'] == Status.OK
    pay, meta = bytearray(len(ref_pay)), bytearray(len(ref_meta))
    seen = set()
    for addr, word in beats:
        assert addr not in seen
        seen.add(addr)
        blob = word.to_bytes(64, 'little')
        if rb <= addr < rb + len(pay):
            pay[addr - rb:addr - rb + 64] = blob
        else:
            assert mb <= addr < mb + len(meta)
            meta[addr - mb:addr - mb + 64] = blob
    assert bytes(pay) == ref_pay
    assert bytes(meta) == ref_meta
    assert len(seen) == (len(pay) + len(meta)) // 64


def test_act_4096_upper_segments():
    """The two ACT banks independently retain segments beyond the old limit."""
    params = dict(PARAMS, act_max_k=4096)
    hp = HasTRoomParams(params)
    assert hp.act_segs == 128 and hp.lg_act_segs == 7
    dut = ActRAM(params)
    m = Module()
    m.submodules.dut = dut
    sim = Simulator(m)
    sim.add_clock(1e-6)

    # Exercise the full read address, not just successful elaboration.
    def run():
        for seg in (0, 63, 64, 95, 127):
            yield dut.wr.valid.eq(1)
            yield dut.wr.bits.seg.eq(seg)
            yield dut.wr.bits.quants.eq(seg + 1)
            yield dut.wr.bits.s_act.eq(0x3f800000 + seg)
            yield dut.wr.bits.sum_a.eq(seg)
            yield
        yield dut.wr.valid.eq(0)
        yield dut.commit.eq(1)
        yield
        yield dut.commit.eq(0)
        for seg in (0, 63, 64, 95, 127):
            yield dut.rd_seg.eq(seg)
            yield
            yield
            assert (yield dut.rd_quants) == seg + 1
            assert (yield dut.rd_s_act) == 0x3f800000 + seg

    sim.add_sync_process(run)
    sim.run()


@pytest.mark.parametrize('k', [3072, 4096])
def test_wide_stream_segments(k):
    params = params_for(data_width=512, act_max_k=4096)
    out = run_stream(k,
                     8,
                     params=params,
                     stall_sink=True,
                     req_prob=0.7,
                     deadline=100000)
    assert out['results'][-1]['status'] == Status.OK
    check_records(out['records'], out['expected'])


@pytest.mark.parametrize('r', [0, 7, 8, 15])
def test_wide_k_partial_metadata(r):
    """A rebased pair patches its original cells on either metadata beat."""
    rng = random.Random(900 + r)
    live = 2 if r & 1 else 1
    pair = [
        rng.randrange(0x3000, 0x4400) if slot < live else 0
        for slot in range(2) for d in range(128)
    ]
    raw = [
        vbuild_quant_row(pair[slot * 128:slot * 128 + 64]) +
        vbuild_quant_row(pair[slot * 128 + 64:(slot + 1) * 128])
        for slot in range(2)
    ]
    rb, mb = 0x5000, 0x7800
    prior_pay = bytes([0xa5]) * 2048
    prior_meta = bytes([0x5a]) * 128
    params = dict(PARAMS,
                  data_width=512,
                  act_max_k=4096,
                  vbuild_max_head_dim=128,
                  vbuild_native_append=False)
    beats, result = run_build(params,
                              pair,
                              r + 1,
                              r_base=rb,
                              m_base=mb,
                              rows=VBUILD_ROWS_HD128 | 17,
                              retain_images=[(rb, prior_pay),
                                             (mb, prior_meta)])
    assert result['status'] == Status.OK
    expected_pay, expected_meta = bytearray(prior_pay), bytearray(prior_meta)
    for slot in range(2):
        for b in range(4):
            q = (b * 4 + slot) * 32
            expected_pay[q:q + 32] = raw[slot][b * 34 + 2:b * 34 + 34]
            if slot < live:
                lane = (r & ~1) + slot
                d = ((lane // 4) * 16 + b * 4 + lane % 4) * 2
                expected_meta[d:d + 2] = raw[slot][b * 34:b * 34 + 2]
    pay, meta = bytearray(prior_pay), bytearray(prior_meta)
    for addr, word in beats:
        blob = word.to_bytes(64, 'little')
        if rb <= addr < rb + len(pay):
            pay[addr - rb:addr - rb + 64] = blob
        else:
            assert addr == mb + ((r & ~1) // 8) * 64
            meta[addr - mb:addr - mb + 64] = blob
    assert pay == expected_pay
    assert meta == expected_meta


@pytest.mark.parametrize('block,live', [(0, 20), (1, 33), (1, 64)])
def test_wide_v_partial_metadata(block, live):
    rng = random.Random(2000 + live)
    data = [
        rng.randrange(0x3000, 0x4400) if slot < live else 0
        for slot in range(64) for d in range(128)
    ]
    rows = [
        vbuild_quant_row([data[slot * 128 + d] for slot in range(64)])
        for d in range(128)
    ]
    ref_pay, ref_meta = pack_rows(rows)
    prior_pay, prior_meta = bytes([0xa5]) * 8192, bytes([0x5a]) * 512
    rb, mb = 0x5000, 0x7800
    params = dict(PARAMS,
                  data_width=512,
                  act_max_k=4096,
                  vbuild_max_head_dim=128,
                  vbuild_native_append=False)
    beats, result = run_build(params,
                              data,
                              live,
                              r_base=rb,
                              m_base=mb,
                              rows=VBUILD_ROWS_HD128 | (block + 1),
                              retain_images=[(rb, prior_pay),
                                             (mb, prior_meta)])
    assert result['status'] == Status.OK
    expected_pay, expected_meta = bytearray(prior_pay), bytearray(prior_meta)
    for row in range(128):
        q = ((row // 4) * 8 + block * 4 + row % 4) * 32
        d = ((row // 4) * 8 + block * 4 + row % 4) * 2
        expected_pay[q:q + 32] = ref_pay[q:q + 32]
        expected_meta[d:d + 2] = ref_meta[d:d + 2]
    pay, meta = bytearray(prior_pay), bytearray(prior_meta)
    for addr, word in beats:
        blob = word.to_bytes(64, 'little')
        if rb <= addr < rb + len(pay):
            pay[addr - rb:addr - rb + 64] = blob
        else:
            assert mb <= addr < mb + len(meta)
            meta[addr - mb:addr - mb + 64] = blob
    assert pay == expected_pay
    assert meta == expected_meta
