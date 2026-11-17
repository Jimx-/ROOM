"""G4: finalized MUL -> quantizer -> ACT, preserving every rounding point."""
import random

import pytest

from troom.consts import Format, Opcode, Status, VecOp
from tests.troom.model import f32_bits, fp32_mul_bits
from tests.troom.test_full_path import PARAMS
from tests.troom.test_vector import SPECIALS, words_to_bytes
from tests.troom.test_vector_quant_act import model_records, run_q_act

B_ADDR = 0x10000
PARAMS_G4 = dict(PARAMS, tmem_bytes=262144, act_max_k=4096)


def fused(a, b, *, params=None, hold=0, desc=None, descs=None):
    command = dict(op=int(VecOp.VEC_MUL_QUANT_ACT),
                   n=len(a),
                   b_base=B_ADDR,
                   o_base=0xffffffff)
    command.update(desc or {})
    records, writes, results, held = run_q_act(params or PARAMS_G4,
                                               descs or [command],
                                               words_to_bytes(a),
                                               b_bytes=words_to_bytes(b),
                                               b_addr=B_ADDR,
                                               hold_act=hold,
                                               deadline=150000)
    return records, writes, results if isinstance(results,
                                                  list) else [results], held


@pytest.mark.parametrize('n', [32, 64, 96, 1024, 3072, 4096])
def test_finalized_product_identity(n):
    rng = random.Random(406 + n)
    a = [f32_bits(rng.uniform(-8, 8)) for _ in range(n)]
    b = [f32_bits(rng.uniform(-2, 2)) for _ in range(n)]
    records, writes, results, _ = fused(a, b)
    assert records == model_records(
        [fp32_mul_bits(x, y) for x, y in zip(a, b)])
    assert not writes
    assert results[0]['status'] == Status.OK
    assert results[0]['elements'] == n


@pytest.mark.parametrize('pipeline', [False, True])
def test_specials_and_backpressure(pipeline):
    # Cross zeros, subnormals, normal values, infinities and NaNs. Include
    # overflow/underflow products: quantization must see the rounded product.
    a = (list(SPECIALS) * 20)[:128]
    b = (list(reversed(SPECIALS)) * 20)[:128]
    records, writes, results, held = fused(a,
                                           b,
                                           params=dict(
                                               PARAMS_G4,
                                               vec_quant_pipe=pipeline),
                                           hold=80)
    assert records == model_records(
        [fp32_mul_bits(x, y) for x, y in zip(a, b)])
    assert not writes
    assert results[0]['status'] == Status.OK
    assert len(held) >= 80 and len(set(held)) == 1


@pytest.mark.parametrize('desc,status', [
    (dict(b_base=B_ADDR + 1), Status.BAD_ALIGN),
    (dict(b_base=262144 - 64), Status.BAD_RANGE),
    (dict(n=33), Status.BAD_SHAPE),
    (dict(n=4128), Status.BAD_SHAPE),
])
def test_rejected_descriptors(desc, status):
    records, writes, results, _ = fused([0] * 64, [0] * 64, desc=desc)
    assert results[0]['status'] == status
    assert not records and not writes


@pytest.mark.parametrize(
    'a,b',
    [
        (0x00000000, 0xbf800000),  # signed zero
        (0x7fc00001, 0x3f800000),  # all-NaN -> empty magnitude fold
        (0x00800000, 0x3f000000),  # rounded products are subnormal
        (0x7f000000, 0x40000000),  # overflowed product, infinite amax
        (0x7e800000, 0x3f800000),  # >= 2^126 clamp
    ])
def test_product_classification(a, b):
    records, writes, results, _ = fused([a] * 32, [b] * 32)
    assert records == model_records([fp32_mul_bits(a, b)] * 32)
    assert results[0]['status'] == Status.OK and not writes


def test_single_multiplier_fallback():
    a = [f32_bits(i / 15) for i in range(64)]
    b = [f32_bits((i % 7 - 3) / 11) for i in range(64)]
    records, writes, results, _ = fused(a,
                                        b,
                                        params=dict(PARAMS_G4,
                                                    vec_mul_units=1))
    assert records == model_records(
        [fp32_mul_bits(x, y) for x, y in zip(a, b)])
    assert results[0]['status'] == Status.OK and not writes


def test_fused_then_direct_then_fused():
    a = [f32_bits(i / 17) for i in range(96)]
    b = [f32_bits((i % 13 - 6) / 7) for i in range(96)]
    descs = [
        dict(op=int(op), n=96, b_base=B_ADDR)
        for op in (VecOp.VEC_MUL_QUANT_ACT, VecOp.VEC_QUANT_ACT,
                   VecOp.VEC_MUL_QUANT_ACT)
    ]
    records, writes, results, _ = fused(a, b, descs=descs)
    ref = model_records([fp32_mul_bits(x, y) for x, y in zip(a, b)])
    assert records == ref + model_records(a) + ref
    assert all(r['status'] == Status.OK for r in results)
    assert not writes


@pytest.mark.parametrize('banks', [1, 2])
def test_fused_product_through_core(banks):
    from troom.consts import Format, Opcode
    from tests.troom.test_full_path import run_program, R_DRAM
    from tests.troom.test_matrix import gen_wsegs
    from tests.troom.test_vector_quant_act import (
        chain_reference,
        check_chain_results,
        conflict_watch,
        q_act_chain,
    )

    k, rows = 96, 4
    rng = random.Random(728)
    a = [f32_bits(rng.uniform(-3, 3)) for _ in range(k)]
    b = [f32_bits(rng.uniform(-2, 2)) for _ in range(k)]
    products = [fp32_mul_bits(x, y) for x, y in zip(a, b)]
    weights = gen_wsegs(rng, Format.Q8_0, rows * (k // 32))
    params = dict(PARAMS_G4, act_banks=banks)
    program, image, _, _ = q_act_chain(params, a, weights, Format.Q8_0, k,
                                       rows)
    image[0x10000:0x10000 + 4 * k] = words_to_bytes(b)
    # Both finalized operands are resident before the single fused pass.
    program[2:2] = [
        dict(opcode=Opcode.LOAD_2D,
             evt=3,
             dram_addr=0x10000,
             tmem_addr=B_ADDR,
             row_bytes=4 * k,
             rows=1),
        dict(opcode=Opcode.WAIT, evt=3)
    ]
    program[4].update(vec_op=int(VecOp.VEC_MUL_QUANT_ACT), m_base=B_ADDR)
    for i, command in enumerate(program):
        command['seq'] = i + 2
    result, dram, _ = run_program(params,
                                  program,
                                  image,
                                  deadline=60000,
                                  watch=conflict_watch)
    assert all(status == Status.OK for status in result.values())
    check_chain_results(
        dram, chain_reference(Format.Q8_0, k, rows, weights, products), k,
        rows)
