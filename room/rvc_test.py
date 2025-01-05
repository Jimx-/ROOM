from amaranth.sim import Settle
import pytest

from room.rvc import RVCDecoder
from room.test import run_test

import tinyrv


def encode_imm(imm, format_str):
    imm_str = list(reversed(f'{imm:032b}'))

    encoded = []
    for f in reversed(format_str.split('|')):
        eps = f.split(':')
        if len(eps) == 1:
            start = int(eps[0])
            end = start + 1
        else:
            start = int(eps[1])
            end = int(eps[0]) + 1

        encoded += imm_str[start:end]

    return int(''.join(reversed(encoded)), 2)


def rvc_unittest(dec, c_inst, check_func):

    def proc():
        yield dec.instr_i.eq(c_inst)
        yield Settle()
        exp_inst = yield dec.instr_o

        check_func(exp_inst)

    return proc


@pytest.mark.parametrize('xlen', [32, 64])
def test_c0_addi4spn(xlen):
    dut = RVCDecoder(xlen)

    imm = 0x5c
    rd_p = 1
    c_imm = encode_imm(imm, '5:4|9:6|2|3') & 0xff
    c_inst = (0b000 << 13) | (c_imm << 5) | (rd_p << 2) | 0b00

    c_op = tinyrv.decode(c_inst, xlen=xlen)
    assert c_op.name == 'c_addi4spn'
    assert c_op.rd_p == rd_p
    assert c_op.nzuimm10 == imm

    def check(exp_inst):
        op = tinyrv.decode(exp_inst, xlen=xlen)
        assert op.name == 'addi'
        assert op.rd == rd_p + 8
        assert op.imm12 == imm

    run_test(dut, rvc_unittest(dut, c_inst, check), sync=False)


@pytest.mark.parametrize('xlen', [32, 64])
def test_c0_fld(xlen):
    dut = RVCDecoder(xlen)

    imm = 0x58
    rd_p = 1
    rs1_p = 2
    c_imm_hi = encode_imm(imm, '5:3')
    c_imm_lo = encode_imm(imm, '7:6')
    c_inst = (0b001 << 13) | (c_imm_hi << 10) | (rs1_p << 7) | (
        c_imm_lo << 5) | (rd_p << 2) | 0b00

    c_op = tinyrv.decode(c_inst, xlen=xlen)
    assert c_op.name == 'c_fld'
    assert c_op.rd_p == rd_p
    assert c_op.rs1_p == rs1_p
    assert c_op.uimm8 == imm

    def check(exp_inst):
        op = tinyrv.decode(exp_inst, xlen=xlen)
        assert op.name == 'fld'
        assert op.rd == rd_p + 8
        assert op.rs1 == rs1_p + 8
        assert op.imm12 == imm

    run_test(dut, rvc_unittest(dut, c_inst, check), sync=False)


@pytest.mark.parametrize('xlen', [32, 64])
def test_c0_lw(xlen):
    dut = RVCDecoder(xlen)

    imm = 0x58
    rd_p = 1
    rs1_p = 2
    c_imm_hi = encode_imm(imm, '5:3')
    c_imm_lo = encode_imm(imm, '2|6')
    c_inst = (0b010 << 13) | (c_imm_hi << 10) | (rs1_p << 7) | (
        c_imm_lo << 5) | (rd_p << 2) | 0b00

    c_op = tinyrv.decode(c_inst, xlen=xlen)
    assert c_op.name == 'c_lw'
    assert c_op.rd_p == rd_p
    assert c_op.rs1_p == rs1_p
    assert c_op.uimm7 == imm

    def check(exp_inst):
        op = tinyrv.decode(exp_inst, xlen=xlen)
        assert op.name == 'lw'
        assert op.rd == rd_p + 8
        assert op.rs1 == rs1_p + 8
        assert op.imm12 == imm

    run_test(dut, rvc_unittest(dut, c_inst, check), sync=False)


def test_c0_flw():
    dut = RVCDecoder(32)

    imm = 0x58
    rd_p = 1
    rs1_p = 2
    c_imm_hi = encode_imm(imm, '5:3')
    c_imm_lo = encode_imm(imm, '2|6')
    c_inst = (0b011 << 13) | (c_imm_hi << 10) | (rs1_p << 7) | (
        c_imm_lo << 5) | (rd_p << 2) | 0b00

    c_op = tinyrv.decode(c_inst, xlen=32)
    assert c_op.name == 'c_flw'
    assert c_op.rd_p == rd_p
    assert c_op.rs1_p == rs1_p
    assert c_op.uimm7 == imm

    def check(exp_inst):
        op = tinyrv.decode(exp_inst, xlen=32)
        assert op.name == 'flw'
        assert op.rd == rd_p + 8
        assert op.rs1 == rs1_p + 8
        assert op.imm12 == imm

    run_test(dut, rvc_unittest(dut, c_inst, check), sync=False)


def test_c0_ld():
    dut = RVCDecoder(64)

    imm = 0x58
    rd_p = 1
    rs1_p = 2
    c_imm_hi = encode_imm(imm, '5:3')
    c_imm_lo = encode_imm(imm, '7:6')
    c_inst = (0b011 << 13) | (c_imm_hi << 10) | (rs1_p << 7) | (
        c_imm_lo << 5) | (rd_p << 2) | 0b00

    c_op = tinyrv.decode(c_inst, xlen=64)
    assert c_op.name == 'c_ld'
    assert c_op.rd_p == rd_p
    assert c_op.rs1_p == rs1_p
    assert c_op.uimm8 == imm

    def check(exp_inst):
        op = tinyrv.decode(exp_inst, xlen=64)
        assert op.name == 'ld'
        assert op.rd == rd_p + 8
        assert op.rs1 == rs1_p + 8
        assert op.imm12 == imm

    run_test(dut, rvc_unittest(dut, c_inst, check), sync=False)


@pytest.mark.parametrize('xlen', [32, 64])
def test_c0_fsd(xlen):
    dut = RVCDecoder(xlen)

    imm = 0x58
    rs2_p = 1
    rs1_p = 2
    c_imm_hi = encode_imm(imm, '5:3')
    c_imm_lo = encode_imm(imm, '7:6')
    c_inst = (0b101 << 13) | (c_imm_hi << 10) | (rs1_p << 7) | (
        c_imm_lo << 5) | (rs2_p << 2) | 0b00

    c_op = tinyrv.decode(c_inst, xlen=xlen)
    assert c_op.name == 'c_fsd'
    assert c_op.rs1_p == rs1_p
    assert c_op.rs2_p == rs2_p
    assert c_op.uimm8 == imm

    def check(exp_inst):
        op = tinyrv.decode(exp_inst, xlen=xlen)
        assert op.name == 'fsd'
        assert op.rs1 == rs1_p + 8
        assert op.rs2 == rs2_p + 8
        assert op.imm12 == imm

    run_test(dut, rvc_unittest(dut, c_inst, check), sync=False)


@pytest.mark.parametrize('xlen', [32, 64])
def test_c0_sw(xlen):
    dut = RVCDecoder(xlen)

    imm = 0x58
    rs2_p = 1
    rs1_p = 2
    c_imm_hi = encode_imm(imm, '5:3')
    c_imm_lo = encode_imm(imm, '2|6')
    c_inst = (0b110 << 13) | (c_imm_hi << 10) | (rs1_p << 7) | (
        c_imm_lo << 5) | (rs2_p << 2) | 0b00

    c_op = tinyrv.decode(c_inst, xlen=xlen)
    assert c_op.name == 'c_sw'
    assert c_op.rs1_p == rs1_p
    assert c_op.rs2_p == rs2_p
    assert c_op.uimm7 == imm

    def check(exp_inst):
        op = tinyrv.decode(exp_inst, xlen=xlen)
        assert op.name == 'sw'
        assert op.rs1 == rs1_p + 8
        assert op.rs2 == rs2_p + 8
        assert op.imm12 == imm

    run_test(dut, rvc_unittest(dut, c_inst, check), sync=False)


def test_c0_fsw():
    dut = RVCDecoder(32)

    imm = 0x58
    rs2_p = 1
    rs1_p = 2
    c_imm_hi = encode_imm(imm, '5:3')
    c_imm_lo = encode_imm(imm, '2|6')
    c_inst = (0b111 << 13) | (c_imm_hi << 10) | (rs1_p << 7) | (
        c_imm_lo << 5) | (rs2_p << 2) | 0b00

    c_op = tinyrv.decode(c_inst, xlen=32)
    assert c_op.name == 'c_fsw'
    assert c_op.rs1_p == rs1_p
    assert c_op.rs2_p == rs2_p
    assert c_op.uimm7 == imm

    def check(exp_inst):
        op = tinyrv.decode(exp_inst, xlen=32)
        assert op.name == 'fsw'
        assert op.rs1 == rs1_p + 8
        assert op.rs2 == rs2_p + 8
        assert op.imm12 == imm

    run_test(dut, rvc_unittest(dut, c_inst, check), sync=False)


def test_c0_sd():
    dut = RVCDecoder(64)

    imm = 0x58
    rs2_p = 1
    rs1_p = 2
    c_imm_hi = encode_imm(imm, '5:3')
    c_imm_lo = encode_imm(imm, '7:6')
    c_inst = (0b111 << 13) | (c_imm_hi << 10) | (rs1_p << 7) | (
        c_imm_lo << 5) | (rs2_p << 2) | 0b00

    c_op = tinyrv.decode(c_inst, xlen=64)
    assert c_op.name == 'c_sd'
    assert c_op.rs1_p == rs1_p
    assert c_op.rs2_p == rs2_p
    assert c_op.uimm8 == imm

    def check(exp_inst):
        op = tinyrv.decode(exp_inst, xlen=64)
        assert op.name == 'sd'
        assert op.rs1 == rs1_p + 8
        assert op.rs2 == rs2_p + 8
        assert op.imm12 == imm

    run_test(dut, rvc_unittest(dut, c_inst, check), sync=False)


@pytest.mark.parametrize('xlen', [32, 64])
def test_c1_addi(xlen):
    dut = RVCDecoder(xlen)

    imm = 0x15
    rs1 = 1
    c_imm_hi = encode_imm(imm, '5')
    c_imm_lo = encode_imm(imm, '4:0')
    c_inst = (0b000 << 13) | (c_imm_hi << 12) | (rs1 << 7) | (
        c_imm_lo << 2) | 0b01

    c_op = tinyrv.decode(c_inst, xlen=xlen)
    assert c_op.name == 'c_addi'
    assert c_op.rd_rs1_n0 == rs1
    assert c_op.nzimm6 == imm

    def check(exp_inst):
        op = tinyrv.decode(exp_inst, xlen=xlen)
        assert op.name == 'addi'
        assert op.rd == rs1
        assert op.rs1 == rs1
        assert op.imm12 == imm

    run_test(dut, rvc_unittest(dut, c_inst, check), sync=False)


def test_c1_jal():
    dut = RVCDecoder(32)

    offset = 0x246
    c_offset = encode_imm(offset, '11|4|9:8|10|6|7|3:1|5')
    c_inst = (0b001 << 13) | (c_offset << 2) | 0b01

    c_op = tinyrv.decode(c_inst, xlen=32)
    assert c_op.name == 'c_jal'
    assert c_op.imm12 == offset

    def check(exp_inst):
        op = tinyrv.decode(exp_inst, xlen=32)
        assert op.name == 'jal'
        assert op.rd == 1
        assert op.jimm20 == offset

    run_test(dut, rvc_unittest(dut, c_inst, check), sync=False)


def test_c1_addiw():
    dut = RVCDecoder(64)

    imm = 0x15
    rs1 = 1
    c_imm_hi = encode_imm(imm, '5')
    c_imm_lo = encode_imm(imm, '4:0')
    c_inst = (0b001 << 13) | (c_imm_hi << 12) | (rs1 << 7) | (
        c_imm_lo << 2) | 0b01

    c_op = tinyrv.decode(c_inst, xlen=64)
    assert c_op.name == 'c_addiw'
    assert c_op.rd_rs1_n0 == rs1
    assert c_op.imm6 == imm

    def check(exp_inst):
        op = tinyrv.decode(exp_inst, xlen=64)
        assert op.name == 'addiw'
        assert op.rd == rs1
        assert op.rd == rs1
        assert op.imm12 == imm

    run_test(dut, rvc_unittest(dut, c_inst, check), sync=False)


@pytest.mark.parametrize('xlen', [32, 64])
def test_c1_li(xlen):
    dut = RVCDecoder(xlen)

    imm = 0x15
    rd = 1
    c_imm_hi = encode_imm(imm, '5')
    c_imm_lo = encode_imm(imm, '4:0')
    c_inst = (0b010 << 13) | (c_imm_hi << 12) | (rd << 7) | (
        c_imm_lo << 2) | 0b01

    c_op = tinyrv.decode(c_inst, xlen=xlen)
    assert c_op.name == 'c_li'
    assert c_op.rd_n0 == rd
    assert c_op.imm6 == imm

    def check(exp_inst):
        op = tinyrv.decode(exp_inst, xlen=xlen)
        assert op.name == 'addi'
        assert op.rd == rd
        assert op.imm12 == imm

    run_test(dut, rvc_unittest(dut, c_inst, check), sync=False)


@pytest.mark.parametrize('xlen', [32, 64])
def test_c1_addi16sp(xlen):
    dut = RVCDecoder(xlen)

    imm = 0x1a0
    rd = 2
    c_imm_hi = encode_imm(imm, '9')
    c_imm_lo = encode_imm(imm, '4|6|8:7|5')
    c_inst = (0b011 << 13) | (c_imm_hi << 12) | (rd << 7) | (
        c_imm_lo << 2) | 0b01

    c_op = tinyrv.decode(c_inst, xlen=xlen)
    assert c_op.name == 'c_addi16sp'
    assert c_op.nzimm10 == imm

    def check(exp_inst):
        op = tinyrv.decode(exp_inst, xlen=xlen)
        assert op.name == 'addi'
        assert op.rd == rd
        assert op.rs1 == rd
        assert op.imm12 == imm

    run_test(dut, rvc_unittest(dut, c_inst, check), sync=False)


@pytest.mark.parametrize('xlen', [32, 64])
def test_c1_lui(xlen):
    dut = RVCDecoder(xlen)

    imm = 0x15000
    rd = 1
    c_imm_hi = encode_imm(imm, '17')
    c_imm_lo = encode_imm(imm, '16:12')
    c_inst = (0b011 << 13) | (c_imm_hi << 12) | (rd << 7) | (
        c_imm_lo << 2) | 0b01

    c_op = tinyrv.decode(c_inst, xlen=xlen)
    assert c_op.name == 'c_lui'
    assert c_op.rd_n2 == rd
    assert c_op.nzimm18 == imm

    def check(exp_inst):
        op = tinyrv.decode(exp_inst, xlen=xlen)
        assert op.name == 'lui'
        assert op.rd == rd
        assert op.imm20 == imm << 12

    run_test(dut, rvc_unittest(dut, c_inst, check), sync=False)


@pytest.mark.parametrize('xlen', [32, 64])
def test_c1_srli(xlen):
    dut = RVCDecoder(xlen)

    imm = 0x1a
    rs1_p = 1
    c_imm_hi = encode_imm(imm, '5')
    c_imm_lo = encode_imm(imm, '4:0')
    c_inst = (0b100 << 13) | (c_imm_hi << 12) | (0b00 << 10) | (rs1_p << 7) | (
        c_imm_lo << 2) | 0b01

    c_op = tinyrv.decode(c_inst, xlen=xlen)
    assert c_op.name == 'c_srli'
    assert c_op.rd_rs1_p == rs1_p
    assert c_op.nzuimm6 == imm

    def check(exp_inst):
        op = tinyrv.decode(exp_inst, xlen=xlen)
        assert op.name == 'srli'
        assert op.rd == rs1_p + 8
        assert op.rs1 == rs1_p + 8
        assert op.shamtd == imm

    run_test(dut, rvc_unittest(dut, c_inst, check), sync=False)


@pytest.mark.parametrize('xlen', [32, 64])
def test_c1_srai(xlen):
    dut = RVCDecoder(xlen)

    imm = 0x1a
    rs1_p = 1
    c_imm_hi = encode_imm(imm, '5')
    c_imm_lo = encode_imm(imm, '4:0')
    c_inst = (0b100 << 13) | (c_imm_hi << 12) | (0b01 << 10) | (rs1_p << 7) | (
        c_imm_lo << 2) | 0b01

    c_op = tinyrv.decode(c_inst, xlen=xlen)
    assert c_op.name == 'c_srai'
    assert c_op.rd_rs1_p == rs1_p
    assert c_op.nzuimm6 == imm

    def check(exp_inst):
        op = tinyrv.decode(exp_inst, xlen=xlen)
        assert op.name == 'srai'
        assert op.rd == rs1_p + 8
        assert op.rs1 == rs1_p + 8
        assert op.shamtd == imm

    run_test(dut, rvc_unittest(dut, c_inst, check), sync=False)


@pytest.mark.parametrize('xlen', [32, 64])
def test_c1_andi(xlen):
    dut = RVCDecoder(xlen)

    imm = 0x1a
    rs1_p = 1
    c_imm_hi = encode_imm(imm, '5')
    c_imm_lo = encode_imm(imm, '4:0')
    c_inst = (0b100 << 13) | (c_imm_hi << 12) | (0b10 << 10) | (rs1_p << 7) | (
        c_imm_lo << 2) | 0b01

    c_op = tinyrv.decode(c_inst, xlen=xlen)
    assert c_op.name == 'c_andi'
    assert c_op.rd_rs1_p == rs1_p
    assert c_op.imm6 == imm

    def check(exp_inst):
        op = tinyrv.decode(exp_inst, xlen=xlen)
        assert op.name == 'andi'
        assert op.rd == rs1_p + 8
        assert op.rs1 == rs1_p + 8
        assert op.imm12 == imm

    run_test(dut, rvc_unittest(dut, c_inst, check), sync=False)


@pytest.mark.parametrize('xlen', [32, 64])
def test_c1_sub(xlen):
    dut = RVCDecoder(xlen)

    rs1_p = 1
    rs2_p = 2
    c_inst = (0b100 << 13) | (0 << 12) | (0b11 << 10) | (rs1_p << 7) | (
        0b00 << 5) | (rs2_p << 2) | 0b01

    c_op = tinyrv.decode(c_inst, xlen=xlen)
    assert c_op.name == 'c_sub'
    assert c_op.rd_rs1_p == rs1_p
    assert c_op.rs2_p == rs2_p

    def check(exp_inst):
        op = tinyrv.decode(exp_inst, xlen=xlen)
        assert op.name == 'sub'
        assert op.rs1 == rs1_p + 8
        assert op.rs2 == rs2_p + 8

    run_test(dut, rvc_unittest(dut, c_inst, check), sync=False)


@pytest.mark.parametrize('xlen', [32, 64])
def test_c1_xor(xlen):
    dut = RVCDecoder(xlen)

    rs1_p = 1
    rs2_p = 2
    c_inst = (0b100 << 13) | (0 << 12) | (0b11 << 10) | (rs1_p << 7) | (
        0b01 << 5) | (rs2_p << 2) | 0b01

    c_op = tinyrv.decode(c_inst, xlen=xlen)
    assert c_op.name == 'c_xor'
    assert c_op.rd_rs1_p == rs1_p
    assert c_op.rs2_p == rs2_p

    def check(exp_inst):
        op = tinyrv.decode(exp_inst, xlen=xlen)
        assert op.name == 'xor'
        assert op.rs1 == rs1_p + 8
        assert op.rs2 == rs2_p + 8

    run_test(dut, rvc_unittest(dut, c_inst, check), sync=False)


@pytest.mark.parametrize('xlen', [32, 64])
def test_c1_or(xlen):
    dut = RVCDecoder(xlen)

    rs1_p = 1
    rs2_p = 2
    c_inst = (0b100 << 13) | (0 << 12) | (0b11 << 10) | (rs1_p << 7) | (
        0b10 << 5) | (rs2_p << 2) | 0b01

    c_op = tinyrv.decode(c_inst, xlen=xlen)
    assert c_op.name == 'c_or'
    assert c_op.rd_rs1_p == rs1_p
    assert c_op.rs2_p == rs2_p

    def check(exp_inst):
        op = tinyrv.decode(exp_inst, xlen=xlen)
        assert op.name == 'or'
        assert op.rs1 == rs1_p + 8
        assert op.rs2 == rs2_p + 8

    run_test(dut, rvc_unittest(dut, c_inst, check), sync=False)


@pytest.mark.parametrize('xlen', [32, 64])
def test_c1_and(xlen):
    dut = RVCDecoder(xlen)

    rs1_p = 1
    rs2_p = 2
    c_inst = (0b100 << 13) | (0 << 12) | (0b11 << 10) | (rs1_p << 7) | (
        0b11 << 5) | (rs2_p << 2) | 0b01

    c_op = tinyrv.decode(c_inst, xlen=xlen)
    assert c_op.name == 'c_and'
    assert c_op.rd_rs1_p == rs1_p
    assert c_op.rs2_p == rs2_p

    def check(exp_inst):
        op = tinyrv.decode(exp_inst, xlen=xlen)
        assert op.name == 'and'
        assert op.rs1 == rs1_p + 8
        assert op.rs2 == rs2_p + 8

    run_test(dut, rvc_unittest(dut, c_inst, check), sync=False)


@pytest.mark.parametrize('xlen', [32, 64])
def test_c1_subw(xlen):
    dut = RVCDecoder(xlen)

    rs1_p = 1
    rs2_p = 2
    c_inst = (0b100 << 13) | (1 << 12) | (0b11 << 10) | (rs1_p << 7) | (
        0b00 << 5) | (rs2_p << 2) | 0b01

    c_op = tinyrv.decode(c_inst, xlen=xlen)
    assert c_op.name == 'c_subw'
    assert c_op.rd_rs1_p == rs1_p
    assert c_op.rs2_p == rs2_p

    def check(exp_inst):
        op = tinyrv.decode(exp_inst, xlen=xlen)
        assert op.name == 'subw'
        assert op.rs1 == rs1_p + 8
        assert op.rs2 == rs2_p + 8

    run_test(dut, rvc_unittest(dut, c_inst, check), sync=False)


@pytest.mark.parametrize('xlen', [32, 64])
def test_c1_addw(xlen):
    dut = RVCDecoder(xlen)

    rs1_p = 1
    rs2_p = 2
    c_inst = (0b100 << 13) | (1 << 12) | (0b11 << 10) | (rs1_p << 7) | (
        0b01 << 5) | (rs2_p << 2) | 0b01

    c_op = tinyrv.decode(c_inst, xlen=xlen)
    assert c_op.name == 'c_addw'
    assert c_op.rd_rs1_p == rs1_p
    assert c_op.rs2_p == rs2_p

    def check(exp_inst):
        op = tinyrv.decode(exp_inst, xlen=xlen)
        assert op.name == 'addw'
        assert op.rs1 == rs1_p + 8
        assert op.rs2 == rs2_p + 8

    run_test(dut, rvc_unittest(dut, c_inst, check), sync=False)


@pytest.mark.parametrize('xlen', [32, 64])
def test_c1_j(xlen):
    dut = RVCDecoder(xlen)

    offset = 0x246
    c_offset = encode_imm(offset, '11|4|9:8|10|6|7|3:1|5')
    c_inst = (0b101 << 13) | (c_offset << 2) | 0b01

    c_op = tinyrv.decode(c_inst, xlen=xlen)
    assert c_op.name == 'c_j'
    assert c_op.imm12 == offset

    def check(exp_inst):
        op = tinyrv.decode(exp_inst, xlen=xlen)
        assert op.name == 'jal'
        assert op.rd == 0
        assert op.jimm20 == offset

    run_test(dut, rvc_unittest(dut, c_inst, check), sync=False)


@pytest.mark.parametrize('xlen', [32, 64])
def test_c1_beqz(xlen):
    dut = RVCDecoder(xlen)

    imm = 170
    rs1_p = 1
    c_imm_hi = encode_imm(imm, '8|4:3')
    c_imm_lo = encode_imm(imm, '7:6|2:1|5')
    c_inst = (0b110 << 13) | (c_imm_hi << 10) | (rs1_p << 7) | (
        c_imm_lo << 2) | 0b01

    c_op = tinyrv.decode(c_inst, xlen=xlen)
    assert c_op.name == 'c_beqz'
    assert c_op.rs1_p == rs1_p
    assert c_op.bimm9 == imm

    def check(exp_inst):
        op = tinyrv.decode(exp_inst, xlen=xlen)
        assert op.name == 'beq'
        assert op.rs1 == rs1_p + 8
        assert op.rs2 == 0
        assert op.bimm12 == imm

    run_test(dut, rvc_unittest(dut, c_inst, check), sync=False)


@pytest.mark.parametrize('xlen', [32, 64])
def test_c1_bnez(xlen):
    dut = RVCDecoder(xlen)

    imm = 170
    rs1_p = 1
    c_imm_hi = encode_imm(imm, '8|4:3')
    c_imm_lo = encode_imm(imm, '7:6|2:1|5')
    c_inst = (0b111 << 13) | (c_imm_hi << 10) | (rs1_p << 7) | (
        c_imm_lo << 2) | 0b01

    c_op = tinyrv.decode(c_inst, xlen=xlen)
    assert c_op.name == 'c_bnez'
    assert c_op.rs1_p == rs1_p
    assert c_op.bimm9 == imm

    def check(exp_inst):
        op = tinyrv.decode(exp_inst, xlen=xlen)
        assert op.name == 'bne'
        assert op.rs1 == rs1_p + 8
        assert op.rs2 == 0
        assert op.bimm12 == imm

    run_test(dut, rvc_unittest(dut, c_inst, check), sync=False)


@pytest.mark.parametrize('xlen', [32, 64])
def test_c2_slli(xlen):
    dut = RVCDecoder(xlen)

    imm = 0x1a
    rs1 = 1
    c_imm_hi = encode_imm(imm, '5')
    c_imm_lo = encode_imm(imm, '4:0')
    c_inst = (0b000 << 13) | (c_imm_hi << 12) | (rs1 << 7) | (
        c_imm_lo << 2) | 0b10

    c_op = tinyrv.decode(c_inst, xlen=xlen)
    assert c_op.name == 'c_slli'
    assert c_op.rd_rs1_n0 == rs1
    assert c_op.nzuimm6 == imm

    def check(exp_inst):
        op = tinyrv.decode(exp_inst, xlen=xlen)
        assert op.name == 'slli'
        assert op.rd == rs1
        assert op.rs1 == rs1
        assert op.shamtd == imm

    run_test(dut, rvc_unittest(dut, c_inst, check), sync=False)


@pytest.mark.parametrize('xlen', [32, 64])
def test_c2_fldsp(xlen):
    dut = RVCDecoder(xlen)

    imm = 0x1a << 3
    rd = 1
    c_imm_hi = encode_imm(imm, '5')
    c_imm_lo = encode_imm(imm, '4:3|8:6')
    c_inst = (0b001 << 13) | (c_imm_hi << 12) | (rd << 7) | (
        c_imm_lo << 2) | 0b10

    c_op = tinyrv.decode(c_inst, xlen=xlen)
    assert c_op.name == 'c_fldsp'
    assert c_op.rd == rd
    assert c_op.uimm9sp == imm

    def check(exp_inst):
        op = tinyrv.decode(exp_inst, xlen=xlen)
        assert op.name == 'fld'
        assert op.rd == rd
        assert op.rs1 == 2
        assert op.imm12 == imm

    run_test(dut, rvc_unittest(dut, c_inst, check), sync=False)


@pytest.mark.parametrize('xlen', [32, 64])
def test_c2_lwsp(xlen):
    dut = RVCDecoder(xlen)

    imm = 0x1a << 2
    rd = 1
    c_imm_hi = encode_imm(imm, '5')
    c_imm_lo = encode_imm(imm, '4:2|7:6')
    c_inst = (0b010 << 13) | (c_imm_hi << 12) | (rd << 7) | (
        c_imm_lo << 2) | 0b10

    c_op = tinyrv.decode(c_inst, xlen=xlen)
    assert c_op.name == 'c_lwsp'
    assert c_op.rd_n0 == rd
    assert c_op.uimm8sp == imm

    def check(exp_inst):
        op = tinyrv.decode(exp_inst, xlen=xlen)
        assert op.name == 'lw'
        assert op.rd == rd
        assert op.rs1 == 2
        assert op.imm12 == imm

    run_test(dut, rvc_unittest(dut, c_inst, check), sync=False)


def test_c2_flwsp():
    dut = RVCDecoder(32)

    imm = 0x1a << 2
    rd = 1
    c_imm_hi = encode_imm(imm, '5')
    c_imm_lo = encode_imm(imm, '4:2|7:6')
    c_inst = (0b011 << 13) | (c_imm_hi << 12) | (rd << 7) | (
        c_imm_lo << 2) | 0b10

    c_op = tinyrv.decode(c_inst, xlen=32)
    assert c_op.name == 'c_flwsp'
    assert c_op.rd == rd
    assert c_op.uimm8sp == imm

    def check(exp_inst):
        op = tinyrv.decode(exp_inst, xlen=32)
        assert op.name == 'flw'
        assert op.rd == rd
        assert op.rs1 == 2
        assert op.imm12 == imm

    run_test(dut, rvc_unittest(dut, c_inst, check), sync=False)


def test_c2_ldsp():
    dut = RVCDecoder(64)

    imm = 0x1a << 3
    rd = 1
    c_imm_hi = encode_imm(imm, '5')
    c_imm_lo = encode_imm(imm, '4:3|8:6')
    c_inst = (0b011 << 13) | (c_imm_hi << 12) | (rd << 7) | (
        c_imm_lo << 2) | 0b10

    c_op = tinyrv.decode(c_inst, xlen=64)
    assert c_op.name == 'c_ldsp'
    assert c_op.rd_n0 == rd
    assert c_op.uimm9sp == imm

    def check(exp_inst):
        op = tinyrv.decode(exp_inst, xlen=64)
        assert op.name == 'ld'
        assert op.rd == rd
        assert op.rs1 == 2
        assert op.imm12 == imm

    run_test(dut, rvc_unittest(dut, c_inst, check), sync=False)


@pytest.mark.parametrize('xlen', [32, 64])
def test_c2_jr(xlen):
    dut = RVCDecoder(xlen)

    rs1 = 1
    c_inst = (0b100 << 13) | (0 << 12) | (rs1 << 7) | (0 << 2) | 0b10

    c_op = tinyrv.decode(c_inst, xlen=xlen)
    assert c_op.name == 'c_jr'
    assert c_op.rs1_n0 == rs1

    def check(exp_inst):
        op = tinyrv.decode(exp_inst, xlen=xlen)
        assert op.name == 'jalr'
        assert op.rd == 0
        assert op.rs1 == rs1
        assert op.imm12 == 0

    run_test(dut, rvc_unittest(dut, c_inst, check), sync=False)


@pytest.mark.parametrize('xlen', [32, 64])
def test_c2_mv(xlen):
    dut = RVCDecoder(xlen)

    rd = 1
    rs2 = 2
    c_inst = (0b100 << 13) | (0 << 12) | (rd << 7) | (rs2 << 2) | 0b10

    c_op = tinyrv.decode(c_inst, xlen=xlen)
    assert c_op.name == 'c_mv'
    assert c_op.rd_n0 == rd
    assert c_op.rs2_n0 == rs2

    def check(exp_inst):
        op = tinyrv.decode(exp_inst, xlen=xlen)
        assert op.name == 'add'
        assert op.rd == rd
        assert op.rs1 == 0
        assert op.rs2 == rs2

    run_test(dut, rvc_unittest(dut, c_inst, check), sync=False)


@pytest.mark.parametrize('xlen', [32, 64])
def test_c2_ebreak(xlen):
    dut = RVCDecoder(xlen)

    c_inst = (0b100 << 13) | (1 << 12) | (0 << 7) | (0 << 2) | 0b10

    c_op = tinyrv.decode(c_inst, xlen=xlen)
    assert c_op.name == 'c_ebreak'

    def check(exp_inst):
        op = tinyrv.decode(exp_inst, xlen=xlen)
        assert op.name == 'ebreak'

    run_test(dut, rvc_unittest(dut, c_inst, check), sync=False)


@pytest.mark.parametrize('xlen', [32, 64])
def test_c2_jalr(xlen):
    dut = RVCDecoder(xlen)

    rs1 = 2
    c_inst = (0b100 << 13) | (1 << 12) | (rs1 << 7) | (0 << 2) | 0b10

    c_op = tinyrv.decode(c_inst, xlen=xlen)
    assert c_op.name == 'c_jalr'
    assert c_op.rs1_n0 == rs1

    def check(exp_inst):
        op = tinyrv.decode(exp_inst, xlen=xlen)
        assert op.name == 'jalr'
        assert op.rd == 1
        assert op.rs1 == rs1
        assert op.imm12 == 0

    run_test(dut, rvc_unittest(dut, c_inst, check), sync=False)


@pytest.mark.parametrize('xlen', [32, 64])
def test_c2_add(xlen):
    dut = RVCDecoder(xlen)

    rd = 1
    rs2 = 2
    c_inst = (0b100 << 13) | (1 << 12) | (rd << 7) | (rs2 << 2) | 0b10

    c_op = tinyrv.decode(c_inst, xlen=xlen)
    assert c_op.name == 'c_add'
    assert c_op.rd_rs1_n0 == rd
    assert c_op.rs2_n0 == rs2

    def check(exp_inst):
        op = tinyrv.decode(exp_inst, xlen=xlen)
        assert op.name == 'add'
        assert op.rd == rd
        assert op.rs1 == rd
        assert op.rs2 == rs2

    run_test(dut, rvc_unittest(dut, c_inst, check), sync=False)


@pytest.mark.parametrize('xlen', [32, 64])
def test_c2_fsdsp(xlen):
    dut = RVCDecoder(xlen)

    imm = 0x1a << 3
    rs2 = 1
    c_imm = encode_imm(imm, '5:3|8:6')
    c_inst = (0b101 << 13) | (c_imm << 7) | (rs2 << 2) | 0b10

    c_op = tinyrv.decode(c_inst, xlen=xlen)
    assert c_op.name == 'c_fsdsp'
    assert c_op.rs2 == rs2
    assert c_op.uimm9sp_s == imm

    def check(exp_inst):
        op = tinyrv.decode(exp_inst, xlen=xlen)
        assert op.name == 'fsd'
        assert op.rs1 == 2
        assert op.rs2 == rs2
        assert op.imm12 == imm

    run_test(dut, rvc_unittest(dut, c_inst, check), sync=False)


@pytest.mark.parametrize('xlen', [32, 64])
def test_c2_swsp(xlen):
    dut = RVCDecoder(xlen)

    imm = 0x1a << 2
    rs2 = 1
    c_imm = encode_imm(imm, '5:2|7:6')
    c_inst = (0b110 << 13) | (c_imm << 7) | (rs2 << 2) | 0b10

    c_op = tinyrv.decode(c_inst, xlen=xlen)
    assert c_op.name == 'c_swsp'
    assert c_op.rs2 == rs2
    assert c_op.uimm8sp_s == imm

    def check(exp_inst):
        op = tinyrv.decode(exp_inst, xlen=xlen)
        assert op.name == 'sw'
        assert op.rs1 == 2
        assert op.rs2 == rs2
        assert op.imm12 == imm

    run_test(dut, rvc_unittest(dut, c_inst, check), sync=False)


def test_c2_fswsp():
    dut = RVCDecoder(32)

    imm = 0x1a << 3
    rs2 = 1
    c_imm = encode_imm(imm, '5:2|7:6')
    c_inst = (0b111 << 13) | (c_imm << 7) | (rs2 << 2) | 0b10

    c_op = tinyrv.decode(c_inst, xlen=32)
    assert c_op.name == 'c_fswsp'
    assert c_op.rs2 == rs2
    assert c_op.uimm8sp_s == imm

    def check(exp_inst):
        op = tinyrv.decode(exp_inst, xlen=32)
        assert op.name == 'fsw'
        assert op.rs1 == 2
        assert op.rs2 == rs2
        assert op.imm12 == imm

    run_test(dut, rvc_unittest(dut, c_inst, check), sync=False)


def test_c2_sdsp():
    dut = RVCDecoder(64)

    imm = 0x1a << 3
    rs2 = 1
    c_imm = encode_imm(imm, '5:3|8:6')
    c_inst = (0b111 << 13) | (c_imm << 7) | (rs2 << 2) | 0b10

    c_op = tinyrv.decode(c_inst, xlen=64)
    assert c_op.name == 'c_sdsp'
    assert c_op.rs2 == rs2
    assert c_op.uimm9sp_s == imm

    def check(exp_inst):
        op = tinyrv.decode(exp_inst, xlen=64)
        assert op.name == 'sd'
        assert op.rs1 == 2
        assert op.rs2 == rs2
        assert op.imm12 == imm

    run_test(dut, rvc_unittest(dut, c_inst, check), sync=False)
