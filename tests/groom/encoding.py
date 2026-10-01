"""GROOM instruction encoders: the custom opcodes plus re-exports of the
base instruction encoders."""

from tests.room.encoding import *  # noqa: F401


def gpu_tmc(rs2):
    return ((rs2 & 0x1f) << 20) | 0b1101011


def gpu_wspawn(rs1, rs2, imm=0):
    """`.insn s 0x6b, 1, rs2, imm(rs1)` as emitted by gpu_wspawn."""
    imm &= 0xfff
    return (((imm >> 5) & 0x7f) << 25) | (rs2 << 20) | (rs1 << 15) | (
        1 << 12) | ((imm & 0x1f) << 7) | 0b1101011


def wgather(rd, rs1, rs2, rs3, src_lane):
    return ((rs3 & 0x1f) << 27) | ((src_lane & 0x3) << 25) | (
        (rs2 & 0x1f) << 20) | ((rs1 & 0x1f) << 15) | (rd << 7) | 0b0101011
