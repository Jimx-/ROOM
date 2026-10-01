"""Base RISC-V instruction encoders for core-level simulations."""


def addi(rd, rs1, imm):
    return ((imm & 0xfff) << 20) | (rs1 << 15) | (rd << 7) | 0b0010011


def add(rd, rs1, rs2):
    return ((rs2 & 0x1f) << 20) | (rs1 << 15) | (rd << 7) | 0b0110011


def slli(rd, rs1, shamt):
    assert 0 <= shamt < 32
    return (shamt << 20) | (rs1 << 15) | (1 << 12) | (rd << 7) | 0b0010011


def lw(rd, rs1, imm):
    return (((imm & 0xfff) << 20) | (rs1 << 15) | (2 << 12) | (rd << 7)
            | 0b0000011)


def lui(rd, imm):
    return ((imm & 0xfffff) << 12) | (rd << 7) | 0b0110111


def csrrs(rd, csr, rs1=0):
    return (
        (csr & 0xfff) << 20) | (rs1 << 15) | (2 << 12) | (rd << 7) | 0b1110011


def jal(rd, imm=0):
    imm &= (1 << 21) - 1
    return (((imm >> 20) & 1) << 31) | (((imm >> 1) & 0x3ff) << 21) | ((
        (imm >> 11) & 1) << 20) | ((
            (imm >> 12) & 0xff) << 12) | (rd << 7) | 0b1101111


def nop():
    return addi(0, 0, 0)
