from enum import IntEnum


class VOpCode(IntEnum):
    NOP = 0


class VFUType(IntEnum):
    X = 0
    ALU = 1
    MEM = 2
    MUL = 4
    DIV = 8
