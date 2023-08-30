from enum import IntEnum


class AXIBurst(IntEnum):
    FIXED = 0b00
    INCR = 0b01
    WRAP = 0b10
    RESERVED = 0b11


class AXIResp(IntEnum):
    OKAY = 0b00
    EXOKAY = 0b01
    SLVERR = 0b10
    DECERR = 0b11
