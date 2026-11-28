from enum import IntEnum


class IpProtocol(IntEnum):
    ICMP = 0x01
    TCP = 0x06
    UDP = 0x11
