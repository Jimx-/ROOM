from enum import IntEnum


class VOpCode(IntEnum):
    NOP = 0
    VADD = 1
    VSUB = 2
    VADC = 3
    VSBC = 4
    VRSUB = 5
    VMINU = 6
    VMIN = 7
    VMAXU = 8
    VMAX = 9
    VAND = 10
    VOR = 11
    VXOR = 12
    VSADDU = 13
    VSADD = 14
    VSSUBU = 15
    VSSUB = 16
    VAADDU = 17
    VAADD = 18
    VASUBU = 19
    VASUB = 20
    VSSRL = 21
    VSSRA = 22
    VNCLIP = 23
    VNCLIPU = 24
    VSLL = 25
    VSRL = 26
    VSRA = 27
    VNSRL = 28
    VNSRA = 29
    VMERGE = 30
    VMVSX = 31
    VFMVSF = 32
    VREDSUM = 33
    VREDAND = 34
    VREDOR = 35
    VREDXOR = 36
    VREDMINU = 37
    VREDMIN = 38
    VREDMAXU = 39
    VREDMAX = 40
    VWREDSUMU = 41
    VWREDSUM = 42
    VMUL = 43
    VMULH = 44
    VMULHU = 45
    VMULHSU = 46
    VMACC = 47
    VNMSAC = 48
    VMADD = 49
    VNMSUB = 50
    VSMUL = 51
    VDIVU = 52
    VDIV = 53
    VREMU = 54
    VREM = 55
    VFADD = 56
    VFSUB = 57
    VFRSUB = 58
    VFMUL = 59
    VFDIV = 60
    VFRDIV = 61
    VFMACC = 62
    VFNMACC = 63
    VFMSAC = 64
    VFNMSAC = 65
    VFMADD = 66
    VFNMADD = 67
    VFMSUB = 68
    VFNMSUB = 69
    VFSQRT = 70
    VFMIN = 71
    VFMAX = 72
    VFREC7 = 73
    VFRSQRT7 = 74
    VFCLASS = 75
    VFSGNJ = 76
    VFSGNJN = 77
    VFSGNJX = 78
    VFCVTXUF = 79
    VFCVTXF = 80
    VFCVTFXU = 81
    VFCVTFX = 82
    VFCVTRTZXUF = 83
    VFCVTRTZXF = 84
    VFNCVTRODFF = 85
    VFCVTFF = 86
    VFREDUSUM = 87
    VFREDOSUM = 88
    VFREDMIN = 89
    VFREDMAX = 90
    VFWREDUSUM = 91
    VFWREDOSUM = 92
    VMFEQ = 93
    VMFLE = 94
    VMFLT = 95
    VMFNE = 96
    VMFGT = 97
    VMFGE = 98
    VMSEQ = 99
    VMSNE = 100
    VMSLTU = 101
    VMSLT = 102
    VMSLEU = 103
    VMSLE = 104
    VMSGTU = 105
    VMSGT = 106
    VMADC = 107
    VMSBC = 108
    VMSBF = 109
    VMSOF = 110
    VMSIF = 111
    VIOTA = 112
    VID = 113
    VCPOP = 114
    VFIRST = 115
    VMANDNOT = 116
    VMAND = 117
    VMOR = 118
    VMXOR = 119
    VMORNOT = 120
    VMNAND = 121
    VMNOR = 122
    VMXNOR = 123
    VRGATHER = 124
    VRGATHEREI16 = 125
    VCOMPRESS = 126
    VMVXS = 127
    VFMVFS = 128
    VSLIDEUP = 129
    VSLIDEDOWN = 130
    VLE = 131
    VLSE = 132
    VLXE = 133
    VSE = 134
    VSSE = 135
    VSXE = 136


class VOpA(IntEnum):
    VS1 = 0
    IMM = 1
    SCALAR = 2


class VALUOperator(IntEnum):
    VADD = 0
    VSL = 1
    VMSEQ = 2
    VMSNE = 3
    VXOR = 4
    VSR = 5
    VOR = 6
    VAND = 7

    VSUB = 8
    VSBC = 9
    VMSBC = 10
    VSRA = 11
    VMSLTU = 12
    VMSLT = 13
    VMSGTU = 14
    VMSGT = 15

    VADC = 16
    VMADC = 17
    VRSUB = 18
    VMVXS = 19
    VMVSX = 20
    VMVNRV = 21

    VANDN = 24
    VORN = 25
    VXNOR = 26

    VMAXU = 28
    VMAX = 29
    VMINU = 30
    VMIN = 31

    VWADDU = 32
    VWADD = 33
    VWADDUWV = 34
    VWADDWV = 35

    VWSUBU = 40
    VWSUB = 41
    VWSUBUWV = 42
    VWSUBWV = 43
    VMSLEU = 44
    VMSLE = 45

    VSADDU = 48
    VSADD = 49
    VAADDU = 50
    VAADD = 51
    VSSR = 52
    VSSRA = 53
    VNCLIPU = 54
    VNCLIP = 55

    VSSUBU = 56
    VSSUB = 57
    VASUBU = 58
    VASUB = 59

    @staticmethod
    def is_sub(x):
        return x[3]

    @staticmethod
    def is_fixp(x):
        return (x >> 4) == 3


class VFUType(IntEnum):
    X = 0
    ALU = 1
    MEM = 2
    MUL = 4
    DIV = 8
