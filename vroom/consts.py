from enum import IntEnum


class VOpCode(IntEnum):
    NOP = 0
    VADD = 1
    VADDU = 2
    VSUB = 3
    VSUBU = 4
    VADC = 5
    VSBC = 6
    VRSUB = 7
    VMINU = 8
    VMIN = 9
    VMAXU = 10
    VMAX = 11
    VAND = 12
    VOR = 13
    VXOR = 14
    VSADDU = 15
    VSADD = 16
    VSSUBU = 17
    VSSUB = 18
    VAADDU = 19
    VAADD = 20
    VASUBU = 21
    VASUB = 22
    VSSRL = 23
    VSSRA = 24
    VNCLIP = 25
    VNCLIPU = 26
    VSLL = 27
    VSRL = 28
    VSRA = 29
    VNSRL = 30
    VNSRA = 31
    VMERGE = 32
    VMVSX = 33
    VFMVSF = 34
    VREDSUM = 35
    VREDAND = 36
    VREDOR = 37
    VREDXOR = 38
    VREDMINU = 39
    VREDMIN = 40
    VREDMAXU = 41
    VREDMAX = 42
    VWREDSUMU = 43
    VWREDSUM = 44
    VMUL = 45
    VMULH = 46
    VMULHU = 47
    VMULHSU = 48
    VMACC = 49
    VNMSAC = 50
    VMADD = 51
    VNMSUB = 52
    VSMUL = 53
    VDIVU = 54
    VDIV = 55
    VREMU = 56
    VREM = 57
    VFADD = 58
    VFSUB = 59
    VFRSUB = 60
    VFMUL = 61
    VFDIV = 62
    VFRDIV = 63
    VFMACC = 64
    VFNMACC = 65
    VFMSAC = 66
    VFNMSAC = 67
    VFMADD = 68
    VFNMADD = 69
    VFMSUB = 70
    VFNMSUB = 71
    VFSQRT = 72
    VFMIN = 73
    VFMAX = 74
    VFREC7 = 75
    VFRSQRT7 = 76
    VFCLASS = 77
    VFSGNJ = 78
    VFSGNJN = 79
    VFSGNJX = 80
    VFCVTXUF = 81
    VFCVTXF = 82
    VFCVTFXU = 83
    VFCVTFX = 84
    VFCVTRTZXUF = 85
    VFCVTRTZXF = 86
    VFNCVTRODFF = 87
    VFCVTFF = 88
    VFREDUSUM = 89
    VFREDOSUM = 90
    VFREDMIN = 91
    VFREDMAX = 92
    VFWREDUSUM = 93
    VFWREDOSUM = 94
    VMFEQ = 95
    VMFLE = 96
    VMFLT = 97
    VMFNE = 98
    VMFGT = 99
    VMFGE = 100
    VMSEQ = 101
    VMSNE = 102
    VMSLTU = 103
    VMSLT = 104
    VMSLEU = 105
    VMSLE = 106
    VMSGTU = 107
    VMSGT = 108
    VMADC = 109
    VMSBC = 110
    VMSBF = 111
    VMSOF = 112
    VMSIF = 113
    VIOTA = 114
    VID = 115
    VCPOP = 116
    VFIRST = 117
    VMANDNOT = 118
    VMAND = 119
    VMOR = 120
    VMXOR = 121
    VMORNOT = 122
    VMNAND = 123
    VMNOR = 124
    VMXNOR = 125
    VRGATHER = 126
    VRGATHEREI16 = 127
    VCOMPRESS = 128
    VMVXS = 129
    VFMVFS = 130
    VSLIDEUP = 131
    VSLIDEDOWN = 132
    VLE = 133
    VLSE = 134
    VLXE = 135
    VSE = 136
    VSSE = 137
    VSXE = 138


class VOpA(IntEnum):
    VS1 = 0
    IMM = 1
    SCALAR = 2


class VALUOperator(IntEnum):
    VADDU = 0
    VADD = 1
    VMSEQ = 2
    VMSNE = 3
    VXOR = 4
    VSR = 5
    VOR = 6
    VAND = 7

    VSUBU = 8
    VSUB = 9
    VSBC = 10
    VMSBC = 11
    VSRA = 12

    VADC = 16
    VMADC = 17
    VRSUB = 18
    VSL = 19
    VMVXS = 20
    VMVSX = 21
    VMVNRV = 22

    VANDN = 24
    VORN = 25
    VXNOR = 26

    VMAXU = 28
    VMAX = 29
    VMINU = 30
    VMIN = 31

    VMSLTU = 40
    VMSLT = 41
    VMSGTU = 42
    VMSGT = 43
    VMSLEU = 44
    VMSLE = 45

    # Fixed-point
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
