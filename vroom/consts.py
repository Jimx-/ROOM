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
    VSEXT = 15
    VZEXT = 16
    VSADDU = 17
    VSADD = 18
    VSSUBU = 19
    VSSUB = 20
    VAADDU = 21
    VAADD = 22
    VASUBU = 23
    VASUB = 24
    VSSRL = 25
    VSSRA = 26
    VNCLIP = 27
    VNCLIPU = 28
    VSLL = 29
    VSRL = 30
    VSRA = 31
    VNSRL = 32
    VNSRA = 33
    VMERGE = 34
    VMVSX = 35
    VFMVSF = 36
    VREDSUM = 37
    VREDAND = 38
    VREDOR = 39
    VREDXOR = 40
    VREDMINU = 41
    VREDMIN = 42
    VREDMAXU = 43
    VREDMAX = 44
    VWREDSUMU = 45
    VWREDSUM = 46
    VMUL = 47
    VMULH = 48
    VMULHU = 49
    VMULHSU = 50
    VMACC = 51
    VNMSAC = 52
    VMADD = 53
    VNMSUB = 54
    VSMUL = 55
    VDIVU = 56
    VDIV = 57
    VREMU = 58
    VREM = 59
    VFADD = 60
    VFSUB = 61
    VFRSUB = 62
    VFMUL = 63
    VFDIV = 64
    VFRDIV = 65
    VFMACC = 66
    VFNMACC = 67
    VFMSAC = 68
    VFNMSAC = 69
    VFMADD = 70
    VFNMADD = 71
    VFMSUB = 72
    VFNMSUB = 73
    VFSQRT = 74
    VFMIN = 75
    VFMAX = 76
    VFREC7 = 77
    VFRSQRT7 = 78
    VFCLASS = 79
    VFSGNJ = 80
    VFSGNJN = 81
    VFSGNJX = 82
    VFCVTXUF = 83
    VFCVTXF = 84
    VFCVTFXU = 85
    VFCVTFX = 86
    VFCVTRTZXUF = 87
    VFCVTRTZXF = 88
    VFNCVTRODFF = 89
    VFCVTFF = 90
    VFREDUSUM = 91
    VFREDOSUM = 92
    VFREDMIN = 93
    VFREDMAX = 94
    VFWREDUSUM = 95
    VFWREDOSUM = 96
    VMFEQ = 97
    VMFLE = 98
    VMFLT = 99
    VMFNE = 100
    VMFGT = 101
    VMFGE = 102
    VMSEQ = 103
    VMSNE = 104
    VMSLTU = 105
    VMSLT = 106
    VMSLEU = 107
    VMSLE = 108
    VMSGTU = 109
    VMSGT = 110
    VMADC = 111
    VMSBC = 112
    VMSBF = 113
    VMSOF = 114
    VMSIF = 115
    VIOTA = 116
    VID = 117
    VCPOP = 118
    VFIRST = 119
    VMANDNOT = 120
    VMAND = 121
    VMOR = 122
    VMXOR = 123
    VMORNOT = 124
    VMNAND = 125
    VMNOR = 126
    VMXNOR = 127
    VRGATHER = 128
    VRGATHEREI16 = 129
    VCOMPRESS = 130
    VMVXS = 131
    VFMVFS = 132
    VSLIDEUP = 133
    VSLIDEDOWN = 134
    VLE = 135
    VLSE = 136
    VLXE = 137
    VSE = 138
    VSSE = 139
    VSXE = 140


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
    VMERGE = 23

    VANDN = 24
    VORN = 25
    VXNOR = 26

    VMAXU = 28
    VMAX = 29
    VMINU = 30
    VMIN = 31

    VZEXT = 32
    VSEXT = 33

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

    @staticmethod
    def is_ext(x):
        return (x == VALUOperator.VZEXT) | (x == VALUOperator.VSEXT)

    @staticmethod
    def is_add_with_carry(x):
        return (x == VALUOperator.VADC) | (x == VALUOperator.VSBC) | (
            x == VALUOperator.VMADC) | (x == VALUOperator.VMSBC)

    @staticmethod
    def is_vmerge(x):
        return (x == VALUOperator.VMERGE)


class VFUType(IntEnum):
    X = 0
    ALU = 1
    MEM = 2
    MUL = 4
    DIV = 8
