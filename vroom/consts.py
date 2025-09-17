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
    VMACCU = 55
    VMACCUS = 56
    VMACCSU = 57
    VSMUL = 58
    VDIVU = 59
    VDIV = 60
    VREMU = 61
    VREM = 62
    VFADD = 63
    VFSUB = 64
    VFRSUB = 65
    VFMUL = 66
    VFDIV = 67
    VFRDIV = 68
    VFMACC = 69
    VFNMACC = 70
    VFMSAC = 71
    VFNMSAC = 72
    VFMADD = 73
    VFNMADD = 74
    VFMSUB = 75
    VFNMSUB = 76
    VFSQRT = 77
    VFMIN = 78
    VFMAX = 79
    VFREC7 = 80
    VFRSQRT7 = 81
    VFCLASS = 82
    VFSGNJ = 83
    VFSGNJN = 84
    VFSGNJX = 85
    VFCVTXUF = 86
    VFCVTXF = 87
    VFCVTFXU = 88
    VFCVTFX = 89
    VFCVTRTZXUF = 90
    VFCVTRTZXF = 91
    VFNCVTRODFF = 92
    VFCVTFF = 93
    VFREDUSUM = 94
    VFREDOSUM = 95
    VFREDMIN = 96
    VFREDMAX = 97
    VFWREDUSUM = 98
    VFWREDOSUM = 99
    VMFEQ = 100
    VMFLE = 101
    VMFLT = 102
    VMFNE = 103
    VMFGT = 104
    VMFGE = 105
    VMSEQ = 106
    VMSNE = 107
    VMSLTU = 108
    VMSLT = 109
    VMSLEU = 110
    VMSLE = 111
    VMSGTU = 112
    VMSGT = 113
    VMADC = 114
    VMSBC = 115
    VMSBF = 116
    VMSOF = 117
    VMSIF = 118
    VIOTA = 119
    VID = 120
    VCPOP = 121
    VFIRST = 122
    VMANDNOT = 123
    VMAND = 124
    VMOR = 125
    VMXOR = 126
    VMORNOT = 127
    VMNAND = 128
    VMNOR = 129
    VMXNOR = 130
    VRGATHER = 131
    VRGATHEREI16 = 132
    VCOMPRESS = 133
    VMVXS = 134
    VFMVFS = 135
    VSLIDEUP = 136
    VSLIDEDOWN = 137
    VSLIDE1UP = 138
    VSLIDE1DOWN = 139
    VLE = 140
    VLSE = 141
    VLXE = 142
    VSE = 143
    VSSE = 144
    VSXE = 145


class VOpA(IntEnum):
    VS1 = 0
    VS1_HALF = 1
    IMM = 2
    SCALAR = 3


class VOpB(IntEnum):
    VS2 = 0
    VS2_HALF = 1
    OLD_VD = 2


class VOpC(IntEnum):
    OLD_VD = 0
    VS2 = 1


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

    VMUL = VADD
    VMULH = VSL
    VMULHSU = VMSEQ
    VMULHU = VMSNE
    VMACCU = VXOR
    VMACC = VSR
    VMACCUS = VOR
    VMACCSU = VAND
    VMADD = VADC
    VNMSUB = VSUB
    VNMSAC = VSBC

    VSMUL = VSADDU

    VDIV = VXOR
    VDIVU = VSR
    VREM = VOR
    VREMU = VAND

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

    @staticmethod
    def is_macc(x):
        return (x == VALUOperator.VMACCU) | (x == VALUOperator.VMACC) | (
            x == VALUOperator.VMACCUS) | (x == VALUOperator.VMACCSU) | (
                x == VALUOperator.VMADD) | (x == VALUOperator.VNMSUB) | (
                    x == VALUOperator.VNMSAC)


class VFUType(IntEnum):
    X = 0
    ALU = 1
    MEM = 2
    MUL = 4
    DIV = 8
    REDUCE = 16
    PERM = 32
    MASK = 64


class VXRoundingMode(IntEnum):
    RNU = 0
    RNE = 1
    RDN = 2
    ROD = 3
