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
    VMVNRV = 34
    VMERGE = 35
    VMVSX = 36
    VFMVSF = 37
    VREDSUM = 38
    VREDAND = 39
    VREDOR = 40
    VREDXOR = 41
    VREDMINU = 42
    VREDMIN = 43
    VREDMAXU = 44
    VREDMAX = 45
    VWREDSUMU = 46
    VWREDSUM = 47
    VMUL = 48
    VMULH = 49
    VMULHU = 50
    VMULHSU = 51
    VMACC = 52
    VNMSAC = 53
    VMADD = 54
    VNMSUB = 55
    VMACCU = 56
    VMACCUS = 57
    VMACCSU = 58
    VSMUL = 59
    VDIVU = 60
    VDIV = 61
    VREMU = 62
    VREM = 63
    VFADD = 64
    VFSUB = 65
    VFRSUB = 66
    VFMUL = 67
    VFDIV = 68
    VFRDIV = 69
    VFMACC = 70
    VFNMACC = 71
    VFMSAC = 72
    VFNMSAC = 73
    VFMADD = 74
    VFNMADD = 75
    VFMSUB = 76
    VFNMSUB = 77
    VFSQRT = 78
    VFMIN = 79
    VFMAX = 80
    VFREC7 = 81
    VFRSQRT7 = 82
    VFCLASS = 83
    VFSGNJ = 84
    VFSGNJN = 85
    VFSGNJX = 86
    VFCVTXUF = 87
    VFCVTXF = 88
    VFCVTFXU = 89
    VFCVTFX = 90
    VFCVTRTZXUF = 91
    VFCVTRTZXF = 92
    VFNCVTRODFF = 93
    VFCVTFF = 94
    VFREDUSUM = 95
    VFREDOSUM = 96
    VFREDMIN = 97
    VFREDMAX = 98
    VFWREDUSUM = 99
    VFWREDOSUM = 100
    VMFEQ = 101
    VMFLE = 102
    VMFLT = 103
    VMFNE = 104
    VMFGT = 105
    VMFGE = 106
    VMSEQ = 107
    VMSNE = 108
    VMSLTU = 109
    VMSLT = 110
    VMSLEU = 111
    VMSLE = 112
    VMSGTU = 113
    VMSGT = 114
    VMADC = 115
    VMSBC = 116
    VMSBF = 117
    VMSOF = 118
    VMSIF = 119
    VIOTA = 120
    VID = 121
    VCPOP = 122
    VFIRST = 123
    VMANDNOT = 124
    VMAND = 125
    VMOR = 126
    VMXOR = 127
    VMORNOT = 128
    VMNAND = 129
    VMNOR = 130
    VMXNOR = 131
    VRGATHER = 132
    VRGATHEREI16 = 133
    VCOMPRESS = 134
    VMVXS = 135
    VFMVFS = 136
    VSLIDEUP = 137
    VSLIDEDOWN = 138
    VSLIDE1UP = 139
    VSLIDE1DOWN = 140
    VLE = 141
    VLSE = 142
    VLXE = 143
    VSE = 144
    VSSE = 145
    VSXE = 146


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
    FPU = 128
    FDIV = 256
    I2F = 512
    F2I = 1024


class VXRoundingMode(IntEnum):
    RNU = 0
    RNE = 1
    RDN = 2
    ROD = 3
