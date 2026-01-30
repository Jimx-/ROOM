from enum import IntEnum


class UOpCode(IntEnum):
    NOP = 0
    LD = 1
    STA = 2
    STD = 3
    LUI = 4

    ADDI = 5
    ANDI = 6
    ORI = 7
    XORI = 8
    SLTI = 9
    SLTIU = 10
    SLLI = 11
    SRAI = 12
    SRLI = 13

    SLL = 14
    ADD = 15
    SUB = 16
    SLT = 17
    SLTU = 18
    AND = 19
    OR = 20
    XOR = 21
    SRA = 22
    SRL = 23

    BEQ = 24
    BNE = 25
    BGE = 26
    BGEU = 27
    BLT = 28
    BLTU = 29
    CSRRW = 30
    CSRRS = 31
    CSRRC = 32
    CSRRWI = 33
    CSRRSI = 34
    CSRRCI = 35

    J = 36
    JAL = 37
    JALR = 38
    AUIPC = 39

    CFLSH = 41
    FENCE = 42

    ADDIW = 43
    ADDW = 44
    SUBW = 45
    SLLIW = 46
    SLLW = 47
    SRAIW = 48
    SRAW = 49
    SRLIW = 50
    SRLW = 51
    MUL = 52
    MULH = 53
    MULHU = 54
    MULHSU = 55
    MULW = 56
    DIV = 57
    DIVU = 58
    REM = 59
    REMU = 60
    DIVW = 61
    DIVUW = 62
    REMW = 63
    REMUW = 64

    FENCEI = 65
    AMO_AG = 66

    FMV_H_X = 67
    FMV_S_X = 68
    FMV_D_X = 69
    FMV_X_H = 70
    FMV_X_S = 71
    FMV_X_D = 72

    FSGNJ_H = 73
    FSGNJ_S = 74
    FSGNJ_D = 75

    FCVT_S_H = 76
    FCVT_H_S = 77
    FCVT_D_H = 78
    FCVT_H_D = 79
    FCVT_S_D = 80
    FCVT_D_S = 81

    FCVT_H_X = 82
    FCVT_S_X = 83
    FCVT_D_X = 84

    FCVT_X_H = 85
    FCVT_X_S = 86
    FCVT_X_D = 87

    CMPR_H = 88
    CMPR_S = 89
    CMPR_D = 90

    FCLASS_H = 91
    FCLASS_S = 92
    FCLASS_D = 93

    FMINMAX_H = 94
    FMINMAX_S = 95
    FMINMAX_D = 96

    FADD_H = 97
    FSUB_H = 98
    FMUL_H = 99
    FADD_S = 100
    FSUB_S = 101
    FMUL_S = 102
    FADD_D = 103
    FSUB_D = 104
    FMUL_D = 105

    FMADD_H = 106
    FMSUB_H = 107
    FNMADD_H = 108
    FNMSUB_H = 109
    FMADD_S = 110
    FMSUB_S = 111
    FNMADD_S = 112
    FNMSUB_S = 113
    FMADD_D = 114
    FMSUB_D = 115
    FNMADD_D = 116
    FNMSUB_D = 117

    FDIV_H = 118
    FDIV_S = 119
    FDIV_D = 120
    FSQRT_H = 121
    FSQRT_S = 122
    FSQRT_D = 123

    WFI = 124
    ERET = 125
    SFENCE = 126

    GPU_TMC = 127
    GPU_WSPAWN = 128
    GPU_SPLIT = 129
    GPU_JOIN = 130
    GPU_BARRIER = 131
    GPU_PRED = 132
    GPU_RAST = 133

    # Zicond
    CZERO_EQZ = 160
    CZERO_NEZ = 161

    # Zba
    ADD_UW = 162
    SH1ADD = 163
    SH1ADD_UW = 164
    SH2ADD = 165
    SH2ADD_UW = 166
    SH3ADD = 167
    SH3ADD_UW = 168
    SLLI_UW = 169

    # Zbb
    MAX = 170
    MIN = 171
    MAXU = 172
    MINU = 173
    ANDN = 174
    ORN = 175
    XNOR = 176
    UNARY = 177
    UNARYW = 178
    ROL = 179
    ROLW = 180
    ROR = 181
    RORI = 182
    RORW = 183
    RORIW = 184

    # Zbs
    BCLR = 185
    BCLRI = 186
    BEXT = 187
    BEXTI = 188
    BINV = 189
    BINVI = 190
    BSET = 191
    BSETI = 192

    # V
    VSETVL = 193
    VSETVLI = 194
    VSETIVLI = 195
    VEC = 196


class RegisterType(IntEnum):
    X = 0
    FIX = 1
    FLT = 2
    PAS = 3
    VEC = 4


class IssueQueueType(IntEnum):
    INT = 1
    MEM = 2
    FP = 4
    VEC = 8

    FMEM = FP | MEM


class BranchType(IntEnum):
    X = 0
    NE = 1
    EQ = 2
    GE = 3
    GEU = 4
    LT = 5
    LTU = 6
    J = 7
    JR = 8


class OpA(IntEnum):
    RS1 = 0
    ZERO = 1
    PC = 2
    RS1SHL = 3


class OpB(IntEnum):
    RS2 = 0
    IMM = 1
    ZERO = 2
    NEXT = 3
    IMMC = 4
    RS2OH = 5
    IMMOH = 6


class ImmSel(IntEnum):
    X = 0
    I = 1
    S = 2
    B = 3
    U = 4
    J = 5
    V = 6


class ALUOperator(IntEnum):
    ADD = 0
    SL = 1
    SEQ = 2
    SNE = 3
    XOR = 4
    SR = 5
    OR = 6
    AND = 7
    CZEQZ = 8
    CZNEZ = 9
    SUB = 10
    SRA = 11
    SLT = 12
    SGE = 13
    SLTU = 14
    SGEU = 15
    UNARY = 16
    ROL = 17
    ROR = 18
    BEXT = 19

    ANDN = 24
    ORN = 25
    XNOR = 26

    MAX = 28
    MIN = 29
    MAXU = 30
    MINU = 31

    MUL = ADD
    MULH = SL
    MULHSU = SEQ
    MULHU = SNE

    DIV = XOR
    DIVU = SR
    REM = OR
    REMU = AND


class ALUWidth(IntEnum):
    DW_XLEN = 0
    DW_32 = 1


class CFIType(IntEnum):
    X = 0
    JAL = 1
    JALR = 2
    BR = 3


class FUType(IntEnum):
    X = 0
    ALU = 1
    JMP = 2
    MEM = 4
    MUL = 8
    DIV = 16
    CSR = 32
    FPU = 64
    FDIV = 128
    I2F = 256
    F2I = 512
    F2IMEM = F2I | MEM
    GPU = 1024
    VEC = 2048
    F2IVEC = F2I | VEC


class PCSel(IntEnum):
    PC_PLUS_4 = 0
    BRJMP = 1
    JALR = 2


class MemoryCommand(IntEnum):
    X = 0
    READ = 1
    WRITE = 2
    AMO_ADD = 3
    AMO_XOR = 4
    AMO_OR = 5
    AMO_AND = 6
    AMO_MIN = 7
    AMO_MAX = 8
    AMO_MINU = 9
    AMO_MAXU = 10
    AMO_SWAP = 11
    LR = 12
    SC = 13
    SFENCE = 14

    @staticmethod
    def is_read(cmd):
        return (cmd == MemoryCommand.READ) | (cmd == MemoryCommand.LR) | (
            cmd == MemoryCommand.SC) | MemoryCommand.is_amo(cmd)

    @staticmethod
    def is_write(cmd):
        return (cmd == MemoryCommand.WRITE) | (
            cmd == MemoryCommand.SC) | MemoryCommand.is_amo(cmd)

    @staticmethod
    def is_amo_logical(cmd):
        return (cmd == MemoryCommand.AMO_SWAP) | (
            cmd == MemoryCommand.AMO_XOR) | (cmd == MemoryCommand.AMO_OR) | (
                cmd == MemoryCommand.AMO_AND)

    @staticmethod
    def is_amo_arithmetic(cmd):
        return (cmd == MemoryCommand.AMO_ADD) | (
            cmd == MemoryCommand.AMO_MIN) | (cmd == MemoryCommand.AMO_MAX) | (
                cmd == MemoryCommand.AMO_MINU) | (cmd
                                                  == MemoryCommand.AMO_MAXU)

    @staticmethod
    def is_amo(cmd):
        return MemoryCommand.is_amo_logical(
            cmd) | MemoryCommand.is_amo_arithmetic(cmd)


class CSRCommand(IntEnum):
    X = 0
    R = 2
    I = 4
    W = 5
    S = 6
    C = 7


class RoundingMode(IntEnum):
    RNE = 0
    RTZ = 1
    RDN = 2
    RUP = 3
    RMM = 4


class PrivilegeMode(IntEnum):
    U = 0
    S = 1
    H = 2
    M = 3
