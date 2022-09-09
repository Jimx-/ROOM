    .option norvc
    .global entry

    #define HALTED      -0x800
    #define GOING       -(0x800 - 4)
    #define RESUMING    -(0x800 - 8)

    #define FLAGS       -0x400
    #define FLAG_GO     0x1
    #define FLAG_RESUME 0x2

    .text
entry:
    csrw dscratch1, s1
    j    _entry

_entry:
    csrw dscratch0, s0
    la   s1, entry
    csrr s0, mhartid
    sw   s0, HALTED(s1)

entry_loop:
    csrr s0, mhartid
    add  s0, s0, s1
    lbu  s0, FLAGS(s0)
    andi s0, s0, (FLAG_GO | FLAG_RESUME)
    beqz s0, entry_loop

    andi  s0, s0, FLAG_GO
    beqz  s0, _resume

    csrr s0, dscratch0

    sw    zero, GOING(s1)
    csrrw s1, dscratch1, s1
    j     trampoline

_resume:
    csrr    s0, mhartid
    sw      s0, RESUMING(s1)
    csrr    s0, dscratch0
    csrr    s1, dscratch1
    dret

    .section .trampoline
trampoline:
    nop
