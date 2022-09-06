    .option norvc
    .global entry

    #define HALTED    -0x800

    .text
entry:
    j _entry

_entry:
    la  s1, entry
    csrr s0, mhartid
    sw  s0, HALTED(s1)

entry_loop:
    j entry_loop
