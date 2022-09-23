.global _start

.text
_start:
    la  t0, trap_vector
    csrw mtvec, t0
    mv t0, zero

    la   sp, _stack_top
    call main

_hang:
    j _hang

trap_vector:
    j trap_vector
