.global _start

.section ".text.init"
_start:
    .option push
    .option norelax
    la  gp, __global_pointer$
    .option pop

    la  t0, trap_vector
    csrw mtvec, t0
    mv  t0, zero

    la   sp, _end + 63
    andi sp, sp, -64
    li   t0, 1
    slli t0, t0, 12
    add  sp, sp, t0

    mv   fp, zero

    call main

_hang:
    j _hang

trap_vector:
    j trap_vector
