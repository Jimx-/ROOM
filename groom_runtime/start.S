    #include <groom/config.h>
    #include <groom/csr.h>

    #define STACKSIZE_LOG2  10

    .section .init, "ax"
    .global _start
    .type   _start, @function
_start:
    la  a0, init_regs
    csrr a1, CSR_MNW
    .insn s 0x6b, 1, a1, 0(a0)  // gpu_wspawn a1, a0
    call init_regs
    li a0, 1
    .insn s 0x6b, 0, a0, 0(x0)  //gpu_tmc 1

    call main

    li a0, 0
    .insn s 0x6b, 0, a0, 0(x0)  //gpu_tmc 0

    .section .text
    .type init_regs, @function
    .global init_regs
init_regs:
    li a0, -1                    // gpu_tmc -1
    .insn s 0x6b, 0, a0, 0(x0)

    .option push
    .option norelax
    la gp, __global_pointer
    .option pop

    li sp, SMEM_BASE
    #if USE_SMEM
    csrr a0, CSR_LTID
    #else
    csrr a0, CSR_GTID
    #endif
    add a0, a0, 1
    sll a1, a0, STACKSIZE_LOG2
    add sp, sp, a1

    csrr a3, CSR_LWID
    beqz a3, 1f
    li a0, 0
    .insn s 0x6b, 0, a0, 0(x0)  // gpu_tmc 0
1:
    ret
