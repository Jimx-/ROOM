#ifndef _COMPLIANCE_MODEL_H
#define _COMPLIANCE_MODEL_H

#include "generated/platform.h"

// clang-format off
#define RVMODEL_DATA_SECTION \
        .pushsection .tohost,"aw",@progbits;                            \
        .align 8; .global tohost; tohost: .dword 0;                     \
        .align 8; .global fromhost; fromhost: .dword 0;                 \
        .popsection;                                                    \
        .align 8; .global begin_regstate; begin_regstate:               \
        .word 128;                                                      \
        .align 8; .global end_regstate; end_regstate:                   \
        .word 4;
// clang-format on

#ifndef RVMODEL_PMP_GRAIN
#define RVMODEL_PMP_GRAIN 0
#endif

#ifndef RVMODEL_NUM_PMPS
#define RVMODEL_NUM_PMPS 16
#endif

// clang-format off

// RV_COMPLIANCE_HALT
#define RVMODEL_HALT                ;\
    la t0, begin_signature          ;\
    la t1, end_signature            ;\
    sub t1, t1, t0                  ;\
    andi t2, t0, 63                 ;\
    sub t0, t0, t2                  ;\
    add t1, t1, t2                  ;\
    add t1, t1, t0                  ;\
    srli t0, t0, 4                  ;\
    srli t1, t1, 4                  ;\
    li t2, L2CACHE_FLUSH32_ADDRESS  ;\
1:                                  ;\
    sw t0, (t2)                     ;\
2:                                  ;\
    lw x1, (t2)                     ;\
    bnez x1, 2b                     ;\
    addi t0, t0, 4                  ;\
    bgtu t1, t0, 1b                 ;\
3:                                  ;\
    wfi                             ;\
    j 3b

// clang-format on

#define RVMODEL_BOOT

// clang-format off

//RV_COMPLIANCE_DATA_BEGIN
#define RVMODEL_DATA_BEGIN                                              \
  RVMODEL_DATA_SECTION                                                        \
  .align 4;\
  .global begin_signature; begin_signature:

//RV_COMPLIANCE_DATA_END
#define RVMODEL_DATA_END                                                      \
  .align 4;\
  .global end_signature; end_signature:

// clang-format on

// RVTEST_IO_INIT
#define RVMODEL_IO_INIT
// RVTEST_IO_WRITE_STR
#define RVMODEL_IO_WRITE_STR(_R, _STR)
// RVTEST_IO_CHECK
#define RVMODEL_IO_CHECK()
// RVTEST_IO_ASSERT_GPR_EQ
#define RVMODEL_IO_ASSERT_GPR_EQ(_S, _R, _I)
// RVTEST_IO_ASSERT_SFPR_EQ
#define RVMODEL_IO_ASSERT_SFPR_EQ(_F, _R, _I)
// RVTEST_IO_ASSERT_DFPR_EQ
#define RVMODEL_IO_ASSERT_DFPR_EQ(_D, _R, _I)

#define RVMODEL_SET_MSW_INT \
    li t1, 1;               \
    li t2, 0x2000000;       \
    sw t1, 0(t2);

#define RVMODEL_CLEAR_MSW_INT \
    li t2, 0x2000000;         \
    sw x0, 0(t2);

#define RVMODEL_CLEAR_MTIMER_INT

#define RVMODEL_CLEAR_MEXT_INT

#endif // _COMPLIANCE_MODEL_H
