#ifndef _GROOM_INTRINSICS_H_
#define _GROOM_INTRINSICS_H_

#include "csr.h"

typedef void (*gpu_thread_func_t)();

#define __if(pred)   \
    gpu_split(pred); \
    if (pred)

#define __else else

#define __endif gpu_join();

#ifdef __cplusplus
extern "C"
{
#endif

    static inline void gpu_tmc(unsigned int tmask)
    {
        asm volatile(".insn s 0x6b, 0, %0, 0(x0)" ::"r"(tmask));
    }

    static inline void gpu_wspawn(unsigned int num_warps, gpu_thread_func_t fp)
    {
        asm volatile(".insn s 0x6b, 1, %0, 0(%1)" ::"r"(num_warps), "r"(fp));
    }

    static inline void gpu_split(int pred)
    {
        asm volatile(".insn s 0x6b, 2, %0, 0(x0)" ::"r"(pred));
    }

    static inline void gpu_join(void)
    {
        asm volatile(".insn s 0x6b, 3, x0, 0(x0)");
    }

    inline void gpu_barrier(unsigned int barried_id, unsigned int num_warps)
    {
        asm volatile (".insn s 0x6b, 4, %1, 0(%0)" :: "r"(barried_id), "r"(num_warps));
    }


    static inline int gpu_thread_id(void)
    {
        int result;
        asm volatile("csrr %0, %1" : "=r"(result) : "i"(CSR_WTID));
        return result;
    }

    static inline int gpu_thread_local_id(void)
    {
        int result;
        asm volatile("csrr %0, %1" : "=r"(result) : "i"(CSR_LTID));
        return result;
    }

    static inline int gpu_thread_global_id(void)
    {
        int result;
        asm volatile("csrr %0, %1" : "=r"(result) : "i"(CSR_GTID));
        return result;
    }

    static inline int gpu_warp_id(void)
    {
        int result;
        asm volatile("csrr %0, %1" : "=r"(result) : "i"(CSR_LWID));
        return result;
    }

    static inline int gpu_warp_global_id(void)
    {
        int result;
        asm volatile("csrr %0, %1" : "=r"(result) : "i"(CSR_GWID));
        return result;
    }

    static inline int gpu_core_id(void)
    {
        int result;
        asm volatile("csrr %0, %1" : "=r"(result) : "i"(CSR_GCID));
        return result;
    }

    static inline unsigned int gpu_thread_mask(void)
    {
        unsigned int result;
        asm volatile("csrr %0, %1" : "=r"(result) : "i"(CSR_TMASK));
        return result;
    }

    static inline int gpu_num_threads(void)
    {
        unsigned int result;
        asm volatile("csrr %0, %1" : "=r"(result) : "i"(CSR_MNT));
        return result;
    }

    static inline int gpu_num_warps(void)
    {
        unsigned int result;
        asm volatile("csrr %0, %1" : "=r"(result) : "i"(CSR_MNW));
        return result;
    }

    static inline int gpu_num_cores(void)
    {
        unsigned int result;
        asm volatile("csrr %0, %1" : "=r"(result) : "i"(CSR_MNC));
        return result;
    }

#ifdef __cplusplus
}
#endif

#endif