#ifndef _ZSBL_MMIO_H_
#define _ZSBL_MMIO_H_

#include <stdint.h>

static inline uint32_t mmio_read32(void* addr) {
    return *(volatile uint32_t*)addr;
}

static inline void mmio_write32(void* addr, uint32_t val) {
    *(volatile uint32_t*)addr = val;
}

#endif
