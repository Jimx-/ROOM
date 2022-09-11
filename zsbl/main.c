#include "serial.h"
#include "sd.h"

#include <stdint.h>

static inline uint32_t get_cycle32(void)
{
    unsigned int cycle;
    asm volatile("csrr %0, mcycle" : "=r"(cycle));
    return cycle;
}

void usleep(unsigned int us)
{
    uint32_t cycles0;
    uint32_t cycles1;

    cycles0 = get_cycle32();
    for (;;) {
        cycles1 = get_cycle32();
        if (cycles1 - cycles0 >= us * 50) break;
    }
}

int main()
{
    serial_init();

    serial_printf("RISC-V Zero Stage Bootloader V0.1\n");

    sd_init();

    for (;;)
        ;

    return 0;
}
