#include "serial.h"
#include "sd.h"
#include "fat_filelib.h"

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

    fl_init();

    if (fl_attach_media(sd_read_sector, sd_write_sector) != FAT_INIT_OK) {
        serial_printf("ERROR: Failed to init file system\n");
        return -1;
    }

    fl_listdirectory("/");

    return 0;
}
