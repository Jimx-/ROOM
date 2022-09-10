#include "serial.h"

int main()
{
    serial_init();

    serial_printf("RISC-V Zero Stage Bootloader V0.1\n");

    for (;;)
        ;

    return 0;
}
