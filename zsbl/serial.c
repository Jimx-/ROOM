#include "generated/platform.h"
#include "mmio.h"

#include <stdio.h>
#include <stdarg.h>

#define UART_SR_TXEMPTY (1 << 0)
#define UART_SR_TXBUSY  (1 << 2)

int vsprintf(char* buf, const char* fmt, va_list args);

void serial_init(void) { mmio_write32((void*)UART_ENABLED_ADDRESS, 1); }

void serial_putc(char c)
{
    unsigned int status;

    mmio_write32((void*)UART_DATA_ADDRESS, (unsigned int)c);

    for (;;) {
        status = mmio_read32((void*)UART_STATUS_ADDRESS);

        if (!(status & UART_SR_TXBUSY)) break;
    }
}

void serial_puts(const char* str)
{
    while (*str) {
        serial_putc(*str);
        str++;
    }
}

int serial_printf(const char* fmt, ...)
{
    int i;
    char buf[256];
    va_list arg;

    va_start(arg, fmt);
    i = vsnprintf(buf, sizeof(buf), fmt, arg);
    serial_puts(buf);

    va_end(arg);

    return i;
}
