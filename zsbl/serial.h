#ifndef _ZSBL_SERIAL_H_
#define _ZSBL_SERIAL_H_

void serial_init(void);

void serial_putc(char c);
void serial_puts(const char* str);
int serial_printf(const char* fmt, ...) __attribute__((format(printf, 1, 2)));

#endif
