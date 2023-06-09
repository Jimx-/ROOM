CC	= $(RISCV)/bin/riscv64-unknown-elf-gcc
LD	= $(RISCV)/bin/riscv64-unknown-elf-ld
AR	= $(RISCV)/bin/riscv64-unknown-elf-ar
OBJCOPY	= $(RISCV)/bin/riscv64-unknown-elf-objcopy

CFLAGS	= -march=rv32imf -mabi=ilp32 -g
CFLAGS	+= -I../include/

SRCS	= start.S
OBJS	= $(patsubst %.c, %.o, $(patsubst %.S, %.o, $(patsubst %.asm, %.o, $(SRCS))))
DEPS	= $(OBJS:.o=.d)

LIB	= libgroomrt.a

all: $(LIB)

clean:
	rm -f $(LIB) $(OBJS)

$(LIB) : $(OBJS)
	$(AR) rcs $@ $^

-include $(DEPS)

%.o: %.c
	$(CC) $(CFLAGS) -MP -MMD -c -o $@ $<

%.o: %.S
	$(CC) $(CFLAGS) -MP -MMD -c -D__ASSEMBLY__ -o  $@ $<
