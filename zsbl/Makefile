CC	= $(RISCV)/bin/riscv64-unknown-elf-gcc
LD	= $(RISCV)/bin/riscv64-unknown-elf-ld
OBJCOPY	= $(RISCV)/bin/riscv64-unknown-elf-objcopy

CFLAGS	= -march=rv64imfdc -mabi=lp64d -mcmodel=medany -nostdlib -nostartfiles -g -O2
CFLAGS	+= -I../include/ -Ifat/src/ -I$(RISCV)/riscv64-unknown-elf/include/
LDFLAGS	= -melf64lriscv -Tlinker.ld

ifdef SIM
CFLAGS	+= -D__SIM__
endif

SRCS	= start.S main.c serial.c memset.S memcpy.S string.c vsprintf.c sd.c
SRCS	+= fat/src/fat_access.c fat/src/fat_filelib.c fat/src/fat_misc.c fat/src/fat_table.c \
			fat/src/fat_cache.c fat/src/fat_format.c fat/src/fat_string.c fat/src/fat_write.c
OBJS	= $(patsubst %.c, %.o, $(patsubst %.S, %.o, $(patsubst %.asm, %.o, $(SRCS))))
DEPS	= $(OBJS:.o=.d)

ELF	= zsbl.elf
BIN	= zsbl.bin

all: $(ELF) $(BIN)

clean:
	rm -f $(BIN) $(ELF) $(OBJS)

$(BIN): $(ELF)
	$(OBJCOPY) -O binary --only-section .text --only-section .text.* --only-section .sdata $^ $@

$(ELF) : $(OBJS)
	$(LD) $(LDFLAGS) -o $@ $^

-include $(DEPS)

%.o: %.c
	$(CC) $(CFLAGS) -MP -MMD -c -o $@ $<

%.o: %.S
	$(CC) $(CFLAGS) -MP -MMD -c -D__ASSEMBLY__ -o  $@ $<
