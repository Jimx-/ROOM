CC	= $(RISCV)/bin/riscv64-unknown-elf-gcc
LD	= $(RISCV)/bin/riscv64-unknown-elf-ld
OBJCOPY	= $(RISCV)/bin/riscv64-unknown-elf-objcopy

CFLAGS	= -march=rv32im -mabi=ilp32 -nostdlib -nostartfiles -g
CFLAGS	+= -I../include/ -I$(RISCV)/riscv64-unknown-elf/include/
LDFLAGS	= -melf32lriscv -Tlinker.ld

SRCS	= start.S main.c serial.c vsprintf.c sd.c
OBJS	= $(patsubst %.c, %.o, $(patsubst %.S, %.o, $(patsubst %.asm, %.o, $(SRCS))))
DEPS	= $(OBJS:.o=.d)

ELF	= zsbl.elf
BIN	= zsbl.bin

all: $(ELF) $(BIN)

clean:
	rm -f $(BIN) $(ELF) $(OBJS)

$(BIN): $(ELF)
	$(OBJCOPY) -O binary --only-section .text $^ $@

$(ELF) : $(OBJS)
	$(LD) $(LDFLAGS) -o $@ $^

-include $(DEPS)

%.o: %.c
	$(CC) $(CFLAGS) -MP -MMD -c -o $@ $<

%.o: %.S
	$(CC) $(CFLAGS) -MP -MMD -c -D__ASSEMBLY__ -o  $@ $<