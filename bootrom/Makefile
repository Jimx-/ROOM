MEMORY_START=0x80000000

CC = $(RISCV)/bin/riscv64-unknown-elf-gcc
OBJCOPY = $(RISCV)/bin/riscv64-unknown-elf-objcopy

CFLAGS = -march=rv64im -mabi=lp64 -nostdlib -nostartfiles -I$(RISCV)/riscv64-unknown-elf/include/ \
			-DMEMORY_START=$(MEMORY_START) -static -Ttext=10000

BIN = bootrom.bin

all: $(BIN)

$(BIN): bootrom
	$(OBJCOPY) -O binary --only-section .text $^ $@

bootrom: ../rtlsim/dromajo/bootrom/bootrom.S
	$(CC) $(CFLAGS) -o $@ $^

clean:
	rm -f $(BIN)
