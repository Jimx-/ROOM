CC = $(RISCV)/bin/riscv64-unknown-elf-gcc
OBJCOPY = $(RISCV)/bin/riscv64-unknown-elf-objcopy

CFLAGS = -march=rv32im -mabi=ilp32 -nostdlib -nostartfiles -I$(RISCV)/riscv64-unknown-elf/include/ -Tlinker.ld

BIN = debug_rom.bin

all: $(BIN)

$(BIN): debug_rom
	$(OBJCOPY) -O binary --only-section .text $^ $@

%: %.S
	$(CC) $(CFLAGS) -o $@ $^

clean:
	rm -f $(BIN)
