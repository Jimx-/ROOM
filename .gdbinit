set arch riscv:rv64
mem 0 0x20000 rw
mem 0x20000000 0x20020000 rw
mem 0x20020000 0x20021000 rw
target extended-remote localhost:3333
