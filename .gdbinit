set arch riscv:rv64

mem 0x10000 0x11000 ro
mem 0x80000000 0x80020000 rw
mem 0xc0000000 0xc0020000 rw

target extended-remote localhost:3333
