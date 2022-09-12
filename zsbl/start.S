.global _start

.text
_start:
    la   sp, _stack_top
    call main

_hang:
    j _hang
