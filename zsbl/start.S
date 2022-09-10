.global _start

.text
_start:
    la   sp, _stack_top
    tail main
