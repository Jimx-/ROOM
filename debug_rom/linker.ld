OUTPUT_ARCH( "riscv" )
ENTRY( entry )
SECTIONS
{
    . = 0x300;
    .trampoline :
    {
        *(.trampoline)
    }

    . = 0x800;
    .text :
    {
        *(.text)
    }

    _end = .;
}
