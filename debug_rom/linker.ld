OUTPUT_ARCH( "riscv" )
ENTRY( entry )
SECTIONS
{
    . = 0x0;
    .text :
    {
        *(.text)
    }
    _end = .;
}
