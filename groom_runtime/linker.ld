OUTPUT_ARCH( "riscv" )
ENTRY( _start )

SECTIONS
{
  . = 0x10000;

  .init :
  {
    KEEP (*(SORT_NONE(.init)))
  }

  .text :
  {
    KEEP (*(SORT_NONE(.reset_handler)))
    KEEP (*(SORT_NONE(.vector_table)))
    *(.text .text.*)
    *(.tohost .tohost.*)
    *(.rodata .rodata.*)
    *(.srodata .srodata.*)
  }

  . = ALIGN(4);

  PROVIDE (__etext = .);
  PROVIDE (_etext = .);
  PROVIDE (etext = .);

  . = 0x80000000;

  _sidata = .;
  .data :
  {
    . = . + 4;
    _sdata = .;
    *(.rdata)
    *(.data .data.*)
    . = ALIGN(16);
    __global_pointer = . + 0x800;
    *(.sdata .sdata.*)
    . = ALIGN(4);
    _edata = .;
  }

  PROVIDE( _edata = . );
  PROVIDE( edata = . );
  PROVIDE( _fbss = . );
  PROVIDE( __bss_start = . );

  .bss :
  {
    _sbss = .;
    *(.sbss*)
    *(.bss .bss.*)
    *(COMMON)
    . = ALIGN(4);
    _ebss = .;
  }

  . = ALIGN(8);
  PROVIDE( _end = . );
  PROVIDE( end = . );
}
