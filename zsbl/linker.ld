OUTPUT_ARCH( "riscv" )
ENTRY( _start )

MEMORY
{
  ROM   (rx)  : ORIGIN = 0x00000000, LENGTH = 40K
  RAM   (rwx) : ORIGIN = 0x20000000, LENGTH = 32K
}

SECTIONS
{
  __stack_size = DEFINED(__stack_size) ? __stack_size : 0x1000;

  .text :
  {
    KEEP (*(SORT_NONE(.reset_handler)))
    KEEP (*(SORT_NONE(.vector_table)))
    *(.text .text.*)
    *(.tohost .tohost.*)
    *(.rodata .rodata.*)
    *(.srodata .srodata.*)
  } >ROM

  . = ALIGN(8);

  PROVIDE (__etext = .);
  PROVIDE (_etext = .);
  PROVIDE (etext = .);

  .data :
  {
    . = . + 8;
    _sdata = .;
    *(.rdata)
    *(.data .data.*)
    *(.sdata .sdata.*)
    . = ALIGN(8);
    _edata = .;
  } >RAM

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
    . = ALIGN(8);
    _ebss = .;
  } >RAM

  . = ALIGN(8);
  PROVIDE( _end = . );
  PROVIDE( end = . );

  .stack ORIGIN(RAM) + LENGTH(RAM) - __stack_size:
  {
    PROVIDE( _heap_end = . );
    . = __stack_size;
    PROVIDE( _stack_top = . );
  } >RAM
}
