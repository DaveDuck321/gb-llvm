MEMORY {
  rom0 (rx)  : ORIGIN = 0x0000, LENGTH = 16K
  romN (rx)  : ORIGIN = 0x4000, LENGTH = 16K
  vram (rw)   : ORIGIN = 0x8000, LENGTH = 8K
  xram (rwx)  : ORIGIN = 0xA000, LENGTH = 8K
  ram0 (rwx)  : ORIGIN = 0xC000, LENGTH = 4K
  ramN (rwx)  : ORIGIN = 0xD000, LENGTH = 4K
  hram (rwx)  : ORIGIN = 0xFF80, LENGTH = 127
}

SECTIONS {
  .rst 0x0000 : {
    /* We emulate `call (HL)` with RST00 */
    BYTE(0xE9)    /* JP (HL) */
    . = DEFINED( rst08 ) ? 0x0008 : .; KEEP(*(.text.__gb_rst08))
    . = DEFINED( rst10 ) ? 0x0010 : .; KEEP(*(.text.__gb_rst10))
    . = DEFINED( rst18 ) ? 0x0018 : .; KEEP(*(.text.__gb_rst18))
    . = DEFINED( rst20 ) ? 0x0020 : .; KEEP(*(.text.__gb_rst20))
    . = DEFINED( rst28 ) ? 0x0028 : .; KEEP(*(.text.__gb_rst28))
    . = DEFINED( rst30 ) ? 0x0030 : .; KEEP(*(.text.__gb_rst30))
    . = DEFINED( rst38 ) ? 0x0038 : .; KEEP(*(.text.__gb_rst38))
    . = 0x0040;
  } > rom0 = 0xd3d3d3d3 /* trap */

  .int 0x0040 : {
    . = 0x0040; KEEP(*(.text.__gb_int_vblank))
    . = 0x0048; KEEP(*(.text.__gb_int_lcd_status))
    . = 0x0050; KEEP(*(.text.__gb_int_timer))
    . = 0x0058; KEEP(*(.text.__gb_int_serial))
    . = 0x0060; KEEP(*(.text.__gb_int_input))
    . = 0x0068;
  } > rom0 = 0xd3d3d3d3 /* trap */

  .entry 0x0100 : {
    BYTE(0)         /* NOP */
    BYTE(0xC3)      /* JP */
    SHORT(_start)
  } > rom0

  .logo 0x0104 : {
    KEEP(*(.logo*))
    . = 0x0134;
  } > rom0 = 0

  .meta 0x0134 : {
    KEEP(*(.meta*))
    . = 0x0150;
  } > rom0 = 0

  .rodata : { *(.rodata*) } > rom0
  .init_array : { KEEP(*(.init_array*)) } > rom0
  .fini_array : { KEEP(*(.fini_array*)) } > rom0
  .text : { *(.text*) } > rom0

  /* Data (VMA: RAM, LMA: ROM Switchable bank) */
  . = ORIGIN(ram0);
  .data . : { *(.data*) } > ram0 AT> romN

  . = ALIGN(8); /* No technical reason, just prevent ld warnings */
  .bss . (NOLOAD) : { *(.bss*) } > ram0

  /* TODO: everything else */
}

__gb_data_start = ADDR(.data);
__gb_data_end = __gb_data_start + SIZEOF(.data);
__gb_data_load_addr = LOADADDR(.data);

__gb_bss_start = ADDR(.bss);
__gb_bss_end = __gb_bss_start + SIZEOF(.bss);
