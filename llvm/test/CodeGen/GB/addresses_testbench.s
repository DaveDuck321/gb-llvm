# RUN: run-emulator-test.sh %s $GB_TEST_PATH/addresses.ll -O3 \
# RUN:   | FileCheck %s -check-prefix=EXPECT
# RUN: run-emulator-test.sh %s $GB_TEST_PATH/addresses.ll -O0 \
# RUN:   | FileCheck %s -check-prefix=EXPECT

    .data
.global external_u16
external_u16:
    .short = 0

    .text
.global _start
_start:
    di
# Startup routine: copy static init data into RAM
_startup:
    ld a, 0x00
    ld hl, __gb_data_start
    ld bc, __gb_data_load_addr
    ld de, __gb_data_end

_loop_start:
# Exit loop if hl == __gb_data_end
    ld a, l
    cp e
    jr nz, _loop_body

    ld a, h
    cp d
    jr z, _main
_loop_body:
    ld a, (bc)
    ldi (HL), a
    inc bc
    jr _loop_start

_main:
    call test_addresses
# EXPECT: a=02
    debugtrap

    call load_i16_global
# EXPECT: hl=1992
    debugtrap

    call test_save_i16_to_external
# EXPECT: hl=0000
    debugtrap

    ld hl, external_u16
    ldi a, (hl)
# EXPECT: a=12
    debugtrap
    ld a, (hl)
# EXPECT: a=11
    debugtrap

_end:
    trap
