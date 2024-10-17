; RUN: run-emulator-test.sh %s $GB_TEST_PATH/functions.ll -O3 \
; RUN:   | FileCheck %s -check-prefix=EXPECT
; RUN: run-emulator-test.sh %s $GB_TEST_PATH/functions.ll -O0 \
; RUN:   | FileCheck %s -check-prefix=EXPECT

.global untyped_fn_symbol
untyped_fn:
    ld a, 0x99
    ret

untyped_fn_symbol:
    .short untyped_fn

.global _start
_start:
    di
    ld a, 0x00
    ld hl, 0x0000
; EXPECT: a=00
; EXPECT: hl=0000
    debugtrap

    ld b, 0x01
    ld c, 0x02
    ld de, 0x0403
    push de

    call argument0
; EXPECT: a=01
    debugtrap

    call argument1
; EXPECT: a=02
    debugtrap

    call argument2
; EXPECT: a=03
    debugtrap

    call argument3
; EXPECT: a=04
    debugtrap

    pop de

    ld hl, 0x1234
    ld bc, 0x5678
    push bc
    call argument0_i16
; EXPECT: a=34
    debugtrap

    ld hl, 0x1234
    call argument1_i16
; EXPECT: a=78
    debugtrap

    pop bc

    call call_argument2
; EXPECT: a=02
    debugtrap

    call call_argument1_i16
; EXPECT: a=01
    debugtrap

    ld b, 0x11
    call call_argument2_with_locals
; EXPECT: a=11
    debugtrap

    call call_argument3
; EXPECT: a=03
    debugtrap

_large_return:
    add sp, -4
    ld hl, sp, 0
    call large_return
    pop hl
; EXPECT: Debug trap!
; EXPECT: hl=0001
    debugtrap
    pop hl
; EXPECT: hl=0000
    debugtrap

_call_large_return:
    add sp, -4
    ld hl, sp, 0
    call call_large_return
    pop hl
; EXPECT: Debug trap!
; EXPECT: hl=0001
    debugtrap
    pop hl
; EXPECT: hl=0000
    debugtrap

    call empty16
; EXPECT: hl=0001
    debugtrap

    ld hl, 0x7777
    call test_spill_arg16
; EXPECT: hl=7777
    debugtrap

    ld b, 0x11
    call test_spill_arg8
; EXPECT: a=11
    debugtrap

    call call_untyped_fn
; EXPECT: a=99
    debugtrap
_end:
    trap
