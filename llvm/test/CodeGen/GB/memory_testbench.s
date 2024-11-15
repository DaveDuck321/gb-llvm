; RUN: run-emulator-test.sh %s $GB_TEST_PATH/memory.ll -O3 \
; RUN:   | FileCheck %s -check-prefix=EXPECT
; RUN: run-emulator-test.sh %s $GB_TEST_PATH/memory.ll -O0 \
; RUN:   | FileCheck %s -check-prefix=EXPECT

.global _start
_start:
    di
    ld a, 0x00
    ld hl, 0x0000
; EXPECT: a=00
; EXPECT: hl=0000
    debugtrap

    ld b, 13
    ld hl, 0xc0fe
    call store8

    ld bc, 0x1234
    ld hl, 0xc0ff
    call store16

    ld hl, 0xc0fd
    or 1
    ld b, a
    call store1

    ld hl, 0xc0fe
    call load8
; EXPECT: Debug trap!
; EXPECT: a=0d
    debugtrap

    ld hl, 0xc0ff
    call load16
; EXPECT: Debug trap!
; EXPECT: hl=1234
    debugtrap

    ld hl, 0xc0fd
    call load1
    and 1
; EXPECT: a=01
    debugtrap

    ld hl, 0xc0fd
    call load_sext
; EXPECT: a=ff
    debugtrap

    ld hl, 0xc0fd
    call load_zext
; EXPECT: a=01
    debugtrap


    ld b, 0xff
    call simple_stack
    and 1
; EXPECT: a=01
    debugtrap

    ld b, 0xfe
    call simple_stack
    and 1
; EXPECT: a=00
    debugtrap

    ld hl, 0xc0f0
    or 1
    ld b, a
    call store_trunc

    ld hl, 0xc0f0
    call load1
    and 1
; EXPECT: a=01
    debugtrap

_end:
    trap
