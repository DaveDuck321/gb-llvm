; RUN: run-emulator-test.sh %s $GB_TEST_PATH/struct.ll -O3 \
; RUN:   | FileCheck %s -check-prefix=EXPECT
; RUN: run-emulator-test.sh %s $GB_TEST_PATH/struct.ll -O0 \
; RUN:   | FileCheck %s -check-prefix=EXPECT

.global _start
_start:
    di
    ld a, 0x00
    ld hl, 0x0000

    call main
; EXPECT: a=0a
    debugtrap

    ; Allocate stack space for return
    add sp, -5
    ld hl, sp+0
    ld b, 0x11
    ld c, 0x77
    call get_player

    ld hl, sp+0
    ld a, (hl)
; EXPECT: a=11
    debugtrap

    ld hl, sp+1
    ld a, (hl)
; EXPECT: a=77
    debugtrap


    ld hl, sp+2
    ld a, (hl)
; EXPECT: a=64
    debugtrap

    ld hl, sp+3
    ld a, (hl)
; EXPECT: a=64
    debugtrap

    ld hl, sp+4
    ld a, (hl)
; EXPECT: a=0a
    debugtrap
    add sp, 5

_end:
    trap
