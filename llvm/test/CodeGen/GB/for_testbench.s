; RUN: run-emulator-test.sh %s $GB_TEST_PATH/for2.ll -O3 \
; RUN:   | FileCheck %s -check-prefix=EXPECT

.global _start
_start:
    di
    ld a, 0x00
    ld hl, 0x0000
; EXPECT: a=00
; EXPECT: hl=0000

    call main
    and 1
; EXPECT: a=01
    debugtrap

_end:
    trap

; sp 14 has sp 0
;
; range @ 0xffee
;
