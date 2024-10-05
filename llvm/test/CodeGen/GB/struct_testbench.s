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
    debugtrap
; EXPECT: a=0a

_end:
    trap
