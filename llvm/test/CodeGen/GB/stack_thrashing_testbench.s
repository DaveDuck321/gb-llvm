; RUN: run-emulator-test.sh %s $GB_TEST_PATH/stack_thrashing.ll -O3 \
; RUN:   | FileCheck %s -check-prefix=EXPECT
; RUN: run-emulator-test.sh %s $GB_TEST_PATH/stack_thrashing.ll -O0 \
; RUN:   | FileCheck %s -check-prefix=EXPECT

.global _start
_start:
    di
    ld a, 0x00
    ld hl, 0x0000
    ld sp, 0xD000

    ; Allocate space for function return
    add sp, -4

    ; Pass first argument for function
    ld hl, 0x1234
    push hl
    ld hl, 0x5678
    push hl

    ; Point to the function return address
    ld hl, sp, 4
    call large_stack_i32_identity
    add sp, 4

    ; Check the next 4-bytes
    pop hl
; EXPECT: hl=5678
    debugtrap
    pop hl
; EXPECT: hl=1234
    debugtrap
_end:
    trap
