; RUN: run-graphene-test.sh %s $GB_TEST_PATH/memory.c3 -O3 \
; RUN:   | FileCheck %s -check-prefixes=EXPECT,EXPECT-CYCLE
; RUN: run-graphene-test.sh %s $GB_TEST_PATH/memory.c3 -O0 \
; RUN:   | FileCheck %s -check-prefix=EXPECT

.global main
main:
    ; Allocate space for arrays

    ; Benchmark memset
; EXPECT: Debug trap!
    debugtrap

    add sp, -100
    ld hl, 100
    push hl
    ld hl, sp + 2
    ld b, 0xee
    call _Z8g_memsetPA_hhs
    pop hl

    ld hl, sp + 16 ; Test a random value for sanity
    ld a, (hl)
; EXPECT: Debug trap!
; EXPECT-CYCLE: 2520
; EXPECT: a=ee
    debugtrap

    add sp, -100
    ld hl, 100
    push hl
    ld hl, sp + 2
    ld b, 0x01
    call _Z8g_memsetPA_hhs
    pop hl

    ld hl, sp + 16 ; Test a random value for sanity
    ld a, (hl)
; EXPECT: Debug trap!
; EXPECT-CYCLE: 2520
; EXPECT: a=01
    debugtrap

    add sp, 100
    add sp, 100
    ret
