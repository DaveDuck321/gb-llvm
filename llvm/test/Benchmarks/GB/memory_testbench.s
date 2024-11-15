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
    ld de, 100
    ld hl, sp + 0
    ld b, 0xee
    call _Z8g_memsetPA_hhs

    ld hl, sp + 16 ; Test a random value
    ld a, (hl)
; EXPECT: Debug trap!
; EXPECT-CYCLE: 1969
; EXPECT: a=ee
    debugtrap

    add sp, -100
    ld de, 100
    ld hl, sp + 0
    ld b, 0x01
    call _Z8g_memsetPA_hhs

    ld hl, sp + 88 ; Test a random value
    ld a, (hl)
; EXPECT: Debug trap!
; EXPECT-CYCLE: 1969
; EXPECT: a=01
    debugtrap

    ld de, 100          ; Length
    ld hl, sp + 100     ; src
    ld b, h
    ld c, l
    ld hl, sp + 0       ; dst
    call _Z8g_memcpyPA_hKPA_hs

    ld hl, sp + 73 ; Test a random value
    ld a, (hl)
; EXPECT: Debug trap!
; EXPECT-CYCLE: 2284
; EXPECT: a=ee
    debugtrap

    ld hl, sp + 99
    ld (hl), 0

    ld hl, sp, 0
    call _Z8g_strlenKPA_h
; EXPECT: Debug trap!
; EXPECT-CYCLE: 1630
; EXPECT: hl=0063
    debugtrap

    add sp, 100
    add sp, 100
    ret
