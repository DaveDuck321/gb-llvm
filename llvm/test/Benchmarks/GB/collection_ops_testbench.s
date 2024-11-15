; RUN: run-graphene-test.sh %s $GB_TEST_PATH/collection_ops.c3 -O3 \
; RUN:   | FileCheck %s -check-prefixes=EXPECT,EXPECT-CYCLE
; RUN: run-graphene-test.sh %s $GB_TEST_PATH/collection_ops.c3 -O0 \
; RUN:   | FileCheck %s -check-prefix=EXPECT

.global main
main:
    ; Allocate space for player1
    add sp, -10

    ; Benchmark get_player
; EXPECT: Debug trap!
    debugtrap

    ld b, 20
    ld c, 20
    ld hl, sp + 5
    call _Z10get_playercc

; EXPECT: Debug trap!
; EXPECT-CYCLE: 37
    debugtrap

    ld b, 30
    ld c, 30
    ld hl, sp + 0
    call _Z10get_playercc

; EXPECT: Debug trap!
; EXPECT-CYCLE: 37
    debugtrap

    ld hl, sp + 0
    ld b, 9
    call _Z6damageR6Playerc
    and 1

; EXPECT: Debug trap!
; EXPECT-CYCLE: 47
; EXPECT: a=00
    debugtrap

    ld hl, sp + 0
    ld b, 2
    call _Z6damageR6Playerc
    and 1

; EXPECT: Debug trap!
; EXPECT-CYCLE: 47
; EXPECT: a=01
    debugtrap

    ld hl, sp + 5
    ld b, h
    ld c, l
    ld hl, sp + 0
    call _Z10do_collideRK6PlayerRK6Player
    and 1

; EXPECT: Debug trap!
; EXPECT-CYCLE: 243
; EXPECT: a=01
    debugtrap

    add sp, 10
    ret
