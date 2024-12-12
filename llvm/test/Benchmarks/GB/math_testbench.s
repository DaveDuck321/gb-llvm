; RUN: run-clang-test.sh %s $GB_TEST_PATH/math.cpp -O3 \
; RUN:   | FileCheck %s -check-prefixes=EXPECT,EXPECT-CYCLE-O3
; RUN: run-clang-test.sh %s $GB_TEST_PATH/math.cpp -Oz \
; RUN:   | FileCheck %s -check-prefixes=EXPECT,EXPECT-CYCLE-Oz
; RUN: run-clang-test.sh %s $GB_TEST_PATH/math.cpp -O0 \
; RUN:   | FileCheck %s -check-prefix=EXPECT

.global main
main:
    debugtrap

    ld b, 7
    ld c, 30
    call multiply_i8
; EXPECT-CYCLE-O3: 114
; EXPECT-CYCLE-Oz: 114
; EXPECT: a=d2
    debugtrap

    ld b, 50
    ld c, 4
    call multiply_i8
; EXPECT-CYCLE-O3: 186
; EXPECT-CYCLE-Oz: 192
; EXPECT: a=c8
    debugtrap

; Note: for some reason the operands are flipped here
    ld a, 0
    ld b, 30
    ld c, 7
    call builtin_multiply
; EXPECT-CYCLE-O3: 119
; EXPECT-CYCLE-Oz: 119
; EXPECT: a=d2
    debugtrap

    ld hl, 51
    ld bc, 63
    call multiply_i16
; EXPECT-CYCLE-O3: 1112
; EXPECT-CYCLE-Oz: 854
; EXPECT: hl=0c8d
    debugtrap


    ld hl, 3600
    ld bc, 8
    call multiply_i16
; EXPECT-CYCLE-O3: 2136
; EXPECT-CYCLE-Oz: 1658
; EXPECT: hl=7080
    debugtrap

    ret
