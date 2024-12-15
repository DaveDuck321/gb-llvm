; RUN: run-clang-test.sh %s $GB_TEST_PATH/math.cpp $GB_TEST_PATH/noopt.cpp -O3 \
; RUN:   | FileCheck %s -check-prefixes=EXPECT,EXPECT-CYCLE-O3
; RUN: run-clang-test.sh %s $GB_TEST_PATH/math.cpp $GB_TEST_PATH/noopt.cpp -Oz \
; RUN:   | FileCheck %s -check-prefixes=EXPECT,EXPECT-CYCLE-Oz
; RUN: run-clang-test.sh %s $GB_TEST_PATH/math.cpp $GB_TEST_PATH/noopt.cpp -O0 \
; RUN:   | FileCheck %s -check-prefix=EXPECT

.global main
main:
    debugtrap

    ld b, 7
    ld c, 30
    call multiply_i8
; EXPECT-CYCLE-O3: 104
; EXPECT-CYCLE-Oz: 101
; EXPECT: a=d2
    debugtrap

    ld b, 50
    ld c, 4
    call multiply_i8
; EXPECT-CYCLE-O3: 167
; EXPECT-CYCLE-Oz: 167
; EXPECT: a=c8
    debugtrap

; Note: for some reason the operands are flipped here
    ld a, 0
    ld b, 30
    ld c, 7
    call builtin_multiply
; EXPECT-CYCLE-O3: 106
; EXPECT-CYCLE-Oz: 106
; EXPECT: a=d2
    debugtrap

    ld hl, 51
    ld bc, 63
    call multiply_i16
; EXPECT-CYCLE-O3: 1075
; EXPECT-CYCLE-Oz: 818
; EXPECT: hl=0c8d
    debugtrap


    ld hl, 3600
    ld bc, 8
    call multiply_i16
; EXPECT-CYCLE-O3: 2063
; EXPECT-CYCLE-Oz: 1610
; EXPECT: hl=7080
    debugtrap

    ld hl, 3600
    call floating_point_add
; EXPECT-CYCLE-O3: 4972
; EXPECT-CYCLE-Oz: 4972
; EXPECT: hl=0e59
    debugtrap


    ld hl, 50
    call floating_point_mul
; EXPECT-CYCLE-O3: 25716
; EXPECT-CYCLE-Oz: 25716
; EXPECT: hl=02fe
    debugtrap

    ret
