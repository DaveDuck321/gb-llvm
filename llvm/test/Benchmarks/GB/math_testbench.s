# RUN: run-clang-test.sh %s $GB_TEST_PATH/math.cpp $GB_TEST_PATH/noopt.cpp -O3 \
# RUN:   | FileCheck %s -check-prefixes=EXPECT,EXPECT-FAILING,EXPECT-CYCLE-O3
# RUN: run-clang-test.sh %s $GB_TEST_PATH/math.cpp $GB_TEST_PATH/noopt.cpp -Oz \
# RUN:   | FileCheck %s -check-prefixes=EXPECT,EXPECT-FAILING,EXPECT-CYCLE-Oz
# RUN: run-clang-test.sh %s $GB_TEST_PATH/math.cpp $GB_TEST_PATH/noopt.cpp -O0 \
# RUN:   | FileCheck %s -check-prefixes=EXPECT,EXPECT-FAILING

.global main
main:
    debugtrap

    ld b, 7
    ld c, 30
    call multiply_i8
# EXPECT-CYCLE-O3: 92
# EXPECT-CYCLE-Oz: 91
# EXPECT: a=d2
    debugtrap

    ld b, 50
    ld c, 4
    call multiply_i8
# EXPECT-CYCLE-O3: 146
# EXPECT-CYCLE-Oz: 148
# EXPECT: a=c8
    debugtrap

# Note: for some reason the operands are flipped here
    ld a, 0
    ld b, 30
    ld c, 7
    call builtin_multiply
# EXPECT-CYCLE-O3: 96
# EXPECT-CYCLE-Oz: 96
# EXPECT: a=d2
    debugtrap

    ld hl, 51
    ld bc, 63
    call multiply_i16
# EXPECT-CYCLE-O3: 747
# EXPECT-CYCLE-Oz: 391
# EXPECT: hl=0c8d
    debugtrap


    ld hl, 3600
    ld bc, 8
    call multiply_i16
# EXPECT-CYCLE-O3: 1443
# EXPECT-CYCLE-Oz: 733
# EXPECT: hl=7080
    debugtrap

    ld hl, 3600
    call floating_point_add
# EXPECT-CYCLE-O3: 4178
# EXPECT-CYCLE-Oz: 4178
# EXPECT: hl=0e59
    debugtrap


    ld hl, 50
    call floating_point_mul
# EXPECT-CYCLE-O3: 20573
# EXPECT-CYCLE-Oz: 20573
# FIXME: 02fe
# EXPECT-FAILING: hl=0302
    debugtrap


    ld hl, 0x80
    ld b, 4
    call shl_i16
# EXPECT-CYCLE-O3: 64
# EXPECT-CYCLE-Oz: 64
# EXPECT: hl=0800
    debugtrap

    ld hl, 0xa5ad
    ld b, 7
    call lshr_i16
# EXPECT-CYCLE-O3: 88
# EXPECT-CYCLE-Oz: 88
# EXPECT: hl=014b
    debugtrap

    ld hl, 0xa5ad
    ld b, 7
    call ashr_i16
# EXPECT-CYCLE-O3: 88
# EXPECT-CYCLE-Oz: 88
# EXPECT: hl=ff4b
    debugtrap

    ret
