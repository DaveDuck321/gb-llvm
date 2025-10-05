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
# EXPECT-CYCLE-O3: 93
# EXPECT-CYCLE-Oz: 91
# EXPECT: a=d2
    debugtrap

    ld b, 50
    ld c, 4
    call multiply_i8
# EXPECT-CYCLE-O3: 150
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

    ld bc, 51
    ld de, 63
    call multiply_i16
# EXPECT-CYCLE-O3: 648
# EXPECT-CYCLE-Oz: 637
# EXPECT: hl=0c8d
    debugtrap


    ld bc, 3600
    ld de, 8
    call multiply_i16
# EXPECT-CYCLE-O3: 1242
# EXPECT-CYCLE-Oz: 1201
# EXPECT: hl=7080
    debugtrap

    ld bc, 3600
    call floating_point_add
# EXPECT-CYCLE-O3: 4316
# EXPECT-CYCLE-Oz: 4316
# EXPECT: hl=0e59
    debugtrap


    ld bc, 50
    call floating_point_mul
# EXPECT-CYCLE-O3: 21696
# EXPECT-CYCLE-Oz: 21696
# FIXME: 02fe
# EXPECT-FAILING: hl=0302
    debugtrap


    ld bc, 0x80
    ld d, 4
    call shl_i16
# EXPECT-CYCLE-O3: 68
# EXPECT-CYCLE-Oz: 68
# EXPECT: hl=0800
    debugtrap

    ld bc, 0xa5ad
    ld d, 7
    call lshr_i16
# EXPECT-CYCLE-O3: 92
# EXPECT-CYCLE-Oz: 92
# EXPECT: hl=014b
    debugtrap

    ld bc, 0xa5ad
    ld d, 7
    call ashr_i16
# EXPECT-CYCLE-O3: 92
# EXPECT-CYCLE-Oz: 92
# EXPECT: hl=ff4b
    debugtrap

    ret
