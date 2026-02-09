# RUN: run-emulator-test.sh %s $GB_TEST_PATH/misc.ll -O3 \
# RUN:   | FileCheck %s -check-prefix=EXPECT
# RUN: run-emulator-test.sh %s $GB_TEST_PATH/misc.ll -O0 \
# RUN:   | FileCheck %s -check-prefix=EXPECT

.global memcpy
memcpy:

.global _start
_start:
    di
    ld a, 0x00
    ld hl, 0x0000
# EXPECT: a=00
# EXPECT: hl=0000
    debugtrap

    ld hl, 0x1234
    push hl
    ld hl, 0x5678
    push hl
    call trunc_i32
# EXPECT: a=78
    debugtrap

    ld hl, 0x1234
    call trunc_i16
# EXPECT: a=34
    debugtrap

    ld b, 0x12
    call trunc_i1
    and 1
# EXPECT: a=00
    debugtrap

    ld b, 0x13
    call trunc_i1
    and 1
# EXPECT: a=01
    debugtrap

    ld b, 0x03
    call sext8_i1
# EXPECT: a=ff
    debugtrap

    ld b, 0xfe
    call sext8_i1
# EXPECT: a=00
    debugtrap

    ld b, 0x03
    call sext16_i1
# EXPECT: Debug trap!
# EXPECT: hl=ffff
    debugtrap

    ld b, 0xfe
    call sext16_i1
# EXPECT: hl=0000
    debugtrap

    ld b, 0x83
    call sext16_i8
# EXPECT: hl=ff83
    debugtrap

    ld b, 0x73
    call sext16_i8
# EXPECT: hl=0073
    debugtrap

    ld b, 0x81
    call zext16_i8
# EXPECT: hl=0081
    debugtrap

    ld b, 0x71
    call zext16_i8
# EXPECT: hl=0071
    debugtrap

    ld b, 0xfe
    call ctpop
# EXPECT: a=07
    debugtrap

    ld b, 0x00
    call ctpop
# EXPECT: a=00
    debugtrap

    ld b, 0xfe
    call cttz
# EXPECT: a=01
    debugtrap

    ld b, 0x00
    call cttz
# EXPECT: a=08
    debugtrap

    ld b, 0xfe
    call ctlz
# EXPECT: a=00
    debugtrap

    ld b, 0x01
    call ctlz
# EXPECT: a=07
    debugtrap

_end:
    trap
