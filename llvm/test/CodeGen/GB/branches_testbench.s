# RUN: run-emulator-test.sh %s $GB_TEST_PATH/branches.ll -O3 \
# RUN:   | FileCheck %s -check-prefix=EXPECT
# RUN: run-emulator-test.sh %s $GB_TEST_PATH/branches.ll -O0 \
# RUN:   | FileCheck %s -check-prefix=EXPECT

.global _start
_start:
    di
    ld a, 0x00
    ld hl, 0x0000
# EXPECT: a=00
# EXPECT: hl=0000
    debugtrap

    ld b, 0x10
    ld c, 0x01
    call simple_branch
# EXPECT: a=00
    debugtrap

_sge:
    ld b, 0x10
    ld c, 0x01
    call sge
# EXPECT: a=01
    debugtrap

    ld b, 0x01
    ld c, 0x02
    call sge
# EXPECT: a=00
    debugtrap

    ld b, 0x80
    ld c, 0x80
    call sge
# EXPECT: a=01
    debugtrap

    ld b, 0xff
    ld c, 0x01
    call sge
# EXPECT: a=00
    debugtrap

    ld b, 0xfe
    ld c, 0x7f
    call sge
# EXPECT: a=00
    debugtrap

    ld b, 0xff
    ld c, 0x7f
    call sge
# EXPECT: a=00
    debugtrap

    ld b, 0x7f
    ld c, 0xfe
    call sge
# EXPECT: a=01
    debugtrap


_sgt:
    ld b, 0x10
    ld c, 0x01
    call sgt
# EXPECT: a=01
    debugtrap

    ld b, 0x01
    ld c, 0x02
    call sgt
# EXPECT: a=00
    debugtrap

    ld b, 0x80
    ld c, 0x80
    call sgt
# EXPECT: a=00
    debugtrap

    ld b, 0xff
    ld c, 0x01
    call sgt
# EXPECT: a=00
    debugtrap


    ld b, 0xfe
    ld c, 0x7f
    call sgt
# EXPECT: a=00
    debugtrap

    ld b, 0xff
    ld c, 0x7f
    call sgt
# EXPECT: a=00
    debugtrap

    ld b, 0x7f
    ld c, 0xfe
    call sgt
# EXPECT: a=01
    debugtrap

    ld b, 0
    call sgt_0
# EXPECT: a=00
    debugtrap

    ld b, 127
    call sgt_0
# EXPECT: a=01
    debugtrap

    ld b, 1
    call sgt_0
# EXPECT: a=01
    debugtrap

    ld b, 0xff
    call sgt_0
# EXPECT: a=00
    debugtrap

    ld b, 0
    call sgt_1
# EXPECT: a=00
    debugtrap

    ld b, 127
    call sgt_1
# EXPECT: a=01
    debugtrap

    ld b, 1
    call sgt_1
# EXPECT: a=00
    debugtrap

    ld b, 0xff
    call sgt_1
# EXPECT: a=00
    debugtrap

    ld b, 0
    call sgt_n1
# EXPECT: a=01
    debugtrap

    ld b, 127
    call sgt_n1
# EXPECT: a=01
    debugtrap

    ld b, 1
    call sgt_n1
# EXPECT: a=01
    debugtrap

    ld b, 0xff
    call sgt_n1
# EXPECT: a=00
    debugtrap

    ld b, 0
    call sgt_n127
# EXPECT: a=01
    debugtrap

    ld b, 127
    call sgt_n127
# EXPECT: a=01
    debugtrap

    ld b, 1
    call sgt_n127
# EXPECT: a=01
    debugtrap

    ld b, 0xff
    call sgt_n127
# EXPECT: a=01
    debugtrap

    ld b, 0x80
    call sgt_n127
# EXPECT: a=00
    debugtrap

_sle:
    ld b, 0x10
    ld c, 0x01
    call sle
# EXPECT: a=00
    debugtrap

    ld b, 0x01
    ld c, 0x02
    call sle
# EXPECT: a=01
    debugtrap

    ld b, 0x80
    ld c, 0x80
    call sle
# EXPECT: a=01
    debugtrap

    ld b, 0xff
    ld c, 0x01
    call sle
# EXPECT: a=01
    debugtrap


    ld b, 0xfe
    ld c, 0x7f
    call sle
# EXPECT: a=01
    debugtrap

    ld b, 0xff
    ld c, 0x7f
    call sle
# EXPECT: a=01
    debugtrap

    ld b, 0x7f
    ld c, 0xfe
    call sle
# EXPECT: a=00
    debugtrap

    ld b, 0
    call sle_0
# EXPECT: a=01
    debugtrap

    ld b, 127
    call sle_0
# EXPECT: a=00
    debugtrap

    ld b, 1
    call sle_0
# EXPECT: a=00
    debugtrap

    ld b, 0xff
    call sle_0
# EXPECT: a=01
    debugtrap

_slt:
    ld b, 0x10
    ld c, 0x01
    call slt
# EXPECT: a=00
    debugtrap

    ld b, 0x01
    ld c, 0x02
    call slt
# EXPECT: a=01
    debugtrap

    ld b, 0x80
    ld c, 0x80
    call slt
# EXPECT: a=00
    debugtrap

    ld b, 0xff
    ld c, 0x01
    call slt
# EXPECT: a=01
    debugtrap

    ld b, 0xfe
    ld c, 0x7f
    call slt
# EXPECT: a=01
    debugtrap

    ld b, 0xff
    ld c, 0x7f
    call slt
# EXPECT: a=01
    debugtrap

    ld b, 0x7f
    ld c, 0xfe
    call slt
# EXPECT: a=00
    debugtrap


_uge:
    ld b, 0x10
    ld c, 0x01
    call uge
# EXPECT: a=01
    debugtrap

    ld b, 0x01
    ld c, 0x02
    call uge
# EXPECT: a=00
    debugtrap

    ld b, 0x80
    ld c, 0x80
    call uge
# EXPECT: a=01
    debugtrap

    ld b, 0xff
    ld c, 0x01
    call uge
# EXPECT: a=01
    debugtrap

_ugt:
    ld b, 0x10
    ld c, 0x01
    call ugt
# EXPECT: a=01
    debugtrap

    ld b, 0x01
    ld c, 0x02
    call ugt
# EXPECT: a=00
    debugtrap

    ld b, 0x80
    ld c, 0x80
    call ugt
# EXPECT: a=00
    debugtrap

    ld b, 0xff
    ld c, 0x01
    call ugt
# EXPECT: a=01
    debugtrap

_ule:
    ld b, 0x10
    ld c, 0x01
    call ule
# EXPECT: a=00
    debugtrap

    ld b, 0x01
    ld c, 0x02
    call ule
# EXPECT: a=01
    debugtrap

    ld b, 0x80
    ld c, 0x80
    call ule
# EXPECT: a=01
    debugtrap

    ld b, 0xff
    ld c, 0x01
    call ule
# EXPECT: a=00
    debugtrap

_ult:
    ld b, 0x10
    ld c, 0x01
    call ult
# EXPECT: a=00
    debugtrap

    ld b, 0x01
    ld c, 0x02
    call ult
# EXPECT: a=01
    debugtrap

    ld b, 0x80
    ld c, 0x80
    call ult
# EXPECT: a=00
    debugtrap

    ld b, 0xff
    ld c, 0x01
    call ult
# EXPECT: a=00
    debugtrap

_eq:
    ld b, 0x10
    ld c, 0x01
    call eq
# EXPECT: a=00
    debugtrap

    ld b, 0x01
    ld c, 0x01
    call eq
# EXPECT: a=01
    debugtrap

_ne:
    ld b, 0x10
    ld c, 0x01
    call ne
# EXPECT: a=01
    debugtrap

    ld b, 0x01
    ld c, 0x01
    call ne
# EXPECT: a=00
    debugtrap

_phi:
    ld b, 0x01
    call phi
# EXPECT: a=04
    debugtrap

    ld b, 0x00
    call phi
# EXPECT: a=05
    debugtrap

_select:
    ld b, 0x01
    call select
# EXPECT: a=04
    debugtrap

    ld b, 0x00
    call select
# EXPECT: a=05
    debugtrap

_jt:
    ld bc, 0x1234
    push bc
    ld hl, sp, 0
    ld b, 1
    call jt
    pop bc
# EXPECT: Debug trap!
# EXPECT: c=04
    debugtrap

    push bc
    ld hl, sp, 0
    ld b, 2
    call jt
    pop bc
# EXPECT: c=03
    debugtrap

_end:
    trap
