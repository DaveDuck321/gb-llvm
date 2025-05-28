; RUN: run-emulator-test.sh %s $GB_TEST_PATH/arithmetic.ll -O3 \
; RUN:   | FileCheck %s -check-prefix=EXPECT
; RUN: run-emulator-test.sh %s $GB_TEST_PATH/arithmetic.ll -O0 \
; RUN:   | FileCheck %s -check-prefix=EXPECT

.global __mulqi3
__mulqi3:
    ret

.global _start
_start:
    di
    ld a, 0x00
    ld hl, 0x0000
; EXPECT: a=00
; EXPECT: hl=0000
    debugtrap

    ld b, 0x03
    ld c, 0x07
    call add
; EXPECT: a=0a
    debugtrap

    ld b, 0xfe
    call inc
; EXPECT: a=ff
    debugtrap

    ld b, 0xfd
    call addi
; EXPECT: a=ff
    debugtrap

    ld bc, 0x1234
    push bc
    ld hl, sp, 0
    ld b, 0x02
    call add_hl
    pop bc
; EXPECT: a=36
    debugtrap

    ld b, 7
    ld c, 2
    call sub
; EXPECT: a=05
    debugtrap

    ld b, 7
    call subi
; EXPECT: a=05
    debugtrap

    ld b, 7
    call dec
; EXPECT: a=06
    debugtrap


    ld b, 0x06
    ld c, 0x14
    call and
; EXPECT: a=04
    debugtrap

    ld b, 0x15
    ld c, 0x14
    call xor
; EXPECT: a=01
    debugtrap

    ld b, 0x04
    ld c, 0x03
    call or
; EXPECT: a=07
    debugtrap

    ld hl, 0x1234
    call add16
; EXPECT: hl=1248
    debugtrap

    ld b, 5
    ld c, 6
    call setcc_eq
    and 0x01
; EXPECT: a=00
    debugtrap

    ld b, 7
    ld c, 7
    call setcc_eq
    and 0x01
; EXPECT: a=01
    debugtrap

    ld b, 1
    call setcc_eq_zero
    and 0x01
; EXPECT: a=00
    debugtrap

    ld b, 0
    call setcc_eq_zero
    and 0x01
; EXPECT: a=01
    debugtrap

    ld b, 1
    call setcc_ne_zero
    and 0x01
; EXPECT: a=01
    debugtrap

    ld b, 0
    call setcc_ne_zero
    and 0x01
; EXPECT: a=00
    debugtrap

    ld b, 5
    ld c, 6
    call setcc_ne
    and 0x01
; EXPECT: a=01
    debugtrap

    ld b, 7
    ld c, 7
    call setcc_ne
    and 0x01
; EXPECT: a=00
    debugtrap

_setcc_gt:
    ld b, 0xff
    ld c, 0xfe
    call setcc_gt
    and 0x01
; EXPECT: a=01
    debugtrap

    ld b, 0xfe
    ld c, 0xff
    call setcc_gt
    and 0x01
; EXPECT: a=00
    debugtrap

    ld b, 0x01
    ld c, 0xff
    call setcc_gt
    and 0x01
; EXPECT: a=01
    debugtrap

    ld b, 0x7f
    ld c, 0xff
    call setcc_gt
    and 0x01
; EXPECT: a=01
    debugtrap


    ld b, 0x7f
    ld c, 0xfe
    call setcc_gt
    and 0x01
; EXPECT: a=01
    debugtrap

    ld b, 0xff
    ld c, 0x7f
    call setcc_gt
    and 0x01
; EXPECT: a=00
    debugtrap


    ld b, 0xfe
    ld c, 0x7f
    call setcc_gt
    and 0x01
; EXPECT: a=00
    debugtrap


    ld b, 0xff
    ld c, 0x01
    call setcc_gt
    and 0x01
; EXPECT: a=00
    debugtrap

    ld b, 0xff
    ld c, 0xff
    call setcc_gt
    and 0x01
; EXPECT: a=00
    debugtrap

_setcc_lt:
    ld b, 0xff
    ld c, 0xfe
    call setcc_lt
    and 0x01
; EXPECT: a=00
    debugtrap

    ld b, 0xfe
    ld c, 0xff
    call setcc_lt
    and 0x01
; EXPECT: a=01
    debugtrap

    ld b, 0x01
    ld c, 0xff
    call setcc_lt
    and 0x01
; EXPECT: a=00
    debugtrap

    ld b, 0xff
    ld c, 0x01
    call setcc_lt
    and 0x01
; EXPECT: a=01
    debugtrap

    ld b, 0xff
    ld c, 0xff
    call setcc_lt
    and 0x01
; EXPECT: a=00
    debugtrap

    ld b, 0x7f
    ld c, 0xff
    call setcc_lt
    and 0x01
; EXPECT: a=00
    debugtrap


    ld b, 0x7f
    ld c, 0xfe
    call setcc_lt
    and 0x01
; EXPECT: a=00
    debugtrap

    ld b, 0xff
    ld c, 0x7f
    call setcc_lt
    and 0x01
; EXPECT: a=01
    debugtrap


    ld b, 0xfe
    ld c, 0x7f
    call setcc_lt
    and 0x01
; EXPECT: a=01
    debugtrap

_setcc_le:
    ld b, 0xff
    ld c, 0xfe
    call setcc_le
    and 0x01
; EXPECT: a=00
    debugtrap

    ld b, 0xfe
    ld c, 0xff
    call setcc_le
    and 0x01
; EXPECT: a=01
    debugtrap

    ld b, 0x01
    ld c, 0xff
    call setcc_le
    and 0x01
; EXPECT: a=00
    debugtrap

    ld b, 0xff
    ld c, 0x01
    call setcc_le
    and 0x01
; EXPECT: a=01
    debugtrap

    ld b, 0xff
    ld c, 0xff
    call setcc_le
    and 0x01
; EXPECT: a=01
    debugtrap

    ld b, 0x7f
    ld c, 0xff
    call setcc_le
    and 0x01
; EXPECT: a=00
    debugtrap


    ld b, 0x7f
    ld c, 0xfe
    call setcc_le
    and 0x01
; EXPECT: a=00
    debugtrap

    ld b, 0xff
    ld c, 0x7f
    call setcc_le
    and 0x01
; EXPECT: a=01
    debugtrap


    ld b, 0xfe
    ld c, 0x7f
    call setcc_le
    and 0x01
; EXPECT: a=01
    debugtrap

_setcc_ge:
    ld b, 0xff
    ld c, 0xfe
    call setcc_ge
    and 0x01
; EXPECT: a=01
    debugtrap

    ld b, 50
    ld c, 30
    call setcc_ge
    and 0x01
; EXPECT: a=01
    debugtrap

    ld b, 30
    ld c, 50
    call setcc_ge
    and 0x01
; EXPECT: a=00
    debugtrap

    ld b, 0xfe
    ld c, 0xff
    call setcc_ge
    and 0x01
; EXPECT: a=00
    debugtrap

    ld b, 0x01
    ld c, 0xff
    call setcc_ge
    and 0x01
; EXPECT: a=01
    debugtrap

    ld b, 0xff
    ld c, 0x01
    call setcc_ge
    and 0x01
; EXPECT: a=00
    debugtrap

    ld b, 0xff
    ld c, 0xff
    call setcc_ge
    and 0x01
; EXPECT: a=01
    debugtrap

    ld b, 0x7f
    ld c, 0xff
    call setcc_ge
    and 0x01
; EXPECT: a=01
    debugtrap


    ld b, 0x7f
    ld c, 0xfe
    call setcc_ge
    and 0x01
; EXPECT: a=01
    debugtrap

    ld b, 0xff
    ld c, 0x7f
    call setcc_ge
    and 0x01
; EXPECT: a=00
    debugtrap


    ld b, 0xfe
    ld c, 0x7f
    call setcc_ge
    and 0x01
; EXPECT: a=00
    debugtrap

_shifts:
    ld b, 0x04
    ld c, 0x03
    call shl
; EXPECT: a=20
    debugtrap

    ld b, 0x04
    ld c, 0x00
    call shl
; EXPECT: a=04
    debugtrap

    ld b, 0x04
    call shl_c
; EXPECT: a=10
    debugtrap

    ld b, 0x80
    ld c, 0x02
    call lshr
; EXPECT: a=20
    debugtrap

    ld b, 0x80
    call lshr_c
; EXPECT: a=20
    debugtrap

    ld b, 0x80
    ld c, 0x02
    call ashr
; EXPECT: a=e0
    debugtrap

    ld b, 0xe0
    call ashr_c
; EXPECT: a=f8
    debugtrap

    ld hl, 0xfedc
    call byte_swap
; EXPECT hl=fedc
    debugtrap

    ld b, 0x79
    call ctpop
; EXPECT: a=05
    debugtrap

    ld b, 0x18
    call cttz
; EXPECT: a=03
    debugtrap

    ld b, 9
    call ctlz
; EXPECT: a=04
    debugtrap


    ld b, 133
    call shl_1
; EXPECT: a=0a
    debugtrap

    ld b, 133
    call shl_2
; EXPECT: a=14
    debugtrap

    ld b, 133
    call shl_3
; EXPECT: a=28
    debugtrap

    ld b, 133
    call shl_4
; EXPECT: a=50
    debugtrap

    ld b, 133
    call shl_5
; EXPECT: a=a0
    debugtrap

    ld b, 133
    call shl_6
; EXPECT: a=40
    debugtrap

    ld b, 133
    call shl_7
; EXPECT: a=80
    debugtrap


    ld b, 133
    call lshr_1
; EXPECT: a=42
    debugtrap

    ld b, 133
    call lshr_2
; EXPECT: a=21
    debugtrap

    ld b, 133
    call lshr_3
; EXPECT: a=10
    debugtrap

    ld b, 133
    call lshr_4
; EXPECT: a=08
    debugtrap

    ld b, 133
    call lshr_5
; EXPECT: a=04
    debugtrap

    ld b, 133
    call lshr_6
; EXPECT: a=02
    debugtrap

    ld b, 133
    call lshr_7
; EXPECT: a=01
    debugtrap

_end:
    trap
