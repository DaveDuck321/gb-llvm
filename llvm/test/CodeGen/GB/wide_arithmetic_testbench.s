# RUN: run-emulator-test.sh %s $GB_TEST_PATH/wide_arithmetic.ll -O3 \
# RUN:   | FileCheck %s -check-prefix=EXPECT
# RUN: run-emulator-test.sh %s $GB_TEST_PATH/wide_arithmetic.ll -O0 \
# RUN:   | FileCheck %s -check-prefix=EXPECT

.global __ashlhi3
__ashlhi3:
    ret

.global __lshrhi3
__lshrhi3:
    ret

.global __ashrhi3
__ashrhi3:
    ret

.global _start
_start:
    di
    ld a, 0x00
    ld hl, 0x0000
# EXPECT: a=00
# EXPECT: hl=0000
    debugtrap

    ld bc, 0x8673
    ld hl, 0x3254
    call and
# EXPECT: hl=0250
    debugtrap

    ld hl, 0x3253
    call andi
# EXPECT: hl=0001
    debugtrap

    ld bc, 0x8673
    ld hl, 0x3254
    call xor
# EXPECT: hl=b427
    debugtrap

    ld hl, 0x3254
    call xori
# EXPECT: hl=3255
    debugtrap

    ld bc, 0x8673
    ld hl, 0x3254
    call or
# EXPECT: hl=b677
    debugtrap

    ld hl, 0x8673
    ld b, 0x71
    call sub16
# EXPECT: hl=8602
    debugtrap

    ld bc, 0x1010
    ld hl, 0x7776
    call sless_than
    and 0x01
# EXPECT: a=00
    debugtrap

    ld bc, 0x7776
    ld hl, 0x1010
    call sless_than
    and 0x01
# EXPECT: a=01
    debugtrap

    ld bc, 0x1010
    ld hl, 0x7776
    call sgreater_than
    and 0x01
# EXPECT: a=01
    debugtrap

    ld bc, 0x7776
    ld hl, 0x1010
    call sgreater_than
    and 0x01
# EXPECT: a=00
    debugtrap

    # Allocate space for return
    add sp - 4

    # Result = this + 3214
    ld bc, 6540
    ld de, 0
    ld hl, sp + 0 # Point to return memory
    call add32

    pop hl
# EXPECT: hl=261a
    debugtrap

    pop hl
# EXPECT: hl=0000
    debugtrap

    ld hl, 0x8066
    call shl16_c2
# EXPECT: hl=0198
    debugtrap

    ld hl, 0x8066
    call shl16_c9
# EXPECT: hl=cc00
    debugtrap

    ld hl, 0x8066
    call asr16_c2
# EXPECT: hl=e019
    debugtrap

    ld hl, 0x8066
    call asr16_c9
# EXPECT: hl=ffc0
    debugtrap

    ld hl, 0x8066
    call lsr16_c2
# EXPECT: hl=2019
    debugtrap

    ld hl, 0x8066
    call lsr16_c9
# EXPECT: hl=0040
    debugtrap

_end:
    trap
