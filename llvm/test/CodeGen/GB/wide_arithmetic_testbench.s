# RUN: run-emulator-test.sh %s $GB_TEST_PATH/wide_arithmetic.ll -O3 \
# RUN:   | FileCheck %s -check-prefix=EXPECT
# RUN: run-emulator-test.sh %s $GB_TEST_PATH/wide_arithmetic.ll -O0 \
# RUN:   | FileCheck %s -check-prefix=EXPECT

# Defined inline for testing but usually pulled in from compiler-rt
.global __ashlhi3
__ashlhi3:
    ld a, c
    or a
    ret z
__ashlhi3_shift_loop_begin:
    sla l
    rl h
    dec a
    jr nz, __ashlhi3_shift_loop_begin
    ret


.global __lshrhi3
__lshrhi3:
    ld a, c
    or a
    ret z
__lshrhi3_shift_loop_begin:
    srl h
    rr l
    dec a
    jr nz, __lshrhi3_shift_loop_begin
    ret


.global __ashrhi3
__ashrhi3:
    ld a, c
    or a
    ret z
__ashrhi3_shift_loop_begin:
    sra h
    rr l
    dec a
    jr nz, __ashrhi3_shift_loop_begin
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

    add sp, -4
    ld hl, sp + 0
    ld b, h
    ld c, l
    call float_constant

    pop hl
# EXPECT: hl=cccd
    debugtrap

    pop hl
# EXPECT: hl=3dcc
    debugtrap

_end:
    trap
