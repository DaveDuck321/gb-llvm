# RUN: run-emulator-test.sh %s $GB_TEST_PATH/atomics.ll -O3 \
# RUN:   | FileCheck %s -check-prefix=EXPECT
# RUN: run-emulator-test.sh %s $GB_TEST_PATH/atomics.ll -O0 \
# RUN:   | FileCheck %s -check-prefix=EXPECT

.section .data
val:
    .byte 0

.section .text
.global _start
_start:
    ld bc, val
    call store_i8
    ld a, (val)
    debugtrap
    # EXPECT: a=02

    ld bc, val
    call load_i8
    debugtrap
    # EXPECT: a=02

    ld bc, val
    call atomic_dec
    ld a, (val)
    debugtrap
    # EXPECT: a=01

    ld a, 0xfe
    ld (val), a
    ld bc, val
    call atomic_fetch_dec
    debugtrap
    # EXPECT: a=fe
    ld a, (val)
    debugtrap
    # EXPECT: a=fd

    ld bc, val
    call atomic_inc
    ld a, (val)
    debugtrap
    # EXPECT: a=fe


    ld a, 0xd0
    ld (val), a
    ld bc, val
    call atomic_add
    ld a, (val)
    debugtrap
    # EXPECT: a=d2

    ld bc, val
    call atomic_fetch_inc
    debugtrap
    # EXPECT: a=d2
    ld a, (val)
    debugtrap
    # EXPECT: a=d3


    ld bc, val
    ld d, 0xd3
    ld e, 4
    call cmpxchg_i8
    and 1
    debugtrap
    # EXPECT: a=01
    ld a, (val)
    debugtrap
    # EXPECT: a=04

    ld bc, val
    ld d, 3
    ld e, 5
    call cmpxchg_i8
    and 1
    debugtrap
    # EXPECT: a=00
    ld a, (val)
    debugtrap
    # EXPECT: a=04


    ld bc, val
    ld d, 6
    call atomic_swap
    debugtrap
    # EXPECT: a=04
    ld a, (val)
    debugtrap
    # EXPECT: a=06

    ld bc, val
    ld d, 12
    call atomic_and
    debugtrap
    # EXPECT: a=06
    ld a, (val)
    debugtrap
    # EXPECT: a=04

    ld bc, val
    ld d, 12
    call atomic_nand
    debugtrap
    # EXPECT: a=04
    ld a, (val)
    debugtrap
    # EXPECT: a=fb

    ld a, 6
    ld (val), a

    ld bc, val
    ld d, 8
    call atomic_or
    debugtrap
    # EXPECT: a=06
    ld a, (val)
    debugtrap
    # EXPECT: a=0e


    ld bc, val
    ld d, 0x18
    call atomic_xor
    debugtrap
    # EXPECT: a=0e
    ld a, (val)
    debugtrap
    # EXPECT: a=16

# atomic_max
    ld bc, val
    ld d, 0x15
    call atomic_max
    debugtrap
    # EXPECT: a=16
    ld a, (val)
    debugtrap
    # EXPECT: a=16

    ld bc, val
    ld d, 0x17
    call atomic_max
    debugtrap
    # EXPECT: a=16
    ld a, (val)
    debugtrap
    # EXPECT: a=17

    ld bc, val
    ld d, 0xfe
    call atomic_max
    debugtrap
    # EXPECT: a=17
    ld a, (val)
    debugtrap
    # EXPECT: a=17


# atomic_min
    ld bc, val
    ld d, 0x15
    call atomic_min
    debugtrap
    # EXPECT: a=17
    ld a, (val)
    debugtrap
    # EXPECT: a=15

    ld bc, val
    ld d, 0x17
    call atomic_min
    debugtrap
    # EXPECT: a=15
    ld a, (val)
    debugtrap
    # EXPECT: a=15

    ld bc, val
    ld d, 0xfe
    call atomic_min
    debugtrap
    # EXPECT: a=15
    ld a, (val)
    debugtrap
    # EXPECT: a=fe

# atomic_umax
    ld a, 0x16
    ld (val), a

    ld bc, val
    ld d, 0x15
    call atomic_umax
    debugtrap
    # EXPECT: a=16
    ld a, (val)
    debugtrap
    # EXPECT: a=16

    ld bc, val
    ld d, 0x17
    call atomic_umax
    debugtrap
    # EXPECT: a=16
    ld a, (val)
    debugtrap
    # EXPECT: a=17

    ld bc, val
    ld d, 0xfe
    call atomic_umax
    debugtrap
    # EXPECT: a=17
    ld a, (val)
    debugtrap
    # EXPECT: a=fe


# atomic_umin
    ld a, 0x17
    ld (val), a

    ld bc, val
    ld d, 0x15
    call atomic_umin
    debugtrap
    # EXPECT: a=17
    ld a, (val)
    debugtrap
    # EXPECT: a=15

    ld bc, val
    ld d, 0x17
    call atomic_umin
    debugtrap
    # EXPECT: a=15
    ld a, (val)
    debugtrap
    # EXPECT: a=15

    ld bc, val
    ld d, 0xfe
    call atomic_umin
    debugtrap
    # EXPECT: a=15
    ld a, (val)
    debugtrap
    # EXPECT: a=15


    ld a, 0x15
    ld (val), a

    ld bc, val
    call atomic_set
    ld a, (val)
    debugtrap
    # EXPECT: a=17


    ld a, 0xf0
    ld (val), a

    ld bc, val
    call atomic_reset
    ld a, (val)
    debugtrap
    # EXPECT: a=d0

    trap
