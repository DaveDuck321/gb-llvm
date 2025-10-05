# RUN: run-graphene-test.sh %s $GB_TEST_PATH/memory.c3 -O3 \
# RUN:   | FileCheck %s -check-prefixes=EXPECT,EXPECT-CYCLE-O3
# RUN: run-graphene-test.sh %s $GB_TEST_PATH/memory.c3 -Oz \
# RUN:   | FileCheck %s -check-prefixes=EXPECT,EXPECT-CYCLE-Oz
# RUN: run-graphene-test.sh %s $GB_TEST_PATH/memory.c3 -O0 \
# RUN:   | FileCheck %s -check-prefix=EXPECT

.global main
main:
    # Allocate space for arrays

    # Benchmark memset
# EXPECT: Debug trap!
    debugtrap

    add sp, -100
    ld hl, sp + 0
    ld b, h
    ld c, l

    ld d, 0xee

    ld hl, 100
    push hl
    call _Z8g_memsetPA_hhs
    pop hl

    ld hl, sp + 16 # Test a random value
    ld a, (hl)
# EXPECT: Debug trap!
# EXPECT-CYCLE-O3: 1307
# EXPECT-CYCLE-Oz: 1297
# EXPECT: a=ee
    debugtrap

    add sp, -100
    ld hl, sp + 0
    ld b, h
    ld c, l

    ld d, 0x01

    ld hl, 100
    push hl
    call _Z8g_memsetPA_hhs
    pop hl

    ld hl, sp + 88 # Test a random value
    ld a, (hl)
# EXPECT: Debug trap!
# EXPECT-CYCLE-O3: 1307
# EXPECT-CYCLE-Oz: 1297
# EXPECT: a=01
    debugtrap

    ld hl, sp + 100     # src
    ld d, h
    ld e, l
    ld hl, sp + 0       # dst
    ld b, h
    ld c, l

    ld hl, 100          # Length
    push hl
    call _Z8g_memcpyPA_hKPA_hs
    pop hl

    ld hl, sp + 73 # Test a random value
    ld a, (hl)
# EXPECT: Debug trap!
# EXPECT-CYCLE-O3: 1450
# EXPECT-CYCLE-Oz: 1418
# EXPECT: a=ee
    debugtrap

    ld hl, sp + 99
    ld (hl), 0

    ld hl, sp, 0
    ld b, h
    ld c, l
    call _Z8g_strlenKPA_h
# EXPECT: Debug trap!
# EXPECT-CYCLE-O3: 1024
# EXPECT-CYCLE-Oz: 1024
# EXPECT: hl=0063
    debugtrap

    add sp, 100
    add sp, 100
    ret
