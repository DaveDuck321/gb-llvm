# RUN: run-graphene-test.sh %s $GB_TEST_PATH/collection_ops.c3 -O3 \
# RUN:   | FileCheck %s -check-prefixes=EXPECT,EXPECT-CYCLE-O3
# RUN: run-graphene-test.sh %s $GB_TEST_PATH/collection_ops.c3 -Oz \
# RUN:   | FileCheck %s -check-prefixes=EXPECT,EXPECT-CYCLE-Oz
# RUN: run-graphene-test.sh %s $GB_TEST_PATH/collection_ops.c3 -O0 \
# RUN:   | FileCheck %s -check-prefix=EXPECT

.global main
main:
    # Allocate space for player1
    add sp, -10

    # Benchmark get_player
# EXPECT: Debug trap!
    debugtrap

    ld d, 20
    ld e, 20
    ld hl, sp + 5
    ld b, h
    ld c, l
    call _Z10get_playercc

# EXPECT: Debug trap!
# EXPECT-CYCLE-O3: 44
# EXPECT-CYCLE-Oz: 44
    debugtrap

    ld d, 30
    ld e, 30
    ld hl, sp + 0
    ld b, h
    ld c, l
    call _Z10get_playercc

# EXPECT: Debug trap!
# EXPECT-CYCLE-O3: 44
# EXPECT-CYCLE-Oz: 44
    debugtrap

    ld hl, sp + 0
    ld b, h
    ld c, l
    ld d, 9
    call _Z6damageR6Playerc
    and 1

# EXPECT: Debug trap!
# EXPECT-CYCLE-O3: 55
# EXPECT-CYCLE-Oz: 55
# EXPECT: a=00
    debugtrap

    ld hl, sp + 0
    ld d, 2
    ld b, h
    ld c, l
    call _Z6damageR6Playerc
    and 1

# EXPECT: Debug trap!
# EXPECT-CYCLE-O3: 52
# EXPECT-CYCLE-Oz: 52
# EXPECT: a=01
    debugtrap

    ld hl, sp + 5
    ld d, h
    ld e, l
    ld hl, sp + 0
    ld b, h
    ld c, l
    call _Z10do_collideRK6PlayerRK6Player
    and 1

# EXPECT: Debug trap!
# EXPECT-CYCLE-O3: 238
# EXPECT-CYCLE-Oz: 248
# EXPECT: a=01
    debugtrap

    add sp, 10
    ret
