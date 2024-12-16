.global __ashlsi3
// hl (return addr), bc (i32 bottom), de (i32 top)
// sp - 3 (shift amount)
__ashlsi3:

    push hl
    ld hl, sp + 4
    ld a, (hl)          // a == shift amount

    or a
    jr z, __shift_loop_end
__shift_loop_begin:

    sla c
    rl b
    rl e
    rl d

    dec a
    jr nz, __shift_loop_begin

__shift_loop_end:
    // Load result into return address
    pop hl
    ld a, c
    ldi (hl), a
    ld a, b
    ldi (hl), a
    ld a, e
    ldi (hl), a
    ld (hl), d

    ret
