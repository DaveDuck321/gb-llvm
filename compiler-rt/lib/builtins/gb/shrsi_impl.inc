#ifndef __shift_instr
#error "__shift_instr must be defined to use this generic implementation"
#endif

#ifndef __shift_fn
#error "__shift_fn must be defined to use this generic implementation"
#endif

.global __shift_fn
// hl (return addr), bc (i32 bottom), de (i32 top)
// sp + 2 (shift amount)
__shift_fn:

    push hl
    ld hl, sp + 4
    ld a, (hl)          // a == shift amount

    or a
    jr z, __shift_loop_end

__shift_loop_begin:

    __shift_instr d
    rr e
    rr b
    rr c

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
