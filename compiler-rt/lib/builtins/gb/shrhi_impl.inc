#ifndef __shift_instr
#error "__shift_instr must be defined to use this generic implementation"
#endif

#ifndef __shift_fn
#error "__shift_fn must be defined to use this generic implementation"
#endif

.global __shift_fn
// hl (return/value), bc (amount)
__shift_fn:
    ld a, c     // a == shift amount

    or a
    ret z

__shift_loop_begin:

    __shift_instr h
    rr l

    dec a
    jr nz, __shift_loop_begin

    ret // hl is already setup
