.global __ashlhi3
// hl (return/input), bc (shift amount)
__ashlhi3:
    ld a, c          // a == shift amount

    or a
    ret z

__shift_loop_begin:

    sla l
    rl h

    dec a
    jr nz, __shift_loop_begin

    ret // hl is already setup
