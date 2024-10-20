init:
    ld hl, __gb_data_start
    ld bc, __gb_data_load_addr
    ld de, __gb_data_end

_loop_start:
; Exit loop if hl == __gb_data_end
    ld a, l
    cp e
    jr nz, _loop_body

    ld a, h
    cp d
    jr nz, _loop_body
    ret
_loop_body:
    ld a, (bc)
    ldi (hl), a
    inc bc
    jr _loop_start


.global _start
_start:
    di
    ld sp, 0xD000

    call init

    ld a, 0
    ld hl, 0
    call main

    trap
