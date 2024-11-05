_init:
    ld hl, __gb_data_start
    ld bc, __gb_data_load_addr
    ld de, __gb_data_end

__init_loop_start:
    ld a, h
    cp d
    jr nz, __init_loop_body

    ld a, l
    cp e
    ret z

__init_loop_body:
    ; Load byte from __gb_data_load_addr to __gb_data_start
    ld a, (bc)
    ldi (hl), a
    inc bc

    jr __init_loop_start



.global _start
_start:
    di
    ld sp, 0xD000

    call _init

    ld a, 0
    ld hl, 0
    call main

    trap
