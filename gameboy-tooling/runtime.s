; void _gb_init_data(void)
_gb_init_data:
    ld hl, __gb_data_start
    ld bc, __gb_data_load_addr
    ld de, __gb_data_end

  __gb_init_data_loop_start:
    ld a, h
    cp d
    jr nz, __gb_init_data_loop_body

    ld a, l
    cp e
    ret z

  __gb_init_data_loop_body:
    ; Load byte from __gb_data_load_addr to __gb_data_start
    ld a, (bc)
    ldi (hl), a
    inc bc

    jr __gb_init_data_loop_start



; void _gb_init_bss(void)
_gb_init_bss:
    ld hl, __gb_bss_start
    ld bc, __gb_bss_end

  __gb_init_bss_loop_start:
    ld a, h
    cp b
    jr nz, __gb_init_bss_loop_body

    ld a, l
    cp c
    ret z

  __gb_init_bss_loop_body:
    ; Zero initialize the bss
    ld a, 0
    ldi (hl), a

    jr __gb_init_bss_loop_start



; void _gb_call_fn_ptr_list(_, void* start, void* end)
_gb_call_fn_ptr_list:
    ; bc = start/ current ptr
    ; de = end

  __gb_call_fn_ptr_list_loop_start:
    ld a, b
    cp d
    jr nz, __gb_call_fn_ptr_list_loop_body

    ld a, c
    cp e
    ret z

  __gb_call_fn_ptr_list_loop_body:
    ; Load the next function pointer from .init_array
    ld a, (bc)
    ld l, a
    inc bc
    ld a, (bc)
    ld h, a
    inc bc

    push bc
    push de
    call (hl)
    pop de
    pop bc

    jr __gb_call_fn_ptr_list_loop_start


.global _start
_start:
    di
    ld sp, 0xD000

    call _gb_init_data
    call _gb_init_bss

    ld bc, __init_array_start
    ld de, __init_array_end
    call _gb_call_fn_ptr_list

    ld a, 0
    ld hl, 0
    call main

    debugtrap

    ld bc, __fini_array_start
    ld de, __fini_array_end
    call _gb_call_fn_ptr_list

    trap
