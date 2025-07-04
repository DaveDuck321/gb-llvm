; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py UTC_ARGS: --version 3
; RUN: llc -mtriple=gb -verify-machineinstrs -O3 < %s \
; RUN:   | FileCheck %s -check-prefix=GBI-O3

define i16 @and(i16 %b, i16 %c) nounwind {
; GBI-O3-LABEL: and:
; GBI-O3:       # %bb.0:
; GBI-O3-NEXT:    ld a, h
; GBI-O3-NEXT:    and b
; GBI-O3-NEXT:    ld d, a
; GBI-O3-NEXT:    ld a, l
; GBI-O3-NEXT:    and c
; GBI-O3-NEXT:    ld l, a
; GBI-O3-NEXT:    ld h, d
; GBI-O3-NEXT:    ret
  %1 = and i16 %b, %c
  ret i16 %1
}

define i16 @andi(i16 %b) nounwind {
; GBI-O3-LABEL: andi:
; GBI-O3:       # %bb.0:
; GBI-O3-NEXT:    ld b, $00
; GBI-O3-NEXT:    ld a, l
; GBI-O3-NEXT:    and $01
; GBI-O3-NEXT:    ld l, a
; GBI-O3-NEXT:    ld h, b
; GBI-O3-NEXT:    ret
  %1 = and i16 %b, 1
  ret i16 %1
}

define i16 @xor(i16 %b, i16 %c) nounwind {
; GBI-O3-LABEL: xor:
; GBI-O3:       # %bb.0:
; GBI-O3-NEXT:    ld a, h
; GBI-O3-NEXT:    xor b
; GBI-O3-NEXT:    ld d, a
; GBI-O3-NEXT:    ld a, l
; GBI-O3-NEXT:    xor c
; GBI-O3-NEXT:    ld l, a
; GBI-O3-NEXT:    ld h, d
; GBI-O3-NEXT:    ret
  %1 = xor i16 %b, %c
  ret i16 %1
}

define i16 @xori(i16 %b) nounwind {
; GBI-O3-LABEL: xori:
; GBI-O3:       # %bb.0:
; GBI-O3-NEXT:    ld a, l
; GBI-O3-NEXT:    xor $01
; GBI-O3-NEXT:    ld l, a
; GBI-O3-NEXT:    ret
  %1 = xor i16 %b, 1
  ret i16 %1
}

define i16 @or(i16 %b, i16 %c) nounwind {
; GBI-O3-LABEL: or:
; GBI-O3:       # %bb.0:
; GBI-O3-NEXT:    ld a, h
; GBI-O3-NEXT:    or b
; GBI-O3-NEXT:    ld d, a
; GBI-O3-NEXT:    ld a, l
; GBI-O3-NEXT:    or c
; GBI-O3-NEXT:    ld l, a
; GBI-O3-NEXT:    ld h, d
; GBI-O3-NEXT:    ret
  %1 = or i16 %b, %c
  ret i16 %1
}

define i16 @ori(i16 %b) nounwind {
; GBI-O3-LABEL: ori:
; GBI-O3:       # %bb.0:
; GBI-O3-NEXT:    ld a, l
; GBI-O3-NEXT:    or $01
; GBI-O3-NEXT:    ld l, a
; GBI-O3-NEXT:    ret
  %1 = or i16 %b, 1
  ret i16 %1
}

define i16 @sub16(i16 %a, i8 %b) nounwind {
; GBI-O3-LABEL: sub16:
; GBI-O3:       # %bb.0:
; GBI-O3-NEXT:    ld a, l
; GBI-O3-NEXT:    sub b
; GBI-O3-NEXT:    ld c, a
; GBI-O3-NEXT:    ld a, h
; GBI-O3-NEXT:    sbc $00
; GBI-O3-NEXT:    ld h, a
; GBI-O3-NEXT:    ld l, c
; GBI-O3-NEXT:    ret
  %1 = zext i8 %b to i16
  %2 = sub i16 %a, %1
  ret i16 %2
}

define i1 @sless_than(i16 %0, i16 %1) {
; GBI-O3-LABEL: sless_than:
; GBI-O3:       # %bb.0: # %begin
; GBI-O3-NEXT:    ld a, l
; GBI-O3-NEXT:    cp c
; GBI-O3-NEXT:    rl d
; GBI-O3-NEXT:    ld a, h
; GBI-O3-NEXT:    sub b
; GBI-O3-NEXT:    ld e, a
; GBI-O3-NEXT:    ld a, h
; GBI-O3-NEXT:    xor b
; GBI-O3-NEXT:    bit 7, a
; GBI-O3-NEXT:    jr nz, .LBB7_3
; GBI-O3-NEXT:  # %bb.1: # %begin
; GBI-O3-NEXT:    rlc e
; GBI-O3-NEXT:    ld a, h
; GBI-O3-NEXT:    cp b
; GBI-O3-NEXT:    jr nz, .LBB7_4
; GBI-O3-NEXT:  .LBB7_2: # %begin
; GBI-O3-NEXT:    ld a, d
; GBI-O3-NEXT:    ret
; GBI-O3-NEXT:  .LBB7_3:
; GBI-O3-NEXT:    ld e, h
; GBI-O3-NEXT:    rlc e
; GBI-O3-NEXT:    ld a, h
; GBI-O3-NEXT:    cp b
; GBI-O3-NEXT:    jr z, .LBB7_2
; GBI-O3-NEXT:  .LBB7_4: # %begin
; GBI-O3-NEXT:    ld d, e
; GBI-O3-NEXT:    ld a, d
; GBI-O3-NEXT:    ret
begin:
  %10 = icmp slt i16 %0, %1
  ret i1 %10
}

define i1 @sgreater_than(i16 %0, i16 %1) {
; GBI-O3-LABEL: sgreater_than:
; GBI-O3:       # %bb.0: # %begin
; GBI-O3-NEXT:    ld a, c
; GBI-O3-NEXT:    cp l
; GBI-O3-NEXT:    rl d
; GBI-O3-NEXT:    ld a, b
; GBI-O3-NEXT:    sub h
; GBI-O3-NEXT:    ld e, a
; GBI-O3-NEXT:    ld a, b
; GBI-O3-NEXT:    xor h
; GBI-O3-NEXT:    bit 7, a
; GBI-O3-NEXT:    jr nz, .LBB8_3
; GBI-O3-NEXT:  # %bb.1: # %begin
; GBI-O3-NEXT:    rlc e
; GBI-O3-NEXT:    ld a, h
; GBI-O3-NEXT:    cp b
; GBI-O3-NEXT:    jr nz, .LBB8_4
; GBI-O3-NEXT:  .LBB8_2: # %begin
; GBI-O3-NEXT:    ld a, d
; GBI-O3-NEXT:    ret
; GBI-O3-NEXT:  .LBB8_3:
; GBI-O3-NEXT:    ld e, b
; GBI-O3-NEXT:    rlc e
; GBI-O3-NEXT:    ld a, h
; GBI-O3-NEXT:    cp b
; GBI-O3-NEXT:    jr z, .LBB8_2
; GBI-O3-NEXT:  .LBB8_4: # %begin
; GBI-O3-NEXT:    ld d, e
; GBI-O3-NEXT:    ld a, d
; GBI-O3-NEXT:    ret
begin:
  %10 = icmp sgt i16 %0, %1
  ret i1 %10
}


define i32 @add32(i32 %in) nounwind {
; GBI-O3-LABEL: add32:
; GBI-O3:       # %bb.0:
; GBI-O3-NEXT:    add sp, -4
; GBI-O3-NEXT:    push hl
; GBI-O3-NEXT:    ld hl, sp, 4
; GBI-O3-NEXT:    ld a, e
; GBI-O3-NEXT:    ldi (hl), a
; GBI-O3-NEXT:    ld (hl), d
; GBI-O3-NEXT:    pop hl
; GBI-O3-NEXT:    ld a, c
; GBI-O3-NEXT:    add $8e
; GBI-O3-NEXT:    ld d, a
; GBI-O3-NEXT:    ld a, b
; GBI-O3-NEXT:    adc $0c
; GBI-O3-NEXT:    ld e, a
; GBI-O3-NEXT:    ld a, d
; GBI-O3-NEXT:    ldi (hl), a
; GBI-O3-NEXT:    ld a, e
; GBI-O3-NEXT:    ld (hl), a
; GBI-O3-NEXT:    cp b
; GBI-O3-NEXT:    rla
; GBI-O3-NEXT:    push hl
; GBI-O3-NEXT:    ld hl, sp, 3
; GBI-O3-NEXT:    ld (hl), a
; GBI-O3-NEXT:    pop hl
; GBI-O3-NEXT:    ld a, d
; GBI-O3-NEXT:    cp c
; GBI-O3-NEXT:    rl d
; GBI-O3-NEXT:    ld a, e
; GBI-O3-NEXT:    cp b
; GBI-O3-NEXT:    jr z, .LBB9_2
; GBI-O3-NEXT:  # %bb.1:
; GBI-O3-NEXT:    push hl
; GBI-O3-NEXT:    ld hl, sp, 3
; GBI-O3-NEXT:    ld d, (hl)
; GBI-O3-NEXT:    pop hl
; GBI-O3-NEXT:  .LBB9_2:
; GBI-O3-NEXT:    ld a, $00
; GBI-O3-NEXT:    bit 0, d
; GBI-O3-NEXT:    jr nz, .LBB9_4
; GBI-O3-NEXT:  # %bb.3:
; GBI-O3-NEXT:    push hl
; GBI-O3-NEXT:    ld hl, sp, 3
; GBI-O3-NEXT:    ld (hl), a
; GBI-O3-NEXT:    jr .LBB9_5
; GBI-O3-NEXT:  .LBB9_4:
; GBI-O3-NEXT:    push hl
; GBI-O3-NEXT:    ld hl, sp, 3
; GBI-O3-NEXT:    ld (hl), $01
; GBI-O3-NEXT:  .LBB9_5:
; GBI-O3-NEXT:    pop hl
; GBI-O3-NEXT:    inc hl
; GBI-O3-NEXT:    ld d, h
; GBI-O3-NEXT:    ld e, l
; GBI-O3-NEXT:    inc de
; GBI-O3-NEXT:    push hl
; GBI-O3-NEXT:    ld hl, sp, 4
; GBI-O3-NEXT:    ld a, (hl)
; GBI-O3-NEXT:    inc hl
; GBI-O3-NEXT:    ld b, (hl)
; GBI-O3-NEXT:    ld hl, sp, 3
; GBI-O3-NEXT:    ld b, (hl)
; GBI-O3-NEXT:    pop hl
; GBI-O3-NEXT:    add b
; GBI-O3-NEXT:    push af
; GBI-O3-NEXT:    push hl
; GBI-O3-NEXT:    ld hl, sp, 5
; GBI-O3-NEXT:    ld (hl), a
; GBI-O3-NEXT:    pop hl
; GBI-O3-NEXT:    pop af
; GBI-O3-NEXT:    push hl
; GBI-O3-NEXT:    push af
; GBI-O3-NEXT:    ld hl, sp, 6
; GBI-O3-NEXT:    ld c, (hl)
; GBI-O3-NEXT:    inc hl
; GBI-O3-NEXT:    ld b, (hl)
; GBI-O3-NEXT:    pop af
; GBI-O3-NEXT:    ld a, b
; GBI-O3-NEXT:    ld b, $00
; GBI-O3-NEXT:    adc b
; GBI-O3-NEXT:    ld (de), a
; GBI-O3-NEXT:    ld hl, sp, 3
; GBI-O3-NEXT:    ld a, (hl)
; GBI-O3-NEXT:    pop hl
; GBI-O3-NEXT:    ld (hl), a
; GBI-O3-NEXT:    add sp, 4
; GBI-O3-NEXT:    ret
  %1 = add i32 %in, 3214
  ret i32 %1
}


define i16 @shl16(i16 %bc, i16 %de) nounwind {
; GBI-O3-LABEL: shl16:
; GBI-O3:       # %bb.0: # %begin
; GBI-O3-NEXT:    ld b, $00
; GBI-O3-NEXT:    call __ashlhi3
; GBI-O3-NEXT:    ret
begin:
  %1 = shl i16 %bc, %de
  ret i16 %1
}

define i16 @lsr16(i16 %bc, i16 %de) nounwind {
; GBI-O3-LABEL: lsr16:
; GBI-O3:       # %bb.0: # %begin
; GBI-O3-NEXT:    ld b, $00
; GBI-O3-NEXT:    call __lshrhi3
; GBI-O3-NEXT:    ret
begin:
  %1 = lshr i16 %bc, %de
  ret i16 %1
}

define i16 @asr16(i16 %bc, i16 %de) nounwind {
; GBI-O3-LABEL: asr16:
; GBI-O3:       # %bb.0: # %begin
; GBI-O3-NEXT:    ld b, $00
; GBI-O3-NEXT:    call __ashrhi3
; GBI-O3-NEXT:    ret
begin:
  %1 = ashr i16 %bc, %de
  ret i16 %1
}

define i16 @shl16_c2(i16 %bc) nounwind {
; GBI-O3-LABEL: shl16_c2:
; GBI-O3:       # %bb.0: # %begin
; GBI-O3-NEXT:    sla l
; GBI-O3-NEXT:    rl h
; GBI-O3-NEXT:    sla l
; GBI-O3-NEXT:    rl h
; GBI-O3-NEXT:    ret
begin:
  %1 = shl i16 %bc, 2
  ret i16 %1
}

define i16 @shl16_c9(i16 %bc) nounwind {
; GBI-O3-LABEL: shl16_c9:
; GBI-O3:       # %bb.0: # %begin
; GBI-O3-NEXT:    sla l
; GBI-O3-NEXT:    ld c, $00
; GBI-O3-NEXT:    ld h, l
; GBI-O3-NEXT:    ld l, c
; GBI-O3-NEXT:    ret
begin:
  %1 = shl i16 %bc, 9
  ret i16 %1
}

define i16 @asr16_c2(i16 %bc) nounwind {
; GBI-O3-LABEL: asr16_c2:
; GBI-O3:       # %bb.0: # %begin
; GBI-O3-NEXT:    sra h
; GBI-O3-NEXT:    rr l
; GBI-O3-NEXT:    sra h
; GBI-O3-NEXT:    rr l
; GBI-O3-NEXT:    ret
begin:
  %1 = ashr i16 %bc, 2
  ret i16 %1
}

define i16 @asr16_c9(i16 %bc) nounwind {
; GBI-O3-LABEL: asr16_c9:
; GBI-O3:       # %bb.0: # %begin
; GBI-O3-NEXT:    ld b, h
; GBI-O3-NEXT:    sra b
; GBI-O3-NEXT:    sra b
; GBI-O3-NEXT:    sra b
; GBI-O3-NEXT:    sra b
; GBI-O3-NEXT:    sra b
; GBI-O3-NEXT:    sra b
; GBI-O3-NEXT:    sra b
; GBI-O3-NEXT:    sra h
; GBI-O3-NEXT:    ld l, h
; GBI-O3-NEXT:    ld h, b
; GBI-O3-NEXT:    ret
begin:
  %1 = ashr i16 %bc, 9
  ret i16 %1
}

define i16 @lsr16_c2(i16 %bc) nounwind {
; GBI-O3-LABEL: lsr16_c2:
; GBI-O3:       # %bb.0: # %begin
; GBI-O3-NEXT:    srl h
; GBI-O3-NEXT:    rr l
; GBI-O3-NEXT:    srl h
; GBI-O3-NEXT:    rr l
; GBI-O3-NEXT:    ret
begin:
  %1 = lshr i16 %bc, 2
  ret i16 %1
}

define i16 @lsr16_c9(i16 %bc) nounwind {
; GBI-O3-LABEL: lsr16_c9:
; GBI-O3:       # %bb.0: # %begin
; GBI-O3-NEXT:    srl h
; GBI-O3-NEXT:    ld b, $00
; GBI-O3-NEXT:    ld l, h
; GBI-O3-NEXT:    ld h, b
; GBI-O3-NEXT:    ret
begin:
  %1 = lshr i16 %bc, 9
  ret i16 %1
}
