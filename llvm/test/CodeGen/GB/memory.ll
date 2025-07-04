; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py UTC_ARGS: --version 3
; RUN: llc -mtriple=gb -verify-machineinstrs -O3 < %s \
; RUN:   | FileCheck %s -check-prefix=GBI

define i8 @load8(ptr %a) nounwind {
; GBI-LABEL: load8:
; GBI:       # %bb.0:
; GBI-NEXT:    ld a, (hl)
; GBI-NEXT:    ret
  %1 = load i8, ptr %a
  ret i8 %1
}

define i16 @load16(ptr %a) nounwind {
; GBI-LABEL: load16:
; GBI:       # %bb.0:
; GBI-NEXT:    ldi a, (hl)
; GBI-NEXT:    ld c, a
; GBI-NEXT:    ld h, (hl)
; GBI-NEXT:    ld l, c
; GBI-NEXT:    ret
  %1 = load i16, ptr %a
  ret i16 %1
}

define void @store8(ptr %a, i8 %b) nounwind {
; GBI-LABEL: store8:
; GBI:       # %bb.0:
; GBI-NEXT:    ld a, b
; GBI-NEXT:    ld (hl), a
; GBI-NEXT:    ret
  store i8 %b, ptr %a
  ret void
}

define void @store16(ptr %a, i16 %b) nounwind {
; GBI-LABEL: store16:
; GBI:       # %bb.0:
; GBI-NEXT:    ld d, h
; GBI-NEXT:    ld e, l
; GBI-NEXT:    inc de
; GBI-NEXT:    ld a, b
; GBI-NEXT:    ld (de), a
; GBI-NEXT:    ld a, c
; GBI-NEXT:    ld (hl), a
; GBI-NEXT:    ret
  store i16 %b, ptr %a
  ret void
}

define void @store1(ptr %a, i1 %b) nounwind {
; GBI-LABEL: store1:
; GBI:       # %bb.0:
; GBI-NEXT:    ld a, b
; GBI-NEXT:    and $01
; GBI-NEXT:    ld (hl), a
; GBI-NEXT:    ret
  store i1 %b, ptr %a
  ret void
}

define i1 @load1(ptr %a) nounwind {
; GBI-LABEL: load1:
; GBI:       # %bb.0:
; GBI-NEXT:    ld a, (hl)
; GBI-NEXT:    ret
  %1 = load i1, ptr %a
  ret i1 %1
}

define i8 @load_sext(ptr %a) nounwind {
; GBI-LABEL: load_sext:
; GBI:       # %bb.0:
; GBI-NEXT:    ld a, (hl)
; GBI-NEXT:    and $01
; GBI-NEXT:    ld b, a
; GBI-NEXT:    ld a, $00
; GBI-NEXT:    sub b
; GBI-NEXT:    ret
  %1 = load i1, ptr %a
  %2 = sext i1 %1 to i8
  ret i8 %2
}

define i8 @load_zext(ptr %a) nounwind {
; GBI-LABEL: load_zext:
; GBI:       # %bb.0:
; GBI-NEXT:    ld a, (hl)
; GBI-NEXT:    and $01
; GBI-NEXT:    ret
  %1 = load i1, ptr %a
  %2 = zext i1 %1 to i8
  ret i8 %2
}

define void @store_trunc(ptr %a, i8 %b) nounwind {
; GBI-LABEL: store_trunc:
; GBI:       # %bb.0:
; GBI-NEXT:    ld a, b
; GBI-NEXT:    and $01
; GBI-NEXT:    ld (hl), a
; GBI-NEXT:    ret
  %1 = trunc i8 %b to i1
  store i1 %1, ptr %a, align 1
  ret void
}

define i1 @simple_stack(i1 %0) {
; GBI-LABEL: simple_stack:
; GBI:       # %bb.0: # %begin
; GBI-NEXT:    add sp, -2
; GBI-NEXT:    ld a, b
; GBI-NEXT:    and $01
; GBI-NEXT:    ld hl, sp, 1
; GBI-NEXT:    ld (hl), a
; GBI-NEXT:    ld a, b
; GBI-NEXT:    add sp, 2
; GBI-NEXT:    ret
begin:
  %1 = alloca i1, align 1
  store i1 %0, ptr %1, align 1
  %2 = load i1, ptr %1, align 1
  ret i1 %2
}

define void @store_bit_1() #0 {
; GBI-LABEL: store_bit_1:
; GBI:       # %bb.0: # %entry
; GBI-NEXT:    ldh a, ($0f)
; GBI-NEXT:    or $01
; GBI-NEXT:    ldh ($0f), a
; GBI-NEXT:    ret
entry:
  %bf.load.i = load volatile i8, ptr inttoptr (i16 -241 to ptr), align 1
  %bf.set.i = or i8 %bf.load.i, 1
  store volatile i8 %bf.set.i, ptr inttoptr (i16 -241 to ptr), align 1
  ret void
}

define dso_local void @store_bit_3() #0 {
; GBI-LABEL: store_bit_3:
; GBI:       # %bb.0: # %entry
; GBI-NEXT:    ldh a, ($0f)
; GBI-NEXT:    or $04
; GBI-NEXT:    ldh ($0f), a
; GBI-NEXT:    ret
entry:
  %bf.load.i = load volatile i8, ptr inttoptr (i16 -241 to ptr), align 1
  %bf.set.i = or i8 %bf.load.i, 4
  store volatile i8 %bf.set.i, ptr inttoptr (i16 -241 to ptr), align 1
  ret void
}

define dso_local noundef zeroext range(i8 0, 2) i8 @load_bit_3() local_unnamed_addr #0 {
; GBI-LABEL: load_bit_3:
; GBI:       # %bb.0: # %entry
; GBI-NEXT:    ldh a, ($0f)
; GBI-NEXT:    srl a
; GBI-NEXT:    srl a
; GBI-NEXT:    and $01
; GBI-NEXT:    ret
entry:
  %bf.load.i = load volatile i8, ptr inttoptr (i16 -241 to ptr), align 1
  %bf.lshr.i = lshr i8 %bf.load.i, 2
  %bf.clear.i = and i8 %bf.lshr.i, 1
  ret i8 %bf.clear.i
}

define dso_local noundef zeroext range(i8 0, 2) i8 @load_bit_1() local_unnamed_addr #0 {
; GBI-LABEL: load_bit_1:
; GBI:       # %bb.0: # %entry
; GBI-NEXT:    ldh a, ($0f)
; GBI-NEXT:    and $01
; GBI-NEXT:    ret
entry:
  %bf.load.i = load volatile i8, ptr inttoptr (i16 -241 to ptr), align 1
  %bf.clear.i = and i8 %bf.load.i, 1
  ret i8 %bf.clear.i
}

@ext = external dso_local global i16, align 2
define i16 @load_global() {
; GBI-LABEL: load_global:
; GBI:       # %bb.0: # %entry
; GBI-NEXT:    ld a, (ext+1)
; GBI-NEXT:    ld h, a
; GBI-NEXT:    ld a, (ext)
; GBI-NEXT:    ld l, a
; GBI-NEXT:    ret
entry:
  %0 = load i16, ptr @ext, align 2
  ret i16 %0
}

define void @store_global(i16 %arg) {
; GBI-LABEL: store_global:
; GBI:       # %bb.0: # %entry
; GBI-NEXT:    ld a, h
; GBI-NEXT:    ld (ext+1), a
; GBI-NEXT:    ld a, l
; GBI-NEXT:    ld (ext), a
; GBI-NEXT:    ret
entry:
  store i16 %arg, ptr @ext, align 2
  ret void
}

%array = type { [16 x i8] }

define i16 @_Z3barN5libgb5ArrayIcLj16EEE(ptr byval(%array) align 1 %data) {
; GBI-LABEL: _Z3barN5libgb5ArrayIcLj16EEE:
; GBI:       # %bb.0: # %entry
; GBI-NEXT:    ld a, l
; GBI-NEXT:    add $09
; GBI-NEXT:    ld c, a
; GBI-NEXT:    ld a, h
; GBI-NEXT:    adc $00
; GBI-NEXT:    ld b, a
; GBI-NEXT:    ld a, (bc)
; GBI-NEXT:    ld d, a
; GBI-NEXT:    ld a, l
; GBI-NEXT:    add $07
; GBI-NEXT:    ld c, a
; GBI-NEXT:    ld a, h
; GBI-NEXT:    adc $00
; GBI-NEXT:    ld h, a
; GBI-NEXT:    ld a, d
; GBI-NEXT:    ld l, c
; GBI-NEXT:    add (hl)
; GBI-NEXT:    ld l, a
; GBI-NEXT:    ld a, $00
; GBI-NEXT:    adc $00
; GBI-NEXT:    ld h, a
; GBI-NEXT:    ret
entry:
  %arrayidx.i = getelementptr inbounds nuw i8, ptr %data, i16 7
  %0 = load i8, ptr %arrayidx.i, align 1
  %conv = zext i8 %0 to i16
  %arrayidx.i3 = getelementptr inbounds nuw i8, ptr %data, i16 9
  %1 = load i8, ptr %arrayidx.i3, align 1
  %conv2 = zext i8 %1 to i16
  %add = add nuw nsw i16 %conv2, %conv
  ret i16 %add
}
