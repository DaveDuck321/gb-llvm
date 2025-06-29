# RUN: llvm-mc %s -triple=gb -show-encoding -g \
# RUN:      | FileCheck -check-prefixes=CHECK-FIXUP %s
# RUN: llvm-mc %s -triple=gb -filetype=obj -g \
# RUN:      | llvm-objdump -d - \
# RUN:      | FileCheck -check-prefixes=CHECK-FIXED %s
# RUN: llvm-mc %s -triple=gb -filetype=obj \
# RUN:      | llvm-readelf -r - \
# RUN:      | FileCheck -check-prefixes=CHECK-RELOC-TYPE,CHECK-RELOC-OFFSET,CHECK-RELOC-NAME %s

# Check that fixups work within a single file

jump_label_start:

add VAL8
jr nz, jump_label_start
LD sp, VAL16
ld hl, sp+SP_REL8
ldh (io), a
ldh a, (io)
jr nz, jump_label_end
ld a, (memory)

add %lo memory
adc %hi memory

jump_label_end:

.set VAL8, 66
.set VAL16, 16962
.set SP_REL8, 10
.set io, 0xEE
.set memory, 0xEEE1

# CHECK-FIXUP: fixup A - offset: 1, value: +VAL8, kind: FK_Data_1
# CHECK-FIXUP: fixup A - offset: 1, value: +jump_label_start-1, kind: FK_PCRel_1
# CHECK-FIXUP: fixup A - offset: 1, value: +VAL16, kind: FK_Data_2
# CHECK-FIXUP: fixup A - offset: 1, value: +SP_REL8, kind: FK_Data_1
# CHECK-FIXUP: fixup A - offset: 1, value: +io, kind: FK_Data_1
# CHECK-FIXUP: fixup A - offset: 1, value: %lo +memory, kind: GB_FIXUP_LO_16
# CHECK-FIXUP: fixup A - offset: 1, value: %hi +memory, kind: GB_FIXUP_HI_16


# CHECK-FIXED: <jump_label_start>
# CHECK-FIXED: add $42
# CHECK-FIXED: jr nz, -4
# CHECK-FIXED: ld sp, $4242
# CHECK-FIXED: ld hl, sp, 10
# CHECK-FIXED: ldh ($ee), a
# CHECK-FIXED: ldh a, ($ee)
# CHECK-FIXED: ld a, ($eee1)

# I dislike this but it looks like only the linker can relocate these
# CHECK-FIXED: add $00
# CHECK-FIXED: adc $00

# CHECK-RELOC-OFFSET: 00000013
# CHECK-RELOC-TYPE: R_GB_LO_16
# CHECK-RELOC-NAME: eee1

# CHECK-RELOC-OFFSET: 00000015
# CHECK-RELOC-TYPE: R_GB_HI_16
# CHECK-RELOC-NAME: eee1
