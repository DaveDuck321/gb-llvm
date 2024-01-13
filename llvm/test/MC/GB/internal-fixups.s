# RUN: llvm-mc %s -triple=gb -show-encoding \
# RUN:      | FileCheck -check-prefixes=CHECK-FIXUP %s
# RUN: llvm-mc %s -triple=gb -filetype=obj \
# RUN:      | llvm-objdump -d - \
# RUN:      | FileCheck -check-prefixes=CHECK-FIXED %s
# RUN: llvm-mc %s -triple=gb -filetype=obj \
# RUN:      | llvm-readelf -r - \
# RUN:      | FileCheck -check-prefixes=CHECK-RELOCATIONS %s

# Check that fixups work within a single file

jump_label:

add VAL8
jr nz, jump_label
LD sp, VAL16
ld hl, sp+SP_REL8
ldh (io), a
ldh a, (io)
ld a, (memory)

.set VAL8, 66
.set VAL16, 16962
.set SP_REL8, 10
.set io, 0xEE
.set memory, 0xEEE1

# CHECK-FIXUP: fixup A - offset: 1, value: VAL8, kind: FK_Data_1
# CHECK-FIXUP: fixup A - offset: 1, value: jump_label, kind: FK_PCRel_1
# CHECK-FIXUP: fixup A - offset: 1, value: VAL16, kind: FK_Data_2
# CHECK-FIXUP: fixup A - offset: 1, value: +SP_REL8, kind: FK_Data_1
# CHECK-FIXUP: fixup A - offset: 1, value: io, kind: FK_Data_1
# CHECK-FIXUP: fixup A - offset: 1, value: memory, kind: FK_Data_2


# CHECK-FIXED: <jump_label>
# CHECK-FIXED: add 66
# CHECK-FIXED: jr nz, -3
# CHECK-FIXED: ld sp, $4242
# CHECK-FIXED: ld hl, sp, 10
# CHECK-FIXED: ldh ($ee), a
# CHECK-FIXED: ldh a, ($ee)
# CHECK-FIXED: ld a, ($eee1)

# CHECK-RELOCATIONS: There are no relocations in this file.
