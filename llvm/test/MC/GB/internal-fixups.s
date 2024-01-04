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

.set VAL8, 66
.set VAL16, 16962


# CHECK-FIXUP: fixup A - offset: 1, value: VAL8, kind: FK_Data_1
# CHECK-FIXUP: fixup A - offset: 1, value: jump_label, kind: FK_PCRel_1
# CHECK-FIXUP: fixup A - offset: 1, value: VAL16, kind: FK_Data_2

# CHECK-FIXED: <jump_label>
# CHECK-FIXED: add 66
# CHECK-FIXED: jr nz, -3
# CHECK-FIXED: ld sp, 16962

# CHECK-RELOCATIONS: There are no relocations in this file.
