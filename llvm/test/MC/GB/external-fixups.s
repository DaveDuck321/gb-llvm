# RUN: llvm-mc %s -triple=gb -show-encoding \
# RUN:      | FileCheck -check-prefixes=CHECK-FIXUP %s
# RUN: llvm-mc %s -triple=gb -filetype=obj -g \
# RUN:      | llvm-objdump -d - \
# RUN:      | FileCheck -check-prefixes=CHECK-ENCODED %s
# RUN: llvm-mc %s -triple=gb -filetype=obj -g \
# RUN:      | llvm-readelf -r - \
# RUN:      | FileCheck -check-prefixes=CHECK-RELOC-TYPE,CHECK-RELOC-OFFSET,CHECK-RELOC-NAME %s

# Check that external fixups are correctly generated

add VAL8
jr nz, jump_label
LD sp, VAL16

add %lo memory
adc %hi memory

# CHECK-FIXUP: fixup A - offset: 1, value: +VAL8, kind: FK_Data_1
# CHECK-FIXUP: fixup A - offset: 1, value: +jump_label-1, kind: FK_PCRel_1
# CHECK-FIXUP: fixup A - offset: 1, value: +VAL16, kind: FK_Data_2
# CHECK-FIXUP: fixup A - offset: 1, value: %lo +memory, kind: GB_FIXUP_LO_16
# CHECK-FIXUP: fixup A - offset: 1, value: %hi +memory, kind: GB_FIXUP_HI_16

# CHECK-ENCODED: add $00
# CHECK-ENCODED: jr nz, 0
# CHECK-ENCODED: ld sp, $0000
# CHECK-ENCODED: add $00
# CHECK-ENCODED: adc $00

# CHECK-RELOC-OFFSET: 00000001
# CHECK-RELOC-TYPE: R_GB_8
# CHECK-RELOC-NAME: VAL8

# CHECK-RELOC-OFFSET: 00000003
# CHECK-RELOC-TYPE: R_GB_PCREL_8
# CHECK-RELOC-NAME: jump_label

# CHECK-RELOC-OFFSET: 00000005
# CHECK-RELOC-TYPE: R_GB_16
# CHECK-RELOC-NAME: VAL16

# CHECK-RELOC-OFFSET: 00000008
# CHECK-RELOC-TYPE: R_GB_LO_16
# CHECK-RELOC-NAME: memory

# CHECK-RELOC-OFFSET: 0000000a
# CHECK-RELOC-TYPE: R_GB_HI_16
# CHECK-RELOC-NAME: memory
