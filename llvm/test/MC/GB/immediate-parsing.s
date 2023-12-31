# RUN: llvm-mc %s -triple=gb -show-encoding -motorola-integers \
# RUN:      | FileCheck -check-prefixes=CHECK,CHECK-INST %s
# RUN: llvm-mc %s -triple=gb -filetype=obj -motorola-integers \
# RUN:      | llvm-objdump -d - \
# RUN:      | FileCheck -check-prefix=CHECK-INST %s

# CHECK-INST: add 80
# CHECK: encoding: [0xc6,0x50]
add 80

# CHECK-INST: add 128
# CHECK: encoding: [0xc6,0x80]
add 0x80

# CHECK-INST: add 128
# CHECK: encoding: [0xc6,0x80]
add $80

# CHECK-INST: ld bc, 28677
# CHECK: encoding: [0x01,0x05,0x70]
ld bc, 0x7005

# CHECK-INST: jr nc, 16
# CHECK: encoding: [0x30,0x10]
jr nc, 16

# CHECK-INST: jr z, -16
# CHECK: encoding: [0x28,0xf0]
jr z, -16
