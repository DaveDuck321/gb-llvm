# RUN: llvm-mc %s -triple=gb -show-encoding -motorola-integers \
# RUN:      | FileCheck -check-prefixes=CHECK,CHECK-INST %s

# CHECK-INST: add 80
# CHECK: encoding: [0xc6,0x50]
add 80

# CHECK-INST: add 128
# CHECK: encoding: [0xc6,0x80]
add 0x80

# CHECK-INST: add 128
# CHECK: encoding: [0xc6,0x80]
add $80
