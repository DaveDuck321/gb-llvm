# RUN: llvm-mc %s -triple=gb -show-encoding \
# RUN:      | FileCheck -check-prefixes=CHECK,CHECK-INST %s

# CHECK-INST: add a
# CHECK: encoding: [0x87]
add A

# CHECK-INST: add a
# CHECK: encoding: [0x87]
add a
