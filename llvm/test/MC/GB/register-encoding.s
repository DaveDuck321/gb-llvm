# RUN: llvm-mc %s -triple=gb -show-encoding \
# RUN:      | FileCheck -check-prefixes=CHECK,CHECK-INST %s

# CHECK-INST: add b
# CHECK: encoding: [0x80]
add b
# CHECK-INST: add c
# CHECK: encoding: [0x81]
add c
# CHECK-INST: add d
# CHECK: encoding: [0x82]
add d
# CHECK-INST: add e
# CHECK: encoding: [0x83]
add e
# CHECK-INST: add h
# CHECK: encoding: [0x84]
add h
# CHECK-INST: add l
# CHECK: encoding: [0x85]
add l
# CHECK-INST: add a
# CHECK: encoding: [0x87]
add a

# CHECK-INST: xor b
# CHECK: encoding: [0xa8]
xor b
# CHECK-INST: xor c
# CHECK: encoding: [0xa9]
xor c
# CHECK-INST: xor d
# CHECK: encoding: [0xaa]
xor d
# CHECK-INST: xor e
# CHECK: encoding: [0xab]
xor e
# CHECK-INST: xor h
# CHECK: encoding: [0xac]
xor h
# CHECK-INST: xor l
# CHECK: encoding: [0xad]
xor l
# CHECK-INST: xor a
# CHECK: encoding: [0xaf]
xor a
