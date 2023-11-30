# RUN: llvm-mc %s -triple=gb -show-encoding \
# RUN:      | FileCheck -check-prefixes=CHECK,CHECK-INST %s

# CHECK-INST: ret nz
# CHECK: encoding: [0xc0]
ret nz
# CHECK-INST: ret nc
# CHECK: encoding: [0xd0]
ret NC
# CHECK-INST: ret z
# CHECK: encoding: [0xc8]
RET z
