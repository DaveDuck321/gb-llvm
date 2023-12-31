# RUN: llvm-mc %s -triple=gb -show-encoding \
# RUN:      | FileCheck -check-prefixes=CHECK,CHECK-INST %s
# RUN: llvm-mc %s -triple=gb -filetype=obj \
# RUN:      | llvm-objdump -d - \
# RUN:      | FileCheck -check-prefix=CHECK-INST %s

# CHECK-INST: ret nz
# CHECK: encoding: [0xc0]
ret nz
# CHECK-INST: ret nc
# CHECK: encoding: [0xd0]
ret nc
# CHECK-INST: ret z
# CHECK: encoding: [0xc8]
ret z
# CHECK-INST: ret c
# CHECK: encoding: [0xd8]
ret c
