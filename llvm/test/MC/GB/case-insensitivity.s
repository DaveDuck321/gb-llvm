# RUN: llvm-mc %s -triple=gb -show-encoding \
# RUN:      | FileCheck -check-prefixes=CHECK,CHECK-INST %s

# CHECK-INST: add a
# CHECK: encoding: [0x87]
add A

# CHECK-INST: add a
# CHECK: encoding: [0x87]
add a

# CHECK-INST: add a
# CHECK: encoding: [0x87]
ADD a

# CHECK-INST: ret nc
# CHECK: encoding: [0xd0]
ret NC

# CHECK-INST: ret nc
# CHECK: encoding: [0xd0]
ret nc

# CHECK-INST: ld a, (hl)
# CHECK: encoding: [0x7e]
ld a, (HL)

# CHECK-INST: ld (hl), a
# CHECK: encoding: [0x77]
ld (hl), a
