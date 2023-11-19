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

# CHECK-INST: ld b, b
# CHECK: encoding: [0x40]
ld b,b
# CHECK-INST: ld b, c
# CHECK: encoding: [0x41]
ld b,c
# CHECK-INST: ld b, d
# CHECK: encoding: [0x42]
ld b,d
# CHECK-INST: ld b, e
# CHECK: encoding: [0x43]
ld b,e
# CHECK-INST: ld b, h
# CHECK: encoding: [0x44]
ld b,h
# CHECK-INST: ld b, l
# CHECK: encoding: [0x45]
ld b,l
# CHECK-INST: ld b, a
# CHECK: encoding: [0x47]
ld b,a

# CHECK-INST: ld c, b
# CHECK: encoding: [0x48]
ld c,b
# CHECK-INST: ld c, c
# CHECK: encoding: [0x49]
ld c,c
# CHECK-INST: ld c, d
# CHECK: encoding: [0x4a]
ld c,d
# CHECK-INST: ld c, e
# CHECK: encoding: [0x4b]
ld c,e
# CHECK-INST: ld c, h
# CHECK: encoding: [0x4c]
ld c,h
# CHECK-INST: ld c, l
# CHECK: encoding: [0x4d]
ld c,l
# CHECK-INST: ld c, a
# CHECK: encoding: [0x4f]
ld c,a

# CHECK-INST: ld b, c
# CHECK: encoding: [0x41]
ld b,c
# CHECK-INST: ld d, c
# CHECK: encoding: [0x51]
ld d,c
# CHECK-INST: ld h, c
# CHECK: encoding: [0x61]
ld h,c

# CHECK-INST: ld c, c
# CHECK: encoding: [0x49]
ld c,c
# CHECK-INST: ld e, c
# CHECK: encoding: [0x59]
ld e,c
# CHECK-INST: ld l, c
# CHECK: encoding: [0x69]
ld l,c
# CHECK-INST: ld a, c
# CHECK: encoding: [0x79]
ld a,c
