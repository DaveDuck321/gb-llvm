# RUN: llvm-mc %s -triple=gb -show-encoding \
# RUN:      | FileCheck -check-prefixes=CHECK,CHECK-INST %s
# RUN: llvm-mc %s -triple=gb -filetype=obj \
# RUN:      | llvm-objdump -d - \
# RUN:      | FileCheck -check-prefix=CHECK-INST %s

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
# CHECK-INST: add (hl)
# CHECK: encoding: [0x86]
add (hl)
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
# CHECK-INST: xor (hl)
# CHECK: encoding: [0xae]
xor (hl)
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
# CHECK-INST: ld b, (hl)
# CHECK: encoding: [0x46]
ld b,(hl)
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
# CHECK-INST: ld c, (hl)
# CHECK: encoding: [0x4e]
ld c,(hl)
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
# CHECK-INST: ld (hl), c
# CHECK: encoding: [0x71]
ld (hl),c

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

# CHECK-INST: ld b, 10
# CHECK: encoding: [0x06,0x0a]
ld b,10
# CHECK-INST: ld c, 10
# CHECK: encoding: [0x0e,0x0a]
ld c,10
# CHECK-INST: ld d, 10
# CHECK: encoding: [0x16,0x0a]
ld d,10
# CHECK-INST: ld e, 10
# CHECK: encoding: [0x1e,0x0a]
ld e,10
# CHECK-INST: ld h, 10
# CHECK: encoding: [0x26,0x0a]
ld h,10
# CHECK-INST: ld l, 10
# CHECK: encoding: [0x2e,0x0a]
ld l,10
# CHECK-INST: ld (hl), 10
# CHECK: encoding: [0x36,0x0a]
ld (hl),10
# CHECK-INST: ld a, 10
# CHECK: encoding: [0x3e,0x0a]
ld a,10

# CHECK-INST: rlc b
# CHECK: encoding: [0xcb,0x00]
rlc b
# CHECK-INST: rlc c
# CHECK: encoding: [0xcb,0x01]
rlc c
# CHECK-INST: rlc d
# CHECK: encoding: [0xcb,0x02]
rlc d
# CHECK-INST: rlc e
# CHECK: encoding: [0xcb,0x03]
rlc e
# CHECK-INST: rlc h
# CHECK: encoding: [0xcb,0x04]
rlc h
# CHECK-INST: rlc l
# CHECK: encoding: [0xcb,0x05]
rlc l
# CHECK-INST: rlc (hl)
# CHECK: encoding: [0xcb,0x06]
rlc (hl)
# CHECK-INST: rlc a
# CHECK: encoding: [0xcb,0x07]
rlc a

# CHECK-INST: rr b
# CHECK: encoding: [0xcb,0x18]
rr b
# CHECK-INST: rr c
# CHECK: encoding: [0xcb,0x19]
rr c
# CHECK-INST: rr d
# CHECK: encoding: [0xcb,0x1a]
rr d
# CHECK-INST: rr e
# CHECK: encoding: [0xcb,0x1b]
rr e
# CHECK-INST: rr h
# CHECK: encoding: [0xcb,0x1c]
rr h
# CHECK-INST: rr l
# CHECK: encoding: [0xcb,0x1d]
rr l
# CHECK-INST: rr (hl)
# CHECK: encoding: [0xcb,0x1e]
rr (hl)
# CHECK-INST: rr a
# CHECK: encoding: [0xcb,0x1f]
rr a

# CHECK-INST: bit 2, b
# CHECK: encoding: [0xcb,0x50]
bit 2,b
# CHECK-INST: bit 2, c
# CHECK: encoding: [0xcb,0x51]
bit 2,c
# CHECK-INST: bit 2, d
# CHECK: encoding: [0xcb,0x52]
bit 2,d
# CHECK-INST: bit 2, e
# CHECK: encoding: [0xcb,0x53]
bit 2,e
# CHECK-INST: bit 2, h
# CHECK: encoding: [0xcb,0x54]
bit 2,h
# CHECK-INST: bit 2, l
# CHECK: encoding: [0xcb,0x55]
bit 2,l
# CHECK-INST: bit 2, (hl)
# CHECK: encoding: [0xcb,0x56]
bit 2,(hl)
# CHECK-INST: bit 2, a
# CHECK: encoding: [0xcb,0x57]
bit 2,a

# CHECK-INST: bit 1, b
# CHECK: encoding: [0xcb,0x48]
bit 1,b
# CHECK-INST: bit 1, c
# CHECK: encoding: [0xcb,0x49]
bit 1,c
# CHECK-INST: bit 1, d
# CHECK: encoding: [0xcb,0x4a]
bit 1,d
# CHECK-INST: bit 1, e
# CHECK: encoding: [0xcb,0x4b]
bit 1,e
# CHECK-INST: bit 1, h
# CHECK: encoding: [0xcb,0x4c]
bit 1,h
# CHECK-INST: bit 1, l
# CHECK: encoding: [0xcb,0x4d]
bit 1,l
# CHECK-INST: bit 1, (hl)
# CHECK: encoding: [0xcb,0x4e]
bit 1,(hl)
# CHECK-INST: bit 1, a
# CHECK: encoding: [0xcb,0x4f]
bit 1,a

# CHECK-INST: inc b
# CHECK: encoding: [0x04]
inc b
# CHECK-INST: inc c
# CHECK: encoding: [0x0c]
inc c
# CHECK-INST: inc d
# CHECK: encoding: [0x14]
inc d
# CHECK-INST: inc e
# CHECK: encoding: [0x1c]
inc e
# CHECK-INST: inc h
# CHECK: encoding: [0x24]
inc h
# CHECK-INST: inc l
# CHECK: encoding: [0x2c]
inc l
# CHECK-INST: inc (hl)
# CHECK: encoding: [0x34]
inc (hl)
# CHECK-INST: inc a
# CHECK: encoding: [0x3c]
inc a

# CHECK-INST: dec b
# CHECK: encoding: [0x05]
dec b
# CHECK-INST: dec c
# CHECK: encoding: [0x0d]
dec c
# CHECK-INST: dec d
# CHECK: encoding: [0x15]
dec d
# CHECK-INST: dec e
# CHECK: encoding: [0x1d]
dec e
# CHECK-INST: dec h
# CHECK: encoding: [0x25]
dec h
# CHECK-INST: dec l
# CHECK: encoding: [0x2d]
dec l
# CHECK-INST: dec (hl)
# CHECK: encoding: [0x35]
dec (hl)
# CHECK-INST: dec a
# CHECK: encoding: [0x3d]
dec a

# CHECK-INST: inc bc
# CHECK: encoding: [0x03]
inc bc
# CHECK-INST: inc de
# CHECK: encoding: [0x13]
inc de
# CHECK-INST: inc hl
# CHECK: encoding: [0x23]
inc hl
# CHECK-INST: inc sp
# CHECK: encoding: [0x33]
inc sp

# CHECK-INST: dec bc
# CHECK: encoding: [0x0b]
dec bc
# CHECK-INST: dec de
# CHECK: encoding: [0x1b]
dec de
# CHECK-INST: dec hl
# CHECK: encoding: [0x2b]
dec hl
# CHECK-INST: dec sp
# CHECK: encoding: [0x3b]
dec sp

# CHECK-INST: add hl, bc
# CHECK: encoding: [0x09]
add hl, bc
# CHECK-INST: add hl, de
# CHECK: encoding: [0x19]
add hl, de
# CHECK-INST: add hl, hl
# CHECK: encoding: [0x29]
add hl, hl
# CHECK-INST: add hl, sp
# CHECK: encoding: [0x39]
add hl, sp

# CHECK-INST: ld bc, $0001
# CHECK: encoding: [0x01,0x01,0x00]
ld bc, 1
# CHECK-INST: ld de, $0001
# CHECK: encoding: [0x11,0x01,0x00]
ld de, 1
# CHECK-INST: ld hl, $0001
# CHECK: encoding: [0x21,0x01,0x00]
ld hl, 1
# CHECK-INST: ld sp, $0001
# CHECK: encoding: [0x31,0x01,0x00]
ld sp, 1

# CHECK-INST: push bc
# CHECK: encoding: [0xc5]
push bc
# CHECK-INST: push de
# CHECK: encoding: [0xd5]
push de
# CHECK-INST: push hl
# CHECK: encoding: [0xe5]
push hl
# CHECK-INST: push af
# CHECK: encoding: [0xf5]
push af
