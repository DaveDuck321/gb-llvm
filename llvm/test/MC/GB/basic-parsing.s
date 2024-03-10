# RUN: llvm-mc %s -triple=gb -show-encoding \
# RUN:      | FileCheck -check-prefixes=CHECK,CHECK-INST %s
# RUN: llvm-mc %s -triple=gb -filetype=obj \
# RUN:      | llvm-objdump -d - \
# RUN:      | FileCheck -check-prefix=CHECK-INST %s

# CHECK-INST: add a
# CHECK: encoding: [0x87]
add a
# CHECK-INST: adc a
# CHECK: encoding: [0x8f]
adc a
# CHECK-INST: sub c
# CHECK: encoding: [0x91]
sub c
# CHECK-INST: sbc e
# CHECK: encoding: [0x9b]
sbc e
# CHECK-INST: and c
# CHECK: encoding: [0xa1]
and c
# CHECK-INST: xor b
# CHECK: encoding: [0xa8]
xor b
# CHECK-INST: or d
# CHECK: encoding: [0xb2]
or d
# CHECK-INST: cp h
# CHECK: encoding: [0xbc]
cp h
# CHECK-INST: add $ff
# CHECK: encoding: [0xc6,0xff]
add 255
# CHECK-INST: sub $01
# CHECK: encoding: [0xd6,0x01]
sub 1
# CHECK-INST: and $07
# CHECK: encoding: [0xe6,0x07]
and 7
# CHECK-INST: or $00
# CHECK: encoding: [0xf6,0x00]
or 0
# CHECK-INST: adc $ff
# CHECK: encoding: [0xce,0xff]
adc 255
# CHECK-INST: sbc $01
# CHECK: encoding: [0xde,0x01]
sbc 1
# CHECK-INST: xor $07
# CHECK: encoding: [0xee,0x07]
xor 7
# CHECK-INST: cp $00
# CHECK: encoding: [0xfe,0x00]
cp 0
# CHECK-INST: rlc e
# CHECK: encoding: [0xcb,0x03]
rlc e
# CHECK-INST: rrc l
# CHECK: encoding: [0xcb,0x0d]
rrc l
# CHECK-INST: rl b
# CHECK: encoding: [0xcb,0x10]
rl b
# CHECK-INST: rr a
# CHECK: encoding: [0xcb,0x1f]
rr a
# CHECK-INST: sla d
# CHECK: encoding: [0xcb,0x22]
sla d
# CHECK-INST: sra c
# CHECK: encoding: [0xcb,0x29]
sra c
# CHECK-INST: swap h
# CHECK: encoding: [0xcb,0x34]
swap h
# CHECK-INST: srl b
# CHECK: encoding: [0xcb,0x38]
srl b
# CHECK-INST: bit 4, a
# CHECK: encoding: [0xcb,0x67]
bit 4, a
# CHECK-INST: res 2, e
# CHECK: encoding: [0xcb,0x93]
res 2, e
# CHECK-INST: set 7, h
# CHECK: encoding: [0xcb,0xfc]
set 7, h
# CHECK-INST: stop
# CHECK: encoding: [0x10]
stop ; TODO: check stop 0
# CHECK-INST: halt
# CHECK: encoding: [0x76]
halt
# CHECK-INST: di
# CHECK: encoding: [0xf3]
di
# CHECK-INST: ei
# CHECK: encoding: [0xfb]
ei
# CHECK-INST: nop
# CHECK: encoding: [0x00]
nop
# CHECK-INST: inc h
# CHECK: encoding: [0x24]
inc h
# CHECK-INST: dec l
# CHECK: encoding: [0x2d]
dec l
# CHECK-INST: ret nz
# CHECK: encoding: [0xc0]
ret nz
# CHECK-INST: reti
# CHECK: encoding: [0xd9]
reti
# CHECK-INST: ret
# CHECK: encoding: [0xc9]
ret
# CHECK-INST: jr c, -16
# CHECK: encoding: [0x38,0xf0]
jr c,-16
# CHECK-INST: rlca
# CHECK: encoding: [0x07]
rlca
# CHECK-INST: rla
# CHECK: encoding: [0x17]
rla
# CHECK-INST: rrca
# CHECK: encoding: [0x0f]
rrca
# CHECK-INST: rra
# CHECK: encoding: [0x1f]
rra
# CHECK-INST: daa
# CHECK: encoding: [0x27]
daa
# CHECK-INST: cpl
# CHECK: encoding: [0x2f]
cpl
# CHECK-INST: scf
# CHECK: encoding: [0x37]
scf
# CHECK-INST: ccf
# CHECK: encoding: [0x3f]
ccf
# CHECK-INST: call $0001
# CHECK: encoding: [0xcd,0x01,0x00]
call 0x0001
# CHECK-INST: call nz, $ffff
# CHECK: encoding: [0xc4,0xff,0xff]
call nz, 0xFFFF
# CHECK-INST: jp $0001
# CHECK: encoding: [0xc3,0x01,0x00]
jp 0x0001
# CHECK-INST: jp c, $ffff
# CHECK: encoding: [0xda,0xff,0xff]
jp c, 0xFFFF
# CHECK-INST: jp (hl)
# CHECK: encoding: [0xe9]
jp (hl)
# CHECK-INST: push hl
# CHECK: encoding: [0xe5]
push hl
# CHECK-INST: pop af
# CHECK: encoding: [0xf1]
pop af
# CHECK-INST: add sp, -10
# CHECK: encoding: [0xe8,0xf6]
add sp, -10
# CHECK-INST: ld hl, sp, 127
# CHECK: encoding: [0xf8,0x7f]
ld hl, sp + 127
# CHECK-INST: ld hl, sp, -128
# CHECK: encoding: [0xf8,0x80]
ld hl, sp - 128
# CHECK-INST: ld sp, hl
# CHECK: encoding: [0xf9]
ld sp, hl
# CHECK-INST: ld ($fedc), sp
# CHECK: encoding: [0x08,0xdc,0xfe]
ld (0xfedc), sp
# CHECK-INST: ldh ($01), a
# CHECK: encoding: [0xe0,0x01]
ldh (0x01), a
# CHECK-INST: ldh a, ($01)
# CHECK: encoding: [0xf0,0x01]
ldh a, (0x01)
# CHECK-INST: ld a, ($fe01)
# CHECK: encoding: [0xfa,0x01,0xfe]
ld a, (0xfe01)
# CHECK-INST: ld ($fe01), a
# CHECK: encoding: [0xea,0x01,0xfe]
ld (0xfe01), a
# CHECK-INST: ldi (hl), a
# CHECK: encoding: [0x22]
ldi (hl), a
# CHECK-INST: ldi a, (hl)
# CHECK: encoding: [0x2a]
ldi a, (hl)
# CHECK-INST: ldd (hl), a
# CHECK: encoding: [0x32]
ldd (hl), a
# CHECK-INST: ldd a, (hl)
# CHECK: encoding: [0x3a]
ldd a, (hl)
# CHECK-INST: ld a, (bc)
# CHECK: encoding: [0x0a]
ld a, (bc)
# CHECK-INST: ld (de), a
# CHECK: encoding: [0x12]
ld (de), a
# CHECK-INST: rst $30
# CHECK: encoding: [0xf7]
rst 0x30
