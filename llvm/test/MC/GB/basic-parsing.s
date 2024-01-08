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
# CHECK-INST: add 255
# CHECK: encoding: [0xc6,0xff]
add 255
# CHECK-INST: sub 1
# CHECK: encoding: [0xd6,0x01]
sub 1
# CHECK-INST: and 7
# CHECK: encoding: [0xe6,0x07]
and 7
# CHECK-INST: or 0
# CHECK: encoding: [0xf6,0x00]
or 0
# CHECK-INST: adc 255
# CHECK: encoding: [0xce,0xff]
adc 255
# CHECK-INST: sbc 1
# CHECK: encoding: [0xde,0x01]
sbc 1
# CHECK-INST: xor 7
# CHECK: encoding: [0xee,0x07]
xor 7
# CHECK-INST: cp 0
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
stop # TODO: check stop 0
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
# CHECK-INST: call 1
# CHECK: encoding: [0xcd,0x01,0x00]
call 0x0001
# CHECK-INST: call nz, 65535
# CHECK: encoding: [0xc4,0xff,0xff]
call nz, 0xFFFF
# CHECK-INST: jp 1
# CHECK: encoding: [0xc3,0x01,0x00]
jp 0x0001
# CHECK-INST: jp c, 65535
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
