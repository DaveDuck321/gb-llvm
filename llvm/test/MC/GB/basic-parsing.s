# RUN: llvm-mc %s -triple=gb -show-encoding \
# RUN:      | FileCheck -check-prefixes=CHECK,CHECK-INST %s

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
