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
