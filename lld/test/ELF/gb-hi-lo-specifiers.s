# REQUIRES: gb

# RUN: llvm-mc -filetype=obj -triple=gb-unknown-unknown %s -o %t.unlinked.o
# RUN: ld.lld %t.unlinked.o -o %t.linked
# RUN: llvm-objdump -d %t.linked | FileCheck --check-prefixes=CHECK-ENCODING,CHECK-DIS %s

_start:
    add %lo memory
    adc %hi memory

.set memory, 0xefbc

# CHECK-ENCODING:   c6 bc
# CHECK-DIS: add $bc

# CHECK-ENCODING: ce ef
# CHECK-DIS: adc $ef
