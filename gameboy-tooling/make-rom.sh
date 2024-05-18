#!/bin/sh
./build/bin/llvm-objcopy -O binary $1 out.rom --gap-fill 0
