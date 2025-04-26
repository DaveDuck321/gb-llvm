#!/bin/sh

script_dir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
$script_dir/../build/bin/llvm-objcopy -O binary $1 out.rom --gap-fill 0
