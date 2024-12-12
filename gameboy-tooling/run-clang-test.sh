#!/bin/bash
set -e

binary_out=$(mktemp)

script_dir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

testbench_source=$1
shift

clang_args=
for arg in "$@"
do
    if [[ "$arg" == "--gdb" ]]
    then
        is_gdb="1"
    else
        clang_args="$clang_args $arg"
    fi
done

$script_dir/../build/bin/clang --target=gb-unknown-unknown -fuse-ld=lld --ld-path=$script_dir/../build/bin/ld.lld $testbench_source $script_dir/runtime.s $clang_args -o $binary_out

if [[ -n "$is_gdb" ]]
then
    trap '' INT
    $GAMEBOY_EMULATOR_PATH/gb_with_gdb.sh $binary_out
else
    $GAMEBOY_EMULATOR_PATH/emulate.out $binary_out
fi
