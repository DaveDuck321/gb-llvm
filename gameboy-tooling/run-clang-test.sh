#!/bin/bash
set -e

binary_out=$(mktemp)

script_dir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

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

$script_dir/../build/bin/clang -g --target=gb-unknown-unknown $script_dir/runtime.S $clang_args -o $binary_out

if [[ -n "$is_gdb" ]]
then
    trap '' INT
    $GAMEBOY_EMULATOR_PATH/gb_with_gdb.sh $binary_out
else
    $GAMEBOY_EMULATOR_PATH/emulate.out $binary_out
fi
