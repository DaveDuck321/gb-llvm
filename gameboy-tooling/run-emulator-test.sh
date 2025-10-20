#!/bin/sh
set -e

testbench=$(mktemp)
to_test=$(mktemp)
binary_out=$(mktemp)

script_dir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

export PATH="$script_dir/../build/bin/":$PATH

asm_file=$1
shift
ir_file=$1
shift

llc_args=
for arg in "$@"
do
    if [[ "$arg" == "--gdb" ]]
    then
        is_gdb="1"
    else
        llc_args="$llc_args $arg"
    fi
done

cat $asm_file | llvm-mc --triple=gb --filetype=obj -g > $testbench
cat $ir_file | llc $llc_args -verify-machineinstrs --mtriple=gb --filetype=obj > $to_test

lib_builtins="$script_dir/../build/lib/clang/*/lib/gb-unknown-unknown/libclang_rt.builtins.a"
ld.lld --script "$script_dir/gb.ld" $testbench $to_test $lib_builtins -o $binary_out

if [[ -n "$is_gdb" ]]
then
    trap '' INT
    $GAMEBOY_EMULATOR_PATH/gb_with_gdb.sh $binary_out
else
    $GAMEBOY_EMULATOR_PATH/emulate.out $binary_out
fi
