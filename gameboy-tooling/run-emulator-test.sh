#!/bin/sh
set -e

testbench=$(mktemp)
to_test=$(mktemp)
binary_out=$(mktemp)

script_dir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

export PATH="$script_dir/../build/bin/":$PATH

cat $1 | llvm-mc --triple=gb --filetype=obj -g > $testbench
cat $2 | llc $3 -verify-machineinstrs --mtriple=gb --filetype=obj > $to_test

ld.lld --script "$script_dir/gb.ld" $testbench $to_test -o $binary_out

if [[ -z $4 ]]
then
    /home/tom/Programming/GameBoy/emulate.out $binary_out
else
    trap '' INT
    /home/tom/Programming/GameBoy/gb_with_gdb.sh $binary_out
fi
