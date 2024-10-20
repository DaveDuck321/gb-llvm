#!/bin/bash
set -e

testbench_obj=$(mktemp)
runtime_obj=$(mktemp)
graphene_obj=$(mktemp)
binary_out=$(mktemp)

script_dir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

export PATH="$script_dir/../build/bin/:$PATH"
export GRAPHENE_CLANG_CMD="/usr/bin/clang"

testbench_source=$1
shift

graphene_args=
for arg in "$@"
do
    if [[ "$arg" == "--gdb" ]]
    then
        is_gdb="1"
    else
        graphene_args="$graphene_args $arg"
    fi
done

$script_dir/glang.sh -c $graphene_args -o $graphene_obj
cat $testbench_source | llvm-mc --triple=gb --filetype=obj -g > $testbench_obj
cat $script_dir/runtime.s | llvm-mc --triple=gb --filetype=obj -g > $runtime_obj

ld.lld --script "$script_dir/gb.ld" $testbench_obj $graphene_obj $runtime_obj -o $binary_out

if [[ -n "$is_gdb" ]]
then
    trap '' INT
    /home/tom/Programming/GameBoy/gb_with_gdb.sh $binary_out
else
    /home/tom/Programming/GameBoy/emulate.out $binary_out
fi
