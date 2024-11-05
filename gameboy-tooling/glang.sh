#!/usr/bin/bash

script_dir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

linker_script=$script_dir/gb.ld

export PATH=$script_dir/../build/bin/:$PATH

pushd $GRAPHENE_PATH
hatch run dev:glang -T $linker_script --target gameboy $@
