#!/usr/bin/sh

script_dir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

pushd $script_dir/..

mkdir build
cmake -S llvm -B build -G Ninja \
    -DCMAKE_BUILD_TYPE=debug \
    -DCMAKE_C_COMPILER=clang \
    -DCMAKE_CXX_COMPILER=clang++ \
    -DLLVM_ENABLE_PROJECTS="clang;llvm;lld;lldb" \
    -DLLVM_TARGETS_TO_BUILD="" \
    -DLLVM_EXPERIMENTAL_TARGETS_TO_BUILD=GB \
    -DLLVM_ENABLE_RUNTIMES="compiler-rt" \
    -DLLVM_RUNTIME_TARGETS="gb-unknown-unknown" \
    -DLLVM_BUILTIN_TARGETS="gb-unknown-unknown" \
    -DLLVM_PARALLEL_LINK_JOBS=8 \
    -DLLVM_OPTIMIZED_TABLEGEN=ON \
    -DLLVM_ENABLE_LLD=ON \
    -DCLANG_DEFAULT_CXX_STDLIB=libc++ \
    -DCLANG_DEFAULT_RTLIB=compiler-rt \
    -DCLANG_DEFAULT_LINKER=lld \
    -DRUNTIMES_gb-unknown-unknown_COMPILER_RT_BUILD_BUILTINS=ON \
    -DRUNTIMES_gb-unknown-unknown_COMPILER_RT_BAREMETAL_BUILD=ON \
    -DBUILTINS_gb-unknown-unknown_COMPILER_RT_BAREMETAL_BUILD=ON \
    -DBUILTINS_gb-unknown-unknown_CMAKE_BUILD_TYPE=release


ninja -C build

mkdir -p $(pwd)/build/lib/clang/21/lib/gb-unknown-unknown/ldscripts
ln -s $(pwd)/gameboy-tooling/gb.ld $(pwd)/build/lib/clang/21/lib/gb-unknown-unknown/ldscripts/gb.ld
