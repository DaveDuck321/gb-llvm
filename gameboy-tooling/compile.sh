#!/usr/bin/sh

script_dir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

pushd $script_dir/..

mkdir build
cmake -S llvm -B build -G Ninja \
    -DLLVM_ENABLE_PROJECTS="lld;lldb;clang" \
    -DCMAKE_BUILD_TYPE=Debug \
    -DLLVM_USE_LINKER=lld \
    -DLLVM_TARGETS_TO_BUILD="" \
    -DLLVM_EXPERIMENTAL_TARGETS_TO_BUILD=GB \
    -DLLDB_TEST_COMPILER=clang++ \
    -DCMAKE_C_COMPILER=clang \
    -DCMAKE_CXX_COMPILER=clang++ \
    -DLLVM_OPTIMIZED_TABLEGEN=ON \
    -DLLVM_PARALLEL_LINK_JOBS=8 \
    -DCMAKE_EXE_LINKER_FLAGS="-fuse-ld=lld"

ninja -C build

mkdir build-compiler-rt
cmake -S compiler-rt -B build-compiler-rt -G Ninja \
    -DCMAKE_BUILD_TYPE=Release \
    -DCOMPILER_RT_DEFAULT_TARGET_ARCH=gb \
    -DCOMPILER_RT_DEFAULT_TARGET_ONLY=ON \
    -DCMAKE_C_COMPILER_WORKS=ON \
    -DCMAKE_CXX_COMPILER_WORKS=ON \
    -DCMAKE_C_COMPILER=$(pwd)/build/bin/clang \
    -DCMAKE_CXX_COMPILER=$(pwd)/build/bin/clang++ \
    -DCMAKE_AR=$(pwd)/build/bin/llvm-ar \
    -DCMAKE_NM=$(pwd)/build/bin/llvm-nm \
    -DCMAKE_EXE_LINKER_FLAGS="-fuse-ld=lld" \
    -DCMAKE_ASM_COMPILER_TARGET=gb \
    -DCMAKE_C_COMPILER_TARGET=gb \
    -DCMAKE_CXX_COMPILER_TARGET=gb \
    -DCOMPILER_RT_BUILD_BUILTINS=ON \
    -DCOMPILER_RT_BAREMETAL_BUILD=ON \
    -DCMAKE_SYSTEM_NAME=Generic-ELF \
    -DCMAKE_CROSSCOMPILING=ON \
    -DNO_INCLUDE_UNWIND_H=ON \
    -DCOMPILER_RT_OS_DIR=gb-unknown-unknown

ninja -C build-compiler-rt

mkdir -p $(pwd)/build/lib/clang/19/lib/gb-unknown-unknown
mkdir -p $(pwd)/build/lib/clang/19/lib/gb-unknown-unknown/ldscripts
ln -s $(pwd)/build-compiler-rt/lib/gb-unknown-unknown/* $(pwd)/build/lib/clang/19/lib/gb-unknown-unknown/
ln -s $(pwd)/gameboy-tooling/gb.ld $(pwd)/build/lib/clang/19/lib/gb-unknown-unknown/ldscripts/gb.ld
