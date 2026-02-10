#!/bin/bash
set -e

script_dir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

pushd $script_dir/..

RELEASE_BUILD_DIR=build-release
RELEASE_OUT_DIR=release-out

mkdir $RELEASE_OUT_DIR
mkdir $RELEASE_OUT_DIR/bin/

# Build
mkdir -p $RELEASE_BUILD_DIR
cmake -S llvm -B $RELEASE_BUILD_DIR -G Ninja \
    -DPACKAGE_BUGREPORT="https://github.com/DaveDuck321/gb-llvm" \
    -DCMAKE_BUILD_TYPE=MinSizeRel \
    -DLLVM_ENABLE_ASSERTIONS=ON \
    -DCMAKE_C_COMPILER=clang \
    -DCMAKE_CXX_COMPILER=clang++ \
    -DLLVM_ENABLE_PROJECTS="clang;llvm;lld;lldb" \
    -DLLVM_TARGETS_TO_BUILD="" \
    -DLLVM_EXPERIMENTAL_TARGETS_TO_BUILD=GB \
    -DLLVM_ENABLE_RUNTIMES="compiler-rt" \
    -DLLVM_RUNTIME_TARGETS="gb-unknown-unknown" \
    -DLLVM_BUILTIN_TARGETS="gb-unknown-unknown" \
    -DLLVM_PARALLEL_LINK_JOBS=8 \
    -DLLVM_ENABLE_LLD=ON \
    -DCLANG_DEFAULT_CXX_STDLIB=libc++ \
    -DCLANG_DEFAULT_RTLIB=compiler-rt \
    -DCLANG_DEFAULT_LINKER=lld \
    -DRUNTIMES_gb-unknown-unknown_COMPILER_RT_BUILD_BUILTINS=ON \
    -DRUNTIMES_gb-unknown-unknown_COMPILER_RT_BAREMETAL_BUILD=ON \
    -DBUILTINS_gb-unknown-unknown_COMPILER_RT_BAREMETAL_BUILD=ON \
    -DBUILTINS_gb-unknown-unknown_CMAKE_BUILD_TYPE=release

ninja -C $RELEASE_BUILD_DIR

mkdir -p $RELEASE_BUILD_DIR/lib/clang/21/lib/gb-unknown-unknown/ldscripts
cp $script_dir/gb.ld $RELEASE_BUILD_DIR/lib/clang/21/lib/gb-unknown-unknown/ldscripts/gb.ld

# Copy artifacts
cp --preserve=links $RELEASE_BUILD_DIR/bin/* $RELEASE_OUT_DIR/bin/

mkdir -p $RELEASE_OUT_DIR/lib/clang/21/
cp -rL $RELEASE_BUILD_DIR/lib/clang/21/* $RELEASE_OUT_DIR/lib/clang/21/

cp $script_dir/runtime.S $RELEASE_OUT_DIR/runtime.S
cp $script_dir/release-extras/* $RELEASE_OUT_DIR/


# Make sure everything is setup properly
pushd $RELEASE_OUT_DIR
./test.sh
popd

# Compress and export
echo "Compressing"
tar -czf release.tar.gz -C $RELEASE_OUT_DIR .
echo "Done: saved to $(pwd)/release.tar.gz"

# Cleanup
rm -r $RELEASE_OUT_DIR
