#!/bin/sh

set -e
./bin/clang++ --target=gb-unknown-unknown test.cpp runtime.S
rm a.out
echo "Test Passed! Toolchain is ready to use."
