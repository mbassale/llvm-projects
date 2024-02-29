#!/bin/sh

mkdir -p build
cmake -GNinja -DLLVM_DIR=$LLVM_DIR -B build -S .
cmake --build build
