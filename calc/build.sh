#!/bin/sh

mkdir -p build
cmake -GNinja -DLLVM_DIR=$LLVM_DIR -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++ -B build -S .
cmake --build build
