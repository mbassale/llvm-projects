#!/bin/sh

#!/bin/bash

# Function to find LLVM directory using llvm-config
find_llvm_dir() {
    # Try to locate llvm-config for version 17
    LLVM_CONFIG=$(which llvm-config-17 2>/dev/null || which llvm-config 2>/dev/null)

    if [ -z "$LLVM_CONFIG" ]; then
        echo "llvm-config not found. Please make sure LLVM-17 is installed." >&2
        exit 1
    fi

    # Use llvm-config to get the installation prefix
    LLVM_DIR=$($LLVM_CONFIG --prefix)
    echo "Found LLVM at ${LLVM_DIR}"

     # Assuming clang-17 and clang++-17 are in the same directory as llvm-config
    export CC="${LLVM_DIR}/bin/clang"
    export CXX="${LLVM_DIR}/bin/clang++"

    if [ ! -f "$CC" ] || [ ! -f "$CXX" ]; then
        echo "Clang-17 compiler not found in the LLVM directory. Please make sure it is installed." >&2
        exit 1
    fi

    echo "Using Clang-17 for C compiler: ${CC}"
    echo "Using Clang-17 for C++ compiler: ${CXX}"
}

# Main script logic
if [ $# -eq 0 ]; then
    echo "No argument provided. Detecting LLVM-17 installation directory..."
    find_llvm_dir
else
    LLVM_DIR=$1
    echo "Using provided LLVM directory: ${LLVM_DIR}"
    echo "Using provided LLVM directory: ${LLVM_DIR}"
    export CC="${LLVM_DIR}/bin/clang"
    export CXX="${LLVM_DIR}/bin/clang++"
    
    if [ ! -f "$CC" ] || [ ! -f "$CXX" ]; then
        echo "Clang-17 compiler not found in the provided LLVM directory. Please make sure it is installed." >&2
        exit 1
    fi
    
    echo "Using Clang-17 for C compiler: ${CC}"
    echo "Using Clang-17 for C++ compiler: ${CXX}"
fi

# Proceed with using LLVM_DIR for further operations...
echo "Proceeding with LLVM directory: ${LLVM_DIR}"

mkdir -p build
cmake -GNinja -DLLVM_DIR=${LLVM_DIR} -DCMAKE_C_COMPILER=${CC} -DCMAKE_CXX_COMPILER=${CXX} -B build -S .
cmake --build build
