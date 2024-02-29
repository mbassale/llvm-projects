
cmake_minimum_required(VERSION 3.20.0)
project("calc")

find_package(LLVM REQUIRED CONFIG)
message("Found LLVM ${LLVM_PACKAGE_VERSION}, build type ${LLVM_BUILD_TYPE}")
list(APPEND CMAKE_MODULE_PATH ${LLVM_DIR})

set(NO_RTTI "-fno-rtti")
add_definitions(${NO_RTTI})

separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})
include_directories(SYSTEM ${LLVM_INCLUDE_DIRS})
llvm_map_components_to_libnames(llvm_libs Core)

add_subdirectory("src")
