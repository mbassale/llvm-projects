
cmake_minimum_required(VERSION 3.20.0)
project("tinylang")

find_package(LLVM REQUIRED CONFIG)
message("Found LLVM ${LLVM_PACKAGE_VERSION}, build type ${LLVM_BUILD_TYPE}")
list(APPEND CMAKE_MODULE_PATH ${LLVM_DIR})

set(NO_RTTI "-fno-rtti")
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
add_definitions(${NO_RTTI})

separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})
include_directories(SYSTEM ${LLVM_INCLUDE_DIRS} "include")
llvm_map_components_to_libnames(llvm_libs 
  Analysis
  CodeGen
  Core
  IPO
  AggressiveInstCombine
  InstCombine
  Instrumentation
  MC
  MCParser
  ObjCARCOpts
  Option
  ScalarOpts
  Support
  TransformUtils
  Vectorize
  AArch64CodeGen
  AArch64AsmParser
)

add_subdirectory("lib")
add_subdirectory("tests")

# TinyLang Compiler Driver Executable
add_executable(tinylang
  tools/driver/tinylang.cpp
)
target_link_libraries(tinylang PRIVATE tinylanglib ${llvm_libs})
