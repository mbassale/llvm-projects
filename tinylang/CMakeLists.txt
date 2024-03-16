
cmake_minimum_required(VERSION 3.20.0)
project("tinylang")

# Set the default build type if not specified
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Debug CACHE STRING "Choose the type of build." FORCE)
endif()

find_package(LLVM REQUIRED CONFIG)
message("Found LLVM ${LLVM_PACKAGE_VERSION}, build type ${LLVM_BUILD_TYPE}")
list(APPEND CMAKE_MODULE_PATH ${LLVM_DIR})

set(NO_RTTI "-fno-rtti")
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
add_definitions(${NO_RTTI})

separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})
include_directories(SYSTEM ${LLVM_INCLUDE_DIRS} "include")

# List of common LLVM components
set(common_llvm_libs
  ${LLVM_TARGETS_TO_BUILD}
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
  CodeGen
  AsmParser
)

# Detect host system processor and add architecture-specific components
#if(CMAKE_HOST_SYSTEM_PROCESSOR MATCHES "x86_64")
  # For x86_64 architecture
  #  list(APPEND common_llvm_libs "X86")
  #elseif(CMAKE_HOST_SYSTEM_PROCESSOR MATCHES "aarch64|ARM64")
  # For ARM architecture
  # list(APPEND common_llvm_libs "AArch64")
  #endif()

# Map LLVM components to library names
llvm_map_components_to_libnames(llvm_libs ${common_llvm_libs})

add_subdirectory("lib")
add_subdirectory("tests")

# TinyLang Compiler Driver Executable
add_executable(tinylang
  tools/driver/tinylang.cpp
)
target_link_libraries(tinylang PRIVATE tinylanglib ${llvm_libs})
