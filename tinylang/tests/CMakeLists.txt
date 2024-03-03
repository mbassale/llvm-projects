
add_executable(
  tinylang-tests
  token.cpp
)

target_link_libraries(tinylang-tests PRIVATE LLVMCore LLVMSupport tinylanglib gtest gtest_main)

