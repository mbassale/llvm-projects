
find_package(GTest CONFIG REQUIRED)

add_executable(
  tinylang-tests
  token.cpp
  lexer.cpp
  parser.cpp
)

target_link_libraries(tinylang-tests PRIVATE LLVMCore LLVMSupport tinylanglib GTest::gtest GTest::gtest_main)

