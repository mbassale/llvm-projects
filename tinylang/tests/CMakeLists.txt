
find_package(GTest CONFIG REQUIRED)

add_executable(
  tinylang-tests
  main.cpp
  token.cpp
  lexer.cpp
  parser.cpp
  codegen.cpp
)

target_link_libraries(tinylang-tests PRIVATE ${llvm_libs} tinylanglib GTest::gtest)

