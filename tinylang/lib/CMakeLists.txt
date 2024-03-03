
add_executable(tinylang
  Basic/Diagnostic.cpp
  Basic/TokenKinds.cpp
  ../tools/driver/tinylang.cpp
)

target_link_libraries(tinylang PRIVATE ${llvm_libs})
