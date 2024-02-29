
add_executable(calc
  Lexer.cpp
  Calc.cpp
)

target_link_libraries(calc PRIVATE ${llvm_libs})
