
add_library(tinylanglib
  STATIC
  Basic/Diagnostic.cpp
  Basic/TokenKinds.cpp
  Lexer/Lexer.cpp
  Parser/Parser.cpp
  Sema/Scope.cpp
  Sema/Sema.cpp
  CodeGen/CodeGenerator.cpp
)

