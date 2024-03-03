#include "tinylang/Lexer/Token.h"
#include "gtest/gtest.h"
#include <llvm/ADT/StringRef.h>
#include <tinylang/Basic/TokenKinds.h>

using namespace llvm;
using namespace tinylang;
using namespace tinylang::tok;

TEST(TokenTest, TestHelpers) {
  StringRef literalStr = "test";
  Token token(literalStr.data(), literalStr.size(), TokenKind::string_literal);
  ASSERT_EQ(token.getKind(), TokenKind::string_literal);
  ASSERT_TRUE(token.is(TokenKind::string_literal));
  ASSERT_FALSE(token.is(TokenKind::integer_literal));
  ASSERT_TRUE(
      token.isOneOf(TokenKind::integer_literal, TokenKind::string_literal));
  ASSERT_EQ(token.getName(), "string_literal");
  ASSERT_EQ(token.getLength(), literalStr.size());
  ASSERT_EQ(token.getLiteralData(), literalStr);
}
