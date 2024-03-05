#include "tinylang/Lexer/Lexer.h"
#include <llvm/Support/Compiler.h>
#include <tinylang/Basic/TokenKinds.h>
#include <utility>

using namespace tinylang;

void KeywordFilter::addKeyword(StringRef Keyword, tok::TokenKind TokenCode) {
  HashTable.insert(std::make_pair(Keyword, TokenCode));
}

void KeywordFilter::addKeywords() {
#define KEYWORD(NAME, FLAGS) addKeyword(StringRef(#NAME), tok::kw_##NAME);
#include "tinylang/Basic/TokenKinds.def"
}

namespace charinfo {

LLVM_READNONE inline bool isASCII(char ch) {
  return static_cast<unsigned char>(ch) <= 127;
}

LLVM_READNONE inline bool isVerticalWhitespace(char ch) {
  return isASCII(ch) && (ch == '\n' || ch == '\r');
}

LLVM_READNONE inline bool isHorizontalWhitespace(char ch) {
  return isASCII(ch) && (ch == ' ' || ch == '\t' || ch == '\f' || ch == '\v');
}

LLVM_READNONE inline bool isWhitespace(char ch) {
  return isHorizontalWhitespace(ch) || isVerticalWhitespace(ch);
}

LLVM_READNONE inline bool isDigit(char ch) {
  return isASCII(ch) && (ch >= '0' && ch <= '9');
}

LLVM_READNONE inline bool isHexDigit(char ch) {
  return isASCII(ch) && (isDigit(ch) || (ch >= 'A' && ch <= 'F'));
}

LLVM_READNONE inline bool isIdentifierStart(char ch) {
  return isASCII(ch) &&
         (ch == '_' || (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z'));
}

LLVM_READNONE inline bool isIdentifierBody(char ch) {
  return isASCII(ch) && ((ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') ||
                         (ch >= '0' && ch <= '9'));
}

} // namespace charinfo

void Lexer::next(Token &Result) {
  while (*CurPtr && charinfo::isWhitespace(*CurPtr)) {
    ++CurPtr;
  }

  // EOF
  if (!*CurPtr) {
    Result.setKind(tok::eof);
    return;
  }

  // Identifier start
  if (charinfo::isIdentifierStart(*CurPtr)) {
    identifier(Result);
    return;
  }

  // Number start
  if (charinfo::isDigit(*CurPtr)) {
    number(Result);
    return;
  }

  // String literal
  if (*CurPtr == '\'' || *CurPtr == '\"') {
    string(Result);
    return;
  }

  // Punctuation
  switch (*CurPtr) {
#define CASE(ch, tok)                                                          \
  case ch:                                                                     \
    makeToken(Result, CurPtr + 1, tok);                                        \
    break
    CASE('=', tok::equal);
    CASE('#', tok::hash);
    CASE('+', tok::plus);
    CASE('-', tok::minus);
    CASE('*', tok::star);
    CASE('/', tok::slash);
    CASE(',', tok::comma);
    CASE('.', tok::period);
    CASE(';', tok::semicolon);
    CASE(')', tok::r_paren);
#undef CASE
  case '(':
    if (*(CurPtr + 1) == '*') {
      comment();
      next(Result);
    } else {
      makeToken(Result, CurPtr + 1, tok::l_paren);
    }
    break;
  case ':':
    if (*(CurPtr + 1) == '=') {
      makeToken(Result, CurPtr + 2, tok::colonequal);
    } else {
      makeToken(Result, CurPtr + 1, tok::colon);
    }
    break;
  case '<':
    if (*(CurPtr + 1) == '=') {
      makeToken(Result, CurPtr + 2, tok::lessequal);
    } else {
      makeToken(Result, CurPtr + 1, tok::less);
    }
    break;
  case '>':
    if (*(CurPtr + 1) == '=') {
      makeToken(Result, CurPtr + 2, tok::greaterequal);
    } else {
      makeToken(Result, CurPtr + 1, tok::greater);
    }
    break;
  default:
    Result.setKind(tok::unknown);
    break;
  }
}

void Lexer::identifier(Token &Result) {
  const char *Start = CurPtr;
  const char *End = CurPtr + 1;
  while (charinfo::isIdentifierBody(*End)) {
    ++End;
  }
  StringRef Name(Start, End - Start);
  makeToken(Result, End, Keywords.getKeyword(Name, tok::identifier));
}

void Lexer::number(Token &Result) {
  const char *Start = CurPtr;
  const char *End = CurPtr + 1;
  tok::TokenKind Kind = tok::unknown;
  bool IsHex = false;
  while (*End) {
    if (!charinfo::isHexDigit(*End)) {
      break;
    }
    if (!charinfo::isDigit(*End)) {
      IsHex = true;
    }
    ++End;
  }
  switch (*End) {
  case 'H':
    Kind = tok::integer_literal;
    ++End;
    break;
  default:
    if (IsHex) {
      DiagsEng.report(getLoc(), diag::err_hex_digit_in_decimal);
    }
    Kind = tok::integer_literal;
    break;
  }
  makeToken(Result, End, tok::integer_literal);
}

void Lexer::string(Token &Result) {
  const char *Start = CurPtr;
  const char *End = CurPtr + 1;
  while (*End && *End != *Start && !charinfo::isVerticalWhitespace(*End)) {
    ++End;
  }
  if (charinfo::isVerticalWhitespace(*End)) {
    DiagsEng.report(getLoc(), diag::err_unterminated_char_or_string);
  }
  makeToken(Result, End + 1, tok::string_literal);
}

void Lexer::comment() {
  const char *End = CurPtr + 2;
  unsigned Level = 1;
  while (*End && Level) {
    // nested comment
    if (*End == '(' && *(End + 1) == '*') {
      End += 2;
      Level++;
    }
    // check for end of comment
    else if (*End == '*' && *(End + 1) == ')') {
      End += 2;
      Level--;
    } else {
      ++End;
    }
    if (!*End) {
      DiagsEng.report(getLoc(), diag::err_unterminated_block_comment);
    }
    CurPtr = End;
  }
}

void Lexer::makeToken(Token &Result, const char *TokEnd, tok::TokenKind Kind) {
  size_t TokenLength = TokEnd - CurPtr;
  Result.Ptr = CurPtr;
  Result.Length = TokenLength;
  Result.Kind = Kind;
  CurPtr = TokEnd;
}
