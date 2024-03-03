#include "tinylang/Basic/TokenKinds.h"
#include "llvm/Support/ErrorHandling.h"

using namespace tinylang;
using namespace tinylang::tok;

static const char *const TokNames[] = {
#define TOK(ID) #ID,
#define KEYWORD(ID, FLAG) #ID,
#include "tinylang/Basic/TokenKinds.def"
    nullptr};

const char *getTokenName(TokenKind Kind) { return TokNames[Kind]; }

const char *getPunctuatorSpelling(TokenKind Kind) {
  switch (Kind) {
#define PUNCTUATOR(ID, SP)                                                     \
  case ID:                                                                     \
    return SP;
#include "tinylang/Basic/TokenKinds.def"
  default:
    break;
  }
  return nullptr;
}

const char *getKeywordSpelling(TokenKind Kind) {
  switch (Kind) {
#define KEYWORD(ID, FLAG)                                                      \
  case kw_##ID:                                                                \
    return #ID;
#include "tinylang/Basic/TokenKinds.def"
  default:
    break;
  }
  return nullptr;
}
