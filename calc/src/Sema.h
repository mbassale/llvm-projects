#ifndef SEMA_H
#define SEMA_H

#include "AST.h"

class Sema {
public:
  bool semantic(AST *Tree);
};

#endif // SEMA_H
