#ifndef INSTRUMENTER_HH
#define INSTRUMENTER_HH

#include "ast.hh"

class Instrumenter {
   public:
    void instrument(AstNode *program);
};

#endif