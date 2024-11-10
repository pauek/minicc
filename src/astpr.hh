#ifndef ASTPRINT_HH
#define ASTPRINT_HH

#include <assert.h>
#include <iostream>
#include "ast.hh"

void ast_print(AstNode *node, std::ostream& o = std::cout);

#endif
