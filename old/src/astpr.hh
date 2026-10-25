#ifndef ASTPRINT_HH
#define ASTPRINT_HH

#include <assert.h>
#include <iostream>
#include "datatype.hh"

void ast_print(AstNode *node, std::ostream& o = std::cout);

#endif
