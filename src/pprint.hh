#ifndef PRETTYPRINT_HH
#define PRETTYPRINT_HH

#include <iostream>
#include "ast.hh"

void        pprint(AstNode *ast, std::ostream& out = std::cout);
std::string spprint(AstNode *ast);

#endif
