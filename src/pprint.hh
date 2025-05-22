#ifndef PRETTYPRINT_HH
#define PRETTYPRINT_HH

#include <iostream>
#include "ast.hh"

void        pretty_print(AstNodeCore *ast, std::ostream& out = std::cout);
std::string pretty_to_string(AstNodeCore *ast);

#endif
