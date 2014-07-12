#ifndef PARSER_H
#define PARSER_H

#include "ast.h"
#include "input.h"

class Parser {
   Input _in;
public:
   Parser(std::istream* i) : _in(i) {}
   
   AstNode* parse();
};

#endif
