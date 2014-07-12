#ifndef PARSER_H
#define PARSER_H

#include "ast.hh"
#include "input.hh"

class Parser {
   Input _in;
public:
   Parser(std::istream* i) : _in(i) {}

   void error(std::string msg);
   void warning(std::string msg);
   
   AstNode* parse();
   AstNode* parse_macro();
};

#endif
