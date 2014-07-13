#ifndef PARSER_H
#define PARSER_H

#include <set>
#include "ast.hh"
#include "input.hh"

class Parser {
   Input _in;

   static std::set<std::string> _types;

public:
   Parser(std::istream *i);

   void error(std::string msg);
   void warning(std::string msg);

   bool is_type(std::string t) const;
   
   AstNode *parse();
   AstNode *parse_macro();
   AstNode *parse_using_declaration();
   AstNode *parse_func_or_var(std::string typ);
   void     parse_function(FuncDecl *fn);
   void     parse_parameter_list(std::vector<FuncDecl::Param>& params);
   bool     parse_param(FuncDecl::Param& p);
   void     parse_block(Block *b);
};

#endif
