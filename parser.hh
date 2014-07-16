#ifndef PARSER_H
#define PARSER_H

#include <set>
#include "ast.hh"
#include "input.hh"

struct ParseError {
   std::string msg;
   ParseError(std::string _msg) : msg(_msg) {}
};

class Parser {
   Input _in;
   std::ostream *_err;

   static std::set<std::string> _types;

public:
   Parser(std::istream *in, std::ostream* err = &std::cerr);

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
   void     parse_statement(Statement *stmt);
   void     parse_colon(Statement *stmt);
};

std::string test_parser_separator(std::string line);
void test_parser(std::string filename);

#endif
