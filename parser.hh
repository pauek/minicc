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

   bool is_type(std::string t) const;
   bool is_literal(std::string s) const;
   
public:
   Parser(std::istream *in, std::ostream* err = &std::cerr);

   void error(std::string msg);
   void warning(std::string msg);

   AstNode *parse();
   AstNode *parse_macro();
   AstNode *parse_using_declaration();
   AstNode *parse_func_or_var(std::string typ);
      void  parse_function(FuncDecl *fn);
      void  parse_parameter_list(std::vector<FuncDecl::Param>& params);
      bool  parse_param(FuncDecl::Param& p);
      void  parse_block(Block *stmt);
      Stmt *parse_stmt();
      void  parse_colon(Stmt *stmt);
      void  parse_for(Stmt *stmt);
      void  parse_while(Stmt *stmt);
      void  parse_if(Stmt *stmt);
      void  parse_switch(Stmt *stmt);
      void  parse_expr_stmt(Stmt *stmt);
      Expr *parse_expr(Expr::Type max = Expr::assignment);
};

#endif
