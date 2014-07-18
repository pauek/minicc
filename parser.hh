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
   
   void _skip(AstNode *n);
   void _parse_while_or_if(Stmt *stmt, string which);

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
     Block *parse_block();
      void  parse_colon(Stmt *stmt);
      Stmt *parse_stmt();
      Stmt *parse_iterstmt(string which);
      Stmt *parse_while();
      Stmt *parse_for();
      Stmt *parse_for_init_stmt();
      Stmt *parse_ifstmt();
      Stmt *parse_switch();
      Stmt *parse_exprstmt();
      Stmt *parse_declstmt();
      Expr *parse_expr(Expr::Type max = Expr::assignment);
};

#endif
