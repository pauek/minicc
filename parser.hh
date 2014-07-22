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

   static set<Token::Type> _basic_types;
   bool is_builtin_type(Token::Type t) const;
   bool is_literal(std::string s) const;
   
   template<typename X> void _skip(X *n, std::string stopset = "\n\t ");

public:
   Parser(std::istream *in, std::ostream* err = &std::cerr);

   void error(std::string msg);
   void warning(std::string msg);

   AstNode *parse();
   AstNode *parse_macro();
   AstNode *parse_using_declaration();
   AstNode *parse_func_or_var();
      void  parse_function(FuncDecl *fn);
     Block *parse_block();
      Stmt *parse_stmt();
      Stmt *parse_iterstmt(string which);
      Stmt *parse_while();
      Stmt *parse_for();
      Stmt *parse_for_init_stmt();
      Stmt *parse_ifstmt();
      Stmt *parse_switch();
      Stmt *parse_exprstmt();
      Stmt *parse_declstmt();
      Stmt *parse_jumpstmt();

      Type *parse_type();

      Expr *parse_expr();
      Expr *parse_binaryexpr(Expr::Type max = Expr::comma);
};

#endif
