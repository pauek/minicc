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
   
   template<typename X> 
   void _skip(X *n, std::string stopset = "\n\t ");
   
   void _skip(std::string stopset = "\n\t ");

   void error(AstNode *n, std::string msg);

   template<class Node>
   typename Node::Error *error(std::string msg);

public:
   Parser(std::istream *in, std::ostream* err = &std::cerr);

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
      Stmt *parse_ifstmt();
      Stmt *parse_switch();
      Stmt *parse_exprstmt();
      Stmt *parse_declstmt();
      Stmt *parse_decl_or_expr_stmt();
      Stmt *parse_jumpstmt();

      Type *parse_type();

      Expr *parse_expr(Expr::Type max = Expr::comma);
      Expr *parse_primary_expr();
      Expr *parse_identifier(Token tok);
      Expr *parse_postfix_expr(Expr *);
      Expr *parse_unary_expr();
      Expr *parse_callexpr(Expr *);
      Expr *parse_indexexpr(Expr *);
      Expr *parse_fieldexpr(Expr *, Token);
      Expr *parse_increxpr(Expr *, Token);
};

#endif
