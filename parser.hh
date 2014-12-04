#ifndef PARSER_H
#define PARSER_H

#include <set>
#include "ast.hh"
#include "input.hh"

struct ParseError {};

class Parser {
   Input _in;
   std::ostream *_err;

   set<std::string> _types; // things known as types
   bool _is_type(std::string);

   void _skip(string stopset) {
      CommentSeq *cn = _in.skip(stopset);
      if (cn != 0) delete cn;
   }

   template<typename X> 
   void _skip(X *n, std::string stopset = "\n\t ") {
      n->comments.push_back(_in.skip(stopset));
   }
   
   void error(AstNode *n, std::string msg);

   template<class Node>
   typename Node::Error *error(std::string msg);

   void parse_expr_seq(AstNode *n, std::vector<Expr*>& v);
   void parse_type_seq(AstNode *n, std::vector<TypeSpec*>& v);
   bool _parse_type_process_token(TypeSpec *type, Token tok, Pos p);

   Decl *_parse_vardecl(std::string name, Decl::Kind kind, CommentSeq *comm);
   Decl *_parse_arraydecl(std::string name, Decl::Kind kind, CommentSeq *comm);
   Decl *_parse_objdecl(std::string name, CommentSeq *comm);

public:
             Parser(std::istream *in, std::ostream* err = &std::cerr);

const Input& input() const { return _in; }

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
   ExprStmt *parse_exprstmt(bool is_return = false);
   DeclStmt *parse_declstmt(bool is_typedef = false);
       Stmt *parse_decl_or_expr_stmt();
       Stmt *parse_jumpstmt();

   TypeSpec *parse_typespec();
  FullIdent *parse_ident(Token tok, Pos ini);

 StructDecl *parse_struct();
TypedefDecl *parse_typedef();
   EnumDecl *parse_enum();

       Expr *parse_expr(Expr::Kind max = Expr::Comma);
       Expr *parse_primary_expr();
       Expr *parse_postfix_expr(Expr *);
       Expr *parse_unary_expr();
       Expr *parse_callexpr(Expr *);
       Expr *parse_indexexpr(Expr *);
       Expr *parse_fieldexpr(Expr *, Token);
       Expr *parse_increxpr(Expr *, Token);
       Expr *parse_exprlist();
};

#endif
