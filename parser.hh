#ifndef PARSER_H
#define PARSER_H

#include <set>
#include "ast.hh"
#include "input.hh"

struct ParseError {
   Pos pos;
   std::string msg;

   ParseError(Pos p, std::string m) : pos(p), msg(m) {}
};

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
   void error(AstNode *n, Pos ini, Pos fin, std::string msg);
   void fatal_error(Pos p, std::string msg);

   template<class Node>
   typename Node::Error *error(std::string msg);

   void parse_expr_seq(AstNode *n, std::vector<Expr*>& v);
   void parse_type_seq(AstNode *n, std::vector<TypeSpec*>& v);
   bool _parse_type_process_token(TypeSpec *type, Token tok, Pos p);

   Decl *_parse_vardecl(AstNode *parent, std::string name, Decl::Kind kind, CommentSeq *comm);
   Decl *_parse_arraydecl(AstNode *parent, std::string name, Decl::Kind kind, CommentSeq *comm);
   Decl *_parse_objdecl(AstNode *parent, std::string name, CommentSeq *comm);

public:
             Parser(std::istream *in, std::ostream* err = &std::cerr);

const Input& input() const { return _in; }

    AstNode *parse();
    AstNode *parse_macro(AstNode *parent);
    AstNode *parse_using_declaration(AstNode *parent);
    AstNode *parse_func_or_var(AstNode *parent);
       void  parse_function(FuncDecl *fn);
      Block *parse_block(AstNode *parent);
       Stmt *parse_stmt(AstNode *parent);
       Stmt *parse_iterstmt(AstNode *parent, string which);
       Stmt *parse_while(AstNode *parent);
       Stmt *parse_for(AstNode *parent);
       Stmt *parse_ifstmt(AstNode *parent);
       Stmt *parse_switch(AstNode *parent);
   ExprStmt *parse_exprstmt(AstNode *parent, bool is_return = false);
   DeclStmt *parse_declstmt(AstNode *parent, bool is_typedef = false);
       Stmt *parse_decl_or_expr_stmt(AstNode *parent);
       Stmt *parse_jumpstmt(AstNode *parent);

   TypeSpec *parse_typespec(AstNode *parent);
  FullIdent *parse_ident(AstNode *parent, Token tok, Pos ini);

 StructDecl *parse_struct(AstNode *parent);
TypedefDecl *parse_typedef(AstNode *parent);
   EnumDecl *parse_enum(AstNode *parent);

       Expr *parse_expr(AstNode *parent, Expr::Kind max = Expr::Comma);
       Expr *parse_primary_expr(AstNode *parent);
       Expr *parse_postfix_expr(AstNode *, Expr *);
       Expr *parse_unary_expr(AstNode *parent);
       Expr *parse_callexpr(Expr *);
       Expr *parse_indexexpr(Expr *);
       Expr *parse_fieldexpr(Expr *, Token);
       Expr *parse_increxpr(Expr *, Token);
       Expr *parse_exprlist(AstNode *parent);
};

#endif
