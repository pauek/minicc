#ifndef PRETTYPRINT_HH
#define PRETTYPRINT_HH

#include <assert.h>
#include <iostream>
#include "ast.hh"

class PrettyPrinter : public AstVisitor {
   void print_block(Block *);

public:
   PrettyPrinter(std::ostream *o = &std::cout) : AstVisitor(o) {}

   void print(AstNode* x) { x->visit(this); }

   void visit_comment(CommentNode *x);
   void visit_include(Include *x);
   void visit_macro(Macro *x);
   void visit_program(Program *x);
   void visit_using(Using *x);
   void visit_type(Type *x);
   void visit_funcdecl(FuncDecl *x);
   void visit_stmt(Stmt *x);
   void visit_block(Block *x);
   void visit_literal(Literal *x);
   void visit_identifier(Identifier *x);
   void visit_binaryexpr(BinaryExpr *x);
   void visit_declstmt(DeclStmt *x);
   void visit_exprstmt(ExprStmt *x);
   void visit_ifstmt(IfStmt *x);
   void visit_iterstmt(IterStmt *x);
   void visit_jumpstmt(JumpStmt *x);
};

#endif