#ifndef PRETTYPRINT_HH
#define PRETTYPRINT_HH

#include <assert.h>
#include <iostream>
#include "ast.hh"

class PrettyPrinter : public AstVisitor {
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
   void visit_expr(Expr *x);
};

#endif
