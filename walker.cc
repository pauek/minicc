#include <sstream>
#include "ast.hh"
#include "walker.hh"
using namespace std;

void Walker::visit_comment(CommentSeq* cn)   { walk(cn); }
void Walker::visit_include(Include* x)       { walk(x); }
void Walker::visit_macro(Macro* x)           { walk(x); }
void Walker::visit_using(Using* x)           { walk(x); }
void Walker::visit_ident(Ident *x)           { walk(x); }
void Walker::visit_literal(Literal *x)       { walk(x); }
void Walker::visit_jumpstmt(JumpStmt *x)     { walk(x); }
void Walker::visit_errorstmt(Stmt::Error *x) { walk(x); }
void Walker::visit_errorexpr(Expr::Error *x) { walk(x); }

void Walker::visit_program(Program* x) {
   walk(x);
   for (AstNode* n : x->nodes) {
      n->visit(this);
   }
}

void Walker::visit_type(Type *x) {
   walk(x);
   for (auto id : x->nested_ids) {
      id->visit(this);
   }
}

void Walker::visit_structdecl(StructDecl *x) {
   walk(x);
   x->id->visit(this);
   for (auto decl : x->decls) {
      decl->visit(this);
   }
}

void Walker::visit_funcdecl(FuncDecl *x) {
   walk(x);
   x->return_type->visit(this);
   if (x->block) {
      x->block->visit(this);
   }
}

void Walker::visit_block(Block *x) {
   walk(x);
   for (Stmt *s : x->stmts) {
      s->visit(this);
   }
}

void Walker::visit_binaryexpr(BinaryExpr *x) {
   walk(x);
   if (x->left) {
      x->left->visit(this);
   }
   if (x->right) {
      x->right->visit(this);
   }
}

void Walker::visit_declstmt(DeclStmt* x) {
   walk(x);
   x->type->visit(this);
   for (DeclStmt::Decl& decl : x->decls) {
      if (decl.init != 0) {
         decl.init->visit(this);
      }
   }
}

void Walker::visit_exprstmt(ExprStmt* x) {
   walk(x);
   if (x->expr) {
      x->expr->visit(this);
   } 
}

void Walker::visit_ifstmt(IfStmt *x) {
   walk(x);
   x->cond->visit(this);
   x->then->visit(this);
   if (x->els) {
      x->els->visit(this);
   }
}

void Walker::visit_iterstmt(IterStmt *x) {
   walk(x);
   if (x->is_for()) {
      x->init->visit(this);
      x->cond->visit(this);
      x->post->visit(this);
   } else {
      x->cond->visit(this);
   }
   x->substmt->visit(this);
}

void Walker::visit_callexpr(CallExpr *x) {
   walk(x);
   x->func->visit(this);
   for (int i = 0; i < x->args.size(); i++) {
      x->args[i]->visit(this);
   }
}

void Walker::visit_indexexpr(IndexExpr *x) {
   walk(x);
   x->base->visit(this);
   x->index->visit(this);
}

void Walker::visit_fieldexpr(FieldExpr *x) {
   walk(x);
   x->base->visit(this);
   x->field->visit(this);
}

void Walker::visit_condexpr(CondExpr *x) {
   walk(x);
   x->cond->visit(this);
   x->then->visit(this);
   x->els->visit(this);
}

void Walker::visit_signexpr(SignExpr *x) {
   walk(x);
   x->expr->visit(this);
}

void Walker::visit_increxpr(IncrExpr *x) {
   walk(x);
   x->expr->visit(this);
}

void Walker::visit_negexpr(NegExpr *x) {
   walk(x);
   x->expr->visit(this);
}

void Walker::visit_addrexpr(AddrExpr *x) {
   walk(x);
   x->expr->visit(this);
}

void Walker::visit_derefexpr(DerefExpr *x) {
   walk(x);
   x->expr->visit(this);
}
