#include <sstream>
#include "ast.hh"
#include "walker.hh"
using namespace std;

void Walker::visit_include(Include* x)             { walk(x); }
void Walker::visit_macro(Macro* x)                 { walk(x); }
void Walker::visit_using(Using* x)                 { walk(x); }
void Walker::visit_simpleident(SimpleIdent *x)     { walk(x); }
void Walker::visit_literal(Literal *x)             { walk(x); }
void Walker::visit_jumpstmt(JumpStmt *x)           { walk(x); }
void Walker::visit_errorstmt(Stmt::Error *x)       { walk(x); }
void Walker::visit_errorexpr(Expr::Error *x)       { walk(x); }
void Walker::visit_vardecl(VarDecl *x)             { walk(x); }
void Walker::visit_arraydecl(ArrayDecl *x)         { walk(x); }
void Walker::visit_objdecl(ObjDecl *x)             { walk(x); }
void Walker::visit_enumdecl(EnumDecl *x)           { walk(x); }

void Walker::visit_templateident(TemplateIdent *x) { 
   walk(x); 
   for (TypeSpec *spec : x->subtypes) {
      spec->accept(this);
   }
}

void Walker::visit_fullident(FullIdent *x) { 
   walk(x);
   for (TemplateIdent *pre : x->prefix) {
      pre->accept(this);
   }
   visit_templateident(x);
}

void Walker::visit_program(Program* x) {
   walk(x);
   for (AstNode* n : x->nodes) {
      n->accept(this);
   }
}

void Walker::visit_typespec(TypeSpec *x) {
   walk(x);
   if (x->id) {
      x->id->accept(this);
   }
}

void Walker::visit_typedefdecl(TypedefDecl *x) {
   walk(x);
   if (x->decl) {
      x->decl->accept(this);
   }
}

void Walker::visit_structdecl(StructDecl *x) {
   walk(x);
   if (x->id) {
      x->id->accept(this);
   }
   for (auto decl : x->decls) {
      decl->accept(this);
   }
}

void Walker::visit_funcdecl(FuncDecl *x) {
   walk(x);
   if (x->return_typespec) {
      x->return_typespec->accept(this);
   }
   if (x->block) {
      x->block->accept(this);
   }
}

void Walker::visit_block(Block *x) {
   walk(x);
   for (Stmt *s : x->stmts) {
      s->accept(this);
   }
}

void Walker::visit_binaryexpr(BinaryExpr *x) {
   walk(x);
   if (x->left) {
      x->left->accept(this);
   }
   if (x->right) {
      x->right->accept(this);
   }
}

void Walker::visit_declstmt(DeclStmt* x) {
   walk(x);
   if (x->typespec) {
      x->typespec->accept(this);
   }
   for (DeclStmt::Item item : x->items) {
      item.decl->accept(this);
      if (item.init) {
         item.init->accept(this);
      }
   }
}

void Walker::visit_exprstmt(ExprStmt* x) {
   walk(x);
   if (x->expr) {
      x->expr->accept(this);
   } 
}

void Walker::visit_ifstmt(IfStmt *x) {
   walk(x);
   if (x->cond) {
      x->cond->accept(this);
   }
   if (x->then) {
      x->then->accept(this);
   }
   if (x->els) {
      x->els->accept(this);
   }
}

void Walker::visit_whilestmt(WhileStmt *x) {
   walk(x);
   if (x->cond) {
      x->cond->accept(this);
   }
   if (x->substmt) {
      x->substmt->accept(this);
   }
}

void Walker::visit_forstmt(ForStmt *x) {
   walk(x);
   if (x->init) {
      x->init->accept(this);
   }
   if (x->cond) {
      x->cond->accept(this);
   }
   if (x->post) {
      x->post->accept(this);
   }
   if (x->substmt) {
      x->substmt->accept(this);
   }
}

void Walker::visit_callexpr(CallExpr *x) {
   walk(x);
   if (x->func) {
      x->func->accept(this);
   }
   for (int i = 0; i < x->args.size(); i++) {
      x->args[i]->accept(this);
   }
}

void Walker::visit_indexexpr(IndexExpr *x) {
   walk(x);
   if (x->base) {
      x->base->accept(this);
   }
   if (x->index) {
      x->index->accept(this);
   }
}

void Walker::visit_fieldexpr(FieldExpr *x) {
   walk(x);
   if (x->base) {
      x->base->accept(this);
   }
   if (x->field) {
      x->field->accept(this);
   }
}

void Walker::visit_condexpr(CondExpr *x) {
   walk(x);
   if (x->cond) {
      x->cond->accept(this);
   }
   if (x->then) {
      x->then->accept(this);
   }
   if (x->els) {
      x->els->accept(this);
   }
}

void Walker::visit_exprlist(ExprList *x) {
   walk(x);
   for (Expr *e : x->exprs) {
      if (e) {
         e->accept(this);
      }
   }
}

void Walker::visit_signexpr(SignExpr *x) {
   walk(x);
   if (x->expr) {
      x->expr->accept(this);
   }
}

void Walker::visit_increxpr(IncrExpr *x) {
   walk(x);
   if (x->expr) {
      x->expr->accept(this);
   }
}

void Walker::visit_negexpr(NegExpr *x) {
   walk(x);
   if (x->expr) {
      x->expr->accept(this);
   }
}

void Walker::visit_addrexpr(AddrExpr *x) {
   walk(x);
   if (x->expr) {
      x->expr->accept(this);
   }
}

void Walker::visit_derefexpr(DerefExpr *x) {
   walk(x);
   if (x->expr) {
      x->expr->accept(this);
   }
}
