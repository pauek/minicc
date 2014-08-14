#include <sstream>
#include "ast.hh"
#include "interpreter.hh"
using namespace std;

void Interpreter::visit_program(Program* x) {
   for (AstNode *n : x->nodes) {
      n->visit(this);
   }
   auto it = _funcs.find("main");
   if (it == _funcs.end()) {
      _error("La funcion 'main' no existe");
   }
   it->second->block->visit(this);
}

void Interpreter::visit_comment(CommentSeq* cn) {}
void Interpreter::visit_macro(Macro* x) {}

void Interpreter::visit_using(Using* x) {
   // ?
}

void Interpreter::visit_include(Include* x) {
   // TODO: Depending on include, register 'fake' functions & types.
}

void Interpreter::visit_funcdecl(FuncDecl *x) {
   auto it = _funcs.insert(make_pair(x->name, x));
   if (!it.second) {
      _error("La función de nombre '" + x->name + "' ya existía");
   }
}

void Interpreter::visit_type(Type *x) {
   _error("Interpreter::visit_type: UNIMPLEMENTED");
}

void Interpreter::visit_enumdecl(EnumDecl *x) {
   _error("Interpreter::visit_enumdecl: UNIMPLEMENTED");
}

void Interpreter::visit_typedefdecl(TypedefDecl *x) {
   _error("Interpreter::visit_typedefdecl: UNIMPLEMENTED");
}

void Interpreter::visit_structdecl(StructDecl *x) {
   _error("Interpreter::visit_structdecl: UNIMPLEMENTED");
}

void Interpreter::visit_ident(Ident *x) {
   if (x->id == "endl") {
      static string s = "\n";
      _curr.val.as_ptr = &s; // FIXME
      _curr.kind = Value::String;
      _curr.type = 0;
      return;
   }
   _curr.val.as_ptr = x;
   _curr.kind = Value::Ref;
   _curr.type = 0;
}

void Interpreter::visit_literal(Literal *x) {
   switch (x->type) {
   case Literal::String: {
      _curr.val.as_ptr = x->val.as_string.s;
      _curr.kind = Value::String;
      _curr.type = 0;
      break;
   }
   default:
      _error("Interpreter::visit_literal: UNIMPLEMENTED");
   }
}

void Interpreter::visit_binaryexpr(BinaryExpr *x) {
   x->left->visit(this);
   Ident *id = _curr.ref_to<Ident*>();
   if (id != 0 && id->id == "cout" && x->op == "<<") {
      Value old = _curr;
      x->right->visit(this);
      out() << _curr;
      _curr = old;
      return;
   }
   _error("Interpreter::visit_binaryexpr: UNIMPLEMENTED");
}

void Interpreter::visit_block(Block *x) {
   for (Stmt *stmt : x->stmts) {
      stmt->visit(this);
   }
}

void Interpreter::visit_vardecl(VarDecl *x) {
   _error("Interpreter::visit_vardecl: UNIMPLEMENTED");
}

void Interpreter::visit_arraydecl(ArrayDecl *x) {
   _error("Interpreter::visit_arraydecl: UNIMPLEMENTED");
}

void Interpreter::visit_objdecl(ObjDecl *x) {
   _error("Interpreter::visit_objdecl: UNIMPLEMENTED");
}

void Interpreter::visit_declstmt(DeclStmt* x) {
   _error("Interpreter::visit_declstmt: UNIMPLEMENTED");
}

void Interpreter::visit_exprstmt(ExprStmt* x) {
   x->expr->visit(this);
}

void Interpreter::visit_ifstmt(IfStmt *x) {
   _error("Interpreter::visit_ifstmt: UNIMPLEMENTED");
}

void Interpreter::visit_iterstmt(IterStmt *x) {
   _error("Interpreter::visit_iterstmt: UNIMPLEMENTED");
}

void Interpreter::visit_jumpstmt(JumpStmt *x) {
   _error("Interpreter::visit_jumpstmt: UNIMPLEMENTED");
}

void Interpreter::visit_callexpr(CallExpr *x) {
   _error("Interpreter::visit_callexpr: UNIMPLEMENTED");
}

void Interpreter::visit_indexexpr(IndexExpr *x) {
   _error("Interpreter::visit_indexexpr: UNIMPLEMENTED");
}

void Interpreter::visit_fieldexpr(FieldExpr *x) {
   _error("Interpreter::visit_fieldexpr: UNIMPLEMENTED");
}

void Interpreter::visit_condexpr(CondExpr *x) {
   _error("Interpreter::visit_condexpr: UNIMPLEMENTED");
}

void Interpreter::visit_signexpr(SignExpr *x) {
   _error("Interpreter::visit_signexpr: UNIMPLEMENTED");
}

void Interpreter::visit_increxpr(IncrExpr *x) {
   _error("Interpreter::visit_increxpr: UNIMPLEMENTED");
}

void Interpreter::visit_negexpr(NegExpr *x) {
   _error("Interpreter::visit_negexpr: UNIMPLEMENTED");
}

void Interpreter::visit_addrexpr(AddrExpr *x) {
   _error("Interpreter::visit_addrexpr: UNIMPLEMENTED");
}

void Interpreter::visit_derefexpr(DerefExpr *x) {
   _error("Interpreter::visit_derefexpr: UNIMPLEMENTED");
}

void Interpreter::visit_errorstmt(Stmt::Error *x) {
   _error("Interpreter::visit_errorstmt: UNIMPLEMENTED");
}

void Interpreter::visit_errorexpr(Expr::Error *x) {
   _error("Interpreter::visit_errorexpr: UNIMPLEMENTED");
}
