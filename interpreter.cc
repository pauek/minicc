#include <sstream>
#include "ast.hh"
#include "interpreter.hh"
using namespace std;

void Interpreter::setenv(string id, const Value& val) {
   _env.back()[id] = val;
}

bool Interpreter::getenv(string id, Value& val) const {
   for (int i = _env.size()-1; i >= 0; i--) {
      auto it = _env[i].find(id);
      if (it != _env[i].end()) {
         val = it->second;
         return true;
      }
   }
   return false;
}

void Interpreter::visit_program(Program* x) {
   _env.clear();
   _env.resize(1);

   setenv("endl", Value("\n"));
   setenv("cout", Value::cout);

   for (AstNode *n : x->nodes) {
      n->visit(this);
   }
   auto it = _funcs.find("main");
   if (it == _funcs.end()) {
      _error("La funcion 'main' no existe");
   }
   vector<Value> main_args;
   invoke_func(it->second, main_args);
}

void Interpreter::invoke_func(FuncDecl *fn, vector<Value>& args) {
   pushenv();
   if (fn->params.size() != args.size()) {
      _error("Error en el número de argumentos al llamar a '" + fn->name + "'");
   }
   for (int i = 0; i < args.size(); i++) {
      setenv(fn->params[i]->name, args[i]);
   }
   fn->block->visit(this);
   popenv();
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
   if (!getenv(x->id, _curr)) {
      _error("No he encontrado la variable '" + x->id + "'");
   }
}

void Interpreter::visit_literal(Literal *x) {
   switch (x->type) {
   case Literal::String:
      _curr = Value(*x->val.as_string.s);
      break;

   case Literal::Int:
      _curr = Value(x->val.as_int);
      break;

   case Literal::Double:
      _curr = Value(x->val.as_double);
      break;

   case Literal::Bool:
      _curr = Value(x->val.as_bool);
      break;

   default:
      _error("Interpreter::visit_literal: UNIMPLEMENTED");
   }
}

void Interpreter::visit_binaryexpr(BinaryExpr *x) {
   x->left->visit(this);

   // cout << ...
   if (_curr == Value::cout && x->op == "<<") {
      Value old = _curr;
      x->right->visit(this);
      out() << _curr;
      _curr = old;
      return;
   }

   Value left = _curr;
   x->right->visit(this);
   Value right = _curr;

   if (x->op == "+") {
      if (left.kind == Value::Int and right.kind == Value::Int) {
         _curr = Value(left.val.as_int + right.val.as_int);
         return;
      }
      if (left.kind == Value::Double and right.kind == Value::Double) {
         _curr = Value(left.val.as_double + right.val.as_double);
         return;
      }
      if (left.kind == Value::String and right.kind == Value::String) {
         _curr = Value(*static_cast<string*>(left.val.as_ptr) +
                       *static_cast<string*>(right.val.as_ptr));
         return;
      }
      _error("Los operandos de '+' no son compatibles");
   } 
   else if (x->op == "*") {
      if (left.kind == Value::Double and right.kind == Value::Double) {
         _curr = Value(left.val.as_double * right.val.as_double);
         return;
      }
   }
   else if (x->op == "&&" or x->op == "and") {
      if (left.kind == Value::Bool and right.kind == Value::Bool) {
         _curr = Value(left.val.as_bool and right.val.as_bool);
         return;
      }
      _error("Los operandos de '" + x->op + "' no son de tipo 'bool'");
   }
   _error("Interpreter::visit_binaryexpr: UNIMPLEMENTED");
}

void Interpreter::visit_block(Block *x) {
   for (Stmt *stmt : x->stmts) {
      stmt->visit(this);
   }
}

void Interpreter::visit_vardecl(VarDecl *x) {
   vector<Value> init;
   if (!x->init.empty()) {
      for (Expr *e : x->init) {
         e->visit(this);
         init.push_back(_curr);
      }
   }
   // TODO: structs
   string left = init[0].type, right = x->type->str();
   if (left != right) {
      // Conversiones implícitas!
      if (!(left == "float"  and right == "double") and
          !(left == "double" and right == "float")) {
         _error("Asignas el tipo '" + init[0].type + "' " +
                "a una variable de tipo '" + x->type->str() + "'");
      }
   }
   setenv(x->name, init[0]);
}

void Interpreter::visit_arraydecl(ArrayDecl *x) {
   _error("Interpreter::visit_arraydecl: UNIMPLEMENTED");
}

void Interpreter::visit_objdecl(ObjDecl *x) {
   _error("Interpreter::visit_objdecl: UNIMPLEMENTED");
}

void Interpreter::visit_declstmt(DeclStmt* x) {
   for (Decl *d : x->decls) {
      d->visit(this);
   }
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
