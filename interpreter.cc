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

Value *Interpreter::getenv(string id) {
   for (int i = _env.size()-1; i >= 0; i--) {
      auto it = _env[i].find(id);
      if (it != _env[i].end()) {
         return &it->second;
      }
   }
   return 0;
}

void Interpreter::visit_program(Program* x) {
   _env.clear();
   _env.resize(1);

   setenv("endl", Value("\n"));
   setenv("cout", Value::cout);
   setenv("cin",  Value::cin);

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
   Value *v = getenv(x->id);
   if (v == 0) {
      _error("No he encontrado la variable '" + x->id + "'");
   }
   _curr = Value(Value::Ref, v->type + "&");
   _curr.val.as_ptr = v;
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

   case Literal::Char:
      _curr = Value((*x->val.as_string.s)[0] /* FIXME */);
      break;

   default:
      _error("Interpreter::visit_literal: UNIMPLEMENTED");
   }
}

void Interpreter::visit_binaryexpr(BinaryExpr *x) {
   x->left->visit(this);
   Value left = _curr;
   if (left.kind == Value::Ref) {
      left = *static_cast<Value*>(left.val.as_ptr);
   }

   // cout << ...
   if (left == Value::cout && x->op == "<<") {
      Value old = _curr;
      x->right->visit(this);
      out() << _curr;
      _curr = old;
      return;
   }

   // cin >> ...
   if (left == Value::cin && x->op == ">>") {
      Value old = _curr;
      Ident *id = dynamic_cast<Ident*>(x->right);
      if (id == 0) {
         _error("La lectura con 'cin' requiere que pongas variables");
      }
      Value *right = getenv(id->id);
      if (right == 0) {
         _error("La variable '" + id->id + "' no está declarada");
      }
      in() >> *right;
      _curr = old;
      return;
   }

   x->right->visit(this);
   Value right = _curr;
   if (right.kind == Value::Ref) {
      right = *static_cast<Value*>(right.val.as_ptr);
   }

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
   else if (x->op == "==") {
      if (left.kind == right.kind) {
         _curr = Value(left == right);
         return;
      }
      _error("Los operandos de '==' no son del mismo tipo");
   }
   else if (x->op == "!=") {
      if (left.kind == right.kind) {
         _curr = Value(left != right);
         return;
      }
      _error("Los operandos de '!=' no son del mismo tipo");
   }
   else if (x->op == "<") {
      if (left.kind == Value::Int and right.kind == Value::Int) {
         _curr = Value(left.val.as_int < right.val.as_int);
         return;
      }
      if (left.kind == Value::Float and right.kind == Value::Float) {
         _curr = Value(left.val.as_float < right.val.as_float);
         return;
      }
      if (left.kind == Value::Double and right.kind == Value::Double) {
         _curr = Value(left.val.as_double < right.val.as_double);
         return;
      }
      if (left.kind == Value::String and right.kind == Value::String) {
         string *s1 = static_cast<string*>(left.val.as_ptr);
         string *s2 = static_cast<string*>(right.val.as_ptr);
         _curr = Value(*s1 < *s2);
         return;
      }
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
   } else {
      setenv(x->name, Value(Value::Unknown, x->type->str()));
   }
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
   x->cond->visit(this);
   if (_curr.kind != Value::Bool) {
      _error("La condición de un 'if' debe ser un valor de tipo 'bool'");
   }
   if (_curr.val.as_bool) {
      x->then->visit(this);
   } else {
      if (x->els != 0) {
         x->els->visit(this);
      }
   }
}

void Interpreter::visit_iterstmt(IterStmt *x) {
   if (x->init) {
      x->init->visit(this);
   }
   while (true) {
      x->cond->visit(this);
      if (_curr.kind != Value::Bool) {
         _error(string("La condición de un '") + (x->is_for() ? "for" : "while") + 
                "' debe ser un valor de tipo 'bool'");
      }
      if (!_curr.val.as_bool) {
         break;
      }
      x->substmt->visit(this);
      if (x->post) {
         x->post->visit(this);
      }
   }
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
   x->cond->visit(this);
   if (_curr.kind != Value::Bool) {
      _error("Una expresión condicional debe tener valor "
             "de tipo 'bool' antes del interrogante");
   }
   if (_curr.val.as_bool) {
      x->then->visit(this);
   } else {
      if (x->els != 0) {
         x->els->visit(this);
      }
   }
}

void Interpreter::visit_signexpr(SignExpr *x) {
   x->expr->visit(this);
   if (x->kind == SignExpr::Positive) {
      return;
   }
   if (_curr.kind == Value::Ref) {
      _curr = *static_cast<Value*>(_curr.val.as_ptr);
   }
   switch (_curr.kind) {
   case Value::Int:
      _curr.val.as_int = -_curr.val.as_int;
      break;

   case Value::Float:
      _curr.val.as_float = -_curr.val.as_float;
      break;

   case Value::Double:
      _curr.val.as_double = -_curr.val.as_double;
      break;

   default:
      _error("El cambio de signo para '" + _curr.type + "' no tiene sentido");
   }
}

void Interpreter::visit_increxpr(IncrExpr *x) {
   x->expr->visit(this);
   if (_curr.kind != Value::Ref) {
      _error("Hay que incrementar una variable, no un valor");
   }
   Value *target = static_cast<Value*>(_curr.val.as_ptr);
   Value before = *target;
   switch (target->kind) {
   case Value::Int:
      if (x->kind == IncrExpr::Positive) {
         target->val.as_int++;
      } else {
         target->val.as_int--;
      }
      break;

   default:
      _error("Estás incrementando un valor de tipo '" + target->type + "'");
   }
   Value after = *target;
   _curr = (x->preincr ? before : after);
}

void Interpreter::visit_negexpr(NegExpr *x) {
   x->expr->visit(this);
   if (_curr.kind != Value::Bool) {
      _error("Para negar una expresión ésta debe ser de tipo 'bool'");
   }
   _curr.val.as_bool = !_curr.val.as_bool;
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
