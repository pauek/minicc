#include <sstream>
#include "ast.hh"
#include "interpreter.hh"
using namespace std;

void Interpreter::setenv(string id, Value *val) {
   _env.back()[id] = val;
}

Value *Interpreter::getenv(string id) {
   for (int i = _env.size()-1; i >= 0; i--) {
      auto it = _env[i].find(id);
      if (it != _env[i].end()) {
         return it->second;
      }
   }
   return 0;
}

void Interpreter::invoke_func_prepare(FuncDecl *fn, const vector<Value*>& args) {
   pushenv();
   if (fn->params.size() != args.size()) {
      _error("Error en el número de argumentos al llamar a '" + fn->name + "'");
   }
   for (int i = 0; i < args.size(); i++) {
      if (args[i]->kind == Value::Ref) {
         if (fn->params[i]->type->reference) {
            setenv(fn->params[i]->name, args[i]);
         } else {
            Value *v = static_cast<Value*>(args[i]->val.as_ptr);
            setenv(fn->params[i]->name, v);
         }
      } else {
         if (fn->params[i]->type->reference) {
            ostringstream S;
            S << "En el parámetro " << i+1 << " se requiere una variable.";
            _error(S.str());
         }
         setenv(fn->params[i]->name, args[i]);
      }
   }
}

void Interpreter::invoke_func(FuncDecl *fn, const vector<Value*>& args) {
   invoke_func_prepare(fn, args);
   fn->block->visit(this);
   popenv();
}


void Interpreter::visit_program_prepare(Program *x) {
   _env.clear();
   _env.resize(1);

   setenv("endl", new Value("\n"));
   setenv("cout", &Value::cout);
   setenv("cin",  &Value::cin);

   for (AstNode *n : x->nodes) {
      n->visit(this);
   }
}

FuncDecl *Interpreter::visit_program_find_main() {
   auto it = _funcs.find("main");
   return (it == _funcs.end() ? 0 : it->second);
}

void Interpreter::visit_program(Program* x) {
   visit_program_prepare(x);
   FuncDecl *main = visit_program_find_main();
   if (main == 0) {
      _error("La funcion 'main' no existe");
   } else {
      invoke_func(main, vector<Value*>());
   }
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

void Interpreter::visit_structdecl(StructDecl *x) {
   _structs[x->id->id] = x;
}

void Interpreter::visit_ident(Ident *x) {
   Value *v = getenv(x->id);
   if (v == 0) {
      _error("La variable '" + x->id + "' no existe.");
   }
   if (v->kind == Value::Ref) {
      _curr = v;
   } else {
      _curr = new Value(Value::Ref, v->type + "&");
      _curr->val.as_ptr = v;
   }
}

void Interpreter::visit_literal(Literal *x) {
   switch (x->type) {
   case Literal::String:
      _curr = new Value(*x->val.as_string.s);
      break;

   case Literal::Int:
      _curr = new Value(x->val.as_int);
      break;

   case Literal::Double:
      _curr = new Value(x->val.as_double);
      break;

   case Literal::Bool:
      _curr = new Value(x->val.as_bool);
      break;

   case Literal::Char:
      _curr = new Value((*x->val.as_string.s)[0] /* FIXME */);
      break;

   default:
      _error("Interpreter::visit_literal: UNIMPLEMENTED");
   }
}

void Interpreter::visit_binaryexpr(BinaryExpr *x) {
   x->left->visit(this);
   Value *left = _curr;
   if (x->kind != Expr::Assignment) {
      if (left->kind == Value::Ref) {
         left = left->ref();
      }
   }

   // cout << ...
   if (left == &Value::cout && x->op == "<<") {
      Value *old = _curr;
      x->right->visit(this);
      out() << *_curr;
      _curr = old;
      return;
   }

   // cin >> ...
   if (left == &Value::cin && x->op == ">>") {
      Value *old = _curr;
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
   Value *right = _curr;
   if (right->kind == Value::Ref) {
      right = right->ref();
   }
   
   if (x->op == "=") {
      visit_binaryexpr_assignment(left, right);
      return;
   }
   else if (x->op == "+=") {
      if (left->kind != Value::Ref) {
         _error("Para usar '+=' se debe poner una variable a la izquierda");
      }
      Value *v = left->ref();
      if (v->kind == Value::Int and right->kind == Value::Int) {
         v->val.as_int += right->val.as_int;
         return;
      } 
      if (v->kind == Value::Float and right->kind == Value::Float) {
         v->val.as_float += right->val.as_float;
         return;
      }
      if (v->kind == Value::Double and right->kind == Value::Double) {
         v->val.as_double += right->val.as_double;
         return;
      }
      if (v->kind == Value::String and right->kind == Value::String) {
         string *s1 = static_cast<string*>(v->val.as_ptr);
         string *s2 = static_cast<string*>(right->val.as_ptr);
         *s1 += *s2;
         return;
      }
      _error("Los operandos de '+=' no son compatibles");
   } 
   if (x->op == "-=") {
      if (left->kind != Value::Ref) {
         _error("Para usar '-=' se debe poner una variable a la izquierda");
      }
      Value *v = left->ref();
      if (v->kind == Value::Int and right->kind == Value::Int) {
         v->val.as_int -= right->val.as_int;
         return;
      } 
      if (v->kind == Value::Float and right->kind == Value::Float) {
         v->val.as_float -= right->val.as_float;
         return;
      }
      if (v->kind == Value::Double and right->kind == Value::Double) {
         v->val.as_double -= right->val.as_double;
         return;
      }
      _error("Los operandos de '-=' no son compatibles");
   } 
   else if (x->op == "+") {
      if (left->kind == Value::Int and right->kind == Value::Int) {
         _curr = new Value(left->val.as_int + right->val.as_int);
         return;
      }
      if (left->kind == Value::Double and right->kind == Value::Double) {
         _curr = new Value(left->val.as_double + right->val.as_double);
         return;
      }
      if (left->kind == Value::String and right->kind == Value::String) {
         _curr = new Value(*static_cast<string*>(left->val.as_ptr) +
                           *static_cast<string*>(right->val.as_ptr));
         return;
      }
      _error("Los operandos de '+' no son compatibles");
   } 
   else if (x->op == "*") {
      if (left->kind == Value::Int and right->kind == Value::Int) {
         _curr = new Value(left->val.as_int * right->val.as_int);
         return;
      }
      if (left->kind == Value::Float and right->kind == Value::Float) {
         _curr = new Value(left->val.as_float * right->val.as_float);
         return;
      }
      if (left->kind == Value::Double and right->kind == Value::Double) {
         _curr = new Value(left->val.as_double * right->val.as_double);
         return;
      }
   }
   else if (x->op == "&&" or x->op == "and") {
      if (left->kind == Value::Bool and right->kind == Value::Bool) {
         _curr = new Value(left->val.as_bool and right->val.as_bool);
         return;
      }
      _error("Los operandos de '" + x->op + "' no son de tipo 'bool'");
   }
   else if (x->op == "==") {
      if (left->kind == right->kind) {
         _curr = new Value(*left == *right);
         return;
      }
      _error("Los operandos de '==' no son del mismo tipo");
   }
   else if (x->op == "!=") {
      if (left->kind == right->kind) {
         _curr = new Value(*left != *right);
         return;
      }
      _error("Los operandos de '!=' no son del mismo tipo");
   }
   else if (x->op == "<") {
      if (left->kind == Value::Int and right->kind == Value::Int) {
         _curr = new Value(left->val.as_int < right->val.as_int);
         return;
      }
      if (left->kind == Value::Float and right->kind == Value::Float) {
         _curr = new Value(left->val.as_float < right->val.as_float);
         return;
      }
      if (left->kind == Value::Double and right->kind == Value::Double) {
         _curr = new Value(left->val.as_double < right->val.as_double);
         return;
      }
      if (left->kind == Value::String and right->kind == Value::String) {
         string *s1 = static_cast<string*>(left->val.as_ptr);
         string *s2 = static_cast<string*>(right->val.as_ptr);
         _curr = new Value(*s1 < *s2);
         return;
      }
   }
   _error("Interpreter::visit_binaryexpr: UNIMPLEMENTED (" + x->op + ")");
}

void Interpreter::visit_binaryexpr_assignment(Value *left, Value *right) {
   if (left->kind != Value::Ref) {
      _error("Intentas asignar sobre algo que no es una variable");
   }
   Value *v = left->ref();
   if (v->type != right->type) {
      _error("Las asignación no se puede hacer porque los tipos no coinciden");
   }
   *v = *right; // DANGER!
   _curr = v;
}


void Interpreter::visit_block(Block *x) {
   for (Stmt *stmt : x->stmts) {
      stmt->visit(this);
   }
}

void Interpreter::visit_vardecl(VarDecl *x) {
   if (!x->init.empty() and !x->curly) {
      x->init[0]->visit(this);
      string left = x->type->str(), right = _curr->type;
      if (left != right) {
         // Conversiones implícitas!
         if (!(left == "float"  and right == "double") and
             !(left == "double" and right == "float")) {
            _error("Asignas el tipo '" + _curr->type + "' " +
                   "a una variable de tipo '" + x->type->str() + "'");
         }
      }
      setenv(x->name, _curr);
   } 
   else if (x->curly) {
      // struct
      auto it = _structs.find(x->type->id->id);
      if (it == _structs.end()) {
         _error("El tipo '" + x->type->id->id + "' no es una tupla");
      }
      StructDecl *D = it->second;
      if (D->num_fields() < x->init.size()) {
         _error("Demasiados valores al inicializar la tupla de tipo '" + x->type->id->id + "'");
      }
      Value *v = new Value(Value::Struct, D->type_str());
      map<string,Value*> *fields = new map<string,Value*>();
      int k = 0;
      for (int i = 0; i < D->decls.size(); i++) {
         string type = D->decls[i]->type->str();
         Value::Kind kind = Value::type2kind(type);
         for (int j = 0; j < D->decls[i]->decls.size(); j++) {
            if (k < x->init.size()) {
               x->init[k]->visit(this);
               (*fields)[D->decls[i]->decls[j]->name] = _curr;
               k++;
            } else {
               (*fields)[D->decls[i]->decls[j]->name] = new Value(kind, type);
            }
         }
      }
      v->val.as_ptr = fields;
      setenv(x->name, v);
   } 
   else {
      setenv(x->name, new Value(Value::Unknown, x->type->str()));
   }
}

void Interpreter::visit_arraydecl(ArrayDecl *x) {
   x->size->visit(this);
   if (_curr->kind != Value::Int) {
      _error("El tamaño de una tabla debe ser un entero");
   }
   if (_curr->val.as_int <= 0) {
      _error("El tamaño de una tabla debe ser un entero positivo");
   }
   const int sz = _curr->val.as_int;
   Value *v = new Value(Value::Array, x->type_str());
   vector<Value*> *vals = new vector<Value*>(sz);
   string cell_type = x->type->str();
   for (int i = 0; i < x->init.size(); i++) {
      x->init[i]->visit(this);
      if (_curr->type != cell_type) {
         ostringstream S;
         S << "La inicialización de la casilla " << i 
           << " tiene tipo '" << _curr->type << "'" 
           << " cuando debería ser '" << cell_type << "'";
         _error(S.str());
      }
      (*vals)[i] = _curr;
   }
   v->val.as_ptr = vals;
   setenv(x->name, v);
}

void Interpreter::visit_declstmt(DeclStmt* x) {
   for (Decl *d : x->decls) {
      d->visit(this);
   }
}

void Interpreter::visit_exprstmt(ExprStmt* x) {
   x->expr->visit(this);
   if (x->is_return) {
      _ret = _curr;
   }
}

void Interpreter::visit_ifstmt(IfStmt *x) {
   x->cond->visit(this);
   if (_curr->kind != Value::Bool) {
      _error("La condición de un 'if' debe ser un valor de tipo 'bool'");
   }
   if (_curr->val.as_bool) {
      x->then->visit(this);
   } else {
      if (x->els != 0) {
         x->els->visit(this);
      }
   }
}

void Interpreter::visit_iterstmt(IterStmt *x) {
   pushenv();
   if (x->init) {
      x->init->visit(this);
   }
   while (true) {
      x->cond->visit(this);
      if (_curr->kind != Value::Bool) {
         _error(string("La condición de un '") + (x->is_for() ? "for" : "while") + 
                "' debe ser un valor de tipo 'bool'");
      }
      if (!_curr->val.as_bool) {
         break;
      }
      x->substmt->visit(this);
      if (x->post) {
         x->post->visit(this);
      }
   }
   popenv();
}

FuncDecl *Interpreter::visit_callexpr_getfunc(CallExpr *x) {
   Ident *fn = dynamic_cast<Ident*>(x->func);
   if (fn == 0) {
      _error("La llamada no-directa a funciones no se ha implementado");
   }
   auto it = _funcs.find(fn->id);
   if (it == _funcs.end()) {
      _error("La función '" + fn->id + "' no existe");
      return 0;
   }
   return it->second;
}

void Interpreter::visit_callexpr(CallExpr *x) {
   FuncDecl *func = visit_callexpr_getfunc(x);
   vector<Value*> args;
   for (int i = 0; i < x->args.size(); i++) {
      x->args[i]->visit(this);
      args.push_back(_curr);
   }
   invoke_func(func, args);
   if (_ret == 0 && func->return_type->str() != "void") {
      _error("La función '" + func->name + "' debería devolver un '" + func->return_type->str() + "'");
   }
}

void Interpreter::visit_indexexpr(IndexExpr *x) {
   x->base->visit(this);
   if (_curr->kind == Value::Ref) {
      _curr = _curr->ref();
   }
   if (_curr->kind != Value::Array) {
      _error("Las expresiones de índice debe usarse sobre tablas o vectores");
   }
   vector<Value*> *vals = static_cast<vector<Value*>*>(_curr->val.as_ptr);
   x->index->visit(this);
   if (_curr->kind != Value::Int) {
      // FIXME: maps!
      _error("El índice en un acceso a tabla debe ser un entero");
   }
   int i = _curr->val.as_int;
   if (i < 0 || i >= vals->size()) {
      ostringstream S;
      S << "La casilla " << i << " no existe";
      _error(S.str());
   }
   Value *v = (*vals)[i];
   _curr = new Value(Value::Ref, v->type + "&");
   _curr->val.as_ptr = v;
}

void Interpreter::visit_fieldexpr(FieldExpr *x) {
   x->base->visit(this);
   if (_curr->kind == Value::Ref) {
      _curr = _curr->ref();
   }
   if (_curr->kind != Value::Struct) {
      _error("El acceso a campos debe hacerse sobre tuplas u objetos");
   }
   map<string,Value*> *fields = static_cast<map<string,Value*>*>(_curr->val.as_ptr);
   auto it = fields->find(x->field->id);
   if (it == fields->end()) {
      _error("No existe el campo '" + x->field->id + "'");
   }
   _curr = new Value(Value::Ref, it->second->type + "&");
   _curr->val.as_ptr = it->second;
}

void Interpreter::visit_condexpr(CondExpr *x) {
   x->cond->visit(this);
   if (_curr->kind != Value::Bool) {
      _error("Una expresión condicional debe tener valor "
             "de tipo 'bool' antes del interrogante");
   }
   if (_curr->val.as_bool) {
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
   if (_curr->kind == Value::Ref) {
      _curr = static_cast<Value*>(_curr->val.as_ptr);
   }
   switch (_curr->kind) {
   case Value::Int:
      _curr->val.as_int = -_curr->val.as_int;
      break;

   case Value::Float:
      _curr->val.as_float = -_curr->val.as_float;
      break;

   case Value::Double:
      _curr->val.as_double = -_curr->val.as_double;
      break;

   default:
      _error("El cambio de signo para '" + _curr->type + "' no tiene sentido");
   }
}

void Interpreter::visit_increxpr(IncrExpr *x) {
   x->expr->visit(this);
   if (_curr->kind != Value::Ref) {
      _error("Hay que incrementar una variable, no un valor");
   }
   Value *target = static_cast<Value*>(_curr->val.as_ptr);
   Value *before = new Value(*target);
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
   Value *after = target;
   _curr = (x->preincr ? before : after);
}

void Interpreter::visit_negexpr(NegExpr *x) {
   x->expr->visit(this);
   if (_curr->kind != Value::Bool) {
      _error("Para negar una expresión ésta debe ser de tipo 'bool'");
   }
   _curr->val.as_bool = !_curr->val.as_bool;
}

