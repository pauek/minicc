#include <sstream>
#include "ast.hh"
#include "translator.hh"
#include "interpreter.hh"
using namespace std;

void Interpreter::setenv(string id, Value *val, bool hidden) {
   _env.back().set(id, val, hidden);
}

Value *Interpreter::getenv(string id) {
   for (int i = _env.size()-1; i >= 0; i--) {
      Value *v = _env[i].get(id);
      if (v) {
         return v;
      }
   }
   return 0;
}

void Interpreter::actenv() {
   for (int i = 0; i < _env.size(); i++) {
      _env[i].active = false;
   }
   _env.back().active = true;
}

void Interpreter::popenv() { 
   _env.pop_back(); 
   _env.back().active = true;
}

string Interpreter::env2json() const {
   ostringstream json;
   json << "[";
   for (int i = 1; i < _env.size(); i++) {
      if (i > 1) {
         json << ",";
      }
      json << "{\"name\":\"" << _env[i].name << "\",\"tab\":";
      _env[i].to_json(json);
      json << "}";
   }
   json << "]";
   return json.str();
}

void Interpreter::invoke_func_prepare(FuncDecl *fn, const vector<Value*>& args) {
   pushenv(fn->id->str());
   if (fn->params.size() != args.size()) {
      _error("Error en el número de argumentos al llamar a '" + fn->id->str() + "'");
   }
   for (int i = 0; i < args.size(); i++) {
      if (args[i] == 0) {
         string type = fn->params[i]->type->str();
         setenv(fn->params[i]->name, new Value(Value::Unknown, type));
      } else if (args[i]->kind == Value::Ref) {
         Value *v = args[i];
         if (!fn->params[i]->type->reference) {
            v = v->ref();
         }
         setenv(fn->params[i]->name, v);
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
   fn->block->accept(this);
   popenv();
}


void Interpreter::visit_program_prepare(Program *x) {
   _env.clear();
   _env.push_back(Environment("<global>"));

   bool hidden = true;
   setenv("endl", new Value("\n"), hidden);
   setenv("cout", &Value::cout,    hidden);
   setenv("cin",  &Value::cin,     hidden);

   for (AstNode *n : x->nodes) {
      n->accept(this);
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
      _error(_T("The '%s' function does not exist.", "main"));
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
   auto it = _funcs.insert(make_pair(x->id->str(), x));
   if (!it.second) {
      _error("La función de nombre '" + x->id->str() + "' ya existía");
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

struct _Add { template<typename T> static T eval(const T& a, const T& b) { return a + b; } };
struct _Sub { template<typename T> static T eval(const T& a, const T& b) { return a - b; } };
struct _Mul { template<typename T> static T eval(const T& a, const T& b) { return a * b; } };
struct _Div { template<typename T> static T eval(const T& a, const T& b) { return a / b; } };

struct _And { template<typename T> static T eval(const T& a, const T& b) { return a & b; } };
struct _Or  { template<typename T> static T eval(const T& a, const T& b) { return a | b; } };
struct _Xor { template<typename T> static T eval(const T& a, const T& b) { return a ^ b; } };

struct _AAdd { template<typename T> static void eval(T& a, const T& b) { a += b; } };
struct _ASub { template<typename T> static void eval(T& a, const T& b) { a -= b; } };
struct _AMul { template<typename T> static void eval(T& a, const T& b) { a *= b; } };
struct _ADiv { template<typename T> static void eval(T& a, const T& b) { a /= b; } };
struct _AAnd { template<typename T> static void eval(T& a, const T& b) { a &= b; } };
struct _AOr  { template<typename T> static void eval(T& a, const T& b) { a |= b; } };
struct _AXor { template<typename T> static void eval(T& a, const T& b) { a ^= b; } };

struct _Lt { template<typename T> static bool eval(const T& a, const T& b) { return a <  b; } };
struct _Le { template<typename T> static bool eval(const T& a, const T& b) { return a <= b; } };
struct _Gt { template<typename T> static bool eval(const T& a, const T& b) { return a >  b; } };
struct _Ge { template<typename T> static bool eval(const T& a, const T& b) { return a >= b; } };


template<class Op>
bool Interpreter::visit_op_assignment(Value *left, Value *right) {
   if (left->kind == Value::Int and right->kind == Value::Int) {
      Op::eval(left->val.as_int, right->val.as_int);
      return true;
   } 
   if (left->kind == Value::Float and right->kind == Value::Float) {
      Op::eval(left->val.as_float, right->val.as_float);
      return true;
   }
   if (left->kind == Value::Double and right->kind == Value::Double) {
      Op::eval(left->val.as_double, right->val.as_double);
      return true;
   }
   return false;
}

template<class Op>
bool Interpreter::visit_bitop_assignment(Value *left, Value *right) {
   if (left->kind == Value::Int and right->kind == Value::Int) {
      Op::eval(left->val.as_int, right->val.as_int);
      return true;
   } 
   return false;
}

template<class Op>
bool Interpreter::visit_sumprod(Value *left, Value *right) {
   if (left->kind == Value::Int and right->kind == Value::Int) {
      _curr = new Value(Op::eval(left->val.as_int, right->val.as_int));
      return true;
   }
   if (left->kind == Value::Float and right->kind == Value::Float) {
      _curr = new Value(Op::eval(left->val.as_float, right->val.as_float));
      return true;
   }
   if (left->kind == Value::Double and right->kind == Value::Double) {
      _curr = new Value(Op::eval(left->val.as_double, right->val.as_double));
      return true;
   }
   return false;
}

template<class Op>
bool Interpreter::visit_bitop(Value *left, Value *right) {
   if (left->kind == Value::Int and right->kind == Value::Int) {
      _curr = new Value(Op::eval(left->val.as_int, right->val.as_int));
      return true;
   }
   return false;
}

template<class Op>
bool Interpreter::visit_comparison(Value *left, Value *right) {
   if (left->kind == Value::Int and right->kind == Value::Int) {
      _curr = new Value(Op::eval(left->val.as_int, right->val.as_int));
      return true;
   }
   if (left->kind == Value::Float and right->kind == Value::Float) {
      _curr = new Value(Op::eval(left->val.as_float, right->val.as_float));
      return true;
   }
   if (left->kind == Value::Double and right->kind == Value::Double) {
      _curr = new Value(Op::eval(left->val.as_double, right->val.as_double));
      return true;
   }
   if (left->kind == Value::String and right->kind == Value::String) {
      string *s1 = static_cast<string*>(left->val.as_ptr);
      string *s2 = static_cast<string*>(right->val.as_ptr);
      _curr = new Value(Op::eval(*s1, *s2));
      return true;
   }
   return false;
}

void Interpreter::visit_binaryexpr(BinaryExpr *x) {
   x->left->accept(this);
   Value *left = _curr;
   if (x->kind != Expr::Assignment) {
      if (left->kind == Value::Ref) {
         left = left->ref();
      }
   }

   // cout << ...
   if (left == &Value::cout && x->op == "<<") {
      Value *old = _curr;
      x->right->accept(this);
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

   x->right->accept(this);
   Value *right = _curr;
   if (right->kind == Value::Ref) {
      right = right->ref();
   }
   
   if (x->op == "=") {
      visit_binaryexpr_assignment(left, right);
      return;
   }
   if (x->op == "+=" || x->op == "-=" || x->op == "*=" || x->op == "/=" ||
       x->op == "&=" || x->op == "|=" || x->op == "^=") {
      visit_binaryexpr_op_assignment(x->op[0], left, right);
      return;
   } 
   else if (x->op == "&" || x->op == "|" || x->op == "^") {
      bool ret = false;
      switch (x->op[0]) {
      case '&': ret = visit_bitop<_And>(left, right); break;
      case '|': ret = visit_bitop<_Or >(left, right); break;
      case '^': ret = visit_bitop<_Xor>(left, right); break;
      }
      if (ret) {
         return;
      }
      _error("Los operandos de '" + x->op + "' son incompatibles");
   }
   else if (x->op == "+" || x->op == "*" || x->op == "-" || x->op == "/") {
      bool ret = false;
      switch (x->op[0]) {
      case '+': {
         if (left->kind == Value::String and right->kind == Value::String) {
            _curr = new Value(*static_cast<string*>(left->val.as_ptr) +
                              *static_cast<string*>(right->val.as_ptr));
            ret = true;
         } else {
            ret = visit_sumprod<_Add>(left, right);
         }
         break;
      }
      case '*': ret = visit_sumprod<_Mul>(left, right); break;
      case '-': ret = visit_sumprod<_Sub>(left, right); break;
      case '/': ret = visit_sumprod<_Div>(left, right); break;
      }
      if (ret) {
         return;
      }
      _error("Los operandos de '*' no son compatibles");
   }
   else if (x->op == "%") {
      if (left->kind == Value::Int and right->kind == Value::Int) {
         _curr = new Value(left->val.as_int % right->val.as_int);
         return;
      }
      _error("Los operandos de '%' no son compatibles");
   }
   else if (x->op == "%=") {
      if (left->kind != Value::Ref) {
         _error("Para usar '" + x->op + "' se debe poner una variable a la izquierda");
      }
      left = left->ref();
      if (left->kind == Value::Int and right->kind == Value::Int) {
         left->val.as_int %= right->val.as_int;
         return;
      }
      _error("Los operandos de '%=' no son compatibles");
   }
   else if (x->op == "&&" or x->op == "and" || x->op == "||" || x->op == "or")  {
      if (left->kind == Value::Bool and right->kind == Value::Bool) {
         _curr = new Value(x->op == "&&" or x->op == "and" 
                           ? left->val.as_bool and right->val.as_bool
                           : left->val.as_bool or right->val.as_bool);
         return;
      }
      _error("Los operandos de '" + x->op + "' no son de tipo 'bool'");
   }
   else if (x->op == "==" || x->op == "!=") {
      if (left->kind == right->kind) {
         _curr = new Value(x->op == "==" ? *left == *right : *left != *right);
         return;
      }
      _error("Los operandos de '" + x->op + "' no son del mismo tipo");
   }
   else if (x->op == "<" || x->op == ">" || x->op == "<=" || x->op == ">=") {
      bool ret = false;
      if (x->op[0] == '<') {
         ret = (x->op.size() == 1 
                ? visit_comparison<_Lt>(left, right)
                : visit_comparison<_Le>(left, right));
      } else {
         ret = (x->op.size() == 1 
                ? visit_comparison<_Gt>(left, right)
                : visit_comparison<_Ge>(left, right));
      }
      if (ret) {
         return;
      }
      _error("Los operandos de '" + x->op + "' no son compatibles");
   }
   _error("Interpreter::visit_binaryexpr: UNIMPLEMENTED (" + x->op + ")");
}

inline bool assignment_types_ok(string a, string b) {
   return 
      (a == b) or
      (a == "float"  and b == "double") or
      (a == "double" and b == "float");
}

void Interpreter::visit_binaryexpr_assignment(Value *left, Value *right) {
   if (left->kind != Value::Ref) {
      _error("Intentas asignar sobre algo que no es una variable");
   }
   left = left->ref();
   if (assignment_types_ok(left->type, right->type)) {
      *left = *right; // DANGER!
   } else { 
      _error(string() + 
             "La asignación no se puede hacer porque los tipos no son compatibles (" +
             left->type + " vs " + right->type + ")");
   }
   _curr = left;
}

void Interpreter::visit_binaryexpr_op_assignment(char op, Value *left, Value *right) {
   if (left->kind != Value::Ref) {
      _error(string("Para usar '") + op + "=' se debe poner una variable a la izquierda");
   }
   left = left->ref();
   bool ok = false;
   switch (op) {
   case '+': {
      if (left->kind == Value::String and right->kind == Value::String) {
         string *s1 = static_cast<string*>(left->val.as_ptr);
         string *s2 = static_cast<string*>(right->val.as_ptr);
         *s1 += *s2;
         ok = true;
      } else {
         ok = visit_op_assignment<_AAdd>(left, right);
      }
      break;
   }
   case '-': ok = visit_op_assignment<_ASub>(left, right); break;
   case '*': ok = visit_op_assignment<_AMul>(left, right); break;
   case '/': ok = visit_op_assignment<_ADiv>(left, right); break;
   case '&': ok = visit_bitop_assignment<_AAnd>(left, right); break;
   case '|': ok = visit_bitop_assignment<_AOr >(left, right); break;
   case '^': ok = visit_bitop_assignment<_AXor>(left, right); break;
   }
   if (!ok) {
      _error(string("Los operandos de '") + op + "=' no son compatibles");
   }
}

void Interpreter::visit_block(Block *x) {
   for (Stmt *stmt : x->stmts) {
      stmt->accept(this);
   }
}

Value *Interpreter::visit_vardecl_struct_new(StructDecl *D, Value *init) {
   if (init and init->kind != Value::ExprList) {
      _error("Inicializas una tupla con algo que no es una lista de valores");
      return 0;
   }
   vector<Value*> *values = (init ? init->exprlist() : 0);
   int k = 0;
   pushenv("[struct]");
   for (int i = 0; i < D->decls.size(); i++) {
      for (int j = 0; j < D->decls[i]->items.size(); j++, k++) {
         if (values and (k < values->size())) {
            _curr = (*values)[k];
         } else {
            _curr = 0;
         }
         DeclStmt::Item item = D->decls[i]->items[j];
         item.decl->accept(this);
      }
   }
   Value *res = new Value(Value::Struct, D->type_str());
   res->val.as_ptr = new Environment(_env.back());
   popenv();
   return res;
}

void Interpreter::visit_vardecl_struct(VarDecl *x, StructDecl *D) {
   string struct_id = x->type->id->id;
   auto it = _structs.find(struct_id);
   if (it == _structs.end()) {
      _error("El tipo '" + struct_id + "' no es una tupla");
   }
   if (_curr) {
      if (_curr->kind != Value::ExprList) {
         _error("Para inicializar una tupla hace falta una lista de expresiones entre '{' y '}'");
         return;
      }
      vector<Value*> *values = _curr->exprlist();
      assert(values != 0);
      if (values->size() > D->num_fields()) {
         _error("Demasiados valores al inicializar la tupla de tipo '" + struct_id + "'");
         return;
      }
   }
   setenv(x->name, visit_vardecl_struct_new(D, _curr));
}

void Interpreter::visit_vardecl(VarDecl *x) {
   auto it = _structs.find(x->type->id->id);
   bool is_struct = (it != _structs.end());
   if (is_struct) {
      visit_vardecl_struct(x, it->second);
   } else {
      if (_curr) {
         if (_curr->kind == Value::Ref) {
            _curr = _curr->ref();
         }
         string left = x->type->str(), right = _curr->type;
         if (left != right) {
            // Conversiones implícitas!
            if (!(left == "float"  and right == "double") and
                !(left == "double" and right == "float")) {
               _error("Asignas el tipo '" + _curr->type + "' " +
                      "a una variable de tipo '" + x->type->str() + "'");
            }
         }
         _curr = new Value(*_curr);
      } else {
         _curr = new Value(Value::Unknown, x->type->str());
      }
      setenv(x->name, _curr);
   } 
}

void Interpreter::visit_arraydecl(ArrayDecl *x) {
   Value *init = _curr;

   x->size->accept(this);
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
   int num_inited = 0;
   if (init) {
      if (init->kind != Value::ExprList) {
         _error("Inicializas una tabla con algo que no es una lista de valores");
         return;
      }
      vector<Value*> *elist = init->exprlist();
      assert(elist != 0);
      if (elist->size() > sz) {
         _error("Demasiados valores al inicializar la tabla");
         return;
      }
      for (int i = 0; i < elist->size(); i++) {
         if ((*elist)[i]->type != cell_type) {
            ostringstream S;
            S << "La inicialización de la casilla " << i 
              << " tiene tipo '" << _curr->type << "'" 
              << " cuando debería ser '" << cell_type << "'";
            _error(S.str());
         }
         (*vals)[i] = (*elist)[i];
      }
      num_inited = elist->size();
   }
   for (int i = num_inited; i < vals->size(); i++) {
      auto it = _structs.find(cell_type);
      (*vals)[i] = (it != _structs.end()
                    ? visit_vardecl_struct_new(it->second, 0)
                    : new Value(Value::Unknown, cell_type));
   }
   v->val.as_ptr = vals;
   setenv(x->name, v);
}

void Interpreter::visit_declstmt(DeclStmt* x) {
   for (DeclStmt::Item& item : x->items) {
      if (item.init) {
         item.init->accept(this);
      } else {
         _curr = 0;
      }
      item.decl->accept(this);
   }
}

void Interpreter::visit_exprstmt(ExprStmt* x) {
   x->expr->accept(this);
   if (x->is_return) {
      _ret = _curr;
   }
}

void Interpreter::visit_ifstmt(IfStmt *x) {
   x->cond->accept(this);
   if (_curr->kind != Value::Bool) {
      _error("An if's condition needs to be a bool value");
   }
   if (_curr->val.as_bool) {
      x->then->accept(this);
   } else {
      if (x->els != 0) {
         x->els->accept(this);
      }
   }
}

void Interpreter::visit_iterstmt(IterStmt *x) {
   pushenv("");
   if (x->init) {
      x->init->accept(this);
   }
   while (true) {
      x->cond->accept(this);
      if (_curr->kind != Value::Bool) {
         _error(string("La condición de un '") + (x->is_for() ? "for" : "while") + 
                "' debe ser un valor de tipo 'bool'");
      }
      if (!_curr->val.as_bool) {
         break;
      }
      x->substmt->accept(this);
      if (x->post) {
         x->post->accept(this);
      }
   }
   popenv();
}

FuncDecl *Interpreter::visit_callexpr_getfunc(CallExpr *x) {
   Ident *fn = dynamic_cast<Ident*>(x->func);
   if (fn == 0) {
      _error(_T("Indirect call to functions is not implemented"));
   }
   auto it = _funcs.find(fn->id);
   if (it == _funcs.end()) {
      _error(_T("The '%s' function does not exist.", fn->id.c_str()));
      return 0;
   }
   return it->second;
}

void Interpreter::visit_callexpr(CallExpr *x) {
   FuncDecl *func = visit_callexpr_getfunc(x);
   vector<Value*> args;
   for (int i = 0; i < x->args.size(); i++) {
      x->args[i]->accept(this);
      args.push_back(_curr);
   }
   invoke_func(func, args);
   if (_ret == 0 && func->return_type->str() != "void") {
      _error("La función '" + func->id->str() 
             + "' debería devolver un '" + func->return_type->str() + "'");
   }
}

void Interpreter::visit_indexexpr(IndexExpr *x) {
   x->base->accept(this);
   if (_curr->kind == Value::Ref) {
      _curr = _curr->ref();
   }
   if (_curr->kind != Value::Array and _curr->kind != Value::Vector) {
      _error("Las expresiones de índice debe usarse sobre tablas o vectores");
   }
   vector<Value*> *vals = static_cast<vector<Value*>*>(_curr->val.as_ptr);
   x->index->accept(this);
   if (_curr->kind == Value::Ref) {
      _curr = _curr->ref();
   }
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
   x->base->accept(this);
   if (_curr->kind == Value::Ref) {
      _curr = _curr->ref();
   }
   if (_curr->kind != Value::Struct) {
      _error("El acceso a campos debe hacerse sobre tuplas u objetos");
   }
   Environment *fields = static_cast<Environment*>(_curr->val.as_ptr);
   Value *v = fields->get(x->field->id);
   if (v == 0) {
      _error("No existe el campo '" + x->field->id + "'");
   }
   _curr = new Value(Value::Ref, v->type + "&");
   _curr->val.as_ptr = v;
}

void Interpreter::visit_condexpr(CondExpr *x) {
   x->cond->accept(this);
   if (_curr->kind != Value::Bool) {
      _error("Una expresión condicional debe tener valor "
             "de tipo 'bool' antes del interrogante");
   }
   if (_curr->val.as_bool) {
      x->then->accept(this);
   } else {
      if (x->els != 0) {
         x->els->accept(this);
      }
   }
}

void Interpreter::visit_exprlist(ExprList *x) {
   vector<Value*> *v = new vector<Value*>();
   for (Expr *e : x->exprs) {
      e->accept(this);
      v->push_back(_curr);
   }
   _curr = new Value(Value::ExprList, "ExprList");
   _curr->val.as_ptr = v;
}

void Interpreter::visit_signexpr(SignExpr *x) {
   x->expr->accept(this);
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
   x->expr->accept(this);
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
   x->expr->accept(this);
   if (_curr->kind != Value::Bool) {
      _error("Para negar una expresión ésta debe ser de tipo 'bool'");
   }
   _curr->val.as_bool = !_curr->val.as_bool;
}

void Interpreter::visit_objdecl_vector(ObjDecl *x) {
   if (x->args.empty()) {
      _error(_T("The vector constructor needs at least 1 parameter."));
   } else if (x->args.size() > 2) {
      _error(_T("The vector constructor receives at most 2 parameters."));
   }
   x->args[0]->accept(this);
   if (_curr->kind != Value::Int) {
      _error(_T("The size of a vector must be an integer."));
   }
   if (_curr->val.as_int <= 0) {
      _error(_T("The size of a vector must be a positive integer."));
   }
   const int sz = _curr->val.as_int;
   Value *init = 0;
   if (x->args.size() == 2) { // initialization
      x->args[1]->accept(this);
      init = _curr;
   } else {
      // Valor por defecto para cada tipo
      Type *celltype = x->type->id->subtypes[0];
      if (celltype->str() == "int") {
         init = new Value(0);
      } else if (celltype->str() == "bool") {
         init = new Value(false);
      } else if (celltype->str() == "float") {
         init = new Value(0.0f);
      } else if (celltype->str() == "double") {
         init = new Value(0.0);
      } else if (celltype->str() == "char") {
         init = new Value('\0');
      }
   }
   Value *v = new Value(Value::Vector, x->type->str());
   vector<Value*> *vec = new vector<Value*>(sz);
   for (Value *&v : *vec) {
      v = new Value(*init);
   }
   v->val.as_ptr = vec;
   setenv(x->name, v);
}

void Interpreter::visit_objdecl(ObjDecl *x) {
   if (x->type->is_vector()) {
      visit_objdecl_vector(x);
      return;
   }
   _error(_T("The type '%s' is not implemented in MiniCC", 
             x->type->str().c_str()));
}
