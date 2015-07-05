#include "ast.hh"
#include "translator.hh"
#include "interpreter.hh"
using namespace std;

void Interpreter::invoke_func_prepare_arg(FuncDecl *fn, Value arg, int i) {
   if (arg.is<Reference>()) {
      if (!fn->params[i]->typespec->reference) {
         arg = Reference::deref(arg);
      }
      setenv(fn->params[i]->name, arg);
   } else {
      if (fn->params[i]->typespec->reference) {
         _error(_T("En el parámetro %d se requiere una variable.", i+1));
      }
      setenv(fn->params[i]->name, arg);
   }
}

void Interpreter::invoke_func_prepare(FuncDecl *fn, const vector<Value>& args) {
   if (fn->params.size() != args.size()) {
      _error(_T("Error en el número de argumentos al llamar a '%s'", 
                fn->funcname().c_str()));
   }
   for (int i = 0; i < args.size(); i++) {
      invoke_func_prepare_arg(fn, args[i], i);
   }
}

void Interpreter::visit_program_prepare(Program *x) {
   prepare_global_environment();

   for (AstNode *n : x->nodes) {
      n->accept(this);
   }
}

void Interpreter::visit_program_find_main() {
   if (!getenv("main", _curr)) {
      _error(_T("The '%s' function does not exist.", "main"));
   }
   if (!_curr.is<Callable>()) {
      _error(_T("'main' is not a function."));
   }
}

void Interpreter::visit_program(Program* x) {
   visit_program_prepare(x);
   visit_program_find_main();
   _curr.as<Callable>().call(this, vector<Value>());
}

void Interpreter::visit_comment(CommentSeq* cn) {}
void Interpreter::visit_macro(Macro* x) {}

void Interpreter::visit_using(Using* x) {
   if (!using_namespace(x->namespc)) {
      _error(_T("No se ha encontrado el \"namespace\" '%s'.", x->namespc.c_str()));
   }
}


void Interpreter::visit_include(Include* x) {
   include_header_file(x->filename);
}

void Interpreter::visit_funcdecl(FuncDecl *x) {
   string funcname = x->funcname();
   Type *return_type = get_type(x->return_typespec);  // return_type == 0 means 'void'
   Function *functype = new Function(return_type);
   for (auto p : x->params) {
      Type *param_type = get_type(p->typespec);
      assert(param_type != 0);
      functype->add_params(param_type);
   }
   Value func = functype->mkvalue(new UserFunc(funcname, x));
   Value callable = Callable::self->mkvalue(Value::null, func); // bind with 'null'
   setenv(funcname, callable, Hidden);
}

void Interpreter::visit_structdecl(StructDecl *x) {
   // Create a new Struct type now
   Struct *type = new Struct(x->struct_name());
   for (int i = 0; i < x->decls.size(); i++) {
      DeclStmt& decl = *x->decls[i];
      Type *field_type = get_type(decl.typespec);
      assert(type != 0);
      for (DeclStmt::Item& item : decl.items) {
         if (item.decl->is<ArrayDecl>()) {
            Expr *size_expr = dynamic_cast<ArrayDecl*>(item.decl)->sizes[0];
            Literal *size_lit = dynamic_cast<Literal*>(size_expr);
            assert(size_lit != 0);
            assert(size_lit->type == Literal::Int);
            const int sz = size_lit->val.as_int;
            // TODO: don't create new Array type every time?
            type->add_field(item.decl->name, new Array(field_type, sz)); 
         } else {
            type->add_field(item.decl->name, field_type);
         }
      }
   }
   register_type(x->struct_name(), type);
}

void Interpreter::visit_fullident(FullIdent *x) {
   Value v;
   
   // Try a namespace
   SimpleIdent *namespc_or_class = x->get_potential_namespace_or_class();
   if (namespc_or_class != 0) {
      Environment *namespc = get_namespace(namespc_or_class->name);
      if (namespc != 0) {
         if (namespc->get(x->name, v)) {
            goto found;
         }
         _error(_T("No se ha encontrado '%s' en el namespace '%s'.", 
                   x->name.c_str(), namespc_or_class->name.c_str()));
         return;
      }
   }

   // Try a static variable in a class
   if (namespc_or_class != 0) {
      FullIdent fid(namespc_or_class->name);
      TypeSpec spec(&fid);
      Type *type = get_type(&spec);
      if (type != 0 and !type->get_static(x->name, v)) {
         _error(_T("No se ha encontrado '%s' en la clase '%s'.",
                   x->name.c_str(), namespc_or_class->name.c_str()));
      }
      goto found;
   }

   // Try the environment
   if (getenv(x->name, v)) {
      goto found;
   } 
     
   _error(_T("No se ha encontrado '%s'.", 
             x->name.c_str()));
 found:
   _curr = (v.is<Reference>() ? v : Reference::mkref(v));
}

void Interpreter::visit_literal(Literal *x) {
   switch (x->type) {
   case Literal::String: _curr = Value(*x->val.as_string.s); break;
   case Literal::Int:    _curr = Value(x->val.as_int);       break;
   case Literal::Double: _curr = Value(x->val.as_double);    break;
   case Literal::Bool:   _curr = Value(x->val.as_bool);      break;
   case Literal::Char:   _curr = Value(x->val.as_char);      break;
   default:
      _error(_T("Interpreter::visit_literal: UNIMPLEMENTED"));
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
bool Interpreter::visit_op_assignment(Value left, Value _right) {
   Value right = left.type()->convert(_right);
   if (left.is<Int>() and right.is<Int>()) {
      Op::eval(left.as<Int>(), right.as<Int>());
      left.touch();
      return true;
   } 
   if (left.is<Float>() and right.is<Float>()) {
      Op::eval(left.as<Float>(), right.as<Float>());
      left.touch();
      return true;
   }
   if (left.is<Double>() and right.is<Double>()) {
      Op::eval(left.as<Double>(), right.as<Double>());
      left.touch();
      return true;
   }
   return false;
}

template<class Op>
bool Interpreter::visit_bitop_assignment(Value left, Value _right) {
   Value right = left.type()->convert(_right);
   if (left.is<Int>() and right.is<Int>()) {
      Op::eval(left.as<Int>(), right.as<Int>());
      left.touch();
      return true;
   } 
   return false;
}

template<class Op>
bool Interpreter::visit_sumprod(Value left, Value _right) {
   Value right = left.type()->convert(_right);
   if (left.is<Int>()) {
      _curr = Value(Op::eval(left.as<Int>(), right.as<Int>()));
      return true;
   }
   if (left.is<Float>()) {
      _curr = Value(Op::eval(left.as<Float>(), right.as<Float>()));
      return true;
   }
   if (left.is<Double>()) {
      _curr = Value(Op::eval(left.as<Double>(), right.as<Double>()));
      return true;
   }
   return false;
}

template<class Op>
bool Interpreter::visit_bitop(Value left, Value right) {
   if (left.is<Int>() and right.is<Int>()) {
      _curr = Value(Op::eval(left.as<Int>(), right.as<Int>()));
      return true;
   }
   return false;
}

template<class Op>
bool Interpreter::visit_comparison(Value left, Value right) {
   if (left.is<Int>() and right.is<Int>()) {
      _curr = Value(Op::eval(left.as<Int>(), right.as<Int>()));
      return true;
   }
   if (left.is<Float>() and right.is<Float>()) {
      _curr = Value(Op::eval(left.as<Float>(), right.as<Float>()));
      return true;
   }
   if (left.is<Double>() and right.is<Double>()) {
      _curr = Value(Op::eval(left.as<Double>(), right.as<Double>()));
      return true;
   }
   if (left.is<String>() and right.is<String>()) {
      _curr = Value(Op::eval(left.as<String>(), right.as<String>()));
      return true;
   }
   return false;
}

void Interpreter::visit_binaryexpr(BinaryExpr *x) {
   x->left->accept(this);
   Value left = _curr;
   if (x->kind != Expr::Assignment) {
      left = Reference::deref(left);
   }

   // Handle booleans first, since they might not need to evaluate the
   // second part
   // 
   if (x->op == "&&" or x->op == "and" or x->op == "||" or x->op == "or")  {
      if (left.is<Bool>() /* and right.is<Bool>() // FIXME: Check in SemanticAnalyzer!! */) {
         // do not evaluate right hand side if already enough with left
         if (x->op == "&&" or x->op == "and") {
            if (!left.as<Bool>()) {
               _curr = Value(false);
               return;
            }
         } else {
            if (left.as<Bool>()) {
               _curr = Value(true);
               return;
            }
         }
         x->right->accept(this);
         Value right = _curr;
         right = Reference::deref(right);
         _curr = Value(x->op == "&&" or x->op == "and" 
                       ? left.as<Bool>() and right.as<Bool>()
                       : left.as<Bool>() or  right.as<Bool>());
         return;
      }
      _error(_T("Los operandos de '%s' no son de tipo 'bool'", x->op.c_str()));
   }

   x->right->accept(this);
   Value right = _curr;
   right = Reference::deref(right);
   if (x->op == ",") {
      return; // already evaluated
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
      _error(_T("Los operandos de '%s' son incompatibles", x->op.c_str()));
   }
   else if (x->op == "+" || x->op == "*" || x->op == "-" || x->op == "/") {
      bool ret = false;
      if (left.type()->is(Type::Basic) and right.type()->is(Type::Basic)) {
         switch (x->op[0]) {
         case '+': {
            if (left.is<Char>() and right.is<Int>()) {
               _curr = Value(char(left.as<Char>() + right.as<Int>()));
               return;
            } else {
               ret = visit_sumprod<_Add>(left, right); break;
            }
         }
         case '*': ret = visit_sumprod<_Mul>(left, right); break;
         case '-': ret = visit_sumprod<_Sub>(left, right); break;
         case '/': ret = visit_sumprod<_Div>(left, right); break;
         }
      } else {
         _curr = left;
         if (!call_operator(x->op, vector<Value>(1, right))) {
            _error(_T("El tipo '%s' no tiene 'operator%s'", 
                      _curr.type()->typestr().c_str(), x->op.c_str()));
         }
         ret = true;
      }
      if (ret) {
         return;
      }
      _error(_T("Los operandos de '%s' son incompatibles", x->op.c_str()));
   }
   else if (x->op == "%") {
      if (left.is<Int>() and right.is<Int>()) {
         _curr = Value(left.as<Int>() % right.as<Int>());
         return;
      }
      _error(_T("Los operandos de '%s' son incompatibles", "%"));
   }
   else if (x->op == "%=") {
      if (!left.is<Reference>()) {
         _error(_T("Para usar '%s' se debe poner una variable a la izquierda", x->op.c_str()));
      }
      left = Reference::deref(left);
      if (left.is<Int>() and right.is<Int>()) {
         left.as<Int>() %= right.as<Int>();
         return;
      }
      _error(_T("Los operandos de '%s' son incompatibles", "%="));
   }
   else if (x->op == "==" || x->op == "!=") {
      if (left.same_type_as(right)) {
         _curr = Value(x->op == "==" ? left.equals(right) : !left.equals(right));
         return;
      }
      _error(_T("Los operandos de '%s' no son del mismo tipo", x->op.c_str()));
   }
   else if (x->op == "<" || x->op == ">" || x->op == "<=" || x->op == ">=") {
      bool ret = false;
      if (left.type()->is(Type::Basic) and right.type()->is(Type::Basic)) {
         if (x->op[0] == '<') {
            ret = (x->op.size() == 1 
                   ? visit_comparison<_Lt>(left, right)
                   : visit_comparison<_Le>(left, right));
         } else {
            ret = (x->op.size() == 1 
                   ? visit_comparison<_Gt>(left, right)
                   : visit_comparison<_Ge>(left, right));
         }
      } else {
         _curr = left;
         if (!call_operator(x->op, vector<Value>(1, right))) {
            _error(_T("El tipo '%s' no tiene 'operator%s'", 
                      _curr.type()->typestr().c_str(), x->op.c_str()));
         }
         ret = true;
      }
      if (ret) {
         return;
      }
      // TODO: Find operator as method or function
      _error(_T("Los operandos de '%s' no son compatibles", x->op.c_str()));
   }
   _curr = left;
   if (call_operator(x->op, vector<Value>(1, right))) {
      return;
   }
   _error(_T("Interpreter::visit_binaryexpr: UNIMPLEMENTED (%s)", x->op.c_str()));
}

inline bool assignment_types_ok(const Value& a, const Value& b) {
   return 
      (a.same_type_as(b)) or
      (a.is<Float>() and b.is<Double>()) or
      (a.is<Double>() and b.is<Float>());
}

void Interpreter::visit_binaryexpr_assignment(Value left, Value right) {
   if (!left.is<Reference>()) {
      _error(_T("Intentas asignar sobre algo que no es una variable"));
   }
   left = Reference::deref(left);
   right = left.type()->convert(right);
   if (right == Value::null) {
      // TODO: Find operator as method or function
      _error(_T("La asignación no se puede hacer porque los "
                "tipos no son compatibles (%s) vs (%s)", 
                left.type_name().c_str(), 
                right.type_name().c_str()));
   }
   if (!left.assign(right)) {
      _error(_T("La asignación no se puede hacer porque los "
                "tipos no son compatibles (%s) vs (%s)", 
                left.type_name().c_str(), 
                right.type_name().c_str()));
   }
   _curr = left;
}

void Interpreter::visit_binaryexpr_op_assignment(char op, Value left, Value right) {
   if (!left.is<Reference>()) {
      _error(_T("Para usar '%s=' se debe poner una variable a la izquierda", op));
   }
   left = Reference::deref(left);
   bool ok = false;
   switch (op) {
   case '+': {
      // FIXME: use 'string::operator+='
      if (left.is<String>() and right.is<String>()) {
         left.as<String>() += right.as<String>();
         ok = true;
      } else if (left.is<String>() and right.is<Char>()) {
         left.as<String>() += right.as<Char>();
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
      string _op = "?=";
      _op[0] = op;
      _error(_T("Los operandos de '%s' no son compatibles", _op.c_str()));
   }
}

void Interpreter::visit_block(Block *x) {
   for (Stmt *stmt : x->stmts) {
      stmt->accept(this);
   }
}

void Interpreter::visit_vardecl(VarDecl *x) {
   string type_name = x->typespec->typestr();
   Type *type = get_type(x->typespec);
   if (type == 0) {
      _error(_T("El tipo '%s' no existe.", type_name.c_str()));
   }
   try {
      Value init = (_curr.is_null() 
                    ? type->create() 
                    : type->convert(_curr));
      if (x->typespec->is(TypeSpec::Const)) {
         init.set_const(true);
      }
      setenv(x->name, init);
   } 
   catch (TypeError& e) {
      _error(e.msg);
   }
}

void Interpreter::visit_arraydecl(ArrayDecl *x) {
   Value init = _curr;
   vector<int> sizes;
   for (int i = 0; i < x->sizes.size(); i++) {
      x->sizes[i]->accept(this);
      _curr = Reference::deref(_curr);
      if (!_curr.is<Int>()) {
         _error(_T("El tamaño de una tabla debe ser un entero"));
      }
      if (_curr.as<Int>() <= 0) {
         _error(_T("El tamaño de una tabla debe ser un entero positivo"));
      }
      const int sz = _curr.as<Int>();
      sizes.push_back(sz);
   }
   Type *celltype = get_type(x->typespec);
   if (celltype == 0) {
      _error(_T("El tipo '%s' no existe", x->typespec->typestr().c_str()));
   }
   // TODO: don't create new Array type every time?
   Type *arraytype = Array::mkarray(celltype, sizes);
   setenv(x->name, (init.is_null() 
                    ? arraytype->create()
                    : arraytype->convert(init)));
}

bool Interpreter::call_operator(string op, const vector<Value>& args) {
   if (!bind_field(_curr, op)) {
      return false;
   }
   if (_curr.is<Overloaded>()) {
      _curr = _curr.as<Overloaded>().resolve(args);
      assert(_curr.is<Callable>());
   }
   Binding& opfun = _curr.as<Callable>();
   const Function *func_type = opfun.func.type()->as<Function>();
   check_arguments(func_type, args);
   _curr = opfun.call(this, args);
   return true;
}
   
void Interpreter::visit_objdecl(ObjDecl *x) {
   Type *type = get_type(x->typespec);
   if (type != 0) {
      vector<Value> args;
      eval_arguments(x->args, args);
      
      string constructor_name = type->name();
      Value new_obj = type->create();
      if (!bind_field(new_obj, constructor_name)) {
         _error(_T("El tipo '%s' no tiene constructor", type->typestr().c_str()));
      }
      if (_curr.is<Overloaded>()) {
         _curr = _curr.as<Overloaded>().resolve(args);
         assert(_curr.is<Callable>());
      }
      Binding& constructor = _curr.as<Callable>();
      const Function *func_type = constructor.func.type()->as<Function>();
      check_arguments(func_type, args);
      constructor.call(this, args); // <-- Invoke!
      
      setenv(x->name, new_obj);
      return;
   }
   _error(_T("The type '%s' is not implemented in MiniCC", 
             x->typespec->typestr().c_str()));
}

void Interpreter::visit_declstmt(DeclStmt* x) {
   for (DeclStmt::Item& item : x->items) {
      if (item.init) {
         item.init->accept(this);
      } else {
         _curr = Value::null;
      }
      item.decl->accept(this);
   }
}

void Interpreter::visit_exprstmt(ExprStmt* x) {
   if (x->expr) {
      x->expr->accept(this);
      if (x->is_return) {
         _ret = _curr;
      }
   }
}

void Interpreter::visit_ifstmt(IfStmt *x) {
   x->cond->accept(this);
   _curr = Reference::deref(_curr);
   if (!_curr.is<Bool>()) {
      if (!call_operator("bool")) {      
         _error(_T("An if's condition needs to be a bool value"));
      }
   }
   if (_curr.as<Bool>()) {
      x->then->accept(this);
   } else {
      if (x->els != 0) {
         x->els->accept(this);
      }
   }
}

void Interpreter::visit_forstmt(ForStmt *x) {
   pushenv("");
   if (x->init) {
      x->init->accept(this);
   }
   while (true) {
      x->cond->accept(this);
      _curr = Reference::deref(_curr);
      if (!_curr.is<Bool>()) {
         if (!call_operator("bool")) {      
            _error(_T("La condición de un for debe ser un valor de tipo bool."));
         }
      }
      if (!_curr.as<Bool>()) {
         break;
      }
      x->substmt->accept(this);
      if (x->post) {
         x->post->accept(this);
      }
   }
   popenv();
}

void Interpreter::visit_whilestmt(WhileStmt *x) {
   pushenv("");
   while (true) {
      x->cond->accept(this);
      _curr = Reference::deref(_curr);
      if (!_curr.is<Bool>()) {
         if (!call_operator("bool")) {      
            _error(_T("La condición de un while debe ser un valor de tipo bool."));
         }
      }
      if (!_curr.as<Bool>()) {
         break;
      }
      x->substmt->accept(this);
   }
   popenv();
}

void Interpreter::invoke_user_func(FuncDecl *decl, const vector<Value>& args) {
   pushenv(decl->funcname());
   invoke_func_prepare(decl, args);
   decl->block->accept(this);
   popenv();
}

void Interpreter::visit_callexpr_getfunc(CallExpr *x) {
   x->func->accept(this);
   _curr = Reference::deref(_curr);
   if (!_curr.is<Callable>() and !_curr.is<Overloaded>()) {
      _error(_T("Calling something other than a function."));
   }
}

void Interpreter::check_arguments(const Function *func_type, const vector<Value>& args) {
   for (int i = 0; i < args.size(); i++) {
      Type *param_type = func_type->param(i);
      if (param_type == Any) {
         continue;
      }
      string t1 = param_type->typestr();
      Value arg_i = args[i];
      if (!func_type->param(i)->is<Reference>()) {
         arg_i = Reference::deref(arg_i);
      } else if (!arg_i.type()->is<Reference>()) {
         _error(_T("En el parámetro %d se requiere una variable.", i+1));
      }
      string t2 = arg_i.type()->typestr();
      if (t1 != t2) {
         _error(_T("El argumento %d no es compatible con el tipo del parámetro "
                   "(%s vs %s)", i+1, t1.c_str(), t2.c_str()));
      }
   }
}

void Interpreter::eval_arguments(const std::vector<Expr*>& exprs, std::vector<Value>& args) {
   for (int i = 0; i < exprs.size(); i++) {
      exprs[i]->accept(this);
      args.push_back(_curr);
   }
}

void Interpreter::check_result(Binding& fn, const Function *func_type) {
   if (_ret == Value::null && !func_type->is_void()) {
      string name = fn.func.as<Function>().ptr->name;
      Type *return_type = func_type->return_type();
      _error(_T("La función '%s' debería devolver un '%s'", 
                name.c_str(),
                return_type->typestr().c_str()));
   }
}

bool Interpreter::visit_type_conversion(CallExpr *x, const vector<Value>& args) {
   FullIdent *id = x->func->as<FullIdent>();
   if (id != 0) {
      TypeSpec spec(id);
      Type *type = get_type(&spec);
      if (type != 0) {
         if (args.size() != 1) {
            _error(_T("La conversión de tipo recibe un solo argumento"));
         }
         _curr = type->convert(args[0]);
         if (_curr == Value::null) {
            _curr = args[0];
            call_operator(id->typestr());
         }
         return true;
      }
   }
   return false;
}

void Interpreter::visit_callexpr_call(Value func, const vector<Value>& args) {
   // TODO: Find operator() (method or function)
   if (func.is<Overloaded>()) {
      func = func.as<Overloaded>().resolve(args);
      assert(func.is<Callable>());
   }
   Binding& fn = func.as<Callable>();
   const Function *func_type = fn.func.type()->as<Function>();
   check_arguments(func_type, args);
   _ret = fn.call(this, args); // <-- Invoke!
   check_result(fn, func_type);
   _curr = _ret;
}

void Interpreter::visit_callexpr(CallExpr *x) {
   vector<Value> args;
   eval_arguments(x->args, args);
   if (visit_type_conversion(x, args)) {
      return;
   }
   visit_callexpr_getfunc(x);
   visit_callexpr_call(_curr, args);
}

void Interpreter::visit_indexexpr(IndexExpr *x) {
   x->base->accept(this);
   Value base = Reference::deref(_curr);
   x->index->accept(this);
   Value index = Reference::deref(_curr);
   if (base.is<Array>()) {
      if (!index.is<Int>()) {
         _error(_T("El índice en un acceso a tabla debe ser un entero"));
      }
      vector<Value>& vals = base.as<Array>();
      const int i = index.as<Int>();
      if (i < 0 || i >= vals.size()) {
         // TODO: Producir error de ejecución
         _error(_T("La casilla %d no existe", i));
      }
      _curr = Reference::mkref(vals[i]);
      return;
   }
   _curr = base;
   if (!call_operator("[]", vector<Value>(1, index))) {
      _error(_T("Las expresiones de índice deben usarse sobre tablas o vectores"));
   }
}

bool Interpreter::bind_field(Value obj, string method_name) {
   vector<Value> candidates;
   int count = obj.type()->get_field(obj, method_name, candidates);
   if (count == 1) {
      Value& v = candidates[0];
      if (v.is<Function>()) {
         _curr = Callable::self->mkvalue(obj, v);
      } else {
         _curr = v;
      }
      return true;
   } else if (count > 1) {
      _curr = Overloaded::self->mkvalue(obj, candidates);
      return true;
   }
   return false;
}

void Interpreter::visit_fieldexpr(FieldExpr *x) {
   x->base->accept(this);
   _curr = Reference::deref(_curr);
   if (x->pointer) {
      if (!call_operator("*")) {
         _error(_T("El tipo '%s' no tiene 'operator*'", 
                   _curr.type()->typestr().c_str()));
      }
      _curr = Reference::deref(_curr);
   }
   Value obj = _curr;
   if (obj.is<Struct>()) {
      // TODO: Move this to 'get_field' in 'Struct' class???
      SimpleTable<Value>& fields = obj.as<Struct>();
      Value v;
      if (!fields.get(x->field->name, v)) {
         _error(_T("No existe el campo '%s'", x->field->name.c_str()));
      }
      _curr = Reference::mkref(v);
      return;
   }
   if (!bind_field(obj, x->field->name)) {
      _error(_T("Este objeto no tiene un campo '%s'", x->field->name.c_str()));
   }
}

void Interpreter::visit_condexpr(CondExpr *x) {
   x->cond->accept(this);
   if (!_curr.is<Bool>()) {
      _error(_T("Una expresión condicional debe tener valor "
                "de tipo 'bool' antes del interrogante"));
   }
   if (_curr.as<Bool>()) {
      x->then->accept(this);
   } else {
      if (x->els != 0) {
         x->els->accept(this);
      }
   }
}

void Interpreter::visit_exprlist(ExprList *x) {
   Value v = VectorValue::make();
   vector<Value>& vals = v.as<VectorValue>();
   for (Expr *e : x->exprs) {
      e->accept(this);
      vals.push_back(_curr);
   }
   _curr = v;
}

void Interpreter::visit_signexpr(SignExpr *x) {
   x->expr->accept(this);
   if (x->kind == SignExpr::Positive) {
      return;
   }
   _curr = Reference::deref(_curr);
   if (_curr.is<Int>()) {
      _curr.as<Int>() = -_curr.as<Int>();
   } else if (_curr.is<Float>()) {
      _curr.as<Float>() = -_curr.as<Float>();
   } else if (_curr.is<Double>()) {
      _curr.as<Double>() = -_curr.as<Double>();
   } else {
      _error(_T("El cambio de signo para '%s' no tiene sentido",
                _curr.type_name().c_str()));
   }
}

void Interpreter::visit_increxpr(IncrExpr *x) {
   x->expr->accept(this);
   if (!_curr.is<Reference>()) {
      _error(_T("Hay que incrementar una variable, no un valor"));
   }
   Value after  = Reference::deref(_curr);
   Value before = after.clone();
   if (after.is<Int>()) {
      if (x->kind == IncrExpr::Positive) {
         after.as<Int>()++;
         after.touch();
      } else {
         after.as<Int>()--;
         after.touch();
      }
   } else {
      _curr = after;
      string op = (x->kind == IncrExpr::Positive ? "++" : "--");
      if (!call_operator(op)) {
         _error(_T("El tipo '%s' no tiene 'operator%s'", 
                   _curr.type()->typestr().c_str(), op.c_str()));
      }
   }
   _curr = (x->preincr ? before : after);
}

void Interpreter::visit_negexpr(NegExpr *x) {
   x->expr->accept(this);
   if (!_curr.is<Bool>()) {
      _error(_T("Para negar una expresión ésta debe ser de tipo 'bool'"));
   }
   _curr.as<Bool>() = !_curr.as<Bool>();
}

void Interpreter::visit_typedefdecl(TypedefDecl *x) {
   string name = x->decl->name;
   Type *type = get_type(x->decl->typespec);
   assert(type != 0);
   if (x->decl->is<VarDecl>()) {
      const VarDecl *var = x->decl->as<VarDecl>();
      register_type(var->name, type);
   } else if (x->decl->is<ArrayDecl>()) {
      const ArrayDecl *array = x->decl->as<ArrayDecl>();
      array->sizes[0]->accept(this);
      if (!_curr.is<Int>()) {
         _error(_T("El tamaño de un array debería ser un entero"));
      }
      const int size = _curr.as<Int>();
      register_type(array->name, new Array(type, size));
   }
}

void Interpreter::visit_derefexpr(DerefExpr *x) {
   // TODO: deal with pointers
   x->expr->accept(this);
   _curr = Reference::deref(_curr);
   if (!call_operator("*")) {
      _error(_T("El tipo '%s' no tiene 'operator*'", 
                _curr.type()->typestr().c_str()));
   }
}
