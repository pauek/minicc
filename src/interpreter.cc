
#include <iostream>
#include <assert.h>
#include "cast.h"
#include "ast.hh"
#include "value.hh"
#include "types.hh"
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

void Interpreter::visit_program_prepare(Program *X) {
   prepare_global_environment();
   for (Ast *n : X->nodes) {
      Eval(n);
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
   _curr = opfun.call(args);
   return true;
}

void Interpreter::invoke_user_func(FuncDecl *decl, const vector<Value>& args) {
   pushenv(decl->funcname());
   invoke_func_prepare(decl, args);
   Eval(decl->block);
   popenv();
}

void Interpreter::visit_callexpr_getfunc(CallExpr *X) {
   Eval(X->func);
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
      Eval(exprs[i]);
      args.push_back(_curr);
   }
}

void Interpreter::check_result(Binding& fn, const Function *func_type) {
   if (_ret == Value::null && !func_type->is_void()) {
      string name = fn.func.as<Function>().ptr->name;
      const Type *return_type = func_type->return_type();
      _error(_T("La función '%s' debería devolver un '%s'", 
                name.c_str(),
                return_type->typestr().c_str()));
   }
}

bool Interpreter::visit_type_conversion(CallExpr *X, const vector<Value>& args) {
   FullIdent *id = X->func->as<FullIdent>();
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
   _ret = fn.call(args); // <-- Invoke!
   check_result(fn, func_type);
   _curr = _ret;
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


// Eval

void Interpreter::Eval(Ast* ast) {
   assert(ast != nullptr);
   switch (ast->type()) {
   case AstType::Program: {
      Program *X = cast<Program>(ast);
      visit_program_prepare(X);
      visit_program_find_main();
      _curr.as<Callable>().call(vector<Value>());
      break;
   }
   case AstType::Macro:
      break;
   case AstType::Using: {
      Using *X = cast<Using>(ast);
      if (!using_namespace(X->namespc)) {
         _error(_T("No se ha encontrado el \"namespace\" '%s'.", 
                   X->namespc.c_str()));
      }
      break;
   }
   case AstType::Include: {
      Include *X = cast<Include>(ast);
      include_header_file(X->filename);
      break;
   }
   case AstType::FuncDecl: {
      FuncDecl *X = cast<FuncDecl>(ast);
      string funcname = X->funcname();
      Type *return_type = get_type(X->return_typespec);  // return_type == 0 means 'void'
      Function *functype = new Function(return_type);
      for (auto p : X->params) {
         Type *param_type = get_type(p->typespec);
         assert(param_type != 0);
         functype->add_params(param_type);
      }
      Value func = functype->mkvalue(new UserFunc(funcname, X, this));
      Value callable = Callable::self->mkvalue(Value::null, func); // bind with 'null'
      setenv(funcname, callable, Hidden);
      break;
   }
   case AstType::StructDecl: {
      StructDecl *X = cast<StructDecl>(ast);
      Struct *type = new Struct(X->struct_name());
      for (int i = 0; i < X->decls.size(); i++) {
         DeclStmt& decl = *X->decls[i];
         Type *field_type = get_type(decl.typespec);
         assert(type != 0);
         for (DeclStmt::Item& item : decl.items) {
            if (item.decl->is<ArrayDecl>()) {
               Expr *size_expr = dynamic_cast<ArrayDecl*>(item.decl)->sizes[0];
               Literal *size_lit = dynamic_cast<Literal*>(size_expr);
               assert(size_lit != 0);
               assert(size_lit->kind == Literal::Int);
               const int sz = size_lit->val.as_int;
               // TODO: don't create new Array type every time?
               type->add_field(item.decl->name, new Array(field_type, sz)); 
            } else {
               type->add_field(item.decl->name, field_type);
            }
         }
      }
      register_type(X->struct_name(), type);      
      break;
   }
   case AstType::FullIdent: {
      FullIdent *X = cast<FullIdent>(ast);
      Value v;
      // Try a namespace
      SimpleIdent *namespc_or_class = X->get_potential_namespace_or_class();
      if (namespc_or_class != 0) {
         Environment *namespc = get_namespace(namespc_or_class->name);
         if (namespc != 0) {
            if (namespc->get(X->name, v)) {
               goto found;
            }
            _error(_T("No se ha encontrado '%s' en el namespace '%s'.", 
                      X->name.c_str(), namespc_or_class->name.c_str()));
            return;
         }
      }
      // Try a static variable in a class
      if (namespc_or_class != 0) {
         FullIdent fid(namespc_or_class->name);
         TypeSpec spec(&fid);
         Type *type = get_type(&spec);
         if (type != 0 and !type->get_static(X->name, v)) {
            _error(_T("No se ha encontrado '%s' en la clase '%s'.",
                      X->name.c_str(), namespc_or_class->name.c_str()));
         }
         goto found;
      }
      // Try the environment
      if (getenv(X->name, v)) {
         goto found;
      } 
      _error(_T("No se ha encontrado '%s'.", 
                X->name.c_str()));
   found:
      _curr = (v.is<Reference>() ? v : Reference::mkref(v));
      break;
   }
   case AstType::Literal: {
      Literal *X = cast<Literal>(ast);
      switch (X->kind) {
      case Literal::String: _curr = Value(*X->val.as_string.s); break;
      case Literal::Int:    _curr = Value(X->val.as_int);       break;
      case Literal::Double: _curr = Value(X->val.as_double);    break;
      case Literal::Bool:   _curr = Value(X->val.as_bool);      break;
      case Literal::Char:   _curr = Value(X->val.as_char);      break;
      default:
         _error(_T("Interpreter::visit_literal: UNIMPLEMENTED"));
      }
      break;
   }
   case AstType::BinaryExpr: {
      BinaryExpr *X = cast<BinaryExpr>(ast);
      Eval(X->left);
      Value left = _curr;
      if (X->kind != Expr::Eqment) {
         left = Reference::deref(left);
      }

      // Handle booleans first, since they might not need to evaluate the
      // second part
      // 
      if (X->op == "&&" or X->op == "and" or X->op == "||" or X->op == "or")  {
         if (left.is<Bool>() /* and right.is<Bool>() // FIXME: Check in SemanticAnalyzer!! */) {
            // do not evaluate right hand side if already enough with left
            if (X->op == "&&" or X->op == "and") {
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
            Eval(X->right);
            Value right = _curr;
            right = Reference::deref(right);
            _curr = Value(X->op == "&&" or X->op == "and" 
                          ? left.as<Bool>() and right.as<Bool>()
                          : left.as<Bool>() or  right.as<Bool>());
            return;
         }
         _error(_T("Los operandos de '%s' no son de tipo 'bool'", X->op.c_str()));
      }

      Eval(X->right);
      Value right = _curr;
      right = Reference::deref(right);
      if (X->op == ",") {
         return; // already evaluated
      }
      if (X->op == "=") {
         visit_binaryexpr_assignment(left, right);
         return;
      }
      if (X->op == "+=" || X->op == "-=" || X->op == "*=" || X->op == "/=" ||
          X->op == "&=" || X->op == "|=" || X->op == "^=") {
         visit_binaryexpr_op_assignment(X->op[0], left, right);
         return;
      } 
      else if (X->op == "&" || X->op == "|" || X->op == "^") {
         bool ret = false;
         switch (X->op[0]) {
         case '&': ret = visit_bitop<_And>(left, right); break;
         case '|': ret = visit_bitop<_Or >(left, right); break;
         case '^': ret = visit_bitop<_Xor>(left, right); break;
         }
         if (ret) {
            return;
         }
         _error(_T("Los operandos de '%s' son incompatibles", X->op.c_str()));
      }
      else if (X->op == "+" || X->op == "*" || X->op == "-" || X->op == "/") {
         bool ret = false;
         if (left.type()->is(Type::Basic) and right.type()->is(Type::Basic)) {
            switch (X->op[0]) {
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
            if (!call_operator(X->op, vector<Value>(1, right))) {
               _error(_T("El tipo '%s' no tiene 'operator%s'", 
                         _curr.type()->typestr().c_str(), X->op.c_str()));
            }
            ret = true;
         }
         if (ret) {
            return;
         }
         _error(_T("Los operandos de '%s' son incompatibles", X->op.c_str()));
      }
      else if (X->op == "%") {
         if (left.is<Int>() and right.is<Int>()) {
            _curr = Value(left.as<Int>() % right.as<Int>());
            return;
         }
         _error(_T("Los operandos de '%s' son incompatibles", "%"));
      }
      else if (X->op == "%=") {
         if (!left.is<Reference>()) {
            _error(_T("Para usar '%s' se debe poner una variable a la izquierda", X->op.c_str()));
         }
         left = Reference::deref(left);
         if (left.is<Int>() and right.is<Int>()) {
            left.as<Int>() %= right.as<Int>();
            return;
         }
         _error(_T("Los operandos de '%s' son incompatibles", "%="));
      }
      else if (X->op == "==" || X->op == "!=") {
         if (left.same_type_as(right)) {
            _curr = Value(X->op == "==" ? left.equals(right) : !left.equals(right));
            return;
         }
         _error(_T("Los operandos de '%s' no son del mismo tipo", X->op.c_str()));
      }
      else if (X->op == "<" || X->op == ">" || X->op == "<=" || X->op == ">=") {
         bool ret = false;
         if (left.type()->is(Type::Basic) and right.type()->is(Type::Basic)) {
            if (X->op[0] == '<') {
               ret = (X->op.size() == 1 
                      ? visit_comparison<_Lt>(left, right)
                      : visit_comparison<_Le>(left, right));
            } else {
               ret = (X->op.size() == 1 
                      ? visit_comparison<_Gt>(left, right)
                      : visit_comparison<_Ge>(left, right));
            }
         } else {
            _curr = left;
            if (!call_operator(X->op, vector<Value>(1, right))) {
               _error(_T("El tipo '%s' no tiene 'operator%s'", 
                         _curr.type()->typestr().c_str(), X->op.c_str()));
            }
            ret = true;
         }
         if (ret) {
            return;
         }
         // TODO: Find operator as method or function
         _error(_T("Los operandos de '%s' no son compatibles", X->op.c_str()));
      }
      _curr = left;
      if (call_operator(X->op, vector<Value>(1, right))) {
         return;
      }
      _error(_T("Interpreter::visit_binaryexpr: UNIMPLEMENTED (%s)", X->op.c_str()));

      break;
   }
   case AstType::Block: {
      Block *X = cast<Block>(ast);
      for (Stmt *stmt : X->stmts) {
         Eval(stmt);
      }
      break;
   }
   case AstType::VarDecl: {
      VarDecl *X = cast<VarDecl>(ast);
      string type_name = X->typespec->typestr();
      Type *type = get_type(X->typespec);
      if (type == 0) {
         _error(_T("El tipo '%s' no existe.", type_name.c_str()));
      }
      try {
         Value init = (_curr.is_null() 
                       ? type->create() 
                       : type->convert(_curr));
         if (X->typespec->is(TypeSpec::Const)) {
            init.set_const(true);
         }
         setenv(X->name, init);
      } 
      catch (TypeError& e) {
         _error(e.msg);
      }
      break;      
   }
   case AstType::ArrayDecl: {
      ArrayDecl *X = cast<ArrayDecl>(ast);
      Value init = _curr;
      vector<int> sizes;
      for (int i = 0; i < X->sizes.size(); i++) {
         Eval(X->sizes[i]);
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
      Type *celltype = get_type(X->typespec);
      if (celltype == 0) {
         _error(_T("El tipo '%s' no existe", X->typespec->typestr().c_str()));
      }
      // TODO: don't create new Array type every time?
      Type *arraytype = Array::mkarray(celltype, sizes);
      setenv(X->name, (init.is_null() 
                       ? arraytype->create()
                       : arraytype->convert(init)));      
      break;
   }
   case AstType::ObjDecl: {
      ObjDecl *X = cast<ObjDecl>(ast);
      Type *type = get_type(X->typespec);
      if (type != 0) {
         vector<Value> args;
         eval_arguments(X->args, args);
         
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
         constructor.call(args); // <-- Invoke!
         
         setenv(X->name, new_obj);
         return;
      }
      _error(_T("The type '%s' is not implemented in MiniCC", 
                X->typespec->typestr().c_str()));
      break;
   }
   case AstType::DeclStmt: {
      DeclStmt *X = cast<DeclStmt>(ast);      
      for (DeclStmt::Item& item : X->items) {
         if (item.init) {
            Eval(item.init);
         } else {
            _curr = Value::null;
         }
         Eval(item.decl);
      }
      break;
   }
   case AstType::ExprStmt: {
      ExprStmt *X = cast<ExprStmt>(ast);
      if (X->expr) {
         Eval(X->expr);
         if (X->is_return) {
            _ret = _curr;
         }
      }
      break;      
   }
   case AstType::IfStmt: {
      IfStmt *X = cast<IfStmt>(ast);
      Eval(X->cond);
      _curr = Reference::deref(_curr);
      if (!_curr.is<Bool>()) {
         if (!call_operator("bool")) {      
            _error(_T("An if's condition needs to be a bool value"));
         }
      }
      if (_curr.as<Bool>()) {
         Eval(X->then);
      } else {
         if (X->els != 0) {
            Eval(X->els);
         }
      }
      break;
   }
   case AstType::ForStmt: {
      ForStmt *X = cast<ForStmt>(ast);
      pushenv("");
      if (X->init) {
         Eval(X->init);
      }
      while (true) {
         Eval(X->cond);
         _curr = Reference::deref(_curr);
         if (!_curr.is<Bool>()) {
            if (!call_operator("bool")) {      
               _error(_T("La condición de un for debe ser un valor de tipo bool."));
            }
         }
         if (!_curr.as<Bool>()) {
            break;
         }
         Eval(X->substmt);
         if (X->post) {
            Eval(X->post);
         }
      }
      popenv();
      break;      
   }
   case AstType::WhileStmt: {
      WhileStmt *X = cast<WhileStmt>(ast);
      pushenv("");
      while (true) {
         Eval(X->cond);
         _curr = Reference::deref(_curr);
         if (!_curr.is<Bool>()) {
            if (!call_operator("bool")) {      
               _error(_T("La condición de un while debe ser un valor de tipo bool."));
            }
         }
         if (!_curr.as<Bool>()) {
            break;
         }
         Eval(X->substmt);
      }
      popenv();
      break;
   }
   case AstType::CallExpr: {
      CallExpr *X = cast<CallExpr>(ast);
      vector<Value> args;
      eval_arguments(X->args, args);
      if (visit_type_conversion(X, args)) {
         return;
      }
      visit_callexpr_getfunc(X);
      visit_callexpr_call(_curr, args);
      break;
   }
   case AstType::IndexExpr: {
      IndexExpr *X = cast<IndexExpr>(ast);
      Eval(X->base);
      Value base = Reference::deref(_curr);
      Eval(X->index);
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
      break;
   }
   case AstType::FieldExpr: {
      FieldExpr *X = cast<FieldExpr>(ast);
      Eval(X->base);
      _curr = Reference::deref(_curr);
      if (X->pointer) {
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
         if (!fields.get(X->field->name, v)) {
            _error(_T("No existe el campo '%s'", X->field->name.c_str()));
         }
         _curr = Reference::mkref(v);
         return;
      }
      if (!bind_field(obj, X->field->name)) {
         _error(_T("Este objeto no tiene un campo '%s'", X->field->name.c_str()));
      }
      break;
   }
   case AstType::CondExpr: {
      CondExpr *X = cast<CondExpr>(ast);
      Eval(X->cond);
      _curr = Reference::deref(_curr);
      if (!_curr.is<Bool>()) {
         _error(_T("Una expresión condicional debe tener valor "
                   "de tipo 'bool' antes del interrogante"));
      }
      if (_curr.as<Bool>()) {
         Eval(X->then);
      } else if (X->els != 0) {
         Eval(X->els);
      } else {
         assert(false);
      }
      break;
   }
   case AstType::ExprList: {
      ExprList *X = cast<ExprList>(ast);
      Value v = VectorValue::make();
      vector<Value>& vals = v.as<VectorValue>();
      for (Expr *e : X->exprs) {
         Eval(e);
         vals.push_back(_curr);
      }
      _curr = v;
      break;   
   }
   case AstType::SignExpr: {
      SignExpr *X = cast<SignExpr>(ast);
      Eval(X->expr);
      if (X->kind == SignExpr::Positive) {
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
      break;
   }
   case AstType::IncrExpr: {
      IncrExpr *X = cast<IncrExpr>(ast);
      Eval(X->expr);
      if (!_curr.is<Reference>()) {
         _error(_T("Hay que incrementar una variable, no un valor"));
      }
      Value after  = Reference::deref(_curr);
      Value before = after.clone();
      if (after.is<Int>()) {
         if (X->kind == IncrExpr::Positive) {
            after.as<Int>()++;
            after.touch();
         } else {
            after.as<Int>()--;
            after.touch();
         }
      } else {
         _curr = after;
         string op = (X->kind == IncrExpr::Positive ? "++" : "--");
         if (!call_operator(op)) {
            _error(_T("El tipo '%s' no tiene 'operator%s'", 
                      _curr.type()->typestr().c_str(), op.c_str()));
         }
      }
      _curr = (X->preincr ? before : after);
      break;
   }
   case AstType::NegExpr: {
      NegExpr *X = cast<NegExpr>(ast);
      Eval(X->expr);
      if (!_curr.is<Bool>()) {
         _error(_T("Para negar una expresión ésta debe ser de tipo 'bool'"));
      }
      _curr.as<Bool>() = !_curr.as<Bool>();
      break;
   }
   case AstType::TypedefDecl: {
      TypedefDecl *X = cast<TypedefDecl>(ast);
      string name = X->decl->name;
      Type *type = get_type(X->decl->typespec);
      assert(type != 0);
      if (X->decl->is<VarDecl>()) {
         const VarDecl *var = X->decl->as<VarDecl>();
         register_type(var->name, type);
      } else if (X->decl->is<ArrayDecl>()) {
         const ArrayDecl *array = X->decl->as<ArrayDecl>();
         Eval(array->sizes[0]);
         if (!_curr.is<Int>()) {
            _error(_T("El tamaño de un array debería ser un entero"));
         }
         const int size = _curr.as<Int>();
         register_type(array->name, new Array(type, size));
      }
      break;
   }
   case AstType::DerefExpr: {
      DerefExpr *X = cast<DerefExpr>(ast);
      Eval(X->expr);
      _curr = Reference::deref(_curr);
      if (!call_operator("*")) {
         _error(_T("El tipo '%s' no tiene 'operator*'", 
                   _curr.type()->typestr().c_str()));
      }
      break;
   }
   default:
      assert(false);
   }
}

void Eval(Ast *ast, std::istream& in, std::ostream& out) {
   Interpreter(&in, &out).Eval(ast);
}