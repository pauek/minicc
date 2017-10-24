#include "cast.h"
#include "ast.hh"
#include "translator.hh"
#include "interpreter.hh"
#include "semantic.hh"
using namespace std;

struct SemanticAnalyzer : public WithEnvironment
{
    std::string  _curr_varname;
            Ast *_curr_node;
          Value  _curr, _ret;

     void  visit_binaryexpr_assignment(BinaryExpr *x, Value left, Value right);
     void  visit_binaryexpr_op_assignment(char, Value left, Value right);
     void  visit_callexpr_getfunc(CallExpr *x);
     bool  visit_type_conversion(CallExpr *x, const std::vector<Value>& args);
     void  check_arguments(const Function *func_type, 
                           const std::vector<Value>& argvals, 
                           std::vector<Expr*> *args = 0);
     bool  bind_field(Value obj, string method_name);
     bool  call_operator(string op, 
                         const std::vector<Value>& args = std::vector<Value>());
     void  check_condition(Expr *cond, std::string who);
     void  check_unknown(Value v, Ast *x, string varname);
     void  eval_arguments(const std::vector<Expr *>& args,
                          std::vector<Value>& argvals);
   
   template<class Op>
     bool  visit_op_assignment(Value left, Value right);

   template<class Op>
     bool  visit_bitop_assignment(Value left, Value right);

   template<class Op>
     bool  visit_sumprod(Value left, Value right, std::string what);

   template<class Op>
     bool  visit_bitop(Value left, Value right);

   template<class Op>
     bool  visit_comparison(Value left, Value right);

   void Analyze(Ast *ast);
};

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

template<class Op, class Type>
Value checked_op(Value left, Value right) {
   if (left.is_concrete() and right.is_concrete()) {
      return Value(Op::eval(left.as<Type>(), right.as<Type>()));
   } else if (left.is_unknown() or right.is_unknown()) {
      return Type::self->create(); // will be unknown
   } else {
      return Type::self->create_abstract();
   }
}

template<class Op, class Type>
void checked_op_assign(Value& left, Value right) {
   if (left.is_concrete() and right.is_concrete()) {
      Op::eval(left.as<Type>(), right.as<Type>());
   } else if (left.is_unknown() or right.is_unknown()) {
      left = Type::self->create(); // will be unknown
   } else {
      left = Type::self->create_abstract();
   }
}

void SemanticAnalyzer::visit_binaryexpr_assignment(BinaryExpr* X, 
                                                   Value left, Value right) {
   if (!left.is<Reference>()) {
      X->add_error(_T("Intentas asignar sobre algo que no es una variable."));
      return;
   }
   left = Reference::deref(left);
   if (left.is_const()) {
      X->add_error(_T("La variable '%s' no se puede modificar (es 'const').",
                      _curr_varname.c_str()));
      return;
   }
   Value right2 = left.type()->convert(right);
   if (right2 == Value::null) {
      string right_type_name = "void";
      if (!right.is_null()) {
         right_type_name = right.type_name();
      }
      X->add_error(_T("No se puede asignar un '%s' a una variable de tipo '%s'.",
                      right_type_name.c_str(), 
                      left.type_name().c_str()));
      return;
   }
   right = right2;
   if (!left.assign(right)) {
      X->add_error(_T("No se puede asignar un '%s' a una variable de tipo '%s'.",
                      right.type_name().c_str(), 
                      left.type_name().c_str()));
      return;
   }
   _curr = left;
}

void SemanticAnalyzer::visit_binaryexpr_op_assignment(char op, Value left, Value right) {
   if (!left.is<Reference>()) {
      _curr_node->add_error(
         _T("En el operador '%c=' la parte izquierda debe ser una variable.", op)
      );
      return;
   }
   left = Reference::deref(left);
   bool ok = false;
   string textual = "";
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
      textual = _T("Intentas sumar un '%s' y un '%s'.",
                   left.type()->typestr().c_str(),
                   right.type()->typestr().c_str());
      break;
   }
   case '-': {
      ok = visit_op_assignment<_ASub>(left, right);
      textual = _T("Intentas restar un '%s' y un '%s'.",
                   left.type()->typestr().c_str(),
                   right.type()->typestr().c_str());
      break;
   }
   case '*': {
      ok = visit_op_assignment<_AMul>(left, right); 
      textual = _T("Intentas multiplicar un '%s' por un '%s'.",
                   left.type()->typestr().c_str(),
                   right.type()->typestr().c_str());
      break;
   }
   case '/': {
      ok = visit_op_assignment<_ADiv>(left, right); 
      textual = _T("Intentas dividir un '%s' por un '%s'.",
                   left.type()->typestr().c_str(),
                   right.type()->typestr().c_str());
      break;
   }
   case '&': {
      ok = visit_bitop_assignment<_AAnd>(left, right); 
      textual = _T("Intentas hacer un AND de un '%s' con un '%s'.",
                   left.type()->typestr().c_str(),
                   right.type()->typestr().c_str());
      break;
   }
   case '|': {
      ok = visit_bitop_assignment<_AOr >(left, right); 
      textual = _T("Intentas hacer un OR de un '%s' con un '%s'.",
                   left.type()->typestr().c_str(),
                   right.type()->typestr().c_str());
      break;
   }
   case '^': {
      ok = visit_bitop_assignment<_AXor>(left, right); 
      textual = _T("Intentas hacer un XOR de un '%s' con un '%s'.",
                   left.type()->typestr().c_str(),
                   right.type()->typestr().c_str());
      break;
   }
   }
   if (!ok) {
      string _op = "?=";
      _op[0] = op;
      _curr_node->add_error(textual);
   }
}

void SemanticAnalyzer::visit_callexpr_getfunc(CallExpr *X) {
   _curr_node = X;
   Analyze(X->func);
   if (_curr.is<UnknownType>()) {
      return;
   }
   _curr = Reference::deref(_curr);
   if (!_curr.is<Callable>() and !_curr.is<Overloaded>()) {
      X->add_error(_T("Intentas llamar como función un valor de tipo '%s'.", 
                      _curr.type()->typestr().c_str()));
   }
}

bool SemanticAnalyzer::visit_type_conversion(CallExpr *X, const vector<Value>& args) {
   _curr_node = X;
   _curr_varname = "";
   FullIdent *id = X->func->as<FullIdent>();
   if (id != 0) {
      TypeSpec spec(id);
      Type *type = get_type(&spec);
      if (type != 0) {
         if (args.size() != 1) {
            X->add_error(_T("La conversión de tipo recibe un solo argumento."));
         }
         _curr = type->convert(args[0]);
         if (_curr == Value::null) {
            _curr = args[0];
            if (!call_operator(id->typestr())) {
               X->add_error(_T("No se puede convertir un '%s' en un '%s'.",
                               args[0].type()->typestr().c_str(),
                               type->typestr().c_str()));
            }
         }
         return true;
      }
   }
   return false;
}

void SemanticAnalyzer::check_arguments(const Function *func_type,
                                       const vector<Value>& argvals,
                                       vector<Expr*> *args)
{
   if (func_type->num_params() != argvals.size()) {
      _curr_node->add_error(_T("Número de argumentos erróneo (son %d y deberían ser %d).",
                               argvals.size(), func_type->num_params()));
      return;
   }
   for (int i = 0; i < argvals.size(); i++) {
      Type *param_type = func_type->param(i);
      if ((args != 0 and (*args)[i]->has_errors()) or
          param_type == Any) {
         continue;
      }
      string t1 = param_type->typestr();
      Value arg_i = argvals[i];
      if (!func_type->param(i)->is<Reference>()) {
         arg_i = Reference::deref(arg_i);
      } else if (!arg_i.type()->is<Reference>()) {
         string cual = _T(numeral[i+1]);
         assert(args != 0);
         (*args)[i]->add_error(_T("En el %s parámetro se requiere una variable.", cual.c_str()));
      }
      string t2 = arg_i.type()->typestr();
      if (t1 != t2 and !(*args)[i]->has_errors()) {
         _curr_node->add_error(_T("El argumento %d no es compatible con el tipo del parámetro "
                                  "(%s vs %s)", i+1, t1.c_str(), t2.c_str()));
      }
   }
}

bool SemanticAnalyzer::bind_field(Value obj, string method_name) {
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


bool SemanticAnalyzer::call_operator(string op, const vector<Value>& args) {
   if (!bind_field(_curr, op)) {
      return false;
   }
   if (_curr.is<Overloaded>()) {
      _curr = _curr.as<Overloaded>().resolve(args);
      assert(_curr.is<Callable>());
   }
   Binding& opfun = _curr.as<Callable>();
   const Function *func_type = opfun.func.type()->as<Function>();
   if (!opfun.call_abstract(_curr_node, args)) {
      check_arguments(func_type, args);
   }
   _curr = func_type->return_type()->create_abstract();
   return true;
}

void SemanticAnalyzer::check_condition(Expr *cond, string who) {
   Analyze(cond);
   _curr = Reference::deref(_curr);
   if (!_curr.is<Bool>()) {
      if (!call_operator("bool")) {
         cond->add_error(_T("La condición de un '%s' debe ser de tipo 'bool'.",
                            who.c_str()));
      }
   } else {
      if (who == "if" and !_curr.is_abstract()) {
         cond->add_error(_T("La condición siempre vale '%s'.", 
                            (_curr.as<Bool>() ? "true" : "false")));
      }
   }
}

void SemanticAnalyzer::check_unknown(Value v, Ast *X, string varname) {
   if (v.is_unknown()) {
      X->add_error(_T("Utilizas la variable '%s' sin haberla inicializado.", 
                      varname.c_str()));
   }
}

void SemanticAnalyzer::eval_arguments(const vector<Expr *>& args,
                                      vector<Value>& argvals) {
   for (int i = 0; i < args.size(); i++) {
      Analyze(args[i]);
      argvals.push_back(_curr);
   }
}


template<class Op>
bool SemanticAnalyzer::visit_op_assignment(Value left, Value _right) {
   Value right = left.type()->convert(_right);
   if (left.is<Int>() and right.is<Int>()) {
      checked_op_assign<Op, Int>(left, right);
      return true;
   } 
   if (left.is<Float>() and right.is<Float>()) {
      checked_op_assign<Op, Float>(left, right);
      return true;
   }
   if (left.is<Double>() and right.is<Double>()) {
      checked_op_assign<Op, Double>(left, right);
      return true;
   }
   return false;
}

template<class Op>
bool SemanticAnalyzer::visit_bitop_assignment(Value left, Value _right) {
   Value right = left.type()->convert(_right);
   if (left.is<Int>() and right.is<Int>()) {
      Op::eval(left.as<Int>(), right.as<Int>());
      return true;
   } 
   return false;
}


template<class Op>
bool SemanticAnalyzer::visit_sumprod(Value left, Value _right, string what) {
   Value right = left.type()->convert(_right);
   if (right.is_null()) {
      _curr_node->add_error(_T("No se puede %s un '%s' con un '%s'.",
                               what.c_str(),
                               left.type()->typestr().c_str(),
                               _right.type()->typestr().c_str()));
      _curr = left.type()->create_abstract();
      return true; // assume the type of the left operand for the rest...
   }
   if (left.is<Int>()) {
      _curr = checked_op<Op, Int>(left, right);
      return true;
   }
   if (left.is<Float>()) {
      _curr = checked_op<Op, Float>(left, right);
      return true;
   }
   if (left.is<Double>()) {
      _curr = checked_op<Op, Double>(left, right);
      return true;
   }
   return false;
}

template<class Op>
bool SemanticAnalyzer::visit_bitop(Value left, Value right) {
   if (left.is<Int>() and right.is<Int>()) {
      _curr = Value(Op::eval(left.as<Int>(), right.as<Int>()));
      return true;
   }
   return false;
}

template<class Op>
bool SemanticAnalyzer::visit_comparison(Value left, Value right) {
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


// Analyze

void SemanticAnalyzer::Analyze(Ast *ast) {
   switch (ast->type()) {
   case AstType::Program: {
      Program *X = cast<Program>(ast);
      _curr_node = X;
      prepare_global_environment();
      for (Ast *n : X->nodes) {
         Analyze(n);
      }
      break;
   }
   case AstType::Macro:
      break;
   case AstType::Using: {
      Using *X = cast<Using>(ast);
      _curr_node = X;
      if (!using_namespace(X->namespc)) {
         X->add_error(_T("El \"namespace\" '%s' no existe.", 
                         X->namespc.c_str()));
      }
      break;
   }
   case AstType::Include: {
      Include *X = cast<Include>(ast);
      _curr_node = X;
      if (!include_header_file(X->filename)) {
         X->add_error(_T("El fichero de cabecera '%s' no existe.", 
                         X->filename.c_str()));
      }
      break;
   }
   case AstType::FuncDecl: {
      FuncDecl *X = cast<FuncDecl>(ast);
      _curr_node = X;
      string funcname = X->funcname();
      Type *return_type = get_type(X->return_typespec);  // return_type == 0 means 'void'
      Function *functype = new Function(return_type);

      // reverse use of '_ret' to check all return statements
      if (return_type) {
         _ret = return_type->create_abstract(); 
      } else {
         _ret = Value::null;
      }
    
      pushenv(X->funcname());
      for (int i = 0; i < X->params.size(); i++) {
         auto p = X->params[i];
         Value v;
         if (getenv(p->name, v)) {
            X->add_error(p->ini, p->fin, _T("El parámetro %d está repetido.", i+1));
         }
         Type *param_type = get_type(p->typespec);
         if (param_type == 0) {
            X->add_error(p->ini, p->fin, _T("El tipo '%s' no existe.", 
                                            p->typespec->typestr().c_str()));
            // TODO: Maybe register some parameter type?
         } else {
            functype->add_params(param_type);
            setenv(p->name, param_type->create_abstract(), Param);
         }
      }
      Analyze(X->block);
      popenv();
     
      Value func = functype->mkvalue(new UserFunc(funcname, X));
      Value callable = Callable::self->mkvalue(Value::null, func); // bind with 'null'
      setenv(funcname, callable, Hidden);
      break;
   }
   case AstType::BinaryExpr: {
      BinaryExpr *X = cast<BinaryExpr>(ast);
      _curr_node = X;

      // left
      Analyze(X->left);
      Value left = _curr;
      if (X->kind != Expr::Eqment) {
         left = Reference::deref(left);
         check_unknown(left, X->left, _curr_varname);
      }
      string left_varname = _curr_varname;

      // right
      Analyze(X->right);
      Value right = _curr;
      right = Reference::deref(right);
      string right_varname = _curr_varname;

      if (X->left->has_errors() or X->right->has_errors()) {
         return; // avoid more errors
      }
      
      // Try operator first
      _curr = left;
      if (call_operator(X->op, vector<Value>(1, right))) {
         return;
      }

      // operate
      if (X->op == ",") {
         return; // already evaluated
      }
      if (X->op == "=") {
         if (!X->left->has_errors()) {
            visit_binaryexpr_assignment(X, left, right);
         }
         return;
      }
      if (X->op == "+=" || X->op == "-=" || X->op == "*=" || X->op == "/=" ||
          X->op == "&=" || X->op == "|=" || X->op == "^=") {
         check_unknown(right, X->right, right_varname);
         _curr_node = X; // ugly
         visit_binaryexpr_op_assignment(X->op[0], left, right);
         return;
      } 
      else if (X->op == "&" || X->op == "|" || X->op == "^") {
         check_unknown(right, X->right, right_varname);
         bool ok = false;
         switch (X->op[0]) {
         case '&': ok = visit_bitop<_And>(left, right); break;
         case '|': ok = visit_bitop<_Or >(left, right); break;
         case '^': ok = visit_bitop<_Xor>(left, right); break;
         }
         if (ok) {
            return;
         }
         if (call_operator(X->op, vector<Value>(1, right))) {
            return;
         }
         if (!left.is<Int>()) {
            X->left->add_error(_T("La parte izquierda del '%s' no es un 'int'.", 
                                  X->op.c_str()));
         }
         if (!right.is<Int>()) {
            X->left->add_error(_T("La parte derecha del '%s' no es un 'int'.", 
                                  X->op.c_str()));
         }
         return;
      }
      else if (X->op == "+" || X->op == "*" || X->op == "-" || X->op == "/") {
         check_unknown(right, X->right, right_varname);
         bool ret = false;
         _curr_node = X;
         if (left.type()->is(Type::Basic) and right.type()->is(Type::Basic)) {
            switch (X->op[0]) {
            case '+': {
               if (left.is<Char>() and right.is<Int>()) {
                  _curr = Value(char(left.as<Char>() + right.as<Int>()));
                  return;
               } else {
                  ret = visit_sumprod<_Add>(left, right, "sumar"); break;
               }
            }
            case '*': ret = visit_sumprod<_Mul>(left, right, "multiplicar"); break;
            case '-': ret = visit_sumprod<_Sub>(left, right, "restar"); break;
            case '/': ret = visit_sumprod<_Div>(left, right, "dividir"); break;
            }
         } else {
            _curr = left;
            if (!call_operator(X->op, vector<Value>(1, right))) {
               X->add_error(_T("El tipo '%s' no tiene operador '%s'.", 
                               _curr.type()->typestr().c_str(), 
                               X->op.c_str()));
            }
            ret = true;
         }
         if (ret) {
            return;
         }
         X->add_error(_T("Los operandos de '%s' son incompatibles", X->op.c_str()));
         return;
      }
      else if (X->op == "%") {
         if (left.is<Int>() and right.is<Int>()) {
            _curr = Value(left.as<Int>() % right.as<Int>());
            return;
         }
         X->add_error(_T("El módulo '%' debe usarse con dos 'int's."));
         return;
      }
      else if (X->op == "%=") {
         if (!left.is<Reference>()) {
            // _error(_T("Para usar '%s' se debe poner una variable a la izquierda", X->op.c_str()));
         }
         left = Reference::deref(left);
         if (left.is<Int>() and right.is<Int>()) {
            left.as<Int>() %= right.as<Int>();
            return;
         }
         X->add_error(_T("Los operandos de '%s' son incompatibles", "%="));
         return;
      }
      else if (X->op == "&&" or X->op == "and" || X->op == "||" || X->op == "or")  {
         if (left.is<Bool>() and right.is<Bool>()) {
            _curr = Value(X->op == "&&" or X->op == "and" 
                          ? left.as<Bool>() and right.as<Bool>()
                          : left.as<Bool>() or  right.as<Bool>());
            return;
         }
         X->add_error(_T("Los operandos de '%s' no son de tipo 'bool'", X->op.c_str()));
         return;
      }
      else if (X->op == "==" || X->op == "!=") {
         if (left.same_type_as(right)) {
            _curr = Value(X->op == "==" ? left.equals(right) : !left.equals(right));
            return;
         }
         X->add_error(_T("Los operandos de '%s' no son del mismo tipo", X->op.c_str()));
         return;
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
         }
         if (ret) {
            return;
         }
         X->add_error(_T("Los operandos de '%s' no son compatibles", X->op.c_str()));
         return;
      }
      X->add_error(_T("No existe el operador '%s' para el tipo '%s'.", 
                      X->op.c_str(), left.type()->typestr().c_str()));
      break;
   }
   case AstType::StructDecl: {
      StructDecl *X = cast<StructDecl>(ast);
      _curr_node = X;
      // Create a new Struct type now
      Struct *type = new Struct(X->struct_name());
      for (int i = 0; i < X->decls.size(); i++) {
         DeclStmt& decl = *X->decls[i];
         Type *field_type = get_type(decl.typespec);
         if (field_type == 0) {
            decl.add_error(_T("El tipo '%s' no existe.", 
                              decl.typespec->typestr().c_str()));
            field_type = new UnknownType(decl.typespec->typestr());
         }
         for (DeclStmt::Item& item : decl.items) {
            if (type->has_field(item.decl->name)) {
               decl.add_error(_T("El campo '%s' está repetido.", item.decl->name.c_str()));
            }
            if (item.decl->is<ArrayDecl>()) {
               ArrayDecl *array_decl = dynamic_cast<ArrayDecl*>(item.decl);
               vector<int> sizes;
               for (Expr *size_expr : array_decl->sizes) {
                  Analyze(size_expr);
                  if (_curr.is_abstract()) {
                     array_decl->add_error(_T("El tamaño de una tabla en un 'struct' debe ser una constante."));
                  } else if (!_curr.is<Int>()) {
                     array_decl->add_error(_T("El tamaño de una tabla no puede ser un '%s'.", 
                                              _curr.type()->typestr().c_str()));
                  } else {
                     sizes.push_back(_curr.as<Int>());
                  }
               }
               Type *arraytype = Array::mkarray(field_type, sizes);
               type->add_field(item.decl->name, arraytype); 
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
      _curr_node = X;
      Value v;
      
      // Try a namespace
      SimpleIdent *namespc_or_class = X->get_potential_namespace_or_class();
      if (namespc_or_class != 0) {
         Environment *namespc = get_namespace(namespc_or_class->name);
         if (namespc != 0) {
            if (namespc->get(X->name, v)) {
               goto found;
            }
            X->add_error(_T("No se ha encontrado '%s' en el namespace '%s'.", 
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
            X->add_error(_T("No se ha encontrado '%s' en la clase '%s'.",
                            X->name.c_str(), namespc_or_class->name.c_str()));
         }
         goto found;
      }

      // Try the environment
      if (getenv(X->name, v)) {
         _curr_varname = X->name;
         goto found;
      } else {
         X->add_error(_T("No se ha declarado '%s'.", X->name.c_str()));
         Type *type = new UnknownType(X->name.c_str());
         _curr = type->create_abstract();

         // TODO: Pistas para 'cout', 'cin', 'endl', 'string', etc.
      }
      return;

   found:
      _curr = (v.is<Reference>() ? v : Reference::mkref(v));
      break;
   }
   case AstType::Literal: {
      Literal *X = cast<Literal>(ast);
      _curr_node = X;
      switch (X->kind) {
      case Literal::String: _curr = Value(*X->val.as_string.s); break;
      case Literal::Int:    _curr = Value(X->val.as_int);       break;
      case Literal::Double: _curr = Value(X->val.as_double);    break;
      case Literal::Float:  _curr = Value(X->val.as_double);    break;
      case Literal::Bool:   _curr = Value(X->val.as_bool);      break;
      case Literal::Char:   _curr = Value(X->val.as_char);      break;
      default:
         X->add_error(_T("SemanticAnalyzer::visit_literal: UNIMPLEMENTED"));
      }
      break;      
   }
   case AstType::Block: {
      Block *X = cast<Block>(ast);
      _curr_node = X;
      for (Stmt *stmt : X->stmts) {
         Analyze(stmt);
      }
      break;
   }
   case AstType::VarDecl: {
      VarDecl *X = cast<VarDecl>(ast);
      _curr_node = X;
      Value prev, init = Reference::deref(_curr);
      if (getenv(X->name, prev)) {
         if (has_flag(X->name, Param)) {
            X->add_error(_T("Ya existe un parámetro con nombre '%s'.", X->name.c_str()));
         } else {
            X->add_error(_T("La variable '%s' ya está declarada antes.", X->name.c_str()));
         }
         return;
      }
      Type *type = get_type(X->typespec);
      if (type == 0) {
         string typestr = X->typespec->typestr(); 
         type = new UnknownType(typestr);
      } 
      
      if (init.is_null()) {
         if (X->typespec->is(TypeSpec::Const)) {
            X->add_error(_T("Las constantes deben tener un valor inicial."));
         }
         init = type->create();
      } else {
         try {
            Value init2 = type->convert(init);
            if (init2.is_null()) {
               X->add_error(_T("El tipo del valor inicial ('%s') no se "
                               "corresponde con el tipo de la variable ('%s').",
                               init.type()->typestr().c_str(),
                               type->typestr().c_str()));
            } else {
               init = init2;
            }
         } catch (TypeError *e) {
            X->add_error(e->msg);
         }
         if (X->typespec->is(TypeSpec::Const) and
             type->is<Struct>() and
             init.contains_unknowns()) {
            X->add_error(_T("En una tupla constante hay que inicializar todas las casillas."));
         }
      }
      if (X->typespec->is(TypeSpec::Const)) {
         init.set_const(true);
      }
      setenv(X->name, init);
      break;      
   }
   case AstType::ArrayDecl: {
      ArrayDecl *X = cast<ArrayDecl>(ast);
      _curr_node = X;
      Value init = _curr;
      vector<int> sizes;
      for (int i = 0; i < X->sizes.size(); i++) {
         Analyze(X->sizes[i]);
         _curr = Reference::deref(_curr);
         if (!_curr.is<Int>()) {
            X->add_error(_T("El tamaño de una tabla debe ser un entero."));
            return;
         } else if (_curr.as<Int>() <= 0) {
            X->add_error(_T("El tamaño de una tabla debe ser un entero positivo."));
            return;
         } else {
            const int sz = _curr.as<Int>();
            sizes.push_back(sz);
         }
      }
      
      Type *celltype = get_type(X->typespec);
      if (celltype == 0) {
         X->add_error(_T("El tipo '%s' no existe", 
                         X->typespec->typestr().c_str()));
         celltype = UnknownType::self;
      }
      // FIXME: don't create new Array type every time?
      Type *arraytype = Array::mkarray(celltype, sizes);
      if (init.is_null()) {
         if (X->typespec->is(TypeSpec::Const)) {
            X->add_error(_T("Las tablas constantes deben tener un valor inicial."));
         }
         init = arraytype->create();
      } else {
         try {
            init = arraytype->convert(init);
         } catch (TypeError& e) {
            X->add_error(e.msg);
            init = arraytype->create();
         }
         if (X->typespec->is(TypeSpec::Const)) {
            if (init.contains_unknowns()) {
               X->add_error(_T("En una tabla constante hay que inicializar todas las casillas."));
            }
         }
      }
      setenv(X->name, init);
      break;
   }
   case AstType::ObjDecl: {
      ObjDecl *X = cast<ObjDecl>(ast);
      _curr_node = X;
      Type *type = get_type(X->typespec);
      if (type != 0) {
         vector<Value> argvals;
         vector<Expr*> args;
         eval_arguments(X->args, argvals);
         string constructor_name = type->name();
         Value new_obj = type->create_abstract();
         if (!bind_field(new_obj, constructor_name)) {
            X->add_error(_T("El tipo '%s' no tiene constructor", type->typestr().c_str()));
         }
         if (_curr.is<Overloaded>()) {
            _curr = _curr.as<Overloaded>().resolve(argvals);
            assert(_curr.is<Callable>());
         }
         Binding& constructor = _curr.as<Callable>();
         const Function *func_type = constructor.func.type()->as<Function>();
         check_arguments(func_type, argvals, &X->args);
         constructor.call_abstract(X, argvals);
         setenv(X->name, new_obj);
         return;
      }
      X->add_error(_T("The type '%s' is not implemented in MiniCC", 
                      X->typespec->typestr().c_str()));
      break;
   }
   case AstType::DeclStmt: {
      DeclStmt *X = cast<DeclStmt>(ast);
      _curr_node = X;
      Type *type = get_type(X->typespec);
      if (type == 0) {
         string typestr = X->typespec->typestr();
         X->add_error(_T("El tipo '%s' no existe.", typestr.c_str()));
      }
      for (DeclStmt::Item& item : X->items) {
         if (item.init) {
            Analyze(item.init);
         } else {
            _curr = Value::null;
         }
         Analyze(item.decl);
      }
      break;
   }
   case AstType::ExprStmt: {
      ExprStmt *X = cast<ExprStmt>(ast);
      _curr_node = X;
      if (X->expr) {
         Analyze(X->expr);
      }
      if (X->is_return) {
         if (X->expr == 0) {
            if (!_ret.is_null()) {
               X->add_error(_T("La función debe devolver un '%s'.", 
                               _ret.type()->typestr().c_str()));
            }
         } else {
            if (!_curr.same_type_as(_ret)) {
               if (_curr.is<Reference>() and !_ret.is<Reference>()) {
                  _curr = Reference::deref(_curr);
                  if (_ret.same_type_as(_curr)) {
                     return;
                  }
               }
               string Tcurr = _curr.type()->typestr();
               if (_ret.is_null()) {
                  X->add_error(_T("Se devuelve un '%s' cuando no se debería devolver ningún valor.",
                                  Tcurr.c_str()));
               } else {
                  string Tret  = _ret.type()->typestr();
                  X->add_error(_T("Se devuelve un '%s' cuando debería ser un '%s'.",
                                  Tcurr.c_str(), Tret.c_str()));
               }
            }
         }
      }
      break;
   }
   case AstType::IfStmt: {
      IfStmt *X = cast<IfStmt>(ast);
      _curr_node = X;
      check_condition(X->cond, "if");
      Analyze(X->then);
      if (X->els) {
         Analyze(X->els);
      }
      break;
   }
   case AstType::ForStmt: {
      ForStmt *X = cast<ForStmt>(ast);
      _curr_node = X;
      pushenv("");
      if (X->init) {
         Analyze(X->init);
      }
      check_condition(X->cond, "for");
      Analyze(X->substmt);
      if (X->post) {
         Analyze(X->post);
      }
      popenv();
      break;
   }
   case AstType::WhileStmt: {
      WhileStmt *X = cast<WhileStmt>(ast);
      _curr_node = X;
      pushenv("");
      check_condition(X->cond, "while");
      Analyze(X->substmt);
      popenv();
      break;
   }
   case AstType::CallExpr: {
      CallExpr *X = cast<CallExpr>(ast);
      _curr_node = X;
      vector<Value> argvals;
      eval_arguments(X->args, argvals);
      if (visit_type_conversion(X, argvals)) {
         return;
      }
      visit_callexpr_getfunc(X);
      if (_curr.is<Callable>()) {
         // TODO: Find operator() (method or function)
         Value func = _curr;
         if (func.is<Overloaded>()) {
            func = func.as<Overloaded>().resolve(argvals);
            assert(func.is<Callable>());
         }
         Binding& fn = func.as<Callable>();
         const Function *func_type = fn.func.type()->as<Function>();
         _curr_node = X; // ugly
         check_arguments(func_type, argvals, &X->args);
         const Type *return_type = func_type->return_type();
         if (return_type != 0) {
            _curr = _ret = return_type->create_abstract();
         } else {
            _curr = _ret = Value::null;
         }
      } 
      else {
         ostringstream oss;
         oss << "CallExpr(" << X << ")";
         Type *rettype = new UnknownType(oss.str());
         _curr = rettype->create_abstract();
      }
      break;
   }
   case AstType::IndexExpr: {
      IndexExpr *X = cast<IndexExpr>(ast);
      _curr_node = X;
      Analyze(X->base);
      Value base = Reference::deref(_curr);
      if (X->index) {
         Analyze(X->index);
      }
      Value index = Reference::deref(_curr);
      if (base.is<Array>()) {
         int i = -1;
         vector<Value>& array = base.as<Array>();
         if (index.is<Int>()) {
            if (!index.is_abstract()) {
               i = index.as<Int>();
               if (i < 0 || i >= array.size()) {
                  X->add_error(_T("El índice está fuera de los límites de la tabla (entre 0 y %d).", 
                                  array.size()-1));
                  i = -1;
               }
            }
         } else {
            X->add_error(_T("El índice debe ser un entero."));
         }
         if (base.is_abstract()) {
            _curr = Reference::mkref(array[0]); // abstract arrays have exactly one abstract element
         } else {
            if (i < 0 || i >= array.size()) {
               Type *celltype = static_cast<const Array*>(base.type())->celltype();
               _curr = Type::mkref(celltype)->create_abstract();
            } else {
               _curr = Reference::mkref(array[i]);
            }
         }
         return;
      }
      _curr = base;
      if (!call_operator("[]", vector<Value>(1, index))) {
         X->add_error(_T("Los corchetes deben usarse sobre tablas o vectores."));
      }
      break;
   }
   case AstType::FieldExpr: {
      FieldExpr *X = cast<FieldExpr>(ast);
      _curr_node = X;
      Analyze(X->base);
      _curr = Reference::deref(_curr);
      if (X->pointer) {
         if (!call_operator("*")) {
            X->add_error(_T("El tipo '%s' no tiene 'operator*'", 
                            _curr.type()->typestr().c_str()));
         }
         _curr = Reference::deref(_curr);
      }
      Value obj = _curr;
      if (obj.is<Struct>()) {
         // FIXME: Move this to 'get_field' in 'Struct' class???
         SimpleTable<Value>& fields = obj.as<Struct>();
         Value v;
         if (!fields.get(X->field->name, v)) {
            X->add_error(_T("El campo '%s' no existe.", X->field->name.c_str()));
         } else {
            _curr = Reference::mkref(v);
         }
         return;
      }
      
      if (!bind_field(obj, X->field->name)) {
         if (obj.type()->is(Type::Class)) {
            Ast *parent = X->parent;
            const char *msg;
            if (parent->is<CallExpr>()) {
               msg = "La clase '%s' no tiene método '%s'.";
            } else {
               msg = "La clase '%s' no tiene campo '%s'.";
            }
            X->add_error(_T(msg, 
                            obj.type()->typestr().c_str(), 
                            X->field->name.c_str()));
         } else {
            X->add_error(_T("El tipo '%s' no tiene el campo '%s'", 
                            obj.type()->typestr().c_str(),
                            X->field->name.c_str()));
         }
         _curr = UnknownType::self->create_abstract();
      }
      break;
   }
   case AstType::CondExpr: {
      CondExpr *X = cast<CondExpr>(ast);
      _curr_node = X;
      Analyze(X->cond);
      Value cond = Reference::deref(_curr);
      if (!cond.is<Bool>()) {
         X->cond->add_error(_T("Debe haber un 'bool' antes del interrogante."));
      } else {
         if (!cond.is_abstract() and !cond.is_unknown()) {
            X->add_error(_T("La condición siempre es '%s'.", 
                            (cond.as<Bool>() ? "true" : "false")));
         }
      }
      if (X->then) {
         Analyze(X->then);
      }
      Value _then = _curr;
      if (X->els) {
         Analyze(X->els);
      }
      Value _els = _curr;
      if (!_then.same_type_as(_els)) {
         X->add_error(_T("Los tipos de las dos expresiones alternativas deben coincidir (son '%s' y '%s').",
                         _then.type()->typestr().c_str(),
                         _els.type()->typestr().c_str()));
      }
      break;
   }
   case AstType::ExprList: {
      ExprList *X = cast<ExprList>(ast);
      _curr_node = X;
      Value v = VectorValue::make();
      vector<Value>& vals = v.as<VectorValue>();
      for (Expr *e : X->exprs) {
         Analyze(e);
         vals.push_back(_curr);
      }
      _curr = v;
      break;
   }
   case AstType::SignExpr: {
      SignExpr *X = cast<SignExpr>(ast);
      _curr_node = X;
      Analyze(X->expr);
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
         X->add_error(_T("El cambio de signo para '%s' no tiene sentido.",
                         _curr.type_name().c_str()));
      }
      break;
   }
   case AstType::IncrExpr: {
      IncrExpr *X = cast<IncrExpr>(ast);
      _curr_node = X;
      Analyze(X->expr);
      if (!_curr.is<Reference>()) {
         X->add_error(_T("Hay que incrementar una variable, no un valor."));
      }
      Value after  = Reference::deref(_curr);
      Value before = after.clone();
      if (after.is<Int>()) {
         if (after.is_unknown()) {
            X->add_error(_T("Incrementas la variable '%s' sin haberla inicializado.",
                            _curr_varname.c_str()));
         } else if (!after.is_abstract()) {
            if (X->kind == IncrExpr::Positive) {
               after.as<Int>()++;
            } else {
               after.as<Int>()--;
            }
         }
      } else {
         _curr = after;
         string op = (X->kind == IncrExpr::Positive ? "++" : "--");
         if (!call_operator(op)) {
            X->add_error(_T("El tipo '%s' no tiene operador '%s'.", 
                            _curr.type()->typestr().c_str(), op.c_str()));
         }
      }
      _curr = (X->preincr ? before : after);
      break;
   }
   case AstType::NegExpr: {
      NegExpr *X = cast<NegExpr>(ast);
      _curr_node = X;
      Analyze(X->expr);
      if (!_curr.is<Bool>()) {
         X->add_error(_T("Sólo se puede negar una expresión de tipo 'bool'."));
         _curr = Bool::self->create_abstract(); // avoid errors downstream
         return;
      }
      _curr.as<Bool>() = !_curr.as<Bool>();
      break;      
   }
   case AstType::TypedefDecl: {
      TypedefDecl *X = cast<TypedefDecl>(ast);
      _curr_node = X;
      string name = X->decl->name;
      Type *type = get_type(X->decl->typespec);
      assert(type != 0);
      if (X->decl->is<VarDecl>()) {
         const VarDecl *var = X->decl->as<VarDecl>();
         register_type(var->name, type);
      } else if (X->decl->is<ArrayDecl>()) {
         const ArrayDecl *array = X->decl->as<ArrayDecl>();
         Analyze(array->sizes[0]);
         if (!_curr.is<Int>()) {
            X->add_error(_T("El tamaño de un array debería ser un entero"));
         }
         const int size = _curr.as<Int>();
         register_type(array->name, new Array(type, size));
      }
      break;
   }
   case AstType::DerefExpr: {
      DerefExpr *X = cast<DerefExpr>(ast);
      _curr_node = X;
      // TODO: deal with pointers
      Analyze(X->expr);
      _curr = Reference::deref(_curr);
      /*
      if (!call_operator("*")) {
         X->add_error(_T("El tipo '%s' no tiene 'operator*'", 
                         _curr.type()->typestr().c_str()));
      }
      */
      break;
   }
   }
}

void AnalyzeSemantics(Ast *ast) {
   SemanticAnalyzer().Analyze(ast);
}
