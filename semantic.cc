#include "ast.hh"
#include "translator.hh"
#include "interpreter.hh"
#include "semantic.hh"
using namespace std;

void SemanticAnalyzer::visit_program(Program* x) {
   prepare_global_environment();
   for (AstNode *n : x->nodes) {
      n->accept(this);
   }
}

void SemanticAnalyzer::visit_comment(CommentSeq* cn) {}
void SemanticAnalyzer::visit_macro(Macro* x) {}

void SemanticAnalyzer::visit_using(Using* x) {
   if (!using_namespace(x->namespc)) {
      x->add_error(_T("El \"namespace\" '%s' no existe.", x->namespc.c_str()));
   }
}

void SemanticAnalyzer::visit_include(Include* x) {
   if (!include_header_file(x->filename)) {
      x->add_error(_T("El fichero de cabecera '%s' no existe.", x->filename.c_str()));
   }
}

void SemanticAnalyzer::visit_funcdecl(FuncDecl *x) {
   string funcname = x->funcname();
   Type *return_type = get_type(x->return_typespec);  // return_type == 0 means 'void'
   Function *functype = new Function(return_type);

   // reverse use of '_ret' to check all return statements
   if (return_type) {
      _ret = return_type->create_abstract(); 
   } else {
      _ret = Value::null;
   }
 
   pushenv(x->funcname());
   for (int i = 0; i < x->params.size(); i++) {
      auto p = x->params[i];
      Value v;
      if (getenv(p->name, v)) {
         x->add_error(p->ini, p->fin, _T("El parámetro %d está repetido.", i+1));
      }
      Type *param_type = get_type(p->typespec);
      if (param_type == 0) {
         x->add_error(p->ini, p->fin, _T("El tipo '%s' no existe.", 
                                         p->typespec->typestr().c_str()));
         // TODO: Maybe register some parameter type?
      } else {
         functype->add_params(param_type);
         setenv(p->name, param_type->create_abstract(), Param);
      }
   }
   x->block->accept(this);
   popenv();
  
   Value func = functype->mkvalue(new UserFunc(funcname, x));
   Value callable = Callable::self->mkvalue(Value::null, func); // bind with 'null'
   setenv(funcname, callable, Hidden);
}

void SemanticAnalyzer::visit_structdecl(StructDecl *x) {
   // Create a new Struct type now
   Struct *type = new Struct(x->struct_name());
   for (int i = 0; i < x->decls.size(); i++) {
      DeclStmt& decl = *x->decls[i];
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
               size_expr->accept(this);
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
   register_type(x->struct_name(), type);
}

void SemanticAnalyzer::visit_fullident(FullIdent *x) {
   Value v;
   
   // Try a namespace
   SimpleIdent *namespc_or_class = x->get_potential_namespace_or_class();
   if (namespc_or_class != 0) {
      Environment *namespc = get_namespace(namespc_or_class->name);
      if (namespc != 0) {
         if (namespc->get(x->name, v)) {
            goto found;
         }
         x->add_error(_T("No se ha encontrado '%s' en el namespace '%s'.", 
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
         x->add_error(_T("No se ha encontrado '%s' en la clase '%s'.",
                         x->name.c_str(), namespc_or_class->name.c_str()));
      }
      goto found;
   }

   // Try the environment
   if (getenv(x->name, v)) {
      goto found;
   } else {
      x->add_error(_T("No se ha declarado '%s'.", x->name.c_str()));
      Type *type = new UnknownType(x->name.c_str());
      _curr = type->create_abstract();

      // TODO: Pistas para 'cout', 'cin', 'endl', 'string', etc.
   }
   return;

 found:
   _curr = (v.is<Reference>() ? v : Reference::mkref(v));
}

void SemanticAnalyzer::visit_literal(Literal *x) {
   switch (x->type) {
   case Literal::String: _curr = Value(*x->val.as_string.s); break;
   case Literal::Int:    _curr = Value(x->val.as_int);       break;
   case Literal::Double: _curr = Value(x->val.as_double);    break;
   case Literal::Bool:   _curr = Value(x->val.as_bool);      break;
   case Literal::Char:   _curr = Value((*x->val.as_string.s)[0] /* FIXME */);
      break;
   default:
      x->add_error(_T("SemanticAnalyzer::visit_literal: UNIMPLEMENTED"));
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
bool SemanticAnalyzer::visit_op_assignment(Value left, Value _right) {
   Value right = left.type()->convert(_right);
   if (left.is<Int>() and right.is<Int>()) {
      Op::eval(left.as<Int>(), right.as<Int>());
      return true;
   } 
   if (left.is<Float>() and right.is<Float>()) {
      Op::eval(left.as<Float>(), right.as<Float>());
      return true;
   }
   if (left.is<Double>() and right.is<Double>()) {
      Op::eval(left.as<Double>(), right.as<Double>());
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
bool SemanticAnalyzer::visit_sumprod(Value left, Value _right) {
   Value right = left.type()->convert(_right);
   if (left.is<Int>()) {
      if (left.is_abstract() or right.is_abstract()) {
         _curr = Int::self->create_abstract();
      } else {
         _curr = Value(Op::eval(left.as<Int>(), right.as<Int>()));
      }
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

void SemanticAnalyzer::visit_binaryexpr(BinaryExpr *x) {
   x->left->accept(this);
   Value left = _curr;
   if (x->kind != Expr::Assignment) {
      left = Reference::deref(left);
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
      // _error(_T("Los operandos de '%s' son incompatibles", x->op.c_str()));
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
         /*
         if (!call_operator(x->op, vector<Value>(1, right))) {
            _error(_T("El tipo '%s' no tiene 'operator%s'", 
                      _curr.type()->typestr().c_str(), x->op.c_str()));
         }
         */
         ret = true;
      }
      if (ret) {
         return;
      }
      // _error(_T("Los operandos de '%s' son incompatibles", x->op.c_str()));
   }
   else if (x->op == "%") {
      if (left.is<Int>() and right.is<Int>()) {
         _curr = Value(left.as<Int>() % right.as<Int>());
         return;
      }
      // _error(_T("Los operandos de '%s' son incompatibles", "%"));
   }
   else if (x->op == "%=") {
      if (!left.is<Reference>()) {
         // _error(_T("Para usar '%s' se debe poner una variable a la izquierda", x->op.c_str()));
      }
      left = Reference::deref(left);
      if (left.is<Int>() and right.is<Int>()) {
         left.as<Int>() %= right.as<Int>();
         return;
      }
      // _error(_T("Los operandos de '%s' son incompatibles", "%="));
   }
   else if (x->op == "&&" or x->op == "and" || x->op == "||" || x->op == "or")  {
      if (left.is<Bool>() and right.is<Bool>()) {
         _curr = Value(x->op == "&&" or x->op == "and" 
                       ? left.as<Bool>() and right.as<Bool>()
                       : left.as<Bool>() or  right.as<Bool>());
         return;
      }
      // _error(_T("Los operandos de '%s' no son de tipo 'bool'", x->op.c_str()));
   }
   else if (x->op == "==" || x->op == "!=") {
      if (left.same_type_as(right)) {
         _curr = Value(x->op == "==" ? left.equals(right) : !left.equals(right));
         return;
      }
      // _error(_T("Los operandos de '%s' no son del mismo tipo", x->op.c_str()));
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
         /*
         if (!call_operator(x->op, vector<Value>(1, right))) {
            _error(_T("El tipo '%s' no tiene 'operator%s'", 
                      _curr.type()->typestr().c_str(), x->op.c_str()));
         }
         */
         ret = true;
      }
      if (ret) {
         return;
      }
      // TODO: Find operator as method or function
      // _error(_T("Los operandos de '%s' no son compatibles", x->op.c_str()));
   }
   _curr = left;
   /*
   if (call_operator(x->op, vector<Value>(1, right))) {
      return;
   }
   */
   // _error(_T("SemanticAnalyzer::visit_binaryexpr: UNIMPLEMENTED (%s)", x->op.c_str()));
}

inline bool assignment_types_ok(const Value& a, const Value& b) {
   return 
      (a.same_type_as(b)) or
      (a.is<Float>() and b.is<Double>()) or
      (a.is<Double>() and b.is<Float>());
}

void SemanticAnalyzer::visit_binaryexpr_assignment(Value left, Value right) {
   if (!left.is<Reference>()) {
      // _error(_T("Intentas asignar sobre algo que no es una variable"));
   }
   left = Reference::deref(left);
   right = left.type()->convert(right);
   if (right == Value::null) {
      // TODO: Find operator as method or function
      /*
      _error(_T("La asignación no se puede hacer porque los "
                "tipos no son compatibles (%s) vs (%s)", 
                left.type_name().c_str(), 
                right.type_name().c_str()));
      */
   }
   if (!left.assign(right)) {
      /*
      _error(_T("La asignación no se puede hacer porque los "
                "tipos no son compatibles (%s) vs (%s)", 
                left.type_name().c_str(), 
                right.type_name().c_str()));
      */
   }
   _curr = left;
}

void SemanticAnalyzer::visit_binaryexpr_op_assignment(char op, Value left, Value right) {
   if (!left.is<Reference>()) {
      // _error(_T("Para usar '%s=' se debe poner una variable a la izquierda", op));
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
      // _error(_T("Los operandos de '%s' no son compatibles", _op.c_str()));
   }
}

void SemanticAnalyzer::visit_block(Block *x) {
   for (Stmt *stmt : x->stmts) {
      stmt->accept(this);
   }
}

void SemanticAnalyzer::visit_vardecl(VarDecl *x) {
   Value prev, init = Reference::deref(_curr);
   if (getenv(x->name, prev)) {
      if (has_flag(x->name, Param)) {
         x->add_error(_T("Ya existe un parámetro con nombre '%s'.", x->name.c_str()));
      } else {
         x->add_error(_T("La variable '%s' ya está declarada antes.", x->name.c_str()));
      }
      return;
   }
   Type *type = get_type(x->typespec);
   if (type == 0) {
      string typestr = x->typespec->typestr(); 
      type = new UnknownType(typestr);
   } 
   
   if (init.is_null()) {
      init = type->create_abstract();
   } else {
      try {
         Value init2 = type->convert(init);
         if (init2.is_null()) {
            x->add_error(_T("El tipo del valor inicial ('%s') no se "
                            "corresponde con el tipo de la variable ('%s').",
                            init.type()->typestr().c_str(),
                            type->typestr().c_str()));
         }
      } catch (TypeError *e) {
         x->add_error(e->msg);
      }
   }
   setenv(x->name, init);
}

void SemanticAnalyzer::visit_arraydecl(ArrayDecl *x) {
   Value init = _curr;
   vector<int> sizes;
   for (int i = 0; i < x->sizes.size(); i++) {
      x->sizes[i]->accept(this);
      _curr = Reference::deref(_curr);
      if (!_curr.is<Int>()) {
         x->add_error(_T("El tamaño de una tabla debe ser un entero"));
         return;
      } else if (_curr.as<Int>() <= 0) {
         x->add_error(_T("El tamaño de una tabla debe ser un entero positivo"));
         return;
      } else {
         const int sz = _curr.as<Int>();
         sizes.push_back(sz);
      }
   }
   
   Type *celltype = get_type(x->typespec);
   if (celltype == 0) {
      x->add_error(_T("El tipo '%s' no existe", 
                      x->typespec->typestr().c_str()));
      celltype = UnknownType::self;
   }
   // FIXME: don't create new Array type every time?
   Type *arraytype = Array::mkarray(celltype, sizes);
   if (init.is_null()) {
      init = arraytype->create();
   } else {
      try {
         init = arraytype->convert(init);
      } catch (TypeError& e) {
         x->add_error(e.msg);
      }
   }
   setenv(x->name, init);
}
   
void SemanticAnalyzer::visit_objdecl(ObjDecl *x) {
   Type *type = get_type(x->typespec);
   if (type != 0) {
      vector<Value> args;
      // eval_arguments(x->args, args);
      
      string constructor_name = type->name();
      Value new_obj = type->create();
      /*
      if (!bind_field(new_obj, constructor_name)) {
         _error(_T("El tipo '%s' no tiene constructor", type->typestr().c_str()));
      }
      */
      if (_curr.is<Overloaded>()) {
         _curr = _curr.as<Overloaded>().resolve(args);
         assert(_curr.is<Callable>());
      }
      Binding& constructor = _curr.as<Callable>();
      const Function *func_type = constructor.func.type()->as<Function>();
      // check_arguments(func_type, args);
      // constructor.call(this, args); // <-- Invoke!
      
      setenv(x->name, new_obj);
      return;
   }
   /*
   _error(_T("The type '%s' is not implemented in MiniCC", 
             x->typespec->typestr().c_str()));
   */
}

void SemanticAnalyzer::visit_declstmt(DeclStmt* x) {
   Type *type = get_type(x->typespec);
   if (type == 0) {
      string typestr = x->typespec->typestr();
      x->add_error(_T("El tipo '%s' no existe.", typestr.c_str()));
   }
   for (DeclStmt::Item& item : x->items) {
      if (item.init) {
         item.init->accept(this);
      } else {
         _curr = Value::null;
      }
      item.decl->accept(this);
   }
}

void SemanticAnalyzer::visit_exprstmt(ExprStmt* x) {
   if (x->expr) {
      x->expr->accept(this);
   }
   if (x->is_return) {
      if (x->expr == 0) {
         if (!_ret.is_null()) {
            x->add_error(_T("La función debe devolver un '%s'.", 
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
               x->add_error(_T("Se devuelve un '%s' cuando no se debería devolver ningún valor.",
                               Tcurr.c_str()));
            } else {
               string Tret  = _ret.type()->typestr();
               x->add_error(_T("Se devuelve un '%s' cuando debería ser un '%s'.",
                               Tcurr.c_str(), Tret.c_str()));
            }
         }
      }
   }
}

void SemanticAnalyzer::visit_ifstmt(IfStmt *x) {
   x->cond->accept(this);
   _curr = Reference::deref(_curr);
   if (!_curr.is<Bool>()) {
      // TODO: if (!call_operator("bool")) { ... }
      x->cond->add_error(_T("An if's condition needs to be a bool value"));
   }
   x->then->accept(this);
   if (x->els) {
      x->els->accept(this);
   }
}

void SemanticAnalyzer::visit_forstmt(ForStmt *x) {
   pushenv("");
   if (x->init) {
      x->init->accept(this);
   }
   x->cond->accept(this);
   if (!_curr.is<Bool>()) {
      /*
        if (!call_operator("bool")) {      
        _error(_T("La condición de un '%s' debe ser un valor de tipo bool.",
        (x->is_for() ? "for" : "while")));
        }
      */
   }
   x->substmt->accept(this);
   if (x->post) {
      x->post->accept(this);
   }
   popenv();
}

void SemanticAnalyzer::visit_whilestmt(WhileStmt *x) {
   pushenv("");
   x->cond->accept(this);
   if (!_curr.is<Bool>()) {
      /*
        if (!call_operator("bool")) {      
        _error(_T("La condición de un '%s' debe ser un valor de tipo bool.",
        (x->is_for() ? "for" : "while")));
        }
      */
   }
   x->substmt->accept(this);
   popenv();
}


void SemanticAnalyzer::visit_callexpr_getfunc(CallExpr *x) {
   x->func->accept(this);
   if (_curr.is<UnknownType>()) {
      return;
   }
   _curr = Reference::deref(_curr);
   if (!_curr.is<Callable>() and !_curr.is<Overloaded>()) {
      x->add_error(_T("Intentas llamar como función un valor de tipo '%s'.", 
                      _curr.type()->typestr().c_str()));
   }
}

bool SemanticAnalyzer::visit_type_conversion(CallExpr *x, const vector<Value>& args) {
   FullIdent *id = x->func->as<FullIdent>();
   if (id != 0) {
      TypeSpec spec(id);
      Type *type = get_type(&spec);
      if (type != 0) {
         if (args.size() != 1) {
            x->add_error(_T("La conversión de tipo recibe un solo argumento"));
         }
         _curr = type->convert(args[0]);
         if (_curr == Value::null) {
            _curr = args[0];
            // call_operator(id->typestr());
         }
         return true;
      }
   }
   return false;
}

void SemanticAnalyzer::check_arguments(CallExpr *x, const Function *func_type, 
                                       const vector<Value>& args) 
{
   if (func_type->num_params() != args.size()) {
      x->add_error(_T("Número de argumentos erróneo (son %d y deberían ser %d).",
                      args.size(), func_type->num_params()));
      return;
   }
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
         // _error(_T("En el parámetro %d se requiere una variable.", i+1));
      }
      string t2 = arg_i.type()->typestr();
      if (t1 != t2) {
         //_error(_T("El argumento %d no es compatible con el tipo del parámetro "
         //          "(%s vs %s)", i+1, t1.c_str(), t2.c_str()));
      }
   }
}

void SemanticAnalyzer::visit_callexpr(CallExpr *x) {
   // eval arguments
   vector<Value> argvals;
   for (int i = 0; i < x->args.size(); i++) {
      x->args[i]->accept(this);
      argvals.push_back(_curr);
   }
   if (visit_type_conversion(x, argvals)) {
      return;
   }
   visit_callexpr_getfunc(x);
   if (_curr.is<Callable>()) {
      // TODO: Find operator() (method or function)
      Value func = _curr;
      if (func.is<Overloaded>()) {
         func = func.as<Overloaded>().resolve(argvals);
         assert(func.is<Callable>());
      }
      Binding& fn = func.as<Callable>();
      const Function *func_type = fn.func.type()->as<Function>();
      check_arguments(x, func_type, argvals);
      Type *return_type = func_type->return_type();
      if (return_type != 0) {
         _curr = _ret = return_type->create_abstract();
      } else {
         _curr = _ret = Value::null;
      }
   } 
   else {
      ostringstream oss;
      oss << "CallExpr(" << x << ")";
      Type *rettype = new UnknownType(oss.str());
      _curr = rettype->create_abstract();
   }
}

void SemanticAnalyzer::visit_indexexpr(IndexExpr *x) {
   x->base->accept(this);
   Value base = Reference::deref(_curr);
   x->index->accept(this);
   Value index = Reference::deref(_curr);
   if (base.is<Array>()) {
      if (!index.is<Int>()) {
         // _error(_T("El índice en un acceso a tabla debe ser un entero"));
      }
      vector<Value>& vals = base.as<Array>();
      const int i = index.as<Int>();
      if (i < 0 || i >= vals.size()) {
         // TODO: Producir error de ejecución
         // _error(_T("La casilla %d no existe", i));
      }
      _curr = Reference::mkref(vals[i]);
      return;
   }
   _curr = base;
   /*
   if (!call_operator("[]", vector<Value>(1, index))) {
      _error(_T("Las expresiones de índice deben usarse sobre tablas o vectores"));
   }
   */
}

void SemanticAnalyzer::visit_fieldexpr(FieldExpr *x) {
   x->base->accept(this);
   _curr = Reference::deref(_curr);
   if (x->pointer) {
      assert(false);
      /*
      if (!call_operator("*")) {
         _error(_T("El tipo '%s' no tiene 'operator*'", 
                   _curr.type()->typestr().c_str()));
      }
      */
      _curr = Reference::deref(_curr);
   }
   Value obj = _curr;
   if (obj.is<Struct>()) {
      // FIXME: Move this to 'get_field' in 'Struct' class???
      SimpleTable<Value>& fields = obj.as<Struct>();
      Value v;
      if (!fields.get(x->field->name, v)) {
         x->add_error(_T("No existe el campo '%s'", x->field->name.c_str()));
      }
      _curr = Reference::mkref(v);
      return;
   }
   
   if (!bind_field(obj, x->field->name)) {
      if (obj.type()->is(Type::Class)) {
         AstNode *parent = x->parent;
         const char *msg;
         if (parent->is<CallExpr>()) {
            msg = "La clase '%s' no tiene método '%s'.";
         } else {
            msg = "La clase '%s' no tiene campo '%s'.";
         }
         x->add_error(_T(msg, 
                         obj.type()->typestr().c_str(), 
                         x->field->name.c_str()));
      } else {
         x->add_error(_T("El tipo '%s' no tiene el campo '%s'", 
                         obj.type()->typestr().c_str(),
                         x->field->name.c_str()));
      }
      _curr = UnknownType::self->create_abstract();
   }
}

void SemanticAnalyzer::visit_condexpr(CondExpr *x) {
   x->cond->accept(this);
   if (!_curr.is<Bool>()) {
      /*
      _error(_T("Una expresión condicional debe tener valor "
                "de tipo 'bool' antes del interrogante"));
      */
   }
   if (_curr.as<Bool>()) {
      x->then->accept(this);
   } else {
      if (x->els != 0) {
         x->els->accept(this);
      }
   }
}

void SemanticAnalyzer::visit_exprlist(ExprList *x) {
   Value v = VectorValue::make();
   vector<Value>& vals = v.as<VectorValue>();
   for (Expr *e : x->exprs) {
      e->accept(this);
      vals.push_back(_curr);
   }
   _curr = v;
}

void SemanticAnalyzer::visit_signexpr(SignExpr *x) {
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
      /*
      _error(_T("El cambio de signo para '%s' no tiene sentido",
                _curr.type_name().c_str()));
      */
   }
}

void SemanticAnalyzer::visit_increxpr(IncrExpr *x) {
   x->expr->accept(this);
   if (!_curr.is<Reference>()) {
      // _error(_T("Hay que incrementar una variable, no un valor"));
   }
   Value after  = Reference::deref(_curr);
   Value before = after.clone();
   if (after.is<Int>()) {
      if (x->kind == IncrExpr::Positive) {
         after.as<Int>()++;
      } else {
         after.as<Int>()--;
      }
   } else {
      _curr = after;
      string op = (x->kind == IncrExpr::Positive ? "++" : "--");
      /*
      if (!call_operator(op)) {
         _error(_T("El tipo '%s' no tiene 'operator%s'", 
                   _curr.type()->typestr().c_str(), op.c_str()));
      }
      */
   }
   _curr = (x->preincr ? before : after);
}

void SemanticAnalyzer::visit_negexpr(NegExpr *x) {
   x->expr->accept(this);
   if (!_curr.is<Bool>()) {
      // _error(_T("Para negar una expresión ésta debe ser de tipo 'bool'"));
   }
   _curr.as<Bool>() = !_curr.as<Bool>();
}

void SemanticAnalyzer::visit_typedefdecl(TypedefDecl *x) {
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
         // _error(_T("El tamaño de un array debería ser un entero"));
      }
      const int size = _curr.as<Int>();
      register_type(array->name, new Array(type, size));
   }
}

void SemanticAnalyzer::visit_derefexpr(DerefExpr *x) {
   // TODO: deal with pointers
   x->expr->accept(this);
   _curr = Reference::deref(_curr);
   /*
   if (!call_operator("*")) {
      _error(_T("El tipo '%s' no tiene 'operator*'", 
                _curr.type()->typestr().c_str()));
   }
   */
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
