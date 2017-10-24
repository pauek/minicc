#ifndef INTERPRETER_HH
#define INTERPRETER_HH

#include <iostream>
#include <vector>
#include "ast.hh"
#include "value.hh"
#include "types.hh"
#include "readwriter.hh"

struct EvalError : public Error {
   EvalError(std::string _msg) : Error(_msg) {}
};

class Interpreter : public ReadWriter, WithEnvironment {
    Value _curr, _ret;

    void _error(std::string msg) { throw new EvalError(msg); }

   Value new_value_from_structdecl(StructDecl *x);

   void  invoke_func_prepare_arg(FuncDecl *x, Value args, int i);
   void  invoke_func_prepare(FuncDecl *x, const std::vector<Value>& args);

   void  visit_program_prepare(Program *x);
   void  visit_program_find_main();
   void  visit_binaryexpr_assignment(Value left, Value right);
   void  visit_binaryexpr_op_assignment(char, Value left, Value right);
   void  visit_callexpr_getfunc(CallExpr *x);
   bool  visit_type_conversion(CallExpr *x, const std::vector<Value>& args);
   void  visit_callexpr_call(Value func, const std::vector<Value>& args);

   void invoke_user_func(FuncDecl *decl, const std::vector<Value>& args);

   template<class Op> bool visit_op_assignment(Value left, Value right);
   template<class Op> bool visit_bitop_assignment(Value left, Value right);
   template<class Op> bool visit_sumprod(Value left, Value right);
   template<class Op> bool visit_bitop(Value left, Value right);
   template<class Op> bool visit_comparison(Value left, Value right);

   friend class Stepper;

   void eval_arguments(const std::vector<Expr*>& exprs, std::vector<Value>& args);
   void check_arguments(const Function *func_type, const std::vector<Value>& args);
   void check_result(Binding& fn, const Function *func_type);
   bool bind_field(Value obj, string method_name);
   bool call_operator(std::string op, const std::vector<Value>& args = std::vector<Value>());

public:
   Interpreter(std::istream *i, std::ostream *o)
      : ReadWriter(i, o), WithEnvironment(i, o) {}

   void Eval(Ast *ast);

   friend class UserFunc;
};

struct UserFunc : public Func {
   FuncDecl *decl;
   Interpreter *I;

   UserFunc(std::string n, FuncDecl *d, Interpreter *_I) 
      : Func(n), decl(d), I(_I) {}

   Value call(Value self, const std::vector<Value>& args) {
      I->invoke_user_func(decl, args);
      return I->_ret;
   }
};

void Eval(Ast *ast, std::istream& in, std::ostream& out);

#endif
