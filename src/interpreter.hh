#ifndef INTERPRETER_HH
#define INTERPRETER_HH

#include <iostream>
#include <vector>
#include "ast.hh"
#include "value.hh"
#include "types.hh"

struct EvalError : public Error {
   EvalError(std::string _msg) : Error(_msg) {}
};

class Interpreter : WithEnvironment {
    Value _curr, _ret;

    void _error(std::string msg) { throw new EvalError(msg); }

   // Value NewValueFromStructDecl(StructDecl *x);

   void  invoke_func_prepare_arg(FuncDecl *x, Value args, int i);
   void  invoke_func_prepare(FuncDecl *x, const std::vector<Value>& args);

   void  ProgramPrepare(Program *x);
   void  FindMain();
   void  EvalBinaryExprAssignment(Value left, Value right);
   void  EvalBinaryExprOpAssignment(char, Value left, Value right);
   void  GetFunc(CallExpr *x);
   bool  TypeConversion(CallExpr *x, const std::vector<Value>& args);
   void  Call(Value func, const std::vector<Value>& args);

   void InvokeUserFunc(FuncDecl *decl, const std::vector<Value>& args);

   template<class Op> bool EvalOpAssignment(Value left, Value right);
   template<class Op> bool EvalBitopAssignment(Value left, Value right);
   template<class Op> bool EvalSumProd(Value left, Value right);
   template<class Op> bool EvalBitop(Value left, Value right);
   template<class Op> bool EvalComparison(Value left, Value right);

   friend class Stepper;

   void EvalArguments(const std::vector<Expr*>& exprs, std::vector<Value>& args);
   void CheckArguments(const Function *func_type, const std::vector<Value>& args);
   void CheckResult(Binding& fn, const Function *func_type);
   bool BindField(Value obj, string method_name);
   bool CallOperator(std::string op, const std::vector<Value>& args = std::vector<Value>());

public:
   Interpreter(std::istream *i, std::ostream *o)
      : WithEnvironment(i, o) {}

   void Eval(Ast *ast);

   friend class UserFunc;
};

struct UserFunc : public Func {
   FuncDecl *decl;
   Interpreter *I;

   UserFunc(std::string n, FuncDecl *d, Interpreter *_I) 
      : Func(n), decl(d), I(_I) {}

   Value call(Value self, const std::vector<Value>& args) {
      I->InvokeUserFunc(decl, args);
      return I->_ret;
   }
};

void Eval(Ast *ast, std::istream& in, std::ostream& out);

#endif
