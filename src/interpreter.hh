#ifndef INTERPRETER_HH
#define INTERPRETER_HH

#include <iostream>
#include <vector>
#include "ast.hh"
#include "error.hh"
#include "types.hh"
#include "value.hh"

class Interpreter : WithEnvironment {
    Value _curr, _ret;

    void _error(std::string msg) { throw EvalError(msg); }

    void invoke_func_prepare_arg(FuncDecl *x, Value args, int i);
    void invoke_func_prepare(FuncDecl *x, const std::vector<Value>& args);
    void program_prepare(Program *x);
    void find_main();
    void eval_binary_expr_assignment(Value left, Value right);
    void eval_binary_expr_op_assignment(char, Value left, Value right);
    void get_func(CallExpr *x);
    bool type_conversion(CallExpr *x, const std::vector<Value>& args);
    void call(Value func, const std::vector<Value>& args);
    void invoke_user_func(FuncDecl *decl, const std::vector<Value>& args);
    template <class Op>
    bool eval_op_assignment(Value left, Value right);
    template <class Op>
    bool eval_bitop_assignment(Value left, Value right);
    template <class Op>
    bool eval_sum_prod(Value left, Value right);
    template <class Op>
    bool eval_bitop(Value left, Value right);
    template <class Op>
    bool eval_comparison(Value left, Value right);
    friend class Stepper;
    void eval_arguments(const std::vector<Expr *>& exprs, std::vector<Value>& args);
    void check_arguments(const Function *func_type, const std::vector<Value>& args);
    void check_result(Binding& fn, const Function *func_type);
    bool bind_field(Value obj, string method_name);
    bool call_operator(std::string op, const std::vector<Value>& args = std::vector<Value>());

   public:
    Interpreter(std::istream *i, std::ostream *o) : WithEnvironment(i, o) {}

    void eval(AstNode *ast);
    friend class UserFunc;
};

struct UserFunc : public Func {
    FuncDecl    *decl;
    Interpreter *I;

    UserFunc(std::string n, FuncDecl *d, Interpreter *_I) : Func(n), decl(d), I(_I) {}

    Value call(Value self, const std::vector<Value>& args) {
        I->invoke_user_func(decl, args);
        return I->_ret;
    }
};

void eval(AstNode *ast, std::istream& in, std::ostream& out);

#endif
