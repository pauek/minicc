#ifndef STEPPER_HH
#define STEPPER_HH

#include <assert.h>
#include <iostream>
#include <list>
#include <map>
#include <sstream>
#include <stack>
#include <vector>
#include "interpreter.hh"

enum Todo { Stop, Next };

class Stepper {
    Interpreter I;

    struct StepperState {
        virtual Todo step(Stepper *) = 0;
        virtual Span span() const = 0;
    };

    std::string                _status;
    std::stack<StepperState *> _stack;
    std::vector<std::string>   _errors;
    EvalError                 *_err;

    void _e() const { assert(!_stack.empty()); }

    void prepare_funcall(FuncDecl *, std::vector<Value>&);

    void _error(std::string msg) { _errors.push_back(msg); }

    void eval(AstNodeCore *X) { I.eval(X); }

    void              generic_visit(AstNodeCore *x);
    void              visit_assignment(BinaryExpr *x);
    std::stringstream _out;
    std::stringstream _in;

   public:
    Stepper() : _err(0), I(&_in, &_out) {}

    Stepper(std::istream *i, std::ostream *o) : I(i, o), _err(0) {}

    ~Stepper() {}

    void status(std::string s) { _status = s; }

    std::string status() const { return _status; }

    EvalError error() const { return *_err; }

    void start(Program *p) { visit_program(p); }

    void push(StepperState *s) { _stack.push(s); }

    void replace(StepperState *s) {
        _e();
        _stack.top() = s;
    }

    void pop() {
        _e();
        _stack.pop();
    }

    bool finished() const { return _stack.empty(); }

    Span span() const {
        _e();
        return _stack.top()->span();
    }

    bool        step();
    std::string state2json() const;
    std::string output();
    void        visit_program(Program *x);
    void        visit_block(Block *x);
    void        visit_binaryexpr(BinaryExpr *x);
    void        visit_increxpr(IncrExpr *x);
    void        visit_exprstmt(ExprStmt *x);
    void        visit_declstmt(DeclStmt *x);
    void        visit_ifstmt(IfStmt *x);
    void        visit_forstmt(ForStmt *x);
    void        visit_whilestmt(WhileStmt *x);
    void        visit_callexpr(CallExpr *x);
    void        visit_literal(Literal *x);
    void        visit_fieldexpr(FieldExpr *x);
    void        visit_fullident(Identifier *x);
    void        visit_indexexpr(IndexExpr *x);
    void        Step(AstNodeCore *);

    struct PopState : public StepperState {
        Span spn;

        PopState(Span _span) : spn(_span) {}

        Todo step(Stepper *S);

        Span span() const { return spn; }
    };

    struct ProgramVisitState : public StepperState {
        FuncDecl *X;

        enum At { Begin, End, Finished };

        At at;

        ProgramVisitState(FuncDecl *_X) : X(_X), at(Begin) {}

        Todo step(Stepper *S);
        Span span() const;
    };

    struct BlockVisitState : public StepperState {
        Block *X;
        int    curr;

        BlockVisitState(Block *_X) : X(_X), curr(0) {}

        Todo step(Stepper *S);

        Span span() const { return X->span; }
    };

    struct AssignVisitState : public StepperState {
        BinaryExpr *X;
        Value       left, right;

        AssignVisitState(BinaryExpr *_X, Value r) : X(_X), right(r) {}

        Todo step(Stepper *S);
        Span span() const;
    };

    struct IfVisitState : public StepperState {
        Span  spn;
        Stmt *next;

        IfVisitState(Span _span, Stmt *_next) : spn(_span), next(_next) {}

        Todo step(Stepper *S);

        Span span() const { return spn; }
    };

    struct ForVisitState : public StepperState {
        enum At { Cond, Block, Post, Leave };

        At       at;
        ForStmt *X;
        bool     cond;

        ForVisitState(ForStmt *_X) : X(_X), at(Cond) {}

        Todo step(Stepper *S);

        Span span() const { return X->span; }
    };

    struct WhileVisitState : public StepperState {
        enum At { Cond, Block, Leave };

        At         at;
        WhileStmt *X;

        WhileVisitState(WhileStmt *_X) : X(_X), at(Cond) {}

        Todo step(Stepper *S);

        Span span() const { return X->cond->span; }
    };

    struct CallExprVisitState : public StepperState {
        int                curr;
        FuncDecl          *fn;
        CallExpr          *X;
        std::vector<Value> args;

        CallExprVisitState(CallExpr *_X, FuncDecl *f) : X(_X), fn(f), curr(0) {
            args.resize(X->args.size());
        }

        Todo             step(Stepper *S);
        Span             span() const;
        static const int Return, Block;
    };

    struct WriteExprVisitState : public StepperState {
        Expr             *curr;
        bool              waiting;
        std::list<Expr *> exprs;
        BinaryExpr       *X;

        WriteExprVisitState(BinaryExpr *_X) : X(_X), curr(0), waiting(false) {}

        Todo step(Stepper *S);
        Todo step_waiting(Stepper *S);
        Span span() const;
    };
};

#endif
