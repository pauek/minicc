#ifndef STEPPER_HH
#define STEPPER_HH

#include <assert.h>
#include <iostream>
#include <sstream>
#include <vector>
#include <list>
#include <stack>
#include <map>
#include "interpreter.hh"

enum Todo { Stop, Next };

class Stepper : public AstVisitor {

   Interpreter I;

   struct StepperState {
      virtual Todo  step(Stepper*) = 0;
      virtual  Span span() const   = 0;
   };

                 std::string _status;
   std::stack<StepperState*> _stack;
   std::vector<std::string>  _errors;
                 EvalError*  _err;

          void _e() const { assert(!_stack.empty()); }
          void prepare_funcall(FuncDecl *, std::vector<Value>&);
          void _error(std::string msg) { _errors.push_back(msg); }

          void eval(Ast *x) { x->accept(&I); }

          void generic_visit(Ast *x);
          void visit_assignment(BinaryExpr *x);

   std::stringstream _out;
   std::stringstream _in;
public:
   Stepper() : _err(0) { 
      I.set_in(&_in), I.set_out(&_out); 
   }

   Stepper(std::istream *i, std::ostream* o) 
      : I(i, o), _err(0) {}

              ~Stepper() {}

          void status(std::string s)    { _status = s; }
   std::string status()           const { return _status; }
     EvalError *error()           const { return _err; }
 
          void start(Program *p)        { visit_program(p); }

          void push(StepperState *s)    {       _stack.push(s);              }
          void replace(StepperState *s) { _e(); _stack.top() = s;            }
          void pop()                    { _e(); _stack.pop();                }
          bool finished()         const {       return _stack.empty();       }
          Span span()             const { _e(); return _stack.top()->span(); }
          bool step();
   std::string state2json()       const;

   std::string output();

          void visit_program(Program *x);
          void visit_block(Block *x);
          void visit_binaryexpr(BinaryExpr *x); 
          void visit_increxpr(IncrExpr *x); 
          void visit_exprstmt(ExprStmt *x); 
          void visit_declstmt(DeclStmt *x);
          void visit_ifstmt(IfStmt *x);
          void visit_forstmt(ForStmt *x);
          void visit_whilestmt(WhileStmt *x);
          void visit_callexpr(CallExpr *x); 
          void visit_literal(Literal *x); 
          void visit_fieldexpr(FieldExpr *x); 
          void visit_fullident(FullIdent *x);
          void visit_indexexpr(IndexExpr *x);

   struct PopState : public StepperState {
      Span spn;
      PopState(Span _span) : spn(_span) {}
      Todo step(Stepper *S);
      Span span() const { return spn; }
   };

   struct ProgramVisitState : public StepperState {
      FuncDecl *x;
      enum At { Begin, End, Finished }; 
      At at;
      ProgramVisitState(FuncDecl *_x) : x(_x), at(Begin) {}
      Todo  step(Stepper *S);
      Span span() const;
   };

   struct BlockVisitState : public StepperState {
      Block *x;
      int curr;
      BlockVisitState(Block *_x) : x(_x), curr(0) {}
      Todo step(Stepper *S);      
      Span span() const { return x->span; }
   };

   struct EqmentVisitState : public StepperState {
      BinaryExpr *x;
      Value left, right;
      EqmentVisitState(BinaryExpr *_x, Value r) : x(_x), right(r) {}
      Todo step(Stepper *S);
      Span span() const;
   };
   
   struct IfVisitState : public StepperState {
      Span spn;
      Stmt *next;
      IfVisitState(Span _span, Stmt *_next) : spn(_span), next(_next) {}
      Todo step(Stepper *S);
      Span span() const { return spn; }
   };

   struct ForVisitState : public StepperState {
      enum At { Cond, Block, Post, Leave };
      At at;
      ForStmt *x;
      bool cond;
      ForVisitState(ForStmt *_x) : x(_x), at(Cond) {}
      Todo step(Stepper *S);
      Span span() const { return x->span; }
   };

   struct WhileVisitState : public StepperState {
      enum At { Cond, Block, Leave };
      At at;
      WhileStmt *x;
      WhileVisitState(WhileStmt *_x) : x(_x), at(Cond) {}
      Todo step(Stepper *S);
      Span span() const { return x->cond->span; }
   };

   struct CallExprVisitState : public StepperState {
      int curr;
      FuncDecl *fn;
      CallExpr *x;
      std::vector<Value> args;
      CallExprVisitState(CallExpr *_x, FuncDecl *f) : x(_x), fn(f), curr(0) {
         args.resize(x->args.size());
      }
      Todo step(Stepper *S);
      Span span() const;

      static const int Return, Block;
   };

   struct WriteExprVisitState : public StepperState {
      Expr *curr;
      bool waiting;
      std::list<Expr*> exprs;
      BinaryExpr *x;
      WriteExprVisitState(BinaryExpr *_x) : x(_x), curr(0), waiting(false) {}
      Todo step(Stepper *S); 
      Todo step_waiting(Stepper *S);
      Span span() const;
   };

};



#endif
