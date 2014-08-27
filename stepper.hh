#ifndef STEPPER_HH
#define STEPPER_HH

#include <assert.h>
#include <iostream>
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
      virtual Range span() const   = 0;
   };

                 std::string _status;
   std::stack<StepperState*> _stack;
   std::vector<std::string>  _errors;
                 EvalError*  _err;

          void _e() const { assert(!_stack.empty()); }
          void prepare_funcall(FuncDecl *, std::vector<Value*>&);
          void _error(std::string msg) { _errors.push_back(msg); }

          void eval(AstNode *x) { x->visit(&I); }
public:
               Stepper(std::istream *i, std::ostream *o) 
                  : AstVisitor(o), I(i, o), _err(0) {}
              ~Stepper() {}

   std::string status()           const { return _status; }
          void status(std::string s)    { _status = s; }
   std::string error()            const { return (_err ? _err->msg : ""); }

          void start(Program *p)        { visit_program(p); }

          void push(StepperState *s)    {       _stack.push(s);           }
          void replace(StepperState *s) { _e(); _stack.top() = s;         }
          void pop()                    { _e(); _stack.pop();             }
          bool finished()         const { return _stack.empty(); }
         Range span() const             { _e(); return _stack.top()->span(); }
          bool step();
   std::string env2json()         const { return I.env2json(); }


          void visit_program(Program *x);
          void visit_block(Block *x);
          void visit_binaryexpr(BinaryExpr *x); 
          void visit_increxpr(IncrExpr *x); 
          void visit_exprstmt(ExprStmt *x); 
          void visit_declstmt(DeclStmt *x);
          void visit_ifstmt(IfStmt *x);
          void visit_iterstmt(IterStmt *x);
          void visit_callexpr(CallExpr *x); 
          void visit_literal(Literal *x); 
          void visit_fieldexpr(FieldExpr *x); 
          void visit_ident(Ident *x); 
  

   template<typename X>
   struct VisitState : public StepperState { // Just to mark the current node (no eval)
      X *x;
      VisitState(X *_x) : x(_x) {}
      Todo step(Stepper *S);
      Range span() const { return x->span(); }
   };

   struct ProgramVisitState : public VisitState<FuncDecl> {
      ProgramVisitState(FuncDecl *x) : VisitState<FuncDecl>(x) {}
      Todo  step(Stepper *S);
   };

   struct BlockVisitState : public VisitState<Block> {
      int curr;
      BlockVisitState(Block *x) : VisitState<Block>(x), curr(0) {}
      Todo step(Stepper *S);      
   };

   struct AssignmentVisitState : public VisitState<BinaryExpr> {
      Value *right;
      AssignmentVisitState(BinaryExpr *x) : VisitState<BinaryExpr>(x), right(0) {}
      Todo step(Stepper *S);
      Range span() const;
   };
   
   struct IfVisitState : public VisitState<IfStmt> {
      IfVisitState(IfStmt *x) : VisitState<IfStmt>(x) {}
      Todo step(Stepper *S);
   };

   struct ForVisitState : public VisitState<IterStmt> {
      enum Location { Init, Cond, Block, Post };
      Location state;
      ForVisitState(IterStmt *x) : VisitState<IterStmt>(x), state(Init) {}
      Todo step(Stepper *S);
   };

   struct WhileVisitState : public VisitState<IterStmt> {
      enum Location { Cond, Block };
      Location state;
      WhileVisitState(IterStmt *x) : VisitState<IterStmt>(x), state(Cond) {}
      Todo step(Stepper *S);
   };

   struct CallExprVisitState : public VisitState<CallExpr> {
      int curr;
      FuncDecl *fn;
      std::vector<Value*> args;
      CallExprVisitState(CallExpr *x, FuncDecl *f) 
         : VisitState<CallExpr>(x), fn(f), curr(0) {
         args.resize(x->args.size());
      }

      Todo step(Stepper *S);
   };

   struct WriteExprVisitState : public VisitState<BinaryExpr> {
      int curr;
      std::list<Expr*> exprs;
      WriteExprVisitState(BinaryExpr *x) : VisitState<BinaryExpr>(x), curr(0) {}
      Todo step(Stepper *S);
   };

};



#endif
