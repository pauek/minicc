#ifndef STEPPER_HH
#define STEPPER_HH

#include <assert.h>
#include <iostream>
#include <vector>
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

          void _e() const { assert(!_stack.empty()); }
          void prepare_funcall(FuncDecl *, std::vector<Value*>&);
          void _error(std::string msg) { _errors.push_back(msg); }

          void eval(AstNode *x) { x->visit(&I); }
public:
               Stepper(std::istream *i, std::ostream *o) 
                  : AstVisitor(o), I(i, o) {}
              ~Stepper() {}

   std::string status()           const { return _status; }
          void status(std::string s)    { _status = s; }
          bool errors()           const { return !_errors.empty(); }

          void start(Program *p)        { visit_program(p); }

          void push(StepperState *s)    {       _stack.push(s);           }
          void replace(StepperState *s) { _e(); _stack.top() = s;         }
          void pop()                    { _e(); _stack.pop();             }
          bool finished()               { return _stack.empty(); }
         Range span() const             { _e(); return _stack.top()->span(); }
          void step();


          void visit_program(Program *x);
          void visit_block(Block *x);
          void visit_binaryexpr(BinaryExpr *x); 
          void visit_exprstmt(ExprStmt *x); 
          void visit_declstmt(DeclStmt *x);
          void visit_ifstmt(IfStmt *x);

   template<typename X>
   struct VisitState : public StepperState { // Just to mark the current node (no eval)
      X *x;
      VisitState(X *_x) : x(_x) {}
      Todo step(Stepper *S);
      Range span() const { return x->span(); }
   };

   struct ProgramVisitState : public StepperState {
      FuncDecl *main;
      ProgramVisitState(FuncDecl *_main) : main(_main) {}
      Todo  step(Stepper *S);
      Range span() const { return main->block->span(); }
   };

   struct BlockVisitState : public StepperState {
      Block *x;
      int curr;
      BlockVisitState(Block *_x) : x(_x), curr(0) {}
      Todo step(Stepper *S);      
      Range span() const { return x->stmts[curr]->span(); }
   };
   
   struct IfVisitState : public StepperState {
      IfStmt *x;
      IfVisitState(IfStmt *_x) : x(_x) {}
      Todo step(Stepper *S);
      Range span() const { return x->cond->span(); }
   };
};



#endif
