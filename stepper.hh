#ifndef STEPPER_HH
#define STEPPER_HH

#include <assert.h>
#include <iostream>
#include <vector>
#include <stack>
#include <map>
#include "interpreter.hh"

enum Todo { Stop, Next };

class Stepper : public Interpreter {

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
public:
               Stepper(std::istream *i, std::ostream *o) : Interpreter(i, o) {}
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
          void visit_ifstmt(IfStmt *x);
   

   struct ProgramVisitState : public StepperState {
      FuncDecl *main;
      ProgramVisitState(FuncDecl *_main) : main(_main) {}
      Todo  step(Stepper *stepper);
      Range span() const { return main->block->span(); }
   };

   struct BlockVisitState : public StepperState {
      Block *x;
      int curr;
      BlockVisitState(Block *_x) : x(_x), curr(0) {}
      Todo step(Stepper *stepper);      
      Range span() const { return x->stmts[curr]->span(); }
   };
   
   struct IfVisitState : public StepperState {
      IfStmt *x;
      Value *c; // Value obtained from the condition
      IfVisitState(IfStmt *_x) : x(_x), c(0) {}
      Todo step(Stepper *stepper);
      Range span() const { return x->cond->span(); }
   };
};



#endif
