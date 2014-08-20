
#include <vector>
#include <sstream>
#include "stepper.hh"
using namespace std;

void Stepper::step() {
   Todo t = Next;
   while (!finished() and t == Next) {
      t = _stack.top()->step(this);
   }
}

void Stepper::visit_program(Program *x) {
   _env.clear();
   _env.resize(1);

   setenv("endl", new Value("\n"));
   setenv("cout", &Value::cout);
   setenv("cin",  &Value::cin);

   for (AstNode *n : x->nodes) {
      n->visit(this);
   }
   auto it = _funcs.find("main");
   if (it == _funcs.end()) {
      _error("La funcion 'main' no existe");
   }
   vector<Value*> main_args;
   pushenv();
   prepare_funcall(it->second, main_args);
   push(new ProgramVisitState(it->second));
   it->second->block->visit(this);
}

Todo Stepper::ProgramVisitState::step(Stepper *S) {
   S->popenv();
   S->status("El programa ha terminado.");
   S->pop();
   delete this;
   return Next;
}

void Stepper::prepare_funcall(FuncDecl *fn, vector<Value*>& args) {
   if (fn->params.size() != args.size()) {
      _error("Error en el número de argumentos al llamar a '" + fn->name + "'");
   }
   for (int i = 0; i < args.size(); i++) {
      if (args[i]->kind == Value::Ref) {
         if (fn->params[i]->type->reference) {
            setenv(fn->params[i]->name, args[i]);
         } else {
            Value *v = static_cast<Value*>(args[i]->val.as_ptr);
            setenv(fn->params[i]->name, v);
         }
      } else {
         if (fn->params[i]->type->reference) {
            ostringstream S;
            S << "En el parámetro " << i+1 << " se requiere una variable.";
            _error(S.str());
         }
         setenv(fn->params[i]->name, args[i]);
      }
   }
   _status = "Saltamos a la función '" + fn->name + "'.";
}

void Stepper::visit_block(Block *x) {
   push(new BlockVisitState(x));
}

Todo Stepper::BlockVisitState::step(Stepper *S) {
   const int last = x->stmts.size()-1;
   if (curr == last) {
      S->pop();
   }
   x->stmts[curr]->visit(S);
   curr++;
   if (curr <= last) {
      return Stop;
   } else {
      delete this;
      return Next;
   }
}

void Stepper::visit_ifstmt(IfStmt *x) {
   push(new IfVisitState(x));
}

Todo Stepper::IfVisitState::step(Stepper *S) {
   if (c == 0) {
      x->cond->visit(S);
      c = S->_curr;
      return Stop;
   } else {
      if (c->kind != Value::Bool) {
         S->_error("La condición de un 'if' debe ser un valor de tipo 'bool'.");
      }
      S->pop();
      if (c->val.as_bool) {
         S->status("La condición ha dado 'true', cogemos la primera rama.");
         x->then->visit(S);
         return Stop;
      } else {
         if (x->els != 0) {
            S->status("La condición ha dado 'false', cogemos la segunda rama.");
            x->els->visit(S);
            return Stop;
         } else {
            S->status("La condición ha dado 'false', continuamos.");
            delete this;
            return Next;
         }
      }
   }
}

