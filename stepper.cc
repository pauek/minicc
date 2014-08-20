
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

template<typename X>
string status_for(X *x) {
   return "UNIMPLEMENTED";
}

template<typename X>
Todo Stepper::VisitState<X>::step(Stepper *S) {
   x->visit(&S->I);
   string s = status_for<X>(x); 
   if (s != "UNIMPLEMENTED") {
      S->status(s);
   }
   S->pop();
   delete this;
   return Next; 
}

void Stepper::visit_program(Program *x) {
   I.visit_program_prepare(x);
   FuncDecl *main = I.visit_program_find_main();
   if (main == 0) {
      _error("La funcion 'main' no existe");
   }
   I.invoke_func_prepare(main, vector<Value*>());
   push(new ProgramVisitState(main));
   main->block->visit(this);
   status("Saltamos a la función 'main'.");
}

Todo Stepper::ProgramVisitState::step(Stepper *S) {
   S->I.popenv();
   S->pop();
   delete this;
   return Next;
}

void Stepper::visit_block(Block *x) {
   push(new BlockVisitState(x));
   x->stmts[0]->visit(this);
}

Todo Stepper::BlockVisitState::step(Stepper *S) {
   const int last = x->stmts.size()-1;
   ++curr;
   if (curr > last) {
      S->pop();
      delete this;
      return Next;
   }
   x->stmts[curr]->visit(S);
   return Stop;
}

void Stepper::visit_ifstmt(IfStmt *x) {
   push(new IfVisitState(x));
   x->cond->visit(this);
}

Todo Stepper::IfVisitState::step(Stepper *S) {
   Value *c = S->I._curr;
   if (c->kind != Value::Bool) {
      S->_error("La condición de un 'if' debe ser un valor de tipo 'bool'.");
   }
   S->pop();
   Todo todo = Stop;
   if (c->val.as_bool) {
      S->status("La condición vale 'true', tomamos la primera rama.");
      x->then->visit(S);
   } else {
      if (x->els != 0) {
         S->status("La condición vale 'false', tomamos la segunda rama.");
         x->els->visit(S);
      } else {
         S->status("La condición vale 'false', continuamos.");
         todo = Next;
      }
   }
   delete this;
   return todo;
}

void Stepper::visit_declstmt(DeclStmt *x) {
   push(new VisitState<DeclStmt>(x));
}

template<>
string status_for<DeclStmt>(DeclStmt *x) {
   ostringstream S;
   S << "Se declara" << (x->decls.size() > 1 ? "n " : " ");
   string plural = (x->decls.size() > 1 ? "s" : "");
   S << "la" << plural << " variable" << plural << " ";
   for (int i = 0; i < x->decls.size(); i++) {
      if (i > 0) {
         if (i == x->decls.size() - 1) {
            S << " y ";
         } else {
            S << ", ";
         }
      }
      S << "'" << x->decls[i]->name << "'";
   }
   S << ".";
   return S.str();
}

void Stepper::visit_exprstmt(ExprStmt *x) {
   push(new VisitState<ExprStmt>(x));
}

template<>
string status_for<ExprStmt>(ExprStmt *x) {
   BinaryExpr *e = dynamic_cast<BinaryExpr*>(x->expr);
   if (e != 0) {
      if (e->is_write_expr()) {
         return "Se escribe a la salida.";
      }
      if (e->is_read_expr()) {
         return "Se lee de la entrada.";
      }
   }
   return "UNIMPLEMENTED";
}

void Stepper::visit_binaryexpr(BinaryExpr *x) {
   push(new VisitState<BinaryExpr>(x));
}

void Stepper::visit_increxpr(IncrExpr *x) {
   push(new VisitState<IncrExpr>(x));
}

template<>
string status_for<IncrExpr>(IncrExpr *x) {
   Ident *id = dynamic_cast<Ident*>(x->expr);
   if (id != 0) {
      return "Se incrementa la variable '" + id->id + "'.";
   }
   return "UNIMPLEMENTED";
}


void Stepper::visit_iterstmt(IterStmt *x) {
   if (x->is_for()) {
      push(new ForVisitState(x));
      x->init->visit(this);
   } else {
      push(new WhileVisitState(x));
      x->cond->visit(this);
   }
}

Todo Stepper::ForVisitState::step(Stepper *S) {
   switch (state) {
   case Stepper::ForVisitState::Init: {
      x->cond->visit(S);
      state = Stepper::ForVisitState::Cond;
      return Stop;
   }
   case Stepper::ForVisitState::Cond: {
      Value *cond = S->I._curr;
      if (cond->kind != Value::Bool) {
         S->_error("La condición de un 'for' debe ser un valor de tipo 'bool'");
      }
      if (!cond->val.as_bool) {
         S->status("La condición vale 'false', salimos del for.");
         S->pop();
         delete this;
         return Next;
      } else {
         S->status("La condición vale 'true', entramos en el for.");
         x->substmt->visit(S);
         state = Stepper::ForVisitState::Block;
         return Stop;
      }
   }
   case Stepper::ForVisitState::Block: {
      x->post->visit(S);
      state = Stepper::ForVisitState::Post;
      return Stop;
   }
   case Stepper::ForVisitState::Post: {
      x->cond->visit(S);
      state = Stepper::ForVisitState::Cond;
      return Stop;
   }
   }
}

Todo Stepper::WhileVisitState::step(Stepper *S) {
   switch (state) {
   case Stepper::WhileVisitState::Cond: {
      Value *cond = S->I._curr;
      if (cond->kind != Value::Bool) {
         S->_error("La condición de un 'while' debe ser un valor de tipo 'bool'");
      }
      if (!cond->val.as_bool) {
         S->pop();
         delete this;
         return Next;
      } else {
         x->substmt->visit(S);
         state = Stepper::WhileVisitState::Block;
         return Stop;
      }
   }
   case Stepper::WhileVisitState::Block:
      x->cond->visit(S);
      state = Stepper::WhileVisitState::Cond;
      return Stop;
   }
   
}


