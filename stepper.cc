
#include <vector>
#include <sstream>
#include "stepper.hh"
using namespace std;

bool Stepper::step() {
   _err = 0;
   try {
      Todo t = Next;
      while (!finished() and t == Next) {
         t = _stack.top()->step(this);
      }
      return true;
   }
   catch (EvalError* e) {
      _err = e;
      return false;
   }
}

template<typename X>
string status_for(X *x) { return x->describe(); }

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
         S->status("La condición vale 'false', salimos del while.");
         S->pop();
         delete this;
         return Next;
      } else {
         S->status("La condición vale 'true', entramos en el while.");
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

void Stepper::visit_declstmt(DeclStmt *x)     { push(new VisitState<DeclStmt>(x)); }
void Stepper::visit_increxpr(IncrExpr *x)     { push(new VisitState<IncrExpr>(x)); }
void Stepper::visit_binaryexpr(BinaryExpr *x) { push(new VisitState<BinaryExpr>(x)); }
void Stepper::visit_literal(Literal *x)       { push(new VisitState<Literal>(x)); }
void Stepper::visit_ident(Ident *x)           { push(new VisitState<Ident>(x)); }
void Stepper::visit_fieldexpr(FieldExpr *x)   { push(new VisitState<FieldExpr>(x)); }

void Stepper::visit_exprstmt(ExprStmt *x) { 
   if (x->expr->is_assignment()) {
      BinaryExpr *e = dynamic_cast<BinaryExpr*>(x->expr);
      assert(e != 0);
      push(new AssignmentVisitState(e));
      e->right->visit(this);
   } else if (x->expr->is_write_expr()) {
      BinaryExpr *e = dynamic_cast<BinaryExpr*>(x->expr);
      assert(e != 0);
      WriteExprVisitState *ws = new WriteExprVisitState(e);
      e->collect_rights(ws->exprs);
      push(ws);
      ws->exprs.front()->visit(this);
   } else {
      push(new VisitState<ExprStmt>(x));
   }
}

Todo Stepper::WriteExprVisitState::step(Stepper* S) {
   exprs.pop_front();
   if (!exprs.empty()) {
      exprs.front()->visit(S);
      S->status("Se escribe a la salida.");
      return Stop;
   } else {
      S->pop();
      delete this;
      return Next;
   }
}

Range Stepper::AssignmentVisitState::span() const {
   return Range(x->left->span().ini, x->right->span().ini);
}

Todo Stepper::AssignmentVisitState::step(Stepper *S) {
   if (right == 0) {
      right = S->I._curr;
      if (right->kind == Value::Ref) {
         right = right->ref();
      }
      ostringstream oss;
      oss << "La expresión ha dado " << *right << ".";
      S->status(oss.str());
      return Stop;
   } else {
      x->left->visit(&S->I);
      Value *left = S->I._curr;
      S->I.visit_binaryexpr_assignment(left, right);
      S->status("Asignamos el valor.");
      S->pop();
      delete this;
      return Next;
   }
}

void Stepper::visit_callexpr(CallExpr *x) {
   FuncDecl *fn = I.visit_callexpr_getfunc(x);
   push(new CallExprVisitState(x, fn));
   if (!x->args.empty()) {
      x->args[0]->visit(this);
   }
}

Todo Stepper::CallExprVisitState::step(Stepper *S) {
   if (curr == -1) { // we are returning from the call
      S->I.popenv();
      S->pop();
      delete this;
      return Next;
   }
   const int size = x->args.size();
   if (size > 0) {
      args[curr] = S->I._curr;
      ++curr;
   }
   if (curr < size) {
      x->args[curr]->visit(S);
      ostringstream oss;
      oss << "Evaluado el argumento " << curr-1 << ".";
      S->status(oss.str());
      return Stop;
   } else {
      S->I.invoke_func_prepare(fn, args);
      fn->block->visit(S);
      S->status("Saltamos a la función '" + fn->name + "'.");
      curr = -1; // signal return
      return Stop;
   }
}
