#include <vector>
#include <sstream>
#include <iomanip>
#include <stdint.h>
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

string Stepper::output() { 
   std::string s = _out.str();
   _out.str("");
   return s;
}

Todo Stepper::PopState::step(Stepper *S) {
   S->pop();
   delete this;
   return Next; 
}

void Stepper::generic_visit(AstNode *x) {
   x->visit(&I);
   status(x->describe());
   push(new PopState(x->span()));
}

void Stepper::visit_declstmt(DeclStmt *x)     { generic_visit(x); }
void Stepper::visit_increxpr(IncrExpr *x)     { generic_visit(x); }
void Stepper::visit_binaryexpr(BinaryExpr *x) { generic_visit(x); }
void Stepper::visit_literal(Literal *x)       { x->visit(&I); }
void Stepper::visit_ident(Ident *x)           { x->visit(&I); }
void Stepper::visit_fieldexpr(FieldExpr *x)   { x->visit(&I); }

void Stepper::visit_program(Program *x) {
   I.visit_program_prepare(x);
   FuncDecl *main = I.visit_program_find_main();
   if (main == 0) {
      _error("La funcion 'main' no existe");
   }
   status("Empieza el programa.");
   I.invoke_func_prepare(main, vector<Value*>());
   I._env.back().active = true;
   push(new ProgramVisitState(main));
}

Range Stepper::ProgramVisitState::span() const {
   if (at == ProgramVisitState::Begin) {
      return x->id->span(); 
   } else if (at == ProgramVisitState::Finished) {
      Pos ini = x->block->fin - 1, fin = x->block->fin;
      return Range(ini, fin);
   }
   return Range();
}

Todo Stepper::ProgramVisitState::step(Stepper *S) {
   switch (at) {
   case Begin: {
      x->block->visit(S);
      at = End;
      return Stop;
   } 
   case End: {
      at = Finished;
      S->status("Termina el programa.");
      return Stop;
   }
   case Finished: {
      S->I.popenv();
      S->pop();
      delete this;
      return Next;
   }
   default:
      assert(false);
   }
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
   x->cond->visit(&I);
   Value *c = I._curr;
   if (c->kind != Value::Bool) {
      _error("La condición de un 'if' debe ser un valor de tipo 'bool'.");
   }
   Stmt *next;
   if (c->val.as_bool) {
      status("La condición vale 'true', tomamos la primera rama.");
      next = x->then;
   } else {
      if (x->els != 0) {
         status("La condición vale 'false', tomamos la segunda rama.");
         next = x->els;
      } else {
         status("La condición vale 'false', continuamos.");
         next = 0;
      }
   }
   push(new IfVisitState(x->cond->span(), next));
}

Todo Stepper::IfVisitState::step(Stepper *S) {
   S->pop();
   Todo todo = Stop;
   if (next == 0) {
      todo = Next;
   } else {
      next->visit(S);
   }
   delete this;
   return todo;
}

void Stepper::visit_iterstmt(IterStmt *x) {
   if (x->is_for()) {
      push(new ForVisitState(x));
      x->init->visit(this);
   } else {
      WhileVisitState *s = new WhileVisitState(x);
      s->step(this);
      push(s);
   }
}

Todo Stepper::ForVisitState::step(Stepper *S) {
   switch (at) {
   case Stepper::ForVisitState::Leave: {
      S->pop();
      delete this;
      return Next;
   }
   case Stepper::ForVisitState::Cond: {
      S->visit(x->cond);
      Value *cond = S->I._curr;
      if (cond->kind != Value::Bool) {
         S->_error("La condición de un 'for' debe ser un valor de tipo 'bool'");
      }
      if (!cond->val.as_bool) {
         S->status("La condición vale 'false', salimos del for.");
         at = Stepper::ForVisitState::Leave;
      } else {
         S->status("La condición vale 'true', entramos en el for.");
         at = Stepper::ForVisitState::Block;
      }
      return Stop;
   }
   case Stepper::ForVisitState::Block: {
      x->substmt->visit(S);
      at = Stepper::ForVisitState::Post;
      return Stop;
   }
   case Stepper::ForVisitState::Post: {
      x->post->visit(S);
      at = Stepper::ForVisitState::Cond;
      return Stop;
   }
   }
}

Todo Stepper::WhileVisitState::step(Stepper *S) {
   switch (at) {
   case Stepper::WhileVisitState::Leave: {
      S->pop();
      delete this;
      return Next;
   }
   case Stepper::WhileVisitState::Cond: {
      S->visit(x->cond);
      Value *cond = S->I._curr;
      if (cond->kind != Value::Bool) {
         S->_error("La condición de un 'while' debe ser un valor de tipo 'bool'");
      }
      if (!cond->val.as_bool) {
         S->status("La condición vale 'false', salimos del while.");
         at = Stepper::WhileVisitState::Leave;
      } else {
         S->status("La condición vale 'true', entramos en el while.");
         at = Stepper::WhileVisitState::Block;
      }
      return Stop;
   }
   case Stepper::WhileVisitState::Block:
      x->substmt->visit(S);
      at = Stepper::WhileVisitState::Cond;
      return Stop;
   }
}

void Stepper::visit_exprstmt(ExprStmt *x) { 
   if (x->expr->is_assignment()) {
      visit_assignment(dynamic_cast<BinaryExpr*>(x->expr));
   } 
   else if (x->expr->is_write_expr()) {
      BinaryExpr *e = dynamic_cast<BinaryExpr*>(x->expr);
      assert(e != 0);
      WriteExprVisitState *ws = new WriteExprVisitState(e);
      e->collect_rights(ws->exprs);
      push(ws);
      ws->step(this);
   } else {
      I.visit(x->expr);
      if (x->is_return) {
         ostringstream oss;
         oss << "Se retorna " << *I._curr << ".";
         status(oss.str());
      } else {
         status(x->expr->describe());
      }
      push(new PopState(x->span()));
   }
}

Range Stepper::WriteExprVisitState::span() const {
   return curr->span();
}

Todo Stepper::WriteExprVisitState::step_waiting(Stepper* S) {
   S->status("Se escribe a la salida.");
   S->_out << *S->I._curr;
   exprs.pop_front();
   waiting = false;
   return Stop;
}

Todo Stepper::WriteExprVisitState::step(Stepper* S) {
   if (!exprs.empty()) {
      if (waiting) {
         return step_waiting(S);
      } else {
         curr = exprs.front();
         const int old_sz = S->_stack.size();
         curr->visit(S);
         if (S->_stack.size() > old_sz) {
            waiting = true;
            return Stop;
         } else {
            return step_waiting(S);
         }
      }
   } else {
      S->pop();
      delete this;
      return Next;
   }
}

void Stepper::visit_assignment(BinaryExpr *e) {
   assert(e != 0);
   I.visit(e);
   Value *right = I._curr;
   if (right->kind == Value::Ref) {
      right = right->ref();
   }
   ostringstream oss;
   oss << "La expresión ha dado " << *right << ".";
   status(oss.str());
   push(new AssignmentVisitState(e, right));
}

Range Stepper::AssignmentVisitState::span() const {
   if (left == 0) {
      return x->right->span();
   } else {
      return Range(x->left->span().ini, x->right->span().ini);
   }
}

Todo Stepper::AssignmentVisitState::step(Stepper *S) {
   if (left != 0) {
      S->pop();
      delete this;
      return Next;
   }
   x->left->visit(&S->I);
   left = S->I._curr;
   S->I.visit_binaryexpr_assignment(left, right);
   S->status("Asignamos el valor.");
   return Stop;
}

void Stepper::visit_callexpr(CallExpr *x) {
   FuncDecl *fn = I.visit_callexpr_getfunc(x);
   CallExprVisitState *s = new CallExprVisitState(x, fn);
   vector<Value*> args(fn->params.size(), 0);
   I.invoke_func_prepare(fn, args);
   s->step(this);
   push(s);
}

string numeral[] = {
   "primer", "segundo", "tercer", "cuarto", "quinto", "sexto", "séptimo"
};

const int Stepper::CallExprVisitState::Block  = -1;
const int Stepper::CallExprVisitState::Return = -2;

Range Stepper::CallExprVisitState::span() const {
   if (curr == CallExprVisitState::Return) {
      return x->span();
   } else if (curr == CallExprVisitState::Block) {
      return fn->id->span();
   } else {
      return x->args[curr-1]->span();
   }
}

Todo Stepper::CallExprVisitState::step(Stepper *S) {
   const int size = x->args.size();
   if (curr == CallExprVisitState::Return) {
      S->I.popenv();
      S->pop();
      delete this;
      return Next;
   } else if (curr == CallExprVisitState::Block) {
      fn->block->visit(S);
      curr = CallExprVisitState::Return;
      return Stop;
   } else if (curr < size) {
      if (curr < 7) {
         S->status("Se evalúa el " + numeral[curr] + " parámetro.");
      } else {
         ostringstream oss;
         oss << "Se evalúa el parámetro número " << curr << ".";
         S->status(oss.str());
      }
      x->args[curr]->visit(&S->I);
      Value *v = S->I._curr;
      if (!fn->params[curr]->type->reference && v->kind == Value::Ref) {
         v = v->ref();
      }
      S->I.setenv(fn->params[curr]->name, v);
      ++curr;
      return Stop;
   } else {
      S->status("Saltamos a la función '" + fn->id->str() + "'.");
      S->I.actenv();
      curr = Block;
      return Stop;
   }
}


string Stepper::state2json() const {
   ostringstream json;
   json << "{";
   json << "\"env\":" << I.env2json() << ",";
   json << "\"status\":" << "\"" << json_encode(_status) << "\",";
   Range s = span();
   json << "\"span\":" << "{";
   // -1 for codemirror!
   json << "\"ini\":{" << "\"line\":" <<  s.ini.lin-1 << ",\"ch\":" << s.ini.col << "},";
   json << "\"fin\":{" << "\"line\":" <<  s.fin.lin-1 << ",\"ch\":" << s.fin.col << "}";
   json << "}}";
   return json.str();
}
