#include <vector>
#include <sstream>
#include <iomanip>
#include <stdint.h>
using namespace std;

#include "stepper.hh"
#include "translator.hh"

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
   x->accept(&I);
   status(x->describe());
   push(new PopState(x->span()));
}

void Stepper::visit_declstmt(DeclStmt *x)     { generic_visit(x); }
void Stepper::visit_increxpr(IncrExpr *x)     { generic_visit(x); }
void Stepper::visit_binaryexpr(BinaryExpr *x) { generic_visit(x); }
void Stepper::visit_literal(Literal *x)       { x->accept(&I); }
void Stepper::visit_ident(Ident *x)           { x->accept(&I); }
void Stepper::visit_fieldexpr(FieldExpr *x)   { x->accept(&I); }

void Stepper::visit_program(Program *x) {
   I.visit_program_prepare(x);
   I.visit_program_find_main();
   status(_T("The program begins."));
   I.pushenv("main");
   FuncDecl *main = dynamic_cast<UserFunc*>(I._curr.as<Function>().ptr)->decl;
   I.invoke_func_prepare(main, vector<Value>());
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
      x->block->accept(S);
      at = End;
      return Stop;
   } 
   case End: {
      at = Finished;
      S->status(_T("The program ends."));
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
   x->stmts[0]->accept(this);
}

Todo Stepper::BlockVisitState::step(Stepper *S) {
   const int last = x->stmts.size()-1;
   ++curr;
   if (curr > last) {
      S->pop();
      delete this;
      return Next;
   }
   x->stmts[curr]->accept(S);
   return Stop;
}

void Stepper::visit_ifstmt(IfStmt *x) {
   x->cond->accept(&I);
   Value cond = I._curr;
   if (!cond.is<Bool>()) {
      _error(_T("The condition in a '%s' has to be a value of type 'bool'.", "if"));
   }
   Stmt *next;
   if (cond.as<Bool>()) {
      status(_T("The condition is 'true', we take the first branch."));
      next = x->then;
   } else {
      if (x->els != 0) {
         status(_T("The condition is 'false', we take the second branch."));
         next = x->els;
      } else {
         status(_T("The condition is 'false', we continue."));
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
      next->accept(S);
   }
   delete this;
   return todo;
}

void Stepper::visit_iterstmt(IterStmt *x) {
   if (x->is_for()) {
      push(new ForVisitState(x));
      x->init->accept(this);
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
      Value cond = S->I._curr;
      if (!cond.is<Bool>()) {
         S->_error(_T("The condition in a '%s' must be a value of type 'bool'.", "for"));
      }
      if (!cond.as<Bool>()) {
         S->status(_T("The condition is 'false', we exit the %s.", "for"));
         at = Stepper::ForVisitState::Leave;
      } else {
         S->status(_T("The condition is 'true', we enter the %s.", "for"));
         at = Stepper::ForVisitState::Block;
      }
      return Stop;
   }
   case Stepper::ForVisitState::Block: {
      x->substmt->accept(S);
      at = Stepper::ForVisitState::Post;
      return Stop;
   }
   case Stepper::ForVisitState::Post: {
      x->post->accept(S);
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
      Value cond = S->I._curr;
      if (!cond.is<Bool>()) {
         S->_error(_T("The condition in a '%s' must be a value of type 'bool'.", "while"));
      }
      if (!cond.as<Bool>()) {
         S->status(_T("The condition is 'false', we exit the %s.", "while"));
         at = Stepper::WhileVisitState::Leave;
      } else {
         S->status(_T("The condition is 'true', we enter the %s.", "while"));
         at = Stepper::WhileVisitState::Block;
      }
      return Stop;
   }
   case Stepper::WhileVisitState::Block:
      x->substmt->accept(S);
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
   } else if (x->expr->is<CallExpr>()) {
      visit_callexpr(dynamic_cast<CallExpr*>(x->expr));
   } else {
      I.visit(x->expr);
      if (x->is_return) {
         ostringstream oss;
         oss << I._curr;
         status(_T("%s is returned.", oss.str().c_str()));
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
   S->status(_T("Some output is written."));
   Value curr = Reference::deref(S->I._curr);
   S->_out << curr;
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
         curr->accept(S);
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
   I.visit(e->right);
   Value right = Reference::deref(I._curr);
   ostringstream oss;
   oss << right;
   status(_T("La expresión ha dado %s.", oss.str().c_str()));
   push(new AssignmentVisitState(e, right));
}

Range Stepper::AssignmentVisitState::span() const {
   if (left.is_null()) {
      return x->right->span();
   } else {
      return Range(x->left->span().ini, x->right->span().ini);
   }
}

Todo Stepper::AssignmentVisitState::step(Stepper *S) {
   if (!left.is_null()) {
      S->pop();
      delete this;
      return Next;
   }
   S->I.visit(x->left);
   left = S->I._curr;
   if (x->op == "=") {
      S->I.visit_binaryexpr_assignment(left, right);
   } else if (x->op.size() == 2 and x->op[1] == '=') {
      S->I.visit_binaryexpr_op_assignment(x->op[0], left, right);
   }
   S->status(_T("We assign the value."));
   return Stop;
}

void Stepper::visit_callexpr(CallExpr *x) {
   I.visit_callexpr_getfunc(x);
   const FuncValue& fval = I._curr.as<Function>();
   FuncDecl *fn = dynamic_cast<const UserFunc*>(fval.ptr)->decl;
   assert(fn != 0);
   CallExprVisitState *s = new CallExprVisitState(x, fn);
   vector<Value> args(fn->params.size(), Value::null);
   I.pushenv(fn->id->str());
   I.invoke_func_prepare(fn, args);
   s->step(this);
   push(s);
}

const char *numeral[] = {
   "first", "second", "third", "fourth", "fifth", "sixth", "seventh"
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
      fn->block->accept(S);
      curr = CallExprVisitState::Return;
      return Stop;
   } else if (curr < size) {
      if (curr < 7) {
         S->status(_T("We evaluate the %s parameter.", _T(numeral[curr]).c_str()));
      } else {
         S->status(_T("We evaluate parameter number %d.", curr));
      }
      S->I.visit(x->args[curr]);
      Value v = S->I._curr;
      if (!fn->params[curr]->type->reference) {
         v = Reference::deref(v);
      }
      S->I.setenv(fn->params[curr]->name, v);
      ++curr;
      return Stop;
   } else {
      S->status(_T("We jump to function '%s'.", fn->id->str().c_str()));
      S->I.actenv();
      curr = Block;
      return Stop;
   }
}

string json_encode(string s) {
   ostringstream json;
   for (int i = 0; i < s.size(); i++) {
      if (uint8_t(s[i]) < 0x7F) {
         json << s[i];
      } else if ((uint8_t(s[i]) & 0x000000E0) == 0x000000C0) {
         // Gran cutrada...
         json << "\\u" << hex << setfill('0') << setw(4) 
              << ((uint8_t(s[i] & 0x1F) << 6) + uint8_t(s[i+1] & 0x3F));
         i++;
      } else {
         assert(false);
      }
   }
   return json.str();
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
