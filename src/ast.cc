#include <iostream>
#include <sstream>
#include <assert.h>
using namespace std;

#include "cast.h"
#include "ast.hh"
#include "translator.hh"

bool CommentSeq::HasEndLine() const {
   for (const Comment& c : items) {
      if (c.kind == Comment::endline) {
         return true;
      }
   }
   return false;
}

bool CommentSeq::EndsWithEmptyLine() const { 
   const int sz = items.size();
   return sz >= 2 and 
      (items[sz-2].kind == Comment::endline and items[sz-1].kind == Comment::endline);
}

void CommentSeq::RemoveEndLines() {
   items.erase(std::remove_if(items.begin(), items.end(), 
                              [](Comment& c) {
                                 return c.kind == Comment::endline; 
                              }),
               items.end());
}

void CommentSeq::OnlyOneEndLineAtEnd() {
   if (items.empty() or items.back().kind != Comment::endline) {
      return;
   }
   int i = items.size()-1;
   while (true) {
      if (items[i-1].kind != Comment::endline) {
         break;
      }
      i--;
   }
   items.resize(i+1);
}


void Ast::AddError(string msg) {
   errors.push_back(new Error(span, msg));
}

void Ast::AddError(Pos _ini, Pos _fin, string msg) {
   errors.push_back(new Error(Span(_ini, _fin), msg));
}

void Error::to_json(ostream& o) const {
   ostringstream oss;
   o << "{";
   o << "\"ini\": "; span.begin.to_json(o); o << ", ";
   o << "\"fin\": "; span.end.to_json(o); o << ", ";
   o << "\"msg\": \"" << msg << "\"";
   o << "}";
}

const string TypeSpec::QualifiersNames[] = { 
   "const",    "volatile", "mutable", 
   "register", "auto",     "extern"
};

Expr::Kind Expr::tok2kind(Token::Type tokkind) {
   switch (tokkind) {
   case Token::Empty:
      return Expr::Unknown;
   case Token::Comma:
      return Expr::Comma;
   case Token::Eq:
   case Token::PlusEq:
   case Token::MinusEq:
   case Token::StarEq:
   case Token::SlashEq:
   case Token::DivEq:
   case Token::LShiftEq:
   case Token::RShiftEq:
   case Token::AmpEq:
   case Token::BarEq:
   case Token::XorEq:
      return Expr::Eq;
   case Token::Colon:
      return Expr::Infinite;
   case Token::QMark:
      return Expr::Conditional;
   case Token::Or:
   case Token::BarBar:
      return Expr::LogicalOr;
   case Token::And:
   case Token::AmpAmp:
      return Expr::LogicalAnd;
   case Token::Bar:
      return Expr::BitOr;
   case Token::Xor:
      return Expr::BitXor;
   case Token::Amp:
      return Expr::BitAnd;
   case Token::EqEq:
   case Token::NotEq:
      return Expr::Equality;
   case Token::LT:
   case Token::GT:
   case Token::GE:
   case Token::LE:
      return Expr::Relational;
   case Token::LShift:
   case Token::RShift:
      return Expr::Shift;
   case Token::Plus:
   case Token::Minus:
      return Expr::Additive;
   case Token::Star:
   case Token::Slash:
   case Token::Div:
      return Expr::Multiplicative;
   default:
      return Expr::Unknown;
   }
}

bool Expr::right_associative(Expr::Kind t) {
   return t == Expr::Eq;
}

void BinaryExpr::set(Expr::Kind k) {
   kind = k;
}

JumpStmt::Kind JumpStmt::keyword2type(string s) {
   if (s == "break") { 
      return JumpStmt::Break; 
   } else if (s == "continue") {
      return JumpStmt::Continue;
   } else if (s == "goto") {
      return JumpStmt::Goto;
   } else {
      return JumpStmt::Unknown;
   }
}

string Literal::escape(char c, char delim) {
   switch (c) {
   case '\a': return "\\a";
   case '\b': return "\\b";
   case '\f': return "\\f";
   case '\n': return "\\n";
   case '\r': return "\\r";
   case '\t': return "\\t";
   case '\v': return "\\v";
   case '\?': return "\\?";
   case '\\': return "\\\\";
   case '\"': 
      return (c == delim ? "\\\"" : "\"");
   case '\'': 
      return (c == delim ? "\\\'" : "\'");
   default: 
      return string(1, c);
   }
}

string Literal::escape(string s, char delim) {
   string r;
   for (char c : s) {
      r += escape(c, delim);
   }
   return r;
}

bool HasErrors(Ast *ast) {

#define CHECK_ERRORS(n) if (HasErrors(n)) return true

   if (ast == 0) {
      return false;
   }
   switch (ast->type()) {
   case AstType::Program: {
      Program *X = cast<Program>(ast);
      for (Ast *n : X->nodes) {
         if (HasErrors(n)) {
            return true;
         }
      }
      return X->HasErrors();
   }
   case AstType::ExprStmt: {
      ExprStmt *X = cast<ExprStmt>(ast);
      CHECK_ERRORS(X->expr);
      return X->HasErrors();
   }
   case AstType::IfStmt: {
      IfStmt *X = cast<IfStmt>(ast);
      CHECK_ERRORS(X->cond); 
      CHECK_ERRORS(X->then); 
      CHECK_ERRORS(X->els);
      return X->HasErrors();
   }
   case AstType::ForStmt: {
      ForStmt *X = cast<ForStmt>(ast);
      CHECK_ERRORS(X->init); 
      CHECK_ERRORS(X->cond); 
      CHECK_ERRORS(X->post); 
      CHECK_ERRORS(X->substmt);
      return X->HasErrors();
   }
   case AstType::WhileStmt: {
      WhileStmt *X = cast<WhileStmt>(ast);
      CHECK_ERRORS(X->cond); 
      CHECK_ERRORS(X->substmt);
      return X->HasErrors();
   }
   case AstType::DeclStmt: {
      DeclStmt *X = cast<DeclStmt>(ast);
      CHECK_ERRORS(X->typespec);
      for (DeclStmt::Item i : X->items) {
         CHECK_ERRORS(i.decl);
         CHECK_ERRORS(i.init);
      } 
      return X->HasErrors();
   }
   case AstType::Block: {
      Block *X = cast<Block>(ast);
      for (Stmt *s : X->stmts) {
         CHECK_ERRORS(s);
      }
      return X->HasErrors();
   }
   case AstType::Identifier: {
      Identifier *X = cast<Identifier>(ast);
      for (Identifier *id : X->prefix) {
         CHECK_ERRORS(id);
      }
      for (TypeSpec *t : X->subtypes) {
         CHECK_ERRORS(t);
      }
      return X->HasErrors();
   }
   case AstType::BinaryExpr: {
      BinaryExpr *X = cast<BinaryExpr>(ast);
      CHECK_ERRORS(X->left); 
      CHECK_ERRORS(X->right);
      return X->HasErrors();
   }
   case AstType::CallExpr: {
      CallExpr *X = cast<CallExpr>(ast);
      CHECK_ERRORS(X->func);
      return X->HasErrors();
   }
   case AstType::IndexExpr: {
      IndexExpr *X = cast<IndexExpr>(ast);
      CHECK_ERRORS(X->base); 
      CHECK_ERRORS(X->index);
      return X->HasErrors();
   }
   case AstType::FieldExpr: {
      FieldExpr *X = cast<FieldExpr>(ast);
      CHECK_ERRORS(X->base); 
      return X->HasErrors();
   }
   case AstType::CondExpr: {
      CondExpr *X = cast<CondExpr>(ast);
      CHECK_ERRORS(X->cond);
      CHECK_ERRORS(X->then);
      CHECK_ERRORS(X->els);
      return X->HasErrors();
   }   
   case AstType::ExprList: {
      ExprList *X = cast<ExprList>(ast);
      for (Expr *e : X->exprs) {
         CHECK_ERRORS(e);
      }
      return X->HasErrors();
   }   
   case AstType::TypeSpec: {
      TypeSpec *X = cast<TypeSpec>(ast);
      CHECK_ERRORS(X->id);
      return X->HasErrors();
   }   
   case AstType::FuncDecl: {
      FuncDecl *X = cast<FuncDecl>(ast);
      CHECK_ERRORS(X->return_typespec); 
      CHECK_ERRORS(X->block);
      for (FuncDecl::Param* p : X->params) {
         CHECK_ERRORS(p->typespec);
      }
      return X->HasErrors();
   }   
   case AstType::StructDecl: {
      StructDecl *X = cast<StructDecl>(ast);
      for (DeclStmt *d : X->decls) {
         CHECK_ERRORS(d);
      }
      return X->HasErrors();
   }   
   case AstType::TypedefDecl: {
      TypedefDecl *X = cast<TypedefDecl>(ast);
      CHECK_ERRORS(X->decl);
      return X->HasErrors();
   }   
   case AstType::SignExpr: {
      SignExpr *X = cast<SignExpr>(ast);
      CHECK_ERRORS(X->expr);
      return X->HasErrors();
   }
   case AstType::IncrExpr: {
      IncrExpr *X = cast<IncrExpr>(ast);
      CHECK_ERRORS(X->expr);
      return X->HasErrors();
   }
   case AstType::NegExpr: {
      NegExpr *X = cast<NegExpr>(ast);
      CHECK_ERRORS(X->expr);
      return X->HasErrors();
   }
   case AstType::AddrExpr: {
      AddrExpr *X = cast<AddrExpr>(ast);
      CHECK_ERRORS(X->expr);
      return X->HasErrors();
   }
   case AstType::DerefExpr: {
      DerefExpr *X = cast<DerefExpr>(ast);
      CHECK_ERRORS(X->expr);
      return X->HasErrors();
   }
   default:
      return ast->HasErrors();
   }
}

string Identifier::typestr() const {
   string _id;
   for (int i = 0; i < prefix.size(); i++) {
      _id += prefix[i]->typestr();
      _id += "::";
   }
   _id += name;
   if (!subtypes.empty()) {
      _id += "<";
      for (int i = 0; i < subtypes.size(); i++) {
         if (i > 0) {
            _id += ",";
         }
         _id += subtypes[i]->typestr();
      }
      _id += ">";
   }
   return _id;
}

Identifier *Identifier::get_potential_namespace_or_class() const {
   if (prefix.size() == 1 and !prefix[0]->is_template()) {
      return prefix[0];
   }
   return 0;
}

vector<Identifier*> Identifier::get_non_namespaces() {
   vector<Identifier*>::iterator it = prefix.begin();
   while (it != prefix.end() and (*it)->is_namespace) {
      it++;
   }
   vector<Identifier*> result(it, prefix.end());
   result.push_back(this);
   return result;
}

Identifier *TypeSpec::get_potential_namespace_or_class() const {
   return id->get_potential_namespace_or_class();
}

bool TypeSpec::is(TypeSpec::Qualifiers q) const {
   return find(qual.begin(), qual.end(), q) != qual.end();
}

string TypeSpec::typestr() const {
   string _id;
   int i = 0, numquals = 0;
   static const string names[] = { 
      "const", "volatile", "mutable", "register", "auto", "extern"
   };
   while (TypeSpec::Qualifiers(1 << i) <= TypeSpec::Extern) {
      int q = TypeSpec::Qualifiers(1 << i);
      if (find(qual.begin(), qual.end(), q) != qual.end()) {
         if (numquals > 0) {
            _id += " ";
         }
         _id += names[i];
         numquals++;
      }
      i++;
   }
   _id += id->typestr();
   if (reference) {
      _id += "&";
   }
   return _id;
}

string ArrayDecl::typestr() const { 
   string brackets;
   for (int i = 0; i < sizes.size(); i++) {
      brackets += "[]";
   }
   return typespec->typestr() + brackets; 
}

string StructDecl::typestr() const {
   ostringstream S;
   S << "struct{";
   for (int i = 0; i < decls.size(); i++) {
      if (i > 0) {
         S << ";";
      }
      S << decls[i]->typespec->typestr();
   }
   S << "}";
   return S.str();
}

int StructDecl::num_fields() const {
   int num = 0;
   for (int i = 0; i < decls.size(); i++) {
      num += decls[i]->items.size();
   }
   return num;
}

void CollectRights(Ast *ast, list<Expr*>& L) {
   if (isa<BinaryExpr>(ast)) {
      BinaryExpr *X = cast<BinaryExpr>(ast);
      L.push_front(X->right);
      CollectRights(X->left, L);
   }
}

bool IsReadExpr(Ast *ast) {
   switch (ast->type()) {
   case AstType::BinaryExpr: {
      BinaryExpr *X = cast<BinaryExpr>(ast);
      if (isa<Identifier>(X->left)) {
         Identifier *id = cast<Identifier>(X->left);
         return id->name == "cin";
      } else {
         return IsReadExpr(X->left) and X->op == ">>";
      }
   }
   default:
      return false;
   }
}

bool IsWriteExpr(Ast *ast) {
   switch (ast->type()) {
   case AstType::BinaryExpr: {
      BinaryExpr *X = cast<BinaryExpr>(ast);
      if (isa<Identifier>(X->left)) {
         Identifier *id = cast<Identifier>(X->left);
         return id->name == "cout";
      } else {
         return IsWriteExpr(X->left) and X->op == "<<";
      }
   }
   default:
      return false;
   }
}

bool IsAssignment(Ast *ast) {
   if (isa<BinaryExpr>(ast)) {
      BinaryExpr *X = cast<BinaryExpr>(ast);
      return X->kind == Expr::Eq;
   }
   return false;
}

string Describe(Ast *ast) {
   switch (ast->type()) {
   case AstType::ExprStmt: {
      ExprStmt *X = cast<ExprStmt>(ast);
      return Describe(X->expr);
   }
   case AstType::IncrExpr: {
      IncrExpr *X = cast<IncrExpr>(ast);
      if (isa<Identifier>(X->expr)) {
         Identifier *id = cast<Identifier>(X->expr);
         return _T("Se incrementa la variable '%s'.", id->name.c_str());
      }
      return _T("UNIMPLEMENTED");
   }
   case AstType::BinaryExpr: {
      BinaryExpr *X = cast<BinaryExpr>(ast);
      if (IsWriteExpr(X)) {
         return _T("Some output is written.");
      }
      if (IsReadExpr(X)) {
         return _T("Some input is read.");
      }
      return _T("UNIMPLEMENTED");
   }
   case AstType::DeclStmt: {
      DeclStmt *X = cast<DeclStmt>(ast);
      if (X->items.size() == 1) {
         return _T("Se declara la variable '%s'.", X->items[0].decl->name.c_str());
      }
      ostringstream S;
      for (int i = 0; i < X->items.size(); i++) {
         if (i > 0) {
            if (i == X->items.size() - 1) {
               S << _T(" and ");
            } else {
               S << ", ";
            }
         }
         S << "'" << X->items[i].decl->name << "'";
      }
      return _T("Variables %s are declared.", S.str().c_str());
   }
   default:
      return _T("UNIMPLEMENTED");
   }
}

void Identifier::shift(string new_id) {
   Identifier *pre = new Identifier(name);
   pre->subtypes.swap(subtypes);
   pre->comments.swap(comments);
   pre->errors.swap(errors);
   // the last two comments are the ones surrounding the "::", 
   // copy them over to new_id
   const int commsz = pre->comments.size();
   comments.push_back(pre->comments[commsz-2]);
   comments.push_back(pre->comments[commsz-1]);
   pre->comments.resize(commsz-2);

   prefix.push_back(pre);
   name = new_id;
}

