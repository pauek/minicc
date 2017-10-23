#include <iostream>
#include <sstream>
#include <assert.h>
using namespace std;

#include "ast.hh"
#include "translator.hh"

bool CommentSeq::has_endl() const {
   for (const Comment& c : items) {
      if (c.kind == Comment::endline) {
         return true;
      }
   }
   return false;
}

bool CommentSeq::ends_with_empty_line() const { 
   const int sz = items.size();
   return sz >= 2 and 
      (items[sz-2].kind == Comment::endline and items[sz-1].kind == Comment::endline);
}

void CommentSeq::remove_endls() {
   items.erase(std::remove_if(items.begin(), items.end(), 
                              [](Comment& c) {
                                 return c.kind == Comment::endline; 
                              }),
               items.end());
}

void CommentSeq::only_one_endl_at_end() {
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


void Ast::add_error(string msg) {
   errors.push_back(new Error(span, msg));
}

void Ast::add_error(Pos _ini, Pos _fin, string msg) {
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

// OJO: El orden de la tabla es importante!
// Hay que dejarla antes que el initializer y el map...
//
struct { 
   string      op; 
   Token::Type tokkind; 
   Expr::Kind  kind; 
} pairs[] = {
   { "",    Token::Empty,        Expr::Unknown },
   { ",",   Token::Comma,        Expr::Comma },

   { "=",   Token::Eq,           Expr::Eqment },
   { "+=",  Token::PlusEq,       Expr::Eqment },
   { "-=",  Token::MinusEq,      Expr::Eqment },
   { "*=",  Token::StarEq,       Expr::Eqment },
   { "/=",  Token::SlashEq,      Expr::Eqment },
   { "%=",  Token::DivEq,        Expr::Eqment },
   { "<<=", Token::LShiftEq,     Expr::Eqment },
   { ">>=", Token::RShiftEq,     Expr::Eqment },
   { "&=",  Token::AmpEq,        Expr::Eqment },
   { "|=",  Token::BarEq,        Expr::Eqment },
   { "^=",  Token::XorEq,        Expr::Eqment },

   { ":",   Token::Colon,        Expr::Infinite },
   { "?",   Token::QMark,        Expr::Conditional },

   { "or",  Token::Or,           Expr::LogicalOr },
   { "||",  Token::BarBar,       Expr::LogicalOr },

   { "and", Token::And,          Expr::LogicalAnd },
   { "&&",  Token::AmpAmp,       Expr::LogicalAnd },

   { "|",   Token::Bar,          Expr::BitOr },
   { "^",   Token::Xor,       Expr::BitXor },
   { "&",   Token::Amp,          Expr::BitAnd },

   { "==",  Token::EqEq,         Expr::Equality },
   { "!=",  Token::NotEq,        Expr::Equality },

   { "<",   Token::LT,           Expr::Relational },
   { ">",   Token::GT,           Expr::Relational },
   { ">=",  Token::GE,           Expr::Relational },
   { "<=",  Token::LE,           Expr::Relational },
      
   { "<<",  Token::LShift,       Expr::Shift },
   { ">>",  Token::RShift,       Expr::Shift },

   { "+",   Token::Plus,         Expr::Additive },
   { "-",   Token::Minus,        Expr::Additive },

   { "*",   Token::Star,         Expr::Multiplicative },
   { "/",   Token::Slash,        Expr::Multiplicative },
   { "%",   Token::Div,          Expr::Multiplicative },

   // { "->*", Expr::multiplicative }, TODO
   // { ".*", Expr::multiplicative }, TODO

   { "END", Token::Unknown,      Expr::Unknown }
};

const string TypeSpec::QualifiersNames[] = { 
   "const",    "volatile", "mutable", 
   "register", "auto",     "extern"
};

map<string, Expr::Kind>      Expr::_op2kind;
map<Token::Type, Expr::Kind> Expr::_tok2kind;
Expr::Op2KindInitializer Expr::initializer;

Expr::Op2KindInitializer::Op2KindInitializer() {
   int i = 0;
   while (pairs[i].op != "END") {
      _op2kind[pairs[i].op] = pairs[i].kind;
      _tok2kind[pairs[i].tokkind] = pairs[i].kind;
      i++;
   }
}

Expr::Kind Expr::op2kind(string op) {
   auto it = _op2kind.find(op);
   return (it != _op2kind.end() ? it->second : Expr::Unknown);
}

Expr::Kind Expr::tok2kind(Token::Type tokkind) {
   auto it = _tok2kind.find(tokkind);
   return (it != _tok2kind.end() ? it->second : Expr::Unknown);
}

bool Expr::right_associative(Expr::Kind t) {
   return t == Expr::Eqment;
}

std::ostream& ReadWriter::out(OutType typ) { 
   ostream *o = _out;
   if (!_stack.empty()) {
      o = _stack.back();
   }
   if (typ == beginl and _indent > 0) {
      *o << indentation();
   }
   return *o; 
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

#define _ERRORS(x) \
   if (x and x->has_errors()) return true;

bool Program::has_errors() const {
   for (Ast *n : nodes) {
      _ERRORS(n);
   }
   return Ast::has_errors();
}

bool ExprStmt::has_errors() const {
   _ERRORS(expr);
   return Ast::has_errors();
}

bool IfStmt::has_errors() const {
   _ERRORS(cond); _ERRORS(then); _ERRORS(els);
   return Ast::has_errors();
}

bool ForStmt::has_errors() const {
   _ERRORS(init); _ERRORS(cond); _ERRORS(post); _ERRORS(substmt);
   return Ast::has_errors();
}

bool WhileStmt::has_errors() const {
   _ERRORS(cond); _ERRORS(substmt);
   return Ast::has_errors();
}

bool DeclStmt::has_errors() const {
   _ERRORS(typespec);
   for (Item i : items) {
      _ERRORS(i.decl);
      _ERRORS(i.init);
   } 
   return Ast::has_errors();
}

bool Block::has_errors() const {
   for (Stmt *s : stmts) {
      _ERRORS(s);
   }
   return Ast::has_errors();
}

bool TemplateIdent::has_errors() const {
   for (TypeSpec *t : subtypes) {
      _ERRORS(t);
   }
   return Ast::has_errors();
}

bool FullIdent::has_errors() const {
   for (TemplateIdent *id : prefix) {
      _ERRORS(id);
   }
   return TemplateIdent::has_errors() || Ast::has_errors();
}

bool BinaryExpr::has_errors() const {
   _ERRORS(left); _ERRORS(right);
   return Ast::has_errors();
}

bool UnaryExpr::has_errors() const {
   _ERRORS(expr);
   return Ast::has_errors();
}

bool CallExpr::has_errors() const {
   _ERRORS(func);
   return Ast::has_errors();
}

bool IndexExpr::has_errors() const {
   _ERRORS(base); _ERRORS(index);
   return Ast::has_errors();
}

bool FieldExpr::has_errors() const {
   _ERRORS(base); _ERRORS(field);
   return Ast::has_errors();
}

bool CondExpr::has_errors() const {
   _ERRORS(cond); _ERRORS(then); _ERRORS(els);
   return Ast::has_errors();
}

bool ExprList::has_errors() const {
   for (Expr *e : exprs) {
      if (e->has_errors()) {
         return true;
      }
   }
   return false;
}

bool TypeSpec::has_errors() const {
   _ERRORS(id);
   return Ast::has_errors();
}

bool FuncDecl::has_errors() const {
   _ERRORS(return_typespec); _ERRORS(block);
   for (Param* p : params) {
      _ERRORS(p->typespec);
   }
   return Ast::has_errors();
}

bool StructDecl::has_errors() const {
   _ERRORS(id);
   for (DeclStmt *d : decls) {
      _ERRORS(d);
   }
   return Ast::has_errors();
}

bool TypedefDecl::has_errors() const {
   _ERRORS(decl);
   return Ast::has_errors();
}

string TemplateIdent::typestr() const {
   string _id = name;
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

string FullIdent::typestr() const {
   string _id;
   for (int i = 0; i < prefix.size(); i++) {
      _id += prefix[i]->typestr();
      _id += "::";
   }
   _id += TemplateIdent::typestr();
   return _id;
}

SimpleIdent *FullIdent::get_potential_namespace_or_class() const {
   if (prefix.size() == 1 and !prefix[0]->is_template()) {
      return prefix[0];
   }
   return 0;
}

vector<TemplateIdent*> FullIdent::get_non_namespaces() {
   vector<TemplateIdent*>::iterator it = prefix.begin();
   while (it != prefix.end() and (*it)->is_namespace) {
      it++;
   }
   vector<TemplateIdent*> result(it, prefix.end());
   result.push_back(this);
   return result;
}

SimpleIdent *TypeSpec::get_potential_namespace_or_class() const {
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

bool BinaryExpr::is_read_expr() const {
   FullIdent *id = dynamic_cast<FullIdent*>(left);
   return 
      (left->is_read_expr() and op == ">>") or
      (id != 0 and id->name == "cin");   
}

bool BinaryExpr::is_write_expr() const {
   FullIdent *id = dynamic_cast<FullIdent*>(left);
   return 
      (left->is_write_expr() and op == "<<") or
      (id != 0 and id->name == "cout");   
}

bool BinaryExpr::is_assignment() const {
   return kind == Expr::Eqment;
}

void BinaryExpr::collect_rights(list<Expr*>& L) const {
   L.push_front(right);
   left->collect_rights(L);
}

string ExprStmt::describe() const {
   return expr->describe();
}

string IncrExpr::describe() const {
   FullIdent *id = dynamic_cast<FullIdent*>(expr);
   if (id != 0) {
      return _T("Se incrementa la variable '%s'.", id->name.c_str());
   }
   return _T("UNIMPLEMENTED");
}

string BinaryExpr::describe() const {
   if (is_write_expr()) {
      return _T("Some output is written.");
   }
   if (is_read_expr()) {
      return _T("Some input is read.");
   }
   return _T("UNIMPLEMENTED");
}

string DeclStmt::describe() const {
   if (items.size() == 1) {
      return _T("Se declara la variable '%s'.", items[0].decl->name.c_str());
   }
   ostringstream S;
   for (int i = 0; i < items.size(); i++) {
      if (i > 0) {
         if (i == items.size() - 1) {
            S << _T(" and ");
         } else {
            S << ", ";
         }
      }
      S << "'" << items[i].decl->name << "'";
   }
   return _T("Variables %s are declared.", S.str().c_str());
}

void FullIdent::shift(string new_id) {
   TemplateIdent *pre = new TemplateIdent(name);
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

string FuncDecl::funcname() const {
   return id->name;
}
