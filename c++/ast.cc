#include <iostream>
#include <sstream>
#include <assert.h>
using namespace std;

#include "ast.hh"
#include "translator.hh"

void AstNode::add_error(string msg) {
   errors.push_back(new Error(ini, fin, msg));
}

void AstNode::add_error(Pos _ini, Pos _fin, string msg) {
   errors.push_back(new Error(_ini, _fin, msg));
}

void Error::to_json(ostream& o) const {
   ostringstream oss;
   o << "{";
   o << "\"ini\": "; ini.to_json(o); o << ", ";
   o << "\"fin\": "; fin.to_json(o); o << ", ";
   o << "\"msg\": \"" << msg << "\"";
   o << "}";
}

// OJO: El orden de la tabla es importante!
// Hay que dejarla antes que el initializer y el map...
//
struct { 
   string      op; 
   Token::Kind tokkind; 
   Expr::Kind  kind; 
} pairs[] = {
   { "",    Token::Empty,        Expr::Unknown },
   { ",",   Token::Comma,        Expr::Comma },

   { "=",   Token::Assign,       Expr::Assignment },
   { "+=",  Token::PlusAssign,   Expr::Assignment },
   { "-=",  Token::MinusAssign,  Expr::Assignment },
   { "*=",  Token::StarAssign,   Expr::Assignment },
   { "/=",  Token::SlashAssign,  Expr::Assignment },
   { "%=",  Token::DivAssign,    Expr::Assignment },
   { "<<=", Token::LShiftAssign, Expr::Assignment },
   { ">>=", Token::RShiftAssign, Expr::Assignment },
   { "&=",  Token::AndAssign,    Expr::Assignment },
   { "|=",  Token::OrAssign,     Expr::Assignment },
   { "^=",  Token::XorAssign,    Expr::Assignment },

   { ":",   Token::Colon,        Expr::Infinite },
   { "?",   Token::QMark,        Expr::Conditional },

   { "or",  Token::Or,           Expr::LogicalOr },
   { "||",  Token::BarBar,       Expr::LogicalOr },

   { "and", Token::And,          Expr::LogicalAnd },
   { "&&",  Token::AmpAmp,       Expr::LogicalAnd },

   { "|",   Token::Bar,          Expr::BitOr },
   { "^",   Token::Circum,       Expr::BitXor },
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
map<Token::Kind, Expr::Kind> Expr::_tok2kind;
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

Expr::Kind Expr::tok2kind(Token::Kind tokkind) {
   auto it = _tok2kind.find(tokkind);
   return (it != _tok2kind.end() ? it->second : Expr::Unknown);
}

bool Expr::right_associative(Expr::Kind t) {
   return t == Expr::Assignment;
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

string Literal::escape(string s, char delim) {
   string r;
   for (char c : s) {
      switch (c) {
      case '\a': r += "\\a"; break;
      case '\b': r += "\\b"; break;
      case '\f': r += "\\f"; break;
      case '\n': r += "\\n"; break;
      case '\r': r += "\\r"; break;
      case '\t': r += "\\t"; break;
      case '\v': r += "\\v"; break;
      case '\?': r += "\\?"; break;
      case '\\': r += "\\\\"; break;
      case '\"': r += (delim == '"' ? "\\\"" : "\""); break;
      case '\'': r += (delim == '\'' ? "\\'" : "'"); break;
      default:   r += c; break;
      }
   }
   return r;
}

#define _ERRORS(x) \
   if (x and x->has_errors()) return true;

bool Program::has_errors() const {
   for (AstNode *n : nodes) {
      _ERRORS(n);
   }
   return AstNode::has_errors();
}

bool ExprStmt::has_errors() const {
   _ERRORS(expr);
   return AstNode::has_errors();
}

bool IfStmt::has_errors() const {
   _ERRORS(cond); _ERRORS(then); _ERRORS(els);
   return AstNode::has_errors();
}

bool ForStmt::has_errors() const {
   _ERRORS(init); _ERRORS(cond); _ERRORS(post); _ERRORS(substmt);
   return AstNode::has_errors();
}

bool WhileStmt::has_errors() const {
   _ERRORS(cond); _ERRORS(substmt);
   return AstNode::has_errors();
}

bool DeclStmt::has_errors() const {
   _ERRORS(typespec);
   for (Item i : items) {
      _ERRORS(i.decl);
      _ERRORS(i.init);
   } 
   return AstNode::has_errors();
}

bool Block::has_errors() const {
   for (Stmt *s : stmts) {
      _ERRORS(s);
   }
   return AstNode::has_errors();
}

bool TemplateIdent::has_errors() const {
   for (TypeSpec *t : subtypes) {
      _ERRORS(t);
   }
   return AstNode::has_errors();
}

bool FullIdent::has_errors() const {
   for (TemplateIdent *id : prefix) {
      _ERRORS(id);
   }
   return TemplateIdent::has_errors() || AstNode::has_errors();
}

bool BinaryExpr::has_errors() const {
   _ERRORS(left); _ERRORS(right);
   return AstNode::has_errors();
}

bool UnaryExpr::has_errors() const {
   _ERRORS(expr);
   return AstNode::has_errors();
}

bool CallExpr::has_errors() const {
   _ERRORS(func);
   return AstNode::has_errors();
}

bool IndexExpr::has_errors() const {
   _ERRORS(base); _ERRORS(index);
   return AstNode::has_errors();
}

bool FieldExpr::has_errors() const {
   _ERRORS(base); _ERRORS(field);
   return AstNode::has_errors();
}

bool CondExpr::has_errors() const {
   _ERRORS(cond); _ERRORS(then); _ERRORS(els);
   return AstNode::has_errors();
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
   return AstNode::has_errors();
}

bool FuncDecl::has_errors() const {
   _ERRORS(return_typespec); _ERRORS(block);
   for (Param* p : params) {
      _ERRORS(p->typespec);
   }
   return AstNode::has_errors();
}

bool StructDecl::has_errors() const {
   _ERRORS(id);
   for (DeclStmt *d : decls) {
      _ERRORS(d);
   }
   return AstNode::has_errors();
}

bool TypedefDecl::has_errors() const {
   _ERRORS(decl);
   return AstNode::has_errors();
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
   return kind == Expr::Assignment;
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