
#include <iostream>
#include <sstream>
#include <assert.h>
#include "ast.hh"
using namespace std;

const int TAB_WIDTH = 3;

// OJO: El orden de la tabla es importante!
// Hay que dejarla antes que el initializer y el map...
//
struct { 
   string op; 
   Token::Type toktype; 
   Expr::Type type; 
} pairs[] = {
   { "",    Token::Empty,        Expr::unknown },
   { ",",   Token::Comma,        Expr::comma },

   { "=",   Token::Assign,       Expr::assignment },
   { "+=",  Token::PlusAssign,   Expr::assignment },
   { "-=",  Token::MinusAssign,  Expr::assignment },
   { "*=",  Token::StarAssign,   Expr::assignment },
   { "/=",  Token::SlashAssign,  Expr::assignment },
   { "%=",  Token::DivAssign,    Expr::assignment },
   { "<<=", Token::LShiftAssign, Expr::assignment },
   { ">>=", Token::RShiftAssign, Expr::assignment },
   { "&=",  Token::AndAssign,    Expr::assignment },
   { "|=",  Token::OrAssign,     Expr::assignment },
   { "^=",  Token::XorAssign,    Expr::assignment },

   { ":",   Token::Colon,        Expr::infinite },
   { "?",   Token::QMark,        Expr::conditional },

   { "or",  Token::Or,           Expr::logical_or },
   { "||",  Token::BarBar,       Expr::logical_or },

   { "and", Token::And,          Expr::logical_and },
   { "&&",  Token::AmpAmp,       Expr::logical_and },

   { "|",   Token::Bar,          Expr::bit_or },
   { "^",   Token::Circum,       Expr::bit_xor },
   { "&",   Token::Amp,          Expr::bit_and },

   { "==",  Token::EqEq,         Expr::equality },
   { "!=",  Token::NotEq,        Expr::equality },

   { "<",   Token::LT,           Expr::relational },
   { ">",   Token::GT,           Expr::relational },
   { ">=",  Token::GE,           Expr::relational },
   { "<=",  Token::LE,           Expr::relational },
      
   { "<<",  Token::LShift,       Expr::shift },
   { ">>",  Token::RShift,       Expr::shift },

   { "+",   Token::Plus,         Expr::additive },
   { "-",   Token::Minus,        Expr::additive },

   { "*",   Token::Star,         Expr::multiplicative },
   { "/",   Token::Slash,        Expr::multiplicative },
   { "%",   Token::Div,          Expr::multiplicative },

   // { "->*", Expr::multiplicative }, TODO
   // { ".*", Expr::multiplicative }, TODO

   { "END", Token::Unknown,      Expr::unknown }
};

map<string, Expr::Type>      Expr::_op2type;
map<Token::Type, Expr::Type> Expr::_tok2type;
Expr::Op2TypeInitializer Expr::initializer;

Expr::Op2TypeInitializer::Op2TypeInitializer() {
   int i = 0;
   while (pairs[i].op != "END") {
      _op2type[pairs[i].op] = pairs[i].type;
      _tok2type[pairs[i].toktype] = pairs[i].type;
      i++;
   }
}

Expr::Type Expr::op2type(string op) {
   auto it = _op2type.find(op);
   return (it != _op2type.end() ? it->second : Expr::unknown);
}

Expr::Type Expr::tok2type(Token::Type toktyp) {
   auto it = _tok2type.find(toktyp);
   return (it != _tok2type.end() ? it->second : Expr::unknown);
}

bool Expr::right_associative(Expr::Type t) {
   return t == Expr::assignment;
}

std::ostream& AstVisitor::out(OutType typ) { 
   if (typ == beginl and _indent > 0) {
      *_out << string(_indent * TAB_WIDTH, ' ');
   }
   return *_out; 
}

string cmt(CommentSeq* cn, bool pre, bool post, bool missing) {
   ostringstream out;
   if (cn != 0) {
      out << (pre ? " " : "") << cn << (post ? " " : "");
   } else if (missing) {
      out << ' ';
   }
   return out.str();
}

string cmtl(CommentSeq *cn) {
   ostringstream out;
   if (cn) {
      out << ' ' << cn;
   }
   if (!cn or !cn->endl()) {
      out << endl;
   }
   return out.str();
}

ostream& operator<<(ostream& o, CommentSeq* C) {
   if (C == 0) {
      return o;
   }
   for (int i = 0; i < C->items.size(); i++) {
      const Comment& c = C->items[i];
      if (c.type == Comment::multiline) {
         if (i > 0 and !C->items[i].endl) {
            o << ' ';
         }
         o << "/*" << c.text << "*/";
      } else {
         o << "//" << c.text;
      }
      if (c.endl and i < C->items.size()-1) {
         o << endl;
      }
   }
   return o;
}

void BinaryExpr::set(Expr::Type _type) {
   type = _type;
}

JumpStmt::Type JumpStmt::keyword2type(string s) {
   if (s == "break") { 
      return JumpStmt::_break; 
   } else if (s == "continue") {
      return JumpStmt::_continue;
   } else if (s == "goto") {
      return JumpStmt::_goto;
   } else {
      return JumpStmt::unknown;
   }
}

string Literal::escape(string s) {
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
      case '\'': r += "\\\'"; break;
      case '\"': r += "\\\""; break;
      case '\?': r += "\\?"; break;
      case '\\': r += "\\\\"; break;
      default:   r += c; break;
      }
   }
   return r;
}
