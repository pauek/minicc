
#include <iostream>
#include <sstream>
#include <assert.h>
#include "ast.hh"
using namespace std;

const int TAB_WIDTH = 3;

map<string, Expr::Type> Expr::_op2type;
Expr::Op2TypeInitializer Expr::initializer;

Expr::Op2TypeInitializer::Op2TypeInitializer() {
   struct { string op; Expr::Type type; } pairs[] = {
      { "=",   Expr::assignment },
      { "+=",  Expr::assignment },
      { "-=",  Expr::assignment },
      { "*=",  Expr::assignment },
      { "/=",  Expr::assignment },
      { "%=",  Expr::assignment },
      { "<<=", Expr::assignment },
      { ">>=", Expr::assignment },
      { "&=",  Expr::assignment },
      { "|=",  Expr::assignment },
      { "^=",  Expr::assignment },

      { "or",  Expr::logical_or },
      { "||",  Expr::logical_or },

      { "and", Expr::logical_and },
      { "&&",  Expr::logical_and },

      { "|",   Expr::bit_or },
      { "^",   Expr::bit_xor },
      { "&",   Expr::bit_and },

      { "==",  Expr::equality },
      { "!=",  Expr::equality },

      { "<",   Expr::relational },
      { ">",   Expr::relational },
      { ">=",  Expr::relational },
      { "<=",  Expr::relational },
      
      { "<<",  Expr::shift },
      { ">>",  Expr::shift },

      { "+",   Expr::additive },
      { "-",   Expr::additive },

      { "*",   Expr::multiplicative },
      { "/",   Expr::multiplicative },
      { "%",   Expr::multiplicative },

      // { "->*", Expr::multiplicative }, TODO
      // { ".*", Expr::multiplicative }, TODO

      { "+", Expr::additive },
      { "-", Expr::additive },
      { "END", Expr::unknown }
   };
   int i = 0;
   while (pairs[i].op != "END") {
      _op2type[pairs[i].op] = pairs[i].type;
      i++;
   }
}

Expr::Type Expr::op2type(string op) {
   map<string, Expr::Type>::const_iterator it = Expr::_op2type.find(op);
   return (it != _op2type.end() ? it->second : Expr::unknown);
}

std::ostream& AstVisitor::out(OutType typ) { 
   if (typ == beginl and _indent > 0) {
      *_out << string(_indent * TAB_WIDTH, ' ');
   }
   return *_out; 
}

string cmt(CommentNode* cn, bool pre, bool post, bool missing) {
   ostringstream out;
   if (cn != 0) {
      out << (pre ? " " : "") << cn << (post ? " " : "");
   } else if (missing) {
      out << ' ';
   }
   return out.str();
}

string cmtl(CommentNode *cn) {
   ostringstream out;
   if (cn) {
      out << ' ' << cn;
   }
   if (!cn or !cn->endl()) {
      out << endl;
   }
   return out.str();
}

ostream& operator<<(ostream& o, CommentNode* C) {
   if (C == 0) {
      return o;
   }
   for (int i = 0; i < C->comments.size(); i++) {
      const Comment& c = C->comments[i];
      if (c.type == Comment::multiline) {
         o << "/*" << c.text << "*/";
      } else {
         o << "//" << c.text;
      }
      if (c.endl and i < C->comments.size()-1) {
         o << endl;
      }
   }
   return o;
}

void Expr::set(string _op) {
   op = _op;
   type = op2type(_op);
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
