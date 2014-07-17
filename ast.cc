
#include <iostream>
#include <sstream>
#include <assert.h>
#include "ast.hh"
using namespace std;

const int TAB_WIDTH = 3;

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

Expr::Type Expr::char2type(char c) {
   switch (c) {
   case '=': return assignment;
   case '+': return additive;
   case '-': return additive;
   case '*': return multiplicative;
   case '/': return multiplicative;
   default: return unknown;
   }
}

char Expr::op2char(Expr::Op op) {
   switch (op) {
   case assign: return '=';
   case add:    return '+';
   case sub:    return '-';
   case mult:   return '*';
   case div:    return '/';
   default:     return ' ';
   }
}

void Expr::set(char c) {
   type = char2type(c);
   switch (c) {
   case '=': op = assign; break;
   case '+': op = add;    break;
   case '-': op = sub;    break;
   case '*': op = mult;   break;
   case '/': op = sub;    break;
   default: assert(false);
   }
}
