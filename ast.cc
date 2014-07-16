
#include <iostream>
#include <assert.h>
#include "ast.hh"
using namespace std;

ostream& operator<<(ostream& o, CommentNode* C) {
   if (C == 0) {
      return o;
   }
   for (const Comment& c : C->comments) {
      if (c.type == Comment::multiline) {
         o << "/*" << c.text << "*/";
      } else {
         o << "//" << c.text;
      }
      if (c.endl) {
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
