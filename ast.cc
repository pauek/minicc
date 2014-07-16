
#include <iostream>
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

