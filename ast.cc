
#include <iostream>
#include "ast.hh"
using namespace std;

ostream& operator<<(ostream& o, CommentNode* C) {
   if (C == 0) {
      return o;
   }
   for (const Comment& c : C->comments) {
      if (c.type == Comment::multiline) {
         o << " /*" << c.text << "*/ ";
      } else {
         o << " // " << c.text;
      }
   }
   return o;
}

void NodeList::visit(AstVisitor* v) {
   v->visit_nodelist(this);
   for (int i = 0; i < _children.size(); i++) {
      _children[i]->visit(v);
   }
}
