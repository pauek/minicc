#ifndef PRETTYPRINT_HH
#define PRETTYPRINT_HH

#include <iostream>
#include "ast.hh"

class PrettyPrinter : public AstVisitor {
   int _indent;
   std::ostream *_out;
   
   std::ostream& out() { return *_out; }
   
public:
   PrettyPrinter(std::ostream *o) : _indent(0) {
      _out = o;
   }

   void visit_comment(CommentNode *x);
   void visit_include(Include *x);
   void visit_macro(Macro *x);
   void visit_nodelist(NodeList *x);
   void visit_using(Using *x);
   void visit_type(Type *x);
   void visit_funcdecl(FuncDecl *x);
   void visit_block(Block *x);
};

#endif
