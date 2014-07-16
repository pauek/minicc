#ifndef PRETTYPRINT_HH
#define PRETTYPRINT_HH

#include <assert.h>
#include <iostream>
#include "ast.hh"

class PrettyPrinter : public AstVisitor {
   int _indent;
   std::ostream *_out;

   enum OutType { normal, beginl };
   std::ostream& out(OutType typ = normal);
   void indent(int x) { 
      _indent += x; 
      assert(_indent >= 0);
   }
   
public:
   PrettyPrinter(std::ostream *o = &std::cout) : _indent(0), _out(o) {}

   void print(AstNode* x) { x->visit(this); }

   void visit_comment(CommentNode *x);
   void visit_include(Include *x);
   void visit_macro(Macro *x);
   void visit_nodelist(NodeList *x);
   void visit_using(Using *x);
   void visit_type(Type *x);
   void visit_funcdecl(FuncDecl *x);
   void visit_block(Block *x);
   void visit_stmt(Statement *x);
};

#endif
