#ifndef PRETTYPRINT_HH
#define PRETTYPRINT_HH

#include <iostream>
#include "ast.hh"

class PrettyPrinter : public AstVisitor {
   int _indent;
   std::ostream *_out;
public:
   PrettyPrinter(std::ostream *o) : _indent(0) {
      _out = o;
   }

   void visit_include(Include* x);
   void visit_macro(Macro* x);
   void visit_nodelist(NodeList* x);
};

#endif
