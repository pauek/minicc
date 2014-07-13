#include <sstream>
#include "ast.hh"
#include "prettyprint.hh"
using namespace std;

string _cmt(CommentNode* cn) {
   ostringstream out;
   if (cn == 0) {
      out << ' ';
   } else {
      out << cn;
   }
   return out.str();
}

void PrettyPrinter::visit_comment(CommentNode* cn) {
   out() << cn;
}

void PrettyPrinter::visit_include(Include* x) {
   string delim = "\"\"";
   if (x->global) delim = "<>";
   out() << "#" << x->comments[0] 
         << "include" << _cmt(x->comments[1])
         << delim[0] << x->filename << delim[1] << x->comments[2] << endl;
}

void PrettyPrinter::visit_macro(Macro* x) {
   out() << "#" << x->macro << endl;
}

void PrettyPrinter::visit_using(Using* x) {
   out() << "using" << _cmt(x->comments[0]) 
         << "namespace" << _cmt(x->comments[1])
         << x->namespc << x->comments[2] 
         << ";" << _cmt(x->comments[3]) << endl;
}

void PrettyPrinter::visit_nodelist(NodeList* x) {}

void PrettyPrinter::visit_type(Type *x) {
   out() << x->name;
}

void PrettyPrinter::visit_funcdecl(FuncDecl *x) {
   visit_type(x->return_type);
   out() << " " << x->name << "(";
   for (int i = 0; i < x->params.size(); i++) {
      if (i > 0) out() << ", ";
      visit_type(x->params[i].type);
      out() << ' ' << x->params[i].name;
   }
   out() << ") ";
   visit_block(x->block);
}

void PrettyPrinter::visit_block(Block *x) {
   out() << "{}" << endl;
}

