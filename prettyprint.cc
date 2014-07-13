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
   out() << "#" << _cmt(x->comments[0]) << "include" << _cmt(x->comments[1]);
   out() << delim[0] << x->filename << delim[1] << x->comments[2] << endl;
}

void PrettyPrinter::visit_macro(Macro* x) {
   out() << "#" << x->macro << endl;
}

void PrettyPrinter::visit_using(Using* x) {
   out() << "using" << _cmt(x->comments[0]) << "namespace" << _cmt(x->comments[1])
         << x->namespc << x->comments[2] << ";" << _cmt(x->comments[3]) << endl;
}

void PrettyPrinter::visit_nodelist(NodeList* x) {}
