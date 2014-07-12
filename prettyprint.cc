#include "ast.hh"
#include "prettyprint.hh"
using namespace std;

void PrettyPrinter::visit_include(Include* x) {
   string delim = "\"\"";
   if (x->global) delim = "<>";
   (*_out) << "#include " << delim[0] << x->filename << delim[1] << endl;
}

void PrettyPrinter::visit_macro(Macro* x) {
   (*_out) << "#" << x->macro << endl;
}

void PrettyPrinter::visit_nodelist(NodeList* x) {}
