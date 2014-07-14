#include <sstream>
#include "ast.hh"
#include "prettyprint.hh"
using namespace std;

string _cmt(string sp, CommentNode* cn) {
   ostringstream out;
   if (cn == 0) {
      out << sp;
   } else {
      out << ' ' << cn << ' ';
   }
   return out.str();
}

void PrettyPrinter::visit_comment(CommentNode* cn) {
   out() << cn;
}

void PrettyPrinter::visit_include(Include* x) {
   string delim = "\"\"";
   if (x->global) delim = "<>";
   out() << "#" << _cmt("", x->comments[0])
         << "include" << _cmt(" ", x->comments[1])
         << delim[0] << x->filename << delim[1];
   if (x->comments[2]) {
      out() << ' ' << x->comments[2];
      if (!x->comments[2]->comments.back().endl) {
         out() << endl;
      }
   } else {
      out() << endl;
   }
}

void PrettyPrinter::visit_macro(Macro* x) {
   out() << "#" << x->macro << endl;
}

void PrettyPrinter::visit_using(Using* x) {
   out() << "using" << _cmt(" ", x->comments[0]) 
         << "namespace" << _cmt(" ", x->comments[1])
         << x->namespc << _cmt("", x->comments[2])
         << ";";
   if (x->comments[3] != 0) {
      out() << ' ' << x->comments[3];
      if (!x->comments[3]->comments.back().endl) {
         out() << endl;
      }
   } else {
      out() << endl;
   }
}

void PrettyPrinter::visit_nodelist(NodeList* x) {}

void PrettyPrinter::visit_type(Type *x) {
   out() << x->name;
}

void PrettyPrinter::visit_funcdecl(FuncDecl *x) {
   out() << endl;
   visit_type(x->return_type);
   out() << _cmt(" ", x->comments[0]) 
         << x->name << _cmt("", x->comments[1]) << "(";
   for (int i = 0; i < x->params.size(); i++) {
      if (i > 0) {
         out() << ",";
      }
      out() << _cmt( (i > 0 ? " " : ""), x->params[i].c[0]);
      visit_type(x->params[i].type);
      out() << _cmt(" ", x->params[i].c[1]);
      out() << x->params[i].name;
      out() << _cmt("", x->params[i].c[2]);
   }
   out() << ") ";
   visit_block(x->block);
}

void PrettyPrinter::visit_block(Block *x) {
   out() << "{}" << endl;
}

