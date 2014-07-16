#include <sstream>
#include "ast.hh"
#include "prettyprint.hh"
using namespace std;

const int TAB_WIDTH = 3;

std::ostream& PrettyPrinter::out(OutType typ) { 
   if (typ == beginl and _indent > 0) {
      *_out << string(_indent * TAB_WIDTH, ' ');
   }
   return *_out; 
}

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
   out() << "#" << _cmt("", x->comment_nodes[0])
         << "include" << _cmt(" ", x->comment_nodes[1])
         << delim[0] << x->filename << delim[1];
   if (x->comment_nodes[2]) {
      out() << ' ' << x->comment_nodes[2];
      if (!x->comment_nodes[2]->comments.back().endl) {
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
   out() << "using" << _cmt(" ", x->comment_nodes[0]) 
         << "namespace" << _cmt(" ", x->comment_nodes[1])
         << x->namespc << _cmt("", x->comment_nodes[2])
         << ";";
   if (x->comment_nodes[3] != 0) {
      out() << ' ' << x->comment_nodes[3];
      if (!x->comment_nodes[3]->comments.back().endl) {
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
   out() << _cmt(" ", x->comment_nodes[0]) 
         << x->name << _cmt("", x->comment_nodes[1]) << "(";
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
   if (x->stmts.empty()) {
      out() << "{}" << endl;
      return;
   } 
   indent(+1);
   out() << "{" << endl;
   for (int i = 0; i < x->stmts.size(); i++) {
      visit_stmt(x->stmts[i]);
   }
   indent(-1);
   out(beginl) << "}" << endl;
}

void PrettyPrinter::visit_stmt(Statement *x) {
   out(beginl) << ";";
   if (x->comment_nodes[0] != 0) {
      out() << ' ' << x->comment_nodes[0];
      if (!x->comment_nodes[0]->comments.back().endl) {
         out() << endl;
      }
   } else {
      out() << endl;
   }
}
