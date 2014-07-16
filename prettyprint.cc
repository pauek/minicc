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

string _cmt(CommentNode* cn, bool space = true) {
   ostringstream out;
   if (cn != 0) {
      out << ' ' << cn << ' ';
   } else if (space) {
      out << ' ';
   }
   return out.str();
}

string _cmt_endl(CommentNode *cn) {
   ostringstream out;
   if (cn) {
      out << ' ' << cn;
   }
   if (!cn or !cn->endl()) {
      out << endl;
   }
   return out.str();
}

string _cmt(AstNode* x, int i, bool space = true) {
   return _cmt(x->comment_nodes[i], space);
}

string _cmt0(AstNode* x, int i) {
   return _cmt(x->comment_nodes[i], false);
}

string _cmt_endl(AstNode* x, int i) {
   return _cmt_endl(x->comment_nodes[i]);
}


void PrettyPrinter::visit_comment(CommentNode* cn) {
   out() << cn;
}

void PrettyPrinter::visit_include(Include* x) {
   string delim = "\"\"";
   if (x->global) delim = "<>";
   out() << "#" << _cmt0(x, 0)
         << "include" << _cmt(x, 1)
         << delim[0] << x->filename << delim[1]
         << _cmt_endl(x, 2);
}

void PrettyPrinter::visit_macro(Macro* x) {
   out() << "#" << x->macro << endl;
}

void PrettyPrinter::visit_using(Using* x) {
   out() << "using" << _cmt(x, 0)
         << "namespace" << _cmt(x, 1)
         << x->namespc << _cmt0(x, 2)
         << ";" << _cmt_endl(x, 3);
}

void PrettyPrinter::visit_nodelist(NodeList* x) {}

void PrettyPrinter::visit_type(Type *x) {
   out() << x->name;
}

void PrettyPrinter::visit_funcdecl(FuncDecl *x) {
   out() << endl;
   visit_type(x->return_type);
   out() << _cmt(x, 0)
         << x->name << _cmt0(x, 1) << "(";
   for (int i = 0; i < x->params.size(); i++) {
      if (i > 0) {
         out() << ",";
      }
      out() << _cmt(x->params[i].c[0], i > 0);
      visit_type(x->params[i].type);
      out() << _cmt(x->params[i].c[1]);
      out() << x->params[i].name;
      out() << _cmt(x->params[i].c[2], false);
   }
   out() << ") ";
   visit_stmt(x->block);
}

void PrettyPrinter::visit_stmt(Stmt *x) {
   switch (x->typ) {
   case Stmt::_empty:
      out(beginl) << ";" << _cmt_endl(x, 0);
      break;

   case Stmt::_block:
      if (x->sub_stmts.empty()) {
         out() << "{}" << endl;
         return;
      } 
      indent(+1);
      out() << "{" << endl;
      for (int i = 0; i < x->sub_stmts.size(); i++) {
         visit_stmt(x->sub_stmts[i]);
      }
      indent(-1);
      out(beginl) << "}" << endl;
      break;

   case Stmt::_expr:
      out(beginl);
      visit_expr(x->expr);
      out() << ";" << _cmt_endl(x, 0);
      break;

   default:
      out(beginl) << "<stmt>;" << endl;
   }
}

void PrettyPrinter::visit_expr(Expr *x) {
   switch (x->typ) {
   case Expr::identifier:
   case Expr::literal:
      out() << x->str; break;

   case Expr::assign:
      visit_expr(x->left);
      out() << " = ";
      visit_expr(x->right);
      break;

   default:
      out() << "<expr>";
   }
}
