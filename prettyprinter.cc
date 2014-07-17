#include <sstream>
#include "ast.hh"
#include "prettyprinter.hh"
using namespace std;

void PrettyPrinter::visit_nodelist(NodeList* x) {
   for (int i = 0; i < x->_children.size(); i++) {
      AstNode *n = x->_children[i];
      if (n->is<FuncDecl>() and i > 0) {
         out() << endl;
      }
      n->visit(this);
   }
}

void PrettyPrinter::visit_comment(CommentNode* cn) {
   out() << cn;
}

void PrettyPrinter::visit_include(Include* x) {
   string delim = "\"\"";
   if (x->global) delim = "<>";
   out() << "#" << _cmt0_(x, 0)
         << "include" << _cmt_(x, 1)
         << delim[0] << x->filename << delim[1]
         << _cmtl(x, 2);
}

void PrettyPrinter::visit_macro(Macro* x) {
   out() << "#" << x->macro << endl;
}

void PrettyPrinter::visit_using(Using* x) {
   out() << "using" << _cmt_(x, 0)
         << "namespace" << _cmt_(x, 1)
         << x->namespc << _cmt0_(x, 2)
         << ";" << _cmtl(x, 3);
}

void PrettyPrinter::visit_type(Type *x) {
   out() << x->name;
}

void PrettyPrinter::visit_funcdecl(FuncDecl *x) {
   visit_type(x->return_type);
   out() << _cmt_(x, 0)
         << x->name << _cmt0_(x, 1) << "(";
   for (int i = 0; i < x->params.size(); i++) {
      if (i > 0) {
         out() << "," << _cmt_(&x->params[i], 0);
      } else {
         out() << _cmt0_(&x->params[i], 0);
      }
      visit_type(x->params[i].type);
      out() << _cmt_(&x->params[i], 1);
      out() << x->params[i].name;
      out() << _cmt0(&x->params[i], 2);
   }
   out() << ") ";
   visit_stmt(x->block);
}

void PrettyPrinter::visit_stmt(Stmt *x) {
   switch (x->type) {
   case Stmt::_empty:
      out(beginl) << ";" << _cmtl(x, 0);
      break;

   case Stmt::_block:
      if (x->sub_stmts.empty()) {
         out() << "{}" << endl;
         return;
      } 
      indent(+1);
      out() << "{" << _cmtl(x, 0);
      for (int i = 0; i < x->sub_stmts.size(); i++) {
         visit_stmt(x->sub_stmts[i]);
      }
      indent(-1);
      out(beginl) << "}" << endl;
      break;

   case Stmt::_expr:
      out(beginl);
      visit_expr(x->expr);
      out() << ";" << _cmtl(x, 0);
      break;

   default:
      out(beginl) << "<stmt>;" << endl;
   }
}

void PrettyPrinter::visit_expr(Expr *x) {
   if (x->paren) {
      out() << "(";
   }
   switch (x->type) {
   case Expr::identifier:
   case Expr::literal:
      out() << x->str 
            << (x->comment_nodes[0] == 0 ? "" : " ") << x->comment_nodes[0]; break;

   case Expr::assignment:
   case Expr::additive:
   case Expr::multiplicative:
      visit_expr(x->left);
      out() << " " << Expr::op2char(x->op) << _cmt_(x, 0);
      visit_expr(x->right);
      break;

   default:
      out() << "<expr>";
   }
   if (x->paren) {
      out() << ")";
   }
}
