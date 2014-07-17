#include <sstream>
#include "ast.hh"
#include "prettyprinter.hh"
using namespace std;

void PrettyPrinter::visit_program(Program* x) {
   for (int i = 0; i < x->nodes.size(); i++) {
      AstNode *n = x->nodes[i];
      if (n->is<FuncDecl>() and i > 0) {
         out() << endl;
      }
      n->visit(this);
      if (n->is<CommentNode>()) {
         out() << endl;
      }
   }
}

void PrettyPrinter::visit_comment(CommentNode* cn) {
   out() << cn;
}

void PrettyPrinter::visit_include(Include* x) {
   string delim = "\"\"";
   if (x->global) {
      delim = "<>";
   }
   out() << "#" << _cmt0_(x, 0) << "include" << _cmt_(x, 1)
         << delim[0] << x->filename << delim[1] << _cmt0(x, 2) << endl;
}

void PrettyPrinter::visit_macro(Macro* x) {
   out() << "#" << x->macro << endl;
}

void PrettyPrinter::visit_using(Using* x) {
   out() << "using" << _cmt_(x, 0) << "namespace" << _cmt_(x, 1)
         << x->namespc << _cmt0_(x, 2) << ";" << _cmt0(x, 3) << endl;
}

void PrettyPrinter::visit_type(Type *x) {
   out() << x->name;
}

void PrettyPrinter::visit_funcdecl(FuncDecl *x) {
   visit_type(x->return_type);
   out() << _cmt_(x, 0) << x->name << _cmt0_(x, 1) << "(";
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
   x->block->visit(this);
   out() << endl;
}

void PrettyPrinter::print_block(Block *x) {
   if (x->stmts.empty()) {
      out() << "{}" << _cmt0(x, 1);
      return;
   } 
   indent(+1);
   out() << "{" << _cmtl(x, 0);
   for (Stmt *s : x->stmts) {
      out(beginl);
      s->visit(this);
      out() << endl;
   }
   indent(-1);
   out(beginl) << "}" << _cmt0(x, 1);
}

void PrettyPrinter::visit_expr(Expr *x) {
   if (x->paren) {
      out() << "(";
   }
   switch (x->type) {
   case Expr::identifier:
   case Expr::literal:
      out() << x->str << _cmt0(x, 0); break;

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

void PrettyPrinter::visit_stmt(Stmt *x) {
   switch (x->type) {
   case Stmt::_empty:
      out() << ";" << _cmt0(x, 0);
      break;

   case Stmt::_expr:
      visit_expr(x->expr);
      out() << ";" << _cmt0(x, 0);
      break;

   case Stmt::_if:
      out() << "if" << _cmt_(x, 0) << "(";
      x->expr->visit(this);
      out() << ")" << _cmt_(x, 1);
      x->sub_stmt[0]->visit(this);
      if (x->sub_stmt[1]) {
         if (!x->sub_stmt[0]->is<Block>()) {
            out() << endl;
            out(beginl);
         } else {
            out() << ' ';
         }
         out() << "else" << _cmt(x, 1);
         x->sub_stmt[1]->visit(this);
      }
      break;

   case Stmt::_while:
      out() << "while" << _cmt_(x, 0) << "(";
      x->expr->visit(this);
      out() << ")" << _cmt_(x, 1);
      x->sub_stmt[0]->visit(this);
      break;

   default:
      out() << "<stmt>;";
   }
}

void PrettyPrinter::visit_block(Block *x) {
   print_block(x);
}

void PrettyPrinter::visit_declstmt(DeclStmt* x) {
   out() << "<declstmt>";
}

void PrettyPrinter::visit_exprstmt(ExprStmt* x) {
   visit_expr(x->expr);
   out() << ";" << _cmt0(x, 0);
}
