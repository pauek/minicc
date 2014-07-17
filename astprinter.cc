#include <sstream>
#include "ast.hh"
#include "astprinter.hh"
using namespace std;

void AstPrinter::visit_program(Program* x) {
   out() << "Program{" << endl;
   indent(+1);
   for (AstNode* n : x->nodes) {
      n->visit(this);
   }
   indent(-1);
   out(beginl) << "}" << endl;
}

void AstPrinter::visit_comment(CommentNode* cn) {
   out() << cn;
}

void AstPrinter::visit_include(Include* x) {
   string D = (x->global ? "<>" : "\"\"");
   out(beginl) << "Include(" << D[0] << x->filename << D[1] << ")" << endl;
}

void AstPrinter::visit_macro(Macro* x) {
   out(beginl) << "Macro(" << x->macro << ")" << endl;
}

void AstPrinter::visit_using(Using* x) {
   out(beginl) << "Using(" << x->namespc << ")" << endl;
}

void AstPrinter::visit_type(Type *x) {
   out() << "Type(" << x->name << ")";
}

void AstPrinter::visit_funcdecl(FuncDecl *x) {
   out(beginl) << "FuncDecl(\"" << x->name << "\", ";
   visit_type(x->return_type);
   out() << ", Params = {";
   for (int i = 0; i < x->params.size(); i++) {
      if (i > 0) {
         out() << ", ";
      }
      out() << "\"" << x->params[i].name << "\": ";
      visit_type(x->params[i].type);
   }
   out() << "}, {" << endl;
   indent(+1);
   x->block->visit(this);
   indent(-1);
   out(beginl) << "})" << endl;
}

void AstPrinter::visit_block(Block *x) {
   out(beginl) << "Block(";
   if (x->stmts.empty()) {
      out() << "{})" << endl;
      return;
   } 
   out() << "{" << endl;
   indent(+1);
   for (Stmt *s : x->stmts) {
      s->visit(this);
   }
   indent(-1);
   out(beginl) << "})" << endl;
}

void AstPrinter::visit_stmt(Stmt *x) {
   out(beginl) << "Stmt(";
   switch (x->type) {
   case Stmt::_empty: 
      out() << "empty)" << endl; break;

   case Stmt::_expr:
      out() << "expr, ";
      visit_expr(x->expr);
      out() << ")" << endl;
      break;

   case Stmt::_while:
      out() << "while, ";
      visit_expr(x->expr);
      out() << ", {" << endl;
      indent(+1);
      x->sub_stmt[0]->visit(this);
      indent(-1);
      out(beginl) << "})" << endl;
      break;      

   default:
      out(beginl) << "unimplemented)" << endl;
   }
}

void AstPrinter::visit_expr(Expr *x) {
   if (x->paren) {
      out() << " (";
   }
   switch (x->type) {
   case Expr::identifier: 
      out() << "id:'" << x->str << "'"; break;

   case Expr::literal:
      out() << "lit:'" << x->str << "'"; break;

   case Expr::assignment:
      out() << Expr::op2char(x->op) << "(";
      visit_expr(x->left);
      out() << ", ";
      visit_expr(x->right);
      out() << ")";
      break;

   case Expr::additive:
      out() << Expr::op2char(x->op) << "(";
      visit_expr(x->left);
      out() << ", ";
      visit_expr(x->right);
      out() << ")";
      break;
      
   case Expr::multiplicative:
      out() << Expr::op2char(x->op) << "(";
      visit_expr(x->left);
      out() << ", ";
      visit_expr(x->right);
      out() << ")";
      break;

   default:
      out() << "<expr>";
   }
   if (x->paren) {
      out() << ") ";
   }
}
