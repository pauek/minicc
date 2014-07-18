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
   x->return_type->visit(this);
   out() << ", Params = {";
   for (int i = 0; i < x->params.size(); i++) {
      if (i > 0) {
         out() << ", ";
      }
      out() << "\"" << x->params[i].name << "\": ";
      x->params[i].type->visit(this);
   }
   out() << "}, {" << endl;
   indent(+1);
   out(beginl);
   x->block->visit(this);
   out() << endl;
   indent(-1);
   out(beginl) << "})" << endl;
}

void AstPrinter::visit_block(Block *x) {
   out() << "Block(";
   if (x->stmts.empty()) {
      out() << "{})";
      return;
   } 
   out() << "{" << endl;
   indent(+1);
   for (Stmt *s : x->stmts) {
      out(beginl);
      s->visit(this);
      out() << endl;
   }
   indent(-1);
   out(beginl) << "})";
}

void AstPrinter::visit_stmt(Stmt *x) {
   out() << "Stmt(";
   switch (x->type) {
   default:
      out(beginl) << "unimplemented)" << endl;
   }
}

void AstPrinter::visit_expr(Expr *x) {
   if (x->paren) {
      out() << "(";
   }
   switch (x->type) {
   case Expr::identifier: 
      out() << "id:'" << x->str << "'"; break;

   case Expr::literal:
      out() << "lit:'" << x->str << "'"; break;

   default:
      out() << x->op << "(";
      if (x->left) {
         x->left->visit(this);
      }
      if (x->right) {
         out() << ", ";
         x->right->visit(this);
      }
      out() << ")";
   }
   if (x->paren) {
      out() << ")";
   }
}

void AstPrinter::visit_declstmt(DeclStmt* x) {
   out() << "<declstmt>";
}

void AstPrinter::visit_exprstmt(ExprStmt* x) {
   out() << "Stmt(";
   if (x->expr) {
      out() << "expr, ";
      x->expr->visit(this);
   } else {
      out() << "empty";
   }
   out() << ")";
}

void AstPrinter::visit_ifstmt(IfStmt *x) {
   out() << "IfStmt(";
   x->cond->visit(this);
   out() << ", ";
   x->then->visit(this);
   if (x->els) {
      out() << ", ";
      x->els->visit(this);
   }
   out() << ")";
}

void AstPrinter::visit_iterstmt(IterStmt *x) {
   if (x->is_for()) {
      out() << "IterStmt<for>(";
      x->init->visit(this);
      out() << ", ";
      x->cond->visit(this);
      out() << ", ";
      x->post->visit(this);
      out() << ", {" << endl;
   } else {
      out() << "IterStmt<while>(";
      x->cond->visit(this);
      out() << ", {" << endl;
   }
   indent(+1);
   out(beginl);
   x->substmt->visit(this);
   out() << endl;
   indent(-1);
   out(beginl) << "})";
}
