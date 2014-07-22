#include <sstream>
#include "ast.hh"
#include "prettypr.hh"
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
         out() << "," << _cmt_(x->params[i], 0);
      } else {
         out() << _cmt0_(x->params[i], 0);
      }
      visit_type(x->params[i]->type);
      out() << _cmt_(x->params[i], 1);
      out() << x->params[i]->name;
      out() << _cmt0(x->params[i], 2);
   }
   out() << ")";
   if (x->block) {
      out() << " ";
      x->block->visit(this);
   } else {
      out() << ";";
   }
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

void PrettyPrinter::visit_identifier(Identifier *x) {
   out() << x->id << _cmt0(x, 0);
}

void PrettyPrinter::visit_literal(Literal *x) {
   if (x->paren) {
      out() << "(";
   }
   out() << x->lit << _cmt0(x, 0);
   if (x->paren) {
      out() << ")";
   }
}

void PrettyPrinter::visit_binaryexpr(BinaryExpr *x) {
   if (x->paren) {
      out() << "(";
   }
   switch (x->type) {
   case BinaryExpr::assignment:
   case BinaryExpr::additive:
   case BinaryExpr::multiplicative:
      x->left->visit(this);
      out() << " " << x->op << _cmt_(x, 0);
      x->right->visit(this);
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
   if (x->expr) {
      x->expr->visit(this);
   }
   out() << ";" << _cmt0(x, 0);
}

void PrettyPrinter::visit_ifstmt(IfStmt *x) {
   out() << "if" << _cmt_(x, 0) << "(";
   x->cond->visit(this);
   out() << ")" << _cmt_(x, 1);
   x->then->visit(this);
   if (x->els) {
      if (!x->then->is<Block>()) {
         out() << endl;
         out(beginl);
      } else {
         out() << ' ';
      }
      out() << "else" << _cmt(x, 1);
      x->els->visit(this);
   }
}

void PrettyPrinter::visit_iterstmt(IterStmt *x) {
   if (x->is_for()) {
      out() << "for" << _cmt_(x, 0) << "(";
      x->init->visit(this);
      x->cond->visit(this);
      out() << "; ";
      x->post->visit(this);
      out() << ")" << _cmt_(x, 1);
   } else {
      out() << "while" << _cmt_(x, 0) << "(";
      x->cond->visit(this);
      out() << ")" << _cmt_(x, 1);
   }
   x->substmt->visit(this);
}

void PrettyPrinter::visit_jumpstmt(JumpStmt *x) {
   string keyword[3] = { "break", "continue", "goto" };
   out() << keyword[x->type] << _cmt0(x, 0);
   if (x->type == JumpStmt::_goto) {
      out() << " " << x->label << _cmt0(x, 1) << ";" << _cmt0(x, 2);
   } else {
      out() << ";" << _cmt0(x, 1);
   }
}

void PrettyPrinter::visit_callexpr(CallExpr *x) {
   x->func->visit(this);
   out() << "(";
   for (int i = 0; i < x->args.size(); i++) {
      if (i > 0) {
         out() << ", ";
      }
      x->args[i]->visit(this);
   }
   out() << ")";
}

void PrettyPrinter::visit_indexexpr(IndexExpr *x) {
   x->base->visit(this);
   out() << "[";
   x->index->visit(this);
   out() << "]";
}

void PrettyPrinter::visit_fieldexpr(FieldExpr *x) {
   x->base->visit(this);
   out() << (x->pointer ? "->" : ".");
   x->field->visit(this);
}

void PrettyPrinter::visit_signexpr(SignExpr *x) {
   out() << (x->type == SignExpr::Positive ? "+" : "-");
   x->expr->visit(this);
}

void PrettyPrinter::visit_negexpr(NegExpr *x) {
   out() << "!";
   x->expr->visit(this);
}
