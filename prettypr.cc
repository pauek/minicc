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
      if (n->is<CommentSeq>()) {
         out() << endl;
      }
   }
}

void PrettyPrinter::visit_comment(CommentSeq* cn) {
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
   int i = 0, numquals = 0;
   static const string names[] = { 
      "const", "volatile", "mutable", "register", "auto", "extern"
   };
   while (Type::Qualifiers(1 << i) <= Type::Extern) {
      if (x->qual & Type::Qualifiers(1 << i)) {
         if (numquals > 0) {
            out() << " ";
         }
         out() << names[i];
         numquals++;
      }
      i++;
   }
   if (numquals > 0) {
      out() << " ";
   }
   if (x->nested_ids.size() == 1) {
      x->nested_ids[0]->visit(this);
   } else {
      for (int i = 0; i < x->nested_ids.size(); i++) {
         if (i > 0) {
            out() << "::";
         }
         x->nested_ids[i]->visit(this);
      }
   }
}

void PrettyPrinter::visit_structdecl(StructDecl *x) {
   out() << "struct ";
   x->id->visit(this);
   out() << " {";
   indent(+1);
   for (DeclStmt *decl : x->decls) {
      out(beginl);
      decl->visit(this);
      out() << endl;
   }
   indent(-1);
   out(beginl) << "};";
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
      if (x->params[i]->ref) {
         out() << "&";
      }
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

void PrettyPrinter::visit_ident(Ident *x) {
   out() << x->id << _cmt0(x, 0);
   if (x->subtype) {
      out() << "<";
      x->subtype->visit(this);
      out() << ">";
   }
}

void PrettyPrinter::visit_literal(Literal *x) {
   if (x->paren) {
      out() << "(";
   }
   switch (x->type) {
   case Literal::Bool:   out() << (x->val.as_bool ? "true" : "false"); break;
   case Literal::Int:    out() << x->val.as_int; break;
   case Literal::String: out() << '"' << Literal::escape(*x->val.as_string.s, '"') << '"'; break;
   case Literal::Char:   out() << "'" << Literal::escape(*x->val.as_string.s, '\'') << "'"; break;
   default:              out() << "<literal>"; break;
   }
   out() << _cmt0(x, 0);
   if (x->paren) {
      out() << ")";
   }
}

void PrettyPrinter::visit_binaryexpr(BinaryExpr *x) {
   if (x->paren) {
      out() << "(";
   }
   x->left->visit(this);
   if (x->op != ",") {
      out() << " ";
   }
   out() << x->op << _cmt_(x, 0);
   x->right->visit(this);

   if (x->paren) {
      out() << ")";
   }
   out() << _cmt0(x, -1);
}

void PrettyPrinter::visit_block(Block *x) {
   print_block(x);
}

void PrettyPrinter::visit_declstmt(DeclStmt* x) {
   x->type->visit(this);
   int c = 0;
   for (int i = 0; i < x->decls.size(); i++) {
      if (i > 0) {
         out() << ",";
      }
      out() << _cmt_(x, c++);
      const DeclStmt::Decl& d = x->decls[i];
      out() << d.name << _cmt0(x, c++);
      if (d.init) {
         out() << " =" << _cmt_(x, c++);
         d.init->visit(this);
      }
   }
   out() << ";" << _cmt0(x, c);
}

void PrettyPrinter::visit_exprstmt(ExprStmt* x) {
   if (x->is_return) {
      out() << "return ";
   }
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
      out() << " ";
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
   if (x->paren) {
      out() << "(";
   }
   x->func->visit(this);
   if (x->func->comments[0] != 0 and !x->func->comments[0]->endl()) {
      out() << " ";
   }
   out() << "(";
   for (int i = 0; i < x->args.size(); i++) {
      if (i > 0) {
         out() << ", ";
      }
      out() << cmt0_(x, i);
      x->args[i]->visit(this);
   }
   out() << ")" << _cmt0_(x, x->args.size());
   if (x->paren) {
      out() << ")";
   }
}

void PrettyPrinter::visit_indexexpr(IndexExpr *x) {
   if (x->paren) {
      out() << "(";
   }
   x->base->visit(this);
   out() << "[";
   x->index->visit(this);
   out() << "]";
   if (x->paren) {
      out() << ")";
   }
}

void PrettyPrinter::visit_fieldexpr(FieldExpr *x) {
   if (x->paren) {
      out() << "(";
   }
   x->base->visit(this);
   out() << (x->pointer ? "->" : ".");
   x->field->visit(this);
   if (x->paren) {
      out() << ")";
   }
}

void PrettyPrinter::visit_condexpr(CondExpr *x) {
   if (x->paren) {
      out() << "(";
   }
   x->cond->visit(this);
   out() << " ?" << _cmt_(x, 0);
   x->then->visit(this);
   out() << " : " << _cmt0_(x, 1);
   x->els->visit(this);
   if (x->paren) {
      out() << ")";
   }
}

void PrettyPrinter::visit_signexpr(SignExpr *x) {
   if (x->paren) {
      out() << "(";
   }
   out() << (x->type == SignExpr::Positive ? "+" : "-");
   x->expr->visit(this);
   if (x->paren) {
      out() << ")";
   }
}

void PrettyPrinter::visit_increxpr(IncrExpr *x) {
   if (x->paren) {
      out() << "(";
   }
   if (x->preincr) {
      out() << (x->type == IncrExpr::Positive ? "++" : "--");
      x->expr->visit(this);
   } else {
      x->expr->visit(this);
      out() << (x->type == IncrExpr::Positive ? "++" : "--");
   }
   if (x->paren) {
      out() << ")";
   }
}

void PrettyPrinter::visit_negexpr(NegExpr *x) {
   if (x->paren) {
      out() << "(";
   }
   out() << "!" << _cmt0_(x, 0);
   x->expr->visit(this);
   if (x->paren) {
      out() << ")";
   }
}

void PrettyPrinter::visit_addrexpr(AddrExpr *x) {
   if (x->paren) {
      out() << "(";
   }
   out() << "&" << _cmt0_(x, 0);
   x->expr->visit(this);
   if (x->paren) {
      out() << ")";
   }
}

void PrettyPrinter::visit_derefexpr(DerefExpr *x) {
   if (x->paren) {
      out() << "(";
   }
   out() << "*" << _cmt0_(x, 0);
   x->expr->visit(this);
   if (x->paren) {
      out() << ")";
   }
}

void PrettyPrinter::visit_errorstmt(Stmt::Error *x) {
   out() << "/* ErrorStmt: \"" << x->code << "\" */";
}

void PrettyPrinter::visit_errorexpr(Expr::Error *x) {
   out() << "/* ErrorExpr: \"" << x->code << "\" */";
}

