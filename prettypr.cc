#include <algorithm>
#include <sstream>
#include "ast.hh"
#include "prettypr.hh"
using namespace std;

void PrettyPrinter::visit_program(Program* x) {
   for (int i = 0; i < x->nodes.size(); i++) {
      AstNode *n = x->nodes[i];
      if ((n->is<FuncDecl>() or n->is<StructDecl>()) and i > 0) {
         out() << endl;
      }
      n->visit(this);
      out() << endl;
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
   out() << "#include" << _cmt_(x, 0)
         << delim[0] << x->filename << delim[1] << _cmt0(x, 1);
}

void PrettyPrinter::visit_macro(Macro* x) {
   out() << "#" << x->macro;
}

void PrettyPrinter::visit_using(Using* x) {
   out() << "using" << _cmt_(x, 0) << "namespace" << _cmt_(x, 1)
         << x->namespc << _cmt0(x, 2) << ";" << _cmt0(x, 3);
}

void PrettyPrinter::visit_type(Type *x) {
   int i = 0, c = 0;
   for (int q : x->qual) {
      out() << Type::QualifiersNames[q] << _cmt_(x, c++);
   }
   x->id->visit(this);
   if (x->reference) {
      out() << _cmt0_(x, c++) << "&";
      out() << _cmt0_(x, c);
   } else {
      out() << _cmt0_(x, c);
   }   
}

void PrettyPrinter::visit_enumdecl(EnumDecl *x) {
   out() << "enum " << cmt0_(x, 0) << x->name << _cmt0(x, 1) 
         << " { " << cmt0_(x, 2);
   int cn = 3;
   for (int i = 0; i < x->values.size(); i++) {
      if (i > 0) {
         out() << ", " << cmt0_(x, cn++);
      }
      out() << x->values[i].id << _cmt0(x, cn++);
      if (x->values[i].has_val) {
         out() << " = " << cmt0_(x, cn++) 
               << x->values[i].val << _cmt0(x, cn++);
      }
   }
   out() << " };";
}

void PrettyPrinter::visit_typedefdecl(TypedefDecl *x) {
   out() << "typedef " << cmt0_(x, 0);
   x->decl->type->visit(this);
   out() << cmt0_(x, 1) << ' ';
   x->decl->visit(this);
   out() << ";" << _cmt0(x, 2);
}

void PrettyPrinter::visit_structdecl(StructDecl *x) {
   out() << "struct " << cmt0_(x, 0);
   x->id->visit(this);
   out() << _cmt0(x, 1) << " {" << _cmt0(x, 2) << endl;
   indent(+1);
   vector<string> decl_strings;
   vector<CommentSeq*> last_comments;
   size_t max_size = 0;
   for (DeclStmt *decl : x->decls) {
      push();
      CommentSeq *last = 0;
      swap(last, decl->comments.back());
      last_comments.push_back(last);
      decl->visit(this);
      string d = pop();
      max_size = std::max(d.size(), max_size);
      decl_strings.push_back(d);
   }
   for (int i = 0; i < decl_strings.size(); i++) {
      string decl = decl_strings[i];
      CommentSeq *c = last_comments[i];
      string filler(max_size - decl.size(), ' ');
      out(beginl) << decl << filler << cmt(c, 1, 0, 0) << endl;
   }
   indent(-1);
   out(beginl) << "}" << _cmt0(x, 3) << ";" << _cmt0(x, 4);  
}

void PrettyPrinter::visit_funcdecl(FuncDecl *x) {
   visit_type(x->return_type);
   out() << _cmt_(x, 0);
   x->id->visit(this);
   out() << "(";
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
}

void PrettyPrinter::print_block(Block *x) {
   if (x->stmts.empty()) {
      out() << "{" << _cmt0_(x, 0) << "}" << _cmt0(x, 1);
      return;
   } 
   indent(+1);
   out() << "{" << _cmt0(x, 0) << endl;
   for (Stmt *s : x->stmts) {
      out(beginl);
      s->visit(this);
      out() << endl;
   }
   indent(-1);
   out(beginl) << "}" << _cmt0(x, 1);
}

void PrettyPrinter::visit_ident(Ident *x) {
   for (Ident *pre : x->prefix) {
      pre->visit(this);
      out() << "::";
   }
   out() << x->id << _cmt0(x, 0);
   if (!x->subtypes.empty()) {
      out() << "<" << _cmt0(x, 1);
      for (int i = 0; i < x->subtypes.size(); i++) {
         if (i > 0) {
            out() << "," << _cmt_(x, i+2);
         }
         x->subtypes[i]->visit(this);
      }
      out() << ">" << _cmt0(x, -1);
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
   int nc = 0;
   if (x->paren) {
      out() << "(" << cmt0_(x, nc++);
   }
   x->left->visit(this);
   if (x->op != ",") {
      out() << " ";
   }
   out() << x->op << _cmt_(x, nc++);
   x->right->visit(this);

   if (x->paren) {
      out() << ")";
   }
   out() << _cmt0(x, -1);
}

void PrettyPrinter::visit_block(Block *x) {
   print_block(x);
}

void PrettyPrinter::visit_vardecl(VarDecl *x) {
   if (x->kind == Decl::Pointer) {
      out() << "*";
   }
   out() << x->name << _cmt0(x, 0);
}

void PrettyPrinter::visit_exprlist(ExprList *x) {
   out() << "{";
   for (int i = 0; i < x->exprs.size(); i++) {
      if (i > 0) {
         out() << ", ";
      }
      x->exprs[i]->visit(this);
   }
   out() << "}";
}

void PrettyPrinter::visit_arraydecl(ArrayDecl *x) {
   out() << x->name << _cmt0(x, 0);
   out() << "[";
   x->size->visit(this);
   out() << "]";
}

void PrettyPrinter::visit_objdecl(ObjDecl *x) {
   out() << x->name << _cmt0(x, 0);
   if (!x->args.empty()) {
      out() << "(";
      for (int i = 0; i < x->args.size(); i++) {
         if (i > 0) {
            out() << ", ";
         }
         out() << _cmt0(x, i+1);
         x->args[i]->visit(this);
      }
      out() << ")";
   }
}

void PrettyPrinter::visit_declstmt(DeclStmt* x) {
   x->type->visit(this);
   out() << " ";
   for (int i = 0; i < x->items.size(); i++) {
      if (i > 0) {
         out() << "," << _cmt_(x, i);
      }
      DeclStmt::Item& item = x->items[i];
      item.decl->visit(this);
      if (item.init) {
         out() << " = " << cmt0_(item.decl, -1);
         item.init->visit(this);
         
      }
   }
   out() << ";" << _cmt0(x, -1);
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
   out() << "if" << _cmt_(x, 0) << "(" << cmt0_(x, 1);
   x->cond->visit(this);
   out() << ")" << _cmt_(x, 2);
   x->then->visit(this);
   if (x->els) {
      if (!x->then->is<Block>()) {
         out() << endl;
         out(beginl);
      } else {
         out() << ' ';
      }
      out() << "else" << _cmt_(x, 3);
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
      out() << "while" << _cmt_(x, 0) << "(" << cmt0_(x, 1);
      x->cond->visit(this);
      out() << ")" << _cmt_(x, 2);
   }
   x->substmt->visit(this);
}

void PrettyPrinter::visit_jumpstmt(JumpStmt *x) {
   string keyword[3] = { "break", "continue", "goto" };
   out() << keyword[x->kind] << _cmt0(x, 0);
   if (x->kind == JumpStmt::Goto) {
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
   out() << " : " << cmt0_(x, 1);
   x->els->visit(this);
   if (x->paren) {
      out() << ")";
   }
}

void PrettyPrinter::visit_signexpr(SignExpr *x) {
   if (x->paren) {
      out() << "(";
   }
   out() << (x->kind == SignExpr::Positive ? "+" : "-");
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
      out() << (x->kind == IncrExpr::Positive ? "++" : "--");
      x->expr->visit(this);
   } else {
      x->expr->visit(this);
      out() << (x->kind == IncrExpr::Positive ? "++" : "--");
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
   int nc = 0;
   if (x->paren) {
      out() << "(" << cmt0_(x, nc++);
   }
   out() << "&" << _cmt0_(x, nc);
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

