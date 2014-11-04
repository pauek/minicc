#include <algorithm>
#include <sstream>
#include "ast.hh"
#include "prettypr.hh"
using namespace std;

// Comment helpers //////////////////////////////////////////////////

class CommentPrinter {
   AstNode *x;
   int i;
   bool was_empty, had_endl;
   PrettyPrinter *pr;

   CommentSeq *commseq() { 
      CommentSeq *c = (i < x->comments.size() ? x->comments[i++] : 0);
      was_empty = (c == 0);
      had_endl = (c != 0 ? c->has_endl() : false);
      return c;
   }
   string CMT(bool pre, bool post, bool _endl, bool missing);
public:
   CommentPrinter(AstNode *_x, PrettyPrinter *_pr) 
      : x(_x), pr(_pr), i(0), had_endl(false), was_empty(true) {}

   CommentSeq *next()    const { return (i < x->comments.size() ? x->comments[i] : 0); }
   bool last_had_endl()  const { return had_endl; }
   bool last_was_empty() const { return was_empty; }

   string _cmt ()  { return CMT(1, 0, 0, 0); }
   string _cmt_()  { return CMT(1, 1, 0, 0); }
   string  cmt_()  { return CMT(0, 1, 0, 0); }
   string  cmt ()  { return CMT(0, 0, 0, 0); }
   string _cmtl () { return CMT(1, 0, 1, 0); }
};

ostream& print_comment_seq(ostream& o, CommentSeq* C, string indentation) {
   if (C == 0) {
      return o;
   }
   for (int i = 0; i < C->items.size(); i++) {
      const Comment& c = C->items[i];
      switch (c.kind) {
      case Comment::none:
         break;
      case Comment::multiline:
         if (i > 0 and C->items[i-1].kind != Comment::endline) {
            o << ' ';
         }
         o << "/*" << c.text << "*/";
         break;
      case Comment::singleline:
         o << "//" << c.text;
         break;
      case Comment::endline:
         o << endl << indentation;
      }
   }
   return o;
}

string CommentPrinter::CMT(bool pre, bool post, bool _endl, bool missing) {
   CommentSeq *cn = commseq();
   ostringstream out;
   if (cn != 0 and !cn->items.empty()) {
      if (pre and !cn->starts_with_endl()) {
         out << " ";
      }
      print_comment_seq(out, cn, pr->indentation());
      if (_endl and !cn->has_endl()) {
         out << endl << pr->indentation();
      } else {
         out << (post ? " " : "");
      }
   } else {
      if (_endl) {
         out << endl << pr->indentation();
      } else if (missing) {
         out << ' ';
      }
   }
   return out.str();
}

// Pretty Printer //////////////////////////////////////////////////

void PrettyPrinter::visit_program(Program* x) {
   CommentPrinter cp(x, this);
   int i;
   for (i = 0; i < x->nodes.size(); i++) {
      out() << cp.cmt();
      AstNode *n = x->nodes[i];
      if ((!cp.last_was_empty() and !cp.last_had_endl()) or
          (i > 0 and n->is<FuncDecl>() and 
           (x->comments[i] and !x->comments[i]->ends_with_empty_line()))) {
         out() << endl;
      }
      n->visit(this);
      if (cp.next() and !cp.next()->starts_with_endl()) {
         out() << ' ';
      }
   }
   CommentSeq *last = cp.next();
   if (last) {
      last->only_one_endl_at_end();
   }
   out() << cp.cmt();
   if (last == 0 or !last->has_endl()) {
      out() << endl;
   }
}

void PrettyPrinter::visit_include(Include* x) {
   CommentPrinter cp(x, this);
   string delim = "\"\"";
   if (x->global) {
      delim = "<>";
   }
   out() << "#include " << cp.cmt_()
         << delim[0] << x->filename << delim[1];
}

void PrettyPrinter::visit_macro(Macro* x) {
   out() << "#" << x->macro;
}

void PrettyPrinter::visit_using(Using* x) {
   CommentPrinter cp(x, this);
   out() << "using " << cp.cmt_() << "namespace " << cp.cmt_()
         << x->namespc << cp._cmt() << ";" << cp.cmt();
}

void PrettyPrinter::visit_type(Type *x) {
   CommentPrinter cp(x, this);
   int i = 0, c = 0;
   for (int q : x->qual) {
      out() << Type::QualifiersNames[q] << " " << cp.cmt_();
   }
   x->id->visit(this);
   if (x->reference) {
      out() << cp._cmt_() << "&";
   }
}

void PrettyPrinter::visit_enumdecl(EnumDecl *x) {
   CommentPrinter cp(x, this);
   out() << "enum " << cp.cmt_() << x->name << cp._cmt() 
         << " { " << cp.cmt_();
   int cn = 3;
   for (int i = 0; i < x->values.size(); i++) {
      if (i > 0) {
         out() << ", " << cp.cmt_();
      }
      out() << x->values[i].id << cp._cmt();
      if (x->values[i].has_val) {
         out() << " = " << cp.cmt_() 
               << x->values[i].val << cp._cmt();
      }
   }
   out() << " };";
}

void PrettyPrinter::visit_typedefdecl(TypedefDecl *x) {
   CommentPrinter cp(x, this);
   out() << "typedef " << cp.cmt_();
   x->decl->type->visit(this);
   out() << " " << cp.cmt_();
   x->decl->visit(this);
   out() << ";" << cp._cmt();
}

void PrettyPrinter::visit_structdecl(StructDecl *x) {
   CommentPrinter cp(x, this);
   out() << "struct " << cp.cmt_();
   x->id->visit(this);
   out() << cp._cmt() << " {";
   indent(+1);
   vector<string> decl_strings;
   size_t max_size = 0;
   int nc = 3;
   for (DeclStmt *decl : x->decls) {
      push();
      decl->visit(this);
      string d = pop();
      max_size = std::max(d.size(), max_size);
      decl_strings.push_back(d);
   }
   for (int i = 0; i < decl_strings.size(); i++) {
      out() << cp._cmtl();
      string decl = decl_strings[i];
      string filler(max_size - decl.size(), ' ');
      out() << decl << filler;
   }
   indent(-1);
   out() << cp._cmtl();
   out() << "}" << cp._cmt() << ";";
}

void PrettyPrinter::visit_funcdecl(FuncDecl *x) {
   CommentPrinter cp(x, this);
   visit_type(x->return_type);
   out() << " " << cp.cmt_();
   x->id->visit(this);
   out() << cp._cmt_();
   if (x->params.empty()) {
      out() << "(" << cp.cmt() << ")";
   } else {
      out() << "(";
      for (int i = 0; i < x->params.size(); i++) {
         if (i > 0) {
            out() << ", ";
         }
         out() << cp.cmt_();
         visit_type(x->params[i]->type);
         out() << " " << cp.cmt_();
         out() << x->params[i]->name;
         out() << cp._cmt();
      }
      out() << ")";
   }
   if (cp.next()) {
      cp.next()->remove_endls();
   }
   if (x->block) {
      out() << " " << cp.cmt_();
      x->block->visit(this);
   } else {
      out() << cp._cmt() << ";";
   }
}

void PrettyPrinter::print_block(Block *x) {
   CommentPrinter cp(x, this);
   if (x->stmts.empty()) {
      out() << "{" << cp._cmt_() << "}";
      return;
   } 
   indent(+1);
   out() << "{";
   for (Stmt *s : x->stmts) {
      out() << cp._cmtl();
      s->visit(this);
   }
   indent(-1);
   out() << cp._cmtl();
   out() << "}" << cp._cmt();
}

void PrettyPrinter::visit_ident(Ident *x) {
   CommentPrinter cp(x, this);
   for (Ident *pre : x->prefix) {
      pre->visit(this);
      out() << cp._cmt_() << "::" << cp._cmt_();
   }
   out() << x->id;
   if (!x->subtypes.empty()) {
      out() << cp._cmt_() << "<" << cp.cmt_();
      for (int i = 0; i < x->subtypes.size(); i++) {
         if (i > 0) {
            out() << ", " << cp.cmt_();
         }
         x->subtypes[i]->visit(this);
         out() << cp._cmt();
      }
      out() << ">";
   }
}

void PrettyPrinter::visit_literal(Literal *x) {
   CommentPrinter cp(x, this);
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
   out() << cp._cmt();
   if (x->paren) {
      out() << ")";
   }
}

void PrettyPrinter::visit_binaryexpr(BinaryExpr *x) {
   CommentPrinter cp(x, this);
   int nc = 0;
   if (x->paren) {
      out() << "(" << cp.cmt_();
   }
   x->left->visit(this);
   if (x->op != ",") {
      out() << cp._cmt() << " ";
   }
   out() << x->op << " " << cp.cmt_();
   x->right->visit(this);

   if (x->paren) {
      out() << cp._cmt() << ")";
   }
   out() << cp._cmt();
}

void PrettyPrinter::visit_block(Block *x) {
   print_block(x);
}

void PrettyPrinter::visit_vardecl(VarDecl *x) {
   CommentPrinter cp(x, this);
   if (x->kind == Decl::Pointer) {
      out() << "*";
   }
   out() << x->name << cp._cmt();
}

void PrettyPrinter::visit_exprlist(ExprList *x) {
   CommentPrinter cp(x, this);
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
   CommentPrinter cp(x, this);
   out() << x->name << cp._cmt_();
   out() << "[" << cp.cmt_();
   x->size->visit(this);
   out() << "]" << cp._cmt();
}

void PrettyPrinter::visit_objdecl(ObjDecl *x) {
   CommentPrinter cp(x, this);
   out() << x->name << cp._cmt();
   if (!x->args.empty()) {
      out() << "(";
      for (int i = 0; i < x->args.size(); i++) {
         if (i > 0) {
            out() << ", ";
         }
         out() << cp.cmt_();
         x->args[i]->visit(this);
      }
      out() << ")";
   }
}

void PrettyPrinter::visit_declstmt(DeclStmt* x) {
   CommentPrinter cp(x, this);
   x->type->visit(this);
   out() << " " << cp.cmt_();
   for (int i = 0; i < x->items.size(); i++) {
      if (i > 0) {
         out() << ", " << cp.cmt_();
      }
      DeclStmt::Item& item = x->items[i];
      item.decl->visit(this);
      if (item.init) {
         out() << " = " << cp.cmt_();
         item.init->visit(this);
      }
   }
   out() << ";";
}

void PrettyPrinter::visit_exprstmt(ExprStmt* x) {
   CommentPrinter cp(x, this);
   int c = 0;
   if (x->is_return) {
      out() << "return " << cp.cmt_();
   }
   if (x->expr) {
      x->expr->visit(this);
   }
   out() << cp._cmt() << ";";
}

void PrettyPrinter::visit_ifstmt(IfStmt *x) {
   CommentPrinter cp(x, this);
   out() << "if " << cp.cmt_() << "(" << cp.cmt_();
   x->cond->visit(this);
   out() << ") " << cp.cmt_();
   x->then->visit(this);
   if (x->els) {
      out() << cp._cmt();
      if (!cp.last_had_endl()) {
         out() << " ";
      }
      out() << "else " << cp.cmt_();
      x->els->visit(this);
   }
}

void PrettyPrinter::visit_iterstmt(IterStmt *x) {
   CommentPrinter cp(x, this);
   if (x->is_for()) {
      out() << "for " << cp.cmt_() << "(";
      x->init->visit(this);
      out() << " ";
      x->cond->visit(this);
      out() << "; ";
      x->post->visit(this);
      out() << ")";
   } else {
      out() << "while " << cp.cmt_() << "(" << cp.cmt_();
      x->cond->visit(this);
      out() << cp._cmt() << ")";
   }
   out() << cp._cmt();
   if (!cp.last_had_endl()) {
      out() << " ";
   }
   if (!x->substmt->is<Block>() and cp.last_had_endl()) {
      out() << indentation();
   } 
   x->substmt->visit(this);
}

void PrettyPrinter::visit_jumpstmt(JumpStmt *x) {
   CommentPrinter cp(x, this);
   string keyword[3] = { "break", "continue", "goto" };
   out() << keyword[x->kind] << cp._cmt();
   if (x->kind == JumpStmt::Goto) {
      out() << " " << x->label << cp._cmt() << ";" << cp._cmt();
   } else {
      out() << ";" << cp._cmt();
   }
}

void PrettyPrinter::visit_callexpr(CallExpr *x) {
   CommentPrinter cp(x, this);
   if (x->paren) {
      out() << "(";
   }
   x->func->visit(this);
   if (cp.next() and cp.next()->ends_with_endl()) {
      out() << " ";
   }
   out() << cp._cmt_() << "(";
   for (int i = 0; i < x->args.size(); i++) {
      if (i > 0) {
         out() << ", " << cp.cmt_();
      }
      out() << cp.cmt_();
      x->args[i]->visit(this);
   }
   out() << ")";
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
   CommentPrinter cp(x, this);
   if (x->paren) {
      out() << "(";
   }
   x->cond->visit(this);
   out() << cp._cmt() << " ? " << cp.cmt_();
   x->then->visit(this);
   out() << cp._cmt() << " : " << cp.cmt_();
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
   CommentPrinter cp(x, this);
   if (x->paren) {
      out() << "(";
   }
   if (x->preincr) {
      out() << (x->kind == IncrExpr::Positive ? "++" : "--") << cp._cmt_();
      x->expr->visit(this);
   } else {
      x->expr->visit(this);
      out() << (x->kind == IncrExpr::Positive ? "++" : "--") << cp._cmt();
   }
   if (x->paren) {
      out() << ")";
   }
}

void PrettyPrinter::visit_negexpr(NegExpr *x) {
   CommentPrinter cp(x, this);
   if (x->paren) {
      out() << "(";
   }
   out() << "!" << cp._cmt_();
   x->expr->visit(this);
   if (x->paren) {
      out() << ")";
   }
}

void PrettyPrinter::visit_addrexpr(AddrExpr *x) {
   CommentPrinter cp(x, this);
   int nc = 0;
   if (x->paren) {
      out() << "(" << cp.cmt_();
   }
   out() << "&" << cp._cmt_();
   x->expr->visit(this);
   if (x->paren) {
      out() << ")";
   }
}

void PrettyPrinter::visit_derefexpr(DerefExpr *x) {
   CommentPrinter cp(x, this);
   if (x->paren) {
      out() << "(";
   }
   out() << "*" << cp._cmt_();
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

