
#include <algorithm>
#include <sstream>
#include "ast.hh"
#include "prettypr.hh"
#include "cast.h"
using namespace std;

struct OutputWriter {
   OutputWriter(ostream& out = std::cout) : indent_(0), out_(out) {}

   void Indent() { indent_ += 3; }
   void Dedent() { indent_ -= 3; }

   template<typename T>
   void Write(const T& t) { out_ << t; }

   template<typename T1, typename T2>
   void Write(const T1& t1, const T2& t2) { out_ << t1 << t2; }

   template<typename T1, typename T2, typename T3>
   void Write(const T1& t1, const T2& t2, const T3& t3) { 
      out_ << t1 << t2 << t3; 
   }

   template<typename T1, typename T2, typename T3, typename T4>
   void Write(const T1& t1, const T2& t2, const T3& t3, const T4& t4) { 
      out_ << t1 << t2 << t3 << t4; 
   }

   template<typename T1, typename T2, typename T3, typename T4, typename T5>
   void Write(const T1& t1, const T2& t2, const T3& t3, const T4& t4, 
              const T5& t5) 
   {
      out_ << t1 << t2 << t3 << t4 < t5; 
   }

   void EndLine() { out_ << endl; }
   void BeginLine() { out_ << string(indent_, ' '); }

   void Line(string s) { BeginLine(); Write(s); EndLine(); }

   std::string Indentation() const { 
      return std::string(indent_, ' ');
   }

private:
   int indent_;
   ostream& out_;
};

class CommentPrinter {
   Ast *x;
   int i;
   bool was_empty, had_endl;
   OutputWriter& out_;

   CommentSeq *get_comment_seq() { 
      CommentSeq *c = (i < x->comments.size() ? x->comments[i] : 0);
      was_empty = (c == 0);
      had_endl = (c != 0 ? c->has_endl() : false);
      i++;
      return c;
   }
   void CMT(bool pre, bool post, bool _endl);
public:
   CommentPrinter(Ast *_x, OutputWriter& wr) 
      : x(_x), out_(wr), i(0), had_endl(false), was_empty(true) {}

   CommentSeq *next()    const { return (i < x->comments.size() ? x->comments[i] : 0); }
   bool last_had_endl()  const { return had_endl; }
   bool last_was_empty() const { return was_empty; }

   void _cmt ()  { return CMT(1, 0, 0); }
   void _cmt_()  { return CMT(1, 1, 0); }
   void  cmt_()  { return CMT(0, 1, 0); }
   void  cmt ()  { return CMT(0, 0, 0); }
   void _cmtl()  { return CMT(1, 0, 1); }
};

void CommentPrinter::CMT(bool pre, bool post, bool _endl) {
   CommentSeq *comm_seq = get_comment_seq();
   ostringstream out;
   if (comm_seq != 0 and !comm_seq->items.empty()) {
      if (pre and !comm_seq->starts_with_endl()) {
         out_.Write(' ');
      }
      if (comm_seq != 0) {
         for (int i = 0; i < comm_seq->items.size(); i++) {
            const Comment& c = comm_seq->items[i];
            switch (c.kind) {
            case Comment::none:
               break;
            case Comment::multiline:
               if (i > 0 and comm_seq->items[i-1].kind != Comment::endline) {
                  out_.Write(' ');
               }
               out_.Write("/*", c.text, "*/");
               break;
            case Comment::singleline:
               out_.Write("//", c.text);
               break;
            case Comment::endline:
               out_.EndLine();
               out_.BeginLine();
               break;
            }
         }
      }
      if (_endl and !comm_seq->has_endl()) {
         out_.EndLine();
         out_.BeginLine();
      } else if (!_endl and post) {
         out_.Write(' ');
      }
   } 
   else if (_endl) {
      out_.EndLine();
      out_.BeginLine();
   }
}

struct PrettyPrinter {
   OutputWriter out;

   PrettyPrinter(ostream& o) : out(o) {}
   void Print(Ast *ast);
};

void PrettyPrinter::Print(Ast* ast) {
   assert(ast != nullptr);
   switch (ast->type()) {
   case AstType::Program: {
      Program *X = cast<Program>(ast);
      CommentPrinter cp(X, out);
      int i;
      for (i = 0; i < X->nodes.size(); i++) {
         cp.cmt();
         Ast *n = X->nodes[i];
         if ((!cp.last_was_empty() and !cp.last_had_endl()) or
             (i > 0 and isa<FuncDecl>(n) and 
              (X->comments[i] and !X->comments[i]->ends_with_empty_line()))) {
            out.EndLine();
         }
         Print(n);
         if (cp.next() and !cp.next()->starts_with_endl()) {
            out.Write(' ');
         }
      }
      CommentSeq *last = cp.next();
      if (last) {
         last->only_one_endl_at_end();
      }
      cp.cmt();
      if (last == 0 or !last->has_endl()) {
         out.EndLine();
      }      
      break;
   }
   case AstType::Include: {
      Include *X = cast<Include>(ast);
      CommentPrinter cp(X, out);
      string delim = (X-> global ? "<>" : "\"\"");
      out.Write("#include ");
      cp.cmt_();
      out.Write(delim[0], X->filename, delim[1]);
      break;
   }
   case AstType::Macro: {
      Macro *X = cast<Macro>(ast);
      out.Write("#", X->macro);
      break;
   }
   case AstType::Using: {
      Using *X = cast<Using>(ast);
      CommentPrinter cp(X, out);
      // WARNING: g++ here optimizes and changes order of instructions!!!
      out.Write("using ");
      cp.cmt_();
      out.Write("namespace "); 
      cp.cmt_();
      out.Write(X->namespc);
      cp._cmt();
      out.Write(";");
      cp.cmt();
      break;
   }
   case AstType::TypeSpec: {
      TypeSpec *X = cast<TypeSpec>(ast);
      CommentPrinter cp(X, out);
      for (int q : X->qual) {
         out.Write(TypeSpec::QualifiersNames[q], " ");
         cp.cmt_();
      }
      Print(X->id);
      if (X->reference) {
         cp._cmt_();
         out.Write("&");
      }
      break;
   }
   case AstType::EnumDecl: {
      EnumDecl *X = cast<EnumDecl>(ast);
      CommentPrinter cp(X, out);
      out.Write("enum ");
      cp.cmt_();
      out.Write(X->name);
      cp._cmt();
      out.Write(" { ");
      cp.cmt_();
      for (int i = 0; i < X->values.size(); i++) {
         if (i > 0) {
            out.Write(", ");
            cp.cmt_();
         }
         out.Write(X->values[i].id);
         cp._cmt();
         if (X->values[i].has_val) {
            out.Write(" = ");
            cp.cmt_();
            out.Write(X->values[i].val);
            cp._cmt();
         }
      }
      out.Write(" };");
      break;
   }
   case AstType::TypedefDecl: {
      TypedefDecl *X = cast<TypedefDecl>(ast);
      CommentPrinter cp(X, out);
      out.Write("typedef ");
      cp.cmt_();
      Print(X->decl->typespec);
      out.Write(" ");
      cp.cmt_();
      Print(X->decl);
      out.Write(";");
      cp._cmt();
      break;
   }
   case AstType::StructDecl: {
      StructDecl *X = cast<StructDecl>(ast);
      CommentPrinter cp(X, out);
      // WARNING: g++ here optimizes and changes order of instructions!!!
      out.Write("struct ");
      cp.cmt_();
      Print(X->id);
      // WARNING: g++ here optimizes and changes order of instructions!!!
      cp._cmt();
      out.Write(" {");
      out.Indent();
      vector<string> decl_strings;
      // TODO: Alinear comentarios y nombres de variable verticalmente!
      for (DeclStmt *decl : X->decls) {
         cp._cmtl();
         Print(decl);
      }
      out.Dedent();
      // WARNING: g++ here optimizes and changes order of instructions!!!
      cp._cmtl();
      out.Write("}");
      cp._cmt();
      out.Write(";");
      break;
   }
   case AstType::FuncDecl: {
      FuncDecl *X = cast<FuncDecl>(ast);
      CommentPrinter cp(X, out);

      Print(X->return_typespec);
      // WARNING: g++ here optimizes and changes order of instructions!!!
      out.Write(" ");
      cp.cmt_();
      Print(X->id);
      cp._cmt_();
      if (X->params.empty()) {
         // WARNING: g++ here optimizes and changes order of instructions!!!
         out.Write("(");
         cp.cmt();
         out.Write(")");
      } else {
         out.Write("(");
         for (int i = 0; i < X->params.size(); i++) {
            if (i > 0) {
               out.Write(", ");
            }
            cp.cmt_();
            Print(X->params[i]->typespec);
            // WARNING: g++ here optimizes and changes order of instructions!!!
            out.Write(" ");
            cp.cmt_();
            out.Write(X->params[i]->name);
            cp._cmt();
         }
         out.Write(")");
      }
      if (cp.next()) {
         cp.next()->remove_endls();
      }
      if (X->block) {
         // WARNING: g++ here optimizes and changes order of instructions!!!
         out.Write(" ");
         cp.cmt_();
         Print(X->block);
      } else {
         // WARNING: g++ here optimizes and changes order of instructions!!!
         cp._cmt();
         out.Write(";");
      }
      break;
   }
   case AstType::Block: {
      Block *X = cast<Block>(ast);
      CommentPrinter cp(X, out);
      if (X->stmts.empty()) {
         out.Write("{");
         if (cp.next() and cp.next()->has_endl()) {
            cp._cmt();
         } else {
            cp._cmt_();
         }
         out.Write("}");
         return;
      }
      out.Indent();
      out.Write("{");
      for (Stmt *stmt : X->stmts) {
         cp._cmtl();
         Print(stmt);
      }
      out.Dedent();
      cp._cmtl();
      out.Write("}");
      cp._cmt();
      break;
   }
   case AstType::SimpleIdent: {
      SimpleIdent *X = cast<SimpleIdent>(ast);
      out.Write(X->name);
      break;
   }
   case AstType::FullIdent: {
      FullIdent *X = cast<FullIdent>(ast);
      CommentPrinter cp(X, out);
      for (TemplateIdent *pre : X->prefix) {
         Print(pre);
         // WARNING: g++ here optimizes and changes order of instructions!!!
         cp._cmt_();
         out.Write("::");
         cp._cmt_();
      }
      // ----> FALLTHROUGH!!!! <-----
      // ----> FALLTHROUGH!!!! <-----
      // ----> FALLTHROUGH!!!! <-----
   }   
   case AstType::TemplateIdent: {
      TemplateIdent *X = cast<TemplateIdent>(ast);
      CommentPrinter cp(X, out);
      out.Write(X->name);
      if (!X->subtypes.empty()) {
         // WARNING: g++ here optimizes and changes order of instructions!!!
         cp._cmt_();
         out.Write("<");
         cp.cmt_();
         for (int i = 0; i < X->subtypes.size(); i++) {
            if (i > 0) {
               // WARNING: g++ here optimizes and changes order of instructions!!!
               out.Write(", ");
               cp.cmt_();
            }
            Print(X->subtypes[i]);
            cp._cmt();
         }
         out.Write(">");
      }
      break;
   }
   case AstType::Literal: {
      Literal *X = cast<Literal>(ast);
      CommentPrinter cp(X, out);
      if (X->paren) {
         out.Write("(");
      } 
      switch (X->kind) {
      case Literal::Bool:   out.Write(X->val.as_bool ? "true" : "false"); break;
      case Literal::Int:    out.Write(X->val.as_int); break;
      case Literal::Double: out.Write(X->val.as_double); break;
      case Literal::String: 
         out.Write('"', Literal::escape(*X->val.as_string.s, '"'), '"'); 
         break;
      case Literal::Char: {
         string ch(1, X->val.as_char);
         out.Write("'", Literal::escape(ch, '\''), "'"); 
         break;
      }
      default:
         out.Write("<literal>"); 
         break;
      }
      cp._cmt();
      if (X->paren) {
         out.Write(")");
      }
      break;
   }
   case AstType::BinaryExpr: {
      BinaryExpr *X = cast<BinaryExpr>(ast);
      CommentPrinter cp(X, out);
      if (X->paren) {
         // WARNING: g++ here optimizes and changes order of instructions!!!
         out.Write("(");
         cp.cmt_();
      }
      Print(X->left);
      if (X->op != ",") {
         // WARNING: g++ here optimizes and changes order of instructions!!!
         cp._cmt();
         out.Write(" ");
      }
      // WARNING: g++ here optimizes and changes order of instructions!!!
      out.Write(X->op, " ");
      cp.cmt_();
      Print(X->right);
      if (X->paren) {
         cp._cmt();
         out.Write(")");
      }
      cp._cmt();
      break;
   }
   case AstType::VarDecl: {
      VarDecl *X = cast<VarDecl>(ast);
      CommentPrinter cp(X, out);
      if (X->kind == Decl::Pointer) {
         out.Write("*");
      }
      // WARNING: g++ here optimizes and changes order of instructions!!!
      out.Write(X->name);
      cp._cmt();
      break;
   }
   case AstType::ExprList: {
      ExprList *X = cast<ExprList>(ast);
      CommentPrinter cp(X, out);
      out.Write("{");
      for (int i = 0; i < X->exprs.size(); i++) {
         if (i > 0) {
            out.Write(", ");
         }
         Print(X->exprs[i]);
      }
      out.Write("}");
      break;
   }
   case AstType::ArrayDecl: {
      ArrayDecl *X = cast<ArrayDecl>(ast);
      CommentPrinter cp(X, out);
      out.Write(X->name);
      cp._cmt_();
      for (int i = 0; i < X->sizes.size(); i++) {
         out.Write("[");
         cp.cmt_();
         Print(X->sizes[i]);
         out.Write("]");
         cp._cmt();
      }
      break;
   }
   case AstType::ObjDecl: {
      ObjDecl *X = cast<ObjDecl>(ast);
      CommentPrinter cp(X, out);
      out.Write(X->name);
      cp._cmt();
      if (!X->args.empty()) {
         out.Write("(");
         for (int i = 0; i < X->args.size(); i++) {
            if (i > 0) {
               out.Write(", ");
            }
            cp.cmt_();
            Print(X->args[i]);
         }
         out.Write(")");
      }
      break;
   }
   case AstType::DeclStmt: {
      DeclStmt *X = cast<DeclStmt>(ast);
      CommentPrinter cp(X, out);
      Print(X->typespec);
      out.Write(" ");
      cp.cmt_();
      for (int i = 0; i < X->items.size(); i++) {
         if (i > 0) {
            out.Write(", ");
            cp.cmt_();
         }
         DeclStmt::Item& item = X->items[i];
         Print(item.decl);
         if (item.init) {
            out.Write(" = ");
            cp.cmt_();
            Print(item.init);
         }
      }
      out.Write(";");
      break;
   }
   case AstType::ExprStmt: {
      ExprStmt *X = cast<ExprStmt>(ast);
      CommentPrinter cp(X, out);
      if (X->is_return) {
         out.Write("return ");
         cp.cmt_();
      }
      if (X->expr) {
         Print(X->expr);
      }
      cp._cmt();
      out.Write(";");
      break;
   }
   case AstType::IfStmt: {
      IfStmt *X = cast<IfStmt>(ast);
      CommentPrinter cp(X, out);
      out.Write("if ");
      cp.cmt_();
      out.Write("(");
      cp.cmt_();
      Print(X->cond);
      out.Write(") ");
      cp.cmt_();
      Print(X->then);
      if (X->els) {
         cp._cmt();
         if (!cp.last_had_endl()) {
            out.Write(" ");
         }
         out.Write("else ");
         cp.cmt_();
         Print(X->els);
      }
      break;
   }
   case AstType::ForStmt: {
      ForStmt *X = cast<ForStmt>(ast);
      CommentPrinter cp(X, out);
      out.Write("for ");
      cp.cmt_();
      out.Write("(");
      if (X->init) {
         Print(X->init);
      }
      out.Write(" ");
      if (X->cond) {
         Print(X->cond);
      }
      out.Write("; ");
      if (X->post) {
         Print(X->post);
      }
      out.Write(")");
      cp._cmt();
      if (!cp.last_had_endl()) {
         out.Write(" ");
      }

      if (!isa<Block>(X->substmt) and cp.last_had_endl()) {
         out.Write(out.Indentation());
      }
      Print(X->substmt);
      break;
   }
   case AstType::WhileStmt: {
      WhileStmt *X = cast<WhileStmt>(ast);
      CommentPrinter cp(X, out);
      // WARNING: g++ here optimizes and changes order of instructions!!!
      out.Write("while ");
      cp.cmt_();
      out.Write("(");
      cp.cmt_();
      Print(X->cond);
      // WARNING: g++ here optimizes and changes order of instructions!!!
      cp._cmt();
      out.Write(")");
      cp._cmt();
      if (!cp.last_had_endl()) {
         out.Write(" ");
      }
      if (!isa<Block>(X->substmt) and cp.last_had_endl()) {
         out.Write(out.Indentation());
      }
      Print(X->substmt);
      break;
   }
   case AstType::JumpStmt: {
      JumpStmt *X = cast<JumpStmt>(ast);
      CommentPrinter cp(X, out);
      string keyword[3] = { "break", "continue", "goto" };
      out.Write(keyword[X->kind]);
      cp._cmt();
      if (X->kind == JumpStmt::Goto) {
         // WARNING: g++ here optimizes and changes order of instructions!!!
         out.Write(" ", X->label);
         cp._cmt();
      }
      // WARNING: g++ here optimizes and changes order of instructions!!!
      out.Write(";");
      cp._cmt();
      break;
   }
   case AstType::CallExpr: {
      CallExpr *X = cast<CallExpr>(ast);
      CommentPrinter cp(X, out);
      if (X->paren) {
         out.Write("(");
      }
      Print(X->func);
      if (cp.next() and cp.next()->ends_with_endl()) {
         out.Write(" ");
      }
      cp._cmt_();
      out.Write("(");
      for (int i = 0; i < X->args.size(); i++) {
         if (i > 0) {
            out.Write(", ");
            cp.cmt_();
         }
         cp.cmt_();
         Print(X->args[i]);
      }
      out.Write(")");
      if (X->paren) {
         out.Write(")");
      }
      break;
   }
   case AstType::IndexExpr: {
      IndexExpr *X = cast<IndexExpr>(ast);
      if (X->paren) {
         out.Write("(");
      }
      Print(X->base);
      out.Write("[");
      Print(X->index);
      out.Write("]");
      if (X->paren) {
         out.Write(")");
      }
      break;
   }
   case AstType::FieldExpr: {
      FieldExpr *X = cast<FieldExpr>(ast);
      if (X->paren) {
         out.Write("(");
      }
      Print(X->base);
      out.Write(X->pointer ? "->" : ".");
      Print(X->field);
      if (X->paren) {
         out.Write(")");
      }
      break;
   }
   case AstType::CondExpr: {
      CondExpr *X = cast<CondExpr>(ast);
      CommentPrinter cp(X, out);
      if (X->paren) {
         out.Write("(");
      }
      Print(X->cond);
      // WARNING: g++ here optimizes and changes order of instructions!!!
      cp._cmt();
      out.Write(" ? ");
      cp.cmt_();
      Print(X->then);
      // WARNING: g++ here optimizes and changes order of instructions!!!
      cp._cmt();
      out.Write(" : ");
      cp.cmt_();
      Print(X->els);
      if (X->paren) {
         out.Write(")");
      }
      break;
   }
   case AstType::SignExpr: {
      SignExpr *X = cast<SignExpr>(ast);
      if (X->paren) {
         out.Write("(");
      }
      out.Write(X->kind == SignExpr::Positive ? "+" : "-");
      Print(X->expr);
      if (X->paren) {
         out.Write(")");
      }
      break;
   }
   case AstType::IncrExpr: {
      IncrExpr *X = cast<IncrExpr>(ast);
      CommentPrinter cp(X, out);
      if (X->paren) {
         out.Write("(");
      }
      if (X->preincr) {
         out.Write(X->kind == IncrExpr::Positive ? "++" : "--");
         cp._cmt_();
         Print(X->expr);
      } else {
         Print(X->expr);
         out.Write(X->kind == IncrExpr::Positive ? "++" : "--");
         cp._cmt_();
      }
      if (X->paren) {
         out.Write(")");
      }
      break;
   }
   case AstType::NegExpr: {
      NegExpr *X = cast<NegExpr>(ast);
      CommentPrinter cp(X, out);
      if (X->paren) {
         out.Write("(");
      }
      out.Write("!");
      cp._cmt_();
      Print(X->expr);
      if (X->paren) {
         out.Write(")");
      }
      break;
   }
   case AstType::AddrExpr: {
      AddrExpr *X = cast<AddrExpr>(ast);
      CommentPrinter cp(X, out);
      if (X->paren) {
         out.Write("(");
         cp.cmt_();
      }
      // WARNING: g++ here optimizes and changes order of instructions!!!
      out.Write("&");
      cp._cmt_();
      Print(X->expr);
      if (X->paren) {
         out.Write(")");
      }
      break;
   }
   case AstType::DerefExpr: {
      DerefExpr *X = cast<DerefExpr>(ast);
      CommentPrinter cp(X, out);
      if (X->paren) {
         out.Write("(");
      }
      out.Write("*");
      cp._cmt_();
      Print(X->expr);
      if (X->paren) {
         out.Write(")");
      }
      break;
   }
   default:
      out.Write("<node>");
      break;
   }
}

void PrettyPrint(Ast *ast, ostream& out) {
   PrettyPrinter(out).Print(ast);
}
