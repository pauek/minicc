#include <algorithm>
#include <sstream>
#include "ast.hh"
#include "astpr.hh"
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

   void EndLine() { out_ << endl; }
   void BeginLine() { out_ << string(indent_, ' '); }

   void Line(string s) { BeginLine(); Write(s); EndLine(); }

private:
   int indent_;
   ostream& out_;
};

struct AstPrinter {
   OutputWriter out;
   AstPrinter(ostream& o) : out(o) {}
   void Print(Ast *ast);
};

void AstPrinter::Print(Ast* ast) {
   assert(ast != nullptr);
   switch (ast->Type()) {
   case AstType::Program: {
      Program *X = cast<Program>(ast);
      out.Line("Program{");
      out.Indent();
      for (Ast *child : X->nodes) {
         out.BeginLine();
         Print(child);
         out.EndLine();
      }
      out.Dedent();
      out.Line("}");
      break;
   }
   case AstType::Include: {
      Include *X = cast<Include>(ast);
      string D = (X->global ? "<>" : "\"\"");
      out.Write("Include(");
      out.Write(D[0], X->filename, D[1]);
      out.Write(")");
      break;
   }
   case AstType::Macro: {
      Macro *X = cast<Macro>(ast);
      out.Write("Macro(", X->macro, ")");
      break;
   }
   case AstType::Using: {
      Using *X = cast<Using>(ast);
      out.Write("Using(", X->namespc, ")");
      break;
   }
   case AstType::TypeSpec: {
      TypeSpec *X = cast<TypeSpec>(ast);
      out.Write("Type");
      if (X->reference) {
         out.Write("<&>");
      }
      out.Write("(");
      Print(X->id);
      if (X->HasQualifiers()) {
         int count = 0;
         out.Write(", {");

#define QUALIFIER(qual, str) \
         if (X->bqual & TypeSpec::qual) {   \
            if (count > 0) out.Write(", "); \
            out.Write(str);                 \
            count++;                        \
         }
         QUALIFIER(Const,    "const")
         QUALIFIER(Volatile, "volatile")
         QUALIFIER(Mutable,  "mutable")
         QUALIFIER(Register, "register")
         QUALIFIER(Auto,     "auto")
         QUALIFIER(Extern,   "extern")
#undef QUALIFIER

         out.Write("}");
      }
      out.Write(")");
      break;
   }
   case AstType::EnumDecl: {
      EnumDecl *X = cast<EnumDecl>(ast);
      out.Write("EnumDecl(\"", X->name, "\", {");
      for (int i = 0; i < X->values.size(); i++) {
         if (i > 0) {
            out.Write(", ");
         }
         out.Write('"', X->values[i].id, '"');
         if (X->values[i].has_val) {
            out.Write(" = ", X->values[i].val);
         }
      }
      out.Write("})");
      break;
   }
   case AstType::TypedefDecl: {
      TypedefDecl *X = cast<TypedefDecl>(ast);
      out.Write("TypedefDecl(\"", X->decl->name, "\" = ");
      Print(X->decl->typespec);
      out.Write(")");
      break;
   }
   case AstType::StructDecl: {
      StructDecl *X = cast<StructDecl>(ast);
      out.Write("StructDecl('", X->name, "', {");
      out.EndLine();
      out.Indent();
      for (DeclStmt *decl : X->decls) {
         out.BeginLine();
         Print(decl);
         out.EndLine();
      }
      out.Dedent();
      out.BeginLine();
      out.Write("})");
      break;
   }
   case AstType::FuncDecl: {
      FuncDecl *X = cast<FuncDecl>(ast);
      out.Write("FuncDecl(");
      Print(X->id);
      out.Write(", ");
      Print(X->return_typespec);
      out.Write(", Params = {");
      for (int i = 0; i < X->params.size(); i++) {
         if (i > 0) {
            out.Write(", ");
         }
         out.Write('"', X->params[i]->name, "\": ");
         Print(X->params[i]->typespec);
      }
      if (X->block) {
         out.Write("}, {");
         out.EndLine();
         out.Indent();
         out.BeginLine();
         Print(X->block);
         out.EndLine();
         out.Dedent();
         out.BeginLine();
      }
      out.Write("})");
      break;
   }
   case AstType::Block: {
      Block *X = cast<Block>(ast);
      out.Write("Block(");
      if (X->stmts.empty()) {
         out.Write("{})");
         break;
      }
      out.Write("{");
      out.EndLine();
      out.Indent();
      for (Stmt *stmt : X->stmts) {
         out.BeginLine();
         Print(stmt);
         out.EndLine();
      }
      out.Dedent();
      out.BeginLine();
      out.Write("})");
      break;
   }
   case AstType::Identifier: {
      Identifier *X = cast<Identifier>(ast);
      out.Write("id:");
      if (!X->prefix.empty()) {
         out.Write("[");
         for (int i = 0; i < X->prefix.size(); i++) {
            if (i > 0) {
               out.Write(", ");
            }
            Print(X->prefix[i]);
         }
         out.Write("]");
      }
      out.Write("'", X->name, "'");
      if (!X->subtypes.empty()) {
         out.Write("<");
         for (int i = 0; i < X->subtypes.size(); i++) {
            if (i > 0) {
               out.Write(", ");
            }
            Print(X->subtypes[i]);
         }
         out.Write(">");
      }
      break;
   }
   case AstType::Literal: {
      Literal *X = cast<Literal>(ast);
      if (X->paren) out.Write("(");
      switch (X->kind) {
      case Literal::Bool: 
         out.Write("Bool<", X->val.as_bool ? "true" : "false", ">");
         break;
      case Literal::Char: 
         out.Write("Char<", Literal::Escape(X->val.as_char, '\''), ">");
         break;
      case Literal::Int: 
         out.Write("Int<", X->val.as_int, ">");
         break;
      case Literal::Float:
         out.Write("Float<", X->val.as_double, ">");
         break;
      case Literal::Double:
         out.Write("Double<", X->val.as_double, ">");
         break;
      case Literal::String:
         out.Write("String<", Literal::Escape(*(X->val.as_string.s), '"'), ">");
         break;
      default:
         out.Write("Literal<>");
         break;
      }
      if (X->paren) out.Write(")");
      break;
   }
   case AstType::BinaryExpr: {
      BinaryExpr *X = cast<BinaryExpr>(ast);
      if (X->paren) out.Write("(");
      out.Write(X->op, "(");
      Print(X->left);
      out.Write(", ");
      Print(X->right);
      out.Write(")");
      if (X->paren) out.Write(")");
      break;
   }
   case AstType::VarDecl: {
      VarDecl *X = cast<VarDecl>(ast);
      if (X->kind == Decl::Pointer) {
         out.Write("*");
      }
      out.Write('"', X->name, '"');
      break;
   }
   case AstType::ArrayDecl: {
      ArrayDecl *X = cast<ArrayDecl>(ast);
      out.Write('"', X->name, "\"(");
      if (X->sizes.size() == 1) {
         out.Write("Size = ");
         Print(X->sizes[0]);
      } else {
         out.Write("Sizes = {");
         for (int i = 0; i < X->sizes.size(); i++) {
            if (i > 0) {
               out.Write(", ");
            }
            Print(X->sizes[i]);
         }
         out.Write("}");
      }
      out.Write(")");
      break;
   }
   case AstType::ExprList: {
      ExprList *X = cast<ExprList>(ast);
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
   case AstType::ObjDecl: {
      ObjDecl *X = cast<ObjDecl>(ast);
      out.Write('"', X->name, "\"(");
      if (!X->args.empty()) {
         out.Write("Args = {");
         for (int i = 0; i < X->args.size(); i++) {
            if (i > 0) {
               out.Write(", ");
            }
            Print(X->args[i]);
         }
         out.Write("}");
      }
      out.Write(")");
      break;
   }
   case AstType::DeclStmt: {
      DeclStmt *X = cast<DeclStmt>(ast);
      out.Write("DeclStmt(");
      Print(X->typespec);
      out.Write(", Vars = {");
      for (int i = 0; i < X->items.size(); i++) {
         if (i > 0) {
            out.Write(", ");
         }
         Print(X->items[i].decl);
         if (X->items[i].init) {
            out.Write(" = ");
            Print(X->items[i].init);
         }
      }
      out.Write("})");
      break;
   }
   case AstType::ExprStmt: {
      ExprStmt *X = cast<ExprStmt>(ast);
      out.Write("ExprStmt");
      if (X->is_return) {
         out.Write("<return>");
      }
      out.Write("(");
      if (X->expr) {
         Print(X->expr);
      }
      out.Write(")");
      break;
   }
   case AstType::IfStmt: {
      IfStmt *X = cast<IfStmt>(ast);
      out.Write("IfStmt(");
      Print(X->cond);
      out.Write(", ");
      Print(X->then);
      if (X->els) {
         out.Write(", ");
         Print(X->els);
      }
      out.Write(")");
      break;
   }
   case AstType::ForStmt: {
      ForStmt *X = cast<ForStmt>(ast);
      out.Write("ForStmt(");
      if (X->init) Print(X->init); else out.Write("_");
      out.Write(", ");
      if (X->cond) Print(X->cond); else out.Write("_");
      out.Write(", ");
      if (X->post) Print(X->post); else out.Write("_");
      out.Write(", {");
      out.EndLine();
      out.Indent();
         out.BeginLine();
         Print(X->substmt);
         out.EndLine();
      out.Dedent();
      out.BeginLine();
      out.Write("})");
      break;
   }
   case AstType::WhileStmt: {
      WhileStmt *X = cast<WhileStmt>(ast);
      out.Write("WhileStmt(");
      Print(X->cond);
      out.Write(", {");
      out.EndLine();
      out.Indent();
         out.BeginLine();
         Print(X->substmt);
         out.EndLine();
      out.Dedent();
      out.BeginLine();
      out.Write("})");
      break;
   }
   case AstType::JumpStmt: {
      JumpStmt *X = cast<JumpStmt>(ast);
      static string keyword[] = { "break", "continue", "goto" };
      out.Write("JumpStmt<", keyword[X->kind], ">(");
      if (X->kind == JumpStmt::Goto) {
         out.Write('"', X->label, '"');
      }
      out.Write(")");
      break;
   }
   case AstType::CallExpr: {
      CallExpr *X = cast<CallExpr>(ast);
      out.Write("CallExpr(");
      Print(X->func);
      out.Write(", Args = {");
      for (int i = 0; i < X->args.size(); i++) {
         if (i > 0) {
            out.Write(", ");
         }
         Print(X->args[i]);
      }
      out.Write("})");
      break;
   }
   case AstType::IndexExpr: {
      IndexExpr *X = cast<IndexExpr>(ast);
      out.Write("IndexExpr(");
      Print(X->base);
      out.Write(", ");
      Print(X->index);
      out.Write(")");
      break;
   }
   case AstType::FieldExpr: {
      FieldExpr *X = cast<FieldExpr>(ast);
      out.Write("FieldExpr");
      if (X->pointer) {
         out.Write("<pointer>");
      }
      out.Write("(");
      Print(X->base);
      out.Write(", '", X->field, "')");
      break;
   }
   case AstType::CondExpr: {
      CondExpr *X = cast<CondExpr>(ast);
      if (X->paren) out.Write("(");
      out.Write("CondExpr(");
      Print(X->cond);
      out.Write(", ");
      Print(X->then);
      out.Write(", ");
      Print(X->els);
      out.Write(")");
      if (X->paren) out.Write(")");
      break;
   }
   case AstType::SignExpr: {
      SignExpr *X = cast<SignExpr>(ast);
      out.Write("SignExpr<", (X->kind == SignExpr::Positive ? '+' : '-'), ">(");
      Print(X->expr);
      out.Write(")");
      break;
   }
   case AstType::NegExpr: {
      NegExpr *X = cast<NegExpr>(ast);
      out.Write("NegExpr(");
      Print(X->expr);
      out.Write(")");
      break;
   }
   case AstType::IncrExpr: {
      IncrExpr *X = cast<IncrExpr>(ast);
      out.Write("IncrExpr<", X->kind == IncrExpr::Positive ? "++" : "--");
      out.Write(", ", (X->preincr ? "pre" : "post"), ">(");
      Print(X->expr);
      out.Write(")");
      break;
   }
   case AstType::AddrExpr: {
      AddrExpr *X = cast<AddrExpr>(ast);
      out.Write("AddrExpr(");
      Print(X->expr);
      out.Write(")");
      break;
   }
   case AstType::DerefExpr: {
      DerefExpr *X = cast<DerefExpr>(ast);
      out.Write("DerefExpr(");
      Print(X->expr);
      out.Write(")");
      break;
   }
   case AstType::StmtError: {
      StmtError *X = cast<StmtError>(ast);
      out.Write("ErrorStmt(\"", X->code, "\")");
      break;
   }
   case AstType::ExprError: {
      ExprError *X = cast<ExprError>(ast);
      out.Write("ErrorExpr(\"", X->code, "\")");
      break;
   }
   default:
      out.Write("<error>");
      break;
   }
}

void AstPrint(Ast *ast, ostream& out) {
   AstPrinter(out).Print(ast);
}
