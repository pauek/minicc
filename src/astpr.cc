#include <algorithm>
#include <sstream>
#include "ast.hh"
#include "astpr.hh"
#include "cast.h"
using namespace std;

void AstPrinter::visit_program(Program* x) {
   out() << "Program{" << endl;
   indent(+1);
   for (Ast* n : x->nodes) {
      out(beginl);
      n->accept(this);
      out() << endl;
   }
   indent(-1);
   out(beginl) << "}" << endl;
}

void AstPrinter::visit_include(Include* x) {
   string D = (x->global ? "<>" : "\"\"");
   out() << "Include(" << D[0] << x->filename << D[1] << ")";
}

void AstPrinter::visit_macro(Macro* x) {
   out() << "Macro(" << x->macro << ")" << endl;
}

void AstPrinter::visit_using(Using* x) {
   out() << "Using(" << x->namespc << ")";
}

void AstPrinter::visit_typespec(TypeSpec *x) {
   out() << "Type" << (x->reference ? "<&>" : "") << "(";
   x->id->accept(this);
   if (!x->qual.empty()) {
      out() << ", {";
      int i = 0, numq = 0;
      for (int q = TypeSpec::Const; q <= TypeSpec::Extern; q++) {
         if (find(x->qual.begin(), x->qual.end(), q) != x->qual.end()) {
            if (numq > 0) {
               out() << ", ";
            }
            out() << TypeSpec::QualifiersNames[i];
            numq++;
         }
         i++;
      }
      out() << "}";
   }
   out() << ")";
}

void AstPrinter::visit_enumdecl(EnumDecl *x) {
   out() << "EnumDecl(\"" << x->name << "\", {";
   for (int i = 0; i < x->values.size(); i++) {
      if (i > 0) {
         out() << ", ";
      }
      out() << '"' << x->values[i].id << '"';
      if (x->values[i].has_val) {
         out() << " = " << x->values[i].val;
      }
   }
   out() << "})";
}

void AstPrinter::visit_typedefdecl(TypedefDecl *x) {
   out() << "TypedefDecl(\"" << x->decl->name << "\" = ";
   x->decl->typespec->accept(this);
   out() << ")";
}

void AstPrinter::visit_structdecl(StructDecl *x) {
   out() << "StructDecl(";
   x->id->accept(this);
   out() << ", {" << endl;
   indent(+1);
   for (DeclStmt *decl : x->decls) {
      out(beginl);
      decl->accept(this);
      out() << endl;
   }
   indent(-1);
   out(beginl) << "})";
}

void AstPrinter::visit_funcdecl(FuncDecl *x) {
   out() << "FuncDecl(";
   x->id->accept(this);
   out() << ", ";
   x->return_typespec->accept(this);
   out() << ", Params = {";
   for (int i = 0; i < x->params.size(); i++) {
      if (i > 0) {
         out() << ", ";
      }
      out() << "\"" << x->params[i]->name << "\": ";
      x->params[i]->typespec->accept(this);
   }
   if (x->block) {
      out() << "}, {" << endl;
      indent(+1);
      out(beginl);
      x->block->accept(this);
      out() << endl;
      indent(-1);
      out(beginl);
   }
   out() << "})";
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
      s->accept(this);
      out() << endl;
   }
   indent(-1);
   out(beginl) << "})";
}

void AstPrinter::visit_simpleident(SimpleIdent *x) {
   out() << "id:'" << x->name << "'";
}

void AstPrinter::visit_template_subtypes(TemplateIdent *x) {
   if (!x->subtypes.empty()) {
      out() << "<";
      for (int i = 0; i < x->subtypes.size(); i++) {
         if (i > 0) {
            out() << ", ";
         }
         x->subtypes[i]->accept(this);
      }
      out() << ">";
   }
}

void AstPrinter::visit_templateident(TemplateIdent *x) {
   out() << "id:'" << x->name << "'";
   visit_template_subtypes(x);
}

void AstPrinter::visit_fullident(FullIdent *x) {
   out() << "id:";
   if (!x->prefix.empty()) {
      out() << '[';
      for (int i = 0; i < x->prefix.size(); i++) {
         if (i > 0) {
            out() << ", ";
         }
         x->prefix[i]->accept(this);
      }
      out() << ']';
   }
   out() << "'" << x->name << "'";
   visit_template_subtypes(x);
}

void AstPrinter::visit_literal(Literal *x) {
   if (x->paren) { 
      out() << "("; 
   }
   switch (x->kind) {
   case Literal::Int:
      out() << "Int<" << x->val.as_int << ">"; 
      break;

   case Literal::Double:
      out() << "Double<" << x->val.as_double << ">"; 
      break;

   case Literal::Float:
      out() << "Float<" << x->val.as_double << ">"; 
      break;

   case Literal::Bool:
      out() << "Bool<" << (x->val.as_bool ? "true" : "false") << ">"; 
      break;

   case Literal::String:
      out() << "String<" << Literal::escape(*(x->val.as_string.s), '"') << ">"; 
      break;

   case Literal::Char: {
      string ch(1, x->val.as_char);
      out() << "Char<" << Literal::escape(ch, '\'') << ">"; 
      break;
   }
   default:
      out() << "Literal<>"; break;
   }
   if (x->paren) { 
      out() << ")"; 
   }
}

void AstPrinter::visit_binaryexpr(BinaryExpr *x) {
   if (x->paren) {
      out() << "(";
   }
   switch (x->kind) {
   default:
      out() << x->op << "(";
      if (x->left) {
         x->left->accept(this);
      }
      if (x->right) {
         out() << ", ";
         x->right->accept(this);
      }
      out() << ")";
   }
   if (x->paren) {
      out() << ")";
   }
}

void AstPrinter::visit_vardecl(VarDecl *x) {
   if (x->kind == Decl::Pointer) {
      out() << "*";
   }
   out() << '"' << x->name << '"';
}

void AstPrinter::visit_arraydecl(ArrayDecl *x) {
   out() << '"' << x->name << "\"(";
   if (x->sizes.size() == 1) {
      out() << "Size = ";
      x->sizes[0]->accept(this);
   } else {
      out() << "Sizes = {";
      for (int i = 0; i < x->sizes.size(); i++) {
         if (i > 0) {
            out() << ", ";
         }
         x->sizes[i]->accept(this);
      }
      out() << "}";
   }
   out() << ")";
}

void AstPrinter::visit_exprlist(ExprList *x) {
   out() << "{";
   for (int i = 0; i < x->exprs.size(); i++) {
      if (i > 0) {
         out() << ", ";
      }
      x->exprs[i]->accept(this);
   }
   out() << "}";
}

void AstPrinter::visit_objdecl(ObjDecl *x) {
   out() << '"' << x->name << "\"(";
   if (!x->args.empty()) {
      out() << "Args = {";
      for (int i = 0; i < x->args.size(); i++) {
         if (i > 0) {
            out() << ", ";
         }
         x->args[i]->accept(this);
      }
      out () << "}";
   }
   out() << ")";
}

void AstPrinter::visit_declstmt(DeclStmt* x) {
   out() << "DeclStmt(";
   x->typespec->accept(this);
   out() << ", Vars = {";
   bool first = true;
   for (DeclStmt::Item item : x->items) {
      if (!first) {
         out() << ", ";
      }
      item.decl->accept(this);
      if (item.init) {
         out() << " = ";
         item.init->accept(this);
      }
      first = false;
   }
   out() << "})";
}

void AstPrinter::visit_exprstmt(ExprStmt* x) {
   out() << "ExprStmt" << (x->is_return ? "<return>" : "") << "(";
   if (x->expr) {
      x->expr->accept(this);
   } 
   out() << ")";
}

void AstPrinter::visit_ifstmt(IfStmt *x) {
   out() << "IfStmt(";
   x->cond->accept(this);
   out() << ", ";
   x->then->accept(this);
   if (x->els) {
      out() << ", ";
      x->els->accept(this);
   }
   out() << ")";
}

void AstPrinter::visit_forstmt(ForStmt *x) {
   out() << "ForStmt(";
   if (x->init) {
      x->init->accept(this);
   } else {
      out() << "_";
   }
   out() << ", ";
   if (x->cond) {
      x->cond->accept(this);
   } else {
      out() << "_";
   }
   out() << ", ";
   if (x->post) {
      x->post->accept(this);
   } else {
      out() << "_";
   }
   out() << ", {" << endl;
   indent(+1);
   out(beginl);
   x->substmt->accept(this);
   out() << endl;
   indent(-1);
   out(beginl) << "})";
}

void AstPrinter::visit_whilestmt(WhileStmt *x) {
   out() << "WhileStmt(";
   x->cond->accept(this);
   out() << ", {" << endl;
   indent(+1);
   out(beginl);
   x->substmt->accept(this);
   out() << endl;
   indent(-1);
   out(beginl) << "})";
}

void AstPrinter::visit_jumpstmt(JumpStmt *x) {
   string keyword[3] = { "break", "continue", "goto" };
   out() << "JumpStmt<" << keyword[x->kind] << ">(";
   if (x->kind == JumpStmt::Goto) {
      out() << '"' << x->label << '"';
   }
   out() << ")";
}

void AstPrinter::visit_callexpr(CallExpr *x) {
   out() << "CallExpr(";
   x->func->accept(this);
   out() << ", Args = {";
   for (int i = 0; i < x->args.size(); i++) {
      if (i > 0) {
         out() << ", ";
      }
      x->args[i]->accept(this);
   }
   out() << "})";
}

void AstPrinter::visit_indexexpr(IndexExpr *x) {
   out() << "IndexExpr(";
   x->base->accept(this);
   out() << ", ";
   x->index->accept(this);
   out() << ")";
}

void AstPrinter::visit_fieldexpr(FieldExpr *x) {
   out() << "FieldExpr";
   if (x->pointer) {
      out() << "<pointer>";
   }
   out() << "(";
   x->base->accept(this);
   out() << ", ";
   x->field->accept(this);
   out() << ")";
}

void AstPrinter::visit_condexpr(CondExpr *x) {
   if (x->paren) { 
      out() << "("; 
   }
   out() << "CondExpr(";
   x->cond->accept(this);
   out() << ", ";
   x->then->accept(this);
   out() << ", ";
   x->els->accept(this);
   out() << ")";
   if (x->paren) { 
      out() << ")"; 
   }
}

void AstPrinter::visit_signexpr(SignExpr *x) {
   out() << "SignExpr<";
   out() << (x->kind == SignExpr::Positive ? "+" : "-");
   out() << ">(";
   x->expr->accept(this);
   out() << ")";
}

void AstPrinter::visit_increxpr(IncrExpr *x) {
   out() << "IncrExpr<";
   out() << (x->kind == IncrExpr::Positive ? "++" : "--") << ", ";
   out() << (x->preincr ? "pre" : "post");
   out() << ">(";
   x->expr->accept(this);
   out() << ")";
}

void AstPrinter::visit_negexpr(NegExpr *x) {
   out() << "NegExpr(";
   x->expr->accept(this);
   out() << ")";
}

void AstPrinter::visit_addrexpr(AddrExpr *x) {
   out() << "AddrExpr(";
   x->expr->accept(this);
   out() << ")";
}

void AstPrinter::visit_derefexpr(DerefExpr *x) {
   out() << "DerefExpr(";
   x->expr->accept(this);
   out() << ")";
}

void AstPrinter::visit_errorstmt(StmtError *x) {
   out() << "ErrorStmt(\"" << x->code << "\")";
}

void AstPrinter::visit_errorexpr(ExprError *x) {
   out() << "ErrorExpr(\"" << x->code << "\")";
}

struct OutputWriter {
   OutputWriter(ostream& out = std::cout) : indent_(0), out_(out) {}

   void Indent() { indent_ += 3; }
   void Dedent() { indent_ -= 3; }

   template<typename T>
   void Write(const T& t) { out_ << t; }

   void EndLine() { out_ << endl; }
   void BeginLine() { out_ << string(indent_, ' '); }

   void Line(string s) { BeginLine(); Write(s); EndLine(); }

private:
   int indent_;
   ostream& out_;
};

struct AstPrinter2 {
   OutputWriter out;
   AstPrinter2(ostream& o) : out(o) {}
   void Print(Ast *ast);
};

void AstPrinter2::Print(Ast* ast) {
   assert(ast != nullptr);
   switch (ast->type()) {
   case AstType::Program: {
      out.Line("Program{");
      out.Indent();
      Program *X = cast<Program>(ast);
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
      out.Write(D[0]);
      out.Write(X->filename);
      out.Write(D[1]);
      out.Write(")");
      break;
   }
   case AstType::Macro: {
      Macro *X = cast<Macro>(ast);
      out.Write("Macro(");
      out.Write(X->macro);
      out.Write(")");
      break;
   }
   case AstType::Using: {
      Using *X = cast<Using>(ast);
      out.Write("Using(");
      out.Write(X->namespc);
      out.Write(")");
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
      if (!X->qual.empty()) {
         out.Write(", {");
         int i = 0, numq = 0;
         for (int q = TypeSpec::Const; q <= TypeSpec::Extern; q++) {
            vector<TypeSpec::Qualifiers>& Q = X->qual;
            if (find(Q.begin(), Q.end(), q) != X->qual.end()) {
               if (numq > 0) {
                  out.Write(", ");
               }
               out.Write(TypeSpec::QualifiersNames[i]);
               numq++;
            }
            i++;
         }
         out.Write("}");
      }
      out.Write(")");
      break;
   }
   case AstType::EnumDecl: {
      EnumDecl *X = cast<EnumDecl>(ast);
      out.Write("EnumDecl(\"");
      out.Write(X->name);
      out.Write("\", {");
      for (int i = 0; i < X->values.size(); i++) {
         if (i > 0)
            out.Write(", ");
         out.Write('"');
         out.Write(X->values[i].id);
         out.Write('"');
         if (X->values[i].has_val) {
            out.Write(" = ");
            out.Write(X->values[i].val);
         }
      }
      out.Write("})");
      break;
   }
   case AstType::TypedefDecl: {
      TypedefDecl *X = cast<TypedefDecl>(ast);
      out.Write("TypedefDecl(\"");
      out.Write(X->decl->name);
      out.Write("\" = ");
      Print(X->decl->typespec);
      out.Write(")");
      break;
   }
   case AstType::StructDecl: {
      StructDecl *X = cast<StructDecl>(ast);
      out.Write("StructDecl(");
      Print(X->id);
      out.Write(", {");
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
         out.Write('"');
         out.Write(X->params[i]->name);
         out.Write("\": ");
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
   case AstType::SimpleIdent: {
      SimpleIdent *X = cast<SimpleIdent>(ast);
      out.Write("id:'");
      out.Write(X->name);
      out.Write("'");
      break;
   }
   case AstType::TemplateIdent: {
      TemplateIdent *X = cast<TemplateIdent>(ast);
      out.Write("id:'");
      out.Write(X->name);
      out.Write("'");
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
   case AstType::FullIdent: {
      FullIdent *X = cast<FullIdent>(ast);
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
      out.Write("'");
      out.Write(X->name);
      out.Write("'");
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
         out.Write("Bool<");
         out.Write(X->val.as_bool ? "true" : "false");
         out.Write(">");
         break;
      case Literal::Char: 
         out.Write("Char<");
         out.Write(Literal::escape(X->val.as_char));
         out.Write(">");
         break;
      case Literal::Int: 
         out.Write("Int<");
         out.Write(X->val.as_int);
         out.Write(">");
         break;
      case Literal::Float:
         out.Write("Float<");
         out.Write(X->val.as_double);
         out.Write(">");
         break;
      case Literal::Double:
         out.Write("Double<");
         out.Write(X->val.as_double);
         out.Write(">");
         break;
      case Literal::String:
         out.Write("String<");
         out.Write(Literal::escape(*(X->val.as_string.s), '"'));
         out.Write(">");
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
      out.Write(X->op);
      out.Write("(");
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
      out.Write('"');
      out.Write(X->name);
      out.Write('"');
      break;
   }
   case AstType::ArrayDecl: {
      ArrayDecl *X = cast<ArrayDecl>(ast);
      out.Write('"');
      out.Write(X->name);
      out.Write("\"(");
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
      out.Write('"');
      out.Write(X->name);
      out.Write("\"(");
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
      out.Write("JumpStmt<");
      out.Write(keyword[X->kind]);
      out.Write(">(");
      if (X->kind == JumpStmt::Goto) {
         out.Write('"');
         out.Write(X->label);
         out.Write('"');
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
      out.Write(", ");
      Print(X->field);
      out.Write(")");
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
      out.Write("SignExpr<");
      out.Write(X->kind == SignExpr::Positive ? '+' : '-');
      out.Write(">(");
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
      out.Write("IncrExpr<");
      out.Write(X->kind == IncrExpr::Positive ? "++" : "--");
      out.Write(", ");
      out.Write(X->preincr ? "pre" : "post");
      out.Write(">(");
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
      out.Write("ErrorStmt(\"");
      out.Write(X->code);
      out.Write("\")");
      break;
   }
   case AstType::ExprError: {
      ExprError *X = cast<ExprError>(ast);
      out.Write("ErrorExpr(\"");
      out.Write(X->code);
      out.Write("\")");
      break;
   }
   default:
      out.Write("node");
      break;
   }
}

void AstPrint(Ast *ast, ostream& out) {
   AstPrinter2(out).Print(ast);
}