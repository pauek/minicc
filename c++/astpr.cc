#include <algorithm>
#include <sstream>
#include "ast.hh"
#include "astpr.hh"
using namespace std;

void AstPrinter::visit_program(Program* x) {
   out() << "Program{" << endl;
   indent(+1);
   for (AstNode* n : x->nodes) {
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
   switch (x->type) {
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

void AstPrinter::visit_errorstmt(Stmt::Error *x) {
   out() << "ErrorStmt(\"" << x->code << "\")";
}

void AstPrinter::visit_errorexpr(Expr::Error *x) {
   out() << "ErrorExpr(\"" << x->code << "\")";
}
