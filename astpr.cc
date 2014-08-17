#include <sstream>
#include "ast.hh"
#include "astpr.hh"
using namespace std;

void AstPrinter::visit_program(Program* x) {
   out() << "Program{" << endl;
   indent(+1);
   for (AstNode* n : x->nodes) {
      out(beginl);
      n->visit(this);
      out() << endl;
   }
   indent(-1);
   out(beginl) << "}" << endl;
}

void AstPrinter::visit_comment(CommentSeq* cn) {
   out() << cn;
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

void AstPrinter::visit_type(Type *x) {
   static const string names[] = { 
      "const", "volatile", "mutable", "register", "auto", "extern"
   };

   out() << "Type" << (x->reference ? "<&>" : "") << "(";
   x->id->visit(this);
   if (x->qual != 0) {
      out() << ", {";
      int i = 0, numquals = 0;
      while (Type::Qualifiers(1 << i) <= Type::Extern) {
         if (x->qual & Type::Qualifiers(1 << i)) {
            if (numquals > 0) {
               out() << "," << _cmt_(x, i);
            }
            out() << names[i];
            numquals++;
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
   x->decl->type->visit(this);
   out() << ")";
}

void AstPrinter::visit_structdecl(StructDecl *x) {
   out() << "StructDecl(";
   x->id->visit(this);
   out() << ", {" << endl;
   indent(+1);
   for (DeclStmt *decl : x->decls) {
      out(beginl);
      decl->visit(this);
      out() << endl;
   }
   indent(-1);
   out(beginl) << "})";
}

void AstPrinter::visit_funcdecl(FuncDecl *x) {
   out() << "FuncDecl(\"" << x->name << "\", ";
   x->return_type->visit(this);
   out() << ", Params = {";
   for (int i = 0; i < x->params.size(); i++) {
      if (i > 0) {
         out() << ", ";
      }
      out() << "\"" << x->params[i]->name << "\": ";
      x->params[i]->type->visit(this);
   }
   if (x->block) {
      out() << "}, {" << endl;
      indent(+1);
      out(beginl);
      x->block->visit(this);
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
      s->visit(this);
      out() << endl;
   }
   indent(-1);
   out(beginl) << "})";
}

void AstPrinter::visit_ident(Ident *x) {
   out() << "id:";
   if (!x->prefix.empty()) {
      out() << '[';
      for (int i = 0; i < x->prefix.size(); i++) {
         if (i > 0) {
            out() << ", ";
         }
         x->prefix[i]->visit(this);
      }
      out() << ']';
   }
   out() << "'" << x->id << "'";
   if (!x->subtypes.empty()) {
      out() << "<";
      for (int i = 0; i < x->subtypes.size(); i++) {
         if (i > 0) {
            out() << ", ";
         }
         x->subtypes[i]->visit(this);
      }
      out() << ">";
   }
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

   case Literal::Bool:  
      out() << "Bool<" << (x->val.as_bool ? "true" : "false") << ">"; 
      break;

   case Literal::String: 
      out() << "String<" << Literal::escape(*(x->val.as_string.s), '"') << ">"; 
      break;

   case Literal::Char:
      out() << "Char<" << Literal::escape(*(x->val.as_string.s), '\'') << ">"; 
      break;

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

void AstPrinter::visit_vardecl(VarDecl *x) {
   if (x->kind == Decl::Pointer) {
      out() << "*";
   }
   out() << '"' << x->name << '"';
   if (!x->init.empty()) {
      out() << " = ";
      x->init[0]->visit(this);
   }
}

void AstPrinter::visit_arraydecl(ArrayDecl *x) {
   out() << '"' << x->name << "\"(Size = ";
   x->size->visit(this);
   if (!x->init.empty()) {
      out() << ", Init = {";
      for (int i = 0; i < x->init.size(); i++) {
         if (i > 0) {
            out() << ", ";
         }
         x->init[i]->visit(this);
      }
      out() << "}";
   }
   out() << ")";
}

void AstPrinter::visit_objdecl(ObjDecl *x) {
   out() << '"' << x->name << "\"(";
   if (!x->args.empty()) {
      out() << "Args = {";
      for (int i = 0; i < x->args.size(); i++) {
         if (i > 0) {
            out() << ", ";
         }
         x->args[i]->visit(this);
      }
      out () << "}";
   }
   out() << ")";
}

void AstPrinter::visit_declstmt(DeclStmt* x) {
   out() << "DeclStmt(";
   x->type->visit(this);
   out() << ", Vars = {";
   bool first = true;
   for (Decl *d : x->decls) {
      if (!first) {
         out() << ", ";
      }
      d->visit(this);
      first = false;
   }
   out() << "})";
}

void AstPrinter::visit_exprstmt(ExprStmt* x) {
   out() << "ExprStmt" << (x->is_return ? "<return>" : "") << "(";
   if (x->expr) {
      x->expr->visit(this);
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
   x->func->visit(this);
   out() << ", Args = {";
   for (int i = 0; i < x->args.size(); i++) {
      if (i > 0) {
         out() << ", ";
      }
      x->args[i]->visit(this);
   }
   out() << "})";
}

void AstPrinter::visit_indexexpr(IndexExpr *x) {
   out() << "IndexExpr(";
   x->base->visit(this);
   out() << ", ";
   x->index->visit(this);
   out() << ")";
}

void AstPrinter::visit_fieldexpr(FieldExpr *x) {
   out() << "FieldExpr";
   if (x->pointer) {
      out() << "<pointer>";
   }
   out() << "(";
   x->base->visit(this);
   out() << ", ";
   x->field->visit(this);
   out() << ")";
}

void AstPrinter::visit_condexpr(CondExpr *x) {
   if (x->paren) { 
      out() << "("; 
   }
   out() << "CondExpr(";
   x->cond->visit(this);
   out() << ", ";
   x->then->visit(this);
   out() << ", ";
   x->els->visit(this);
   out() << ")";
   if (x->paren) { 
      out() << ")"; 
   }
}

void AstPrinter::visit_signexpr(SignExpr *x) {
   out() << "SignExpr<";
   out() << (x->kind == SignExpr::Positive ? "+" : "-");
   out() << ">(";
   x->expr->visit(this);
   out() << ")";
}

void AstPrinter::visit_increxpr(IncrExpr *x) {
   out() << "IncrExpr<";
   out() << (x->kind == IncrExpr::Positive ? "++" : "--") << ", ";
   out() << (x->preincr ? "pre" : "post");
   out() << ">(";
   x->expr->visit(this);
   out() << ")";
}

void AstPrinter::visit_negexpr(NegExpr *x) {
   out() << "NegExpr(";
   x->expr->visit(this);
   out() << ")";
}

void AstPrinter::visit_addrexpr(AddrExpr *x) {
   out() << "AddrExpr(";
   x->expr->visit(this);
   out() << ")";
}

void AstPrinter::visit_derefexpr(DerefExpr *x) {
   out() << "DerefExpr(";
   x->expr->visit(this);
   out() << ")";
}

void AstPrinter::visit_errorstmt(Stmt::Error *x) {
   out() << "ErrorStmt(\"" << x->code << "\")";
}

void AstPrinter::visit_errorexpr(Expr::Error *x) {
   out() << "ErrorExpr(\"" << x->code << "\")";
}
