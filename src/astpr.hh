#ifndef ASTPRINT_HH
#define ASTPRINT_HH

#include <assert.h>
#include <iostream>
#include "ast.hh"

class AstPrinter : public AstVisitor, public ReadWriter {

   void visit_template_subtypes(TemplateIdent *);
   
public:
   AstPrinter(std::ostream *o = &std::cout) 
      : ReadWriter(o) {}

   void print(Ast* x) { x->accept(this); }

   void visit_comment(CommentSeq *x);
   void visit_include(Include *x);
   void visit_macro(Macro *x);
   void visit_program(Program *x);
   void visit_using(Using *x);
   void visit_typespec(TypeSpec *x);
   void visit_funcdecl(FuncDecl *x);
   void visit_structdecl(StructDecl *x);
   void visit_typedefdecl(TypedefDecl *x);
   void visit_enumdecl(EnumDecl *x);
   void visit_block(Block *x);
   void visit_binaryexpr(BinaryExpr *x);
   void visit_literal(Literal *x);
   void visit_simpleident(SimpleIdent *x);
   void visit_templateident(TemplateIdent *x);
   void visit_fullident(FullIdent *x);
   void visit_vardecl(VarDecl *);
   void visit_arraydecl(ArrayDecl *);
   void visit_objdecl(ObjDecl *);
   void visit_declstmt(DeclStmt *x);
   void visit_exprstmt(ExprStmt *x);
   void visit_ifstmt(IfStmt *x);
   void visit_forstmt(ForStmt *x);
   void visit_whilestmt(WhileStmt *x);
   void visit_jumpstmt(JumpStmt *x);
   void visit_callexpr(CallExpr *x);
   void visit_indexexpr(IndexExpr *x);
   void visit_fieldexpr(FieldExpr *x);
   void visit_condexpr(CondExpr *x);
   void visit_exprlist(ExprList *x);
   void visit_signexpr(SignExpr *x);
   void visit_increxpr(IncrExpr *x);
   void visit_negexpr(NegExpr *x);
   void visit_addrexpr(AddrExpr *x);
   void visit_derefexpr(DerefExpr *x);

   void visit_errorstmt(StmtError *x);
   void visit_errorexpr(Expr::Error *x);
};

#endif
