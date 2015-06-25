#ifndef SEMANTIC_HH
#define SEMANTIC_HH

#include <assert.h>
#include <iostream>
#include <vector>
#include <map>

#include "ast.hh"
#include "value.hh"
#include "types.hh"

class SemanticAnalyzer : 
   public AstVisitor, public WithEnvironment
{
    Value _curr, _ret;

     void  visit_binaryexpr_assignment(Value left, Value right);
     void  visit_binaryexpr_op_assignment(char, Value left, Value right);
     void  visit_callexpr_getfunc(CallExpr *x);
     bool  visit_type_conversion(CallExpr *x, const std::vector<Value>& args);
     void  visit_callexpr_call(Value func, const std::vector<Value>& args);

   template<class Op>
     bool  visit_op_assignment(Value left, Value right);

   template<class Op>
     bool  visit_bitop_assignment(Value left, Value right);

   template<class Op>
     bool  visit_sumprod(Value left, Value right);

   template<class Op>
     bool  visit_bitop(Value left, Value right);

   template<class Op>
     bool  visit_comparison(Value left, Value right);

public:
   SemanticAnalyzer() {}

   void visit_comment(CommentSeq *x);
   void visit_include(Include *x);
   void visit_macro(Macro *x);
   void visit_program(Program *x);
   void visit_using(Using *x);
   void visit_funcdecl(FuncDecl *x);
   void visit_structdecl(StructDecl *x);
   void visit_block(Block *x);
   void visit_fullident(FullIdent *x);
   void visit_binaryexpr(BinaryExpr *x); 
   void visit_vardecl(VarDecl *);
   void visit_arraydecl(ArrayDecl *);
   void visit_objdecl(ObjDecl *x);
   void visit_declstmt(DeclStmt *x);
   void visit_exprstmt(ExprStmt *x);
   void visit_ifstmt(IfStmt *x);
   void visit_iterstmt(IterStmt *x);
   void visit_callexpr(CallExpr *x);
   void visit_indexexpr(IndexExpr *x);
   void visit_fieldexpr(FieldExpr *x);
   void visit_condexpr(CondExpr *x);
   void visit_exprlist(ExprList *x);
   void visit_signexpr(SignExpr *x);
   void visit_increxpr(IncrExpr *x);
   void visit_negexpr(NegExpr *x);
   void visit_literal(Literal *x);
   void visit_typedefdecl(TypedefDecl *x);
   void visit_derefexpr(DerefExpr *x);
};

#endif
