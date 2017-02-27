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
     std::string _curr_varname;
   AstNode *_curr_node;
     Value _curr, _ret;

     void  visit_binaryexpr_assignment(BinaryExpr *x, Value left, Value right);
     void  visit_binaryexpr_op_assignment(char, Value left, Value right);
     void  visit_callexpr_getfunc(CallExpr *x);
     bool  visit_type_conversion(CallExpr *x, const std::vector<Value>& args);
     void  check_arguments(const Function *func_type, const std::vector<Value>& argvals, std::vector<Expr*> *args = 0);
     bool  bind_field(Value obj, string method_name);
     bool  call_operator(string op, const std::vector<Value>& args = std::vector<Value>());
     void  check_condition(Expr *cond, std::string who);
     void  check_unknown(Value v, AstNode *x, string varname);
     void  eval_arguments(const std::vector<Expr *>& args,
                          std::vector<Value>& argvals);
   
   template<class Op>
     bool  visit_op_assignment(Value left, Value right);

   template<class Op>
     bool  visit_bitop_assignment(Value left, Value right);

   template<class Op>
     bool  visit_sumprod(Value left, Value right, std::string what);

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
   void visit_forstmt(ForStmt *x);
   void visit_whilestmt(WhileStmt *x);
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