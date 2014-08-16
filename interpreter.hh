#ifndef INTERPRETER_HH
#define INTERPRETER_HH

#include <assert.h>
#include <iostream>
#include <vector>
#include <map>
#include "ast.hh"
#include "value.hh"

struct EvalError {
   std::string msg;
   EvalError(std::string _msg) : msg(_msg) {}
};

class Interpreter : public AstVisitor {
   Value _curr;

   std::vector< std::map<std::string, Value> > _env;
   std::map<std::string, FuncDecl*> _funcs;

   void _error(std::string msg) {
      throw new EvalError(msg);
   }

   void pushenv() { _env.push_back(std::map<std::string, Value>()); }
   void popenv()  { _env.pop_back(); }

   void setenv(std::string id, const Value& val);
   bool getenv(std::string id, Value& val) const;

   void invoke_func(FuncDecl *, std::vector<Value>&);

public:
   Interpreter(std::istream *i, std::ostream *o)
      : AstVisitor(i, o) {}

   ~Interpreter() {}

   void visit_comment(CommentSeq *x);
   void visit_include(Include *x);
   void visit_macro(Macro *x);
   void visit_program(Program *x);
   void visit_using(Using *x);
   void visit_type(Type *x);
   void visit_funcdecl(FuncDecl *x);
   void visit_structdecl(StructDecl *x);
   void visit_typedefdecl(TypedefDecl *x);
   void visit_enumdecl(EnumDecl *x);
   void visit_block(Block *x);
   void visit_ident(Ident *x);
   void visit_binaryexpr(BinaryExpr *x); 
   void visit_vardecl(VarDecl *);
   void visit_arraydecl(ArrayDecl *);
   void visit_objdecl(ObjDecl *);
   void visit_declstmt(DeclStmt *x);
   void visit_exprstmt(ExprStmt *x);
   void visit_ifstmt(IfStmt *x);
   void visit_iterstmt(IterStmt *x);
   void visit_jumpstmt(JumpStmt *x);
   void visit_callexpr(CallExpr *x);
   void visit_indexexpr(IndexExpr *x);
   void visit_fieldexpr(FieldExpr *x);
   void visit_condexpr(CondExpr *x);
   void visit_signexpr(SignExpr *x);
   void visit_increxpr(IncrExpr *x);
   void visit_negexpr(NegExpr *x);
   void visit_addrexpr(AddrExpr *x);
   void visit_derefexpr(DerefExpr *x);
   void visit_literal(Literal *x);

   void visit_errorstmt(Stmt::Error *x);
   void visit_errorexpr(Expr::Error *x);
};

#endif
