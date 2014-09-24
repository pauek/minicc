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

class Interpreter : public AstVisitor, public ReadWriter {
    Value *_curr, *_ret;

            std::vector<Environment> _env;
    std::map<std::string, FuncDecl*> _funcs;
  std::map<std::string, StructDecl*> _structs;

   bool  is_struct(std::string name) {
      return _structs.find(name) != _structs.end();
   }

   void   pushenv(std::string name) { _env.push_back(Environment(name));  }
   void   popenv();
   void   actenv();
   void   setenv(std::string id, Value *val, bool hidden = false);
   Value* getenv(std::string id);

std::string env2json() const;

    void _error(std::string msg) {
       throw new EvalError(msg);
    }

     Value new_value_from_structdecl(StructDecl *x);

     void  invoke_func_prepare(FuncDecl *x, const std::vector<Value*>& args);
     void  invoke_func(FuncDecl *, const std::vector<Value*>&);

     void  visit_program_prepare(Program *x);
 FuncDecl *visit_program_find_main();
     void  visit_binaryexpr_assignment(Value *, Value *);
     void  visit_binaryexpr_op_assignment(char, Value *, Value *);
 FuncDecl *visit_callexpr_getfunc(CallExpr *x);
     void  visit_vardecl_struct(VarDecl *x, StructDecl *decl);
    Value *visit_vardecl_struct_new(StructDecl *D, Value *init);

   template<class Op>
     bool  visit_op_assignment(Value *left, Value *right);

   template<class Op>
     bool  visit_bitop_assignment(Value *left, Value *right);

   template<class Op>
     bool  visit_sumprod(Value *left, Value *right);

   template<class Op>
     bool  visit_bitop(Value *left, Value *right);

   template<class Op>
     bool  visit_comparison(Value *left, Value *right);

    friend class Stepper;

public:
   Interpreter() 
      : _curr(0), _ret(0) {}

   Interpreter(std::istream *i, std::ostream *o)
      : ReadWriter(i, o), _curr(0), _ret(0) {}

   ~Interpreter() {}

   void visit_comment(CommentSeq *x);
   void visit_include(Include *x);
   void visit_macro(Macro *x);
   void visit_program(Program *x);
   void visit_using(Using *x);
   void visit_funcdecl(FuncDecl *x);
   void visit_structdecl(StructDecl *x);
   void visit_block(Block *x);
   void visit_ident(Ident *x);
   void visit_binaryexpr(BinaryExpr *x); 
   void visit_vardecl(VarDecl *);
   void visit_arraydecl(ArrayDecl *);
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
};

#endif
