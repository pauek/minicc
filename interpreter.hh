#ifndef INTERPRETER_HH
#define INTERPRETER_HH

#include <assert.h>
#include <iostream>
#include <vector>
#include <map>

#include "ast.hh"
#include "value.hh"
#include "types.hh"

struct EvalError {
   std::string msg;
   EvalError(std::string _msg) : msg(_msg) {}
};

class Interpreter : public AstVisitor, public ReadWriter 
{
                              Value _curr, _ret;
                       Environment *_env;
           std::vector<std::string> _env_names;
std::map<std::string, Environment*> _namespaces;

   void  pushenv(std::string name);
   void  popenv();
   void  actenv();
   void  setenv(std::string id, Value v, bool hidden = false);
   bool  getenv(std::string id, Value& v);

   std::string 
         env2json() const;

    void   _error(std::string msg) {
       throw new EvalError(msg);
    }

     Value new_value_from_structdecl(StructDecl *x);

     void  prepare_global_environment();
     void  invoke_func_prepare_arg(FuncDecl *x, Value args, int i);
     void  invoke_func_prepare(FuncDecl *x, const std::vector<Value>& args);
     void  invoke_user_func(FuncDecl *x, const std::vector<Value>&);

     void  visit_program_prepare(Program *x);
     void  visit_program_find_main();
     void  visit_binaryexpr_assignment(Value left, Value right);
     void  visit_binaryexpr_op_assignment(char, Value left, Value right);
     void  visit_callexpr_getfunc(CallExpr *x);

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

    friend class Stepper;

   void _init();

   Type *get_type(TypeSpec *spec);
   void  register_type(std::string name, Type *);

public:
   Interpreter() : _env(0) { _init(); }
   Interpreter(std::istream *i, std::ostream *o)
      : _env(0), ReadWriter(i, o) { _init(); }

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

   friend class UserFunc;
};

struct UserFunc : public Func {
   Interpreter *I;
   FuncDecl *decl;

   UserFunc(Interpreter *_I, std::string n, FuncDecl *d) 
      : I(_I), Func(n), decl(d) {}

   Value call(Value self, const std::vector<Value>& args) {
      I->invoke_user_func(decl, args);
      return I->_ret;
   }
};

#endif
