#ifndef AST_H
#define AST_H

#include <assert.h>
#include <string>
#include <sstream>
#include <vector>
#include <list>
#include <map>
#include <algorithm>
#include "lexer.hh"

std::ostream& operator<<(std::ostream& o, CommentSeq* C);

struct Error {
   Span span;
   std::string msg;
   bool stopper;     // this error should eclipse the following errors
                     // (probably an avalanche of parsing errors)
   Error(std::string m)         : stopper(false), msg(m) {}
   Error(Pos p, std::string m)  : stopper(false), span(p), msg(m) {}
   Error(Span s, std::string m) : stopper(false), span(s), msg(m) {}
   void to_json(std::ostream& o) const;
};

struct Comment {
   enum Kind { none, singleline, multiline, endline };
   Kind kind;
   std::string text;
   Comment(Kind k = none) : kind(k) {}
};

struct CommentSeq {
   std::vector<Comment> items;

   bool has_endl() const;
   bool ends_with_empty_line() const;
   void remove_endls();
   void only_one_endl_at_end();

   bool starts_with_endl() const { 
      return !items.empty() and items.front().kind == Comment::endline; 
   }
   bool ends_with_endl() const { 
      return !items.empty() and items.back().kind  == Comment::endline; 
   }
};

class AstVisitor;
struct TypeSpec;

enum class AstType {
   Program,
   Include,
   Macro,
   Using,
   FuncDecl,
   StructDecl,
   TypedefDecl,
   EnumDecl,

   StmtError,
   ExprStmt,
   IfStmt,
   ForStmt,
   WhileStmt,
   DeclStmt,
   JumpStmt,

   TypeSpec,
   VarDecl,
   ArrayDecl,
   ObjDecl,

   ExprError,
   Literal,
   SimpleIdent,
   TemplateIdent,
   FullIdent,
   BinaryExpr,
   UnaryExpr,
   SignExpr,
   IncrExpr,
   NegExpr,
   AddrExpr,
   DerefExpr,
   CallExpr,
   IndexExpr,
   FieldExpr,
   CondExpr,
   ExprList,

   Block,
};

struct Ast {
                     Span  span;
      std::vector<Error*>  errors;
 std::vector<CommentSeq*>  comments;
                      Ast *parent;

                  void add_error(std::string msg);
                  void add_error(Pos ini, Pos fin, std::string msg);

               AstType type() const { return type_; }

   virtual            ~Ast() {}
   virtual        void accept(AstVisitor* v) = 0;
   virtual         int num_children() const { return 0; }
   virtual        Ast* child(int n)   const { return 0; }
   virtual        bool has_errors()   const { return !errors.empty(); }
   virtual std::string describe()     const { return "UNIMPLEMENTED"; }

   template<typename X>
                  bool is() const { return dynamic_cast<const X*>(this) != 0; }
   template<typename X>
              const X *as() const { return dynamic_cast<const X*>(this); }
   template<typename X>
                    X *as()       { return dynamic_cast<X*>(this); }
protected:
   AstType  type_;
};

template<AstType Type>
struct AstDerived : Ast {
   static bool classof(const Ast *ast) { return ast->type() == Type; }
   AstDerived() { type_ = Type; }
};

struct Program : public AstDerived<AstType::Program> {
   std::vector<Ast*> nodes;

   int  num_children() const { return nodes.size(); }
   Ast* child(int n)         { return nodes[n]; }
   void add(Ast* n)      { nodes.push_back(n), n->parent = this; }
   void accept(AstVisitor* v);

   bool has_errors() const;
};

struct Include : public AstDerived<AstType::Include> {
   std::string filename;
   bool global;

   Include(std::string _filename = "", bool _global = false) 
      : filename(_filename), global(_global) {}

   void accept(AstVisitor* v);
};

struct Macro : public AstDerived<AstType::Macro> {
   std::string macro;
   Macro(std::string _macro) : macro(_macro) {}
   void accept(AstVisitor *v);
};

struct Using : public AstDerived<AstType::Using> {
   std::string namespc;
   void accept(AstVisitor *v);
};

// Statements //////////////////////////////////////////////

struct Expr;

struct Stmt : public Ast {
   void accept(AstVisitor *v);
};

template<AstType Type>
struct StmtDerived : Stmt {
   static bool classof(const Ast *ast) { return ast->type() == Type; }
   StmtDerived() { type_ = Type; }
};

struct StmtError : public StmtDerived<AstType::StmtError> {
   std::string code;
   void accept(AstVisitor *v);
};

struct ExprStmt : public StmtDerived<AstType::ExprStmt> {
   Expr *expr;
   bool is_return;
   ExprStmt() : expr(0), is_return(false) {}
   void accept(AstVisitor *v);
   bool has_errors() const;
   std::string describe() const;
};

struct IfStmt : public StmtDerived<AstType::IfStmt> {
   Expr *cond;
   Stmt *then, *els;

   IfStmt() : cond(0), then(0), els(0) {}
   void accept(AstVisitor *v);
   bool has_errors() const;
};

struct ForStmt : public StmtDerived<AstType::ForStmt> { // while + for
   Stmt *init;
   Expr *cond, *post;
   Stmt *substmt;

   ForStmt() : cond(0), init(0), substmt(0), post(0) {}
   void accept(AstVisitor *v);
   bool has_errors() const;
};

struct WhileStmt : public StmtDerived<AstType::WhileStmt> { // while + for
   Expr *cond;
   Stmt *substmt;

   WhileStmt() : cond(0), substmt(0) {}
   void accept(AstVisitor *v);
   bool has_errors() const;
};

struct Decl : public Ast {
   enum Kind { Normal, Pointer };
   TypeSpec *typespec;
   std::string name;
   Decl() : typespec(0) {}
};

template<AstType Type>
struct DeclDerived : Decl {
   static bool classof(const Ast *ast) { return ast->type() == Type; }
   DeclDerived() { type_ = Type; }
};

struct VarDecl : public DeclDerived<AstType::VarDecl> {
   Kind kind;
   VarDecl() : kind(Normal) {}
   void accept(AstVisitor *v);
};

struct ArrayDecl : public DeclDerived<AstType::ArrayDecl> {
   std::vector<Expr*> sizes;
   Kind kind;
   ArrayDecl() : kind(Normal) {}
   void accept(AstVisitor *v);
   std::string typestr() const;
};

struct ObjDecl : public DeclDerived<AstType::ObjDecl> {
   std::vector<Expr *> args;
   void accept(AstVisitor *v);
};

struct DeclStmt : public StmtDerived<AstType::DeclStmt> {
   TypeSpec *typespec;
   struct Item {
      Decl *decl;
      Expr *init;
      Item() : decl(0), init(0) {}
   };
   std::vector<Item> items;

   void accept(AstVisitor *v);
   bool has_errors() const;
   std::string describe() const;
};

struct JumpStmt : public StmtDerived<AstType::JumpStmt> {
   enum Kind { Unknown = -1, Break = 0, Continue = 1, Goto = 2 };

   Kind kind;
   std::string label;

   JumpStmt() : kind(Unknown) {}
   void accept(AstVisitor *v);

   static Kind keyword2type(std::string s);
};

struct Block : public StmtDerived<AstType::Block> {
   std::vector<Stmt*> stmts;
   void accept(AstVisitor *v);
   bool has_errors() const;
};

// Expressions /////////////////////////////////////////////

struct Expr : public Ast {
   enum Kind { 
      Unknown,
      // pm_expression 
      Multiplicative, Additive, Shift, Relational, Equality, 
      BitAnd, BitXor, BitOr, LogicalAnd, LogicalOr, Conditional,
      Eqment, Comma, Infinite
   };
   struct Op2KindInitializer { Op2KindInitializer(); }; // init _op2kind

   bool paren; // if this is true, comments will have an extra element!

   Expr() : paren(false) {}

   virtual bool is_read_expr()  const { return false; }
   virtual bool is_write_expr() const { return false; }
   virtual bool is_assignment() const { return false; }
   virtual void collect_rights(std::list<Expr*>& L) const {}

   static std::map<std::string, Kind> _op2kind;
   static std::map<Token::Type, Kind> _tok2kind;
   static Kind op2kind(std::string op);
   static Kind tok2kind(Token::Type toktyp);
   static Op2KindInitializer initializer;
   static bool right_associative(Kind t);

   struct Error;
};

template<AstType Type>
struct ExprDerived : Expr {
   static bool classof(const Ast *ast) { ast->type() == Type; }
   ExprDerived() { type_ = Type; }
};

struct ExprError : public ExprDerived<AstType::ExprError> {
   std::string code;
   void accept(AstVisitor *v);
};

struct Literal : public ExprDerived<AstType::Literal> 
{
   enum Kind { Bool, Int, String, Char, Float, Double };
   struct StringData {
      std::string *s;
      bool L;
   };
   union Data {
      bool       as_bool;
      int        as_int;
      double     as_double;
      char       as_char;
      StringData as_string;
   };
   Kind kind;
   Data val;
   bool L; // for strings

   Literal(Kind k) : kind(k) {}
   void accept(AstVisitor *v);

   static std::string escape(char c, char delim);
   static std::string escape(std::string s, char delim);
};

struct SimpleIdent : ExprDerived<AstType::SimpleIdent> {
   std::string name;
   bool is_namespace = false; // (used by the interpreter)

   SimpleIdent(std::string _name = "") : name(_name), is_namespace(false) {}
   void accept(AstVisitor *v);
   virtual bool is_template() const { return false; }
};

struct TemplateIdent : SimpleIdent {
   std::vector<TypeSpec*> subtypes;

   TemplateIdent(std::string name_) : SimpleIdent(name_) { 
      // overwrite!
      type_ = AstType::TemplateIdent;
   } 
   bool is_template() const { return !subtypes.empty(); }
   std::string typestr() const;
   void accept(AstVisitor *v);
   bool has_errors() const;

   static bool classof(const Ast *ast) { ast->type() == AstType::TemplateIdent; }
};

struct FullIdent : TemplateIdent {
   std::vector<TemplateIdent*> prefix;  // for classes & namespaces;

   FullIdent(std::string name_) : TemplateIdent(name_) {
      type_ = AstType::FullIdent;
   }
   std::string typestr() const;
   void accept(AstVisitor *v);
   bool has_errors() const;

   void shift(std::string new_id);
   SimpleIdent *get_potential_namespace_or_class() const;
   std::vector<TemplateIdent*> get_non_namespaces();

   static bool classof(const Ast *ast) { ast->type() == AstType::FullIdent; }
};

struct BinaryExpr : public ExprDerived<AstType::BinaryExpr> {
   Kind kind;
   std::string op;
   std::string str;
   Expr *left, *right;

   BinaryExpr(Kind k = Unknown) : kind(k), op("") {}

   void accept(AstVisitor *v);
   void set(Expr::Kind _kind);
   bool has_errors() const;

   bool is_read_expr()  const;
   bool is_write_expr() const;
   bool is_assignment() const;
   void collect_rights(std::list<Expr*>& L) const;
   std::string describe() const;
};

struct UnaryExpr : public Expr {
   Expr *expr;
   UnaryExpr() : expr(0) {}
   bool has_errors() const;
};

template<AstType Type>
struct UnaryExprDerived : UnaryExpr {
   UnaryExprDerived() { type_ = Type; }
   static bool classof(const Ast *ast) { return ast->type() == Type; }
};

struct SignExpr : public UnaryExprDerived<AstType::SignExpr> {
   enum Kind { Positive, Negative };
   Kind kind;
   SignExpr(Kind k) : kind(k) {}
   void accept(AstVisitor *v);
};

struct IncrExpr : public UnaryExprDerived<AstType::IncrExpr> {
   enum Kind { Positive, Negative };
   Kind kind;
   bool preincr;
   IncrExpr(Kind k, bool pre = false) : kind(k), preincr(pre) {}
   void accept(AstVisitor *v);
   std::string describe() const;
};

struct NegExpr : public UnaryExprDerived<AstType::NegExpr> { 
   void accept(AstVisitor *v);
};

struct AddrExpr : public UnaryExprDerived<AstType::AddrExpr> { 
   void accept(AstVisitor *v);
};

struct DerefExpr : public UnaryExprDerived<AstType::DerefExpr> { 
   void accept(AstVisitor *v);
};

struct CallExpr : public ExprDerived<AstType::CallExpr> {
   Expr *func;
   std::vector<Expr *> args;
   CallExpr() : func(0) {}
   void accept(AstVisitor *v);
   bool has_errors() const;
};

struct IndexExpr : public ExprDerived<AstType::IndexExpr> {
   Expr *base, *index;
   IndexExpr() : base(0), index(0) {}
   void accept(AstVisitor *v);
   bool has_errors() const;
};

struct FieldExpr : public ExprDerived<AstType::FieldExpr> {
   Expr *base;
   SimpleIdent *field;
   bool pointer;

   FieldExpr() : base(0), field(0) {}
   void accept(AstVisitor *v);
   bool has_errors() const;
};

struct CondExpr : public ExprDerived<AstType::CondExpr> {
   Expr *cond, *then, *els;
   CondExpr() : cond(0), then(0), els(0) {}
   void accept(AstVisitor *v);
   bool has_errors() const;
};

struct ExprList : public ExprDerived<AstType::ExprList> {
   std::vector<Expr*> exprs;
   void accept(AstVisitor *v);
   bool has_errors() const;
};

struct TypeSpec : public AstDerived<AstType::TypeSpec> {
   static const std::string QualifiersNames[];

   enum Qualifiers {
      Const    = 0, Volatile = 1, Mutable = 2, 
      Register = 3, Auto     = 4, Extern  = 5
   };

   bool                    reference;
   std::vector<Qualifiers> qual;
   FullIdent              *id;

   TypeSpec() : id(0), reference(false) {}
   TypeSpec(FullIdent *_id) : id(_id), reference(false) {}
   void accept(AstVisitor *v);
   bool has_errors() const;
   bool is(Qualifiers q) const;
   std::string typestr() const;

   bool is_template() const { return !id->subtypes.empty(); }
   SimpleIdent *get_potential_namespace_or_class() const;
};

// Declarations ////////////////////////////////////////////

struct FuncDecl : public AstDerived<AstType::FuncDecl> {
   struct Param {
      Pos ini, fin;
      TypeSpec *typespec;
      std::string name;
      Param() : typespec(0) {}
   };

   TypeSpec *return_typespec;
   SimpleIdent *id;
   std::vector<Param*> params;
   Block* block;
   
   FuncDecl(SimpleIdent *_id) : id(_id) {}

   std::string funcname() const;
   void accept(AstVisitor *v);
   bool has_errors() const;
};

struct StructDecl : public AstDerived<AstType::StructDecl> {
   SimpleIdent *id;
   std::vector<DeclStmt*> decls;
   
   StructDecl() : id(0) {}
   void accept(AstVisitor *v);
   bool has_errors() const;
   std::string struct_name() const { return id->name; }
   std::string typestr() const;
   int num_fields() const;
};

struct TypedefDecl : public AstDerived<AstType::TypedefDecl> {
   Decl *decl;
   TypedefDecl() : decl(0) {}
   void accept(AstVisitor *v);
   bool has_errors() const;
};

struct EnumDecl : public AstDerived<AstType::EnumDecl> {
   struct Value {
      std::string id;
      bool has_val;
      int val;
      Value(std::string _id) : id(_id), has_val(false) {}
   };

   std::string name;
   std::vector<Value> values;

   void accept(AstVisitor *v);
};

// AstVisitor

class ReadWriter {
   int _indent;
   std::istream *_in;
   std::ostream *_out;
   std::vector<std::ostringstream*> _stack; // temporary output

   static const int TAB_WIDTH = 3;
   
protected:
   enum OutType { normal, beginl };

   std::ostream& out(OutType typ = normal);
   void indent(int x) { 
      _indent += x; 
      assert(_indent >= 0);
   }

   std::istream& in() { return *_in; }

   void push() { _stack.push_back(new std::ostringstream()); }

   std::string pop()  {
      std::string res = _stack.back()->str();
      delete _stack.back();
      _stack.pop_back();
      return res;
   }

public:
   ReadWriter(std::ostream *o)     : _indent(0),         _out(o) {}
   ReadWriter(std::istream *i = 0, 
              std::ostream *o = 0) : _indent(0), _in(i), _out(o) {}

   std::string indentation() const { 
      return std::string(_indent * TAB_WIDTH, ' '); 
   }

   void set_in(std::istream *i)  { _in  = i; }
   void set_out(std::ostream *o) { _out = o; }
};

class AstVisitor {
public:
   void visit(Ast *x) { x->accept(this); }

   virtual void visit_program(Program*)              { assert(false); }
   virtual void visit_include(Include*)              { assert(false); }
   virtual void visit_macro(Macro *)                 { assert(false); }
   virtual void visit_using(Using *)                 { assert(false); }
   virtual void visit_funcdecl(FuncDecl *)           { assert(false); }
   virtual void visit_structdecl(StructDecl *)       { assert(false); }
   virtual void visit_typedefdecl(TypedefDecl *)     { assert(false); }
   virtual void visit_enumdecl(EnumDecl *)           { assert(false); }
   virtual void visit_typespec(TypeSpec *)           { assert(false); }
   virtual void visit_block(Block *)                 { assert(false); }
   virtual void visit_simpleident(SimpleIdent *)     { assert(false); }
   virtual void visit_templateident(TemplateIdent *) { assert(false); }
   virtual void visit_fullident(FullIdent *)         { assert(false); }
   virtual void visit_binaryexpr(BinaryExpr *)       { assert(false); }
   virtual void visit_vardecl(VarDecl *)             { assert(false); }
   virtual void visit_arraydecl(ArrayDecl *)         { assert(false); }
   virtual void visit_objdecl(ObjDecl *)             { assert(false); }
   virtual void visit_declstmt(DeclStmt *)           { assert(false); }
   virtual void visit_exprstmt(ExprStmt *)           { assert(false); }
   virtual void visit_ifstmt(IfStmt *)               { assert(false); }
   virtual void visit_forstmt(ForStmt *)             { assert(false); }
   virtual void visit_whilestmt(WhileStmt *)         { assert(false); }
   virtual void visit_jumpstmt(JumpStmt *)           { assert(false); }
   virtual void visit_callexpr(CallExpr *)           { assert(false); }
   virtual void visit_indexexpr(IndexExpr *)         { assert(false); }
   virtual void visit_fieldexpr(FieldExpr *)         { assert(false); }
   virtual void visit_condexpr(CondExpr *)           { assert(false); }
   virtual void visit_exprlist(ExprList *)           { assert(false); }
   virtual void visit_signexpr(SignExpr *)           { assert(false); }
   virtual void visit_increxpr(IncrExpr *)           { assert(false); }
   virtual void visit_negexpr(NegExpr *)             { assert(false); }
   virtual void visit_addrexpr(AddrExpr *)           { assert(false); }
   virtual void visit_derefexpr(DerefExpr *)         { assert(false); }
   virtual void visit_literal(Literal *)             { assert(false); }

   virtual void visit_errorstmt(StmtError *)         { assert(false); }
   virtual void visit_errorexpr(ExprError *)         { assert(false); }
};   

// Visit implementations
inline void Program::accept(AstVisitor *v)       { v->visit_program(this); }
inline void Include::accept(AstVisitor *v)       { v->visit_include(this); }
inline void Macro::accept(AstVisitor *v)         { v->visit_macro(this); }
inline void Using::accept(AstVisitor *v)         { v->visit_using(this); }
inline void FuncDecl::accept(AstVisitor* v)      { v->visit_funcdecl(this); }
inline void StructDecl::accept(AstVisitor* v)    { v->visit_structdecl(this); }
inline void TypedefDecl::accept(AstVisitor* v)   { v->visit_typedefdecl(this); }
inline void EnumDecl::accept(AstVisitor* v)      { v->visit_enumdecl(this); }
inline void TypeSpec::accept(AstVisitor *v)      { v->visit_typespec(this); }
inline void Block::accept(AstVisitor *v)         { v->visit_block(this); }
inline void SimpleIdent::accept(AstVisitor *v)   { v->visit_simpleident(this); }
inline void TemplateIdent::accept(AstVisitor *v) { v->visit_templateident(this); }
inline void FullIdent::accept(AstVisitor *v)     { v->visit_fullident(this); }
inline void BinaryExpr::accept(AstVisitor *v)    { v->visit_binaryexpr(this); }
inline void VarDecl::accept(AstVisitor *v)       { v->visit_vardecl(this); }
inline void ArrayDecl::accept(AstVisitor *v)     { v->visit_arraydecl(this); }
inline void ObjDecl::accept(AstVisitor *v)       { v->visit_objdecl(this); }
inline void DeclStmt::accept(AstVisitor *v)      { v->visit_declstmt(this); }
inline void ExprStmt::accept(AstVisitor *v)      { v->visit_exprstmt(this); }
inline void IfStmt::accept(AstVisitor *v)        { v->visit_ifstmt(this); }
inline void ForStmt::accept(AstVisitor *v)       { v->visit_forstmt(this); }
inline void WhileStmt::accept(AstVisitor *v)     { v->visit_whilestmt(this); }
inline void JumpStmt::accept(AstVisitor *v)      { v->visit_jumpstmt(this); }
inline void CallExpr::accept(AstVisitor *v)      { v->visit_callexpr(this); }
inline void IndexExpr::accept(AstVisitor *v)     { v->visit_indexexpr(this); }
inline void FieldExpr::accept(AstVisitor *v)     { v->visit_fieldexpr(this); }
inline void CondExpr::accept(AstVisitor *v)      { v->visit_condexpr(this); }
inline void ExprList::accept(AstVisitor *v)      { v->visit_exprlist(this); }
inline void SignExpr::accept(AstVisitor *v)      { v->visit_signexpr(this); }
inline void IncrExpr::accept(AstVisitor *v)      { v->visit_increxpr(this); }
inline void NegExpr::accept(AstVisitor *v)       { v->visit_negexpr(this); }
inline void AddrExpr::accept(AstVisitor *v)      { v->visit_addrexpr(this); }
inline void DerefExpr::accept(AstVisitor *v)     { v->visit_derefexpr(this); }
inline void Literal::accept(AstVisitor *v)       { v->visit_literal(this); }

inline void StmtError::accept(AstVisitor *v)     { v->visit_errorstmt(this); }
inline void ExprError::accept(AstVisitor *v)     { v->visit_errorexpr(this); }

inline void Stmt::accept(AstVisitor *v)          { assert(false); }


#endif
