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

   bool HasEndLine() const;
   bool EndsWithEmptyLine() const;
   void RemoveEndLines();
   void OnlyOneEndLineAtEnd();

   bool StartsWithEndLine() const { 
      return !items.empty() and items.front().kind == Comment::endline; 
   }
   bool EndsWithEndLine() const { 
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
   Identifier,
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
// virtual ~Ast() {} // <-- Do we need this???
    void AddError(std::string msg);
    void AddError(Pos ini, Pos fin, std::string msg);
    bool HasErrors() const { return !errors.empty(); }
 AstType Type() const { return type_; }

protected:
   AstType  type_;
};

template<AstType T>
struct AstDerived : Ast {
   static bool classof(const Ast *ast) { return ast->Type() == T; }
   AstDerived() { type_ = T; }
};

struct Program : public AstDerived<AstType::Program> {
   std::vector<Ast*> nodes;

   void add(Ast* n) { 
      nodes.push_back(n);
      n->parent = this; 
   }
};

struct Include : public AstDerived<AstType::Include> {
   std::string filename;
   bool global;

   Include(std::string f = "", bool g = false) 
      : filename(f), global(g) {}
};

struct Macro : public AstDerived<AstType::Macro> {
   std::string macro;
   Macro(std::string m) : macro(m) {}
};

struct Using : public AstDerived<AstType::Using> {
   std::string namespc;
};

// Statements //////////////////////////////////////////////

struct Expr;
struct Stmt : public Ast {};

template<AstType T>
struct StmtDerived : Stmt {
   static bool classof(const Ast *ast) { return ast->Type() == T; }
   StmtDerived() { type_ = T; }
};

struct StmtError : public StmtDerived<AstType::StmtError> {
   std::string code;
};

struct ExprStmt : public StmtDerived<AstType::ExprStmt> {
   Expr *expr = 0;
   bool is_return = false;
};

struct IfStmt : public StmtDerived<AstType::IfStmt> {
   Expr *cond = 0;
   Stmt *then = 0, *els = 0;
};

struct ForStmt : public StmtDerived<AstType::ForStmt> {
   Stmt *init = 0;
   Expr *cond = 0, *post = 0;
   Stmt *substmt = 0;
};

struct WhileStmt : public StmtDerived<AstType::WhileStmt> {
   Expr *cond = 0;
   Stmt *substmt = 0;
};

struct Decl : public Ast {
   enum Kind { Normal, Pointer };
   TypeSpec *typespec = 0;
   std::string name;
};

template<AstType T>
struct DeclDerived : Decl {
   static bool classof(const Ast *ast) { return ast->Type() == T; }
   DeclDerived() { type_ = T; }
};

struct VarDecl : public DeclDerived<AstType::VarDecl> {
   Kind kind = Normal;
};

struct ArrayDecl : public DeclDerived<AstType::ArrayDecl> {
   std::vector<Expr*> sizes;
   Kind kind = Normal;
   std::string TypeStr() const;
};

struct ObjDecl : public DeclDerived<AstType::ObjDecl> {
   std::vector<Expr *> args;
};

struct DeclStmt : public StmtDerived<AstType::DeclStmt> {
   TypeSpec *typespec;
   struct Item {
      Decl *decl = 0;
      Expr *init = 0;
   };
   std::vector<Item> items;
};

struct JumpStmt : public StmtDerived<AstType::JumpStmt> {
   enum Kind { Unknown = -1, Break = 0, Continue = 1, Goto = 2 };

   Kind kind = Unknown;
   std::string label;

   static Kind keyword2type(std::string s);
};

struct Block : public StmtDerived<AstType::Block> {
   std::vector<Stmt*> stmts;
};

// Expressions /////////////////////////////////////////////

struct Expr : public Ast {
   enum Kind { 
      Unknown,
      // pm_expression 
      Multiplicative, Additive, Shift, Relational, Equality, 
      BitAnd, BitXor, BitOr, LogicalAnd, LogicalOr, Conditional,
      Eq, Comma, Infinite
   };

   bool paren = false; // if this is true, comments will have an extra element!

   static Kind tok2kind(Token::Type toktyp);
   static bool right_associative(Kind t);

   struct Error;
};

template<AstType T>
struct ExprDerived : Expr {
   static bool classof(const Ast *ast) { return ast->Type() == T; }
   ExprDerived() { type_ = T; }
};

struct ExprError : public ExprDerived<AstType::ExprError> {
   std::string code;
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

   static std::string escape(char c, char delim);
   static std::string escape(std::string s, char delim);
};

struct Identifier : ExprDerived<AstType::Identifier> {
   std::string name;
   std::vector<Identifier*> prefix;
   std::vector<TypeSpec*> subtypes;
   bool is_namespace = false; // (used by the interpreter)

   Identifier(std::string name_) : name(name_) {}

   bool is_template() const { return !subtypes.empty(); }
   std::string TypeStr() const;
   void shift(std::string new_id);
   Identifier *get_potential_namespace_or_class() const;
   std::vector<Identifier*> get_non_namespaces();

   static bool classof(const Ast *ast) { 
      return ast->Type() == AstType::Identifier; 
   }
};

struct BinaryExpr : public ExprDerived<AstType::BinaryExpr> {
   Kind kind;
   std::string op, str;
   Expr *left, *right;

   BinaryExpr(Kind k = Unknown) : kind(k), op("") {}

   void set(Expr::Kind _kind);
};

struct UnaryExpr : public Expr {
   Expr *expr = 0;
};

template<AstType T>
struct UnaryExprDerived : UnaryExpr {
   UnaryExprDerived() { type_ = T; }
   static bool classof(const Ast *ast) { return ast->Type() == T; }
};

struct SignExpr : public UnaryExprDerived<AstType::SignExpr> {
   enum Kind { Positive, Negative };
   Kind kind;
   SignExpr(Kind k) : kind(k) {}
};

struct IncrExpr : public UnaryExprDerived<AstType::IncrExpr> {
   enum Kind { Positive, Negative };
   Kind kind;
   bool preincr;
   IncrExpr(Kind k, bool pre = false) : kind(k), preincr(pre) {}
};

struct NegExpr   : public UnaryExprDerived<AstType::NegExpr>   {};
struct AddrExpr  : public UnaryExprDerived<AstType::AddrExpr>  {};
struct DerefExpr : public UnaryExprDerived<AstType::DerefExpr> {};

struct CallExpr : public ExprDerived<AstType::CallExpr> {
   Expr *func = 0;
   std::vector<Expr *> args;
};

struct IndexExpr : public ExprDerived<AstType::IndexExpr> {
   Expr *base = 0, *index = 0;
};

struct FieldExpr : public ExprDerived<AstType::FieldExpr> {
   Expr *base = 0;
   std::string field;
   bool pointer;
};

struct CondExpr : public ExprDerived<AstType::CondExpr> {
   Expr *cond = 0, *then = 0, *els = 0;
};

struct ExprList : public ExprDerived<AstType::ExprList> {
   std::vector<Expr*> exprs;
};

struct TypeSpec : public AstDerived<AstType::TypeSpec> {
   static const std::string QualifiersNames[];

   enum Qualifiers {
      Const    = 0, Volatile = 1, Mutable = 2, 
      Register = 3, Auto     = 4, Extern  = 5
   };

   bool                    reference = false;
   std::vector<Qualifiers> qual;
   Identifier              *id = 0;

   TypeSpec() = default;
   TypeSpec(Identifier *_id) : id(_id), reference(false) {}
   bool is(Qualifiers q) const;
   std::string TypeStr() const;

   bool is_template() const { return !id->subtypes.empty(); }
   Identifier *get_potential_namespace_or_class() const;
};

// Declarations ////////////////////////////////////////////

struct FuncDecl : public AstDerived<AstType::FuncDecl> {
   struct Param {
      Pos ini, fin;
      TypeSpec *typespec = 0;
      std::string name;
   };

   TypeSpec *return_typespec;
   Identifier *id;
   std::vector<Param*> params;
   Block* block;
   
   FuncDecl(Identifier *_id) : id(_id) {}

   std::string funcname() const { return id->name; }
};

struct StructDecl : public AstDerived<AstType::StructDecl> {
   std::string name;
   std::vector<DeclStmt*> decls;
   
   std::string TypeStr() const;
   int num_fields() const;
};

struct TypedefDecl : public AstDerived<AstType::TypedefDecl> {
   Decl *decl = 0;
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
};

std::string Describe(Ast *ast);
bool HasErrors(Ast *ast);
bool IsReadExpr(Ast *ast);
bool IsWriteExpr(Ast *ast);
bool IsAssignment(Ast *ast);
void CollectRights(Ast *ast, std::list<Expr*>& L);

#endif
