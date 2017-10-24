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
// virtual ~Ast() {} // <-- Do we need this???
    void AddError(std::string msg);
    void AddError(Pos ini, Pos fin, std::string msg);
    bool HasErrors() const { return !errors.empty(); }
 AstType type() const { return type_; }

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

template<AstType Type>
struct StmtDerived : Stmt {
   static bool classof(const Ast *ast) { return ast->type() == Type; }
   StmtDerived() { type_ = Type; }
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

struct ForStmt : public StmtDerived<AstType::ForStmt> { // while + for
   Stmt *init = 0;
   Expr *cond = 0, *post = 0;
   Stmt *substmt = 0;
};

struct WhileStmt : public StmtDerived<AstType::WhileStmt> { // while + for
   Expr *cond = 0;
   Stmt *substmt = 0;
};

struct Decl : public Ast {
   enum Kind { Normal, Pointer };
   TypeSpec *typespec = 0;
   std::string name;
};

template<AstType Type>
struct DeclDerived : Decl {
   static bool classof(const Ast *ast) { return ast->type() == Type; }
   DeclDerived() { type_ = Type; }
};

struct VarDecl : public DeclDerived<AstType::VarDecl> {
   Kind kind = Normal;
};

struct ArrayDecl : public DeclDerived<AstType::ArrayDecl> {
   std::vector<Expr*> sizes;
   Kind kind = Normal;
   std::string typestr() const;
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
   struct Op2KindInitializer { Op2KindInitializer(); }; // init _op2kind

   bool paren; // if this is true, comments will have an extra element!

   Expr() : paren(false) {}

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
   static bool classof(const Ast *ast) { return ast->type() == Type; }
   ExprDerived() { type_ = Type; }
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

struct SimpleIdent : ExprDerived<AstType::SimpleIdent> {
   std::string name;
   bool is_namespace = false; // (used by the interpreter)

   SimpleIdent(std::string _name = "") : name(_name), is_namespace(false) {}
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

   static bool classof(const Ast *ast) { 
      return ast->type() == AstType::TemplateIdent; 
   }
};

struct FullIdent : TemplateIdent {
   std::vector<TemplateIdent*> prefix;  // for classes & namespaces;

   FullIdent(std::string name_) : TemplateIdent(name_) {
      type_ = AstType::FullIdent;
   }
   std::string typestr() const;

   void shift(std::string new_id);
   SimpleIdent *get_potential_namespace_or_class() const;
   std::vector<TemplateIdent*> get_non_namespaces();

   static bool classof(const Ast *ast) { 
      return ast->type() == AstType::FullIdent; 
   }
};

struct BinaryExpr : public ExprDerived<AstType::BinaryExpr> {
   Kind kind;
   std::string op;
   std::string str;
   Expr *left, *right;

   BinaryExpr(Kind k = Unknown) : kind(k), op("") {}

   void set(Expr::Kind _kind);
   void collect_rights(std::list<Expr*>& L) const;
};

struct UnaryExpr : public Expr {
   Expr *expr;
   UnaryExpr() : expr(0) {}
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
   SimpleIdent *field = 0;
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
   FullIdent              *id = 0;

   TypeSpec() = default;
   TypeSpec(FullIdent *_id) : id(_id), reference(false) {}
   bool is(Qualifiers q) const;
   std::string typestr() const;

   bool is_template() const { return !id->subtypes.empty(); }
   SimpleIdent *get_potential_namespace_or_class() const;
};

// Declarations ////////////////////////////////////////////

struct FuncDecl : public AstDerived<AstType::FuncDecl> {
   struct Param {
      Pos ini, fin;
      TypeSpec *typespec = 0;
      std::string name;
   };

   TypeSpec *return_typespec;
   SimpleIdent *id;
   std::vector<Param*> params;
   Block* block;
   
   FuncDecl(SimpleIdent *_id) : id(_id) {}

   std::string funcname() const { return id->name; }
};

struct StructDecl : public AstDerived<AstType::StructDecl> {
   SimpleIdent *id = 0;
   std::vector<DeclStmt*> decls;
   
   std::string struct_name() const { return id->name; }
   std::string typestr() const;
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

#endif
