#ifndef AST_H
#define AST_H

#include <assert.h>
#include <string>
#include <sstream>
#include <vector>
#include <list>
#include <map>
#include <algorithm>
#include "input.hh"

class AstVisitor;

struct Error;
struct CommentSeq;
struct TypeSpec;

std::ostream& operator<<(std::ostream& o, CommentSeq* C);

struct AstNode {
                      Pos ini, fin;
      std::vector<Error*> errors;
 std::vector<CommentSeq*> comments;

   virtual            ~AstNode() {}
   virtual        void accept(AstVisitor* v) = 0;
   virtual         int num_children() const { return 0; }
   virtual    AstNode* child(int n)   const { return 0; }
   virtual        bool has_errors()   const { return !errors.empty(); }
   virtual std::string describe()     const { return "UNIMPLEMENTED"; }
                 Range span()         const { return Range(ini, fin); }

   template<typename X>
                  bool is()           const { return dynamic_cast<const X*>(this) != 0; }
};

struct Error {
   Pos pos;
   std::string msg;
   Error(Pos p, std::string m) : pos(p), msg(m) {}
};

struct Comment {
   enum Kind { none, singleline, multiline, endline };
   Kind kind;
   std::string text;
   Comment(Kind k = none) : kind(k) {}
};

struct CommentSeq {
   std::vector<Comment> items;
   bool has_endl() {
      for (Comment& c : items) {
         if (c.kind == Comment::endline) {
            return true;
         }
      }
      return false;
   }
   bool starts_with_endl()  const { return !items.empty() and items.front().kind == Comment::endline; }
   bool ends_with_endl()    const { return !items.empty() and items.back().kind  == Comment::endline; }
   bool ends_with_empty_line() const { 
      const int sz = items.size();
      return sz >= 2 and 
         (items[sz-2].kind == Comment::endline and items[sz-1].kind == Comment::endline);
   }

   void remove_endls() {
      items.erase(std::remove_if(items.begin(), items.end(), 
                                 [](Comment& c) {
                                    return c.kind == Comment::endline; 
                                 }),
                  items.end());
   }

   void only_one_endl_at_end() {
      if (items.empty() or items.back().kind != Comment::endline) {
         return;
      }
      int i = items.size()-1;
      while (true) {
         if (items[i-1].kind != Comment::endline) {
            break;
         }
         i--;
      }
      items.resize(i+1);
   }
};

struct Program : public AstNode {
   std::vector<AstNode*> nodes;

   int      num_children() const { return nodes.size(); }
   AstNode* child(int n)         { return nodes[n]; }
   void     add(AstNode* n)      { nodes.push_back(n); }
   void     accept(AstVisitor* v);

   bool     has_errors() const;
};

struct Include : public AstNode {
   std::string filename;
   bool global;

   Include(std::string _filename = "", bool _global = false) 
      : filename(_filename), global(_global) {}

   void accept(AstVisitor* v);
};

struct Macro : public AstNode {
   std::string macro;
   Macro(std::string _macro) : macro(_macro) {}
   void accept(AstVisitor *v);
};

struct Using : public AstNode {
   std::string namespc;
   void accept(AstVisitor *v);
};

// Statements //////////////////////////////////////////////

struct Expr;

struct Stmt : public AstNode {
   void accept(AstVisitor *v);
   struct Error;
};

struct Stmt::Error : public Stmt {
   std::string code;
   void accept(AstVisitor *v);
};

struct ExprStmt : public Stmt {
   Expr *expr;
   bool is_return;
   ExprStmt() : expr(0), is_return(false) {}
   void accept(AstVisitor *v);
   bool has_errors() const;
   std::string describe() const;
};

struct IfStmt : public Stmt {
   Expr *cond;
   Stmt *then, *els;

   IfStmt() : cond(0), then(0), els(0) {}
   void accept(AstVisitor *v);
   bool has_errors() const;
};

struct IterStmt : public Stmt { // while + for
   Stmt *init;
   Expr *cond, *post;
   Stmt *substmt;

   IterStmt() : cond(0), init(0), substmt(0), post(0) {}
   void accept(AstVisitor *v);
   bool is_for() { return init != 0 and post != 0; }
   bool has_errors() const;
};

struct Decl : public AstNode {
   enum Kind { Normal, Pointer };
   TypeSpec *typespec;
   std::string name;
   Decl() : typespec(0) {}
};

struct VarDecl : public Decl {
   Kind kind;
   VarDecl() : kind(Normal) {}
   void accept(AstVisitor *v);
};

struct ArrayDecl : public Decl {
   Expr *size;
   Kind kind;
   ArrayDecl() : size(0), kind(Normal) {}
   void accept(AstVisitor *v);
   std::string type_str() const;
};

struct ObjDecl : public Decl {
   std::vector<Expr *> args;
   void accept(AstVisitor *v);
};

struct DeclStmt : public Stmt {
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

struct JumpStmt : public Stmt {
   enum Kind { Unknown = -1, Break = 0, Continue = 1, Goto = 2 };

   Kind kind;
   std::string label;

   JumpStmt() : kind(Unknown) {}
   void accept(AstVisitor *v);

   static Kind keyword2type(std::string s);
};

struct Block : public Stmt {
   std::vector<Stmt*> stmts;
   void accept(AstVisitor *v);
   bool has_errors() const;
};

// Expressions /////////////////////////////////////////////

struct Expr : public AstNode {
   enum Kind { 
      Unknown,
      // pm_expression 
      Multiplicative, Additive, Shift, Relational, Equality, 
      BitAnd, BitXor, BitOr, LogicalAnd, LogicalOr, Conditional,
      Assignment, Comma, Infinite
   };
   struct Op2KindInitializer { Op2KindInitializer(); }; // init _op2kind

   bool paren; // if this is true, comments will have an extra element!

   Expr() : paren(false) {}

   virtual bool is_read_expr()  const { return false; }
   virtual bool is_write_expr() const { return false; }
   virtual bool is_assignment() const { return false; }
   virtual void collect_rights(std::list<Expr*>& L) const {}

   static std::map<std::string, Kind> _op2kind;
   static std::map<Token::Kind, Kind> _tok2kind;
   static Kind op2kind(std::string op);
   static Kind tok2kind(Token::Kind toktyp);
   static Op2KindInitializer initializer;
   static bool right_associative(Kind t);

   struct Error;
};

struct Expr::Error : public Expr {
   std::string code;
   void accept(AstVisitor *v);
};

struct Literal : public Expr {
   enum Type { Bool, Int, String, Char, Float, Double };
   struct StringData {
      std::string *s;
      bool L;
   };
   union Data {
      bool       as_bool;
      int        as_int;
      double     as_double;
      StringData as_string;
   };
   Type type;
   Data val;
   bool L; // for strings

   Literal(Type t) : type(t) {}
   void accept(AstVisitor *v);

   static std::string escape(std::string s, char delim);
};

struct Ident : public Expr {
   std::string name;
   std::vector<TypeSpec*> subtypes; // for templates
   std::vector<Ident*> prefix;  // for classes & namespaces;

   Ident(std::string _name = "") : name(_name) {}
   void accept(AstVisitor *v);
   bool has_errors() const;
   std::string typestr() const;

   void shift(std::string new_id);
};

struct BinaryExpr : public Expr {
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

struct SignExpr : public UnaryExpr {
   enum Kind { Positive, Negative };
   Kind kind;
   SignExpr(Kind k) : kind(k) {}
   void accept(AstVisitor *v);
};

struct IncrExpr : public UnaryExpr {
   enum Kind { Positive, Negative };
   Kind kind;
   bool preincr;
   IncrExpr(Kind k, bool pre = false) : kind(k), preincr(pre) {}
   void accept(AstVisitor *v);
   std::string describe() const;
};

struct NegExpr : public UnaryExpr { 
   void accept(AstVisitor *v);
};

struct AddrExpr : public UnaryExpr { 
   void accept(AstVisitor *v);
};

struct DerefExpr : public UnaryExpr { 
   void accept(AstVisitor *v);
};

struct CallExpr : public Expr {
   Expr *func;
   std::vector<Expr *> args;
   CallExpr() : func(0) {}
   void accept(AstVisitor *v);
   bool has_errors() const;
};

struct IndexExpr : public Expr {
   Expr *base, *index;
   IndexExpr() : base(0), index(0) {}
   void accept(AstVisitor *v);
   bool has_errors() const;
};

struct FieldExpr : public Expr {
   Expr *base;
   Ident *field;
   bool pointer;

   FieldExpr() : base(0), field(0) {}
   void accept(AstVisitor *v);
   bool has_errors() const;
};

struct CondExpr : public Expr {
   Expr *cond, *then, *els;
   CondExpr() : cond(0), then(0), els(0) {}
   void accept(AstVisitor *v);
   bool has_errors() const;
};

struct ExprList : public Expr {
   std::vector<Expr*> exprs;
   void accept(AstVisitor *v);
   bool has_errors() const;
};

struct TypeSpec : public AstNode {
   static const std::string QualifiersNames[];

   enum Qualifiers {
      Const    = 0, Volatile = 1, Mutable = 2, 
      Register = 3, Auto     = 4, Extern  = 5
   };

   bool                    reference;
   std::vector<Qualifiers> qual;
   Ident                  *id;

   TypeSpec() : id(0), reference(false) {}
   void accept(AstVisitor *v);
   bool has_errors() const;
   std::string typestr() const;

   bool is_template() const { return !id->subtypes.empty(); }
};

// Declarations ////////////////////////////////////////////

struct FuncDecl : public AstNode {
   struct Param {
      TypeSpec *typespec;
      std::string name;
      Param() : typespec(0) {}
   };

   TypeSpec *return_type;
   Ident *id;
   std::vector<Param*> params;
   Block* block;
   
   FuncDecl(Ident *_id) : id(_id) {}

   std::string funcname() const;
   void accept(AstVisitor *v);
   bool has_errors() const;
};

struct StructDecl : public AstNode {
   Ident *id;
   std::vector<DeclStmt*> decls;
   
   StructDecl() : id(0) {}
   void accept(AstVisitor *v);
   bool has_errors() const;
   std::string struct_name() const { return id->name; }
   std::string type_str() const;
   int num_fields() const;
};

struct TypedefDecl : public AstNode {
   Decl *decl;
   TypedefDecl() : decl(0) {}
   void accept(AstVisitor *v);
   bool has_errors() const;
};

struct EnumDecl : public AstNode {
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
   ReadWriter(std::ostream *o)                          : _indent(0),         _out(o) {}
   ReadWriter(std::istream *i = 0, std::ostream *o = 0) : _indent(0), _in(i), _out(o) {}

   std::string indentation() const { 
      return std::string(_indent * TAB_WIDTH, ' '); 
   }

   void set_in(std::istream *i)  { _in  = i; }
   void set_out(std::ostream *o) { _out = o; }
};

class AstVisitor {
public:
   void visit(AstNode *x) { x->accept(this); }

   virtual void visit_program(Program*)           { assert(false); }
   virtual void visit_include(Include*)           { assert(false); }
   virtual void visit_macro(Macro *)              { assert(false); }
   virtual void visit_using(Using *)              { assert(false); }
   virtual void visit_funcdecl(FuncDecl *)        { assert(false); }
   virtual void visit_structdecl(StructDecl *)    { assert(false); }
   virtual void visit_typedefdecl(TypedefDecl *)  { assert(false); }
   virtual void visit_enumdecl(EnumDecl *)        { assert(false); }
   virtual void visit_typespec(TypeSpec *)        { assert(false); }
   virtual void visit_block(Block *)              { assert(false); }
   virtual void visit_ident(Ident *)              { assert(false); }
   virtual void visit_binaryexpr(BinaryExpr *)    { assert(false); }
   virtual void visit_vardecl(VarDecl *)          { assert(false); }
   virtual void visit_arraydecl(ArrayDecl *)      { assert(false); }
   virtual void visit_objdecl(ObjDecl *)          { assert(false); }
   virtual void visit_declstmt(DeclStmt *)        { assert(false); }
   virtual void visit_exprstmt(ExprStmt *)        { assert(false); }
   virtual void visit_ifstmt(IfStmt *)            { assert(false); }
   virtual void visit_iterstmt(IterStmt *)        { assert(false); }
   virtual void visit_jumpstmt(JumpStmt *)        { assert(false); }
   virtual void visit_callexpr(CallExpr *)        { assert(false); }
   virtual void visit_indexexpr(IndexExpr *)      { assert(false); }
   virtual void visit_fieldexpr(FieldExpr *)      { assert(false); }
   virtual void visit_condexpr(CondExpr *)        { assert(false); }
   virtual void visit_exprlist(ExprList *)        { assert(false); }
   virtual void visit_signexpr(SignExpr *)        { assert(false); }
   virtual void visit_increxpr(IncrExpr *)        { assert(false); }
   virtual void visit_negexpr(NegExpr *)          { assert(false); }
   virtual void visit_addrexpr(AddrExpr *)        { assert(false); }
   virtual void visit_derefexpr(DerefExpr *)      { assert(false); }
   virtual void visit_literal(Literal *)          { assert(false); }

   virtual void visit_errorstmt(Stmt::Error *)    { assert(false); }
   virtual void visit_errorexpr(Expr::Error *)    { assert(false); }
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
inline void Ident::accept(AstVisitor *v)         { v->visit_ident(this); }
inline void BinaryExpr::accept(AstVisitor *v)    { v->visit_binaryexpr(this); }
inline void VarDecl::accept(AstVisitor *v)       { v->visit_vardecl(this); }
inline void ArrayDecl::accept(AstVisitor *v)     { v->visit_arraydecl(this); }
inline void ObjDecl::accept(AstVisitor *v)       { v->visit_objdecl(this); }
inline void DeclStmt::accept(AstVisitor *v)      { v->visit_declstmt(this); }
inline void ExprStmt::accept(AstVisitor *v)      { v->visit_exprstmt(this); }
inline void IfStmt::accept(AstVisitor *v)        { v->visit_ifstmt(this); }
inline void IterStmt::accept(AstVisitor *v)      { v->visit_iterstmt(this); }
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

inline void Stmt::Error::accept(AstVisitor *v)   { v->visit_errorstmt(this); }
inline void Expr::Error::accept(AstVisitor *v)   { v->visit_errorexpr(this); }

inline void Stmt::accept(AstVisitor *v)          { assert(false); }


#endif
