#ifndef AST_H
#define AST_H

#include <assert.h>
#include <string>
#include <vector>
#include <map>
#include "input.hh"

class AstVisitor;

struct CommentNode;

std::ostream& operator<<(std::ostream& o, CommentNode* C);

struct AstNode {
   Pos ini, fin;
   std::vector<CommentNode*> comment_nodes;
   virtual ~AstNode() {}
   virtual int num_children()    const { return 0; }
   virtual AstNode* child(int n) const { return 0; }
   virtual void visit(AstVisitor* v) = 0;

   template<typename X>
   bool is() { return dynamic_cast<X*>(this) != 0; }
};

struct Comment {
   enum Type { singleline, multiline };
   Type type;
   std::string text;
   bool endl;

   Comment(Type t) : type(t), endl(false) {}
};

struct CommentNode : public AstNode {
   std::vector<Comment> comments;
   void visit(AstVisitor* v);
   bool endl() const { return !comments.empty() and comments.back().endl; }
};

struct Program : public AstNode {
   std::vector<AstNode*> nodes;

   int      num_children() const { return nodes.size(); }
   AstNode* child(int n)         { return nodes[n]; }
   void     add(AstNode* n)      { nodes.push_back(n); }
   void     visit(AstVisitor* v);
};

struct Include : public AstNode {
   std::string filename;
   bool global;

   Include(std::string _filename, bool _global) 
      : filename(_filename), global(_global) {}

   void visit(AstVisitor* v);
};

struct Macro : public AstNode {
   std::string macro;
   Macro(std::string _macro) : macro(_macro) {}
   void visit(AstVisitor *v);
};

struct Using : public AstNode {
   std::string namespc;
   void visit(AstVisitor *v);
};

struct Type;
struct Stmt;
struct Block;

struct FuncDecl : public AstNode {
   struct Param {
      Type *type;
      std::string name;
      std::vector<CommentNode *> comment_nodes;
   };

   Type *return_type;
   std::string name;
   std::vector<Param*> params;
   Block* block;
   
   FuncDecl(std::string _name) : name(_name) {}
   void visit(AstVisitor *v);
};

struct Expr;

struct Stmt : public AstNode {
   enum Type { _unknown, _block, _for, _while, _if, _switch };

   Type type;
   Expr *expr; 
   Stmt *sub_stmt[2];

   Stmt(Type _type = _unknown) : type(_type), expr(0) {
      sub_stmt[0] = sub_stmt[1] = 0;
   }

   void visit(AstVisitor *v);
};

struct ExprStmt : public Stmt {
   Expr *expr;
   ExprStmt() : expr(0) {}
   void visit(AstVisitor *v);
};

struct IfStmt : public Stmt {
   Expr *cond;
   Stmt *then, *els;

   IfStmt() : cond(0), then(0), els(0) {}
   void visit(AstVisitor *v);
};

struct IterStmt : public Stmt { // while + for
   Stmt *init;
   Expr *cond, *post;
   Stmt *substmt;

   IterStmt() : cond(0), init(0), substmt(0), post(0) {}
   void visit(AstVisitor *v);
   bool is_for() { return init != 0 and post != 0; }
};

struct DeclStmt : public Stmt {
   struct Decl {
      std::string name;
      Expr *init;
      CommentNode *comment_node;
   };

   AstNode *type;
   std::vector<Decl> decls;

   void visit(AstVisitor *v);
};

struct JumpStmt : public Stmt {
   enum Type { unknown = -1, _break = 0, _continue = 1, _goto = 2 };

   Type type;
   std::string label;

   JumpStmt() : type(unknown) {}
   void visit(AstVisitor *v);

   static Type keyword2type(std::string s);
};

struct Block : public Stmt {
   std::vector<Stmt*> stmts;
   Block() : Stmt(Stmt::_block) {}
   void visit(AstVisitor *v);
};

struct Expr : public AstNode {
   enum Type { 
      unknown,
      // pm_expression 
      multiplicative, additive, shift, relational, equality, 
      bit_and, bit_xor, bit_or, logical_and, logical_or, conditional,
      assignment, comma
   };
   struct Op2TypeInitializer { Op2TypeInitializer(); }; // init _op2type

   bool paren;

   Expr() : paren(false) {}

   static std::map<std::string, Type> _op2type;
   static Type op2type(std::string op);
   static Op2TypeInitializer initializer;
   static bool right_associative(Type t);
};

struct Literal : public Expr {
   std::string lit;
   Literal(std::string _lit = "") : lit(_lit) {}
   void visit(AstVisitor *v);
};

struct Identifier : public Expr {
   std::string id;
   Identifier(std::string _id = "") : id(_id) {}
   void visit(AstVisitor *v);
};

struct BinaryExpr : public Expr {
   Type type;
   std::string op;
   std::string str;
   Expr *left, *right;

   BinaryExpr(Type _type = unknown) 
      : type(_type), op("") {}

   void visit(AstVisitor *v);
   void set(std::string op);
};

struct SignExpr : public Expr {
   Expr *expr;
   enum Type { Positive, Negative };
   Type type;
   SignExpr(Type t) : type(t) {}
   void visit(AstVisitor *v);
};

struct CallExpr : public Expr {
   Expr *func;
   std::vector<Expr *> args;
   CallExpr() : func(0) {}
   void visit(AstVisitor *v);
};

struct IndexExpr : public Expr {
   Expr *base, *index;
   IndexExpr() : base(0), index(0) {}
   void visit(AstVisitor *v);
};

struct FieldExpr : public Expr {
   Expr *base;
   Identifier *field;
   bool pointer;

   FieldExpr() : base(0), field(0) {}
   void visit(AstVisitor *v);
};

struct Type : public AstNode {
   std::string name;
   Type(std::string _name) : name(_name) {}
   void visit(AstVisitor *v);
};

// AstVisitor

class AstVisitor {
   int _indent;
   std::ostream *_out;

protected:
   enum OutType { normal, beginl };

   std::ostream& out(OutType typ = normal);
   void indent(int x) { 
      _indent += x; 
      assert(_indent >= 0);
   }

public:
   AstVisitor(std::ostream *o) : _indent(0), _out(o) {}

   virtual void visit_comment(CommentNode*) = 0;
   virtual void visit_program(Program*) = 0;
   virtual void visit_include(Include*) = 0;
   virtual void visit_macro(Macro *) = 0;
   virtual void visit_using(Using *) = 0;
   virtual void visit_funcdecl(FuncDecl *) = 0;
   virtual void visit_type(Type *) = 0;
   virtual void visit_stmt(Stmt *) = 0;
   virtual void visit_block(Block *) = 0;
   virtual void visit_literal(Literal *) = 0;
   virtual void visit_identifier(Identifier *) = 0;
   virtual void visit_binaryexpr(BinaryExpr *) = 0;
   virtual void visit_declstmt(DeclStmt *) = 0;
   virtual void visit_exprstmt(ExprStmt *) = 0;
   virtual void visit_ifstmt(IfStmt *) = 0;
   virtual void visit_iterstmt(IterStmt *) = 0;
   virtual void visit_jumpstmt(JumpStmt *) = 0;
   virtual void visit_callexpr(CallExpr *) = 0;
   virtual void visit_indexexpr(IndexExpr *) = 0;
   virtual void visit_fieldexpr(FieldExpr *) = 0;
   virtual void visit_signexpr(SignExpr *) = 0;
};

// Visit implementations
inline void Program::visit(AstVisitor *v)     { v->visit_program(this); }
inline void CommentNode::visit(AstVisitor *v) { v->visit_comment(this); }
inline void Include::visit(AstVisitor *v)     { v->visit_include(this); }
inline void Macro::visit(AstVisitor *v)       { v->visit_macro(this); }
inline void Using::visit(AstVisitor *v)       { v->visit_using(this); }
inline void FuncDecl::visit(AstVisitor* v)    { v->visit_funcdecl(this); }
inline void Type::visit(AstVisitor *v)        { v->visit_type(this); }
inline void Stmt::visit(AstVisitor *v)        { v->visit_stmt(this); }
inline void Block::visit(AstVisitor *v)       { v->visit_block(this); }
inline void Literal::visit(AstVisitor *v)     { v->visit_literal(this); }
inline void Identifier::visit(AstVisitor *v)  { v->visit_identifier(this); }
inline void BinaryExpr::visit(AstVisitor *v)  { v->visit_binaryexpr(this); }
inline void DeclStmt::visit(AstVisitor *v)    { v->visit_declstmt(this); }
inline void ExprStmt::visit(AstVisitor *v)    { v->visit_exprstmt(this); }
inline void IfStmt::visit(AstVisitor *v)      { v->visit_ifstmt(this); }
inline void IterStmt::visit(AstVisitor *v)    { v->visit_iterstmt(this); }
inline void JumpStmt::visit(AstVisitor *v)    { v->visit_jumpstmt(this); }
inline void CallExpr::visit(AstVisitor *v)    { v->visit_callexpr(this); }
inline void IndexExpr::visit(AstVisitor *v)   { v->visit_indexexpr(this); }
inline void FieldExpr::visit(AstVisitor *v)   { v->visit_fieldexpr(this); }
inline void SignExpr::visit(AstVisitor *v)    { v->visit_signexpr(this); }

// Comment helpers
std::string cmt(CommentNode* cn, bool pre, bool post, bool missing);
std::string cmtl(CommentNode *cn);

template<typename T> std::string _cmt  (T* x, int i) { return cmt(x->comment_nodes[i], 1, 0, 1); }
template<typename T> std::string _cmt_ (T* x, int i) { return cmt(x->comment_nodes[i], 1, 1, 1); }
template<typename T> std::string _cmt0 (T* x, int i) { return cmt(x->comment_nodes[i], 1, 0, 0); }
template<typename T> std::string _cmt0_(T* x, int i) { return cmt(x->comment_nodes[i], 1, 1, 0); }
template<typename T> std::string _cmtl (T* x, int i) { return cmtl(x->comment_nodes[i]); }

#endif
