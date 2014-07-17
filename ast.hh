#ifndef AST_H
#define AST_H

#include <assert.h>
#include <string>
#include <vector>
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
   Using(std::string _namespc) : namespc(_namespc) {}
   void visit(AstVisitor *v);
};

struct Type;
struct Stmt;
struct Block;

struct FuncDecl : public AstNode {
   struct Param {
      Type *type;
      std::string name;
      CommentNode *comment_nodes[3];
   };

   Type *return_type;
   std::string name;
   std::vector<Param> params;
   Block* block;
   
   FuncDecl(std::string _name) : name(_name) {}
   void visit(AstVisitor *v);
};

struct Expr;

struct Stmt : public AstNode {
   enum Type { _empty, _expr, _block, _for, _while, _if, _switch };

   Type type;
   Expr *expr; 
   Stmt *sub_stmt[2];

   Stmt(Type _type = _empty) 
      : type(_type), expr(0)
   {
      sub_stmt[0] = sub_stmt[1] = 0;
   }

   void visit(AstVisitor *v);
};

struct Block : public Stmt {
   std::vector<Stmt*> stmts;
   Block() : Stmt(Stmt::_block) {}
   void visit(AstVisitor *v);
};

struct Expr : public AstNode {
   enum Type { 
      unknown, identifier, literal, 
      multiplicative, additive, shift, relational, equality, 
      bit_and, bit_xor, bit_or, logical_and, logical_or, conditional,
      assignment
   };
   enum Op { 
      none, assign, add, sub, mult, div 
   };
   
   Type type;
   bool paren;
   Op op;
   std::string str;
   Expr *left, *right;

   Expr(Type _type = unknown) 
      : type(_type), op(none), paren(false) {}

   void visit(AstVisitor *v);

   static Expr::Type char2type(char c);
   static       char op2char(Expr::Op type);
   void set(char op);
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
   virtual void visit_expr(Expr *) = 0;
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
inline void Expr::visit(AstVisitor *v)        { v->visit_expr(this); }

// Comment helpers
std::string cmt(CommentNode* cn, bool pre, bool post, bool missing);
std::string cmtl(CommentNode *cn);

template<typename T> std::string _cmt  (T* x, int i) { return cmt(x->comment_nodes[i], true, false, true); }
template<typename T> std::string _cmt_ (T* x, int i) { return cmt(x->comment_nodes[i], true, true,  true); }
template<typename T> std::string _cmt0 (T* x, int i) { return cmt(x->comment_nodes[i], true, false, false); }
template<typename T> std::string _cmt0_(T* x, int i) { return cmt(x->comment_nodes[i], true, true,  false); }
template<typename T> std::string _cmtl (T* x, int i) { return cmtl(x->comment_nodes[i]); }

#endif
